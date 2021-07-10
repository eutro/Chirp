#include "../Parser.h"
#include <llvm/Support/raw_ostream.h>

int main() {
  lexer::Lexer<ast::Tok> lexer(TOKEN_PATTERNS);
  auto tokens = lexer.lex(std::cin);

  err::ErrorContext ec;
  ast::Program program = parser::parseProgram(ec, tokens);

  if (!ec.errors.empty()) {
    std::cerr << "Aborting due to errors while parsing:\n";
    err::ErrorPrintContext epc(std::move(tokens.stream.lines), std::cerr);
    for (const auto &err : ec.errors) {
      (epc << err).os << "\n";
    }
    return 1;
  }

  type::TypeContext tc;
  ast::ParseContext pc(tc);
  program.inferTypes(pc);

  if (!tc.errors.empty()) {
    std::cerr << "Aborting due to errors:\n";
    for (const auto &err : tc.errors) {
      std::cerr << err << "\n\n";
    }
    return 1;
  }

  llvm::LLVMContext lc;
  llvm::IRBuilder builder(lc);
  llvm::Module module("module", lc);
  ast::CompileContext cc(lc, builder, module, pc);
  program.compile(cc);
  module.print(llvm::outs(), nullptr);
}
