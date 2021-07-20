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
  char *fileName = std::getenv("CRP_FILENAME");
  char *fileDir = std::getenv("CRP_FILEDIR");
  llvm::Module module(fileName ? fileName : "module", lc);
  module.addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
  llvm::IRBuilder builder(lc);
  llvm::DIBuilder diBuilder(module);
  ast::CompileContext cc(lc, builder, diBuilder, module, pc);
  cc.diCU = diBuilder.createCompileUnit(
      llvm::dwarf::DW_LANG_C,
      diBuilder.createFile(fileName ? fileName : "module.crp", fileDir ? fileDir : "."),
      "Chirp Compiler",
      false, "", 0
  );
  program.compile(cc);
  diBuilder.finalize();
  module.print(llvm::outs(), nullptr);
}
