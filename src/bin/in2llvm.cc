#include "../Parser.h"
#include <llvm-6.0/llvm/Support/raw_ostream.h>

int main() {
  lexer::Lexer<ast::Tok> lexer(TOKEN_PATTERNS);
  ast::Program program = parser::parseProgram(lexer.lex(std::cin));

  type::TypeContext tc;
  ast::ParseContext pc(tc);
  program.inferTypes(pc);

  llvm::LLVMContext lc;
  llvm::IRBuilder builder(lc);
  llvm::Module module("module", lc);
  ast::CompileContext cc(lc, builder, module, pc);
  program.compile(cc);
  module.print(llvm::outs(), nullptr);
}
