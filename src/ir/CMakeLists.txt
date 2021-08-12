add_library(tok tok/Tok.cc)
target_link_libraries(tok regex loc)

add_library(ast ast/Ast.cc)

add_library(hir hir/Hir.cc)

add_library(parser tok/Parser.cc)
target_link_libraries(parser tok ast err)

add_library(astlower ast/Lowering.cc)
target_link_libraries(astlower ast hir err)
