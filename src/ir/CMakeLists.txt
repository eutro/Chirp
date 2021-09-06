add_library(tok tok/Tok.cc)
target_link_libraries(tok regex loc)

add_library(ast ast/Ast.cc)

add_library(hir hir/Hir.cc)

add_library(parser tok/Parser.cc)
target_link_libraries(parser tok ast err)

add_library(hirrebind hir/Rebind.cc)
target_link_libraries(hirrebind hir)

add_library(hirjson hir/Json.cc)
target_link_libraries(hirjson PUBLIC hir)
target_link_libraries(hirjson PUBLIC nlohmann_json::nlohmann_json)

add_library(astlower ast/Lowering.cc)
target_link_libraries(astlower ast hirrebind err)

add_library(hirinfer hir/Infer.cc)
target_link_libraries(hirinfer hir err type)

add_library(hirlower hir/Lowering.cc)
target_link_libraries(hirlower hir err type)
