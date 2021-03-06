add_library(tok tok/Tok.cc)
target_link_libraries(tok regex loc)

add_executable(tokencode tok/Encode.cc)
target_compile_definitions(tokencode PUBLIC USE_COMPILED_DFA=0)
target_link_libraries(tokencode regex loc)

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

add_library(hiridx hir/IdxCounter.cc)
target_link_libraries(hiridx hir)

add_library(hirinfer hir/Infer.cc)
target_link_libraries(hirinfer hir err type typeinfer hiridx logging)

add_library(hirlower hir/Lowering.cc)
target_link_libraries(hirlower hir err type hiridx)

add_library(lirdisas lir/Disas.cc)

add_library(lircodegen lir/codegen/Codegen.cc lir/codegen/Util.cc lir/codegen/Util.h lir/codegen/Intrinsics.cc)
target_link_libraries(lircodegen ${llvm_libs})
