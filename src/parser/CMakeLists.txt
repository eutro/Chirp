add_library(parser Ast.cc Parser.cc ../compiler/Type.cc)

add_executable(parser_test test.cc)
target_link_libraries(parser_test parser)

add_executable(in2tokens in2tokens.cc)
target_link_libraries(in2tokens parser)

add_executable(in2ast in2ast.cc)
target_link_libraries(in2ast parser)

add_executable(in2types in2types.cc)
target_link_libraries(in2types parser)

add_test(parser_test parser_test)
