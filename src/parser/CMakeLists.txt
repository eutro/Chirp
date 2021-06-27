add_executable(parser_test test.cc Ast.cc Parser.cc)
add_test(parser_test parser_test)

add_executable(in2ast in2ast.cc Ast.cc Parser.cc)
add_executable(in2tokens in2tokens.cc Ast.cc Parser.cc)
