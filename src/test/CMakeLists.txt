add_executable(fsm_test fsm_test.cc)
target_link_libraries(fsm_test fsm)
target_include_directories(fsm_test PUBLIC fsm)
add_test(fsm_test fsm_test)

add_executable(lexer_test lexer_test)
target_link_libraries(lexer_test lexer)
target_include_directories(lexer_test PUBLIC lexer)
add_test(lexer_test lexer_test)

add_executable(type_test type_test.cc)
target_link_libraries(type_test parser)
target_include_directories(type_test PUBLIC parser)
add_test(type_test type_test)