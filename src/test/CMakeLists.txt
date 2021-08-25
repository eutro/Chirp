add_executable(fsm_test fsm_test.cc)
target_link_libraries(fsm_test regex)
add_test(fsm_test fsm_test)

add_executable(lexer_test lexer_test.cc)
target_link_libraries(lexer_test regex err)
add_test(lexer_test lexer_test)

add_executable(try_lower_hir try_lower_hir.cc)
target_link_libraries(try_lower_hir parser astlower hirjson)

add_executable(try_infer try_infer.cc)
target_link_libraries(try_infer parser astlower hirinfer)
