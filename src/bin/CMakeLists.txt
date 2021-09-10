add_executable(in2tokens in2tokens.cc)
target_link_libraries(in2tokens parser)

add_executable(infer infer.cc)
target_link_libraries(infer parser astlower hirinfer)

add_executable(in2llvm in2llvm.cc)
target_link_libraries(in2llvm parser astlower hirinfer hirlower lircodegen)
