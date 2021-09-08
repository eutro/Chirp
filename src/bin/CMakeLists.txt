add_executable(in2tokens in2tokens.cc)
target_link_libraries(in2tokens parser)

add_executable(in2llvm in2llvm.cc)
target_link_libraries(in2llvm parser astlower hirinfer hirlower lircodegen)
