add_executable(in2tokens in2tokens.cc)
target_link_libraries(in2tokens ast parser)
target_include_directories(in2tokens PUBLIC parser)

add_executable(in2ast in2ast.cc)
target_link_libraries(in2ast ast astprint parser)
target_include_directories(in2ast PUBLIC parser)

add_executable(in2types in2types.cc)
target_link_libraries(in2types ast astprint parser)
target_include_directories(in2types PUBLIC parser)

add_executable(in2llvm in2llvm.cc)
target_link_libraries(in2llvm ast parser)
target_include_directories(in2llvm PUBLIC parser)
