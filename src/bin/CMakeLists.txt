add_executable(in2tokens in2tokens.cc)
target_link_libraries(in2tokens parser)

add_executable(chirpinfer infer.cc)
target_link_libraries(chirpinfer parser astlower hirinfer)
install(TARGETS chirpinfer RUNTIME)

add_executable(chirp2llvm in2llvm.cc)
target_link_libraries(chirp2llvm parser astlower hirinfer hirlower lircodegen)
install(TARGETS chirp2llvm RUNTIME)
