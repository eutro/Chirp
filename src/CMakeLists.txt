cmake_minimum_required(VERSION 3.1)

project(CodingResearchProject)

set(CMAKE_CXX_STANDARD 17)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(llvm_libs core analysis)

add_library(fsm RegEx.cc RegEx.h DFA.h NFA.h)

add_library(lexer Lexer.cc)
target_link_libraries(lexer fsm)

add_library(parser Ast.cc Parser.cc Type.cc)
target_link_libraries(parser lexer)

add_subdirectory(bin)

enable_testing()

add_subdirectory(test)
