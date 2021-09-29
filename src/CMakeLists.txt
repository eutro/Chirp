cmake_minimum_required(VERSION 3.19)

project(Chirp)

set(CMAKE_CXX_STANDARD 17)

find_package(LLVM 12 REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(llvm_libs core support)

include(FetchContent)

FetchContent_Declare(json
  GIT_REPOSITORY https://github.com/ArthurSonzogni/nlohmann_json_cmake_fetchcontent
  GIT_TAG v3.9.1)

FetchContent_GetProperties(json)
if(NOT json_POPULATED)
  FetchContent_Populate(json)
  add_subdirectory(${json_SOURCE_DIR} ${json_BINARY_DIR} EXCLUDE_FROM_ALL)
endif()

if(MSVC)
  add_compile_options(/W4)
else()
  add_compile_options(-Wall -Wextra -Wno-unused-parameter -Wno-missing-field-initializers)
endif()

option(CHIRP_DISABLE_COLLECTOR "Disable the garbage collector" OFF)
option(CHIRP_COLLECTOR_MARK_AND_SWEEP
  "Use a simple mark and sweep garbage collector instead of the default generational one"
  OFF)

if(CHIRP_DISABLE_COLLECTOR)
  add_definitions(-DCHIRP_COLLECTOR_DISABLED)
endif()

if(CHIRP_COLLECTOR_MARK_AND_SWEEP)
  add_definitions(-DCHIRP_COLLECTOR_MARK_AND_SWEEP)
endif()

add_subdirectory(common)
add_subdirectory(fsm)
add_subdirectory(type)
add_subdirectory(ir)

add_library(chirp_runtime SHARED runtime/Collector.c runtime/Runtime.c)
install(TARGETS chirp_runtime ARCHIVE)

add_subdirectory(bin)

if(UNIX) # this almost certainly doesn't work on other systems
  install(PROGRAMS ../scripts/crpc TYPE SBIN)
  install(PROGRAMS ../scripts/crpi TYPE SBIN)
endif(UNIX)

enable_testing()

add_subdirectory(test)
