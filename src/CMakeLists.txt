cmake_minimum_required(VERSION 3.19)

project(Chirp)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

find_package(LLVM 13 REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(llvm_libs core support)

if(MSVC)
  add_compile_options(/W4
    /wd4458 # declaration hides class member
    /wd4100 # unused formal parameter
    /wd4267 # lossy conversions
    /wd4244 # also lossy conversions
    /wd4245 # also lossy conversions
    /wd4996 # deprecations
    /wd4624 # destructor implicitly marked as deleted
    )
else()
  add_compile_options(-Wall -Wextra -Wno-unused-parameter -Wno-missing-field-initializers)
endif()

add_subdirectory(common)
add_subdirectory(fsm)
add_subdirectory(type)
add_subdirectory(ir)

set(CHIRP_KNOWN_COLLECTOR_STRATEGIES "Noop" "MarkAndSweep" "Generational")
set(CHIRP_COLLECTOR_STRATEGY
  "Dynamic"
  CACHE
  STRING
  "The type of garbage collector used by the Chirp runtime.")
set_property(CACHE CHIRP_COLLECTOR_STRATEGY
  PROPERTY STRINGS "Dynamic" ${CHIRP_KNOWN_COLLECTOR_STRATEGIES})
message(STATUS "Garbage collector style: ${CHIRP_COLLECTOR_STRATEGY}")

add_library(gc)
target_sources(gc PUBLIC runtime/collector/RootVisitor.c)
if(${CHIRP_COLLECTOR_STRATEGY} STREQUAL "Dynamic")
  add_compile_definitions(CHIRP_DYNAMIC_COLLECTOR)
  target_sources(gc PUBLIC runtime/collector/DynamicCollector.c)
  list(TRANSFORM CHIRP_KNOWN_COLLECTOR_STRATEGIES
    REPLACE ".+" "runtime/collector/\\0GC.c"
    OUTPUT_VARIABLE CHIRP_COLLECTOR_SOURCES)
  target_sources(gc PUBLIC ${CHIRP_COLLECTOR_SOURCES})
elseif(${CHIRP_COLLECTOR_STRATEGY} IN_LIST CHIRP_KNOWN_COLLECTOR_STRATEGIES)
  target_sources(gc PUBLIC runtime/collector/${CHIRP_COLLECTOR_STRATEGY}GC.c)
else()
  list(JOIN CHIRP_KNOWN_COLLECTOR_STRATEGIES ", " COLLECTOR_STRATEGIES_PP)
  message(FATAL_ERROR
    "Unknown garbage collector strategy: ${CHIRP_COLLECTOR_STRATEGY}\n"
    "Known styles: Dynamic, ${COLLECTOR_STRATEGIES_PP}")
endif()

add_library(chirp_runtime SHARED runtime/Runtime.c)
target_link_libraries(chirp_runtime gc)

install(TARGETS chirp_runtime ARCHIVE)

add_subdirectory(bin)

if(UNIX) # this almost certainly doesn't work on other systems
  install(PROGRAMS ../scripts/crpc TYPE SBIN)
  install(PROGRAMS ../scripts/crpi TYPE SBIN)
endif(UNIX)

enable_testing()

add_subdirectory(test)
