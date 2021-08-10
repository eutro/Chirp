add_library(loc Loc.cc)
add_library(err Err.cc)
target_link_libraries(err loc)
