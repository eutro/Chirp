add_library(typeinfer Fn.cc VM.cc LookupTable.cc)
target_link_libraries(typeinfer type)
