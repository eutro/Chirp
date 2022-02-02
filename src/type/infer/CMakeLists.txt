add_library(typeinfer Fn.cc VM.cc LookupTable.cc Inst.cc Insns.cc)
target_link_libraries(typeinfer type err logging)
