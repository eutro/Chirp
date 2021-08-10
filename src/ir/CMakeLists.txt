add_library(ast ast/Visitor.cc)
add_library(tokens tokens/Parser.cc)
target_link_libraries(tokens ast err regex)
