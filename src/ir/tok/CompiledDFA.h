class CompiledDFA {
public:
  size_t initial;
  CompiledDFA();
  bool accept(size_t &current, char &symbol) const;
  tok::Tok finished(size_t state) const;
};