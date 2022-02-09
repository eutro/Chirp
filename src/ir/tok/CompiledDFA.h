class CompiledDFA {
public:
  Idx initial;
  CompiledDFA();
  bool accept(Idx &current, char &symbol) const;
  tok::Tok finished(Idx state) const;
};