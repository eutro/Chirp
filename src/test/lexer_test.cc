#include "../fsm/Lexer.h"
#include <sstream>

enum class TokType {
  Invalid,

  Whitespace,
  Newline,

  ParenOpen,
  ParenClose,
  BraceOpen,
  BraceClose,
  SquareOpen,
  SquareClose,
  Add,
  Sub,
  Mul,
  Div,
  AddAssign,
  SubAssign,
  MulAssign,
  DivAssign,
  Int,
  Float,
  Ident,
};

const std::vector<std::pair<TokType, std::string>> tokenTypes =
    {
        {TokType::Whitespace,  "[ \t]+"},
        {TokType::Newline,     "\n(\r|)"},

        {TokType::ParenOpen,   "\\("},
        {TokType::ParenClose,  "\\)"},
        {TokType::BraceOpen,   "\\{"},
        {TokType::BraceClose,  "\\}"},
        {TokType::SquareOpen,  "\\["},
        {TokType::SquareClose, "\\]"},

        {TokType::Add,         "\\+"},
        {TokType::Sub,         "\\-"},
        {TokType::Mul,         "\\*"},
        {TokType::Div,         "\\/"},
        {TokType::AddAssign,   "\\+="},
        {TokType::SubAssign,   "\\-="},
        {TokType::MulAssign,   "\\*="},
        {TokType::DivAssign,   "\\/="},

        {TokType::Ident,       "[a-zA-Z_][a-zA-Z_0-9]*"},

        {TokType::Int,         "0|[1-9][0-9]*"},
        {TokType::Float,       "[0-9]+\\.[0-9]+"},
    };

template<>
TokType fsm::Finished<TokType>::rejecting() {
  return TokType::Invalid;
}

std::ostream &operator<<(std::ostream &out, const TokType &tt) {
  return out << (int) tt;
}

int main() {
  lexer::Lexer<TokType> lexer(tokenTypes);

  std::stringstream sin("## bar -= 1 + 2.0 * (6 / foo) + foo");
  for (const auto &tok : lexer.lex(sin)) {
    std::cout << "\"" << tok.value << "\" (" << tok.type << ")\n";
  }
}
