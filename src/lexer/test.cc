#include "Lexer.tcc"
#include <iostream>
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

std::vector<std::pair<TokType, std::string>> tokenTypes =
  {
    {TokType::Whitespace, "[ \t]"},
    {TokType::Newline, "\n(\r|)"},

    {TokType::ParenOpen, "\\("},
    {TokType::ParenClose, "\\)"},
    {TokType::BraceOpen, "\\{"},
    {TokType::BraceClose, "\\}"},
    {TokType::SquareOpen, "\\["},
    {TokType::SquareClose, "\\]"},
    {TokType::Add, "\\+"},
    {TokType::Sub, "\\-"},
    {TokType::Mul, "\\*"},
    {TokType::Div, "\\/"},
    {TokType::AddAssign, "\\+="},
    {TokType::SubAssign, "\\-="},
    {TokType::MulAssign, "\\*="},
    {TokType::DivAssign, "\\/="},
    {TokType::Ident, "[abcdefghijklmnopqrstuvwxyz]+"},
    {TokType::Int, "0|[123456789][0123456789]*"},
    {TokType::Float, "[0123456789]+\\.[0123456789]+"},
  };

TokType &operator|=(TokType &a, const TokType &b) {
  if (b > a) {
    a = b;
  }
  return a;
}

bool operator!(const TokType &tt) {
  return tt == TokType::Invalid;
}

std::ostream &operator<<(std::ostream &out, const TokType &tt) {
  return out << (int) tt;
}

int main() {
  lexer::Lexer<TokType> lexer(tokenTypes);

  std::stringstream sin("bar -= 1 + 2.0 * (6 / foo)");
  auto lexed = lexer.lex(sin);
  for (const auto &tok : lexed) {
    std::cout << tok.value << " (" << tok.type << ")\n";
  }
}
