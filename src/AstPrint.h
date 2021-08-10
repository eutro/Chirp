#include "Ast.h"

namespace ast::print {
  std::ostream &operator<<(std::ostream &os, const Token &token);
  std::ostream &operator<<(std::ostream &os, const Program &program);
  std::ostream &operator<<(std::ostream &os, const Identifier &identifier);
  std::ostream &operator<<(std::ostream &os, const TypeHint &hint);
  std::ostream &operator<<(std::ostream &os, const RawBinding &binding);
  std::ostream &operator<<(std::ostream &os, const Binding::Arguments &arguments);
  std::ostream &operator<<(std::ostream &os, const Binding::TypeArguments &arguments);
  std::ostream &operator<<(std::ostream &os, const Binding &binding);
  std::ostream &operator<<(std::ostream &os, const Statement &statement);
  std::ostream &operator<<(std::ostream &os, const Defn &it);
  std::ostream &operator<<(std::ostream &os, const IfExpr &it);
  std::ostream &operator<<(std::ostream &os, const LetExpr &it);
  std::ostream &operator<<(std::ostream &os, const BlockExpr &it);
  std::ostream &operator<<(std::ostream &os, const BracketExpr &it);
  std::ostream &operator<<(std::ostream &os, const ColonExpr &it);
  std::ostream &operator<<(std::ostream &os, const LiteralExpr &it);
  std::ostream &operator<<(std::ostream &os, const VarExpr &it);
  std::ostream &operator<<(std::ostream &os, const BinaryExpr &it);
  std::ostream &operator<<(std::ostream &os, const PrefixExpr &it);
  std::ostream &operator<<(std::ostream &os, const FunCallExpr &it);
  std::ostream &operator<<(std::ostream &os, const HintedExpr &it);
  std::ostream &operator<<(std::ostream &os, const FnExpr &it);
  std::ostream &operator<<(std::ostream &os, const LambdaExpr &it);
  std::ostream &operator<<(std::ostream &os, const IfExpr::ElseIf &anIf);
  std::ostream &operator<<(std::ostream &os, const IfExpr::Else &anElse);
  std::ostream &operator<<(std::ostream &os, const Type &it);
  std::ostream &operator<<(std::ostream &os, const PlaceholderType &it);
  std::ostream &operator<<(std::ostream &os, const NamedType &it);
  std::ostream &operator<<(std::ostream &os, const NamedType::TypeParameters &parameters);
  std::ostream &operator<<(std::ostream &os, const BlockExpr::Stmt &stmt);
  std::ostream &operator<<(std::ostream &os, const BinaryExpr::Rhs &rhs);
}
