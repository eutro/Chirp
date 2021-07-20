#include "Ast.h"

namespace ast {
  std::ostream &operator<<(std::ostream &os, const Token &token) {
    os << '"';
    for (char c : token.value) {
      switch (c) {
        case '\n':
          os << "\\n";
          break;
        case '"':
          os << "\\\"";
          break;
        case '\\':
          os << "\\\\";
          break;
        default:
          os << c;
      }
    }
    os << '"';
    return os;
  }

  template<typename Iterable>
  void printMulti(std::ostream &os, const Iterable &ts) {
    for (const auto &el : ts) {
      os << " " << el;
    }
  }

  template<typename IterableA, typename IterableB>
  void printMulti(std::ostream &os, const IterableA &ts, const IterableB &commas) {
    auto it = ts.begin();
    auto ci = commas.begin();
    for (; it != ts.end(); ++it) {
      os << " " << *it;
      if (ci != commas.end()) {
        os << " " << *ci;
        ++ci;
      }
    }
  }

  std::ostream &operator<<(std::ostream &os, const Program &program) {
    os << "(Program";
    printMulti(os, program.statements);
    os << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Statement> &statement) {
    statement->print(os);
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Expr> &statement) {
    statement->print(os);
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const std::unique_ptr<DelimitedExpr> &statement) {
    statement->print(os);
    return os;
  }

  void Defn::print(std::ostream &os) const {
    os << "(Defn " << defnToken << " " << binding << ")";
  }

  void IfExpr::print(std::ostream &os) const {
    os << "(IfExpr ";
    if (type) os << "#\"" << *CType::get(type) << "\" ";
    os << ifToken << " " << predExpr << " " << thenExpr;
    printMulti(os, this->elseIfClauses);
    if (this->elseClause) os << " " << *this->elseClause;
    os << ")";
  }

  void LetExpr::print(std::ostream &os) const {
    os << "(LetExpr ";
    if (type) os << "#\"" << *CType::get(type) << "\" ";
    os << letToken;
    printMulti(os, bindings, commas);
    os << " " << inToken;
    if (name) os << " " << *name;
    os << " " << body << ")";
  }

  void BlockExpr::print(std::ostream &os) const {
    os << "(BlockExpr ";
    if (type) os << "#\"" << *CType::get(type) << "\" ";
    os << openToken;
    printMulti(os, statements);
    if (value) os << " " << value;
    os << " " << closeToken << ")";
  }

  void BracketExpr::print(std::ostream &os) const {
    os << "(BracketExpr ";
    if (type) os << "#\"" << *CType::get(type) << "\" ";
    os << openToken << " " << value << " " << closeToken << ")";
  }

  void ColonExpr::print(std::ostream &os) const {
    os << "(ColonExpr ";
    if (type) os << "#\"" << *CType::get(type) << "\" ";
    os << colonToken << " " << value << " " << ")";
  }

  void LiteralExpr::print(std::ostream &os) const {
    os << "(LiteralExpr ";
    if (type) os << "#\"" << *CType::get(type) << "\" ";
    os << value << ")";
  }

  void VarExpr::print(std::ostream &os) const {
    os << "(VarExpr ";
    if (type) os << "#\"" << *CType::get(type) << "\" ";
    os << name << ")";
  }

  void BinaryExpr::print(std::ostream &os) const {
    os << "(BinaryExpr ";
    if (type) os << "#\"" << *CType::get(type) << "\" ";
    os << this->lhs;
    printMulti(os, this->terms);
    os << ")";
  }

  void PrefixExpr::print(std::ostream &os) const {
    os << "(PrefixExpr";
    if (type) os << " #\"" << *CType::get(type) << "\"";
    printMulti(os, this->prefixes);
    os << " " << this->expr << ")";
  }

  void FunCallExpr::print(std::ostream &os) const {
    os << "(FunCallExpr ";
    if (type) os << "#\"" << *CType::get(type) << "\" ";
    os << this->function << " " << this->openToken;
    printMulti(os, this->arguments, this->commas);
    os << " " << this->closeToken << ")";
  }

  void HintedExpr::print(std::ostream &os) const {
    os << "(HintedExpr ";
    if (type) os << "#\"" << *CType::get(type) << "\" ";
    os << this->expr << " " << this->hint << ")";
  }

  void FnExpr::print(std::ostream &os) const {
    os << "(FnExpr ";
    if (type) os << "#\"" << *CType::get(type) << "\" ";
    os << fnToken << " ";
    if (name) os << *name << " ";
    os << arguments << " ";
    if (typeHint) os << *typeHint << " ";
    os << eqToken << " " << body << ")";
  }

  void LambdaExpr::print(std::ostream &os) const {
    os << "(LambdaExpr";
    if (type) os << " #\"" << *CType::get(type) << "\"";
    os << " " << lambdaToken;
    printMulti(os, arguments, commas);
    os << " " << dotToken << " " << body << ")";
  }

  std::ostream &operator<<(std::ostream &os, const IfExpr::ElseIf &anIf) {
    os << "(ElseIf " << anIf.elseToken << " " << anIf.ifToken << " " << anIf.predExpr
       << " " << anIf.thenExpr << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const IfExpr::Else &anElse) {
    os << "(Else " << anElse.elseToken << " " << anElse.thenExpr << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const Binding &binding) {
    os << "(Binding ";
    if (binding.var) os << "#\"" << *binding.var->type << "\" ";
    os << binding.name;
    if (binding.arguments) os << " " << *binding.arguments;
    if (binding.typeHint) os << " " << *binding.typeHint;
    os << " " << binding.eqToken << " ";
    if (binding.value) os << binding.value;
    else os << *binding.foreignToken;
    os << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const Identifier &identifier) {
    os << "(Identifier " << identifier.ident << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const Binding::Arguments &arguments) {
    os << "(Arguments " << arguments.openToken << " ";
    printMulti(os, arguments.bindings, arguments.commas);
    os << " " << arguments.closeToken << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const Binding::TypeArguments &arguments) {
    os << "(TypeArguments " << arguments.openToken << " ";
    printMulti(os, arguments.idents, arguments.commas);
    os << " " << arguments.closeToken << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const TypeHint &hint) {
    os << "(TypeHint " << hint.colon << " " << hint.type << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const RawBinding &binding) {
    os << "(RawBinding " << binding.name;
    if (binding.typeHint) os << *binding.typeHint;
    os << ")";
    return os;
  }

  void PlaceholderType::print(std::ostream &os) const {
    os << "(PlaceholderType " << placeholder << ")";
  }

  void NamedType::print(std::ostream &os) const {
    os << "(NamedType " << raw;
    if (this->parameters) os << " " << *this->parameters;
    os << ")";
  }

  std::ostream &operator<<(std::ostream &os, const NamedType::TypeParameters &parameters) {
    os << "(TypeParameters " << parameters.openToken << " ";
    printMulti(os, parameters.types, parameters.commas);
    os << " " << parameters.closeToken << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Type> &statement) {
    statement->print(os);
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const BlockExpr::Stmt &stmt) {
    os << "(Statement " << stmt.statement << " " << stmt.delimiter << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const BinaryExpr::Rhs &rhs) {
    os << rhs.operatorToken << " " << rhs.expr;
    return os;
  }
}
