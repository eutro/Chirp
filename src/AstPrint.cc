#include "AstPrint.h"
#include <memory>
#include <ostream>

namespace ast::print {
  class PrintingVisitor : public Visitor {
  public:
    std::ostream &os;
    PrintingVisitor(std::ostream &os): os(os) {}

    void visitPlaceholderType(PlaceholderType &it) { os << it; }
    void visitNamedType(NamedType &it) { os << it; }

    void visitDefn(Defn &it) { os << it; }

    void visitIfExpr(IfExpr &it, Position pos) { os << it; }
    void visitLetExpr(LetExpr &it, Position pos) { os << it; }
    void visitFnExpr(FnExpr &it, Position pos) { os << it; }
    void visitLambdaExpr(LambdaExpr &it, Position pos) { os << it; }
    void visitBlockExpr(BlockExpr &it, Position pos) { os << it; }
    void visitBracketExpr(BracketExpr &it, Position pos) { os << it; }
    void visitColonExpr(ColonExpr &it, Position pos) { os << it; }
    void visitLiteralExpr(LiteralExpr &it, Position pos) { os << it; }
    void visitVarExpr(VarExpr &it, Position pos) { os << it; }
    void visitBinaryExpr(BinaryExpr &it, Position pos) { os << it; }
    void visitPrefixExpr(PrefixExpr &it, Position pos) { os << it; }
    void visitFunCallExpr(FunCallExpr &it, Position pos) { os << it; }
    void visitHintedExpr(HintedExpr &it, Position pos) { os << it; }
  };

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

  std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Statement> &it) {
    return os << *it;
  }
  std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Expr> &it) {
    return os << *it;
  }
  std::ostream &operator<<(std::ostream &os, const std::unique_ptr<Type> &it) {
    return os << *it;
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

  std::ostream &operator<<(std::ostream &os, const Identifier &identifier) {
    os << "(Identifier " << identifier.ident << ")";
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

  std::ostream &operator<<(std::ostream &os, const Binding &binding) {
    os << "(Binding ";
    if (binding.var) os << "#\"" << *binding.var->type << "\" ";
    os << binding.name;
    if (binding.arguments) os << " " << *binding.arguments;
    if (binding.typeHint) os << " " << *binding.typeHint;
    os << " " << binding.eqToken << " ";
    if (binding.value) os << *binding.value;
    else os << *binding.foreignToken;
    os << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const Statement &statement) {
    PrintingVisitor(os).visitStatement(const_cast<Statement&>(statement));
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const Defn &it) {
    return os << "(Defn " << it.defnToken << " " << it.binding << ")";
  }

  std::ostream &operator<<(std::ostream &os, const IfExpr &it) {
    os << "(IfExpr ";
    if (it.type) os << "#\"" << *CType::get(it.type) << "\" ";
    os << it.ifToken << " " << *it.predExpr << " " << *it.thenExpr;
    printMulti(os, it.elseIfClauses);
    if (it.elseClause) os << " " << *it.elseClause;
    return os << ")";
  }

  std::ostream &operator<<(std::ostream &os, const LetExpr &it) {
    os << "(LetExpr ";
    if (it.type) os << "#\"" << *CType::get(it.type) << "\" ";
    os << it.letToken;
    printMulti(os, it.bindings, it.commas);
    os << " " << it.inToken;
    if (it.name) os << " " << *it.name;
    return os << " " << *it.body << ")";
  }

  std::ostream &operator<<(std::ostream &os, const BlockExpr &it) {
    os << "(BlockExpr ";
    if (it.type) os << "#\"" << *CType::get(it.type) << "\" ";
    os << it.openToken;
    printMulti(os, it.statements);
    if (it.value) os << " " << *it.value;
    return os << " " << it.closeToken << ")";
  }

  std::ostream &operator<<(std::ostream &os, const BracketExpr &it) {
    os << "(BracketExpr ";
    if (it.type) os << "#\"" << *CType::get(it.type) << "\" ";
    return os << it.openToken << " " << *it.value << " " << it.closeToken << ")";
  }

  std::ostream &operator<<(std::ostream &os, const ColonExpr &it) {
    os << "(ColonExpr ";
    if (it.type) os << "#\"" << *CType::get(it.type) << "\" ";
    return os << it.colonToken << " " << *it.value << " " << ")";
  }

  std::ostream &operator<<(std::ostream &os, const LiteralExpr &it) {
    os << "(LiteralExpr ";
    if (it.type) os << "#\"" << *CType::get(it.type) << "\" ";
    return os << it.value << ")";
  }

  std::ostream &operator<<(std::ostream &os, const VarExpr &it) {
    os << "(VarExpr ";
    if (it.type) os << "#\"" << *CType::get(it.type) << "\" ";
    return os << it.name << ")";
  }

  std::ostream &operator<<(std::ostream &os, const BinaryExpr &it) {
    os << "(BinaryExpr ";
    if (it.type) os << "#\"" << *CType::get(it.type) << "\" ";
    os << *it.lhs;
    printMulti(os, it.terms);
    return os << ")";
  }

  std::ostream &operator<<(std::ostream &os, const PrefixExpr &it) {
    os << "(PrefixExpr";
    if (it.type) os << " #\"" << *CType::get(it.type) << "\"";
    printMulti(os, it.prefixes);
    return os << " " << *it.expr << ")";
  }

  std::ostream &operator<<(std::ostream &os, const FunCallExpr &it) {
    os << "(FunCallExpr ";
    if (it.type) os << "#\"" << *CType::get(it.type) << "\" ";
    os << *it.function << " " << it.openToken;
    printMulti(os, it.arguments, it.commas);
    return os << " " << it.closeToken << ")";
  }

  std::ostream &operator<<(std::ostream &os, const HintedExpr &it) {
    os << "(HintedExpr ";
    if (it.type) os << "#\"" << *CType::get(it.type) << "\" ";
    return os << *it.expr << " " << it.hint << ")";
  }

  std::ostream &operator<<(std::ostream &os, const FnExpr &it) {
    os << "(FnExpr ";
    if (it.type) os << "#\"" << *CType::get(it.type) << "\" ";
    os << it.fnToken << " ";
    if (it.name) os << *it.name << " ";
    os << it.arguments << " ";
    if (it.typeHint) os << *it.typeHint << " ";
    return os << it.eqToken << " " << *it.body << ")";
  }

  std::ostream &operator<<(std::ostream &os, const LambdaExpr &it) {
    os << "(LambdaExpr";
    if (it.type) os << " #\"" << *CType::get(it.type) << "\"";
    os << " " << it.lambdaToken;
    printMulti(os, it.arguments, it.commas);
    return os << " " << it.dotToken << " " << *it.body << ")";
  }

  std::ostream &operator<<(std::ostream &os, const IfExpr::ElseIf &anIf) {
    os << "(ElseIf " << anIf.elseToken << " " << anIf.ifToken << " " << *anIf.predExpr
       << " " << *anIf.thenExpr << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const IfExpr::Else &anElse) {
    os << "(Else " << anElse.elseToken << " " << *anElse.thenExpr << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const Type &it) {
    PrintingVisitor(os).visitType(const_cast<Type&>(it));
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const PlaceholderType &it) {
    return os << "(PlaceholderType " << it.placeholder << ")";
  }

  std::ostream &operator<<(std::ostream &os, const NamedType &it) {
    os << "(NamedType " << it.raw;
    if (it.parameters) os << " " << *it.parameters;
    return os << ")";
  }

  std::ostream &operator<<(std::ostream &os, const NamedType::TypeParameters &parameters) {
    os << "(TypeParameters " << parameters.openToken << " ";
    printMulti(os, parameters.types, parameters.commas);
    os << " " << parameters.closeToken << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const BlockExpr::Stmt &stmt) {
    os << "(Statement " << *stmt.statement << " " << stmt.delimiter << ")";
    return os;
  }

  std::ostream &operator<<(std::ostream &os, const BinaryExpr::Rhs &rhs) {
    os << rhs.operatorToken << " " << *rhs.expr;
    return os;
  }
}
