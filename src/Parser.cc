#include <sstream>
#include "Parser.h"

ast::Tok fsm::Finished<ast::Tok>::rejecting() {
  return ast::Tok::TInvalid;
}

void fsm::Finished<ast::Tok>::merge(ast::Tok &lhs, ast::Tok rhs) {
  lhs = std::max(lhs, rhs);
}

namespace parser {
  ParseError::ParseError(const std::string &message, const lexer::SrcLoc &loc) :
      runtime_error(message),
      loc(loc) {
    std::stringstream ss;
    ss << message << " at " << loc;
    this->message = ss.str();
  }

  const char *ParseError::what() const noexcept {
    return message.c_str();
  }

  class ParserStream {
  public:
    std::deque<Token> lookahead;
    lexer::TokenIter<ast::Tok>::Iter iter;
    lexer::TokenIter<ast::Tok>::Iter end;

    lexer::SrcLoc lastLoc;

    ParserStream(lexer::TokenIter<Tok> tokenStream) :
        iter(tokenStream.begin()),
        end(tokenStream.end()) {}

    bool isEmpty() {
      if (!lookahead.empty() && !canSkip(lookahead.back().type)) {
        return false;
      }
      while (iter != end && canSkip(iter->type)) {
        lookahead.push_back(std::move(*iter));
        ++iter;
      }
      return iter == end || canSkip(iter->type);
    }

    bool canSkip(Tok type) {
      return type == Tok::TWhitespace || type == Tok::TLinebreak;
    }

    std::optional<Token> optional(const std::set<Tok> &types) {
      for (auto it = lookahead.begin(); it != lookahead.end(); ++it) {
        if (types.count(it->type)) {
          lookahead.erase(lookahead.begin(), it);
          Token tok = std::move(lookahead.front());
          lastLoc = tok.loc;
          lastLoc.add(tok.value);
          lookahead.pop_front();
          return tok;
        }
      }
      while (true) {
        if (iter == end) return std::nullopt;
        if (types.count(iter->type)) {
          Token tok = std::move(*iter);
          lastLoc = tok.loc;
          lastLoc.add(tok.value);
          ++iter;
          lookahead.clear();
          return tok;
        }
        if (!canSkip(iter->type)) {
          lastLoc = iter->loc;
          break;
        }
        lookahead.push_back(std::move(*iter));
        ++iter;
      }
      return std::nullopt;
    }

    std::optional<Token> optional(Tok type) {
      return optional((std::set<Tok>) {type});
    }

    Token require(const std::set<Tok> types, const std::string &msg) {
      auto opt = optional(types);
      if (opt) {
        return std::move(*opt);
      }
      throw ParseError(msg, lastLoc);
    }

    Token require(Tok type, const std::string &msg) {
      return require((std::set<Tok>) {type}, msg);
    }
  };

  Identifier parseIdent(ParserStream &stream) {
    return {.ident = stream.require(Tok::TIdent, "Identifier expected")};
  }

  std::unique_ptr<Type> parseType(ParserStream &stream) {
    auto placeholder = stream.optional(Tok::TPlaceholder);
    if (placeholder) {
      PlaceholderType type;
      type.placeholder = *placeholder;
      return std::make_unique<PlaceholderType>(std::move(type));
    } else {
      NamedType type;
      type.raw = parseIdent(stream);
      auto ltToken = stream.optional(Tok::TLt);
      if (ltToken) {
        NamedType::TypeParameters params;
        params.openToken = std::move(*ltToken);
        while (true) {
          auto closeToken = stream.optional(Tok::TGt);
          if (closeToken) {
            params.closeToken = std::move(*closeToken);
            break;
          }
          params.types.push_back(parseType(stream));
          auto comma = stream.optional(Tok::TComma);
          if (!comma) {
            params.closeToken = stream.require(Tok::TGt, "> or , expected");
            break;
          }
          params.commas.push_back(std::move(*comma));
        }
        type.parameters = std::move(params);
      }
      return std::make_unique<NamedType>(std::move(type));
    }
  }

  std::optional<TypeHint> parseTypeHint(ParserStream &stream) {
    auto colon = stream.optional(Tok::TColon);
    if (colon) {
      return (TypeHint) {
          .colon = *colon,
          .type = parseType(stream)
      };
    }
    return std::nullopt;
  }

  std::unique_ptr<Expr> parseExpr(ParserStream &stream);

  std::unique_ptr<Statement> parseStatement(ParserStream &stream);

  RawBinding parseRawBinding(ParserStream &stream) {
    return {
        .name = parseIdent(stream),
        .typeHint = parseTypeHint(stream),
    };
  }

  Binding parseBinding(ParserStream &stream) {
    Binding binding;
    binding.name = parseIdent(stream);
    auto openToken = stream.optional(Tok::TParOpen);
    if (openToken) {
      Binding::Arguments args;
      args.openToken = std::move(*openToken);
      while (true) {
        auto closeToken = stream.optional(Tok::TParClose);
        if (closeToken) {
          args.closeToken = std::move(*closeToken);
          break;
        }
        args.bindings.push_back(parseRawBinding(stream));
        auto comma = stream.optional(Tok::TComma);
        if (!comma) {
          args.closeToken = stream.require(Tok::TParClose, ") or , expected");
          break;
        }
        args.commas.push_back(std::move(*comma));
      }
      binding.arguments = std::move(args);
    }
    binding.typeHint = parseTypeHint(stream);
    binding.eqToken = stream.require(Tok::TEq, "= expected");
    binding.value = parseExpr(stream);
    return binding;
  }

  std::unique_ptr<DelimitedExpr> parseBracketExpr(ParserStream &stream, Token &&openToken) {
    BracketExpr expr;
    expr.openToken = openToken;
    expr.value = parseExpr(stream);
    expr.closeToken = stream.require(Tok::TParClose, ") expected");
    return std::make_unique<BracketExpr>(std::move(expr));
  }

  std::unique_ptr<DelimitedExpr> parseBlockExpr(ParserStream &stream, Token &&openToken) {
    BlockExpr expr;
    expr.openToken = openToken;

    do {
      BlockExpr::Stmt stmt;
      auto defnToken = stream.optional(Tok::TDefn);
      if (defnToken) {
        Defn defn;
        defn.defnToken = std::move(*defnToken);
        defn.binding = parseBinding(stream);
        stmt.statement = std::make_unique<Defn>(std::move(defn));
        goto putstmt;
      }

      {
        std::unique_ptr<Expr> value = parseExpr(stream);

        auto token = stream.optional(Tok::TBrClose);
        if (token) {
          expr.value = std::move(value);
          expr.closeToken = std::move(*token);
          return std::make_unique<BlockExpr>(std::move(expr));
        }

        stmt.statement = std::move(value);
      }

     putstmt:
      stmt.delimiter = stream.require({Tok::TLinebreak, Tok::TComma},
                                      "line break or , or } expected");
      expr.statements.push_back(std::move(stmt));
    } while (true);
  }

  std::unique_ptr<DelimitedExpr> parseBlockExpr(ParserStream &stream) {
    return parseBlockExpr(stream, stream.require(Tok::TBrOpen, "{ expected"));
  }

  std::unique_ptr<DelimitedExpr> parseDelimitedExpr(ParserStream &stream) {
    auto token = stream.optional(Tok::TParOpen);
    if (token) {
      return parseBracketExpr(stream, std::move(*token));
    }
    return parseBlockExpr(stream, stream.require(Tok::TBrOpen, "( or { expected"));
  }

  std::unique_ptr<Expr> parsePrimaryExpr(ParserStream &stream) {
    auto token = stream.optional(
        {
            Tok::TLet,
            Tok::TIf,
            Tok::TIdent,
            Tok::TStr,
            Tok::TInt,
            Tok::TFloat,
            Tok::TTrue,
            Tok::TFalse,
            Tok::TBrOpen,
            Tok::TParOpen,
        });
    if (!token) {
      throw ParseError("Expression expected", stream.lastLoc);
    }
    switch (token->type) {
      case Tok::TLet: {
        LetExpr expr;
        expr.letToken = std::move(*token);
        while (true) {
          auto inToken = stream.optional(Tok::TIn);
          if (inToken) {
            expr.inToken = std::move(*inToken);
            break;
          }
          expr.bindings.push_back(parseBinding(stream));
          auto comma = stream.optional({Tok::TComma, Tok::TLinebreak});
          if (!comma) {
            expr.inToken = stream.require(Tok::TIn, "line break or , or 'in' expected");
            break;
          }
          expr.commas.push_back(std::move(*comma));
        }
        auto identToken = stream.optional(Tok::TIdent);
        if (identToken) {
          expr.name = {.ident = *identToken};
        }
        expr.body = parseDelimitedExpr(stream);
        return std::make_unique<LetExpr>(std::move(expr));
      }
      case Tok::TIf: {
        IfExpr expr;
        expr.ifToken = std::move(*token);
        expr.predExpr = parseExpr(stream);
        expr.thenExpr = parseBlockExpr(stream);
        while ((token = stream.optional(Tok::TElse))) {
          Token elseToken = std::move(*token);
          if ((token = stream.optional(Tok::TIf))) {
            IfExpr::ElseIf elseIf;
            elseIf.elseToken = std::move(elseToken);
            elseIf.ifToken = std::move(*token);
            elseIf.predExpr = parseExpr(stream);
            elseIf.thenExpr = parseBlockExpr(stream);
            expr.elseIfClauses.push_back(std::move(elseIf));
          } else {
            IfExpr::Else elseClause;
            elseClause.elseToken = std::move(elseToken);
            elseClause.thenExpr = parseBlockExpr(stream);
            expr.elseClause = std::move(elseClause);
            break;
          }
        }
        return std::make_unique<IfExpr>(std::move(expr));
      }
      case Tok::TIdent: {
        VarExpr expr;
        expr.name.ident = *token;
        return std::make_unique<VarExpr>(std::move(expr));
      }
      case Tok::TStr:
      case Tok::TInt:
      case Tok::TFloat:
      case Tok::TTrue:
      case Tok::TFalse: {
        LiteralExpr expr;
        expr.value = *token;
        return std::make_unique<LiteralExpr>(expr);
      }
      case Tok::TBrOpen:
        return parseBlockExpr(stream, std::move(*token));
      case Tok::TParOpen: {
        return parseBracketExpr(stream, std::move(*token));
      }
      default:
        throw std::runtime_error("Unexpected token.");
    }
  }

  std::unique_ptr<Expr> parsePostfixExpr(ParserStream &stream) {
    std::unique_ptr<Expr> expr = parsePrimaryExpr(stream);
    auto openToken = stream.optional(Tok::TParOpen);
    if (!openToken) {
      return expr;
    }
    FunCallExpr callExpr;
    callExpr.function = std::move(expr);
    callExpr.openToken = std::move(*openToken);
    while (true) {
      auto closeToken = stream.optional(Tok::TParClose);
      if (closeToken) {
        callExpr.closeToken = std::move(*closeToken);
        break;
      }
      callExpr.arguments.push_back(parseExpr(stream));
      auto comma = stream.optional(Tok::TComma);
      if (!comma) {
        callExpr.closeToken = stream.require(Tok::TParClose, ") or , expected");
        break;
      }
      callExpr.commas.push_back(std::move(*comma));
    }
    return std::make_unique<FunCallExpr>(std::move(callExpr));
  }

  std::unique_ptr<Expr> parsePrefixExpr(ParserStream &stream) {
    std::vector<Token> prefixes;
    std::optional<Token> prefix;
    while ((prefix = stream.optional({Tok::TAdd, Tok::TSub}))) {
      prefixes.push_back(std::move(*prefix));
    }
    if (prefixes.empty()) {
      return parsePostfixExpr(stream);
    } else {
      PrefixExpr expr;
      expr.prefixes = prefixes;
      expr.expr = parsePostfixExpr(stream);
      return std::make_unique<PrefixExpr>(std::move(expr));
    }
  }

  std::unique_ptr<Expr> parseHintedExpr(ParserStream &stream) {
    std::unique_ptr<Expr> expr = parsePrefixExpr(stream);
    std::optional<TypeHint> hint = parseTypeHint(stream);
    if (hint) {
      HintedExpr hinted;
      hinted.expr = std::move(expr);
      hinted.hint = std::move(*hint);
      return std::make_unique<HintedExpr>(std::move(hinted));
    } else {
      return expr;
    }
  }

  const std::vector<std::set<Tok>> binaryOperators{
      {Tok::TAnd2,   Tok::TOr2},
      {Tok::TNe,     Tok::TEq,  Tok::TEq2},
      {Tok::TLt,     Tok::TLe,  Tok::TGt, Tok::TGe},
      {Tok::TOr1,    Tok::TAnd1},
      {Tok::TShLeft, Tok::TShRight},
      {Tok::TAdd,    Tok::TSub},
      {Tok::TMul,    Tok::TDiv, Tok::TRem}
  };

  std::unique_ptr<Expr> parseBinaryExpr(ParserStream &stream, size_t index) {
    if (index >= binaryOperators.size()) {
      return parseHintedExpr(stream);
    }
    std::unique_ptr<Expr> lhs = parseBinaryExpr(stream, index + 1);
    std::optional<Token> opToken = stream.optional(binaryOperators[index]);
    if (!opToken) {
      return lhs;
    }
    BinaryExpr expr;
    expr.lhs = std::move(lhs);
    do {
      expr.terms.push_back((BinaryExpr::Rhs) {
          .operatorToken = *opToken,
          .expr = parseBinaryExpr(stream, index + 1)
      });
      opToken = stream.optional(binaryOperators[index]);
    } while (opToken);
    return std::make_unique<BinaryExpr>(std::move(expr));
  }

  std::unique_ptr<Expr> parseExpr(ParserStream &stream) {
    return parseBinaryExpr(stream, 0);
  }

  std::unique_ptr<Statement> parseStatement(ParserStream &stream) {
    auto defnToken = stream.optional(Tok::TDefn);
    if (defnToken) {
      Defn defn;
      defn.defnToken = std::move(*defnToken);
      defn.binding = parseBinding(stream);
      return std::make_unique<Defn>(std::move(defn));
    } else {
      return parseExpr(stream);
    }
  }

  Program parseProgram(ParserStream &stream) {
    Program program;
    while (!stream.isEmpty()) {
      program.statements.push_back(parseStatement(stream));
    }
    return program;
  }

  Program parseProgram(lexer::TokenIter<Tok> &&tokens) {
    ParserStream stream = ParserStream(tokens);
    return parseProgram(stream);
  }
}
