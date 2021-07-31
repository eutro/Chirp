#include <sstream>
#include "Lexer.h"
#include "Parser.h"

ast::Tok fsm::Finished<ast::Tok>::rejecting() {
  return ast::Tok::TInvalid;
}

void fsm::Finished<ast::Tok>::merge(ast::Tok &lhs, ast::Tok rhs) {
  lhs = std::max(lhs, rhs);
}

namespace parser {
  loc::SrcLoc tokEnd(const Token &tok) {
    loc::SrcLoc loc = tok.loc;
    loc.add(tok.value);
    return loc;
  }

  class ParseError {
  public:
    std::string message;
    lexer::SrcLoc loc;

    ParseError(const std::string &message, const lexer::SrcLoc &loc) :
        message(message),
        loc(loc) {
    }
  };

  class ParserStream;

  ParseError parseError(const std::string &message, ParserStream &stream);

  class ParserStream {
  private:
    std::deque<Token> lookahead;
    lexer::TokenIter<ast::Tok>::Iter iter;
    lexer::TokenIter<ast::Tok>::Iter end;

  public:
    /**
     * The position after the last yielded token.
     */
    lexer::SrcLoc lastLoc;

    ParserStream(lexer::TokenIter<Tok> &stream) :
        iter(stream.begin()),
        end(stream.end()) {}

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
      return Tok::TWhitespace <= type && type <= Tok::TLinebreak;
    }

    template <class Pred>
    std::optional<Token> optional(const Pred &pred, bool consume) {
      for (auto it = lookahead.begin(); it != lookahead.end(); ++it) {
        if (pred(it->type)) {
          lookahead.erase(lookahead.begin(), it);
          if (consume) {
            Token tok = std::move(lookahead.front());
            lookahead.pop_front();
            lastLoc = tokEnd(tok);
            return tok;
          } else {
            return lookahead.front();
          }
        }
      }
      while (true) {
        if (iter == end) return std::nullopt;
        if (pred(iter->type)) {
          if (consume) {
            Token tok = std::move(*iter);
            ++iter;
            lastLoc = tokEnd(tok);
            lookahead.clear();
            return tok;
          } else {
            return *iter;
          }
        }
        if (!canSkip(iter->type)) {
          break;
        }
        lookahead.push_back(std::move(*iter));
        ++iter;
      }
      return std::nullopt;
    }

    struct SetPred {
      const std::set<Tok> &types;
      SetPred(const std::set<Tok> &types) : types(types) {}
      bool operator()(const Tok &type) const {
        return types.count(type);
      }
    };

    std::optional<Token> optional(const std::set<Tok> &types) {
      SetPred pred(types);
      return optional(pred, true);
    }

    struct SinglePred {
      Tok type;
      SinglePred(Tok type) : type(type) {}
      bool operator()(const Tok &oType) const {
        return type == oType;
      }
    };

    std::optional<Token> peekFor(Tok type) {
      SinglePred pred(type);
      return optional(pred, false);
    }

    std::optional<Token> optional(Tok type) {
      SinglePred pred(type);
      return optional(pred, true);
    }
    
    struct AlwaysPred {
      bool operator()(const Tok &oType) const {
        return true;
      }
    };

    std::optional<Token> skipUntil(const std::set<Tok> types) {
      while (true) {
        std::optional<Token> tok = optional(AlwaysPred(), true);
        if (tok) {
          if (types.count(tok->type)) {
            return tok;
          }
        } else {
          return std::nullopt;
        }
      }
    }

    Token require(const std::set<Tok> types, const std::string &msg) {
      auto opt = optional(types);
      if (opt) {
        return std::move(*opt);
      }
      throw parseError(msg, *this);
    }

    Token require(Tok type, const std::string &msg) {
      return require((std::set<Tok>) {type}, msg);
    }
  };

  ParseError parseError(const std::string &message, ParserStream &stream) {
    return ParseError(message, stream.lastLoc);
  }

  Identifier parseIdent(Token &&token) {
    return {.ident = token};
  }

  Identifier parseIdent(ParserStream &stream) {
    return parseIdent(stream.require(Tok::TIdent, "Identifier expected"));
  }

  std::optional<Identifier> maybeParseIdent(ParserStream &stream) {
    auto token = stream.optional(Tok::TIdent);
    if (token) {
      return parseIdent(std::move(*token));
    }
    return std::nullopt;
  }

  std::unique_ptr<Type> parseType(ParserStream &stream) {
    auto placeholder = stream.optional(Tok::TPlaceholder);
    if (placeholder) {
      PlaceholderType type;
      type.placeholder = std::move(*placeholder);
      type.span.lo = type.placeholder.loc;
      type.span.hi = tokEnd(type.placeholder);
      return std::make_unique<PlaceholderType>(std::move(type));
    } else {
      NamedType type;
      type.raw = parseIdent(stream.require({Tok::TFn, Tok::TIdent}, "Type name expected"));
      type.span.lo = type.raw.ident.loc;
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
        type.span.hi = tokEnd(params.closeToken);
        type.parameters = std::move(params);
      } else {
        type.span.hi = tokEnd(type.raw.ident);
      }
      return std::make_unique<NamedType>(std::move(type));
    }
  }

  std::optional<TypeHint> parseTypeHint(ParserStream &stream) {
    auto colon = stream.optional(Tok::TColon);
    if (colon) {
      TypeHint hint;
      hint.colon = std::move(*colon);
      hint.type = parseType(stream);
      return std::move(hint);
    }
    return std::nullopt;
  }

  std::unique_ptr<Expr> parseExpr(ParserStream &stream);

  std::unique_ptr<Statement> parseStatement(ParserStream &stream);

  RawBinding parseRawBinding(ParserStream &stream) {
    RawBinding rb;
    rb.name = parseIdent(stream);
    rb.typeHint = parseTypeHint(stream);
    return rb;
  }

  Binding::Arguments parseArgs(Token &&openToken, ParserStream &stream) {
    Binding::Arguments args;
    if (openToken.type == Tok::TLt) {
      Binding::TypeArguments typeArgs;
      typeArgs.openToken = std::move(openToken);
      while (true) {
        auto closeToken = stream.optional(Tok::TGt);
        if (closeToken) {
          typeArgs.closeToken = std::move(*closeToken);
          break;
        }
        typeArgs.idents.push_back(parseIdent(stream));
        auto comma = stream.optional(Tok::TComma);
        if (!comma) {
          typeArgs.closeToken = stream.require(Tok::TGt, "> or , expected");
          break;
        }
        typeArgs.commas.push_back(std::move(*comma));
      }
      args.typeArguments = std::move(typeArgs);
      openToken = stream.require(Tok::TParOpen, "( expected");
    }
    args.openToken = std::move(openToken);
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
    return args;
  }

  Binding parseBinding(ParserStream &stream) {
    Binding binding;
    binding.name = parseIdent(stream);
    binding.span.lo = binding.name.ident.loc;
    auto openToken = stream.optional({Tok::TParOpen, Tok::TLt});
    if (openToken) {
      binding.arguments = parseArgs(std::move(*openToken), stream);
    }
    binding.typeHint = parseTypeHint(stream);
    binding.eqToken = stream.require(Tok::TEq, "= expected");
    auto foreign = stream.optional(Tok::TForeign);
    if (foreign) {
      binding.foreignToken = std::move(foreign);
      binding.span.hi = tokEnd(*binding.foreignToken);
    } else {
      binding.value = parseExpr(stream);
      binding.span.hi = binding.value->span.hi;
    }
    return binding;
  }

  std::unique_ptr<DelimitedExpr> parseBracketExpr(ParserStream &stream, Token &&openToken) {
    BracketExpr expr;
    expr.openToken = std::move(openToken);
    expr.span.lo = expr.openToken.loc;
    expr.value = parseExpr(stream);
    expr.closeToken = stream.require(Tok::TParClose, ") expected");
    expr.span.hi = tokEnd(expr.closeToken);
    return std::make_unique<BracketExpr>(std::move(expr));
  }

  std::unique_ptr<DelimitedExpr> parseColonExpr(ParserStream &stream, Token &&colonToken) {
    ColonExpr expr;
    expr.colonToken = std::move(colonToken);
    expr.span.lo = expr.colonToken.loc;
    expr.value = parseExpr(stream);
    expr.span.hi = expr.value->span.hi;
    return std::make_unique<ColonExpr>(std::move(expr));
  }

  std::unique_ptr<Statement> parseDefn(ParserStream &stream, const std::optional<Token> &defnToken) {
    Defn defn;
    defn.defnToken = std::move(*defnToken);
    defn.span.lo = defn.defnToken.loc;
    defn.binding = parseBinding(stream);
    defn.span.hi = defn.binding.span.hi;
    return std::make_unique<Defn>(std::move(defn));
  }

  std::unique_ptr<DelimitedExpr> parseBlockExpr(ParserStream &stream, Token &&openToken) {
    BlockExpr expr;
    expr.openToken = openToken;
    expr.span.lo = expr.openToken.loc;
    auto close = stream.optional(Tok::TBrClose);
    if (close) {
      expr.value = nullptr;
      expr.closeToken = std::move(*close);
      expr.span.hi = tokEnd(expr.closeToken);
      return std::make_unique<BlockExpr>(std::move(expr));
    }

    do {
      BlockExpr::Stmt stmt;
      auto defnToken = stream.optional(Tok::TDefn);
      if (defnToken) {
        stmt.statement = parseDefn(stream, defnToken);
        goto putstmt;
      }

      {
        std::unique_ptr<Expr> value = parseExpr(stream);

        auto token = stream.optional(Tok::TBrClose);
        if (token) {
          expr.value = std::move(value);
          expr.closeToken = std::move(*token);
          expr.span.hi = tokEnd(expr.closeToken);
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
    auto openToken = stream.optional(Tok::TParOpen);
    if (openToken) {
      return parseBracketExpr(stream, std::move(*openToken));
    }
    auto colonToken = stream.optional(Tok::TColon);
    if (colonToken) {
      return parseColonExpr(stream, std::move(*colonToken));
    }
    return parseBlockExpr(stream, stream.require(Tok::TBrOpen, ": or ( or { expected"));
  }

  std::unique_ptr<Expr> parsePrimaryExpr(ParserStream &stream) {
    auto token = stream.optional(
        {
            Tok::TLet,
            Tok::TIf,
            Tok::TFn,
            Tok::TBackslash,
            Tok::TIdent,
            Tok::TStr,
            Tok::TInt,
            Tok::TFloat,
            Tok::TTrue,
            Tok::TFalse,
            Tok::TBrOpen,
            Tok::TParOpen,
            Tok::TColon,
        });
    if (!token) {
      throw parseError("Expression expected", stream);
    }
    switch (token->type) {
      case Tok::TLet: {
        LetExpr expr;
        expr.letToken = std::move(*token);
        expr.span.lo = expr.letToken.loc;
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
        expr.name = maybeParseIdent(stream);
        expr.body = parseDelimitedExpr(stream);
        expr.span.hi = expr.body->span.hi;
        return std::make_unique<LetExpr>(std::move(expr));
      }
      case Tok::TIf: {
        IfExpr expr;
        expr.ifToken = std::move(*token);
        expr.predExpr = parseExpr(stream);
        expr.thenExpr = parseBlockExpr(stream);
        expr.span.hi = expr.thenExpr->span.hi;
        while ((token = stream.optional(Tok::TElse))) {
          Token elseToken = std::move(*token);
          if ((token = stream.optional(Tok::TIf))) {
            IfExpr::ElseIf elseIf;
            elseIf.elseToken = std::move(elseToken);
            elseIf.ifToken = std::move(*token);
            elseIf.predExpr = parseExpr(stream);
            elseIf.thenExpr = parseBlockExpr(stream);
            expr.span.hi = elseIf.thenExpr->span.hi;
            expr.elseIfClauses.push_back(std::move(elseIf));
          } else {
            IfExpr::Else elseClause;
            elseClause.elseToken = std::move(elseToken);
            elseClause.thenExpr = parseBlockExpr(stream);
            expr.span.hi = elseClause.thenExpr->span.hi;
            expr.elseClause = std::move(elseClause);
            break;
          }
        }
        return std::make_unique<IfExpr>(std::move(expr));
      }
      case Tok::TFn: {
        FnExpr expr;
        expr.fnToken = std::move(*token);
        expr.span.lo = expr.fnToken.loc;
        expr.name = maybeParseIdent(stream);
        expr.arguments = parseArgs(stream.require({Tok::TParOpen, Tok::TLt}, "( or < expected"), stream);
        expr.typeHint = parseTypeHint(stream);
        expr.eqToken = stream.require(Tok::TEq, "= expected");
        expr.body = parseExpr(stream);
        expr.span.hi = expr.body->span.hi;
        return std::make_unique<FnExpr>(std::move(expr));
      }
      case Tok::TBackslash: {
        LambdaExpr expr;
        expr.lambdaToken = std::move(*token);
        expr.span.lo = expr.lambdaToken.loc;
        auto ident = stream.optional(Tok::TIdent);
        if (ident) {
          Token arg = std::move(*ident);
          while (true) {
            RawBinding rb;
            rb.name = parseIdent(std::move(arg));
            expr.arguments.push_back(std::move(rb));
            auto comma = stream.optional(Tok::TComma);
            if (comma) {
              expr.commas.push_back(std::move(*comma));
              arg = stream.require(Tok::TIdent, "identifier expected");
            } else {
              break;
            }
          }
        }
        expr.dotToken = stream.require(Tok::TDot, ". expected");
        expr.body = parseExpr(stream);
        expr.span.hi = expr.body->span.hi;
        return std::make_unique<LambdaExpr>(std::move(expr));
      }
      case Tok::TIdent: {
        VarExpr expr;
        expr.name = parseIdent(std::move(*token));
        expr.span.lo = expr.name.ident.loc;
        expr.span.hi = tokEnd(expr.name.ident);
        return std::make_unique<VarExpr>(std::move(expr));
      }
      case Tok::TStr:
      case Tok::TInt:
      case Tok::TFloat:
      case Tok::TTrue:
      case Tok::TFalse: {
        LiteralExpr expr;
        expr.value = std::move(*token);
        expr.span.lo = expr.value.loc;
        expr.span.hi = tokEnd(expr.value);
        return std::make_unique<LiteralExpr>(expr);
      }
      case Tok::TBrOpen:
        return parseBlockExpr(stream, std::move(*token));
      case Tok::TParOpen: {
        return parseBracketExpr(stream, std::move(*token));
      }
      case Tok::TColon: {
        return parseColonExpr(stream, std::move(*token));
      }
      default:
        throw parseError("Unexpected token", stream);
    }
  }

  std::unique_ptr<Expr> parsePostfixExpr(ParserStream &stream) {
    std::unique_ptr<Expr> expr = parsePrimaryExpr(stream);
    if (stream.peekFor(Tok::TLinebreak)) {
      return std::move(expr);
    }
    auto openToken = stream.optional(Tok::TParOpen);
    while (openToken) {
      FunCallExpr callExpr;
      callExpr.function = std::move(expr);
      callExpr.span.lo = callExpr.function->span.lo;
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
      callExpr.span.hi = tokEnd(callExpr.closeToken);
      expr = std::make_unique<FunCallExpr>(std::move(callExpr));
      if (stream.peekFor(Tok::TLinebreak)) {
        return std::move(expr);
      }
      openToken = stream.optional(Tok::TParOpen);
    }
    return expr;
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
      expr.prefixes = std::move(prefixes);
      expr.span.lo = expr.prefixes.front().loc;
      expr.expr = parsePostfixExpr(stream);
      expr.span.hi = expr.expr->span.hi;
      return std::make_unique<PrefixExpr>(std::move(expr));
    }
  }

  std::unique_ptr<Expr> parseHintedExpr(ParserStream &stream) {
    std::unique_ptr<Expr> expr = parsePrefixExpr(stream);
    std::optional<TypeHint> hint = parseTypeHint(stream);
    if (hint) {
      HintedExpr hinted;
      hinted.expr = std::move(expr);
      hinted.span.lo = hinted.expr->span.lo;
      hinted.hint = std::move(*hint);
      hinted.span.hi = hinted.hint.type->span.hi;
      return std::make_unique<HintedExpr>(std::move(hinted));
    } else {
      return expr;
    }
  }

  const std::vector<std::set<Tok>> binaryOperators{
      {Tok::TAnd2,   Tok::TOr2},
      {Tok::TNe,     Tok::TEq,       Tok::TEq2},
      {Tok::TLt,     Tok::TLe,       Tok::TGt, Tok::TGe},
      {Tok::TOr1,    Tok::TAnd1},
      {Tok::TAdd,    Tok::TSub},
      {Tok::TMul,    Tok::TDiv,      Tok::TRem}
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
    expr.precedence = index;
    expr.lhs = std::move(lhs);
    expr.span.lo = expr.lhs->span.lo;
    do {
      expr.terms.push_back((BinaryExpr::Rhs) {
          .operatorToken = *opToken,
          .expr = parseBinaryExpr(stream, index + 1)
      });
      opToken = stream.optional(binaryOperators[index]);
    } while (opToken);
    expr.span.hi = expr.terms.back().expr->span.hi;
    return std::make_unique<BinaryExpr>(std::move(expr));
  }

  std::unique_ptr<Expr> parseExpr(ParserStream &stream) {
    return parseBinaryExpr(stream, 0);
  }

  std::unique_ptr<Statement> parseStatement(ParserStream &stream) {
    auto defnToken = stream.optional(Tok::TDefn);
    if (defnToken) {
      return parseDefn(stream, defnToken);
    } else {
      return parseExpr(stream);
    }
  }

  Program parseProgram(err::ErrorContext &ctx, lexer::TokenIter<Tok> &tokens) {
    tokens.stream.setYieldLines();
    ParserStream stream = ParserStream(tokens);
    Program program;
    while (!stream.isEmpty()) {
      try {
        program.statements.push_back(parseStatement(stream));
        if (!stream.isEmpty()) {
          program.delimiters.push_back(stream.require({Tok::TLinebreak, Tok::TComma},
                                                      "line break or , or } expected"));
        }
      } catch (ParseError &err) {
        ctx.err().pos(err.loc, err.message);

        if (!stream.isEmpty()) {
          std::optional<Token> delim = stream.skipUntil({Tok::TLinebreak, Tok::TComma});
          if (delim) {
            program.delimiters.push_back(std::move(*delim));
          } else {
            break;
          }
        }
      }
    }
    return program;
  }
}
