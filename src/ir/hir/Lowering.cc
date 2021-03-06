#include "Lowering.h"
#include "Hir.h"
#include "Infer.h"
#include "IdxCounter.h"

#include <limits>

namespace hir::lower {
  using namespace lir;

#define RET_T Insn *
#define ARGS(TYPE) (TYPE &e, BlockList &l, Idx *bb, bool tail)

  class LoweringVisitor :
      public AbstractLoweringVisitor,
      public ExprVisitor<Insn *,
          BlockList &, Idx *, bool> {
  public:
    Program *prog = nullptr;

    LowerResult visitProgram(Program &p) override {
      prog = &p;
      LowerResult ret;
      ret.module.topLevel = visitRootBlock(p.topLevel, false);
      for (auto &ti : p.traitImpls) {
        lir::TraitImpl &trait = ret.module.traitImpls.emplace_back();
        for (auto &m : ti.methods) {
          trait.methods.emplace_back(visitRootBlock(m, true));
        }
      }
      return ret;
    }

    std::map<Expr*, Idx> exprTys;
    std::map<Idx, Idx> defTys;
    BlockList visitRootBlock(Block &block, bool func) {
      Idx counter = 0;
      {
        TyCounter tc;
        exprTys.clear();
        tc.visitBlock(block, exprTys, counter);
      }
      {
        DefCounter dc;
        defTys.clear();
        dc.visitBlock(block, defTys, counter);
      }
      BlockList l;
      l.blockIdx = *block.idx;
      Idx bb = l.push();
      visitBlock(block, l, &bb, true, func);
      return l;
    }

    Insn *voidValue(BlockList &l, Idx *bb) {
      return l[*bb].emplace_back(Insn::NewTuple{{}});
    }

    void setTyAndLoc(Expr &e, Insn *insn) {
      insn->ty = exprTys.at(&e);
      insn->span = e.span;
    }

    RET_T visitExpr ARGS(Expr) override {
      RET_T ret = ExprVisitor::visitExpr(e, l, bb, tail);
      if (ret) setTyAndLoc(e, ret);
      return ret;
    }

    RET_T visitBlock(Block &e, BlockList &l, Idx *bb, bool tail, bool root) {
      if (e.span) {
        Insn *start = l[*bb].emplace_back(Insn::BlockStart{});
        start->span = e.span;
      }
      for (auto &binding : e.bindings) {
        Definition &def = prog->bindings.at(binding);
        Decl &declare = (root ? l.params : l.vars)[binding];
        declare.ty = defTys.at(binding);
        declare.span = def.source;
        declare.name = def.name;
      }
      for (auto &expr : e.body) {
        auto define = dynamic_cast<DefineExpr *>(expr.get());
        if (define) {
          if (dynamic_cast<NewExpr *>(define->value.get())) {
            auto halloc = l[*bb].emplace_back(Insn::HeapAlloc{});
            setTyAndLoc(*define->value, halloc);
            auto setVar = l[*bb].emplace_back<false>(Insn::SetVar{define->idx, halloc});
            setTyAndLoc(*expr, setVar);
          }
        }
      }
      RET_T last;
      for (auto it = e.body.begin();;) {
        Expr &expr = **it;
        bool isLast = ++it == e.body.end();
        last = visitExpr(expr, l, bb, tail && isLast);
        if (isLast) break;
      }
      if (e.span) {
        l[*bb].emplace_back<false>(Insn::BlockEnd{});
      }
      return last;
    }

    RET_T visitBlockExpr ARGS(BlockExpr) override {
      return visitBlock(e.block, l, bb, tail, false);
    }
    RET_T visitVarExpr ARGS(VarExpr) override {
      return l[*bb].emplace_back(Insn::GetVar{e.ref});
    }
    RET_T visitCondExpr ARGS(CondExpr) override {
      Insn *pred = visitExpr(*e.predE, l, bb, false);
      Idx thenB = l.push();
      Idx elseB = l.push();
      l[*bb].end = Jump::CondBr{pred, &l[thenB], &l[elseB]};
      Insn *thenV = visitExpr(*e.thenE, l, &thenB, tail);
      Insn *elseV = visitExpr(*e.elseE, l, &elseB, tail);
      *bb = l.push();
      l[elseB].end = l[thenB].end = Jump::Br{&l[*bb]};
      if (e.pos == Pos::Stmt) {
        return voidValue(l, bb);
      } else {
        return l[*bb].emplace_back(Insn::PhiNode{{
              {thenV, &l[thenB]},
              {elseV, &l[elseB]},
            }});
      }
    }
    RET_T visitVoidExpr ARGS(VoidExpr) override {
      return voidValue(l, bb);
    }
    RET_T visitLiteralExpr ARGS(LiteralExpr) override {
      auto removeUnderscores = [](const std::string &s) {
        std::string value;
        value.reserve(s.size());
        for (auto c : s) {
          if (c != '_') {
            value.push_back(c);
          }
        }
        value.shrink_to_fit();
        return value;
      };
      switch (e.type) {
        case LiteralExpr::Type::Int: {
          return l[*bb].emplace_back(Insn::LiteralInt{removeUnderscores(e.value)});
        }
        case LiteralExpr::Type::Float: {
          return l[*bb].emplace_back(Insn::LiteralFloat{removeUnderscores(e.value)});
        }
        case LiteralExpr::Type::String: {
          std::string value;
          value.reserve(e.value.size());
          for (auto it = e.value.begin() + 1; it != e.value.end() - 1; ++it) {
            if (*it == '\\') {
              ++it;
              switch (*it) {
                case 't':
                  value.push_back('\t');
                  continue;
                case 'v':
                  value.push_back('\v');
                  continue;
                case 'f':
                  value.push_back('\f');
                  continue;
                case 'r':
                  value.push_back('\r');
                  continue;
                case 'n':
                  value.push_back('\n');
                  continue;
                default:
                  break;
              }
            }
            value.push_back(*it);
          }
          value.shrink_to_fit();
          return l[*bb].emplace_back(Insn::LiteralString{std::move(value)});
        }
        default:
          throw 0;
      }
    }
    RET_T visitBoolExpr ARGS(BoolExpr) override {
      return l[*bb].emplace_back(Insn::LiteralBool{e.value});
    }
    RET_T visitBinExpr ARGS(BinExpr) override {
      auto receiver = visitExpr(*e.lhs, l, bb, false);
      std::vector<Insn *> args = {visitExpr(*e.rhs, l, bb, false)};
      Idx trait = exprTys.at(&e);
      return l[*bb].emplace_back(Insn::CallTrait{receiver, args, trait, 0});
    }
    RET_T visitCmpExpr ARGS(CmpExpr) override {
      auto receiver = visitExpr(*e.lhs, l, bb, false);
      std::vector<Insn *> args = {visitExpr(*e.rhs, l, bb, false)};
      Idx trait = exprTys.at(&e);
      return l[*bb].emplace_back(Insn::CallTrait{
        receiver, args, trait,
        e.op <= CmpExpr::Eq ?
        (Idx)e.op :
        (Idx)e.op - (Idx)CmpExpr::Lt
      });
    }
    RET_T visitNegExpr ARGS(NegExpr) override {
      auto receiver = visitExpr(*e.value, l, bb, false);
      Idx trait = exprTys.at(&e);
      return l[*bb].emplace_back(Insn::CallTrait{receiver, {}, trait, 0});
    }
    RET_T visitCallExpr ARGS(CallExpr) override {
      auto receiver = visitExpr(*e.func, l, bb, false);
      std::vector<Insn *> args;
      args.reserve(e.args.size());
      for (auto &expr : e.args) {
        args.push_back(visitExpr(*expr, l, bb, false));
      }
      Idx trait = exprTys.at(&e);
      return l[*bb].emplace_back(Insn::CallTrait{receiver, args, trait, 0});
    }
    RET_T visitDefineExpr ARGS(DefineExpr) override {
      NewExpr *newE = dynamic_cast<NewExpr *>(e.value.get());
      if (newE) {
        Idx i = 0;
        auto getVar = l[*bb].emplace_back(Insn::GetVar{e.idx});
        setTyAndLoc(*e.value, getVar);
        for (auto &v : newE->values) {
          auto value = visitExpr(*v, l, bb, false);
          l[*bb].emplace_back<false>(Insn::SetField{getVar, i++, value});
        }
      } else {
        auto value = visitExpr(*e.value, l, bb, false);
        Insn *setVar = l[*bb].emplace_back<false>(Insn::SetVar{e.idx, value});
        setTyAndLoc(e, setVar);
      }
      return voidValue(l, bb);
    }
    RET_T visitNewExpr ARGS(NewExpr) override {
      std::vector<Insn *> values;
      values.reserve(e.values.size());
      for (auto &v : e.values) {
        values.push_back(visitExpr(*v, l, bb, false));
      }
      auto obj = l[*bb].emplace_back(Insn::HeapAlloc{}); // type will be added
      Idx i = 0;
      for (auto &v : values) {
        l[*bb].emplace_back<false>(Insn::SetField{obj, i++, v});
      }
      l[*bb].end.v = Jump::Ret{obj};
      return obj;
    }
    RET_T visitGetExpr ARGS(GetExpr) override {
      auto obj = visitExpr(*e.value, l, bb, false);
      return l[*bb].emplace_back(Insn::GetField{obj, e.field});
    }
    RET_T visitForeignExpr ARGS(ForeignExpr) override {
      return l[*bb].emplace_back(Insn::ForeignRef{e.name});
    }
    RET_T visitDummyExpr ARGS(DummyExpr) override {
      throw util::ICE("Dummy expression");
    }
  };

  std::unique_ptr<AbstractLoweringVisitor> loweringVisitor() {
    return std::make_unique<LoweringVisitor>();
  }
}
