#include "Builtins.h"
#include "Hir.h"
#include "Infer.h"
#include "../lir/Lir.h"
#include <cstdint>
#include <string>
#include <variant>

namespace hir::lower {
  using namespace lir;

  class LoweringVisitor :
#define RET_T Insn *
#define ARGS , BlockList &l, Idx *bb, bool tail
    public ExprVisitor<Insn *,
                       BlockList, Idx*, bool> {
    infer::InferResult &res;

    std::map<DefIdx, Insn *> vars;

    LoweringVisitor(infer::InferResult &res): res(res) {}

    Block *rootBlock = nullptr;

    BlockList visitRootBlock(Block &block) {
      rootBlock = &block;
      auto &insnts = res.insts.at(&block);
      BlockList l;
      Idx bb = l.push();
      visitBlock(block, l, &bb, true);
      return l;
    }

    Insn *voidValue(BlockList l, Idx *bb) {
      return l[*bb].emplace_back(Insn::NewTuple{{}});
    }

    RET_T visitExpr(Expr &e ARGS) {
      Idx ty = res.insts[rootBlock].exprTypes.at(&e);
      RET_T ret = ExprVisitor::visitExpr(e, l, bb, tail);
      ret->ty = ty;
      return ret;
    }

    RET_T visitBlock(Block &block ARGS) {
      for (auto &binding : block.bindings) {
        Idx ty = res.insts[rootBlock].varTypes.at(binding);
        vars[binding] = l[*bb].emplace_back(Insn::DeclareVar{});
      }
      for (auto &e : block.body) {
        auto define = dynamic_cast<DefineExpr*>(e.get());
        if (define) {
          if (dynamic_cast<NewExpr*>(define->value.get())) {
            auto halloc = l[*bb].emplace_back(Insn::HeapAlloc{});
            l[*bb].emplace_back(Insn::SetVar{{}, halloc});
          }
        }
      }
      RET_T last;
      for (auto it = block.body.begin();;) {
        Expr &e = **it;
        bool isLast = ++it == block.body.end();
        last = visitExpr(e, l, bb, tail && isLast);
        if (isLast) break;
      }
      return last;
    }

    RET_T visitBlockExpr(BlockExpr &e ARGS) {
      return visitBlock(e.block, l, bb, tail);
    }
    RET_T visitVarExpr(VarExpr &e ARGS) {
      return l[*bb].emplace_back(Insn::GetVar{vars.at(e.ref)});
    }
    RET_T visitCondExpr(CondExpr &e ARGS) {
      Insn *pred = visitExpr(*e.predE, l, bb, false);
      Idx thenB = l.push();
      Idx elseB = l.push();
      l[*bb].end = Jump::CondBr{pred, &l[thenB], &l[elseB]};
      Insn *thenV = visitExpr(*e.thenE, l, &thenB, tail);
      Insn *elseV = visitExpr(*e.elseE, l, &elseB, tail);
      if (tail) {
        return nullptr; // ignored
      } else {
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
    }
    RET_T visitVoidExpr(VoidExpr &e ARGS) {
      return voidValue(l, bb);
    }
    RET_T visitLiteralExpr(LiteralExpr &e ARGS) {
      switch (e.type) {
      case LiteralExpr::Int: {
        return l[*bb].emplace_back(Insn::LiteralInt{std::stoull(e.value)});
      }
      case LiteralExpr::Float: {
        return l[*bb].emplace_back(Insn::LiteralFloat{std::stold(e.value)});
      }
      case LiteralExpr::String: {
        std::string value;
        value.reserve(e.value.size());
        for (auto it = e.value.begin(); it != e.value.end(); ++it) {
          if (*it == '\\') {
            ++it;
          }
          value.push_back(*it);
        }
        value.shrink_to_fit();
        return l[*bb].emplace_back(Insn::LiteralString{std::move(value)});
      }
      }
    }
    RET_T visitBoolExpr(BoolExpr &e ARGS) {
      return l[*bb].emplace_back(Insn::LiteralBool{e.value});
    }
    RET_T visitBinExpr(BinExpr &e ARGS) {
      auto receiver = visitExpr(*e.lhs, l, bb, false);
      std::vector<Insn *> args = {visitExpr(*e.rhs, l, bb, false)};
      Idx trait = res.insts[rootBlock].traitTypes.at(&e);
      return l[*bb].emplace_back(Insn::CallTrait{receiver, args, trait, 0});
    }
    RET_T visitCmpExpr(CmpExpr &e ARGS) {
      auto receiver = visitExpr(*e.lhs, l, bb, false);
      std::vector<Insn *> args = {visitExpr(*e.rhs, l, bb, false)};
      Idx trait = res.insts[rootBlock].traitTypes.at(&e);
      return l[*bb].emplace_back(Insn::CallTrait{receiver, args, trait, 1 + (Idx)e.op});
    }
    RET_T visitNegExpr(NegExpr &e ARGS) {
      auto receiver = visitExpr(*e.value, l, bb, false);
      Idx trait = res.insts[rootBlock].traitTypes.at(&e);
      return l[*bb].emplace_back(Insn::CallTrait{receiver, {}, trait, 0});
    }
    RET_T visitCallExpr(CallExpr &e ARGS) {
      auto receiver = visitExpr(*e.func, l, bb, false);
      std::vector<Insn *> args;
      args.reserve(e.args.size());
      for (auto &e : e.args) {
        args.push_back(visitExpr(*e, l, bb, false));
      }
      Idx trait = res.insts[rootBlock].traitTypes.at(&e);
      return l[*bb].emplace_back(Insn::CallTrait{receiver, args, trait, 0});
    }
    RET_T visitDefineExpr(DefineExpr &e ARGS) {
      NewExpr *newE = dynamic_cast<NewExpr *>(e.value.get());
      if (newE) {
        Idx i = 0;
        for (auto &v : newE->values) {
          auto value = visitExpr(*v, l, bb, false);
          auto getVar = l[*bb].emplace_back(Insn::GetVar{vars.at(e.idx)});
          l[*bb].emplace_back(Insn::SetField{getVar, newE->variant, i++, value});
        }
      } else {
        auto value = visitExpr(*e.value, l, bb, false);
        l[*bb].emplace_back(Insn::SetVar{vars.at(e.idx), value});
      }
      return voidValue(l, bb);
    }
    RET_T visitNewExpr(NewExpr &e ARGS) {
      throw std::runtime_error("New must be in a define");
    }
    RET_T visitGetExpr(GetExpr &e ARGS) {
      auto obj = visitExpr(*e.value, l, bb, false);
      return l[*bb].emplace_back(Insn::GetField{obj}, e.variant, e.field);
    }
    RET_T visitForeignExpr(ForeignExpr &e ARGS) {
      return l[*bb].emplace_back(Insn::ForeignRef{e.name});
    }
    RET_T visitDummyExpr(DummyExpr &e ARGS) {
      throw std::runtime_error("Dummy expression");
    }
  };
}
