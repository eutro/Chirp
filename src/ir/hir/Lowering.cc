#include "Lowering.h"
#include "Hir.h"
#include "Infer.h"

namespace hir::lower {
  using namespace lir;

#define RET_T Insn *
#define ARGS(TYPE) (TYPE &e, BlockList &l, Idx *bb, bool tail)
  class LoweringVisitor :
    public AbstractLoweringVisitor,
    public ExprVisitor<Insn *,
                       BlockList&, Idx*, bool> {
  public:
    infer::InferResult &infer;

    std::map<DefIdx, Insn *> vars;

    LoweringVisitor(infer::InferResult &res) : infer(res) {}

    Block *rootBlock = nullptr;

    LowerResult visitProgram(Program &p) override {
      LowerResult ret;
      // TODO visit blocks
      return ret;
    }

    BlockList visitRootBlock(Block &block) {
      rootBlock = &block;
      auto &insnts = infer.insts.at(&block);
      BlockList l;
      Idx bb = l.push();
      visitBlock(block, l, &bb, true);
      return std::move(l);
    }

    Insn *voidValue(BlockList &l, Idx *bb) {
      return l[*bb].emplace_back(Insn::NewTuple{{}});
    }

    void setTy(Expr &e, Insn *insn) {
      insn->ty = infer.insts[rootBlock].exprTypes.at(&e);
    }
    
    RET_T visitExpr ARGS(Expr) override {
      RET_T ret = ExprVisitor::visitExpr(e, l, bb, tail);
      setTy(e, ret);
      return ret;
    }

    RET_T visitBlock ARGS(Block) {
      for (auto &binding : e.bindings) {
        Insn *declare = l[*bb].emplace_back(Insn::DeclareVar{});
        declare->ty = infer.insts[rootBlock].varTypes.at(binding);
        vars[binding] = declare;
      }
      for (auto &expr : e.body) {
        auto define = dynamic_cast<DefineExpr*>(expr.get());
        if (define) {
          if (dynamic_cast<NewExpr*>(define->value.get())) {
            auto halloc = l[*bb].emplace_back(Insn::HeapAlloc{});
            setTy(*define->value, halloc);
            l[*bb].emplace_back(Insn::SetVar{vars.at(define->idx), halloc});
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
      return last;
    }

    RET_T visitBlockExpr ARGS(BlockExpr) override {
      return visitBlock(e.block, l, bb, tail);
    }
    RET_T visitVarExpr ARGS(VarExpr) override {
      return l[*bb].emplace_back(Insn::GetVar{vars.at(e.ref)});
    }
    RET_T visitCondExpr ARGS(CondExpr) override {
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
    RET_T visitVoidExpr ARGS(VoidExpr) override {
      return voidValue(l, bb);
    }
    RET_T visitLiteralExpr ARGS(LiteralExpr) override {
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
      default: throw 0;
      }
    }
    RET_T visitBoolExpr ARGS(BoolExpr) override {
      return l[*bb].emplace_back(Insn::LiteralBool{e.value});
    }
    RET_T visitBinExpr ARGS(BinExpr) override {
      auto receiver = visitExpr(*e.lhs, l, bb, false);
      std::vector<Insn *> args = {visitExpr(*e.rhs, l, bb, false)};
      Idx trait = infer.insts[rootBlock].traitTypes.at(&e);
      return l[*bb].emplace_back(Insn::CallTrait{receiver, args, trait, 0});
    }
    RET_T visitCmpExpr ARGS(CmpExpr) override {
      auto receiver = visitExpr(*e.lhs, l, bb, false);
      std::vector<Insn *> args = {visitExpr(*e.rhs, l, bb, false)};
      Idx trait = infer.insts[rootBlock].traitTypes.at(&e);
      return l[*bb].emplace_back(Insn::CallTrait{receiver, args, trait, 1 + (Idx)e.op});
    }
    RET_T visitNegExpr ARGS(NegExpr) override {
      auto receiver = visitExpr(*e.value, l, bb, false);
      Idx trait = infer.insts[rootBlock].traitTypes.at(&e);
      return l[*bb].emplace_back(Insn::CallTrait{receiver, {}, trait, 0});
    }
    RET_T visitCallExpr ARGS(CallExpr) override {
      auto receiver = visitExpr(*e.func, l, bb, false);
      std::vector<Insn *> args;
      args.reserve(e.args.size());
      for (auto &expr : e.args) {
        args.push_back(visitExpr(*expr, l, bb, false));
      }
      Idx trait = infer.insts[rootBlock].traitTypes.at(&e);
      return l[*bb].emplace_back(Insn::CallTrait{receiver, args, trait, 0});
    }
    RET_T visitDefineExpr ARGS(DefineExpr) override {
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
    RET_T visitNewExpr ARGS(NewExpr) override {
      Idx i = 0;
      auto obj = l[*bb].emplace_back(Insn::HeapAlloc{}); // type will be added
      for (auto &v : e.values) {
        auto value = visitExpr(*v, l, bb, false);
        l[*bb].emplace_back(Insn::SetField{obj, e.variant, i++, value});
      }
      return obj;
    }
    RET_T visitGetExpr ARGS(GetExpr) override {
      auto obj = visitExpr(*e.value, l, bb, false);
      return l[*bb].emplace_back(Insn::GetField{obj, e.variant, e.field});
    }
    RET_T visitForeignExpr ARGS(ForeignExpr) override {
      return l[*bb].emplace_back(Insn::ForeignRef{e.name});
    }
    RET_T visitDummyExpr ARGS(DummyExpr) override {
      throw std::runtime_error("Dummy expression");
    }
  };

  std::unique_ptr<AbstractLoweringVisitor> loweringVisitor(infer::InferResult &inferResult) {
    return std::make_unique<LoweringVisitor>(inferResult);
  }
}
