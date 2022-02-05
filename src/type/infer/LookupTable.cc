#include "LookupTable.h"

#include "../../common/Logging.h"
#include "VM.h"

#include <memory>
#include <map>
#include <utility>
#include <variant>
#include <ostream>
#include <deque>

namespace type::infer {
  struct WrapFnPtr {
    Fn *impl;
    WrapFnPtr(Fn *impl) : impl(impl) {}
    std::vector<Tp> operator()(const std::vector<Tp> &tys, const std::vector<Constant> &cs) const {
      return (*impl)(tys, cs);
    }
  };
  static_assert(std::is_assignable_v<Fn, WrapFnPtr>);
  
  struct Ctor {
    std::optional<Idx> idx;
    std::vector<Idx> keys;
    bool operator<(const Ctor &rhs) const {
      return std::tie(idx, keys) < std::tie(rhs.idx, rhs.keys);
    }
    bool operator==(const Ctor &rhs) const {
      return std::tie(idx, keys) == std::tie(rhs.idx, rhs.keys);
    }
    bool operator!=(const Ctor &rhs) const {
      return !(rhs == *this);
    }

    friend std::ostream &operator<<(std::ostream &os, const Ctor &ctor) {
      os << "c_{";
      if (ctor.idx) {
        os << *ctor.idx;
      } else {
        os << "v";
      }
      for (Idx k : ctor.keys) {
        os << "," << k;
      }
      os << "}";
      return os;
    }

    using TyV = decltype(Ty::v);
    Ctor() = default;
    Ctor(std::optional<Idx> idx, std::vector<Idx> keys) :
        idx(idx), keys(std::move(keys)) {}
    Ctor(Tp ty): idx{} {
      using TT = std::tuple<decltype(idx), decltype(keys)>;
      std::tie(idx, keys) = std::visit(
          overloaded{
              [](const Ty::Err &t) -> TT { return {util::index_of_type_v<Ty::Err, TyV>, {}}; },
              [](const Ty::Bool &t) -> TT { return {util::index_of_type_v<Ty::Bool, TyV>, {}}; },
              [](const Ty::Int &t) -> TT { return {util::index_of_type_v<Ty::Int, TyV>, {(Idx) t.s}}; },
              [](const Ty::UInt &t) -> TT { return {util::index_of_type_v<Ty::UInt, TyV>, {(Idx) t.s}}; },
              [](const Ty::Float &t) -> TT { return {util::index_of_type_v<Ty::Float, TyV>, {(Idx) t.s}}; },
              [](const Ty::Placeholder &t) -> TT { throw util::ICE("Wildcard does not have a constructor"); },
              [](const Ty::ADT &t) -> TT { return {util::index_of_type_v<Ty::ADT, TyV>, {t.i, (Idx) t.s.size()}}; },
              [](const Ty::Union &t) -> TT { return {util::index_of_type_v<Ty::Union, TyV>, {(Idx) t.tys.size()}}; },
              [](const Ty::Tuple &t) -> TT { return {util::index_of_type_v<Ty::Tuple, TyV>, {(Idx) t.t.size()}}; },
              [](const Ty::TypeToken &t) -> TT { return {util::index_of_type_v<Ty::TypeToken, TyV>, {}}; },
              [](const Ty::String &t) -> TT { return {util::index_of_type_v<Ty::String, TyV>, {(Idx) t.nul}}; },
              [](const Ty::Cyclic &t) -> TT { throw util::ICE("Cyclic pattern matching unsupported"); },
              [](const Ty::CyclicRef &t) -> TT { throw util::ICE("Cyclic pattern matching unsupported"); },
              [](const Ty::Undetermined &t) -> TT { throw util::ICE("Undetermined pattern matching unsupported"); },
              [](const Ty::FfiFn &t) -> TT { return {util::index_of_type_v<Ty::FfiFn, TyV>, {}}; },
          },
          ty->v);
    }
  };
  
  std::vector<Tp> childrenOf(Tp ty) {
    return std::visit(overloaded {
        [=](const Ty::FfiFn &t) -> std::vector<Tp> { return {t.args, t.ret}; },
        [=](const Ty::Tuple &t) { return t.t; },
        [=](const Ty::ADT &t) { return t.s; },
        [=](const auto &) -> std::vector<Tp> { return {}; }
    }, ty->v);
  }

  struct Pattern {
    struct CtorPat {
      Ctor ctor;
      std::vector<std::shared_ptr<Pattern>> ps;
      bool operator<(const CtorPat &rhs) const {
        return std::tie(ctor, ps) < std::tie(rhs.ctor, rhs.ps);
      }

      friend std::ostream &operator<<(std::ostream &os, const CtorPat &pat) {
        os << pat.ctor << "(";
        for (auto it = pat.ps.begin(); it != pat.ps.end();) {
          os << **it;
          if (++it != pat.ps.end()) os << ", ";
        }
        os << ")";
        return os;
      }
    };
    struct Wildcard {
      bool operator<(const Wildcard &o) const { return false; }

      friend std::ostream &operator<<(std::ostream &os, const Wildcard &wildcard) {
        os << "\\_";
        return os;
      }
    };
    std::variant<CtorPat, Wildcard> v;
    using VT = decltype(Pattern::v);
    bool operator<(const Pattern &o) const { return v < o.v; }

    friend std::ostream &operator<<(std::ostream &os, const Pattern &pattern) {
      std::visit([&os](const auto &x) { os << x; }, pattern.v);
      return os;
    }

    Pattern(): v(Wildcard{}) {}
    Pattern(Tp ty) {
      if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
        v = Wildcard{};
      } else {
        v = CtorPat{ty, manyPatterns(childrenOf(ty))}; 
      }
    }
    template <typename... Arg>
    Pattern(Arg &&...arg): v(std::forward<Arg>(arg)...) {}

    template <typename Iterable>
    static void addPatterns(std::vector<std::shared_ptr<Pattern>> &ret, const Iterable &i) {
      for (const auto &v : i) {
        ret.push_back(std::make_unique<Pattern>(v));
      }
    }

    template <typename ...Args>
    static std::vector<std::shared_ptr<Pattern>> manyPatterns(const Args &...ps) {
      std::vector<std::shared_ptr<Pattern>> ret;
      ret.reserve((ps.size() + ...));
      (addPatterns(ret, ps), ...);
      return ret;
    }
    [[nodiscard]] bool isWildcard() const {
      return std::holds_alternative<Wildcard>(v);
    }
  };

  struct Matrix {
    struct Row {
      std::vector<std::shared_ptr<Pattern>> patterns;
      Fn *val;
      Row(): val{} {}
      Row(std::vector<std::shared_ptr<Pattern>> patterns, Fn *val) :
          patterns(std::move(patterns)),
          val(val) {}
      bool operator<(const Row &rhs) const {
        return std::tie(patterns, val) < std::tie(rhs.patterns, rhs.val);
      }
    };

    friend std::ostream &operator<<(std::ostream &os, const Matrix &matrix) {
      os << R"(\[\left[\begin{array}{)";
      if (!matrix.rows.empty()) {
        Idx w = matrix.rows.front().patterns.size();
        os << std::string(w, 'c') << "|c";
      }
      os << "}\n";
      for (auto it = matrix.rows.begin(); it != matrix.rows.end();) {
        const Row &r = *it;
        for (const auto &p : r.patterns) {
          os << *p << " & ";
        }
        os << "\\tt{" << r.val << "}";
        if (++it != matrix.rows.end()) {
          os << R"( \\)";
        }
        os << "\n";
      }
      os << R"(\end{array}\right]\])";
      return os;
    }
    std::vector<Row> rows;
  };

  struct DTree {
    struct Leaf { Fn *value; };
    struct Fail {};
    struct Switch {
      std::map<Ctor, DTree> mapping;
      std::unique_ptr<DTree> fallback;
      Idx col{};
    };
    std::variant<Leaf, Fail, Switch> v;
    DTree(Matrix &m);
    template <typename... Arg>
    DTree(Arg &&...arg): v(std::forward<Arg>(arg)...) {}

    friend std::ostream &operator<<(std::ostream &os, const DTree &tree) {
      std::visit(overloaded {
          [&](const Leaf &l) { os << "Leaf(" << l.value << ")"; },
          [&](const Fail &) { os << "Fail"; },
          [&](const Switch &s) {
            os << "Switch[" << s.col << "]" << "{";
            for (auto it = s.mapping.begin(); it != s.mapping.end();) {
              os << it->first << ": " << it->second;
              if (++it != s.mapping.end()) os << ", ";
              else if (s.fallback) {
                os << ", _: " << *s.fallback;
              }
            }
            os << "}";
          },
      }, tree.v);
      return os;
    }

  private:
    struct StackVal {
      Tp ty;
      std::optional<Idx> splittableUnion = std::nullopt;
      StackVal(Tp ty) : ty(ty) {}
    };
    
    std::optional<Fn> decideStack(DTree *tree, std::deque<StackVal> &stack) {
      while (true) {
        auto &v = tree->v;
        if (std::holds_alternative<Leaf>(v)) {
          return WrapFnPtr(std::get<Leaf>(v).value);
        } else if (std::holds_alternative<Switch>(v)) {
          auto &s = std::get<Switch>(v);
          if (s.col != stack.size() - 1) {
            std::swap(stack[s.col], stack[stack.size() - 1]);
          }
          StackVal stackVal = stack.back();
          stack.pop_back();
          Tp ty = type::uncycle(stackVal.ty);
          if (stackVal.splittableUnion && std::holds_alternative<Ty::Union>(ty->v)) {
            Idx splitIdx = *stackVal.splittableUnion;
            return [splitIdx, this](const std::vector<Tp> &args, const std::vector<Constant> &constArgs) -> std::vector<Tp> {
              std::optional<Fn> unionDispatch = ENV->table->lookupFn(LookupKey::intern("union-dispatch"), {}, {});
              std::function<type::infer::Fn(const std::vector<Tp> &)> recursiveLookup = [this](const std::vector<Tp> &cArgs) {
                std::optional<Fn> decided = this->decide(cArgs);
                if (!decided) {
                  throw err::LocationError("Undefined function");
                }
                return *decided;
              };
              std::vector<Constant> callArgs{constArgs.front(), splitIdx, &recursiveLookup};
              return (*unionDispatch)(args, callArgs);
            };
          } else {
            Ctor c(ty);
            auto found = s.mapping.find(c);
            if (found == s.mapping.end()) {
              tree = s.fallback.get();
            } else {
              std::vector<Tp> children = childrenOf(ty);
              stack.insert(stack.end(), children.begin(), children.end());
              tree = &found->second;
            }
          }
        } else {
          return std::nullopt;
        }
      }
    }

  public:
    std::optional<Fn> decide(const std::vector<Tp> &tys) {
      if (std::holds_alternative<Leaf>(v)) {
        return WrapFnPtr(std::get<Leaf>(v).value);
      } else if (std::holds_alternative<Switch>(v)) {
        auto &s = std::get<Switch>(v);
        Ctor arity{std::nullopt, {(Idx)tys.size()}};
        auto found = s.mapping.find(arity);
        if (found != s.mapping.end()) {
          std::deque<StackVal> stack(tys.begin(), tys.end());
          Idx i = 0;
          for (auto &item : stack) item.splittableUnion = i++;
          return decideStack(&found->second, stack);
        }
      }
      return std::nullopt;
    }
  };

  DTree::DTree(Matrix &m) {
    logging::CHIRP.debug("Building tree from:\n", m, "\n");
    // based on Compiling Pattern Matching to good Decision Trees by Luc Maranget <3
    // https://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf
    // comments below make reference to this paper

    // we differ slightly from ML pattern matching in that the cases are NOT ordered,
    // this will be emulated by considering that wildcard patterns are always at the bottom
    // and thus no rows are "useless"/"redundant", this is helpful for the heuristic as well;
    // furthermore step 2 of the compilation scheme described in the paper:
    // "If the first row of P exists and is made of wildcards, then
    // matching always succeeds and yields the first action."
    // is augmented by the condition that there must be only one action at this point,
    // otherwise there is an error (overload resolution being ambiguous)

    // step 1
    if (m.rows.empty()) {
      logging::CHIRP.debug("Empty matrix: built Fail\n\n");
      v = Fail();
      return;
    }

    // step 2
    Idx rows = m.rows.size();
    Idx cols = m.rows.front().patterns.size();
    {
      for (auto &p : m.rows.front().patterns) {
        if (!p->isWildcard()) {
          goto endStep2;
        }
      }
      if (rows != 1) {
        throw err::LocationError("Overload resolution is ambiguous");
      }
      logging::CHIRP.debug("Wildcard matrix: built Leaf\n\n");
      v = Leaf{m.rows.front().val};
      return;
      endStep2:;
    }

    // step 3
    Idx selectedCol = 0;
    long selectedScore = -1;
    for (Idx col = 0; col < cols; ++col) {
      long colScore = 0;
      // the score of the column is theoretically calculated by any of the equivalent
      // schemes described in the paper: d, n, p and q; these are equivalent because of
      // how our tree is assumed to have no useless rows ever:
      // proposition 2 of the paper says that the column i is
      // necessary for row j in P iff it's either:
      // 1. a constructor pattern
      // 2. a wildcard, and row j of P/i is useless
      // but since in our design, rows are never useless, only the first clause is relevant;
      // thus d ~ n and p ~ q; even further, since all the wildcards are positioned at the bottom,
      // n ~ p and thus d ~ n ~ p ~ q.
      for (Idx row = 0; row < rows; ++row) {
        if (!m.rows[row].patterns[col]->isWildcard()) colScore++;
      }
      if (colScore >= selectedScore) {
        // no secondary heuristic
        selectedScore = colScore;
        selectedCol = col;
        if (selectedScore == rows) {
          break; // the column is necessary
        }
      }
    }

    logging::CHIRP.debug("Selected column ", selectedCol, "\n");
    // swap cols, note that we swap to the end rather than the beginning
    if (selectedCol != cols - 1) {
      for (Idx row = 0; row < rows; ++row) {
        auto &rowV = m.rows[row].patterns;
        std::swap(rowV[selectedCol], rowV[cols - 1]);
      }
    }

    std::map<Ctor, std::vector<Idx>> ctors;
    std::vector<Idx> phRows;
    std::map<Ctor, Matrix> decompositions;
    Matrix defaultMat;
    for (Idx row = 0; row < rows; ++row) { // just over all constructors
      Pattern &pat = *m.rows[row].patterns.back();
      if (pat.isWildcard()) {
        phRows.push_back(row);
      } else {
        auto &ctorPat = std::get<Pattern::CtorPat>(pat.v);
        Ctor &ctor = ctorPat.ctor;
        ctors[ctor].push_back(row);
      }
    }

    for (auto &ctorE : ctors) {
      const Ctor &ctor = ctorE.first;
      Idx arity = std::get<Pattern::CtorPat>(m.rows[ctorE.second.front()].patterns.back()->v).ps.size();
      Matrix &dMat = decompositions[ctor];
      for (Idx dRow : ctorE.second) {
        Matrix::Row &oldRow = m.rows[dRow];
        Pattern &dPat = *oldRow.patterns.back();
        Matrix::Row &newRow = dMat.rows.emplace_back(Matrix::Row({}, oldRow.val));
        newRow.patterns.reserve(oldRow.patterns.size() + arity - 1);
        std::copy(oldRow.patterns.begin(), oldRow.patterns.end() - 1,
                  std::back_inserter(newRow.patterns));
        auto &dCtorPat = std::get<Pattern::CtorPat>(dPat.v);
        std::copy(dCtorPat.ps.begin(), dCtorPat.ps.end(),
                  std::back_inserter(newRow.patterns));
      }
      for (Idx dRow : phRows) {
        Matrix::Row &oldRow = m.rows[dRow];
        Matrix::Row &newRow = dMat.rows.emplace_back(Matrix::Row({}, oldRow.val));
        newRow.patterns.reserve(oldRow.patterns.size() + arity - 1);
        std::copy(oldRow.patterns.begin(), oldRow.patterns.end() - 1,
                  std::back_inserter(newRow.patterns));
        std::fill_n(std::back_inserter(newRow.patterns), arity,
                    std::make_shared<Pattern>(Pattern::Wildcard{}));
      }
    }
    for (Idx dRow : phRows) {
      Matrix::Row &oldRow = m.rows[dRow];
      Pattern &dPat = *oldRow.patterns.back();
      if (!dPat.isWildcard()) continue;
      Matrix::Row &newRow = defaultMat.rows.emplace_back(Matrix::Row({}, oldRow.val));
      std::copy(oldRow.patterns.begin(), oldRow.patterns.end() - 1,
                std::back_inserter(newRow.patterns));
    }
    if (logging::DEBUG.isEnabled) {
      std::ostream &os = logging::CHIRP.debug("Decomposed into:\n");
      os << "\\begin{tabular}{c|c}\n";
      for (auto it = decompositions.begin(); it != decompositions.end();) {
        os << "$" << it->first << "$" << " & " << it->second;
        if (++it != decompositions.end()) {
          os << R"(\\ \hline)";
        }
        os << "\n";
      }
      os << "\\end{tabular}\n\n{\n";
    }
    DTree::Switch s;
    for (auto &e : decompositions) {
      s.mapping.insert({e.first, e.second});
    }
    s.fallback = std::make_unique<DTree>(defaultMat);
    s.col = selectedCol;
    v = std::move(s);
    logging::CHIRP.debug("}\n");
  }

  struct OverloadLookup {
    std::vector<std::pair<std::vector<Tp>, Fn>> overloads;
    std::optional<Fn> fallback;
    std::optional<DTree> tree;

    DTree &getOrBuildTree() {
      if (!tree) {
        Matrix mat;
        for (auto &ov : overloads) {
          mat.rows.push_back(Matrix::Row(
              {std::make_shared<Pattern>(
                  Pattern::CtorPat{Ctor(std::nullopt, {(Idx)ov.first.size()}),
                                   Pattern::manyPatterns(ov.first)}
              )},
              &ov.second
          ));
        }
        if (fallback) {
          mat.rows.push_back(Matrix::Row(
              {std::make_shared<Pattern>(Pattern::Wildcard{})},
              &*fallback
          ));
        }
        tree = mat;
      }
      return *tree;
    }

    std::optional<Fn> lookup(const std::vector<Tp> &args) {
      return getOrBuildTree().decide(args);
    }

    void insert(const std::vector<Tp> &params, Fn &&fnv) {
      overloads.emplace_back(params, std::forward<Fn>(fnv));
      tree = {};
    }
    void insertFallback(Fn &&fnv) {
      fallback = std::forward<Fn>(fnv);
      tree = {};
    }
  };

  struct LookupTableImpl : public LookupTable {
    std::map<std::pair<LookupKey::P, std::vector<Constant>>, OverloadLookup> fns;
    Fn lookupFn(
      LookupKey::P fn,
      const std::vector<Constant> &constants,
      const std::vector<Tp> &args
    ) override {
      {
        auto found = fns.find({fn, constants});
        if (found == fns.end()) goto fail;
        OverloadLookup &overloads = found->second;
        std::optional<Fn> lookedUp = overloads.lookup(args);
        if (!lookedUp) goto fail;
        return *lookedUp;
      }
      fail:
      {
        err::Location loc;
        {
          std::stringstream s;
          if (!constants.empty()) {
            s << " with constants:";
            for (const auto &c : constants) {
              s << " " << c;
            }
          }
          loc.msg(s.str());
        }
        {
          std::stringstream s;
          s << " with types:";
          for (const auto &t : args) {
            s << " " << t;
          }
          loc.msg(s.str());
        }
        loc.msg(" trace:");
        throw err::LocationError(util::toStr("Failed lookup for ", fn->value), {loc});
      }

    }
    void insertFn(
      LookupKey::P fn,
      const std::vector<Constant> &constants,
      const std::vector<Tp> &params,
      Fn &&fnv
    ) override {
      fns[{fn, constants}].insert(params, std::forward<Fn>(fnv));
    }
    void insertFallback(
        LookupKey::P fn,
        const std::vector<Constant> &constants,
        Fn &&fnv
    ) override {
      fns[{fn, constants}].insertFallback(std::forward<Fn>(fnv));
    }
  };

  static arena::InternArena<LookupKey> keys;
  LookupKey::P LookupKey::intern(const std::string &value) {
    return keys.intern(std::string(value));
  }

  std::unique_ptr<LookupTable> LookupTable::create() {
    return std::make_unique<LookupTableImpl>();
  }
}
