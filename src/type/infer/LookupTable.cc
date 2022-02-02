#include "LookupTable.h"

#include "../TypePrint.h"
#include "../../common/Logging.h"

#include <memory>
#include <map>
#include <utility>
#include <variant>
#include <ostream>
#include <deque>

namespace type::infer {
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
              [](Ty::Err &t) -> TT { return {util::index_of_type_v<Ty::Err, TyV>, {}}; },
              [](Ty::Bool &t) -> TT { return {util::index_of_type_v<Ty::Bool, TyV>, {}}; },
              [](Ty::Int &t) -> TT { return {util::index_of_type_v<Ty::Int, TyV>, {(Idx) t.s}}; },
              [](Ty::UInt &t) -> TT { return {util::index_of_type_v<Ty::UInt, TyV>, {(Idx) t.s}}; },
              [](Ty::Float &t) -> TT { return {util::index_of_type_v<Ty::Float, TyV>, {(Idx) t.s}}; },
              [](Ty::Placeholder &t) -> TT { throw std::runtime_error("Wildcard does not have a constructor"); },
              [](Ty::ADT &t) -> TT { return {util::index_of_type_v<Ty::ADT, TyV>, {t.i, (Idx) t.s.size()}}; },
              [](Ty::Union &t) -> TT { throw std::runtime_error("Union pattern matching unsupported"); },
              [](Ty::Tuple &t) -> TT { return {util::index_of_type_v<Ty::Tuple, TyV>, {(Idx) t.t.size()}}; },
              [](Ty::String &t) -> TT { return {util::index_of_type_v<Ty::String, TyV>, {(Idx) t.nul}}; },
              [](Ty::Cyclic &t) -> TT { throw std::runtime_error("Cyclic pattern matching unsupported"); },
              [](Ty::CyclicRef &t) -> TT { throw std::runtime_error("Cyclic pattern matching unsupported"); },
              [](Ty::FfiFn &t) -> TT { return {util::index_of_type_v<Ty::FfiFn, TyV>, {}}; },
          },
          ty->v);
    }
  };
  
  std::vector<Tp> childrenOf(Tp ty) {
    return std::visit(overloaded {
        [=](Ty::FfiFn &t) -> std::vector<Tp> { return {t.args, t.ret}; },
        [=](Ty::Tuple &t) { return t.t; },
        [=](Ty::ADT &t) { return t.s; },
        [=](auto &) -> std::vector<Tp> { return {}; }
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

  private:
    static Fn *decideStack(DTree *tree, std::deque<Tp> &stack) {
      while (true) {
        auto &v = tree->v;
        if (std::holds_alternative<Leaf>(v)) {
          return std::get<Leaf>(v).value;
        } else if (std::holds_alternative<Switch>(v)) {
          auto &s = std::get<Switch>(v);
          if (s.col != stack.size() - 1) {
            std::swap(stack[s.col], stack[stack.size() - 1]);
          }
          Tp ty = type::uncycle(stack.back());
          Ctor c(ty);
          stack.pop_back();
          auto found = s.mapping.find(c);
          if (found == s.mapping.end()) {
            tree = s.fallback.get();
          } else {
            std::vector<Tp> children = childrenOf(ty);
            stack.insert(stack.end(), children.begin(), children.end());
            tree = &found->second;
          }
        } else {
          return nullptr;
        }
      }
    }

  public:
    Fn *decide(const std::vector<Tp> &tys) {
      if (std::holds_alternative<Leaf>(v)) {
        return std::get<Leaf>(v).value;
      } else if (std::holds_alternative<Switch>(v)) {
        auto &s = std::get<Switch>(v);
        Ctor arity{std::nullopt, {(Idx)tys.size()}};
        auto found = s.mapping.find(arity);
        if (found != s.mapping.end()) {
          std::deque<Tp> stack(tys.begin(), tys.end());
          return decideStack(&found->second, stack);
        }
      }
      return nullptr;
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
        throw std::runtime_error("Overload resolution is ambiguous");
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

    Fn *lookup(const std::vector<Tp> &args) {
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
    std::map<std::pair<LookupKey*, std::vector<Constant>>, OverloadLookup> fns;
    Fn *lookupFn(
      LookupKey *fn,
      const std::vector<Constant> &constants,
      const std::vector<Tp> &args
    ) override {
      auto found = fns.find({fn, constants});
      if (found == fns.end()) {
        std::stringstream s;
        s << "Undefined function: " << fn->value;
        if (!constants.empty()) {
          s << "\n with constants:";
          for (const auto &c : constants) {
            s << " " << c;
          }
        }
        throw std::runtime_error(s.str());
      }
      OverloadLookup &overloads = found->second;
      Fn *lookedUp = overloads.lookup(args);
      if (!lookedUp) {
        std::stringstream s;
        s << "Undefined function: " << fn->value;
        if (!constants.empty()) {
          s << "\n with constants:";
          for (const auto &c : constants) {
            s << " " << c;
          }
        }
        s << "\n with types:";
        for (const auto &t : args) {
          s << " " << t;
        }
        throw std::runtime_error(s.str());
      }
      return lookedUp;
    }
    void insertFn(
      LookupKey *fn,
      const std::vector<Constant> &constants,
      const std::vector<Tp> &params,
      Fn &&fnv
    ) override {
      fns[{fn, constants}].insert(params, std::forward<Fn>(fnv));
    }
    void insertFallback(
        LookupKey *fn,
        const std::vector<Constant> &constants,
        Fn &&fnv
    ) override {
      fns[{fn, constants}].insertFallback(std::forward<Fn>(fnv));
    }
  };

  static arena::InternArena<LookupKey> keys;
  LookupKey *LookupKey::intern(const std::string &value) {
    return keys.intern(std::string(value));
  }

  std::unique_ptr<LookupTable> LookupTable::create() {
    return std::make_unique<LookupTableImpl>();
  }
}
