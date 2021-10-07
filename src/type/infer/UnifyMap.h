#pragma once

#include "../Type.h"

#include <map>

namespace type::infer {
  struct IndexAndArityCmp {
    bool operator()(const std::vector<Tp> &lhs, const std::vector<Tp> &rhs) const;
    bool operator()(Tp lhs, Tp rhs) const;
  };

  void addChildren(std::vector<Tp> &c, Tp ty);
  std::vector<Tp> childrenOf(const std::vector<Tp> &tys, size_t hint = 0);

  // TODO make it work with cyclic types
  template <typename T>
  struct UnifyMap {
    // mask of placeholders and types at this level
    // for each index, false if the index is a placeholder,
    // true if it is not (and should be matched on)
    std::vector<bool> mask;
    size_t maskedArity; // number of trues in mask
    std::variant<
      // each key is a vector of types, where the length of the vector
      // is the same as the number of true values in the mask
      std::map<std::vector<Tp>, UnifyMap, IndexAndArityCmp>,
      T // value if mask is all false
    > map;

    friend const T *findLoop(const UnifyMap *node, const std::vector<Tp> *tys) {
      std::vector<Tp> heapTys;
      while (true) {
        if (node->map.index() == 0 && std::get<0>(node->map).empty()) {
          return nullptr;
        }
        if (tys->size() != node->mask.size()) {
          throw std::runtime_error("Bad arity in find");
        }
        if (node->maskedArity == 0) {
          return &std::get<1>(node->map);
        }
        std::vector<Tp> masked;
        masked.reserve(node->maskedArity);
        {
          auto ti = tys->begin();
          auto mi = node->mask.begin();
          for (; ti != tys->end(); ++ti, ++mi) {
            if (*mi) masked.push_back(*ti);
          }
        }
        auto &m = std::get<0>(node->map);
        auto found = m.find(masked);
        if (found == m.end()) {
          return nullptr;
        }
        node = &found->second;
        heapTys = childrenOf(masked, found->second.mask.size());
        tys = &heapTys;
      }
    }

    const T *find(const std::vector<Tp> &tys) const {
      return findLoop(this, &tys);
    }

    friend UnifyMap *getOrCreateNode(
      UnifyMap *node,
      const std::vector<Tp> *tys,
      bool &newNode
    ) {
      std::vector<Tp> heapTys;
      while (true) {
        if (tys->size() != node->mask.size()) {
          throw std::runtime_error("Bad arity in insert");
        }
        std::vector<bool> mask(tys->size(), true);
        size_t maskArity = 0;
        for (Idx i = 0; i < tys->size(); ++i) {
          Tp ty = (*tys)[i];
          if (std::holds_alternative<Ty::Placeholder>(ty->v)) {
            mask[i] = false;
            ++maskArity;
          }
        }
        if (newNode) {
          node->mask = mask;
          node->maskedArity = maskArity;
        } else {
          if (mask != node->mask) {
            throw std::runtime_error("Mismatched masks");
          }
        }
        if (maskArity == 0) {
          return node;
        } else {
          std::vector<Tp> masked;
          masked.reserve(maskArity);
          for (Tp ty : *tys) {
            if (!std::holds_alternative<Ty::Placeholder>(ty->v)) {
              masked.push_back(ty);
            }
          }
          auto &m = std::get<0>(node->map);
          auto found = m.find(masked);
          if (found == m.end()) {
            node = &m[masked];
            newNode = true;
          } else {
            node = &found->second;
          }
          heapTys = childrenOf(masked, 0);
          tys = &heapTys;
        }
      }
    }

    template<typename... Args>
    bool insert(const std::vector<Tp> &tys, Args &&...args) {
      bool newNode = map.index() == 0 && std::get<0>(map).empty();
      UnifyMap *node = getOrCreateNode(this, &tys, newNode);
      node->map.template emplace<1>(std::forward<Args>(args)...);
      return newNode;
    }

    T &operator[](const std::vector<Tp> &tys) {
      bool newNode;
      UnifyMap *node = getOrCreateNode(this, &tys, newNode);
      if (newNode) {
        return node->map.template emplace<1>();
      } else {
        return std::get<1>(node->map);
      }
    }
  };
}
