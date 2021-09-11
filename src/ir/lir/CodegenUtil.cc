#include "CodegenUtil.h"

namespace lir::codegen {
  const TyPair &getTyPair(CC &cc, type::Ty *ty) {
    auto found = cc.tyCache.find(ty);
    if (found != cc.tyCache.end()) {
      if (std::get<0>(found->second)) {
        return found->second;
      } else {
        throw std::runtime_error("Recursive flat type has infinite size");
      }
    }
    if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
      Ty *uncycled = type::uncycle(cc.tcx, cc.tbcx, ty);
      auto &pair = getTyPair(cc, uncycled);
      return cc.tyCache[ty] = pair;
    } else {
      cc.tyCache[ty] = std::make_tuple(nullptr, nullptr);
    }
    TyPair tPair = std::visit(overloaded {
        [&](Ty::Bool &v) -> TyPair {
          return std::make_tuple(
              llvm::Type::getInt1Ty(cc.ctx),
              cc.db.createBasicType("bool", 1, llvm::dwarf::DW_ATE_boolean)
          );
        },
        [&](Ty::Int &v) -> TyPair {
          Idx bitC = type::bitCount(v.s);
          return std::make_tuple(
              llvm::Type::getIntNTy(cc.ctx, bitC),
              cc.db.createBasicType("i" + std::to_string(bitC), bitC, llvm::dwarf::DW_ATE_signed)
          );
        },
        [&](Ty::UInt &v) -> TyPair {
          Idx bitC = type::bitCount(v.s);
          return std::make_tuple(
              llvm::Type::getIntNTy(cc.ctx, bitC),
              cc.db.createBasicType("u" + std::to_string(bitC), bitC, llvm::dwarf::DW_ATE_unsigned)
          );
        },
        [&](Ty::Float &v) -> TyPair {
          llvm::Type *lt;
          switch (v.s) {
            case type::FloatSize::f16: lt = llvm::Type::getHalfTy(cc.ctx); break;
            case type::FloatSize::f32: lt = llvm::Type::getFloatTy(cc.ctx); break;
            case type::FloatSize::f64: lt = llvm::Type::getDoubleTy(cc.ctx); break;
            default: throw 0;
          }
          Idx bitC = type::bitCount(v.s);
          return std::make_tuple(
              lt,
              cc.db.createBasicType("f" + std::to_string(bitC), bitC, llvm::dwarf::DW_ATE_float)
          );
        },
        [&](Ty::ADT &v) -> TyPair {
          auto sType = llvm::StructType::get(cc.ctx); // opaque
          llvm::PointerType *lt = llvm::PointerType::getUnqual(sType);
          return std::make_tuple(
              lt,
              cc.db.createUnspecifiedType("adt")
          );
        },
        [&](Ty::Tuple &v) -> TyPair {
          std::vector<llvm::Type *> fieldTys;
          std::vector<llvm::Metadata *> fieldDiTys;
          fieldTys.reserve(v.t.size());
          for (Tp fieldTy : v.t) {
            auto &pair = getTyPair(cc, fieldTy);
            fieldTys.push_back(std::get<0>(pair));
            fieldDiTys.push_back(std::get<1>(pair));
          }
          llvm::StructType *lt = llvm::StructType::get(cc.ctx, fieldTys);
          return std::make_tuple(
              lt,
              cc.db.createStructType(
                  cc.cu->getFile(), "tuple", cc.cu->getFile(), 1,
                  0, 1, llvm::DINode::DIFlags::FlagPublic,
                  nullptr, cc.db.getOrCreateArray(fieldDiTys)
              )
          );
        },
        [&](Ty::String &v) -> TyPair {
          return std::make_tuple(
              llvm::StructType::get(cc.ctx, {
                  llvm::Type::getInt64Ty(cc.ctx), // len
                  llvm::Type::getInt8PtrTy(cc.ctx), // bytes_utf8
              }),
              cc.db.createStructType(
                  cc.cu->getFile(), "string", cc.cu->getFile(), 1,
                  128, 64, llvm::DINode::DIFlags::FlagPublic,
                  nullptr, cc.db.getOrCreateArray({
                                                      cc.db.createBasicType("u64", 64,llvm::dwarf::DW_ATE_signed_fixed),
                                                      cc.db.createStringType("string", 64)
                                                  })
              )
          );
        },
        [&](Ty::FfiFn &v) -> TyPair {
          auto &tup = std::get<Ty::Tuple>(v.args->v);
          std::vector<llvm::Metadata *> argTys;
          argTys.reserve(tup.t.size() + 1);
          argTys.push_back(std::get<1>(getTyPair(cc, v.ret)));
          for (auto &t : tup.t) {
            argTys.push_back(std::get<1>(getTyPair(cc, t)));
          }
          return std::make_pair(
              ffiFnTy(cc, v)->getPointerTo(),
              cc.db.createPointerType(
                  cc.db.createSubroutineType(cc.db.getOrCreateTypeArray(argTys)),
                  64
              )
          );
        },
        [](auto&) -> TyPair {
          throw std::runtime_error("Type cannot exist after inference");
        }
    }, ty->v);
    return cc.tyCache[ty] = tPair;
  }

  llvm::DILocation *locFromSpan(CC &cc, LocalCC &lcc, const loc::SrcLoc &loc) {
    return llvm::DILocation::get(cc.ctx, loc.line, loc.col, lcc.scopes.back());
  }

  llvm::FunctionType *ffiFnTy(CC &cc, Ty::FfiFn &v) {
    auto &tup = std::get<Ty::Tuple>(v.args->v);
    std::vector<llvm::Type *> argTys;
    argTys.reserve(tup.t.size());
    for (auto &t : tup.t) {
      argTys.push_back(getTy(cc, t));
    }
    return llvm::FunctionType::get(getTy(cc, v.ret), argTys, false);
  }

  llvm::Type *adtTy(CC &cc, Ty::ADT &v) {
    std::vector<llvm::Type *> fieldTys;
    fieldTys.reserve(v.s.size());
    for (Tp fieldTy : v.s) {
      fieldTys.push_back(getTy(cc, fieldTy));
    }
    return llvm::StructType::get(cc.ctx, fieldTys);
  }
}
