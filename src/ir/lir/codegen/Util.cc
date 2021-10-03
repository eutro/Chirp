#include "Util.h"

namespace lir::codegen {
  const TyTuple &getTyTuple(CC &cc, type::Ty *ty) {
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
      return getTyTuple(cc, uncycled);
    } else {
      cc.tyCache[ty] = std::make_tuple(nullptr, nullptr, std::nullopt);
    }
    TyTuple tyTup = std::visit(overloaded {
        [&](Ty::Bool &v) -> TyTuple {
          return std::make_tuple(
              llvm::Type::getInt1Ty(cc.ctx),
              cc.db.createBasicType("bool", 1, llvm::dwarf::DW_ATE_boolean),
              std::nullopt
          );
        },
        [&](Ty::Int &v) -> TyTuple {
          Idx bitC = type::bitCount(v.s);
          return std::make_tuple(
              llvm::Type::getIntNTy(cc.ctx, bitC),
              cc.db.createBasicType("i" + std::to_string(bitC), bitC, llvm::dwarf::DW_ATE_signed),
              std::nullopt
          );
        },
        [&](Ty::UInt &v) -> TyTuple {
          Idx bitC = type::bitCount(v.s);
          return std::make_tuple(
              llvm::Type::getIntNTy(cc.ctx, bitC),
              cc.db.createBasicType("u" + std::to_string(bitC), bitC, llvm::dwarf::DW_ATE_unsigned),
              std::nullopt
          );
        },
        [&](Ty::Float &v) -> TyTuple {
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
              cc.db.createBasicType("f" + std::to_string(bitC), bitC, llvm::dwarf::DW_ATE_float),
              std::nullopt
          );
        },
        [&](Ty::ADT &v) -> TyTuple {
          auto sType = llvm::StructType::get(cc.ctx); // opaque
          llvm::PointerType *lt = llvm::PointerType::getUnqual(sType);
          bool zeroSize = isZeroSize(ty);
          if (zeroSize) {
            sType->setBody(llvm::None);
          }
          auto tup = std::make_tuple(
            zeroSize ? (llvm::Type *) sType : lt,
            cc.db.createUnspecifiedType("adt"),
            zeroSize ? std::nullopt : std::make_optional(GCData{nullptr})
          );
          if (zeroSize) {
            return tup;
          }
          cc.tyCache[ty] = tup;
          auto &optRef = std::get<2>(tup);
          std::vector<Idx> collectibles;
          for (Idx i = 0; i < v.s.size(); ++i) {
            const std::optional<GCData> &gcMeta =
                std::get<2>(getTyTuple(cc, v.s[i]));
            if (gcMeta) {
              collectibles.push_back(i);
            }
          }
          if (!collectibles.empty()) {
            auto i8PtrTy = llvm::Type::getInt8PtrTy(cc.ctx);
            auto i8PtrPtrTy = i8PtrTy->getPointerTo();
            ensureMetaTy(cc);

            auto gcMetaFn = llvm::Function::Create(
                cc.metaFnTy,
                llvm::GlobalValue::PrivateLinkage,
                "adt.mark",
                cc.mod
            );
            optRef->metadata = new llvm::GlobalVariable(
                cc.mod,
                cc.gcMetaTy,
                true,
                llvm::GlobalVariable::PrivateLinkage,
                llvm::ConstantStruct::get(
                  cc.gcMetaTy,
                  {
                    gcMetaFn,
                    llvm::ConstantInt::get(cc.ctx, llvm::APInt(8, 0))
                  }
                ),
                "adt.meta"
            );
            cc.deferred.push_back([&v, // lifetime of the tcx it's from
                                   gcMetaFn, collectibles, i8PtrPtrTy]
                                   (CC &cc) {
              llvm::IRBuilder<> ib(cc.ctx);
              llvm::BasicBlock *entry = llvm::BasicBlock::Create(cc.ctx, "entry", gcMetaFn);
              ib.SetInsertPoint(entry);
              llvm::Argument *self = gcMetaFn->getArg(0);
              llvm::StructType *structTy = adtTy(cc, v);
              llvm::Value *castSelf = ib.CreatePointerCast(self, structTy->getPointerTo());
              llvm::Argument *visitor = gcMetaFn->getArg(1);
              for (auto idx : collectibles) {
                llvm::Value *gep = ib.CreateInBoundsGEP(structTy, castSelf, {
                    llvm::ConstantInt::get(cc.ctx, llvm::APInt(32, 0)),
                    llvm::ConstantInt::get(cc.ctx, llvm::APInt(32, idx))
                });
                llvm::Value *castGep = ib.CreatePointerCast(gep, i8PtrPtrTy);
                llvm::Constant *meta = std::get<2>(getTyTuple(cc, v.s[idx]))->metadata;
                if (!meta) {
                  meta = llvm::ConstantPointerNull::get(cc.gcMetaTy->getPointerTo());
                }
                ib.CreateCall(cc.visitFnTy, visitor, {castGep, meta});
              }
              ib.CreateRetVoid();
            });
          }
          return tup;
        },
        [&](Ty::Tuple &v) -> TyTuple {
          std::vector<llvm::Type *> fieldTys;
          std::vector<llvm::Metadata *> fieldDiTys;
          fieldTys.reserve(v.t.size());
          for (Tp fieldTy : v.t) {
            auto &pair = getTyTuple(cc, fieldTy);
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
              ),
              // TODO this is technically wrong, but the only tuple that currently exists is the unit type
              std::nullopt
          );
        },
        [&](Ty::String &v) -> TyTuple {
          if (v.nul) {
            return std::make_tuple(
                llvm::Type::getInt8PtrTy(cc.ctx),
                cc.db.createStringType("cstr", 64),
                std::nullopt
            );
          }
          return std::make_tuple(
              llvm::StructType::get(cc.ctx, {
                  llvm::Type::getInt64Ty(cc.ctx), // len
                  llvm::Type::getInt8PtrTy(cc.ctx), // bytes_utf8
              }),
              cc.db.createStructType(
                  cc.cu->getFile(), "string", cc.cu->getFile(), 1,
                  128, 64, llvm::DINode::DIFlags::FlagPublic,
                  nullptr,
                  cc.db.getOrCreateArray(
                      {
                          cc.db.createBasicType("u64", 64, llvm::dwarf::DW_ATE_signed_fixed),
                          cc.db.createStringType("string", 64)
                      }
                  )
              ),
              std::nullopt
          );
        },
        [&](Ty::FfiFn &v) -> TyTuple {
          auto &tup = std::get<Ty::Tuple>(v.args->v);
          std::vector<llvm::Metadata *> argTys;
          argTys.reserve(tup.t.size() + 1);
          argTys.push_back(std::get<1>(getTyTuple(cc, v.ret)));
          for (auto &t : tup.t) {
            argTys.push_back(std::get<1>(getTyTuple(cc, t)));
          }
          return std::make_tuple(
              ffiFnTy(cc, v)->getPointerTo(),
              cc.db.createPointerType(
                  cc.db.createSubroutineType(cc.db.getOrCreateTypeArray(argTys)),
                  64
              ),
              std::nullopt
          );
        },
        [](auto&) -> TyTuple {
          throw std::runtime_error("Type cannot exist after inference");
        }
    }, ty->v);
    return cc.tyCache[ty] = tyTup;
  }

  bool isZeroSize(Tp ty) {
    return std::visit(overloaded {
        [](Ty::Tuple &v) {
          for (auto &ty : v.t) {
            if (!isZeroSize(ty)) return false;
          }
          return true;
        },
        [](Ty::ADT &v) {
          for (auto &ty : v.s) {
            if (!isZeroSize(ty)) return false;
          }
          return true;
        },
        // cyclic types are only non-zero sized if a contained
        // component is, so cyclic references should be ignored
        [](Ty::Cyclic &c) { return isZeroSize(c.ty); },
        [](Ty::CyclicRef&) { return true; },
        // other types assumed to be non-zero sized
        [](auto &v) { return false; },
      }, ty->v);
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

  llvm::StructType *adtTy(CC &cc, Ty::ADT &v) {
    std::vector<llvm::Type *> fieldTys;
    fieldTys.reserve(v.s.size());
    for (Tp fieldTy : v.s) {
      fieldTys.push_back(getTy(cc, fieldTy));
    }
    return llvm::StructType::get(cc.ctx, fieldTys);
  }

  llvm::Value *LocalCC::load(Value &v) {
    auto ref = v.ref();
    switch (v.loadTy) {
      case Value::Pointer:
        return ib.CreateLoad(v.ty, ref);
      case Value::Direct:
        return ref;
      default: throw 0;
    }
  }

  llvm::Value *LocalCC::reference(Value &v) {
    switch (v.loadTy) {
      case Value::Pointer:
        return v.ref();
      case Value::Direct:
        // allocate temporary?
        throw std::runtime_error("Attempted to get reference to direct value");
      default: throw 0;
    }
  }
  
  llvm::Value *LocalCC::load(Insn *insn) {
    return load(vals.at(insn));
  }

  llvm::Value *LocalCC::reference(Insn *insn) {
    return reference(vals.at(insn));
  }

  Value &LocalCC::varFor(Idx idx) {
    auto found = vars.find(idx);
    if (found != vars.end()) {
      return found->second;
    }
    return cc.vars.at(idx);
  }

  llvm::Value *LocalCC::load(Idx idx) {
    return load(varFor(idx));
  }

  llvm::Value *LocalCC::reference(Idx idx) {
    return reference(varFor(idx));
  }

  void gcRoot(CC &cc, llvm::IRBuilder<> &ib, llvm::Value *reference, llvm::Value *meta) {
    llvm::Function *gcRoot = llvm::Intrinsic::getDeclaration(&cc.mod, llvm::Intrinsic::gcroot);
    llvm::PointerType *i8PtrTy = llvm::Type::getInt8PtrTy(cc.ctx);

    llvm::Value *refAsVoidPtrPtr = ib.CreatePointerCast(reference, i8PtrTy->getPointerTo());
    llvm::Value *metaConst = meta ?
      ib.CreatePointerCast(meta, i8PtrTy) :
      llvm::ConstantPointerNull::get(i8PtrTy);
    ib.CreateCall(gcRoot, {refAsVoidPtrPtr, metaConst});
  }

  llvm::AllocaInst *addTemporary(LocalCC &lcc, llvm::Type *ty, llvm::Value *meta) {
    llvm::IRBuilder<> ib(lcc.cc.ctx);
    llvm::BasicBlock *firstBB = &lcc.func->getBasicBlockList().front();
    ib.SetInsertPoint(firstBB, firstBB->getFirstInsertionPt());
    auto alloca = ib.CreateAlloca(ty);
    ib.CreateStore(llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ty)), alloca);
    gcRoot(lcc.cc, ib, alloca, meta);
    return alloca;
  }

  void ensureMetaTy(CC &cc) {
    if (!cc.gcMetaTy) {
      auto i8PtrTy = llvm::Type::getInt8PtrTy(cc.ctx);
      auto i8PtrPtrTy = i8PtrTy->getPointerTo();
      cc.gcMetaTy = llvm::StructType::create(cc.ctx, "GCMeta");
      llvm::Type *voidTy = llvm::Type::getVoidTy(cc.ctx);
      cc.visitFnTy = llvm::FunctionType::get(
        voidTy,
        {
          i8PtrPtrTy,
          cc.gcMetaTy->getPointerTo()
        },
        false
      );
      cc.metaFnTy = llvm::FunctionType::get(
        voidTy,
        {
          i8PtrTy,
          cc.visitFnTy->getPointerTo(),
        },
        false
      );
      cc.gcMetaTy->setBody(
        cc.metaFnTy->getPointerTo(),
        llvm::Type::getInt8Ty(cc.ctx)
      );
    }
  }
}
