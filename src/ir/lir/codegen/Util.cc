#include "Util.h"

#include <llvm/IR/Constants.h>

namespace lir::codegen {
  TyTuple getTyTuple(CC &cc, Tp ty, const Ty::Bool &v) {
    return std::make_tuple(
        llvm::Type::getInt1Ty(cc.ctx),
        cc.db.createBasicType("bool", 1, llvm::dwarf::DW_ATE_boolean),
        std::nullopt
    );
  }
  
  TyTuple getTyTuple(CC &cc, Tp ty, const Ty::Int &v) {
    Idx bitC = type::bitCount(v.s);
    return std::make_tuple(
        llvm::Type::getIntNTy(cc.ctx, bitC),
        cc.db.createBasicType("i" + std::to_string(bitC), bitC, llvm::dwarf::DW_ATE_signed),
        std::nullopt
    );
  }
  
  TyTuple getTyTuple(CC &cc, Tp ty, const Ty::UInt &v) {
    Idx bitC = type::bitCount(v.s);
    return std::make_tuple(
        llvm::Type::getIntNTy(cc.ctx, bitC),
        cc.db.createBasicType("u" + std::to_string(bitC), bitC, llvm::dwarf::DW_ATE_unsigned),
        std::nullopt
    );
  }
  
  TyTuple getTyTuple(CC &cc, Tp ty, const Ty::Float &v) {
    llvm::Type *lt;
    switch (v.s) {
      case type::FloatSize::f16: lt = llvm::Type::getHalfTy(cc.ctx); break;
      case type::FloatSize::f32: lt = llvm::Type::getFloatTy(cc.ctx); break;
      case type::FloatSize::f64: lt = llvm::Type::getDoubleTy(cc.ctx); break;
      default: throw util::Unreachable();
    }
    Idx bitC = type::bitCount(v.s);
    return std::make_tuple(
        lt,
        cc.db.createBasicType("f" + std::to_string(bitC), bitC, llvm::dwarf::DW_ATE_float),
        std::nullopt
    );
  }
  
  TyTuple getTyTuple(CC &cc, Tp ty, const Ty::ADT &v) {
    std::string name = util::toStr(ty);
    auto sType = llvm::StructType::create(cc.ctx, name); // opaque
    llvm::PointerType *lt = llvm::PointerType::getUnqual(sType);
    bool zeroSize = isZeroSize(ty);
    if (zeroSize) {
      sType->setBody(llvm::None);
    }
    auto tup = std::make_tuple(
        zeroSize ? (llvm::Type *) sType : lt,
        cc.db.createUnspecifiedType(name),
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
          name + ".mark",
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
          name + ".meta"
      );
      cc.deferred.emplace_back([&v, // lifetime of the tcx it's from
                                   gcMetaFn,
                                   i8PtrPtrTy,
                                   collectibles = std::move(collectibles)]
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
  }
  
  TyTuple getTyTuple(CC &cc, Tp ty, const Ty::Tuple &v) {
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
  }
  
  TyTuple getTyTuple(CC &cc, Tp ty, const Ty::String &v) {
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
  }
  
  TyTuple getTyTuple(CC &cc, Tp ty, const Ty::FfiFn &v) {
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
  }
  
  TyTuple getTyTuple(CC &cc, Tp ty, const Ty::Union &u) {
    if (u.tys.empty()) {
      return std::make_tuple(
          llvm::StructType::create(cc.ctx, {}, "!"),
          cc.db.createStructType(
              cc.cu->getFile(), "!", cc.cu->getFile(), 1,
              0, 0, llvm::DINode::DIFlags::FlagPublic,
              nullptr,
              cc.db.getOrCreateArray({})
          ),
          std::nullopt
      );
    } else {
      std::string name = util::toStr(ty);
      /* TODO enums
      if (isEnum(cc, u)) {
        return std::make_tuple(
            llvm::StructType::create(cc.ctx, llvm::IntegerType::getInt32Ty(cc.ctx), name),
            cc.db.createBasicType(name, 32, llvm::dwarf::DW_ATE_unsigned),
            std::nullopt
        );
      } else */ {
        auto sType = llvm::StructType::create(cc.ctx, name); // opaque
        std::vector<const TyTuple *> members;
        auto tup = std::make_tuple(
            llvm::PointerType::getUnqual(sType),
            cc.db.createUnspecifiedType(name),
            std::make_optional(GCData{nullptr})
        );
        cc.tyCache[ty] = tup;
        auto &optRef = std::get<2>(tup);
        members.reserve(u.tys.size());
        for (Tp m : u.tys) {
          members.push_back(&getTyTuple(cc, m));
        }
        std::vector<std::pair<Idx, GCData>> collected;
        Idx i = 0;
        for (const TyTuple *tt : members) {
          auto &gcd = std::get<2>(*tt);
          if (gcd) {
            collected.emplace_back(i, *gcd);
          }
          i++;
        }
        if (!collected.empty()) {
          auto i8PtrTy = llvm::Type::getInt8PtrTy(cc.ctx);
          auto i8PtrPtrTy = i8PtrTy->getPointerTo();
          ensureMetaTy(cc);

          auto gcMetaFn = llvm::Function::Create(
              cc.metaFnTy,
              llvm::GlobalValue::PrivateLinkage,
              name + ".mark",
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
              name + ".meta"
          );
          cc.deferred.emplace_back([&u, // lifetime of the tcx it's from
                                       gcMetaFn, collected, i8PtrPtrTy]
                                       (CC &cc) {
            llvm::IntegerType *i32Ty = llvm::Type::getInt32Ty(cc.ctx);
            llvm::IRBuilder<> ib(cc.ctx);
            llvm::BasicBlock *entry = llvm::BasicBlock::Create(cc.ctx, "entry", gcMetaFn);
            ib.SetInsertPoint(entry);
            llvm::Argument *self = gcMetaFn->getArg(0);
            llvm::StructType *erasedUnionTy = unionTy(cc, llvm::StructType::get(cc.ctx, false));
            llvm::Value *castErasedVal = ib.CreatePointerCast(self, erasedUnionTy->getPointerTo());
            llvm::Argument *visitor = gcMetaFn->getArg(1);
            llvm::Value *disc = ib.CreateLoad(i32Ty, ib.CreateStructGEP(erasedUnionTy, castErasedVal, 0));
            llvm::BasicBlock *end = llvm::BasicBlock::Create(cc.ctx, "end", gcMetaFn);
            llvm::SwitchInst *switchI = ib.CreateSwitch(disc, end, collected.size());
            for (auto &p : collected) {
              Idx i = p.first;
              llvm::BasicBlock *bb = llvm::BasicBlock::Create(cc.ctx, util::toStr("union.case.", i), gcMetaFn);
              switchI->addCase(llvm::ConstantInt::get(i32Ty, i), bb);
              ib.SetInsertPoint(bb);
              llvm::Constant *meta = p.second.metadata;
              if (!meta) {
                meta = llvm::ConstantPointerNull::get(cc.gcMetaTy->getPointerTo());
              }
              llvm::StructType *sTy = unionTy(cc, getTy(cc, u.tys[i]));
              llvm::Value *castVal = ib.CreatePointerCast(self, sTy->getPointerTo());
              llvm::Value *valueGep = ib.CreatePointerCast(ib.CreateStructGEP(sTy, castVal, 1), i8PtrPtrTy);
              ib.CreateCall(cc.visitFnTy, visitor, {valueGep, meta});
              ib.CreateBr(end);
            }
            ib.SetInsertPoint(end);
            ib.CreateRetVoid();
          });
        }
        return tup;
      }
    }
  }
  
  template <typename T>
  TyTuple getTyTuple(CC &cc, Tp ty, const T &) {
    throw util::ICE(util::toStr("Type ", ty, " cannot exist after inference"));
  }
  
  const TyTuple &getTyTuple(CC &cc, type::Tp ty) {
    auto found = cc.tyCache.find(ty);
    if (found != cc.tyCache.end()) {
      if (std::get<0>(found->second)) {
        return found->second;
      } else {
        throw util::ICE("Recursive flat type has infinite size");
      }
    }
    if (std::holds_alternative<Ty::Cyclic>(ty->v)) {
      Tp uncycled = type::uncycle(ty);
      return getTyTuple(cc, uncycled);
    } else {
      cc.tyCache[ty] = std::make_tuple(nullptr, nullptr, std::nullopt);
    }
    TyTuple tyTup = std::visit([&](auto &x) { return getTyTuple(cc, ty, x); }, ty->v);
    return cc.tyCache[ty] = tyTup;
  }

  bool isZeroSize(Tp ty) {
    return std::visit(overloaded {
        [](const Ty::Tuple &v) {
          return std::all_of(v.t.begin(), v.t.end(), isZeroSize);
        },
        [](const Ty::ADT &v) {
          return std::all_of(v.s.begin(), v.s.end(), isZeroSize);
        },
        // cyclic types are only non-zero sized if a contained
        // component is, so cyclic references should be ignored
        [](const Ty::Cyclic &c) { return isZeroSize(c.ty); },
        [](const Ty::CyclicRef&) { return true; },
        // other types assumed to be non-zero sized
        [](auto &v) { return false; },
      }, ty->v);
  }

  llvm::DILocation *locFromSpan(CC &cc, LocalCC &lcc, const loc::SrcLoc &loc) {
    return llvm::DILocation::get(cc.ctx, loc.line, loc.col, lcc.scopes.back());
  }

  llvm::FunctionType *ffiFnTy(CC &cc, const Ty::FfiFn &v) {
    auto &tup = std::get<Ty::Tuple>(v.args->v);
    std::vector<llvm::Type *> argTys;
    argTys.reserve(tup.t.size());
    for (auto &t : tup.t) {
      argTys.push_back(getTy(cc, t));
    }
    return llvm::FunctionType::get(getTy(cc, v.ret), argTys, false);
  }

  llvm::StructType *adtTy(CC &cc, const Ty::ADT &v) {
    std::vector<llvm::Type *> fieldTys;
    fieldTys.reserve(v.s.size());
    for (Tp fieldTy : v.s) {
      fieldTys.push_back(getTy(cc, fieldTy));
    }
    return llvm::StructType::get(cc.ctx, fieldTys);
  }

  llvm::StructType *unionTy(CC &cc, llvm::Type *valueType) {
    // if the union is bigger than this, we have bigger problems
    auto disc = llvm::Type::getInt32Ty(cc.ctx);
    return llvm::StructType::get(cc.ctx, {disc, valueType});
  }

  bool isEnum(CC &cc, Ty::Union &u) {
    return std::all_of(u.tys.begin(), u.tys.end(), isZeroSize);
  }

  llvm::Value *LocalCC::load(Value &v) {
    auto ref = v.ref();
    switch (v.loadTy) {
      case Value::Pointer:
        return ib.CreateLoad(v.ty, ref);
      case Value::Direct:
        return ref;
      default: throw util::Unreachable();
    }
  }

  llvm::Value *LocalCC::reference(Value &v) {
    switch (v.loadTy) {
      case Value::Pointer:
        return v.ref();
      case Value::Direct:
        // allocate temporary?
        throw util::ICE("Attempted to get reference to direct value");
      default: throw util::Unreachable();
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

  Tp getChirpTy(LocalCC &lcc, Idx i) {
    return lcc.inst.loggedTys.at(i);
  }

  llvm::Value *gcAlloc(LocalCC &lcc, llvm::Type *structTy) {
    auto i32Ty = llvm::IntegerType::getInt32Ty(lcc.cc.ctx);
    auto i8PtrTy = llvm::IntegerType::getInt8PtrTy(lcc.cc.ctx);
    auto gcAlloc = lcc.cc.mod.getOrInsertFunction("chirpGcAlloc", i8PtrTy, i32Ty, i32Ty);
    auto rawSize = llvm::ConstantExpr::getSizeOf(structTy);
    auto rawAlign = llvm::ConstantExpr::getAlignOf(structTy);
    auto size = llvm::ConstantExpr::getTruncOrBitCast(rawSize, i32Ty);
    auto align = llvm::ConstantExpr::getTruncOrBitCast(rawAlign, i32Ty);
    auto call = lcc.ib.CreateCall(gcAlloc, {size, align});
    return lcc.ib.CreatePointerCast(call, structTy->getPointerTo());
  }

  llvm::Value *unionise(LocalCC &lcc, Value &inValue, Tp inTy, Tp outTy) {
    if (inTy == outTy) return lcc.load(inValue);
    if (!std::holds_alternative<Ty::Union>(outTy->v)) {
      throw util::ICE("Attempted to unionise to non-union type");
    }
    llvm::IntegerType *i32Ty = llvm::Type::getInt32Ty(lcc.cc.ctx);
    llvm::Type *outUnionTy = getTy(lcc.cc, outTy);
    auto &outU = std::get<Ty::Union>(outTy->v);
    auto innerUnionise = [&](Value &inV, Idx discIdx) {
      llvm::StructType *sTy = unionTy(lcc.cc, inV.ty);
      llvm::Value *ptr = gcAlloc(lcc, sTy);
      llvm::Value *rawValuePtr = lcc.ib.CreateStructGEP(sTy, ptr, 1, "union.value");
      llvm::Value *valuePtr = lcc.ib.CreatePointerCast(rawValuePtr, inV.ty->getPointerTo(), "union.value.cast");
      lcc.ib.CreateStore(lcc.load(inV), valuePtr);
      llvm::Value *idxPtr = lcc.ib.CreateStructGEP(sTy, ptr, 0, "union.idx");
      lcc.ib.CreateStore(llvm::ConstantInt::get(i32Ty, discIdx), idxPtr);
      return lcc.ib.CreatePointerCast(ptr, outUnionTy);
    };
    if (std::holds_alternative<Ty::Union>(inTy->v)) {
      auto found = lcc.cc.unionConversions.find({inTy, outTy});
      if (found == lcc.cc.unionConversions.end()) {
        llvm::Type *inUnionTy = getTy(lcc.cc, inTy);
        auto &inU = std::get<Ty::Union>(inTy->v);
        llvm::Type *uStructTy = unionTy(lcc.cc, llvm::StructType::get(lcc.cc.ctx, false));
        llvm::FunctionType *conversionType = llvm::FunctionType::get(outUnionTy, {inUnionTy}, false);
        llvm::Function *convert = llvm::Function::Create(conversionType, llvm::GlobalValue::PrivateLinkage,
                                                         "union.convert", lcc.cc.mod);
        convert->setGC(GC_METHOD);
        found = lcc.cc.unionConversions.insert({{inTy, outTy}, llvm::FunctionCallee(conversionType, convert)}).first;

        auto insertBlock = lcc.ib.GetInsertBlock();
        auto insertPoint = lcc.ib.GetInsertPoint();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(lcc.cc.ctx, "entry", convert);
        lcc.ib.SetInsertPoint(entry);
        llvm::Argument *arg = convert->getArg(0);
        arg->setName("union.in");
        auto inPtrTy = uStructTy->getPointerTo();
        llvm::Value *castInput = lcc.ib.CreatePointerCast(arg, inPtrTy);
        llvm::AllocaInst *unionRef = lcc.ib.CreateAlloca(inPtrTy, nullptr, "union.in.ref");
        lcc.ib.CreateStore(castInput, unionRef);
        gcRoot(lcc.cc, lcc.ib, unionRef, getTy<std::optional<GCData>>(lcc.cc, inTy)->metadata);
        llvm::Value *loaded = lcc.ib.CreateLoad(inPtrTy, unionRef);
        llvm::Value *discGep = lcc.ib.CreateStructGEP(uStructTy, loaded, 0);
        llvm::Value *inUnionDisc = lcc.ib.CreateLoad(i32Ty, discGep, "union.in.idx");
        llvm::BasicBlock *cont = llvm::BasicBlock::Create(lcc.cc.ctx, "union.cont");
        llvm::PHINode *phi = llvm::PHINode::Create(outUnionTy, inU.tys.size() + 1, "union.out", cont);
        llvm::SwitchInst *switchI = lcc.ib.CreateSwitch(inUnionDisc, cont, inU.tys.size());
        phi->addIncoming(llvm::PoisonValue::get(outUnionTy), lcc.ib.GetInsertBlock());
        for (Idx i = 0; i < inU.tys.size(); ++i) {
          llvm::BasicBlock *bb = llvm::BasicBlock::Create(lcc.cc.ctx, util::toStr("union.case.", i), convert);
          switchI->addCase(llvm::ConstantInt::get(i32Ty, i), bb);
          lcc.ib.SetInsertPoint(bb);
          Tp branchCTy = inU.tys[i];
          llvm::Type *branchTy = getTy(lcc.cc, branchCTy);
          Value branchVal([&lcc, unionRef, branchTy]() {
                    auto sTy = unionTy(lcc.cc, branchTy);
                    llvm::PointerType *sTyPtr = sTy->getPointerTo();
                    llvm::Value *loaded = lcc.ib.CreateLoad(sTyPtr, lcc.ib.CreatePointerCast(unionRef, sTyPtr->getPointerTo()));
                    llvm::Value *rawValueGep = lcc.ib.CreateStructGEP(sTy, loaded, 1, "union.in.value");
                    llvm::Value *castGep = lcc.ib.CreatePointerCast(rawValueGep, branchTy->getPointerTo(), "union.ptr.cast");
                    return castGep;
                  },
                  branchTy,
                  Value::Pointer);
          llvm::Value *retVal = innerUnionise(branchVal, i); // might collect and move the input union
          retVal->setName("union.out.ptr");
          lcc.ib.CreateBr(cont);
          phi->addIncoming(retVal, bb);
        }
        cont->insertInto(convert);
        lcc.ib.SetInsertPoint(cont);
        lcc.ib.CreateRet(lcc.ib.CreatePointerCast(phi, outUnionTy));

        lcc.ib.SetInsertPoint(insertBlock, insertPoint);
      }
      return lcc.ib.CreateCall(found->second, {lcc.load(inValue)});
    } else {
      if (!std::binary_search(outU.tys.begin(), outU.tys.end(), inTy)) {
        throw util::ICE("Attempted to unionise into unrelated union");
      }
      auto it = std::lower_bound(outU.tys.begin(), outU.tys.end(), inTy);
      Idx discIdx = std::distance(outU.tys.begin(), it);
      llvm::Value *retVal = innerUnionise(inValue, discIdx);
      retVal->setName("union.ref");
      return lcc.ib.CreatePointerCast(retVal, outUnionTy);
    }
  }

  bool maybeTemp(LocalCC &lcc, Tp ty, llvm::Value *value, Value &out) {
    const std::optional<GCData> &gcData = getTy<std::optional<GCData>>(lcc.cc, ty);
    if (gcData) {
      auto alloca = addTemporary(lcc, value->getType(), gcData->metadata);
      lcc.ib.CreateStore(value, alloca);
      out = {alloca, value->getType(), Value::Pointer};
      return true;
    }
    return false;
  }
}
