#pragma once
#include "SemanticChecker.hpp"
#include "SymbolsVisitor.hpp"
#include "Visitor.hpp"
#include "compiler/AST2.hpp"
#include "compiler/NodeDescriptors.hpp"
#include "compiler/SymbolTable.hpp"
#include "compiler/Type.hpp"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/ADT/APInt.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <memory>
#include <stack>
#include <stdexcept>
#include <string>
#include <unordered_map>

class CodegenVisitor final : public Visitor {
  struct ExprInfo {
    const Type *evalType = nullptr;
    SymbolNode* sym = nullptr;
    llvm::Value* LLVMVal = nullptr;
    ValueCategory valCat = ValueCategory::NOT_VALUE;
  };

public:
  CodegenVisitor()
      : LLVMCtx_(std::make_unique<llvm::LLVMContext>()),
        LLVMModule_(std::make_unique<llvm::Module>("JackProgram", *LLVMCtx_)),
        IRBuilder_(std::make_unique<llvm::IRBuilder<>>(*LLVMCtx_)),
        globalTable_(std::make_unique<SymbolTable>()),
        localTable_(std::make_unique<SymbolTable>()),
        semantic_(std::make_unique<SemanticChecker>(globalTable_.get(),
                                                    localTable_.get())) {
    llvm::DataLayout DL(LLVMModule_.get());
    LLVMModule_->setDataLayout(DL);
  }

  void preVisit(const Program *prog) override {
    SymbolsVisitor symVisitor(globalTable_.get());
    prog->accept(symVisitor);
    semantic_->checkMainFunc();
    declareNewArrayFunc();
    declareDeleteArrayFunc();

    auto processFieldAndStaticVars = [this](auto varItBegin, auto varItEnd, 
                                            const std::string& className) {
      std::vector<llvm::Type *> fieldTypes;
      std::vector<const StaticVarDec *> staticVars;
      for (auto varIt = varItBegin; varIt != varItEnd; ++varIt) {
        if ((*varIt)->getNodeType() == NodeType::FIELD_VAR_DEC)
          fieldTypes.push_back(TypeToLLVMType((*varIt)->getVarType()));
        else
          staticVars.push_back(static_cast<const StaticVarDec*>(*varIt));
      }

      for (const StaticVarDec* var : staticVars) {
        new llvm::GlobalVariable(*LLVMModule_,
            TypeToLLVMType(var->getVarType()), false,
            llvm::GlobalVariable::LinkageTypes::InternalLinkage, nullptr,
            var->getVarName());
      }

      ClassToLLVMType_[ClassType::getClassTy(className)] =
          llvm::StructType::create(*LLVMCtx_, fieldTypes, className);
    };

    auto processSubroutines = [this](auto subrtnBegin, auto subrtnEnd,
                                     const std::string& className) {
      for (auto subrtnIt = subrtnBegin;
           subrtnIt != subrtnEnd; ++subrtnIt) {
        SubroutineDec* S = *subrtnIt;
        std::vector<const Type *> argTypes;
        std::vector<std::string> argNames;
        if (S->getNodeType() == NodeType::METHOD_DEC) {
          argNames.push_back("this");
          argTypes.push_back(ClassType::getClassTy(className));
        }
        for (auto it = S->args_begin(); it != S->args_end(); ++it) {
          auto arg = *it;
          argNames.push_back(arg->getVarName());
          argTypes.push_back(arg->getVarType());
        }
        std::string funcName = className + '_' + S->getName();
        if (className == "Main" && S->getName() == "main") {
          funcName = S->getName();
        }
        createFunction(funcName, S->getReturnType(), argNames, argTypes);
      }
    };

    for (auto classIt = prog->classes_begin();
         classIt != prog->classes_end(); ++classIt) {
      ClassDec* cl = *classIt;
      processFieldAndStaticVars(cl->var_begin(),
                                cl->var_end(),
                                cl->getName());
      processSubroutines(cl->subroutine_begin(),
                         cl->subroutine_end(),
                         cl->getName());
    }
  }

  void postVisit(const Program *prog) override {
    std::string output;
    llvm::raw_string_ostream sstream(output);
    bool res = llvm::verifyModule(*LLVMModule_, &sstream);
    LLVMModule_->dump();
    if (res) {
      throw std::runtime_error("CodegenVisitor error:\n" + output);
    }
  }

  void preVisit(const ClassDec *cl) override {
    std::string className = cl->getName();
    currentClass_ =
        std::static_pointer_cast<ClassSymbol>(globalTable_->find(className));
  }

  void preVisit(const SubroutineDec *subDec) override {
    std::string subName = subDec->getName();
    currentSubrtn_ = std::static_pointer_cast<SubroutineSymbol>(
        currentClass_->findMember(subName));
    if (!currentSubrtn_->haveBody())
      return;
    semantic_->setCurrentSubroutine(currentSubrtn_.get());
    semantic_->checkSubroutine(subDec, currentSubrtn_.get());

    SubroutineKind subKind = currentSubrtn_->getKind();
    auto func = LLVMModule_->getFunction(currentSubrtn_->getName());
    auto &&EntryBB = llvm::BasicBlock::Create(*LLVMCtx_, "", func);
    IRBuilder_->SetInsertPoint(EntryBB);
    BBReturn_ = llvm::BasicBlock::Create(*LLVMCtx_, "", func);
    if (!subDec->getReturnType()->isVoidTy()) {
      auto &&LLVMRetType = TypeToLLVMType(subDec->getReturnType());
      returnVal_ = IRBuilder_->CreateAlloca(LLVMRetType);
      IRBuilder_->SetInsertPoint(BBReturn_);
      auto &&RetVal = IRBuilder_->CreateLoad(LLVMRetType, returnVal_);
      IRBuilder_->CreateRet(RetVal);
    } else {
      IRBuilder_->SetInsertPoint(BBReturn_);
      IRBuilder_->CreateRet(nullptr);
    }
    IRBuilder_->SetInsertPoint(EntryBB);
    if (subKind == SubroutineKind::CONSTRUCTOR_S ||
        subKind == SubroutineKind::METHOD_S) {
      auto thisSym = std::make_shared<LocalVarSymbol>(
          "this", currentClass_->getClassType(), subDec->getSourceLoc());
      localTable_->insert("this", thisSym);

      if (subKind == SubroutineKind::CONSTRUCTOR_S) {
        auto &&classType = currentClass_->getClassType();
        auto &&alloca = IRBuilder_->CreateAlloca(
            TypeToLLVMType(classType), nullptr, "this");
        symbolToAlloca_[thisSym.get()] = alloca;
        auto &&newFunc = LLVMModule_->getFunction("newArray");
        auto &&LLVMIntTy = llvm::Type::getInt32Ty(*LLVMCtx_);
        auto &&callRes = IRBuilder_->CreateCall(newFunc,
            { getSizeOf(classType), llvm::ConstantInt::get(LLVMIntTy, 1) });
        IRBuilder_->CreateStore(callRes, alloca);
      }
    }
  }

  void postVisit(const SubroutineDec *subDec) override {
    if (!currentSubrtn_->haveBody())
      return;
    auto &&func = IRBuilder_->GetInsertBlock()->getParent();
    for (auto &&BBIt = func->begin(); BBIt != func->end(); ) {
      if (BBIt->empty()) {
        BBIt = BBIt->eraseFromParent();
      } else {
        ++BBIt;
      }
    }
    localTable_->clear();
    currentSubrtn_ = nullptr;
  }

  void preVisit(const LocalVarDec *var) override {
    semantic_->checkVariableDec(var);
    auto varSym = std::make_shared<LocalVarSymbol>(var->getVarName(),
                                                   var->getVarType(),
                                                   var->getSourceLoc());
    localTable_->insert(var->getVarName(), varSym);
    auto alloca = IRBuilder_->CreateAlloca(
        TypeToLLVMType(var->getVarType()), nullptr, var->getVarName());
    symbolToAlloca_[varSym.get()] = alloca;
  }

  void preVisit(const ArgumentVarDec *var) override {
    semantic_->checkVariableDec(var);
    if (!currentSubrtn_->haveBody())
      return;
    auto varSym =
        std::make_shared<ArgumentVarSymbol>(var->getVarName(),
                                            var->getVarType(),
                                            var->getSourceLoc());
    localTable_->insert(var->getVarName(), varSym);
    auto alloca = IRBuilder_->CreateAlloca(
        TypeToLLVMType(var->getVarType()), nullptr, var->getVarName());
    auto LLVMFunc = LLVMModule_->getFunction(currentSubrtn_->getName());
    auto argVal =
            LLVMFunc->getValueSymbolTable()->lookup(var->getVarName());
    IRBuilder_->CreateStore(argVal, alloca);
    symbolToAlloca_[varSym.get()] = alloca;
  }

  void postVisit(const NameExpr *nameExpr) override {
    auto curSymbol = semantic_->checkName(currentClass_.get(), nameExpr);

    const Type* evalType = symbolToEvalType(curSymbol.get());
    ValueCategory valueCat = evalType ? ValueCategory::LVALUE :
                                        ValueCategory::NOT_VALUE;
    llvm::Value* LLVMVal = nullptr;
    switch(curSymbol->getSymbolType()) {
    case SymbolType::ARGUMENT_VARIABLE_SYM:
    case SymbolType::LOCAL_VARIABLE_SYM:
      LLVMVal = symbolToAlloca_[curSymbol.get()];
      break;
    case SymbolType::FIELD_VARIABLE_SYM: {
      auto LLVMFunc = LLVMModule_->getFunction(currentSubrtn_->getName());
      auto thisArg =
          LLVMFunc->getValueSymbolTable()->lookup("this");
      auto fieldVar = static_cast<FieldVarSymbol*>(curSymbol.get());
      LLVMVal = createMemberAccess(thisArg, currentClass_.get(), fieldVar);
      break;
    }
    case SymbolType::SUBROUTINE_SYM:
      LLVMVal = LLVMModule_->getFunction(curSymbol->getName());
      break;
    default:
      LLVMVal = nullptr;
    }
    
    exprStack_.push({ evalType, curSymbol.get(), LLVMVal, valueCat });
  }

  void postVisit(const NewArrayExpr *newArrExpr) override {
    const Type *elementType = newArrExpr->getArrayElemType();
    ExprInfo newArraySizeExpr = exprStack_.top();
    exprStack_.pop();
    semantic_->checkNewArray(elementType, newArraySizeExpr.evalType,
                             newArrExpr);

    auto &&newArrayFunc = declareNewArrayFunc();
    llvm::Value* sizeVal = LValueToRValue(newArraySizeExpr);
    auto newArrayVal = IRBuilder_->CreateCall(newArrayFunc->getFunctionType(),
        newArrayFunc, { getSizeOf(elementType), sizeVal } );
    exprStack_.push({ ArrayType::getArrayTy(elementType),
                      nullptr, newArrayVal, ValueCategory::RVALUE });
  }

  void postVisit(const DeleteArrayExpr *delArrExpr) override {
    ExprInfo deleteArrayExpr = exprStack_.top();
    exprStack_.pop();
    semantic_->checkDeleteArray(deleteArrayExpr.evalType, delArrExpr);
    auto &&deleteArrayFunc = declareDeleteArrayFunc();
    IRBuilder_->CreateCall(deleteArrayFunc->getFunctionType(),
                           deleteArrayFunc, { deleteArrayExpr.LLVMVal });
  }

  void postVisit(const LiteralExpr *litExpr) override {
    llvm::Value* LLVMLiteral = nullptr;
    llvm::Type* LLVMTy = TypeToLLVMType(litExpr->getType());
    if (litExpr->getType()->isIntTy()) {
      LLVMLiteral = llvm::ConstantInt::get(LLVMTy,
          std::stoi(litExpr->getValue()));
    } else if (litExpr->getType()->isCharTy()) {
      LLVMLiteral = llvm::ConstantInt::get(LLVMTy,
          litExpr->getValue()[0]);
    } else if (litExpr->getType()->isBoolTy()) {
      int val = litExpr->getValue() == "true" ? 1 : 0;
      LLVMLiteral = llvm::ConstantInt::get(LLVMTy, val);
    } else if (litExpr->getType()->isNullTy()) {
      LLVMLiteral = llvm::Constant::getNullValue(
          llvm::PointerType::get(*LLVMCtx_, 0));
    }
    exprStack_.push(
        { litExpr->getType(), nullptr, LLVMLiteral, ValueCategory::RVALUE });
  }

  void postVisit(const MemberExpr *memberExpr) override {
    ExprInfo memberBase = exprStack_.top();
    exprStack_.pop();

    SymbolNode* memberSym;
    if (memberBase.evalType == nullptr) {
      memberSym = semantic_->checkMemberOfClass(memberBase.sym, memberExpr);
    } else {
      memberSym =
          semantic_->checkMemberOfValue(memberBase.evalType, memberExpr);
    }

    const Type *evalType = symbolToEvalType(memberSym);
    llvm::Value* LLVMVal = nullptr;
    switch(memberSym->getSymbolType()) {
    case SymbolType::FIELD_VARIABLE_SYM:
      LLVMVal = createMemberAccess(memberBase.LLVMVal,
                                   static_cast<ClassSymbol*>(memberBase.sym),
                                   static_cast<FieldVarSymbol*>(memberSym));
      break;
    case SymbolType::STATIC_VARIABLE_SYM:
      LLVMVal = 
          LLVMModule_->getGlobalVariable(memberSym->getName());
      break;
    case SymbolType::SUBROUTINE_SYM: {
      auto subrtnKind = static_cast<SubroutineSymbol*>(memberSym)->getKind();
      if (subrtnKind == SubroutineKind::METHOD_S)
        exprStack_.push(memberBase);
      LLVMVal = LLVMModule_->getFunction(memberSym->getName());
      break;
    }
    default:
      LLVMVal = nullptr;
    }

    exprStack_.push({ evalType, memberSym, LLVMVal, memberBase.valCat });
  }

  void postVisit(const ArrayMemberExpr *arrayExpr) override {
    ExprInfo indexInfo = exprStack_.top();
    exprStack_.pop();
    ExprInfo nameInfo = exprStack_.top();
    exprStack_.pop();
    semantic_->checkArrayMember(nameInfo.evalType, indexInfo.evalType,
                                arrayExpr);
    auto elemType = nameInfo.evalType->getArrayElementType();
    auto LLVMVal =
        createArrayMemberAccess(elemType, nameInfo.LLVMVal, indexInfo.LLVMVal);
    exprStack_.push({ nameInfo.evalType->getArrayElementType(),
                      nullptr, LLVMVal,
                      ValueCategory::LVALUE });
  }

  void postVisit(const CallExpr *subCall) override {
    std::vector<const Type *> argTypes;
    std::vector<llvm::Value *> LLVMArgs;
    for (unsigned i = 0; i < subCall->getArgsCount(); ++i) {
      if (exprStack_.empty()) break;
      ExprInfo exprInfo = exprStack_.top();
      llvm::Value* LLVMVal = LValueToRValue(exprInfo);
      LLVMArgs.push_back(LLVMVal);
      argTypes.push_back(exprInfo.evalType);
      exprStack_.pop();
    }
    std::reverse(argTypes.begin(), argTypes.end());

    ExprInfo nameInfo = exprStack_.top();
    exprStack_.pop();
    SubroutineSymbol *subSym = semantic_->checkSubroutineCall(
        nameInfo.sym, subCall, argTypes.begin(), argTypes.end());
    if (subSym->getKind() == SubroutineKind::METHOD_S) {
      ExprInfo memberBase = exprStack_.top();
      exprStack_.pop();
      llvm::Value* LLVMThisArg = LValueToRValue(memberBase);
      LLVMArgs.push_back(LLVMThisArg);
    }
    std::reverse(LLVMArgs.begin(), LLVMArgs.end());
    llvm::Function *func = llvm::cast<llvm::Function>(nameInfo.LLVMVal);
    auto LLVMVal =
        IRBuilder_->CreateCall(func->getFunctionType(), func, LLVMArgs);
    auto valCateg =
        subSym->getRetType()->isVoidTy() ? ValueCategory::NOT_VALUE :
                                           ValueCategory::RVALUE;
    exprStack_.push({ subSym->getRetType(), nullptr, LLVMVal, valCateg });
  }

  void postVisit(const UnopExpr *unop) override {
    ExprInfo exprInfo = exprStack_.top();
    exprStack_.pop();
    llvm::Value* operand = LValueToRValue(exprInfo);
    semantic_->checkUnop(exprInfo.evalType, unop);
    auto LLVMVal = [&](UnopType opType) {
      switch(opType) {
      case UnopType::NEG_OP:
        return IRBuilder_->CreateNeg(operand);
      case UnopType::NOT_OP:
        return IRBuilder_->CreateNot(operand);
      }
    }(unop->getOpType());

    exprStack_.push({ exprInfo.evalType, nullptr,
                      LLVMVal, ValueCategory::RVALUE });
  }

  void postVisit(const BinopExpr *binop) override {
    ExprInfo secondOp = exprStack_.top();
    exprStack_.pop();
    ExprInfo firstOp = exprStack_.top();
    exprStack_.pop();

    semantic_->checkBinop(firstOp.evalType, secondOp.evalType, binop);

    Type *exprType = [](BinopType opType){
      switch (opType) {
      case BinopType::ADD_OP:
      case BinopType::SUB_OP:
      case BinopType::DIV_OP:
      case BinopType::MUL_OP:
      case BinopType::BIT_AND_OP:
      case BinopType::BIT_OR_OP:
        return Type::getIntTy();
      case BinopType::LSS_OP:
      case BinopType::GTR_OP:
      case BinopType::LOG_AND_OP:
      case BinopType::LOG_OR_OP:
      case BinopType::EQL_OP:
        return Type::getBoolTy();
      }
    }(binop->getOpType());

    llvm::Value* lhs = LValueToRValue(firstOp);
    llvm::Value* rhs = LValueToRValue(secondOp);
    llvm::Value *LLVMVal = [&](BinopType opType) {
      switch(opType) {
      case BinopType::ADD_OP:
        return IRBuilder_->CreateAdd(lhs, rhs);
      case BinopType::SUB_OP:
        return IRBuilder_->CreateSub(lhs, rhs);
      case BinopType::DIV_OP:
        return IRBuilder_->CreateSDiv(lhs, rhs);
      case BinopType::MUL_OP:
        return IRBuilder_->CreateMul(lhs, rhs);
      case BinopType::BIT_AND_OP:
        return IRBuilder_->CreateAnd(lhs, rhs);
      case BinopType::BIT_OR_OP:
        return IRBuilder_->CreateOr(lhs, rhs);
      case BinopType::LSS_OP:
        return IRBuilder_->CreateCmp(llvm::CmpInst::ICMP_SLT, lhs, rhs);
      case BinopType::GTR_OP:
        return IRBuilder_->CreateCmp(llvm::CmpInst::ICMP_SGT, lhs, rhs);
      case BinopType::LOG_AND_OP:
        return IRBuilder_->CreateLogicalAnd(lhs, rhs);
      case BinopType::LOG_OR_OP:
        return IRBuilder_->CreateLogicalOr(lhs, rhs);
      case BinopType::EQL_OP:
        return IRBuilder_->CreateCmp(llvm::CmpInst::ICMP_EQ, lhs, rhs);
      }
    }(binop->getOpType());

    exprStack_.push({ exprType, nullptr, LLVMVal, ValueCategory::RVALUE });
  }

  void postVisit(const LetStatement *letStmt) override {
    ExprInfo rhs = exprStack_.top();
    exprStack_.pop();
    ExprInfo lhs = exprStack_.top();
    exprStack_.pop();
    semantic_->checkLetStatement(lhs.valCat, lhs.evalType, rhs.evalType,
                                 letStmt);
    llvm::Value* rhsVal = LValueToRValue(rhs);
    IRBuilder_->CreateStore(rhsVal, lhs.LLVMVal);
  }

  void interVisit(const IfStatement *ifStmt, unsigned visitCnt) override {
    auto &&parentFunc = IRBuilder_->GetInsertBlock()->getParent();
    switch(visitCnt) {
    case 1: {
      ExprInfo condExpr = exprStack_.top();
      exprStack_.pop();
      semantic_->checkIfStatement(condExpr.evalType, ifStmt);
      auto BBTrue = llvm::BasicBlock::Create(*LLVMCtx_, "", parentFunc);
      auto BBAfter = llvm::BasicBlock::Create(*LLVMCtx_, "", parentFunc);
      BBs.push(BBAfter);
      llvm::BasicBlock* BBNext = nullptr;
      if (ifStmt->getElseBody() != nullptr) {
        BBNext = llvm::BasicBlock::Create(*LLVMCtx_, "", parentFunc);
        BBs.push(BBNext);
      } else {
        BBNext = BBAfter;
      }
      llvm::Value* condVal = LValueToRValue(condExpr);
      IRBuilder_->CreateCondBr(condVal, BBTrue, BBNext);
      IRBuilder_->SetInsertPoint(BBTrue);
      break;
    }
    case 2: {
      auto BBNext = BBs.top();
      BBs.pop();
      auto BBAfter = BBs.top();
      IRBuilder_->CreateBr(BBAfter);
      IRBuilder_->SetInsertPoint(BBNext);
      break;
    }
    }
  }

  void postVisit(const IfStatement *ifStmt) override {
    auto BBNext = BBs.top();
    BBs.pop();
    IRBuilder_->CreateBr(BBNext);
    IRBuilder_->SetInsertPoint(BBNext);
  }

  void interVisit(const WhileStatement *whileStmt, unsigned visitCnt) override {
    auto &&parentFunc = IRBuilder_->GetInsertBlock()->getParent();
    switch(visitCnt) {
    case 1: {
      ExprInfo condExpr = exprStack_.top();
      exprStack_.pop();
      semantic_->checkWhileStatement(condExpr.evalType, whileStmt);
      auto BBAfter = llvm::BasicBlock::Create(*LLVMCtx_, "", parentFunc);
      auto BBNext = llvm::BasicBlock::Create(*LLVMCtx_, "", parentFunc);
      llvm::Value* condVal = LValueToRValue(condExpr);
      IRBuilder_->CreateCondBr(condVal, BBNext, BBAfter);
      IRBuilder_->SetInsertPoint(BBNext);
      BBs.push(BBAfter);
      BBs.push(BBNext);
    }
    }
  }

  void postVisit(const WhileStatement *whileStmt) override {
    whileStmt->getCondition()->accept(*this);
    ExprInfo updatedCondExpr = exprStack_.top();
    exprStack_.pop();
    auto BBLoop = BBs.top();
    BBs.pop();
    auto BBAfter = BBs.top();
    BBs.pop();
    llvm::Value* condVal = LValueToRValue(updatedCondExpr);
    IRBuilder_->CreateCondBr(condVal, BBLoop, BBAfter);
    IRBuilder_->SetInsertPoint(BBAfter);
  }

  void postVisit(const DoStatement *doStmt) override {
    exprStack_.pop();
    semantic_->checkDoStatement(doStmt);
  }

  void postVisit(const ReturnStatement *retStmt) override {
    ExprInfo retExpr;
    if (currentSubrtn_->getRetType() && !exprStack_.empty()) {
      retExpr = exprStack_.top();
      retExpr.LLVMVal = LValueToRValue(retExpr);
      exprStack_.pop();
    }
    semantic_->checkReturnStatement(retExpr.evalType, retStmt);
    if (currentSubrtn_->getRetType())
      IRBuilder_->CreateStore(retExpr.LLVMVal, returnVal_);
    IRBuilder_->CreateBr(BBReturn_);
    auto &&func = IRBuilder_->GetInsertBlock()->getParent();
    auto &&BBAfterRet = llvm::BasicBlock::Create(*LLVMCtx_, "", func);
    IRBuilder_->SetInsertPoint(BBAfterRet);
  }

private:
  llvm::Type* TypeToLLVMType(const Type *type) {
    switch (type->getTypeId()) {
    case Type::TypeId::IntTy:
      return llvm::Type::getInt32Ty(*LLVMCtx_);
    case Type::TypeId::CharTy:
      return llvm::Type::getInt8Ty(*LLVMCtx_);
    case Type::TypeId::BoolTy:
      return llvm::Type::getInt1Ty(*LLVMCtx_);
    case Type::TypeId::VoidTy:
      return llvm::Type::getVoidTy(*LLVMCtx_);
    case Type::TypeId::NullTy:
      return llvm::PointerType::get(*LLVMCtx_, 0);
    case Type::TypeId::ClassTy:
      return llvm::PointerType::get(*LLVMCtx_, 0);
    case Type::TypeId::ArrayTy:
      return llvm::PointerType::get(*LLVMCtx_, 0);
    default:
      return nullptr;
    }
  }

  llvm::Value *createMemberAccess(llvm::Value *memberBaseValue,
                                  ClassSymbol *classSym,
                                  FieldVarSymbol *FieldSym) {
    unsigned idx = FieldSym->getIndex();
    auto intType = llvm::Type::getInt32Ty(*LLVMCtx_);
    auto classType = ClassToLLVMType_[currentClass_->getClassType()];
    return IRBuilder_->CreateGEP(classType, memberBaseValue,
                          { llvm::ConstantInt::get(intType, 0),
                            llvm::ConstantInt::get(intType, idx) });
  }

  llvm::Value *createArrayMemberAccess(const Type *elemType,
                                       llvm::Value *arrayVal,
                                       llvm::Value *indexVal) {
    auto LLVMType = TypeToLLVMType(elemType);
    return IRBuilder_->CreateGEP(LLVMType, arrayVal, indexVal);
  }

  llvm::Function *createFunction(const std::string &name, const Type *retType,
                                 const std::vector<std::string> &argNames,
                                 const std::vector<const Type *> &argTypes) {
    std::vector<llvm::Type *> llvmArgTypes;
    llvmArgTypes.reserve(argTypes.size());
    for (auto *type : argTypes) {
      llvmArgTypes.push_back(TypeToLLVMType(type));
    }

    llvm::FunctionType *funcType = llvm::FunctionType::get(
        TypeToLLVMType(retType), llvmArgTypes, false);
    llvm::Function *func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, name, LLVMModule_.get());

    for (auto &&[arg, argName] : llvm::zip(func->args(), argNames)) {
      arg.setName(argName);
    }
    return func;
  }

  llvm::Function* declareNewArrayFunc() {
    static llvm::Function* newArrayFunc =
        createFunction("newArray", ArrayType::getArrayTy(Type::getCharTy()),
                       { "objectSize", "arraySize" },
                       { Type::getIntTy(), Type::getIntTy() });
    return newArrayFunc;
  }

  llvm::Function* declareDeleteArrayFunc() {
    static llvm::Function* deleteArrayFunc =
        createFunction("deleteArray", Type::getNullTy(), { "array" },
                       { ArrayType::getArrayTy(Type::getCharTy()) });
    return deleteArrayFunc;
  }

  llvm::Value* getSizeOf(const Type* type) {
    llvm::Type* llvmType =
        type->isClassTy() ?
        ClassToLLVMType_[static_cast<const ClassType*>(type)] : 
        TypeToLLVMType(type);
    auto intType = llvm::Type::getInt32Ty(*LLVMCtx_);
    auto sizeVal = IRBuilder_->CreateGEP(
        llvmType,
        llvm::Constant::getNullValue(llvm::PointerType::get(*LLVMCtx_, 0)),
        llvm::ConstantInt::get(intType, 1));
    return IRBuilder_->CreateCast(llvm::Instruction::PtrToInt, 
                                  sizeVal, intType);
  }

  llvm::Value* LValueToRValue(ExprInfo& expr) {
    if (expr.valCat == ValueCategory::LVALUE) {
      return IRBuilder_->CreateLoad(
          TypeToLLVMType(expr.evalType), expr.LLVMVal);
    }
    return expr.LLVMVal;
  }

  bool isLoadableType(const Type* type) {
    switch (type->getTypeId()) {
    case Type::TypeId::BoolTy:
    case Type::TypeId::CharTy:
    case Type::TypeId::IntTy:
      return true;
    default:
      return false;
    }
  }

  const Type *symbolToEvalType(SymbolNode* sym) {
    switch(sym->getSymbolType()) {
    case SymbolType::LOCAL_VARIABLE_SYM:
    case SymbolType::ARGUMENT_VARIABLE_SYM:
    case SymbolType::FIELD_VARIABLE_SYM:
    case SymbolType::STATIC_VARIABLE_SYM:
      return static_cast<VarSymbol *>(sym)->getVarType();
    default:
      return nullptr;
    }
  }

private:
  std::stack<ExprInfo> exprStack_;
  std::stack<llvm::BasicBlock*> BBs;
  std::unordered_map<SymbolNode*, llvm::AllocaInst*> symbolToAlloca_;
  std::unordered_map<const ClassType*, llvm::Type*> ClassToLLVMType_;
  std::unique_ptr<llvm::LLVMContext> LLVMCtx_;
  std::unique_ptr<llvm::Module> LLVMModule_;
  std::unique_ptr<llvm::IRBuilder<>> IRBuilder_;
  std::unique_ptr<SymbolTable> globalTable_;
  std::unique_ptr<SymbolTable> localTable_;
  std::unique_ptr<SemanticChecker> semantic_;
  std::shared_ptr<ClassSymbol> currentClass_;
  std::shared_ptr<SubroutineSymbol> currentSubrtn_;
  llvm::AllocaInst* returnVal_;
  llvm::BasicBlock* BBReturn_;
};