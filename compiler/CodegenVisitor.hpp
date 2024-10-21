#pragma once
#include "SemanticChecker.hpp"
#include "SymbolsVisitor.hpp"
#include "Visitor.hpp"
#include "llvm/IR/Value.h"
#include <stack>
#include <string>

class CodegenVisitor final : public Visitor {
  struct ExprInfo {
    Type *evalType;
    SymbolNode *sym;
    llvm::Value *llvmValue;
    ValueCategory valCat;
  };

public:
  CodegenVisitor()
      : globalTable_(std::make_unique<SymbolTable>()),
        localTable_(std::make_unique<SymbolTable>()),
        semantic_(std::make_unique<SemanticChecker>(globalTable_.get(),
                                                    localTable_.get())) {}

  void preVisit(Program *prog) override {
    symbolsVisitor symVisitor(globalTable_.get());
    prog->accept(symVisitor);
  }

  void preVisit(ClassDec *cl) override {
    std::string className = cl->getName();
    currentClass_ =
        std::static_pointer_cast<ClassSymbol>(globalTable_->find(className));
  }

  void preVisit(SubroutineDec *subDec) override {
    std::string subName = subDec->getName();
    currentSubrtn_ = std::static_pointer_cast<SubroutineSymbol>(
        currentClass_->findMember(subName));
    semantic_->setCurrentSubroutine(currentSubrtn_.get());
    SubroutineKind subKind = currentSubrtn_->getKind();
    if (subKind == SubroutineKind::CONSTRUCTOR_S ||
        subKind == SubroutineKind::METHOD_S) {
      auto thisSym = std::make_shared<ArgumentVariableSymbol>(
          "this", currentClass_->getClassType(), subDec->getLinePos(),
          subDec->getInLinePos());
      if (subKind == SubroutineKind::CONSTRUCTOR_S) {
        thisSym->setMode(VarModifier::LOCAL_V);
      } else if (subKind == SubroutineKind::METHOD_S) {
        thisSym->setMode(VarModifier::ARG_V);
      }
      localTable_->insert("this", thisSym);
    }
  }

  void postVisit(SubroutineDec *subDec) {
    localTable_->clear();
    currentSubrtn_ = nullptr;
  }

  void preVisit(VariableDecList *varList) override {
    for (unsigned i = 0; i < varList->children(); i++) {
      auto var = std::static_pointer_cast<VariableDec>(varList->child(i));
      VarSymbol varSym(var->getVarName(), var->getVarType(), var->getLinePos(),
                       var->getInLinePos());
      localTable_->insert(var->getVarName(),
                          std::make_shared<VarSymbol>(varSym));
    }
  }

  void postVisit(NameExpr *nameExpr) override {
    std::string name = nameExpr->getName();
    auto curSymbol = semantic_->checkName(currentClass_.get(), nameExpr);

    Type *evalType = symbolToEvalType(curSymbol.get());
    ValueCategory valueCat = ValueCategory::NOT_VALUE;
    if (evalType)
      valueCat = ValueCategory::LVALUE;

    exprStack_.push({evalType, curSymbol.get(), valueCat});
  }

  void postVisit(NewArrayExpr *newArrExpr) override {
    Type *elementType = newArrExpr->getArrayType();
    ExprInfo newArraySizeExpr = exprStack_.top();
    exprStack_.pop();
    semantic_->checkNewArray(elementType, newArraySizeExpr.evalType,
                             newArrExpr);
    exprStack_.push(
        {ArrayType::getArrayTy(elementType), nullptr, ValueCategory::RVALUE});
  }

  void postVisit(DeleteArrayExpr *delArrExpr) override {
    ExprInfo newArraySizeExpr = exprStack_.top();
    exprStack_.pop();
    semantic_->checkDeleteArray(newArraySizeExpr.evalType, delArrExpr);
  }

  void postVisit(LiteralExpr *litExpr) override {
    exprStack_.push(
        {litExpr->getLiteralType(), nullptr, ValueCategory::RVALUE});
  }

  void postVisit(MemberExpr *memberExpr) override {
    ExprInfo nameInfo = exprStack_.top();
    exprStack_.pop();

    auto memberSym = semantic_->checkMember(nameInfo.sym, memberExpr);
    Type *evalType = symbolToEvalType(memberSym);
    ValueCategory valueCat = ValueCategory::NOT_VALUE;
    if (!evalType)
      valueCat = ValueCategory::LVALUE;

    exprStack_.push({evalType, memberSym, valueCat});
  }

  void postVisit(ArrayMemberExpr *arrayExpr) override {
    ExprInfo indexInfo = exprStack_.top();
    exprStack_.pop();
    ExprInfo nameInfo = exprStack_.top();
    exprStack_.pop();
    semantic_->checkArrayMember(nameInfo.evalType, indexInfo.evalType,
                                arrayExpr);

    exprStack_.push({nameInfo.evalType->getArrayElementType(), nullptr,
                     ValueCategory::LVALUE});
  }

  void postVisit(SubroutineCallExpr *subCall) override {
    std::vector<Type *> argTypes(subCall->getArgCount());
    for (auto argIt = argTypes.rbegin(), endIt = argTypes.rend();
         argIt != endIt; ++argIt) {
      *argIt = exprStack_.top().evalType;
      exprStack_.pop();
    }

    ExprInfo nameInfo = exprStack_.top();
    exprStack_.pop();
    SubroutineSymbol *subSym = semantic_->checkSubroutineCall(
        nameInfo.sym, subCall, argTypes.begin(), argTypes.end());

    exprStack_.push({subSym->getRetType(), nullptr, ValueCategory::RVALUE});
  }

  void postVisit(UnopExpr *unop) override {
    ExprInfo expr = exprStack_.top();
    exprStack_.pop();
    semantic_->checkUnop(expr.evalType, unop);

    exprStack_.push({expr.evalType, nullptr, ValueCategory::RVALUE});
  }

  void postVisit(BinopExpr *binop) override {
    ExprInfo secondOp = exprStack_.top();
    exprStack_.pop();
    ExprInfo firstOp = exprStack_.top();
    exprStack_.pop();

    semantic_->checkBinop(firstOp.evalType, secondOp.evalType, binop);

    Type *exprType = nullptr;
    switch (binop->getOpType()) {
    case OpType::ADD_OP:
    case OpType::SUB_OP:
    case OpType::DIV_OP:
    case OpType::MUL_OP:
    case OpType::BIT_AND_OP:
    case OpType::BIT_OR_OP:
      exprType = Type::getIntTy();
      break;
    case OpType::LSS_OP:
    case OpType::GTR_OP:
    case OpType::LOG_AND_OP:
    case OpType::LOG_OR_OP:
    case OpType::EQL_OP:
      exprType = Type::getBoolTy();
      break;
    }

    exprStack_.push({exprType, nullptr, ValueCategory::RVALUE});
  }

  void preVisit(VariableDec *var) override {
    semantic_->checkVariableDec(var->getVarType(), var);
  }

  void postVisit(LetStatement *letStmt) override {
    ExprInfo rhs = exprStack_.top();
    exprStack_.pop();
    ExprInfo lhs = exprStack_.top();
    exprStack_.pop();
    semantic_->checkLetStatement(lhs.valCat, lhs.evalType, rhs.evalType,
                                 letStmt);
  }

  void postVisit(IfStatement *ifStmt) override {
    ExprInfo condExpr = exprStack_.top();
    exprStack_.pop();
    semantic_->checkIfStatement(condExpr.evalType, ifStmt);
  }

  void postVisit(WhileStatement *whileStmt) override {
    ExprInfo condExpr = exprStack_.top();
    exprStack_.pop();
    semantic_->checkWhileStatement(condExpr.evalType, whileStmt);
  }

  void postVisit(DoStatement *doStmt) override {
    exprStack_.pop();
    semantic_->checkDoStatement(doStmt);
  }

  void postVisit(ReturnStatement *retStmt) override {
    Type *retExprType = nullptr;
    if (!exprStack_.empty()) {
      retExprType = exprStack_.top().evalType;
      exprStack_.pop();
    }
    semantic_->checkReturnStatement(retExprType, retStmt);
  }

private:
  Type *symbolToEvalType(SymbolNode *sym) {
    Type *evalType = nullptr;
    if (sym->getSymbolType() == SymbolType::VARIABLE_SYM) {
      evalType = static_cast<VarSymbol *>(sym)->getVarType();
    }
    return evalType;
  }

private:
  std::stack<ExprInfo> exprStack_;
  std::unique_ptr<SymbolTable> globalTable_;
  std::unique_ptr<SymbolTable> localTable_;
  std::unique_ptr<SemanticChecker> semantic_;
  std::shared_ptr<ClassSymbol> currentClass_;
  std::shared_ptr<SubroutineSymbol> currentSubrtn_;
};