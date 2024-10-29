#pragma once
#include "SymbolTable.hpp"
#include "Visitor.hpp"
#include "compiler/AST2.hpp"
#include "compiler/NodeDescriptors.hpp"
#include "compiler/Type.hpp"
#include <cassert>
#include <memory>
#include <string>

class SymbolsVisitor final : public Visitor {
public:
  SymbolsVisitor(SymbolTable *globalTable)
    : globalTable_(globalTable) {}

  void preVisit(const ClassDec *cl) override {
    currentClass_ =
        std::make_shared<ClassSymbol>(cl->getName(), 
                                      ClassType::getClassTy(cl->getName()),
                                      cl->getSourceLoc());
    unsigned fieldVarIndex = 0;
    for (auto it = cl->var_begin();
         it != cl->var_end();
         ++it, ++fieldVarIndex) {
      VariableDec* var = *it;
      FieldVarSymbol varSym(var->getVarName(), var->getVarType(),
                            currentClass_.get(), fieldVarIndex, 
                            var->getSourceLoc());
      currentClass_->addSymbol(var->getVarName(),
                               std::make_shared<FieldVarSymbol>(varSym));
    }
  }

  void preVisit(const SubroutineDec *subDec) override {
    std::vector<const Type *> argTypes;
    for (auto it = subDec->args_begin(); it != subDec->args_end(); ++it) {
      argTypes.push_back((*it)->getVarType());
    }
    std::string subName = subDec->getName();
    SubroutineSymbol subSym(subName, subDec->getReturnType(), std::move(argTypes),
                            toSubroutineKind(subDec), subDec->getSourceLoc());
    currentClass_->addSymbol(subName,
                             std::make_shared<SubroutineSymbol>(subSym));
  }

  void preVisit(const StaticVarDec *var) override {
    StaticVarSymbol varSym(var->getVarName(), var->getVarType(),
                                currentClass_.get(), var->getSourceLoc());
    currentClass_->addSymbol(var->getVarName(),
                              std::make_shared<StaticVarSymbol>(varSym));
  }

  void postVisit(const ClassDec *cl) override {
    globalTable_->insert(currentClass_->getName(), currentClass_);
    currentClass_ = nullptr;
  }

  SymbolTable *getGlobalTable() { return globalTable_; }

private:
  SubroutineKind toSubroutineKind(const SubroutineDec* subrtn) {
    switch (subrtn->getNodeType()) {
    case NodeType::METHOD_DEC:
      return SubroutineKind::METHOD_S;
    case NodeType::CONSTRUCTOR_DEC:
      return SubroutineKind::CONSTRUCTOR_S;
    case NodeType::FUNCTION_DEC:
      return SubroutineKind::FUNCTION_S;
    default:
      assert(false && "unreachable");
    }
  }

private:
  std::shared_ptr<ClassSymbol> currentClass_ = nullptr;
  SymbolTable *globalTable_;
};