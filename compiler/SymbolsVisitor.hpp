#pragma once
#include "AST.hpp"
#include "SymbolTable.hpp"
#include "Visitor.hpp"
#include <string>

class symbolsVisitor final : public Visitor {
public:
  symbolsVisitor(SymbolTable *globalTable)
      : currentClass_(std::make_shared<ClassSymbol>()),
        globalTable_(globalTable) {}

  void preVisit(ClassDec *cl) override {
    currentClass_->setName(cl->getName());
    currentClass_->setLinePos(cl->getSourceLoc().getLinePos());
    currentClass_->setInLinePos(cl->getSourceLoc().getInLinePos());
  }

  void preVisit(SubroutineDec *subDec) override {
    auto argsList = subDec->child(0);
    std::vector<Type *> argTypes;
    for (unsigned i = 0; i < argsList->children(); i++) {
      auto arg = static_cast<VariableDec *>(argsList->child(i));
      Type *type = arg->getVarType();
      argTypes.push_back(type);
    }

    std::string subName = subDec->getName();
    SubroutineSymbol subSym(subName, subDec->getRetType(), argTypes,
                            subDec->getSubroutineKind(), subDec->getLinePos(),
                            subDec->getInLinePos());
    currentClass_->addSymbol(subName,
                             std::make_shared<SubroutineSymbol>(subSym));
  }

  void preVisit(FieldVariableList *memberList) override {
    for (unsigned idx = 0; idx < memberList->children(); idx++) {
      auto var = std::static_pointer_cast<VariableDec>(memberList->child(idx));
      FieldVariableSymbol varSym(var->getVarName(), var->getVarType(),
                                 currentClass_.get(), idx, var->getLinePos(),
                                 var->getInLinePos());
      currentClass_->addSymbol(var->getVarName(),
                               std::make_shared<FieldVariableSymbol>(varSym));
    }
  }

  void preVisit(StaticVariableList *memberList) override {
    for (unsigned idx = 0; idx < memberList->children(); idx++) {
      auto var = std::static_pointer_cast<VariableDec>(memberList->child(idx));
      StaticVariableSymbol varSym(var->getVarName(), var->getVarType(),
                                  currentClass_.get(), var->getLinePos(),
                                  var->getInLinePos());
      currentClass_->addSymbol(var->getVarName(),
                               std::make_shared<StaticVariableSymbol>(varSym));
    }
  }

  void postVisit(ClassDec *cl) override {
    globalTable_->insert(currentClass_->getName(), currentClass_);
    currentClass_ = std::make_shared<ClassSymbol>();
  }

  SymbolTable *getGlobalTable() { return globalTable_; }

private:
  std::shared_ptr<ClassSymbol> currentClass_;
  SymbolTable *globalTable_;
};