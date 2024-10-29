#pragma once
#include "SymbolTable.hpp"
#include "compiler/AST2.hpp"
#include "sstream"
#include <iostream>

enum class ValueCategory {
  NOT_VALUE,
  LVALUE,
  RVALUE,
};

class SemanticChecker final {
public:
  SemanticChecker(const SymbolTable *globalTable, const SymbolTable *localTable)
      : globalTable_(globalTable), localTable_(localTable) {}

  void setCurrentSubroutine(const SubroutineSymbol *curSubrtn) {
    currentSubrtn_ = curSubrtn;
  }

  void checkSubroutine(const SubroutineDec *subrtnNode,
                       const SubroutineSymbol *subrtnSym) {
    const Type *retType = subrtnSym->getRetType();
    if (!isTypeExist(retType)) {
      std::cerr << lineErrMsg(subrtnNode->getSourceLoc())
                << "Return type is undefined\n"
                << "Provided " << retType->toString() << "\n"
                << "In function " << subrtnSym->getName() << std::endl;
      throw std::runtime_error("Semantic error");
    }

    for (auto argIt = subrtnNode->args_begin();
         argIt != subrtnNode->args_end();
         ++argIt) {
      VariableDec* arg = *argIt;
      if (!isTypeExist(arg->getVarType())) {
        std::cerr << lineErrMsg(arg->getSourceLoc())
                  << "Argument type is undefined\n"
                  << "Provided " << retType->toString() << "\n"
                  << "In function " << subrtnSym->getName() << std::endl;
        throw std::runtime_error("Semantic error");
      }
    }
  }

  std::shared_ptr<SymbolNode> checkName(const ClassSymbol *currentClass,
                                        const NameExpr *nameExpr) {
    std::string name = nameExpr->getName();
    auto curSymbol = localTable_->find(name);
    if (curSymbol == nullptr) {
      curSymbol = currentClass->findMember(name);
    }
    if (curSymbol == nullptr) {
      curSymbol = globalTable_->find(name);
    }
    if (curSymbol == nullptr) {
      std::cerr << currentFuncNameErrMsg()
                << lineErrMsg(nameExpr->getSourceLoc())
                << "Undeclared name " << name << std::endl;
    }

    if (currentSubrtn_->getKind() != SubroutineKind::FUNCTION_S) {
      return curSymbol;
    }

    if (curSymbol->getSymbolType() == SymbolType::FIELD_VARIABLE_SYM) {
      printErrMsg("Cannot use field variables in static function",
                  nameExpr->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }
    if (curSymbol->getName() == "this") {
      printErrMsg("Cannot use \'this\' in static function",
                  nameExpr->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }

    return curSymbol;
  }

  SymbolNode *checkMemberOfValue(const Type *baseType,
                                 const MemberExpr *memberExpr) {
    if (isPrimitiveType(baseType)) {
        printErrMsg("Request for member in variable of primitive type",
                  memberExpr->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }
    auto className = baseType->getClassName();
    auto classTypeSym =
        std::static_pointer_cast<ClassSymbol>(globalTable_->find(className));
    auto memberSym = classTypeSym->findMember(memberExpr->getMember());
    if (memberSym == nullptr) {
      std::cerr << currentFuncNameErrMsg()
                << lineErrMsg(memberExpr->getSourceLoc())
                << "Class " << className << " do not have "
                << memberExpr->getMember() << " member" << std::endl;
      throw std::runtime_error("Semantic error");
    }
    return memberSym.get();
  }

  SymbolNode *checkMemberOfClass(SymbolNode *baseSym,
                                 const MemberExpr *memberExpr) {
    if (baseSym->getSymbolType() != SymbolType::CLASS_SYM) {
      printErrMsg("Request for member in something not a class",
                  memberExpr->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }

    std::string className = baseSym->getName();
    auto classTypeSym =
        std::static_pointer_cast<ClassSymbol>(globalTable_->find(className));
    auto memberSym = classTypeSym->findMember(memberExpr->getMember());
    if (memberSym == nullptr) {
      std::cerr << currentFuncNameErrMsg()
                << lineErrMsg(memberExpr->getSourceLoc())
                << "Class " << className << " do not have "
                << memberExpr->getMember() << " member" << std::endl;
      throw std::runtime_error("Semantic error");
    }

    bool staticMember = false;
    if (memberSym->getSymbolType() == SymbolType::STATIC_VARIABLE_SYM) {
      staticMember = true;
    }
    if (memberSym->getSymbolType() == SymbolType::SUBROUTINE_SYM) {
      auto subrtnSym = std::static_pointer_cast<SubroutineSymbol>(memberSym);
      staticMember = subrtnSym->getKind() == SubroutineKind::FUNCTION_S ||
                     subrtnSym->getKind() == SubroutineKind::CONSTRUCTOR_S;
    }
    if (!staticMember) {
      printErrMsg(
          "Cannot use non-static or non-constructor member with class name",
          memberExpr->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }

    return memberSym.get();
  }

  void checkArrayMember(const Type *nameType, const Type *indexType,
                        const ArrayMemberExpr *arrayExpr) {
    if (!nameType->isArrayTy()) {
      printErrMsg("Subscripted object is not \"Array\" type",
                  arrayExpr->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }

    if (!indexType->isIntTy()) {
      printErrMsg("Array index expression is not an integer",
                  arrayExpr->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }
  }

  SubroutineSymbol *checkSubroutineCall(SymbolNode *nameSym,
        const CallExpr *subCall,
        std::vector<const Type *>::const_iterator providedArgBegin,
        std::vector<const Type *>::const_iterator providedArgEnd) {
    if (nameSym->getSymbolType() != SymbolType::SUBROUTINE_SYM) {
      printErrMsg("Called object is not a subroutine", subCall->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }

    auto subSym = static_cast<SubroutineSymbol *>(nameSym);
    unsigned subArgsCount = subSym->getArgsCount();
    unsigned callArgsCount = subCall->getArgsCount();
    if (subArgsCount != callArgsCount) {
      std::cerr << currentFuncNameErrMsg()
                << lineErrMsg(subCall->getSourceLoc())
                << "Argument count mismatch\n"
                << "Provided " << callArgsCount << "\n"
                << "In function " << subSym->getName()
                << " declared arguments count is " << subSym->getArgsCount()
                << std::endl;
      throw std::runtime_error("Semantic error");
    }

    for (auto providedIt = providedArgBegin,
         definedIt = subSym->arg_type_begin();
         providedIt != providedArgEnd;
         ++providedIt, ++definedIt) {
      if (!Type::isAssignable(*providedIt, *definedIt)) {
        std::cerr << currentFuncNameErrMsg()
                  << lineErrMsg(nameSym->getSourceLocation())
                  << "Argument type mismatch\n"
                  << "Provided argument have type: " << (*providedIt)->toString()
                  << "\n"
                  << "In function " << subSym->getName() << " declared type is "
                  << (*definedIt)->toString() << std::endl;
        throw std::runtime_error("Semantic error");
      }
    }

    return subSym;
  }

  void checkNewArray(const Type *arrayElementType,const Type *sizeExprType,
                     const NewArrayExpr *newExpr) {
    if (!isTypeExist(arrayElementType)) {
      std::cerr << lineErrMsg(newExpr->getSourceLoc())
                << "First argument of \'newArray\' must be typename\n"
                << "Provided " << arrayElementType->toString() << std::endl;
      throw std::runtime_error("Semantic error");
    }

    if (arrayElementType->isArrayTy()) {
      std::cerr << lineErrMsg(newExpr->getSourceLoc())
                << "First argument of \'newArray\' must be non-array typename\n"
                << "Provided " << arrayElementType->toString() << std::endl;
      throw std::runtime_error("Semantic error");
    }

    if (!sizeExprType->isIntTy()) {
      std::cerr << lineErrMsg(newExpr->getSourceLoc())
                << "Second argument of \'newArray\' must be of type int\n"
                << "Provided " << sizeExprType->toString() << std::endl;
      throw std::runtime_error("Semantic error");
    }
  }

  void checkDeleteArray(const Type *deleteExprType,
                        const DeleteArrayExpr *delExpr) {
    if (!deleteExprType->isArrayTy()) {
      std::cerr << lineErrMsg(delExpr->getSourceLoc())
                << "Argument of \'deleteArray\' must be \"Array\" type\n"
                << "Provided " << deleteExprType->toString() << std::endl;
      throw std::runtime_error("Semantic error");
    }
  }

  void checkUnop(const Type *operandType, const UnopExpr *unop) {
    if (operandType == nullptr) {
      printErrMsg("Operand of unary operation must be value",
                  unop->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }
    if (!operandType->isBoolTy() && !operandType->isIntTy()) {
      std::cerr << currentFuncNameErrMsg()
                << lineErrMsg(unop->getSourceLoc())
                << "Invalid operand to unary operation\n"
                << "Operand have type: " << operandType->toString()
                << std::endl;
      throw std::runtime_error("Semantic error");
    }
  }

  void checkBinop(const Type *firstOpType, const Type *secondOpType,
                  const BinopExpr *binop) {
    const Type *operandsType = nullptr;
    BinopType binopType = binop->getOpType();
    switch (binopType) {
    case BinopType::ADD_OP:
    case BinopType::SUB_OP:
    case BinopType::DIV_OP:
    case BinopType::MUL_OP:
    case BinopType::BIT_AND_OP:
    case BinopType::BIT_OR_OP:
    case BinopType::LSS_OP:
    case BinopType::GTR_OP:
      operandsType = Type::getIntTy();
      break;
    case BinopType::LOG_AND_OP:
    case BinopType::LOG_OR_OP:
      operandsType = Type::getBoolTy();
      break;
    case BinopType::EQL_OP:
      operandsType = firstOpType;
      break;
    }

    if (!firstOpType || !secondOpType) {
      printErrMsg("Operands of a binary operation are not a value",
                  binop->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }

    if (!Type::isAssignable(firstOpType, operandsType) ||
        !Type::isAssignable(secondOpType, operandsType)) {
      std::cerr << currentFuncNameErrMsg()
                << lineErrMsg(binop->getSourceLoc())
                << "Invalid types of operands in binary operation\n"
                << "Left operand have type: " << firstOpType->toString() << "\n"
                << "Right operand have type: " << secondOpType->toString()
                << std::endl;
      throw std::runtime_error("Semantic error");
    }
  }

  void checkVariableDec(const VariableDec *var) {
    auto varType = var->getVarType();
    if (!isTypeExist(varType)) {
      std::cerr << lineErrMsg(var->getSourceLoc())
                << "Undefined type: " << varType->toString() << std::endl;
      throw std::runtime_error("Semantic error");
    }
  }

  void checkLetStatement(ValueCategory lhsCategory, const Type *lhsType,
                         const Type *rhsType, const LetStatement *letStmt) {
    if (lhsCategory != ValueCategory::LVALUE) {
      auto lhsNode = letStmt->getLhs();
      printErrMsg("Lvalue required as left operand of let-statement",
                  lhsNode->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }
    if (!rhsType) {
      auto rhsNode = letStmt->getRhs();
      printErrMsg("Right operand of let-statement must be value",
                  rhsNode->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }
    if (!Type::isAssignable(lhsType, rhsType)) {
      std::cerr << currentFuncNameErrMsg()
                << lineErrMsg(letStmt->getSourceLoc())
                << "Operands type mismatch in let-statement\n"
                << "Left operand have type: " << lhsType->toString() << '\n'
                << "Right operand have type: " << rhsType->toString()
                << std::endl;
      throw std::runtime_error("Semantic error");
    }
  }

  void checkIfStatement(const Type *conditionType, const IfStatement *ifStmt) {
    if (!conditionType->isBoolTy()) {
      auto conditionNode = ifStmt->getCondition();
      printErrMsg("Condition expression in if-statement must be bool type",
                  conditionNode->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }
  }

  void checkWhileStatement(const Type *conditionType,
                           const WhileStatement *whileStmt) {
    if (!conditionType->isBoolTy()) {
      auto conditionNode = whileStmt->getCondition();
      printErrMsg("Condition expression in while-statement must be bool type",
                  conditionNode->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }
  }

  void checkDoStatement(const DoStatement *doStmt) {
    auto subCallNode = doStmt->getCallExpr();
    if (subCallNode->getNodeType() != NodeType::CALL_EXPR) {
      printErrMsg("Do statement must contain call expression",
                  doStmt->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }
  }

  void checkReturnStatement(const Type *retExprType,
                            const ReturnStatement *retStmt) {
    const Type* subrtnRetType = currentSubrtn_->getRetType();

    if (subrtnRetType->isVoidTy() && !retExprType)
      return;

    if (subrtnRetType->isVoidTy() && !retExprType) {
      printErrMsg("Returning value in void function", retStmt->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }
    if (subrtnRetType->isVoidTy() && !retExprType) {
      printErrMsg("No return value in non-void function",
                  retStmt->getSourceLoc());
      throw std::runtime_error("Semantic error");
    }

    if (!Type::isAssignable(retExprType, subrtnRetType)) {
      std::cerr << currentFuncNameErrMsg()
                << lineErrMsg(retStmt->getSourceLoc())
                << "Return statement with " << retExprType->toString()
                << " expression, "
                << "in function returning " << subrtnRetType->toString()
                << std::endl;
      throw std::runtime_error("Semantic error");
    }
  }

  bool isTypeExist(const Type *type) {
    if (type->isArrayTy()) {
      type = type->getArrayElementType();
    }
    if (!isPrimitiveType(type) &&
        (type->isClassTy() &&
         globalTable_->find(type->getClassName()) == nullptr)) {
      return false;
    }
    return true;
  }

  bool isPrimitiveType(const Type *type) {
    switch (type->getTypeId()) {
    case Type::TypeId::BoolTy:
    case Type::TypeId::CharTy:
    case Type::TypeId::IntTy:
    case Type::TypeId::VoidTy:
    case Type::TypeId::NullTy:
    case Type::TypeId::ArrayTy:
      return true;
    default:
      return false;
    }
  }

  void printErrMsg(const char *msg, SourceLocation srcLoc) {
    std::cerr << currentFuncNameErrMsg() << lineErrMsg(srcLoc)
              << msg << std::endl;
  }

  std::string currentFuncNameErrMsg() {
    return "In function " + currentSubrtn_->getName() + "\n";
  }

  std::string lineErrMsg(SourceLocation srcLoc) {
    std::stringstream msg;
    msg << "On line " << srcLoc.getLinePos() << ":" 
        << srcLoc.getInLinePos() << '\n';
    return msg.str();
  }

private:
  const SymbolTable *globalTable_ = nullptr;
  const SymbolTable *localTable_ = nullptr;
  const SubroutineSymbol *currentSubrtn_ = nullptr;
};