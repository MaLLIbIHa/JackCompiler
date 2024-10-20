#pragma once
#include "sstream"
#include "SymbolTable.hpp"
#include "AST.hpp"

enum class ValueCategory {
	NOT_VALUE,
	LVALUE,
	RVALUE,
};

class SemanticChecker final {
public:
	SemanticChecker(const SymbolTable *globalTable,
					 const SymbolTable *localTable)
		: globalTable_(globalTable),
		  localTable_(localTable) {}

	void setCurrentSubroutine(SubroutineSymbol* curSubrtn) { currentSubrtn_ = curSubrtn; }

	void checkSubroutine(SubroutineDec* subrtnNode,
						 SubroutineSymbol* subrtnSym) {
		Type* retType = subrtnSym->getRetType();
		if (!isTypeExist(retType)) {
			std::cerr << lineErrMsg(subrtnNode->getLinePos(),
																subrtnNode->getInLinePos())
				<< "Return type is undefined\n"
				<< "Provided " << retType->toString() << "\n"
				<< "In function " << subrtnSym->getName() << std::endl;
			throw std::runtime_error("Semantic error");
		}

		auto argListNode = subrtnNode->child(0);
		unsigned argsCount = subrtnSym->getArgCount();
		for (unsigned i = 0; i < argsCount; i++) {
			Type* argType = subrtnSym->getArgType(i);
			if (!isTypeExist(argType)) {
				unsigned linePos = argListNode->child(i)->getLinePos();
				unsigned inLinePos = argListNode->child(i)->getInLinePos();
				std::cerr << lineErrMsg(linePos, inLinePos)
					<< "Argument type is undefined\n"
					<< "Provided " << retType->toString() << "\n"
					<< "In function " << subrtnSym->getName() << std::endl;
				throw std::runtime_error("Semantic error");
			}
		}
	}

	std::shared_ptr<SymbolNode> checkName(const ClassSymbol *currentClass,
										  NameExpr* nameExpr) {
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
				<< lineErrMsg(nameExpr->getLinePos(), nameExpr->getInLinePos())
				<< "Undeclared name " << name << std::endl;
		}

		if (currentSubrtn_->getKind() != SubroutineKind::FUNCTION_S) {
			return curSymbol;
		}

		if (curSymbol->getSymbolType() == SymbolType::FIELD_VARIABLE_SYM) {
			printErrMsg("Cannot use field variables in static function",
				nameExpr->getLinePos(),
				nameExpr->getInLinePos());
			throw std::runtime_error("Semantic error");
		}
		if (curSymbol->getName() == "this") {
			printErrMsg("Cannot use \'this\' in static function",
				nameExpr->getLinePos(),
				nameExpr->getInLinePos());
			throw std::runtime_error("Semantic error");
		}
		
		return curSymbol;
	}

	SymbolNode* checkMember(SymbolNode *baseSym,
							MemberExpr *memberExpr) {
		if (baseSym->getSymbolType() == SymbolType::SUBROUTINE_SYM) {
			printErrMsg("Request for member in something not a class",
					memberExpr->getLinePos(),
					memberExpr->getInLinePos());
			throw std::runtime_error("Semantic error");
		}
		
		std::string className;
		if (baseSym->getSymbolType() == SymbolType::CLASS_SYM) {
			className = baseSym->getName();
		} else {
			Type* nameType = static_cast<VarSymbol*>(baseSym)->getVarType();
			if (isPrimitiveType(nameType)) {
				printErrMsg("Request for member in variable of primitive type",
						memberExpr->getLinePos(),
						memberExpr->getInLinePos());
				throw std::runtime_error("Semantic error");
			}
			className = nameType->getClassName();
		}

		auto classTypeSym = std::static_pointer_cast<ClassSymbol>(globalTable_->find(className));

		auto memberSym = classTypeSym->findMember(memberExpr->getMember());
		if (memberSym == nullptr) {
			std::cerr << currentFuncNameErrMsg()
				<< lineErrMsg(memberExpr->getLinePos(), memberExpr->getInLinePos())
				<< "Class " << className << " do not have "
				<< memberExpr->getMember() << " member" << std::endl;
			throw std::runtime_error("Semantic error");
		}

		if (baseSym->getSymbolType() == SymbolType::CLASS_SYM) {
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
				printErrMsg("Cannot use non-static or non-constructor member with class name",
					memberExpr->getLinePos(),
					memberExpr->getInLinePos());
				throw std::runtime_error("Semantic error");
			}
		}

		return memberSym.get();
	}

	void checkArrayMember(Type* nameType,
							Type* indexType,
							ArrayMemberExpr *arrayExpr) {
		if (!nameType->isArrayTy()) {
			printErrMsg("Subscripted object is not \"Array\" type",
				arrayExpr->getLinePos(),
				arrayExpr->getInLinePos());
			throw std::runtime_error("Semantic error");
		}

		if (!indexType->isIntTy()) {
			printErrMsg("Array index expression is not an integer",
				arrayExpr->getLinePos(),
				arrayExpr->getInLinePos());
			throw std::runtime_error("Semantic error");
		}
	}

	SubroutineSymbol *checkSubroutineCall(SymbolNode *nameSym,
											 SubroutineCallExpr *subCall,
											 std::vector<Type*>::iterator argBegin,
											 std::vector<Type*>::iterator argEnd) {
		if (nameSym->getSymbolType() != SymbolType::SUBROUTINE_SYM) {
			printErrMsg("Called object is not a subroutine",
						  subCall->getLinePos(),
						  subCall->getInLinePos());
			throw std::runtime_error("Semantic error");
		}

		auto subSym = static_cast<SubroutineSymbol*>(nameSym);
		unsigned subArgsCount = subSym->getArgCount();
		unsigned callArgsCount = subCall->getArgCount();
		if (subArgsCount != callArgsCount) {
			std::cerr << currentFuncNameErrMsg()
				<< lineErrMsg(subCall->getLinePos(), subCall->getInLinePos())
				<< "Argument count mismatch\n"
				<< "Provided " << callArgsCount << "\n"
				<< "In function " << subSym->getName()
				<< " declared arguments count is " << subSym->getArgCount() << std::endl;
			throw std::runtime_error("Semantic error");
		}

		unsigned i = 0;
		for (auto&& argIt = argBegin; argBegin != argEnd; ++argIt, ++i) {
			if (!Type::isAssignable(*argIt, subSym->getArgType(i))) {
				auto argNode = subCall->child(1)->child(i);
				std::cerr << currentFuncNameErrMsg()
					<< lineErrMsg(argNode->getLinePos(), argNode->getInLinePos())
					<< "Argument type mismatch\n"
					<< "Provided argument have type: " << (*argIt)->toString() << "\n"
					<< "In function " << subSym->getName()
					<< " declared type is " << subSym->getArgType(i)->toString() << std::endl;
				throw std::runtime_error("Semantic error");
			}
		}

		return subSym;
	}

	void checkNewArray(Type* arrayElementType, Type* sizeExprType,
										 NewArrayExpr* newExpr) {
		if (!isTypeExist(arrayElementType)) {
			std::cerr << lineErrMsg(newExpr->getLinePos(),
															newExpr->getInLinePos())
								<< "First argument of \'newArray\' must be typename\n"
								<< "Provided " << arrayElementType->toString() << std::endl;
			throw std::runtime_error("Semantic error");
		}

		if (arrayElementType->isArrayTy()) {
			std::cerr << lineErrMsg(newExpr->getLinePos(),
				newExpr->getInLinePos())
				<< "First argument of \'newArray\' must be non-array typename\n"
				<< "Provided " << arrayElementType->toString() << std::endl;
			throw std::runtime_error("Semantic error");
		}

		if (!sizeExprType->isIntTy()) {
			std::cerr << lineErrMsg(newExpr->getLinePos(),
																newExpr->getInLinePos())
								<< "Second argument of \'newArray\' must be of type int\n"
								<< "Provided " << sizeExprType->toString() << std::endl;
			throw std::runtime_error("Semantic error");
		}
	}

	void checkDeleteArray(Type* deleteExprType, DeleteArrayExpr* delExpr) {
		if (!deleteExprType->isArrayTy()) {
			std::cerr << lineErrMsg(delExpr->getLinePos(),
															delExpr->getInLinePos())
								<< "Argument of \'deleteArray\' must be \"Array\" type\n"
								<< "Provided " << deleteExprType->toString() << std::endl;
			throw std::runtime_error("Semantic error");
		}
	}

	void checkUnop(Type* operandType,
								 UnopExpr *unop) {
		if (operandType == nullptr) {
			printErrMsg("Operand of unary operation must be value",
				unop->getLinePos(),
				unop->getInLinePos());
			throw std::runtime_error("Semantic error");
		}
		if (!operandType->isBoolTy() && !operandType->isIntTy()) {
			std::cerr << currentFuncNameErrMsg()
				<< lineErrMsg(unop->getLinePos(), unop->getInLinePos())
				<< "Invalid operand to unary operation\n"
				<< "Operand have type: " << operandType->toString() << std::endl;
			throw std::runtime_error("Semantic error");
		}
	}

	void checkBinop(Type* firstOpType,
					 				Type* secondOpType,
					 				BinopExpr *binop) {
		Type* operandsType = nullptr;
		OpType binopType = binop->getOpType();
		switch (binopType) {
		case OpType::ADD_OP:
		case OpType::SUB_OP:
		case OpType::DIV_OP:
		case OpType::MUL_OP:
		case OpType::BIT_AND_OP:
		case OpType::BIT_OR_OP:
		case OpType::LSS_OP:
		case OpType::GTR_OP:
			operandsType = Type::getIntTy();
			break;
		case OpType::LOG_AND_OP:
		case OpType::LOG_OR_OP:
			operandsType = Type::getBoolTy();
			break;
		case OpType::EQL_OP:
			operandsType = firstOpType;
			break;
		}

		if (!firstOpType || !secondOpType) {
			printErrMsg("Operands of a binary operation are not a value",
				binop->getLinePos(), binop->getInLinePos());
			throw std::runtime_error("Semantic error");
		}

		if (!Type::isAssignable(firstOpType, operandsType) ||
				!Type::isAssignable(secondOpType, operandsType)) {
			std::cerr << currentFuncNameErrMsg()
				<< lineErrMsg(binop->getLinePos(), binop->getInLinePos())
				<< "Invalid types of operands in binary operation\n"
				<< "Left operand have type: " << firstOpType->toString() << "\n"
				<< "Right operand have type: " << secondOpType->toString() << std::endl;
			throw std::runtime_error("Semantic error");
		}
	}

	void checkVariableDec(Type* varType, VariableDec *var) {
		if (!isTypeExist(varType)) {
			std::cerr << lineErrMsg(var->getLinePos(), var->getInLinePos())
				<< "Undefined type: " << varType->toString() << std::endl;
			throw std::runtime_error("Semantic error");
		}
	}

	void checkLetStatement(ValueCategory lhsCategory,
						   					 Type* lhsType,
						   					 Type* rhsType,
						   					 LetStatement *letStmt) {
		if (lhsCategory != ValueCategory::LVALUE) {
			auto lhsNode = letStmt->child(0);
			printErrMsg("Lvalue required as left operand of let-statement",
				lhsNode->getLinePos(),
				lhsNode->getInLinePos());
			throw std::runtime_error("Semantic error");
		}
		if (!rhsType) {
			auto rhsNode = letStmt->child(1);
			printErrMsg("Right operand of let-statement must be value",
				rhsNode->getLinePos(),
				rhsNode->getInLinePos());
			throw std::runtime_error("Semantic error");
		}
		if (!Type::isAssignable(lhsType, rhsType)) {
			std::cerr << currentFuncNameErrMsg()
				<< lineErrMsg(letStmt->getLinePos(), letStmt->getInLinePos())
				<< "Operands type mismatch in let-statement\n"
				<< "Left operand have type: " << lhsType->toString() << '\n'
				<< "Right operand have type: " << rhsType->toString() << std::endl;
			throw std::runtime_error("Semantic error");
		}
	}

	void checkIfStatement(Type* conditionType, 
												IfStatement *ifStmt) {
		if (!conditionType->isBoolTy()) {
			auto conditionNode = ifStmt->child(0);
			printErrMsg("Condition expression in if-statement must be bool type",
				conditionNode->getLinePos(),
				conditionNode->getInLinePos());
			throw std::runtime_error("Semantic error");
		}
	}

	void checkWhileStatement(Type* conditionType,
							   					 WhileStatement *whileStmt) {
		if (!conditionType->isBoolTy()) {
			auto conditionNode = whileStmt->child(0);
			printErrMsg("Condition expression in while-statement must be bool type",
				conditionNode->getLinePos(),
				conditionNode->getInLinePos());
			throw std::runtime_error("Semantic error");
		}
	}

	void checkDoStatement(DoStatement *doStmt) {
		auto subCallNode = doStmt->child(0);
		if (subCallNode->getNodeType() != NodeType::CALL_EXPR) {
			printErrMsg("Do statement must contain call expression",
				doStmt->getLinePos(),
				doStmt->getInLinePos());
			throw std::runtime_error("Semantic error");
		}
	}

	void checkReturnStatement(Type* retExprType,
								ReturnStatement *retStmt) {
		Type* subrtnRetType = currentSubrtn_->getRetType();
		
		if (subrtnRetType->isVoidTy() && !retExprType) return;

		if (subrtnRetType->isVoidTy() && !retExprType) {
			printErrMsg("Returning value in void function",
				retStmt->getLinePos(),
				retStmt->getInLinePos());
			throw std::runtime_error("Semantic error");
		}
		if (subrtnRetType->isVoidTy() && !retExprType) {
			printErrMsg("No return value in non-void function",
				retStmt->getLinePos(),
				retStmt->getInLinePos());
			throw std::runtime_error("Semantic error");
		}

		if (!Type::isAssignable(retExprType, subrtnRetType)) {
			std::cerr << currentFuncNameErrMsg()
				<< lineErrMsg(retStmt->getLinePos(), retStmt->getInLinePos())
				<< "Return statement with " << retExprType->toString() << " expression, "
				<< "in function returning " << subrtnRetType->toString() << std::endl;
			throw std::runtime_error("Semantic error");
		}
	}

	bool isTypeExist(Type* type) {
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

	bool isPrimitiveType(Type* type) {
		switch (type->getTypeId()) {
			case Type::TypeId::BoolTy:
			case Type::TypeId::CharTy:
			case Type::TypeId::IntTy:
			case Type::TypeId::VoidTy:
			case Type::TypeId::NullTy:
			case Type::TypeId::ArrayTy:
				return true;
		}
		return false;
	}

	void printErrMsg(const char* msg, unsigned linePos, unsigned inLinePos) {
		std::cerr << currentFuncNameErrMsg()
			<< lineErrMsg(linePos, inLinePos)
			<< msg << std::endl;
	}

	std::string currentFuncNameErrMsg() {
		return "In function " + currentSubrtn_->getName() + "\n";
	}

	std::string lineErrMsg(unsigned linePos, unsigned inLinePos) {
		std::stringstream msg;
		msg << "On line " << linePos << ":" << inLinePos << '\n';
		return msg.str();
	}
	
private:
	const SymbolTable *globalTable_ = nullptr;
	const SymbolTable *localTable_ = nullptr;
	const SubroutineSymbol *currentSubrtn_ = nullptr;
};