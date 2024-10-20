#pragma once
#include <vector>
#include <memory>
#include <string>
#include <iostream>
#include "Type.hpp"
#include "SourceLocation.hpp"
#include "Visitor.hpp"
#include "NodeDescriptors.hpp"

enum class ValueCategory {
	NOT_VALUE,
	LVALUE,
	RVALUE,
};

/*
-------------------------------
Jack program structure
-------------------------------
*/

class Node {
public:
    Node(SourceLocation sourceLoc) : sourceLoc_(std::move(sourceLoc)) {}
    
    Node(Node* parentNode,
         SourceLocation sourceLoc
        )
        : parentNode_(parentNode),
          sourceLoc_(std::move(sourceLoc))
        {}

    virtual ~Node() = default;

    virtual NodeType getNodeType() const = 0;

    void setParent(Node* parent) { parentNode_ = parent; }

    Node* getParent() { return parentNode_; }

    const SourceLocation& getSourceLoc() const { return sourceLoc_; }

    virtual void accept(Visitor &) = 0;

    virtual Node* child(unsigned index) = 0;

    virtual unsigned children() const = 0;

private:
    Node* parentNode_;
    SourceLocation sourceLoc_;
};

class Statement : public Node {
protected:
    Statement(Node* parentNode, SourceLocation sourceLoc) 
        : Node(parentNode, sourceLoc) {}
};

class Expression : public Node {
protected:
    Expression(Node* parentNode,
               SourceLocation sourceLoc, 
               Type* type, ValueCategory valCategory)
        : Node(parentNode, std::move(sourceLoc)), 
          evalType_(type), 
          valCategory_(valCategory) {}

    ValueCategory getValueCat() const { return valCategory_; }

    Type* getEvalType() const { return evalType_; }

    Type* evalType_;
    ValueCategory valCategory_;
};


/*
Variable class represents variable name and type
*/
class VariableDec : public Node {
public:
    VariableDec(Node* parent,
                SourceLocation sourceLoc,
                std::string name,
                Type* type)
            : Node(parent, std::move(sourceLoc)),
              name_(std::move(name)),
              type_(type)
            {}

    virtual NodeType getNodeType() const = 0;
    
    void setVarName(std::string name) { name_ = std::move(name); }

    void setVarType(Type* type) { type_ = type; }

    std::string getVarName() const { return name_; }

    Type* getVarType() const { return type_; }

    unsigned int children() const override { return 0; }

    Node* child(unsigned int i) override { return nullptr; }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        vstr.postVisit(this);
    }
    
private:
    std::string name_;
    Type* type_;
};

class FieldVarDec final : public VariableDec {
public:
    NodeType getNodeType() const override { return NodeType::FIELD_VAR_DEC; }
};

class StaticVarDec final : public VariableDec {
public:
    NodeType getNodeType() const override { return NodeType::STATIC_VAR_DEC; }
};

class ArgumentVarDec final : public VariableDec {
public:
    NodeType getNodeType() const override { return NodeType::ARGUMENT_VAR_DEC; }
};

class LocalVarDec final : public VariableDec {
public:
    NodeType getNodeType() const override { return NodeType::LOCAL_VAR_DEC; }
};

#if 0
class VariableDecList : public Node {
public:
    VariableDecList() = default;

    void addVar(VariableDec* varDec) {
        vars_.push_back(varDec);
    }

    virtual NodeType getNodeType() const = 0;

    unsigned int children() const override { return vars_.size(); }

    Node* child(unsigned int i) override {
        if (i >= vars_.size()) return nullptr;
        return vars_[i];
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        for (auto&& var : vars_) {
            var->accept(vstr);
        }
        vstr.postVisit(this);
    }
    
private:
    std::vector<VariableDec*> vars_;
};

class FieldVariableList final : public VariableDecList {
public:
    NodeType getNodeType() const override { return NodeType::FIELD_VAR_DEC_LIST; }
};

class StaticVariableList final : public VariableDecList {
public:
    NodeType getNodeType() const override { return NodeType::STATIC_VAR_DEC_LIST; }
};

class SubroutineArgumentList final : public VariableDecList {
    NodeType getNodeType() const override { return NodeType::ARGUMENT_VAR_DEC_LIST; }
};

class LocalVariableList final : public VariableDecList {
    NodeType getNodeType() const override { return NodeType::LOCAL_VAR_DEC_LIST; }
};
#endif

/*
-------------------------------
Statements
-------------------------------
*/

class StatementList : public Node {
public:
    StatementList(Node* parent, SourceLocation sourceLoc,
                  std::vector<Statement*> statements)
        : Node(parent, sourceLoc), 
          statementList_(std::move(statements)) {}

    void addStatement(Statement* stmt) {
        statementList_.push_back(stmt);
    }

    NodeType getNodeType() const override { return NodeType::STATEMENT_LIST; }

    std::vector<Statement*>::iterator
    statementListBegin() { return statementList_.begin(); }

    std::vector<Statement*>::iterator
    StatementListEnd() { return statementList_.end(); }

    unsigned int children() const override { return statementList_.size(); }

    Node* child(unsigned int i) override {
        if (i >= statementList_.size()) return nullptr;
        return statementList_[i];
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        for (auto&& stmt : statementList_) {
            stmt->accept(vstr);
        }
        vstr.postVisit(this);
    }

private:
    std::vector<Statement*> statementList_;
};

class LetStatement : public Statement {
public:
    LetStatement(Node* parent, SourceLocation sourceLoc,
                 Expression* lhs, Expression* rhs) 
        : Statement(parent, sourceLoc), lhsExpr_(lhs), rhsExpr_(rhs) {}

    void addLhs(Expression* lhs) { lhsExpr_ = lhs; }

    void addRhs(Expression* rhs) { rhsExpr_ = rhs; }

    Expression* getLhsExpr() { return lhsExpr_; }

    Expression* getRhsExpr() { return rhsExpr_; }
    
    NodeType getNodeType() const override { return NodeType::LET_STATEMENT; }

    unsigned int children() const override { return 2; }

    Node* child(unsigned int i) override {
        if (i == 0) return lhsExpr_;
        if (i == 1) return rhsExpr_;
        return nullptr;
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        lhsExpr_->accept(vstr);
        rhsExpr_->accept(vstr);
        vstr.postVisit(this);
    }

private:
    Expression* lhsExpr_;
    Expression* rhsExpr_;
};

class IfStatement : public Statement {
public:
    IfStatement(Node* parent, SourceLocation sourceLoc,
                Expression* condition,
                StatementList* ifBody,
                StatementList* elseBody)
        : Statement(parent, sourceLoc), condition_(condition),
          ifBody_(ifBody), elseBody_(elseBody) {}

    void addCondition(Expression* condition) {
        condition_ = condition;    
    }
    
    void addIfBody(StatementList* ifBody) {
        ifBody_ = ifBody;
    }

    void addElseBody(StatementList* elseBody) {
        elseBody_ = elseBody;
    }

    Expression* getConditionExpr() { return condition_; }

    StatementList* getIfBody() { return ifBody_; }

    StatementList* getElseBody() { return elseBody_; }

    NodeType getNodeType() const override { return NodeType::IF_STATEMENT; }
    
    unsigned int children() const override { return 3; }

    Node* child(unsigned int i) override {
        if (i == 0) return condition_;
        if (i == 1) return ifBody_;
        if (i == 2) return elseBody_;
        return nullptr;
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        condition_->accept(vstr);
        ifBody_->accept(vstr);
        if (elseBody_) {
            elseBody_->accept(vstr);
        }
        vstr.postVisit(this);
    }

private:
    Expression* condition_;
    StatementList* ifBody_;
    StatementList* elseBody_;
};

class WhileStatement : public Statement {
public:
    WhileStatement(Node* parent, SourceLocation sourceLoc,
                   Expression* condition,
                   StatementList* body)
        : Statement(parent, sourceLoc), condition_(condition),
          body_(body) {}
    
    void addCondition(Expression* condition) {
        condition_ = condition;
    }

    void addBody(StatementList* body) {
        body_ = body;
    }

    Expression* getConditionExpr() { return condition_; }

    StatementList* getBody() { return body_; }

    NodeType getNodeType() const override { return NodeType::WHILE_STATEMENT; }

    unsigned int children() const override { return 3; }

    Node* child(unsigned int i) override {
        if (i == 0) return condition_;
        if (i == 1) return body_;
        return nullptr;
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        condition_->accept(vstr);
        body_->accept(vstr);
        vstr.postVisit(this);
    }

private:
    Expression* condition_;
    StatementList* body_;
};

class DoStatement : public Statement {
public:
    DoStatement(Node* parent, SourceLocation sourceLoc,
                Expression* callExpr)
        : Statement(parent, sourceLoc), callExpr_(callExpr) {}

    void addCallExpr(Expression* callExpr) {
        callExpr_ = callExpr;
    }

    Expression* getCallExpr() { return callExpr_; }

    NodeType getNodeType() const override { return NodeType::DO_STATEMENT; }

    unsigned int children() const override { return 1; }

    Node* child(unsigned int i) override {
        if (i == 0) return callExpr_;
        return nullptr;
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        callExpr_->accept(vstr);
        vstr.postVisit(this);
    }

private:
    Expression* callExpr_;
};

class ReturnStatement : public Statement {
public:
    ReturnStatement(Node* parent, SourceLocation sourceLoc,
                    Expression* callExpr)
        : Statement(parent, sourceLoc), retExpr_(callExpr) {}

    void addRetExpr(Expression* retExpr) {
        retExpr_ = retExpr;
    }

    Expression* getReturnExpr() { return retExpr_; }

    NodeType getNodeType() const override { return NodeType::RETURN_STATEMENT; }

    unsigned int children() const override {
        if (retExpr_ != nullptr) return 1;
        return 0;
    }

    Node* child(unsigned int i) override {
        if (i == 0) return retExpr_;
        return nullptr;
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        if (retExpr_) {
            retExpr_->accept(vstr);
        }
        vstr.postVisit(this);
    }

private:
    Expression* retExpr_;
};

/*
-------------------------------
Expressions
-------------------------------
*/

#if 0
class ExpressionList : public Node {
public:
    ExpressionList() = default;

    void addExpression(Expression* expr) {
        ExpressionList_.push_back(expr);
    }

    NodeType getNodeType() const override { return NodeType::EXPRESSION_LIST; }

    unsigned int children() const override { return ExpressionList_.size(); }

    Node* child(unsigned int i) override {
        if (i >= ExpressionList_.size()) return nullptr;
        return ExpressionList_[i];
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        for (auto&& expr : ExpressionList_) {
            expr->accept(vstr);
        }
        vstr.postVisit(this);
    }

private:
    std::vector<Expression*> ExpressionList_;
};
#endif

class BinopExpr : public Expression {
public:
    BinopExpr(Node* parent, SourceLocation sourceLoc,
              Type* evalType, Expression* lExp, 
              Expression* rExp, OpType bType)
      : Expression(parent, sourceLoc, evalType, ValueCategory::RVALUE), 
        leftExpr_(lExp), rightExpr_(rExp), OpType_(bType) {}

    void addLeftExpr(Expression* leftExpr) {
        leftExpr_ = leftExpr;
    }

    void addRightExpr(Expression* rightExpr) {
        rightExpr_ = rightExpr;
    }

    void setOpType(OpType OpType) {
        OpType_ = OpType;
    }

    Expression* getLhsExpr() { return leftExpr_; }

    Expression* getRhsExpr() { return rightExpr_; }

    OpType getOpType() const { return OpType_; }

    NodeType getNodeType() const override { return NodeType::BINOP_EXPR; }

    unsigned int children() const override { return 2; }

    Node* child(unsigned int i) override {
        if (i == 0) return leftExpr_;
        if (i == 1) return rightExpr_;
        return nullptr;
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        leftExpr_->accept(vstr);
        rightExpr_->accept(vstr);
        vstr.postVisit(this);
    }

private:
    Expression* leftExpr_;
    Expression* rightExpr_;
    OpType OpType_;
};

class UnopExpr : public Expression {
public:
    UnopExpr(Node* parent, SourceLocation sourceLoc,
             Type* evalType, Expression* expr, OpType uType)
              : Expression(parent, sourceLoc, evalType, ValueCategory::RVALUE), 
                operandExpr_(expr), OpType_(uType) {}

    void addUnopExpr(Expression* expr) {
        operandExpr_ = expr;
    }

    void setUnOpType(OpType type) { OpType_ = type; } 

    Expression* getOperand() { return operandExpr_; }

    OpType getOpType() const { return OpType_; }

    NodeType getNodeType() const override { return NodeType::UNOP_EXPR; }

    unsigned int children() const override { return 1; }

    Node* child(unsigned int i) override {
        if (i == 0) return operandExpr_;
        return nullptr;
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        operandExpr_->accept(vstr);
        vstr.postVisit(this);
    }
    
private:
    Expression* operandExpr_;
    OpType OpType_;
};

class NewArrayExpr : public Expression {
public:
    NewArrayExpr(Node* parent, SourceLocation sourceLoc,
                 Type* arrayType, Type* arrayElemType,
                 Expression* sizeExpr)
              : Expression(parent, sourceLoc, arrayType, ValueCategory::LVALUE), 
                arrayElemType_(arrayElemType), sizeExpr_(sizeExpr) {}
    
    void setArrayElemType(Type* type) { arrayElemType_ = type; }

    void setArraySize(Expression* sizeExpr) { sizeExpr_ = sizeExpr; }

    Type* getArrayElemType() const { return arrayElemType_; }

    Expression* getSizeExpr() { return sizeExpr_; }

    NodeType getNodeType() const override { return NodeType::NEW_ARRAY_EXPR; }

    unsigned children() const override { return 1; }

    Node* child(unsigned int i) override { 
        if (i == 0) return sizeExpr_;
        return nullptr; 
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        sizeExpr_->accept(vstr);
        vstr.postVisit(this);
    }

private:
    Type* arrayElemType_;
    Expression* sizeExpr_;
};

class DeleteArrayExpr : public Expression {
public:
    DeleteArrayExpr(Node* parent, SourceLocation sourceLoc,
                    Type* arrayElemType, Expression* arrayExpr)
        : Expression(parent, sourceLoc,
                     Type::getVoidTy(), ValueCategory::NOT_VALUE), 
          arrayExpr_(arrayExpr) {}
    
    void setArray(Expression* arrayExpr) { arrayExpr_ = arrayExpr; }

    Expression* getArrayExpr() { return arrayExpr_; }

    NodeType getNodeType() const override { return NodeType::NEW_ARRAY_EXPR; }

    unsigned children() const override { return 1; }

    Node* child(unsigned int i) override { 
        if (i == 0) return arrayExpr_;
        return nullptr; 
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        arrayExpr_->accept(vstr);
        vstr.postVisit(this);
    }

private:
    Expression* arrayExpr_;
};

class LiteralExpr : public Expression {
public:
    LiteralExpr(Node* parent, SourceLocation sourceLoc,
                Type* literalType, std::string literalValue)
        : Expression(parent, sourceLoc,
                     literalType, ValueCategory::RVALUE), 
          value_(literalValue) {}

    void setLiteralType(Type* litType) { evalType_ = litType; }

    void setValue(std::string value) { value_ = value; }

    Type* getLiteralType() const { return evalType_; }

    std::string getValue() const { return value_; }

    NodeType getNodeType() const override { return NodeType::LITERAL_EXPR; }

    unsigned children() const override { return 0; }

    Node* child(unsigned int i) override { return nullptr; }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        vstr.postVisit(this);
    }

private:
    std::string value_;
};

class NameExpr : public Expression {
public:
    NameExpr(Node* parent, SourceLocation sourceLoc,
             Type* literalType, std::string name)
        : Expression(parent, sourceLoc,
                     literalType, ValueCategory::LVALUE), 
          name_(name) {}

    std::string getName() const { return name_; }

    void setName(std::string name) { name_ = name; }

    NodeType getNodeType() const override { return NodeType::NAME_EXPR; }

    unsigned int children() const override { return 0; }

    Node* child(unsigned int i) override { return nullptr; }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        vstr.postVisit(this);
    }

private:
    std::string name_; 
};

class ArrayMemberExpr : public Expression {
public:
    ArrayMemberExpr(Node* parent, SourceLocation sourceLoc,
                    Type* arrayType, Expression* arrayExpr,
                    Expression* indexExpr)
        : Expression(parent, sourceLoc,
                     arrayType, ValueCategory::LVALUE),
          arrayExpr_(arrayExpr), indexExpr_(indexExpr) {}

    void addArrayExpr(Expression* arrayExpr) {
        arrayExpr_ = arrayExpr;
    }

    void addIndexExpr(Expression* indexExpr) {
        indexExpr_ = indexExpr;
    }

    NodeType getNodeType() const override { return NodeType::ARRAY_MEMBER_EXPR; }

    unsigned int children() const override { return 2; }

    Node* child(unsigned int i) override {
        if (i == 0) return arrayExpr_;
        if (i == 1) return indexExpr_;
        return nullptr;
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        arrayExpr_->accept(vstr);
        indexExpr_->accept(vstr);
        vstr.postVisit(this);
    }
    
private:
    Expression* arrayExpr_;
    Expression* indexExpr_;
};

class SubroutineCallExpr : public Expression {
public:
    SubroutineCallExpr(Node* parent, SourceLocation sourceLoc,
                      Type* returnType, ValueCategory valueCategory,
                      Expression* subroutineExpr,
                      std::vector<Expression*> arguments)
        : Expression(parent, sourceLoc,
                     returnType, valueCategory),
          subroutineExpr_(subroutineExpr), args_(std::move(arguments)) {}

    void addSubroutineExpr(Expression* subroutineExpr) {
        subroutineExpr_ = subroutineExpr;
    }

    void addArgs(std::vector<Expression*> args) {
        args_ = std::move(args);
    }

    unsigned getArgCount() const { return args_.size(); }

    NodeType getNodeType() const override { return NodeType::CALL_EXPR; }

    unsigned children() const override { return 2; }

    Node* child(unsigned i) override {
        if (i == 0) return subroutineExpr_;
        if (i >= 1 && i <= args_.size()) return args_[i - 1];
        return nullptr;
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        subroutineExpr_->accept(vstr);
        for (auto argExpr : args_) {
            argExpr->accept(vstr);
        }
        vstr.postVisit(this);
    }
    
private:
    Expression* subroutineExpr_;
    std::vector<Expression*> args_;
};

class MemberExpr : public Expression {
public:
    MemberExpr(Node* parent, SourceLocation sourceLoc,
            Type* memberType, ValueCategory valueCategory,
            Expression* identifier,
            std::string member)
        : Expression(parent, sourceLoc,
                     memberType, valueCategory),
          identifier_(identifier), member_(std::move(member)) {}

    void addIdentifier(Expression* identifier) {
        identifier_ = identifier;
    }

    void addMember(std::string member) {
        member_ = member;
    }

    std::string getMember() const { return member_; }

    NodeType getNodeType() const override { return NodeType::MEMBER_EXPR; }

    unsigned int children() const override { return 1; }

    Node* child(unsigned int i) override {
        if (i == 0) return identifier_;
        return nullptr;
    }
    
    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        identifier_->accept(vstr);
        vstr.postVisit(this);
    }

private:
    Expression* identifier_;
    std::string member_;
};


/*
Subroutine body Node handles variables declaration and all statements in body
*/
class SubroutineBody : public Node {
public:
    SubroutineBody(Node* parent,
                SourceLocation sourceLoc,
                std::vector<LocalVarDec*> localVars,
                StatementList* statementList)
            : Node(parent, std::move(sourceLoc)),
              localVars_(std::move(localVars)),
              statementList_(statementList)
            {}

    void setVarList(std::vector<LocalVarDec*> vars) {
        localVars_ = std::move(vars);
    }

    void setStatementList(StatementList* statementList) {
        statementList_ = statementList;
    }

    NodeType getNodeType() const override { return NodeType::SUBRTN_BODY; }

    unsigned int children() const override { return localVars_.size() + 1; }

    Node* child(unsigned int i) override {
        if (i >= 0 && i < localVars_.size()) return localVars_[i];
        if (i == localVars_.size()) return statementList_;
        return nullptr;
    }
    
    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        for (auto varDec : localVars_) {
            varDec->accept(vstr);
        }
        statementList_->accept(vstr);
        vstr.postVisit(this);
    }

private:
    std::vector<LocalVarDec*> localVars_;
    StatementList* statementList_;
};

/*
Subroutine declaration Node handles name and body of method/constructor/function
*/
class SubroutineDec : public Node {
public:
    SubroutineDec(Node* parent,
                  SourceLocation sourceLoc,
                  std::string name,
                  Type* retType,
                  std::vector<ArgumentVarDec*> arguments,
                  SubroutineBody* body)
            : Node(parent, std::move(sourceLoc)),
              name_(std::move(name)), retType_(retType),
              args_(std::move(arguments)), body_(body)
            {}

    virtual NodeType getNodeType() const = 0;

    void setName(std::string name) { name_ = name; }

    void setRetType(Type* retType) { retType_ = retType; }

    void addArgList(std::vector<ArgumentVarDec*> args) {
        args_ = std::move(args);
    }

    void addBody(SubroutineBody* body) {
        body_ = body;
    }

    std::string getName() const { return name_; }

    Type* getRetType() const { return retType_; }

    unsigned children() const override { return args_.size() + 1; }

    Node* child(unsigned idx) override {
        if (idx >= 0 && idx < args_.size()) return args_[idx];
        if (idx == args_.size()) return body_;
        return nullptr;
    }
  
    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        for (auto arg : args_) {
            arg->accept(vstr);
        }
        if (body_) body_->accept(vstr);
        vstr.postVisit(this);
    }

private:
    std::string name_;
    Type* retType_;
    std::vector<ArgumentVarDec*> args_;
    SubroutineBody* body_;
};

class ConstructorDec final : public SubroutineDec {
public:
    NodeType getNodeType() const override { return NodeType::CONSTRUCTOR_DEC; }
};

class FunctionDec final : public SubroutineDec {
public:
    NodeType getNodeType() const override { return NodeType::FUNCTION_DEC; }
};

class MethodDec final : public SubroutineDec {
public:
    NodeType getNodeType() const override { return NodeType::METHOD_DEC; }
};

#if 0
class SubroutineList : public Node {
public:
    SubroutineList() = default;

    NodeType getNodeType() const override { return NodeType::SUBRNT_DEC_LIST; };
    
    void addSubroutine(SubroutineDec* subrtn) {
        subrtns_.push_back(subrtn);
    }

    unsigned int children() const override { return subrtns_.size(); }

    Node* child(unsigned int i) override {
        if (i >= subrtns_.size()) return nullptr;
        return subrtns_[i];
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        for (auto&& sub : subrtns_) {
            sub->accept(vstr);
        }
        vstr.postVisit(this);
    }
     
private:
    std::vector<SubroutineDec*> subrtns_;
};
#endif

/*
Class declaration Node which handles Jack class name and subroutines
*/
class ClassDec : public Node {
public:
    ClassDec(Node* parent,
             SourceLocation sourceLoc,
             std::vector<FieldVarDec*> fieldVars,
             std::vector<StaticVarDec*> staticVars,
             std::vector<SubroutineDec*> subroutineList)
        : Node(parent, std::move(sourceLoc)),
          fieldVars_(std::move(fieldVars)),
          staticVars_(std::move(staticVars)),
          subroutineList_(std::move(subroutineList))
        {}

    void addFieldVarList(std::vector<FieldVarDec*> fieldVars) {
        fieldVars_ = std::move(fieldVars);
    }

    void addStaticVarList(std::vector<StaticVarDec*> staticVars) {
        staticVars_ = std::move(staticVars);
    }

    void addSubroutineList(std::vector<SubroutineDec*> subroutineList) {
        subroutineList_ = std::move(subroutineList);
    }

    void setName(std::string name) { className_ = std::move(name); }

    std::string getName() const { return className_; }

    NodeType getNodeType() const override { return NodeType::CLASS_DEC; }

    unsigned children() const override { 
        return fieldVars_.size() + staticVars_.size() + subroutineList_.size(); 
    }

    Node* child(unsigned idx) override {
        if (idx >= 0 && idx < fieldVars_.size()) {
            return fieldVars_[idx];
        }

        unsigned staticVarsSentinel = fieldVars_.size() + 
                                    staticVars_.size();
        if (idx >= fieldVars_.size() && idx < staticVarsSentinel) {
            unsigned fIdx = idx - fieldVars_.size();
            return staticVars_[fIdx];
        }

        unsigned subroutinesSentinel = staticVarsSentinel + 
                                    subroutineList_.size();
        if (idx >= staticVarsSentinel && idx < subroutinesSentinel) {
            unsigned sIdx = idx - fieldVars_.size() - staticVars_.size();
            return subroutineList_[sIdx];
        }

        return nullptr;
    }

    void accept(Visitor &vstr) override {
        vstr.preVisit(this);
        for (auto fieldVar : fieldVars_) fieldVar->accept(vstr);
        for (auto staticVar : staticVars_) staticVar->accept(vstr);
        for (auto subroutine : subroutineList_) subroutine->accept(vstr);
        vstr.postVisit(this);
    }

private:
    std::string className_;
    std::vector<FieldVarDec*> fieldVars_;
    std::vector<StaticVarDec*> staticVars_;
    std::vector<SubroutineDec*> subroutineList_;
};

class FileUnit final {
public:
    FileUnit(std::string fileName) : fileName_(std::move(fileName))
        {}

    std::string getFileName() const { return fileName_; }

    void addClass(ClassDec* clDec) {
        classes_.push_back(clDec);
    }

    unsigned classCount() const { return classes_.size(); }

    ClassDec* getClass(unsigned i) { 
        if (i >= classes_.size()) return nullptr;
        return classes_[i];
    }

    void accept(Visitor &vstr) {
        vstr.preVisit(this);
        for (auto && clDec : classes_) {
            clDec->accept(vstr);
        }
        vstr.postVisit(this);
    }

private:
    std::string fileName_;
    std::vector<ClassDec*> classes_;
};

/*
Program Node which handles all Jack classes in file
*/
class Program final {
public:
    Program() = default;

    void addFile(FileUnit* clDec) {
        fileUnits_.push_back(clDec);
    }
    
    unsigned filesCount() const { return fileUnits_.size(); }
    
    FileUnit* getFile(unsigned int i) {
        if (i >= fileUnits_.size()) return nullptr;
        return fileUnits_[i];
    }

    void accept(Visitor &vstr) {
        vstr.preVisit(this);
        for (auto && fileUnit : fileUnits_) {
            fileUnit->accept(vstr);
        }
        vstr.postVisit(this);
    }

private:
    std::vector<FileUnit*> fileUnits_;
};