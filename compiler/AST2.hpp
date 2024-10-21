#pragma once
#include "compiler/Type.hpp"
#include "compiler/NodeDescriptors.hpp"
#include "compiler/SourceLocation.hpp"
#include "compiler/Visitor.hpp"
#include <vector>

enum class ValueCategory {
	NOT_VALUE,
	LVALUE,
	RVALUE,
};

// Main base class
class Node {
public:
  Node(SourceLocation srcLoc, Node* parent)
    : srcLoc_(srcLoc), parent_(parent) {}
  SourceLocation getSourceLoc() const { return srcLoc_; }
  const Node *getParent() const { return parent_; };
  virtual NodeType getNodeType() const = 0;
  virtual void accept(Visitor &) const = 0;
  virtual ~Node() = default;
private:
  SourceLocation srcLoc_;
  Node *parent_;
};

class Statement : public Node { 
  using Node::Node;
};

class StatementList final :  public Node { 
public:
  StatementList(SourceLocation srcLoc, Node* parent, 
                std::vector<Statement *> stmtList) 
    : Node(srcLoc, parent), statementList_(std::move(stmtList)) {}
  
  NodeType getNodeType() const override { return NodeType::STATEMENT_LIST; }
  std::vector<Statement*>::const_iterator
  begin() const { return statementList_.cbegin(); }
  std::vector<Statement*>::const_iterator
  end() const { return statementList_.cend(); }

  void accept(Visitor &v) const override {
    v.preVisit(this);
    for (Statement* stmt : statementList_) stmt->accept(v);
    v.postVisit(this);
  }
private:
  std::vector<Statement *> statementList_;
};


// Expressions

class Expression : public Node {
public:
  Expression(SourceLocation srcLoc, Node* parent, Type* evalType,
             ValueCategory valCateg)
    : Node(srcLoc, parent), evalType_(evalType), valCateg_(valCateg) {}
  ValueCategory getValueCategory() const { return valCateg_; }
  const Type* getEvalType() const { return evalType_; }
private:
  Type* evalType_;
  ValueCategory valCateg_;
};

class BinopExpr final : public Expression {
public:
  BinopExpr(SourceLocation srcLoc, Node* parent, Type* evalType,
            ValueCategory valCateg, Expression* lhs, Expression* rhs,
            OpType operation)
    : Expression(srcLoc, parent, evalType, valCateg), 
      lhs_(lhs), rhs_(rhs), op_(operation) {}
  
  NodeType getNodeType() const override { return NodeType::BINOP_EXPR; }
  const Expression* getLhsOperand() const { return lhs_; }
  const Expression* getRhsOperand() const { return rhs_; }
  OpType getOpType() const { return op_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);
    lhs_->accept(v);
    rhs_->accept(v);
    v.postVisit(this);
  }
private:
  Expression* lhs_;
  Expression* rhs_;
  OpType op_;
};

class UnopExpr final : public Expression {
public:
  UnopExpr(SourceLocation srcLoc, Node* parent, Type* evalType,
           ValueCategory valCateg, Expression* operand, OpType operation)
    : Expression(srcLoc, parent, evalType, valCateg),
      operand_(operand), op_(operation) {}
  
  NodeType getNodeType() const override { return NodeType::UNOP_EXPR; }
  const Expression* getOperand() const { return operand_; }
  OpType getOpType() const { return op_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);
    operand_->accept(v);
    v.postVisit(this);
  }
private:
  Expression* operand_;
  OpType op_;
};

class NewArrayExpr final : public Expression {
public:
  NewArrayExpr(SourceLocation srcLoc, Node* parent, Type* evalType,
               ValueCategory valCateg, Type* arrayElemType, 
               Expression* sizeExpr)
    : Expression(srcLoc, parent, evalType, valCateg), 
      arrayElemType_(arrayElemType), sizeExpr_(sizeExpr) {}
  
  NodeType getNodeType() const override { return NodeType::NEW_ARRAY_EXPR; }
  const Type* getArrayElemType() const { return arrayElemType_; }
  const Expression* getSizeExpr() const { return sizeExpr_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);
    sizeExpr_->accept(v);
    v.postVisit(this);
  }
private:
  Type* arrayElemType_;
  Expression* sizeExpr_;
};

class DeleteArrayExpr final : public Expression {
public:
  DeleteArrayExpr(SourceLocation srcLoc, Node* parent, Type* evalType,
                  ValueCategory valCateg, Expression* arrayExpr)
    : Expression(srcLoc, parent, evalType, valCateg),
      arrayExpr_(arrayExpr) {}
  
  NodeType getNodeType() const override { return NodeType::DELETE_ARRAY_EXPR; }
  const Expression* getArrayExpr() const { return arrayExpr_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);
    arrayExpr_->accept(v); 
    v.postVisit(this);
  }
private:
  Expression* arrayExpr_;
};

class LiteralExpr final : public Expression {
public:
  LiteralExpr(SourceLocation srcLoc, Node* parent, Type* evalType,
              ValueCategory valCateg, std::string value)
    : Expression(srcLoc, parent, evalType, valCateg),
      value_(std::move(value)) {}
  
  NodeType getNodeType() const override { return NodeType::LITERAL_EXPR; }
  const std::string& getValue() const { return value_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);
    v.postVisit(this); 
  }
private:
  std::string value_;
};

class NameExpr final : public Expression {
public:
  NameExpr(SourceLocation srcLoc, Node* parent, Type* evalType,
           ValueCategory valCateg, std::string name)
    : Expression(srcLoc, parent, evalType, valCateg),
      name_(std::move(name)) {}
  
  NodeType getNodeType() const override { return NodeType::NAME_EXPR; }
  const std::string& getName() const { return name_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);
    v.postVisit(this); 
  }
private:
  std::string name_;
};

class ArrayMemberExpr final : public Expression {
public:
  ArrayMemberExpr(SourceLocation srcLoc, Node* parent, Type* evalType,
                  ValueCategory valCateg, Expression* arrayExpr,
                  Expression* indexExpr)
    : Expression(srcLoc, parent, evalType, valCateg),
      arrayExpr_(arrayExpr), indexExpr_(indexExpr) {}
  
  NodeType getNodeType() const override { return NodeType::ARRAY_MEMBER_EXPR; }
  const Expression* getArrayExpr() const { return arrayExpr_; }
  const Expression* getIndexExpr() const { return indexExpr_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);
    arrayExpr_->accept(v);
    indexExpr_->accept(v);
    v.postVisit(this);
  }
private:
  Expression* arrayExpr_;
  Expression* indexExpr_;
};

class CallExpr final : public Expression {
public:
  CallExpr(SourceLocation srcLoc, Node* parent, Type* evalType,
           ValueCategory valCateg, Expression* subroutineExpr,
           std::vector<Expression *> args)
    : Expression(srcLoc, parent, evalType, valCateg),
      args_(std::move(args)), subroutineExpr_(subroutineExpr) {}
  
  NodeType getNodeType() const override { return NodeType::CALL_EXPR; }
  const Expression* getSubroutineExpr() const { return subroutineExpr_; }
  std::vector<Expression*>::const_iterator
  args_begin() { return args_.cbegin(); }
  std::vector<Expression*>::const_iterator
  args_end() { return args_.cend(); }
  
  void accept(Visitor &v) const override {
    v.preVisit(this); 
    subroutineExpr_->accept(v);
    v.postVisit(this);
  }
private:
  std::vector<Expression *> args_;
  Expression* subroutineExpr_;
};

class MemberExpr final : public Expression {
public:
  MemberExpr(SourceLocation srcLoc, Node* parent, Type* evalType,
             ValueCategory valCateg, std::string member,
             Expression* identifier)
    : Expression(srcLoc, parent, evalType, valCateg),
      member_(std::move(member)), identifier_(identifier) {}
  
  NodeType getNodeType() const override { return NodeType::MEMBER_EXPR; }
  const std::string& getMember() const { return member_; }
  const Expression* getIdentifier() const { return identifier_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);
    identifier_->accept(v);
    v.postVisit(this);
  }
private:
  std::string member_;
  Expression* identifier_;
};

// Final statements

class LetStatement final : public Statement {
public:
  LetStatement(SourceLocation srcLoc, Node* parent, Expression* lhs,
               Expression* rhs)
    : Statement(srcLoc, parent), lhs_(lhs), rhs_(rhs) {}
  
  NodeType getNodeType() const override { return NodeType::LET_STATEMENT; }
  const Expression* getLhs() const { return lhs_; }
  const Expression* getRhs() const { return rhs_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this); 
    lhs_->accept(v);
    rhs_->accept(v); 
    v.postVisit(this);
  }
private:
  Expression* lhs_;
  Expression* rhs_;
};

class IfStatement final : public Statement {
public:
  IfStatement(SourceLocation srcLoc, Node* parent, Expression* condition,
              StatementList* ifBody, StatementList* elseBody)
    : Statement(srcLoc, parent), condition_(condition),
      ifBody_(ifBody), elseBody_(elseBody) {}
  
  NodeType getNodeType() const override { return NodeType::IF_STATEMENT; }
  const Expression* getCondition() const { return condition_; }
  const StatementList* getIfBody() const { return ifBody_; }
  const StatementList* getElseBody() const { return elseBody_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this); 
    condition_->accept(v);
    ifBody_->accept(v);
    elseBody_->accept(v); 
    v.postVisit(this);
  }
private:
  Expression* condition_;
  StatementList* ifBody_;
  StatementList* elseBody_;
};

class WhileStatement final : public Statement {
public:
  WhileStatement(SourceLocation srcLoc, Node* parent, Expression* condition,
                 StatementList* whileBody)
    : Statement(srcLoc, parent), condition_(condition), 
      whileBody_(whileBody) {}
  
  NodeType getNodeType() const override { return NodeType::WHILE_STATEMENT; }
  const Expression* getCondition() const { return condition_; }
  const StatementList* getWhileBody() const { return whileBody_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this); 
    condition_->accept(v);
    whileBody_->accept(v); 
    v.postVisit(this);
  }
private:
  Expression* condition_;
  StatementList* whileBody_;
};

class DoStatement final : public Statement {
public:
  DoStatement(SourceLocation srcLoc, Node* parent, CallExpr* callExpr)
    : Statement(srcLoc, parent), callExpr_(callExpr) {}
  
  NodeType getNodeType() const override { return NodeType::DO_STATEMENT; }
  const CallExpr* getCallExpr() const { return callExpr_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this); 
    callExpr_->accept(v); 
    v.postVisit(this);
  }
private:
  CallExpr* callExpr_;
};

// Variable Decls

class VariableDec : public Statement {
public:
  VariableDec(SourceLocation srcLoc, Node* parent, std::string name,
              Type* type)
    : Statement(srcLoc, parent), name_(std::move(name)), type_(type) {}
  
  const std::string& getVarName() const { return name_; }
  const Type* getVarType() const { return type_; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);
    v.postVisit(this);
  }
private:
  std::string name_;
  Type* type_;
};

class FieldVarDec final : public VariableDec {
public:
  using VariableDec::VariableDec;
  NodeType getNodeType() const override { return NodeType::FIELD_VAR_DEC; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);  
    v.postVisit(this);
  }
};
class StaticVarDec final : public VariableDec {
public:
  using VariableDec::VariableDec;
  NodeType getNodeType() const override { return NodeType::STATIC_VAR_DEC; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);     
    v.postVisit(this);
  }
};
class ArgumentVarDec final : public VariableDec { 
public:
  using VariableDec::VariableDec;
  NodeType getNodeType() const override { return NodeType::ARGUMENT_VAR_DEC; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);  
    v.postVisit(this);
  }
};
class LocalVarDec final : public VariableDec { 
public:
  using VariableDec::VariableDec;
  NodeType getNodeType() const override { return NodeType::LOCAL_VAR_DEC; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);   
    v.postVisit(this);
  }
};

// Class and subroutine nodes

class SubroutineBody final : public Node {
public:
  SubroutineBody(SourceLocation srcLoc, Node* parent,
                 std::vector<LocalVarDec *> localVars,
                 StatementList* statements)
    : Node(srcLoc, parent), localVarList_(std::move(localVars)),
      statementList_(statements) {}
  
  NodeType getNodeType() const override { return NodeType::SUBRTN_BODY; }
  const StatementList* getStatmentList() const { return statementList_; }
  std::vector<LocalVarDec*>::const_iterator
  locals_begin() const { return localVarList_.cbegin(); }
  std::vector<LocalVarDec*>::const_iterator
  locals_end() const { return localVarList_.cend(); }
  
  void accept(Visitor &v) const override {
    v.preVisit(this); 
    for (LocalVarDec* local : localVarList_) local->accept(v);
    statementList_->accept(v); 
    v.postVisit(this);
  }
private:
  std::vector<LocalVarDec *> localVarList_;
  StatementList* statementList_;
};

class SubroutineDec : public Node {
public:
  SubroutineDec(SourceLocation srcLoc, Node* parent,
                std::string name, std::vector<ArgumentVarDec *> args,
                Type* retType, SubroutineBody* body)
    : Node(srcLoc, parent), name_(std::move(name)), args_(std::move(args)),
      retType_(retType), body_(body) {}

  const std::string& getName() const { return name_; }
  const Type* getReturnType() const { return retType_; }
  const SubroutineBody* getSubroutineBody() const { return body_; }
  std::vector<ArgumentVarDec*>::const_iterator
  args_begin() const { return args_.cbegin(); }
  std::vector<ArgumentVarDec*>::const_iterator
  args_end() const { return args_.cend(); }
  
  void accept(Visitor &v) const override {
    v.preVisit(this); 
    acceptOnChildren(v);
    v.postVisit(this);
  }
  
  void acceptOnChildren(Visitor &v) const {
    for (ArgumentVarDec* arg : args_) arg->accept(v);
    body_->accept(v);
  }
private:
  std::string name_;
  std::vector<ArgumentVarDec*> args_;
  Type* retType_;
  SubroutineBody* body_;
};

class ConstructorDec final : public SubroutineDec {
  using SubroutineDec::SubroutineDec;
  NodeType getNodeType() const override { return NodeType::CONSTRUCTOR_DEC; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);
    acceptOnChildren(v);
    v.postVisit(this);
  }
};
class FunctionDec final : public SubroutineDec {
  using SubroutineDec::SubroutineDec;
  NodeType getNodeType() const override { return NodeType::FUNCTION_DEC; }

  void accept(Visitor &v) const override {
    v.preVisit(this); 
    acceptOnChildren(v);
    v.postVisit(this);
  }
};
class MethodDec final : public SubroutineDec {
  using SubroutineDec::SubroutineDec;
  NodeType getNodeType() const override { return NodeType::METHOD_DEC; }
  
  void accept(Visitor &v) const override {
    v.preVisit(this); 
    acceptOnChildren(v);
    v.postVisit(this);
  }
};

class ClassDec final : public Node {
public:
  ClassDec(SourceLocation srcLoc, Node* parent, std::string name,
           std::vector<VariableDec *> vars,
           std::vector<SubroutineDec *> subroutins)
    : Node(srcLoc, parent), name_(std::move(name)),
      varList_(std::move(vars)), subroutineList_(std::move(subroutins)) {}

  NodeType getNodeType() const override { return NodeType::CLASS_DEC; }
  const std::string& getName() const { return name_; }
  
  std::vector<VariableDec*>::const_iterator
  var_begin() const { return varList_.cbegin(); }
  std::vector<VariableDec*>::const_iterator
  var_end() const { return varList_.cend(); }
  std::vector<SubroutineDec*>::const_iterator
  subroutine_begin() const { return subroutineList_.cbegin(); }
  std::vector<SubroutineDec*>::const_iterator
  subroutine_end() const { return subroutineList_.cend(); }
  
  void accept(Visitor &v) const override {
    v.preVisit(this);
    for (VariableDec* var : varList_) var->accept(v);
    for (SubroutineDec* s : subroutineList_) s->accept(v);
    v.postVisit(this);
  }
private:
  std::string name_;
  std::vector<VariableDec *> varList_;
  std::vector<SubroutineDec *> subroutineList_;
};

class Program final {
public:
  Program(std::string programName) : programName_(std::move(programName)) {}

  Program(std::string programName, std::vector<ClassDec *> classList)
    : programName_(std::move(programName)), classList_(std::move(classList)) {}
  
  const std::string& getProgramName() const { return programName_; }
  
  std::vector<ClassDec*>::const_iterator
  classes_begin() const { return classList_.cbegin(); }
  
  std::vector<ClassDec*>::const_iterator
  classes_end() const { return classList_.cend(); }
  
  void accept(Visitor &v) const {
    v.preVisit(this);
    for (ClassDec* cls : classList_) cls->accept(v);
    v.postVisit(this);
  }
  
  void addClassDec(ClassDec* classDec) { classList_.push_back(classDec); }

private:
  std::string programName_;
  std::vector<ClassDec*> classList_;
};