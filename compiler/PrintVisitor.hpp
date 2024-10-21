#pragma once
#include "AST.hpp"
#include "Visitor.hpp"
#include <fstream>
#include <memory>

class PrintVisitor final : public Visitor {
public:
  PrintVisitor() = default;
  PrintVisitor(int indentDiff) : indentDiff_(indentDiff) {}

private:
  void preVisit(Program *prog) override {
    std::cout << opentag("program") << std::endl;
  }
  void postVisit(Program *prog) override {
    std::cout << closetag("program") << std::endl;
  }

  void preVisit(FileUnit *fileUnit) override {
    std::cout << opentagWithIncIndent("fileUnit");
    incIndent();
    std::cout << onelinetagWithIndent("name", fileUnit->getFileName());
    decIndent();
  }
  void postVisit(FileUnit *fileUnit) override {
    std::cout << closetagWithDecIndent("fileUnit");
  }

  void preVisit(ClassDec *cl) override {
    std::cout << opentagWithIncIndent("class");
    incIndent();
    std::cout << onelinetagWithIndent("name", cl->getName());
    decIndent();
  }
  void postVisit(ClassDec *cl) override {
    std::cout << closetagWithDecIndent("class");
  }

  void preVisit(FieldVariableList *varList) override {
    std::cout << opentagWithIncIndent("fieldVariableList");
  }
  void postVisit(FieldVariableList *varList) override {
    std::cout << closetagWithDecIndent("fieldVariableList");
  }

  void preVisit(StaticVariableList *varList) override {
    std::cout << opentagWithIncIndent("staticVariableList");
  }
  void postVisit(StaticVariableList *varList) override {
    std::cout << closetagWithDecIndent("staticVariableList");
  }

  void preVisit(SubroutineArgumentList *varList) override {
    std::cout << opentagWithIncIndent("subroutineArgumentList");
  }
  void postVisit(SubroutineArgumentList *varList) override {
    std::cout << closetagWithDecIndent("subroutineArgumentList");
  }

  void preVisit(LocalVariableList *varList) override {
    std::cout << opentagWithIncIndent("LocalVariableList");
  }
  void postVisit(LocalVariableList *varList) override {
    std::cout << closetagWithDecIndent("LocalVariableList");
  }

  void preVisit(ArgumentVarDec *var) override {
    std::string kind;
    std::string varTypeStr = typeToStr(var->getVarType());
    std::cout << opentagWithIncIndent("argumentVariableDeclaration");
    incIndent();
    std::cout << onelinetagWithIndent("name", var->getVarName());
    std::cout << onelinetagWithIndent("type", varTypeStr);
    decIndent();
  }
  void postVisit(ArgumentVarDec *var) override {
    std::cout << closetagWithDecIndent("argumentVariableDeclaration");
  }

  void preVisit(LocalVarDec *var) override {
    std::string kind;
    std::string varTypeStr = typeToStr(var->getVarType());
    std::cout << opentagWithIncIndent("localVariableDeclaration");
    incIndent();
    std::cout << onelinetagWithIndent("name", var->getVarName());
    std::cout << onelinetagWithIndent("type", varTypeStr);
    decIndent();
  }
  void postVisit(LocalVarDec *var) override {
    std::cout << closetagWithDecIndent("localVariableDeclaration");
  }

  void preVisit(FieldVarDec *var) override {
    std::string kind;
    std::string varTypeStr = typeToStr(var->getVarType());
    std::cout << opentagWithIncIndent("fieldVariableDeclaration");
    incIndent();
    std::cout << onelinetagWithIndent("name", var->getVarName());
    std::cout << onelinetagWithIndent("type", varTypeStr);
    decIndent();
  }
  void postVisit(FieldVarDec *var) override {
    std::cout << closetagWithDecIndent("fieldVariableDeclaration");
  }

  void preVisit(StaticVarDec *var) override {
    std::string kind;
    std::string varTypeStr = typeToStr(var->getVarType());
    std::cout << opentagWithIncIndent("staticVariableDeclaration");
    incIndent();
    std::cout << onelinetagWithIndent("name", var->getVarName());
    std::cout << onelinetagWithIndent("type", varTypeStr);
    decIndent();
  }
  void postVisit(StaticVarDec *var) override {
    std::cout << closetagWithDecIndent("staticVariableDeclaration");
  }

  void preVisit(SubroutineList *subList) override {
    std::cout << opentagWithIncIndent("subroutineList");
  }
  void postVisit(SubroutineList *subList) override {
    std::cout << closetagWithDecIndent("subroutineList");
  }

  void preVisit(SubroutineDec *sub) override {
    std::string subrtnType;
    std::string retTypeStr = typeToStr(sub->getRetType());
    switch (sub->getSubroutineKind()) {
    case SubroutineKind::FUNCTION_S:
      subrtnType = "function";
      break;
    case SubroutineKind::METHOD_S:
      subrtnType = "method";
      break;
    case SubroutineKind::CONSTRUCTOR_S:
      subrtnType = "constructor";
      break;
    }
    std::cout << opentagWithIncIndent("subroutineDeclaration");
    incIndent();
    std::cout << onelinetagWithIndent("name", sub->getName());
    std::cout << onelinetagWithIndent("returnType", retTypeStr);
    std::cout << onelinetagWithIndent("kind", subrtnType);
    decIndent();
  }
  void postVisit(SubroutineDec *sub) override {
    std::cout << closetagWithDecIndent("subroutineDeclaration");
  }

  void preVisit(SubroutineBody *subBody) override {
    std::cout << opentagWithIncIndent("subroutineBody");
  }
  void postVisit(SubroutineBody *subBody) override {
    std::cout << closetagWithDecIndent("subroutineBody");
  }

  void preVisit(StatementList *stmtList) override {
    std::cout << opentagWithIncIndent("statementList");
  }
  void postVisit(StatementList *stmtList) override {
    std::cout << closetagWithDecIndent("statementList");
  }

  void preVisit(LetStatement *letStmt) override {
    std::cout << opentagWithIncIndent("letStatement");
  }
  void postVisit(LetStatement *letStmt) override {
    std::cout << closetagWithDecIndent("letStatement");
  }

  void preVisit(IfStatement *ifStmt) override {
    std::cout << opentagWithIncIndent("ifStatement");
  }
  void postVisit(IfStatement *ifStmt) override {
    std::cout << closetagWithDecIndent("ifStatement");
  }

  void preVisit(WhileStatement *whileStmt) override {
    std::cout << opentagWithIncIndent("whileStatement");
  }
  void postVisit(WhileStatement *whileStmt) override {
    std::cout << closetagWithDecIndent("whileStatement");
  }

  void preVisit(DoStatement *doStmt) override {
    std::cout << opentagWithIncIndent("doStatement");
  }
  void postVisit(DoStatement *doStmt) override {
    std::cout << closetagWithDecIndent("doStatement");
  }

  void preVisit(ReturnStatement *retStmt) override {
    std::cout << opentagWithIncIndent("retStatement");
  }
  void postVisit(ReturnStatement *retStmt) override {
    std::cout << closetagWithDecIndent("retStatement");
  }

  void preVisit(ExpressionList *exprList) override {
    std::cout << opentagWithIncIndent("expressionList");
  }
  void postVisit(ExpressionList *exprList) override {
    std::cout << closetagWithDecIndent("expressionList");
  }

  void preVisit(BinopExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    if (!exprKind.empty()) {
      std::cout << opentagWithIncIndent(exprKind);
    }
    std::string op = convertOpType(expr->getOpType());
    std::cout << opentagWithIncIndent("binaryOperation");
    incIndent();
    std::cout << onelinetagWithIndent("operation", op);
    decIndent();
  }
  void postVisit(BinopExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    std::cout << closetagWithDecIndent("binaryOperation");
    if (!exprKind.empty()) {
      std::cout << closetagWithDecIndent(exprKind);
    }
  }

  void preVisit(UnopExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    if (!exprKind.empty()) {
      std::cout << opentagWithIncIndent(exprKind);
    }
    std::string op = convertOpType(expr->getOpType());
    std::cout << opentagWithIncIndent("unaryOperation");
    incIndent();
    std::cout << onelinetagWithIndent("operation", op);
    decIndent();
  }
  void postVisit(UnopExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    std::cout << closetagWithDecIndent("unaryOperation");
    if (!exprKind.empty()) {
      std::cout << closetagWithDecIndent(exprKind);
    }
  }

  void preVisit(NewArrayExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    std::string arrayType = typeToStr(expr->getArrayType());
    if (!exprKind.empty()) {
      std::cout << opentagWithIncIndent(exprKind);
    }
    std::cout << opentagWithIncIndent("newArrayCall");
    incIndent();
    std::cout << onelinetagWithIndent("type", arrayType);
    decIndent();
  }
  void postVisit(NewArrayExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    std::cout << closetagWithDecIndent("newArrayCall");
    if (!exprKind.empty()) {
      std::cout << closetagWithDecIndent(exprKind);
    }
  }

  void preVisit(DeleteArrayExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    if (!exprKind.empty()) {
      std::cout << opentagWithIncIndent(exprKind);
    }
    std::cout << opentagWithIncIndent("deleteArrayCall");
  }
  void postVisit(DeleteArrayExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    std::cout << closetagWithDecIndent("newArrayCall");
    if (!exprKind.empty()) {
      std::cout << closetagWithDecIndent(exprKind);
    }
  }

  void preVisit(LiteralExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    std::string data = expr->getValue();
    if (!exprKind.empty()) {
      std::cout << opentagWithIncIndent(exprKind);
    }
    incIndent();
    std::cout << onelinetagWithIndent("literal", data);
    decIndent();
  }
  void postVisit(LiteralExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    if (!exprKind.empty()) {
      std::cout << closetagWithDecIndent(exprKind);
    }
  }

  void preVisit(NameExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    std::string data = expr->getName();
    if (!exprKind.empty()) {
      std::cout << opentagWithIncIndent(exprKind);
    }
    incIndent();
    std::cout << onelinetagWithIndent("name", data);
    decIndent();
  }
  void postVisit(NameExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    if (!exprKind.empty()) {
      std::cout << closetagWithDecIndent(exprKind);
    }
  }

  void preVisit(ArrayMemberExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    if (!exprKind.empty()) {
      std::cout << opentagWithIncIndent(exprKind);
    }
    std::cout << opentagWithIncIndent("arrayMemberExpression");
  }
  void postVisit(ArrayMemberExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    std::cout << closetagWithDecIndent("arrayMemberExpression");
    if (!exprKind.empty()) {
      std::cout << closetagWithDecIndent(exprKind);
    }
  }

  void preVisit(SubroutineCallExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    if (!exprKind.empty()) {
      std::cout << opentagWithIncIndent(exprKind);
    }
    std::cout << opentagWithIncIndent("callExpression");
  }
  void postVisit(SubroutineCallExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    std::cout << closetagWithDecIndent("callExpression");
    if (!exprKind.empty()) {
      std::cout << closetagWithDecIndent(exprKind);
    }
  }

  void preVisit(MemberExpr *expr) override {
    std::string exprKind = checkexprKind(expr);
    if (!exprKind.empty()) {
      std::cout << opentagWithIncIndent(exprKind);
    }
    std::cout << opentagWithIncIndent("memberExpression");
  }
  void postVisit(MemberExpr *expr) override {
    incIndent();
    std::cout << onelinetagWithIndent("member", expr->getMember());
    decIndent();
    std::string exprKind = checkexprKind(expr);
    std::cout << closetagWithDecIndent("memberExpression");
    if (!exprKind.empty()) {
      std::cout << closetagWithDecIndent(exprKind);
    }
  }

  std::string checkexprKind(Expression *expr) {
    NodeType stmtType = expr->getParent()->getNodeType();
    std::string exprKind;
    switch (stmtType) {
    case NodeType::IF_STATEMENT:
    case NodeType::WHILE_STATEMENT:
      exprKind = "condition";
      break;

    case NodeType::LET_STATEMENT:
      if (expr->getParent()->child(0).get() == expr) {
        exprKind = "leftExpression";
      } else {
        exprKind = "rightExpression";
      }
      break;

    case NodeType::BINOP_EXPR:
      if (expr->getParent()->child(0).get() == expr) {
        exprKind = "leftOperand";
      } else {
        exprKind = "rightOperand";
      }
      break;

    case NodeType::ARRAY_MEMBER_EXPR:
      if (expr->getParent()->child(0).get() == expr) {
        exprKind = "identifier";
      } else {
        exprKind = "indexExpression";
      }
      break;

    case NodeType::MEMBER_EXPR:
      exprKind = "identifier";
      break;

    default:
      return std::string{};
    }
    return exprKind;
  }

  std::string typeToStr(Type *type) { return type->toString(); }

  std::string convertOpType(OpType type) {
    std::string op;
    switch (type) {
    case OpType::ADD_OP:
      op = "+";
      break;
    case OpType::SUB_OP:
      op = "-";
      break;
    case OpType::MUL_OP:
      op = "*";
      break;
    case OpType::DIV_OP:
      op = "/";
      break;
    case OpType::LOG_AND_OP:
      op = "&&";
      break;
    case OpType::LOG_OR_OP:
      op = "||";
      break;
    case OpType::BIT_AND_OP:
      op = "&";
      break;
    case OpType::BIT_OR_OP:
      op = "|";
      break;
    case OpType::LSS_OP:
      op = "<";
      break;
    case OpType::GTR_OP:
      op = ">";
      break;
    case OpType::EQL_OP:
      op = "==";
      break;
    case OpType::NEG_OP:
      op = "-";
      break;
    case OpType::TILDE_OP:
      op = "~";
      break;
    }
    return op;
  }

  void incIndent() {
    indentSize_ += indentDiff_;
    currentIndent_ = std::string(indentSize_, ' ');
  }

  void decIndent() {
    indentSize_ -= indentDiff_;
    currentIndent_ = std::string(indentSize_, ' ');
  }

  std::string opentagWithIncIndent(std::string tag) {
    incIndent();
    return currentIndent_ + opentag(tag) + '\n';
  }

  std::string closetagWithDecIndent(std::string tag) {
    std::string tmp = currentIndent_ + closetag(tag) + '\n';
    decIndent();
    return tmp;
  }

  std::string onelinetagWithIndent(std::string tag, std::string data) {
    return currentIndent_ + opentag(tag) + " " + data + " " + closetag(tag) +
           '\n';
  }

  std::string opentag(std::string tagName) { return "<" + tagName + ">"; }

  std::string closetag(std::string tagName) { return "</" + tagName + ">"; }

  std::ofstream output_;
  std::string currentIndent_;
  unsigned int indentSize_ = 0;
  unsigned int indentDiff_ = 2;
};