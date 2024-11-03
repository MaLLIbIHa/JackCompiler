#pragma once
#include "AST2.hpp"
#include "Visitor.hpp"
#include "compiler/NodeDescriptors.hpp"
#include <cassert>
#include <string>

class PrintVisitor final : public Visitor {
public:
  PrintVisitor(int indentDiff, std::ostream& outStream)
    : indentDiff_(indentDiff), output_(outStream) {}

private:
  void preVisit(const Program *prog) override {
    output_ << opentag("program") << std::endl;
  }
  void postVisit(const Program *prog) override {
    output_ << closetag("program") << std::endl;
  }

  void preVisit(const ClassDec *cl) override {
    output_ << opentagWithIncIndent("class");
    incIndent();
    output_ << onelinetagWithIndent("name", cl->getName());
    decIndent();
  }
  void postVisit(const ClassDec *cl) override {
    output_ << closetagWithDecIndent("class");
  }

  void preVisit(const ArgumentVarDec* varDec) override {
    printVariableDec("argumentVariableDeclaration", varDec);
  }
  void postVisit(const ArgumentVarDec*) override {
    output_ << closetagWithDecIndent("argumentVariableDeclaration");
  }

  void preVisit(const LocalVarDec* varDec) override {
    printVariableDec("localVariableDeclaration", varDec);
  }
  void postVisit(const LocalVarDec*) override {
    output_ << closetagWithDecIndent("localVariableDeclaration");
  }

  void preVisit(const FieldVarDec* varDec) override {
    printVariableDec("fieldVariableDeclaration", varDec);
  }
  void postVisit(const FieldVarDec*) override {
    output_ << closetagWithDecIndent("fieldVariableDeclaration");
  }

  void preVisit(const StaticVarDec* varDec) override {
    printVariableDec("staticVariableDeclaration", varDec);
  }
  void postVisit(const StaticVarDec*) override {
    output_ << closetagWithDecIndent("staticVariableDeclaration");
  }

  void preVisit(const SubroutineList *subList) override {
    output_ << opentagWithIncIndent("subroutineList");
  }
  void postVisit(const SubroutineList *subList) override {
    output_ << closetagWithDecIndent("subroutineList");
  }

  void preVisit(const SubroutineDec *sub) override {
    std::string retTypeStr = sub->getReturnType()->toString();
    std::string subrtnType = [](NodeType nodeType) {
      switch (nodeType) {
      case NodeType::FUNCTION_DEC:
        return "function";
      case NodeType::METHOD_DEC:
        return "method";
      case NodeType::CONSTRUCTOR_DEC:
        return "constructor";
      default:
        assert(false && "unreachable");
      }
    }(sub->getNodeType());
    output_ << opentagWithIncIndent("subroutineDeclaration");
    incIndent();
    output_ << onelinetagWithIndent("name", sub->getName());
    output_ << onelinetagWithIndent("returnType", retTypeStr);
    output_ << onelinetagWithIndent("kind", subrtnType);
    decIndent();
  }
  void postVisit(const SubroutineDec *sub) override {
    output_ << closetagWithDecIndent("subroutineDeclaration");
  }

  void preVisit(const SubroutineBody *subBody) override {
    output_ << opentagWithIncIndent("subroutineBody");
  }
  void postVisit(const SubroutineBody *subBody) override {
    output_ << closetagWithDecIndent("subroutineBody");
  }

  void preVisit(const StatementList *stmtList) override {
    output_ << opentagWithIncIndent("statementList");
  }
  void postVisit(const StatementList *stmtList) override {
    output_ << closetagWithDecIndent("statementList");
  }

  void preVisit(const LetStatement* letStmt) override {
    output_ << opentagWithIncIndent("letStatement");
    output_ << opentagWithIncIndent("left");
  }
  void interVisit(const LetStatement* letStmt, unsigned) override {
    output_ << closetagWithDecIndent("left");
    output_ << opentagWithIncIndent("right");
  }
  void postVisit(const LetStatement *letStmt) override {
    output_ << closetagWithDecIndent("right");
    output_ << closetagWithDecIndent("letStatement");
  }

  void preVisit(const IfStatement* ifStmt) override {
    output_ << opentagWithIncIndent("ifStatement");
    output_ << opentagWithIncIndent("condition");
  }
  void interVisit(const IfStatement* ifStmt, unsigned visitCount) override {
    if (visitCount == 1) {
      output_ << closetagWithDecIndent("condition");
      output_ << opentagWithIncIndent("ifBody");
    } else if (visitCount == 2) {
      output_ << closetagWithDecIndent("ifBody");
      if (ifStmt->getElseBody() != nullptr) {
        output_ << opentagWithIncIndent("elseBody");
      }
    }
  }
  void postVisit(const IfStatement* ifStmt) override {
    if (ifStmt->getElseBody() == nullptr)
      output_ << closetagWithDecIndent("ifBody");
    else
      output_ << closetagWithDecIndent("elseBody");
    output_ << closetagWithDecIndent("ifStatement");
  }

  void preVisit(const WhileStatement* whileStmt) override {
    output_ << opentagWithIncIndent("whileStatement");
    output_ << opentagWithIncIndent("condition");
  }
  void interVisit(const WhileStatement* whileStmt,
                  unsigned visitCount) override {
    output_ << closetagWithDecIndent("condition");
    output_ << opentagWithIncIndent("whileBody");
  }
  void postVisit(const WhileStatement *whileStmt) override {
    output_ << closetagWithDecIndent("whileBody");
    output_ << closetagWithDecIndent("whileStatement");
  }

  void preVisit(const DoStatement *doStmt) override {
    output_ << opentagWithIncIndent("doStatement");
  }
  void postVisit(const DoStatement *doStmt) override {
    output_ << closetagWithDecIndent("doStatement");
  }

  void preVisit(const ReturnStatement *retStmt) override {
    output_ << opentagWithIncIndent("retStatement");
  }
  void postVisit(const ReturnStatement *retStmt) override {
    output_ << closetagWithDecIndent("retStatement");
  }

  void preVisit(const BinopExpr* expr) override {
    output_ << opentagWithIncIndent("binaryOperation");
  }
  void interVisit(const BinopExpr* expr, unsigned) override {
    std::string op = binopTypeToString(expr->getOpType());
    incIndent();
    output_ << onelinetagWithIndent("operation", op);
    decIndent();
  }
  void postVisit(const BinopExpr *expr) override {
    output_ << closetagWithDecIndent("binaryOperation");
  }

  void preVisit(const UnopExpr *expr) override {
    std::string op = unopTypeToString(expr->getOpType());
    output_ << opentagWithIncIndent("unaryOperation");
    incIndent();
    output_ << onelinetagWithIndent("operation", op);
    decIndent();
  }
  void postVisit(const UnopExpr *expr) override {
    output_ << closetagWithDecIndent("unaryOperation");
  }

  void preVisit(const NewArrayExpr *expr) override {
    std::string arrayType = expr->getArrayElemType()->toString();
    output_ << opentagWithIncIndent("newArrayCall");
    incIndent();
    output_ << onelinetagWithIndent("type", arrayType);
    decIndent();
  }
  void postVisit(const NewArrayExpr *expr) override {
    output_ << closetagWithDecIndent("newArrayCall");
  }

  void preVisit(const DeleteArrayExpr *expr) override {
    output_ << opentagWithIncIndent("deleteArrayCall");
  }
  void postVisit(const DeleteArrayExpr *expr) override {
    output_ << closetagWithDecIndent("deleteArrayCall");
  }

  void preVisit(const LiteralExpr *expr) override {
    incIndent();
    output_ << onelinetagWithIndent("literal", expr->getValue());
    decIndent();
  }

  void preVisit(const NameExpr *expr) override {
    incIndent();
    output_ << onelinetagWithIndent("name", expr->getName());
    decIndent();
  }

  void preVisit(const ArrayMemberExpr *expr) override {
    output_ << opentagWithIncIndent("arrayMemberExpression");
  }
  void postVisit(const ArrayMemberExpr *expr) override {
    output_ << closetagWithDecIndent("arrayMemberExpression");
  }

  void preVisit(const CallExpr *expr) override {
    output_ << opentagWithIncIndent("callExpression");
  }
  void interVisit(const CallExpr *expr, unsigned visitCnt) override {
    if (visitCnt != 1) {
      output_ << closetagWithDecIndent(
          "argument " + std::to_string(visitCnt - 1));
    }
    output_ << opentagWithIncIndent("argument " + std::to_string(visitCnt));
  }
  void postVisit(const CallExpr *expr) override {
    if (expr->getArgsCount() != 0) {
      output_ << closetagWithDecIndent(
          "argument" + std::to_string(expr->getArgsCount()));
    }
    output_ << closetagWithDecIndent("callExpression");
  }

  void preVisit(const MemberExpr *expr) override {
    output_ << opentagWithIncIndent("memberExpression");
  }
  void postVisit(const MemberExpr *expr) override {
    incIndent();
    output_ << onelinetagWithIndent("member", expr->getMember());
    decIndent();
  }

  void printVariableDec(std::string tag, const VariableDec* varDec) {
    std::string varTypeStr = varDec->getVarType()->toString();
    output_ << opentagWithIncIndent(tag);
    incIndent();
    output_ << onelinetagWithIndent("name", varDec->getVarName());
    output_ << onelinetagWithIndent("type", varTypeStr);
    decIndent();
  }

  std::string binopTypeToString(BinopType type) {
    switch (type) {
    case BinopType::ADD_OP:
      return "+";
    case BinopType::SUB_OP:
      return "-";
    case BinopType::MUL_OP:
      return "*";
    case BinopType::DIV_OP:
      return "/";
    case BinopType::LOG_AND_OP:
      return "&&";
    case BinopType::LOG_OR_OP:
      return "||";
    case BinopType::BIT_AND_OP:
      return "&";
    case BinopType::BIT_OR_OP:
      return "|";
    case BinopType::LSS_OP:
      return "<";
    case BinopType::GTR_OP:
      return ">";
    case BinopType::EQL_OP:
      return "==";
    }
  }

  std::string unopTypeToString(UnopType type) {
    switch(type) {
    case UnopType::NEG_OP:
      return "-";
    case UnopType::NOT_OP:
      return "~";
    }
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

  std::ostream& output_;
  std::string currentIndent_;
  unsigned int indentSize_ = 0;
  unsigned int indentDiff_ = 2;
};