#pragma once

class Program;
class FileUnit;
class ClassDec;
class VariableDecList;
class FieldVariableList;
class StaticVariableList;
class SubroutineArgumentList;
class LocalVariableList;
class SubroutineDec;
class SubroutineList;
class ConstructorDec;
class FunctionDec;
class MethodDec;
class SubroutineBody;
class VariableDec;
class FieldVarDec;
class StaticVarDec;
class ArgumentVarDec;
class LocalVarDec;
class StatementList;
class LetStatement;
class IfStatement;
class WhileStatement;
class DoStatement;
class ReturnStatement;
class ExpressionList;
class BinopExpr;
class UnopExpr;
class NewArrayExpr;
class DeleteArrayExpr;
class LiteralExpr;
class NameExpr;
class ArrayMemberExpr;
class SubroutineCallExpr;
class MemberExpr;
class Expression;

struct Visitor {
    virtual void preVisit(Program *prog) {}
    virtual void preVisit(FileUnit *fileUnit) {}
    virtual void preVisit(ClassDec *cl) {}
    virtual void preVisit(VariableDecList *varList) {}
    virtual void preVisit(StaticVariableList *staticVarList) {}
    virtual void preVisit(FieldVariableList *memberVarList) {}
    virtual void preVisit(SubroutineArgumentList *argList) {}
    virtual void preVisit(LocalVariableList *localVarList) {}
    virtual void preVisit(SubroutineList *subList) {}
    virtual void preVisit(SubroutineDec *subDec) {}
    virtual void preVisit(SubroutineBody *subBody) {}
    virtual void preVisit(VariableDec *varDec) {}
    virtual void preVisit(ArgumentVarDec *varDec) {}
    virtual void preVisit(LocalVarDec *varDec) {}
    virtual void preVisit(StaticVarDec *varDec) {}
    virtual void preVisit(FieldVarDec *varDec) {}
    virtual void preVisit(StatementList *stmtList) {}
    virtual void preVisit(LetStatement *letStmt) {}
    virtual void preVisit(IfStatement *ifStmt) {}
    virtual void preVisit(WhileStatement *whileStmt) {}
    virtual void preVisit(DoStatement *doStmt) {}
    virtual void preVisit(ReturnStatement *retStmt) {}
    virtual void preVisit(ExpressionList *exprList) {}
    virtual void preVisit(BinopExpr *binop) {}
    virtual void preVisit(UnopExpr *unop) {}
    virtual void preVisit(NewArrayExpr *newArrayExpr) {}
    virtual void preVisit(DeleteArrayExpr *deleteArrayExpr) {}
    virtual void preVisit(LiteralExpr *literal) {}
    virtual void preVisit(NameExpr *literal) {}
    virtual void preVisit(ArrayMemberExpr *arrExpr) {}
    virtual void preVisit(SubroutineCallExpr *subCall) {}
    virtual void preVisit(MemberExpr *memExpr) {}
    virtual void preVisit(Expression *expr) {}

    virtual void postVisit(Program *prog) {}
    virtual void postVisit(FileUnit *fileUnit) {}
    virtual void postVisit(ClassDec *cl) {}
    virtual void postVisit(VariableDecList *varList) {}
    virtual void postVisit(StaticVariableList *staticVarList) {}
    virtual void postVisit(FieldVariableList *memberVarList) {}
    virtual void postVisit(SubroutineArgumentList *argList) {}
    virtual void postVisit(LocalVariableList *localVarList) {}
    virtual void postVisit(SubroutineList *subList) {}
    virtual void postVisit(SubroutineDec *subDec) {}
    virtual void postVisit(SubroutineBody *subBody) {}
    virtual void postVisit(VariableDec *varDec) {}
    virtual void postVisit(ArgumentVarDec *varDec) {}
    virtual void postVisit(LocalVarDec *varDec) {}
    virtual void postVisit(StaticVarDec *varDec) {}
    virtual void postVisit(FieldVarDec *varDec) {}
    virtual void postVisit(StatementList *stmtList) {}
    virtual void postVisit(LetStatement *letStmt) {}
    virtual void postVisit(IfStatement *ifStmt) {}
    virtual void postVisit(WhileStatement *whileStmt) {}
    virtual void postVisit(DoStatement *doStmt) {}
    virtual void postVisit(ReturnStatement *retStmt) {}
    virtual void postVisit(ExpressionList *exprList) {}
    virtual void postVisit(BinopExpr *binop) {}
    virtual void postVisit(UnopExpr *unop) {}
    virtual void postVisit(NewArrayExpr *newArrayExpr) {}
    virtual void postVisit(DeleteArrayExpr *deleteArrayExpr) {}
    virtual void postVisit(LiteralExpr *literal) {}
    virtual void postVisit(NameExpr *literal) {}
    virtual void postVisit(ArrayMemberExpr *arrExpr) {}
    virtual void postVisit(SubroutineCallExpr *subCall) {}
    virtual void postVisit(MemberExpr *memExpr) {}
    virtual void postVisit(Expression *expr) {}

    virtual ~Visitor() = default;
};