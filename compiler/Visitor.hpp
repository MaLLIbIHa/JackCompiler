#pragma once

class Program;
class StatementList;
class BinopExpr;
class UnopExpr;
class NewArrayExpr;
class DeleteArrayExpr;
class LiteralExpr;
class NameExpr;
class ArrayMemberExpr;
class CallExpr;
class MemberExpr;
class LetStatement;
class IfStatement;
class WhileStatement;
class DoStatement;
class ReturnStatement;
class VariableDec;
class FieldVarDec;
class StaticVarDec;
class LocalVarDec;
class ArgumentVarDec;
class SubroutineBody;
class SubroutineList;
class SubroutineDec;
class ConstructorDec;
class FunctionDec;
class MethodDec;
class ClassDec;

struct Visitor {
  virtual void preVisit(const Program *) {}
  virtual void preVisit(const ClassDec *) {}
  virtual void preVisit(const SubroutineDec *) {}
  virtual void preVisit(const SubroutineList *) {}
  virtual void preVisit(const SubroutineBody *) {}
  virtual void preVisit(const VariableDec *) {}
  virtual void preVisit(const ArgumentVarDec *) {}
  virtual void preVisit(const LocalVarDec *) {}
  virtual void preVisit(const StaticVarDec *) {}
  virtual void preVisit(const FieldVarDec *) {}
  virtual void preVisit(const StatementList *) {}
  virtual void preVisit(const LetStatement *) {}
  virtual void preVisit(const IfStatement *) {}
  virtual void preVisit(const WhileStatement *) {}
  virtual void preVisit(const DoStatement *) {}
  virtual void preVisit(const ReturnStatement *) {}
  virtual void preVisit(const BinopExpr *) {}
  virtual void preVisit(const UnopExpr *) {}
  virtual void preVisit(const NewArrayExpr *) {}
  virtual void preVisit(const DeleteArrayExpr *) {}
  virtual void preVisit(const LiteralExpr *) {}
  virtual void preVisit(const NameExpr *) {}
  virtual void preVisit(const ArrayMemberExpr *) {}
  virtual void preVisit(const CallExpr *) {}
  virtual void preVisit(const MemberExpr *) {}

  virtual void postVisit(const Program *) {}
  virtual void postVisit(const ClassDec *) {}
  virtual void postVisit(const SubroutineDec *) {}
  virtual void postVisit(const SubroutineList *) {}
  virtual void postVisit(const SubroutineBody *) {}
  virtual void postVisit(const VariableDec *) {}
  virtual void postVisit(const ArgumentVarDec *) {}
  virtual void postVisit(const LocalVarDec *) {}
  virtual void postVisit(const StaticVarDec *) {}
  virtual void postVisit(const FieldVarDec *) {}
  virtual void postVisit(const StatementList *) {}
  virtual void postVisit(const LetStatement *) {}
  virtual void postVisit(const IfStatement *) {}
  virtual void postVisit(const WhileStatement *) {}
  virtual void postVisit(const DoStatement *) {}
  virtual void postVisit(const ReturnStatement *) {}
  virtual void postVisit(const BinopExpr *) {}
  virtual void postVisit(const UnopExpr *) {}
  virtual void postVisit(const NewArrayExpr *) {}
  virtual void postVisit(const DeleteArrayExpr *) {}
  virtual void postVisit(const LiteralExpr *) {}
  virtual void postVisit(const NameExpr *) {}
  virtual void postVisit(const ArrayMemberExpr *) {}
  virtual void postVisit(const CallExpr *) {}
  virtual void postVisit(const MemberExpr *) {}

  virtual ~Visitor() = default;
};