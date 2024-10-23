#pragma once
#include "AST2.hpp"
#include "Lexer.hpp"
#include "Token.hpp"
#include "compiler/ASTContext.hpp"
#include <iterator>

enum class Associativity {
  LEFT_ASC,
  RIGHT_ASC,
};

class Parser final {
public:
  Parser(std::string sourceText, ASTContext &Ctx);

  // Main parsing methods
  void parseProgram();

private:
  void parseClass();

  SubroutineDec* parseSubroutineDec();

  SubroutineBody* parseSubroutineBody();

  StatementList* parseStatements();

  Type *parseType();

  Type *parseReturnType();

  void parseVarDec(std::back_insert_iterator<std::vector<VariableDec *>>);

  // Statements parsing methods
  Statement* parseLet();

  Statement* parseIf();

  Statement* parseWhile();

  Statement* parseDo();

  Statement* parseReturn();

  // Expression parsing methods
  Expression* parseExpression(int = 0);

  Expression* parseTerm();

  Expression* parseNewArray();

  Expression* parseDeleteArray();

  std::vector<Expression *> parseArgList();

  Expression*
  parseCompoundId(Expression* currNode);

  Expression* createUnopNode(SourceLocation srcLoc, Token opTok,
                             Expression* operand);

  Expression*
  createBinopNode(SourceLocation srcLoc, Token opTok, Expression* lhs,
                  Expression* rhs);

  SubroutineDec* createSubroutineDec(SourceLocation srcLoc, Token subrtnKind,
                                     std::string name,
                                     std::vector<VariableDec *> args,
                                     Type* retType,
                                     SubroutineBody* body);

  VariableDec* createVariableDec(std::string name, Type *type,
                                 VarModifier mod);

  // Token stream functions
  bool isType(Token tok);

  bool isUnaryOp(Token tok);

  bool isBinaryOp(Token tok);

  bool isClassVarDec(Token tok);

  bool isFuncDec(Token tok);

  bool isConstantLiteral(Token tok);

  Token consume(TokenType type);

  Token consume(TokenKind kind);

  Token consume();

  const Token& currentToken();

  bool hasTokens();

  int getPrec(Token tok);

  Associativity getAsc(Token tok);

  Type *tokenToType(Token typeTok);

  VarModifier tokenToVarModifier(Token modTok);

  SubroutineKind tokenToSubroutineKind(Token typeTok);

  Type *tokenToLiteralType(Token kind);

  // Error output functions
  void printExpectedErr(TokenKind kind, SourceLocation srcLoc);

  void printExpectedErr(TokenType type, SourceLocation srcLoc);

  void printExpectedErr(const char *errMsg, SourceLocation srcLoc);

  ASTContext &Ctx_;
  std::unique_ptr<Lexer> lexer_;
};