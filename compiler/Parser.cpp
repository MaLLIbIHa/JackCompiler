#include "Parser.hpp"
#include "compiler/AST2.hpp"
#include "compiler/ASTContext.hpp"
#include "compiler/Token.hpp"
#include "compiler/Visitor.hpp"
#include <cassert>
#include <iterator>
#include <vector>

std::map<std::string, std::pair<int, Associativity>> opInfo_ = {
    {"||", {1, Associativity::LEFT_ASC}}, {"&&", {2, Associativity::LEFT_ASC}},
    {"|", {3, Associativity::LEFT_ASC}},  {"&", {4, Associativity::LEFT_ASC}},
    {"==", {5, Associativity::LEFT_ASC}}, {"<", {6, Associativity::LEFT_ASC}},
    {">", {6, Associativity::LEFT_ASC}},  {"+", {7, Associativity::LEFT_ASC}},
    {"-", {7, Associativity::LEFT_ASC}},  {"*", {8, Associativity::LEFT_ASC}},
    {"/", {8, Associativity::LEFT_ASC}},  {"-u", {9, Associativity::RIGHT_ASC}},
    {"~", {9, Associativity::RIGHT_ASC}}, {".", {10, Associativity::LEFT_ASC}},
    {"[", {10, Associativity::LEFT_ASC}}, {"(", {10, Associativity::LEFT_ASC}},
};

Parser::Parser(std::string sourceText, ASTContext &Ctx)
    : Ctx_(Ctx), lexer_(std::make_unique<Lexer>(sourceText)) {}

void Parser::parseProgram() {
  while (hasTokens()) {
    parseClass();
  }
}

void Parser::parseClass() {
  Token classKeyword = consume(TokenKind::CLASS);
  std::string className = consume(TokenType::IDENTIFIER).getValue();
  consume(TokenKind::LBRACE);

  std::vector<VariableDec *> vars;
  while (hasTokens() && isClassVarDec(currentToken())) {
    parseVarDec(std::back_inserter(vars));
  }

  std::vector<SubroutineDec *> subrtns;
  while (hasTokens() && isFuncDec(currentToken())) {
    auto subrtn = parseSubroutineDec();
    subrtns.push_back(subrtn);
  }

  consume(TokenKind::RBRACE);
  Ctx_.createNode<ClassDec>(classKeyword.getSourceLocation(),
                            std::move(className),
                            std::move(vars),
                            std::move(subrtns));
}

void Parser::parseVarDec(
    std::back_insert_iterator<std::vector<VariableDec*>> varInserter) {
  bool endedStmt = false;
  Token varKindTok = consume();
  VarModifier mod = tokenToVarModifier(varKindTok);

  Type *varType = parseType();

  while (hasTokens()) {
    Token id = consume(TokenType::IDENTIFIER);
    auto var = createVariableDec(id.getValue(), varType, mod);
    varInserter = var;

    Token sepTok = consume();
    if (sepTok.getKind() == TokenKind::COMMA) {
      continue;
    } else if (sepTok.getKind() == TokenKind::SEMICOLON) {
      endedStmt = true;
      break;
    } else {
      printExpectedErr("Expected \",\" or \";\"",
                       sepTok.getSourceLocation());
      throw std::runtime_error("Parser error");
    }
  }

  if (!endedStmt) {
    printExpectedErr("Unexpected end of Statement",
                     varKindTok.getSourceLocation());
    throw std::runtime_error("Parser error");
  }
}

// function|method|constructor type|void name ( type name, ...)
SubroutineDec* Parser::parseSubroutineDec() {
  bool endedStmt = false;
  Token subrtnKindTok = consume();
  Type *retType = parseReturnType();
  std::string name = consume(TokenType::IDENTIFIER).getValue();
  consume(TokenKind::LPAREN);
  
  std::vector<VariableDec *> args;
  while (hasTokens() &&
         currentToken().getKind() != TokenKind::RPAREN) {
    Type *argType = parseType();
    Token argId = consume(TokenType::IDENTIFIER);
    auto arg = Ctx_.createNode<ArgumentVarDec>(argId.getSourceLocation(),
                                               std::move(argId.getValue()),
                                               argType);
    args.push_back(arg);

    Token sepToken = currentToken();
    if (sepToken.getKind() == TokenKind::COMMA) {
      consume();
      continue;
    } else if (sepToken.getKind() == TokenKind::RPAREN) {
      endedStmt = true;
      break;
    } else {
      printExpectedErr("Expected \",\" or \")\"",
                       sepToken.getSourceLocation());
      throw std::runtime_error("Parser error");
    }
  }

  consume(TokenKind::RPAREN);

  auto body = parseSubroutineBody();
  return createSubroutineDec(subrtnKindTok.getSourceLocation(), 
                             std::move(subrtnKindTok), name,
                             std::move(args), retType, body);
}

SubroutineBody* Parser::parseSubroutineBody() {
  std::vector<VariableDec *> vars;
  Token bodyBeginTok = consume(TokenKind::LBRACE);
  while (hasTokens() &&
         currentToken().getKind() == TokenKind::VAR) {
    parseVarDec(std::back_inserter(vars));
  }

  auto statements = parseStatements();
  consume(TokenKind::RBRACE);
  return Ctx_.createNode<SubroutineBody>(bodyBeginTok.getSourceLocation(),
                                         std::move(vars),
                                         std::move(statements));
}

StatementList* Parser::parseStatements() {
  bool endedBody = false;
  std::vector<Statement *> statements;
  while (hasTokens()) {
    Statement* stmt;
    Token stmtTok = currentToken();
    switch (stmtTok.getKind()) {
    case TokenKind::IF:
      stmt = parseIf();
      break;
    case TokenKind::LET:
      stmt = parseLet();
      break;
    case TokenKind::WHILE:
      stmt = parseWhile();
      break;
    case TokenKind::DO:
      stmt = parseDo();
      break;
    case TokenKind::RETURN:
      stmt = parseReturn();
      break;
    case TokenKind::RBRACE:
      endedBody = true;
      break;
    default:
      printExpectedErr("Expected Statement or \"}\"",
                       stmtTok.getSourceLocation());
      throw std::runtime_error("Parser error");
    }

    if (stmt != nullptr)
      statements.push_back(stmt);

    if (endedBody) {
      break;
    }
  }
  return Ctx_.createNode<StatementList>(std::move(statements));
}

Statement* Parser::parseIf() {
  Token ifTok = consume(TokenKind::IF);
  consume(TokenKind::LPAREN);
  auto condition = parseExpression();
  consume(TokenKind::RPAREN);
  consume(TokenKind::LBRACE);
  auto ifBody = parseStatements();
  consume(TokenKind::RBRACE);
  StatementList* elseBody;
  if (currentToken().getKind() == TokenKind::ELSE) {
    consume(TokenKind::ELSE);
    consume(TokenKind::LBRACE);
    elseBody = parseStatements();
    consume(TokenKind::RBRACE);
  }
  return Ctx_.createNode<IfStatement>(ifTok.getSourceLocation(), 
                                      condition, ifBody, elseBody);
}

Statement* Parser::parseLet() {
  Token letTok = consume(TokenKind::LET);
  auto lhs = parseExpression();
  consume(TokenKind::ASSIGN);
  auto rhs = parseExpression();
  consume(TokenKind::SEMICOLON);
  return Ctx_.createNode<LetStatement>(letTok.getSourceLocation(),
                                       lhs, rhs);
}

Statement* Parser::parseWhile() {
  Token whileTok = consume(TokenKind::WHILE);
  consume(TokenKind::LPAREN);
  auto condition = parseExpression();
  consume(TokenKind::RPAREN);
  consume(TokenKind::LBRACE);
  auto body = parseStatements();
  consume(TokenKind::RBRACE);
  return Ctx_.createNode<WhileStatement>(whileTok.getSourceLocation(),
                                         condition, body);
}

Statement* Parser::parseReturn() {
  Token retTok = consume(TokenKind::RETURN);
  Expression* retExpr = nullptr;
  if (currentToken().getKind() == TokenKind::SEMICOLON) {
    consume(TokenKind::SEMICOLON);
    retExpr = parseExpression();
  }
  consume(TokenKind::SEMICOLON);
  return Ctx_.createNode<ReturnStatement>(retTok.getSourceLocation(),
                                          retExpr);
}

Statement* Parser::parseDo() {
  Token doTok = consume(TokenKind::DO);
  auto callExpr = parseExpression();
  consume(TokenKind::SEMICOLON);
  return Ctx_.createNode<DoStatement>(doTok.getSourceLocation(), callExpr);
}

Expression* Parser::parseExpression(int prevPrec) {
  Expression* firstOperand = parseTerm();
  while (isBinaryOp(currentToken()) &&
         getPrec(currentToken()) >= prevPrec) {
    Token op = consume();
    int nextPrec = getPrec(op);
    if (getAsc(op) == Associativity::LEFT_ASC) {
      nextPrec++;
    }
    auto secondOperand = parseExpression(nextPrec);
    auto newBinop = createBinopNode(firstOperand->getSourceLoc(), op,
                                    firstOperand, secondOperand);
    firstOperand = newBinop;
  }
  return firstOperand;
}

//  Parse Expression which includes parenthesised Expressions,
//  Expressions with unary operators and compound identifiers.
Expression* Parser::parseTerm() {
  Token curr = currentToken();
  if (isUnaryOp(curr)) {
    Token op = consume();
    int precedence = getPrec(op);
    Expression* expr = parseExpression(precedence);
    return createUnopNode(curr.getSourceLocation(), op, expr);
  } else if (curr.getKind() == TokenKind::LPAREN) {
    consume(TokenKind::LPAREN);
    Expression* expr = parseExpression();
    consume(TokenKind::RPAREN);
    return expr;
  } else if (curr.getType() == TokenType::IDENTIFIER ||
             curr.getKind() == TokenKind::THIS) {
    Token name = consume();
    auto currentNode =
      Ctx_.createNode<NameExpr>(name.getSourceLocation(), name.getValue());
    return parseCompoundId(currentNode);
  } else if (curr.getKind() == TokenKind::NEW_ARRAY) {
    Token newArrayTok = curr;
    auto newArrayNode = parseNewArray();
    return parseCompoundId(newArrayNode);
  } else if (curr.getKind() == TokenKind::DELETE_ARRAY) {
    Token deleteArrayTok = curr;
    auto deleteArrayNode = parseDeleteArray();
    return parseCompoundId(deleteArrayNode);
  } else if (isConstantLiteral(curr)) {
    Token lit = consume();
    Type *litType = tokenToLiteralType(lit);
    auto litExpr = Ctx_.createNode<LiteralExpr>(lit.getSourceLocation(),
                                                lit.getValue(), litType);
    return litExpr;
  } else {
    printExpectedErr("Expected term expression", curr.getSourceLocation());
    throw std::runtime_error("Parser error");
  }
  assert(false && "unreachable");
}

Expression* Parser::parseNewArray() {
  Token newArrTok = consume(TokenKind::NEW_ARRAY);
  consume(TokenKind::LPAREN);
  Token tok = consume();
  if (!isType(tok)) {
    printExpectedErr("Expected type", tok.getSourceLocation());
    throw std::runtime_error("Parser error");
  }
  consume(TokenKind::COMMA);
  auto sizeExpr = parseExpression();
  consume(TokenKind::RPAREN);
  return Ctx_.createNode<NewArrayExpr>(newArrTok.getSourceLocation(),
                                       tokenToType(tok), sizeExpr);
}

Expression* Parser::parseDeleteArray() {
  Token deleteArrTok = consume(TokenKind::DELETE_ARRAY);
  consume(TokenKind::LPAREN);
  auto sizeExpr = parseExpression();
  consume(TokenKind::RPAREN);
  return Ctx_.createNode<DeleteArrayExpr>(
      deleteArrTok.getSourceLocation(), sizeExpr);
}

//  Parse compound identifiers which includes dot, square brackets,
//  function call operators.
//  First identifier can be class name or this keyword
Expression*
Parser::parseCompoundId(Expression* currNode) {
  Token nextId;
  Expression* newNode;
  Expression* memberExpr;
  std::vector<Expression *> subrtnArgs;

  while (hasTokens()) {
    switch (currentToken().getKind()) {
    case TokenKind::DOT:
      consume(TokenKind::DOT);
      nextId = consume(TokenType::IDENTIFIER);
      newNode = Ctx_.createNode<MemberExpr>(currNode->getSourceLoc(),
                                            currNode, nextId.getValue());
      break;

    case TokenKind::LBRACK:
      consume(TokenKind::LBRACK);
      memberExpr = parseExpression();
      consume(TokenKind::RBRACK);
      newNode = Ctx_.createNode<ArrayMemberExpr>(currNode->getSourceLoc(),
                                                 currNode, memberExpr);
      break;

    case TokenKind::LPAREN:
      nextId = consume(TokenKind::LPAREN);
      subrtnArgs = parseArgList();
      consume(TokenKind::RPAREN);
      newNode = Ctx_.createNode<CallExpr>(currNode->getSourceLoc(),
                                          currNode, std::move(subrtnArgs));
      break;

    default:
      return currNode;
    }

    currNode = newNode;
  }
  printExpectedErr("Unexpected token in compound identifier",
                   currentToken().getSourceLocation());
  throw std::runtime_error("Parser error");
}

std::vector<Expression *> Parser::parseArgList() {
  Token argListBeg = currentToken();
  std::vector<Expression *> argExprs;
  if (argListBeg.getKind() == TokenKind::RPAREN) {
    return argExprs;
  }
  bool endedStmt = false;
  while (hasTokens()) {
    auto expr = parseExpression();
    argExprs.push_back(expr);
    if (currentToken().getKind() == TokenKind::RPAREN) {
      endedStmt = true;
      break;
    }
    consume(TokenKind::COMMA);
  }

  if (!endedStmt) {
    printExpectedErr("Unexpected end of arguments list",
                     argListBeg.getSourceLocation());
    throw std::runtime_error("Parser error");
  }

  return argExprs;
}

Type *Parser::parseType() {
  Token typeTok = consume();
  if (!isType(typeTok)) {
    printExpectedErr("Expected type", typeTok.getSourceLocation());
    throw std::runtime_error("Parser error");
  }
  Type *type = tokenToType(typeTok);
  if (currentToken().getKind() == TokenKind::LBRACK) {
    consume(TokenKind::LBRACK);
    consume(TokenKind::RBRACK);
    type = ArrayType::getArrayTy(type);
  }
  return type;
}

Type *Parser::parseReturnType() {
  if (currentToken().getKind() == TokenKind::VOID) {
    consume();
    return Type::getVoidTy();
  }
  return parseType();
}

Expression*
Parser::createUnopNode(SourceLocation srcLoc, Token opTok,
                       Expression* operand) {
  OpType opType = [](TokenKind kind) {
    switch (kind) {
    case TokenKind::SUB:
      return OpType::NEG_OP;
    case TokenKind::TILDE:
      return OpType::TILDE_OP;
    default:
      assert(false && "unreachable");
    }
  }(opTok.getKind());
  return Ctx_.createNode<UnopExpr>(srcLoc, operand, opType);
}

Expression*
Parser::createBinopNode(SourceLocation srcLoc, Token opTok,
                        Expression* lhs, Expression* rhs) {
  OpType opType = [](TokenKind kind) {
    switch (kind) {
    case TokenKind::ADD:
      return OpType::ADD_OP;
    case TokenKind::SUB:
      return OpType::SUB_OP;
    case TokenKind::MUL:
      return OpType::MUL_OP;
    case TokenKind::QUO:
      return OpType::DIV_OP;
    case TokenKind::LOG_AND:
      return OpType::LOG_AND_OP;
    case TokenKind::LOG_OR:
      return OpType::LOG_OR_OP;
    case TokenKind::BIT_AND:
      return OpType::BIT_AND_OP;
    case TokenKind::LSS:
      return OpType::LSS_OP;
    case TokenKind::GTR:
      return OpType::GTR_OP;
    case TokenKind::EQL:
      return OpType::EQL_OP;
    default:
      assert(false && "unreachable");
    }
  }(opTok.getKind());
  return Ctx_.createNode<BinopExpr>(srcLoc, lhs, rhs, opType);
}

SubroutineDec*
Parser::createSubroutineDec(SourceLocation srcLoc, Token subrtnKind,
                            std::string name,
                            std::vector<VariableDec *> args,
                            Type* retType,
                            SubroutineBody* body) {
  switch(subrtnKind.getKind()) {
  case TokenKind::CONSTRUCTOR:
    return Ctx_.createNode<ConstructorDec>(srcLoc, name, args, retType, body);
  case TokenKind::FUNCTION:
    return Ctx_.createNode<FunctionDec>(srcLoc, name, args, retType, body);
  case TokenKind::METHOD:
    return Ctx_.createNode<MethodDec>(srcLoc, name, args, retType, body);
  default:
    assert(false && "wrong subroutine kind token provided "
                    "in createSubroutineDec()");
  }
  assert(false && "unreachable");
}

VariableDec*
Parser::createVariableDec(std::string name, Type *type, VarModifier mod) {
  switch (mod) {
  case VarModifier::ARG_V:
    return Ctx_.createNode<ArgumentVarDec>(std::move(name), type);
  case VarModifier::FIELD_V:
    return Ctx_.createNode<FieldVarDec>(std::move(name), type);
  case VarModifier::LOCAL_V:
    return Ctx_.createNode<LocalVarDec>(std::move(name), type);
  case VarModifier::STATIC_V:
    return Ctx_.createNode<StaticVarDec>(std::move(name), type);
  }
  assert(false && "unreachable");
}

Token Parser::consume(TokenType type) {
  Token cur = currentToken();
  if (cur.getType() == type) {
    return consume();
  } else {
    printExpectedErr(type, cur.getSourceLocation());
    throw std::runtime_error("Parser error");
  }
  return Token();
}

Token Parser::consume(TokenKind kind) {
  auto&& cur = currentToken();
  if (cur.getKind() == kind) {
    return consume();
  } else {
    printExpectedErr(kind, cur.getSourceLocation());
    throw std::runtime_error("Parser error");
  }
  return Token();
}

Token Parser::consume() { return lexer_->consume(); }

const Token& Parser::currentToken() { return lexer_->currentToken(); }

bool Parser::hasTokens() { return lexer_->hasTokens(); }

bool Parser::isClassVarDec(Token tok) {
  if (tok.getKind() == TokenKind::STATIC || tok.getKind() == TokenKind::FIELD) {
    return true;
  }
  return false;
}

bool Parser::isFuncDec(Token tok) {
  if (tok.getKind() == TokenKind::CONSTRUCTOR ||
      tok.getKind() == TokenKind::FUNCTION ||
      tok.getKind() == TokenKind::METHOD) {
    return true;
  }
  return false;
}

bool Parser::isType(Token tok) {
  if (tok.getKind() == TokenKind::INT || tok.getKind() == TokenKind::CHAR ||
      tok.getKind() == TokenKind::BOOLEAN ||
      tok.getType() == TokenType::IDENTIFIER) {
    return true;
  }
  return false;
}

bool Parser::isConstantLiteral(Token tok) {
  if (tok.getType() == TokenType::STR_LITERAL ||
      tok.getType() == TokenType::CHAR_LITERAL ||
      tok.getType() == TokenType::INT_LITERAL ||
      tok.getKind() == TokenKind::TRUE || tok.getKind() == TokenKind::FALSE ||
      tok.getKind() == TokenKind::NULL_KEYWORD) {
    return true;
  }
  return false;
}

bool Parser::isUnaryOp(Token tok) {
  if (tok.getKind() == TokenKind::TILDE || tok.getKind() == TokenKind::SUB) {
    return true;
  }
  return false;
}

bool Parser::isBinaryOp(Token tok) {
  if (TokenKind::binopBeg < tok.getKind() &&
      tok.getKind() < TokenKind::binopEnd) {
    return true;
  }
  return false;
}

int Parser::getPrec(Token tok) {
  return opInfo_.find(tok.getValue())->second.first;
}

Associativity Parser::getAsc(Token tok) {
  return opInfo_.find(tok.getValue())->second.second;
}

Type *Parser::tokenToType(Token typeTok) {
  if (typeTok.getType() == TokenType::KEYWORD) {
    switch (typeTok.getKind()) {
    case TokenKind::INT:
      return Type::getIntTy();
    case TokenKind::CHAR:
      return Type::getCharTy();
    case TokenKind::BOOLEAN:
      return Type::getBoolTy();
    case TokenKind::VOID:
      return Type::getVoidTy();
    default:
      assert(false && "Provided wrong token in tokenToType()");
    }
  }
  return ClassType::getClassTy(typeTok.getValue());
}

VarModifier Parser::tokenToVarModifier(Token modTok) {
  switch (modTok.getKind()) {
  case TokenKind::STATIC:
    return VarModifier::STATIC_V;
  case TokenKind::FIELD:
    return VarModifier::FIELD_V;
  case TokenKind::VAR:
    return VarModifier::LOCAL_V;
  default:
    assert(false && "Provided wrong token in tokenToVarModifier()");
  }
}

SubroutineKind Parser::tokenToSubroutineKind(Token typeTok) {
  switch (typeTok.getKind()) {
  case TokenKind::METHOD:
    return SubroutineKind::METHOD_S;
  case TokenKind::CONSTRUCTOR:
    return SubroutineKind::CONSTRUCTOR_S;
  case TokenKind::FUNCTION:
    return SubroutineKind::FUNCTION_S;
  default:
    assert(false && "provided wrong token to tokenToSubroutineKind()");
  }
}

Type *Parser::tokenToLiteralType(Token kindTok) {
  switch (kindTok.getKind()) {
  case TokenKind::TRUE:
  case TokenKind::FALSE:
    return Type::getBoolTy();
  case TokenKind::NULL_KEYWORD:
    return Type::getNullTy();
  }

  switch (kindTok.getType()) {
  case TokenType::INT_LITERAL:
    return Type::getIntTy();
  case TokenType::STR_LITERAL:
    return ArrayType::getArrayTy(Type::getCharTy());
  case TokenType::CHAR_LITERAL:
    return Type::getCharTy();
  }
  assert(false && "provided wrong token to tokenToLiteralType()");
}

void Parser::printExpectedErr(TokenKind kind, SourceLocation srcLoc) {
  auto it = std::find_if(Token::keywords.begin(), Token::keywords.end(),
                         [kind](const auto &p) { return p.second == kind; });
  std::cerr << "Expected \"" << it->first << "\" on line: " 
            << srcLoc.getLinePos() << ":"
            << srcLoc.getInLinePos() << std::endl;
}

void Parser::printExpectedErr(TokenType type, SourceLocation srcLoc) {
  std::string expected;
  switch (type) {
  case TokenType::IDENTIFIER:
    expected = "Identifier";
    break;
  case TokenType::INT_LITERAL:
    expected = "Integer literal";
    break;
  case TokenType::STR_LITERAL:
    expected = "String literal";
    break;
  case TokenType::KEYWORD:
    expected = "Keyword";
    break;
  case TokenType::SYMBOL:
    expected = "Symbol";
    break;
  default:
    assert(false && "provided wrong TokenType to printExpectedErr()");
  }
  std::cerr << "Expected \"" << expected << " Token\" on line: " 
            << srcLoc.getLinePos() << ":" << srcLoc.getInLinePos() 
            << std::endl;
}

void Parser::printExpectedErr(const char *err, SourceLocation srcLoc) {
  std::cerr << err << " on line " << srcLoc.getLinePos() << ":"
            << srcLoc.getInLinePos() << std::endl;
}