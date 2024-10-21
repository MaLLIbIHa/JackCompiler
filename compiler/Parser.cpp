#include "Parser.hpp"
#include "compiler/ASTContext.hpp"

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
  while (lexer_->hasTokens()) {
    parseClass();
  }
}

ClassDec *Parser::parseClass() {
  auto classNode = std::make_shared<ClassDec>();
  auto memberVarList = std::make_shared<FieldVariableList>();
  auto staticVarList = std::make_shared<StaticVariableList>();
  auto subrtnList = std::make_shared<SubroutineList>();

  Token classKeyword = consume(TokenKind::CLASS);

  std::string className = consume(TokenType::IDENTIFIER).getValue();
  classNode->setName(className);
  setPosition(classNode, classKeyword);

  consume(TokenKind::LBRACE);

  while (lexer_->hasTokens() && isClassVarDec(lexer_->currentToken())) {
    Token varKindTok = consume();
    if (varKindTok.getKind() == TokenKind::STATIC) {
      parseVarDec(varKindTok, staticVarList);
    } else if (varKindTok.getKind() == TokenKind::FIELD) {
      parseVarDec(varKindTok, memberVarList);
    }
  }

  while (lexer_->hasTokens() && isFuncDec(lexer_->currentToken())) {
    std::shared_ptr<SubroutineDec> subrtn = parseSubroutineDec(consume());
    subrtnList->addSubroutine(subrtn);
    subrtn->setParent(subrtnList);
  }

  consume(TokenKind::RBRACE);
  classNode->addFieldVarList(memberVarList);
  memberVarList->setParent(classNode);
  classNode->addStaticVarList(staticVarList);
  staticVarList->setParent(classNode);
  classNode->addSubroutineList(subrtnList);
  subrtnList->setParent(classNode);

  return classNode;
}

void Parser::parseVarDec(Token varKind,
                         std::shared_ptr<VariableDecList> varList) {
  bool endedStmt = false;
  VarModifier mod = tokenToVarModifier(varKind);

  Type *varType = parseType();

  while (lexer_->hasTokens()) {
    Token id = consume(TokenType::IDENTIFIER);
    auto var = createVarDecWithMod(id.getValue(), varType, mod);
    varList->addVar(var);
    var->setParent(varList);
    setPosition(var, id);

    Token sepTok = consume();
    if (sepTok.getKind() == TokenKind::COMMA) {
      continue;
    } else if (sepTok.getKind() == TokenKind::SEMICOLON) {
      endedStmt = true;
      break;
    } else {
      printExpectedErr("Expected \",\" or \";\"", sepTok.getLinePos(),
                       sepTok.getInLinePos());
      throw std::runtime_error("Parser error");
    }
  }

  if (!endedStmt) {
    printExpectedErr("Unexpected end of Statement", varKind.getLinePos(),
                     varKind.getInLinePos());
    throw std::runtime_error("Parser error");
  }
}

// function/method/constructor type|void name ( type name, ...)
std::shared_ptr<SubroutineDec> Parser::parseSubroutineDec(Token subrtnKind) {
  bool endedStmt = false;
  auto subrtnNode = std::make_shared<SubroutineDec>();
  auto argList = std::make_shared<SubroutineArgumentList>();
  Token retTypeTok = lexer_->currentToken();

  Type *retType = parseReturnType();

  std::string name = consume(TokenType::IDENTIFIER).getValue();
  subrtnNode->setName(name);
  subrtnNode->setRetType(retType);
  subrtnNode->setSubroutineKind(tokenToSubroutineKind(subrtnKind));
  setPosition(subrtnNode, subrtnKind);

  Token argListBegin = consume(TokenKind::LPAREN);
  setPosition(argList, argListBegin);

  while (lexer_->hasTokens() &&
         lexer_->currentToken().getKind() != TokenKind::RPAREN) {
    Type *argType = parseType();
    Token argId = consume(TokenType::IDENTIFIER);
    auto arg = std::make_shared<ArgumentVarDec>(argId.getValue(), argType);

    argList->addVar(arg);
    arg->setParent(argList);
    setPosition(arg, argId);

    Token sepToken = lexer_->currentToken();
    if (sepToken.getKind() == TokenKind::COMMA) {
      consume();
      continue;
    } else if (sepToken.getKind() == TokenKind::RPAREN) {
      endedStmt = true;
      break;
    } else {
      printExpectedErr("Expected \",\" or \")\"", sepToken.getLinePos(),
                       sepToken.getInLinePos());
      throw std::runtime_error("Parser error");
    }
  }

  consume(TokenKind::RPAREN);

  if (lexer_->currentToken().getKind() != TokenKind::SEMICOLON) {
    std::shared_ptr<SubroutineBody> body = parseSubroutineBody();
    subrtnNode->addBody(body);
    body->setParent(subrtnNode);
  } else {
    consume(TokenKind::SEMICOLON);
  }
  subrtnNode->addArgList(argList);
  argList->setParent(subrtnNode);

  return subrtnNode;
}

std::shared_ptr<SubroutineBody> Parser::parseSubroutineBody() {
  auto body = std::make_shared<SubroutineBody>();
  auto varDecList = std::make_shared<LocalVariableList>();

  consume(TokenKind::LBRACE);
  while (lexer_->hasTokens() &&
         lexer_->currentToken().getKind() == TokenKind::VAR) {
    Token kind = consume(TokenKind::VAR);
    parseVarDec(kind, varDecList);
  }

  auto stmtList = parseStatements();
  body->setStatementList(stmtList);
  stmtList->setParent(body);

  body->setVarList(varDecList);
  varDecList->setParent(body);
  consume(TokenKind::RBRACE);
  return body;
}

std::shared_ptr<StatementList> Parser::parseStatements() {
  bool endedBody = false;
  std::shared_ptr<StatementList> stmts = std::make_shared<StatementList>();
  while (lexer_->hasTokens()) {
    std::shared_ptr<Statement> stmtNode;
    Token stmtTok = lexer_->currentToken();
    switch (stmtTok.getKind()) {
    case TokenKind::IF:
      stmtNode = parseIf();
      break;
    case TokenKind::LET:
      stmtNode = parseLet();
      break;
    case TokenKind::WHILE:
      stmtNode = parseWhile();
      break;
    case TokenKind::DO:
      stmtNode = parseDo();
      break;
    case TokenKind::RETURN:
      stmtNode = parseReturn();
      break;
    case TokenKind::RBRACE:
      endedBody = true;
      break;
    default:
      printExpectedErr("Expected Statement or \"}\"", stmtTok.getLinePos(),
                       stmtTok.getInLinePos());
      throw std::runtime_error("Parser error");
    }

    if (stmtNode != nullptr) {
      stmts->addStatement(stmtNode);
      stmtNode->setParent(stmts);
      setPosition(stmtNode, stmtTok);
    }

    if (endedBody) {
      break;
    }
  }
  return stmts;
}

std::shared_ptr<Statement> Parser::parseIf() {
  std::shared_ptr<IfStatement> ifStmt = std::make_shared<IfStatement>();
  std::shared_ptr<Expression> condition;
  std::shared_ptr<StatementList> body;
  Token ifTok = consume(TokenKind::IF);
  setPosition(ifStmt, ifTok);
  consume(TokenKind::LPAREN);
  condition = parseExpression();
  ifStmt->addCondition(condition);
  condition->setParent(ifStmt);
  consume(TokenKind::RPAREN);
  consume(TokenKind::LBRACE);
  body = parseStatements();
  ifStmt->addIfBody(body);
  body->setParent(ifStmt);
  consume(TokenKind::RBRACE);
  if (lexer_->currentToken().getKind() == TokenKind::ELSE) {
    consume(TokenKind::ELSE);
    consume(TokenKind::LBRACE);
    body = parseStatements();
    ifStmt->addElseBody(body);
    body->setParent(ifStmt);
    consume(TokenKind::RBRACE);
  }
  return ifStmt;
}

std::shared_ptr<Statement> Parser::parseLet() {
  auto letStmt = std::make_shared<LetStatement>();
  std::shared_ptr<Expression> lhs;
  std::shared_ptr<Expression> rhs;
  Token letTok = consume(TokenKind::LET);
  setPosition(letStmt, letTok);
  lhs = parseExpression();
  letStmt->addLhs(lhs);
  lhs->setParent(letStmt);
  consume(TokenKind::ASSIGN);
  rhs = parseExpression();
  letStmt->addRhs(rhs);
  rhs->setParent(letStmt);
  consume(TokenKind::SEMICOLON);
  return letStmt;
}

std::shared_ptr<Statement> Parser::parseWhile() {
  auto whileStmt = std::make_shared<WhileStatement>();
  std::shared_ptr<Expression> condition;
  std::shared_ptr<StatementList> body;
  Token whileTok = consume(TokenKind::WHILE);
  setPosition(whileStmt, whileTok);
  consume(TokenKind::LPAREN);
  condition = parseExpression();
  whileStmt->addCondition(condition);
  condition->setParent(whileStmt);
  consume(TokenKind::RPAREN);
  consume(TokenKind::LBRACE);
  body = parseStatements();
  whileStmt->addBody(body);
  body->setParent(whileStmt);
  consume(TokenKind::RBRACE);
  return whileStmt;
}

std::shared_ptr<Statement> Parser::parseReturn() {
  auto retStmt = std::make_shared<ReturnStatement>();
  std::shared_ptr<Expression> retExpr;
  Token retTok = consume(TokenKind::RETURN);
  setPosition(retStmt, retTok);
  if (lexer_->currentToken().getKind() == TokenKind::SEMICOLON) {
    consume(TokenKind::SEMICOLON);
    retStmt->addRetExpr(nullptr);
    return retStmt;
  }
  retExpr = parseExpression();
  retStmt->addRetExpr(retExpr);
  retExpr->setParent(retStmt);
  consume(TokenKind::SEMICOLON);
  return retStmt;
}

std::shared_ptr<Statement> Parser::parseDo() {
  auto doStmt = std::make_shared<DoStatement>();
  std::shared_ptr<Expression> callExpr;
  Token doTok = consume(TokenKind::DO);
  setPosition(doStmt, doTok);
  callExpr = parseExpression();
  doStmt->addCallExpr(callExpr);
  callExpr->setParent(doStmt);
  consume(TokenKind::SEMICOLON);
  return doStmt;
}

std::shared_ptr<Expression> Parser::parseExpression(int prevPrec) {
  std::shared_ptr<Expression> firstOperand = parseTerm();
  std::shared_ptr<Expression> secondOperand;
  std::shared_ptr<Expression> newBinop;
  while (isBinaryOp(lexer_->currentToken()) &&
         getPrec(lexer_->currentToken()) >= prevPrec) {
    Token op = consume();
    int nextPrec = getPrec(op);
    if (getAsc(op) == Associativity::LEFT_ASC) {
      nextPrec++;
    }
    secondOperand = parseExpression(nextPrec);
    newBinop = mkBinopNode(op, firstOperand, secondOperand);
    setPosition(newBinop, op);
    firstOperand->setParent(newBinop);
    secondOperand->setParent(newBinop);
    firstOperand = newBinop;
  }
  return firstOperand;
}

//  Parse Expression which includes parenthesised Expressions,
//  Expressions with unary operators and compound identifiers.
std::shared_ptr<Expression> Parser::parseTerm() {
  Token curr = lexer_->currentToken();
  if (isUnaryOp(curr)) {
    Token op = consume();
    int precedence = getPrec(op);
    std::shared_ptr<Expression> exp = parseExpression(precedence);
    std::shared_ptr<Expression> unop = mkUnopNode(op, exp);
    setPosition(unop, op);
    exp->setParent(unop);
    return unop;
  } else if (curr.getKind() == TokenKind::LPAREN) {
    consume(TokenKind::LPAREN);
    std::shared_ptr<Expression> exp = parseExpression();
    consume(TokenKind::RPAREN);
    return exp;
  } else if (curr.getType() == TokenType::IDENTIFIER ||
             curr.getKind() == TokenKind::THIS) {
    Token name = consume();
    auto currentNode = std::make_shared<NameExpr>(name.getValue());
    setPosition(currentNode, name);
    return parseCompoundId(currentNode, name);
  } else if (curr.getKind() == TokenKind::NEW_ARRAY) {
    Token newArrayTok = curr;
    auto newArrayNode = parseNewArray();
    return parseCompoundId(newArrayNode, newArrayTok);
  } else if (curr.getKind() == TokenKind::DELETE_ARRAY) {
    Token deleteArrayTok = curr;
    auto deleteArrayNode = parseDeleteArray();
    return parseCompoundId(deleteArrayNode, deleteArrayTok);
  } else if (isConstantLiteral(curr)) {
    Token lit = consume();
    Type *litType = tokenToLiteralType(lit);
    auto litExpr = std::make_shared<LiteralExpr>(lit.getValue(), litType);
    setPosition(litExpr, lit);
    return litExpr;
  } else {
    printExpectedErr("Expected term Expression", curr.getLinePos(),
                     curr.getInLinePos());
    throw std::runtime_error("Parser error");
  }
  return nullptr;
}

std::shared_ptr<Expression> Parser::parseNewArray() {
  Token newArrTok = consume(TokenKind::NEW_ARRAY);
  consume(TokenKind::LPAREN);
  Token tok = consume();
  if (!isType(tok)) {
    printExpectedErr("Expected type", tok.getLinePos(), tok.getInLinePos());
    throw std::runtime_error("Parser error");
  }
  consume(TokenKind::COMMA);
  auto sizeExpr = parseExpression();
  consume(TokenKind::RPAREN);
  auto newArrExpr = std::make_shared<NewArrayExpr>(tokenToType(tok), sizeExpr);
  setPosition(newArrExpr, newArrTok);
  sizeExpr->setParent(newArrExpr);
  return newArrExpr;
}

std::shared_ptr<Expression> Parser::parseDeleteArray() {
  Token deleteArrTok = consume(TokenKind::DELETE_ARRAY);
  consume(TokenKind::LPAREN);
  auto sizeExpr = parseExpression();
  consume(TokenKind::RPAREN);
  auto deleteArrExpr = std::make_shared<DeleteArrayExpr>(sizeExpr);
  setPosition(deleteArrExpr, deleteArrTok);
  sizeExpr->setParent(deleteArrExpr);
  return deleteArrExpr;
}

//  Parse compound identifiers which includes dot, square brackets,
//  function call operators.
//  First identifier can be class name or this keyword
std::shared_ptr<Expression>
Parser::parseCompoundId(std::shared_ptr<Expression> currNode, Token beginTok) {
  Token nextId;
  std::shared_ptr<Expression> newNode;
  std::shared_ptr<Expression> memberExp;
  std::shared_ptr<ExpressionList> subrtnArgs;

  while (lexer_->hasTokens()) {
    switch (lexer_->currentToken().getKind()) {
    case TokenKind::DOT:
      consume(TokenKind::DOT);
      nextId = consume(TokenType::IDENTIFIER);
      newNode = std::make_shared<MemberExpr>(currNode, nextId.getValue());
      break;

    case TokenKind::LBRACK:
      consume(TokenKind::LBRACK);
      memberExp = parseExpression();
      consume(TokenKind::RBRACK);
      newNode = std::make_shared<ArrayMemberExpr>(currNode, memberExp);
      memberExp->setParent(newNode);
      break;

    case TokenKind::LPAREN:
      nextId = consume(TokenKind::LPAREN);
      subrtnArgs = parseArgList();
      setPosition(subrtnArgs, nextId);
      consume(TokenKind::RPAREN);
      newNode = std::make_shared<SubroutineCallExpr>(currNode, subrtnArgs);
      subrtnArgs->setParent(newNode);
      break;

    default:
      return currNode;
    }

    setPosition(newNode, beginTok);
    currNode->setParent(newNode);
    currNode = newNode;
  }
  return nullptr;
}

std::shared_ptr<ExpressionList> Parser::parseArgList() {
  Token argListBeg = lexer_->currentToken();
  if (argListBeg.getKind() == TokenKind::RPAREN) {
    return std::make_shared<ExpressionList>();
  }
  std::shared_ptr<ExpressionList> argExprs = std::make_shared<ExpressionList>();
  std::shared_ptr<Expression> expr;
  bool endedStmt = false;
  while (lexer_->hasTokens()) {
    expr = parseExpression();
    argExprs->addExpression(expr);
    expr->setParent(argExprs);
    if (lexer_->currentToken().getKind() == TokenKind::RPAREN) {
      endedStmt = true;
      break;
    }
    consume(TokenKind::COMMA);
  }

  if (!endedStmt) {
    printExpectedErr("Unexpected end of arguments list",
                     argListBeg.getLinePos(), argListBeg.getInLinePos());
    throw std::runtime_error("Parser error");
  }

  return argExprs;
}

Type *Parser::parseType() {
  Token typeTok = consume();
  if (!isType(typeTok)) {
    printExpectedErr("Expected type", typeTok.getLinePos(),
                     typeTok.getInLinePos());
    throw std::runtime_error("Parser error");
  }
  Type *type = tokenToType(typeTok);
  if (lexer_->currentToken().getKind() == TokenKind::LBRACK) {
    consume(TokenKind::LBRACK);
    consume(TokenKind::RBRACK);
    type = ArrayType::getArrayTy(type);
  }
  return type;
}

Type *Parser::parseReturnType() {
  if (lexer_->currentToken().getKind() == TokenKind::VOID) {
    consume();
    return Type::getVoidTy();
  }
  return parseType();
}

std::shared_ptr<Expression>
Parser::mkUnopNode(Token op, std::shared_ptr<Expression> operand) {
  OpType type;
  switch (op.getKind()) {
  case TokenKind::SUB:
    type = OpType::NEG_OP;
    break;
  case TokenKind::TILDE:
    type = OpType::TILDE_OP;
    break;
  default:
    printExpectedErr("Wrong unary operation", op.getLinePos(),
                     op.getInLinePos());
    throw std::runtime_error("Parser error");
  }
  return std::make_shared<UnopExpr>(operand, type);
}

std::shared_ptr<Expression>
Parser::mkBinopNode(Token op, std::shared_ptr<Expression> firstOperand,
                    std::shared_ptr<Expression> secondOperand) {
  OpType type;
  switch (op.getKind()) {
  case TokenKind::ADD:
    type = OpType::ADD_OP;
    break;
  case TokenKind::SUB:
    type = OpType::SUB_OP;
    break;
  case TokenKind::MUL:
    type = OpType::MUL_OP;
    break;
  case TokenKind::QUO:
    type = OpType::DIV_OP;
    break;
  case TokenKind::LOG_AND:
    type = OpType::LOG_AND_OP;
    break;
  case TokenKind::LOG_OR:
    type = OpType::LOG_OR_OP;
    break;
  case TokenKind::BIT_AND:
    type = OpType::BIT_AND_OP;
    break;
  case TokenKind::LSS:
    type = OpType::LSS_OP;
    break;
  case TokenKind::GTR:
    type = OpType::GTR_OP;
    break;
  case TokenKind::EQL:
    type = OpType::EQL_OP;
    break;
  default:
    printExpectedErr("Wrong binary operation", op.getLinePos(),
                     op.getInLinePos());
    throw std::runtime_error("Parser error");
  }
  return std::make_shared<BinopExpr>(firstOperand, secondOperand, type);
}

std::shared_ptr<VariableDec>
Parser::createVarDecWithMod(std::string name, Type *type, VarModifier mod) {
  switch (mod) {
  case VarModifier::ARG_V:
    return std::make_shared<ArgumentVarDec>(name, type);
  case VarModifier::FIELD_V:
    return std::make_shared<FieldVarDec>(name, type);
  case VarModifier::LOCAL_V:
    return std::make_shared<LocalVarDec>(name, type);
  case VarModifier::STATIC_V:
    return std::make_shared<StaticVarDec>(name, type);
  }
  return nullptr;
}

Token Parser::consume(TokenType type) {
  Token cur = lexer_->currentToken();
  if (cur.getType() == type) {
    return lexer_->consume();
  } else {
    printExpectedErr(type, cur.getLinePos(), cur.getInLinePos());
    throw std::runtime_error("Parser error");
  }
  return Token();
}

Token Parser::consume(TokenKind kind) {
  Token cur = lexer_->currentToken();
  if (cur.getKind() == kind) {
    return lexer_->consume();
  } else {
    printExpectedErr(kind, cur.getLinePos(), cur.getInLinePos());
    throw std::runtime_error("Parser error");
  }
  return Token();
}

Token Parser::consume() { return lexer_->consume(); }

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

void Parser::setPosition(std::shared_ptr<Node> node, Token tok) {
  node->setLinePos(tok.getLinePos());
  node->setInLinePos(tok.getInLinePos());
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
      printExpectedErr("Wrong type Token", typeTok.getLinePos(),
                       typeTok.getInLinePos());
      throw std::runtime_error("Parser error");
    }
  }
  return ClassType::getClassTy(typeTok.getValue());
}

VarModifier Parser::tokenToVarModifier(Token modTok) {
  VarModifier mod;
  switch (modTok.getKind()) {
  case TokenKind::STATIC:
    mod = VarModifier::STATIC_V;
    break;
  case TokenKind::FIELD:
    mod = VarModifier::FIELD_V;
    break;
  case TokenKind::VAR:
    mod = VarModifier::LOCAL_V;
    break;
  }
  return mod;
}

SubroutineKind Parser::tokenToSubroutineKind(Token typeTok) {
  SubroutineKind subType;
  switch (typeTok.getKind()) {
  case TokenKind::METHOD:
    subType = SubroutineKind::METHOD_S;
  case TokenKind::CONSTRUCTOR:
    subType = SubroutineKind::CONSTRUCTOR_S;
  case TokenKind::FUNCTION:
    subType = SubroutineKind::FUNCTION_S;
  }
  return subType;
}

Type *Parser::tokenToLiteralType(Token kindTok) {
  Type *litType;
  switch (kindTok.getKind()) {
  case TokenKind::TRUE:
  case TokenKind::FALSE:
    litType = Type::getBoolTy();
    break;
  case TokenKind::NULL_KEYWORD:
    litType = Type::getNullTy();
    break;
  }

  switch (kindTok.getType()) {
  case TokenType::INT_LITERAL:
    litType = Type::getIntTy();
    break;
  case TokenType::STR_LITERAL:
    litType = Type::getCharTy();
    litType = ArrayType::getArrayTy(litType);
    break;
  case TokenType::CHAR_LITERAL:
    litType = Type::getCharTy();
    break;
  }
  return litType;
}

void Parser::printExpectedErr(TokenKind kind, unsigned linePos,
                              unsigned inLinePos) {
  auto it = std::find_if(Token::keywords.begin(), Token::keywords.end(),
                         [kind](const auto &p) { return p.second == kind; });
  std::cerr << "Expected \"" << it->first << "\" on line: " << linePos << ":"
            << inLinePos << std::endl;
}

void Parser::printExpectedErr(TokenType type, unsigned linePos,
                              unsigned inLinePos) {
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
  }
  std::cerr << "Expected \"" << expected << " Token\" on line: " << linePos
            << ":" << inLinePos << std::endl;
}

void Parser::printExpectedErr(const char *err, unsigned linePos,
                              unsigned inLinePos) {
  std::cerr << err << " on line " << linePos << ":" << inLinePos << std::endl;
}