#pragma once
#include "AST.hpp"
#include "Token.hpp"
#include "Lexer.hpp"

enum class Associativity {
    LEFT_ASC,
    RIGHT_ASC,
};

extern std::map<std::string, std::pair<int, Associativity>> opInfo_;

class Parser final {
public:
    Parser(std::string fileName, std::string sourceText);

    //Main parsing methods
    std::shared_ptr<FileUnit> parseFileUnit();

private:
    std::shared_ptr<ClassDec> parseClass();

    std::shared_ptr<SubroutineDec> parseSubroutineDec(Token subrtnKind);

    std::shared_ptr<SubroutineBody> parseSubroutineBody();

    std::shared_ptr<StatementList> parseStatements();

    Type* parseType();

    Type* parseReturnType();

    void parseVarDec(Token varKind,
                       std::shared_ptr<VariableDecList> vars);

    //Statements parsing methods
    std::shared_ptr<Statement> parseLet();

    std::shared_ptr<Statement> parseIf();

    std::shared_ptr<Statement> parseWhile();

    std::shared_ptr<Statement> parseDo();

    std::shared_ptr<Statement> parseReturn();

    //Expression parsing methods
    std::shared_ptr<Expression> parseExpression(int prevPrec = 0);

    std::shared_ptr<Expression> parseTerm();

    std::shared_ptr<Expression> parseNewArray();

    std::shared_ptr<Expression> parseDeleteArray();

    std::shared_ptr<ExpressionList> parseArgList();

    std::shared_ptr<Expression> parseCompoundId(
        std::shared_ptr<Expression> currNode,
        Token beginTok);

    std::shared_ptr<Expression> mkUnopNode(Token op, 
                                             std::shared_ptr<Expression> operand);

    std::shared_ptr<Expression> mkBinopNode(Token op,
                                              std::shared_ptr<Expression> firstOperand,
                                              std::shared_ptr<Expression> secondOperand);

    std::shared_ptr<VariableDec> createVarDecWithMod(std::string name,
                                                     Type* type,
                                                     VarModifier mod);

    //Token stream functions
    bool isType(Token tok);

    bool isUnaryOp(Token tok);

    bool isBinaryOp(Token tok);

    bool isClassVarDec(Token tok);

    bool isFuncDec(Token tok);

    bool isConstantLiteral(Token tok);

    Token consume(TokenType type);

    Token consume(TokenKind kind);

    Token consume();

    int getPrec(Token tok);

    void setPosition(std::shared_ptr<Node> node, Token tok);

    Associativity getAsc(Token tok);

    Type* tokenToType(Token typeTok);

    VarModifier tokenToVarModifier(Token modTok);

    SubroutineKind tokenToSubroutineKind(Token typeTok);

    Type* tokenToLiteralType(Token kind);

    //Error output functions
    void printExpectedErr(TokenKind kind, unsigned linePos, unsigned inLinePos);

    void printExpectedErr(TokenType type, unsigned linePos, unsigned inLinePos);

    void printExpectedErr(const char *errMsg, unsigned linePos, unsigned inLinePos);

    std::shared_ptr<FileUnit> fileUnit_;
    std::unique_ptr<Lexer> lexer_;
};