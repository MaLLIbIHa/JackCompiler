#pragma once
#include "compiler/AST2.hpp"
#include <cctype>
#include <map>
#include <string>

enum class TokenKind {
  NOT_KEYWORD = 0,
  keywordBeg,
  CLASS,
  CONSTRUCTOR,
  FUNCTION,
  METHOD,
  FIELD,
  STATIC,
  VAR,
  INT,
  CHAR,
  BOOLEAN,
  VOID,
  NEW_ARRAY,
  DELETE_ARRAY,

  constantBeg,
  TRUE,
  FALSE,
  NULL_KEYWORD,
  THIS,
  constantEnd,

  statementBeg,
  LET,
  DO,
  IF,
  ELSE,
  WHILE,
  RETURN,
  statementEnd,
  keywordEnd,

  symbolBeg,
  LPAREN,    // (
  RPAREN,    // )
  LBRACK,    // [
  RBRACK,    // ]
  LBRACE,    // {
  RBRACE,    // }
  DOT,       // .
  COMMA,     // ,
  SEMICOLON, // ;
  ASSIGN,    // =

  binopBeg,
  ADD,     // +
  SUB,     // -
  MUL,     // *
  QUO,     // /
  LOG_AND, // &&
  LOG_OR,  // ||
  BIT_OR,  // |
  BIT_AND, // &
  LSS,     // <
  GTR,     // >
  EQL,     // ==
  binopEnd,

  TILDE, // ~
  symbolEnd
};

enum class TokenType {
  EMPTY_TOKEN,
  KEYWORD,
  SYMBOL,
  CHAR_LITERAL,
  STR_LITERAL,
  INT_LITERAL,
  IDENTIFIER,
  TEOF,
};

class Token final {
public:
  inline static const std::map<std::string, TokenKind> keywords = {
      {"class", TokenKind::CLASS},
      {"constructor", TokenKind::CONSTRUCTOR},
      {"function", TokenKind::FUNCTION},
      {"method", TokenKind::METHOD},
      {"field", TokenKind::FIELD},
      {"static", TokenKind::STATIC},
      {"var", TokenKind::VAR},
      {"int", TokenKind::INT},
      {"char", TokenKind::CHAR},
      {"boolean", TokenKind::BOOLEAN},
      {"void", TokenKind::VOID},
      {"newArray", TokenKind::NEW_ARRAY},
      {"deleteArray", TokenKind::DELETE_ARRAY},
      {"true", TokenKind::TRUE},
      {"false", TokenKind::FALSE},
      {"null", TokenKind::NULL_KEYWORD},
      {"this", TokenKind::THIS},
      {"let", TokenKind::LET},
      {"do", TokenKind::DO},
      {"if", TokenKind::IF},
      {"else", TokenKind::ELSE},
      {"while", TokenKind::WHILE},
      {"return", TokenKind::RETURN},
      {"(", TokenKind::LPAREN},
      {")", TokenKind::RPAREN},
      {"[", TokenKind::LBRACK},
      {"]", TokenKind::RBRACK},
      {"{", TokenKind::LBRACE},
      {"}", TokenKind::RBRACE},
      {".", TokenKind::DOT},
      {",", TokenKind::COMMA},
      {";", TokenKind::SEMICOLON},
      {"+", TokenKind::ADD},
      {"-", TokenKind::SUB},
      {"*", TokenKind::MUL},
      {"/", TokenKind::QUO},
      {"&&", TokenKind::LOG_AND},
      {"||", TokenKind::LOG_OR},
      {"&", TokenKind::BIT_AND},
      {"|", TokenKind::BIT_OR},
      {"<", TokenKind::LSS},
      {">", TokenKind::GTR},
      {"=", TokenKind::ASSIGN},
      {"==", TokenKind::EQL},
      {"~", TokenKind::TILDE}};

public:
  Token() = default;

  Token(TokenType tokT, TokenKind kind, std::string value)
      : type_(tokT), kind_(kind), value_(value) {}

  Token(TokenType tokT, TokenKind kind, std::string value, 
        SourceLocation srcLoc)
      : srcLoc_(srcLoc) {}

  void setSourceLocation(SourceLocation srcLoc) { srcLoc_ = srcLoc; }
  void setValue(std::string val) { value_ = val; }
  void setType(TokenType type) { type_ = type; }
  void setKind(TokenKind kind) { kind_ = kind; }

  SourceLocation getSourceLocation() const { return srcLoc_; }
  unsigned getLinePos() { return srcLoc_.getLinePos(); }
  unsigned getInLinePos() { return srcLoc_.getInLinePos(); }
  const std::string& getValue() const { return value_; }
  TokenType getType() const { return type_; }
  TokenKind getKind() const { return kind_; }

private:
  std::string value_;
  TokenType type_ = TokenType::EMPTY_TOKEN;
  TokenKind kind_ = TokenKind::NOT_KEYWORD;
  SourceLocation srcLoc_;
};