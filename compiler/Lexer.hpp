#pragma once
#include "Token.hpp"
#include "compiler/AST2.hpp"
#include <algorithm>
#include <concepts>
#include <iostream>
#include <istream>
#include <ostream>

template<typename T>
concept InputStreamWithUnget = requires(T stream) {
  { stream.get() } -> std::convertible_to<char>;
  { stream.peek() } -> std::convertible_to<char>;
  stream.unget();
  { stream.eof() } -> std::convertible_to<bool>;
};

class Lexer final {
public:
  Lexer(std::istream& inputStream)
      : inputStream_(inputStream) {
    skipSpacesAndComments();
    currentToken_ = getToken();
  }

  Token consume() {
    Token tmp = getToken();
    std::swap(currentToken_, tmp);
    return tmp;
  }

  const Token& currentToken() { return currentToken_; }

  void printTokens() {
    while (hasTokens()) {
      Token tok = consume();
      if (tok.getType() == TokenType::EMPTY_TOKEN)
        return;
      std::cout << tok.getValue() << std::endl;
      std::cout << "Token line " << tok.getLinePos() << std::endl;
      std::cout << "Token in line " << tok.getInLinePos() << std::endl;
    }
  }

  bool hasTokens() { return currentToken_.getType() != TokenType::TEOF; }

private:
  Token getToken() {
    SourceLocation srcLoc(currentLinePos_, currentInLinePos_);
    if (isEof()) {
      return Token(TokenType::TEOF, TokenKind::NOT_KEYWORD, "", srcLoc);
    }
    
    Token tok;
    char currentChar = peek();
    if (currentChar == '\"') {
      tok = consumeStrLiteral();
    } else if (currentChar == '\'') {
      tok = consumeCharLiteral();
    } else if (std::isdigit(currentChar)) {
      tok = consumeNumberLiteral();
    } else if (isSymbol(std::string{currentChar})) {
      tok = consumeSymbol();
    } else {
      std::string word = consumeWord();
      if (word.empty()) {
        std::cerr << "Unexpected Token on line " << currentLinePos_ << ":"
                  << currentInLinePos_;
        throw std::runtime_error("Lexer error");
      } else if (isKeyword(word)) {
        tok = Token(TokenType::KEYWORD, getKeywordSymbol(word), word);
      } else if (isIdentifier(word)) {
        tok = Token(TokenType::IDENTIFIER, TokenKind::NOT_KEYWORD, word);
      }
    }
    skipSpacesAndComments();
    tok.setSourceLocation(srcLoc);
    return tok;
  }

  void skipSpacesAndComments() {
    while (!isEof()) {
      if (std::isspace(peek())) {
        consumeChar();
      } else if (isComment()) {
        skipComment();
      } else {
        break;
      }
    }
  }

  bool isComment() {
    std::string maybeComment = {peek()};
    consumeChar();
    if (isEof())
      return false;
    maybeComment += peek();
    if (maybeComment == "//" || maybeComment == "/*") {
      return true;
    }
    putbackChar();
    return false;
  }

  void skipComment() {
    if (peek() == '/') {
      while (peek() != '\n') {
        consumeChar();
      }
    } else if (peek() == '*') {
      char prev = 0;
      char cur = peek();
      while (prev != '*' && cur != '\\') {
        prev = cur;
        cur = consumeChar();
      }
    }
    consumeChar();
  }

  char consumeChar() {
    char c = inputStream_.get();
    switch (c) {
    case '\n':
      currentLinePos_++;
      currentInLinePos_ = 1;
      break;
    case '\t':
      currentInLinePos_ += 4;
      break;
    default:
      currentInLinePos_++;
      break;
    }
    return c;
  }

  char peek() { return inputStream_.peek(); }

  bool isEof() { return inputStream_.eof(); }

  void putbackChar() {
    inputStream_.unget();
    currentInLinePos_--;
  }

  Token consumeSymbol() {
    char nextChar = 0;
    char currentChar = peek();
    consumeChar();
    if (!isEof()) {
      nextChar = peek();
    }

    std::string maybeDoubleCharSym = {currentChar, nextChar};
    auto keywordIter = Token::keywords.find(maybeDoubleCharSym);
    std::string finSymbol;
    if (keywordIter != Token::keywords.end()) {
      consumeChar();
      finSymbol = maybeDoubleCharSym;
    } else {
      finSymbol = std::string{currentChar};
      keywordIter = Token::keywords.find(finSymbol);
    }
    return Token(TokenType::SYMBOL, keywordIter->second, finSymbol);
  }

  std::string consumeWord() {
    std::string word;
    while (!isEof() && (std::isalnum(peek()) || peek() == '_')) {
      word += peek();
      consumeChar();
    }
    return word;
  }

  Token consumeCharLiteral() {
    unsigned beginLinePos = currentLinePos_;
    unsigned beginInLinePos = currentInLinePos_;
    consumeChar();
    char charLiteral = 0;
    if (!isEof()) {
      charLiteral = peek();
      consumeChar();
    } else {
      std::cerr << "Missing terminating \' character\n"
                   "Initial \" character on line "
                << beginLinePos << ":" << beginInLinePos;
      throw std::runtime_error("Lexer error");
    }

    if (isEof() || peek() != '\'') {
      std::cerr << "Missing terminating \' character\n"
                   "Initial \" character on line "
                << beginLinePos << ":" << beginInLinePos;
      throw std::runtime_error("Lexer error");
    }
    consumeChar();
    return Token(TokenType::CHAR_LITERAL, TokenKind::NOT_KEYWORD,
                 std::string{charLiteral});
  }

  Token consumeStrLiteral() {
    unsigned beginLinePos = currentLinePos_;
    unsigned beginInLinePos = currentInLinePos_;
    consumeChar();
    std::string strLiteral;
    while (!isEof() && peek() != '\"') {
      strLiteral += peek();
      consumeChar();
    }
    if (isEof()) {
      std::cerr << "Missing terminating \" character\n"
                   "Initial \" character on line "
                << beginLinePos << ":" << beginInLinePos;
      throw std::runtime_error("Lexer error");
    }
    consumeChar();
    return Token(TokenType::STR_LITERAL, TokenKind::NOT_KEYWORD, strLiteral);
  }

  Token consumeNumberLiteral() {
    std::string num;
    while (!isEof() && std::isdigit(peek())) {
      num += peek();
      consumeChar();
    }
    return Token(TokenType::INT_LITERAL, TokenKind::NOT_KEYWORD, num);
  }

  /*
  Lexer utility function definitions
  */
  bool isSymbol(const std::string &lexem) {
    auto keyword = Token::keywords.find(lexem);
    if (keyword != Token::keywords.end() &&
        TokenKind::symbolBeg < keyword->second &&
        keyword->second < TokenKind::symbolEnd) {
      return true;
    } else {
      return false;
    }
  }

  bool isKeyword(const std::string &lexem) {
    auto keywordIter = Token::keywords.find(lexem);
    if (keywordIter != Token::keywords.end() &&
        TokenKind::keywordBeg < keywordIter->second &&
        keywordIter->second < TokenKind::keywordEnd) {
      return true;
    }
    return false;
  }

  bool isNumber(const std::string &lexem) {
    for (const char &c : lexem) {
      if (std::isdigit(c))
        continue;
      else
        return false;
    }

    return true;
  }

  bool isIdentifier(const std::string &lexem) {
    if (std::isdigit(lexem[0])) {
      return false;
    }
    for (const char c : lexem) {
      if (std::isalnum(c) || c == '_') {
        continue;
      } else {
        return false;
      }
    }
    return true;
  }

  TokenKind getKeywordSymbol(const std::string &lexem) {
    TokenKind type = Token::keywords.find(lexem)->second;
    return type;
  }

private:
  std::istream& inputStream_;
  Token currentToken_;
  unsigned int currentLinePos_ = 1;
  unsigned int currentInLinePos_ = 1;
};