#pragma once
#include <vector>
#include <memory>
#include <iostream>
#include <iterator>
#include <exception>
#include <algorithm>
#include "token.hpp"

class lexer {
public:
    lexer(std::string sourceText) 
        : sourceText_(sourceText), 
          iter_(sourceText_.begin()),
          endIter_(sourceText_.end())
    {
        skipSpacesAndComments();
        currentToken_ = get_token();
    }

    token get_token() {
        if (iter_ == endIter_) {
            return token(token_type::T_EOF, token_kind::NOT_KEYWORD, "");
        }

        unsigned token_line_pos = current_line_pos_;
        unsigned token_in_line_pos = current_in_line_pos_;
        token tok;
        char currentChar = *iter_;
        if (currentChar == '\"') {
            tok = consumeStrLiteral();
        } else if (std::isdigit(currentChar)) {
            tok = consumeNumberLiteral();
        } else if (isSymbol(std::string{currentChar})) {
            tok = consumeSymbol();
        } else {
            std::string word = consumeWord();
            if (word.empty()) {
                std::cerr << "Unexpected token on line " <<
                             current_line_pos_ << ":" << current_in_line_pos_;
                throw std::runtime_error("Lexer error");
            } else if (isKeyword(word)) {
                tok = token(token_type::KEYWORD, getKeywordSymbol(word), word);
            } else if (isIdentifier(word)) {
                tok = token(token_type::IDENTIFIER, token_kind::NOT_KEYWORD, word);
            }
        }
        skipSpacesAndComments();
        tok.set_line_pos(token_line_pos);
        tok.set_in_line_pos(token_in_line_pos);
        return tok;
    }

    token consume() {
        token tmp = get_token();
        std::swap(currentToken_, tmp);
        return tmp;
    }

    token currentToken() {
        return currentToken_;
    }

    void printTokens() {
        while (hasTokens()) {
            token tok = consume();
            if (tok.get_type() == token_type::EMPTY_TOKEN) return;
            std::cout << tok.get_value() << std::endl;
            std::cout << "Token line " <<  tok.get_line_pos() << std::endl;
            std::cout << "Token in line " << tok.get_in_line_pos() << std::endl;
        }
    }

    bool hasTokens() {
        return currentToken_.get_type() != token_type::T_EOF;
    }

private:
    void skipSpacesAndComments() {
        while (iter_ != endIter_) {
            if (std::isspace(*iter_)) {
                consumeChar();
            } else if (isComment()) {
                skipComment();
            } else {
                break;
            }
        }
    }

    bool isComment() {
        std::string maybeComment = {*iter_};
        consumeChar();
        if (iter_ == endIter_) return false;
        maybeComment += *iter_;
        if (maybeComment == "//" || maybeComment == "/*") {
            return true;
        }
        putbackChar();
        return false;
    }

    void skipComment() {
        if (*iter_ == '/') {
            while (*iter_ != '\n') {
                consumeChar();
            }
        } else if (*iter_ == '*') {
            char prev = 0;
            char cur = *iter_;
            while (prev != '*' && cur != '\\') {
                prev = cur;
                cur = *consumeChar();
            }
        }
        consumeChar();
    }

    std::string::iterator consumeChar() {
        switch (*iter_) {
            case '\n': 
                current_line_pos_++;
                current_in_line_pos_ = 1;
                break;
            case '\t':
                current_in_line_pos_ += 4;
                break;
            default:
                current_in_line_pos_++;
                break;
        }
        
        return ++iter_;
    }

    std::string::iterator putbackChar() {
        --iter_;
        current_in_line_pos_--;
        return iter_;
    }

    token consumeSymbol() {
        char next_char = 0;
        char currentChar = *iter_;
        consumeChar();
        if (iter_ != endIter_) {
            next_char = *iter_;
        }

        std::string maybeDoubleCharSym = {currentChar, next_char};
        auto keyword_iter = token::keywords.find(maybeDoubleCharSym);
        std::string finSymbol;
        if (keyword_iter != token::keywords.end()) {
            consumeChar();
            finSymbol = maybeDoubleCharSym;
        } else {
            finSymbol = std::string{currentChar};
            keyword_iter = token::keywords.find(finSymbol);
        }
        return token(token_type::SYMBOL, keyword_iter->second, finSymbol);;
    }

    std::string consumeWord() {
        std::string word;
        while (iter_ != endIter_ && (std::isalnum(*iter_) || *iter_ == '_')) {
            word += *iter_;
            consumeChar();
        }
        return word;
    }

    token consumeStrLiteral() {
        unsigned begin_line_pos = current_line_pos_;
        unsigned begin_in_line_pos = current_in_line_pos_;
        consumeChar();
        std::string strLiteral;
        while(iter_ != endIter_ && *iter_ != '\"') {
            strLiteral += *iter_;
            consumeChar();
        }
        if (iter_ == endIter_) {
            std::cerr << "Missing terminating \" character\n"
                         "Initial \" character on line " <<
                          begin_line_pos << ":" << begin_in_line_pos;
            throw std::runtime_error("Lexer error");
        }
        consumeChar();

        return token(token_type::STR_LITERAL, token_kind::NOT_KEYWORD, strLiteral);
    }

    token consumeNumberLiteral() {
        std::string num;
        while (iter_ != endIter_ && std::isdigit(*iter_)) {
            num += *iter_;
            consumeChar();
        }

        return token(token_type::INT_LITERAL, token_kind::NOT_KEYWORD, num);
    }

    /*
    Lexer utility function definitions
    */
    bool isSymbol(const std::string &lexem) {
        auto keyword = token::keywords.find(lexem);
        if (keyword != token::keywords.end() &&
            token_kind::symbol_beg < keyword->second && keyword->second < token_kind::symbol_end) 
        {    
            return true;
        } else {
            return false;
        }
    }

    bool isKeyword(const std::string &lexem) {
        auto keyword_iter = token::keywords.find(lexem);
        if (keyword_iter != token::keywords.end() && 
            token_kind::keyword_beg < keyword_iter->second && keyword_iter->second < token_kind::keyword_end) 
        {
            return true;
        }
        return false;
    }

    bool isNnumber(const std::string &lexem) {
        for (const char &c : lexem) {
            if (std::isdigit(c)) continue;
            else return false;
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

    token_kind getKeywordSymbol(const std::string &lexem) {
        token_kind type = token::keywords.find(lexem)->second;
        return type;
    }

private:
    std::string sourceText_;
    std::string::iterator iter_;
    std::string::iterator endIter_;
    token currentToken_;
    unsigned int current_line_pos_ = 1;
    unsigned int current_in_line_pos_ = 1;
};