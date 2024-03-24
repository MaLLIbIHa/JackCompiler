#pragma once
#include <vector>
#include <memory>
#include <iostream>
#include "token.hpp"

namespace compiler {

class lexer {
public:
    lexer(std::string text) : text_(text) {}

    std::vector<token> lex();

private:
    void skip_comments();

    bool is_double_char_sym(std::size_t index);

    void push_token(std::string &lexem);

    void print_tokens();

    std::vector<token> tokens_;
    std::string text_;
};
}