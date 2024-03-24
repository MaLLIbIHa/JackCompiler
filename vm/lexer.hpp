#pragma once
#include "token.hpp"
#include <vector>
#include <memory>

class lexer {
public:
    std::vector<token> operator()(const std::string &text);

private:
    std::vector<token> lex(const std::string &text);
};