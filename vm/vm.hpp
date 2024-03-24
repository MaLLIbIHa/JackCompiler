#pragma once
#include <string>
#include <vector>
#include <cctype>
#include <map>
#include <iostream>
#include <sstream>
#include <fstream>
#include <memory>
#include <functional>
#include "token.hpp"
#include "lexer.hpp"
#include "parser.hpp"

class vm {
    //void print_err(token t, token_type expected);

    using lexer_func = std::function<std::vector<token>(const std::string &)>;
    using parser_func = std::function<std::string(const std::vector<token> &)>;

    lexer_func lex_;
    parser_func parse_;

    std::string text_;
    std::vector<token> tokens_;
    std::stringstream output_;
    std::ofstream file_output_;

public:
    vm(std::string&, lexer_func, parser_func);

    void compile();

    void set_output_file(std::ofstream&);

    void print_to_file();

    void print();
};