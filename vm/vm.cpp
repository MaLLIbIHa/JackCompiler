#include <string>
#include <vector>
#include <cctype>
#include <map>
#include <iostream>
#include "vm.hpp"
#include "lexer.hpp"

vm::vm(std::string &text, lexer_func lex, parser_func parse) 
        : text_(text), lex_(lex), parse_(parse) {
    text_ += '\n';
}

void vm::compile() {
    tokens_ = lex_(text_);
    output_ << parse_(tokens_);
}

void vm::print() {
    std::cout << output_.str();
}

void vm::set_output_file(std::ofstream &file) {
    file_output_ = std::move(file);
}

void vm::print_to_file() {
    file_output_ << output_.str();
}


// void virtual_machine::print_err(token t, token_type expected) {
//     std::string type = token_type_to_str_.find(t.type)->second;
//     std::string expected_type = token_type_to_str_.find(expected)->second;
//     std::cerr << "Invalid token " << type << ", expected " << expected_type << std::endl;
// }