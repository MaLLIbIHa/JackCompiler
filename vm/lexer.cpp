#include "lexer.hpp"
#include "token.hpp"
#include <string>
#include <vector>
#include <memory>
#include <iostream>

std::vector<token> lexer::operator() (const std::string &text) {
    return lex(text);
}

std::vector<token> lexer::lex(const std::string &text) {
    std::vector<token> tokens;
    std::string current_lexem;
    for (const char &c : text) {
        if (!std::isspace(c)) {
            current_lexem.push_back(c);
        } else if (!current_lexem.empty()) {
            if (is_keyword(current_lexem)) {
                tokens.push_back(keyword_tok(get_keyword_type(current_lexem)));
            } else if (is_mem_seg(current_lexem)) {
                tokens.push_back(mem_segment_tok(get_mem_segment(current_lexem)));
            } else if (is_label(current_lexem)) {;
                tokens.push_back(label_tok(current_lexem));
            } else if (is_number(current_lexem)) {
                tokens.push_back(number_tok(std::stoi(current_lexem)));
            } else {
                std::cerr << "Undefined token " << current_lexem << std::endl;
                return {};
            }
            current_lexem.clear();
        }
    }

    return tokens;
}