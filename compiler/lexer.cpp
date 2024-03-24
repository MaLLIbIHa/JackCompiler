#include "lexer.hpp"

namespace compiler {

std::vector<token> lexer::lex() {
    skip_comments();

    std::string current_lexem;
    for (std::size_t index = 0; index < text_.size(); index++) {
        char current = text_[index];

        if (current == '\"') {
            if (!current_lexem.empty()) {
                push_token(current_lexem);
                current_lexem.clear();
            }

            unsigned int str_literal_end = text_.find("\"", index + 1);
            if (str_literal_end == std::string::npos) {
                //error
            }

            std::string str_literal = text_.substr(index + 1, str_literal_end - index - 1);
            tokens_.push_back(token(STR_LITERAL, NOT_KEYWORD, str_literal));
            index = str_literal_end;
            continue;
        }
        if (std::isspace(current)) {
            if (!current_lexem.empty()) {
                push_token(current_lexem);
                current_lexem.clear();
            }
            continue;
        }
        if (is_symbol({current})) {
            if (!current_lexem.empty()) {
                push_token(current_lexem);
                current_lexem.clear();
            }

            std::string sym;
            sym += text_[index];
            if (is_double_char_sym(index)) {
                sym += text_[index + 1];
                index++;
            }
            tokens_.push_back(token(SYMBOL, get_keyword_symbol(sym), sym));
            continue;
        }
        current_lexem += current;
    }
    tokens_.push_back(token(T_EOF, NOT_KEYWORD, std::string{}));
    return tokens_;
}

void lexer::skip_comments() {
    std::string result_text;
    std::size_t current_index = 0;
    std::size_t comment_index = text_.find("//", current_index);
    std::size_t multicomment_index = text_.find("/*", current_index);
    
    while (comment_index != std::string::npos || multicomment_index != std::string::npos) {
        unsigned int closing_index = 0;
        if (multicomment_index < comment_index) {
            closing_index = text_.find("*/", current_index);
            if (closing_index == std::string::npos) {
                //error
            }
            result_text += text_.substr(current_index, multicomment_index - current_index);
            current_index = closing_index + 2;
        } else if (comment_index < multicomment_index) {
            closing_index = text_.find("\n", comment_index);
            if (closing_index == std::string::npos) {
                closing_index = text_.length();
            }
            result_text += text_.substr(current_index, comment_index - current_index);
            current_index = closing_index;
        }
        comment_index = text_.find("//", current_index);
        multicomment_index = text_.find("/*", current_index);
    }
    result_text += text_.substr(current_index, text_.length() - current_index);
    text_ = result_text;
}

bool lexer::is_double_char_sym(std::size_t index) {
    if (text_.substr(index, 2) == "&&") {
        return true;
    } else if (text_.substr(index, 2) == "||") {
        return true;
    } else if (text_.substr(index, 2) == "==") {
        return true;
    }
    return false;
}

void lexer::push_token(std::string &lexem) {
    if (is_keyword(lexem)) {
        tokens_.push_back(token(KEYWORD, get_keyword_symbol(lexem), lexem));
    } else if (is_number(lexem)) {
        tokens_.push_back(token(INT_LITERAL, NOT_KEYWORD, lexem));
    } else if (is_identifier(lexem)) {
        tokens_.push_back(token(IDENTIFIER, NOT_KEYWORD, lexem));
    } else {
        //error
    }
}

void lexer::print_tokens() {
    for (token t : tokens_) {
        std::cout << t.type << std::endl;
        std::cout << t.value << std::endl;
        std::cout << "--------------------\n";
    }
}
}