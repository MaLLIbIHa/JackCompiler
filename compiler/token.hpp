#pragma once
#include <map>
#include <string>
#include <variant>
#include <cctype>
#include <vector>

namespace compiler {

enum keyword_symbol {
    NOT_KEYWORD = 0,
    keyword_beg,
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

    constant_beg,
    TRUE,
    FALSE,
    NULL_KEYWORD,
    THIS,
    constant_end,

    statement_beg,
    LET,
    DO,
    IF,
    ELSE,
    WHILE,
    RETURN,
    statement_end,
    keyword_end,

    symbol_beg,
    LPAREN,     // (
    RPAREN,     // )
    LBRACK,     // [
    RBRACK,     // ]
    LBRACE,     // {
    RBRACE,     // }
    DOT,        // .
    COMMA,      // ,
    SEMICOLON,  // ;
    ASSIGN,     // =

    binop_beg,
    ADD,        // +
    SUB,        // -
    MUL,        // *
    QUO,        // /
    LOG_AND,    // &&
    LOG_OR,     // ||
    BIT_OR,     // |
    BIT_AND,    // &
    LSS,        // <
    GTR,        // >
    EQL,        // ==
    binop_end,

    TILDE,      // ~
    symbol_end
};

enum token_type {
    EMPTY_TOKEN,
    KEYWORD,
    SYMBOL,
    STR_LITERAL,
    INT_LITERAL,
    IDENTIFIER,
    T_EOF,
};

extern std::map<std::string, keyword_symbol> keywords;

bool is_keyword(const std::string &lexem);

bool is_symbol(const std::string &lexem);

bool is_number(const std::string &lexem);

bool is_identifier(const std::string &lexem);

keyword_symbol get_keyword_symbol(const std::string &lexem);

struct token {
    token() = default;

    token(token_type tok_t, keyword_symbol kind, std::string value)
         : type(tok_t), kind(kind), value(value)
         {}

    std::string value;
    token_type type = EMPTY_TOKEN;
    keyword_symbol kind = NOT_KEYWORD;
};
}