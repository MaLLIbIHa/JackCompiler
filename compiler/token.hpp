#pragma once
#include <map>
#include <string>
#include <variant>
#include <cctype>
#include <vector>

enum class token_kind {
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

enum class token_type {
    EMPTY_TOKEN,
    KEYWORD,
    SYMBOL,
    STR_LITERAL,
    INT_LITERAL,
    IDENTIFIER,
    T_EOF,
};

class token {
public:
    inline static const std::map<std::string, token_kind> keywords = {{"class"       , token_kind::CLASS},
                                                                      {"constructor" , token_kind::CONSTRUCTOR},
                                                                      {"function"    , token_kind::FUNCTION},
                                                                      {"method"      , token_kind::METHOD},
                                                                      {"field"       , token_kind::FIELD},
                                                                      {"static"      , token_kind::STATIC},
                                                                      {"var"         , token_kind::VAR},
                                                                      {"int"         , token_kind::INT},
                                                                      {"char"        , token_kind::CHAR},
                                                                      {"boolean"     , token_kind::BOOLEAN},
                                                                      {"void"        , token_kind::VOID},
                                                                      {"true"        , token_kind::TRUE},
                                                                      {"false"       , token_kind::FALSE},
                                                                      {"null"        , token_kind::NULL_KEYWORD},
                                                                      {"this"        , token_kind::THIS},
                                                                      {"let"         , token_kind::LET},
                                                                      {"do"          , token_kind::DO},
                                                                      {"if"          , token_kind::IF},
                                                                      {"else"        , token_kind::ELSE},
                                                                      {"while"       , token_kind::WHILE},
                                                                      {"return"      , token_kind::RETURN},
                                                                      {"("           , token_kind::LPAREN},
                                                                      {")"           , token_kind::RPAREN},
                                                                      {"["           , token_kind::LBRACK},
                                                                      {"]"           , token_kind::RBRACK}, 
                                                                      {"{"           , token_kind::LBRACE},
                                                                      {"}"           , token_kind::RBRACE},
                                                                      {"."           , token_kind::DOT},
                                                                      {","           , token_kind::COMMA},
                                                                      {";"           , token_kind::SEMICOLON},
                                                                      {"+"           , token_kind::ADD},
                                                                      {"-"           , token_kind::SUB},
                                                                      {"*"           , token_kind::MUL},
                                                                      {"/"           , token_kind::QUO},
                                                                      {"&&"          , token_kind::LOG_AND},
                                                                      {"||"          , token_kind::LOG_OR},
                                                                      {"&"           , token_kind::BIT_AND},
                                                                      {"|"           , token_kind::BIT_OR},
                                                                      {"<"           , token_kind::LSS},
                                                                      {">"           , token_kind::GTR},
                                                                      {"="           , token_kind::ASSIGN},
                                                                      {"=="          , token_kind::EQL},
                                                                      {"~"           , token_kind::TILDE}};

public:
    token() = default;

    token(token_type tok_t, token_kind kind, std::string value)
         : type_(tok_t), kind_(kind), value_(value)
         {}

    void set_line_pos(unsigned int line_pos) { line_pos_ = line_pos; }

    void set_in_line_pos(unsigned int in_line_pos) { in_line_pos_ = in_line_pos; }

    void set_value(std::string val) { value_ = val; }

    void set_type(token_type type) { type_ = type; }

    void set_kind(token_kind kind) { kind_ = kind; }

    unsigned int get_line_pos() { return line_pos_; }

    unsigned int get_in_line_pos() { return in_line_pos_; }

    std::string get_value() { return value_; }

    token_type get_type() { return type_; }

    token_kind get_kind() { return kind_; }

private:
    std::string value_;
    token_type type_ = token_type::EMPTY_TOKEN;
    token_kind kind_ = token_kind::NOT_KEYWORD;
    unsigned int line_pos_ = 0;
    unsigned int in_line_pos_ = 0;
};