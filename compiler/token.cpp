//included in hpp
#include "token.hpp"

namespace compiler {

std::map<std::string, keyword_symbol> keywords      = {{"class"       , CLASS},
                                                       {"constructor" , CONSTRUCTOR},
                                                       {"function"    , FUNCTION},
                                                       {"method"      , METHOD},
                                                       {"field"       , FIELD},
                                                       {"static"      , STATIC},
                                                       {"var"         , VAR},
                                                       {"int"         , INT},
                                                       {"char"        , CHAR},
                                                       {"boolean"     , BOOLEAN},
                                                       {"void"        , VOID},
                                                       {"true"        , TRUE},
                                                       {"false"       , FALSE},
                                                       {"null"        , NULL_KEYWORD},
                                                       {"this"        , THIS},
                                                       {"let"         , LET},
                                                       {"do"          , DO},
                                                       {"if"          , IF},
                                                       {"else"        , ELSE},
                                                       {"while"       , WHILE},
                                                       {"return"      , RETURN},
                                                       {"("           , LPAREN},
                                                       {")"           , RPAREN},
                                                       {"["           , LBRACK},
                                                       {"]"           , RBRACK}, 
                                                       {"{"           , LBRACE},
                                                       {"}"           , RBRACE},
                                                       {"."           , DOT},
                                                       {","           , COMMA},
                                                       {";"           , SEMICOLON},
                                                       {"+"           , ADD},
                                                       {"-"           , SUB},
                                                       {"*"           , MUL},
                                                       {"/"           , QUO},
                                                       {"&&"          , LOG_AND},
                                                       {"||"          , LOG_OR},
                                                       {"&"           , BIT_AND},
                                                       {"|"           , BIT_OR},
                                                       {"<"           , LSS},
                                                       {">"           , GTR},
                                                       {"="           , ASSIGN},
                                                       {"=="          , EQL},
                                                       {"~"           , TILDE}};
/*
Token utility function definitions
*/
bool is_keyword(const std::string &lexem) {
    auto keyword = keywords.find(lexem);
    if (keyword != keywords.end() && 
        keyword_beg < keyword->second && keyword->second < keyword_end) 
    {    
        return true;
    } else {
        return false;
    }
}

bool is_symbol(const std::string &lexem) {
    auto keyword = keywords.find(lexem);
    if (keyword != keywords.end() &&
        symbol_beg < keyword->second && keyword->second < symbol_end) 
    {    
        return true;
    } else {
        return false;
    }
}

bool is_number(const std::string &lexem) {
    for (const char &c : lexem) {
        if (std::isdigit(c)) continue;
        else return false;
    }

    return true;
}

bool is_identifier(const std::string &lexem) {
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

keyword_symbol get_keyword_symbol(const std::string &lexem) {
    keyword_symbol type = keywords.find(lexem)->second;
    return type;
}
}