#include "token.hpp"


std::map<token_type, std::string> token_type_to_str_ = {{KEYWORD    , "KEYWORD"}, 
                                                        {MEM_SEG    , "MEMORY_SEGMENT"},
                                                        {LABELNAME  , "LABELNAME"},
                                                        {NUMBER     , "NUMBER"}};

std::map<keyword_type, std::string> instr_2op_to_str = {{ADD        , "add"},
                                                        {SUB        , "sub"},
                                                        {EQ         , "eq"},
                                                        {GT         , "slt"},
                                                        {LT         , "slt"},
                                                        {AND        , "and"},
                                                        {OR         , "or"}};

std::map<std::string, keyword_type> keywords_        = {{"add"      , ADD},
                                                        {"sub"      , SUB},
                                                        {"neg"      , NEG},
                                                        {"eq"       , EQ},
                                                        {"gt"       , GT},
                                                        {"lt"       , LT},
                                                        {"and"      , AND},
                                                        {"or"       , OR},
                                                        {"not"      , NOT},
                                                        {"label"    , LABEL},
                                                        {"function" , FUNCTION},
                                                        {"push"     , PUSH},
                                                        {"pop"      , POP},
                                                        {"goto"     , GOTO},
                                                        {"if-goto"  , IFGOTO},
                                                        {"return"   , RETURN},
                                                        {"call"     , CALL}};

std::map<std::string, mem_segment> mem_seg_          = {{"argument" , ARGUMENT},
                                                        {"static"   , STATIC},
                                                        {"local"    , LOCAL},
                                                        {"constant" , CONSTANT}};

/*
Token utility function definitions
*/
bool is_keyword(const std::string &lexem) {
    return (keywords_.find(lexem) != keywords_.end());
}

bool is_mem_seg(const std::string &lexem) {
    return (mem_seg_.find(lexem) != mem_seg_.end());
}

bool is_label(const std::string &lexem) {
    if (std::isdigit(lexem[0])) return false;

    for (const char &c : lexem) {
        if (std::isalnum(c) || c == '.' || c == '_') continue;
        else return false;
    }

    return true;
}

bool is_number(const std::string &lexem) {
    for (const char &c : lexem) {
        if (std::isdigit(c)) continue;
        else return false;
    }

    return true;
}

keyword_type get_keyword_type(const std::string &lexem) {
    keyword_type type = keywords_.find(lexem)->second;
    return type;
}

mem_segment get_mem_segment(const std::string &lexem) {
    mem_segment seg = mem_seg_.find(lexem)->second;
    return seg;
}

/*
Keyword token class definitions
*/
keyword_tok::keyword_tok(keyword_type t) : keyword_t_(t) {}

keyword_type keyword_tok::get_keyword_type() {
    return keyword_t_;
}

/*
Memory segment token class definitions
*/
mem_segment_tok::mem_segment_tok(mem_segment s) : mem_seg_(s) {}

mem_segment mem_segment_tok::get_mem_segment() {
    return mem_seg_;
}

/*
Number token class definitions
*/
number_tok::number_tok(int n) : num_(n) {}

int number_tok::get_number() {
    return num_;
}

/*
Label name token class definitions
*/
label_tok::label_tok(std::string str) : label_(str) {}

std::string label_tok::get_label() {
    return label_;
}