#pragma once
#include <map>
#include <string>
#include <variant>

enum token_type {
    KEYWORD,
    MEM_SEG,
    LABELNAME,
    NUMBER
};

enum keyword_type {
    ADD,
    SUB,
    NEG,
    EQ,
    GT,
    LT,
    AND,
    OR,
    NOT,
    LABEL,
    FUNCTION,
    PUSH,
    POP,
    GOTO,
    IFGOTO,
    CALL,
    RETURN
};

enum mem_segment {
    ARGUMENT,
    STATIC,
    LOCAL,
    CONSTANT,
    TEMP,
    THIS,
    THAT,
};

extern std::map<token_type, std::string> token_type_to_str_;

extern std::map<keyword_type, std::string> instr_2op_to_str;

extern std::map<std::string, keyword_type> keywords_;

extern std::map<std::string, mem_segment> mem_seg_;


bool is_keyword(const std::string &);

bool is_mem_seg(const std::string &);

bool is_label(const std::string &);

bool is_number(const std::string &);

keyword_type get_keyword_type(const std::string &lexem);

mem_segment get_mem_segment(const std::string &lexem);



class keyword_tok {
public:
    keyword_tok(keyword_type t);

    keyword_type get_keyword_type();

private:
    keyword_type keyword_t_;
};


class mem_segment_tok {
public:
    mem_segment_tok(mem_segment s);

    mem_segment get_mem_segment();

private:
    mem_segment mem_seg_;
};


class number_tok {
public:
    number_tok(int n);

    int get_number();

private:
    int num_;
};

class label_tok {
public:
    label_tok(std::string str);

    std::string get_label();

private:
    std::string label_;
};

using token = std::variant<keyword_tok,
                           mem_segment_tok,
                           number_tok,
                           label_tok>;