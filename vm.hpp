#pragma once
#include <string>
#include <vector>
#include <cctype>
#include <map>
#include <iostream>
#include <sstream>
#include <variant>

class virtual_machine {
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
        CONSTANT
    };
    
    std::map<token_type, std::string> token_type_to_str_ = {{KEYWORD, "KEYWORD"}, 
                                                           {MEM_SEG, "MEMORY_SEGMENT"},
                                                           {LABELNAME, "LABELNAME"},
                                                           {NUMBER, "NUMBER"}};

    std::map<keyword_type, std::string> instr_2op_ = {{ADD, "add"},
                                                     {SUB, "sub"},
                                                     {EQ, "eq"},
                                                     {GT, "slt"},
                                                     {LT, "slt"},
                                                     {AND, "and"},
                                                     {OR, "or"}};

    std::map<std::string, keyword_type> keywords_ = {{"add", ADD},
                                                    {"sub", SUB},
                                                    {"neg", NEG},
                                                    {"eq" , EQ},
                                                    {"gt" , GT},
                                                    {"lt" , LT},
                                                    {"and", AND},
                                                    {"or" , OR},
                                                    {"not", NOT},
                                                    {"label", LABEL},
                                                    {"function", FUNCTION},
                                                    {"push", PUSH},
                                                    {"pop", POP},
                                                    {"goto", GOTO},
                                                    {"if-goto", IFGOTO},
                                                    {"return", RETURN},
                                                    {"call", CALL}};
    
    std::map<std::string, mem_segment> mem_seg_ = {{"argument", ARGUMENT},
                                                          {"static", STATIC},
                                                          {"local", LOCAL},
                                                          {"constant", CONSTANT}};

    struct token {
        token_type type;
        std::variant<keyword_type, mem_segment, int, std::string> value;
    };

    bool is_keyword(const std::string &);

    bool is_mem_seg(const std::string &);

    bool is_label(const std::string &);
    
    bool is_number(const std::string &);

    keyword_type get_keyword_type(const std::string &);

    mem_segment get_mem_segment(const std::string &);

    void compile_2op_instr(const keyword_type);

    void compile_1op_instr(const keyword_type);

    void compile_push(const token, const token);

    void compile_pop(const token, const token);

    void compile_label(const token);

    void compile_function(const token, const token);

    void compile_return();

    void compile_call(const token);

    void compile_goto(const token);

    void compile_ifgoto(const token);

    void print_err(const token, const token_type);

public:
    virtual_machine(std::string &);

    int lex();

    int parse();

    void print();

    std::string text_;
    std::vector<token> tokens_;
    std::stringstream output_;
};