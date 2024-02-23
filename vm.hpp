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
        ARGUMENT,
        STATIC,
        LOCAL,
        CONSTANT,
        GOTO,
        IFGOTO,
        CALL,
        RETURN
    };
    
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
                                                    {"argument", ARGUMENT},
                                                    {"static", STATIC},
                                                    {"local", LOCAL},
                                                    {"constant", CONSTANT},
                                                    {"goto", GOTO},
                                                    {"if-goto", IFGOTO},
                                                    {"return", RETURN},
                                                    {"call", CALL}};
    
    struct token {
        token_type type;
        std::variant<keyword_type, int, std::string> value;
    };

    bool is_keyword(const std::string &);

    bool is_label(const std::string &);
    
    bool is_number(const std::string &);

    keyword_type get_keyword_type(std::string &);

    void compile_2op_instr(keyword_type);

    void compile_1op_instr(keyword_type);

    void compile_push(token, token);

    void compile_pop(token, token);

    void compile_label(token);

    void compile_function(token, token);

    void compile_return();

    void compile_call(token);

    void compile_goto(token);

    void compile_ifgoto(token);

public:
    virtual_machine(std::string &);

    int lex();

    void parse();

    void print();

    std::string text_;
    std::vector<token> tokens_;
    std::stringstream output_;
};