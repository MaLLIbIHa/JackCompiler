#pragma once
#include "token.hpp"
#include <sstream>
#include <variant>
#include <vector>

class parser {
public:
    parser();

    std::string operator()(const std::vector<token> &tokens);

    std::string parse(const std::vector<token> &token);
private:
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

    std::stringstream output_;
};
