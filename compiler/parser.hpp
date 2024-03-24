#pragma once
#include "ast.hpp"
#include "symbol_table.hpp"
#include "token.hpp"

namespace compiler {

enum associativity {
    LEFT_ASC,
    RIGHT_ASC,
};

extern std::map<std::string, std::pair<int, associativity>> op_info_;

class parser {
public:
    parser(std::vector<token> tokens);

    parser(std::vector<token> tokens, std::shared_ptr<program> prog);

    //Main parsing methods
    std::shared_ptr<program> parse_program();

private:
    std::shared_ptr<class_dec> parse_class();

    std::shared_ptr<subrtn_dec> parse_subrtn_dec(token subrtn_kind);

    std::shared_ptr<subrtn_body> parse_subrtn_body();

    std::shared_ptr<statement_list> parse_statements();

    void parse_var_dec(token var_kind,
                       std::shared_ptr<variable_dec_list> vars);

    //Statements parsing methods
    std::shared_ptr<statement> parse_let();

    std::shared_ptr<statement> parse_if();

    std::shared_ptr<statement> parse_while();

    std::shared_ptr<statement> parse_do();

    std::shared_ptr<statement> parse_return();

    //Expression parsing methods
    std::shared_ptr<expression> parse_expression(int prev_prec = 0);

    std::shared_ptr<expression> parse_term();

    std::shared_ptr<expression_list> parse_arg_list();

    std::shared_ptr<expression> parse_compound_id();

    std::shared_ptr<expression> mk_unop_node(token op, 
                                             std::shared_ptr<expression> operand);

    std::shared_ptr<expression> mk_binop_node(token op,
                                              std::shared_ptr<expression> first_operand,
                                              std::shared_ptr<expression> second_operand);

    //Token stream functions
    token consume(keyword_symbol type);

    token consume(token_type type);

    token consume();

    token consume_type();

    token current();

    token next();

    bool has_tokens();

    bool is_type(token tok);

    bool is_unary_op(token tok);

    bool is_binary_op(token tok);

    bool is_class_var_dec(token tok);

    bool is_func_dec(token tok);

    bool is_constant_literal(token tok);

    int get_prec(token tok);

    associativity get_asc(token tok);

    std::string token_to_var_type(token type_tok);

    var_modifier token_to_var_modifier(token mod_tok);

    subroutine_type token_to_subrtn_type(token type_tok);

    //Error output functions
    void expected_err(keyword_symbol tok);

    std::shared_ptr<program> prog_;
    std::vector<token> tokens_;
    std::size_t current_token_ = 0;
};
}