#pragma once
#include "ast.hpp"
#include "token.hpp"
#include "lexer.hpp"

enum class associativity {
    LEFT_ASC,
    RIGHT_ASC,
};

extern std::map<std::string, std::pair<int, associativity>> op_info_;

class parser final {
public:
    parser(std::string sourceText);

    parser(std::string sourceText, std::shared_ptr<program> prog);

    //Main parsing methods
    std::shared_ptr<program> parse_program();

private:
    std::shared_ptr<class_dec> parse_class();

    std::shared_ptr<subroutine_dec> parse_subroutine_dec(token subrtn_kind);

    std::shared_ptr<subroutine_body> parse_subroutine_body();

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
    bool is_type(token tok);

    bool is_unary_op(token tok);

    bool is_binary_op(token tok);

    bool is_class_var_dec(token tok);

    bool is_func_dec(token tok);

    bool is_constant_literal(token tok);

    token consume(token_type type);

    token consume(token_kind kind);

    token consume();

    int get_prec(token tok);

    void set_position(std::shared_ptr<node> node, token tok);

    associativity get_asc(token tok);

    std::string token_to_var_type(token type_tok);

    var_modifier token_to_var_modifier(token mod_tok);

    subroutine_kind token_to_subroutine_kind(token type_tok);

    literal_type token_to_literal_type(token kind);

    //Error output functions
    void print_expected_err(token_kind kind, unsigned line_pos, unsigned in_line_pos);

    void print_expected_err(token_type type, unsigned line_pos, unsigned in_line_pos);

    void print_expected_err(const char *err_msg, unsigned line_pos, unsigned in_line_pos);

    std::shared_ptr<program> prog_;
    lexer lexer_;
};