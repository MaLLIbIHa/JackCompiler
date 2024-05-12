#pragma once

class program;
class class_dec;
class variable_dec_list;
class subroutine_list;
class subroutine_dec;
class subroutine_body;
class variable_dec;
class statement_list;
class let_statement;
class if_statement;
class while_statement;
class do_statement;
class return_statement;
class expression_list;
class binop_expr;
class unop_expr;
class literal_expr;
class name_expr;
class array_member_expr;
class subroutine_call_expr;
class member_expr;
class expression;

struct visitor {
    virtual void pre_visit(program *prog) {}
    virtual void pre_visit(class_dec *cl) {}
    virtual void pre_visit(variable_dec_list *var_list) {}
    virtual void pre_visit(subroutine_list *sub_list) {}
    virtual void pre_visit(subroutine_dec *sub_dec) {}
    virtual void pre_visit(subroutine_body *sub_body) {}
    virtual void pre_visit(variable_dec *var_dec) {}
    virtual void pre_visit(statement_list *stmt_list) {}
    virtual void pre_visit(let_statement *let_stmt) {}
    virtual void pre_visit(if_statement *if_stmt) {}
    virtual void pre_visit(while_statement *while_stmt) {}
    virtual void pre_visit(do_statement *do_stmt) {}
    virtual void pre_visit(return_statement *ret_stmt) {}
    virtual void pre_visit(expression_list *expr_list) {}
    virtual void pre_visit(binop_expr *binop) {}
    virtual void pre_visit(unop_expr *unop) {}
    virtual void pre_visit(literal_expr *literal) {}
    virtual void pre_visit(name_expr *literal) {}
    virtual void pre_visit(array_member_expr *arr_expr) {}
    virtual void pre_visit(subroutine_call_expr *sub_call) {}
    virtual void pre_visit(member_expr *mem_expr) {}
    virtual void pre_visit(expression *expr) {}

    virtual void post_visit(program *prog) {}
    virtual void post_visit(class_dec *cl) {}
    virtual void post_visit(variable_dec_list *var_list) {}
    virtual void post_visit(subroutine_list *sub_list) {}
    virtual void post_visit(subroutine_dec *sub_dec) {}
    virtual void post_visit(subroutine_body *sub_body) {}
    virtual void post_visit(variable_dec *var_dec) {}
    virtual void post_visit(statement_list *stmt_list) {}
    virtual void post_visit(let_statement *let_stmt) {}
    virtual void post_visit(if_statement *if_stmt) {}
    virtual void post_visit(while_statement *while_stmt) {}
    virtual void post_visit(do_statement *do_stmt) {}
    virtual void post_visit(return_statement *ret_stmt) {}
    virtual void post_visit(expression_list *expr_list) {}
    virtual void post_visit(binop_expr *binop) {}
    virtual void post_visit(unop_expr *unop) {}
    virtual void post_visit(literal_expr *literal) {}
    virtual void post_visit(name_expr *literal) {}
    virtual void post_visit(array_member_expr *arr_expr) {}
    virtual void post_visit(subroutine_call_expr *sub_call) {}
    virtual void post_visit(member_expr *mem_expr) {}
    virtual void post_visit(expression *expr) {}

    virtual ~visitor() = default;
};