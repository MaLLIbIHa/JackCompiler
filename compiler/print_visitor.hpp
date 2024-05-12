#pragma once
#include <fstream>
#include <memory>
#include "visitor.hpp"
#include "ast.hpp"

class print_visitor final : public visitor {
public:
    print_visitor() = default;
    print_visitor(int indent_diff) : indent_diff_(indent_diff) {}
private:
    void pre_visit(program *prog) override {
        std::cout << open_tag("program") << std::endl;
    }
    void post_visit(program *prog) override {
        std::cout << close_tag("program") << std::endl;
    }

    void pre_visit(class_dec *cl) override {
        std::cout << open_tag_with_inc_indent("class");
        inc_indent();
        std::cout << oneline_tag_with_indent("name", cl->get_name());
        dec_indent();
    }
    void post_visit(class_dec *cl) override {
        std::cout << close_tag_with_dec_indent("class");
    }

    void pre_visit(variable_dec_list *var_list) override {
        std::cout << open_tag_with_inc_indent("variableList");
    }
    void post_visit(variable_dec_list *var_list) override {
        std::cout << close_tag_with_dec_indent("variableList");
    }

    void pre_visit(variable_dec *var) override {
        std::string kind;
        switch (var->get_var_kind()) {
        case var_modifier::LOCAL_V:
            kind = "local";
            break;
        case var_modifier::ARG_V:
            kind = "argument";
            break;
        case var_modifier::FIELD_V:
            kind = "field";
            break;
        case var_modifier::STATIC_V:
            kind = "static";
            break;
        }
        std::cout << open_tag_with_inc_indent("variableDeclaration");
        inc_indent();
        std::cout << oneline_tag_with_indent("name", var->get_var_name());
        std::cout << oneline_tag_with_indent("type", var->get_var_type());
        std::cout << oneline_tag_with_indent("kind", kind);
        dec_indent();
    }
    void post_visit(variable_dec *var) override {
        std::cout << close_tag_with_dec_indent("variableDeclaration");
    }

    void pre_visit(subroutine_list *sub_list) override {
        std::cout << open_tag_with_inc_indent("subroutineList");
    }
    void post_visit(subroutine_list *sub_list) override {
        std::cout << close_tag_with_dec_indent("subroutineList");
    }

    void pre_visit(subroutine_dec *sub) override {
        std::string subrtn_type;
        switch (sub->get_subroutine_kind()) {
        case subroutine_kind::FUNCTION_S:
            subrtn_type = "function";
            break;
        case subroutine_kind::METHOD_S:
            subrtn_type = "method";
            break;
        case subroutine_kind::CONSTRUCTOR_S:
            subrtn_type = "constructor";
            break;
        }
        std::cout << open_tag_with_inc_indent("subroutineDeclaration");
        inc_indent();
        std::cout << oneline_tag_with_indent("name", sub->get_name());
        std::cout << oneline_tag_with_indent("returnType", sub->get_ret_type());
        std::cout << oneline_tag_with_indent("kind", subrtn_type);
        dec_indent();
    }
    void post_visit(subroutine_dec *sub) override {
        std::cout << close_tag_with_dec_indent("subroutineDeclaration");
    }

    void pre_visit(subroutine_body *sub_body) override {
        std::cout << open_tag_with_inc_indent("subroutineBody");
    }
    void post_visit(subroutine_body *sub_body) override {
        std::cout << close_tag_with_dec_indent("subroutineBody");
    }

    void pre_visit(statement_list *stmt_list) override {
        std::cout << open_tag_with_inc_indent("statementList");
    }
    void post_visit(statement_list *stmt_list) override {
        std::cout << close_tag_with_dec_indent("statementList");
    }

    void pre_visit(let_statement *let_stmt) override {
        std::cout << open_tag_with_inc_indent("letStatement");
    }
    void post_visit(let_statement *let_stmt) override {
        std::cout << close_tag_with_dec_indent("letStatement");
    }

    void pre_visit(if_statement *if_stmt) override {
        std::cout << open_tag_with_inc_indent("ifStatement");
    }
    void post_visit(if_statement *if_stmt) override {
        std::cout << close_tag_with_dec_indent("ifStatement");
    }

    void pre_visit(while_statement *while_stmt) override {
        std::cout << open_tag_with_inc_indent("whileStatement");
    }
    void post_visit(while_statement *while_stmt) override {
        std::cout << close_tag_with_dec_indent("whileStatement");
    }

    void pre_visit(do_statement *do_stmt) override {
        std::cout << open_tag_with_inc_indent("doStatement");
    }
    void post_visit(do_statement *do_stmt) override {
        std::cout << close_tag_with_dec_indent("doStatement");
    }

    void pre_visit(return_statement *ret_stmt) override {
        std::cout << open_tag_with_inc_indent("retStatement");
    }
    void post_visit(return_statement *ret_stmt) override {
        std::cout << close_tag_with_dec_indent("retStatement");
    }

    void pre_visit(expression_list *expr_list) override {
        std::cout << open_tag_with_inc_indent("expressionList");
    }
    void post_visit(expression_list *expr_list) override {
        std::cout << close_tag_with_dec_indent("expressionList");
    }

    void pre_visit(binop_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        if (!expr_kind.empty()) {
            std::cout << open_tag_with_inc_indent(expr_kind);
        }
        std::string op = convert_op_type(expr->get_op_type());
        std::cout << open_tag_with_inc_indent("binaryOperation");
        inc_indent();
        std::cout << oneline_tag_with_indent("operation", op);
        dec_indent();
        }
    void post_visit(binop_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        std::cout << close_tag_with_dec_indent("binaryOperation");
        if (!expr_kind.empty()) {
            std::cout << close_tag_with_dec_indent(expr_kind);
        }
    }

    void pre_visit(unop_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        if (!expr_kind.empty()) {
            std::cout << open_tag_with_inc_indent(expr_kind);
        }
        std::string op = convert_op_type(expr->get_op_type());
        std::cout << open_tag_with_inc_indent("unaryOperation");
        inc_indent();
        std::cout << oneline_tag_with_indent("operation", op);
        dec_indent();
    }
    void post_visit(unop_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        std::cout << close_tag_with_dec_indent("unaryOperation");
        if (!expr_kind.empty()) {
            std::cout << close_tag_with_dec_indent(expr_kind);
        }
    }

    void pre_visit(literal_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        std::string data = expr->get_value();
        if (!expr_kind.empty()) {
            std::cout << open_tag_with_inc_indent(expr_kind);
        }
        inc_indent();
        std::cout << oneline_tag_with_indent("literal", data);
        dec_indent();
    }
    void post_visit(literal_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        if (!expr_kind.empty()) {
            std::cout << close_tag_with_dec_indent(expr_kind);
        }
    }

    void pre_visit(name_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        std::string data = expr->get_name();
        if (!expr_kind.empty()) {
            std::cout << open_tag_with_inc_indent(expr_kind);
        }
        inc_indent();
        std::cout << oneline_tag_with_indent("name", data);
        dec_indent();
    }
    void post_visit(name_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        if (!expr_kind.empty()) {
            std::cout << close_tag_with_dec_indent(expr_kind);
        }
    }

    void pre_visit(array_member_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        if (!expr_kind.empty()) {
            std::cout << open_tag_with_inc_indent(expr_kind);
        }
        std::cout << open_tag_with_inc_indent("arrayMemberExpression");
    }
    void post_visit(array_member_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        std::cout << close_tag_with_dec_indent("arrayMemberExpression");
        if (!expr_kind.empty()) {
            std::cout << close_tag_with_dec_indent(expr_kind);
        }
    }

    void pre_visit(subroutine_call_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        if (!expr_kind.empty()) {
            std::cout << open_tag_with_inc_indent(expr_kind);
        }
        std::cout << open_tag_with_inc_indent("callExpression");
    }
    void post_visit(subroutine_call_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        std::cout << close_tag_with_dec_indent("callExpression");
        if (!expr_kind.empty()) {
            std::cout << close_tag_with_dec_indent(expr_kind);
        }
    }

    void pre_visit(member_expr *expr) override {
        std::string expr_kind = check_expr_kind(expr);
        if (!expr_kind.empty()) {
            std::cout << open_tag_with_inc_indent(expr_kind);
        }
        std::cout << open_tag_with_inc_indent("memberExpression");
    }
    void post_visit(member_expr *expr) override {
        inc_indent();
        std::cout << oneline_tag_with_indent("member", expr->get_member());
        dec_indent();
        std::string expr_kind = check_expr_kind(expr);
        std::cout << close_tag_with_dec_indent("memberExpression");
        if (!expr_kind.empty()) {
            std::cout << close_tag_with_dec_indent(expr_kind);
        }
    }
    
    std::string check_expr_kind(expression *expr) {
        node_type stmt_type = expr->get_parent()->get_type();
        std::string expr_kind;
        switch (stmt_type) {
        case node_type::IF_STATEMENT:
        case node_type::WHILE_STATEMENT:
            expr_kind = "condition";
            break;

        case node_type::LET_STATEMENT:
            if (expr->get_parent()->child(0).get() == expr) {
                expr_kind = "leftExpression";
            } else {
                expr_kind = "rightExpression";
            }
            break;

        case node_type::BINOP_EXPR:
            if (expr->get_parent()->child(0).get() == expr) {
                expr_kind = "leftOperand";
            } else {
                expr_kind = "rightOperand";
            }
            break;

        case node_type::ARRAY_MEMBER_EXPR:
            if (expr->get_parent()->child(0).get() == expr) {
                expr_kind = "identifier";
            } else {
                expr_kind = "indexExpression";
            }
            break;

        case node_type::MEMBER_EXPR:
            expr_kind = "identifier";
            break;

        default:
            return std::string{};
        }
        return expr_kind;
    }

    std::string convert_op_type(op_type type) {
        std::string op;
        switch (type) {
        case op_type::ADD_OP:
            op = "+";
            break;
        case op_type::SUB_OP:
            op = "-";
            break;
        case op_type::MUL_OP:
            op = "*";
            break;
        case op_type::DIV_OP:
            op = "/";
            break;
        case op_type::LOG_AND_OP:
            op = "&&";
            break;
        case op_type::LOG_OR_OP:
            op = "||";
            break;
        case op_type::BIT_AND_OP:
            op = "&";
            break;
        case op_type::BIT_OR_OP:
            op = "|";
            break;
        case op_type::LSS_OP:
            op = "<";
            break;
        case op_type::GTR_OP:
            op = ">";
            break;
        case op_type::EQL_OP:
            op = "==";
			break;
        case op_type::NEG_OP:
            op = "-";
            break;
        case op_type::TILDE_OP:
            op = "~";
            break;
        }
        return op;
    }

    void inc_indent() {
        indent_size_ += indent_diff_;
        current_indent_ = std::string(indent_size_, ' ');
    }

    void dec_indent() {
        indent_size_ -= indent_diff_;
        current_indent_ = std::string(indent_size_, ' ');
    }

    std::string open_tag_with_inc_indent(std::string tag) {
        inc_indent();
        return current_indent_ + open_tag(tag) + '\n';
    }

    std::string close_tag_with_dec_indent(std::string tag) {
        std::string tmp = current_indent_ + close_tag(tag) + '\n';
        dec_indent();
        return tmp;
    }

    std::string oneline_tag_with_indent(std::string tag, std::string data) {
        return current_indent_ + 
               open_tag(tag)   + " " + data + " " + close_tag(tag) + '\n';
    }

    std::string open_tag(std::string tag_name) {
        return "<" + tag_name + ">";
    }

    std::string close_tag(std::string tag_name) {
        return "</" + tag_name + ">";
    }

    std::ofstream output_;
    std::string current_indent_;
    unsigned int indent_size_ = 0;
    unsigned int indent_diff_ = 2;
};