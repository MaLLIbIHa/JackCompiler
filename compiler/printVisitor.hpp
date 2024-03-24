#pragma once
#include <fstream>
#include "ast.hpp"

namespace compiler {

class PrintVisitor : public visitor {
public:
    PrintVisitor() = default;
    PrintVisitor(int indent_diff) : indent_diff_(indent_diff) {}
private:
    visit_order get_order() override { return PRE_POST_ORDER; }

    void visit(std::shared_ptr<program> prog) override {
        if (get_state() == PRE_ORDER) {
            std::cout << open_tag("program") << '\n';
        } else {
            std::cout << close_tag("program") << '\n';
        }
    }

    void visit(std::shared_ptr<class_dec> cl) override {
        if (get_state() == PRE_ORDER) {
            std::cout << open_tag_with_inc_indent("class");
            inc_indent();
            std::cout << oneline_tag_with_indent("name", cl->get_name());
            dec_indent();
        } else {
            std::cout << close_tag_with_dec_indent("class");
        }
    }

    void visit(std::shared_ptr<variable_dec_list> var_list) override {
        print_simple_tag("variableList");
    }

    void visit(std::shared_ptr<variable_dec> var) override {
        if (get_state() == PRE_ORDER) {
            std::string kind;
            switch (var->get_var_kind()) {
            case LOCAL_V:
                kind = "local";
                break;
            case ARG_V:
                kind = "argument";
                break;
            case FIELD_V:
                kind = "field";
                break;
            case STATIC_V:
                kind = "static";
                break;
            }
            std::cout << open_tag_with_inc_indent("variableDeclaration");
            inc_indent();
            std::cout << oneline_tag_with_indent("name", var->get_var_name());
            std::cout << oneline_tag_with_indent("type", var->get_var_type());
            std::cout << oneline_tag_with_indent("kind", kind);
            dec_indent();
        } else {
            std::cout << close_tag_with_dec_indent("variableDeclaration");
        }
    }

    void visit(std::shared_ptr<subroutine_list> subrtn_list) override {
        print_simple_tag("subroutineList");
    }

    void visit(std::shared_ptr<subrtn_dec> subrtn) override {
        if (get_state() == PRE_ORDER) {
            std::string subrtn_type;
            switch (subrtn->get_subrtn_type()) {
            case FUNCTION_S:
                subrtn_type = "function";
                break;
            case METHOD_S:
                subrtn_type = "method";
                break;
            case CONSTRUCTOR_S:
                subrtn_type = "constructor";
                break;
            }
            std::cout << open_tag_with_inc_indent("subroutineDeclaration");
            inc_indent();
            std::cout << oneline_tag_with_indent("name", subrtn->get_name());
            std::cout << oneline_tag_with_indent("returnType", subrtn->get_ret_type());
            std::cout << oneline_tag_with_indent("kind", subrtn_type);
            dec_indent();
        } else {
            std::cout << close_tag_with_dec_indent("subroutineDeclaration");
        }
    }

    void visit(std::shared_ptr<subrtn_body> body) override {
        print_simple_tag("subroutineBody");
    }
    void visit(std::shared_ptr<statement_list> stmt_list) override {
        print_simple_tag("statementList");
    }

    void visit(std::shared_ptr<let_statement> let_stmt) override {
        print_simple_tag("letStatement");
    }

    void visit(std::shared_ptr<if_statement> if_stmt) override {
        print_simple_tag("ifStatement");
    }

    void visit(std::shared_ptr<while_statement> while_stmt) override {
        print_simple_tag("whileStatement");
    }

    void visit(std::shared_ptr<do_statement> do_stmt) override {
        print_simple_tag("doStatement");
    }

    void visit(std::shared_ptr<return_statement> stmt) override {
        print_simple_tag("returnStatement");
    }

    void visit(std::shared_ptr<expression_list> expr_list) override {
        print_simple_tag("expressionList");
    }
    void visit(std::shared_ptr<binop_expr> expr) override {
        std::string expr_kind = check_expr_kind(expr);
        if (get_state() == PRE_ORDER) {
            if (!expr_kind.empty()) {
                std::cout << open_tag_with_inc_indent(expr_kind);
            }
            std::string op = convert_op_type(expr->get_op_type());
            std::cout << open_tag_with_inc_indent("binaryOperation");
            inc_indent();
            std::cout << oneline_tag_with_indent("operation", op);
            dec_indent();
        } else {
            std::cout << close_tag_with_dec_indent("binaryOperation");
            if (!expr_kind.empty()) {
                std::cout << close_tag_with_dec_indent(expr_kind);
            }
        }
    }
    void visit(std::shared_ptr<unop_expr> expr) override {
        std::string expr_kind = check_expr_kind(expr);
        if (get_state() == PRE_ORDER) {
            if (!expr_kind.empty()) {
                std::cout << open_tag_with_inc_indent(expr_kind);
            }
            std::string op = convert_op_type(expr->get_op_type());
            std::cout << open_tag_with_inc_indent("unaryOperation");
            inc_indent();
            std::cout << oneline_tag_with_indent("operation", op);
            dec_indent();
        } else {
            std::cout << close_tag_with_dec_indent("unaryOperation");
            if (!expr_kind.empty()) {
                std::cout << close_tag_with_dec_indent(expr_kind);
            }
        }
    }

    void visit(std::shared_ptr<literal_expr> expr) override {
        std::string expr_kind = check_expr_kind(expr);
        if (get_state() == PRE_ORDER) {
            std::string data = expr->get_literal().value;
            if (!expr_kind.empty()) {
                std::cout << open_tag_with_inc_indent(expr_kind);
            }
            inc_indent();
            std::cout << oneline_tag_with_indent("literal", data);
            dec_indent();
            if (!expr_kind.empty()) {
                std::cout << close_tag_with_dec_indent(expr_kind);
            }
        }
    }

    void visit(std::shared_ptr<array_member_expr> expr) override {
        print_expr_tag("arrayMemberExpression", expr);
    }

    void visit(std::shared_ptr<subrtn_call_expr> expr) override {
        print_expr_tag("callExpression", expr);
    }

    void visit(std::shared_ptr<member_expr> expr) override {
        print_expr_tag("memberExpression", expr);
    }

    void print_simple_tag(std::string data) {
        if (get_state() == PRE_ORDER) {
            std::cout << open_tag_with_inc_indent(data);
        } else {
            std::cout << close_tag_with_dec_indent(data);
        }
    }

    void print_expr_tag(std::string expr_tag, std::shared_ptr<expression> expr) {
        std::string expr_kind = check_expr_kind(expr);
        if (get_state() == PRE_ORDER) {
            if (!expr_kind.empty()) {
                std::cout << open_tag_with_inc_indent(expr_kind);
            }
            std::cout << open_tag_with_inc_indent(expr_tag);
        } else {
            std::cout << close_tag_with_dec_indent(expr_tag);
            if (!expr_kind.empty()) {
                std::cout << close_tag_with_dec_indent(expr_kind);
            }
        }
    }
    
    std::string check_expr_kind(std::shared_ptr<expression> expr) {
        node_type stmt_type = expr->get_parent()->get_type();
        std::string expr_kind;
        switch (stmt_type) {
        case IF_STATEMENT:
        case WHILE_STATEMENT:
            expr_kind = "condition";
            break;

        case LET_STATEMENT:
            if (expr->get_parent()->child(0) == expr) {
                expr_kind = "leftExpression";
            } else {
                expr_kind = "rightExpression";
            }
            break;

        case BINOP_EXPR:
            if (expr->get_parent()->child(0) == expr) {
                expr_kind = "leftOperand";
            } else {
                expr_kind = "rightOperand";
            }
            break;

        case ARRAY_MEMBER_EXPR:
            if (expr->get_parent()->child(0) == expr) {
                expr_kind = "identifier";
            } else {
                expr_kind = "indexExpression";
            }
            break;

        case MEMBER_EXPR:
            if (expr->get_parent()->child(0) == expr) {
                expr_kind = "identifier";
            } else {
                expr_kind = "member";
            }
            break;

        default:
            return std::string{};
        }
        return expr_kind;
    }

    std::string convert_op_type(op_type type) {
        std::string op;
        switch (type) {
        case ADD_OP:
            op = "+";
            break;
        case SUB_OP:
            op = "-";
            break;
        case MUL_OP:
            op = "*";
            break;
        case DIV_OP:
            op = "/";
            break;
        case LOG_AND_OP:
            op = "&&";
            break;
        case LOG_OR_OP:
            op = "||";
            break;
        case BIT_AND_OP:
            op = "&";
            break;
        case BIT_OR_OP:
            op = "|";
            break;
        case LSS_OP:
            op = "<";
            break;
        case GTR_OP:
            op = ">";
            break;
        case EQL_OP:
            op = "==";
        case NEG_OP:
            op = "-";
            break;
        case TILDE_OP:
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
}