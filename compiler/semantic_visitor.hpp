#pragma once
#include <stack>
#include <string>
#include <sstream>
#include "symbols_visitor.hpp"
#include "visitor.hpp"

class semantic_visitor : public visitor {
    enum class value_category {
        NOT_VALUE,
        LVALUE,
        RVALUE,
    };

    struct expr_info {
        std::string eval_type;
        std::shared_ptr<symbol_node> sym;
        value_category val_cat;
    };

public:
    void pre_visit(program* prog) override {
        symbols_visitor sym_visitor;
        prog->accept(sym_visitor);
        global_table_ = sym_visitor.get_global_table();
    }

    void pre_visit(class_dec *cl) override {
        std::string class_name = cl->get_name();
        current_class_ = std::static_pointer_cast<class_symbol>(global_table_.find(class_name));
    }

    void pre_visit(subroutine_dec *sub_dec) override {
        std::string sub_name = sub_dec->get_name();
        current_subrtn_ = std::static_pointer_cast<subroutine_symbol>(current_class_->find_member(sub_name));
        subroutine_kind sub_kind = current_subrtn_->get_kind();
        if (sub_kind == subroutine_kind::FUNCTION_S)
        {
            return;
        }

        local_index_ = 0;
        arg_index_ = 0;
        var_modifier var_mod;
        if (sub_kind == subroutine_kind::METHOD_S) {
            var_mod = var_modifier::ARG_V;
            arg_index_ = 1;
        } else if (sub_kind == subroutine_kind::CONSTRUCTOR_S) {
            var_mod = var_modifier::LOCAL_V;
            local_index_ = 1;
        } 
        auto this_sym = std::make_shared<var_symbol>("this", 
                                                     current_class_->get_name(),
                                                     var_mod,
                                                     0,
                                                     sub_dec->get_line_pos(),
                                                     sub_dec->get_in_line_pos());
        local_table_.insert("this", this_sym);
    }

    void post_visit(subroutine_dec *sub_dec) {
        local_table_.clear();
        current_subrtn_ = nullptr;
    }

    void pre_visit(variable_dec_list* var_list) override {
        std::shared_ptr<subroutine_dec> subrtn_node;
        node_type parent_node_type = var_list->get_parent()->get_type();
        std::shared_ptr<node> parent_node = var_list->get_parent();
        if (parent_node_type == node_type::SUBRTN_DEC) {
            subrtn_node = std::static_pointer_cast<subroutine_dec>(parent_node);
        } else if (parent_node_type == node_type::SUBRTN_BODY) {
            subrtn_node = std::static_pointer_cast<subroutine_dec>(parent_node->get_parent());
        } else {
            return;
        }

        int current_index = parent_node_type == node_type::SUBRTN_BODY ? arg_index_ :
                                                                         local_index_;

        for (int i = 0; i < var_list->children(); i++) {
            auto var = std::static_pointer_cast<variable_dec>(var_list->child(i));
            var_symbol var_sym(var->get_var_name(),
                               var->get_var_type(),
                               var->get_var_kind(),
                               current_index,
                               var->get_line_pos(),
                               var->get_in_line_pos());
            local_table_.insert(var->get_var_name(), std::make_shared<var_symbol>(var_sym));
            current_index++;
        }
    }

    void post_visit(name_expr* name_expr) override {
        std::string name = name_expr->get_name();
        auto cur_symbol = local_table_.find(name);
        if (cur_symbol == nullptr) {
            cur_symbol = current_class_->find_member(name);
        }
        if (cur_symbol == nullptr) {
            cur_symbol = global_table_.find(name);
        }
        if (cur_symbol == nullptr) {
            std::cerr << current_func_name_err_msg()
                      << line_err_msg(name_expr->get_line_pos(), name_expr->get_in_line_pos())
                      << "Undeclared name " << name << std::endl;
        }

        if (cur_symbol->get_symbol_type() == symbol_type::VARIABLE_SYM &&
            current_subrtn_->get_kind() == subroutine_kind::FUNCTION_S) 
        {
            auto var_sym = std::static_pointer_cast<var_symbol>(cur_symbol);
            if (var_sym->get_mode() == var_modifier::FIELD_V) {
                print_err_msg("Cannot use field variables in static function",
                              name_expr->get_line_pos(),
                              name_expr->get_in_line_pos());
                throw std::runtime_error("Semantic error");
            }
            if (var_sym->get_name() == "this") {
                print_err_msg("Cannot use \'this\' in static function",
                              name_expr->get_line_pos(),
                              name_expr->get_in_line_pos());
                throw std::runtime_error("Semantic error");
            }
        }

        std::string eval_type = symbol_to_eval_type(cur_symbol);
        value_category value_cat = value_category::NOT_VALUE;
        if (!eval_type.empty()) value_cat = value_category::LVALUE;

        expr_stack_.push({eval_type, cur_symbol, value_cat});
    }

    void post_visit(literal_expr *lit_expr) override {
        std::string eval_type;
        switch(lit_expr->get_literal_type()) {
        case literal_type::INT_LITERAL:
            eval_type = "int";
            break;
        case literal_type::STR_LITERAL:
            eval_type = "string";
            break;
        case literal_type::NULL_LITERAL:
            eval_type = "null_t";
            break;
        case literal_type::FALSE_LITERAL:
        case literal_type::TRUE_LITERAL:
            eval_type = "bool";
            break;
        }

        expr_stack_.push({eval_type, nullptr, value_category::RVALUE});
    }

    void post_visit(member_expr* member_expr) override {
        expr_info name_info = expr_stack_.top();
        expr_stack_.pop();
        if (name_info.sym == nullptr ||
            name_info.sym->get_symbol_type() == symbol_type::SUBROUTINE_SYM) 
        {
            print_err_msg("Request for member in something not a class",
                          member_expr->get_line_pos(),
                          member_expr->get_in_line_pos());
            throw std::runtime_error("Semantic error");
        }

        std::string name_type = name_info.eval_type;
        if (name_info.sym->get_symbol_type() == symbol_type::CLASS_SYM) {
            name_type = name_info.sym->get_name();
        }
        auto class_type_sym = std::static_pointer_cast<class_symbol>(global_table_.find(name_type));

        auto member_sym = class_type_sym->find_member(member_expr->get_member());
        if (member_sym == nullptr) {
            std::cerr << current_func_name_err_msg()
                      << line_err_msg(member_expr->get_line_pos(), member_expr->get_in_line_pos())
                      << "Class " << name_type << " do not have " 
                      << member_expr->get_member() << " member" << std::endl;
            throw std::runtime_error("Semantic error");
        }

        if (name_info.sym->get_symbol_type() == symbol_type::CLASS_SYM) {
            bool static_member = false;
            if (member_sym->get_symbol_type() == symbol_type::VARIABLE_SYM) {
                auto var_sym = std::static_pointer_cast<var_symbol>(member_sym);
                static_member = var_sym->get_mode() == var_modifier::STATIC_V;
            }
            if (member_sym->get_symbol_type() == symbol_type::SUBROUTINE_SYM) {
                auto subrtn_sym = std::static_pointer_cast<subroutine_symbol>(member_sym);
                static_member = subrtn_sym->get_kind() == subroutine_kind::FUNCTION_S;
            }
            if (!static_member) {
                print_err_msg("Cannot use non-static member with class name",
                              member_expr->get_line_pos(),
                              member_expr->get_in_line_pos());
                throw std::runtime_error("Semantic error");
            }
        }

        std::string eval_type = symbol_to_eval_type(member_sym);
        value_category value_cat = value_category::NOT_VALUE;
        if (!eval_type.empty()) value_cat = value_category::LVALUE;

        expr_stack_.push({eval_type, member_sym, value_cat});
    }

    void post_visit(array_member_expr *array_expr) override {
        expr_info index_expr = expr_stack_.top();
        expr_stack_.pop();
        expr_info name_info = expr_stack_.top();
        expr_stack_.pop();
        if (name_info.eval_type != "Array") {
            print_err_msg("Subscripted object is not \"Array\" type",
                          array_expr->get_line_pos(),
                          array_expr->get_in_line_pos());
            throw std::runtime_error("Semantic error");
        }

        if (index_expr.eval_type != "int") {
            print_err_msg("Array index expression in not an integer",
                          array_expr->get_line_pos(),
                          array_expr->get_in_line_pos());
            throw std::runtime_error("Semantic error");
        }

        expr_stack_.push({"int", nullptr, value_category::LVALUE});
    }

    void post_visit(subroutine_call_expr *sub_call) override {
        std::stack<expr_info> args;
        unsigned call_args_count = sub_call->get_arg_count();
        for (unsigned i = 0; i < call_args_count; i++) {
            args.push(expr_stack_.top());
            expr_stack_.pop();
        }

        expr_info name_info = expr_stack_.top();
        expr_stack_.pop();
        if (name_info.sym == nullptr ||
            name_info.sym->get_symbol_type() != symbol_type::SUBROUTINE_SYM) 
        {
            print_err_msg("Called object is not a subroutine",
                          sub_call->get_line_pos(),
                          sub_call->get_in_line_pos());
            throw std::runtime_error("Semantic error");
        }

        auto sub_sym = std::static_pointer_cast<subroutine_symbol>(name_info.sym);
        std::string ret_type = sub_sym->get_ret_type();
        unsigned sub_arg_count = sub_sym->get_arg_count();
        if (sub_arg_count != call_args_count) {
            std::cerr << current_func_name_err_msg()
                      << line_err_msg(sub_call->get_line_pos(), sub_call->get_in_line_pos())
                      << "Argument count mismatch\n"
                      << "Provided " << call_args_count << "\n"
                      << "In function " << sub_sym->get_name()
                      << " declared arguments count is " << sub_sym->get_arg_count() << std::endl;
            throw std::runtime_error("Semantic error");
        }
        for (unsigned i = 0; i < sub_arg_count; i++) {
            expr_info arg_info = args.top();
            args.pop();
            if (arg_info.eval_type != sub_sym->get_arg_type(i)) {
                auto arg_node = sub_call->child(1)->child(i);
                std::cerr << current_func_name_err_msg()
                          << line_err_msg(arg_node->get_line_pos(), arg_node->get_in_line_pos())
                          << "Argument type mismatch\n"
                          << "Provided argument have type: " << arg_info.eval_type << "\n"
                          << "In function " << sub_sym->get_name()
                          << " declared type is " << sub_sym->get_arg_type(i) << std::endl;
                throw std::runtime_error("Semantic error");
            }
        }

        expr_stack_.push({ret_type, nullptr, value_category::RVALUE});
    }

    void post_visit(unop_expr *unop) override {
        expr_info expr = expr_stack_.top();
        expr_stack_.pop();
        if (expr.val_cat == value_category::NOT_VALUE) {
            print_err_msg("Operand of unary operation must be value",
                          unop->get_line_pos(),
                          unop->get_in_line_pos());
            throw std::runtime_error("Semantic error");
        }
        if (expr.eval_type != "bool" && expr.eval_type != "int") {
            std::cerr << current_func_name_err_msg()
                      << line_err_msg(unop->get_line_pos(), unop->get_in_line_pos())
                      << "Invalid operand to unary operation\n"
                      << "Operand have type: " << expr.eval_type << std::endl;
            throw std::runtime_error("Semantic error");
        }

        expr_stack_.push({expr.eval_type, nullptr, value_category::RVALUE});
    }

    void post_visit(binop_expr *binop) override {
        expr_info second_op = expr_stack_.top();
        expr_stack_.pop();
        expr_info first_op = expr_stack_.top();
        expr_stack_.pop();

        std::string expr_type;
        std::string operands_type;
        switch (binop->get_op_type()) {
        case op_type::ADD_OP:
        case op_type::SUB_OP:
        case op_type::DIV_OP:
        case op_type::MUL_OP:
        case op_type::BIT_AND_OP:
        case op_type::BIT_OR_OP:
            expr_type = "int";
            operands_type = "int";
            break;
        case op_type::LSS_OP:
        case op_type::GTR_OP:
            expr_type = "bool";
            operands_type = "int";
            break;
        case op_type::LOG_AND_OP:
        case op_type::LOG_OR_OP:
            expr_type = "bool";
            operands_type = "bool";
            break;
        }

        if (first_op.eval_type != operands_type ||
            second_op.eval_type != operands_type) 
        {
            std::cerr << current_func_name_err_msg()
                      << line_err_msg(binop->get_line_pos(), binop->get_in_line_pos())
                      << "Invalind types of operands in binary operation\n"
                      << "Left operand have type: " << first_op.eval_type << "\n"
                      << "Right operand have type: " << second_op.eval_type << std::endl;
            throw std::runtime_error("Semantic error");
        }

        expr_stack_.push({expr_type, nullptr, value_category::RVALUE});
    }

    void pre_visit(variable_dec *var) override {
        std::string var_type = var->get_var_type();
        if (var_type != "char" &&
            var_type != "bool" &&
            var_type != "int"  &&
            global_table_.find(var_type) == nullptr) 
        {
            std::cerr << line_err_msg(var->get_line_pos(), var->get_in_line_pos())
                      << "Undefined type: " << var_type << std::endl;
            throw std::runtime_error("Semantic error");
        }
    }

    void post_visit(let_statement *let_stmt) override {
        expr_info rhs = expr_stack_.top();
        expr_stack_.pop();
        expr_info lhs = expr_stack_.top();
        expr_stack_.pop();
        if (lhs.val_cat != value_category::LVALUE) {
            auto lhs_node = let_stmt->child(0);
            print_err_msg("Lvalue required as left operand of let-statement",
                          lhs_node->get_line_pos(),
                          lhs_node->get_in_line_pos());
            throw std::runtime_error("Semantic error");
        }
        if (rhs.val_cat == value_category::NOT_VALUE) {
            auto rhs_node = let_stmt->child(1);
            print_err_msg("Right operand of let-statement must be value",
                          rhs_node->get_line_pos(),
                          rhs_node->get_in_line_pos());
            throw std::runtime_error("Semantic error");
        }
        if (rhs.eval_type != lhs.eval_type) {
            std::cerr << current_func_name_err_msg()
                      << line_err_msg(let_stmt->get_line_pos(), let_stmt->get_in_line_pos())
                      << "Operands type mismatch in let-statement\n"
                      << "Left operand have type: " << lhs.eval_type << '\n'
                      << "Right operand have type: " << rhs.eval_type << std::endl;
            throw std::runtime_error("Semantic error");
        }
    }

    void post_visit(if_statement *if_stmt) override {
        expr_info cond_expr = expr_stack_.top();
        expr_stack_.pop();
        if (cond_expr.eval_type != "bool") {
            auto condition_node = if_stmt->child(0);
            print_err_msg("Condition expression in if-statement must be bool type",
                          condition_node->get_line_pos(),
                          condition_node->get_in_line_pos());
            throw std::runtime_error("Semantic error");
        }
    }

    void post_visit(while_statement *while_stmt) override {
        expr_info cond_expr = expr_stack_.top();
        expr_stack_.pop();
        if (cond_expr.eval_type != "bool") {
            auto condition_node = while_stmt->child(0);
            print_err_msg("Condition expression in while-statement must be bool type",
                          condition_node->get_line_pos(),
                          condition_node->get_in_line_pos());
            throw std::runtime_error("Semantic error");
        }
    }

    void post_visit(do_statement *do_stmt) override {
        expr_stack_.pop();
        auto sub_call_node = do_stmt->child(0);
        if (sub_call_node->get_type() != node_type::CALL_EXPR) {
            print_err_msg("Do statement must contain call expression",
                          do_stmt->get_line_pos(),
                          do_stmt->get_in_line_pos());
            throw std::runtime_error("Semantic error");
        }
    }

    void post_visit(return_statement *ret_stmt) override {
        std::string subrtn_ret_type = current_subrtn_->get_ret_type();
        if (subrtn_ret_type == "void" && expr_stack_.empty()) return;

        if (subrtn_ret_type == "void" && !expr_stack_.empty()) {
            print_err_msg("Returning value in void function",
                          ret_stmt->get_line_pos(),
                          ret_stmt->get_in_line_pos());
            throw std::runtime_error("Semantic error");
        }
        if (subrtn_ret_type != "void" && expr_stack_.empty()) {
            print_err_msg("No return value in non-void function",
                          ret_stmt->get_line_pos(),
                          ret_stmt->get_in_line_pos());
            throw std::runtime_error("Semantic error");
        }

        expr_info ret_expr = expr_stack_.top();
        expr_stack_.pop();
        if (ret_expr.eval_type != subrtn_ret_type) {
            std::cerr << current_func_name_err_msg()
                      << line_err_msg(ret_stmt->get_line_pos(), ret_stmt->get_in_line_pos())
                      << "Return statement with " << ret_expr.eval_type << " expression, "
                      << "in function returning " << subrtn_ret_type << std::endl;
            throw std::runtime_error("Semantic error");
        }
    }

    std::string symbol_to_eval_type(std::shared_ptr<symbol_node> sym) {
        std::string eval_type;
        if (sym->get_symbol_type() == symbol_type::VARIABLE_SYM) {
            eval_type = std::static_pointer_cast<var_symbol>(sym)->get_type();
        }
        return eval_type;
    }

    void print_err_msg(const char *msg, unsigned line_pos, unsigned in_line_pos) {
        std::cerr << current_func_name_err_msg()
                  << line_err_msg(line_pos, in_line_pos)
                  << msg << std::endl;
    }

    std::string current_func_name_err_msg() {
        return "In function " + current_subrtn_->get_name() + "\n";
    }

    std::string line_err_msg(unsigned line_pos, unsigned in_line_pos) {
        std::stringstream msg;
        msg << "On line " << line_pos << ":" << in_line_pos << '\n';
        return msg.str();
    }

private:
    std::stack<expr_info> expr_stack_;
    symbol_table global_table_;
    symbol_table local_table_;
    std::shared_ptr<class_symbol> current_class_;
    std::shared_ptr<subroutine_symbol> current_subrtn_;
    unsigned arg_index_ = 0;
    unsigned local_index_ = 0;
};