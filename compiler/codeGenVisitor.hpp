#include <sstream>
#include <queue>
#include "ast.hpp"

namespace compiler {

class codeGenVisitor : public visitor {
    enum value_type {
        POINTER,
        VALUE,
        FUNCTION,
    };
public:
    codeGenVisitor(symbol_table table) : global_table_(table) {}

private:
    bool condition(std::shared_ptr<node> node) override {
        if (node->get_type() == SUBRTN_BODY) {
            return false;
        }
        return true;
    }

    visit_order get_order() override { return PRE_POST_ORDER; }
    
    void visit(std::shared_ptr<class_dec> cl) override {
        if (get_state() == PRE_ORDER) {
            auto sym = global_table_.find(cl->get_name());
            current_class_ = *std::static_pointer_cast<class_symbol>(sym);
        }
    }
    
    void visit(std::shared_ptr<subrtn_dec> subrtn) {
        if (get_state() == PRE_ORDER) {
            local_table_.clear();
            local_index_ = 0;
            arg_index_ = 0;
            auto sym = current_class_.find_member(subrtn->get_name());
            if (sym == nullptr) {
                //error
            }
            subroutine_symbol sub_sym = *std::static_pointer_cast<subroutine_symbol>(sym);
            auto locals = std::static_pointer_cast<variable_dec_list>(subrtn->child(1)->child(0));
            int local_count = locals->children();
            std::cout << "function" 
                    << current_class_.get_name() 
                    << '.' 
                    << subrtn->get_name()
                    << ' '
                    << local_count
                    << '\n';
            if (sub_sym.get_kind() == CONSTRUCTOR_S) {
                std::string type = current_class_.get_name();
                auto sym = std::make_shared<var_symbol>(type, LOCAL_V, 0);
                local_index_++;
                local_table_.insert("this", sym);
                int field_count = current_class_.get_field_count();
                std::cout << "push constant " << field_count << '\n';
                std::cout << "call Memory.alloc 1" << std::endl;
                std::cout << "pop local 0"    << std::endl;
            }
            if (sub_sym.get_kind() == METHOD_S) {
                std::string type = current_class_.get_name();
                auto sym = std::make_shared<var_symbol>(type, LOCAL_V, 0);
                arg_index_++;
                local_table_.insert("this", sym);
            }
        }
    }

    void visit_var(std::shared_ptr<variable_dec> var) {
        if (get_order() == PRE_ORDER) {
            var_modifier mod = var->get_var_kind();
            if (mod == STATIC_V || mod == FIELD_V) {
                return;
            }
            std::string name = var->get_var_name();
            std::string type = var->get_var_type();
            int index = 0;
            if (var->get_var_kind() == LOCAL_V) {
                index = local_index_;
                local_index_++;
            } else {
                index = arg_index_;
                arg_index_++;
            }
            auto sym = std::make_shared<var_symbol>(type, mod, index);
            local_table_.insert(name, sym);
        }
    }

    void visit_literal(std::shared_ptr<literal_expr> literal) {
        literal_q_.push(literal->get_literal().value);
    }

    void visit_array(std::shared_ptr<array_member_expr> array) {
        if (literal_q_.size() == 1) {
            output_ << "";
        }
    }

    void visit_member(std::shared_ptr<member_expr> member) {

    }

    void visit_call(std::shared_ptr<subrtn_call_expr> call) {

    }

    void visit_term(std::shared_ptr<expression> term) {
        switch (term->get_type()) {
        case LITERAL_EXPR:
            visit_literal(std::static_pointer_cast<literal_expr>(term));
            break;
        case ARRAY_MEMBER_EXPR:
            visit_array(std::static_pointer_cast<array_member_expr>(term));
            break;
        case MEMBER_EXPR:
            visit_member(std::static_pointer_cast<member_expr>(term));
            break;
        case CALL_EXPR:
            visit_call(std::static_pointer_cast<subrtn_call_expr>(term));
            break;
        }
    }

    void compile_expr(std::shared_ptr<expression> expr) {
        switch (expr->get_type())
        {
        case LITERAL_EXPR:
        case ARRAY_MEMBER_EXPR:
        case MEMBER_EXPR:
        case CALL_EXPR:
            visit_term(expr);
            break;
        default:
            break;
        }
    }


    void visit_let(std::shared_ptr<let_statement> stmt) {
        std::shared_ptr<expression> left_expr = 
            std::static_pointer_cast<expression>(stmt->child(0));
        std::shared_ptr<expression> right_expr = 
            std::static_pointer_cast<expression>(stmt->child(1));

        compile_expr(left_expr);
    }

    void visit_stmt(std::shared_ptr<statement> stmt) {
        switch (stmt->get_type())
        {
        case LET_STATEMENT:
            visit_let(std::static_pointer_cast<let_statement>(stmt));
            break;
        case IF_STATEMENT:
            break;
        case WHILE_STATEMENT:
            break;

        case DO_STATEMENT:
            break;

        case RETURN_STATEMENT:

            break;
        default:
            break;
        }
    }

    void visit(std::shared_ptr<subrtn_dec> sub_dec) {
        std::shared_ptr<node> sub_body = sub_dec->child(1);
        std::shared_ptr<node> body_var = sub_body->child(0);
        std::shared_ptr<node> body_stmts = sub_body->child(1);

        for (int i = 0; i < body_var->children(); i++) {
            visit_var(std::static_pointer_cast<variable_dec>(body_var->child(i)));
        }
        for (int i = 0; i < body_stmts->children(); i++) {
            visit_stmt(std::static_pointer_cast<statement>(body_stmts->child(i)));
        }
    }


    class_symbol current_class_;
    symbol_table global_table_;
    symbol_table local_table_;
    std::stringstream output_;
    std::queue<std::string> literal_q_;
    int arg_index_ = 0;
    int local_index_ = 0;
};
}