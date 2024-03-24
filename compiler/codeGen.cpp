#include "ast.hpp"

namespace compiler {

class codeGen {
    codeGen(std::shared_ptr<node> program, symbol_table table) 
        : prog_(program), global_table_(table) {}

private:
    void visit(std::shared_ptr<node> node) {
        switch(node->get_type()) {
        case PROGRAM: 
            visit(std::static_pointer_cast<program>(node));
            break;
        case CLASS_DEC: 
            visit(std::static_pointer_cast<class_dec>(node));
            break;
        case VAR_DEC_LIST:
            visit(std::static_pointer_cast<variable_dec_list>(node));
            break;
        case SUBRTN_LIST:
            visit(std::static_pointer_cast<subroutine_list>(node));
            break;
        case SUBRTN_DEC:
            visit(std::static_pointer_cast<subrtn_dec>(node));
            break;
        case SUBRTN_BODY:
            visit(std::static_pointer_cast<subrtn_body>(node));
            break;
        case VAR_DEC:
            visit(std::static_pointer_cast<variable_dec>(node));
            break;
        case STATEMENT_LIST:
            visit(std::static_pointer_cast<statement_list>(node));
            break;
        case LET_STATEMENT:
            visit(std::static_pointer_cast<let_statement>(node));
            break;
        case IF_STATEMENT:
            visit(std::static_pointer_cast<if_statement>(node));
            break;
        case WHILE_STATEMENT:
            visit(std::static_pointer_cast<while_statement>(node));
            break;
        case DO_STATEMENT:
            visit(std::static_pointer_cast<do_statement>(node));
            break;
        case RETURN_STATEMENT:
            visit(std::static_pointer_cast<return_statement>(node));
            break;
        case EXPRESSION_LIST:
            visit(std::static_pointer_cast<expression_list>(node));
            break;
        case BINOP_EXPR:
            visit(std::static_pointer_cast<binop_expr>(node));
            break;
        case UNOP_EXPR:
            visit(std::static_pointer_cast<unop_expr>(node));
            break;
        case LITERAL_EXPR:
            visit(std::static_pointer_cast<literal_expr>(node));
            break;
        case ARRAY_MEMBER_EXPR:
            visit(std::static_pointer_cast<array_member_expr>(node));
            break;
        case MEMBER_EXPR:
            visit(std::static_pointer_cast<member_expr>(node));
            break;
        case CALL_EXPR:
            visit(std::static_pointer_cast<subrtn_call_expr>(node));
            break;
        case EXPRESSION:
            visit(std::static_pointer_cast<expression>(node));
            break;
        }
    }

    void program_visit(std::shared_ptr<program> prog) {
        for (int i = 0; i < prog->children(); i++) {
            
        }
    }


    void visit(std::shared_ptr<class_dec> cl) {
        auto sym = global_table_.find(cl->get_name());
        current_class_ = *std::static_pointer_cast<class_symbol>(sym);
    }
    
    void visit(std::shared_ptr<subrtn_dec> subrtn) {
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

    void visit(std::shared_ptr<variable_dec> var) {
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




    class_symbol current_class_;
    std::shared_ptr<node> prog_;
    symbol_table local_table_;
    symbol_table global_table_;
    int arg_index_ = 0;
    int local_index_;
};
}