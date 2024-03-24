#pragma once
#include "ast.hpp"

namespace compiler {

class classVisitor : public visitor {
    bool condition(std::shared_ptr<node> node) override {
        if (node->get_type() == SUBRTN_BODY) {
            return false;
        } else {
            return true;
        }
    }

    visit_order get_order() override { return POST_ORDER; }

    void visit(std::shared_ptr<class_dec> cl) override {
        std::string name = cl->get_name();
        if (current_class_.find_member(name) != nullptr) {
            //error
        }
        current_class_.set_name(name);
        current_class_.set_field_count(field_index);
        std::shared_ptr<class_symbol> cl_sym = std::make_shared<class_symbol>(current_class_);
        global_sym_table_.insert(cl->get_name(), cl_sym);
        current_class_.clear();
        field_index = 0;
    }

    void visit(std::shared_ptr<variable_dec> var) override {
        var_modifier kind = var->get_var_kind();
        int index = 0;
        if (kind == FIELD_V) {
            index = field_index;
            field_index++;
        } else if (kind == STATIC_V) {
            index = static_index;
            static_index++;
        } else if (kind == ARG_V){
            arg_types_.push_back(var->get_var_type());
            return;
        } else {
            return;
        }
        std::string type = var->get_var_type();
        std::string name = var->get_var_name();
        
        // if (kind == FIELD_V) std::cout << "field ";
        // if (kind == STATIC_V) std::cout << "static ";
        // std::cout << "VARIABLE " << name << ' ' << index << std::endl; 
        
        std::shared_ptr<var_symbol> var_sym = 
            std::make_shared<var_symbol>(type, kind, index);
        current_class_.add_symbol(name, var_sym);
    }

    void visit(std::shared_ptr<subrtn_dec> subrtn) override {
        subroutine_type s_type = subrtn->get_subrtn_type();
        std::string name = subrtn->get_name();
        std::string ret_type = subrtn->get_ret_type();
        std::shared_ptr<subroutine_symbol> sym = 
            std::make_shared<subroutine_symbol>(s_type, ret_type, arg_types_);
        current_class_.add_symbol(name, sym);
        arg_types_.clear();
    }

    symbol_table get_global_table() { return global_sym_table_; }

    int get_static_count() { return static_index; }

    symbol_table global_sym_table_;
    class_symbol current_class_;
    std::vector<std::string> arg_types_;
    int static_index = 0;
    int field_index = 0;
};
}