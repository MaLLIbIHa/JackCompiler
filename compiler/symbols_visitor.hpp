#pragma once
#include <string>
#include "visitor.hpp"
#include "ast.hpp"
#include "symbol_table.hpp"

class symbols_visitor final : public visitor {
public:
    void pre_visit(class_dec *cl) override {
        current_class_.set_name(cl->get_name());
        current_class_.set_line_pos(cl->get_line_pos());
        current_class_.set_in_line_pos(cl->get_in_line_pos());
    }

    void pre_visit(subroutine_dec *sub_dec) override {
        std::shared_ptr<node> args_list = sub_dec->child(0);
        std::vector<std::string> arg_types;
        for (int i = 0; i < args_list->children(); i++) {
            auto arg = std::static_pointer_cast<variable_dec>(args_list->child(i));
            std::string type = arg->get_var_type();
            arg_types.push_back(type);
        }

        std::string sub_name = sub_dec->get_name();
        subroutine_symbol sub_sym(sub_name,
                                  sub_dec->get_ret_type(),
                                  arg_types,
                                  sub_dec->get_subroutine_kind(),
                                  sub_dec->get_line_pos(),
                                  sub_dec->get_in_line_pos());
        current_class_.add_symbol(sub_name, std::make_shared<subroutine_symbol>(sub_sym));
    }

    void pre_visit(variable_dec_list *var_list) override {
        if ( var_list->get_parent()->get_type() != node_type::CLASS_DEC ) return;
        
        for (int i = 0; i < var_list->children(); i++) {
            auto var = std::static_pointer_cast<variable_dec>(var_list->child(i));
            var_symbol var_sym(var->get_var_name(),
                               var->get_var_type(),
                               var->get_var_kind(),
                               current_index,
                               var->get_line_pos(),
                               var->get_in_line_pos());
            current_class_.add_symbol(var->get_var_name(), std::make_shared<var_symbol>(var_sym));
            current_index++;
        }
    }

    void post_visit(class_dec* cl) override {
        current_class_.set_field_count(current_index);
        global_table_.insert(current_class_.get_name(), std::make_shared<class_symbol>(current_class_));
        current_class_.clear();
        current_index = 0;
    }

    symbol_table get_global_table() {
        return global_table_;
    }

private:
    class_symbol current_class_;
    symbol_table global_table_;
    int current_index = 0;
};