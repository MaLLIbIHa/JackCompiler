#include "symbol_table.hpp"

namespace compiler {

symbol_node::~symbol_node() = default;

// class_table::class_table() = default;

// class_table::class_table(std::string n) : name_(n) {}

subroutine_symbol::subroutine_symbol(subroutine_type s_type,
                                     std::string ret_t,
                                     std::vector<std::string> args_types
                                    ) :
                                      s_type_(s_type),
                                      ret_type_(ret_t),
                                      args_types_(args_types) {}

var_symbol::var_symbol(std::string v_type,
                       std::string v_scope,
                       var_modifier v_mod, 
                       int index
                       ) :
                         v_type_(v_type),
                         v_scope_(v_scope),
                         v_mod_(v_mod),
                         v_index_(index) {}
}