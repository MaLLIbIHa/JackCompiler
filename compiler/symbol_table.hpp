#pragma once
#include <unordered_map>
#include <string>
#include <memory>
#include <vector>

namespace compiler {

enum symbol_type {
    CLASS_SYM,
    VARIABLE_SYM,
    SUBROUTINE_SYM,
};

enum subroutine_type {
    CONSTRUCTOR_S,
    FUNCTION_S,
    METHOD_S,
};

enum var_modifier {
    ARG_V,
    LOCAL_V,
    STATIC_V,
    FIELD_V,
};

class symbol_node {
public:
    virtual symbol_type get_symbol_type() = 0;

    virtual ~symbol_node() = default;
};

class symbol_table {
public:
    std::shared_ptr<symbol_node> find(std::string name) {
        auto symbol = table_.find(name);
        if (symbol != table_.end()) {
            return symbol->second;
        }
        return nullptr;
    }

    void insert(std::string name, std::shared_ptr<symbol_node> symbol) {
        if (table_.find(name) != table_.end()) {
            //error
            //member already exist
        }
        table_[name] = symbol;
    }

    void clear() {
        table_.clear();
    }

private:
    std::unordered_map<std::string, std::shared_ptr<symbol_node>> table_;
};

class class_symbol : public symbol_node {
public:
    class_symbol() = default;
    class_symbol(std::string name, int field_count) 
                : name_(name), field_count_(field_count) {}

    symbol_type get_symbol_type() override { return CLASS_SYM; }

    void set_name(std::string name) { name_ = name; }

    std::string get_name() { return name_; }

    void set_field_count(int count) { field_count_ = count; }

    int get_field_count() { return field_count_; }

    std::shared_ptr<symbol_node> find_member(std::string name) {
        return sym_table_.find(name);
    }

    void add_symbol(std::string name, std::shared_ptr<symbol_node> symbol) {
        sym_table_.insert(name, symbol);
    }

    void clear() {
        sym_table_.clear();
    }

private:
    std::string name_;
    symbol_table sym_table_;
    int field_count_;
};

class subroutine_symbol : public symbol_node {
public:
    subroutine_symbol() = default;

    subroutine_symbol(subroutine_type s_type,
                      std::string ret_t,
                      std::vector<std::string> args
                     )
                     : s_type_(s_type),
                       ret_type_(ret_t),
                       args_types_(args)
                       {}

    symbol_type get_symbol_type() override { return SUBROUTINE_SYM; }

    void set_kind(subroutine_type kind) { s_type_ = kind; }

    subroutine_type get_kind() { return s_type_; }

    void set_ret_type(std::string ret_type) { ret_type_ = ret_type; }

    std::string get_ret_type() { return ret_type_; }

    void add_arg_type(std::string type) {
        args_types_.push_back(type);
    }

    std::string get_arg_type(int i) { return args_types_[i]; }

    int get_arg_count() { return args_types_.size(); }

private:
    subroutine_type s_type_;
    std::string ret_type_;
    std::vector<std::string> args_types_;
};

class var_symbol : public symbol_node {
public:
    var_symbol() = default;

    var_symbol(std::string v_type,
               var_modifier v_mod,
               int index
               )
               : v_type_(v_type),
                 v_mod_(v_mod),
                 v_index_(index) {}

    symbol_type get_symbol_type() override { return VARIABLE_SYM; }

    void set_type(std::string type) { v_type_ = type; }

    std::string get_type() { return v_type_; }

    void set_mode(var_modifier mod) { v_mod_ = mod; }

    var_modifier get_mode() { return v_mod_; }

    void set_index(int i) { v_index_ = i; }

    int get_index() { return v_index_; }

private:
    std::string v_type_;
    var_modifier v_mod_;
    int v_index_;
};
}