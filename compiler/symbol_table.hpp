#pragma once
#include <unordered_map>
#include <string>
#include <memory>
#include <vector>
#include "node_descriptors.hpp"

enum class symbol_type {
    CLASS_SYM,
    VARIABLE_SYM,
    SUBROUTINE_SYM,
};

class symbol_node {
public:
    symbol_node() = default;
    symbol_node(std::string name,
                unsigned line_pos,
                unsigned in_line_pos 
               ) 
               : name_(name),
                 line_pos_(line_pos),
                 in_line_pos_(in_line_pos) 
                 {}

    virtual symbol_type get_symbol_type() = 0;

    virtual ~symbol_node() = default;

    void set_name(std::string name) { name_ = name; }

    void set_line_pos(unsigned int line_pos) { line_pos_ = line_pos; }

    void set_in_line_pos(unsigned int in_line_pos) { in_line_pos_ = in_line_pos; }

    std::string get_name() { return name_; }

    unsigned int get_in_line_pos() { return in_line_pos_; }

    unsigned int get_line_pos() { return line_pos_; } 

protected:
    std::string name_;
    unsigned line_pos_ = 0;
    unsigned in_line_pos_ = 0;
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

    bool insert(std::string name, std::shared_ptr<symbol_node> symbol) {
        if (table_.find(name) != table_.end()) {
            return false;
        }
        table_[name] = symbol;
        return true;
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
    class_symbol(std::string name, 
                 int field_count,
                 unsigned line_pos,
                 unsigned in_line_pos)
                : symbol_node(name, line_pos, in_line_pos),
                  field_count_(field_count) {}

    symbol_type get_symbol_type() override { return symbol_type::CLASS_SYM; }

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
        name_.clear();
    }

private:
    symbol_table sym_table_;
    int field_count_ = 0;
};

class subroutine_symbol : public symbol_node {
public:
    subroutine_symbol() = default;

    subroutine_symbol(std::string name,
                      std::string ret_t,
                      std::vector<std::string> args,
                      subroutine_kind s_type,
                      unsigned line_pos,
                      unsigned in_line_pos
                     )
                     : symbol_node(name, line_pos, in_line_pos),
                       ret_type_(ret_t),
                       args_types_(args),
                       s_type_(s_type)
                       {}

    symbol_type get_symbol_type() override { return symbol_type::SUBROUTINE_SYM; }

    void set_kind(subroutine_kind kind) { s_type_ = kind; }

    subroutine_kind get_kind() { return s_type_; }

    void set_ret_type(std::string ret_type) { ret_type_ = ret_type; }

    std::string get_ret_type() { return ret_type_; }

    void add_arg_type(std::string type) {
        args_types_.push_back(type);
    }

    std::string get_arg_type(int i) { return args_types_[i]; }

    int get_arg_count() { return args_types_.size(); }

private:
    std::string ret_type_;
    std::vector<std::string> args_types_;
    subroutine_kind s_type_;
};

class var_symbol : public symbol_node {
public:
    var_symbol() = default;

    var_symbol(std::string name,
               std::string v_type,
               var_modifier v_mod,
               int index,
               unsigned line_pos,
               unsigned in_line_pos
               )
               : symbol_node(name, line_pos, in_line_pos),
                 v_type_(v_type),
                 v_mod_(v_mod),
                 v_index_(index) {}

    symbol_type get_symbol_type() override { return symbol_type::VARIABLE_SYM; }

    void set_type(std::string type) { v_type_ = type; }

    std::string get_type() { return v_type_; }

    void set_mode(var_modifier mod) { v_mod_ = mod; }

    var_modifier get_mode() { return v_mod_; }

    void set_index(int i) { v_index_ = i; }

    int get_index() { return v_index_; }

private:
    std::string v_type_;
    var_modifier v_mod_;
    int v_index_ = 0;
};