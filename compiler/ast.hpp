#pragma once
#include <vector>
#include <memory>
#include <string>
#include <iostream>
#include "visitor.hpp"
#include "node_descriptors.hpp"

/*
-------------------------------
Jack program structure
-------------------------------
*/

class node {
public:
    node() = default;
    node(std::shared_ptr<node> parent_node,
         unsigned int in_line_pos,
         unsigned int line_pos
        )
        : parent_node_(parent_node),
          in_line_pos_(in_line_pos),
          line_pos_(line_pos)
        {}

    virtual ~node() = default;

    virtual unsigned int children() = 0;

    virtual std::shared_ptr<node> child(unsigned int i) = 0;
    
    virtual node_type get_type() = 0;
    
    std::shared_ptr<node> get_parent() { return parent_node_; }
    
    void set_parent(std::shared_ptr<node> parent_node) {
        parent_node_ = parent_node;
    }

    void set_line_pos(unsigned int line_pos) { line_pos_ = line_pos; }

    void set_in_line_pos(unsigned int in_line_pos) { in_line_pos_ = in_line_pos; }

    unsigned int get_in_line_pos() { return in_line_pos_; }

    unsigned int get_line_pos() { return line_pos_; } 

    virtual void accept(visitor &) = 0;
    
private:
    std::shared_ptr<node> parent_node_;
    unsigned int line_pos_ = 0;
    unsigned int in_line_pos_ = 0;
};

class statement : public node {};

class expression : public node {};


/*
Variable class represents variable name and type 
*/
class variable_dec : public node {
public:
    variable_dec() = default;

    variable_dec(std::string name,
                 std::string type,
                 var_modifier mod
                )
                : name_(name),
                  type_(type),
                  kind_(mod) 
                {}

    void set_name(std::string &name) { name_ = name; }

    void set_type(std::string &type) { type_ = type; }

    void set_type(var_modifier mod) { kind_ = mod; }

    std::string get_var_type() { return type_; }

    std::string get_var_name() { return name_; }

    var_modifier get_var_kind() { return kind_; }

    node_type get_type() override { return node_type::VAR_DEC; }

    unsigned int children() override { return 0; }

    std::shared_ptr<node> child(unsigned int i) override { return nullptr; }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        vstr.post_visit(this);
    }
    
private:
    std::string name_;
    std::string type_;
    var_modifier kind_;
};


class variable_dec_list : public node {
public:
    variable_dec_list() = default;

    void add_var(std::shared_ptr<variable_dec> var_dec) {
        vars_.push_back(var_dec);
    }

    node_type get_type() override { return node_type::VAR_DEC_LIST; }

    unsigned int children() override { return vars_.size(); }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i >= vars_.size()) return nullptr;
        return vars_[i];
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        for (auto&& var : vars_) {
            var->accept(vstr);
        }
        vstr.post_visit(this);
    }
    
private:
    std::vector<std::shared_ptr<variable_dec>> vars_;
};

/*
-------------------------------
Statements
-------------------------------
*/

class statement_list : public node {
public:
    statement_list() = default;

    void add_statement(std::shared_ptr<statement> stmt) {
        statement_list_.push_back(stmt);
    }

    node_type get_type() override { return node_type::STATEMENT_LIST; }

    unsigned int children() override { return statement_list_.size(); }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i >= statement_list_.size()) return nullptr;
        return statement_list_[i];
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        for (auto&& stmt : statement_list_) {
            stmt->accept(vstr);
        }
        vstr.post_visit(this);
    }

private:
    std::vector<std::shared_ptr<statement>> statement_list_;
};

class let_statement : public statement {
public:
    let_statement() = default;

    void add_lhs(std::shared_ptr<expression> lhs) {
        lhs_expr_ = lhs;
    }

    void add_rhs(std::shared_ptr<expression> rhs) {
        rhs_expr_ = rhs;
    }
    
    node_type get_type() override { return node_type::LET_STATEMENT; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return lhs_expr_;
        if (i == 1) return rhs_expr_;
        return nullptr;
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        lhs_expr_->accept(vstr);
        rhs_expr_->accept(vstr);
        vstr.post_visit(this);
    }

private:
    std::shared_ptr<expression> lhs_expr_;
    std::shared_ptr<expression> rhs_expr_;
};

class if_statement : public statement {
public:
    if_statement() = default;

    void add_condition(std::shared_ptr<expression> condition) {
        condition_ = condition;    
    }
    
    void add_if_body(std::shared_ptr<statement_list> if_body) {
        if_body_ = if_body;
    }

    void add_else_body(std::shared_ptr<statement_list> else_body) {
        else_body_ = else_body;
    }

    node_type get_type() override { return node_type::IF_STATEMENT; }
    
    unsigned int children() override { return 3; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return condition_;
        if (i == 1) return if_body_;
        if (i == 2) return else_body_;
        return nullptr;
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        condition_->accept(vstr);
        if_body_->accept(vstr);
        if (else_body_) {
            else_body_->accept(vstr);
        }
        vstr.post_visit(this);
    }

private:
    std::shared_ptr<expression> condition_;
    std::shared_ptr<statement_list> if_body_;
    std::shared_ptr<statement_list> else_body_;
};

class while_statement : public statement {
public:
    while_statement() = default;
    
    void add_condition(std::shared_ptr<expression> condition) {
        condition_ = condition;
    }

    void add_body(std::shared_ptr<statement_list> body) {
        body_ = body;
    }

    node_type get_type() override { return node_type::WHILE_STATEMENT; }

    unsigned int children() override { return 3; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return condition_;
        if (i == 1) return body_;
        return nullptr;
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        condition_->accept(vstr);
        body_->accept(vstr);
        vstr.post_visit(this);
    }

private:
    std::shared_ptr<expression> condition_;
    std::shared_ptr<statement_list> body_;
};

class do_statement : public statement {
public:
    do_statement() = default;

    void add_call_expr(std::shared_ptr<expression> call_expr) {
        call_expr_ = call_expr;
    }

    node_type get_type() override { return node_type::DO_STATEMENT; }

    unsigned int children() override { return 1; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return call_expr_;
        return nullptr;
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        call_expr_->accept(vstr);
        vstr.post_visit(this);
    }

private:
    std::shared_ptr<expression> call_expr_;
};

class return_statement : public statement {
public:
    return_statement() = default;

    void add_ret_expr(std::shared_ptr<expression> ret_expr) {
        ret_expr_ = ret_expr;
    }

    node_type get_type() override { return node_type::RETURN_STATEMENT; }

    unsigned int children() override {
        if (ret_expr_ != nullptr) return 1;
        return 0;
    }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return ret_expr_;
        return nullptr;
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        if (ret_expr_) {
            ret_expr_->accept(vstr);
        }
        vstr.post_visit(this);
    }

private:
    std::shared_ptr<expression> ret_expr_;
};

/*
-------------------------------
Expressions
-------------------------------
*/

class expression_list : public node {
public:
    expression_list() = default;

    void add_expression(std::shared_ptr<expression> expr) {
        expression_list_.push_back(expr);
    }

    node_type get_type() override { return node_type::EXPRESSION_LIST; }

    unsigned int children() override { return expression_list_.size(); }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i >= expression_list_.size()) return nullptr;
        return expression_list_[i];
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        for (auto&& expr : expression_list_) {
            expr->accept(vstr);
        }
        vstr.post_visit(this);
    }

private:
    std::vector<std::shared_ptr<expression>> expression_list_;
};

class binop_expr : public expression {
public:
    binop_expr() = default;

    binop_expr(std::shared_ptr<expression> l_exp,
               std::shared_ptr<expression> r_exp,
               op_type b_type
              )
              : left_expr_(l_exp)
              , right_expr_(r_exp)
              , op_type_(b_type) {}

    void add_left_expr(std::shared_ptr<expression> left_expr) {
        left_expr_ = left_expr;
    }

    void add_right_expr(std::shared_ptr<expression> right_expr) {
        right_expr_ = right_expr;
    }

    void set_op_type(op_type op_type) {
        op_type_ = op_type;
    }

    op_type get_op_type() { return op_type_; }

    node_type get_type() override { return node_type::BINOP_EXPR; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return left_expr_;
        if (i == 1) return right_expr_;
        return nullptr;
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        left_expr_->accept(vstr);
        right_expr_->accept(vstr);
        vstr.post_visit(this);
    }

private:
    std::shared_ptr<expression> left_expr_;
    std::shared_ptr<expression> right_expr_;
    op_type op_type_;
};

class unop_expr : public expression {
public:
    unop_expr() = default;

    unop_expr(std::shared_ptr<expression> exp, 
              op_type u_type
             )
             : expr_(exp), 
               op_type_(u_type) {}

    void add_unop_expr(std::shared_ptr<expression> expr) {
        expr_ = expr;
    }

    void set_unop_type(op_type type) { op_type_ = type; } 

    op_type get_op_type() { return op_type_; }

    node_type get_type() override { return node_type::UNOP_EXPR; }

    unsigned int children() override { return 1; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return expr_;
        return nullptr;
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        expr_->accept(vstr);
        vstr.post_visit(this);
    }
    
private:
    std::shared_ptr<expression> expr_;
    op_type op_type_;
};

class literal_expr : public expression {
public:
    literal_expr() = default;
    literal_expr(std::string value, literal_type lit_kind)
                : value_(value), lit_kind_(lit_kind) {}

    void set_literal_type(literal_type lit_kind) { lit_kind_ = lit_kind; }

    void set_value(std::string value) { value_ = value; }

    literal_type get_literal_type() { return lit_kind_; }

    std::string get_value() { return value_; }

    node_type get_type() override { return node_type::LITERAL_EXPR; }

    unsigned int children() override { return 0; }

    std::shared_ptr<node> child(unsigned int i) override { return nullptr; }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        vstr.post_visit(this);
    }
    
private:
    std::string value_;
    literal_type lit_kind_;
};

class name_expr : public expression {
public:
    name_expr() = default;
    name_expr(std::string name) : name_(name) {}

    std::string get_name() { return name_; }

    void set_name(std::string name) { name_ = name; }

    node_type get_type() override { return node_type::NAME_EXPR; }

    unsigned int children() override { return 0; }

    std::shared_ptr<node> child(unsigned int i) override { return nullptr; }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        vstr.post_visit(this);
    }

private:
    std::string name_; 
};

class array_member_expr : public expression {
public:
    array_member_expr() = default;

    array_member_expr(std::shared_ptr<expression> id,
                      std::shared_ptr<expression> member
                     )
                     : identifier_(id)
                     , index_(member) {}

    void add_identtifier_expr(std::shared_ptr<expression> identifier) {
        identifier_ = identifier;
    }

    void add_index_expr(std::shared_ptr<expression> index) {
        index_ = index;
    }

    node_type get_type() override { return node_type::ARRAY_MEMBER_EXPR; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return identifier_;
        if (i == 1) return index_;
        return nullptr;
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        identifier_->accept(vstr);
        index_->accept(vstr);
        vstr.post_visit(this);
    }
    
private:
    std::shared_ptr<expression> identifier_;
    std::shared_ptr<expression> index_;
};

class subroutine_call_expr : public expression {
public:
    subroutine_call_expr() = default;
    
    subroutine_call_expr(std::shared_ptr<expression> id,
                         std::shared_ptr<expression_list> arguments
                        )
                        : identifier_(id)
                        , args_(arguments) {}

    void add_identifier(std::shared_ptr<expression> identifier) {
        identifier_ = identifier;
    }

    void add_args(std::shared_ptr<expression_list> args) {
        args_ = args;
    }

    unsigned get_arg_count() { return args_->children(); }

    node_type get_type() override { return node_type::CALL_EXPR; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return identifier_;
        if (i == 1) return args_;
        return nullptr;
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        identifier_->accept(vstr);
        args_->accept(vstr);
        vstr.post_visit(this);
    }
    
private:
    std::shared_ptr<expression> identifier_;
    std::shared_ptr<expression_list> args_;
};

class member_expr : public expression {
public:
    member_expr() = default;

    member_expr(std::shared_ptr<expression> id,
                std::string member
               ) 
               : identifier_(id)
               , member_(member) {}

    void add_identifier(std::shared_ptr<expression> identifier) {
        identifier_ = identifier;
    }

    void add_member(std::string member) {
        member_ = member;
    }

    std::string get_member() { return member_; }

    node_type get_type() override { return node_type::MEMBER_EXPR; }

    unsigned int children() override { return 1; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return identifier_;
        return nullptr;
    }
    
    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        identifier_->accept(vstr);
        vstr.post_visit(this);
    }

private:
    std::shared_ptr<expression> identifier_;
    std::string member_;
};


/*
Subroutine body node handles variables declaration and all statements in body
*/
class subroutine_body : public node {
public:
    subroutine_body() = default;

    void set_var_list(std::shared_ptr<variable_dec_list> vars) {
        var_list_ = vars;
    }

    void set_statement_list(std::shared_ptr<statement_list> stmts) {
        stmt_list_ = stmts;
    }

    node_type get_type() override { return node_type::SUBRTN_BODY; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return var_list_;
        if (i == 1) return stmt_list_;
        return nullptr;
    }
    
    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        var_list_->accept(vstr);
        stmt_list_->accept(vstr);
        vstr.post_visit(this);
    }

private:
    std::shared_ptr<variable_dec_list> var_list_;
    std::shared_ptr<statement_list> stmt_list_;
};

/*
Subroutine declaration node handles name and body of method/constructor/function
*/
class subroutine_dec : public node {
public:
    subroutine_dec() = default;

    void set_name(std::string name) { name_ = name; }

    void set_ret_type(std::string ret_type) { ret_type_ = ret_type; }

    void add_arg_list(std::shared_ptr<variable_dec_list> args) { 
        args_ = args;
    }

    void add_body(std::shared_ptr<subroutine_body> body) {
        body_ = body;
    }

    void set_subroutine_kind(subroutine_kind s_type) {
        subrtn_type_ = s_type;
    }

    std::string get_name() { return name_; }

    std::string get_ret_type() { return ret_type_; }

    subroutine_kind get_subroutine_kind() { return subrtn_type_; }

    node_type get_type() override { return node_type::SUBRTN_DEC; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return args_;
        if (i == 1) return body_;
        return nullptr;
    }
  
    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        args_->accept(vstr);
        body_->accept(vstr);
        vstr.post_visit(this);
    }

private:
    std::string name_;
    std::string ret_type_;
    std::shared_ptr<variable_dec_list> args_;
    std::shared_ptr<subroutine_body> body_;
    subroutine_kind subrtn_type_;
};


class subroutine_list : public node {
public:
    subroutine_list() = default;

    void add_subroutine(std::shared_ptr<subroutine_dec> subrtn) {
        subrtns_.push_back(subrtn);
    }

    node_type get_type() override { return node_type::SUBRTN_LIST; }

    unsigned int children() override { return subrtns_.size(); }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i >= subrtns_.size()) return nullptr;
        return subrtns_[i];
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        for (auto&& sub : subrtns_) {
            sub->accept(vstr);
        }
        vstr.post_visit(this);
    }
     
private:
    std::vector<std::shared_ptr<subroutine_dec>> subrtns_;
};

/*
Class declaration node which handles Jack class name and subroutines
*/
class class_dec : public node {
public:
    class_dec() = default;

    void add_var_list(std::shared_ptr<variable_dec_list> var_list) {
        vars_ = var_list;
    }

    void add_subroutine_list(std::shared_ptr<subroutine_list> subrtn_list) {
        subrtns_ = subrtn_list;
    }

    void set_name(std::string name) { class_name_ = name; }

    std::string get_name() { return class_name_; }

    node_type get_type() override { return node_type::CLASS_DEC; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return vars_;
        if (i == 1) return subrtns_;
        return nullptr;
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        vars_->accept(vstr);
        subrtns_->accept(vstr);
        vstr.post_visit(this);
    }

private:
    std::string class_name_;
    std::shared_ptr<variable_dec_list> vars_;
    std::shared_ptr<subroutine_list> subrtns_;
};

/*
Program node which handles all Jack classes in file
*/
class program : public node {
public:
    program() = default;

    void add_class(std::shared_ptr<class_dec> cl_dec) {
        classes_.push_back(cl_dec);
    }

    node_type get_type() override {return node_type::PROGRAM;};
    
    unsigned int children() override {return classes_.size();}
    
    std::shared_ptr<node> child(unsigned int i) override {
        if (i >= classes_.size()) {
            return nullptr;
        }
        return classes_[i];
    }

    void accept(visitor &vstr) override {
        vstr.pre_visit(this);
        for (auto&& cl : classes_) {
            cl->accept(vstr);
        }
        vstr.post_visit(this);
    }

private:
    std::vector<std::shared_ptr<class_dec>> classes_;
};