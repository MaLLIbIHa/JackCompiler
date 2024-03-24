#pragma once
#include <vector>
#include <memory>
#include <string>
#include <iostream>
#include "token.hpp"
#include "symbol_table.hpp"

namespace compiler {

enum node_type {
    PROGRAM,
    CLASS_DEC,
    SUBRTN_LIST,
    SUBRTN_DEC,
    SUBRTN_BODY,
    VAR_DEC,
    VAR_DEC_LIST,
    STATEMENT_LIST,
    LET_STATEMENT,
    IF_STATEMENT,
    WHILE_STATEMENT,
    DO_STATEMENT,
    RETURN_STATEMENT,
    EXPRESSION,
    EXPRESSION_LIST,
    BINOP_EXPR,
    UNOP_EXPR,
    LITERAL_EXPR,
    ARRAY_MEMBER_EXPR,
    MEMBER_EXPR,
    CALL_EXPR,
};

enum op_type {
    ADD_OP,
    SUB_OP,
    MUL_OP,
    DIV_OP,
    LOG_AND_OP,
    LOG_OR_OP,
    BIT_AND_OP,
    BIT_OR_OP,
    LSS_OP,
    GTR_OP,
    EQL_OP,
    NEG_OP,
    TILDE_OP,
};

/*
-------------------------------
Jack program structure
-------------------------------
*/

class node {
public:
    node() = default;
    node(std::shared_ptr<node> parent_node) : parent_node_(parent_node) {}
    virtual ~node() = default;

    virtual unsigned int children() = 0;
    virtual std::shared_ptr<node> child(unsigned int i) = 0;
    virtual node_type get_type() = 0;
    virtual std::shared_ptr<node> get_parent() { return parent_node_; }
    void set_parent(std::shared_ptr<node> parent_node) { 
        parent_node_ = parent_node; 
    }
    
private:
    std::shared_ptr<node> parent_node_;
};

class statement : public node {};
class expression : public node {};

/*
Program node which handles all Jack classes in file
*/
class program : public node {
public:
    program() = default;

    void add_class(std::shared_ptr<node> cl_dec) {
        classes_.push_back(cl_dec);
    }

    node_type get_type() override {return PROGRAM;};
    
    unsigned int children() override {return classes_.size();}
    
    std::shared_ptr<node> child(unsigned int i) override {
        if (i >= classes_.size()) {
            return nullptr;
        }
        return classes_[i];
    }

private:
    std::vector<std::shared_ptr<node>> classes_;
};

/*
Class declaration node which handles Jack class name and subroutines
*/
class class_dec : public node {
public:
    class_dec() = default;

    void add_var_list(std::shared_ptr<node> var_list) {
        vars_ = var_list;
    }

    void add_subrtn_list(std::shared_ptr<node> subrtn_list) {
        subrtns_ = subrtn_list;
    }

    void set_name(std::string name) { class_name_ = name; }

    std::string get_name() { return class_name_; }

    node_type get_type() override { return CLASS_DEC; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return vars_;
        if (i == 1) return subrtns_;
        return nullptr;
    }

private:
    std::string class_name_;
    std::shared_ptr<node> vars_;
    std::shared_ptr<node> subrtns_;
};

class variable_dec_list : public node {
public:
    variable_dec_list() = default;

    void add_var(std::shared_ptr<node> var_dec) {
        vars.push_back(var_dec);
    }

    node_type get_type() override { return VAR_DEC_LIST; }

    unsigned int children() override {return vars.size();}

    std::shared_ptr<node> child(unsigned int i) override {
        if (i >= vars.size()) return nullptr;
        return vars[i];
    }
    
private:
    std::vector<std::shared_ptr<node>> vars;
};

class subroutine_list : public node {
public:
    subroutine_list() = default;

    void add_subrtn(std::shared_ptr<node> subrtn) {
        subrtns.push_back(subrtn);
    }

    node_type get_type() override { return SUBRTN_LIST; }

    unsigned int children() override { return subrtns.size(); }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i >= subrtns.size()) return nullptr;
        return subrtns[i];
    }
     
private:
    std::vector<std::shared_ptr<node>> subrtns; 
};

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
                :name_(name),
                 type_(type),
                 kind_(mod) 
                 {}

    void set_name(std::string &name) { name_ = name; }

    void set_type(std::string &type) { type_ = type; }

    void set_type(var_modifier mod) { kind_ = mod; }

    std::string get_var_type() { return type_; }

    std::string get_var_name() { return name_; }

    var_modifier get_var_kind() { return kind_; }

    node_type get_type() override { return VAR_DEC; }

    unsigned int children() override { return 0; }

    std::shared_ptr<node> child(unsigned int i) override { return nullptr; }
    
private:
    std::string name_;
    std::string type_;
    var_modifier kind_;
};

/*
Subroutine body node handles variables declaration and all statements in body
*/
class subrtn_body : public node {
public:
    subrtn_body() = default;

    void set_var_list(std::shared_ptr<node> vars) {
        var_list = vars;
    }

    void set_statement_list(std::shared_ptr<node> stmts) {
        stmt_list = stmts;
    }

    node_type get_type() override { return SUBRTN_BODY; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return var_list;
        if (i == 1) return stmt_list;
        return nullptr;
    }
    
private:
    std::shared_ptr<node> var_list;
    std::shared_ptr<node> stmt_list;
};

/*
Subroutine declaration node handles name and body of method/constructor/function
*/
class subrtn_dec : public node {
public:
    subrtn_dec() = default;

    void set_name(std::string name) { name_ = name; }

    void set_ret_type(std::string ret_type) { ret_type_ = ret_type; }

    void add_arg_list(std::shared_ptr<node> args) { 
        args_ = args;
    }

    void add_body(std::shared_ptr<node> body) {
        body_ = body;
    }

    void set_subrtn_type(subroutine_type s_type) {
        subrtn_type_ = s_type;
    }

    std::string get_name() { return name_; }

    std::string get_ret_type() { return ret_type_; }

    subroutine_type get_subrtn_type() { return subrtn_type_; }

    node_type get_type() override { return SUBRTN_DEC; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return args_;
        if (i == 1) return body_;
        return nullptr;
    }
  
private:
    std::string name_;
    std::string ret_type_;
    std::shared_ptr<node> args_;
    std::shared_ptr<node> body_;
    subroutine_type subrtn_type_;
};


/*
-------------------------------
Statements
-------------------------------
*/

class statement_list : public node {
public:
    statement_list() = default;

    void add_statement(std::shared_ptr<node> stmt) {
        statement_list_.push_back(stmt);
    }

    node_type get_type() override { return STATEMENT_LIST; }

    unsigned int children() override { return statement_list_.size(); }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i >= statement_list_.size()) return nullptr;
        return statement_list_[i];
    }

private:
    std::vector<std::shared_ptr<node>> statement_list_;
};

/*
Statement base class, all statements inherits from this class
*/
class let_statement : public statement {
public:
    let_statement() = default;

    void add_lhs(std::shared_ptr<node> lhs) {
        lhs_expr = lhs;
    }

    void add_rhs(std::shared_ptr<node> rhs) {
        rhs_expr = rhs;
    }
    
    node_type get_type() override { return LET_STATEMENT; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return lhs_expr;
        if (i == 1) return rhs_expr;
        return nullptr;
    }

private:
    std::shared_ptr<node> lhs_expr;
    std::shared_ptr<node> rhs_expr;
};

class if_statement : public statement {
public:
    if_statement() = default;

    void add_condition(std::shared_ptr<node> condition) {
        condition_ = condition;    
    }
    
    void add_if_body(std::shared_ptr<node> if_body) {
        if_body_ = if_body;
    }

    void add_else_body(std::shared_ptr<node> else_body) {
        else_body_ = else_body;
    }

    node_type get_type() override { return IF_STATEMENT; }
    
    unsigned int children() override { return 3; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return condition_;
        if (i == 1) return if_body_;
        if (i == 2) return else_body_;
        return nullptr;
    }

private:
    std::shared_ptr<node> condition_;
    std::shared_ptr<node> if_body_;
    std::shared_ptr<node> else_body_;
};

class while_statement : public statement {
public:
    while_statement() = default;
    
    void add_condition(std::shared_ptr<node> condition) {
        if (condition) {
            condition_ = condition;
        }
    }

    void add_body(std::shared_ptr<node> body) {
        if (body) {
            body_ = body;
        }
    }

    node_type get_type() override { return WHILE_STATEMENT; }

    unsigned int children() override { return 3; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return condition_;
        if (i == 1) return body_;
        return nullptr;
    }

private:
    std::shared_ptr<node> condition_;
    std::shared_ptr<node> body_;
};

class do_statement : public statement {
public:
    do_statement() = default;

    void add_subrtn(std::shared_ptr<node> call_expr) {
        call_expr_ = call_expr;
    }

    node_type get_type() override { return DO_STATEMENT; }

    unsigned int children() override { return 1; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return call_expr_;
        return nullptr;
    }

private:
    std::shared_ptr<node> call_expr_;
};

class return_statement : public statement {
public:
    return_statement() = default;

    void add_subrtn(std::shared_ptr<node> ret_expr) {
        ret_expr_ = ret_expr;
    }

    node_type get_type() override { return RETURN_STATEMENT; }

    unsigned int children() override {
        if (ret_expr_ != nullptr) return 1;
        return 0;
    }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return ret_expr_;
        return nullptr;
    }

private:
    std::shared_ptr<node> ret_expr_;
};


/*
-------------------------------
Expressions
-------------------------------
*/

class expression_list : public node {
public:
    expression_list() = default;

    void add_expression(std::shared_ptr<node> expr) {
        expression_list_.push_back(expr);
    }

    node_type get_type() override { return EXPRESSION_LIST; }

    unsigned int children() override { return expression_list_.size(); }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i >= expression_list_.size()) return nullptr;
        return expression_list_[i];
    }

private:
    std::vector<std::shared_ptr<node>> expression_list_;
};

class binop_expr : public expression {
public:
    binop_expr() = default;

    binop_expr(std::shared_ptr<node> l_exp,
               std::shared_ptr<node> r_exp,
               op_type b_type
              )
              : left_expr_(l_exp)
              , right_expr_(r_exp)
              , op_type_(b_type) {}

    void add_left_expr(std::shared_ptr<node> left_expr) {
        if (left_expr) {
            left_expr_ = left_expr;
        }
    }

    void add_right_expr(std::shared_ptr<node> right_expr) {
        if (right_expr) {
            right_expr_ = right_expr;
        }
    }

    void set_op_type(op_type op_type) {
        op_type_ = op_type;
    }

    op_type get_op_type() { return op_type_; }

    node_type get_type() override { return BINOP_EXPR; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return left_expr_;
        if (i == 1) return right_expr_;
        return nullptr;
    }

private:
    std::shared_ptr<node> left_expr_;
    std::shared_ptr<node> right_expr_;
    op_type op_type_;
};

class unop_expr : public expression {
public:
    unop_expr() = default;

    unop_expr(std::shared_ptr<node> exp, 
              op_type u_type
             )
             : expr_(exp), 
               op_type_(u_type) {}

    void add_unop_expr(std::shared_ptr<node> expr) {
        expr_ = expr;
    }

    void set_unop_type(op_type type) { op_type_ = type; } 

    op_type get_op_type() { return op_type_; }

    node_type get_type() override { return UNOP_EXPR; }

    unsigned int children() override { return 1; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return expr_;
        return nullptr;
    }
    
private:
    std::shared_ptr<node> expr_;
    op_type op_type_;
};

class literal_expr : public expression {
public:
    literal_expr() = default;
    literal_expr(token lit) : literal_(lit) {};

    void set_literal(token tok) {
        literal_ = tok;
    }

    token get_literal() { return literal_; }

    node_type get_type() override { return LITERAL_EXPR; }

    unsigned int children() override { return 0; }

    std::shared_ptr<node> child(unsigned int i) override { return nullptr; }

    
private:
    token literal_;
};

class array_member_expr : public expression {
public:
    array_member_expr() = default;

    array_member_expr(std::shared_ptr<node> id,
                      std::shared_ptr<node> member
                     )
                     : identifier_(id)
                     , index_(member) {}

    void add_identtifier_expr(std::shared_ptr<node> identifier) {
        if (identifier) {
            identifier_ = identifier;
        }
    }

    void add_index_expr(std::shared_ptr<node> index) {
        if (index) {
            index_ = index;
        }
    }

    node_type get_type() { return ARRAY_MEMBER_EXPR; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return identifier_;
        if (i == 1) return index_;
        return nullptr;
    }
    
private:
    std::shared_ptr<node> identifier_;
    std::shared_ptr<node> index_;
};

class subrtn_call_expr : public expression {
public:
    subrtn_call_expr() = default;
    
    subrtn_call_expr(std::shared_ptr<node> id,
                     std::shared_ptr<node> arguments
                    )
                    : identifier_(id)
                    , args_(arguments) {}

    void add_identifier(std::shared_ptr<node> identifier) {
        if (identifier) {
            identifier_ = identifier;
        }
    }

    void add_args(std::shared_ptr<node> args) {
        args_ = args;
    }

    node_type get_type() { return CALL_EXPR; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return identifier_;
        if (i == 1) return args_;
        return nullptr;
    }
    
private:
    std::shared_ptr<node> identifier_;
    std::shared_ptr<node> args_;
};

class member_expr : public expression {
public:
    member_expr() = default;

    member_expr(std::shared_ptr<node> id,
                std::shared_ptr<node> member
               ) 
               : identifier_(id)
               , member_(member) {}

    void add_identifier(std::shared_ptr<node> identifier) {
        if (identifier) {
            identifier_ = identifier;
        }
    }

    void add_member(std::shared_ptr<node> member) {
        if (member) {
            member_ = member;
        }
    }

    node_type get_type() override { return MEMBER_EXPR; }

    unsigned int children() override { return 2; }

    std::shared_ptr<node> child(unsigned int i) override {
        if (i == 0) return identifier_;
        if (i == 1) return member_;
        return nullptr;
    }
    
private:
    std::shared_ptr<node> identifier_;
    std::shared_ptr<node> member_;
};

/*
Visitor classes
*/

class visitor {
public:
    enum visit_order {
        PRE_ORDER = 1,
        POST_ORDER,
        PRE_POST_ORDER
    };
    
    visitor() = default;
    virtual ~visitor() = default;
protected:
    virtual bool condition(std::shared_ptr<node> node) { return true; }
    virtual visit_order get_order() { return PRE_ORDER; }

    virtual void visit(std::shared_ptr<program> prog) {}
    virtual void visit(std::shared_ptr<class_dec> cl) {}
    virtual void visit(std::shared_ptr<variable_dec_list> var_list) {}
    virtual void visit(std::shared_ptr<variable_dec> var) {}
    virtual void visit(std::shared_ptr<subroutine_list> subrtn_list) {}
    virtual void visit(std::shared_ptr<subrtn_dec> subrtn) {}
    virtual void visit(std::shared_ptr<subrtn_body> body) {}
    virtual void visit(std::shared_ptr<statement_list> stmt_list) {}
    virtual void visit(std::shared_ptr<let_statement> let_stmt) {}
    virtual void visit(std::shared_ptr<if_statement> if_stmt) {}
    virtual void visit(std::shared_ptr<while_statement> while_stmt){}
    virtual void visit(std::shared_ptr<do_statement> do_stmt) {}
    virtual void visit(std::shared_ptr<return_statement> stmt) {}
    virtual void visit(std::shared_ptr<expression_list> expr_list) {}
    virtual void visit(std::shared_ptr<binop_expr> expr) {}
    virtual void visit(std::shared_ptr<unop_expr> expr) {}
    virtual void visit(std::shared_ptr<literal_expr> expr) {}
    virtual void visit(std::shared_ptr<array_member_expr> expr) {}
    virtual void visit(std::shared_ptr<subrtn_call_expr> expr) {}
    virtual void visit(std::shared_ptr<member_expr> expr) {}
    virtual void visit(std::shared_ptr<expression> expr) {}

    
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

    void set_state(visit_order state) {
        current_state_ = state;
    }

protected:
    visit_order get_state() {
        return current_state_;
    }

public:
    void traversal(std::shared_ptr<node> node) {
        if ( !condition(node) ) return;
        set_state(PRE_ORDER);
        if (get_order() & PRE_ORDER) {
            visit(node);
        }

        for (unsigned int i = 0; i < node->children(); i++) {
            traversal(node->child(i));
        }

        set_state(POST_ORDER);
        if (get_order() & POST_ORDER) {
            visit(node);
        }
    }
private:
    visit_order current_state_ = PRE_ORDER;
};
}