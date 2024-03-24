#include "parser.hpp"

namespace compiler {

std::map<std::string, std::pair<int, associativity>> op_info_ = {{"||" , {1, LEFT_ASC}},
                                                                 {"&&" , {2, LEFT_ASC}},
                                                                 {"|"  , {3, LEFT_ASC}},
                                                                 {"&"  , {4, LEFT_ASC}},
                                                                 {"==" , {5, LEFT_ASC}},
                                                                 {"+"  , {6, LEFT_ASC}},
                                                                 {"-"  , {6, LEFT_ASC}},
                                                                 {"*"  , {7, LEFT_ASC}},
                                                                 {"/"  , {7, LEFT_ASC}},
                                                                 {"-u" , {8, RIGHT_ASC}},
                                                                 {"~"  , {8, RIGHT_ASC}},
                                                                 {"."  , {9, LEFT_ASC}},
                                                                 {"["  , {9, LEFT_ASC}},
                                                                 {"("  , {9, LEFT_ASC}},};

parser::parser(std::vector<token> tokens) : tokens_(tokens) {
    prog_ = std::make_shared<program>();
}

parser::parser(std::vector<token> tokens, 
               std::shared_ptr<program> prog
              ) : tokens_(tokens), prog_(prog) {}

std::shared_ptr<program> parser::parse_program() {
    std::shared_ptr<class_dec> class_dec;

    while(has_tokens()) {
        class_dec = parse_class();
        prog_->add_class(class_dec);
        class_dec->set_parent(prog_);
    }
    return prog_;
}

std::shared_ptr<class_dec> parser::parse_class() {
    std::shared_ptr<class_dec> class_node = std::make_shared<class_dec>();
    std::shared_ptr<variable_dec_list> var_list = std::make_shared<variable_dec_list>();
    std::shared_ptr<subroutine_list> subrtn_list = std::make_shared<subroutine_list>();

    consume(CLASS);
    
    std::string class_name = consume(IDENTIFIER).value;
    class_node->set_name(class_name);
    
    consume(LBRACE);

    while (has_tokens() && is_class_var_dec(current())) {
        parse_var_dec(consume(), var_list);
    }

    while (has_tokens() && is_func_dec(current())) {
        std::shared_ptr<subrtn_dec> subrtn = parse_subrtn_dec(consume());
        subrtn_list->add_subrtn(subrtn);
        subrtn->set_parent(subrtn_list);
    }

    consume(RBRACE);
    class_node->add_var_list(var_list);
    var_list->set_parent(class_node);
    class_node->add_subrtn_list(subrtn_list);
    subrtn_list->set_parent(class_node);

    return class_node;
}

void parser::parse_var_dec(token var_kind, 
                           std::shared_ptr<variable_dec_list> var_list) {
    bool ended_stmt = false;
    std::shared_ptr<variable_dec> var;
    var_modifier mod = token_to_var_modifier(var_kind);

    token type_tok = consume_type();
    std::string var_type = token_to_var_type(type_tok);

    while(has_tokens()) {
        token id = consume(IDENTIFIER);
        var = std::make_shared<variable_dec>(id.value, var_type, mod);
        var_list->add_var(var);
        var->set_parent(var_list);

        token sep_tok = consume();
        if (sep_tok.kind == COMMA) {
            continue;
        } else if (sep_tok.kind == SEMICOLON) {
            ended_stmt = true;
            break;
        } else {
            //error: expected comma or semicolon
        }
    }

    if (!ended_stmt) {
        //error: unexpected end of declaration
    }
}

// function/method/constructor type|void name ( type name, ...)
std::shared_ptr<subrtn_dec> parser::parse_subrtn_dec(token subrtn_kind) {
    bool ended_stmt = false;
    std::shared_ptr<subrtn_dec> subrtn_node = std::make_shared<subrtn_dec>();
    std::shared_ptr<variable_dec_list> arg_list = std::make_shared<variable_dec_list>();
    std::string name;
    std::string ret_type;

    if (is_type(current()) || current().kind == VOID) {
        ret_type = token_to_var_type(consume());
    } else {
        //error: expected type or void
    }

    name = consume(IDENTIFIER).value;
    subrtn_node->set_name(name);
    subrtn_node->set_ret_type(ret_type);
    subrtn_node->set_subrtn_type(token_to_subrtn_type(subrtn_kind));

    consume(LPAREN);

    while (has_tokens() && current().kind != RPAREN) {
        std::string arg_type;
        std::string arg_name;

        token type = consume_type();
        arg_type = token_to_var_type(type);
        arg_name = consume(IDENTIFIER).value;
        std::shared_ptr<variable_dec> arg = std::make_shared<variable_dec>(arg_name, arg_type, ARG_V);
        arg_list->add_var(arg);
        arg->set_parent(arg_list);

        token sep_token = current();
        if (sep_token.kind == COMMA) {
            consume();
            continue;
        } else if (sep_token.kind == RPAREN) {
            ended_stmt = true;
            break;
        } else {
            //error: expected comma or paren
        }
    }

    consume(RPAREN);

    if (!ended_stmt) {
        //error: unexpected end of stmt
    }

    std::shared_ptr<subrtn_body> body = parse_subrtn_body();
    subrtn_node->add_body(body);
    body->set_parent(subrtn_node);
    subrtn_node->add_arg_list(arg_list);
    arg_list->set_parent(subrtn_node);

    return subrtn_node;
}

std::shared_ptr<subrtn_body> parser::parse_subrtn_body() {
    std::shared_ptr<subrtn_body> body = std::make_shared<subrtn_body>();
    std::shared_ptr<variable_dec_list> var_dec_list = std::make_shared<variable_dec_list>();
    std::shared_ptr<statement_list> stmt_list;

    consume(LBRACE);
    while (has_tokens() && current().kind == VAR) {
        token type = consume(VAR);
        parse_var_dec(type, var_dec_list);
    }

    stmt_list = parse_statements();
    body->set_statement_list(stmt_list);
    stmt_list->set_parent(body);

    body->set_var_list(var_dec_list);
    var_dec_list->set_parent(body);
    consume(RBRACE);
    return body;
}


//todo empty stmt list
std::shared_ptr<statement_list> parser::parse_statements() {
    bool ended_body = false;
    std::shared_ptr<statement_list> stmts = std::make_shared<statement_list>();
    while (has_tokens()) {
        std::shared_ptr<statement> stmt;
        switch (current().kind) {
        case IF:
            stmt = parse_if();
            break;
        case LET:
            stmt = parse_let();
            break;
        case WHILE:
            stmt = parse_while();
            break;
        case DO:
            stmt = parse_do();
            break;
        case RETURN:
            stmt = parse_return();
            break;
        case RBRACE:
            ended_body = true;
            break;
        default:
            //error unexpected token
            break;
        }
        
        if (stmt != nullptr) {
            stmts->add_statement(stmt);
            stmt->set_parent(stmts);
        }

        if (ended_body) {
            break;
        }
    }
    return stmts;
}


std::shared_ptr<statement> parser::parse_if() {
    std::shared_ptr<if_statement> if_stmt = std::make_shared<if_statement>();
    std::shared_ptr<expression> condition;
    std::shared_ptr<statement_list> body;
    consume(IF);
    consume(LPAREN);
    condition = parse_expression();
    if_stmt->add_condition(condition);
    condition->set_parent(if_stmt);
    consume(RPAREN);
    consume(LBRACE);
    body = parse_statements();
    if_stmt->add_if_body(body);
    body->set_parent(if_stmt);
    consume(RBRACE);
    if (current().kind == ELSE) {
        consume(ELSE);
        consume(LBRACE);
        body = parse_statements();
        if_stmt->add_else_body(body);
        body->set_parent(if_stmt);
        consume(RBRACE);
    }
    consume(RBRACE);
    return if_stmt;
}

std::shared_ptr<statement> parser::parse_let() {
    std::shared_ptr<let_statement> let_stmt = std::make_shared<let_statement>();
    std::shared_ptr<expression> lhs;
    std::shared_ptr<expression> rhs;
    consume(LET);
    lhs = parse_expression();
    let_stmt->add_lhs(lhs);
    lhs->set_parent(let_stmt);
    consume(ASSIGN);
    rhs = parse_expression();
    let_stmt->add_rhs(rhs);
    rhs->set_parent(let_stmt);
    consume(SEMICOLON);
    return let_stmt;
}

std::shared_ptr<statement> parser::parse_while() {
    std::shared_ptr<while_statement> while_stmt = std::make_shared<while_statement>();
    std::shared_ptr<expression> condition;
    std::shared_ptr<statement_list> body;
    consume(WHILE);
    consume(LPAREN);
    condition = parse_expression();
    while_stmt->add_condition(condition);
    condition->set_parent(while_stmt);
    consume(RPAREN);
    consume(LBRACE);
    body = parse_statements();
    while_stmt->add_body(body);
    body->set_parent(while_stmt);
    consume(RBRACE);
    return while_stmt;
}

std::shared_ptr<statement> parser::parse_return() {
    std::shared_ptr<return_statement> ret_stmt = std::make_shared<return_statement>();
    std::shared_ptr<expression> ret_expr;
    consume(RETURN);
    if (current().kind == SEMICOLON) {
        consume(SEMICOLON);
        ret_stmt->add_subrtn(nullptr);
        return ret_stmt;
    }
    ret_expr = parse_expression();
    ret_stmt->add_subrtn(ret_expr);
    ret_expr->set_parent(ret_stmt);
    consume(SEMICOLON);
    return ret_stmt;
}

std::shared_ptr<statement> parser::parse_do() {
    std::shared_ptr<do_statement> do_stmt = std::make_shared<do_statement>();
    std::shared_ptr<expression> call_expr;
    consume(DO);
    call_expr = parse_expression();
    do_stmt->add_subrtn(call_expr);
    call_expr->set_parent(do_stmt);
    consume(SEMICOLON);
    return do_stmt;
}

std::shared_ptr<expression> parser::parse_expression(int prev_prec) {
    std::shared_ptr<expression> first_operand = parse_term();
    std::shared_ptr<expression> second_operand;
    std::shared_ptr<expression> new_binop;
    while ( is_binary_op(current()) && get_prec(current()) >= prev_prec) {
        token op = consume();
        int next_prec = get_prec(op);
        if (get_asc(op) == LEFT_ASC) {
            next_prec++;
        }
        second_operand = parse_expression(next_prec);
        new_binop = mk_binop_node(op, first_operand, second_operand);
        first_operand->set_parent(new_binop);
        second_operand->set_parent(new_binop);
        first_operand = new_binop;
    }
    return first_operand;
}

//  Parse expression which includes parenthesised expressions,
//  expressions with unary operators and compound identifiers.
std::shared_ptr<expression> parser::parse_term() {
    token curr = current();
    if ( is_unary_op(curr) ) {
        token op = consume();
        int precedence = get_prec(op);
        std::shared_ptr<expression> exp = parse_expression(precedence);
        std::shared_ptr<expression> unop = mk_unop_node(op, exp);
        exp->set_parent(unop);
        return unop;
    } else if (curr.kind == LPAREN) {
        consume(LPAREN);
        std::shared_ptr<expression> exp = parse_expression();
        consume(RPAREN);
        return exp;
    } else if (curr.type == IDENTIFIER || curr.kind == THIS) {
        return parse_compound_id();
    } else if (is_constant_literal(curr)) {
        token lit = consume();
        return std::make_shared<literal_expr>(lit);
    } else {
        throw;
        //error: expected term
    }
    return nullptr;
}

//  Parse compound identifiers which includes dot, square brackets, 
//  function call operators.
//  First identifier can be class name or this keyword
std::shared_ptr<expression> parser::parse_compound_id() {
    token id = consume();
    std::shared_ptr<expression> curr_node = std::make_shared<literal_expr>(id);
    std::shared_ptr<expression> new_node;
    std::shared_ptr<expression> member_exp;
    std::shared_ptr<expression_list> subrtn_args;
    bool id_end = false;
    while (has_tokens() && !id_end) {
        switch (current().kind) {
        case DOT:
            consume(DOT);
            id = consume(IDENTIFIER);
            member_exp = std::make_shared<literal_expr>(id);
            new_node = std::make_shared<member_expr>(curr_node, member_exp);
            member_exp->set_parent(new_node);
            curr_node->set_parent(new_node);
            curr_node = new_node;
            break;

        case LBRACK:
            consume(LBRACK);
            member_exp = parse_expression();
            consume(RBRACK);
            new_node = std::make_shared<array_member_expr>(curr_node, member_exp);
            member_exp->set_parent(new_node);
            curr_node->set_parent(new_node);
            curr_node = new_node;
            break;

        case LPAREN:
            consume(LPAREN);
            subrtn_args = parse_arg_list();
            consume(RPAREN);
            new_node = std::make_shared<subrtn_call_expr>(curr_node, subrtn_args);
            curr_node->set_parent(new_node);
            subrtn_args->set_parent(new_node);
            curr_node = new_node;
            break;

        //make concrete case
        default:
            id_end = true;
            break;
        }
    }
    return curr_node;
}

std::shared_ptr<expression_list> parser::parse_arg_list() {
    if (current().kind == RPAREN) {
        return std::make_shared<expression_list>();
    }
    std::shared_ptr<expression_list> arg_exprs = std::make_shared<expression_list>();
    std::shared_ptr<expression> expr;
    bool ended_stmt = false;
    while (has_tokens()) {
        expr = parse_expression();
        arg_exprs->add_expression(expr);
        expr->set_parent(arg_exprs);
        if (current().kind == RPAREN) {
            ended_stmt = true;
            break;
        }
        consume(COMMA);
    }

    if (!ended_stmt) {
        //error unexpected end of stmt
    }

    return arg_exprs;
}


std::shared_ptr<expression> parser::mk_unop_node(token op, 
                                                 std::shared_ptr<expression> operand) {
    op_type type;
    switch (op.kind) {
    case SUB:
        type = NEG_OP;
        break;
    case TILDE:
        type = TILDE_OP;
        break;
    default:
        //error 
        break;
    }
    return std::make_shared<unop_expr>(operand, type);
}

//todo write expression maker layer 
std::shared_ptr<expression> parser::mk_binop_node(token op,
                                                  std::shared_ptr<expression> first_operand,
                                                  std::shared_ptr<expression> second_operand) {
    op_type type;
    switch (op.kind) {
    case ADD:
        type = ADD_OP;
        break;
    case SUB:
        type = SUB_OP;
        break;
    case MUL:
        type = MUL_OP;
        break;
    case QUO:
        type = DIV_OP;
        break;
    case LOG_AND:
        type = LOG_AND_OP;
        break;
    case LOG_OR:
        type = LOG_OR_OP;
        break;
    case BIT_AND:
        type = BIT_AND_OP;
        break;
    case LSS:
        type = LSS_OP;
        break;
    case GTR:
        type = GTR_OP;
        break;
    case EQL:
        type = EQL_OP;
        break;
    default:
        //error
        break;
    }
    return std::make_shared<binop_expr>(first_operand, second_operand, type);
}


token parser::consume_type() {
    if (is_type(current())) {
        return consume();
    } else {
        //error: expected type
    }
    return token();
}

token parser::consume(keyword_symbol type) {
    if (tokens_[current_token_].kind == type) {    
        return consume();
    } else {
        //expected_err();
    }
    return token();
}

token parser::consume(token_type type) {
    if (tokens_[current_token_].type == type) {
        return consume();
    } else {
        //expected_err();
    }
    return token();
}

token parser::consume() {
    if (has_tokens()) {
        current_token_++;
        return tokens_[current_token_ - 1];
    }
    return token();
}

token parser::current() {
    return tokens_[current_token_];
}

bool parser::has_tokens() {
    return current_token_ < tokens_.size() && 
           tokens_[current_token_].type != T_EOF;
}

token parser::next() {
    if (current_token_ + 1 < tokens_.size()) {
        return tokens_[current_token_ + 1];
    }
    return token();
}

bool parser::is_class_var_dec(token tok) {
    if (tok.kind == STATIC || tok.kind == FIELD) {
        return true;
    }
    return false;
}

bool parser::is_func_dec(token tok) {
    if (tok.kind == CONSTRUCTOR ||
        tok.kind == FUNCTION ||
        tok.kind == METHOD) 
    {
        return true;
    }
    return false;
}

bool parser::is_type(token tok) {
    if (tok.kind == INT || tok.kind == CHAR || tok.kind == BOOLEAN ||
        tok.type == IDENTIFIER) 
    {
        return true;
    }
    return false;
}

bool parser::is_constant_literal(token tok) {
    if (tok.type == STR_LITERAL || 
        tok.type == INT_LITERAL ||
        tok.kind == TRUE ||
        tok.kind == FALSE ||
        tok.kind == NULL_KEYWORD )
    {
        return true;
    }
    return false;
}

bool parser::is_unary_op(token tok) {
    if (tok.kind == TILDE || tok.kind == SUB) {
        return true;
    }
    return false;
}

bool parser::is_binary_op(token tok) {
    if (binop_beg < tok.kind && tok.kind < binop_end) {
        return true;
    }
    return false;
}

int parser::get_prec(token tok) {
    return op_info_.find(tok.value)->second.first;
}

associativity parser::get_asc(token tok) {
    return op_info_.find(tok.value)->second.second;
} 

std::string parser::token_to_var_type(token type_token) {
    if (type_token.type == KEYWORD) {
        switch (type_token.kind) {
        case INT:
            return "int";
        case CHAR:
            return "char";
        case BOOLEAN:
            return "boolean";
        case VOID:
            return "void";
        default:
            //error
            break;
        }
    }
    return type_token.value;
}

var_modifier parser::token_to_var_modifier(token mod_tok) {
    switch (mod_tok.kind) {
    case STATIC:
        return STATIC_V;
    case FIELD:
        return FIELD_V;
    case VAR:
        return LOCAL_V;
    default:
        //error
        break;
    }
    return ARG_V;
}

subroutine_type parser::token_to_subrtn_type(token type_tok) {
    switch (type_tok.kind) {
    case METHOD:
        return METHOD_S;
    case CONSTRUCTOR:
        return CONSTRUCTOR_S;
    case FUNCTION:
        return FUNCTION_S;
    default:
        //error
        break;
    }
    return CONSTRUCTOR_S;
}

void parser::expected_err(keyword_symbol tok) {
    //append string: error expected token
}
}