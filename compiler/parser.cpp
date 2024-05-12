#include "parser.hpp"

std::map<std::string, std::pair<int, associativity>> op_info_ = {{"||" , {1, associativity::LEFT_ASC}},
                                                                 {"&&" , {2, associativity::LEFT_ASC}},
                                                                 {"|"  , {3, associativity::LEFT_ASC}},
                                                                 {"&"  , {4, associativity::LEFT_ASC}},
                                                                 {"==" , {5, associativity::LEFT_ASC}},
                                                                 {"+"  , {6, associativity::LEFT_ASC}},
                                                                 {"-"  , {6, associativity::LEFT_ASC}},
                                                                 {"*"  , {7, associativity::LEFT_ASC}},
                                                                 {"/"  , {7, associativity::LEFT_ASC}},
                                                                 {"-u" , {8, associativity::RIGHT_ASC}},
                                                                 {"~"  , {8, associativity::RIGHT_ASC}},
                                                                 {"."  , {9, associativity::LEFT_ASC}},
                                                                 {"["  , {9, associativity::LEFT_ASC}},
                                                                 {"("  , {9, associativity::LEFT_ASC}},};

parser::parser(std::string sourceText) : lexer_(sourceText) {
    prog_ = std::make_shared<program>();
}

parser::parser(std::string sourceText,
               std::shared_ptr<program> prog
              )
              : lexer_(sourceText), prog_(prog) {}

std::shared_ptr<program> parser::parse_program() {
    std::shared_ptr<class_dec> class_dec;

    while(lexer_.hasTokens()) {
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

    token class_keyword = consume(token_kind::CLASS);
    
    std::string class_name = consume(token_type::IDENTIFIER).get_value();
    class_node->set_name(class_name);
    set_position(class_node, class_keyword);

    consume(token_kind::LBRACE);

    while (lexer_.hasTokens() && is_class_var_dec(lexer_.currentToken())) {
        parse_var_dec(consume(), var_list);
    }

    while (lexer_.hasTokens() && is_func_dec(lexer_.currentToken())) {
        std::shared_ptr<subroutine_dec> subrtn = parse_subroutine_dec(consume());
        subrtn_list->add_subroutine(subrtn);
        subrtn->set_parent(subrtn_list);
    }

    consume(token_kind::RBRACE);
    class_node->add_var_list(var_list);
    var_list->set_parent(class_node);
    class_node->add_subroutine_list(subrtn_list);
    subrtn_list->set_parent(class_node);

    return class_node;
}

void parser::parse_var_dec(token var_kind, 
                           std::shared_ptr<variable_dec_list> var_list) {
    bool ended_stmt = false;
    std::shared_ptr<variable_dec> var;
    var_modifier mod = token_to_var_modifier(var_kind);

    token type_tok = consume();
    if ( !is_type(type_tok) ) {
        print_expected_err("Expected type", 
                           type_tok.get_line_pos(), 
                           type_tok.get_in_line_pos());
        throw std::runtime_error("Parser error");
    }
    std::string var_type = token_to_var_type(type_tok);

    while(lexer_.hasTokens()) {
        token id = consume(token_type::IDENTIFIER);
        var = std::make_shared<variable_dec>(id.get_value(), var_type, mod);
        var_list->add_var(var);
        var->set_parent(var_list);
        set_position(var, id);

        token sep_tok = consume();
        if (sep_tok.get_kind() == token_kind::COMMA) {
            continue;
        } else if (sep_tok.get_kind() == token_kind::SEMICOLON) {
            ended_stmt = true;
            break;
        } else {
            print_expected_err("Expected \",\" or \";\"", 
                               sep_tok.get_line_pos(),
                               sep_tok.get_in_line_pos());
            throw std::runtime_error("Parser error");
        }
    }

    if (!ended_stmt) {
        print_expected_err("Unexpected end of statement",
                           var_kind.get_line_pos(), 
                           var_kind.get_in_line_pos());
        throw std::runtime_error("Parser error");
    }
}

// function/method/constructor type|void name ( type name, ...)
std::shared_ptr<subroutine_dec> parser::parse_subroutine_dec(token subrtn_kind) {
    bool ended_stmt = false;
    std::shared_ptr<subroutine_dec> subrtn_node = std::make_shared<subroutine_dec>();
    std::shared_ptr<variable_dec_list> arg_list = std::make_shared<variable_dec_list>();
    std::string name;
    std::string ret_type;
    token ret_type_tok = lexer_.currentToken();

    if (is_type(ret_type_tok) || ret_type_tok.get_kind() == token_kind::VOID) {
        ret_type = token_to_var_type(consume());
    } else {
        print_expected_err("Expected type or void",
                           ret_type_tok.get_line_pos(),
                           ret_type_tok.get_in_line_pos());
        throw std::runtime_error("Parser error");
    }

    name = consume(token_type::IDENTIFIER).get_value();
    subrtn_node->set_name(name);
    subrtn_node->set_ret_type(ret_type);
    subrtn_node->set_subroutine_kind(token_to_subroutine_kind(subrtn_kind));
    set_position(subrtn_node, subrtn_kind);

    token arg_list_begin = consume(token_kind::LPAREN);
    set_position(arg_list, arg_list_begin);

    while (lexer_.hasTokens() && lexer_.currentToken().get_kind() != token_kind::RPAREN) {
        std::shared_ptr<variable_dec> arg;

        token type_tok = consume();
        if ( !is_type(type_tok) ) {
            print_expected_err("Expected type",
                               type_tok.get_line_pos(),
                               type_tok.get_in_line_pos());
            throw std::runtime_error("Parser error");
        }
        std::string arg_type = token_to_var_type(type_tok);
        token arg_id = consume(token_type::IDENTIFIER);
        arg = std::make_shared<variable_dec>(arg_id.get_value(), arg_type, var_modifier::ARG_V);
        arg_list->add_var(arg);
        arg->set_parent(arg_list);
        set_position(arg, arg_id);

        token sep_token = lexer_.currentToken();
        if (sep_token.get_kind() == token_kind::COMMA) {
            consume();
            continue;
        } else if (sep_token.get_kind() == token_kind::RPAREN) {
            ended_stmt = true;
            break;
        } else {
            print_expected_err("Expected \",\" or \")\"",
                               sep_token.get_line_pos(), sep_token.get_in_line_pos());
            std::runtime_error("Parser error");
        }
    }

    consume(token_kind::RPAREN);

    std::shared_ptr<subroutine_body> body = parse_subroutine_body();
    subrtn_node->add_body(body);
    body->set_parent(subrtn_node);
    subrtn_node->add_arg_list(arg_list);
    arg_list->set_parent(subrtn_node);

    return subrtn_node;
}

std::shared_ptr<subroutine_body> parser::parse_subroutine_body() {
    std::shared_ptr<subroutine_body> body = std::make_shared<subroutine_body>();
    std::shared_ptr<variable_dec_list> var_dec_list = std::make_shared<variable_dec_list>();
    std::shared_ptr<statement_list> stmt_list;

    consume(token_kind::LBRACE);
    while (lexer_.hasTokens() && lexer_.currentToken().get_kind() == token_kind::VAR) {
        token kind = consume(token_kind::VAR);
        parse_var_dec(kind, var_dec_list);
    }

    stmt_list = parse_statements();
    body->set_statement_list(stmt_list);
    stmt_list->set_parent(body);

    body->set_var_list(var_dec_list);
    var_dec_list->set_parent(body);
    consume(token_kind::RBRACE);
    return body;
}

std::shared_ptr<statement_list> parser::parse_statements() {
    bool ended_body = false;
    std::shared_ptr<statement_list> stmts = std::make_shared<statement_list>();
    while (lexer_.hasTokens()) {
        std::shared_ptr<statement> stmt_node;
        token stmt_tok = lexer_.currentToken();
        switch (stmt_tok.get_kind()) {
        case token_kind::IF:
            stmt_node = parse_if();
            break;
        case token_kind::LET:
            stmt_node = parse_let();
            break;
        case token_kind::WHILE:
            stmt_node = parse_while();
            break;
        case token_kind::DO:
            stmt_node = parse_do();
            break;
        case token_kind::RETURN:
            stmt_node = parse_return();
            break;
        case token_kind::RBRACE:
            ended_body = true;
            break;
        default:
            print_expected_err("Expected statement or \"}\"",
                               stmt_tok.get_line_pos(),
                               stmt_tok.get_in_line_pos());
            throw std::runtime_error("Parser error");
        }
        
        if (stmt_node != nullptr) {
            stmts->add_statement(stmt_node);
            stmt_node->set_parent(stmts);
            set_position(stmt_node, stmt_tok);
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
    token if_tok = consume(token_kind::IF);
    set_position(if_stmt, if_tok);
    consume(token_kind::LPAREN);
    condition = parse_expression();
    if_stmt->add_condition(condition);
    condition->set_parent(if_stmt);
    consume(token_kind::RPAREN);
    consume(token_kind::LBRACE);
    body = parse_statements();
    if_stmt->add_if_body(body);
    body->set_parent(if_stmt);
    consume(token_kind::RBRACE);
    if (lexer_.currentToken().get_kind() == token_kind::ELSE) {
        consume(token_kind::ELSE);
        consume(token_kind::LBRACE);
        body = parse_statements();
        if_stmt->add_else_body(body);
        body->set_parent(if_stmt);
        consume(token_kind::RBRACE);
    }
    return if_stmt;
}

std::shared_ptr<statement> parser::parse_let() {
    std::shared_ptr<let_statement> let_stmt = std::make_shared<let_statement>();
    std::shared_ptr<expression> lhs;
    std::shared_ptr<expression> rhs;
    token let_tok = consume(token_kind::LET);
    set_position(let_stmt, let_tok);
    lhs = parse_expression();
    let_stmt->add_lhs(lhs);
    lhs->set_parent(let_stmt);
    consume(token_kind::ASSIGN);
    rhs = parse_expression();
    let_stmt->add_rhs(rhs);
    rhs->set_parent(let_stmt);
    consume(token_kind::SEMICOLON);
    return let_stmt;
}

std::shared_ptr<statement> parser::parse_while() {
    std::shared_ptr<while_statement> while_stmt = std::make_shared<while_statement>();
    std::shared_ptr<expression> condition;
    std::shared_ptr<statement_list> body;
    token while_tok = consume(token_kind::WHILE);
    set_position(while_stmt, while_tok);
    consume(token_kind::LPAREN);
    condition = parse_expression();
    while_stmt->add_condition(condition);
    condition->set_parent(while_stmt);
    consume(token_kind::RPAREN);
    consume(token_kind::LBRACE);
    body = parse_statements();
    while_stmt->add_body(body);
    body->set_parent(while_stmt);
    consume(token_kind::RBRACE);
    return while_stmt;
}

std::shared_ptr<statement> parser::parse_return() {
    std::shared_ptr<return_statement> ret_stmt = std::make_shared<return_statement>();
    std::shared_ptr<expression> ret_expr;
    token ret_tok = consume(token_kind::RETURN);
    set_position(ret_stmt, ret_tok);
    if (lexer_.currentToken().get_kind() == token_kind::SEMICOLON) {
        consume(token_kind::SEMICOLON);
        ret_stmt->add_ret_expr(nullptr);
        return ret_stmt;
    }
    ret_expr = parse_expression();
    ret_stmt->add_ret_expr(ret_expr);
    ret_expr->set_parent(ret_stmt);
    consume(token_kind::SEMICOLON);
    return ret_stmt;
}

std::shared_ptr<statement> parser::parse_do() {
    std::shared_ptr<do_statement> do_stmt = std::make_shared<do_statement>();
    std::shared_ptr<expression> call_expr;
    token do_tok = consume(token_kind::DO);
    set_position(do_stmt, do_tok);
    call_expr = parse_expression();
    do_stmt->add_call_expr(call_expr);
    call_expr->set_parent(do_stmt);
    consume(token_kind::SEMICOLON);
    return do_stmt;
}

std::shared_ptr<expression> parser::parse_expression(int prev_prec) {
    std::shared_ptr<expression> first_operand = parse_term();
    std::shared_ptr<expression> second_operand;
    std::shared_ptr<expression> new_binop;
    while ( is_binary_op(lexer_.currentToken()) && get_prec(lexer_.currentToken()) >= prev_prec) {
        token op = consume();
        int next_prec = get_prec(op);
        if (get_asc(op) == associativity::LEFT_ASC) {
            next_prec++;
        }
        second_operand = parse_expression(next_prec);
        new_binop = mk_binop_node(op, first_operand, second_operand);
        set_position(new_binop, op);
        first_operand->set_parent(new_binop);
        second_operand->set_parent(new_binop);
        first_operand = new_binop;
    }
    return first_operand;
}

//  Parse expression which includes parenthesised expressions,
//  expressions with unary operators and compound identifiers.
std::shared_ptr<expression> parser::parse_term() {
    token curr = lexer_.currentToken();
    if ( is_unary_op(curr) ) {
        token op = consume();
        int precedence = get_prec(op);
        std::shared_ptr<expression> exp = parse_expression(precedence);
        std::shared_ptr<expression> unop = mk_unop_node(op, exp);
        set_position(unop, op);
        exp->set_parent(unop);
        return unop;
    } else if (curr.get_kind() == token_kind::LPAREN) {
        consume(token_kind::LPAREN);
        std::shared_ptr<expression> exp = parse_expression();
        consume(token_kind::RPAREN);
        return exp;
    } else if (curr.get_type() == token_type::IDENTIFIER || curr.get_kind() == token_kind::THIS) {
        return parse_compound_id();
    } else if (is_constant_literal(curr)) {
        token lit = consume();
        literal_type lit_type = token_to_literal_type(lit);
        auto lit_expr = std::make_shared<literal_expr>(lit.get_value(), lit_type);
        set_position(lit_expr, lit);
        return lit_expr;
    } else {
        print_expected_err("Expected term expression",
                           curr.get_line_pos(),
                           curr.get_in_line_pos());
        throw std::runtime_error("Parser error");
    }
    return nullptr;
}

//  Parse compound identifiers which includes dot, square brackets, 
//  function call operators.
//  First identifier can be class name or this keyword
std::shared_ptr<expression> parser::parse_compound_id() {
    token id = consume();
    token next_id;
    std::shared_ptr<expression> new_node;
    std::shared_ptr<expression> member_exp;
    std::shared_ptr<expression_list> subrtn_args;
    std::shared_ptr<expression> curr_node = std::make_shared<name_expr>(id.get_value());
    set_position(curr_node, id);

    while (lexer_.hasTokens()) {
        switch (lexer_.currentToken().get_kind()) {
        case token_kind::DOT:
            consume(token_kind::DOT);
            next_id = consume(token_type::IDENTIFIER);
            new_node = std::make_shared<member_expr>(curr_node, next_id.get_value());
            break;

        case token_kind::LBRACK:
            consume(token_kind::LBRACK);
            member_exp = parse_expression();
            consume(token_kind::RBRACK);
            new_node = std::make_shared<array_member_expr>(curr_node, member_exp);
            member_exp->set_parent(new_node);
            break;

        case token_kind::LPAREN:
            next_id = consume(token_kind::LPAREN);
            subrtn_args = parse_arg_list();
            set_position(subrtn_args, next_id);
            consume(token_kind::RPAREN);
            new_node = std::make_shared<subroutine_call_expr>(curr_node, subrtn_args);
            subrtn_args->set_parent(new_node);
            break;

        default:
            return curr_node;
        }

        set_position(new_node, id);
        curr_node->set_parent(new_node);
        curr_node = new_node;
    }
    return nullptr;
}

std::shared_ptr<expression_list> parser::parse_arg_list() {
    token arg_list_beg = lexer_.currentToken();
    if (arg_list_beg.get_kind() == token_kind::RPAREN) {
        return std::make_shared<expression_list>();
    }
    std::shared_ptr<expression_list> arg_exprs = std::make_shared<expression_list>();
    std::shared_ptr<expression> expr;
    bool ended_stmt = false;
    while (lexer_.hasTokens()) {
        expr = parse_expression();
        arg_exprs->add_expression(expr);
        expr->set_parent(arg_exprs);
        if (lexer_.currentToken().get_kind() == token_kind::RPAREN) {
            ended_stmt = true;
            break;
        }
        consume(token_kind::COMMA);
    }

    if (!ended_stmt) {
        print_expected_err("Unexpected end of arguments list",
                           arg_list_beg.get_line_pos(),
                           arg_list_beg.get_in_line_pos());
        throw std::runtime_error("Parser error");
    }

    return arg_exprs;
}


std::shared_ptr<expression> parser::mk_unop_node(token op, 
                                                 std::shared_ptr<expression> operand) {
    op_type type;
    switch (op.get_kind()) {
    case token_kind::SUB:
        type = op_type::NEG_OP;
        break;
    case token_kind::TILDE:
        type = op_type::TILDE_OP;
        break;
    default:
        print_expected_err("Wrong unary operation",
                           op.get_line_pos(),
                           op.get_in_line_pos());
        throw std::runtime_error("Parser error");
    }
    return std::make_shared<unop_expr>(operand, type);
}

std::shared_ptr<expression> parser::mk_binop_node(token op,
                                                  std::shared_ptr<expression> first_operand,
                                                  std::shared_ptr<expression> second_operand) {
    op_type type;
    switch (op.get_kind()) {
    case token_kind::ADD:
        type = op_type::ADD_OP;
        break;
    case token_kind::SUB:
        type = op_type::SUB_OP;
        break;
    case token_kind::MUL:
        type = op_type::MUL_OP;
        break;
    case token_kind::QUO:
        type = op_type::DIV_OP;
        break;
    case token_kind::LOG_AND:
        type = op_type::LOG_AND_OP;
        break;
    case token_kind::LOG_OR:
        type = op_type::LOG_OR_OP;
        break;
    case token_kind::BIT_AND:
        type = op_type::BIT_AND_OP;
        break;
    case token_kind::LSS:
        type = op_type::LSS_OP;
        break;
    case token_kind::GTR:
        type = op_type::GTR_OP;
        break;
    case token_kind::EQL:
        type = op_type::EQL_OP;
        break;
    default:
        print_expected_err("Wrong binary operation",
                           op.get_line_pos(),
                           op.get_in_line_pos());
        throw std::runtime_error("Parser error");
    }
    return std::make_shared<binop_expr>(first_operand, second_operand, type);
}

token parser::consume(token_type type) {
    token cur = lexer_.currentToken();
    if (cur.get_type() == type) {
        return lexer_.consume();
    } else {
        print_expected_err(type, cur.get_line_pos(), cur.get_in_line_pos());
        throw std::runtime_error("Parser error");
    }
    return token();
}

token parser::consume(token_kind kind) {
    token cur = lexer_.currentToken();
    if (cur.get_kind() == kind) {    
        return lexer_.consume();
    } else {
        print_expected_err(kind, cur.get_line_pos(), cur.get_in_line_pos());
        throw std::runtime_error("Parser error");
    }
    return token();
}

token parser::consume() {
    return lexer_.consume();
}

bool parser::is_class_var_dec(token tok) {
    if (tok.get_kind() == token_kind::STATIC || tok.get_kind() == token_kind::FIELD) {
        return true;
    }
    return false;
}

bool parser::is_func_dec(token tok) {
    if (tok.get_kind() == token_kind::CONSTRUCTOR ||
        tok.get_kind() == token_kind::FUNCTION ||
        tok.get_kind() == token_kind::METHOD) 
    {
        return true;
    }
    return false;
}

bool parser::is_type(token tok) {
    if (tok.get_kind() == token_kind::INT || 
        tok.get_kind() == token_kind::CHAR || 
        tok.get_kind() == token_kind::BOOLEAN ||
        tok.get_type() == token_type::IDENTIFIER) 
    {
        return true;
    }
    return false;
}

bool parser::is_constant_literal(token tok) {
    if (tok.get_type() == token_type::STR_LITERAL || 
        tok.get_type() == token_type::INT_LITERAL ||
        tok.get_kind() == token_kind::TRUE ||
        tok.get_kind() == token_kind::FALSE ||
        tok.get_kind() == token_kind::NULL_KEYWORD )
    {
        return true;
    }
    return false;
}

bool parser::is_unary_op(token tok) {
    if (tok.get_kind() == token_kind::TILDE || tok.get_kind() == token_kind::SUB) {
        return true;
    }
    return false;
}

bool parser::is_binary_op(token tok) {
    if (token_kind::binop_beg < tok.get_kind() && tok.get_kind() < token_kind::binop_end) {
        return true;
    }
    return false;
}

void parser::set_position(std::shared_ptr<node> node, token tok) {
    node->set_line_pos(tok.get_line_pos());
    node->set_in_line_pos(tok.get_in_line_pos());
}

int parser::get_prec(token tok) {
    return op_info_.find(tok.get_value())->second.first;
}

associativity parser::get_asc(token tok) {
    return op_info_.find(tok.get_value())->second.second;
}

std::string parser::token_to_var_type(token type_tok) {
    if (type_tok.get_type() == token_type::KEYWORD) {
        switch (type_tok.get_kind()) {
        case token_kind::INT:
            return "int";
        case token_kind::CHAR:
            return "char";
        case token_kind::BOOLEAN:
            return "boolean";
        case token_kind::VOID:
            return "void";
        default:
            print_expected_err("Wrong type token",
                               type_tok.get_line_pos(),
                               type_tok.get_in_line_pos());
            throw std::runtime_error("Parser error");
        }
    }
    return type_tok.get_value();
}

var_modifier parser::token_to_var_modifier(token mod_tok) {
    var_modifier mod;
    switch (mod_tok.get_kind()) {
    case token_kind::STATIC:
        mod = var_modifier::STATIC_V;
    case token_kind::FIELD:
        mod = var_modifier::FIELD_V;
    case token_kind::VAR:
        mod = var_modifier::LOCAL_V;
    }
    return mod;
}

subroutine_kind parser::token_to_subroutine_kind(token type_tok) {
    subroutine_kind sub_type;
    switch (type_tok.get_kind()) {
    case token_kind::METHOD:
        sub_type = subroutine_kind::METHOD_S;
    case token_kind::CONSTRUCTOR:
        sub_type = subroutine_kind::CONSTRUCTOR_S;
    case token_kind::FUNCTION:
        sub_type = subroutine_kind::FUNCTION_S;
    }
    return sub_type;
}

literal_type parser::token_to_literal_type(token kind_tok) {
    literal_type lit_kind;
    switch(kind_tok.get_kind()) {
    case token_kind::TRUE:
        lit_kind = literal_type::TRUE_LITERAL;
        break;
    case token_kind::FALSE:
        lit_kind = literal_type::FALSE_LITERAL;
        break;
    case token_kind::NULL_KEYWORD:
        lit_kind = literal_type::NULL_LITERAL;
        break;
    }

    switch (kind_tok.get_type()) {
    case token_type::INT_LITERAL:
        lit_kind = literal_type::INT_LITERAL;
        break;
    case token_type::STR_LITERAL:
        lit_kind = literal_type::STR_LITERAL;
        break;
    }
    return lit_kind;
}

void parser::print_expected_err(token_kind kind, unsigned line_pos, unsigned in_line_pos) {
    auto it = std::find_if(token::keywords.begin(), 
                           token::keywords.end(), 
                           [kind](const auto& p) { return p.second == kind; });
    std::cerr << "Expected \"" << it->first << "\" on line: " <<
                    line_pos << ":" << in_line_pos << std::endl;
}

void parser::print_expected_err(token_type type, unsigned line_pos, unsigned in_line_pos) {
    std::string expected;
    switch (type) {
    case token_type::IDENTIFIER:
        expected = "Identifier";
        break;
    case token_type::INT_LITERAL:
        expected = "Integer literal";
        break;
    case token_type::STR_LITERAL:
        expected = "String literal";
        break;
    case token_type::KEYWORD:
        expected = "Keyword";
        break;
    case token_type::SYMBOL:
        expected = "Symbol";
        break;
    }
    std::cerr << "Expected \"" << expected << " token\" on line: " <<
                    line_pos << ":" << in_line_pos << std::endl;
}

void parser::print_expected_err(const char *err, unsigned line_pos, unsigned in_line_pos) {
    std::cerr << err << " on line "
              << line_pos << ":" << in_line_pos << std::endl;
}