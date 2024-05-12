#pragma once

enum class node_type {
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
    RETURN_STATEMENT, //
    EXPRESSION,
    EXPRESSION_LIST,
    BINOP_EXPR,
    UNOP_EXPR,
    NAME_EXPR,
    LITERAL_EXPR,
    ARRAY_MEMBER_EXPR,
    MEMBER_EXPR,
    CALL_EXPR,
};

enum class op_type {
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

enum class subroutine_kind {
    CONSTRUCTOR_S,
    FUNCTION_S,
    METHOD_S,
};

enum class var_modifier {
    ARG_V,
    LOCAL_V,
    STATIC_V,
    FIELD_V,
};

enum class literal_type {
    INT_LITERAL,
    STR_LITERAL,
    TRUE_LITERAL,
    FALSE_LITERAL,
    NULL_LITERAL,
};