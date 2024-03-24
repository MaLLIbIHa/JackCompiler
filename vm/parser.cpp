#include "parser.hpp"

parser::parser() = default;

std::string parser::operator()(const std::vector<token> &tokens) {
    return parse(tokens);
}

std::string parser::parse(const std::vector<token> &tokens) {
    for (int index = 0; index < tokens.size(); ) {
        token t = tokens[index];

        keyword_type keyword = std::get<keyword_tok>(t).get_keyword_type();
        
        switch(keyword) {
        case ADD:
        case SUB:
        case AND:
        case OR:
        case EQ:
        case GT:
        case LT:
            compile_2op_instr(keyword);
            index += 1;
            break;

        case NEG:
        case NOT:
            compile_1op_instr(keyword);
            index += 1;
            break;

        case LABEL:
            compile_label(tokens[index + 1]);
            index += 2;
            break;

        case FUNCTION:
            compile_function(tokens[index + 1], tokens[index + 2]);
            index += 3;
            break;

        case GOTO:
            compile_goto(tokens[index + 1]);
            index += 2;
            break;

        case IFGOTO:
            compile_ifgoto(tokens[index + 1]);
            index += 2;
            break;

        case CALL:
            compile_call(tokens[index + 1]);
            index += 2;
            break;

        case RETURN:
            compile_return();
            index += 1;
            break;

        case PUSH:
            compile_push(tokens[index + 1], tokens[index + 2]);
            index += 3;
            break;
        
        case POP:
            compile_pop(tokens[index + 1], tokens[index + 2]);
            index += 3;
            break;
        }
    }

    return output_.str();
}

void parser::compile_2op_instr(keyword_type t) {
    output_ << "lw t0, 4(sp)\n"
        << "lw t1, (sp)\n";
    
    switch (t) {

    case ADD:
    case SUB:
    case AND:
    case OR:
        output_ << instr_2op_to_str[t] << " t0, t0, t1\n";
        break;

    case EQ:
        output_ << instr_2op_to_str[SUB] << " t0, t0, t1\n"
                << "seqz t0, t0\n";
        break;

    case GT:
        output_ << instr_2op_to_str[t] << " t0, t1, t0\n";
        break;

    case LT:
        output_ << instr_2op_to_str[t] << "t0, t0, t1\n";
        break;
    }

    output_ << "sw t0, 4(sp)\n"
        << "addi sp, sp, 4\n";
}

void parser::compile_1op_instr(const keyword_type t) {
    output_ << "lw t0, (sp)\n";
    switch (t) {
    
    case NOT:
        output_ << "xori t0, t0, -1\n";
        break;

    case NEG:
        output_ << "neg t0, t0\n";
        break;
    }

    output_ << "sw t0, (sp)\n";
}

void parser::compile_label(token arg1) {
    output_ << std::get<label_tok>(arg1).get_label() << ":\n";
}

//where is ret value
void parser::compile_function(token arg1, token arg2) {
    std::string label = std::get<label_tok>(arg1).get_label();
    int local_count = std::get<number_tok>(arg2).get_number();

    output_ << label << ":\n"
        << "sw ra, -4(sp)\n"
        << "sw fp, -8(sp)\n"
        << "addi fp, sp, -12\n"
        << "addi sp, sp," << -(local_count * 4) - 8 << '\n';
}

void parser::compile_return() {
    output_ << "addi sp, fp, 12\n"
        << "lw ra, -4(sp)\n"
        << "lw fp, -8(sp)\n"
        << "ret";
}

//FIX
void parser::compile_call(token arg1) {
    std::string label = std::get<label_tok>(arg1).get_label();
    output_ << "jal " << label << '\n';
}

void parser::compile_goto(token arg1) {
    std::string label = std::get<label_tok>(arg1).get_label();
    output_ << "j " << label << '\n';
}

void parser::compile_ifgoto(token arg1) {
    std::string label = std::get<label_tok>(arg1).get_label();
    output_ << "lw t0, (sp)\n"
        << "addi sp, sp, 4\n"
        << "bnez t0, " << label << '\n';
}

void parser::compile_push(token arg1, token arg2) {
    mem_segment segment = std::get<mem_segment_tok>(arg1).get_mem_segment();
    int seg_num = std::get<number_tok>(arg2).get_number();
    int seg_offset = seg_num * 4;

    output_ << "addi sp, sp, -4\n";

    switch (segment) {
    case STATIC:
        output_ << "lw t0, static" << seg_num << '\n'
            << "sw t0, (sp)\n";
        break;
    case LOCAL:
        output_ << "lw t0, " << -seg_offset << "(fp)\n"
            << "sw t0, (sp)\n";
        break;
    case ARGUMENT:
        output_ << "lw t0, " << seg_offset + 12 << "(fp)\n"
            << "sw t0, (sp)\n";
        break;
    case CONSTANT:
        output_ << "li t0, " << seg_num << '\n'
            << "sw t0, (sp)\n";
        break;
    }
}

void parser::compile_pop(token arg1, token arg2) {
    mem_segment segment = std::get<mem_segment_tok>(arg1).get_mem_segment();
    int seg_num = std::get<number_tok>(arg2).get_number();
    int seg_offset = seg_num * 4;

    switch (segment) {
    case STATIC:
        output_ << "lw t0, (sp)\n"
                << "sw t0, segment" << seg_num << ", t1\n";
        break;
    case LOCAL:
        output_ << "lw t0, (sp)\n"
                << "sw t0, " << -seg_offset << "(fp)\n";
        break;
    case ARGUMENT:
        output_ << "lw t0, (sp)\n"
                << "sw t0, " << seg_offset + 12 << "(fp)\n";
        break;
    case CONSTANT:
        break;
    case TEMP:
        output_ << "lw t" << seg_num << ", (sp)\n";
        break;
    case THIS:
        output_ << "lw t6, (sp)\n";
        break;
    case THAT:
        output_ << "lw t0, (sp)\n"
                << "sw t0, " << seg_offset << "(t6)\n";
        break;
    }

    output_ << "addi sp, sp, 4\n";
}