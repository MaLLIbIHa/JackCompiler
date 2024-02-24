#include <string>
#include <vector>
#include <cctype>
#include <map>
#include <iostream>
#include "vm.hpp"


virtual_machine::virtual_machine(std::string &text) {
    text_ = text;
    text_ += '\n';
}

bool virtual_machine::is_keyword(const std::string &lexem) {
    return (keywords_.find(lexem) != keywords_.end());
}

bool virtual_machine::is_mem_seg(const std::string &lexem) {
    return (mem_seg_.find(lexem) != mem_seg_.end());
}

bool virtual_machine::is_label(const std::string &lexem) {
    if (std::isdigit(lexem[0])) return false;

    for (const char &c : lexem) {
        if (std::isalnum(c) || c == '.' || c == '_') continue;
        else return false;
    }

    return true;
}

bool virtual_machine::is_number(const std::string &lexem) {
    for (const char &c : lexem) {
        if (std::isdigit(c)) continue;
        else return false;
    }

    return true;
}

virtual_machine::keyword_type virtual_machine::get_keyword_type(const std::string &lexem) {
    keyword_type type = keywords_.find(lexem)->second;
    return type;
}

virtual_machine::mem_segment virtual_machine::get_mem_segment(const std::string &lexem) {
    mem_segment seg = mem_seg_.find(lexem)->second;
    return seg;
}

int virtual_machine::lex() {
    std::string current_lexem;
    for (const char &c : text_) {
        if (!std::isspace(c)) {
            current_lexem.push_back(c);
        } else if (!current_lexem.empty()) {
            if (is_keyword(current_lexem)) {
                tokens_.push_back(token{KEYWORD, get_keyword_type(current_lexem)});
            } else if (is_mem_seg(current_lexem)) {
                tokens_.push_back({token{MEM_SEG, get_mem_segment(current_lexem)}});
            } else if (is_label(current_lexem)) {;
                tokens_.push_back(token{LABELNAME, current_lexem});
            } else if (is_number(current_lexem)) {
                tokens_.push_back(token{NUMBER, std::stoi(current_lexem)});
            } else {
                std::cerr << "Undefined token " << current_lexem << std::endl;
                return 1;
            }
            current_lexem.clear();
        }
    }

    return 0;
}

//add error check

int virtual_machine::parse() {
    for (int index = 0; index < tokens_.size(); ) {
        token t = tokens_[index];
        if (t.type != KEYWORD) {
            print_err(t, KEYWORD);
            return 1;
        }

        keyword_type keyword = std::get<keyword_type>(t.value);
        
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
            if (tokens_[index + 1].type != LABELNAME) {
                print_err(tokens_[index + 1], LABELNAME);
                return 1;
            }
            compile_label(tokens_[index + 1]);
            index += 2;
            break;

        case FUNCTION:
            if (tokens_[index + 1].type != LABELNAME) {
                print_err(tokens_[index + 1], LABELNAME);
                return 1;
            }
            if (tokens_[index + 2].type != NUMBER) {
                print_err(tokens_[index + 2], NUMBER);
                return 1;
            }
            compile_function(tokens_[index + 1], tokens_[index + 2]);
            index += 3;
            break;

        case GOTO:
            if (tokens_[index + 1].type != LABELNAME) {
                print_err(tokens_[index + 1], LABELNAME);
                return 1;
            }
            compile_goto(tokens_[index + 1]);
            index += 2;
            break;

        case IFGOTO:
            if (tokens_[index + 1].type != LABELNAME) {
                print_err(tokens_[index + 1], LABELNAME);
                return 1;
            }
            compile_ifgoto(tokens_[index + 1]);
            index += 2;
            break;

        case CALL:
            if (tokens_[index + 1].type != LABELNAME) {
                print_err(tokens_[index + 1], LABELNAME);
                return 1;
            }
            compile_call(tokens_[index + 1]);
            index += 2;
            break;

        case RETURN:
            compile_return();
            index += 1;
            break;

        case PUSH:
            if (tokens_[index + 1].type != MEM_SEG) {
                print_err(tokens_[index + 1], MEM_SEG);
                return 1;
            }
            if (tokens_[index + 2].type != NUMBER) {
                print_err(tokens_[index + 2], NUMBER);
                return 1;
            }
            compile_push(tokens_[index + 1], tokens_[index + 2]);
            index += 3;
            break;
        
        case POP:
            if (tokens_[index + 1].type != MEM_SEG) {
                print_err(tokens_[index + 1], MEM_SEG);
                return 1;
            }
            if (tokens_[index + 2].type != NUMBER) {
                print_err(tokens_[index + 2], NUMBER);
                return 1;
            }
            compile_pop(tokens_[index + 1], tokens_[index + 2]);
            index += 3;
            break;
        }
    }
    
    return 0;
}

void virtual_machine::compile_2op_instr(const keyword_type t) {
    output_ << "lw t0, 4(sp)\n"
            << "lw t1, (sp)\n";
    
    switch (t) {

    case ADD:
    case SUB:
    case AND:
    case OR:
        output_ << instr_2op_[t] << " t0, t0, t1\n";
        break;

    case EQ:
        output_ << instr_2op_[SUB] << " t0, t0, t1\n"
                << "seqz t0, t0\n";
        break;

    case GT:
        output_ << instr_2op_[t] << " t0, t1, t0\n";
        break;

    case LT:
        output_ << instr_2op_[t] << "t0, t0, t1\n";
        break;
    }

    output_ << "sw t0, 4(sp)\n"
            << "addi sp, sp, 4\n";
}

void virtual_machine::compile_1op_instr(const keyword_type t) {
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

void virtual_machine::compile_label(const token arg1) {
    output_ << std::get<std::string>(arg1.value) << ":\n";
}

void virtual_machine::compile_function(const token arg1, const token arg2) {
    std::string label = std::get<std::string>(arg1.value);
    int local_count = std::get<int>(arg2.value);

    output_ << label << ":\n"
            << "sw ra, -4(sp)\n"
            << "sw fp, -8(sp)\n"
            << "addi fp, sp, -12\n"
            << "addi sp, sp," << -(local_count * 4) - 8 << '\n';
}

void virtual_machine::compile_return() {
    output_ << "addi sp, fp, 12\n"
            << "lw ra, -4(sp)\n"
            << "lw fp, -8(sp)\n"
            << "ret";
}

void virtual_machine::compile_call(const token arg1) {
    std::string label = std::get<std::string>(arg1.value);
    output_ << "jal " << label << '\n';
}

void virtual_machine::compile_goto(const token arg1) {
    std::string label = std::get<std::string>(arg1.value);
    output_ << "j " << label << '\n';
}

void virtual_machine::compile_ifgoto(const token arg1) {
    std::string label = std::get<std::string>(arg1.value);
    output_ << "lw t0, (sp)\n"
            << "addi sp, sp, 4\n"
            << "bnez t0, " << label << '\n';
}

void virtual_machine::compile_push(const token arg1, const token arg2) {
    mem_segment segment = std::get<mem_segment>(arg1.value);
    int seg_num = std::get<int>(arg2.value);
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

void virtual_machine::compile_pop(const token arg1, const token arg2) {
    mem_segment segment = std::get<mem_segment>(arg1.value);
    int seg_num = std::get<int>(arg2.value);
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
    }

    output_ << "addi sp, sp, 4\n";
}

void virtual_machine::print() {
    std::cout << output_.str();
}

void virtual_machine::set_output_file(std::ofstream &file) {
    file_output_ = std::move(file);
}

void virtual_machine::print_to_file() {
    file_output_ << output_.str();
}

void virtual_machine::print_err(token t, token_type expected) {
    std::string type = token_type_to_str_.find(t.type)->second;
    std::string expected_type = token_type_to_str_.find(expected)->second;
    std::cerr << "Invalid token " << type << ", expected " << expected_type << std::endl;
}