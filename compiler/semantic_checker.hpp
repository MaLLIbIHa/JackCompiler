#pragma once
#include "sstream"
#include "symbol_table.hpp"
#include "ast.hpp"

enum class value_category {
	NOT_VALUE,
	LVALUE,
	RVALUE,
};

class semantic_checker {
public:
	semantic_checker(const symbol_table *global_table,
					 const symbol_table *local_table)
		: global_table_(global_table),
		  local_table_(local_table) {}

	void set_current_subroutine(subroutine_symbol* cur_subrtn) { current_subrtn_ = cur_subrtn; }

	std::shared_ptr<symbol_node> check_name(const class_symbol *current_class,
											name_expr* name_expr) {
		std::string name = name_expr->get_name();
		auto cur_symbol = local_table_->find(name);
		if (cur_symbol == nullptr) {
			cur_symbol = current_class->find_member(name);
		}
		if (cur_symbol == nullptr) {
			cur_symbol = global_table_->find(name);
		}
		if (cur_symbol == nullptr) {
			std::cerr << current_func_name_err_msg()
				<< line_err_msg(name_expr->get_line_pos(), name_expr->get_in_line_pos())
				<< "Undeclared name " << name << std::endl;
		}

		if (cur_symbol->get_symbol_type() == symbol_type::VARIABLE_SYM &&
			current_subrtn_->get_kind() == subroutine_kind::FUNCTION_S)
		{
			auto var_sym = std::static_pointer_cast<var_symbol>(cur_symbol);
			if (var_sym->get_mode() == var_modifier::FIELD_V) {
				print_err_msg("Cannot use field variables in static function",
					name_expr->get_line_pos(),
					name_expr->get_in_line_pos());
				throw std::runtime_error("Semantic error");
			}
			if (var_sym->get_name() == "this") {
				print_err_msg("Cannot use \'this\' in static function",
					name_expr->get_line_pos(),
					name_expr->get_in_line_pos());
				throw std::runtime_error("Semantic error");
			}
		}

		return cur_symbol;
	}

	symbol_node* check_member(symbol_node *base_sym,
							  member_expr *member_expr) {
		if (base_sym->get_symbol_type() == symbol_type::SUBROUTINE_SYM) {
			print_err_msg("Request for member in something not a class",
				member_expr->get_line_pos(),
				member_expr->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}

		std::string name_type;
		if (base_sym->get_symbol_type() == symbol_type::CLASS_SYM) {
			name_type = base_sym->get_name();
		} else {
			name_type = static_cast<var_symbol*>(base_sym)->get_var_type();
		}

		auto class_type_sym = std::static_pointer_cast<class_symbol>(global_table_->find(name_type));

		auto member_sym = class_type_sym->find_member(member_expr->get_member());
		if (member_sym == nullptr) {
			std::cerr << current_func_name_err_msg()
				<< line_err_msg(member_expr->get_line_pos(), member_expr->get_in_line_pos())
				<< "Class " << name_type << " do not have "
				<< member_expr->get_member() << " member" << std::endl;
			throw std::runtime_error("Semantic error");
		}

		if (base_sym->get_symbol_type() == symbol_type::CLASS_SYM) {
			bool static_member = false;
			if (member_sym->get_symbol_type() == symbol_type::VARIABLE_SYM) {
				auto var_sym = std::static_pointer_cast<var_symbol>(member_sym);
				static_member = var_sym->get_mode() == var_modifier::STATIC_V;
			}
			if (member_sym->get_symbol_type() == symbol_type::SUBROUTINE_SYM) {
				auto subrtn_sym = std::static_pointer_cast<subroutine_symbol>(member_sym);
				static_member = subrtn_sym->get_kind() == subroutine_kind::FUNCTION_S;
			}
			if (!static_member) {
				print_err_msg("Cannot use non-static member with class name",
					member_expr->get_line_pos(),
					member_expr->get_in_line_pos());
				throw std::runtime_error("Semantic error");
			}
		}
	}

	void check_array_member(std::string name_type,
							std::string index_type,
							array_member_expr *array_expr) {
		if (name_type != "Array") {
			print_err_msg("Subscripted object is not \"Array\" type",
				array_expr->get_line_pos(),
				array_expr->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}

		if (index_type != "int") {
			print_err_msg("Array index expression in not an integer",
				array_expr->get_line_pos(),
				array_expr->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}
	}

	subroutine_symbol *check_subroutine_call(symbol_node *name_sym,
											 subroutine_call_expr *sub_call,
											 std::vector<std::string>::iterator arg_begin,
											 std::vector<std::string>::iterator arg_end) {
		if (name_sym->get_symbol_type() != symbol_type::SUBROUTINE_SYM) {
			print_err_msg("Called object is not a subroutine",
						  sub_call->get_line_pos(),
						  sub_call->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}

		auto sub_sym = static_cast<subroutine_symbol*>(name_sym);
		unsigned sub_args_count = sub_sym->get_arg_count();
		unsigned call_args_count = sub_call->get_arg_count();
		if (sub_args_count != call_args_count) {
			std::cerr << current_func_name_err_msg()
				<< line_err_msg(sub_call->get_line_pos(), sub_call->get_in_line_pos())
				<< "Argument count mismatch\n"
				<< "Provided " << call_args_count << "\n"
				<< "In function " << sub_sym->get_name()
				<< " declared arguments count is " << sub_sym->get_arg_count() << std::endl;
			throw std::runtime_error("Semantic error");
		}

		std::string ret_type = sub_sym->get_ret_type();
		unsigned i = 0;
		for (auto&& arg_it = arg_begin; arg_begin != arg_end; ++arg_it, ++i) {
			if (*arg_it != sub_sym->get_arg_type(i)) {
				auto arg_node = sub_call->child(1)->child(i);
				std::cerr << current_func_name_err_msg()
					<< line_err_msg(arg_node->get_line_pos(), arg_node->get_in_line_pos())
					<< "Argument type mismatch\n"
					<< "Provided argument have type: " << *arg_it << "\n"
					<< "In function " << sub_sym->get_name()
					<< " declared type is " << sub_sym->get_arg_type(i) << std::endl;
				throw std::runtime_error("Semantic error");
			}
		}

		return sub_sym;
	}

	void check_unop(value_category val_cat,
					std::string unop_type,
					unop_expr *unop) {
		if (val_cat == value_category::NOT_VALUE) {
			print_err_msg("Operand of unary operation must be value",
				unop->get_line_pos(),
				unop->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}
		if (unop_type != "bool" && unop_type != "int") {
			std::cerr << current_func_name_err_msg()
				<< line_err_msg(unop->get_line_pos(), unop->get_in_line_pos())
				<< "Invalid operand to unary operation\n"
				<< "Operand have type: " << unop_type << std::endl;
			throw std::runtime_error("Semantic error");
		}
	}

	void check_binop(std::string first_op_type,
					 std::string second_op_type,
					 binop_expr *binop) {
		std::string operands_type;
		switch (binop->get_op_type()) {
		case op_type::ADD_OP:
		case op_type::SUB_OP:
		case op_type::DIV_OP:
		case op_type::MUL_OP:
		case op_type::BIT_AND_OP:
		case op_type::BIT_OR_OP:
			operands_type = "int";
			break;
		case op_type::LSS_OP:
		case op_type::GTR_OP:
			operands_type = "int";
			break;
		case op_type::LOG_AND_OP:
		case op_type::LOG_OR_OP:
			operands_type = "bool";
			break;
		}

		if (!first_op_type.empty() || !second_op_type.empty()) {
			print_err_msg("Operands of a binary operation are not a value",
				binop->get_line_pos(), binop->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}

		if (first_op_type != operands_type ||
			second_op_type != operands_type) 
		{
			std::cerr << current_func_name_err_msg()
				<< line_err_msg(binop->get_line_pos(), binop->get_in_line_pos())
				<< "Invalind types of operands in binary operation\n"
				<< "Left operand have type: " << first_op_type << "\n"
				<< "Right operand have type: " << second_op_type << std::endl;
			throw std::runtime_error("Semantic error");
		}
	}

	void check_variable_dec(std::string var_type, variable_dec *var) {
		if (!is_primitive_type(var_type) &&
			global_table_->find(var_type) == nullptr) 
		{
			std::cerr << line_err_msg(var->get_line_pos(), var->get_in_line_pos())
				<< "Undefined type: " << var_type << std::endl;
			throw std::runtime_error("Semantic error");
		}
	}

	void check_let_statement(value_category lhs_category,
							 std::string lhs_type,
							 std::string rhs_type,
							 let_statement *let_stmt) {
		if (lhs_category != value_category::LVALUE) {
			auto lhs_node = let_stmt->child(0);
			print_err_msg("Lvalue required as left operand of let-statement",
				lhs_node->get_line_pos(),
				lhs_node->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}
		if (rhs_type.empty()) {
			auto rhs_node = let_stmt->child(1);
			print_err_msg("Right operand of let-statement must be value",
				rhs_node->get_line_pos(),
				rhs_node->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}
		if (lhs_type != rhs_type) {
			std::cerr << current_func_name_err_msg()
				<< line_err_msg(let_stmt->get_line_pos(), let_stmt->get_in_line_pos())
				<< "Operands type mismatch in let-statement\n"
				<< "Left operand have type: " << lhs_type << '\n'
				<< "Right operand have type: " << rhs_type << std::endl;
			throw std::runtime_error("Semantic error");
		}
	}

	void check_if_statement(std::string condition_type, 
							if_statement *if_stmt) {
		if (condition_type != "bool") {
			auto condition_node = if_stmt->child(0);
			print_err_msg("Condition expression in if-statement must be bool type",
				condition_node->get_line_pos(),
				condition_node->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}
	}

	void check_while_statement(std::string condition_type,
							   while_statement *while_stmt) {
		if (condition_type != "bool") {
			auto condition_node = while_stmt->child(0);
			print_err_msg("Condition expression in while-statement must be bool type",
				condition_node->get_line_pos(),
				condition_node->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}
	}

	void check_do_statement(do_statement *do_stmt) {
		auto sub_call_node = do_stmt->child(0);
		if (sub_call_node->get_type() != node_type::CALL_EXPR) {
			print_err_msg("Do statement must contain call expression",
				do_stmt->get_line_pos(),
				do_stmt->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}
	}

	void check_return_statement(std::string ret_expr_type,
								return_statement *ret_stmt) {
		std::string subrtn_ret_type = current_subrtn_->get_ret_type();
		
		if (subrtn_ret_type == "void" && ret_expr_type.empty()) return;

		if (subrtn_ret_type == "void" && !ret_expr_type.empty()) {
			print_err_msg("Returning value in void function",
				ret_stmt->get_line_pos(),
				ret_stmt->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}
		if (subrtn_ret_type != "void" && ret_expr_type.empty()) {
			print_err_msg("No return value in non-void function",
				ret_stmt->get_line_pos(),
				ret_stmt->get_in_line_pos());
			throw std::runtime_error("Semantic error");
		}

		if (ret_expr_type != subrtn_ret_type) {
			std::cerr << current_func_name_err_msg()
				<< line_err_msg(ret_stmt->get_line_pos(), ret_stmt->get_in_line_pos())
				<< "Return statement with " << ret_expr_type << " expression, "
				<< "in function returning " << subrtn_ret_type << std::endl;
			throw std::runtime_error("Semantic error");
		}
	}

	bool is_primitive_type(std::string type) {
		if (type != "char" &&
			type != "bool" &&
			type != "int")
		{
			return false;
		}
		return true;
	}

	void print_err_msg(const char* msg, unsigned line_pos, unsigned in_line_pos) {
		std::cerr << current_func_name_err_msg()
			<< line_err_msg(line_pos, in_line_pos)
			<< msg << std::endl;
	}

	std::string current_func_name_err_msg() {
		return "In function " + current_subrtn_->get_name() + "\n";
	}

	std::string line_err_msg(unsigned line_pos, unsigned in_line_pos) {
		std::stringstream msg;
		msg << "On line " << line_pos << ":" << in_line_pos << '\n';
		return msg.str();
	}
	
private:
	const symbol_table *global_table_;
	const symbol_table *local_table_;
	const subroutine_symbol *current_subrtn_;
};