/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

#include "vhdl_emitter.h"

#include "beemithlp.h"
#include "beemitter.h"
#include "gen_vhdl_emitter.h"
#include "gen_vhdl_new_nodes.h"
#include "irgwalk.h"
#include "irnodehashmap.h"
#include "irouts_t.h"
#include "irprintf.h"
#include "panic.h"
#include "plist.h"
#include "vhdl_modes.h"

#include <inttypes.h>
#include <stdbool.h>
#include <strcalc.h>
#include <tv_t.h>

#define VHDL_INT_MAX 2147483647
#define VHDL_INT_MIN -2147483648

static plist_t *variables;
static plist_t *signals;

static plist_t *temp_attrs;

static bool use_barrel_left;
static bool use_barrel_right;
static bool use_barrel_right_signed;

static const char *format_relation(ir_relation rel)
{
	rel &= ~ir_relation_unordered;
	switch (rel) {
		case ir_relation_equal:
			return "=";
		case ir_relation_less:
			return "<";
		case ir_relation_greater:
			return ">";
		case ir_relation_less_equal:
			return "<=";
		case ir_relation_greater_equal:
			return ">=";
		case ir_relation_less_greater:
			return "/=";
		default:
			panic("unsupported_relation");
	}
}

/**
 * Emit function for nodes. For nodes that write a variable or signal, emit only the name of the var/sig instead of an
 * assignment. All other nodes are emitted using the be_emit_node function.
 * This function is called in all node specific emit functions to traverse the graph. Traversing ends at
 * var/sig assigning nodes.
 * @param node node to emit
 */
static void emit_vhdl_node(ir_node const *const node)
{
	if (!is_vhdl_irn(node)) {
		be_emit_node(node);
		return;
	}

	switch (get_vhdl_irn_opcode(node)) {
		case iro_vhdl_AssignVar:
		case iro_vhdl_AssignSig:
		case iro_vhdl_Mux:
			be_emit_irprintf("%s", get_vhdl_varsig_attr_const(node)->name);
			break;
		default:
			be_emit_node(node);
	}
}

static void emit_binop(ir_node const *const node, const char *op)
{
	be_emit_irprintf("(");
	emit_vhdl_node(get_irn_n(node, n_vhdl_Add_left));
	be_emit_irprintf(") %s (", op);
	emit_vhdl_node(get_irn_n(node, n_vhdl_Add_right));
	be_emit_irprintf(")");
}

static void emit_Add(ir_node const *const node)
{
	emit_binop(node, "+");
}

static void emit_And(ir_node const *const node)
{
	emit_binop(node, "and");
}

static void emit_AssignSig(ir_node const *const node)
{
	vhdl_varsig_attr_t const *const attr = get_vhdl_varsig_attr_const(node);
	be_emit_irprintf("\t\t\t%s <= ", attr->name);
	emit_vhdl_node(get_irn_n(node, n_vhdl_AssignSig_val));
	be_emit_irprintf("; -- %+F\n", node);
	be_emit_write_line();
}

static void emit_AssignVar(ir_node const *const node)
{
	vhdl_varsig_attr_t const *const attr = get_vhdl_varsig_attr_const(node);
	be_emit_irprintf("\t\t\t%s := ", attr->name);
	emit_vhdl_node(get_irn_n(node, n_vhdl_AssignVar_val));
	be_emit_irprintf("; -- %+F\n", node);
	be_emit_write_line();
}

static void emit_Block(ir_node const *const node)
{
	be_emit_irprintf("\t\tend if;\n\n");
	be_emit_write_line();
	if (get_irg_end_block(get_irn_irg(node)) == node) {
		return;
	}
	be_emit_irprintf("\t\tif EXEC%d = '1' then -- %+F\n", get_irn_node_nr(node), node);
	be_emit_irprintf("\t\t\tEXEC%d <= '0';\n", get_irn_node_nr(node));
	be_emit_write_line();
}

static void emit_Cmp(ir_node const *const node)
{
	emit_binop(node, format_relation(get_vhdl_cmp_attr_const(node)->rel));
}

static void emit_Cond(ir_node const *const node)
{
	ir_node *cmp = get_irn_n(node, n_vhdl_Cond_cond);
	ir_node *block_true = NULL;
	ir_node *block_false = NULL;

	for (unsigned i = 0; i < get_irn_n_outs(node); i++) {
		ir_node *proj = get_irn_out(node, i);
		unsigned pn = get_Proj_num(proj);

		assert(get_irn_n_outs(proj) == 1);
		ir_node *next_block = get_irn_out(proj, 0);

		if (pn == pn_Cond_true) {
			block_true = next_block;
		} else if (pn == pn_Cond_false) {
			block_false = next_block;
		} else {
			panic("Unexpected Proj(Cond)");
		}
	}

	assert(block_true && block_false);

	be_emit_irprintf("\t\t\tif ");
	emit_vhdl_node(cmp);
	be_emit_irprintf(" then -- %+F\n", node);
	be_emit_irprintf("\t\t\t\tEXEC%N <= '1';\n", block_true);
	be_emit_irprintf("\t\t\telse\n");
	be_emit_irprintf("\t\t\t\tEXEC%N <= '1';\n", block_false);
	be_emit_irprintf("\t\t\tend if;\n");
	be_emit_write_line();
}

#define get_sign_string(mode) mode_is_signed(mode) ? "signed" : "unsigned"

static void emit_Const(ir_node const *const node)
{
	ir_mode *mode = get_irn_mode(node);
	ir_tarval *val = get_vhdl_immediate_attr_const(node)->val;
	unsigned    bits = get_mode_size_bits(get_tarval_mode(val));
	if (tarval_is_long(val) && (get_tarval_long(val) <= VHDL_INT_MAX && get_tarval_long(val) >= VHDL_INT_MIN)) {
		const char *str  = sc_print(val->value, bits, SC_DEC, 0);
		be_emit_irprintf("to_%s(%s, %d)", get_sign_string(mode), str, get_mode_size_bits(mode));
	} else {
		unsigned buffer_len = bits / CHAR_BIT + (bits % CHAR_BIT != 0);
		unsigned char buffer[buffer_len];
		sc_val_to_bytes((const sc_word *) val->value, buffer, buffer_len);

		int pos = 0;
		char buf[bits];
		unsigned char *ptr = (unsigned char *) &buffer;
		for (int i = (int) buffer_len - 1; i >= 0; i--) {
			for (int j = CHAR_BIT - 1; j >= 0; j--) {
				buf[pos++] = '0' + ((ptr[i] & 1U << j) != 0);
			}
		}

		buf[pos] = '\0';

		be_emit_irprintf("\"%s\"", buf);
	}
}

static int is_downconv(ir_mode *src_mode, ir_mode *dest_mode)
{
	return (get_mode_size_bits(dest_mode) < get_mode_size_bits(src_mode));
}

static void emit_Conv(ir_node const *const node)
{
	ir_mode *src_mode = get_irn_mode(get_irn_n(node, n_vhdl_Conv_val));
	ir_mode *dest_mode = get_irn_mode(node);
	if (get_mode_arithmetic(src_mode) == irma_none) {
		// convert from std logic vector to signed/unsigned (entity inputs)
		be_emit_irprintf("%s(", get_sign_string(dest_mode));
		emit_vhdl_node(get_irn_n(node, n_vhdl_Conv_val));
		be_emit_irprintf(")");
		return;
	}
	if (get_mode_arithmetic(dest_mode) == irma_none) {
		// convert to std logic vector from signed/unsigned (entity outputs)
		be_emit_irprintf("std_logic_vector(");
		emit_vhdl_node(get_irn_n(node, n_vhdl_Conv_val));
		be_emit_irprintf(")");
		return;
	}
	bool vhdl_hack = is_downconv(src_mode, dest_mode) && mode_is_signed(src_mode);
	bool change_sign = false;
	if (mode_is_signed(src_mode) != mode_is_signed(dest_mode) || (vhdl_hack && mode_is_signed(dest_mode))) {
		be_emit_irprintf("%s(", get_sign_string(dest_mode));
		change_sign = true;
	}
	if (get_mode_size_bits(dest_mode) != get_mode_size_bits(src_mode)) {
		be_emit_irprintf("resize(", get_sign_string(dest_mode));
		/* IEEE.numeric_std transplants the sign bit on downconv from a
		 * signed type.  Avoid this case to maintain C semantics. */
		if (vhdl_hack) {
			be_emit_irprintf("unsigned(");
		}
		emit_vhdl_node(get_irn_n(node, n_vhdl_Conv_val));
		if (vhdl_hack) {
			be_emit_irprintf(")");
		}
		be_emit_irprintf(", %d)", get_mode_size_bits(dest_mode));
	} else {
		emit_vhdl_node(get_irn_n(node, n_vhdl_Conv_val));
	}

	if (change_sign) {
		be_emit_irprintf(")");
	}
}

static void emit_Eor(ir_node const *const node)
{
	emit_binop(node, "xor");
}

static void emit_Jmp(ir_node const *const node)
{
	assert(get_irn_n_outs(node) == 1);
	ir_node *next_block = get_irn_out(node, 0);
	be_emit_irprintf("\t\t\tEXEC%N <= '1'; -- %+F\n", next_block, node);
	be_emit_write_line();
}


static void emit_Minus(ir_node const *const node)
{
	be_emit_irprintf("- (");
	emit_vhdl_node(get_irn_n(node, n_vhdl_Minus_val));
	be_emit_irprintf(")");
}

static void emit_Mul(ir_node const *const node)
{
	emit_binop(node, "*");
}

static void emit_Mux(ir_node const *const node)
{
	be_emit_irprintf("\t\t\tif ");
	emit_vhdl_node(get_irn_n(node, n_vhdl_Mux_sel));
	be_emit_irprintf(" then -- %+F\n\t\t\t\t%s := (", node, get_vhdl_varsig_attr_const(node)->name);
	emit_vhdl_node(get_irn_n(node, n_vhdl_Mux_t));
	be_emit_irprintf(");\n\t\t\telse\n\t\t\t\t%s := (", get_vhdl_varsig_attr_const(node)->name);
	emit_vhdl_node(get_irn_n(node, n_vhdl_Mux_f));
	be_emit_irprintf(");\n\t\t\tend if;\n");
	be_emit_write_line();
}

static void emit_Not(ir_node const *const node)
{
	be_emit_irprintf("not (");
	emit_vhdl_node(get_irn_n(node, n_vhdl_Not_val));
	be_emit_irprintf(")");
}

static void emit_Or(ir_node const *const node)
{
	emit_binop(node, "or");
}

static void emit_Phi(ir_node const *const node)
{
	// all predecessors should have the same signal name set
	be_emit_irprintf("%s", get_vhdl_varsig_attr_const(get_irn_n(node, 0))->name);
}

static void emit_Proj(ir_node const *const node)
{
	assert(is_vhdl_Start(get_Proj_pred(node)));
	vhdl_start_attr_t const *start_attr = get_vhdl_start_attr_const(get_Proj_pred(node));
	be_emit_irprintf("%s", start_attr->signals[get_Proj_num(node)].name);
}

static void emit_Return(ir_node const *const node)
{
	(void) node;
	be_emit_irprintf("\t\t\tREADY <= '1';\n");
	be_emit_write_line();
}

static void emit_const_shiftop(ir_node const *const node, const char *op)
{
	be_emit_irprintf("%s((", op);
	emit_vhdl_node(get_irn_n(node, n_vhdl_Shl_Const_left));
	// print natural constant for shift_left and shift_right
	assert(is_vhdl_Const(get_irn_n(node, n_vhdl_Shl_Const_right)));
	be_emit_irprintf("), ", op);
	ir_tarval *val = get_vhdl_immediate_attr_const(get_irn_n(node, n_vhdl_Shl_Const_right))->val;
	unsigned    bits = get_mode_size_bits(get_tarval_mode(val));
	const char *str  = sc_print(val->value, bits, SC_DEC, 0);
	be_emit_irprintf("%s", str);
	be_emit_irprintf(")");
}

static void emit_shiftop(ir_node const *const node, const char *op)
{
	be_emit_irprintf("%s((", op);
	emit_vhdl_node(get_irn_n(node, n_vhdl_Shl_Const_left));
	be_emit_irprintf("), (", op);
	emit_vhdl_node(get_irn_n(node, n_vhdl_Shl_Const_right));
	be_emit_irprintf("))");
}

static void emit_Shl_Const(ir_node const *const node)
{
	emit_const_shiftop(node, "shift_left");
}

static void emit_Shl_Barrel(ir_node const *const node)
{
	emit_shiftop(node, "barrel_left");
}

static void emit_Shr_Const(ir_node const *const node)
{
	emit_const_shiftop(node, "shift_right");
}

static void emit_Shr_Barrel(ir_node const *const node)
{
	emit_shiftop(node, "barrel_unsigned_right");
}

static void emit_Shrs_Const(ir_node const *const node)
{
	emit_const_shiftop(node, "shift_right");
}

static void emit_Shrs_Barrel(ir_node const *const node)
{
	emit_shiftop(node, "barrel_signed_right");
}

static void emit_Sub(ir_node const *const node)
{
	emit_binop(node, "-");
}

static void vhdl_register_emitters(void)
{
	be_init_emitters();
	vhdl_register_spec_emitters();
	be_set_emitter(op_Phi,              emit_Phi);
	be_set_emitter(op_Proj,             emit_Proj);
	be_set_emitter(op_vhdl_AssignVar,   emit_AssignVar);
	be_set_emitter(op_vhdl_AssignSig,   emit_AssignSig);
	be_set_emitter(op_vhdl_Add,         emit_Add);
	be_set_emitter(op_vhdl_And,         emit_And);
	be_set_emitter(op_vhdl_Cmp,         emit_Cmp);
	be_set_emitter(op_vhdl_Cond,        emit_Cond);
	be_set_emitter(op_vhdl_Const,       emit_Const);
	be_set_emitter(op_vhdl_Conv,        emit_Conv);
	be_set_emitter(op_vhdl_Eor,         emit_Eor);
	be_set_emitter(op_vhdl_Jmp,         emit_Jmp);
	be_set_emitter(op_vhdl_Minus,       emit_Minus);
	be_set_emitter(op_vhdl_Mul,         emit_Mul);
	be_set_emitter(op_vhdl_Mux,         emit_Mux);
	be_set_emitter(op_vhdl_Not,         emit_Not);
	be_set_emitter(op_vhdl_Or,          emit_Or);
	be_set_emitter(op_vhdl_Return,      emit_Return);
	be_set_emitter(op_vhdl_Shl_Const,   emit_Shl_Const);
	be_set_emitter(op_vhdl_Shl_Barrel,  emit_Shl_Barrel);
	be_set_emitter(op_vhdl_Shr_Const,   emit_Shr_Const);
	be_set_emitter(op_vhdl_Shr_Barrel,  emit_Shr_Barrel);
	be_set_emitter(op_vhdl_Shrs_Const,  emit_Shrs_Const);
	be_set_emitter(op_vhdl_Shrs_Barrel, emit_Shrs_Barrel);
	be_set_emitter(op_vhdl_Sub,         emit_Sub);
}

/**
 * walker function for emitting the graph as VHDL code
 * @param node
 * @param env
 */
static void vhdl_walk_emit_node(ir_node *node, void *env)
{
	(void) env;
	/*
	 * Only start emitting at the following opcodes. Other nodes are visited by the emit function itself.
	 */

	if (!is_vhdl_irn(node)) {
		switch (get_irn_opcode(node)) {
			case iro_Block:
				emit_Block(node);
			default:
				break;
		}
		return;
	}
	switch (get_vhdl_irn_opcode(node)) {
		case iro_vhdl_AssignVar:
		case iro_vhdl_AssignSig:
		case iro_vhdl_Cond:
		case iro_vhdl_Jmp:
		case iro_vhdl_Mux:
		case iro_vhdl_Return:
			be_emit_node(node);
		default:
			break;
	}
}

static void plist_add_sorted_varsig_attr(plist_t *list, vhdl_varsig_attr_t *new)
{
	plist_element_t *current;

	if (plist_count(list) == 0) {
		plist_insert_front(list, new);
		return;
	}

	current = plist_first(list);

	do {
		vhdl_varsig_attr_t *cur_attr = plist_element_get_value(current);
		int cmp = strcmp(cur_attr->name, new->name);
		if (cmp == 0)
			// make sure each name is only inserted once
			return;
		else if (cmp > 0) {
			plist_insert_before(list, current, new);
			return;
		}
	} while (plist_element_has_next(current) && (current = plist_element_get_next(current)));
	plist_insert_after(list, current, new);
}

/**
 * walker function for collecting signal and variable names.
 * AssignVar, AssignSig, Mux and Block nodes set signal and variables. The names are used to get distinct lists of
 * variable and signal names.
 *
 * Check if any barrel shift function is needed in the graph.
 * @param node visited node
 * @param env walker env
 */
static void collect_walk(ir_node *const node, void *env)
{
	(void) env;
	if (is_vhdl_AssignVar(node)) {
		plist_add_sorted_varsig_attr(variables, get_vhdl_varsig_attr(node));
	}
	if (is_vhdl_AssignSig(node)) {
		vhdl_varsig_attr_t *varsig = get_vhdl_varsig_attr(node);
		if (strcmp("OUTPUT0", varsig->name) != 0) {
			// do not include entity output signal in process signals
			plist_add_sorted_varsig_attr(signals, varsig);
		}
	}
	if (is_vhdl_Mux(node)) {
		plist_add_sorted_varsig_attr(variables, get_vhdl_varsig_attr(node));
	}
	if (is_Block(node)) {
		vhdl_varsig_attr_t *sig = malloc(sizeof(vhdl_varsig_attr_t));
		char block_signal[16];
		sprintf(block_signal, "EXEC%ld", get_irn_node_nr(node));
		strncpy(sig->name, block_signal, 16);
		sig->mode = get_mode_std_logic();
		plist_insert_back(temp_attrs, sig);
		plist_add_sorted_varsig_attr(signals, sig);
	}
	// set flags if a barrel function is used
	use_barrel_left = use_barrel_left || is_vhdl_Shl_Barrel(node);
	use_barrel_right = use_barrel_right || is_vhdl_Shr_Barrel(node);
	use_barrel_right_signed = use_barrel_right_signed || is_vhdl_Shrs_Barrel(node);
}

/**
 * terminates a string at the first '.'
 * This is used for entity and architecture names, since dots are not allowed in VHDL entity and architecture names.
 * @param name string
 */
static void fix_arch_name(char *name)
{
	char *location = strchr(name, '.');
	if (location) {
		*location = '\0';
	}
}

static void emit_process_start(ir_graph *irg)
{
	be_emit_irprintf("\t\tif START = '1' then\n");
	be_emit_irprintf("\t\t\tEXEC%N <= '1';\n", get_irg_start_block(irg));
	be_emit_irprintf("\t\t\tREADY <= '0';\n");
	be_emit_write_line();
}

static void emit_barrel_functions(void)
{
	if (use_barrel_left) {
		be_emit_irprintf(
				"\n"
				"\tfunction barrel_left(x : unsigned; n : unsigned) return unsigned is\n"
				"\t\tvariable xvec : std_logic_vector(x'length-1 downto 0);\n"
				"\t\tvariable nvec : std_logic_vector(4 downto 0);\n"
				"\t\tvariable result : std_logic_vector(31 downto 0);\n"
				"\tbegin\n"
				"\t\txvec := std_logic_vector(x);\n"
				"\t\tnvec := std_logic_vector(n(4 downto 0));\n"
				"\t\tresult := xvec;\n"
				"\t\tif nvec(4) = '1' then\n"
				"\t\t\tresult := result(15 downto 0) & \"0000000000000000\";\n"
				"\t\tend if;\n"
				"\t\tif nvec(3) = '1' then\n"
				"\t\t\tresult := result(23 downto 0) & \"00000000\";\n"
				"\t\tend if;\n"
				"\t\tif nvec(2) = '1' then\n"
				"\t\t\tresult := result(27 downto 0) & \"0000\";\n"
				"\t\tend if;\n"
				"\t\tif nvec(1) = '1' then\n"
				"\t\t\tresult := result(29 downto 0) & \"00\";\n"
				"\t\tend if;\n"
				"\t\tif nvec(0) = '1' then\n"
				"\t\t\tresult := result(30 downto 0) & \"0\";\n"
				"\t\tend if;\n"
				"\t\treturn unsigned(result);\n"
				"\tend barrel_left;\n");
	}
	if (use_barrel_right) {
		be_emit_irprintf(
				"\n"
				"\tfunction barrel_unsigned_right(x : unsigned; n : unsigned) return unsigned is\n"
				"\t\tvariable xvec : std_logic_vector(x'length-1 downto 0);\n"
				"\t\tvariable nvec : std_logic_vector(4 downto 0);\n"
				"\t\tvariable result : std_logic_vector(31 downto 0);\n"
				"\tbegin\n"
				"\t\txvec := std_logic_vector(x);\n"
				"\t\tnvec := std_logic_vector(n(4 downto 0));\n"
				"\t\tresult := xvec;\n"
				"\t\tif nvec(4) = '1' then\n"
				"\t\t\tresult := \"0000000000000000\" & result(31 downto 16);\n"
				"\t\tend if;\n"
				"\t\tif nvec(3) = '1' then\n"
				"\t\t\tresult := \"00000000\" & result(31 downto 8);\n"
				"\t\tend if;\n"
				"\t\tif nvec(2) = '1' then\n"
				"\t\t\tresult := \"0000\" & result(31 downto 4);\n"
				"\t\tend if;\n"
				"\t\tif nvec(1) = '1' then\n"
				"\t\t\tresult := \"00\" & result(31 downto 2);\n"
				"\t\tend if;\n"
				"\t\tif nvec(0) = '1' then\n"
				"\t\t\tresult := \"0\" & result(31 downto 1);\n"
				"\t\tend if;\n"
				"\t\treturn unsigned(result);\n"
				"\tend barrel_unsigned_right;\n");
	}
	if (use_barrel_right_signed) {
		be_emit_irprintf(
				"\n"
				"\tfunction barrel_signed_right(x : signed; n : unsigned) return signed is\n"
				"\t\tvariable xvec : std_logic_vector(x'length-1 downto 0);\n"
				"\t\tvariable nvec : std_logic_vector(4 downto 0);\n"
				"\t\tvariable result : std_logic_vector(31 downto 0);\n"
				"\t\tvariable signbit : std_logic_vector(31 downto 0);\n"
				"\tbegin\n"
				"\t\txvec := std_logic_vector(x);\n"
				"\t\tnvec := std_logic_vector(n(4 downto 0));\n"
				"\t\tresult := xvec;\n"
				"\t\tsignbit := (others => xvec(x'length-1));\n"
				"\t\tif nvec(4) = '1' then\n"
				"\t\t\tresult := signbit(15 downto 0) & result(31 downto 16);\n"
				"\t\tend if;\n"
				"\t\tif nvec(3) = '1' then\n"
				"\t\t\tresult := signbit(7 downto 0) & result(31 downto 8);\n"
				"\t\tend if;\n"
				"\t\tif nvec(2) = '1' then\n"
				"\t\t\tresult := signbit(3 downto 0) & result(31 downto 4);\n"
				"\t\tend if;\n"
				"\t\tif nvec(1) = '1' then\n"
				"\t\t\tresult := signbit(1 downto 0) & result(31 downto 2);\n"
				"\t\tend if;\n"
				"\t\tif nvec(0) = '1' then\n"
				"\t\t\tresult := signbit(0 downto 0) & result(31 downto 1);\n"
				"\t\tend if;\n"
				"\t\treturn signed(result);\n"
				"\tend barrel_signed_right;\n");
	}
	be_emit_write_line();
}

/**
 * print the signal or variable initialisation code including bit size and 0-initialized value
 * @param mode mode of the signal or variable
 */
static void print_mode_init_string(ir_mode *mode)
{
	if (mode == mode_b) {
		be_emit_irprintf("boolean := false;\n");
	} else if (mode == get_mode_std_logic()) {
		be_emit_irprintf("std_logic := '0';\n");
	} else {
		const char *type;
		if (be_mode_needs_gp_reg(mode)) {
			type = get_sign_string(mode);
		} else {
			type = "std_logic_vector";
		}
		be_emit_irprintf("%s(%d downto 0) := (others => '0');\n", type, get_mode_size_bits(mode) - 1);
	}
}

static void emit_architecture(ir_graph *irg, char *arch_name)
{
	be_emit_irprintf("architecture %s of %s_ent is\n", arch_name, arch_name);
	foreach_plist(signals, el) {
		vhdl_varsig_attr_t *varsig = plist_element_get_value(el);
		be_emit_irprintf("\tsignal %s : ", varsig->name);
		print_mode_init_string(varsig->mode);
	}

	emit_barrel_functions();

	be_emit_irprintf("begin\n\nprocess (CLK)\n");
	foreach_plist(variables, el) {
		vhdl_varsig_attr_t *varsig = plist_element_get_value(el);
		be_emit_irprintf("\tvariable %s : ", varsig->name);
		print_mode_init_string(varsig->mode);
	}
	be_emit_irprintf("begin\n\tif rising_edge(CLK) then\n");
	emit_process_start(irg);

	be_emit_write_line();
}

static void emit_architecture_end(char *arch_name)
{
	be_emit_irprintf("\tend if;\nend process;\nend %s;", arch_name);
	be_emit_write_line();
}

static void emit_entity(ir_graph *irg, char *arch_name)
{
	be_emit_irprintf(
			"library IEEE;\nuse IEEE.NUMERIC_STD.ALL;\nuse IEEE.STD_LOGIC_1164.ALL;\n\nentity %s_ent is\n\tport(\n",
			arch_name);
	be_emit_irprintf("\t\tCLK     : in  std_logic;\n");

	vhdl_start_attr_t const *start_attr = get_vhdl_start_attr_const(get_irg_start(irg));
	for (int i = 0; i < start_attr->n_signals; i++) {
		be_emit_irprintf("\t\t%s  : in  std_logic_vector(%d downto 0);\n", start_attr->signals[i].name,
		                 get_mode_size_bits(start_attr->signals[i].mode) - 1);
	}

	be_emit_irprintf("\t\tOUTPUT0 : out std_logic_vector(31 downto 0);\n");
	be_emit_irprintf("\t\tSTART   : in  std_logic;\n");
	be_emit_irprintf("\t\tREADY   : out std_logic\n\t);\n");

	be_emit_irprintf("end %s_ent;\n\n", arch_name);
	be_emit_write_line();
}

void vhdl_emit_function(ir_graph *const irg)
{
	use_barrel_left = false;
	use_barrel_right = false;
	use_barrel_right_signed = false;
	vhdl_register_emitters();
	temp_attrs = plist_new();
	variables = plist_new();
	signals = plist_new();

	char name[strlen(get_entity_name(get_irg_entity(irg)))];
	strcpy(name, get_entity_name(get_irg_entity(irg)));
	fix_arch_name(name);
	emit_entity(irg, name);

	irg_walk_blkwise_graph(irg, NULL, collect_walk, NULL);

	emit_architecture(irg, name);

	irg_walk_blkwise_graph(irg, NULL, vhdl_walk_emit_node, NULL);

	emit_architecture_end(name);
	foreach_plist(temp_attrs, element) {
		free(element->data);
	}
	plist_free(temp_attrs);
	plist_free(signals);
	plist_free(variables);
}
