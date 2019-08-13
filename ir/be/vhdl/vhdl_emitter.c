/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

#include "vhdl_emitter.h"

#include "beemithlp.h"
#include "beemitter.h"
#include "cpset.h"
#include "gen_vhdl_emitter.h"
#include "gen_vhdl_new_nodes.h"
#include "irgwalk.h"
#include "irnodehashmap.h"
#include "irouts_t.h"
#include "irprintf.h"
#include "panic.h"
#include "plist.h"
#include "vhdl_new_nodes_t.h"

#include <stdbool.h>


static cpset_t variables;
static cpset_t signals;

static plist_t *temp_attrs;

static bool use_barrel_left;
static bool use_barrel_right;
static bool use_barrel_right_signed;

static const char *format_relation(ir_relation rel)
{
	switch(rel) {
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

static void emit_vhdl_node(ir_node const *const node) {
	if (!is_vhdl_irn(node)) {
		be_emit_node(node);
		return;
	}

	switch(get_vhdl_irn_opcode(node)) {
		case iro_vhdl_AssignVar:
		case iro_vhdl_AssignSig:
		case iro_vhdl_Mux:
			be_emit_irprintf("%s", get_vhdl_varsig_attr_const(node)->name);
			break;
		default:
			be_emit_node(node);
	}
}

static void emit_binop(ir_node const *const node, const char *op) {
	be_emit_irprintf("(");
	emit_vhdl_node(get_irn_n(node, n_vhdl_Add_left));
	be_emit_irprintf(") %s (", op);
	emit_vhdl_node(get_irn_n(node, n_vhdl_Add_right));
	be_emit_irprintf(")");
}

static void emit_Add(ir_node const *const node) {
	emit_binop(node, "+");
}

static void emit_And(ir_node const *const node) {
	emit_binop(node, "and");
}

static void emit_AssignSig(ir_node const *const node) {
	vhdl_varsig_attr_t const *const attr = get_vhdl_varsig_attr_const(node);
	be_emit_irprintf("\t\t\t%s <= ", attr->name);
	emit_vhdl_node(get_irn_n(node, n_vhdl_AssignSig_val));
	be_emit_irprintf("; -- %+F\n", node);
	be_emit_write_line();
}

static void emit_AssignVar(ir_node const *const node) {
	vhdl_varsig_attr_t const *const attr = get_vhdl_varsig_attr_const(node);
	be_emit_irprintf("\t\t\t%s := ", attr->name);
	emit_vhdl_node(get_irn_n(node, n_vhdl_AssignVar_val));
	be_emit_irprintf("; -- %+F\n", node);
	be_emit_write_line();
}

static void emit_Block(ir_node const *const node) {
	be_emit_irprintf("\t\tend if;\n\n");
	be_emit_write_line();
	if (get_irg_end_block(get_irn_irg(node)) == node) {
		return;
	}
	be_emit_irprintf("\t\tif EXEC%d = '1' then -- %+F\n", get_irn_node_nr(node), node);
	be_emit_irprintf("\t\t\tEXEC%d <= '0';\n", get_irn_node_nr(node));
	be_emit_write_line();
}

static void emit_Cmp(ir_node const *const node) {
	emit_binop(node, format_relation(get_vhdl_cmp_attr_const(node)->rel));
}

static void emit_Cond(ir_node const *const node) {
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

static void emit_Const(ir_node const *const node) {
	be_emit_irprintf("%d", get_vhdl_immediate_attr_const(node)->val);
}

static int is_downconv(ir_mode *src_mode, ir_mode *dest_mode)
{
	return (get_mode_size_bits(dest_mode) < get_mode_size_bits(src_mode));
}

#define get_sign_string(mode) mode_is_signed(dest_mode) ? "signed" : "unsigned"

static void emit_Conv(ir_node const *const node) {
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
	bool change_sign = false;
	if (mode_is_signed(src_mode) != mode_is_signed(dest_mode)) {
		be_emit_irprintf("%s(", get_sign_string(dest_mode));
		change_sign = true;
	}
	if (get_mode_size_bits(dest_mode) != get_mode_size_bits(src_mode)) {
		be_emit_irprintf("resize(", get_sign_string(dest_mode));
		/* IEEE.numeric_std transplants the sign bit on downconv from a
		 * signed type.  Avoid this case to maintain C semantics. */
		bool vhdl_hack = is_downconv(src_mode, dest_mode) && mode_is_signed(src_mode);
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

static void emit_Eor(ir_node const *const node) {
	emit_binop(node, "xor");
}

static void emit_Jmp(ir_node const *const node) {
	assert(get_irn_n_outs(node) == 1);
	ir_node *next_block = get_irn_out(node, 0);
	be_emit_irprintf("\t\t\tEXEC%N <= '1'; -- %+F\n", next_block, node);
	be_emit_write_line();
}


static void emit_Minus(ir_node const *const node) {
	be_emit_irprintf("- (");
	emit_vhdl_node(get_irn_n(node, n_vhdl_Minus_val));
	be_emit_irprintf(")");
}

static void emit_Mul(ir_node const *const node) {
	emit_binop(node, "*");
}

static void emit_Mux(ir_node const *const node) {
	be_emit_irprintf("\t\t\tif ");
	emit_vhdl_node(get_irn_n(node, n_vhdl_Mux_sel));
	be_emit_irprintf(" then -- %+F\n\t\t\t\t%s := (", node, get_vhdl_varsig_attr_const(node)->name);
	emit_vhdl_node(get_irn_n(node, n_vhdl_Mux_t));
	be_emit_irprintf(");\n\t\t\telse\n\t\t\t\t%s := (", get_vhdl_varsig_attr_const(node)->name);
	emit_vhdl_node(get_irn_n(node, n_vhdl_Mux_f));
	be_emit_irprintf(");\n\t\t\tend if;\n");
	be_emit_write_line();
}

static void emit_Not(ir_node const *const node) {
	be_emit_irprintf("not (");
	emit_vhdl_node(get_irn_n(node, n_vhdl_Not_val));
	be_emit_irprintf(")");
}

static void emit_Or(ir_node const *const node) {
	emit_binop(node, "or");
}

static void emit_Phi(ir_node const *const node) {
	// all predecessors should have the same signal name set
	be_emit_irprintf("%s", get_vhdl_varsig_attr_const(get_irn_n(node, 0))->name);
}

static void emit_Proj(ir_node const *const node) {
	assert(is_vhdl_Start(get_Proj_pred(node)));
	vhdl_start_attr_t const *start_attr = get_vhdl_start_attr_const(get_Proj_pred(node));
	be_emit_irprintf("%s", start_attr->signals[get_Proj_num(node)].name);
}

static void emit_Return(ir_node const *const node) {
	(void)node;
	be_emit_irprintf("\t\t\tREADY <= '1';\n");
	be_emit_write_line();
}

static void emit_shiftop(ir_node const *const node, const char *op) {
	be_emit_irprintf("%s((", op);
	emit_vhdl_node(get_irn_n(node, n_vhdl_Shl_Const_left));
	be_emit_irprintf(") , (", op);
	emit_vhdl_node(get_irn_n(node, n_vhdl_Shl_Const_right));
	be_emit_irprintf("))");
	//TODO casts
}

static void emit_Shl_Const(ir_node const *const node) {
	emit_shiftop(node, "shift_left");
}

static void emit_Shl_Barrel(ir_node const *const node) {
	emit_shiftop(node, "barrel_left");
}

static void emit_Shr_Const(ir_node const *const node) {
	emit_shiftop(node, "shift_right");
}

static void emit_Shr_Barrel(ir_node const *const node) {
	emit_shiftop(node, "barrel_right");
}

static void emit_Shrs_Const(ir_node const *const node) {
	emit_shiftop(node, "shift_right");
}

static void emit_Shrs_Barrel(ir_node const *const node) {
	emit_shiftop(node, "barrel_right");
}

static void emit_Sub(ir_node const *const node) {
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

static void vhdl_walk_emit_node(ir_node *node, void *env) {
	(void)env;
	if (!is_vhdl_irn(node)) {
		switch(get_irn_opcode(node)) {
			case iro_Block:
				emit_Block(node);
			default:
				break;
		}
		//be_emit_node(node);
		return;
	}
	switch(get_vhdl_irn_opcode(node)) {
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

static void collect_walk(ir_node *const node, void *env)
{
	(void)env;
	if (is_vhdl_AssignVar(node)) {
		cpset_insert(&variables, get_vhdl_varsig_attr(node));
	}
	if (is_vhdl_AssignSig(node)) {
		vhdl_varsig_attr_t *varsig = get_vhdl_varsig_attr(node);
		if (strcmp("OUTPUT0", varsig->name) != 0) {
			// do not include entity output signal in process signals
			cpset_insert(&signals, varsig);
		}
	}
	if (is_vhdl_Mux(node)) {
		cpset_insert(&variables, get_vhdl_varsig_attr(node));
	}
	if (is_Block(node)) {
		vhdl_varsig_attr_t *sig = malloc(sizeof(vhdl_varsig_attr_t));
		char block_signal[16];
		sprintf(block_signal, "EXEC%ld", get_irn_node_nr(node));
		strncpy(sig->name, block_signal, 16);
		plist_insert_back(temp_attrs, sig);
		cpset_insert(&signals, sig);
	}
	use_barrel_left = use_barrel_left || is_vhdl_Shl_Barrel(node);
	use_barrel_right = use_barrel_right || is_vhdl_Shr_Barrel(node);
	use_barrel_right_signed = use_barrel_right_signed || is_vhdl_Shrs_Barrel(node);
}

static void get_arch_name(ir_graph *irg, char *name) {
	strcpy(name, get_entity_name(get_irg_entity(irg)));
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

static void emit_barrel_functions(void) {
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

static void emit_architecture(ir_graph *irg, char *arch_name)
{
	be_emit_irprintf("architecture %s of %s_ent is\n", arch_name, arch_name);
	cpset_iterator_t it;
	cpset_iterator_init(&it, &signals);
	vhdl_varsig_attr_t *varsig;
	while((varsig = cpset_iterator_next(&it)) != NULL) {
		be_emit_irprintf("\tsignal %s : std_logic := '0';\n", varsig->name);
	}

	emit_barrel_functions();

	be_emit_irprintf("begin\n\nprocess (CLK)\n");
	cpset_iterator_init(&it, &variables);
	while((varsig = cpset_iterator_next(&it)) != NULL) {
		be_emit_irprintf("\tvariable %s : unsigned(31 downto 0) := (others => '0');\n", varsig->name);
	}
	be_emit_irprintf("begin\n\tif rising_edge(CLK) then\n");
	emit_process_start(irg);

	be_emit_write_line();
}

static void emit_architecture_end(char *arch_name) {
	be_emit_irprintf("\tend if;\nend process;\nend %s;", arch_name);
	be_emit_write_line();
}

static void emit_entity(ir_graph *irg, char *arch_name) {
	be_emit_irprintf("library IEEE;\nuse IEEE.NUMERIC_STD.ALL;\nuse IEEE.STD_LOGIC_1164.ALL;\n\nentity %s_ent is\n\tport(\n", arch_name);
	be_emit_irprintf("\t\tCONTROL : in  std_logic_vector(7 downto 0);\n");
	be_emit_irprintf("\t\tCLK     : in  std_logic;\n");

	vhdl_start_attr_t const *start_attr = get_vhdl_start_attr_const(get_irg_start(irg));
	for (int i = 0; i < start_attr->n_signals; i++) {
		//TODO use correct data type for input and output
		be_emit_irprintf("\t\t%s  : in  std_logic_vector(31 downto 0);\n", start_attr->signals[i].name);
	}

	be_emit_irprintf("\t\tOUTPUT0 : out std_logic_vector(31 downto 0);\n");
	be_emit_irprintf("\t\tSTART   : in  std_logic;\n");
	be_emit_irprintf("\t\tREADY   : out std_logic\n\t);\n");

	be_emit_irprintf("end %s_ent;\n\n", arch_name);
	be_emit_write_line();
}

static unsigned hash_varsig_attr(void *data) {
	return hash_data(data, sizeof(vhdl_varsig_attr_t));
}

void vhdl_emit_function(ir_graph *const irg)
{
	use_barrel_left = false;
	use_barrel_right = false;
	use_barrel_right_signed = false;
	vhdl_register_emitters();
	cpset_init(&variables, (cpset_hash_function) hash_varsig_attr, (cpset_cmp_function) vhdl_varsig_attrs_equal_);
	cpset_init(&signals, (cpset_hash_function) hash_varsig_attr, (cpset_cmp_function) vhdl_varsig_attrs_equal_);
	temp_attrs = plist_new();

	char name[strlen(get_entity_name(get_irg_entity(irg)))];
	get_arch_name(irg, name);
	emit_entity(irg, name);

	irg_walk_blkwise_graph(irg, NULL, collect_walk, NULL);

	emit_architecture(irg, name);

	irg_walk_blkwise_graph(irg, NULL, vhdl_walk_emit_node, NULL);

	emit_architecture_end(name);
	cpset_destroy(&variables);
	cpset_destroy(&signals);
	foreach_plist(temp_attrs, element) {
		free(element->data);
	}
	plist_free(temp_attrs);
}
