/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

#include "vhdl_emitter.h"

#include "beemithlp.h"
#include "beemitter.h"
#include "cpset.h"
#include "irgwalk.h"
#include "irnodehashmap.h"
#include "irprintf.h"
#include "gen_vhdl_emitter.h"
#include "gen_vhdl_new_nodes.h"
#include "panic.h"
#include "plist.h"
#include "vhdl_new_nodes_t.h"


cpset_t variables;
cpset_t signals;

plist_t *temp_attrs;

static void emit_binop(ir_node const *const node, const char *op) {
	be_emit_irprintf("(");
	be_emit_node(get_irn_n(node, 0));
	be_emit_irprintf(") %s (", op);
	be_emit_node(get_irn_n(node, 1));
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
	be_emit_node(get_irn_n(node, 0));
	be_emit_irprintf("; -- %+F\n", node);
	be_emit_write_line();
}

static void emit_AssignVar(ir_node const *const node) {
	vhdl_varsig_attr_t const *const attr = get_vhdl_varsig_attr_const(node);
	be_emit_irprintf("\t\t\t%s := ", attr->name);
	be_emit_node(get_irn_n(node, 0));
	be_emit_irprintf("; -- %+F\n", node);
	be_emit_write_line();
}

static void emit_Block(ir_node const *const node) {
	be_emit_irprintf("\t\tend if;\n\n");
	be_emit_write_line();
	if (get_irg_end_block(get_irn_irg(node)) == node) {
		return;
	}
	be_emit_irprintf("\t\tif exec%d = '1' then -- %+F\n", get_irn_node_nr(node), node);
	be_emit_irprintf("\t\t\texec%d <= '0';\n", get_irn_node_nr(node));
	be_emit_write_line();
}

static void emit_Const(ir_node const *const node) {
	be_emit_irprintf("%d", get_vhdl_immediate_attr_const(node)->val);
}

static void emit_Eor(ir_node const *const node) {
	emit_binop(node, "xor");
}

static void emit_Mul(ir_node const *const node) {
	emit_binop(node, "*");
}

static void emit_Or(ir_node const *const node) {
	emit_binop(node, "or");
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

static void emit_Sub(ir_node const *const node) {
	emit_binop(node, "-");
}

static void vhdl_register_emitters(void)
{
	be_init_emitters();
	vhdl_register_spec_emitters();
	be_set_emitter(op_Proj,           emit_Proj);
	be_set_emitter(op_vhdl_AssignVar, emit_AssignVar);
	be_set_emitter(op_vhdl_AssignSig, emit_AssignSig);
	be_set_emitter(op_vhdl_Add,       emit_Add);
	be_set_emitter(op_vhdl_And,       emit_And);
	be_set_emitter(op_vhdl_Const,     emit_Const);
	be_set_emitter(op_vhdl_Eor,       emit_Eor);
	be_set_emitter(op_vhdl_Mul,       emit_Mul);
	be_set_emitter(op_vhdl_Or,        emit_Or);
	be_set_emitter(op_vhdl_Return,    emit_Return);
	be_set_emitter(op_vhdl_Sub,       emit_Sub);
}

static void vhdl_emit_node(ir_node *node, void *env) {
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
		cpset_insert(&signals, get_vhdl_varsig_attr(node));
	}
	if (is_Block(node)) {
		vhdl_varsig_attr_t *sig = malloc(sizeof(vhdl_varsig_attr_t));
		char block_signal[16];
		sprintf(block_signal, "exec%ld", get_irn_node_nr(node));
		strncpy(sig->name, block_signal, 16);
		plist_insert_back(temp_attrs, sig);
		cpset_insert(&signals, sig);
	}
}

static void emit_process_start(ir_graph *irg)
{
	be_emit_irprintf("\t\tif START = '1' then\n");
	be_emit_irprintf("\t\t\texec%N <= '1';\n", get_irg_start_block(irg));
	be_emit_irprintf("\t\t\tREADY <= '0';\n");
	be_emit_write_line();
}

static void emit_architecture(ir_graph *irg)
{
	const char *name = get_entity_name(get_irg_entity(irg));
	be_emit_irprintf("architecture %s of %s_ent is\n", name, name);
	cpset_iterator_t it;
	cpset_iterator_init(&it, &signals);
	vhdl_varsig_attr_t *varsig;
	while((varsig = cpset_iterator_next(&it)) != NULL) {
		be_emit_irprintf("\tsignal %s : std_logic := '0';\n", varsig->name);
	}
	be_emit_irprintf("begin\n\nprocess (clk)\n");
	cpset_iterator_init(&it, &variables);
	while((varsig = cpset_iterator_next(&it)) != NULL) {
		be_emit_irprintf("\tvariable %s : unsigned(31 downto 0) := (others => '0');\n", varsig->name);
	}
	be_emit_irprintf("begin\n\tif rising_edge(clk) then\n");
	emit_process_start(irg);

	be_emit_write_line();
}

static void emit_architecture_end(ir_graph *irg) {
	const char *name = get_entity_name(get_irg_entity(irg));
	be_emit_irprintf("\tend if;\nend process;\nend %s;", name);
	be_emit_write_line();
}

void vhdl_emit_function(ir_graph *const irg)
{
	vhdl_register_emitters();
	cpset_init(&variables, hash_ptr, (cpset_cmp_function) vhdl_varsig_attrs_equal);
	cpset_init(&signals, hash_ptr, (cpset_cmp_function) vhdl_varsig_attrs_equal);
	temp_attrs = plist_new();

	irg_walk_blkwise_graph(irg, NULL, collect_walk, NULL);

	emit_architecture(irg);

	irg_walk_blkwise_graph(irg, NULL, vhdl_emit_node, NULL);

	emit_architecture_end(irg);
	cpset_destroy(&variables);
	cpset_destroy(&signals);
	foreach_plist(temp_attrs, element) {
		free(element->data);
	}
	plist_free(temp_attrs);
}
