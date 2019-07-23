/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

#include <bepeephole.h>
#include <be_t.h>
#include "vhdl_transform.h"

#include "betranshlp.h"
#include "gen_vhdl_new_nodes.h"
#include "gen_vhdl_regalloc_if.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irouts_t.h"
#include "panic.h"

typedef ir_node *cons_unop(dbg_info*, ir_node*, ir_node*);
typedef ir_node *cons_binop(dbg_info*, ir_node*, ir_node*, ir_node*);

static ir_node *gen_binop(ir_node *node, cons_binop cons) {
	ir_node *const l    = get_binop_left(node);
	ir_node *const r    = get_binop_right(node);
	ir_mode *const mode = get_irn_mode(node);
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const new_l = be_transform_node(l);
	ir_node *const new_r = be_transform_node(r);
	ir_node *new = cons(dbgi, block, new_l, new_r);
	set_irn_mode(new, mode);
	return new;
}

static ir_node *gen_unop(ir_node *node, cons_unop cons) {
	ir_node *const op    = get_irn_n(node, 0);
	ir_mode *const mode = get_irn_mode(node);
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const new_op = be_transform_node(op);
	ir_node *new = cons(dbgi, block, new_op);
	set_irn_mode(new, mode);
	return new;
}

static ir_node *gen_Add(ir_node *const node)
{
	return gen_binop(node, new_bd_vhdl_Add);
}

static ir_node *gen_And(ir_node *const node)
{
	return gen_binop(node, new_bd_vhdl_And);
}

static ir_node *gen_Cmp(ir_node *const node)
{
	ir_node *const l    = get_binop_left(node);
	ir_node *const r    = get_binop_right(node);
	ir_mode *const mode = get_irn_mode(node);
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const new_l = be_transform_node(l);
	ir_node *const new_r = be_transform_node(r);
	ir_node *new = new_bd_vhdl_Cmp(dbgi, block, new_l, new_r, get_Cmp_relation(node));
	set_irn_mode(new, mode);
	return new;
}

static ir_node *gen_Cond(ir_node *const node)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);

	ir_node *cond = new_bd_vhdl_Cond(dbgi, block, be_transform_node(get_Cond_selector(node)));
	return cond;
}

static ir_node *gen_Conv(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Eor(ir_node *const node)
{
	return gen_binop(node, new_bd_vhdl_Eor);
}

static ir_node *gen_Jmp(ir_node *const node)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	return new_bd_vhdl_Jmp(dbgi, block);
}

static ir_node *gen_Mul(ir_node *const node)
{
	return gen_binop(node, new_bd_vhdl_Mul);
}

static ir_node *gen_Minus(ir_node *const node)
{
	return gen_unop(node, new_bd_vhdl_Minus);
}

static ir_node *gen_Mux(ir_node *const node)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node *const block = be_transform_nodes_block(node);
	ir_node *const new_sel = be_transform_node(get_Mux_sel(node));
	ir_node *const new_true = be_transform_node(get_Mux_true(node));
	ir_node *const new_false = be_transform_node(get_Mux_false(node));
	char var_name[16];
	sprintf(var_name, "mux%ld", get_irn_node_nr(node));
	return new_bd_vhdl_Mux(dbgi, block, new_sel, new_true, new_false, var_name);
}

static ir_node *gen_Not(ir_node *const node) {
	return gen_unop(node, new_bd_vhdl_Not);
}

static ir_node *gen_Or(ir_node *const node)
{
	return gen_binop(node, new_bd_vhdl_Or);
}

static ir_node *gen_Phi(ir_node *const node)
{
	arch_register_req_t const *req;
	ir_mode            *const  mode = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		req = &vhdl_class_reg_req_gp;
	} else if (mode == mode_M) {
		req = arch_memory_req;
	} else {
		panic("unhandled mode");
	}
	return be_transform_phi(node, req);
}

static ir_node *gen_PinnedConst(ir_node *const node)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	return new_bd_vhdl_Const(dbgi, block, NULL, get_tarval_long(get_PinnedConst_tarval(node)));
}

static ir_node *gen_Proj_Proj_Start(ir_node *const node)
{
	ir_node *start = get_irg_start(get_irn_irg(node));
	return be_new_Proj(start, get_Proj_num(node));
}

static ir_node *gen_Proj_Proj(ir_node *const node)
{
	ir_node *const pred      = get_Proj_pred(node);
	ir_node *const pred_pred = get_Proj_pred(pred);
	switch (get_irn_opcode(pred_pred)) {
	case iro_Start: return gen_Proj_Proj_Start(node);
	default:        panic("unexpected Proj-Proj");
	}
}

static ir_node *gen_Proj_Start(ir_node *const node)
{
	ir_graph *const irg = get_irn_irg(node);
	switch ((pn_Start)get_Proj_num(node)) {
		case pn_Start_M:            return be_new_Proj(get_irg_start(irg), pn_Start_M);
		case pn_Start_P_frame_base: return new_r_Bad(irg, mode_P);
		case pn_Start_T_args:       return new_r_Bad(irg, mode_T);
	}
	panic("unexpected Proj");
}

static ir_node *gen_Return(ir_node *const node)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node *val = be_transform_node(get_irn_n(node, 1));

	char name[16];
	sprintf(name, "OUTPUT0");
	ir_node *assign_signal = new_bd_vhdl_AssignSig(dbgi, block, val, name);

	ir_node *ret = new_bd_vhdl_Return(dbgi, block, assign_signal);
	return ret;
}

static ir_node *gen_shift_op(ir_node *const node, cons_binop *const cons, cons_binop *const cons_imm)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const l     = get_binop_left(node);
	ir_node  *const new_l = be_transform_node(l);
	ir_node  *const r     = get_binop_right(node);
	ir_node  *const new_r = be_transform_node(r);
	if (is_PinnedConst(r)) {
		return cons_imm(dbgi, block, new_l, new_r);
	}
	return cons(dbgi, block, new_l, new_r);
}

static ir_node *gen_Shl(ir_node *const node)
{
	return gen_shift_op(node, new_bd_vhdl_Shl_Barrel, new_bd_vhdl_Shl_Const);
}

static ir_node *gen_Shr(ir_node *const node)
{
	return gen_shift_op(node, new_bd_vhdl_Shr_Barrel, new_bd_vhdl_Shr_Const);
}

static ir_node *gen_Shrs(ir_node *const node)
{
	return gen_shift_op(node, new_bd_vhdl_Shrs_Barrel, new_bd_vhdl_Shrs_Const);
}

static ir_node *gen_Start(ir_node *const node)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_type *method_type = get_entity_type(get_irg_entity(get_irn_irg(node)));
	int n_params = get_method_n_params(method_type);
	vhdl_varsig_attr_t *signals = malloc(sizeof(vhdl_varsig_attr_t)*n_params);
	// TODO free later

	ir_node *start = new_bd_vhdl_Start(dbgi, block, n_params + 1, n_params, signals);
	arch_set_irn_register_req_out(start, 0, arch_memory_req);
	for (int i = 0; i < n_params; i++) {
		arch_set_irn_register_req_out(start, i + 1, &vhdl_class_reg_req_gp);
		char param[16];
		sprintf(param, "PARAM%d", i);
		strncpy(signals[i].name, param, 16);
	}

	return start;
}

static ir_node *gen_Sub(ir_node *const node)
{
	return gen_binop(node, new_bd_vhdl_Sub);
}

static void vhdl_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_Add,         gen_Add);
	be_set_transform_function(op_And,         gen_And);
	be_set_transform_function(op_Cmp,         gen_Cmp);
	be_set_transform_function(op_Cond,        gen_Cond);
	be_set_transform_function(op_Conv,        gen_Conv);
	be_set_transform_function(op_Eor,         gen_Eor);
	be_set_transform_function(op_Jmp,         gen_Jmp);
	be_set_transform_function(op_Mul,         gen_Mul);
	be_set_transform_function(op_Minus,       gen_Minus);
	be_set_transform_function(op_Mux,         gen_Mux);
	be_set_transform_function(op_Not,         gen_Not);
	be_set_transform_function(op_Or,          gen_Or);
	be_set_transform_function(op_Phi,         gen_Phi);
	be_set_transform_function(op_PinnedConst, gen_PinnedConst);
	be_set_transform_function(op_Return,      gen_Return);
	be_set_transform_function(op_Shl,         gen_Shl);
	be_set_transform_function(op_Shr,         gen_Shr);
	be_set_transform_function(op_Shrs,        gen_Shrs);
	be_set_transform_function(op_Start,       gen_Start);
	be_set_transform_function(op_Sub,         gen_Sub);

	be_set_transform_proj_function(op_Proj,   gen_Proj_Proj);
	be_set_transform_proj_function(op_Start,  gen_Proj_Start);
}

static void assign_walker(ir_node *const node, void *env) {
	if (is_Block(node)) {
		return;
	}
	(void)env;
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = get_nodes_block(node);
	// assign phi operands in signals
	if (is_Phi(node) && mode_is_int(get_irn_mode(node))) {
		for (int i = 0; i < get_irn_arity(node); i++) {
			ir_node *pred = get_irn_n(node, i);

			if (!is_vhdl_AssignSig(pred) && pred != node) {
				char sig_name[16];
				sprintf(sig_name, "PHI%ld", get_irn_node_nr(node));
				ir_node *assign = new_bd_vhdl_AssignSig(dbgi, get_nodes_block(pred), pred, sig_name);
				set_irn_n(node, i, assign);
			}
		}
	}
	// store results with multiple users in a temporary variable
	if (!be_has_only_one_user(node)) {
		if (!is_vhdl_irn(node)) {
			return;
		}
		switch(get_vhdl_irn_opcode(node)) {
			case iro_vhdl_AssignVar:
			case iro_vhdl_AssignSig:
			case iro_vhdl_Cond:
			case iro_vhdl_Const:
			case iro_vhdl_Mux:
			case iro_vhdl_Start:
				return;
			default:
				break;
		}
		char var_name[16];
		sprintf(var_name, "node%ld", get_irn_node_nr(node));
		ir_node *assign = new_bd_vhdl_AssignVar(dbgi, block, node, var_name);
		foreach_irn_out(node, i, succ) {
			if (succ == assign) {
				continue;
			}
			for (int j = 0; j < get_irn_arity(succ); j++) {
				if (get_irn_n(succ, j) == node) {
					set_irn_n(succ, j, assign);
				}
			}
		}
	}
}

void vhdl_transform_graph(ir_graph *const irg)
{
	vhdl_register_transformers();
	be_transform_graph(irg, NULL);
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
	be_dump(DUMP_BE, irg, "before-assigns");
	irg_walk_graph(irg, NULL, assign_walker, NULL);
}
