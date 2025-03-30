/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   code selection (transform FIRM into TEMPLATE FIRM)
 */
#include "TEMPLATE_transform.h"

#include "TEMPLATE_new_nodes.h"
#include "TEMPLATE_nodes_attr.h"
#include "beirg.h"
#include "benode.h"
#include "betranshlp.h"
#include "debug.h"
#include "gen_TEMPLATE_regalloc_if.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "panic.h"
#include "util.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef ir_node* (*new_binop_func)(dbg_info *dbgi, ir_node *block,
                                   ir_node *left, ir_node *right);

static ir_node *transform_const(ir_node *const node, ir_entity *const entity, ir_tarval *const value)
{
	ir_node  *const block = be_transform_nodes_block(node);
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	return new_bd_TEMPLATE_Const(dbgi, block, entity, value);
}

static ir_node *transform_binop(ir_node *node, new_binop_func new_func)
{
	ir_node  *new_block = be_transform_nodes_block(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *left      = get_binop_left(node);
	ir_node  *new_left  = be_transform_node(left);
	ir_node  *right     = get_binop_right(node);
	ir_node  *new_right = be_transform_node(right);

	return new_func(dbgi, new_block, new_left, new_right);
}

static ir_node *gen_And(ir_node *node)
{
	return transform_binop(node, new_bd_TEMPLATE_And);
}

static ir_node *gen_Or(ir_node *node)
{
	return transform_binop(node, new_bd_TEMPLATE_Or);
}

static ir_node *gen_Eor(ir_node *node)
{
	return transform_binop(node, new_bd_TEMPLATE_Xor);
}

static ir_node *gen_Div(ir_node *node)
{
#ifndef NDEBUG
	ir_mode *mode = get_Div_resmode(node);
	assert(mode_is_float(mode));
#endif
	return transform_binop(node, new_bd_TEMPLATE_fDiv);
}

static ir_node *gen_Shl(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (get_mode_modulo_shift(mode) != 32)
		panic("modulo shift!=32 not supported");
	return transform_binop(node, new_bd_TEMPLATE_Shl);
}

static ir_node *gen_Shr(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (get_mode_modulo_shift(mode) != 32)
		panic("modulo shift!=32 not supported");
	return transform_binop(node, new_bd_TEMPLATE_Shr);
}

static ir_node *gen_Add(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		return transform_binop(node, new_bd_TEMPLATE_fAdd);
	}

	ir_node *const l = get_Add_left(node);
	ir_node *const r = get_Add_right(node);
	if (is_Address(l) && is_Const(r)) {
		ir_entity *const entity = get_Address_entity(l);
		ir_tarval *const value  = get_Const_tarval(r);
		return transform_const(node, entity, value);
	}

	return transform_binop(node, new_bd_TEMPLATE_Add);
}

static ir_node *gen_Sub(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		return transform_binop(node, new_bd_TEMPLATE_fSub);
	}
	return transform_binop(node, new_bd_TEMPLATE_Sub);
}

static ir_node *gen_Mul(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		return transform_binop(node, new_bd_TEMPLATE_fMul);
	}
	return transform_binop(node, new_bd_TEMPLATE_Mul);
}


typedef ir_node* (*new_unop_func)(dbg_info *dbgi, ir_node *block, ir_node *op);

static ir_node *transform_unop(ir_node *node, int op_index, new_unop_func new_func)
{
	ir_node  *new_block = be_transform_nodes_block(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *op        = get_irn_n(node, op_index);
	ir_node  *new_op    = be_transform_node(op);

	return new_func(dbgi, new_block, new_op);
}

static ir_node *gen_Minus(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		return transform_unop(node, n_Minus_op, new_bd_TEMPLATE_fMinus);
	}
	return transform_unop(node, n_Minus_op, new_bd_TEMPLATE_Minus);
}

static ir_node *gen_Not(ir_node *node)
{
	return transform_unop(node, n_Not_op, new_bd_TEMPLATE_Not);
}

static ir_node *gen_Const(ir_node *node)
{
	ir_tarval *const value = get_Const_tarval(node);
	return transform_const(node, NULL, value);
}

static ir_node *gen_Address(ir_node *node)
{
	ir_entity *const entity = get_Address_entity(node);
	return transform_const(node, entity, NULL);
}

static ir_node *gen_Load(ir_node *node)
{
	ir_node  *new_block = be_transform_nodes_block(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *ptr       = get_Load_ptr(node);
	ir_node  *new_ptr   = be_transform_node(ptr);
	ir_node  *mem       = get_Load_mem(node);
	ir_node  *new_mem   = be_transform_node(mem);
	ir_mode  *mode      = get_Load_mode(node);

	if (mode_is_float(mode)) {
		return new_bd_TEMPLATE_fLoad(dbgi, new_block, new_mem, new_ptr);
	}
	return new_bd_TEMPLATE_Load(dbgi, new_block, new_mem, new_ptr);
}

static ir_node *gen_Store(ir_node *node)
{
	ir_node  *new_block = be_transform_nodes_block(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *ptr       = get_Store_ptr(node);
	ir_node  *new_ptr   = be_transform_node(ptr);
	ir_node  *val       = get_Store_value(node);
	ir_node  *new_val   = be_transform_node(val);
	ir_node  *mem       = get_Store_mem(node);
	ir_node  *new_mem   = be_transform_node(mem);
	ir_mode  *mode      = get_irn_mode(node);

	if (mode_is_float(mode)) {
		return new_bd_TEMPLATE_fStore(dbgi, new_block, new_mem, new_ptr, new_val);
	}
	return new_bd_TEMPLATE_Store(dbgi, new_block, new_mem, new_ptr, new_val);
}

static ir_node *gen_Jmp(ir_node *node)
{
	ir_node  *new_block = be_transform_nodes_block(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	return new_bd_TEMPLATE_Jmp(dbgi, new_block);
}

static unsigned const reg_params[] = {
	REG_R0,
	REG_R1,
	REG_R2,
	REG_R3,
};

static ir_node *gen_Start(ir_node *node)
{
	be_start_out outs[N_TEMPLATE_REGISTERS] = { [REG_SP] = BE_START_IGNORE };

	/* function parameters in registers */
	for (size_t i = 0; i != ARRAY_SIZE(reg_params); ++i) {
		outs[reg_params[i]] = BE_START_REG;
	}

	ir_graph *const irg = get_irn_irg(node);
	return be_new_Start(irg, outs);
}

static ir_node *gen_Return(ir_node *node)
{
	int                               p     = n_TEMPLATE_Return_first_result;
	unsigned                    const n_res = get_Return_n_ress(node);
	unsigned                    const n_ins = p + n_res;
	ir_node                   **const in    = ALLOCAN(ir_node*, n_ins);
	ir_graph                   *const irg   = get_irn_irg(node);
	arch_register_req_t const **const reqs  = be_allocate_in_reqs(irg, n_ins);

	in[n_TEMPLATE_Return_mem]   = be_transform_node(get_Return_mem(node));
	reqs[n_TEMPLATE_Return_mem] = arch_memory_req;

	in[n_TEMPLATE_Return_stack]   = get_irg_frame(irg);
	reqs[n_TEMPLATE_Return_stack] = &TEMPLATE_single_reg_req_gp_sp;

	for (unsigned i = 0; i != n_res; ++p, ++i) {
		ir_node *const res = get_Return_res(node, i);
		in[p]   = be_transform_node(res);
		reqs[p] = arch_get_irn_register_req(in[p])->cls->class_req;
	}

	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const ret   = new_bd_TEMPLATE_Return(dbgi, block, n_ins, in, reqs);
	return ret;
}

static ir_node *gen_Phi(ir_node *node)
{
	ir_mode                   *mode = get_irn_mode(node);
	const arch_register_req_t *req;
	if (be_mode_needs_gp_reg(mode)) {
		req = &TEMPLATE_class_reg_req_gp;
	} else {
		req = arch_memory_req;
	}

	return be_transform_phi(node, req);
}

static ir_node *gen_Proj_Proj(ir_node *node)
{
	ir_node *pred      = get_Proj_pred(node);
	ir_node *pred_pred = get_Proj_pred(pred);
	if (is_Start(pred_pred)) {
		if (get_Proj_num(pred) == pn_Start_T_args) {
			// assume everything is passed in gp registers
			unsigned arg_num = get_Proj_num(node);
			if (arg_num >= ARRAY_SIZE(reg_params))
				panic("more than 4 arguments not supported");
			ir_graph *const irg = get_irn_irg(node);
			return be_get_Start_proj(irg, &TEMPLATE_registers[reg_params[arg_num]]);
		}
	}
	panic("No transformer for %+F -> %+F -> %+F", node, pred, pred_pred);
}

static ir_node *gen_Proj_Load(ir_node *node)
{
	ir_node *load     = get_Proj_pred(node);
	ir_node *new_load = be_transform_node(load);
	switch ((pn_Load)get_Proj_num(node)) {
	case pn_Load_M:
		return be_new_Proj(new_load, pn_TEMPLATE_Load_M);
	case pn_Load_res:
		return be_new_Proj(new_load, pn_TEMPLATE_Load_res);
	case pn_Load_X_regular:
	case pn_Load_X_except:
		panic("exception handling not supported yet");
	}
	panic("invalid Proj %+F -> %+F", node, load);
}

static ir_node *gen_Proj_Store(ir_node *node)
{
	ir_node *store     = get_Proj_pred(node);
	ir_node *new_store = be_transform_node(store);
	switch ((pn_Store)get_Proj_num(node)) {
	case pn_Store_M:
		return new_store;
	case pn_Store_X_regular:
	case pn_Store_X_except:
		panic("exception handling not supported yet");
	}
	panic("invalid Proj %+F -> %+F", node, store);
}

static ir_node *gen_Proj_Start(ir_node *node)
{
	ir_graph *const irg = get_irn_irg(node);
	unsigned  const pn  = get_Proj_num(node);
	switch ((pn_Start)pn) {
	case pn_Start_M:
		return be_get_Start_mem(irg);
	case pn_Start_T_args:
		return new_r_Bad(irg, mode_T);
	case pn_Start_P_frame_base:
		return be_get_Start_proj(irg, &TEMPLATE_registers[REG_SP]);
	}
	panic("unexpected Start proj %u", pn);
}

static void TEMPLATE_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_Add,     gen_Add);
	be_set_transform_function(op_Address, gen_Address);
	be_set_transform_function(op_And,     gen_And);
	be_set_transform_function(op_Const,   gen_Const);
	be_set_transform_function(op_Div,     gen_Div);
	be_set_transform_function(op_Eor,     gen_Eor);
	be_set_transform_function(op_Jmp,     gen_Jmp);
	be_set_transform_function(op_Load,    gen_Load);
	be_set_transform_function(op_Minus,   gen_Minus);
	be_set_transform_function(op_Mul,     gen_Mul);
	be_set_transform_function(op_Not,     gen_Not);
	be_set_transform_function(op_Or,      gen_Or);
	be_set_transform_function(op_Phi,     gen_Phi);
	be_set_transform_function(op_Return,  gen_Return);
	be_set_transform_function(op_Shl,     gen_Shl);
	be_set_transform_function(op_Shr,     gen_Shr);
	be_set_transform_function(op_Start,   gen_Start);
	be_set_transform_function(op_Store,   gen_Store);
	be_set_transform_function(op_Sub,     gen_Sub);
	be_set_transform_proj_function(op_Load,  gen_Proj_Load);
	be_set_transform_proj_function(op_Proj,  gen_Proj_Proj);
	be_set_transform_proj_function(op_Start, gen_Proj_Start);
	be_set_transform_proj_function(op_Store, gen_Proj_Store);
}

static const unsigned ignore_regs[] = {
	REG_SP,
	REG_BP,
};

static void setup_calling_convention(ir_graph *irg)
{
	be_irg_t       *birg = be_birg_from_irg(irg);
	struct obstack *obst = &birg->obst;

	unsigned *allocatable_regs = rbitset_obstack_alloc(obst, N_TEMPLATE_REGISTERS);
	rbitset_set_all(allocatable_regs, N_TEMPLATE_REGISTERS);
	for (size_t r = 0, n = ARRAY_SIZE(ignore_regs); r < n; ++r) {
		rbitset_clear(allocatable_regs, ignore_regs[r]);
	}
	birg->allocatable_regs = allocatable_regs;
}

/**
 * Transform generic IR-nodes into TEMPLATE machine instructions
 */
void TEMPLATE_transform_graph(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_TUPLES
	                         | IR_GRAPH_PROPERTY_NO_BADS);

	TEMPLATE_register_transformers();

	setup_calling_convention(irg);

	be_transform_graph(irg, NULL);
}

void TEMPLATE_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.TEMPLATE.transform");
}
