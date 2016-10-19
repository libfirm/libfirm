/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#include "mips_transform.h"

#include "beirg.h"
#include "benode.h"
#include "betranshlp.h"
#include "gen_mips_new_nodes.h"
#include "gen_mips_regalloc_if.h"
#include "mips_cconv.h"
#include "nodes.h"
#include "panic.h"
#include "util.h"

static mips_calling_convention_t cur_cconv;

static be_stack_env_t stack_env;

static unsigned const callee_saves[] = {
	REG_S0,
	REG_S1,
	REG_S2,
	REG_S3,
	REG_S4,
	REG_S5,
	REG_S6,
	REG_S7,
	REG_S8,
};

static unsigned const caller_saves[] = {
	REG_V0,
	REG_V1,
	REG_A0,
	REG_A1,
	REG_A2,
	REG_A3,
	REG_T0,
	REG_T1,
	REG_T2,
	REG_T3,
	REG_T4,
	REG_T5,
	REG_T6,
	REG_T7,
	REG_T8,
	REG_T9,
	REG_RA,
};

static ir_node *get_Start_sp(ir_graph *const irg)
{
	return be_get_Start_proj(irg, &mips_registers[REG_SP]);
}

static ir_node *get_Start_zero(ir_graph *const irg)
{
	return be_get_Start_proj(irg, &mips_registers[REG_ZERO]);
}

static ir_node *gen_Const(ir_node *const node)
{
	ir_mode *const mode = get_irn_mode(node);;
	if (be_mode_needs_gp_reg(mode)) {
		long const val = get_Const_long(node);
		if (val == 0) {
			ir_graph *const irg = get_irn_irg(node);
			return get_Start_zero(irg);
		}
	}
	panic("TODO");
}

static ir_node *gen_Proj_Start(ir_node *const node)
{
	ir_graph *const irg = get_irn_irg(node);
	switch ((pn_Start)get_Proj_num(node)) {
	case pn_Start_M:            return be_get_Start_mem(irg);
	case pn_Start_P_frame_base: return get_Start_sp(irg);
	case pn_Start_T_args:       return new_r_Bad(irg, mode_T);
	}
	panic("unexpected Proj");
}

static ir_node *gen_Return(ir_node *const node)
{
	unsigned       p     = n_mips_ret_first_result;
	unsigned const n_res = get_Return_n_ress(node);
	unsigned const n_ins = p + n_res + ARRAY_SIZE(callee_saves);

	ir_graph                   *const irg  = get_irn_irg(node);
	arch_register_req_t const **const reqs = be_allocate_in_reqs(irg, n_ins);
	ir_node                   **const in   = ALLOCAN(ir_node*, n_ins);

	ir_node *const mem = get_Return_mem(node);
	in[n_mips_ret_mem]   = be_transform_node(mem);
	reqs[n_mips_ret_mem] = arch_memory_req;

	in[n_mips_ret_stack]   = get_Start_sp(irg);
	reqs[n_mips_ret_stack] = &mips_single_reg_req_gp_sp;

	in[n_mips_ret_addr]    = be_get_Start_proj(irg, &mips_registers[REG_RA]);
	reqs[n_mips_ret_addr]  = &mips_class_reg_req_gp;

	mips_reg_or_slot_t *const results = cur_cconv.results;
	for (size_t i = 0; i != n_res; ++i) {
		ir_node *const res = get_Return_res(node, i);
		in[p]   = be_transform_node(res);
		reqs[p] = results[i].reg->single_req;
		++p;
	}

	for (size_t i = 0; i != ARRAY_SIZE(callee_saves); ++i) {
		arch_register_t const *const reg = &mips_registers[callee_saves[i]];
		in[p]   = be_get_Start_proj(irg, reg);
		reqs[p] = reg->single_req;
		++p;
	}

	assert(p == n_ins);
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const ret   = new_bd_mips_ret(dbgi, block, n_ins, in, reqs);
	be_stack_record_chain(&stack_env, ret, n_mips_ret_stack, NULL);
	return ret;
}

static ir_node *gen_Start(ir_node *const node)
{
	be_start_out outs[N_MIPS_REGISTERS] = {
		[REG_ZERO] = BE_START_IGNORE,
		[REG_SP]   = BE_START_IGNORE,
		[REG_RA]   = BE_START_REG,
	};
	for (size_t i = 0; i != ARRAY_SIZE(callee_saves); ++i) {
		outs[callee_saves[i]] = BE_START_REG;
	}

	ir_graph *const irg = get_irn_irg(node);
	return be_new_Start(irg, outs);
}

static void mips_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_Const,  gen_Const);
	be_set_transform_function(op_Return, gen_Return);
	be_set_transform_function(op_Start,  gen_Start);

	be_set_transform_proj_function(op_Start, gen_Proj_Start);
}

static void mips_set_allocatable_regs(ir_graph *const irg)
{
	be_irg_t       *const birg = be_birg_from_irg(irg);
	struct obstack *const obst = &birg->obst;

	unsigned *const a = rbitset_obstack_alloc(obst, N_MIPS_REGISTERS);
	for (size_t i = 0; i != ARRAY_SIZE(callee_saves); ++i) {
		rbitset_set(a, callee_saves[i]);
	}
	for (size_t i = 0; i != ARRAY_SIZE(caller_saves); ++i) {
		rbitset_set(a, caller_saves[i]);
	}
	birg->allocatable_regs = a;
}

void mips_transform_graph(ir_graph *const irg)
{
	mips_register_transformers();
	mips_set_allocatable_regs(irg);
	be_stack_init(&stack_env);

	ir_entity *const fun_ent  = get_irg_entity(irg);
	ir_type   *const fun_type = get_entity_type(fun_ent);
	mips_determine_calling_convention(&cur_cconv, fun_type);
	be_transform_graph(irg, NULL);
	mips_free_calling_convention(&cur_cconv);

	be_stack_finish(&stack_env);
}
