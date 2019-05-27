/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

#include "be_t.h"
#include "becconv.h"
#include "betranshlp.h"
#include "gen_riscv_regalloc_if.h"
#include "irgwalk.h"
#include "riscv_bearch_t.h"
#include "riscv_cconv.h"
#include "util.h"

static unsigned const regs_param_gp[] = {
	REG_A0,
	REG_A1,
	REG_A2,
	REG_A3,
	REG_A4,
	REG_A5,
	REG_A6,
	REG_A7,
};

static unsigned const regs_result_gp[] = {
	REG_A0,
	REG_A1,
};

static void check_omit_fp(ir_node *node, void *env)
{
	/* omit-fp is not possible if:
	 *  - we have allocations on the stack
	 */
	if (is_Alloc(node) || is_Free(node)) {
		bool *can_omit_fp = (bool*) env;
		*can_omit_fp = false;
	}
}

void riscv_determine_calling_convention(riscv_calling_convention_t *const cconv, ir_type *const fun_type,
                                        ir_graph *const irg)
{
	bool omit_fp = false;
	if (irg != NULL) {
		omit_fp = be_options.omit_fp;
		if (omit_fp)
			irg_walk_graph(irg, check_omit_fp, NULL, &omit_fp);
		riscv_get_irg_data(irg)->omit_fp = omit_fp;
	}

	/* Handle parameters. */
	riscv_reg_or_slot_t *params   = NULL;
	size_t               gp_param = 0;
	size_t         const n_params = get_method_n_params(fun_type);
	if (n_params != 0) {
		params = XMALLOCNZ(riscv_reg_or_slot_t, n_params);

		for (size_t i = 0; i != n_params; ++i) {
			ir_type *const param_type = get_method_param_type(fun_type, i);
			ir_mode *const param_mode = get_type_mode(param_type);
			if (!param_mode) {
				panic("TODO");
			} else if (mode_is_float(param_mode)) {
				panic("TODO");
			} else {
				if (param_type->flags & tf_lowered_dw && gp_param % 2 != 0)
					++gp_param;
				if (gp_param < ARRAY_SIZE(regs_param_gp))
					params[i].reg = &riscv_registers[regs_param_gp[gp_param]];
				params[i].offset = (gp_param - ARRAY_SIZE(regs_param_gp)) * (RISCV_MACHINE_SIZE / 8);
				++gp_param;
			}
		}
	}
	cconv->param_stack_size = gp_param * (RISCV_MACHINE_SIZE / 8);
	cconv->n_mem_param      = gp_param > ARRAY_SIZE(regs_param_gp) ? gp_param - ARRAY_SIZE(regs_param_gp) : 0;
	cconv->parameters       = params;

	/* Handle results. */
	riscv_reg_or_slot_t *results   = NULL;
	size_t         const n_results = get_method_n_ress(fun_type);
	if (n_results != 0) {
		results = XMALLOCNZ(riscv_reg_or_slot_t, n_results);

		size_t gp_res = 0;
		for (size_t i = 0; i != n_results; ++i) {
			ir_type *const res_type = get_method_res_type(fun_type, i);
			ir_mode *const res_mode = get_type_mode(res_type);
			if (!res_mode) {
				panic("TODO");
			} else if (mode_is_float(res_mode)) {
				panic("TODO");
			} else {
				if (gp_res == ARRAY_SIZE(regs_result_gp))
					panic("too many gp results");
				results[i].reg = &riscv_registers[regs_result_gp[gp_res++]];
			}
		}
	}
	cconv->results = results;

	if (irg != NULL) {
		be_irg_t *birg = be_birg_from_irg(irg);
		if (!omit_fp)
			rbitset_clear(birg->allocatable_regs, REG_S0); // s0 = frame pointer
	}
	cconv->omit_fp = omit_fp;
}

void riscv_layout_parameter_entities(riscv_calling_convention_t *const cconv, ir_graph *const irg)
{
	ir_entity **const param_map  = be_collect_parameter_entities(irg);
	ir_type    *const frame_type = get_irg_frame_type(irg);
	ir_entity  *const fun_ent    = get_irg_entity(irg);
	ir_type    *const fun_type   = get_entity_type(fun_ent);
	size_t      const n_params   = get_method_n_params(fun_type);
	for (size_t i = 0; i != n_params; ++i) {
		riscv_reg_or_slot_t *const param      = &cconv->parameters[i];
		ir_type             *const param_type = get_method_param_type(fun_type, i);
		if (!is_atomic_type(param_type))
			panic("unhandled parameter type");
		ir_entity *param_ent = param_map[i];
		if (!param->reg) {
			if (!param_ent)
				param_ent = new_parameter_entity(frame_type, i, param_type);
			assert(get_entity_offset(param_ent) == INVALID_OFFSET);
			set_entity_offset(param_ent, param->offset);
		}
		param->entity = param_ent;
	}
	free(param_map);
}

void riscv_free_calling_convention(riscv_calling_convention_t *const cconv)
{
	free(cconv->parameters);
	free(cconv->results);
}
