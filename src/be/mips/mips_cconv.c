/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#include "mips_cconv.h"

#include "betranshlp.h"
#include "gen_mips_regalloc_if.h"
#include "mips_bearch_t.h"
#include "util.h"

static unsigned const regs_param_gp[] = {
	REG_A0,
	REG_A1,
	REG_A2,
	REG_A3,
};

static unsigned const regs_result_gp[] = {
	REG_V0,
	REG_V1,
};

void mips_determine_calling_convention(mips_calling_convention_t *const cconv, ir_type *const fun_type)
{
	/* Handle parameters. */
	mips_reg_or_slot_t *params;
	size_t              gp_param = 0;
	size_t        const n_params = get_method_n_params(fun_type);
	if (n_params != 0) {
		params = XMALLOCNZ(mips_reg_or_slot_t, n_params);

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
					params[i].reg = &mips_registers[regs_param_gp[gp_param]];
				params[i].offset = gp_param * (MIPS_MACHINE_SIZE / 8);
				++gp_param;
			}
		}
	} else {
		params = 0;
	}
	size_t const n_reg_params = ARRAY_SIZE(regs_param_gp);
	cconv->param_stack_size = MAX(n_reg_params, gp_param) * (MIPS_MACHINE_SIZE / 8);
	cconv->n_mem_param      = gp_param > n_reg_params ? gp_param - n_reg_params : 0;
	cconv->parameters       = params;

	/* Handle results. */
	mips_reg_or_slot_t *results;
	size_t        const n_results = get_method_n_ress(fun_type);
	if (n_results != 0) {
		results = XMALLOCNZ(mips_reg_or_slot_t, n_results);

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
				results[i].reg = &mips_registers[regs_result_gp[gp_res++]];
			}
		}
	} else {
		results = 0;
	}
	cconv->results = results;
}

void mips_layout_parameter_entities(mips_calling_convention_t *const cconv, ir_graph *const irg)
{
	ir_entity **const param_map  = be_collect_parameter_entities(irg);
	ir_type    *const frame_type = get_irg_frame_type(irg);
	ir_entity  *const fun_ent    = get_irg_entity(irg);
	ir_type    *const fun_type   = get_entity_type(fun_ent);
	size_t      const n_params   = get_method_n_params(fun_type);
	for (size_t i = 0; i != n_params; ++i) {
		mips_reg_or_slot_t *const param      = &cconv->parameters[i];
		ir_type            *const param_type = get_method_param_type(fun_type, i);
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

void mips_free_calling_convention(mips_calling_convention_t *const cconv)
{
	free(cconv->parameters);
	free(cconv->results);
}
