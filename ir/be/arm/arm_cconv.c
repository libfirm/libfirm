/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   calling convention helpers
 * @author  Matthias Braun
 */
#include "arm_cconv.h"
#include "beirg.h"
#include "irmode.h"
#include "typerep.h"
#include "xmalloc.h"
#include "panic.h"
#include "util.h"

static const unsigned ignore_regs[] = {
	REG_R12,
	REG_SP,
	REG_PC,
	REG_FL,
};

static const arch_register_t* const param_regs[] = {
	&arm_registers[REG_R0],
	&arm_registers[REG_R1],
	&arm_registers[REG_R2],
	&arm_registers[REG_R3],
};

static const arch_register_t* const result_regs[] = {
	&arm_registers[REG_R0],
	&arm_registers[REG_R1],
	&arm_registers[REG_R2],
	&arm_registers[REG_R3],
};

static const arch_register_t* const float_result_regs[] = {
	&arm_registers[REG_F0],
	&arm_registers[REG_F1],
};

calling_convention_t *arm_decide_calling_convention(const ir_graph *irg,
                                                    ir_type *function_type)
{
	/* determine how parameters are passed */
	unsigned            stack_offset = 0;
	size_t const        n_param_regs = ARRAY_SIZE(param_regs);
	size_t const        n_params     = get_method_n_params(function_type);
	size_t              regnum       = 0;
	reg_or_stackslot_t *params       = XMALLOCNZ(reg_or_stackslot_t, n_params);

	for (size_t i = 0; i < n_params; ++i) {
		ir_type            *param_type = get_method_param_type(function_type,i);
		ir_mode            *mode       = get_type_mode(param_type);
		int                 bits       = get_mode_size_bits(mode);
		reg_or_stackslot_t *param      = &params[i];
		param->type = param_type;

		if (regnum < n_param_regs) {
			const arch_register_t *reg = param_regs[regnum];
			param->reg0       = reg;
			param->reg_offset = regnum;
			++regnum;
		} else {
			param->offset = stack_offset;
			/* increase offset 4 bytes so everything is aligned */
			stack_offset += MAX(bits / 8, 4);
			continue;
		}

		/* we might need a 2nd 32bit component (for 64bit or double values) */
		if (bits > 32) {
			if (bits > 64)
				panic("only 32 and 64bit modes supported");

			if (regnum < n_param_regs) {
				const arch_register_t *reg = param_regs[regnum++];
				param->reg1 = reg;
			} else {
				ir_mode *pmode = param_regs[0]->reg_class->mode;
				ir_type *type  = get_type_for_mode(pmode);
				param->type    = type;
				param->offset  = stack_offset;
				assert(get_mode_size_bits(pmode) == 32);
				stack_offset += 4;
			}
		}
	}
	unsigned const n_param_regs_used = regnum;

	size_t const        n_result_regs= ARRAY_SIZE(result_regs);
	size_t const n_float_result_regs = ARRAY_SIZE(float_result_regs);
	size_t              n_results    = get_method_n_ress(function_type);
	size_t              float_regnum = 0;
	reg_or_stackslot_t *results      = XMALLOCNZ(reg_or_stackslot_t, n_results);
	regnum = 0;
	for (size_t i = 0; i < n_results; ++i) {
		ir_type            *result_type = get_method_res_type(function_type, i);
		ir_mode            *result_mode = get_type_mode(result_type);
		reg_or_stackslot_t *result      = &results[i];

		if (mode_is_float(result_mode)) {
			if (float_regnum >= n_float_result_regs) {
				panic("Too many float results");
			} else {
				const arch_register_t *reg = float_result_regs[float_regnum++];
				result->reg0 = reg;
			}
		} else {
			if (get_mode_size_bits(result_mode) > 32) {
				panic("Results with more than 32bits not supported yet");
			}

			if (regnum >= n_result_regs) {
				panic("Too many results");
			} else {
				const arch_register_t *reg = result_regs[regnum++];
				result->reg0 = reg;
			}
		}
	}

	calling_convention_t *cconv = XMALLOCZ(calling_convention_t);
	cconv->parameters       = params;
	cconv->param_stack_size = stack_offset;
	cconv->n_param_regs     = n_param_regs_used;
	cconv->results          = results;

	/* setup allocatable registers */
	if (irg != NULL) {
		be_irg_t       *birg      = be_birg_from_irg(irg);
		size_t          n_ignores = ARRAY_SIZE(ignore_regs);
		struct obstack *obst      = &birg->obst;

		assert(birg->allocatable_regs == NULL);
		birg->allocatable_regs = rbitset_obstack_alloc(obst, N_ARM_REGISTERS);
		rbitset_set_all(birg->allocatable_regs, N_ARM_REGISTERS);
		for (size_t r = 0; r < n_ignores; ++r) {
			rbitset_clear(birg->allocatable_regs, ignore_regs[r]);
		}
	}

	return cconv;
}

void arm_free_calling_convention(calling_convention_t *cconv)
{
	free(cconv->parameters);
	free(cconv->results);
	free(cconv);
}
