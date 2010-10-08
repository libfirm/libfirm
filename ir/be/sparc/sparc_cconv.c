/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   calling convention helpers
 * @author  Matthias Braun
 * @version $Id$
 */
#include "config.h"

#include "sparc_cconv.h"
#include "irmode.h"
#include "irgwalk.h"
#include "typerep.h"
#include "xmalloc.h"
#include "util.h"
#include "error.h"
#include "gen_sparc_regalloc_if.h"

static const unsigned ignore_regs[] = {
	REG_G0,
	/* reserved for sparc ABI: */
	REG_G5,
	REG_G6,
	REG_G7,

	REG_SP,
	REG_O7,
	REG_FRAME_POINTER,
	REG_I7,

	REG_FPFLAGS,
	REG_FLAGS,
	REG_Y,
};

/**
 * Maps an input register representing the i'th register input
 * to the i'th register output.
 */
static const arch_register_t *map_i_to_o_reg(const arch_register_t *reg)
{
	unsigned idx = reg->global_index;
	assert(REG_I0 <= idx && idx <= REG_I7);
	idx += REG_O0 - REG_I0;
	assert(REG_O0 <= idx && idx <= REG_O7);
	return &sparc_registers[idx];
}

static void check_omit_fp(ir_node *node, void *env)
{
	bool *can_omit_fp = (bool*) env;

	/* omit-fp is not possible if:
	 *  - we have allocations on the stack
	 *  - we have calls (with the exception of tail-calls once we support them)
	 */
	if ((is_Alloc(node) && get_Alloc_where(node) == stack_alloc)
			|| (is_Free(node) && get_Free_where(node) == stack_alloc)
			|| is_Call(node)) {
		*can_omit_fp = false;
	}
}

calling_convention_t *sparc_decide_calling_convention(ir_type *function_type,
                                                      ir_graph *irg)
{
	int                   stack_offset        = 0;
	int                   n_param_regs        = ARRAY_SIZE(param_regs);
	int                   n_float_result_regs = ARRAY_SIZE(float_result_regs);
	bool                  omit_fp             = false;
	reg_or_stackslot_t   *params;
	reg_or_stackslot_t   *results;
	int                   n_params;
	int                   n_results;
	int                   i;
	int                   regnum;
	int                   float_regnum;
	calling_convention_t *cconv;

	if (irg != NULL) {
		const be_options_t *options = be_get_irg_options(irg);
		omit_fp = options->omit_fp;
		irg_walk_graph(irg, check_omit_fp, NULL, &omit_fp);
	}

	/* determine how parameters are passed */
	n_params = get_method_n_params(function_type);
	regnum   = 0;
	params   = XMALLOCNZ(reg_or_stackslot_t, n_params);

	for (i = 0; i < n_params; ++i) {
		ir_type            *param_type = get_method_param_type(function_type,i);
		ir_mode            *mode       = get_type_mode(param_type);
		int                 bits       = get_mode_size_bits(mode);
		reg_or_stackslot_t *param      = &params[i];

		if (regnum < n_param_regs) {
			const arch_register_t *reg = param_regs[regnum++];
			if (irg == NULL || omit_fp)
				reg = map_i_to_o_reg(reg);
			param->reg0 = reg;
		} else {
			param->type   = param_type;
			param->offset = stack_offset;
			/* increase offset 4 bytes so everything is aligned */
			stack_offset += bits > 32 ? bits/8 : 4;
			continue;
		}

		/* we might need a 2nd 32bit component (for 64bit or double values) */
		if (bits > 32) {
			if (bits > 64)
				panic("only 32 and 64bit modes supported in sparc backend");

			if (regnum < n_param_regs) {
				const arch_register_t *reg = param_regs[regnum++];
				if (irg == NULL || omit_fp)
					reg = map_i_to_o_reg(reg);
				param->reg1 = reg;
			} else {
				ir_mode *mode = param_regs[0]->reg_class->mode;
				ir_type *type = get_type_for_mode(mode);
				param->type   = type;
				param->offset = stack_offset;
				assert(get_mode_size_bits(mode) == 32);
				stack_offset += 4;
			}
		}
	}

	/* determine how results are passed */
	n_results    = get_method_n_ress(function_type);
	regnum       = 0;
	float_regnum = 0;
	results      = XMALLOCNZ(reg_or_stackslot_t, n_results);
	for (i = 0; i < n_results; ++i) {
		ir_type            *result_type = get_method_res_type(function_type, i);
		ir_mode            *result_mode = get_type_mode(result_type);
		reg_or_stackslot_t *result      = &results[i];

		if (mode_is_float(result_mode)) {
			if (float_regnum >= n_float_result_regs) {
				panic("Too many float results for sparc backend");
			} else {
				const arch_register_t *reg = float_result_regs[float_regnum++];
				result->reg0 = reg;
			}
		} else {
			if (get_mode_size_bits(result_mode) > 32) {
				panic("Results with more than 32bits not supported by sparc backend yet");
			}

			if (regnum >= n_param_regs) {
				panic("Too many results for sparc backend");
			} else {
				const arch_register_t *reg = param_regs[regnum++];
				if (irg == NULL || omit_fp)
					reg = map_i_to_o_reg(reg);
				result->reg0 = reg;
			}
		}
	}

	cconv                   = XMALLOCZ(calling_convention_t);
	cconv->parameters       = params;
	cconv->param_stack_size = stack_offset;
	cconv->results          = results;
	cconv->omit_fp          = omit_fp;

	/* setup ignore register array */
	if (irg != NULL) {
		be_irg_t       *birg      = be_birg_from_irg(irg);
		size_t          n_ignores = ARRAY_SIZE(ignore_regs);
		struct obstack *obst      = &birg->obst;
		size_t          r;

		assert(birg->allocatable_regs == NULL);
		birg->allocatable_regs = rbitset_obstack_alloc(obst, N_SPARC_REGISTERS);
		rbitset_set_all(birg->allocatable_regs, N_SPARC_REGISTERS);
		for (r = 0; r < n_ignores; ++r) {
			rbitset_clear(birg->allocatable_regs, ignore_regs[r]);
		}
	}

	return cconv;
}

void sparc_free_calling_convention(calling_convention_t *cconv)
{
	free(cconv->parameters);
	free(cconv->results);
	free(cconv);
}
