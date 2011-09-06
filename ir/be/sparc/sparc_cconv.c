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
#include "bitfiddle.h"

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

static const arch_register_t* const param_regs[] = {
	&sparc_registers[REG_I0],
	&sparc_registers[REG_I1],
	&sparc_registers[REG_I2],
	&sparc_registers[REG_I3],
	&sparc_registers[REG_I4],
	&sparc_registers[REG_I5],
};
COMPILETIME_ASSERT(ARRAY_SIZE(param_regs) == SPARC_N_PARAM_REGS, sparcparamregs)

static const arch_register_t* const float_result_regs[] = {
	&sparc_registers[REG_F0],
	&sparc_registers[REG_F1],
	&sparc_registers[REG_F2],
	&sparc_registers[REG_F3],
	&sparc_registers[REG_F4],
	&sparc_registers[REG_F5],
	&sparc_registers[REG_F6],
	&sparc_registers[REG_F7],
};
static arch_register_req_t float_result_reqs_double[8];
static arch_register_req_t float_result_reqs_quad[8];

static const arch_register_t *const caller_saves[] = {
	&sparc_registers[REG_G1],
	&sparc_registers[REG_G2],
	&sparc_registers[REG_G3],
	&sparc_registers[REG_G4],
	&sparc_registers[REG_O0],
	&sparc_registers[REG_O1],
	&sparc_registers[REG_O2],
	&sparc_registers[REG_O3],
	&sparc_registers[REG_O4],
	&sparc_registers[REG_O5],
	&sparc_registers[REG_F0],
	&sparc_registers[REG_F1],
	&sparc_registers[REG_F2],
	&sparc_registers[REG_F3],
	&sparc_registers[REG_F4],
	&sparc_registers[REG_F5],
	&sparc_registers[REG_F6],
	&sparc_registers[REG_F7],
	&sparc_registers[REG_F8],
	&sparc_registers[REG_F9],
	&sparc_registers[REG_F10],
	&sparc_registers[REG_F11],
	&sparc_registers[REG_F12],
	&sparc_registers[REG_F13],
	&sparc_registers[REG_F14],
	&sparc_registers[REG_F15],
	&sparc_registers[REG_F16],
	&sparc_registers[REG_F17],
	&sparc_registers[REG_F18],
	&sparc_registers[REG_F19],
	&sparc_registers[REG_F20],
	&sparc_registers[REG_F21],
	&sparc_registers[REG_F22],
	&sparc_registers[REG_F23],
	&sparc_registers[REG_F24],
	&sparc_registers[REG_F25],
	&sparc_registers[REG_F26],
	&sparc_registers[REG_F27],
	&sparc_registers[REG_F28],
	&sparc_registers[REG_F29],
	&sparc_registers[REG_F30],
	&sparc_registers[REG_F31],
};
static unsigned default_caller_saves[BITSET_SIZE_ELEMS(N_SPARC_REGISTERS)];

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

static unsigned determine_n_float_regs(ir_mode *mode)
{
	unsigned bits = get_mode_size_bits(mode);
	switch (bits) {
	case 32:
		return 1;
	case 64:
		return 2;
	case 128:
		return 4;
	default:
		panic("sparc: Unexpected floatingpoint mode %+F", mode);
	}
}

calling_convention_t *sparc_decide_calling_convention(ir_type *function_type,
                                                      ir_graph *irg)
{
	unsigned              stack_offset        = 0;
	unsigned              n_param_regs_used   = 0;
	int                   n_param_regs        = ARRAY_SIZE(param_regs);
	unsigned              n_float_result_regs = ARRAY_SIZE(float_result_regs);
	bool                  omit_fp             = false;
	reg_or_stackslot_t   *params;
	reg_or_stackslot_t   *results;
	int                   n_params;
	int                   n_results;
	int                   i;
	int                   regnum;
	unsigned              float_regnum;
	unsigned              n_reg_results = 0;
	calling_convention_t *cconv;
	unsigned             *caller_saves;

	if (irg != NULL) {
		const be_options_t *options = be_get_irg_options(irg);
		omit_fp = options->omit_fp;
		irg_walk_graph(irg, check_omit_fp, NULL, &omit_fp);
	}

	caller_saves = rbitset_malloc(N_SPARC_REGISTERS);
	rbitset_copy(caller_saves, default_caller_saves, N_SPARC_REGISTERS);

	/* determine how parameters are passed */
	n_params = get_method_n_params(function_type);
	regnum   = 0;
	params   = XMALLOCNZ(reg_or_stackslot_t, n_params);

	for (i = 0; i < n_params; ++i) {
		ir_type            *param_type = get_method_param_type(function_type,i);
		ir_mode            *mode;
		int                 bits;
		reg_or_stackslot_t *param;

		if (is_compound_type(param_type))
			panic("sparc: compound arguments not supported yet");

		mode  = get_type_mode(param_type);
		bits  = get_mode_size_bits(mode);
		param = &params[i];

		if (i == 0 &&
		    (get_method_calling_convention(function_type) & cc_compound_ret)) {
			assert(mode_is_reference(mode) && bits == 32);
			/* special case, we have reserved space for this on the between
			 * type */
			param->type   = param_type;
			param->offset = -SPARC_MIN_STACKSIZE+SPARC_AGGREGATE_RETURN_OFFSET;
			continue;
		}

		if (regnum < n_param_regs) {
			const arch_register_t *reg = param_regs[regnum];
			if (irg == NULL || omit_fp)
				reg = map_i_to_o_reg(reg);
			param->reg0       = reg;
			param->req0       = reg->single_req;
			param->reg_offset = regnum;
			++regnum;
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
				const arch_register_t *reg = param_regs[regnum];
				if (irg == NULL || omit_fp)
					reg = map_i_to_o_reg(reg);
				param->reg1       = reg;
				param->req1       = reg->single_req;
				++regnum;
			} else {
				ir_mode *regmode = param_regs[0]->reg_class->mode;
				ir_type *type    = get_type_for_mode(regmode);
				param->type      = type;
				param->offset    = stack_offset;
				assert(get_mode_size_bits(regmode) == 32);
				stack_offset += 4;
			}
		}
	}
	n_param_regs_used = regnum;

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
			unsigned n_regs   = determine_n_float_regs(result_mode);
			unsigned next_reg = round_up2(float_regnum, n_regs);

			if (next_reg >= n_float_result_regs) {
				panic("Too many float results for sparc backend");
			} else {
				const arch_register_t *reg = float_result_regs[next_reg];
				rbitset_clear(caller_saves, reg->global_index);
				result->reg_offset = i;
				if (n_regs == 1) {
					result->req0 = reg->single_req;
				} else if (n_regs == 2) {
					result->req0 = &float_result_reqs_double[next_reg];
					rbitset_clear(caller_saves, reg->global_index+1);
				} else if (n_regs == 4) {
					result->req0 = &float_result_reqs_quad[next_reg];
					rbitset_clear(caller_saves, reg->global_index+1);
					rbitset_clear(caller_saves, reg->global_index+2);
					rbitset_clear(caller_saves, reg->global_index+3);
				} else {
					panic("invalid number of registers in sparc float result");
				}
				float_regnum = next_reg + n_regs;

				++n_reg_results;
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
				result->req0       = reg->single_req;
				result->reg_offset = i;
				rbitset_clear(caller_saves, reg->global_index);
				++n_reg_results;
			}
		}
	}

	cconv                   = XMALLOCZ(calling_convention_t);
	cconv->parameters       = params;
	cconv->param_stack_size = stack_offset;
	cconv->n_param_regs     = n_param_regs_used;
	cconv->results          = results;
	cconv->omit_fp          = omit_fp;
	cconv->caller_saves     = caller_saves;
	cconv->n_reg_results    = n_reg_results;

	/* setup ignore register array */
	if (irg != NULL) {
		be_irg_t       *birg      = be_birg_from_irg(irg);
		size_t          n_ignores = ARRAY_SIZE(ignore_regs);
		struct obstack *obst      = &birg->obst;
		size_t          r;

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
	free(cconv->caller_saves);
	free(cconv);
}

void sparc_cconv_init(void)
{
	size_t i;
	for (i = 0; i < ARRAY_SIZE(caller_saves); ++i) {
		rbitset_set(default_caller_saves, caller_saves[i]->global_index);
	}

	for (i = 0; i < ARRAY_SIZE(float_result_reqs_double); i += 2) {
		arch_register_req_t *req = &float_result_reqs_double[i];
		*req = *float_result_regs[i]->single_req;
		req->type |= arch_register_req_type_aligned;
		req->width = 2;
	}
	for (i = 0; i < ARRAY_SIZE(float_result_reqs_quad); i += 4) {
		arch_register_req_t *req = &float_result_reqs_quad[i];
		*req = *float_result_regs[i]->single_req;
		req->type |= arch_register_req_type_aligned;
		req->width = 4;
	}
}
