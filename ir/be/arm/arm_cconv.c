/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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

#include "arm_cconv.h"
#include "irmode.h"
#include "typerep.h"
#include "xmalloc.h"
#include "error.h"

calling_convention_t *decide_calling_convention(ir_type *function_type)
{
	int                   stack_offset = 0;
	reg_or_stackslot_t   *params;
	reg_or_stackslot_t   *results;
	int                   n_param_regs
		= sizeof(param_regs)/sizeof(param_regs[0]);
	int                   n_result_regs
		= sizeof(result_regs)/sizeof(result_regs[0]);
	int                   n_float_result_regs
		= sizeof(float_result_regs)/sizeof(float_result_regs[0]);
	int                   n_params;
	int                   n_results;
	int                   i;
	int                   regnum;
	int                   float_regnum;
	calling_convention_t *cconv;

	/* determine how parameters are passed */
	n_params = get_method_n_params(function_type);
	regnum   = 0;
	params   = XMALLOCNZ(reg_or_stackslot_t, n_params);

	for (i = 0; i < n_params; ++i) {
		ir_type            *param_type = get_method_param_type(function_type,i);
		ir_mode            *mode       = get_type_mode(param_type);
		int                 bits       = get_mode_size_bits(mode);
		reg_or_stackslot_t *param      = &params[i];
		param->type = param_type;

		if (regnum < n_param_regs) {
			const arch_register_t *reg = param_regs[regnum++];
			param->reg0 = reg;
		} else {
			param->offset = stack_offset;
			/* increase offset 4 bytes so everything is aligned */
			stack_offset += bits > 32 ? bits/8 : 4;
			continue;
		}

		/* we might need a 2nd 32bit component (for 64bit or double values) */
		if (bits > 32) {
			if (bits > 64)
				panic("only 32 and 64bit modes supported in arm backend");

			if (regnum < n_param_regs) {
				const arch_register_t *reg = param_regs[regnum++];
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
				panic("Too many float results for arm backend");
			} else {
				const arch_register_t *reg = float_result_regs[float_regnum++];
				result->reg0 = reg;
			}
		} else {
			if (get_mode_size_bits(result_mode) > 32) {
				panic("Results with more than 32bits not supported by arm backend yet");
			}

			if (regnum >= n_result_regs) {
				panic("Too many results for arm backend");
			} else {
				const arch_register_t *reg = result_regs[regnum++];
				result->reg0 = reg;
			}
		}
	}

	cconv                   = XMALLOCZ(calling_convention_t);
	cconv->parameters       = params;
	cconv->param_stack_size = stack_offset;
	cconv->results          = results;

	return cconv;
}

void free_calling_convention(calling_convention_t *cconv)
{
	free(cconv->parameters);
	free(cconv->results);
	free(cconv);
}
