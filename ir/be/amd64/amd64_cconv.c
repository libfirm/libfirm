/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   calling convention helpers
 * @author  Matthias Braun
 */
#include "be_t.h"
#include "beirg.h"
#include "amd64_cconv.h"
#include "irmode.h"
#include "irgwalk.h"
#include "typerep.h"
#include "xmalloc.h"
#include "util.h"
#include "panic.h"
#include "gen_amd64_regalloc_if.h"
#include "bitfiddle.h"

static const unsigned ignore_regs[] = {
	REG_RSP,
	REG_EFLAGS,
};

static const arch_register_t* const param_regs[] = {
	&amd64_registers[REG_RDI],
	&amd64_registers[REG_RSI],
	&amd64_registers[REG_RDX],
	&amd64_registers[REG_RCX],
	&amd64_registers[REG_R8],
	&amd64_registers[REG_R9],
};

static const arch_register_t* const float_param_regs[] = {
	&amd64_registers[REG_XMM0],
	&amd64_registers[REG_XMM1],
	&amd64_registers[REG_XMM2],
	&amd64_registers[REG_XMM3],
	&amd64_registers[REG_XMM4],
	&amd64_registers[REG_XMM5],
	&amd64_registers[REG_XMM6],
	&amd64_registers[REG_XMM7],
};

static const arch_register_t* const result_regs[] = {
	&amd64_registers[REG_RAX],
	&amd64_registers[REG_RDX],
};

static const arch_register_t* const float_result_regs[] = {
	&amd64_registers[REG_XMM0],
};

static const unsigned caller_saves[] = {
	REG_RAX,
	REG_RCX,
	REG_RDX,
	REG_RSI,
	REG_RDI,
	REG_R8,
	REG_R9,
	REG_R10,
	REG_R11,
	REG_XMM0,
	REG_XMM1,
	REG_XMM2,
	REG_XMM3,
	REG_XMM4,
	REG_XMM5,
	REG_XMM6,
	REG_XMM7,
};
static unsigned default_caller_saves[BITSET_SIZE_ELEMS(N_AMD64_REGISTERS)];

static const unsigned callee_saves[] = {
	REG_RBX,
	REG_RBP,
	REG_R12,
	REG_R13,
	REG_R14,
	REG_R15,
};
static unsigned default_callee_saves[BITSET_SIZE_ELEMS(N_AMD64_REGISTERS)];

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

amd64_cconv_t *amd64_decide_calling_convention(ir_type *function_type,
                                               ir_graph *irg)
{
	bool omit_fp = false;
	if (irg != NULL) {
		omit_fp = be_options.omit_fp;
		if (omit_fp == true) {
			irg_walk_graph(irg, check_omit_fp, NULL, &omit_fp);
		}
	}

	mtp_additional_properties mtp
		= get_method_additional_properties(function_type);
	unsigned *caller_saves = rbitset_malloc(N_AMD64_REGISTERS);
	unsigned *callee_saves = rbitset_malloc(N_AMD64_REGISTERS);
	if (mtp & mtp_property_returns_twice)
		panic("amd64: returns_twice calling convention NIY");
	rbitset_copy(caller_saves, default_caller_saves, N_AMD64_REGISTERS);
	rbitset_copy(callee_saves, default_callee_saves, N_AMD64_REGISTERS);

	/* determine how parameters are passed */
	size_t              n_params           = get_method_n_params(function_type);
	size_t              param_regnum       = 0;
	size_t              float_param_regnum = 0;
	reg_or_stackslot_t *params             = XMALLOCNZ(reg_or_stackslot_t,
	                                                   n_params);

	size_t   n_param_regs = ARRAY_SIZE(param_regs);
	size_t   n_float_param_regs = ARRAY_SIZE(float_param_regs);
	unsigned stack_offset = 0;
	for (size_t i = 0; i < n_params; ++i) {
		ir_type *param_type = get_method_param_type(function_type,i);
		if (is_compound_type(param_type))
			panic("amd64: compound arguments NIY");

		ir_mode *mode = get_type_mode(param_type);
		int      bits = get_mode_size_bits(mode);
		reg_or_stackslot_t *param = &params[i];

		if (mode_is_float(mode) && float_param_regnum < n_float_param_regs) {
			const arch_register_t *reg = float_param_regs[float_param_regnum];
			param->reg                 = reg;
			param->req                 = reg->single_req;
			param->reg_offset          = float_param_regnum + param_regnum;
			++float_param_regnum;
		} else if (!mode_is_float(mode) && param_regnum < n_param_regs) {
			const arch_register_t *reg = param_regs[param_regnum];
			param->reg        = reg;
			param->req        = reg->single_req;
			param->reg_offset = float_param_regnum + param_regnum;
			++param_regnum;
		} else {
			param->type   = param_type;
			param->offset = stack_offset;
			/* increase offset by at least AMD64_REGISTER_SIZE bytes so
			 * everything is aligned */
			stack_offset += MAX(bits / 8, AMD64_REGISTER_SIZE);
			continue;
		}

	}

	unsigned n_param_regs_used = param_regnum + float_param_regnum;

	/* determine how results are passed */
	size_t              n_results           = get_method_n_ress(function_type);
	unsigned            n_reg_results       = 0;
	reg_or_stackslot_t *results = XMALLOCNZ(reg_or_stackslot_t, n_results);
	unsigned            res_regnum          = 0;
	unsigned            res_float_regnum    = 0;
	size_t              n_result_regs       = ARRAY_SIZE(result_regs);
	size_t              n_float_result_regs = ARRAY_SIZE(float_result_regs);
	for (size_t i = 0; i < n_results; ++i) {
		ir_type            *result_type = get_method_res_type(function_type, i);
		ir_mode            *result_mode = get_type_mode(result_type);
		reg_or_stackslot_t *result      = &results[i];

		if (mode_is_float(result_mode)) {
			if (res_float_regnum >= n_float_result_regs) {
				panic("Too many floating points results");
			} else {
				const arch_register_t *reg = float_result_regs[res_float_regnum++];
				result->req                = reg->single_req;
				result->reg_offset         = i;
				rbitset_clear(caller_saves, reg->global_index);
				++n_reg_results;
			}
		} else {
			if (res_regnum >= n_result_regs) {
				panic("Too many results");
			} else {
				const arch_register_t *reg = result_regs[res_regnum++];
				result->req                = reg->single_req;
				result->reg_offset         = i;
				rbitset_clear(caller_saves, reg->global_index);
				++n_reg_results;
			}
		}
	}

	amd64_cconv_t *cconv    = XMALLOCZ(amd64_cconv_t);
	cconv->parameters       = params;
	cconv->param_stack_size = stack_offset;
	cconv->n_param_regs     = n_param_regs_used;
	cconv->n_xmm_regs       = float_param_regnum;
	cconv->results          = results;
	cconv->omit_fp          = omit_fp;
	cconv->caller_saves     = caller_saves;
	cconv->callee_saves     = callee_saves;
	cconv->n_reg_results    = n_reg_results;

	if (irg != NULL) {
		be_irg_t       *birg      = be_birg_from_irg(irg);
		size_t          n_ignores = ARRAY_SIZE(ignore_regs);
		struct obstack *obst      = &birg->obst;

		birg->allocatable_regs = rbitset_obstack_alloc(obst, N_AMD64_REGISTERS);
		rbitset_set_all(birg->allocatable_regs, N_AMD64_REGISTERS);
		for (size_t r = 0; r < n_ignores; ++r) {
			rbitset_clear(birg->allocatable_regs, ignore_regs[r]);
		}
		if (!omit_fp)
			rbitset_clear(birg->allocatable_regs, REG_RBP);
	}

	return cconv;
}

void amd64_free_calling_convention(amd64_cconv_t *cconv)
{
	free(cconv->parameters);
	free(cconv->results);
	free(cconv->caller_saves);
	free(cconv->callee_saves);
	free(cconv);
}

void amd64_cconv_init(void)
{
	for (size_t i = 0; i < ARRAY_SIZE(caller_saves); ++i) {
		rbitset_set(default_caller_saves, caller_saves[i]);
	}
	for (size_t i = 0; i < ARRAY_SIZE(callee_saves); ++i) {
		rbitset_set(default_callee_saves, callee_saves[i]);
	}
}
