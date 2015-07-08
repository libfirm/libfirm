/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief   calling convention helpers
 * @author  Matthias Braun
 */
#include "be_t.h"
#include "bearch_ia32_t.h"
#include "beirg.h"
#include "x86_cconv.h"
#include "irmode.h"
#include "irgwalk.h"
#include "typerep.h"
#include "xmalloc.h"
#include "util.h"
#include "panic.h"
#include "gen_ia32_regalloc_if.h"
#include "bitfiddle.h"

static const unsigned ignore_regs[] = {
	REG_ESP,
	REG_GP_NOREG,
	REG_FP_NOREG,
};

static const arch_register_t* const default_param_regs[] = {};
static const arch_register_t* const float_param_regs[]   = {};

#if 0
static const arch_register_t* const regparam_param_regs[] = {
	&ia32_registers[REG_EAX],
	&ia32_registers[REG_EDX],
	&ia32_registers[REG_ECX],
};

static const arch_register_t* const this_param_regs[] = {
	&ia32_registers[REG_ECX],
};

static const arch_register_t* const sse_param_regs[] = {
	&ia32_registers[REG_XMM0],
	&ia32_registers[REG_XMM1],
	&ia32_registers[REG_XMM2],
	&ia32_registers[REG_XMM3],
	&ia32_registers[REG_XMM4],
	&ia32_registers[REG_XMM5],
	&ia32_registers[REG_XMM6],
	&ia32_registers[REG_XMM7],
};
#endif

static const arch_register_t* const result_regs[] = {
	&ia32_registers[REG_EAX],
	&ia32_registers[REG_EDX],
};

static const arch_register_t* const float_result_regs[] = {
	&ia32_registers[REG_ST0],
};

#if 0
static const arch_register_t* const sse_result_regs[] = {
	&ia32_registers[REG_XMM0],
};
#endif

static const unsigned caller_saves[] = {
	REG_EAX,
	REG_ECX,
	REG_EDX,
	REG_ST0,
	REG_ST1,
	REG_ST2,
	REG_ST3,
	REG_ST4,
	REG_ST5,
	REG_ST6,
	REG_ST7,
	REG_XMM0,
	REG_XMM1,
	REG_XMM2,
	REG_XMM3,
	REG_XMM4,
	REG_XMM5,
	REG_XMM6,
	REG_XMM7,
};
static unsigned default_caller_saves[BITSET_SIZE_ELEMS(N_IA32_REGISTERS)];

static const unsigned callee_saves[] = {
	REG_EBX,
	REG_EBP,
	REG_ESI,
	REG_EDI,
	REG_FPCW,
};
static unsigned default_callee_saves[BITSET_SIZE_ELEMS(N_IA32_REGISTERS)];

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

x86_cconv_t *ia32_decide_calling_convention(ir_type *function_type,
                                            ir_graph *irg)
{
	bool omit_fp = false;
	if (irg != NULL) {
		omit_fp = be_options.omit_fp;
		if (omit_fp)
			irg_walk_graph(irg, check_omit_fp, NULL, &omit_fp);
	}

	mtp_additional_properties mtp
		= get_method_additional_properties(function_type);
	(void)mtp;
	/* TODO: do something with cc_reg_param/cc_this_call */

	unsigned *caller_saves = rbitset_malloc(N_IA32_REGISTERS);
	unsigned *callee_saves = rbitset_malloc(N_IA32_REGISTERS);
	rbitset_copy(caller_saves, default_caller_saves, N_IA32_REGISTERS);
	rbitset_copy(callee_saves, default_callee_saves, N_IA32_REGISTERS);

	/* determine how parameters are passed */
	unsigned            n_params           = get_method_n_params(function_type);
	unsigned            param_regnum       = 0;
	unsigned            float_param_regnum = 0;
	reg_or_stackslot_t *params             = XMALLOCNZ(reg_or_stackslot_t,
	                                                   n_params);

	unsigned n_param_regs       = ARRAY_SIZE(default_param_regs);
	unsigned n_float_param_regs = ARRAY_SIZE(float_param_regs);
	unsigned stack_offset       = 0;
	for (unsigned i = 0; i < n_params; ++i) {
		ir_type            *param_type = get_method_param_type(function_type, i);
		reg_or_stackslot_t *param      = &params[i];
		if (is_aggregate_type(param_type)) {
			param->type   = param_type;
			param->offset = stack_offset;
			stack_offset += get_type_size_bytes(param_type);
			goto align_stack;
		}

		ir_mode *mode = get_type_mode(param_type);
		if (mode_is_float(mode) && float_param_regnum < n_float_param_regs) {
			param->reg = float_param_regs[float_param_regnum++];
		} else if (!mode_is_float(mode) && param_regnum < n_param_regs) {
			param->reg = default_param_regs[param_regnum++];
		} else {
			param->type   = param_type;
			param->offset = stack_offset;
			stack_offset += get_type_size_bytes(param_type);
align_stack:;
			/* increase offset by at least IA32_REGISTER_SIZE bytes so
			 * everything is aligned */
			unsigned misalign = stack_offset % IA32_REGISTER_SIZE;
			if (misalign > 0)
				stack_offset += IA32_REGISTER_SIZE - misalign;
		}
	}

	unsigned n_param_regs_used = param_regnum + float_param_regnum;

	/* determine how results are passed */
	unsigned            n_results           = get_method_n_ress(function_type);
	unsigned            n_reg_results       = 0;
	reg_or_stackslot_t *results = XMALLOCNZ(reg_or_stackslot_t, n_results);
	unsigned            res_regnum          = 0;
	unsigned            res_float_regnum    = 0;
	unsigned            n_result_regs       = ARRAY_SIZE(result_regs);
	unsigned            n_float_result_regs = ARRAY_SIZE(float_result_regs);
	for (size_t i = 0; i < n_results; ++i) {
		ir_type            *result_type = get_method_res_type(function_type, i);
		ir_mode            *result_mode = get_type_mode(result_type);
		reg_or_stackslot_t *result      = &results[i];

		const arch_register_t *reg;
		if (mode_is_float(result_mode)) {
			if (res_float_regnum >= n_float_result_regs) {
				panic("too many floating points results");
			}
			reg = float_result_regs[res_float_regnum++];
		} else {
			if (res_regnum >= n_result_regs) {
				panic("too many results");
			}
			reg = result_regs[res_regnum++];
		}
		result->reg = reg;
		rbitset_clear(caller_saves, reg->global_index);
		++n_reg_results;
	}

	calling_convention cc = get_method_calling_convention(function_type);

	x86_cconv_t *cconv    = XMALLOCZ(x86_cconv_t);
	cconv->sp_delta       = (cc & cc_compound_ret) && !(cc & cc_reg_param)
	                        ? IA32_REGISTER_SIZE : 0;
	cconv->parameters     = params;
	cconv->callframe_size = stack_offset;
	cconv->n_param_regs   = n_param_regs_used;
	cconv->n_xmm_regs     = float_param_regnum;
	cconv->results        = results;
	cconv->omit_fp        = omit_fp;
	cconv->caller_saves   = caller_saves;
	cconv->callee_saves   = callee_saves;
	cconv->n_reg_results  = n_reg_results;

	if (irg != NULL) {
		be_irg_t       *birg      = be_birg_from_irg(irg);
		size_t          n_ignores = ARRAY_SIZE(ignore_regs);
		struct obstack *obst      = &birg->obst;

		birg->allocatable_regs = rbitset_obstack_alloc(obst, N_IA32_REGISTERS);
		rbitset_set_all(birg->allocatable_regs, N_IA32_REGISTERS);
		for (size_t r = 0; r < n_ignores; ++r) {
			rbitset_clear(birg->allocatable_regs, ignore_regs[r]);
		}
		if (!omit_fp)
			rbitset_clear(birg->allocatable_regs, REG_EBP);
	}

	return cconv;
}

void ia32_cconv_init(void)
{
	for (size_t i = 0; i < ARRAY_SIZE(caller_saves); ++i) {
		rbitset_set(default_caller_saves, caller_saves[i]);
	}
	for (size_t i = 0; i < ARRAY_SIZE(callee_saves); ++i) {
		rbitset_set(default_callee_saves, callee_saves[i]);
	}
}
