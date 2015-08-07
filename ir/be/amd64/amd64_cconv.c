/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   calling convention helpers
 * @author  Matthias Braun
 *
 * See also:
 *  - http://www.x86-64.org/documentation/abi.pdf
 *  - MSDN - "x64 Software Conventions"
 */
#include "be_t.h"
#include "beirg.h"
#include "irmode.h"
#include "irgwalk.h"
#include "typerep.h"
#include "xmalloc.h"
#include "util.h"
#include "panic.h"
#include "gen_amd64_regalloc_if.h"
#include "bitfiddle.h"
#include "bearch_amd64_t.h"
#include "../ia32/x86_cconv.h"

/*
 * Note: "X64 ABI" refers to the Windows ABI for x86_64 (the SysV ABI
 * calls itself "AMD64 ABI").
 */
bool amd64_use_x64_abi;

static const unsigned ignore_regs[] = {
	REG_RSP,
	REG_EFLAGS,
};

static const arch_register_t *const *param_regs;
static unsigned n_param_regs;

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
static unsigned n_float_param_regs;

static const arch_register_t* const result_regs[] = {
	&amd64_registers[REG_RAX],
	&amd64_registers[REG_RDX],
};

static const arch_register_t* const float_result_regs[] = {
	&amd64_registers[REG_XMM0],
	&amd64_registers[REG_XMM1],
};

static unsigned default_caller_saves[BITSET_SIZE_ELEMS(N_AMD64_REGISTERS)];
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

x86_cconv_t *amd64_decide_calling_convention(ir_type *function_type,
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
	/* x64 always reserves space to spill the first 4 arguments to have it
	 * easy in case of variadic functions. */
	unsigned stack_offset = amd64_use_x64_abi ? 32 : 0;
	for (size_t i = 0; i < n_params; ++i) {
		ir_type *param_type = get_method_param_type(function_type,i);
		if (is_compound_type(param_type))
			panic("amd64: compound arguments NIY");

		ir_mode *mode = get_type_mode(param_type);
		int      bits = get_mode_size_bits(mode);
		reg_or_stackslot_t *param = &params[i];

		if (mode_is_float(mode) && float_param_regnum < n_float_param_regs) {
			param->reg = float_param_regs[float_param_regnum++];
			if (amd64_use_x64_abi) {
				++param_regnum;
			}
		} else if (!mode_is_float(mode) && param_regnum < n_param_regs) {
			param->reg = param_regs[param_regnum++];
			if (amd64_use_x64_abi) {
				++float_param_regnum;
			}
		} else {
			param->type   = param_type;
			param->offset = stack_offset;
			/* increase offset by at least AMD64_REGISTER_SIZE bytes so
			 * everything is aligned */
			stack_offset += MAX(bits / 8, AMD64_REGISTER_SIZE);
		}
	}

	/* If the function is variadic, we add all unused parameter
	 * passing registers to the end of the params array, first GP,
	 * then XMM. */
	if (is_method_variadic(function_type)) {
		if (amd64_use_x64_abi) {
			panic("Variadic functions on Windows ABI not supported");
		}

		int params_remaining = (n_param_regs - param_regnum) +
			(n_float_param_regs - float_param_regnum);
		params = XREALLOC(params, reg_or_stackslot_t, n_params + params_remaining);
		size_t i = n_params;

		for (; param_regnum < n_param_regs; param_regnum++, i++) {
			params[i].reg = param_regs[param_regnum];
		}

		for (; float_param_regnum < n_float_param_regs; float_param_regnum++, i++) {
			params[i].reg = float_param_regs[float_param_regnum];
		}
	}

	unsigned n_param_regs_used
		= amd64_use_x64_abi ? param_regnum : param_regnum + float_param_regnum;

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

	x86_cconv_t *cconv    = XMALLOCZ(x86_cconv_t);
	cconv->parameters     = params;
	cconv->n_parameters   = n_params;
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

void amd64_cconv_init(void)
{
	static const unsigned common_caller_saves[] = {
		REG_RAX,
		REG_RCX,
		REG_RDX,
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
		REG_XMM8,
		REG_XMM9,
		REG_XMM10,
		REG_XMM11,
		REG_XMM12,
		REG_XMM13,
		REG_XMM14,
		REG_XMM15,
	};
	for (size_t i = 0; i < ARRAY_SIZE(common_caller_saves); ++i) {
		rbitset_set(default_caller_saves, common_caller_saves[i]);
	}
	if (!amd64_use_x64_abi) {
		rbitset_set(default_caller_saves, REG_RSI);
		rbitset_set(default_caller_saves, REG_RDI);
	}

	static const unsigned common_callee_saves[] = {
		REG_RBX,
		REG_RBP,
		REG_R12,
		REG_R13,
		REG_R14,
		REG_R15,
	};
	for (size_t i = 0; i < ARRAY_SIZE(common_callee_saves); ++i) {
		rbitset_set(default_callee_saves, common_callee_saves[i]);
	}
	if (amd64_use_x64_abi) {
		rbitset_set(default_callee_saves, REG_RSI);
		rbitset_set(default_callee_saves, REG_RDI);
	}

	static const arch_register_t* const param_regs_list[] = {
		&amd64_registers[REG_RDI],
		&amd64_registers[REG_RSI],
		&amd64_registers[REG_RDX],
		&amd64_registers[REG_RCX],
		&amd64_registers[REG_R8],
		&amd64_registers[REG_R9],
	};
	param_regs   = amd64_use_x64_abi ? &param_regs_list[2] : param_regs_list;
	n_param_regs = ARRAY_SIZE(param_regs_list) - (amd64_use_x64_abi ? 2 : 0);

	n_float_param_regs = amd64_use_x64_abi ? 4 : ARRAY_SIZE(float_param_regs);
}
