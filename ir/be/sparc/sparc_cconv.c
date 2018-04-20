/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   calling convention helpers
 * @author  Matthias Braun
 */
#include "sparc_cconv.h"

#include "be_t.h"
#include "becconv.h"
#include "beirg.h"
#include "bitfiddle.h"
#include "gen_sparc_regalloc_if.h"
#include "irgwalk.h"
#include "irmode_t.h"
#include "panic.h"
#include "typerep.h"
#include "util.h"
#include "xmalloc.h"

static const unsigned ignore_regs[] = {
	REG_G0,
	/* used in case an address offset does not fit into an immediate: */
	REG_G4,
	/* reserved for SPARC ABI: */
	REG_G5,
	REG_G6,
	REG_G7,

	REG_SP,
	REG_O7,
	REG_FP,
	REG_I7,

	REG_PSR,
	REG_FSR,
	REG_Y,

	REG_F30,
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
//      &sparc_registers[REG_F1],
	&sparc_registers[REG_F2],
//      &sparc_registers[REG_F3],
	&sparc_registers[REG_F4],
//      &sparc_registers[REG_F5],
	&sparc_registers[REG_F6],
//      &sparc_registers[REG_F7],
};
static arch_register_req_t float_result_reqs_double[4];
static arch_register_req_t float_result_reqs_quad[4];

static const unsigned caller_saves[] = {
	REG_G1,
	REG_G2,
	REG_G3,
	REG_O0,
	REG_O1,
	REG_O2,
	REG_O3,
	REG_O4,
	REG_O5,
	REG_F0,
//      REG_F1,
	REG_F2,
//      REG_F3,
	REG_F4,
//      REG_F5,
	REG_F6,
//      REG_F7,
	REG_F8,
//      REG_F9,
	REG_F10,
//      REG_F11,
	REG_F12,
//      REG_F13,
	REG_F14,
//      REG_F15,
	REG_F16,
//      REG_F17,
	REG_F18,
//      REG_F19,
	REG_F20,
//      REG_F21,
	REG_F22,
//      REG_F23,
	REG_F24,
//      REG_F25,
	REG_F26,
//      REG_F27,
	REG_F28,
//      REG_F29,
//      REG_F30,
	REG_PSR,
	REG_FSR,
	REG_Y,
};
static unsigned default_caller_saves[BITSET_SIZE_ELEMS(N_SPARC_REGISTERS)];

static const unsigned returns_twice_saved[] = {
	REG_SP,
	REG_FP,
	REG_I7
};
static unsigned default_returns_twice_saves[BITSET_SIZE_ELEMS(N_SPARC_REGISTERS)];

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
	/* omit-fp is not possible if:
	 *  - we have allocations on the stack
	 *  - we have calls (with the exception of tail-calls once we support them)
	 */
	if (is_Alloc(node) || is_Free(node) || is_Call(node)) {
		bool *can_omit_fp = (bool*) env;
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
		panic("unexpected floatingpoint mode %+F", mode);
	}
}

calling_convention_t *sparc_decide_calling_convention(ir_type *function_type,
                                                      ir_graph *irg)
{
	bool omit_fp = false;
	if (irg != NULL) {
		omit_fp = be_options.omit_fp;
		/* our current vaarg handling needs the standard space to store the
		 * args 0-5 in it */
		if (is_method_variadic(function_type))
			omit_fp = false;
		/* The pointer to the aggregate return value belongs to the 92 magic bytes.
		 * Thus, if the called functions increases the stack size,
		 * it must copy the value to the appropriate location.
		 * This is not implemented yet, so we forbid to omit the frame pointer.
		 */
		if (get_method_calling_convention(function_type) & cc_compound_ret)
			omit_fp = false;
		if (omit_fp)
			irg_walk_graph(irg, check_omit_fp, NULL, &omit_fp);
		sparc_get_irg_data(irg)->omit_fp = omit_fp;
	}

	mtp_additional_properties mtp
		= get_method_additional_properties(function_type);
	unsigned *caller_saves = rbitset_malloc(N_SPARC_REGISTERS);
	if (mtp & mtp_property_returns_twice) {
		rbitset_copy(caller_saves, default_returns_twice_saves,
		             N_SPARC_REGISTERS);
	} else {
		rbitset_copy(caller_saves, default_caller_saves, N_SPARC_REGISTERS);
	}

	/* determine how parameters are passed */
	int                 n_params = get_method_n_params(function_type);
	int                 regnum   = 0;
	reg_or_stackslot_t *params   = XMALLOCNZ(reg_or_stackslot_t, n_params);

	int      n_param_regs = ARRAY_SIZE(param_regs);
	unsigned stack_offset = !omit_fp ? SPARC_MIN_STACKSIZE : 0;
	for (int i = 0; i < n_params; ++i) {
		ir_type            *param_type = get_method_param_type(function_type,i);
		ir_mode            *mode;
		int                 bits;
		reg_or_stackslot_t *param;

		if (is_compound_type(param_type))
			panic("compound arguments not supported yet");

		mode  = get_type_mode(param_type);
		bits  = get_mode_size_bits(mode);
		param = &params[i];

		if (i == 0 &&
		    (get_method_calling_convention(function_type) & cc_compound_ret)) {
			assert(mode_is_reference(mode) && bits == 32);
			/* special case, we have reserved space for this on the between
			 * type */
			param->type   = param_type;
			param->offset = SPARC_AGGREGATE_RETURN_OFFSET;
			param->already_stored = true;
			continue;
		}

		if (regnum < n_param_regs) {
			param->offset = SPARC_PARAMS_SPILL_OFFSET
			                + regnum * SPARC_REGISTER_SIZE;
			param->type   = param_type;
			arch_register_t const *reg = param_regs[regnum++];
			if (irg == NULL || omit_fp)
				reg = map_i_to_o_reg(reg);
			param->reg0 = reg;
			param->req0 = reg->single_req;
		} else {
			param->type   = param_type;
			param->offset = stack_offset;
			param->already_stored = true;
			/* increase offset by at least SPARC_REGISTER_SIZE bytes so
			 * everything is aligned */
			stack_offset += MAX(bits / 8, SPARC_REGISTER_SIZE);
			continue;
		}

		/* we might need a 2nd 32bit component (for 64bit or double values) */
		if (bits > 32) {
			if (bits > 64)
				panic("only 32 and 64bit modes supported");

			if (regnum < n_param_regs) {
				param->offset = SPARC_PARAMS_SPILL_OFFSET
				                + regnum * SPARC_REGISTER_SIZE;
				arch_register_t const *reg = param_regs[regnum++];
				if (irg == NULL || omit_fp)
					reg = map_i_to_o_reg(reg);
				param->reg1 = reg;
				param->req1 = reg->single_req;
			} else {
				ir_mode *regmode = param_regs[0]->cls->mode;
				ir_type *type    = get_type_for_mode(regmode);
				param->type      = type;
				param->offset    = stack_offset;
				assert(get_mode_size_bits(regmode) == 32);
				stack_offset += SPARC_REGISTER_SIZE;
			}
		}
	}
	unsigned n_param_regs_used = regnum;

	/* determine how results are passed */
	int                 n_results           = get_method_n_ress(function_type);
	unsigned            float_regnum        = 0;
	unsigned            n_reg_results       = 0;
	unsigned            n_float_result_regs = ARRAY_SIZE(float_result_regs);
	reg_or_stackslot_t *results = XMALLOCNZ(reg_or_stackslot_t, n_results);
	regnum        = 0;
	for (int i = 0; i < n_results; ++i) {
		ir_type            *result_type = get_method_res_type(function_type, i);
		ir_mode            *result_mode = get_type_mode(result_type);
		reg_or_stackslot_t *result      = &results[i];

		if (mode_is_float(result_mode)) {
			unsigned n_regs   = determine_n_float_regs(result_mode);
			unsigned next_reg = round_up2(float_regnum, n_regs);

			if (next_reg >= n_float_result_regs) {
				panic("too many float results");
			} else {
				const arch_register_t *reg = float_result_regs[next_reg];
				rbitset_clear(caller_saves, reg->global_index);
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
					panic("invalid number of registers in result");
				}
				float_regnum = next_reg + n_regs;

				++n_reg_results;
			}
		} else {
			if (get_mode_size_bits(result_mode) > 32) {
				panic("results with more than 32bits not supported yet");
			}

			if (regnum >= n_param_regs) {
				panic("too many results");
			} else {
				const arch_register_t *reg = param_regs[regnum++];
				if (irg == NULL || omit_fp)
					reg = map_i_to_o_reg(reg);
				result->req0 = reg->single_req;
				rbitset_clear(caller_saves, reg->global_index);
				++n_reg_results;
			}
		}
	}

	calling_convention_t *cconv = XMALLOCZ(calling_convention_t);
	cconv->n_parameters     = n_params;
	cconv->parameters       = params;
	cconv->param_stack_size = stack_offset - SPARC_MIN_STACKSIZE;
	cconv->n_param_regs     = n_param_regs_used;
	cconv->results          = results;
	cconv->omit_fp          = omit_fp;
	cconv->caller_saves     = caller_saves;
	cconv->n_reg_results    = n_reg_results;

	/* setup ignore register array */
	if (irg != NULL) {
		be_irg_t *birg = be_birg_from_irg(irg);

		birg->allocatable_regs = be_cconv_alloc_all_regs(&birg->obst, N_SPARC_REGISTERS);
		be_cconv_rem_regs(birg->allocatable_regs, ignore_regs, ARRAY_SIZE(ignore_regs));
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
	be_cconv_add_regs(default_caller_saves, caller_saves, ARRAY_SIZE(caller_saves));

	rbitset_set_all(default_returns_twice_saves, N_SPARC_REGISTERS);
	be_cconv_rem_regs(default_returns_twice_saves, returns_twice_saved, ARRAY_SIZE(returns_twice_saved));
	be_cconv_rem_regs(default_returns_twice_saves, ignore_regs,         ARRAY_SIZE(ignore_regs));

	for (size_t i = 0; i < ARRAY_SIZE(float_result_reqs_double); i += 2) {
		arch_register_req_t *req = &float_result_reqs_double[i];
		*req = *float_result_regs[i]->single_req;
		req->width = 1;
	}
	for (size_t i = 0; i < ARRAY_SIZE(float_result_reqs_quad); i += 4) {
		arch_register_req_t *req = &float_result_reqs_quad[i];
		*req = *float_result_regs[i]->single_req;
		req->width = 4;
	}
}
