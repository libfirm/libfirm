/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#include "mips_cconv.h"

#include "gen_mips_regalloc_if.h"
#include "util.h"

static unsigned const regs_result_gp[] = {
	REG_V0,
	REG_V1,
};

void mips_determine_calling_convention(mips_calling_convention_t *const cconv, ir_type *const fun_type)
{
	/* Handle parameters. */
	size_t const n_params = get_method_n_params(fun_type);
	if (n_params != 0) {
		panic("TODO");
	}

	/* Handle results. */
	mips_reg_or_slot_t *results;
	size_t        const n_results = get_method_n_ress(fun_type);
	if (n_results != 0) {
		results = XMALLOCNZ(mips_reg_or_slot_t, n_results);

		size_t gp_res = 0;
		for (size_t i = 0; i != n_results; ++i) {
			ir_type *const res_type = get_method_res_type(fun_type, i);
			ir_mode *const res_mode = get_type_mode(res_type);
			if (mode_is_float(res_mode)) {
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

void mips_free_calling_convention(mips_calling_convention_t *const cconv)
{
	free(cconv->results);
}
