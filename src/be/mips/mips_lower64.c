/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */
#include "mips_lower64.h"

#include "gen_mips_regalloc_if.h"
#include "lower_dw.h"

void mips_lower64(void)
{
	ir_mode *const word_unsigned = mips_reg_classes[CLASS_mips_gp].mode;
	ir_mode *const word_signed   = find_signed_mode(word_unsigned);
	lwrdw_param_t const lower_dw_params = {
		.word_unsigned    = word_unsigned,
		.word_signed      = word_signed,
		.doubleword_size  = 64,
	};

	ir_prepare_dw_lowering(&lower_dw_params);
	ir_lower_dw_ops();
}
