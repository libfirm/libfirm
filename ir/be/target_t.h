/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */
#ifndef FIRM_TARGET_TARGET_T_H
#define FIRM_TARGET_TARGET_T_H

#include "target.h"

#include "bearch.h"
#include "be_types.h"
#include "compiler.h"
#include "iroptimize.h"
#include <assert.h>
#include <stdbool.h>

#define ir_target_big_endian()   ir_target_big_endian_()

typedef struct target_info_t {
	arch_isa_if_t   const *isa;
	char const            *experimental;
	arch_allow_ifconv_func allow_ifconv;
	ir_mode               *mode_float_arithmetic;
	bool isa_initialized          : 1;
	bool fast_unaligned_memaccess : 1;
	ENUMBF(float_int_conversion_overflow_style_t) float_int_overflow : 2;
} target_info_t;

extern target_info_t ir_target;

static inline int ir_target_big_endian_(void)
{
	return ir_target.isa->big_endian;
}

static inline bool ir_is_cpu_x86_32(char const *const cpu)
{
	/* i386, i486, i586, i686, i786 */
	return cpu[0] == 'i' && cpu[2] == '8' && cpu[3] == '6' && cpu[4] == '\0'
	    && cpu[1] >= '3' && cpu[1] <= '7';
}

void finish_target(void);

#endif
