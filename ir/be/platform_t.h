/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */
#ifndef FIRM_TARGET_PLATFORM_T_H
#define FIRM_TARGET_PLATFORM_T_H

#include "target.h"

#include <stdbool.h>
#include "compiler.h"

typedef struct platform_t {
	ir_platform_define_t *defines;
	const char      *default_exe_name;
	char             user_label_prefix;
	unsigned         long_double_size                  : 5;
	unsigned         long_double_align                 : 5;
	unsigned         long_size                         : 4;
	unsigned         int_size                          : 4;
	bool             x87_long_double                   : 1;
	/** Override for alignment of long long and double types in structs. */
	unsigned         long_long_and_double_struct_align : 3;
	bool             pic_is_default                    : 1;
	bool             wchar_is_signed                   : 1;
	ENUMBF(ir_platform_type_t) wchar_type              : 4;
	ENUMBF(ir_platform_type_t) intptr_type             : 4;
} platform_t;

extern platform_t ir_platform;

void ir_init_platform(ir_machine_triple_t const *machine);

void ir_finish_platform(void);

#endif
