/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */
#ifndef FIRM_TARGET_TARGET_T_H
#define FIRM_TARGET_TARGET_T_H

#include "target.h"

#include "be_types.h"
#include <assert.h>
#include <stdbool.h>

#define ir_target_big_endian()   ir_target_big_endian_()

typedef enum object_format_t {
	OBJECT_FORMAT_ELF,     /**< Executable and Linkable Format (unixes) */
	OBJECT_FORMAT_PE_COFF, /**< Portable Executable/Common Object File Format
	                            (Windows) */
	OBJECT_FORMAT_MACH_O,  /**< Mach Object File Format (OS/X) */
	OBJECT_FORMAT_LAST = OBJECT_FORMAT_MACH_O
} object_format_t;

typedef struct target_info_t {
	arch_isa_if_t const *isa;
	object_format_t      object_format;
	const char          *arch;

	bool                 initialized         : 1;
	bool                 big_endian          : 1;
	bool                 ia32_struct_in_regs : 1;
	unsigned             ia32_po2_stackalign : 4;
	bool                 amd64_x64abi        : 1;
} target_info_t;

extern target_info_t ir_target;

static inline int ir_target_big_endian_(void)
{
	assert(ir_target.initialized);
	return ir_target.big_endian;
}

static inline bool ir_is_cpu_x86_32(char const *const cpu)
{
	/* i386, i486, i586, i686, i786 */
	return cpu[0] == 'i' && cpu[2] == '8' && cpu[3] == '6' && cpu[4] == '\0'
	    && cpu[1] >= '3' && cpu[1] <= '7';
}

void finish_target(void);

#endif
