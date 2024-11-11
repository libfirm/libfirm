/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */
#ifndef FIRM_TARGET_PLATFORM_T_H
#define FIRM_TARGET_PLATFORM_T_H

#include "target.h"

#include <stdbool.h>
#include "compiler.h"

typedef enum object_format_t {
	OBJECT_FORMAT_ELF,     /**< Executable and Linkable Format (unixes) */
	OBJECT_FORMAT_PE_COFF, /**< Portable Executable/Common Object File Format
	                            (Windows) */
	OBJECT_FORMAT_MACH_O   /**< Mach Object File Format (OS/X) */
} object_format_t;

typedef enum be_pic_style_t {
	BE_PIC_NONE,
	BE_PIC_MACH_O,
	BE_PIC_ELF_PLT,
	BE_PIC_ELF_NO_PLT,
} be_pic_style_t;

typedef struct platform_t {
	ir_platform_define_t *defines;
	char const           *default_exe_name;
	ir_type               *va_list_type;
	char                  user_label_prefix;
	bool                  initialized                       : 1;
	unsigned              long_double_size                  : 5;
	unsigned              long_double_align                 : 5;
	unsigned              long_size                         : 4;
	unsigned              int_size                          : 4;
	bool                  x87_long_double                   : 1;
	/** Override for alignment of long long and double types in structs. */
	unsigned              long_long_and_double_struct_align : 3;
	bool                  pic_is_default                    : 1;
	bool                  wchar_is_signed                   : 1;
	bool                  is_darwin                         : 1;
	bool                  supports_thread_local_storage     : 1;
	bool                  ia32_struct_in_regs               : 1;
	unsigned              ia32_po2_stackalign               : 4;
	bool                  amd64_x64abi                      : 1;
	ENUMBF(object_format_t)    object_format                : 2;
	ENUMBF(ir_platform_type_t) wchar_type                   : 4;
	ENUMBF(ir_platform_type_t) intptr_type                  : 4;
	ENUMBF(be_pic_style_t)     pic_style                    : 2;
} platform_t;

extern platform_t ir_platform;

/**
 * Initializes platform information based on target triple. The target/platform
 * may still be configured by ir_target_option().
 */
void ir_platform_set(ir_machine_triple_t const *machine, unsigned pointer_size);

/** Finish platform initialization after all ir_target_option() calls. */
void ir_platform_init(void);

void ir_platform_finish(void);

void ir_platform_set_va_list_type_pointer(void);

#endif
