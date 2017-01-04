/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */
#include "platform_t.h"

#include "be_t.h"
#include "ident_t.h"
#include "panic.h"
#include "target_t.h"
#include "util.h"
#include "xmalloc.h"
#include <assert.h>

struct ir_platform_define_t {
	char const      *name;
	char const      *value;
	ir_platform_define_t *next;
};

platform_t ir_platform;

/** Add a target specific preprocessor define. */
static void ppdef(const char *name, const char *value)
{
	ir_platform_define_t *define = XMALLOCZ(ir_platform_define_t);
	define->name  = name;
	define->value = value;
	define->next  = ir_platform.defines;
	ir_platform.defines = define;
}

static void ppdef1(const char *name)
{
	ppdef(name, "1");
}

static void ppdef_unix(void)
{
	ppdef1("__unix");
	ppdef1("__unix__");
	ppdef1("unix");
}

void ir_init_platform(ir_machine_triple_t const *machine)
{
	memset(&ir_platform, 0, sizeof(ir_platform));
	unsigned const pointer_size = ir_target_pointer_size();
	assert(pointer_size >= 4 && "TODO: smaller sizes");
	ir_platform.int_size        = 4;
	ir_platform.long_size       = MIN(pointer_size, 8);
	ir_platform.intptr_type     = IR_TYPE_LONG;
	ir_platform.wchar_is_signed = true;
	ir_platform.wchar_type      = IR_TYPE_INT;

	const char *const cpu          = ir_triple_get_cpu_type(machine);
	const char *const manufacturer = ir_triple_get_manufacturer(machine);
	const char *const os           = ir_triple_get_operating_system(machine);
	if (ir_is_cpu_x86_32(cpu)) {
		ppdef1("i386");
		ppdef1("__i386");
		ppdef1("__i386__");
		switch (cpu[1]) {
		case '4':
			ppdef1("__i486");
			ppdef1("__i486__");
			break;
		case '5':
			ppdef1("__i586");
			ppdef1("__i586__");
			ppdef1("__pentium");
			ppdef1("__pentium__");
			break;
		case '6':
			ppdef1("__pentiumpro");
			ppdef1("__pentiumpro__");
			ppdef1("__i686");
			ppdef1("__i686__");
			break;
		case '7':
			ppdef1("__pentium4");
			ppdef1("__pentium4__");
			break;
		}

		ir_platform.long_double_size  = 12;
		ir_platform.long_double_align = 4;
		/* long long and double has a 4 byte alignment inside structs, this odd
		 * mode is everywhere except for windows OSes (they will revert it
		 * below) */
		ir_platform.long_long_and_double_struct_align = 4;
		ir_platform.x87_long_double                   = true;
	} else if (streq(cpu, "arm")) {
		/* TODO: what about those:
		 * ARM_FEATURE_UNALIGNED, ARMEL, ARM_ARCH_7A, ARM_FEATURE_DSP, ... */
		ppdef1("__arm__");
		if (strstr(os, "eabi") != NULL)
			ppdef1("__ARM_EABI__");

		ir_platform.long_double_size  = 8;
		ir_platform.long_double_align = 8;
	} else if (streq(cpu, "sparc")) {
		ppdef1("sparc");
		ppdef1("__sparc");
		ppdef1("__sparc__");
		/* we always produce sparc V8 code at the moment */
		ppdef1("__sparc_v8__");
		if (strstr(manufacturer, "leon") != NULL
		 || streq(manufacturer, "invasic")) {
			ppdef1("__leon__");
		}
		ir_platform.long_double_size  = 16;
		ir_platform.long_double_align = 8;
	} else if (streq(cpu, "x86_64") || streq(cpu, "amd64")) {
		ppdef1("__x86_64");
		ppdef1("__x86_64__");
		ppdef1("__amd64");
		ppdef1("__amd64__");
		ir_platform.long_double_size  = 16;
		ir_platform.long_double_align = 16;
		ir_platform.x87_long_double = true;
	} else if (streq(cpu, "mips")) {
		ppdef1("__mips__");
		ir_platform.long_double_size  = 8;
		ir_platform.long_double_align = 8;
	} else if (streq(cpu, "TEMPLATE")) {
		ir_platform.long_double_size  = 8;
		ir_platform.long_double_align = 8;
	} else {
		/** Everything that passes ir_init_target_from_triple()
		 * should work here as well. */
		panic("Unexpected ABI");
	}

	if (strstr(os, "linux") != NULL) {
		ppdef_unix();
		ppdef1("__linux");
		ppdef1("__linux__");
		ppdef1("linux");
		if (strstr(os, "gnu") != NULL)
			ppdef1("__gnu_linux__");
	} else if (strstart(os, "freebsd")) {
		ppdef("__FreeBSD__", "");
		ppdef_unix();
	} else if (strstr(os, "bsd") != NULL) {
		ppdef_unix();
	} else if (strstart(os, "darwin")) {
		ir_platform.default_exe_name  = "a.out";
		ir_platform.user_label_prefix = '_';
		ir_platform.long_double_size  = 16;
		ir_platform.long_double_align = 16;
		ir_platform.pic_is_default    = true;
		ppdef1("__MACH__");
		ppdef1("__APPLE__");
		ppdef1("__APPLE_CC__");
		ppdef1("__CONSTANT_CFSTRINGS__");
		ppdef1("__DYNAMIC__");
		ppdef("__weak",                 "");
		ppdef("__strong",               "");
		ppdef("__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__", "1050");
		if (!ir_target.big_endian)
			ppdef1("__LITTLE_ENDIAN__");
	} else if (strstart(os, "mingw")) {
		ir_platform.wchar_type      = IR_TYPE_SHORT;
		ir_platform.wchar_is_signed = false;
		ir_platform.long_long_and_double_struct_align = 0;
		ppdef1("__MINGW32__");
		ppdef1("__MSVCRT__");
		ppdef1("_WINNT");
		ppdef1("__WINNT");
		ppdef1("__WINNT__");
		ppdef1("WINNT");
		ppdef1("_WIN32");
		ppdef1("__WIN32");
		ppdef1("__WIN32__");
		ppdef1("WIN32");
		if (pointer_size == 8) {
			ppdef1("_WIN64");
			ppdef1("__WIN64");
			ppdef1("__WIN64__");
			ppdef1("WIN64");
			ppdef1("__MINGW64__");
			/* to ease porting of old c-code microsoft decided to use 32bits
			 * even for long */
			ir_platform.long_size = 4;
			ir_platform.intptr_type = IR_TYPE_LONG_LONG;
		} else {
			assert(pointer_size == 4);
			ir_platform.user_label_prefix = '_';
			ir_platform.intptr_type = IR_TYPE_INT;
		}
	} else if (strstart(os, "midipix")) {
		ir_platform.long_long_and_double_struct_align = 0;
		ppdef1("__midipix__");
		if (pointer_size == 8) {
			ppdef1("__NT64");
		} else {
			assert(pointer_size == 4);
			ir_platform.user_label_prefix = '_';
			ppdef1("__NT32");
		}
	} else if (streq(os, "elf") || streq(os, "octopos") || streq(os, "irtss")
	        || streq(os, "unknown")) {
	    /* Nothing to do, just added to show that we check for the same
	     * systems as target.c... */
	}

	switch (ir_target.object_format) {
	case OBJECT_FORMAT_ELF:
		ppdef1("__ELF__");
		ir_platform.default_exe_name = "a.out";
		break;
	case OBJECT_FORMAT_PE_COFF:
		ppdef1("__PE__");
		ir_platform.default_exe_name = "a.exe";
		break;
	case OBJECT_FORMAT_MACH_O:
		ir_platform.default_exe_name = "a.out";
		break;
	}

	be_set_pic(ir_platform.pic_is_default, false);
}

ir_platform_define_t const *ir_platform_define_first(void)
{
	assert(ir_target.initialized);
	return ir_platform.defines;
}

ir_platform_define_t const *ir_platform_define_next(
		ir_platform_define_t const *const define)
{
	return define->next;
}

char const *ir_platform_define_name(ir_platform_define_t const *const define)
{
	return define->name;
}

char const *ir_platform_define_value(ir_platform_define_t const *const define)
{
	return define->value;
}

unsigned ir_platform_long_long_and_double_struct_align_override(void)
{
	assert(ir_target.initialized);
	return ir_platform.long_long_and_double_struct_align;
}

int ir_platform_x87_long_double(void)
{
	assert(ir_target.initialized);
	return ir_platform.x87_long_double;
}

ir_platform_type_t ir_platform_wchar_type(void)
{
	assert(ir_target.initialized);
	return ir_platform.wchar_type;
}

int ir_platform_wchar_is_signed(void)
{
	assert(ir_target.initialized);
	return ir_platform.wchar_is_signed;
}

ir_platform_type_t ir_platform_intptr_type(void)
{
	assert(ir_target.initialized);
	return ir_platform.intptr_type;
}

int ir_platform_pic_is_default(void)
{
	assert(ir_target.initialized);
	return ir_platform.pic_is_default;
}

unsigned ir_platform_type_size(ir_platform_type_t type)
{
	switch (type) {
	case IR_TYPE_BOOL:        return 1;
	case IR_TYPE_CHAR:        return 1;
	case IR_TYPE_SHORT:       return 2;
	case IR_TYPE_INT:         return 4;
	case IR_TYPE_LONG:        return ir_platform.long_size;
	case IR_TYPE_LONG_LONG:   return 8;
	case IR_TYPE_FLOAT:       return 4;
	case IR_TYPE_DOUBLE:      return 8;
	case IR_TYPE_LONG_DOUBLE: return ir_platform.long_double_size;
	}
	panic("Invalid object type");
}

unsigned ir_platform_type_align(ir_platform_type_t type)
{
	if (type == IR_TYPE_LONG_DOUBLE)
		return ir_platform.long_double_align;
	return ir_platform_type_size(type);
}

char ir_platform_user_label_prefix(void)
{
	return ir_platform.user_label_prefix;
}

char const *ir_platform_default_exe_name(void)
{
	return ir_platform.default_exe_name;
}

void ir_finish_platform(void)
{
	for (ir_platform_define_t *next, *define = ir_platform.defines;
	     define != NULL; define = next) {
	    next = define->next;
	    free(define);
	}
}

ident *platform_mangle_global(ident *id)
{
	char user_label_prefix = ir_platform.user_label_prefix;
	return user_label_prefix == 0 ? id
	                              : new_id_fmt("%c%s", user_label_prefix, id);
}
