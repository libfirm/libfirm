/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */
#include "platform_t.h"

#include "be_t.h"
#include "bearch.h"
#include "bediagnostic.h"
#include "ident_t.h"
#include "irtools.h"
#include "lc_opts.h"
#include "panic.h"
#include "target_t.h"
#include "util.h"
#include "xmalloc.h"
#include <assert.h>

static bool option_pic;
static bool option_pic_noplt;

struct ir_platform_define_t {
	char const      *name;
	char const      *value;
	ir_platform_define_t *next;
};

platform_t ir_platform;

static void init_lc_opts(void)
{
	static const lc_opt_table_entry_t platform_options[] = {
		LC_OPT_ENT_BOOL("pic",   "Generate position independent code",
		                &option_pic),
		LC_OPT_ENT_BOOL("noplt", "Avoid using PLT in PIC code",
		                &option_pic_noplt),
		LC_OPT_LAST
	};
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_add_table(be_grp, platform_options);
}


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

void ir_platform_set(ir_machine_triple_t const *machine,
                     unsigned const pointer_size)
{
	memset(&ir_platform, 0, sizeof(ir_platform));
	assert(pointer_size >= 4 && "TODO: smaller sizes");
	ir_platform.int_size            = 4;
	ir_platform.long_size           = MIN(pointer_size, 8);
	ir_platform.intptr_type         = IR_TYPE_LONG;
	ir_platform.wchar_is_signed     = true;
	ir_platform.wchar_type          = IR_TYPE_INT;
	ir_platform.ia32_po2_stackalign = 2;

	const char *const cpu          = ir_triple_get_cpu_type(machine);
	const char *const manufacturer = ir_triple_get_manufacturer(machine);
	const char *const os           = ir_triple_get_operating_system(machine);
	if (ir_is_cpu_x86_32(cpu)) {
		ir_platform.supports_thread_local_storage = true;
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
		ir_platform.supports_thread_local_storage = true;
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
	} else if (streq(cpu, "riscv32")) {
		ppdef1("__riscv");
		ppdef1("__riscv_div");
		ppdef1("__riscv_mul");
		ppdef1("__riscv_muldiv");
		ppdef("__riscv_xlen", "32");
		ir_platform.long_double_size  = 16;
		ir_platform.long_double_align = 16;
	} else if (streq(cpu, "TEMPLATE")) {
		ir_platform.long_double_size  = 8;
		ir_platform.long_double_align = 8;
	} else {
		/** Everything that passes ir_init_target_from_triple()
		 * should work here as well. */
		panic("Unexpected ABI");
	}

	object_format_t object_format;
	if (strstr(os, "linux") != NULL) {
		object_format = OBJECT_FORMAT_ELF;

		ppdef_unix();
		ppdef1("__linux");
		ppdef1("__linux__");
		ppdef1("linux");
		if (strstr(os, "gnu") != NULL)
			ppdef1("__gnu_linux__");
	} else if (strstart(os, "freebsd")) {
		ppdef("__FreeBSD__", "");
		goto BSD;
	} else if (strstart(os, "openbsd")) {
		ppdef1("__OpenBSD__");
		ir_platform.pic_is_default = true;
		goto BSD;
	} else if (strstr(os, "bsd") != NULL) {
BSD:
		object_format = OBJECT_FORMAT_ELF;
		ir_platform.ia32_struct_in_regs = true;
		ppdef_unix();
	} else if (strstart(os, "darwin")) {
		object_format = OBJECT_FORMAT_MACH_O;
		ir_platform.is_darwin                     = true;
		ir_platform.user_label_prefix             = '_';
		ir_platform.long_double_size              = 16;
		ir_platform.long_double_align             = 16;
		ir_platform.pic_is_default                = true;
		ir_platform.ia32_po2_stackalign           = 4;
		ir_platform.ia32_struct_in_regs           = true;
		ir_platform.supports_thread_local_storage = false;

		ppdef1("__MACH__");
		ppdef1("__APPLE__");
		ppdef1("__APPLE_CC__");
		ppdef1("__CONSTANT_CFSTRINGS__");
		ppdef1("__DYNAMIC__");
		ppdef("__weak",                 "");
		ppdef("__strong",               "");
		ppdef("__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__", "1050");
	} else if (strstart(os, "mingw")) {
		object_format = OBJECT_FORMAT_PE_COFF;
		ir_platform.wchar_type                        = IR_TYPE_SHORT;
		ir_platform.wchar_is_signed                   = false;
		ir_platform.long_long_and_double_struct_align = 0;
		ir_platform.supports_thread_local_storage     = false;
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
			ir_platform.long_size    = 4;
			ir_platform.intptr_type  = IR_TYPE_LONG_LONG;
			ir_platform.amd64_x64abi = true;
		} else {
			assert(pointer_size == 4);
			ir_platform.user_label_prefix = '_';
			ir_platform.intptr_type = IR_TYPE_INT;
		}
	} else if (strstart(os, "midipix")) {
		object_format = OBJECT_FORMAT_PE_COFF;
		ir_platform.long_long_and_double_struct_align = 0;
		ir_platform.supports_thread_local_storage     = false;
		ppdef1("__midipix__");
		if (pointer_size == 8) {
			ir_platform.amd64_x64abi = true;
			ppdef1("__NT64");
		} else {
			assert(pointer_size == 4);
			ir_platform.user_label_prefix = '_';
			ppdef1("__NT32");
		}
	} else if (streq(os, "elf") || streq(os, "octopos") || streq(os, "irtss")
	        || streq(os, "unknown")) {
	    object_format = OBJECT_FORMAT_ELF;
	} else {
		object_format = OBJECT_FORMAT_ELF;
		be_warningf(NULL, "Unknown operating system '%s'", os);
	}
	ir_platform.object_format = object_format;

	switch (object_format) {
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

	option_pic       = ir_platform.pic_is_default;
	option_pic_noplt = false;
	init_lc_opts();
}

void ir_platform_init(void)
{
	/* Decide PIC style */
	be_pic_style_t style = BE_PIC_NONE;
	if (option_pic) {
		ppdef("__PIC__", "2");
		ppdef("__pic__", "2");
		switch (ir_platform.object_format) {
		case OBJECT_FORMAT_ELF:
			style = option_pic_noplt ? BE_PIC_ELF_NO_PLT : BE_PIC_ELF_PLT;
			break;
		case OBJECT_FORMAT_MACH_O:
			style = BE_PIC_MACH_O;
			break;
		case OBJECT_FORMAT_PE_COFF:
			panic("Windows PIC not implemented");
		}
	}
	ir_platform.pic_style = style;

	if (ir_platform.is_darwin && !ir_target_big_endian())
		ppdef1("__LITTLE_ENDIAN__");

	ir_platform.initialized = true;
}

ir_platform_define_t const *ir_platform_define_first(void)
{
	assert(ir_platform.initialized);
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
	assert(ir_platform.initialized);
	return ir_platform.long_long_and_double_struct_align;
}

ir_platform_type_t ir_platform_wchar_type(void)
{
	assert(ir_platform.initialized);
	return ir_platform.wchar_type;
}

int ir_platform_wchar_is_signed(void)
{
	assert(ir_platform.initialized);
	return ir_platform.wchar_is_signed;
}

ir_platform_type_t ir_platform_intptr_type(void)
{
	assert(ir_platform.initialized);
	return ir_platform.intptr_type;
}

int ir_platform_pic_is_default(void)
{
	assert(ir_platform.initialized);
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

ir_type *ir_platform_va_list_type(void)
{
	return ir_platform.va_list_type;
}

int ir_platform_supports_thread_local_storage(void)
{
	return ir_platform.supports_thread_local_storage;
}

static ir_mode *make_int_mode(ir_platform_type_t type, bool is_signed)
{
	unsigned bitsize = ir_platform_type_size(type) * 8;
	char name[16];
	snprintf(name, sizeof(name), "%c%u", is_signed ? 'I' : 'U', bitsize);
	unsigned modulo_shift = ir_target.isa->modulo_shift;
	if (modulo_shift > 0)
		modulo_shift = MAX(bitsize, modulo_shift);
	return new_int_mode(name, bitsize, is_signed, modulo_shift);
}

static ir_mode *make_float_mode(ir_platform_type_t type)
{
	ir_mode_arithmetic arithmetic = irma_ieee754;
	char const *name;
	unsigned exponent_size;
	unsigned mantissa_size;
	if (type == IR_TYPE_FLOAT) {
		name = "F32";
		exponent_size = 8;
		mantissa_size = 23;
	} else if (type == IR_TYPE_DOUBLE) {
		name = "F64";
		exponent_size = 11;
		mantissa_size = 52;
	} else {
		assert(type == IR_TYPE_LONG_DOUBLE);
		if (ir_platform.x87_long_double) {
			arithmetic = irma_x86_extended_float;
			name = "F80";
			exponent_size = 15;
			mantissa_size = 64;
		} else if (ir_platform.long_double_size == 8) {
			return make_float_mode(IR_TYPE_DOUBLE);
		} else {
			assert(ir_platform.long_double_size == 16);
			name = "F128";
			exponent_size = 15;
			mantissa_size = 112;
		}
	}
	return new_float_mode(name, arithmetic, exponent_size, mantissa_size,
	                      ir_target.float_int_overflow);
}

ir_mode *ir_platform_type_mode(ir_platform_type_t type, int is_signed)
{
	assert(ir_platform.initialized);
	switch (type) {
	case IR_TYPE_BOOL:
		assert(!is_signed && "Bool type must be unsigned");
		/* FALLTHROUGH */
	case IR_TYPE_CHAR:
	case IR_TYPE_SHORT:
	case IR_TYPE_INT:
	case IR_TYPE_LONG:
	case IR_TYPE_LONG_LONG:
		return make_int_mode(type, is_signed);
	case IR_TYPE_FLOAT:
	case IR_TYPE_DOUBLE:
	case IR_TYPE_LONG_DOUBLE:
		assert(is_signed && "Float type must be signed");
		return make_float_mode(type);
	}
	panic("Invalid object type");
}

void ir_platform_finish(void)
{
	for (ir_platform_define_t *next, *define = ir_platform.defines;
	     define != NULL; define = next) {
	    next = define->next;
	    free(define);
	}
}

void ir_platform_set_va_list_type_pointer(void)
{
	ir_platform.va_list_type = new_type_pointer(get_type_for_mode(mode_ANY));
}

ident *ir_platform_mangle_global(char const *const name)
{
	char user_label_prefix = ir_platform.user_label_prefix;
	return user_label_prefix == 0 ? new_id_from_str(name)
	                              : new_id_fmt("%c%s", user_label_prefix, name);
}
