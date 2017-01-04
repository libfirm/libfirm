/*
 * This file is part of libFirm.
 * Copyright (C) 2017 Matthias Braun <matze@braunis.de>
 */
#ifndef FIRM_TARGET_H
#define FIRM_TARGET_H

#include "firm_types.h"

#include "begin.h"

/**
 * @defgroup target Target Selection
 * @{
 */

/** Returns 1 if target uses big endian byte order, 0 if for little endian. */
FIRM_API int ir_target_big_endian(void);

/** Returns the biggest alignment required for any target data access. */
FIRM_API unsigned ir_target_biggest_alignment(void);

/**
 * Returns modulo shift value for the target.
 * Shifts on this architecture only read some bits of the shift value.
 * For example on x86 for every mode with less than 32bits only 5 bits of
 * the shift value are read resulting in a modulo shift value of 32.
 * On an architecture without modulo_shift this value is 0.
 */
FIRM_API unsigned ir_target_modulo_shift(void);

/**
 * Returns the size of a pointer in bytes for the target.
 */
FIRM_API unsigned ir_target_pointer_size(void);

/**
 * @}
 */

/**
 * @defgroup platform Target platform information
 * @{
 */

/**
 * Object types from the C programming language.
 * This is a simplified list that does not differentiate between signed and
 * unsigned types.
 */
typedef enum ir_platform_type_t {
	IR_TYPE_BOOL,
	IR_TYPE_CHAR,
	IR_TYPE_SHORT,
	IR_TYPE_INT,
	IR_TYPE_LONG,
	IR_TYPE_LONG_LONG,
	IR_TYPE_FLOAT,
	IR_TYPE_DOUBLE,
	IR_TYPE_LONG_DOUBLE
} ir_platform_type_t;

typedef struct ir_platform_define_t ir_platform_define_t;

/**
 * Returns 0 for default behaviour or an override value for the alignment
 * of the long long and double type in C compound types.
 */
FIRM_API unsigned ir_platform_long_long_and_double_struct_align_override(void);

/**
 * Returns 1 if the x87 80bit float type should be used for the long double
 * type.
 */
FIRM_API int ir_platform_x87_long_double(void);

/**
 * Returns 1 if target should compile position independent code by default.
 */
FIRM_API int ir_platform_pic_is_default(void);

/**
 * Returns the first element in the list of target preprocessor defines.
 */
FIRM_API ir_platform_define_t const *ir_platform_define_first(void);

/**
 * Returns the element following \p define in the list of target preprocessor
 * defines or NULL if \p define is the last element.
 */
FIRM_API ir_platform_define_t const *ir_platform_define_next(
		ir_platform_define_t const *define);

/**
 * Returns the name of the preprocessor define \p define.
 */
FIRM_API char const *ir_platform_define_name(
		ir_platform_define_t const *define);

/**
 * Returns the value that the proprocessor define \p define should be defined
 * to.
 */
FIRM_API char const *ir_platform_define_value(
		ir_platform_define_t const *define);

/**
 * Returns the C type corresponding to wchar_t.
 * Note that technically in C++ wchar_t is an own type; this function will
 * return a type with the same characteristics anyway.
 */
FIRM_API ir_platform_type_t ir_platform_wchar_type(void);

/**
 * Returns true if the wchar_t type is signed.
 */
FIRM_API int ir_platform_wchar_is_signed(void);

/**
 * Returns C type used for pointer sized integer like intptr_t or size_t.
 */
FIRM_API ir_platform_type_t ir_platform_intptr_type(void);

/**
 * Returns the size of the C type \p type in bytes.
 */
FIRM_API unsigned ir_platform_type_size(ir_platform_type_t type);

/**
 * Returns the alignment of the C type \p type in bytes.
 */
FIRM_API unsigned ir_platform_type_align(ir_platform_type_t type);

/**
 * Returns the character "normal" linker identifiers are prefixed with.
 */
FIRM_API char ir_platform_user_label_prefix(void);

/**
 * Returns the default executable name for the target (a.out or a.exe).
 */
FIRM_API char const *ir_platform_default_exe_name(void);

/**
 * Simple name mangling for target platform. This usually just adds the
 * user_label_prefix ('_' on win32, macOS).
 */
FIRM_API ident *platform_mangle_global(ident *id);

/**
 * @}
 */


/**
 * @defgroup machine_triple Machine Triple
 *
 * Functions to work with machine triples. A machine triple describes a
 * combination of CPU type, manufacturer and operating system.
 * A machine triple is specified as a single string of the form
 * 'cpu-manufacturer-system' where 'system' may be the name of the operating
 * system or just the name of the kernel.
 * This is the same concept as the target triplet in autoconfs
 * config.guess/config.sub. Or LLVMs triple.
 *
 * The manufacturer and system part may be left out, "none" or "unknown".
 * There is some built-in disambiguation logic that recognizes linux-gnu as the
 * operating system even when the manufacturer was left out.
 *
 * "unknown" and "elf" may be used for the operating system resulting in
 * ELF objects being produced without assumptions about the operating system.
 *
 * Examples:
 *
 * Triple                  | CPU    | Manufacturer  | Operating System
 * ----------------------- | ------ | ------------- | -----------------
 * i686-linux-gnu          | i686   | unknown       | linux-gnu
 * i686-unknown-linux-gnu  | i686   | unknown       | linux-gnu
 * x86_64-pc-mingw32       | x86_64 | pc            | mingw32
 * i686-apple-darwin       | x86_64 | apple         | darwin
 * x86_64-apple-darwin16.4 | x86_64 | apple         | darwin16.4
 * sparc-leon-elf          | sparc  | leon          | elf
 * arm-eabi                | arm    | unknown       | eabi
 * arm-none-eabi           | arm    | unknown       | eabi
 * mips--                  | mips   | unknown       | unknown
 * mips-                   | mips   | unknown       | unknown
 * mips                    | mips   | unknown       | unknown
 *
 * Contrary to most functions, the triple manipulation functions may be used
 * safely before ir_init() is called.
 * @{
 */

/**
 * Parses machine triple string \p triple_string into a ir_machine_triple_t
 * structure.
 */
FIRM_API ir_machine_triple_t *ir_parse_machine_triple(
		const char *triple_string);

/**
 * Creates a machine triple matching the host machine. Always succeeds
 * or calls panic().
 */
FIRM_API ir_machine_triple_t *ir_get_host_machine_triple(void);

/** Returns the CPU type part of machine triple \p triple. */
FIRM_API const char *ir_triple_get_cpu_type(const ir_machine_triple_t *triple);

/** Returns the manufacturer part of machine triple \p triple. */
FIRM_API const char *ir_triple_get_manufacturer(
		const ir_machine_triple_t *triple);

/** Returns the operating system part of machine triple \p triple. */
FIRM_API const char *ir_triple_get_operating_system(
		const ir_machine_triple_t *triple);

/** Changes CPU type of \p triple to \p cpu_type. */
FIRM_API void ir_triple_set_cpu_type(ir_machine_triple_t *triple,
                                     const char *cpu_type);

/** Frees the machine triple structure \p triple. */
FIRM_API void ir_free_machine_triple(ir_machine_triple_t *triple);

/**
 * @}
 */

#include "end.h"

#endif
