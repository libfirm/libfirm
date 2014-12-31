/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Generic backend types and interfaces.
 * @author      Sebastian Hack
 */
#ifndef FIRM_BE_H
#define FIRM_BE_H

#include <stdio.h>
#include "irarch.h"
#include "lowering.h"
#include "iroptimize.h"
#include "irmode.h"
#include "begin.h"

/**
 * @defgroup be  Code Generation
 *
 * Code Generation (backend) produces machine-code.
 * @{
 */

/**
 * flags categorizing assembler constraint specifications
 */
typedef enum asm_constraint_flags_t {
	ASM_CONSTRAINT_FLAG_NONE                  = 0, /**< no constraints */
	/** input/output can be in a register */
	ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER     = 1u << 0,
	/** input/output can be read/written to/from a memory address */
	ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP        = 1u << 1,
	/** input can be encoded as an immediate number */
	ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE    = 1u << 2,
	/** the constraint is not supported yet by libFirm (but valid in gcc) */
	ASM_CONSTRAINT_FLAG_NO_SUPPORT            = 1u << 3,
	/** The input is also written to */
	ASM_CONSTRAINT_FLAG_MODIFIER_WRITE        = 1u << 4,
	/** the input is read */
	ASM_CONSTRAINT_FLAG_MODIFIER_READ         = 1u << 5,
	/** the value is modified before all inputs to the asm block
	 * are handled. */
	ASM_CONSTRAINT_FLAG_MODIFIER_EARLYCLOBBER = 1u << 6,
	/** This operand and the following operand are commutative */
	ASM_CONSTRAINT_FLAG_MODIFIER_COMMUTATIVE  = 1u << 7,
	/** invalid constraint (due to parse error) */
	ASM_CONSTRAINT_FLAG_INVALID               = 1u << 8
} asm_constraint_flags_t;
ENUM_BITSET(asm_constraint_flags_t)

/** Dwarf source language codes. */
typedef enum {
	DW_LANG_C89 = 0x0001,
	DW_LANG_C = 0x0002,
	DW_LANG_Ada83 = 0x0003,
	DW_LANG_C_plus_plus = 0x0004,
	DW_LANG_Cobol74 = 0x0005,
	DW_LANG_Cobol85 = 0x0006,
	DW_LANG_Fortran77 = 0x0007,
	DW_LANG_Fortran90 = 0x0008,
	DW_LANG_Pascal83 = 0x0009,
	DW_LANG_Modula2 = 0x000a,
	DW_LANG_Java = 0x000b,
	DW_LANG_C99 = 0x000c,
	DW_LANG_Ada95 = 0x000d,
	DW_LANG_Fortran95 = 0x000e,
	DW_LANG_PLI = 0x000f,
	DW_LANG_ObjC = 0x0010,
	DW_LANG_ObjC_plus_plus = 0x0011,
	DW_LANG_UPC = 0x0012,
	DW_LANG_D = 0x0013,
	DW_LANG_Python = 0x0014,
	DW_LANG_Go = 0x0016,
} dwarf_source_language;

/**
 * This structure contains parameters that should be
 * propagated to the libFirm parameter set.
 */
typedef struct backend_params {
	/** the backend uses big-endian byte ordering if set, else little endian */
	unsigned byte_order_big_endian:1;
	/** 1 if backend supports generation of position independent code (PIC) */
	unsigned pic_supported:1;
	/** unaligned memory accesses are not supported natively (but the backend
	 * may break the access up into several smaller ones) */
	unsigned unaligned_memaccess_supported:1;

	/**
	 * Shifts on this architecture only read some bits of the shift value.
	 * For example on x86 for every mode with less than 32bits only 5 bits of
	 * the shift value are read resulting in a modulo shift value of 32.
	 * On an architecture without modulo_shift this value is 0.
	 */
	unsigned modulo_shift;

	/** Settings for architecture dependent optimizations. */
	const ir_settings_arch_dep_t *dep_param;

	/** Backend settings for if-conversion. */
	arch_allow_ifconv_func allow_ifconv;

	/** size of machine word in bits. This is usually the size of the general
	 * purpose integer/address registers. */
	unsigned machine_size;

	/**
	 * some backends like x87 can only do arithmetic in a specific float
	 * mode (load/store are still done in the "normal" float/double modes).
	 */
	ir_mode *mode_float_arithmetic;

	/**
	 * type used for long long or NULL if none available.
	 */
	ir_type *type_long_long;

	/**
	 * type used for unsigned long long or NULL if none available
	 */
	ir_type *type_unsigned_long_long;

	/**
	 * type used for long double or NULL if none available.
	 */
	ir_type *type_long_double;

	/** Alignment of stack parameters */
	unsigned stack_param_align;

	/** Semantic on float->int conversion overflow. */
	float_int_conversion_overflow_style_t float_int_overflow;
} backend_params;

/**
 * Parse one backend argument. This is intended to provide commandline options
 * to various backend parameters that might be changing.
 * Returns -1 if 'help' was found, 0 if the argument could not be parsed,
 * 1 if the option could be set.
 */
FIRM_API int be_parse_arg(const char *arg);

/**
 * Returns 1 if the backend uses big-endian byte ordering
 * and 0 for little-endian.
 */
FIRM_API int be_is_big_endian(void);

/**
 * Returns size of machine words. This is usually the size
 * of the general purpose integer registers.
 */
FIRM_API unsigned be_get_machine_size(void);

/**
 * Returns supported float arithmetic mode or NULL if mode_D and mode_F
 * are supported natively.
 * Some backends like x87 can only do arithmetic in a specific float
 * mode (load/store are still done in the "normal" float/double modes).
 */
FIRM_API ir_mode *be_get_mode_float_arithmetic(void);

/** Returns type used for long long or NULL if none available. */
FIRM_API ir_type *be_get_type_long_long(void);

/** Returns type used for unsigned long long or NULL if none available. */
FIRM_API ir_type *be_get_type_unsigned_long_long(void);

/** Returns type used for long double or NULL if none available. */
FIRM_API ir_type *be_get_type_long_double(void);

/** Returns the backend behaviour on float to integer conversion overflow. */
FIRM_API float_int_conversion_overflow_style_t be_get_float_int_overflow(void);

/**
 * Returns the backend configuration parameter.
 *
 * @return libFirm configuration parameters for the selected
 *         backend
 */
FIRM_API const backend_params *be_get_backend_param(void);

/**
 * Lowers current program for the target architecture.
 * This must be run once before using be_main. The idea here is that the backend
 * can perform lowerings like doubleword-lowering, ABI adjustments or
 * implementation of boolean values, if-conversion, with target specific
 * settings.
 * The resulting graph is still a "normal" firm-graph on which you can and
 * should perform further architecture-neutral optimizations before be_main.
 */
FIRM_API void be_lower_for_target(void);

typedef void (*after_transform_func)(ir_graph *irg, const char *name);

/**
 * Sets a callback that is called after each transformation step in
 * be_lower_for_target(). This is typically used to run dump & verify steps
 * to help debugging.
 */
FIRM_API void be_set_after_transform_func(after_transform_func func);

/**
 * Main interface to the frontend.
 */
FIRM_API void be_main(FILE *output, const char *compilation_unit_name);

/**
 * parse assembler constraint strings and returns flags (so the frontend knows
 * which operands are inputs/outputs and whether memory is required)
 */
FIRM_API asm_constraint_flags_t be_parse_asm_constraints(const char *constraints);

/**
 * tests whether a string is a valid clobber in an ASM instruction
 */
FIRM_API int be_is_valid_clobber(const char *clobber);

/**
 * Sets source language for dwarf debug information.
 */
FIRM_API void be_dwarf_set_source_language(dwarf_source_language language);

/**
 * Sets working directory of the compiler (or directory where the compiler
 * searched for sources) for dwarf debug information.
 */
FIRM_API void be_dwarf_set_compilation_directory(const char *directory);

/** @} */

#include "end.h"

#endif
