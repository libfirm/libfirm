/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Generic backend types and interfaces.
 * @author      Sebastian Hack
 */
#ifndef FIRM_BE_MAIN_H
#define FIRM_BE_MAIN_H

#include <stdio.h>
#include "irarch.h"
#include "lowering.h"
#include "iroptimize.h"
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
	/** the input is not written to */
	ASM_CONSTRAINT_FLAG_MODIFIER_NO_WRITE     = 1u << 5,
	/** the input is read */
	ASM_CONSTRAINT_FLAG_MODIFIER_READ         = 1u << 6,
	/** the input is not read */
	ASM_CONSTRAINT_FLAG_MODIFIER_NO_READ      = 1u << 7,
	/** the value is modified before all inputs to the asm block
	 * are handled. */
	ASM_CONSTRAINT_FLAG_MODIFIER_EARLYCLOBBER = 1u << 8,
	/** This operand and the following operand are commutative */
	ASM_CONSTRAINT_FLAG_MODIFIER_COMMUTATIVE  = 1u << 9,
	/** invalid constraint (due to parse error) */
	ASM_CONSTRAINT_FLAG_INVALID               = 1u << 10
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
 * Build a Trampoline for the closure.
 * @param block       the block where to build the trampoline
 * @param mem         memory
 * @param trampoline  address of a trampoline region
 * @param env         address of the environment
 * @param callee      address of the function to call
 *
 * @return modified memory
 */
typedef ir_node *(create_trampoline_fkt)(ir_node *block, ir_node *mem, ir_node *trampoline, ir_node *env, ir_node *callee);

/**
 * This structure contains parameters that should be
 * propagated to the libFirm parameter set.
 */
typedef struct backend_params {
	/** If set, the backend supports Rotl nodes */
	unsigned support_rotl:1;
	/** the backend uses big-endian byte ordering if set, else little endian */
	unsigned byte_order_big_endian:1;
	/** whether the architecure can natively handle modulo shift modes.
	 * If this is true, then you can assume that shifting in modes with
	 * module_shift==machine_size (if mode size is <= machine_size) is efficient
	 */
	unsigned modulo_shift_efficient:1;
	/** whether the architecure can natively handle modulo shift modes.
	 * If this is true, then you can assume that shifting without modulo shift
	 * is efficient
	 */
	unsigned non_modulo_shift_efficient:1;

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

	/** Size of the trampoline code. */
	unsigned trampoline_size;

	/** Alignment of the trampoline code. */
	unsigned trampoline_align;

	/** If non-zero, build the trampoline. */
	create_trampoline_fkt *build_trampoline;

	/** Alignment of stack parameters */
	unsigned stack_param_align;
} backend_params;

/**
 * Parse one backend argument.
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
 * should perform further architecture-neutral optimisations before be_main.
 */
FIRM_API void be_lower_for_target(void);

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
