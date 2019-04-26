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
#include "irmode.h"
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
	/** input/output can be in a register, in memory or an immediate */
	ASM_CONSTRAINT_FLAG_SUPPORTS_ANY          = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE | ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP | ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER,
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
