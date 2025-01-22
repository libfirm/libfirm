/*
 * This file is part of libFirm.
 * Copyright (C) 2016 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Data structures for immediate values and relocations.
 * @author Matthias Braun
 */
#ifndef FIRM_BE_IA32_X86_NODES_ATTR_H
#define FIRM_BE_IA32_X86_NODES_ATTR_H

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include "irmode_t.h"
#include "panic.h"

/** x86 condition codes (the numbers correspond to the real encoding order) */
typedef enum x86_condition_code_t {
	x86_cc_negated       = 0x01, /**< negates condition */

	x86_cc_overflow      = 0x00,                              /**< OF=1 */
	x86_cc_below         = 0x02,                              /**< CF=1 */
	x86_cc_carry         = x86_cc_below,
	x86_cc_equal         = 0x04,                              /**< ZF=1 */
	x86_cc_below_equal   = 0x06,                              /**< ZF=1 or CF=1 */
	x86_cc_sign          = 0x08,                              /**< SF=1 */
	x86_cc_parity        = 0x0A,                              /**< PF=1 */
	x86_cc_less          = 0x0C,                              /**< SF!=OF */
	x86_cc_less_equal    = 0x0E,                              /**< ZF=1 or SF!=OF */
	x86_cc_not_overflow  = x86_cc_negated|x86_cc_overflow,    /**< OF=0 */
	x86_cc_above_equal   = x86_cc_negated|x86_cc_below,       /**< CF=0 */
	x86_cc_not_equal     = x86_cc_negated|x86_cc_equal,       /**< ZF=0 */
	x86_cc_above         = x86_cc_negated|x86_cc_below_equal, /**< ZF=0 and CF=0 */
	x86_cc_not_sign      = x86_cc_negated|x86_cc_sign,        /**< SF=0 */
	x86_cc_not_parity    = x86_cc_negated|x86_cc_parity,      /**< PF=0 */
	x86_cc_greater_equal = x86_cc_negated|x86_cc_less,        /**< SF=OF */
	x86_cc_greater       = x86_cc_negated|x86_cc_less_equal,  /**< ZF=0 and SF=OF */

	/* the following codes are (unfortunately) NOT real hardware codes but
	 * simplify our backend as you need these combinations for some
	 * floatingpoint compares (the emitter will split them into multiple
	 * instructions) */
	x86_cc_float_parity_cases = 0x20,
	/* we need even more cases as inversing the cc is different for float
	 * comparisons (though for the following we need no special
	 * parity+x combinations) */
	x86_cc_additional_float_cases = 0x10,

	/* make sure that the lower 4 bit correspond to the real encoding
	 * (of the comparison not involving the parity special) */
	x86_cc_float_equal        = 0x34,                              /**< PF=0 and ZF=1 */
	x86_cc_float_below        = 0x32,                              /**< PF=0 and CF=1 */
	x86_cc_float_below_equal  = 0x36,                              /**< PF=0 and (ZF=1 or CF=1) */
	x86_cc_float_not_equal    = x86_cc_negated|x86_cc_float_equal, /**< PF=1 or ZF=0 */
	x86_cc_float_unordered_above_equal
		= x86_cc_negated|x86_cc_float_below,                       /**< PF=1 or CF=0 */
	x86_cc_float_unordered_above
		= x86_cc_negated|x86_cc_float_below_equal,                 /**< PF=1 or (ZF=0 and CF=0) */

	x86_cc_float_unordered_below_equal = 0x16,                     /**< ZF=1 or CF=1 */
	x86_cc_float_unordered_below       = 0x12,                     /**< CF=1 */
	x86_cc_float_above
		= x86_cc_negated|x86_cc_float_unordered_below_equal,       /**< ZF=0 and CF=0 */
	x86_cc_float_above_equal
		= x86_cc_negated|x86_cc_float_unordered_below,             /**< CF=0 */
} x86_condition_code_t;
ENUM_BITSET(x86_condition_code_t)

/** instruction data size. Keep sorted! */
typedef enum x86_insn_size_t {
	X86_SIZE_8,
	X86_SIZE_16,
	X86_SIZE_32,
	X86_SIZE_64,
	X86_SIZE_80,
	X86_SIZE_128,
} x86_insn_size_t;

/** immediate/relocation types (see also ELF file format) */
typedef enum x86_immediate_kind_t {
	X86_IMM_VALUE,       /**< no relocation, just a value */
	X86_IMM_ADDR,        /**< "normal" absolute addresses to a symbol */
	X86_IMM_PCREL,       /**< PC relative address */
	X86_IMM_PICBASE_REL, /**< relative to pic base address */
	X86_IMM_TLS_IE,      /**< thread local storage, initial exec */
	X86_IMM_TLS_LE,      /**< thread local storage, load exec */
	X86_IMM_FRAMEENT,    /**< offset to entity on stackframe */
	/** The offset field specifies an offset relative to the SP value at
	 * the beginning of the function. The offset will be adjusted to the
	 * actual stack pointer offset later. */
	X86_IMM_FRAMEOFFSET,
	X86_IMM_GOTPCREL,    /**< global offset table entry PC relative (elf64) */
	X86_IMM_GOTOFF,      /**< address relative to global offset table */
	X86_IMM_GOT,         /**< global offset table entry offset */
	X86_IMM_PLT,         /**< address to entry in procedure linkage table */
} x86_immediate_kind_t;

typedef struct x86_imm32_t {
	ir_entity                   *entity;
	int32_t                      offset;
	ENUMBF(x86_immediate_kind_t) kind:8;
} x86_imm32_t;

extern char const *x86_pic_base_label;

static inline x86_condition_code_t x86_negate_condition_code(
		x86_condition_code_t code)
{
	return code ^ x86_cc_negated;
}

static inline x86_condition_code_t x86_invert_condition_code(
		x86_condition_code_t code)
{
	/* doesn't appear to have any systematic, so use a table */
	switch (code) {
	case x86_cc_below:             return x86_cc_above;
	case x86_cc_below_equal:       return x86_cc_above_equal;
	case x86_cc_above:             return x86_cc_below;
	case x86_cc_above_equal:       return x86_cc_below_equal;
	case x86_cc_less:              return x86_cc_greater;
	case x86_cc_less_equal:        return x86_cc_greater_equal;
	case x86_cc_greater:           return x86_cc_less;
	case x86_cc_greater_equal:     return x86_cc_less_equal;
	case x86_cc_float_below:       return x86_cc_float_above;
	case x86_cc_float_below_equal: return x86_cc_float_above_equal;
	case x86_cc_float_above:       return x86_cc_float_below;
	case x86_cc_float_above_equal: return x86_cc_float_below_equal;
	case x86_cc_float_unordered_below:       return x86_cc_float_unordered_above;
	case x86_cc_float_unordered_below_equal: return x86_cc_float_unordered_above_equal;
	case x86_cc_float_unordered_above:       return x86_cc_float_unordered_below;
	case x86_cc_float_unordered_above_equal: return x86_cc_float_unordered_below_equal;
	default:                       return code;
	}
}

x86_condition_code_t ir_relation_to_x86_condition_code(ir_relation relation,
                                                       ir_mode *mode,
                                                       bool overflow_possible);

void x86_emit_condition_code(x86_condition_code_t cc);

bool x86_match_immediate(x86_imm32_t *immediate, const ir_node *node,
                         char constraint);

char const *x86_get_immediate_kind_str(x86_immediate_kind_t const kind);

void x86_dump_imm32(x86_imm32_t const *imm, FILE *F);

void x86_emit_imm32(x86_imm32_t const *imm);
void x86_emit_relocation_no_offset(x86_immediate_kind_t kind,
                                   ir_entity const *entity);

static inline bool x86_imm32_equal(x86_imm32_t const *const imm0,
								   x86_imm32_t const *const imm1)
{
	return imm0->entity == imm1->entity && imm0->offset == imm1->offset
	    && imm0->kind == imm1->kind;
}

static inline x86_insn_size_t x86_size_from_bytes(unsigned bytes)
{
	switch (bytes) {
	case 1:  return X86_SIZE_8;
	case 2:  return X86_SIZE_16;
	case 4:  return X86_SIZE_32;
	case 8:  return X86_SIZE_64;
	case 10: return X86_SIZE_80;
	case 16: return X86_SIZE_128;
	}
	panic("Unexpected size");
}

static inline x86_insn_size_t x86_size_from_mode(ir_mode *const mode)
{
	return x86_size_from_bytes(get_mode_size_bytes(mode));
}

static inline unsigned x86_bytes_from_size(x86_insn_size_t const size)
{
	switch (size) {
	case X86_SIZE_8:   return 1;
	case X86_SIZE_16:  return 2;
	case X86_SIZE_32:  return 4;
	case X86_SIZE_64:  return 8;
	case X86_SIZE_80:  return 10;
	case X86_SIZE_128: return 16;
	}
	panic("Invalid size");
}

#endif
