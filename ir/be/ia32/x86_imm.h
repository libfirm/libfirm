/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Data structures for immediate values and relocations.
 * @author Matthias Braun
 */
#ifndef FIRM_BE_IA32_IMM_H
#define FIRM_BE_IA32_IMM_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include "compiler.h"
#include "firm_types.h"
#include "irmode.h"
#include "panic.h"

extern char const *x86_pic_base_label;

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
	 * actualy stack pointer offset later. */
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

static inline x86_insn_size_t x86_size_from_mode(ir_mode *const mode)
{
	switch (get_mode_size_bits(mode)) {
	case 8:   return X86_SIZE_8;
	case 16:  return X86_SIZE_16;
	case 32:  return X86_SIZE_32;
	case 64:  return X86_SIZE_64;
	case 80:  return X86_SIZE_80;
	case 128: return X86_SIZE_128;
	}
	panic("Unexpected mode");
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
