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

/** immediate/relocation types (see also ELF file format) */
typedef enum x86_immediate_kind_t {
	X86_IMM_VALUE,       /**< no relocation, just a value */
	X86_IMM_ADDR,        /**< "normal" absolute addresses to a symbol */
	X86_IMM_PICBASE_REL, /**< relative to pic base address */
	X86_IMM_TLS_IE,      /**< thread local storage, initial exec */
	X86_IMM_TLS_LE,      /**< thread local storage, load exec */
	X86_IMM_FRAMEOFFSET, /**< offset to entity on stackframe */
	X86_IMM_GOTPCREL,    /**< global offset table entry PIC relative (elf64) */
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

static inline bool x86_imm32_equal(x86_imm32_t const *const imm0,
								   x86_imm32_t const *const imm1)
{
	return imm0->entity == imm1->entity && imm0->offset == imm1->offset
	    && imm0->kind == imm1->kind;
}

#endif
