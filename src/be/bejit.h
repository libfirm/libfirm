/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Interface for binary machine code output.
 * @author      Matthias Braun
 * @date        12.03.2007
 */
#ifndef FIRM_BE_BEEMITTER_BINARY_H
#define FIRM_BE_BEEMITTER_BINARY_H

#include <stdint.h>

#include "firm_types.h"
#include "jit.h"
#include "obst.h"

typedef unsigned (*emit_relocation_func) (char *buffer, uint8_t be_kind,
                                          ir_entity *entity, int32_t offset);

typedef struct be_jit_emit_interface_t {
	/** create @p size of NOP instructions for alignment */
	void (*nops) (char *buffer, unsigned size);

	/** dest is an absolute address for RELOC_DEST_ENTITY and a relative
	 * offset for RELOC_DEST_CODE_FRAGMENT (cast to int64_t in this case) */
	emit_relocation_func relocation;
} be_jit_emit_interface_t;

void be_jit_emit_memory(char *buffer, ir_jit_function_t *function,
                        be_jit_emit_interface_t const *emitter);

void be_jit_emit_as_asm(ir_jit_function_t *function, emit_relocation_func emit);

void be_jit_begin_function(ir_jit_segment_t *segment);
ir_jit_function_t *be_jit_finish_function(void);

unsigned be_begin_fragment(uint8_t p2align, uint8_t max_skip);
void be_finish_fragment(void);

extern struct obstack *code_obst;

/** Append a byte to the current fragment */
static inline void be_emit8(uint8_t const byte)
{
	obstack_1grow(code_obst, byte);
}

/** Append a word (16bits) to the current fragment */
static inline void be_emit16(uint16_t const u16)
{
	obstack_grow(code_obst, &u16, 2);
}

/** Append a dword (32bits) to the current fragment */
static inline void be_emit32(uint32_t const u32)
{
	obstack_grow(code_obst, &u32, 4);
}

void be_emit_reloc_fragment(unsigned len, uint8_t be_kind,
                            unsigned fragment_num, int32_t offset);

void be_emit_reloc_entity(unsigned len, uint8_t be_kind, ir_entity *entity,
                          int32_t offset);

#endif
