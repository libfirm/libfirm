/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Matthias Braun
 */

/**
 * @file
 * @brief       ia32 binary encoding/emissiong
 * @author      Matthias Braun, Christoph Mallon
 */
#ifndef FIRM_BE_IA32_IA32_ENCODE_H
#define FIRM_BE_IA32_IA32_ENCODE_H

#include <stdint.h>
#include "firm_types.h"
#include "jit.h"

enum {
	IA32_RELOCATION_RELJUMP = 128,
};

ir_jit_function_t *ia32_emit_jit(ir_jit_segment_t *segment, ir_graph *irg);

void ia32_emit_jit_function(char *buffer, ir_jit_function_t *function);

void ia32_enc_simple(uint8_t opcode);

void ia32_enc_binop(ir_node const *node, unsigned code);

void ia32_enc_binop_mem(ir_node const *node, unsigned code);

void ia32_enc_shiftop(ir_node const *node, uint8_t ext);

void ia32_enc_shiftop_mem(ir_node const *node, uint8_t ext);

void ia32_enc_unop(ir_node const *node, uint8_t code, uint8_t ext, int input);

void ia32_enc_unop_mem(ir_node const *node, uint8_t code, uint8_t ext);

void ia32_enc_0f_unop_reg(ir_node const *node, uint8_t code, int input);

void ia32_enc_fsimple(uint8_t opcode);

void ia32_enc_fbinop(ir_node const *node, unsigned op_fwd, unsigned op_rev);

void ia32_enc_fop_reg(ir_node const *node, uint8_t op0, uint8_t op1);

#endif
