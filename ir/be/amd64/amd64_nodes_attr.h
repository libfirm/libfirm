/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   attributes attached to all amd64 nodes
 */
#ifndef FIRM_BE_AMD64_AMD64_NODES_ATTR_H
#define FIRM_BE_AMD64_AMD64_NODES_ATTR_H

#include <stdint.h>

#include "../ia32/x86_asm.h"
#include "../ia32/x86_cc.h"
#include "../ia32/x86_x87.h"
#include "compiler.h"
#include "irnode_t.h"

enum amd64_arch_irn_flags_t {
	amd64_arch_irn_flag_commutative_binop = arch_irn_flag_backend << 0,
};

/** instruction data size. Keep sorted! */
typedef enum {
	INSN_MODE_INVALID = 0,
	INSN_MODE_8,
	INSN_MODE_16,
	INSN_MODE_32,
	INSN_MODE_64,
	INSN_MODE_128,
} amd64_insn_mode_t;

typedef enum {
	AMD64_SEGMENT_DEFAULT,
	AMD64_SEGMENT_CS,
	AMD64_SEGMENT_SS,
	AMD64_SEGMENT_DS,
	AMD64_SEGMENT_ES,
	AMD64_SEGMENT_FS,
	AMD64_SEGMENT_GS,
} amd64_segment_selector_t;

typedef enum {
	AMD64_OP_NONE,
	AMD64_OP_ADDR,
	AMD64_OP_REG,
	AMD64_OP_REG_ADDR,
	AMD64_OP_REG_REG,
	AMD64_OP_REG_IMM,
	AMD64_OP_IMM32,
	AMD64_OP_IMM64,
	AMD64_OP_ADDR_REG,
	AMD64_OP_ADDR_IMM,
	AMD64_OP_SHIFT_REG,
	AMD64_OP_SHIFT_IMM,
	AMD64_OP_X87,
	AMD64_OP_X87_ADDR,
	AMD64_OP_X87_ADDR_REG,
} amd64_op_mode_t;

enum {
	NO_INPUT  = 0xFF,
	RIP_INPUT = 0xFE, /* can be used as base_input for PIC code */
};

typedef struct {
	ir_entity                   *entity;
	int64_t                      offset;
	ENUMBF(x86_immediate_kind_t) kind : 8;
} amd64_imm64_t;

typedef struct {
	x86_imm32_t immediate;
	uint8_t     base_input;
	uint8_t     index_input;
	uint8_t     mem_input;
	unsigned    log_scale : 2; /* 0, 1, 2, 3  (giving scale 1, 2, 4, 8) */
	ENUMBF(amd64_segment_selector_t) segment : 4;
} amd64_addr_t;

typedef struct {
	except_attr exc; /**< the exception attribute. MUST be the first one. */
	amd64_op_mode_t op_mode;
} amd64_attr_t;

typedef struct {
	amd64_attr_t base;
	ENUMBF(amd64_insn_mode_t) insn_mode : 3;
	amd64_addr_t addr;
} amd64_addr_attr_t;

typedef struct {
	amd64_addr_attr_t base;
	union {
		uint8_t     reg_input;
		x86_imm32_t immediate;
	} u;
} amd64_binop_addr_attr_t;

typedef struct {
	amd64_attr_t base;
	ENUMBF(amd64_insn_mode_t) insn_mode : 3;
	uint8_t                   immediate;
} amd64_shift_attr_t;

typedef struct {
	amd64_attr_t base;
	ENUMBF(amd64_insn_mode_t) insn_mode : 3;
	amd64_imm64_t             immediate;
} amd64_movimm_attr_t;

typedef struct {
	amd64_attr_t         base;
	x86_condition_code_t cc;
	ENUMBF(amd64_insn_mode_t) insn_mode       : 3;
} amd64_cc_attr_t;

typedef struct {
	amd64_attr_t           base;
	const ir_switch_table *table;
	ir_entity             *table_entity;
} amd64_switch_jmp_attr_t;

typedef struct {
	amd64_addr_attr_t  base;
	unsigned           n_reg_results; /**< number of results in registers */
	ir_type           *call_tp;
} amd64_call_addr_attr_t;

typedef struct {
	amd64_attr_t base;
	x87_attr_t   x87;
} amd64_x87_attr_t;

typedef struct {
	amd64_addr_attr_t base;
	x87_attr_t        x87;
} amd64_x87_addr_attr_t;

typedef struct {
	amd64_binop_addr_attr_t base;
	x87_attr_t              x87;
} amd64_x87_binop_addr_attr_t;

#endif
