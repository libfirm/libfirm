/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Handling of x86 inline assembly
 * @author Matthias Braun
 * Matching and emitting of inline assembly nodes.
 */
#ifndef FIRM_BE_IA32_X86_ASM_H
#define FIRM_BE_IA32_X86_ASM_H

#include <stdbool.h>
#include <stdint.h>
#include "be_types.h"
#include "compiler.h"
#include "firm_types.h"
#include "util.h"

typedef struct x86_imm32_t {
	ir_entity *entity;
	int32_t    offset;
} x86_imm32_t;

typedef enum x86_asm_operand_kind_t {
	ASM_OP_INVALID,
	ASM_OP_IN_REG,
	ASM_OP_OUT_REG,
	ASM_OP_MEMORY,
	ASM_OP_IMMEDIATE,
} x86_asm_operand_kind_t;

typedef struct x86_asm_operand_t {
	ENUMBF(x86_asm_operand_kind_t) kind : 3;
	uint8_t inout_pos; /**< in/out pos where register is assigned */
	union {
		ir_mode    *mode;
		x86_imm32_t imm32;
	} u;
} x86_asm_operand_t;

typedef struct x86_asm_attr_t {
	ident                   *asm_text;
	const x86_asm_operand_t *operands;
} x86_asm_attr_t;

typedef struct x86_clobber_name_t {
	const char *name;
	unsigned    index;
} x86_clobber_name_t;

typedef enum x86_asm_constraint_kind_t {
	MATCH_INVALID,
	MATCH_REG,
	MATCH_IMM,
	MATCH_MEM,
	MATCH_ANY,
} x86_asm_constraint_kind_t;

typedef struct x86_asm_constraint_t {
	x86_asm_constraint_kind_t    kind;
	const arch_register_class_t *cls;
	unsigned                     limited;
} x86_asm_constraint_t;

typedef x86_asm_constraint_t x86_asm_constraint_list_t[128];

static inline bool x86_asm_attr_equal(const x86_asm_attr_t *attr0,
                                      const x86_asm_attr_t *attr1)
{
	return attr0->asm_text == attr1->asm_text;
}

typedef void (*emit_register_func)(const arch_register_t *reg, char modifier,
                                   ir_mode *mode);

void x86_emit_asm(const ir_node *node, const x86_asm_attr_t *attr,
                  emit_register_func emit_register);

const arch_register_t *x86_parse_clobber(const arch_env_t *arch_env,
	const x86_clobber_name_t *additional_clobber_names, const char *name);

typedef ir_node* (*new_bd_asm_func)(dbg_info *dbgi, ir_node *block, int arity,
                                    ir_node *in[], int out_arity,
                                    const x86_asm_attr_t *attr);

ir_node *x86_match_ASM(const ir_node *node, new_bd_asm_func new_bd_asm,
                       const x86_clobber_name_t *names,
                       const x86_asm_constraint_list_t *constraints);

bool x86_match_immediate(x86_imm32_t *immediate, const ir_node *node,
                         char constraint);

void x86_set_be_asm_constraint_support(const x86_asm_constraint_list_t *constraints);

#endif
