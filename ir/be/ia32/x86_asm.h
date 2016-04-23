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
#include "x86_node.h"

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

typedef void (*emit_register_func)(const arch_register_t *reg, char modifier,
                                   ir_mode *mode);

arch_register_t const *x86_parse_clobber(x86_clobber_name_t const *additional_clobber_names, char const *name);

ir_node *x86_match_ASM(ir_node const *node, x86_clobber_name_t const *names, x86_asm_constraint_list_t const *constraints);

void x86_set_be_asm_constraint_support(const x86_asm_constraint_list_t *constraints);

char const *x86_get_constraint_name(x86_asm_operand_kind_t);

#endif
