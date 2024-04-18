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
#include "beasm.h"
#include "compiler.h"
#include "firm_types.h"
#include "util.h"
#include "x86_address_mode.h"
#include "x86_node.h"

typedef struct x86_asm_operand_t {
	be_asm_operand_t op;
	union {
		ir_mode     *mode;
		x86_imm32_t  imm32;
		x86_addr_t   addr;
	} u;
} x86_asm_operand_t;

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

ir_node *x86_match_ASM(ir_node const *node, x86_asm_constraint_list_t const *constraints);

void x86_set_be_asm_constraint_support(const x86_asm_constraint_list_t *constraints);

#endif
