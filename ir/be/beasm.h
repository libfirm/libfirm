/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Helper functions to handle inline assembler nodes.
 */
#ifndef FIRM_BE_BEASM_H
#define FIRM_BE_BEASM_H

#include <stdbool.h>

#include "be_types.h"
#include "firm_types.h"
#include "obstack.h"

typedef enum be_asm_operand_kind_t {
	BE_ASM_OPERAND_INVALID,
	BE_ASM_OPERAND_INPUT_VALUE,
	BE_ASM_OPERAND_OUTPUT_VALUE,
	BE_ASM_OPERAND_IMMEDIATE,
	BE_ASM_OPERAND_MEMORY,
} be_asm_operand_kind_t;

/**
 * An assembler constraint.
 */
typedef struct be_asm_constraint_t {
	arch_register_class_t const *cls;
	unsigned                     allowed_registers;
	bool                         all_registers_allowed;
	bool                         memory_possible;
	char                         immediate_type;
	int                          same_as;
} be_asm_constraint_t;

arch_register_req_t const *be_make_register_req(struct obstack *obst, be_asm_constraint_t const *c, int n_outs, arch_register_req_t const **out_reqs, int pos);

typedef void parse_constraint_letter_func_t(void const *env, be_asm_constraint_t*, char l);

void be_parse_asm_constraints_internal(be_asm_constraint_t *constraint, ident *constraint_text, bool is_output, parse_constraint_letter_func_t *parse_constraint_letter, void const *env);

/* Determine number of operands. */
unsigned be_count_asm_operands(ir_node const *node);

ir_node *be_make_asm(ir_node const *node, ir_node **in, arch_register_req_t const **in_reqs, arch_register_req_t const **out_reqs, void *operands);

typedef void be_emit_asm_operand_func(ir_node const *asmn, char modifier, unsigned pos);

void be_emit_asm(ir_node const *asmn, be_emit_asm_operand_func *emit_asm_operand);

bool be_is_valid_asm_operand_kind(ir_node const *node, char modifier, unsigned pos, be_asm_operand_kind_t have, char const *mod_any, char const *mod_imm, char const *mod_mem);

#endif
