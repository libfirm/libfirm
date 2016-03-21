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

/**
 * An assembler constraint.
 */
typedef struct be_asm_constraint_t {
	arch_register_class_t const *cls;
	unsigned                     allowed_registers;
	bool                         all_registers_allowed;
	bool                         memory_possible;
	char                         immediate_type;
	same_as_t                    same_as;
} be_asm_constraint_t;

arch_register_req_t const *be_make_register_req(struct obstack *obst, be_asm_constraint_t const *c, int n_outs, arch_register_req_t const **out_reqs, int pos);

typedef void parse_constraint_letter_func_t(void const *env, be_asm_constraint_t*, char l);

void be_parse_asm_constraints_internal(be_asm_constraint_t *constraint, ident *constraint_text, bool is_output, parse_constraint_letter_func_t *parse_constraint_letter, void const *env);

/* Determine number of operands. */
unsigned be_count_asm_operands(ir_node const *node);

ir_node *be_make_asm(ir_node const *node, ir_node **in, arch_register_req_t const **in_reqs, arch_register_req_t const **out_reqs, void *operands);

typedef void be_emit_asm_operand_func(ir_node const *asmn, char modifier, unsigned pos);

void be_emit_asm(ir_node const *asmn, be_emit_asm_operand_func *emit_asm_operand);

#endif
