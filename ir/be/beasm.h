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
	int                          same_as;
} be_asm_constraint_t;

arch_register_req_t const *be_make_register_req(struct obstack *obst, be_asm_constraint_t const *c, int n_outs, arch_register_req_t const **out_reqs, int pos);

typedef void be_emit_asm_operand_func(ir_node const *asmn, char modifier, unsigned pos);

void be_emit_asm(ir_node const *asmn, ident *text, unsigned n_operands, be_emit_asm_operand_func *emit_asm_operand);

#endif
