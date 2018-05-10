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

#include <assert.h>
#include <stdbool.h>

#include "array.h"
#include "be_types.h"
#include "bearch.h"
#include "betranshlp.h"
#include "firm_types.h"
#include "obstack.h"

typedef enum be_asm_operand_kind_t {
	BE_ASM_OPERAND_INVALID,
	BE_ASM_OPERAND_INPUT_VALUE,
	BE_ASM_OPERAND_OUTPUT_VALUE,
	BE_ASM_OPERAND_IMMEDIATE,
	BE_ASM_OPERAND_MEMORY,
} be_asm_operand_kind_t;

typedef struct be_asm_operand_t {
	be_asm_operand_kind_t kind;
	int                   pos;
} be_asm_operand_t;

static inline void be_set_asm_operand(be_asm_operand_t* const op, be_asm_operand_kind_t const kind, int const pos)
{
	assert((kind == BE_ASM_OPERAND_IMMEDIATE) == (pos == -1));
	op->kind = kind;
	op->pos  = pos;
}

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

arch_register_req_t const *be_make_register_req(struct obstack *obst, be_asm_constraint_t const *c);

typedef void parse_constraint_letter_func_t(void const *env, be_asm_constraint_t*, char l);

void be_parse_asm_constraints_internal(be_asm_constraint_t *constraint, ident *constraint_text, parse_constraint_letter_func_t *parse_constraint_letter, void const *env);

typedef struct be_asm_info_t {
	ir_node                   **ins;
	arch_register_req_t const **in_reqs;
	arch_register_req_t const **out_reqs;
} be_asm_info_t;

be_asm_info_t be_asm_prepare_info(ir_node const *node);

static inline void be_asm_add_in(be_asm_info_t *const info, be_asm_operand_t *const op, be_asm_operand_kind_t const kind, ir_node *const in, arch_register_req_t const *const req)
{
	assert(kind == BE_ASM_OPERAND_INPUT_VALUE || kind == BE_ASM_OPERAND_MEMORY);
	be_set_asm_operand(op, kind, ARR_LEN(info->ins));
	ARR_APP1(ir_node*,                   info->ins,     in);
	ARR_APP1(arch_register_req_t const*, info->in_reqs, req);
}

static inline arch_register_req_t const *be_asm_make_same_req(struct obstack *const obst, arch_register_req_t const *const req, unsigned const pos)
{
	arch_register_req_t *const oreq = OALLOCZ(obst, arch_register_req_t);
	*oreq                = *req;
	oreq->should_be_same = 1U << pos;
	return oreq;
}

static inline void be_asm_add_inout(be_asm_info_t *const info, be_asm_operand_t *const op, struct obstack *const obst, ir_node *const in, arch_register_req_t const *const ireq, int const opos)
{
	ir_node *const new_in = be_transform_node(in);
	be_asm_add_in(info, op, BE_ASM_OPERAND_INPUT_VALUE, new_in, ireq);
	if (opos >= 0) {
		arch_register_req_t const *const oreq = be_asm_make_same_req(obst, ireq, ARR_LEN(info->in_reqs) - 1);
		info->out_reqs[opos] = oreq;
	}
}

static inline void be_asm_add_out(be_asm_info_t *const info, be_asm_operand_t *const op, struct obstack *const obst, be_asm_constraint_t const *const be_constraint, int const opos)
{
	be_set_asm_operand(op, BE_ASM_OPERAND_OUTPUT_VALUE, opos);
	arch_register_req_t const *const oreq = be_make_register_req(obst, be_constraint);
	info->out_reqs[opos] = oreq;
}

ir_node *be_make_asm(ir_node const *node, be_asm_info_t const *info, void *operands);

typedef void be_emit_asm_operand_func(ir_node const *asmn, char modifier, unsigned pos);

void be_emit_asm(ir_node const *asmn, be_emit_asm_operand_func *emit_asm_operand);

bool be_is_valid_asm_operand_kind(ir_node const *node, char modifier, unsigned pos, be_asm_operand_kind_t have, char const *mod_any, char const *mod_imm, char const *mod_mem);

struct be_register_name_t {
	char const *name;
	unsigned    index;
};

arch_register_t const *be_parse_register_name(char const *clobber);

#endif
