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
#include "x86_asm.h"

#include "array.h"
#include "be_t.h"
#include "bearch.h"
#include "beasm.h"
#include "betranshlp.h"
#include "gen_ia32_regalloc_if.h"
#include "panic.h"
#include "target_t.h"
#include <assert.h>

static void x86_parse_constraint_letter(void const *const env, be_asm_constraint_t* const c, char const l)
{
	x86_asm_constraint_list_t const *const constraints = (x86_asm_constraint_list_t const*)env;

	unsigned char const u = (unsigned char)l;
	if (u < ARRAY_SIZE(*constraints)) {
		x86_asm_constraint_t const *const constraint = &(*constraints)[u];
		switch (constraint->kind) {
		case MATCH_REG:
			c->cls = constraint->cls;
			if (constraint->limited == 0)
				c->all_registers_allowed = true;
			else
				c->allowed_registers = constraint->limited;
			return;

		case MATCH_MEM:
			/* memory constraint no need to do anything in backend about it
			 * (dependencies are already respected by the memory edge of the
			 *  node) */
			c->memory_possible = true;
			return;

		case MATCH_IMM:
			c->cls            = constraint->cls;
			c->immediate_type = l;
			return;

		case MATCH_ANY:
			c->cls                   = constraint->cls;
			c->immediate_type        = l;
			c->memory_possible       = true;
			c->all_registers_allowed = true;
			return;

		case MATCH_INVALID:
			break;
		}
	}
	panic("Unknown asm constraint '%c'", l);
}

static void parse_asm_constraints(be_asm_constraint_t *const constraint, x86_asm_constraint_list_t const *const constraints, ident *const constraint_text, bool const is_output)
{
	be_parse_asm_constraints_internal(constraint, constraint_text, is_output, &x86_parse_constraint_letter, constraints);
}

static void set_operand_if_invalid(x86_asm_operand_t *const op, be_asm_operand_kind_t const kind, unsigned const pos, ir_asm_constraint const *const constraint)
{
	/* Multiple constraints for same pos. This can happen for example when
	 * a =A constraint gets lowered to two constraints: =a and =d for the
	 * same pos. */
	if (op->op.kind == BE_ASM_OPERAND_INVALID) {
		be_set_asm_operand(&op->op, kind, pos);
		op->u.mode = constraint->mode;
		assert((unsigned)op->op.pos == pos); // Make sure we had no overflow.
	}
}

ir_node *x86_match_ASM(ir_node const *const node, x86_asm_constraint_list_t const *const constraints)
{
	be_asm_info_t info = be_asm_prepare_info();

	unsigned           const n_operands = be_count_asm_operands(node);
	ir_graph          *const irg        = get_irn_irg(node);
	struct obstack    *const obst       = get_irg_obstack(irg);
	x86_asm_operand_t *const operands   = NEW_ARR_DZ(x86_asm_operand_t, obst, n_operands);

	int                      const n_inputs          = get_ASM_n_inputs(node);
	size_t                   const n_out_constraints = get_ASM_n_output_constraints(node);
	ir_asm_constraint const *const in_constraints    = get_ASM_input_constraints(node);
	ir_asm_constraint const *const out_constraints   = get_ASM_output_constraints(node);

	/* construct output constraints */
	for (unsigned o = 0; o < n_out_constraints; ++o) {
		ir_asm_constraint const *const constraint = &out_constraints[o];

		be_asm_constraint_t parsed_constraint;
		parse_asm_constraints(&parsed_constraint, constraints,
		                      constraint->constraint, true);

		arch_register_req_t const *const req = be_make_register_req(obst, &parsed_constraint, n_out_constraints, info.out_reqs, o);
		ARR_APP1(arch_register_req_t const*, info.out_reqs, req);

		x86_asm_operand_t *const op = &operands[constraint->pos];
		set_operand_if_invalid(op, BE_ASM_OPERAND_OUTPUT_VALUE, o, constraint);
	}

	/* inputs + input constraints */
	for (int i = 0; i < n_inputs; ++i) {
		ir_asm_constraint const *const constraint = &in_constraints[i];

		be_asm_constraint_t parsed_constraint;
		parse_asm_constraints(&parsed_constraint, constraints,
		                      constraint->constraint, false);

		/* try to match an immediate operand */
		x86_asm_operand_t *const op       = &operands[constraint->pos];
		ir_node           *const pred     = get_ASM_input(node, i);
		char               const imm_type = parsed_constraint.immediate_type;
		if (imm_type != '\0'
		    && x86_match_immediate(&op->u.imm32, pred, imm_type)) {
			be_set_asm_operand(&op->op, BE_ASM_OPERAND_IMMEDIATE, -1);
			continue;
		}

		ir_node            *const  new_pred = be_transform_node(pred);
		unsigned            const  in_pos   = ARR_LEN(info.in_reqs);
		arch_register_req_t const *req      = be_make_register_req(obst, &parsed_constraint, n_out_constraints, info.out_reqs, in_pos);

		set_operand_if_invalid(op, BE_ASM_OPERAND_INPUT_VALUE, in_pos, constraint);

		if (!parsed_constraint.cls && parsed_constraint.same_as < 0) {
			op->op.kind = BE_ASM_OPERAND_MEMORY;
			req = arch_get_irn_register_req(new_pred)->cls->class_req;
		} else if (parsed_constraint.memory_possible) {
			/* TODO: match Load or Load/Store if memory possible is set */
		}

		ARR_APP1(arch_register_req_t const*, info.in_reqs, req);
		ARR_APP1(ir_node*, info.ins, new_pred);
	}

	return be_make_asm(node, &info, operands);
}

void x86_set_be_asm_constraint_support(const x86_asm_constraint_list_t *constraints)
{
	for (unsigned char c = 0; c < ARRAY_SIZE(*constraints); ++c) {
		const x86_asm_constraint_t *constraint = &(*constraints)[c];
		asm_constraint_flags_t flags;
		switch (constraint->kind) {
		case MATCH_INVALID:
			continue;
		case MATCH_REG:
			flags = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
			goto fine;
		case MATCH_IMM:
			flags = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
			goto fine;
		case MATCH_MEM:
			flags = ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP;
			goto fine;
		case MATCH_ANY:
			flags = ASM_CONSTRAINT_FLAG_SUPPORTS_ANY;
			goto fine;
		}
		panic("invalid constraint");
fine:
		assert(be_asm_constraint_flags[c] == ASM_CONSTRAINT_FLAG_INVALID
		    || be_asm_constraint_flags[c] == ASM_CONSTRAINT_FLAG_NO_SUPPORT);
		be_asm_constraint_flags[c] = flags;
	}
}
