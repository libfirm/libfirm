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

#include <assert.h>
#include <inttypes.h>

#include "array.h"
#include "bearch.h"
#include "beasm.h"
#include "beemitter.h"
#include "bediagnostic.h"
#include "begnuas.h"
#include "beirg.h"
#include "benode.h"
#include "betranshlp.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_new_nodes.h"
#include "irprintf.h"
#include "panic.h"
#include "util.h"

arch_register_t const *x86_parse_clobber(x86_clobber_name_t const *const additional_clobber_names, char const *const clobber)
{
	arch_register_t const *reg = arch_find_register(clobber);
	if (reg != NULL)
		return reg;
	arch_register_t const *const regs = isa_if->registers;
	for (size_t i = 0; additional_clobber_names[i].name != NULL; ++i) {
		if (strcmp(additional_clobber_names[i].name, clobber) == 0)
			return &regs[additional_clobber_names[i].index];
	}
	return NULL;
}

static void x86_parse_constraint_letter(void const *const env, be_asm_constraint_t* const c, char const l)
{
	x86_asm_constraint_list_t const *const constraints = (x86_asm_constraint_list_t const*)env;

	unsigned char const u = (unsigned char)l;
	if (u >= ARRAY_SIZE(*constraints))
		panic("Unknown asm constraint '%c'", l);

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
	panic("Unknown asm constraint '%c'", l);
}

static void parse_asm_constraints(be_asm_constraint_t *const constraint, x86_asm_constraint_list_t const *const constraints, ident *const constraint_text, bool const is_output)
{
	be_parse_asm_constraints_internal(constraint, constraint_text, is_output, &x86_parse_constraint_letter, constraints);
}

static void set_operand_if_invalid(x86_asm_operand_t *const op, x86_asm_operand_kind_t const kind, unsigned const pos, ir_asm_constraint const *const constraint)
{
	/* Multiple constraints for same pos. This can happen for example when
	 * a =A constraint gets lowered to two constraints: =a and =d for the
	 * same pos. */
	if (op->kind == ASM_OP_INVALID) {
		op->kind      = kind;
		op->inout_pos = pos;
		op->u.mode    = constraint->mode;
		assert(op->inout_pos == pos); // Make sure we had no overflow.
	}
}

ir_node *x86_match_ASM(ir_node const *const node, x86_clobber_name_t const *const additional_clobber_names, x86_asm_constraint_list_t const *const constraints)
{
	unsigned           const n_operands = be_count_asm_operands(node);
	ir_graph          *const irg        = get_irn_irg(node);
	struct obstack    *const obst       = get_irg_obstack(irg);
	x86_asm_operand_t *const operands   = NEW_ARR_DZ(x86_asm_operand_t, obst, n_operands);

	int                      const n_inputs          = get_ASM_n_inputs(node);
	size_t                   const n_out_constraints = get_ASM_n_output_constraints(node);
	ir_asm_constraint const *const in_constraints    = get_ASM_input_constraints(node);
	ir_asm_constraint const *const out_constraints   = get_ASM_output_constraints(node);

	/* construct output constraints */
	size_t              const   n_clobbers = get_ASM_n_clobbers(node);
	arch_register_req_t const **out_reqs   = NEW_ARR_F(arch_register_req_t const*, 0);

	for (unsigned o = 0; o < n_out_constraints; ++o) {
		ir_asm_constraint const *const constraint = &out_constraints[o];

		be_asm_constraint_t parsed_constraint;
		parse_asm_constraints(&parsed_constraint, constraints,
		                      constraint->constraint, true);

		arch_register_req_t const *const req = be_make_register_req(obst, &parsed_constraint, n_out_constraints, out_reqs, o);
		ARR_APP1(arch_register_req_t const*, out_reqs, req);

		x86_asm_operand_t *const op = &operands[constraint->pos];
		set_operand_if_invalid(op, ASM_OP_OUT_REG, o, constraint);
	}

	/* parse clobbers */
	unsigned clobber_bits[isa_if->n_register_classes];
	memset(&clobber_bits, 0, sizeof(clobber_bits));
	ident **const clobbers = get_ASM_clobbers(node);
	for (size_t c = 0; c < n_clobbers; ++c) {
		char            const *const clobber = get_id_str(clobbers[c]);
		arch_register_t const *const reg     = x86_parse_clobber(additional_clobber_names, clobber);
		if (reg != NULL) {
			assert(reg->cls->n_regs <= sizeof(unsigned) * 8);
			/* x87 registers may still be used as input, even if clobbered. */
			if (reg->cls != &ia32_reg_classes[CLASS_ia32_fp])
				clobber_bits[reg->cls->index] |= 1U << reg->index;
			ARR_APP1(arch_register_req_t const*, out_reqs, reg->single_req);
		}
	}

	/* inputs + input constraints */
	ir_node                   **in      = NEW_ARR_F(ir_node*, 0);
	arch_register_req_t const **in_reqs = NEW_ARR_F(arch_register_req_t const*, 0);
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
		    op->kind = ASM_OP_IMMEDIATE;
			continue;
		}

		arch_register_class_t const *const cls = parsed_constraint.cls;
		if (cls != NULL) {
			unsigned const r_clobber_bits = clobber_bits[cls->index];
			if (r_clobber_bits != 0) {
				if (parsed_constraint.all_registers_allowed) {
					parsed_constraint.all_registers_allowed = false;
					be_get_allocatable_regs(irg, cls, &parsed_constraint.allowed_registers);
				}
				parsed_constraint.allowed_registers &= ~r_clobber_bits;
			}
		}

		ir_node            *const  new_pred = be_transform_node(pred);
		unsigned            const  in_pos   = ARR_LEN(in_reqs);
		arch_register_req_t const *req      = be_make_register_req(obst, &parsed_constraint, n_out_constraints, out_reqs, in_pos);

		set_operand_if_invalid(op, ASM_OP_IN_REG, in_pos, constraint);

		if (cls == NULL && parsed_constraint.same_as < 0) {
			op->kind = ASM_OP_MEMORY;
			req = arch_get_irn_register_req(new_pred)->cls->class_req;
		} else if (parsed_constraint.memory_possible) {
			/* TODO: match Load or Load/Store if memory possible is set */
		}

		ARR_APP1(arch_register_req_t const*, in_reqs, req);
		ARR_APP1(ir_node*, in, new_pred);
	}

	return be_make_asm(node, in, in_reqs, out_reqs, operands);
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
	/* we also support any number for two-address code constraints */
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER,
	                          "0123456789");
}
