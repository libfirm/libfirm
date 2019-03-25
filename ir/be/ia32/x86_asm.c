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

static size_t x86_add_addr_in(be_asm_info_t *const info, ir_node *const in)
{
	ir_node                   *const new_in = be_transform_node(in);
	arch_register_req_t const *const req    = arch_get_irn_register_req(new_in)->cls->class_req;
	return be_asm_append_in(info, new_in, req);
}

ir_node *x86_match_ASM(ir_node const *const node, x86_asm_constraint_list_t const *const constraint_list)
{
	be_asm_info_t info = be_asm_prepare_info(node);

	ir_asm_constraint const *const constraints   = get_ASM_constraints(node);
	size_t                   const n_constraints = get_ASM_n_constraints(node);
	ir_graph                *const irg           = get_irn_irg(node);
	struct obstack          *const obst          = get_irg_obstack(irg);
	x86_asm_operand_t       *const operands      = NEW_ARR_DZ(x86_asm_operand_t, obst, n_constraints);
	for (size_t i = 0; i != n_constraints; ++i) {
		ir_asm_constraint const *const c = &constraints[i];

		be_asm_constraint_t be_constraint;
		be_parse_asm_constraints_internal(&be_constraint, c->constraint, &x86_parse_constraint_letter, constraint_list);

		x86_asm_operand_t *const op = &operands[i];
		op->u.mode = c->mode;

		int const in_pos = c->in_pos;
		if (in_pos >= 0) {
			ir_node *const in  = get_ASM_input(node, in_pos);
			char     const imm = be_constraint.immediate_type;
			if (imm != '\0' && x86_match_immediate(&op->u.imm32, in, imm)) {
				be_asm_add_immediate(&op->op);
			} else if (be_constraint.same_as >= 0) {
				int                        const out_pos = operands[be_constraint.same_as].op.pos;
				arch_register_req_t const *const ireq    = info.out_reqs[out_pos];
				be_asm_add_inout(&info, &op->op, obst, in, ireq, out_pos);
			} else if (be_constraint.cls) {
				arch_register_req_t const *const ireq = be_make_register_req(obst, &be_constraint);
				be_asm_add_inout(&info, &op->op, obst, in, ireq, c->out_pos);
			} else {
				x86_address_t address;
				x86_create_address_mode(&address, in, x86_create_am_normal);

				x86_addr_t *const addr = &op->u.addr;
				*addr = (x86_addr_t){
					.immediate = address.imm,
					.log_scale = address.scale,
					.variant   = address.variant,
				};

				if (x86_addr_variant_has_base(address.variant))
					addr->base_input = x86_add_addr_in(&info, address.base);

				if (x86_addr_variant_has_index(address.variant))
					addr->index_input = x86_add_addr_in(&info, address.index);

				be_set_asm_operand(&op->op, BE_ASM_OPERAND_MEMORY, -1);
			}
		} else {
			be_asm_add_out(&info, &op->op, obst, &be_constraint, c->out_pos);
		}
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
