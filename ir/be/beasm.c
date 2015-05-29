/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */

#include <ctype.h>

#include "bearch.h"
#include "beasm.h"
#include "bediagnostic.h"
#include "beemitter.h"
#include "benode.h"
#include "betranshlp.h"
#include "ident_t.h"
#include "panic.h"
#include "util.h"
#include "xmalloc.h"

arch_register_req_t const *be_make_register_req(struct obstack *obst, be_asm_constraint_t const *const c, int const n_outs, arch_register_req_t const **const out_reqs, int const pos)
{
	int const same_as = c->same_as;
	if (same_as >= 0) {
		if (same_as >= n_outs)
			panic("invalid output number in same_as constraint");

		arch_register_req_t       *const req   = OALLOC(obst, arch_register_req_t);
		arch_register_req_t const *const other = out_reqs[same_as];
		*req            = *other;
		req->type      |= arch_register_req_type_should_be_same;
		req->other_same = 1U << pos;

		/* Switch constraints. This is because in firm we have same_as
		 * constraints on the output constraints while in the gcc asm syntax
		 * they are specified on the input constraints. */
		out_reqs[same_as] = req;
		return other;
	}

	/* Pure memory ops. */
	if (!c->cls)
		return arch_no_register_req;

	if (c->all_registers_allowed)
		return c->cls->class_req;

	if (c->allowed_registers == 0)
		panic("constraint does not allow any registers");

	arch_register_req_t *const req     = (arch_register_req_t*)obstack_alloc(obst, sizeof(*req) + sizeof(unsigned));
	unsigned            *const limited = (unsigned*)(req + 1);
	*limited = c->allowed_registers;

	memset(req, 0, sizeof(*req));
	req->type    = arch_register_req_type_limited;
	req->cls     = c->cls;
	req->limited = limited;
	req->width   = 1;
	return req;
}

void be_parse_asm_constraints_internal(be_asm_constraint_t *const constraint, ident *const constraint_text, bool const is_output, parse_constraint_letter_func_t *const parse_constraint_letter, void const *const env)
{
	memset(constraint, 0, sizeof(*constraint));
	constraint->same_as = -1;

	char const *i = get_id_str(constraint_text);
	/* a memory constraint: no need to do anything in backend about it
	 * (dependencies are already respected by the memory edge of the node) */
	if (*i == '\0')
		return;

	/* TODO: improve error messages with node and source info. (As users can
	 * easily hit these) */
	char                         immediate_type        = '\0';
	unsigned                     limited               = 0;
	arch_register_class_t const *cls                   = NULL;
	bool                         memory_possible       = false;
	bool                         all_registers_allowed = false;
	int                          same_as               = -1;
	while (*i != '\0') {
		switch (*i) {
		/* Skip spaces, out/in-out marker. */
		case ' ':
		case '\t':
		case '\n':
		case '=':
		case '+':
		case '&':
		case '*':
			++i;
			break;

		case '#':
			do {
				++i;
			} while (*i != '\0' && *i != ',');
			break;

		default:
			if (is_digit(*i)) {
				if (is_output)
					panic("can only specify same constraint on input");

				int p;
				sscanf(i, "%d%n", &same_as, &p);
				if (same_as >= 0)
					i += p;
			} else {
				be_asm_constraint_t new_constraint;
				memset(&new_constraint, 0, sizeof(new_constraint));
				parse_constraint_letter(env, &new_constraint, *i++);

				limited               |= new_constraint.allowed_registers;
				all_registers_allowed |= new_constraint.all_registers_allowed;
				memory_possible       |= new_constraint.memory_possible;

				arch_register_class_t const *const new_cls = new_constraint.cls;
				if (new_cls) {
					if (cls && cls != new_cls)
						panic("multiple register classes not supported");
					cls = new_cls;
				}

				char const new_imm = new_constraint.immediate_type;
				if (new_imm != '\0') {
					if (immediate_type != '\0' && immediate_type != new_imm)
						panic("multiple immediate types not supported");
					immediate_type = new_imm;
				}
			}
			break;
		}
	}

	if (same_as >= 0) {
		if (cls)
			panic("same as and register constraint not supported");
		if (immediate_type != '\0')
			panic("same as and immediate constraint not supported");
	}

	if (!cls && same_as < 0 && !memory_possible)
		panic("no constraint specified for assembler input");

	constraint->same_as               = same_as;
	constraint->cls                   = cls;
	constraint->allowed_registers     = limited;
	constraint->all_registers_allowed = all_registers_allowed;
	constraint->memory_possible       = memory_possible;
	constraint->immediate_type        = immediate_type;
}

ir_node *be_make_asm(ir_node const *const node, unsigned const n_ins, ir_node **const in, arch_register_req_t const **const in_reqs, arch_register_req_t const **const out_reqs, void *const operands)
{
	dbg_info *const dbgi     = get_irn_dbg_info(node);
	ir_node  *const block    = be_transform_nodes_block(node);
	unsigned  const n_outs   = ARR_LEN(out_reqs);
	ident    *const text     = get_ASM_text(node);
	ir_node  *const new_node = be_new_Asm(dbgi, block, n_ins, in, n_outs, text, operands);

	backend_info_t *const info = be_get_info(new_node);
	for (size_t o = 0; o < n_outs; ++o) {
		info->out_infos[o].req = out_reqs[o];
	}
	arch_set_irn_register_reqs_in(new_node, in_reqs);

	DEL_ARR_F(out_reqs);

	return new_node;
}

void be_emit_asm(ir_node const *const asmn, be_emit_asm_operand_func *const emit_asm_operand)
{
	be_emit_cstring("#APP");
	be_emit_finish_line_gas(asmn);

	be_asm_attr_t const *const attr = get_be_asm_attr_const(asmn);

	char const *s = get_id_str(attr->text);
	if (s[0] != '\t')
		be_emit_char('\t');

	char     const *last       = s;
	unsigned const  n_operands = ARR_LEN(attr->operands);
	while ((s = strchr(s, '%'))) {
		be_emit_string_len(last, s - last);
		++s; /* Skip '%'. */
		switch (*s) {
		case '%':
		case '{':
		case '|':
		case '}':
			be_emit_char(*s++);
			break;

		default: {
			char const modifier = isalpha((unsigned char)*s) ? *s++ : '\0';
			unsigned   pos;
			int        p;
			if (sscanf(s, "%u%n", &pos, &p) == 1) {
				s += p;
				if (pos < n_operands) {
					emit_asm_operand(asmn, modifier, pos);
				} else {
					be_errorf(asmn, "asm operand number '%u' out of range", pos);
				}
			} else {
				be_errorf(asmn, "could not parse asm operand number");
			}
			break;
		}
		}

		last = s;
	}
	be_emit_string(last);

	be_emit_cstring("\n#NO_APP\n");
	be_emit_write_line();
}
