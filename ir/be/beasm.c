/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */

#include <ctype.h>

#include "bearch.h"
#include "beasm.h"
#include "bediagnostic.h"
#include "beemitter.h"
#include "ident_t.h"
#include "panic.h"
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

void be_emit_asm(ir_node const *const asmn, ident *const text, unsigned const n_operands, be_emit_asm_operand_func *const emit_asm_operand)
{
	be_emit_cstring("#APP");
	be_emit_finish_line_gas(asmn);

	char const *s = get_id_str(text);
	if (s[0] != '\t')
		be_emit_char('\t');

	char const *last = s;
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
