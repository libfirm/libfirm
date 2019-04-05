/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */
#include "beasm.h"

#include "be_t.h"
#include "bearch.h"
#include "bediagnostic.h"
#include "beemitter.h"
#include "begnuas.h"
#include "beirg.h"
#include "benode.h"
#include "betranshlp.h"
#include "ident_t.h"
#include "panic.h"
#include "target_t.h"
#include "util.h"
#include "xmalloc.h"
#include <ctype.h>

arch_register_req_t const *be_make_register_req(struct obstack *obst, be_asm_constraint_t const *const c)
{
	assert(c->same_as < 0);
	assert(c->cls);

	if (c->all_registers_allowed)
		return c->cls->class_req;

	if (c->allowed_registers == 0)
		panic("constraint does not allow any registers");

	arch_register_req_t *const req     = (arch_register_req_t*)obstack_alloc(obst, sizeof(*req) + sizeof(unsigned));
	unsigned            *const limited = (unsigned*)(req + 1);
	*limited = c->allowed_registers;

	memset(req, 0, sizeof(*req));
	req->cls     = c->cls;
	req->limited = limited;
	req->width   = 1;
	return req;
}

void be_parse_asm_constraints_internal(be_asm_constraint_t *const constraint, ident *const constraint_text, parse_constraint_letter_func_t *const parse_constraint_letter, void const *const env)
{
	memset(constraint, 0, sizeof(*constraint));
	constraint->same_as = -1;

	char const *i = get_id_str(constraint_text);

	if (!i) {
		/* Labels have no constraint text. */
		constraint->all_registers_allowed = true;
		constraint->cls                   = &arch_exec_cls;
		return;
	}

	bool is_output = false;
	if (i[0] == '+' || i[0] == '=') {
		++i;
		is_output = true;
	}

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
		case '%':
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
				int p;
				if (sscanf(i, "%d%n", &same_as, &p) == 0) {
					be_errorf(NULL, "error while reading matching constraint in \"%s\"", constraint_text);
					i += 1;
				} else {
					i += p;
				}
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
						be_warningf(NULL, "multiple register classes not supported in \"%s\"", constraint_text);
					cls = new_cls;
				}

				char const new_imm = new_constraint.immediate_type;
				if (new_imm != '\0') {
					if (immediate_type != '\0' && immediate_type != new_imm)
						be_warningf(NULL, "multiple immediate types not supported in \"%s\"", constraint_text);
					immediate_type = new_imm;
				}
			}
			break;
		}
	}

	if (same_as >= 0) {
		if (is_output) {
			be_warningf(NULL, "output constraint \"%s\" has matching constraint; ignoring it", constraint_text);
			same_as = -1;
		} else {
			if (cls) {
				be_warningf(NULL, "matching and register constraint not supported in constraint \"%s\"; ignoring register constraint", constraint_text);
				cls = NULL;
			}
			if (immediate_type != '\0') {
				be_warningf(NULL, "matching and immediate constraint not supported in constraint \"%s\"; ignoring immediate constraint", constraint_text);
				immediate_type = '\0';
			}
		}
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

/* Determine number of output operands. */
static unsigned be_count_asm_outputs(ir_node const *const node)
{
	unsigned                       n_outputs   = pn_be_Asm_first_out; /* At least the default outputs. */
	ir_asm_constraint const *const constraints = get_ASM_constraints(node);
	for (unsigned i = 0, n = get_ASM_n_constraints(node); i != n; ++i) {
		n_outputs = MAX(n_outputs, (unsigned)(constraints[i].out_pos + 1));
	}
	return n_outputs;
}

be_asm_info_t be_asm_prepare_info(ir_node const *const node)
{
	unsigned                    const n_outs   = be_count_asm_outputs(node);
	arch_register_req_t const **const out_reqs = NEW_ARR_F(arch_register_req_t const*, n_outs);
	out_reqs[pn_be_Asm_M]         = arch_memory_req;
	out_reqs[pn_be_Asm_X_regular] = arch_exec_req;

	ir_node                   **const ins     = NEW_ARR_F(ir_node*, n_be_Asm_first_in);
	arch_register_req_t const **const in_reqs = NEW_ARR_F(arch_register_req_t const*, n_be_Asm_first_in);
	ins[n_be_Asm_mem]     = be_transform_node(get_ASM_mem(node));
	in_reqs[n_be_Asm_mem] = arch_memory_req;

	return (be_asm_info_t){
		.ins      = ins,
		.in_reqs  = in_reqs,
		.out_reqs = out_reqs,
	};
}

static bool can_match(arch_register_req_t const *const in, arch_register_req_t const *const out)
{
	if (in->cls != out->cls)
		return false;
	if (in->limited == NULL || out->limited == NULL)
		return true;
	return (*in->limited & *out->limited) != 0;
}

static bool match_requirement(arch_register_req_t const **reqs, size_t const n_reqs, bitset_t *const used, arch_register_req_t const *const req)
{
	for (size_t i = 0; i != n_reqs; ++i) {
		if (bitset_is_set(used, i))
			continue;
		if (!can_match(req, reqs[i]))
			continue;
		bitset_set(used, i);
		return true;
	}
	return false;
}

ir_node *be_make_asm(ir_node const *const node, be_asm_info_t const *const info, void *const operands)
{
	ir_node                   **in       = info->ins;
	arch_register_req_t const **in_reqs  = info->in_reqs;
	arch_register_req_t const **out_reqs = info->out_reqs;

	assert(ARR_LEN(in) == ARR_LEN(in_reqs));

	ir_graph       *const irg  = get_irn_irg(node);
	struct obstack *const obst = get_irg_obstack(irg);

	/* Handle early clobbers and labels. */
	size_t                         n_labels    = 0;
	size_t                   const orig_n_ins  = ARR_LEN(in_reqs);
	size_t                   const orig_n_outs = ARR_LEN(out_reqs);
	ir_asm_constraint const *const constraints = get_ASM_constraints(node);
	for (unsigned o = 0, n = get_ASM_n_constraints(node); o != n; ++o) {
		ir_asm_constraint const *const constraint = &constraints[o];
		if (!constraint->constraint) {
			++n_labels;
		} else if (strchr(get_id_str(constraint->constraint), '&')) {
			int const out_pos = constraint->out_pos;
			if (out_pos >= 0) {
				arch_register_req_t   const **const oslot = &out_reqs[out_pos];
				arch_register_req_t   const  *const oreq  = *oslot;
				arch_register_class_t const  *const cls   = oreq->cls;

				unsigned different = 0;

				/* Add each input in the same register class. */
				for (unsigned i = 0; i != orig_n_ins; ++i) {
					if (in_reqs[i]->cls == cls)
						different |= 1U << i;
				}

				/* Remove each input which has a matching output.
				 * The output already ensures that the register is different than the
				 * early clobber output. */
				for (unsigned i = 0; i != orig_n_outs; ++i) {
					arch_register_req_t const *const other_oreq = out_reqs[i];
					if (other_oreq->cls == cls) {
						unsigned const same_as = other_oreq->should_be_same;
						assert(is_po2_or_zero(same_as));
						different &= ~same_as;
					}
				}

				if (different != 0) {
					arch_register_req_t *const req = OALLOCZ(obst, arch_register_req_t);
					*req                   = *oreq;
					req->must_be_different = different;
					*oslot                 = req;
				}
			}
		}
	}

	size_t const n_clobbers = get_ASM_n_clobbers(node);
	if (n_clobbers != 0) {
		/* Collect clobbers and add them as outputs. */
		unsigned clobber_bits[ir_target.isa->n_register_classes];
		memset(&clobber_bits, 0, sizeof(clobber_bits));

		be_irg_t       *const birg             = be_birg_from_irg(irg);
		unsigned const *const allocatable_regs = birg->allocatable_regs;
		ident         **const clobbers         = get_ASM_clobbers(node);
		for (size_t i = 0; i < n_clobbers; ++i) {
			char            const *const clobber = get_id_str(clobbers[i]);
			arch_register_t const *const reg     = be_parse_register_name(clobber);
			if (reg && rbitset_is_set(allocatable_regs, reg->global_index)) {
				assert(reg->cls->n_regs <= sizeof(unsigned) * 8);
				if (!reg->cls->allow_clobber_input)
					clobber_bits[reg->cls->index] |= 1U << reg->index;
				ARR_APP1(arch_register_req_t const*, out_reqs, reg->single_req);
			}
		}

		/* Restrict inputs by clobbers. */
		for (size_t i = n_be_Asm_first_in, n = ARR_LEN(in_reqs); i != n; ++i) {
			arch_register_req_t   const *const req = in_reqs[i];
			arch_register_class_t const *const cls = req->cls;
			assert(cls->index < ARRAY_SIZE(clobber_bits));
			unsigned const clobber = clobber_bits[cls->index];
			if (clobber != 0) {
				arch_register_req_t *const new_req = (arch_register_req_t*)obstack_alloc(obst, sizeof(*new_req) + sizeof(unsigned));
				unsigned            *const limited = (unsigned*)(new_req + 1);
				if (req->limited) {
					*limited = *req->limited;
				} else {
					be_get_allocatable_regs(irg, cls, limited);
				}
				*limited        &= ~clobber;
				*new_req         = *req;
				new_req->limited = limited;
				in_reqs[i] = new_req;
			}
		}
	}

	unsigned const n_ins  = ARR_LEN(in);
	unsigned const n_outs = ARR_LEN(out_reqs); // Clobbers might add further outputs.

	/* Attempt to make ASM node register pressure faithful.
	 * (This does not work for complicated cases yet!)
	 *
	 * Algorithm: Check if there are fewer inputs or outputs (I will call this
	 * the smaller list). Then try to match each constraint of the smaller list
	 * to 1 of the other list. If we can't match it, then we add additional
	 * register pressure.
	 *
	 * FIXME: This is still broken in lots of cases. But at least better than
	 *        before...
	 * FIXME: need to do this per register class...
	 */
	be_add_pressure_t add_pressure[ir_target.isa->n_register_classes];
	memset(add_pressure, 0, sizeof(add_pressure));
	if (n_outs - n_labels - pn_be_Asm_first_out < n_ins - n_be_Asm_first_in) {
		bitset_t *const used_ins = bitset_alloca(n_ins);
		for (size_t o = pn_be_Asm_first_out; o < n_outs; ++o) {
			arch_register_req_t const *const outreq = out_reqs[o];
			if (!outreq->cls->manual_ra) {
				assert(outreq->cls->index < ARRAY_SIZE(add_pressure));
				if (!match_requirement(in_reqs, n_ins, used_ins, outreq))
					add_pressure[outreq->cls->index]++;
			}
		}
	} else {
		bitset_t *const used_outs = bitset_alloca(n_outs);
		for (unsigned i = n_be_Asm_first_in; i < n_ins; ++i) {
			arch_register_req_t const *const inreq = in_reqs[i];
			if (!inreq->cls->manual_ra) {
				assert(inreq->cls->index < ARRAY_SIZE(add_pressure));
				if (!match_requirement(out_reqs, n_outs, used_outs, inreq))
					add_pressure[inreq->cls->index]--;
			}
		}
	}

	dbg_info                   *const dbgi        = get_irn_dbg_info(node);
	ir_node                    *const block       = be_transform_nodes_block(node);
	ident                      *const text        = get_ASM_text(node);
	arch_register_req_t const **const dup_in_reqs = DUP_ARR_D(arch_register_req_t const*, obst, in_reqs);
	ir_node                    *const new_node    = be_new_Asm(dbgi, block, n_ins, in, dup_in_reqs, n_outs, text, operands);

	if (n_labels != 0) {
		ir_set_throws_exception(new_node, true);
		set_irn_pinned(new_node, op_pin_state_pinned);
	}

	for (unsigned i = 0; i != ARRAY_SIZE(add_pressure); ++i) {
		if (add_pressure[i] != 0) {
			arch_register_class_t const *const cls = &ir_target.isa->register_classes[i];
			arch_set_additional_pressure(new_node, cls, add_pressure[i]);
		}
	}

	backend_info_t *const be_info = be_get_info(new_node);
	for (size_t o = 0; o < n_outs; ++o) {
		be_info->out_infos[o].req = out_reqs[o];
	}

	DEL_ARR_F(in);
	DEL_ARR_F(in_reqs);
	DEL_ARR_F(out_reqs);

	return new_node;
}

ir_node *be_emit_asm(ir_node const *const asmn, be_emit_asm_operand_func *const emit_asm_operand)
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

		case '=':
			/* From gcc source:
			 * %= outputs a number which is unique to each insn in the entire
			 * compilation.  This is useful for making local labels that are
			 * referred to more than once in a given insn. */
			++s; /* Skip '='. */
			be_emit_irprintf("%ld", get_irn_node_nr(asmn));
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

	/* Return the fallthrough proj if present. */
	return get_Proj_for_pn(asmn, pn_be_Asm_X_regular);
}

static char const *be_get_constraint_name(be_asm_operand_kind_t const kind)
{
	switch (kind) {
	case BE_ASM_OPERAND_INVALID:      return "invalid";
	case BE_ASM_OPERAND_INPUT_VALUE:  return "input register";
	case BE_ASM_OPERAND_OUTPUT_VALUE: return "output register";
	case BE_ASM_OPERAND_IMMEDIATE:    return "immediate";
	case BE_ASM_OPERAND_MEMORY:       return "memory";
	case BE_ASM_OPERAND_LABEL:        return "label";
	}
	panic("invalid constraint kind");
}

bool be_is_valid_asm_operand_kind(ir_node const *const node, char const modifier, unsigned const pos, be_asm_operand_kind_t const have, char const *const mod_any, char const *const mod_imm, char const *const mod_mem)
{
	be_asm_operand_kind_t want;
	if (strchr(mod_any, modifier)) {
		return true;
	} else if (strchr(mod_imm, modifier)) {
		want = BE_ASM_OPERAND_IMMEDIATE;
	} else if (strchr(mod_mem, modifier)) {
		want = BE_ASM_OPERAND_MEMORY;
	} else if (modifier == 'l') {
		want = BE_ASM_OPERAND_LABEL;
	} else {
		be_errorf(node, "asm contains unknown modifier '%c'", modifier);
		return false;
	}
	if (want != have) {
		char const *const name_want = be_get_constraint_name(want);
		char const *const name_have = be_get_constraint_name(have);
		be_errorf(node, "modifier of operand '%%%c%u' requires an operand of type '%s', but got '%s'", modifier, pos, name_want, name_have);
		return false;
	}
	return true;
}

arch_register_t const *be_parse_register_name(char const *clobber)
{
	arch_isa_if_t const *const isa = ir_target.isa;

	/* GCC always accepts '#' and '%' as register name prefix.
	 * See strip_reg_name() in varasm.c. */
	char const c = clobber[0];
	if (c == '#' || c == '%' || (isa->register_prefix != '\0' && c == isa->register_prefix))
		clobber += 1;

	arch_register_t const *const reg = arch_find_register(clobber);
	if (reg)
		return reg;

	be_register_name_t const *const add = isa->additional_reg_names;
	if (add) {
		for (be_register_name_t const *i = add; i->name; ++i) {
			if (streq(i->name, clobber))
				return &isa->registers[i->index];
		}
	}

	return NULL;
}
