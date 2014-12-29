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
#include "beemitter.h"
#include "begnuas.h"
#include "beirg.h"
#include "benode.h"
#include "betranshlp.h"
#include "beutil.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_new_nodes.h"
#include "irprintf.h"
#include "panic.h"
#include "util.h"

/**
 * An assembler constraint.
 */
typedef struct parsed_constraint_t {
	const arch_register_class_t *cls;
	unsigned                     allowed_registers;
	bool                         all_registers_allowed;
	bool                         memory_possible;
	char                         immediate_type;
	int                          same_as;
} parsed_constraint_t;

static arch_register_req_t const *x86_make_register_req(struct obstack *obst,
		parsed_constraint_t const *const c, int const n_outs,
		arch_register_req_t const **const out_reqs, int const pos)
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
	if (c->cls == NULL)
		return arch_no_register_req;

	if (c->allowed_registers == 0 || c->all_registers_allowed)
		return c->cls->class_req;

	arch_register_req_t *const req     = (arch_register_req_t*)obstack_alloc(obst, sizeof(req[0]) + sizeof(unsigned));
	unsigned            *const limited = (unsigned*)(req + 1);
	*limited = c->allowed_registers;

	memset(req, 0, sizeof(req[0]));
	req->type    = arch_register_req_type_limited;
	req->cls     = c->cls;
	req->limited = limited;
	req->width   = 1;
	return req;
}

arch_register_t const *x86_parse_clobber(const arch_env_t *arch_env,
	const x86_clobber_name_t *additional_clobber_names,
	const char *const clobber)
{
	arch_register_t const *reg = arch_find_register(arch_env, clobber);
	if (reg != NULL)
		return reg;
	for (size_t i = 0; additional_clobber_names[i].name != NULL; ++i) {
		if (strcmp(additional_clobber_names[i].name, clobber) == 0)
			return &arch_env->registers[additional_clobber_names[i].index];
	}
	return NULL;
}

static void parse_asm_constraints(parsed_constraint_t *const constraint,
                                  const x86_asm_constraint_list_t *constraints,
                                  ident *const constraint_text,
                                  bool const is_output)
{
	memset(constraint, 0, sizeof(constraint[0]));
	constraint->same_as = -1;

	unsigned char const *c = (const unsigned char*)get_id_str(constraint_text);
	/* a memory constraint: no need to do anything in backend about it
	 * (dependencies are already respected by the memory edge of the node) */
	if (*c == 0)
		return;

	/* TODO: improve error messages with node and source info. (As users can
	 * easily hit these) */
	char                         immediate_type        = '\0';
	unsigned                     limited               = 0;
	arch_register_class_t const *cls                   = NULL;
	bool                         memory_possible       = false;
	bool                         all_registers_allowed = false;
	int                          same_as               = -1;
	for ( ; *c != 0; ++c) {
		arch_register_class_t const *new_cls = NULL;
		char                         new_imm = '\0';
		switch (*c) {
		/* Skip spaces, out/in-out marker. */
		case ' ':
		case '\t':
		case '\n':
		case '=':
		case '+':
		case '&':
		case '*':
			break;

		case '#':
			while (*c != '\0' && *c != ',')
				++c;
			break;

		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9': {
			if (is_output)
				panic("can only specify same constraint on input");

			int p;
			sscanf((const char*)c, "%d%n", &same_as, &p);
			if (same_as >= 0) {
				c += p-1; /* loop will do +1 */
				continue;
			}
			break;
		}

		default:
			if (*c >= ARRAY_SIZE(*constraints))
				panic("Unknown asm constraint '%c'", *c);
			const x86_asm_constraint_t *constraint = &(*constraints)[*c];
			switch (constraint->kind) {
			case MATCH_REG:
				new_cls = constraint->cls;
				if (constraint->limited == 0)
					all_registers_allowed = true;
				else
					limited |= constraint->limited;
				goto fine;
			case MATCH_MEM:
				/* memory constraint no need to do anything in backend about it
				 * (dependencies are already respected by the memory edge of the
				 *  node) */
				memory_possible = true;
				goto fine;
			case MATCH_IMM:
				new_imm = *c;
				goto fine;
			case MATCH_ANY:
				new_imm = *c;
				new_cls = constraint->cls;
				memory_possible = true;
				goto fine;
			case MATCH_INVALID:
				break;
			}
			panic("Unknown asm constraint '%s'", *c);
fine:
			break;
		}

		if (new_cls != NULL) {
			if (cls != NULL && cls != new_cls)
				panic("multiple register classes not supported");
			cls = new_cls;
		}

		if (new_imm != '\0') {
			if (immediate_type != '\0' && immediate_type != new_imm)
				panic("multiple immediate types not supported");
			immediate_type = new_imm;
		}
	}

	if (same_as >= 0) {
		if (cls != NULL)
			panic("same as and register constraint not supported");
		if (immediate_type != '\0')
			panic("same as and immediate constraint not supported");
	}

	if (cls == NULL && same_as < 0 && !memory_possible)
		panic("no constraint specified for assembler input");

	constraint->same_as               = same_as;
	constraint->cls                   = cls;
	constraint->allowed_registers     = limited;
	constraint->all_registers_allowed = all_registers_allowed;
	constraint->memory_possible       = memory_possible;
	constraint->immediate_type        = immediate_type;
}

static bool can_match(const arch_register_req_t *in,
                      const arch_register_req_t *out)
{
	if (in->cls != out->cls)
		return false;
	if (!arch_register_req_is(in,  limited) ||
	    !arch_register_req_is(out, limited))
		return true;

	return (*in->limited & *out->limited) != 0;
}

static bool match_requirement(arch_register_req_t const **reqs,
                              size_t const n_reqs, bitset_t *const used,
                              arch_register_req_t const *const req)
{
	if (!req->cls)
		return true;
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

static bool check_immediate_constraint(long val, char immediate_constraint_type)
{
	switch (immediate_constraint_type) {
	case 'g':
	case 'i':
	case 'n': return true;

	case 'I': return 0 <= val && val <=  31;
	case 'J': return 0 <= val && val <=  63;
	case 'K': return -128 <= val && val < 128;
	case 'L': return val == 0xff || val == 0xffff;
	case 'M': return 0 <= val && val <=   3;
	case 'N': return 0 <= val && val <= 255;
	case 'O': return 0 <= val && val <= 127;
	}
	panic("invalid immediate constraint found");
}

bool x86_match_immediate(x86_imm32_t *immediate, const ir_node *node,
                         char const constraint)
{
	ir_mode *const mode = get_irn_mode(node);
	if (get_mode_arithmetic(mode) != irma_twos_complement)
		return false;

	const ir_node *cnst;
	ir_entity     *entity;
	if (is_Const(node)) {
		cnst   = node;
		entity = NULL;
	} else if (is_Address(node)) {
		cnst   = NULL;
		entity = get_Address_entity(node);
		if (is_tls_entity(entity))
			return false;
	} else if (is_Add(node)) {
		ir_node *left  = get_Add_left(node);
		ir_node *right = get_Add_right(node);
		if (is_Const(left) && is_Address(right)) {
			cnst   = left;
			entity = get_Address_entity(right);
		} else if (is_Address(left) && is_Const(right)) {
			cnst   = right;
			entity = get_Address_entity(left);
		} else {
			return false;
		}
	} else {
		return false;
	}

	long val = 0;
	if (cnst != NULL) {
		ir_tarval *offset = get_Const_tarval(cnst);
		if (!tarval_is_long(offset)) {
			ir_fprintf(stderr,
			           "Optimization warning: tarval of %+F is not a long?\n",
			           cnst);
			return false;
		}

		val = get_tarval_long(offset);
		if (!check_immediate_constraint(val, constraint))
			return false;
	}

	if (entity != NULL) {
		/* we need full 32bits for entities */
		if (constraint != 'i' && constraint != 'n' && constraint != 'g')
			return false;
	}

	/* we are fine */
	immediate->entity = entity;
	immediate->offset = (int32_t)val;
	return true;
}

static void x86_emit_immediate(const x86_imm32_t *immediate)
{
	const ir_entity *entity = immediate->entity;
	if (entity != NULL)
		be_gas_emit_entity(entity);
	int32_t offset = immediate->offset;
	if (entity == NULL || offset != 0) {
		if (entity != NULL) {
			be_emit_irprintf("%+"PRId32, offset);
		} else {
			be_emit_irprintf("0x%"PRIX32, (uint32_t)offset);
		}
	}
}

ir_node *x86_match_ASM(const ir_node *node, new_bd_asm_func new_bd_asm,
                       const x86_clobber_name_t *additional_clobber_names,
                       const x86_asm_constraint_list_t *constraints)
{
	int                      const n_inputs          = get_ASM_n_inputs(node);
	size_t                   const n_out_constraints = get_ASM_n_output_constraints(node);
	ir_asm_constraint const *const in_constraints    = get_ASM_input_constraints(node);
	ir_asm_constraint const *const out_constraints   = get_ASM_output_constraints(node);

	/* determine maximum number of operands */
	unsigned max_operands = 0;
	for (size_t i = 0; i < n_out_constraints; ++i) {
		max_operands = MAX(max_operands, out_constraints[i].pos + 1);
	}
	for (int i = 0; i < n_inputs; ++i) {
		max_operands = MAX(max_operands, in_constraints[i].pos + 1);
	}

	ir_graph       *const irg  = get_irn_irg(node);
	struct obstack *const obst = get_irg_obstack(irg);
	x86_asm_operand_t *const operands
		= NEW_ARR_DZ(x86_asm_operand_t, obst, max_operands);

	/* construct output constraints */
	size_t              const   n_clobbers   = get_ASM_n_clobbers(node);
	size_t                      out_size     = n_out_constraints + n_clobbers;
	arch_register_req_t const **out_reg_reqs = OALLOCN(obst, arch_register_req_t const*, out_size);

	size_t out_arity = 0;
	for (; out_arity < n_out_constraints; ++out_arity) {
		ir_asm_constraint const *const constraint = &out_constraints[out_arity];

		parsed_constraint_t parsed_constraint;
		parse_asm_constraints(&parsed_constraint, constraints,
		                      constraint->constraint, true);

		out_reg_reqs[out_arity]
			= x86_make_register_req(obst, &parsed_constraint, n_out_constraints,
			                        out_reg_reqs, out_arity);

		x86_asm_operand_t *const op = &operands[constraint->pos];
		/* multiple constraints for same pos. This can happen for example when
		 * a =A constraint gets lowered to two constraints: =a and =d for the
		 * same pos */
		if (op->kind != ASM_OP_INVALID)
			continue;

		op->kind      = ASM_OP_OUT_REG;
		op->inout_pos = out_arity;
		assert(op->inout_pos == out_arity); // make sure we had no overflow
		op->u.mode    = constraint->mode;
	}

	/* parse clobbers */
	const arch_env_t *arch_env = be_get_irg_arch_env(irg);

	unsigned clobber_bits[arch_env->n_register_classes];
	memset(&clobber_bits, 0, sizeof(clobber_bits));
	ident **const clobbers = get_ASM_clobbers(node);
	for (size_t c = 0; c < n_clobbers; ++c) {
		char            const *const clobber = get_id_str(clobbers[c]);
		arch_register_t const *const reg
			= x86_parse_clobber(arch_env, additional_clobber_names, clobber);
		if (reg != NULL) {
			assert(reg->cls->n_regs <= sizeof(unsigned) * 8);
			/* x87 registers may still be used as input, even if clobbered. */
			if (reg->cls != &ia32_reg_classes[CLASS_ia32_fp])
				clobber_bits[reg->cls->index] |= 1U << reg->index;
			out_reg_reqs[out_arity++] = reg->single_req;
		}
	}

	/* inputs + input constraints */
	unsigned                    max_ins     = n_inputs + 1;
	ir_node                   **in          = ALLOCANZ(ir_node*, max_ins);
	arch_register_req_t const **in_reg_reqs = OALLOCN(obst, arch_register_req_t const*, max_ins);
	unsigned                    n_ins       = 0;
	for (int i = 0; i < n_inputs; ++i) {
		ir_asm_constraint const *const constraint = &in_constraints[i];

		parsed_constraint_t parsed_constraint;
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

		ir_node *new_pred = be_transform_node(pred);

		op->kind = ASM_OP_IN_REG;
		int in_pos = n_ins++;
		in[in_pos] = new_pred;
		in_reg_reqs[in_pos]
			= x86_make_register_req(obst, &parsed_constraint, n_out_constraints,
			                        out_reg_reqs, in_pos);
		op->inout_pos = in_pos;
		op->u.mode    = constraint->mode;

		if (cls == NULL && parsed_constraint.same_as < 0) {
			op->kind = ASM_OP_MEMORY;
			const arch_register_req_t *req
				= arch_get_irn_register_req(new_pred);
			in_reg_reqs[i] = req->cls->class_req;
		} else if (parsed_constraint.memory_possible) {
			/* TODO: match Load or Load/Store if memory possible is set */
		}
	}

	assert(n_ins < max_ins);
	int in_mem = n_ins++;
	in[in_mem]          = be_transform_node(get_ASM_mem(node));
	in_reg_reqs[in_mem] = arch_no_register_req;

	/* Handle early clobbers. */
	for (size_t o = 0; o != n_out_constraints; ++o) {
		ir_asm_constraint const *const constraint = &out_constraints[o];
		if (!strchr(get_id_str(constraint->constraint), '&'))
			continue;
		arch_register_req_t const *const oreq = out_reg_reqs[o];

		unsigned different = 0;
		for (int i = 0; i != n_inputs; ++i) {
			if (in_reg_reqs[i]->cls == oreq->cls)
				different |= 1U << i;
		}

		if (different != 0) {
			arch_register_req_t *const req = OALLOC(obst, arch_register_req_t);
			*req                 = *oreq;
			req->type           |= arch_register_req_type_must_be_different;
			req->other_different = different;
			out_reg_reqs[o]      = req;
		}
	}

	ir_node *const block = be_transform_node(get_nodes_block(node));

	/* Attempt to make ASM node register pressure faithful.
	 * (This does not work for complicated cases yet!)
	 *
	 * Algorithm: Check if there are fewer inputs or outputs (I will call this
	 * the smaller list). Then try to match each constraint of the smaller list
	 * to 1 of the other list. If we can't match it, then we have to add a dummy
	 * input/output to the other list
	 *
	 * FIXME: This is still broken in lots of cases. But at least better than
	 *        before...
	 * FIXME: need to do this per register class...
	 */
	if (out_arity <= (size_t)n_inputs) {
		unsigned        in_size     = n_ins;
		unsigned  const orig_inputs = n_ins;
		bitset_t *const used_ins    = bitset_alloca(n_ins);
		for (size_t o = 0; o < out_arity; ++o) {
			arch_register_req_t const *const outreq = out_reg_reqs[o];
			if (match_requirement(in_reg_reqs, orig_inputs, used_ins, outreq))
				continue;

			/* we might need more space in the input arrays */
			if (n_ins >= in_size) {
				in_size *= 2;
				arch_register_req_t const **const new_in_reg_reqs
					= OALLOCN(obst, arch_register_req_t const*, in_size);
				MEMCPY(new_in_reg_reqs, in_reg_reqs, n_ins);
				ir_node **const new_in = ALLOCANZ(ir_node*, in_size);
				MEMCPY(new_in, in, n_ins);

				in_reg_reqs = new_in_reg_reqs;
				in          = new_in;
			}

			/* add a new (dummy) input which occupies the register */
			assert(arch_register_req_is(outreq, limited));
			in_reg_reqs[n_ins] = outreq;
			in[n_ins]          = be_new_AnyVal(block, outreq->cls);
			++n_ins;
		}
	} else {
		bitset_t *used_outs      = bitset_alloca(out_arity);
		size_t    orig_out_arity = out_arity;
		for (int i = 0; i < n_inputs; ++i) {
			arch_register_req_t const *const inreq = in_reg_reqs[i];
			if (match_requirement(out_reg_reqs, orig_out_arity, used_outs, inreq))
				continue;

			/* we might need more space in the output arrays */
			if (out_arity >= out_size) {
				out_size *= 2;
				arch_register_req_t const **const new_out_reg_reqs
					= OALLOCN(obst, arch_register_req_t const*, out_size);
				MEMCPY(new_out_reg_reqs, out_reg_reqs, out_arity);
				out_reg_reqs = new_out_reg_reqs;
			}

			/* add a new (dummy) output which occupies the register */
			assert(arch_register_req_is(inreq, limited));
			out_reg_reqs[out_arity++] = inreq;
		}
	}

	/* append none register requirement for the memory output */
	if (out_arity + 1 >= out_size) {
		out_size = out_arity + 1;
		arch_register_req_t const **const new_out_reg_reqs
			= OALLOCN(obst, arch_register_req_t const*, out_size);
		MEMCPY(new_out_reg_reqs, out_reg_reqs, out_arity);
		out_reg_reqs = new_out_reg_reqs;
	}

	/* add a new (dummy) output which occupies the register */
	out_reg_reqs[out_arity++] = arch_no_register_req;

	dbg_info      *const dbgi     = get_irn_dbg_info(node);
	x86_asm_attr_t const attr     = { get_ASM_text(node), operands };
	ir_node       *const new_node = new_bd_asm(dbgi, block, n_ins, in,
	                                           out_arity, &attr);

	backend_info_t *const info = be_get_info(new_node);
	for (size_t o = 0; o < out_arity; ++o) {
		info->out_infos[o].req = out_reg_reqs[o];
	}
	arch_set_irn_register_reqs_in(new_node, in_reg_reqs);

	return new_node;
}

/**
 * Emit an inline assembler operand.
 * @return  pointer to the first char in s NOT in the current operand
 */
static const char* emit_asm_operand(const ir_node *node,
                                    const x86_asm_attr_t *attr,
                                    emit_register_func emit_register,
                                    const char *s)
{
	assert(*s == '%');
	char c = *(++s);

	/* parse modifiers */
	char modifier = 0;
	switch (c) {
	case '\0':
		/* TODO: move this warning to the frontend */
		ir_fprintf(stderr, "Warning: asm text (%+F) ends with %%\n", node);
		be_emit_char('%');
		return s;

	case '%':
		be_emit_char('%');
		return s + 1;
	case 'w':
	case 'b':
	case 'h':
		modifier = c;
		++s;
		break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		break;
	default:
		ir_fprintf(stderr,
		           "Warning: asm text (%+F) contains unknown modifier '%c' for asm op\n",
		           node, c);
		++s;
		break;
	}

	/* parse number */
	unsigned num;
	int p;
	if (sscanf(s, "%u%n", &num, &p) != 1) {
		ir_fprintf(stderr, "Warning: Couldn't parse assembler operand (%+F)\n",
		           node);
		return s;
	} else {
		s += p;
	}

	const x86_asm_operand_t *operands = attr->operands;
	if (num >= ARR_LEN(operands)) {
		ir_fprintf(stderr,
		           "Error: Custom assembler references invalid input/output (%+F)\n",
		           node);
		return s;
	}
	const x86_asm_operand_t *op = &operands[num];
	assert(op->kind != ASM_OP_INVALID);

	/* immediate? */
	if (op->kind == ASM_OP_IMMEDIATE) {
		be_emit_char('$');
		x86_emit_immediate(&op->u.imm32);
		return s;
	}

	/* get register */
	const arch_register_t *reg;
	if (op->kind == ASM_OP_OUT_REG) {
		reg = arch_get_irn_register_out(node, op->inout_pos);
	} else if (op->kind == ASM_OP_IN_REG || op->kind == ASM_OP_MEMORY) {
		reg = arch_get_irn_register_in(node, op->inout_pos);
	} else {
		panic("invalid asm operand");
	}
	if (reg == NULL) {
		ir_fprintf(stderr,
		           "Warning: no register assigned for %d asm op (%+F)\n",
		           num, node);
		return s;
	}

	/* Emit the register. */
	if (op->kind == ASM_OP_MEMORY) {
		be_emit_char('(');
		emit_register(reg, '\0', NULL);
		be_emit_char(')');
	} else {
		emit_register(reg, modifier, op->u.mode);
	}

	return s;
}

void x86_emit_asm(const ir_node *node, const x86_asm_attr_t *attr,
                  emit_register_func emit_register)
{
	ident      *asm_text = attr->asm_text;
	const char *s        = get_id_str(asm_text);

	be_emit_cstring("#APP");
	be_emit_finish_line_gas(node);

	if (s[0] != '\t')
		be_emit_char('\t');

	while (*s != 0) {
		if (*s == '%') {
			s = emit_asm_operand(node, attr, emit_register, s);
		} else {
			be_emit_char(*s++);
		}
	}

	be_emit_cstring("\n#NO_APP\n");
	be_emit_write_line();
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
			flags = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE
			      | ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP
			      | ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
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
