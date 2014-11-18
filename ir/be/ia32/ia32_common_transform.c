/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file implements the common parts of IR transformation from
 *              firm into ia32-Firm.
 * @author      Matthias Braun, Sebastian Buchwald
 */
#include "bearch_ia32_t.h"
#include "beutil.h"
#include "panic.h"
#include "ircons.h"
#include "irprintf.h"
#include "typerep.h"
#include "bitset.h"
#include "heights.h"

#include "betranshlp.h"

#include "ia32_architecture.h"
#include "ia32_common_transform.h"
#include "ia32_new_nodes.h"
#include "util.h"

#include "gen_ia32_new_nodes.h"
#include "gen_ia32_regalloc_if.h"

ir_heights_t *ia32_heights = NULL;

static bool check_immediate_constraint(long val, char immediate_constraint_type)
{
	switch (immediate_constraint_type) {
	case 'g':
	case 'i':
	case 'n': return true;

	case 'I': return 0 <= val && val <=  31;
	case 'J': return 0 <= val && val <=  63;
	case 'K': return ia32_is_8bit_val(val);
	case 'L': return val == 0xff || val == 0xffff;
	case 'M': return 0 <= val && val <=   3;
	case 'N': return 0 <= val && val <= 255;
	case 'O': return 0 <= val && val <= 127;

	default: panic("invalid immediate constraint found");
	}
}

ir_type *ia32_get_prim_type(const ir_mode *mode)
{
	if (mode == ia32_mode_E) {
		return ia32_type_E;
	} else {
		return get_type_for_mode(mode);
	}
}

ir_entity *ia32_create_float_const_entity(ia32_isa_t *isa, ir_tarval *tv,
                                          ident *name)
{
	ir_entity        *res = pmap_get(ir_entity, isa->tv_ent, tv);
	ir_initializer_t *initializer;
	ir_mode          *mode;
	ir_type          *tp;

	if (res != NULL)
		return res;

	mode = get_tarval_mode(tv);

	if (! ia32_cg_config.use_sse2) {
		/* try to reduce the mode to produce smaller sized entities */
		if (mode != mode_F) {
			if (tarval_ieee754_can_conv_lossless(tv, mode_F)) {
				mode = mode_F;
				tv = tarval_convert_to(tv, mode);
			} else if (mode != mode_D) {
				if (tarval_ieee754_can_conv_lossless(tv, mode_D)) {
					mode = mode_D;
					tv = tarval_convert_to(tv, mode);
				}
			}
		}
	}

	if (name == NULL)
		name = id_unique("C%u");

	tp  = ia32_get_prim_type(mode);
	res = new_entity(get_glob_type(), name, tp);
	set_entity_ld_ident(res, get_entity_ident(res));
	set_entity_visibility(res, ir_visibility_private);
	add_entity_linkage(res, IR_LINKAGE_CONSTANT);

	initializer = create_initializer_tarval(tv);
	set_entity_initializer(res, initializer);

	pmap_insert(isa->tv_ent, tv, res);
	return res;
}

ir_node *ia32_create_Immediate_full(ir_graph *const irg, ir_entity *const entity, bool const no_pic_adjust, int32_t const val)
{
	ir_node *const start_block = get_irg_start_block(irg);
	ir_node *const immediate   = new_bd_ia32_Immediate(NULL, start_block, entity, no_pic_adjust, val);
	arch_set_irn_register(immediate, &ia32_registers[REG_GP_NOREG]);
	return immediate;
}

const arch_register_t *ia32_get_clobber_register(const char *clobber)
{
	/* TODO: construct a hashmap instead of doing linear search for clobber
	 * register */
	for (size_t i = 0; i != N_IA32_REGISTERS; ++i) {
		arch_register_t const *const reg = &ia32_registers[i];
		if (strcmp(reg->name, clobber) == 0 ||
		    (reg->cls == &ia32_reg_classes[CLASS_ia32_gp] && strcmp(reg->name + 1, clobber) == 0)) {
			return reg;
		}
	}

	return NULL;
}

bool ia32_mode_needs_gp_reg(ir_mode *mode)
{
	return get_mode_arithmetic(mode) == irma_twos_complement;
}

/**
 * An assembler constraint.
 */
typedef struct constraint_t {
	const arch_register_class_t *cls;
	unsigned                     allowed_registers;
	char                         all_registers_allowed;
	char                         memory_possible;
	char                         immediate_type;
	int                          same_as;
} constraint_t;

static void parse_asm_constraints(constraint_t *const constraint, ident *const constraint_text, bool const is_output)
{
	memset(constraint, 0, sizeof(constraint[0]));
	constraint->same_as = -1;

	char const *c = get_id_str(constraint_text);
	if (*c == 0) {
		/* a memory constraint: no need to do anything in backend about it
		 * (the dependencies are already respected by the memory edge of
		 * the node) */
		return;
	}

	arch_register_class_t const *const gp = &ia32_reg_classes[CLASS_ia32_gp];

	/* TODO: improve error messages with node and source info. (As users can
	 * easily hit these) */
	char                         immediate_type        = '\0';
	unsigned                     limited               = 0;
	arch_register_class_t const *cls                   = NULL;
	bool                         memory_possible       = false;
	bool                         all_registers_allowed = false;
	int                          same_as               = -1;
	while (*c != 0) {
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
			while (*c != 0 && *c != ',')
				++c;
			break;

		case 'a': new_cls = gp; limited |= 1 << REG_GP_EAX; break;
		case 'b': new_cls = gp; limited |= 1 << REG_GP_EBX; break;
		case 'c': new_cls = gp; limited |= 1 << REG_GP_ECX; break;
		case 'd': new_cls = gp; limited |= 1 << REG_GP_EDX; break;
		case 'D': new_cls = gp; limited |= 1 << REG_GP_EDI; break;
		case 'S': new_cls = gp; limited |= 1 << REG_GP_ESI; break;

		case 'Q':
		case 'q':
			/* q means lower part of the regs only, this makes no
			 * difference to Q for us (we only assign whole registers) */
			new_cls  = gp;
			limited |= 1 << REG_GP_EAX | 1 << REG_GP_EBX | 1 << REG_GP_ECX |
			           1 << REG_GP_EDX;
			break;

		case 'A':
			new_cls  = gp;
			limited |= 1 << REG_GP_EAX | 1 << REG_GP_EDX;
			break;

		case 'l':
			new_cls  = gp;
			limited |= 1 << REG_GP_EAX | 1 << REG_GP_EBX | 1 << REG_GP_ECX |
			           1 << REG_GP_EDX | 1 << REG_GP_ESI | 1 << REG_GP_EDI |
			           1 << REG_GP_EBP;
			break;

		case 'R':
		case 'r':
		case 'p':
			new_cls               = gp;
			all_registers_allowed = true;
			break;

		case 'f':
		case 't':
		case 'u':
			/* TODO: mark values so the x87 simulator knows about t and u */
			new_cls               = &ia32_reg_classes[CLASS_ia32_fp];
			all_registers_allowed = true;
			break;

		case 'Y':
		case 'x':
			new_cls               = &ia32_reg_classes[CLASS_ia32_xmm];
			all_registers_allowed = true;
			break;

		case 'I':
		case 'J':
		case 'K':
		case 'L':
		case 'M':
		case 'N':
		case 'O':
			new_cls = gp;
			new_imm = *c;
			break;

		case 'n':
		case 'i':
			new_cls = gp;
			new_imm = 'i';
			break;

		case 'X':
		case 'g':
			new_cls               = gp;
			new_imm               = 'i';
			all_registers_allowed = true;
			memory_possible       = true;
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
			sscanf(c, "%d%n", &same_as, &p);
			if (same_as >= 0) {
				c += p;
				continue;
			}
			break;
		}

		case 'm':
		case 'o':
		case 'V':
			/* memory constraint no need to do anything in backend about it
			 * (the dependencies are already respected by the memory edge of
			 * the node) */
			memory_possible = true;
			break;

		case 'E': /* no float consts yet */
		case 'F': /* no float consts yet */
		case 's': /* makes no sense on x86 */
		case '<': /* no autodecrement on x86 */
		case '>': /* no autoincrement on x86 */
		case 'C': /* sse constant not supported yet */
		case 'G': /* 80387 constant not supported yet */
		case 'y': /* we don't support mmx registers yet */
		case 'Z': /* not available in 32 bit mode */
		case 'e': /* not available in 32 bit mode */
			panic("unsupported asm constraint '%c'", *c);

		default:
			panic("unknown asm constraint '%c'", *c);
		}

		if (new_cls) {
			if (!cls) {
				cls = new_cls;
			} else if (cls != new_cls) {
				panic("multiple register classes not supported");
			}
		}

		if (new_imm != '\0') {
			if (immediate_type == '\0') {
				immediate_type = new_imm;
			} else if (immediate_type != new_imm) {
				panic("multiple immediate types not supported");
			}
		}

		++c;
	}

	if (same_as >= 0) {
		if (cls != NULL)
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

static bool match_requirement(arch_register_req_t const **reqs, size_t const n_reqs, bitset_t *const used, arch_register_req_t const *const req)
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

static arch_register_req_t const *ia32_make_register_req(ir_graph *irg, constraint_t const *constraint, int n_outs, arch_register_req_t const **out_reqs, int pos);
static arch_register_t const *ia32_parse_clobber(char const *clobber);

ir_node *ia32_gen_ASM(ir_node *node)
{
	ir_node        *block        = get_nodes_block(node);
	ir_node        *new_block    = be_transform_node(block);
	dbg_info       *dbgi         = get_irn_dbg_info(node);
	int             n_inputs     = get_ASM_n_inputs(node);
	int             n_ins        = n_inputs+1;
	ir_node       **in           = ALLOCANZ(ir_node*, n_ins);
	size_t          n_clobbers   = 0;
	ident         **clobbers     = get_ASM_clobbers(node);
	unsigned        reg_map_size = 0;
	ir_graph       *irg          = get_irn_irg(node);
	struct obstack *obst         = get_irg_obstack(irg);
	unsigned        clobber_bits[N_IA32_CLASSES];
	memset(&clobber_bits, 0, sizeof(clobber_bits));

	for (size_t c = 0; c < get_ASM_n_clobbers(node); ++c) {
		char            const *const clobber = get_id_str(clobbers[c]);
		arch_register_t const *const reg     = ia32_parse_clobber(clobber);
		if (reg) {
			assert(reg->cls->n_regs <= sizeof(unsigned) * 8);
			clobber_bits[reg->cls->index] |= 1U << reg->index;
			++n_clobbers;
		}
	}
	size_t n_out_constraints = get_ASM_n_output_constraints(node);
	size_t out_arity         = n_out_constraints + n_clobbers;

	const ir_asm_constraint *in_constraints  = get_ASM_input_constraints(node);
	const ir_asm_constraint *out_constraints = get_ASM_output_constraints(node);

	/* determine size of register_map */
	for (size_t out_idx = 0; out_idx < n_out_constraints; ++out_idx) {
		const ir_asm_constraint *constraint = &out_constraints[out_idx];
		if (constraint->pos+1 > reg_map_size)
			reg_map_size = constraint->pos+1;
	}
	for (int i = 0; i < n_inputs; ++i) {
		const ir_asm_constraint *constraint = &in_constraints[i];
		if (constraint->pos+1 > reg_map_size)
			reg_map_size = constraint->pos+1;
	}

	ia32_asm_reg_t *const register_map = NEW_ARR_DZ(ia32_asm_reg_t, obst, reg_map_size);

	/* construct output constraints */
	size_t                      out_size = out_arity + 1;
	const arch_register_req_t **out_reg_reqs
		= OALLOCN(obst, const arch_register_req_t*, out_size);

	size_t out_idx;
	for (out_idx = 0; out_idx < n_out_constraints; ++out_idx) {
		constraint_t             parsed_constraint;
		const ir_asm_constraint *constraint = &out_constraints[out_idx];
		unsigned                 pos        = constraint->pos;
		parse_asm_constraints(&parsed_constraint, constraint->constraint, true);
		arch_register_req_t const *const req = ia32_make_register_req(irg, &parsed_constraint, n_out_constraints, out_reg_reqs, out_idx);
		out_reg_reqs[out_idx] = req;

		/* multiple constraints for same pos. This can happen for example when
		 * a =A constraint gets lowered to two constraints: =a and =d for the
		 * same pos */
		if (register_map[pos].valid)
			continue;

		register_map[pos].use_input = 0;
		register_map[pos].valid     = 1;
		register_map[pos].memory    = 0;
		register_map[pos].inout_pos = out_idx;
		register_map[pos].mode      = constraint->mode;
	}

	/* inputs + input constraints */
	const arch_register_req_t **in_reg_reqs
		= OALLOCN(obst, const arch_register_req_t*, n_ins);
	for (int i = 0; i < n_inputs; ++i) {
		constraint_t               parsed_constraint;
		ir_node                   *pred         = get_ASM_input(node, i);
		const ir_asm_constraint   *constraint   = &in_constraints[i];
		unsigned                   pos          = constraint->pos;
		int                        is_memory_op = 0;
		ir_node                   *input        = NULL;

		parse_asm_constraints(&parsed_constraint, constraint->constraint, false);
		if (parsed_constraint.cls != NULL) {
			unsigned r_clobber_bits
				= clobber_bits[parsed_constraint.cls->index];
			if (r_clobber_bits != 0) {
				if (parsed_constraint.all_registers_allowed) {
					parsed_constraint.all_registers_allowed = 0;
					be_get_allocatable_regs(irg, parsed_constraint.cls, &parsed_constraint.allowed_registers);
				}
				parsed_constraint.allowed_registers &= ~r_clobber_bits;
			}
		}

		arch_register_req_t const *const req = ia32_make_register_req(irg, &parsed_constraint, n_out_constraints, out_reg_reqs, i);
		in_reg_reqs[i] = req;

		if (parsed_constraint.immediate_type != '\0') {
			char imm_type = parsed_constraint.immediate_type;
			input = ia32_try_create_Immediate(pred, imm_type);
		}

		if (input == NULL) {
			input = be_transform_node(pred);

			if (parsed_constraint.cls == NULL
					&& parsed_constraint.same_as < 0) {
				is_memory_op = 1;
				in_reg_reqs[i] = ia32_reg_classes[CLASS_ia32_gp].class_req;
			} else if (parsed_constraint.memory_possible) {
				/* TODO: match Load or Load/Store if memory possible is set */
			}
		}
		in[i] = input;

		register_map[pos].use_input = 1;
		register_map[pos].valid     = 1;
		register_map[pos].memory    = is_memory_op;
		register_map[pos].inout_pos = i;
		register_map[pos].mode      = constraint->mode;
	}

	assert(n_inputs == n_ins-1);
	ir_node *mem = get_ASM_mem(node);
	in[n_inputs]          = be_transform_node(mem);
	in_reg_reqs[n_inputs] = arch_no_register_req;

	/* parse clobbers */
	for (size_t c = 0; c < get_ASM_n_clobbers(node); ++c) {
		char            const *const clobber = get_id_str(clobbers[c]);
		arch_register_t const *const reg     = ia32_parse_clobber(clobber);
		if (reg)
			out_reg_reqs[out_idx++] = reg->single_req;
	}

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
		int       orig_inputs = n_ins;
		int       in_size     = n_ins;
		bitset_t *used_ins    = bitset_alloca(n_ins);
		for (size_t o = 0; o < out_arity; ++o) {
			const arch_register_req_t *outreq = out_reg_reqs[o];
			if (match_requirement(in_reg_reqs, orig_inputs, used_ins, outreq))
				continue;

			/* we might need more space in the input arrays */
			if (n_ins >= in_size) {
				in_size *= 2;
				const arch_register_req_t **new_in_reg_reqs
					= OALLOCN(obst, const arch_register_req_t*,
				                          in_size);
				MEMCPY(new_in_reg_reqs, in_reg_reqs, n_ins);
				ir_node **new_in = ALLOCANZ(ir_node*, in_size);
				MEMCPY(new_in, in, n_ins);

				in_reg_reqs = new_in_reg_reqs;
				in          = new_in;
			}

			/* add a new (dummy) input which occupies the register */
			assert(arch_register_req_is(outreq, limited));
			in_reg_reqs[n_ins] = outreq;
			in[n_ins]          = new_bd_ia32_ProduceVal(NULL, block);
			++n_ins;
		}
	} else {
		bitset_t *used_outs      = bitset_alloca(out_arity);
		size_t    orig_out_arity = out_arity;
		for (int i = 0; i < n_inputs; ++i) {
			const arch_register_req_t *inreq = in_reg_reqs[i];
			if (match_requirement(out_reg_reqs, orig_out_arity, used_outs, inreq))
				continue;

			/* we might need more space in the output arrays */
			if (out_arity >= out_size) {
				const arch_register_req_t **new_out_reg_reqs;

				out_size *= 2;
				new_out_reg_reqs
					= OALLOCN(obst, const arch_register_req_t*, out_size);
				MEMCPY(new_out_reg_reqs, out_reg_reqs, out_arity);
				out_reg_reqs = new_out_reg_reqs;
			}

			/* add a new (dummy) output which occupies the register */
			assert(arch_register_req_is(inreq, limited));
			out_reg_reqs[out_arity] = inreq;
			++out_arity;
		}
	}

	/* append none register requirement for the memory output */
	if (out_arity + 1 >= out_size) {
		const arch_register_req_t **new_out_reg_reqs;

		out_size = out_arity + 1;
		new_out_reg_reqs
			= OALLOCN(obst, const arch_register_req_t*, out_size);
		MEMCPY(new_out_reg_reqs, out_reg_reqs, out_arity);
		out_reg_reqs = new_out_reg_reqs;
	}

	/* add a new (dummy) output which occupies the register */
	out_reg_reqs[out_arity] = arch_no_register_req;
	++out_arity;

	ir_node *new_node = new_bd_ia32_Asm(dbgi, new_block, n_ins, in, out_arity,
	                                    get_ASM_text(node), register_map);

	backend_info_t *info = be_get_info(new_node);
	for (size_t o = 0; o < out_arity; ++o) {
		info->out_infos[o].req = out_reg_reqs[o];
	}
	arch_set_irn_register_reqs_in(new_node, in_reg_reqs);

	SET_IA32_ORIG_NODE(new_node, node);

	return new_node;
}

ir_node *ia32_gen_CopyB(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *src      = get_CopyB_src(node);
	ir_node  *new_src  = be_transform_node(src);
	ir_node  *dst      = get_CopyB_dst(node);
	ir_node  *new_dst  = be_transform_node(dst);
	ir_node  *mem      = get_CopyB_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	int      size      = get_type_size_bytes(get_CopyB_type(node));
	int      rem;

	/* If we have to copy more than 32 bytes, we use REP MOVSx and */
	/* then we need the size explicitly in ECX.                    */
	ir_node *projm;
	if (size >= 32 * 4) {
		rem = size & 0x3; /* size % 4 */
		size >>= 2;

		ir_node *cnst  = new_bd_ia32_Const(dbgi, block, NULL, 0, size);
		ir_node *copyb = new_bd_ia32_CopyB(dbgi, block, new_dst, new_src, cnst,
		                                   new_mem, rem);
		SET_IA32_ORIG_NODE(copyb, node);
		projm = new_r_Proj(copyb, mode_M, pn_ia32_CopyB_M);
	} else {
		if (size == 0) {
			ir_fprintf(stderr, "Optimization warning: %+F with size <4\n", node);
		}
		ir_node *copyb = new_bd_ia32_CopyB_i(dbgi, block, new_dst, new_src,
		                                     new_mem, size);
		SET_IA32_ORIG_NODE(copyb, node);
		projm = new_r_Proj(copyb, mode_M, pn_ia32_CopyB_i_M);
	}
	return projm;
}

ir_node *ia32_gen_Proj_tls(ir_node *node)
{
	ir_node *block = be_transform_node(get_nodes_block(node));
	ir_node *res   = new_bd_ia32_LdTls(NULL, block);
	return res;
}

ir_node *ia32_gen_Unknown(ir_node *node)
{
	ir_mode  *mode  = get_irn_mode(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *block = get_irg_start_block(irg);
	ir_node  *res   = NULL;

	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2) {
			res = new_bd_ia32_xUnknown(dbgi, block);
		} else {
			res = new_bd_ia32_fldz(dbgi, block);
		}
	} else if (ia32_mode_needs_gp_reg(mode)) {
		res = new_bd_ia32_Unknown(dbgi, block);
	} else {
		panic("unsupported Unknown-Mode");
	}

	return res;
}

static arch_register_req_t const *ia32_make_register_req(ir_graph *const irg, constraint_t const *const c, int const n_outs, arch_register_req_t const **const out_reqs, int const pos)
{
	int const same_as = c->same_as;
	if (same_as >= 0) {
		if (same_as >= n_outs)
			panic("invalid output number in same_as constraint");

		struct obstack            *const obst  = get_irg_obstack(irg);
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

	if (c->allowed_registers == 0 || c->all_registers_allowed)
		return c->cls->class_req;

	struct obstack      *const obst    = get_irg_obstack(irg);
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

static arch_register_t const *ia32_parse_clobber(char const *const clobber)
{
	if (strcmp(clobber, "memory") == 0 || strcmp(clobber, "cc") == 0)
		return NULL;

	arch_register_t const *const reg = ia32_get_clobber_register(clobber);
	if (!reg)
		panic("register '%s' mentioned in asm clobber is unknown", clobber);

	return reg;
}


bool ia32_prevents_AM(ir_node *const block, ir_node *const am_candidate,
                      ir_node *const other)
{
	if (get_nodes_block(other) != block)
		return false;

	if (is_Sync(other)) {
		int i;

		for (i = get_Sync_n_preds(other) - 1; i >= 0; --i) {
			ir_node *const pred = get_Sync_pred(other, i);

			if (get_nodes_block(pred) != block)
				continue;

			/* Do not block ourselves from getting eaten */
			if (is_Proj(pred) && get_Proj_pred(pred) == am_candidate)
				continue;

			if (!heights_reachable_in_block(ia32_heights, pred, am_candidate))
				continue;

			return true;
		}

		return false;
	} else {
		/* Do not block ourselves from getting eaten */
		if (is_Proj(other) && get_Proj_pred(other) == am_candidate)
			return false;

		if (!heights_reachable_in_block(ia32_heights, other, am_candidate))
			return false;

		return true;
	}
}

ir_node *ia32_try_create_Immediate(ir_node *node, char immediate_constraint_type)
{
	ir_mode *const mode = get_irn_mode(node);
	if (!mode_is_int(mode) && !mode_is_reference(mode))
		return NULL;

	ir_node   *cnst;
	ir_entity *entity;
	if (is_Const(node)) {
		cnst   = node;
		entity = NULL;
	} else if (is_Address(node)) {
		cnst   = NULL;
		entity = get_Address_entity(node);
		if (is_tls_entity(entity))
			return NULL;
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
			return NULL;
		}
	} else {
		return NULL;
	}

	long val = 0;
	if (cnst != NULL) {
		ir_tarval *offset = get_Const_tarval(cnst);
		if (!tarval_is_long(offset)) {
			ir_fprintf(stderr, "Optimization warning: tarval of %+F is not a long?\n", cnst);
			return NULL;
		}

		val = get_tarval_long(offset);
		if (!check_immediate_constraint(val, immediate_constraint_type))
			return NULL;
	}

	if (entity != NULL) {
		/* we need full 32bits for entities */
		if (immediate_constraint_type != 'i')
			return NULL;
	}

	ir_graph *const irg = get_irn_irg(node);
	return ia32_create_Immediate_full(irg, entity, ia32_no_pic_adjust, val);
}
