/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       This file implements the common parts of IR transformation from
 *              firm into ia32-Firm.
 * @author      Matthias Braun, Sebastian Buchwald
 * @version     $Id: ia32_common_transform.c 21012 2008-08-06 13:35:17Z beck $
 */
#include "config.h"

#include "error.h"
#include "irargs_t.h"
#include "ircons.h"
#include "irprintf.h"
#include "typerep.h"

#include "../betranshlp.h"
#include "../beirg_t.h"

#include "ia32_architecture.h"
#include "ia32_common_transform.h"
#include "ia32_new_nodes.h"

#include "gen_ia32_new_nodes.h"
#include "gen_ia32_regalloc_if.h"

/** hold the current code generator during transformation */
ia32_code_gen_t *env_cg = NULL;

heights_t *heights = NULL;

static const arch_register_req_t no_register_req = {
	arch_register_req_type_none,
	NULL,                         /* regclass */
	NULL,                         /* limit bitset */
	0,                            /* same pos */
	0                             /* different pos */
};

static int check_immediate_constraint(long val, char immediate_constraint_type)
{
	switch (immediate_constraint_type) {
		case 0:
		case 'i': return 1;

		case 'I': return    0 <= val && val <=  31;
		case 'J': return    0 <= val && val <=  63;
		case 'K': return -128 <= val && val <= 127;
		case 'L': return val == 0xff || val == 0xffff;
		case 'M': return    0 <= val && val <=   3;
		case 'N': return    0 <= val && val <= 255;
		case 'O': return    0 <= val && val <= 127;

		default: panic("Invalid immediate constraint found");
	}
}

/**
 * creates a unique ident by adding a number to a tag
 *
 * @param tag   the tag string, must contain a %d if a number
 *              should be added
 */
static ident *unique_id(const char *tag)
{
	static unsigned id = 0;
	char str[256];

	snprintf(str, sizeof(str), tag, ++id);
	return new_id_from_str(str);
}

/**
 * Get a primitive type for a mode.
 */
static ir_type *ia32_get_prim_type(pmap *types, ir_mode *mode)
{
	pmap_entry *e = pmap_find(types, mode);
	ir_type *res;

	if (! e) {
		char buf[64];
		snprintf(buf, sizeof(buf), "prim_type_%s", get_mode_name(mode));
		res = new_type_primitive(new_id_from_str(buf), mode);
		set_type_alignment_bytes(res, 16);
		pmap_insert(types, mode, res);
	}
	else
		res = e->value;
	return res;
}

ir_entity *create_float_const_entity(ir_node *cnst)
{
	ia32_isa_t *isa = env_cg->isa;
	tarval *key     = get_Const_tarval(cnst);
	pmap_entry *e   = pmap_find(isa->tv_ent, key);
	ir_entity *res;
	ir_graph *rem;

	if (e == NULL) {
		tarval  *tv   = key;
		ir_mode *mode = get_tarval_mode(tv);
		ir_type *tp;

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

		if (mode == get_irn_mode(cnst)) {
			/* mode was not changed */
			tp = get_Const_type(cnst);
			if (tp == firm_unknown_type)
				tp = ia32_get_prim_type(isa->types, mode);
		} else
			tp = ia32_get_prim_type(isa->types, mode);

		res = new_entity(get_glob_type(), unique_id(".LC%u"), tp);

		set_entity_ld_ident(res, get_entity_ident(res));
		set_entity_visibility(res, visibility_local);
		set_entity_variability(res, variability_constant);
		set_entity_allocation(res, allocation_static);

		 /* we create a new entity here: It's initialization must resist on the
		    const code irg */
		rem = current_ir_graph;
		current_ir_graph = get_const_code_irg();
		set_atomic_ent_value(res, new_Const_type(tv, tp));
		current_ir_graph = rem;

		pmap_insert(isa->tv_ent, key, res);
	} else {
		res = e->value;
	}

	return res;
}

ir_node *create_Immediate(ir_entity *symconst, int symconst_sign, long val)
{
	ir_graph *irg         = current_ir_graph;
	ir_node  *start_block = get_irg_start_block(irg);
	ir_node  *immediate   = new_rd_ia32_Immediate(NULL, irg, start_block,
	                                              symconst, symconst_sign, val);
	arch_set_irn_register(immediate, &ia32_gp_regs[REG_GP_NOREG]);

	return immediate;
}

const arch_register_t *ia32_get_clobber_register(const char *clobber)
{
	const arch_register_t       *reg = NULL;
	int                          c;
	size_t                       r;
	const arch_register_class_t *cls;

	/* TODO: construct a hashmap instead of doing linear search for clobber
	 * register */
	for(c = 0; c < N_CLASSES; ++c) {
		cls = & ia32_reg_classes[c];
		for(r = 0; r < cls->n_regs; ++r) {
			const arch_register_t *temp_reg = arch_register_for_index(cls, r);
			if(strcmp(temp_reg->name, clobber) == 0
					|| (c == CLASS_ia32_gp && strcmp(temp_reg->name+1, clobber) == 0)) {
				reg = temp_reg;
				break;
			}
		}
		if(reg != NULL)
			break;
	}

	return reg;
}

#ifndef NDEBUG
const char *ia32_get_old_node_name(ia32_code_gen_t *cg, ir_node *irn) {
	const ia32_isa_t *isa = cg->isa;

	lc_eoprintf(firm_get_arg_env(), isa->name_obst, "%+F", irn);
	obstack_1grow(isa->name_obst, 0);
 	return obstack_finish(isa->name_obst);
}
#endif /* NDEBUG */

int ia32_mode_needs_gp_reg(ir_mode *mode) {
	if(mode == mode_fpcw)
		return 0;
	if(get_mode_size_bits(mode) > 32)
		return 0;
	return mode_is_int(mode) || mode_is_reference(mode) || mode == mode_b;
}

static void parse_asm_constraints(constraint_t *constraint, const char *c,
                           int is_output)
{
	char                         immediate_type     = '\0';
	unsigned                     limited            = 0;
	const arch_register_class_t *cls                = NULL;
	int                          memory_possible       = 0;
	int                          all_registers_allowed = 0;
	int                          p;
	int                          same_as = -1;

	memset(constraint, 0, sizeof(constraint[0]));
	constraint->same_as = -1;

	if(*c == 0) {
		/* a memory constraint: no need to do anything in backend about it
		 * (the dependencies are already respected by the memory edge of
		 * the node) */
		return;
	}

	/* TODO: improve error messages with node and source info. (As users can
	 * easily hit these) */
	while(*c != 0) {
		switch(*c) {
		case ' ':
		case '\t':
		case '\n':
			break;

		/* Skip out/in-out marker */
		case '=': break;
		case '+': break;

		case '*':
			++c;
			break;
		case '#':
			while(*c != 0 && *c != ',')
				++c;
			break;

		case 'a':
			assert(cls == NULL || cls == &ia32_reg_classes[CLASS_ia32_gp]);
			cls      = &ia32_reg_classes[CLASS_ia32_gp];
			limited |= 1 << REG_EAX;
			break;
		case 'b':
			assert(cls == NULL || cls == &ia32_reg_classes[CLASS_ia32_gp]);
			cls      = &ia32_reg_classes[CLASS_ia32_gp];
			limited |= 1 << REG_EBX;
			break;
		case 'c':
			assert(cls == NULL || cls == &ia32_reg_classes[CLASS_ia32_gp]);
			cls      = &ia32_reg_classes[CLASS_ia32_gp];
			limited |= 1 << REG_ECX;
			break;
		case 'd':
			assert(cls == NULL || cls == &ia32_reg_classes[CLASS_ia32_gp]);
			cls      = &ia32_reg_classes[CLASS_ia32_gp];
			limited |= 1 << REG_EDX;
			break;
		case 'D':
			assert(cls == NULL || cls == &ia32_reg_classes[CLASS_ia32_gp]);
			cls      = &ia32_reg_classes[CLASS_ia32_gp];
			limited |= 1 << REG_EDI;
			break;
		case 'S':
			assert(cls == NULL || cls == &ia32_reg_classes[CLASS_ia32_gp]);
			cls      = &ia32_reg_classes[CLASS_ia32_gp];
			limited |= 1 << REG_ESI;
			break;
		case 'Q':
		case 'q':
			/* q means lower part of the regs only, this makes no
			 * difference to Q for us (we only assign whole registers) */
			assert(cls == NULL || cls == &ia32_reg_classes[CLASS_ia32_gp]);
			cls      = &ia32_reg_classes[CLASS_ia32_gp];
			limited |= 1 << REG_EAX | 1 << REG_EBX | 1 << REG_ECX |
			           1 << REG_EDX;
			break;
		case 'A':
			assert(cls == NULL || cls == &ia32_reg_classes[CLASS_ia32_gp]);
			cls      = &ia32_reg_classes[CLASS_ia32_gp];
			limited |= 1 << REG_EAX | 1 << REG_EDX;
			break;
		case 'l':
			assert(cls == NULL || cls == &ia32_reg_classes[CLASS_ia32_gp]);
			cls      = &ia32_reg_classes[CLASS_ia32_gp];
			limited |= 1 << REG_EAX | 1 << REG_EBX | 1 << REG_ECX |
			           1 << REG_EDX | 1 << REG_ESI | 1 << REG_EDI |
			           1 << REG_EBP;
			break;

		case 'R':
		case 'r':
		case 'p':
			if (cls != NULL && cls != &ia32_reg_classes[CLASS_ia32_gp])
				panic("multiple register classes not supported");
			cls                   = &ia32_reg_classes[CLASS_ia32_gp];
			all_registers_allowed = 1;
			break;

		case 'f':
		case 't':
		case 'u':
			/* TODO: mark values so the x87 simulator knows about t and u */
			if (cls != NULL && cls != &ia32_reg_classes[CLASS_ia32_vfp])
				panic("multiple register classes not supported");
			cls                   = &ia32_reg_classes[CLASS_ia32_vfp];
			all_registers_allowed = 1;
			break;

		case 'Y':
		case 'x':
			if (cls != NULL && cls != &ia32_reg_classes[CLASS_ia32_xmm])
				panic("multiple register classes not supproted");
			cls                   = &ia32_reg_classes[CLASS_ia32_xmm];
			all_registers_allowed = 1;
			break;

		case 'I':
		case 'J':
		case 'K':
		case 'L':
		case 'M':
		case 'N':
		case 'O':
			if (cls != NULL && cls != &ia32_reg_classes[CLASS_ia32_gp])
				panic("multiple register classes not supported");
			if (immediate_type != '\0')
				panic("multiple immediate types not supported");
			cls            = &ia32_reg_classes[CLASS_ia32_gp];
			immediate_type = *c;
			break;
		case 'n':
		case 'i':
			if (cls != NULL && cls != &ia32_reg_classes[CLASS_ia32_gp])
				panic("multiple register classes not supported");
			if (immediate_type != '\0')
				panic("multiple immediate types not supported");
			cls            = &ia32_reg_classes[CLASS_ia32_gp];
			immediate_type = 'i';
			break;

		case 'X':
		case 'g':
			if (cls != NULL && cls != &ia32_reg_classes[CLASS_ia32_gp])
				panic("multiple register classes not supported");
			if (immediate_type != '\0')
				panic("multiple immediate types not supported");
			immediate_type        = 'i';
			cls                   = &ia32_reg_classes[CLASS_ia32_gp];
			all_registers_allowed = 1;
			memory_possible       = 1;
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
			if (is_output)
				panic("can only specify same constraint on input");

			sscanf(c, "%d%n", &same_as, &p);
			if(same_as >= 0) {
				c += p;
				continue;
			}
			break;

		case 'm':
		case 'o':
		case 'V':
			/* memory constraint no need to do anything in backend about it
			 * (the dependencies are already respected by the memory edge of
			 * the node) */
			memory_possible = 1;
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
			panic("unsupported asm constraint '%c' found in (%+F)",
			      *c, current_ir_graph);
			break;
		default:
			panic("unknown asm constraint '%c' found in (%+F)", *c,
			      current_ir_graph);
			break;
		}
		++c;
	}

	if(same_as >= 0) {
		if (cls != NULL)
			panic("same as and register constraint not supported");
		if (immediate_type != '\0')
			panic("same as and immediate constraint not supported");
	}

	if (cls == NULL && same_as < 0) {
		if (!memory_possible)
			panic("no constraint specified for assembler input");
	}

	constraint->same_as               = same_as;
	constraint->cls                   = cls;
	constraint->allowed_registers     = limited;
	constraint->all_registers_allowed = all_registers_allowed;
	constraint->memory_possible       = memory_possible;
	constraint->immediate_type        = immediate_type;
}

ir_node *gen_ASM(ir_node *node)
{
	ir_graph                   *irg       = current_ir_graph;
	ir_node                    *block = NULL;
	ir_node                    *new_block = NULL;
	dbg_info                   *dbgi      = get_irn_dbg_info(node);
	int                         i, arity;
	int                         out_idx;
	ir_node                   **in;
	ir_node                    *new_node;
	int                         out_arity;
	int                         n_out_constraints;
	int                         n_clobbers;
	const arch_register_req_t **out_reg_reqs;
	const arch_register_req_t **in_reg_reqs;
	ia32_asm_reg_t             *register_map;
	unsigned                    reg_map_size = 0;
	struct obstack             *obst;
	const ir_asm_constraint    *in_constraints;
	const ir_asm_constraint    *out_constraints;
	ident                     **clobbers;
	int                         clobbers_flags = 0;
	unsigned                    clobber_bits[N_CLASSES];

	memset(&clobber_bits, 0, sizeof(clobber_bits));

	switch (be_transformer) {
	case TRANSFORMER_DEFAULT:
		block     = get_nodes_block(node);
		new_block = be_transform_node(block);
		break;

#ifdef FIRM_GRGEN_BE
	case TRANSFORMER_PBQP:
	case TRANSFORMER_RAND:
		new_block = get_nodes_block(node);
		break;
#endif

	default:
		panic("invalid transformer");
	}

	/* workaround for lots of buggy code out there as most people think volatile
	 * asm is enough for everything and forget the flags (linux kernel, etc.)
	 */
	if (get_irn_pinned(node) == op_pin_state_pinned) {
		clobbers_flags = 1;
	}

	arity = get_irn_arity(node);
	in    = alloca(arity * sizeof(in[0]));
	memset(in, 0, arity * sizeof(in[0]));

	clobbers   = get_ASM_clobbers(node);
	n_clobbers = 0;
	for (i = 0; i < get_ASM_n_clobbers(node); ++i) {
		const arch_register_req_t *req;
		const char                *c = get_id_str(clobbers[i]);

		if (strcmp(c, "memory") == 0)
			continue;
		if (strcmp(c, "cc") == 0) {
			clobbers_flags = 1;
			continue;
		}

		req = parse_clobber(c);
		clobber_bits[req->cls->index] |= *req->limited;

		n_clobbers++;
	}
	n_out_constraints = get_ASM_n_output_constraints(node);
	out_arity         = n_out_constraints + n_clobbers;

	in_constraints  = get_ASM_input_constraints(node);
	out_constraints = get_ASM_output_constraints(node);

	/* determine size of register_map */
	for (out_idx = 0; out_idx < n_out_constraints; ++out_idx) {
		const ir_asm_constraint *constraint = &out_constraints[out_idx];
		if (constraint->pos > reg_map_size)
			reg_map_size = constraint->pos;
	}
	for (i = 0; i < arity; ++i) {
		const ir_asm_constraint   *constraint = &in_constraints[i];
		if(constraint->pos > reg_map_size)
			reg_map_size = constraint->pos;
	}
	++reg_map_size;

	obst         = get_irg_obstack(irg);
	register_map = NEW_ARR_D(ia32_asm_reg_t, obst, reg_map_size);
	memset(register_map, 0, reg_map_size * sizeof(register_map[0]));

	/* construct output constraints */
	out_reg_reqs = obstack_alloc(obst, out_arity * sizeof(out_reg_reqs[0]));

	for (out_idx = 0; out_idx < n_out_constraints; ++out_idx) {
		const ir_asm_constraint   *constraint = &out_constraints[out_idx];
		const char                *c       = get_id_str(constraint->constraint);
		unsigned                   pos        = constraint->pos;
		constraint_t               parsed_constraint;
		const arch_register_req_t *req;

		parse_asm_constraints(&parsed_constraint, c, 1);
		req = make_register_req(&parsed_constraint, n_out_constraints,
		                        out_reg_reqs, out_idx);
		out_reg_reqs[out_idx] = req;

		register_map[pos].use_input = 0;
		register_map[pos].valid     = 1;
		register_map[pos].memory    = 0;
		register_map[pos].inout_pos = out_idx;
		register_map[pos].mode      = constraint->mode;
	}

	/* inputs + input constraints */
	in_reg_reqs = obstack_alloc(obst, arity * sizeof(in_reg_reqs[0]));
	for (i = 0; i < arity; ++i) {
		ir_node                   *pred         = get_irn_n(node, i);
		const ir_asm_constraint   *constraint   = &in_constraints[i];
		ident                     *constr_id    = constraint->constraint;
		const char                *c            = get_id_str(constr_id);
		unsigned                   pos          = constraint->pos;
		int                        is_memory_op = 0;
		ir_node                   *input        = NULL;
		unsigned                   r_clobber_bits;
		constraint_t               parsed_constraint;
		const arch_register_req_t *req;

		parse_asm_constraints(&parsed_constraint, c, 0);
		if (parsed_constraint.cls != NULL) {
			r_clobber_bits = clobber_bits[parsed_constraint.cls->index];
			if (r_clobber_bits != 0) {
				if (parsed_constraint.all_registers_allowed) {
					parsed_constraint.all_registers_allowed = 0;
					be_abi_set_non_ignore_regs(env_cg->birg->abi,
							parsed_constraint.cls,
							&parsed_constraint.allowed_registers);
				}
				parsed_constraint.allowed_registers &= ~r_clobber_bits;
			}
		}

		req = make_register_req(&parsed_constraint, n_out_constraints,
		                        out_reg_reqs, i);
		in_reg_reqs[i] = req;

		if (parsed_constraint.immediate_type != '\0') {
			char imm_type = parsed_constraint.immediate_type;
			input = try_create_Immediate(pred, imm_type);
		}

		if (input == NULL) {
			ir_node *pred = NULL;
			switch (be_transformer) {
			case TRANSFORMER_DEFAULT:
				pred  = get_irn_n(node, i);
				input = be_transform_node(pred);
				break;

#ifdef FIRM_GRGEN_BE
			case TRANSFORMER_PBQP:
			case TRANSFORMER_RAND:
				input = get_irn_n(node, i);
				break;
#endif

			default: panic("invalid transformer");
			}

			if (parsed_constraint.cls == NULL
					&& parsed_constraint.same_as < 0) {
				is_memory_op = 1;
			} else if(parsed_constraint.memory_possible) {
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

	/* parse clobbers */
	for (i = 0; i < get_ASM_n_clobbers(node); ++i) {
		const char                *c = get_id_str(clobbers[i]);
		const arch_register_req_t *req;

		if (strcmp(c, "memory") == 0 || strcmp(c, "cc") == 0)
			continue;

		req = parse_clobber(c);
		out_reg_reqs[out_idx] = req;
		++out_idx;
	}

	new_node = new_rd_ia32_Asm(dbgi, irg, new_block, arity, in, out_arity,
	                           get_ASM_text(node), register_map);

	if (arity == 0)
		be_dep_on_frame(new_node);

	set_ia32_out_req_all(new_node, out_reg_reqs);
	set_ia32_in_req_all(new_node, in_reg_reqs);

	SET_IA32_ORIG_NODE(new_node, ia32_get_old_node_name(env_cg, node));

	return new_node;
}

ir_node *gen_CopyB(ir_node *node) {
	ir_node  *block    = NULL;
	ir_node  *src      = NULL;
	ir_node  *new_src  = NULL;
	ir_node  *dst      = NULL;
	ir_node  *new_dst  = NULL;
	ir_node  *mem      = NULL;
	ir_node  *new_mem  = NULL;
	ir_node  *res      = NULL;
	ir_graph *irg      = current_ir_graph;
	dbg_info *dbgi     = get_irn_dbg_info(node);
	int      size      = get_type_size_bytes(get_CopyB_type(node));
	int      rem;

	switch (be_transformer) {
		case TRANSFORMER_DEFAULT:
			block    = be_transform_node(get_nodes_block(node));
			src      = get_CopyB_src(node);
			new_src  = be_transform_node(src);
			dst      = get_CopyB_dst(node);
			new_dst  = be_transform_node(dst);
			mem      = get_CopyB_mem(node);
			new_mem  = be_transform_node(mem);
			break;

#ifdef FIRM_GRGEN_BE
		case TRANSFORMER_PBQP:
		case TRANSFORMER_RAND:
			block    = get_nodes_block(node);
			new_src  = get_CopyB_src(node);
			new_dst  = get_CopyB_dst(node);
			new_mem  = get_CopyB_mem(node);
			break;
#endif

		default: panic("invalid transformer");
	}

	/* If we have to copy more than 32 bytes, we use REP MOVSx and */
	/* then we need the size explicitly in ECX.                    */
	if (size >= 32 * 4) {
		rem = size & 0x3; /* size % 4 */
		size >>= 2;

		res = new_rd_ia32_Const(dbgi, irg, block, NULL, 0, size);
		be_dep_on_frame(res);

		res = new_rd_ia32_CopyB(dbgi, irg, block, new_dst, new_src, res, new_mem, rem);
	} else {
		if(size == 0) {
			ir_fprintf(stderr, "Optimization warning copyb %+F with size <4\n",
			           node);
		}
		res = new_rd_ia32_CopyB_i(dbgi, irg, block, new_dst, new_src, new_mem, size);
	}

	SET_IA32_ORIG_NODE(res, ia32_get_old_node_name(env_cg, node));

	return res;
}

ir_node *gen_Proj_tls(ir_node *node) {
	ir_node  *block = NULL;
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbgi  = NULL;
	ir_node  *res   = NULL;

	switch (be_transformer) {
		case TRANSFORMER_DEFAULT:
			block = be_transform_node(get_nodes_block(node));
			break;

#ifdef FIRM_GRGEN_BE
		case TRANSFORMER_PBQP:
		case TRANSFORMER_RAND:
			block = get_nodes_block(node);
			break;
#endif

		default: panic("invalid transformer");
	}

	res   = new_rd_ia32_LdTls(dbgi, irg, block, mode_Iu);

	return res;
}

ir_node *gen_Unknown(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2) {
			return ia32_new_Unknown_xmm(env_cg);
		} else {
			/* Unknown nodes are buggy in x87 simulator, use zero for now... */
			ir_graph *irg   = current_ir_graph;
			dbg_info *dbgi  = get_irn_dbg_info(node);
			ir_node  *block = get_irg_start_block(irg);
			ir_node  *ret   = new_rd_ia32_vfldz(dbgi, irg, block);

			be_dep_on_frame(ret);
			return ret;
		}
	} else if (ia32_mode_needs_gp_reg(mode)) {
		return ia32_new_Unknown_gp(env_cg);
	} else {
		panic("unsupported Unknown-Mode");
	}
	return NULL;
}

const arch_register_req_t *make_register_req(const constraint_t *constraint,
		int n_outs, const arch_register_req_t **out_reqs, int pos)
{
	struct obstack      *obst    = get_irg_obstack(current_ir_graph);
	int                  same_as = constraint->same_as;
	arch_register_req_t *req;

	if (same_as >= 0) {
		const arch_register_req_t *other_constr;

		if (same_as >= n_outs)
			panic("invalid output number in same_as constraint");

		other_constr     = out_reqs[same_as];

		req              = obstack_alloc(obst, sizeof(req[0]));
		*req             = *other_constr;
		req->type       |= arch_register_req_type_should_be_same;
		req->other_same  = 1U << pos;

		/* switch constraints. This is because in firm we have same_as
		 * constraints on the output constraints while in the gcc asm syntax
		 * they are specified on the input constraints */
		out_reqs[same_as] = req;
		return other_constr;
	}

	/* pure memory ops */
	if (constraint->cls == NULL) {
		return &no_register_req;
	}

	if (constraint->allowed_registers != 0
			&& !constraint->all_registers_allowed) {
		unsigned *limited_ptr;

		req         = obstack_alloc(obst, sizeof(req[0]) + sizeof(unsigned));
		memset(req, 0, sizeof(req[0]));
		limited_ptr = (unsigned*) (req+1);

		req->type    = arch_register_req_type_limited;
		*limited_ptr = constraint->allowed_registers;
		req->limited = limited_ptr;
	} else {
		req       = obstack_alloc(obst, sizeof(req[0]));
		memset(req, 0, sizeof(req[0]));
		req->type = arch_register_req_type_normal;
	}
	req->cls = constraint->cls;

	return req;
}

const arch_register_req_t *parse_clobber(const char *clobber)
{
	struct obstack        *obst = get_irg_obstack(current_ir_graph);
	const arch_register_t *reg  = ia32_get_clobber_register(clobber);
	arch_register_req_t   *req;
	unsigned              *limited;

	if(reg == NULL) {
		panic("Register '%s' mentioned in asm clobber is unknown", clobber);
	}

	assert(reg->index < 32);

	limited  = obstack_alloc(obst, sizeof(limited[0]));
	*limited = 1 << reg->index;

	req          = obstack_alloc(obst, sizeof(req[0]));
	memset(req, 0, sizeof(req[0]));
	req->type    = arch_register_req_type_limited;
	req->cls     = arch_register_get_class(reg);
	req->limited = limited;

	return req;
}


int prevents_AM(ir_node *const block, ir_node *const am_candidate,
                       ir_node *const other)
{
	if (get_nodes_block(other) != block)
		return 0;

	if (is_Sync(other)) {
		int i;

		for (i = get_Sync_n_preds(other) - 1; i >= 0; --i) {
			ir_node *const pred = get_Sync_pred(other, i);

			if (get_nodes_block(pred) != block)
				continue;

			/* Do not block ourselves from getting eaten */
			if (is_Proj(pred) && get_Proj_pred(pred) == am_candidate)
				continue;

			if (!heights_reachable_in_block(heights, pred, am_candidate))
				continue;

			return 1;
		}

		return 0;
	} else {
		/* Do not block ourselves from getting eaten */
		if (is_Proj(other) && get_Proj_pred(other) == am_candidate)
			return 0;

		if (!heights_reachable_in_block(heights, other, am_candidate))
			return 0;

		return 1;
	}
}

ir_node *try_create_Immediate(ir_node *node, char immediate_constraint_type)
{
	int          minus         = 0;
	tarval      *offset        = NULL;
	int          offset_sign   = 0;
	long         val = 0;
	ir_entity   *symconst_ent  = NULL;
	int          symconst_sign = 0;
	ir_mode     *mode;
	ir_node     *cnst          = NULL;
	ir_node     *symconst      = NULL;
	ir_node     *new_node;

	mode = get_irn_mode(node);
	if(!mode_is_int(mode) && !mode_is_reference(mode)) {
		return NULL;
	}

	if(is_Minus(node)) {
		minus = 1;
		node  = get_Minus_op(node);
	}

	if(is_Const(node)) {
		cnst        = node;
		symconst    = NULL;
		offset_sign = minus;
	} else if(is_SymConst(node)) {
		cnst          = NULL;
		symconst      = node;
		symconst_sign = minus;
	} else if(is_Add(node)) {
		ir_node *left  = get_Add_left(node);
		ir_node *right = get_Add_right(node);
		if(is_Const(left) && is_SymConst(right)) {
			cnst          = left;
			symconst      = right;
			symconst_sign = minus;
			offset_sign   = minus;
		} else if(is_SymConst(left) && is_Const(right)) {
			cnst          = right;
			symconst      = left;
			symconst_sign = minus;
			offset_sign   = minus;
		}
	} else if(is_Sub(node)) {
		ir_node *left  = get_Sub_left(node);
		ir_node *right = get_Sub_right(node);
		if(is_Const(left) && is_SymConst(right)) {
			cnst          = left;
			symconst      = right;
			symconst_sign = !minus;
			offset_sign   = minus;
		} else if(is_SymConst(left) && is_Const(right)) {
			cnst          = right;
			symconst      = left;
			symconst_sign = minus;
			offset_sign   = !minus;
		}
	} else {
		return NULL;
	}

	if(cnst != NULL) {
		offset = get_Const_tarval(cnst);
		if(tarval_is_long(offset)) {
			val = get_tarval_long(offset);
		} else {
			ir_fprintf(stderr, "Optimisation Warning: tarval from %+F is not a "
			           "long?\n", cnst);
			return NULL;
		}

		if(!check_immediate_constraint(val, immediate_constraint_type))
			return NULL;
	}
	if(symconst != NULL) {
		if(immediate_constraint_type != 0) {
			/* we need full 32bits for symconsts */
			return NULL;
		}

		/* unfortunately the assembler/linker doesn't support -symconst */
		if(symconst_sign)
			return NULL;

		if(get_SymConst_kind(symconst) != symconst_addr_ent)
			return NULL;
		symconst_ent = get_SymConst_entity(symconst);
	}
	if(cnst == NULL && symconst == NULL)
		return NULL;

	if(offset_sign && offset != NULL) {
		offset = tarval_neg(offset);
	}

	new_node = create_Immediate(symconst_ent, symconst_sign, val);

	return new_node;
}
