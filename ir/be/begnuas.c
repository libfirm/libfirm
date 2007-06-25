/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       Dumps global variables and constants as gas assembler.
 * @author      Christian Wuerdig, Matthias Braun
 * @date        04.11.2005
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "begnuas.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "obst.h"
#include "tv.h"
#include "irnode.h"
#include "irprog.h"
#include "pdeq.h"
#include "error.h"

#include "be_t.h"
#include "beemitter.h"
#include "be_dbgout.h"

typedef struct obstack obstack_t;

/** by default, we generate assembler code for the Linux gas */
be_gas_flavour_t be_gas_flavour = GAS_FLAVOUR_NORMAL;

/**
 * Return the pseudo-instruction to be issued for a section switch
 * depending on the current flavour.
 *
 * @param section  the section to switch to
 *
 * @return  the pseudo-instruction
 */
static const char *get_section_name(be_gas_section_t section) {
	static const char *text[GAS_FLAVOUR_MAX][GAS_SECTION_MAX] = {
		{
			".section\t.text",
			".section\t.data",
			".section\t.rodata",
			".section\t.bss",
			".section\t.tbss,\"awT\",@nobits",
			".section\t.ctors,\"aw\",@progbits"
		},
		{
			".section\t.text",
			".section\t.data",
			".section .rdata,\"dr\"",
			".section\t.bss",
			".section\t.tbss,\"awT\",@nobits",
			".section\t.ctors,\"aw\",@progbits"
		}
	};

	assert((int) be_gas_flavour >= 0 && be_gas_flavour < GAS_FLAVOUR_MAX);
	assert((int) section >= 0 && section < GAS_SECTION_MAX);
	return text[be_gas_flavour][section];
}

/**
 * Emit necessary code to switch to a section.
 *
 * @param env      the emitter environment
 * @param section  the section to switch to
 */
void be_gas_emit_switch_section(be_emit_env_t *env, be_gas_section_t section) {
	be_emit_char(env, '\t');
	be_emit_string(env, get_section_name(section));
	be_emit_char(env, '\n');
	be_emit_write_line(env);
}

/**
 * An environment containing all needed dumper data.
 * Currently we create the file completely in memory first, then
 * write it to the disk. This is an artifact from the old C-generating backend
 * and even there NOT needed. So we might change it in the future.
 */
typedef struct _be_gas_decl_env {
	obstack_t *rodata_obst;        /**< An obstack that will be filled with all rodata entities. */
	obstack_t *data_obst;          /**< An obstack that will be filled with the initialized entities. */
	obstack_t *bss_obst;           /**< An obstack that will be filled with the uninitialized entities. */
	obstack_t *ctor_obst;          /**< An obstack that will be filled with the constructor entities. */
	const be_main_env_t *main_env; /**< The main backend environment, used for it's debug handle. */
	waitq     *worklist;           /**< A worklist we use to place not yet handled entities on. */
} be_gas_decl_env_t;

/************************************************************************/

/**
 * Output a tarval.
 *
 * @param obst   the obstack where the data is written too
 * @param tv     the tarval
 * @param bytes  the width of the tarvals value in bytes
 */
static void dump_arith_tarval(obstack_t *obst, tarval *tv, int bytes)
{
	switch (bytes) {

	case 1:
		obstack_printf(obst, "0x%02x", get_tarval_sub_bits(tv, 0));
		break;

	case 2:
		obstack_printf(obst, "0x%02x%02x", get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		break;

	case 4:
		obstack_printf(obst, "0x%02x%02x%02x%02x",
			get_tarval_sub_bits(tv, 3), get_tarval_sub_bits(tv, 2), get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		break;

	case 8:
		obstack_printf(obst, "0x%02x%02x%02x%02x%02x%02x%02x%02x",
			get_tarval_sub_bits(tv, 7), get_tarval_sub_bits(tv, 6), get_tarval_sub_bits(tv, 5), get_tarval_sub_bits(tv, 4),
			get_tarval_sub_bits(tv, 3), get_tarval_sub_bits(tv, 2), get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		break;

	case 10:
	case 12:
		break;

	case 16:
		obstack_printf(obst, "0x%02x%02x%02x%02x%02x%02x%02x%02x"
				               "%02x%02x%02x%02x%02x%02x%02x%02x",
			get_tarval_sub_bits(tv, 15), get_tarval_sub_bits(tv, 16),
			get_tarval_sub_bits(tv, 13), get_tarval_sub_bits(tv, 12),
			get_tarval_sub_bits(tv, 11), get_tarval_sub_bits(tv, 10),
			get_tarval_sub_bits(tv, 9), get_tarval_sub_bits(tv, 8),
			get_tarval_sub_bits(tv, 7), get_tarval_sub_bits(tv, 6),
			get_tarval_sub_bits(tv, 5), get_tarval_sub_bits(tv, 4),
			get_tarval_sub_bits(tv, 3), get_tarval_sub_bits(tv, 2),
			get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		break;


	default:
		fprintf(stderr, "Try to dump an tarval with %d bytes\n", bytes);
		assert(0);
	}
}

/**
 * Return the tarval of an atomic initializer.
 *
 * @param init  a node representing the initializer (on teh const code irg)
 *
 * @return the tarval
 */
static tarval *get_atomic_init_tv(ir_node *init)
{
	for (;;) {
		ir_mode *mode = get_irn_mode(init);

		switch (get_irn_opcode(init)) {

		case iro_Cast:
			init = get_Cast_op(init);
			continue;

		case iro_Conv:
			init = get_Conv_op(init);
			continue;

		case iro_Const:
			return get_Const_tarval(init);

		case iro_SymConst:
			switch (get_SymConst_kind(init)) {
			case symconst_type_size:
				return new_tarval_from_long(get_type_size_bytes(get_SymConst_type(init)), mode);

			case symconst_type_align:
				return new_tarval_from_long(get_type_alignment_bytes(get_SymConst_type(init)), mode);

			case symconst_ofs_ent:
				return new_tarval_from_long(get_entity_offset(get_SymConst_entity(init)), mode);

			case symconst_enum_const:
				return get_enumeration_value(get_SymConst_enum(init));

			default:
				return NULL;
			}

		default:
			return NULL;
		}
	}
}

/**
 * Dump an atomic value.
 *
 * @param env   the gas output environment
 * @param obst  an obstack the output is written to
 * @param init  a node representing the atomic value (on the const code irg)
 */
static void do_dump_atomic_init(be_gas_decl_env_t *env, obstack_t *obst,
                                ir_node *init)
{
	ir_mode *mode = get_irn_mode(init);
	int bytes     = get_mode_size_bytes(mode);
	tarval *tv;
	ir_entity *ent;

	switch (get_irn_opcode(init)) {

	case iro_Cast:
		do_dump_atomic_init(env, obst, get_Cast_op(init));
		return;

	case iro_Conv:
		do_dump_atomic_init(env, obst, get_Conv_op(init));
		return;

	case iro_Const:
		tv = get_Const_tarval(init);

		/* it's a arithmetic value */
		dump_arith_tarval(obst, tv, bytes);
		return;

	case iro_SymConst:
		switch (get_SymConst_kind(init)) {
		case symconst_addr_name:
			obstack_printf(obst, "%s", get_id_str(get_SymConst_name(init)));
			break;

		case symconst_addr_ent:
			ent = get_SymConst_entity(init);
			if(!is_entity_backend_marked(ent)) {
				waitq_put(env->worklist, ent);
				set_entity_backend_marked(ent, 1);
			}
			obstack_printf(obst, "%s", get_entity_ld_name(ent));
			break;

		case symconst_ofs_ent:
			ent = get_SymConst_entity(init);
#if 0       /* not needed, is it? */
			if(!is_entity_backend_marked(ent)) {
				waitq_put(env->worklist, ent);
				set_entity_backend_marked(ent, 1);
			}
#endif
			obstack_printf(obst, "%d", get_entity_offset(ent));
			break;

		case symconst_type_size:
			obstack_printf(obst, "%d", get_type_size_bytes(get_SymConst_type(init)));
			break;

		case symconst_type_align:
			obstack_printf(obst, "%d", get_type_alignment_bytes(get_SymConst_type(init)));
			break;

		case symconst_enum_const:
			tv = get_enumeration_value(get_SymConst_enum(init));
			dump_arith_tarval(obst, tv, bytes);
			break;

		default:
			assert(!"dump_atomic_init(): don't know how to init from this SymConst");
		}
		return;

		case iro_Add:
			do_dump_atomic_init(env, obst, get_Add_left(init));
			obstack_printf(obst, " + ");
			do_dump_atomic_init(env, obst, get_Add_right(init));
			return;

		case iro_Sub:
			do_dump_atomic_init(env, obst, get_Sub_left(init));
			obstack_printf(obst, " - ");
			do_dump_atomic_init(env, obst, get_Sub_right(init));
			return;

		case iro_Mul:
			do_dump_atomic_init(env, obst, get_Mul_left(init));
			obstack_printf(obst, " * ");
			do_dump_atomic_init(env, obst, get_Mul_right(init));
			return;

		default:
			assert(0 && "dump_atomic_init(): unknown IR-node");
	}
}

/**
 * Dumps the type for given size (.byte, .long, ...)
 *
 * @param obst  an obstack the output is written to
 * @param size  the size in bytes
 */
static void dump_size_type(obstack_t *obst, int size) {
	switch (size) {

	case 1:
		obstack_printf(obst, "\t.byte\t");
		break;

	case 2:
		obstack_printf(obst, "\t.value\t");
		break;

	case 4:
		obstack_printf(obst, "\t.long\t");
		break;

	case 8:
		obstack_printf(obst, "\t.quad\t");
		break;

	case 10:
	case 12:
		/* handled in arith */
		break;

	case 16:
		obstack_printf(obst, "\t.octa\t");
		break;

	default:
		fprintf(stderr, "Try to dump a type with %d bytes\n", size);
		assert(0);
	}
}

/**
 * Dump an atomic value to an obstack.
 *
 * @param env   the gas output environment
 * @param obst  an obstack the output is written to
 * @param init  a node representing the atomic value (on the const code irg)
 */
static void dump_atomic_init(be_gas_decl_env_t *env, obstack_t *obst,
                             ir_node *init)
{
	ir_mode *mode = get_irn_mode(init);
	int bytes     = get_mode_size_bytes(mode);

	dump_size_type(obst, bytes);
	do_dump_atomic_init(env, obst, init);
	obstack_printf(obst, "\n");
}

/************************************************************************/
/* Routines to dump global variables                                    */
/************************************************************************/

/**
 * Determine if an entity is a string constant
 * @param ent The entity
 * @return 1 if it is a string constant, 0 otherwise
 */
static int ent_is_string_const(ir_entity *ent)
{
	ir_type *type, *element_type;
	ir_mode *mode;
	int i, c, n;

	type = get_entity_type(ent);

	/* if it's an array */
	if (!is_Array_type(type))
		return 0;

	element_type = get_array_element_type(type);

	/* and the array's element type is primitive */
	if (!is_Primitive_type(element_type))
		return 0;

	/* and the mode of the element type is an int of
	 * the same size as the byte mode */
	mode = get_type_mode(element_type);
	if (!mode_is_int(mode)
		|| get_mode_size_bits(mode) != get_mode_size_bits(mode_Bs))
		return 0;

	/* if it contains only printable chars and a 0 at the end */
	n = get_compound_ent_n_values(ent);
	for (i = 0; i < n; ++i) {
		ir_node *irn = get_compound_ent_value(ent, i);
		if(get_irn_opcode(irn) != iro_Const)
			return 0;

		c = (int) get_tarval_long(get_Const_tarval(irn));

		if((i < n - 1 && !(isgraph(c) || isspace(c)))
				|| (i == n - 1 && c != '\0'))
			return 0;
	}

	/* then we can emit it as a string constant */
	return 1;
}

/**
 * Dump a string constant.
 * No checks are made!!
 *
 * @param obst The obst to dump on.
 * @param ent  The entity to dump.
 */
static void dump_string_cst(obstack_t *obst, ir_entity *ent)
{
	int      i, n;
	ir_type *type;
	int      type_size;
	int      remaining_space;

	obstack_printf(obst, "\t.string \"");
	n = get_compound_ent_n_values(ent);

	for (i = 0; i < n-1; ++i) {
		ir_node *irn;
		int c;

		irn = get_compound_ent_value(ent, i);
		c = (int) get_tarval_long(get_Const_tarval(irn));

		switch (c) {
		case '"' : obstack_printf(obst, "\\\""); break;
		case '\n': obstack_printf(obst, "\\n"); break;
		case '\r': obstack_printf(obst, "\\r"); break;
		case '\t': obstack_printf(obst, "\\t"); break;
		case '\\': obstack_printf(obst, "\\\\"); break;
		default  :
			if (isprint(c))
				obstack_printf(obst, "%c", c);
			else
				obstack_printf(obst, "\\%o", c);
			break;
		}
	}
	obstack_printf(obst, "\"\n");

	type            = get_entity_type(ent);
	type_size       = get_type_size_bytes(type);
	remaining_space = type_size - n;
	assert(remaining_space >= 0);
	if(remaining_space > 0) {
		obstack_printf(obst, "\t.skip\t%d\n", remaining_space);
	}
}

enum normal_or_bitfield_kind {
	NORMAL = 0,
	BITFIELD
};

typedef struct {
	enum normal_or_bitfield_kind kind;
	union {
		ir_node *value;
		unsigned char bf_val;
	} v;
} normal_or_bitfield;

/**
 * Dump an initializer for a compound entity.
 */
static void dump_compound_init(be_gas_decl_env_t *env, obstack_t *obst,
                               ir_entity *ent)
{
	normal_or_bitfield *vals;
	int i, j, n = get_compound_ent_n_values(ent);
	int last_ofs;

	/* Find the initializer size. Sorrily gcc support a nasty feature:
	   The last field of a compound may be a flexible array. This allows
	   initializers bigger than the type size. */
	last_ofs = get_type_size_bytes(get_entity_type(ent));
	for (i = 0; i < n; ++i) {
		int offset = get_compound_ent_value_offset_bytes(ent, i);
		int bits_remainder = get_compound_ent_value_offset_bit_remainder(ent, i);
		const compound_graph_path *path = get_compound_ent_value_path(ent, i);
		int path_len = get_compound_graph_path_length(path);
		ir_entity *last_ent = get_compound_graph_path_node(path, path_len - 1);
		int value_len = get_type_size_bits(get_entity_type(last_ent));

		offset += (value_len + bits_remainder + 7) >> 3;

		if (offset > last_ofs) {
			last_ofs = offset;
		}
	}

	/*
	 * In the worst case, every initializer allocates one byte.
	 * Moreover, initializer might be big, do not allocate on stack.
	 */
	vals = xcalloc(last_ofs, sizeof(vals[0]));

	/* collect the values and store them at the offsets */
	for (i = 0; i < n; ++i) {
		const compound_graph_path *path = get_compound_ent_value_path(ent, i);
		int path_len = get_compound_graph_path_length(path);
		int offset = get_compound_ent_value_offset_bytes(ent, i);
		int offset_bits = get_compound_ent_value_offset_bit_remainder(ent, i);
		ir_node *value = get_compound_ent_value(ent, i);
		ir_entity *last_ent = get_compound_graph_path_node(path, path_len - 1);
		int value_len = get_type_size_bits(get_entity_type(last_ent));
		assert(offset >= 0);
		assert(offset_bits >= 0);

		if (offset_bits != 0 ||
			(value_len != 8 && value_len != 16 && value_len != 32 && value_len != 64)) {
			tarval *shift, *shifted;
			tarval *tv = get_atomic_init_tv(value);
			if (tv == NULL) {
				panic("Couldn't get numeric value for bitfield initializer '%s'\n",
				      get_entity_ld_name(ent));
			}
			tv = tarval_convert_to(tv, mode_Lu);
			shift = new_tarval_from_long(offset_bits, mode_Is);
			shifted = tarval_shl(tv, shift);
			if (shifted == tarval_bad || shifted == tarval_undefined) {
				panic("Couldn't shift numeric value for bitfield initializer '%s'\n",
				      get_entity_ld_name(ent));
			}

			for (j = 0; value_len > 0; ++j) {
				assert(offset + j < last_ofs);
				assert(vals[offset + j].kind == BITFIELD || vals[offset + j].v.value == NULL);
				vals[offset + j].kind = BITFIELD;
				vals[offset + j].v.bf_val |= get_tarval_sub_bits(shifted, j);
				value_len -= 8 - offset_bits;
				offset_bits = 0;
			}
		} else {
			assert(offset < last_ofs);
			assert(vals[offset].kind == NORMAL);
			assert(vals[offset].v.value == NULL);
			vals[offset].v.value = value;
		}
	}

	/* now write them sorted */
	for (i = 0; i < last_ofs; ) {
		int space = 0, skip = 0;
		if (vals[i].kind == NORMAL) {
			if(vals[i].v.value != NULL) {
				dump_atomic_init(env, obst, vals[i].v.value);
				skip = get_mode_size_bytes(get_irn_mode(vals[i].v.value)) - 1;
	 		} else {
	 			space = 1;
	 		}
		} else {
			assert(vals[i].kind == BITFIELD);
			obstack_printf(obst, "\t.byte\t%d\n", vals[i].v.bf_val);
		}

		++i;
		space = 0;
		while (i < last_ofs && vals[i].kind == NORMAL && vals[i].v.value == NULL) {
			++space;
			++i;
		}
		space -= skip;
		assert(space >= 0);

		/* a gap */
		if (space > 0)
			obstack_printf(obst, "\t.skip\t%d\n", space);
	}
	xfree(vals);
}

/**
 * Dump a global entity.
 *
 * @param env           the gas output environment
 * @param ent           the entity to be dumped
 * @param emit_commons  if non-zero, emit commons (non-local uninitialized entities)
 */
static void dump_global(be_gas_decl_env_t *env, ir_entity *ent, int emit_commons)
{
	obstack_t *obst;
	ir_type *type = get_entity_type(ent);
	const char *ld_name = get_entity_ld_name(ent);
	int align = get_type_alignment_bytes(type);
	int emit_as_common = 0;
	ir_variability variability;
	ir_visibility visibility;

	obst = env->data_obst;
	if (is_Method_type(type)) {
		if (get_method_img_section(ent) == section_constructors) {
			obst = env->ctor_obst;
			obstack_printf(obst, ".balign\t%d\n", align);
			dump_size_type(obst, align);
			obstack_printf(obst, "%s\n", ld_name);
		}
		return;
	}

	variability = get_entity_variability(ent);
	visibility = get_entity_visibility(ent);
	if (variability == variability_constant) {
		/* a constant entity, put it on the rdata */
		obst = env->rodata_obst;
	} else if (variability == variability_uninitialized) {
		/* uninitialized entity put it in bss segment */
		obst = env->bss_obst;
		if (emit_commons && visibility != visibility_local)
			emit_as_common = 1;
	}

	be_dbg_variable(env->main_env->db_handle, obst, ent);

	/* global or not global */
	if (visibility == visibility_external_visible && !emit_as_common) {
		obstack_printf(obst, ".global\t%s\n", ld_name);
	} else if(visibility == visibility_external_allocated) {
		obstack_printf(obst, ".global\t%s\n", ld_name);
		/* we can return now... */
		return;
	}
	/* alignment */
	if (align > 1 && !emit_as_common) {
		obstack_printf(obst, ".balign\t%d\n", align);
	}

	if (!emit_as_common) {
		obstack_printf(obst, "%s:\n", ld_name);
	}

	if (variability == variability_uninitialized) {
		if(emit_as_common) {
			if (be_gas_flavour == GAS_FLAVOUR_NORMAL)
				obstack_printf(obst, "\t.comm %s,%d,%d\n",
					ld_name, get_type_size_bytes(type), align);
			else
				obstack_printf(obst, "\t.comm %s,%d # %d\n",
					ld_name, get_type_size_bytes(type), align);
		} else {
			obstack_printf(obst, "\t.zero %d\n", get_type_size_bytes(type));
		}
	} else {
		if (is_atomic_entity(ent)) {
			dump_atomic_init(env, obst, get_atomic_ent_value(ent));
		} else {
			/* sort_compound_ent_values(ent); */

			switch (get_type_tpop_code(get_entity_type(ent))) {
			case tpo_array:
				if (ent_is_string_const(ent))
					dump_string_cst(obst, ent);
				else
					dump_compound_init(env, obst, ent);
				break;
			case tpo_struct:
			case tpo_class:
				dump_compound_init(env, obst, ent);
				break;
			default:
				assert(0);
			}
		}
	}
}

/**
 * Dumps declarations of global variables and the initialization code.
 *
 * @param gt                a global like type, either the global or the TLS one
 * @param env               an environment
 * @param emit_commons      if non-zero, emit commons (non-local uninitialized entities)
 * @param only_emit_marked  if non-zero, external allocated entities that do not have
 *                          its visited flag set are ignored
 */
static void be_gas_dump_globals(ir_type *gt, be_gas_decl_env_t *env,
                              int emit_commons, int only_emit_marked)
{
	int i, n = get_compound_n_members(gt);
	waitq *worklist = new_waitq();

	if (only_emit_marked) {
		for (i = 0; i < n; i++) {
			ir_entity *ent = get_compound_member(gt, i);
			if (is_entity_backend_marked(ent) ||
			    get_entity_visibility(ent) != visibility_external_allocated) {
				waitq_put(worklist, ent);
				set_entity_backend_marked(ent, 1);
			}
		}
	} else {
		for (i = 0; i < n; i++) {
			ir_entity *ent = get_compound_member(gt, i);
			set_entity_backend_marked(ent, 1);
			waitq_put(worklist, ent);
		}
	}

	env->worklist = worklist;

	while (!waitq_empty(worklist)) {
		ir_entity *ent = waitq_get(worklist);

		dump_global(env, ent, emit_commons);
	}

	del_waitq(worklist);
	env->worklist = NULL;
}

/************************************************************************/

/* Generate all entities. */
void be_gas_emit_decls(be_emit_env_t *emit, const be_main_env_t *main_env,
                       int only_emit_marked_entities)
{
	be_gas_decl_env_t env;
	obstack_t rodata, data, bss, ctor;
	int    size;
	char   *cp;

	/* dump the global type */
	obstack_init(&rodata);
	obstack_init(&data);
	obstack_init(&bss);
	obstack_init(&ctor);

	env.rodata_obst = &rodata;
	env.data_obst   = &data;
	env.bss_obst    = &bss;
	env.ctor_obst   = &ctor;
	env.main_env    = main_env;

	be_gas_dump_globals(get_glob_type(), &env, 1, only_emit_marked_entities);

	size = obstack_object_size(&data);
	cp   = obstack_finish(&data);
	if (size > 0) {
		be_gas_emit_switch_section(emit, GAS_SECTION_DATA);
		be_emit_string_len(emit, cp, size);
		be_emit_write_line(emit);
	}

	size = obstack_object_size(&rodata);
	cp   = obstack_finish(&rodata);
	if (size > 0) {
		be_gas_emit_switch_section(emit, GAS_SECTION_RODATA);
		be_emit_string_len(emit, cp, size);
		be_emit_write_line(emit);
	}

	size = obstack_object_size(&bss);
	cp   = obstack_finish(&bss);
	if (size > 0) {
		be_gas_emit_switch_section(emit, GAS_SECTION_COMMON);
		be_emit_string_len(emit, cp, size);
		be_emit_write_line(emit);
	}

	size = obstack_object_size(&ctor);
	cp   = obstack_finish(&ctor);
	if (size > 0) {
		be_gas_emit_switch_section(emit, GAS_SECTION_CTOR);
		be_emit_string_len(emit, cp, size);
		be_emit_write_line(emit);
	}

	obstack_free(&rodata, NULL);
	obstack_free(&data, NULL);
	obstack_free(&bss, NULL);
	obstack_free(&ctor, NULL);

	/* dump the Thread Local Storage */
	obstack_init(&data);

	env.rodata_obst = &data;
	env.data_obst   = &data;
	env.bss_obst    = &data;
	env.ctor_obst   = NULL;

	be_gas_dump_globals(get_tls_type(), &env, 0, only_emit_marked_entities);

	size = obstack_object_size(&data);
	cp   = obstack_finish(&data);
	if (size > 0) {
		be_gas_emit_switch_section(emit, GAS_SECTION_TLS);
		be_emit_cstring(emit, ".balign\t32\n");
		be_emit_write_line(emit);
		be_emit_string_len(emit, cp, size);
		be_emit_write_line(emit);
	}

	obstack_free(&data, NULL);
}
