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
#include "entity_t.h"
#include "error.h"

#include "be_t.h"
#include "beemitter.h"
#include "be_dbgout.h"

typedef struct obstack obstack_t;

/** by default, we generate assembler code for the Linux gas */
be_gas_flavour_t be_gas_flavour = GAS_FLAVOUR_ELF;

/**
 * Return the pseudo-instruction to be issued for a section switch
 * depending on the current flavour.
 *
 * @param section  the section to switch to
 *
 * @return  the pseudo-instruction
 */
static const char *get_section_name(be_gas_section_t section) {
	static const char *text[GAS_FLAVOUR_LAST+1][GAS_SECTION_LAST+1] = {
		{ /* GAS_FLAVOUR_ELF */
			".section\t.text",
			".section\t.data",
			".section\t.rodata",
			".section\t.bss",
			".section\t.tbss,\"awT\",@nobits",
			".section\t.ctors,\"aw\",@progbits"
		},
		{ /* GAS_FLAVOUR_MINGW */
			".section\t.text",
			".section\t.data",
			".section .rdata,\"dr\"",
			".section\t.bss",
			".section\t.tbss,\"awT\",@nobits",
			".section\t.ctors,\"aw\",@progbits"
		},
		{ /* GAS_FLAVOUR_YASM */
			".section\t.text",
			".section\t.data",
			".section\t.rodata",
			".section\t.bss",
			".section\t.tbss,\"awT\",@nobits",
			".section\t.ctors,\"aw\",@progbits"
		},
		{ /* GAS_FLAVOUR_MACH_O */
			".text",
			".data",
			".const",
			".data",
			"", /* TLS is not supported on Mach-O */
			""  /* constructors aren't marked with sections in Mach-O */
		}
	};

	assert((int) be_gas_flavour >= 0 && be_gas_flavour <= GAS_FLAVOUR_LAST);
	assert((int) section >= 0 && section <= GAS_SECTION_LAST);
	return text[be_gas_flavour][section];
}

void be_gas_emit_switch_section(be_gas_section_t section) {
	be_emit_char('\t');
	be_emit_string(get_section_name(section));
	be_emit_char('\n');
	be_emit_write_line();
}

void be_gas_emit_function_prolog(ir_entity *entity, unsigned alignment)
{
	const char *name = get_entity_ld_name(entity);

	be_gas_emit_switch_section(GAS_SECTION_TEXT);

	/* write the begin line (used by scripts processing the assembler... */
	be_emit_write_line();
	be_emit_cstring("# -- Begin  ");
	be_emit_string(name);
	be_emit_char('\n');
	be_emit_write_line();

	if (be_gas_flavour != GAS_FLAVOUR_MACH_O) {
		unsigned maximum_skip = (1 << alignment) - 1;
		be_emit_cstring("\t.p2align ");
		be_emit_irprintf("%u,,%u\n", alignment, maximum_skip);
		be_emit_write_line();
	}

	if (get_entity_visibility(entity) == visibility_external_visible) {
		be_emit_cstring(".global ");
		be_emit_string(name);
		be_emit_char('\n');
		be_emit_write_line();
	}

	switch (be_gas_flavour) {
	case GAS_FLAVOUR_ELF:
		be_emit_cstring("\t.type\t");
		be_emit_string(name);
		be_emit_cstring(", @function\n");
		be_emit_write_line();
		break;
	case GAS_FLAVOUR_MINGW:
		be_emit_cstring("\t.def\t");
		be_emit_string(name);
		be_emit_cstring(";\t.scl\t2;\t.type\t32;\t.endef\n");
		be_emit_write_line();
		break;
	case GAS_FLAVOUR_MACH_O:
	case GAS_FLAVOUR_YASM:
		break;
	}
	be_emit_string(name);
	be_emit_cstring(":\n");
	be_emit_write_line();
}

void be_gas_emit_function_epilog(ir_entity *entity)
{
	const char *name = get_entity_ld_name(entity);

	if (be_gas_flavour == GAS_FLAVOUR_ELF) {
		be_emit_cstring("\t.size\t");
		be_emit_string(name);
		be_emit_cstring(", .-");
		be_emit_string(name);
		be_emit_char('\n');
		be_emit_write_line();
	}

	be_emit_cstring("# -- End  ");
	be_emit_string(name);
	be_emit_char('\n');
	be_emit_write_line();
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
 * Return the label prefix for labeled blocks.
 */
const char *be_gas_label_prefix(void) {
	return ".LG";
}

/**
 * Dump a label.
 */
static void dump_label(obstack_t *obst, ir_label_t label) {
	obstack_printf(obst, "%s%ld", be_gas_label_prefix(), label);
}

/**
 * Return the tarval of an atomic initializer.
 *
 * @param init  a node representing the initializer (on the const code irg)
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

			case symconst_label:
				return NULL;

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
	ir_label_t label;
	ir_entity *ent;

	init = skip_Id(init);

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
			obstack_printf(obst, "%u", get_type_size_bytes(get_SymConst_type(init)));
			break;

		case symconst_type_align:
			obstack_printf(obst, "%u", get_type_alignment_bytes(get_SymConst_type(init)));
			break;

		case symconst_enum_const:
			tv = get_enumeration_value(get_SymConst_enum(init));
			dump_arith_tarval(obst, tv, bytes);
			break;

		case symconst_label:
			label = get_SymConst_label(init);
			dump_label(obst, label);
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
static void dump_size_type(obstack_t *obst, size_t size) {
	switch (size) {

	case 1:
		obstack_printf(obst, "\t.byte\t");
		break;

	case 2:
		obstack_printf(obst, "\t.word\t");
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
		fprintf(stderr, "Try to dump a type with %u bytes\n", (unsigned) size);
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

static int initializer_is_string_const(const ir_initializer_t *initializer)
{
	size_t i, len;

	if(initializer->kind != IR_INITIALIZER_COMPOUND)
		return 0;

	len = initializer->compound.n_initializers;
	if (len < 1)
		return 0;
	for(i = 0; i < len; ++i) {
		int               c;
		tarval           *tv;
		ir_mode          *mode;
		ir_initializer_t *sub_initializer
			= initializer->compound.initializers[i];

		if(sub_initializer->kind != IR_INITIALIZER_TARVAL)
			return 0;

		tv   = sub_initializer->tarval.value;
		mode = get_tarval_mode(tv);

		if (!mode_is_int(mode)
				|| get_mode_size_bits(mode) != get_mode_size_bits(mode_Bs))
			return 0;

		c = get_tarval_long(tv);
		if (i < len - 1) {
			if (!isgraph(c) && !isspace(c))
				return 0;
		} else {
			if (c != 0)
				return 0;
		}
	}

	return 1;
}

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

	if(ent->has_initializer) {
		/* TODO */
		return 0;
	} else {
		/* if it contains only printable chars and a 0 at the end */
		n = get_compound_ent_n_values(ent);
		for (i = 0; i < n; ++i) {
			ir_node *irn = get_compound_ent_value(ent, i);
			if (! is_Const(irn))
				return 0;

			c = (int) get_tarval_long(get_Const_tarval(irn));

			if((i < n - 1 && !(isgraph(c) || isspace(c)))
					|| (i == n - 1 && c != '\0'))
				return 0;
		}
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

static void dump_string_initializer(obstack_t *obst,
                                    const ir_initializer_t *initializer)
{
	size_t i, len;

	obstack_printf(obst, "\t.string \"");

	len = initializer->compound.n_initializers;
	for(i = 0; i < len - 1; ++i) {
		const ir_initializer_t *sub_initializer
			= get_initializer_compound_value(initializer, i);

		tarval *tv = get_initializer_tarval_value(sub_initializer);
		int     c  = get_tarval_long(tv);

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
}

enum normal_or_bitfield_kind {
	NORMAL = 0,
	TARVAL,
	BITFIELD
};

typedef struct {
	enum normal_or_bitfield_kind kind;
	union {
		ir_node       *value;
		tarval        *tarval;
		unsigned char  bf_val;
	} v;
} normal_or_bitfield;

static int is_type_variable_size(ir_type *type)
{
	(void) type;
	/* TODO */
	return 0;
}

static size_t get_initializer_size(const ir_initializer_t *initializer,
                                   ir_type *type)
{
	switch(get_initializer_kind(initializer)) {
	case IR_INITIALIZER_TARVAL: {
		assert(get_tarval_mode(get_initializer_tarval_value(initializer)) == get_type_mode(type));
		return get_type_size_bytes(type);
	}
	case IR_INITIALIZER_CONST:
	case IR_INITIALIZER_NULL:
		return get_type_size_bytes(type);
	case IR_INITIALIZER_COMPOUND: {
		if(!is_type_variable_size(type)) {
			return get_type_size_bytes(type);
		} else {
			unsigned n_entries
				= get_initializer_compound_n_entries(initializer);
			unsigned i;
			unsigned initializer_size = get_type_size_bytes(type);
			for(i = 0; i < n_entries; ++i) {
				ir_entity *entity = get_compound_member(type, i);
				ir_type   *type   = get_entity_type(entity);

				const ir_initializer_t *sub_initializer
					= get_initializer_compound_value(initializer, i);

				unsigned offset = get_entity_offset(entity);
				unsigned size   = get_initializer_size(sub_initializer, type);

				if(offset + size > initializer_size) {
					initializer_size = offset + size;
				}
			}
			return initializer_size;
		}
	}
	}

	panic("found invalid initializer");
}

#ifndef NDEBUG
static normal_or_bitfield *glob_vals;
static size_t              max_vals;
#endif

static void dump_bitfield(normal_or_bitfield *vals, size_t offset_bits,
                          const ir_initializer_t *initializer, ir_type *type)
{
	unsigned char  last_bits = 0;
	ir_mode       *mode      = get_type_mode(type);
	tarval        *tv        = NULL;
	unsigned char  curr_bits;
	int            value_len;
	int            j;

	switch(get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL:
		return;
	case IR_INITIALIZER_TARVAL:
		tv = get_initializer_tarval_value(initializer);
		break;
	case IR_INITIALIZER_CONST: {
		ir_node *node = get_initializer_const_value(initializer);
		if(!is_Const(node)) {
			panic("bitfield initializer not a Const node");
		}
		tv = get_Const_tarval(node);
		break;
	}
	case IR_INITIALIZER_COMPOUND:
		panic("bitfield initializer is compound");
	}
	if (tv == NULL) {
		panic("Couldn't get numeric value for bitfield initializer\n");
	}

	/* normalize offset */
	vals        += offset_bits >> 3;
	offset_bits &= 7;
	value_len    = get_mode_size_bits(mode);

	/* combine bits with existing bits */
	for (j = 0; value_len + (int) offset_bits > 0; ++j) {
		assert((size_t) (vals - glob_vals) + j < max_vals);
		assert(vals[j].kind == BITFIELD ||
				(vals[j].kind == NORMAL && vals[j].v.value == NULL));
		vals[j].kind = BITFIELD;
		curr_bits    = get_tarval_sub_bits(tv, j);
		vals[j].v.bf_val
			|= (last_bits >> (8 - offset_bits)) | (curr_bits << offset_bits);
		value_len -= 8;
		last_bits = curr_bits;
	}
}

static void dump_ir_initializer(normal_or_bitfield *vals,
                                const ir_initializer_t *initializer,
                                ir_type *type)
{
	assert((size_t) (vals - glob_vals) < max_vals);

	switch(get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL:
		return;
	case IR_INITIALIZER_TARVAL: {
		size_t i;

		assert(vals->kind != BITFIELD);
		vals->kind     = TARVAL;
		vals->v.tarval = get_initializer_tarval_value(initializer);
		assert(get_type_mode(type) == get_tarval_mode(vals->v.tarval));
		for(i = 1; i < get_type_size_bytes(type); ++i) {
			vals[i].kind    = NORMAL;
			vals[i].v.value = NULL;
		}
		return;
	}
	case IR_INITIALIZER_CONST: {
		size_t i;

		assert(vals->kind != BITFIELD);
		vals->kind    = NORMAL;
		vals->v.value = get_initializer_const_value(initializer);
		for(i = 1; i < get_type_size_bytes(type); ++i) {
			vals[i].kind    = NORMAL;
			vals[i].v.value = NULL;
		}
		return;
	}
	case IR_INITIALIZER_COMPOUND: {
		size_t i = 0;
		size_t n = get_initializer_compound_n_entries(initializer);

		if(is_Array_type(type)) {
			ir_type *element_type = get_array_element_type(type);
			size_t   skip         = get_type_size_bytes(element_type);
			size_t   alignment    = get_type_alignment_bytes(element_type);
			size_t   misalign     = skip % alignment;
			if(misalign != 0) {
				skip += alignment - misalign;
			}

			for(i = 0; i < n; ++i) {
				ir_initializer_t *sub_initializer
					= get_initializer_compound_value(initializer, i);

				dump_ir_initializer(vals, sub_initializer, element_type);

				vals += skip;
			}
		} else {
			size_t n_members, i;
			assert(is_compound_type(type));
			n_members = get_compound_n_members(type);
			for(i = 0; i < n_members; ++i) {
				ir_entity        *member    = get_compound_member(type, i);
				size_t            offset    = get_entity_offset(member);
				ir_type          *subtype   = get_entity_type(member);
				ir_mode          *mode      = get_type_mode(subtype);
				ir_initializer_t *sub_initializer;

				assert(i < get_initializer_compound_n_entries(initializer));
				sub_initializer
					= get_initializer_compound_value(initializer, i);

				if(mode != NULL) {
					size_t offset_bits
						= get_entity_offset_bits_remainder(member);
					size_t value_len   = get_mode_size_bits(mode);

					if(offset_bits != 0 ||
						(value_len != 8 && value_len != 16 && value_len != 32
						 && value_len != 64)) {
						dump_bitfield(&vals[offset], offset_bits,
						              sub_initializer, subtype);
						continue;
					}
				}

				dump_ir_initializer(&vals[offset], sub_initializer, subtype);
			}
		}

		return;
	}
	}
	panic("invalid ir_initializer kind found");
}

static void dump_initializer(be_gas_decl_env_t *env, obstack_t *obst,
                             ir_entity *entity)
{
	const ir_initializer_t *initializer = entity->attr.initializer;
	ir_type                *type;
	normal_or_bitfield     *vals;
	size_t                  size;
	size_t                  k;

	if(initializer_is_string_const(initializer)) {
		dump_string_initializer(obst, initializer);
		return;
	}

	type = get_entity_type(entity);
	size = get_initializer_size(initializer, type);

	if (size == 0)
		return;

	/*
	 * In the worst case, every initializer allocates one byte.
	 * Moreover, initializer might be big, do not allocate on stack.
	 */
	vals = xcalloc(size, sizeof(vals[0]));

#ifndef NDEBUG
	glob_vals = vals;
	max_vals  = size;
#endif

	dump_ir_initializer(vals, initializer, type);

	/* now write values sorted */
	for (k = 0; k < size; ) {
		int space = 0, skip = 0;
		if (vals[k].kind == NORMAL) {
			if(vals[k].v.value != NULL) {
				dump_atomic_init(env, obst, vals[k].v.value);
				skip = get_mode_size_bytes(get_irn_mode(vals[k].v.value)) - 1;
	 		} else {
	 			space = 1;
	 		}
		} else if(vals[k].kind == TARVAL) {
			tarval *tv   = vals[k].v.tarval;
			size_t  size = get_mode_size_bytes(get_tarval_mode(tv));

			assert(tv != NULL);

			skip = size - 1;
			dump_size_type(obst, size);
			dump_arith_tarval(obst, tv, size);
			obstack_1grow(obst, '\n');
		} else {
			assert(vals[k].kind == BITFIELD);
			obstack_printf(obst, "\t.byte\t%d\n", vals[k].v.bf_val);
		}

		++k;
		while (k < size && vals[k].kind == NORMAL && vals[k].v.value == NULL) {
			++space;
			++k;
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
 * Dump an initializer for a compound entity.
 */
static void dump_compound_init(be_gas_decl_env_t *env, obstack_t *obst,
                               ir_entity *ent)
{
	normal_or_bitfield *vals;
	int i, j, n;
	unsigned k, last_ofs;

	if(ent->has_initializer) {
		dump_initializer(env, obst, ent);
		return;
	}

	n = get_compound_ent_n_values(ent);

	/* Find the initializer size. Sorrily gcc support a nasty feature:
	   The last field of a compound may be a flexible array. This allows
	   initializers bigger than the type size. */
	last_ofs = get_type_size_bytes(get_entity_type(ent));
	for (i = 0; i < n; ++i) {
		unsigned offset         = get_compound_ent_value_offset_bytes(ent, i);
		unsigned bits_remainder = get_compound_ent_value_offset_bit_remainder(ent, i);
		ir_node  *value         = get_compound_ent_value(ent, i);
		unsigned value_len      = get_mode_size_bits(get_irn_mode(value));

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
		unsigned offset      = get_compound_ent_value_offset_bytes(ent, i);
		int      offset_bits = get_compound_ent_value_offset_bit_remainder(ent, i);
		ir_node  *value      = get_compound_ent_value(ent, i);
		int      value_len   = get_mode_size_bits(get_irn_mode(value));

		assert(offset_bits >= 0);

		if (offset_bits != 0 ||
			(value_len != 8 && value_len != 16 && value_len != 32 && value_len != 64)) {
			tarval *tv = get_atomic_init_tv(value);
			unsigned char curr_bits, last_bits = 0;
			if (tv == NULL) {
				panic("Couldn't get numeric value for bitfield initializer '%s'\n",
				      get_entity_ld_name(ent));
			}
			/* normalize offset */
			offset += offset_bits >> 3;
			offset_bits &= 7;

			for (j = 0; value_len + offset_bits > 0; ++j) {
				assert(offset + j < last_ofs);
				assert(vals[offset + j].kind == BITFIELD || vals[offset + j].v.value == NULL);
				vals[offset + j].kind = BITFIELD;
				curr_bits = get_tarval_sub_bits(tv, j);
				vals[offset + j].v.bf_val |= (last_bits >> (8 - offset_bits)) | (curr_bits << offset_bits);
				value_len -= 8;
				last_bits = curr_bits;
			}
		} else {
			int i;

			assert(offset < last_ofs);
			assert(vals[offset].kind == NORMAL);
			for (i = 1; i < value_len / 8; ++i) {
				assert(vals[offset + i].v.value == NULL);
			}
			vals[offset].v.value = value;
		}
	}

	/* now write them sorted */
	for (k = 0; k < last_ofs; ) {
		int space = 0, skip = 0;
		if (vals[k].kind == NORMAL) {
			if(vals[k].v.value != NULL) {
				dump_atomic_init(env, obst, vals[k].v.value);
				skip = get_mode_size_bytes(get_irn_mode(vals[k].v.value)) - 1;
	 		} else {
	 			space = 1;
	 		}
		} else {
			assert(vals[k].kind == BITFIELD);
			obstack_printf(obst, "\t.byte\t%d\n", vals[k].v.bf_val);
		}

		++k;
		while (k < last_ofs && vals[k].kind == NORMAL && vals[k].v.value == NULL) {
			++space;
			++k;
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
	unsigned align = get_type_alignment_bytes(type);
	int emit_as_common = 0;
	ir_variability variability;
	ir_visibility visibility;

	obst = env->data_obst;
	if (is_Method_type(type)) {
		if (be_gas_flavour != GAS_FLAVOUR_MACH_O
				&& get_method_img_section(ent) == section_constructors) {
			obst = env->ctor_obst;
			obstack_printf(obst, ".balign\t%u\n", align);
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

	be_dbg_variable(obst, ent);

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
		obstack_printf(obst, ".balign\t%u\n", align);
	}

	if (!emit_as_common) {
		obstack_printf(obst, "%s:\n", ld_name);
	}

	if (variability == variability_uninitialized) {
		if (emit_as_common) {
			switch (be_gas_flavour) {
			case GAS_FLAVOUR_ELF:
			case GAS_FLAVOUR_MACH_O:
			case GAS_FLAVOUR_YASM:
				obstack_printf(obst, "\t.comm %s,%u,%u\n",
					ld_name, get_type_size_bytes(type), align);
				break;
			case GAS_FLAVOUR_MINGW:
				obstack_printf(obst, "\t.comm %s,%u # %u\n",
					ld_name, get_type_size_bytes(type), align);
				break;
			}
		} else {
			obstack_printf(obst, "\t.zero %u\n", get_type_size_bytes(type));
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
			case tpo_union:
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
void be_gas_emit_decls(const be_main_env_t *main_env,
                       int only_emit_marked_entities)
{
	be_gas_decl_env_t env;
	obstack_t         rodata;
	obstack_t         data;
	obstack_t         bss;
	obstack_t         ctor;
	int               size;
	char              *cp;

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
		be_gas_emit_switch_section(GAS_SECTION_DATA);
		be_emit_string_len(cp, size);
		be_emit_write_line();
	}

	size = obstack_object_size(&rodata);
	cp   = obstack_finish(&rodata);
	if (size > 0) {
		be_gas_emit_switch_section(GAS_SECTION_RODATA);
		be_emit_string_len(cp, size);
		be_emit_write_line();
	}

	size = obstack_object_size(&bss);
	cp   = obstack_finish(&bss);
	if (size > 0) {
		be_gas_emit_switch_section(GAS_SECTION_COMMON);
		be_emit_string_len(cp, size);
		be_emit_write_line();
	}

	size = obstack_object_size(&ctor);
	cp   = obstack_finish(&ctor);
	if (size > 0) {
		be_gas_emit_switch_section(GAS_SECTION_CTOR);
		be_emit_string_len(cp, size);
		be_emit_write_line();
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
		be_gas_emit_switch_section(GAS_SECTION_TLS);
		be_emit_cstring(".balign\t32\n");
		be_emit_write_line();
		be_emit_string_len(cp, size);
		be_emit_write_line();
	}

	obstack_free(&data, NULL);
}
