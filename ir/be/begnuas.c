/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
#include "config.h"

#include "begnuas.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "obst.h"
#include "tv.h"
#include "irnode.h"
#include "irprog.h"
#include "entity_t.h"
#include "error.h"
#include "util.h"

#include "be_t.h"
#include "beemitter.h"
#include "be_dbgout.h"

/** by default, we generate assembler code for the Linux gas */
object_file_format_t  be_gas_object_file_format = OBJECT_FILE_FORMAT_ELF;
bool                  be_gas_emit_types         = true;
char                  be_gas_elf_type_char      = '@';

static be_gas_section_t current_section = (be_gas_section_t) -1;

/**
 * An environment containing all needed dumper data.
 * Currently we create the file completely in memory first, then
 * write it to the disk. This is an artifact from the old C-generating backend
 * and even there NOT needed. So we might change it in the future.
 */
typedef struct be_gas_decl_env {
	be_gas_section_t     section;
	const be_main_env_t *main_env;
} be_gas_decl_env_t;

static void emit_section_macho(be_gas_section_t section)
{
	be_gas_section_t  base  = section & GAS_SECTION_TYPE_MASK;
	be_gas_section_t  flags = section & ~GAS_SECTION_TYPE_MASK;
	const char       *name;

	if (current_section == section)
		return;
	current_section = section;

	/* shortforms */
	if (flags == 0) {
		switch (base) {
		case GAS_SECTION_TEXT:            name = "text";          break;
		case GAS_SECTION_DATA:            name = "data";          break;
		case GAS_SECTION_RODATA:          name = "const";         break;
		case GAS_SECTION_BSS:             name = "data";          break;
		case GAS_SECTION_CONSTRUCTORS:    name = "mod_init_func"; break;
		case GAS_SECTION_DESTRUCTORS:     name = "mod_term_func"; break;
		case GAS_SECTION_PIC_TRAMPOLINES: name = "section\t__IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5"; break;
		case GAS_SECTION_PIC_SYMBOLS:     name = "section\t__IMPORT,__pointers,non_lazy_symbol_pointers"; break;
		case GAS_SECTION_CSTRING:         name = "cstring";       break;
		default: panic("unsupported scetion type 0x%X", section);
		}
		be_emit_irprintf("\t.%s\n", name);
		be_emit_write_line();
	} else if (flags & GAS_SECTION_FLAG_COMDAT) {
		switch (base) {
		case GAS_SECTION_TEXT:            name = "section __TEXT,__textcoal_nt,coalesced,pure_instructions"; break;
		case GAS_SECTION_BSS:
		case GAS_SECTION_DATA:            name = "section __DATA,__datacoal_nt,coalesced"; break;
		case GAS_SECTION_RODATA:          name = "section __TEXT,__const_coal,coalesced"; break;
		case GAS_SECTION_CSTRING:         name = "section __TEXT,__const_coal,coalesced"; break;
		default: panic("unsupported scetion type 0x%X", section);
		}
	} else {
		panic("unsupported section type 0x%X\n", section);
	}
}

static void emit_section_sparc(be_gas_section_t section, const ir_entity *entity)
{
	be_gas_section_t base = section & GAS_SECTION_TYPE_MASK;
	be_gas_section_t flags = section & ~GAS_SECTION_TYPE_MASK;
	static const char *const basename[] = {
		"text", "data", "rodata", "bss", "ctors", "dtors"
	};

	if (current_section == section && !(section & GAS_SECTION_FLAG_COMDAT))
		return;
	current_section = section;

	be_emit_cstring("\t.section\t\".");

	/* Part1: section-name */
	if (flags & GAS_SECTION_FLAG_TLS)
		be_emit_char('t');
	assert(base < (be_gas_section_t)ARRAY_SIZE(basename));
	be_emit_string(basename[base]);

	if (flags & GAS_SECTION_FLAG_COMDAT) {
		be_emit_char('.');
		be_gas_emit_entity(entity);
	}
	be_emit_char('"');

	/* for the simple sections we're done here */
	if (flags == 0)
		goto end;

	be_emit_cstring(",#alloc");

	switch (base) {
	case GAS_SECTION_TEXT: be_emit_cstring(",#execinstr"); break;
	case GAS_SECTION_DATA:
	case GAS_SECTION_BSS:  be_emit_cstring(",#write"); break;
	default:
		/* nothing */
		break;
	}
	if (flags & GAS_SECTION_FLAG_TLS) {
		be_emit_cstring(",#tls");
	}

end:
	be_emit_char('\n');
	be_emit_write_line();
}

static void emit_section(be_gas_section_t section, const ir_entity *entity)
{
	be_gas_section_t base = section & GAS_SECTION_TYPE_MASK;
	be_gas_section_t flags = section & ~GAS_SECTION_TYPE_MASK;
	static const char *const basename[] = {
		"text", "data", "rodata", "bss", "ctors", "dtors"
	};
	static const char *const type[] = {
		"progbits", "progbits", "progbits", "nobits", "init_array", "fini_array"
	};

	if (be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O) {
		emit_section_macho(section);
		return;
	} else if(be_gas_object_file_format == OBJECT_FILE_FORMAT_ELF_SPARC) {
		emit_section_sparc(section, entity);
		return;
	}

	if (current_section == section && !(section & GAS_SECTION_FLAG_COMDAT))
		return;
	current_section = section;

	/* shortforms */
	if (flags == 0) {
		switch (base) {
		case GAS_SECTION_TEXT:
			be_emit_cstring("\t.text\n");
			be_emit_write_line();
			return;
		case GAS_SECTION_DATA:
			be_emit_cstring("\t.data\n");
			be_emit_write_line();
			return;
		case GAS_SECTION_RODATA:
			be_emit_cstring("\t.section\t.rodata\n");
			be_emit_write_line();
			return;
		case GAS_SECTION_BSS:
			be_emit_cstring("\t.bss\n");
			be_emit_write_line();
			return;
		default:
			break;
		}
	}

	assert(base < (be_gas_section_t) ARRAY_SIZE(basename));
	be_emit_cstring("\t.section\t.");
	/* section name */
	if (flags & GAS_SECTION_FLAG_TLS)
		be_emit_char('t');
	be_emit_string(basename[base]);
	if (flags & GAS_SECTION_FLAG_COMDAT) {
		be_emit_char('.');
		be_gas_emit_entity(entity);
	}

	/* section flags */
	be_emit_cstring(",\"");
	if (be_gas_object_file_format != OBJECT_FILE_FORMAT_COFF)
		be_emit_char('a');
	if (base == GAS_SECTION_TEXT)
		be_emit_char('x');
	if (base != GAS_SECTION_RODATA && base != GAS_SECTION_TEXT)
		be_emit_char('w');
	if (flags & GAS_SECTION_FLAG_TLS)
		be_emit_char('T');
	if (flags & GAS_SECTION_FLAG_COMDAT)
		be_emit_char('G');
	/* section type */
	if (be_gas_object_file_format != OBJECT_FILE_FORMAT_COFF) {
		be_emit_cstring("\",");
		be_emit_char(be_gas_elf_type_char);
		be_emit_string(type[base]);
	}

	if (flags & GAS_SECTION_FLAG_COMDAT) {
		be_emit_char(',');
		be_gas_emit_entity(entity);
		be_emit_cstring(",comdat");
	}
	be_emit_char('\n');
	be_emit_write_line();
}



void be_gas_emit_switch_section(be_gas_section_t section)
{
	/* you have to produce a switch_section call with entity manually
	 * for comdat sections */
	assert( !(section & GAS_SECTION_FLAG_COMDAT));

	emit_section(section, NULL);
}

static ir_tarval *get_initializer_tarval(const ir_initializer_t *initializer)
{
	if (initializer->kind == IR_INITIALIZER_TARVAL)
		return initializer->tarval.value;
	if (initializer->kind == IR_INITIALIZER_CONST) {
		ir_node *node = initializer->consti.value;
		if (is_Const(node)) {
			return get_Const_tarval(node);
		}
	}
	return get_tarval_undefined();
}

static bool initializer_is_string_const(const ir_initializer_t *initializer)
{
	size_t i, len;
	bool found_printable = false;

	if (initializer->kind != IR_INITIALIZER_COMPOUND)
		return false;

	len = initializer->compound.n_initializers;
	if (len < 1)
		return false;
	for (i = 0; i < len; ++i) {
		int               c;
		ir_tarval        *tv;
		ir_mode          *mode;
		ir_initializer_t *sub_initializer
			= initializer->compound.initializers[i];

		tv = get_initializer_tarval(sub_initializer);
		if (!tarval_is_constant(tv))
			return false;

		mode = get_tarval_mode(tv);
		if (!mode_is_int(mode) || get_mode_size_bits(mode) != 8)
			return false;

		c = get_tarval_long(tv);
		if (isgraph(c) || isspace(c))
			found_printable = true;
		else if (c != 0)
			return false;

		if (i == len - 1 && c != '\0')
			return false;
	}

	return found_printable;
}

static bool initializer_is_null(const ir_initializer_t *initializer)
{
	switch (initializer->kind) {
	case IR_INITIALIZER_NULL:
		return true;
	case IR_INITIALIZER_TARVAL: {
		ir_tarval *tv = initializer->tarval.value;
		return tarval_is_null(tv);
	}
	case IR_INITIALIZER_CONST: {
		ir_node *value = initializer->consti.value;
		if (!is_Const(value))
			return false;
		return is_Const_null(value);
	}
	case IR_INITIALIZER_COMPOUND: {
		size_t i;
		for (i = 0; i < initializer->compound.n_initializers; ++i) {
			ir_initializer_t *subinitializer
				= initializer->compound.initializers[i];
			if (!initializer_is_null(subinitializer))
				return false;
		}
		return true;
	}
	}
	panic("invalid initializer in initializer_is_null");
}

/**
 * Determine if an entity is a string constant
 * @param ent The entity
 * @return 1 if it is a string constant, 0 otherwise
 */
static int entity_is_string_const(const ir_entity *ent)
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
	if (!mode_is_int(mode) || get_mode_size_bits(mode) != 8)
		return 0;

	if (ent->initializer != NULL) {
		return initializer_is_string_const(ent->initializer);
	} else if (entity_has_compound_ent_values(ent)) {
		int found_printable = 0;
		/* if it contains only printable chars and a 0 at the end */
		n = get_compound_ent_n_values(ent);
		for (i = 0; i < n; ++i) {
			ir_node *irn = get_compound_ent_value(ent, i);
			if (! is_Const(irn))
				return 0;

			c = (int) get_tarval_long(get_Const_tarval(irn));

			if (isgraph(c) || isspace(c))
				found_printable = 1;
			else if (c != 0)
				return 0;

			if (i == n - 1 && c != '\0')
				return 0;
		}
		return found_printable;
	}

	return 0;
}

static bool entity_is_null(const ir_entity *entity)
{
	if (entity->initializer != NULL) {
		return initializer_is_null(entity->initializer);
	} else if (entity_has_compound_ent_values(entity)) {
		/* I'm too lazy to implement this case as compound graph paths will be
		 * remove anyway in the future */
		return false;
	}
	/* uninitialized, NULL is fine */
	return true;
}

static bool is_comdat(const ir_entity *entity)
{
	ir_linkage linkage = get_entity_linkage(entity);
	return (linkage & IR_LINKAGE_MERGE)
		&& (linkage & IR_LINKAGE_GARBAGE_COLLECT);
}

static be_gas_section_t determine_basic_section(const ir_entity *entity)
{
	ir_linkage linkage;

	if (is_method_entity(entity))
		return GAS_SECTION_TEXT;

	linkage = get_entity_linkage(entity);
	if (linkage & IR_LINKAGE_CONSTANT) {
		/* mach-o is the only one with a cstring section */
		if (be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O
				&& entity_is_string_const(entity))
			return GAS_SECTION_CSTRING;

		return GAS_SECTION_RODATA;
	}
	if (entity_is_null(entity))
		return GAS_SECTION_BSS;

	return GAS_SECTION_DATA;
}

static be_gas_section_t determine_section(be_gas_decl_env_t *env,
                                          const ir_entity *entity)
{
	ir_type *owner = get_entity_owner(entity);

	if (owner == get_segment_type(IR_SEGMENT_GLOBAL)) {
		be_gas_section_t section = determine_basic_section(entity);
		if (is_comdat(entity))
			section |= GAS_SECTION_FLAG_COMDAT;
		return section;
	} else if (env != NULL && owner == env->main_env->pic_symbols_type) {
		return GAS_SECTION_PIC_SYMBOLS;
	} else if (env != NULL && owner == env->main_env->pic_trampolines_type) {
		return GAS_SECTION_PIC_TRAMPOLINES;
	} else if (owner == get_segment_type(IR_SEGMENT_CONSTRUCTORS)) {
		return GAS_SECTION_CONSTRUCTORS;
	} else if (owner == get_segment_type(IR_SEGMENT_DESTRUCTORS)) {
		return GAS_SECTION_DESTRUCTORS;
	} else if (owner == get_segment_type(IR_SEGMENT_THREAD_LOCAL)) {
		be_gas_section_t section = determine_basic_section(entity);
		if (is_comdat(entity))
			section |= GAS_SECTION_FLAG_COMDAT;

		return section | GAS_SECTION_FLAG_TLS;
	}

	/* the java frontend keeps some functions inside classes */
	if (is_Class_type(owner)) {
		return determine_basic_section(entity);
	}

	panic("Couldn't determine section for %+F?!?", entity);
}

static void emit_weak(const ir_entity *entity)
{
	if (be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O) {
		be_emit_cstring("\t.weak_reference ");
	} else {
		be_emit_cstring("\t.weak ");
	}
	be_gas_emit_entity(entity);
	be_emit_char('\n');
	be_emit_write_line();
}

static void emit_visibility(const ir_entity *entity)
{
	ir_linkage linkage = get_entity_linkage(entity);

	if (get_entity_linkage(entity) & IR_LINKAGE_WEAK) {
		emit_weak(entity);
		/* Note: .weak seems to imply .globl so no need to output .globl */
	} else if (get_entity_visibility(entity) == ir_visibility_default) {
		be_emit_cstring(".globl ");
		be_gas_emit_entity(entity);
		be_emit_char('\n');
		be_emit_write_line();
	}

	if (be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O
			&& (linkage & IR_LINKAGE_HIDDEN_USER)
			&& get_entity_ld_name(entity)[0] != '\0') {
		be_emit_cstring("\t.no_dead_strip ");
		be_gas_emit_entity(entity);
		be_emit_char('\n');
		be_emit_write_line();
	}
}

void be_gas_emit_function_prolog(const ir_entity *entity, unsigned po2alignment)
{
	be_gas_section_t section = determine_section(NULL, entity);
	emit_section(section, entity);

	/* write the begin line (makes the life easier for scripts parsing the
	 * assembler) */
	be_emit_write_line();
	be_emit_cstring("# -- Begin  ");
	be_gas_emit_entity(entity);
	be_emit_char('\n');
	be_emit_write_line();

	if (po2alignment > 0) {
		const char *fill_byte = "";
		unsigned    maximum_skip = (1 << po2alignment) - 1;
		/* gcc fills space between function with 0x90... */
		if (be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O) {
			fill_byte = "0x90";
		}
		be_emit_cstring("\t.p2align ");
		be_emit_irprintf("%u,%s,%u\n", po2alignment, fill_byte, maximum_skip);
		be_emit_write_line();
	}
	emit_visibility(entity);

	switch (be_gas_object_file_format) {
	case OBJECT_FILE_FORMAT_ELF:
	case OBJECT_FILE_FORMAT_ELF_SPARC:
		be_emit_cstring("\t.type\t");
		be_gas_emit_entity(entity);
		be_emit_cstring(", ");
		be_emit_char(be_gas_elf_type_char);
		be_emit_cstring("function\n");
		be_emit_write_line();
		break;
	case OBJECT_FILE_FORMAT_COFF:
		be_emit_cstring("\t.def\t");
		be_gas_emit_entity(entity);
		be_emit_cstring(";");
		if (get_entity_visibility(entity) == ir_visibility_local) {
			be_emit_cstring("\t.scl\t3;");
		} else {
			be_emit_cstring("\t.scl\t2;");
		}
		be_emit_cstring("\t.type\t32;\t.endef\n");
		be_emit_write_line();
		break;
	case OBJECT_FILE_FORMAT_MACH_O:
		break;
	}
	be_gas_emit_entity(entity);
	be_emit_cstring(":\n");
	be_emit_write_line();
}

void be_gas_emit_function_epilog(const ir_entity *entity)
{
	if (be_gas_object_file_format == OBJECT_FILE_FORMAT_ELF) {
		be_emit_cstring("\t.size\t");
		be_gas_emit_entity(entity);
		be_emit_cstring(", .-");
		be_gas_emit_entity(entity);
		be_emit_char('\n');
		be_emit_write_line();
	}

	be_emit_cstring("# -- End  ");
	be_gas_emit_entity(entity);
	be_emit_char('\n');
	be_emit_write_line();
}

/**
 * Output a tarval.
 *
 * @param tv     the tarval
 * @param bytes  the width of the tarvals value in bytes
 */
static void emit_arith_tarval(ir_tarval *tv, int bytes)
{
	switch (bytes) {
	case 1:
		be_emit_irprintf("0x%02x", get_tarval_sub_bits(tv, 0));
		return;

	case 2:
		be_emit_irprintf("0x%02x%02x", get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		return;

	case 4:
		be_emit_irprintf("0x%02x%02x%02x%02x",
			get_tarval_sub_bits(tv, 3), get_tarval_sub_bits(tv, 2), get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		return;

	case 8:
		be_emit_irprintf("0x%02x%02x%02x%02x%02x%02x%02x%02x",
			get_tarval_sub_bits(tv, 7), get_tarval_sub_bits(tv, 6), get_tarval_sub_bits(tv, 5), get_tarval_sub_bits(tv, 4),
			get_tarval_sub_bits(tv, 3), get_tarval_sub_bits(tv, 2), get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		return;

	case 12:
		/* Beware: Mixed endian output!  One little endian number emitted as
		 * three longs.  Each long initializer is written in big endian. */
		be_emit_irprintf(
			"\t.long\t0x%02x%02x%02x%02x\n"
			"\t.long\t0x%02x%02x%02x%02x\n"
			"\t.long\t0x%02x%02x%02x%02x",
			get_tarval_sub_bits(tv,  3), get_tarval_sub_bits(tv,  2),
			get_tarval_sub_bits(tv,  1), get_tarval_sub_bits(tv,  0),
			get_tarval_sub_bits(tv,  7), get_tarval_sub_bits(tv,  6),
			get_tarval_sub_bits(tv,  5), get_tarval_sub_bits(tv,  4),
			get_tarval_sub_bits(tv, 11), get_tarval_sub_bits(tv, 10),
			get_tarval_sub_bits(tv,  9), get_tarval_sub_bits(tv,  8)
		);
		return;

	case 16:
		/* Beware: Mixed endian output!  One little endian number emitted as
		 * three longs.  Each long initializer is written in big endian. */
		be_emit_irprintf(
			"\t.long\t0x%02x%02x%02x%02x\n"
			"\t.long\t0x%02x%02x%02x%02x\n"
			"\t.long\t0x%02x%02x%02x%02x\n"
			"\t.long\t0x%02x%02x%02x%02x",
			get_tarval_sub_bits(tv,  3), get_tarval_sub_bits(tv,  2),
			get_tarval_sub_bits(tv,  1), get_tarval_sub_bits(tv,  0),
			get_tarval_sub_bits(tv,  7), get_tarval_sub_bits(tv,  6),
			get_tarval_sub_bits(tv,  5), get_tarval_sub_bits(tv,  4),
			get_tarval_sub_bits(tv, 11), get_tarval_sub_bits(tv, 10),
			get_tarval_sub_bits(tv,  9), get_tarval_sub_bits(tv,  8),
			get_tarval_sub_bits(tv, 15), get_tarval_sub_bits(tv, 14),
			get_tarval_sub_bits(tv, 13), get_tarval_sub_bits(tv, 12)
		);
		return;
	}

	panic("Can't dump a tarval with %d bytes", bytes);
}

/**
 * Return the label prefix for labeled instructions.
 */
const char *be_gas_insn_label_prefix(void)
{
	return ".LE";
}

/**
 * Return the tarval of an atomic initializer.
 *
 * @param init  a node representing the initializer (on the const code irg)
 *
 * @return the tarval
 */
static ir_tarval *get_atomic_init_tv(ir_node *init)
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
 * @param init  a node representing the atomic value (on the const code irg)
 */
static void do_emit_atomic_init(be_gas_decl_env_t *env, ir_node *init)
{
	ir_mode *mode = get_irn_mode(init);
	int bytes     = get_mode_size_bytes(mode);
	ir_tarval *tv;
	ir_entity *ent;

	init = skip_Id(init);

	switch (get_irn_opcode(init)) {
	case iro_Cast:
		do_emit_atomic_init(env, get_Cast_op(init));
		return;

	case iro_Conv:
		do_emit_atomic_init(env, get_Conv_op(init));
		return;

	case iro_Const:
		tv = get_Const_tarval(init);

		/* it's an arithmetic value */
		emit_arith_tarval(tv, bytes);
		return;

	case iro_SymConst:
		switch (get_SymConst_kind(init)) {
		case symconst_addr_ent:
			ent = get_SymConst_entity(init);
			be_gas_emit_entity(ent);
			break;

		case symconst_ofs_ent:
			ent = get_SymConst_entity(init);
			be_emit_irprintf("%d", get_entity_offset(ent));
			break;

		case symconst_type_size:
			be_emit_irprintf("%u", get_type_size_bytes(get_SymConst_type(init)));
			break;

		case symconst_type_align:
			be_emit_irprintf("%u", get_type_alignment_bytes(get_SymConst_type(init)));
			break;

		case symconst_enum_const:
			tv = get_enumeration_value(get_SymConst_enum(init));
			emit_arith_tarval(tv, bytes);
			break;

		default:
			assert(!"emit_atomic_init(): don't know how to init from this SymConst");
		}
		return;

	case iro_Add:
		if (!mode_is_int(mode) && !mode_is_reference(mode)) {
			panic("Constant must be int or pointer for '+' to work");
		}
		do_emit_atomic_init(env, get_Add_left(init));
		be_emit_cstring(" + ");
		do_emit_atomic_init(env, get_Add_right(init));
		return;

	case iro_Sub:
		if (!mode_is_int(mode) && !mode_is_reference(mode)) {
			panic("Constant must be int or pointer for '-' to work");
		}
		do_emit_atomic_init(env, get_Sub_left(init));
		be_emit_cstring(" - ");
		do_emit_atomic_init(env, get_Sub_right(init));
		return;

	case iro_Mul:
		if (!mode_is_int(mode) && !mode_is_reference(mode)) {
			panic("Constant must be int or pointer for '*' to work");
		}
		do_emit_atomic_init(env, get_Mul_left(init));
		be_emit_cstring(" * ");
		do_emit_atomic_init(env, get_Mul_right(init));
		return;

	case iro_Unknown:
		be_emit_cstring("0");
		return;

	default:
		panic("emit_atomic_init(): unsupported IR-node %+F", init);
	}
}

/**
 * Dumps the type for given size (.byte, .long, ...)
 *
 * @param size  the size in bytes
 */
static void emit_size_type(size_t size)
{
	switch (size) {
	case 1:
		be_emit_cstring("\t.byte\t");
		break;

	case 2:
		be_emit_cstring("\t.short\t");
		break;

	case 4:
		be_emit_cstring("\t.long\t");
		break;

	case 8:
		be_emit_cstring("\t.quad\t");
		break;

	case 10:
	case 12:
	case 16: /* Note: .octa does not work on mac */
		/* handled in arith */
		break;

	default:
		panic("Try to dump a type with %u bytes", (unsigned)size);
	}
}

/**
 * Emit an atomic value.
 *
 * @param env   the gas output environment
 * @param init  a node representing the atomic value (on the const code irg)
 */
static void emit_atomic_init(be_gas_decl_env_t *env, ir_node *init)
{
	ir_mode *mode = get_irn_mode(init);
	int bytes     = get_mode_size_bytes(mode);

	emit_size_type(bytes);
	do_emit_atomic_init(env, init);
	be_emit_char('\n');
	be_emit_write_line();
}

/**
 * Dump a string constant.
 * No checks are made!!
 *
 * @param ent  The entity to dump.
 */
static void emit_string_cst(const ir_entity *ent)
{
	int      i, len;
	int      output_len;
	ir_type *type;
	int      type_size;
	int      remaining_space;

	len        = get_compound_ent_n_values(ent);
	output_len = len;
	if (be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O) {
		be_emit_cstring("\t.ascii \"");
	} else {
		be_emit_cstring("\t.string \"");
		output_len -= 1;
	}

	for (i = 0; i < output_len; ++i) {
		ir_node *irn;
		int c;

		irn = get_compound_ent_value(ent, i);
		c = (int) get_tarval_long(get_Const_tarval(irn));

		switch (c) {
		case '"' : be_emit_cstring("\\\""); break;
		case '\n': be_emit_cstring("\\n"); break;
		case '\r': be_emit_cstring("\\r"); break;
		case '\t': be_emit_cstring("\\t"); break;
		case '\\': be_emit_cstring("\\\\"); break;
		default  :
			if (isprint(c))
				be_emit_char(c);
			else
				be_emit_irprintf("\\%o", c);
			break;
		}
	}
	be_emit_cstring("\"\n");
	be_emit_write_line();

	type            = get_entity_type(ent);
	type_size       = get_type_size_bytes(type);
	remaining_space = type_size - len;
	assert(remaining_space >= 0);
	if (remaining_space > 0) {
		be_emit_irprintf("\t.space\t%d, 0\n", remaining_space);
	}
}

static size_t emit_string_initializer(const ir_initializer_t *initializer)
{
	size_t i, len;

	len = initializer->compound.n_initializers;
	if (be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O) {
		be_emit_cstring("\t.ascii \"");
	} else {
		be_emit_cstring("\t.string \"");
		len -= 1;
	}

	for (i = 0; i < len; ++i) {
		const ir_initializer_t *sub_initializer
			= get_initializer_compound_value(initializer, i);

		ir_tarval *tv = get_initializer_tarval(sub_initializer);
		int        c  = get_tarval_long(tv);

		switch (c) {
		case '"' : be_emit_cstring("\\\""); break;
		case '\n': be_emit_cstring("\\n"); break;
		case '\r': be_emit_cstring("\\r"); break;
		case '\t': be_emit_cstring("\\t"); break;
		case '\\': be_emit_cstring("\\\\"); break;
		default  :
			if (isprint(c))
				be_emit_char(c);
			else
				be_emit_irprintf("\\%o", c);
			break;
		}
	}
	be_emit_cstring("\"\n");
	be_emit_write_line();

	return initializer->compound.n_initializers;
}

typedef enum normal_or_bitfield_kind {
	NORMAL = 0,
	TARVAL,
	STRING,
	BITFIELD
} normal_or_bitfield_kind;

typedef struct {
	normal_or_bitfield_kind kind;
	union {
		ir_node                *value;
		ir_tarval              *tarval;
		unsigned char           bf_val;
		const ir_initializer_t *string;
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
	switch (get_initializer_kind(initializer)) {
	case IR_INITIALIZER_TARVAL:
		assert(get_tarval_mode(get_initializer_tarval_value(initializer)) == get_type_mode(type));
		return get_type_size_bytes(type);
	case IR_INITIALIZER_CONST:
	case IR_INITIALIZER_NULL:
		return get_type_size_bytes(type);
	case IR_INITIALIZER_COMPOUND:
		if (!is_type_variable_size(type)) {
			return get_type_size_bytes(type);
		} else {
			size_t n_entries
				= get_initializer_compound_n_entries(initializer);
			size_t i;
			unsigned initializer_size = get_type_size_bytes(type);
			for (i = 0; i < n_entries; ++i) {
				ir_entity *entity = get_compound_member(type, i);
				ir_type   *type   = get_entity_type(entity);

				const ir_initializer_t *sub_initializer
					= get_initializer_compound_value(initializer, i);

				unsigned offset = get_entity_offset(entity);
				unsigned size   = get_initializer_size(sub_initializer, type);

				if (offset + size > initializer_size) {
					initializer_size = offset + size;
				}
			}
			return initializer_size;
		}
	}

	panic("found invalid initializer");
}

#ifndef NDEBUG
static normal_or_bitfield *glob_vals;
static size_t              max_vals;
#endif

static void emit_bitfield(normal_or_bitfield *vals, size_t offset_bits,
                          const ir_initializer_t *initializer, ir_type *type)
{
	static const size_t BITS_PER_BYTE = 8;
	ir_mode   *mode      = get_type_mode(type);
	ir_tarval *tv        = NULL;
	int        value_len;
	size_t     bit_offset;
	size_t     end;
	bool       big_endian = be_get_backend_param()->byte_order_big_endian;

	switch (get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL:
		return;
	case IR_INITIALIZER_TARVAL:
		tv = get_initializer_tarval_value(initializer);
		break;
	case IR_INITIALIZER_CONST: {
		ir_node *node = get_initializer_const_value(initializer);
		if (!is_Const(node)) {
			panic("bitfield initializer not a Const node");
		}
		tv = get_Const_tarval(node);
		break;
	}
	case IR_INITIALIZER_COMPOUND:
		panic("bitfield initializer is compound");
	}
	if (tv == NULL) {
		panic("Couldn't get numeric value for bitfield initializer");
	}
	tv = tarval_convert_to(tv, get_type_mode(type));

	value_len  = get_type_size_bytes(get_primitive_base_type(type));
	bit_offset = 0;
	end        = get_mode_size_bits(mode);
	while (bit_offset < end) {
		size_t        src_offset      = bit_offset / BITS_PER_BYTE;
		size_t        src_offset_bits = bit_offset % BITS_PER_BYTE;
		size_t        dst_offset      = (bit_offset+offset_bits) / BITS_PER_BYTE;
		size_t        dst_offset_bits = (bit_offset+offset_bits) % BITS_PER_BYTE;
		size_t        src_bits_len    = end-bit_offset;
		size_t        dst_bits_len    = BITS_PER_BYTE-dst_offset_bits;
		unsigned char curr_bits;
		normal_or_bitfield *val;
		if (src_bits_len > dst_bits_len)
			src_bits_len = dst_bits_len;

		if (big_endian) {
			val = &vals[value_len - dst_offset - 1];
		} else {
			val = &vals[dst_offset];
		}

		assert((val-glob_vals) < (ptrdiff_t) max_vals);
		assert(val->kind == BITFIELD ||
				(val->kind == NORMAL && val->v.value == NULL));
		val->kind  = BITFIELD;
		curr_bits  = get_tarval_sub_bits(tv, src_offset);
		curr_bits  = curr_bits >> src_offset_bits;
		if (src_offset_bits + src_bits_len > 8) {
			unsigned next_bits = get_tarval_sub_bits(tv, src_offset+1);
			curr_bits |= next_bits << (8 - src_offset_bits);
		}
		curr_bits &= (1 << src_bits_len) - 1;
		val->v.bf_val |= curr_bits << dst_offset_bits;

		bit_offset += dst_bits_len;
	}
}

static void emit_ir_initializer(normal_or_bitfield *vals,
                                const ir_initializer_t *initializer,
                                ir_type *type)
{
	assert((size_t) (vals - glob_vals) < max_vals);

	if (initializer_is_string_const(initializer)) {
		assert(vals->kind != BITFIELD);
		vals->kind     = STRING;
		vals->v.string = initializer;
		return;
	}

	switch (get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL:
		return;
	case IR_INITIALIZER_TARVAL: {
		size_t i;

		assert(vals->kind != BITFIELD);
		vals->kind     = TARVAL;
		vals->v.tarval = get_initializer_tarval_value(initializer);
		assert(get_type_mode(type) == get_tarval_mode(vals->v.tarval));
		for (i = 1; i < get_type_size_bytes(type); ++i) {
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
		for (i = 1; i < get_type_size_bytes(type); ++i) {
			vals[i].kind    = NORMAL;
			vals[i].v.value = NULL;
		}
		return;
	}
	case IR_INITIALIZER_COMPOUND: {
		size_t i = 0;
		size_t n = get_initializer_compound_n_entries(initializer);

		if (is_Array_type(type)) {
			ir_type *element_type = get_array_element_type(type);
			size_t   skip         = get_type_size_bytes(element_type);
			size_t   alignment    = get_type_alignment_bytes(element_type);
			size_t   misalign     = skip % alignment;
			if (misalign != 0) {
				skip += alignment - misalign;
			}

			for (i = 0; i < n; ++i) {
				ir_initializer_t *sub_initializer
					= get_initializer_compound_value(initializer, i);

				emit_ir_initializer(vals, sub_initializer, element_type);

				vals += skip;
			}
		} else {
			size_t n_members, i;
			assert(is_compound_type(type));
			n_members = get_compound_n_members(type);
			for (i = 0; i < n_members; ++i) {
				ir_entity        *member    = get_compound_member(type, i);
				size_t            offset    = get_entity_offset(member);
				ir_type          *subtype   = get_entity_type(member);
				ir_mode          *mode      = get_type_mode(subtype);
				ir_initializer_t *sub_initializer;

				assert(i < get_initializer_compound_n_entries(initializer));
				sub_initializer
					= get_initializer_compound_value(initializer, i);

				if (mode != NULL) {
					size_t offset_bits
						= get_entity_offset_bits_remainder(member);
					size_t value_len = get_mode_size_bits(mode);

					if (offset_bits != 0 ||
						(value_len != 8 && value_len != 16 && value_len != 32
						 && value_len != 64) ||
						(is_Primitive_type(subtype) && get_primitive_base_type(subtype) != NULL)) {
						emit_bitfield(&vals[offset], offset_bits,
						              sub_initializer, subtype);
						continue;
					}
				}

				emit_ir_initializer(&vals[offset], sub_initializer, subtype);
			}
		}

		return;
	}
	}
	panic("invalid ir_initializer kind found");
}

static void emit_initializer(be_gas_decl_env_t *env, const ir_entity *entity)
{
	const ir_initializer_t *initializer = entity->initializer;
	ir_type                *type;
	normal_or_bitfield     *vals;
	size_t                  size;
	size_t                  k;

	if (initializer_is_string_const(initializer)) {
		emit_string_initializer(initializer);
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
	vals = XMALLOCNZ(normal_or_bitfield, size);

#ifndef NDEBUG
	glob_vals = vals;
	max_vals  = size;
#endif

	emit_ir_initializer(vals, initializer, type);

	/* now write values sorted */
	for (k = 0; k < size; ) {
		int                     space     = 0;
		int                     elem_size = 1;
		normal_or_bitfield_kind kind      = vals[k].kind;
		switch (kind) {
		case NORMAL:
			if (vals[k].v.value != NULL) {
				emit_atomic_init(env, vals[k].v.value);
				elem_size = get_mode_size_bytes(get_irn_mode(vals[k].v.value));
			} else {
				elem_size = 0;
			}
			break;
		case TARVAL: {
			ir_tarval *tv   = vals[k].v.tarval;
			size_t     size = get_mode_size_bytes(get_tarval_mode(tv));

			assert(tv != NULL);

			elem_size = size;
			emit_size_type(size);
			emit_arith_tarval(tv, size);
			be_emit_char('\n');
			be_emit_write_line();
			break;
		}
		case STRING:
			elem_size = emit_string_initializer(vals[k].v.string);
			break;
		case BITFIELD:
			be_emit_irprintf("\t.byte\t%d\n", vals[k].v.bf_val);
			be_emit_write_line();
			break;
		}

		k += elem_size;
		while (k < size && vals[k].kind == NORMAL && vals[k].v.value == NULL) {
			++space;
			++k;
		}

		/* a gap */
		if (space > 0) {
			be_emit_irprintf("\t.space\t%d, 0\n", space);
			be_emit_write_line();
		}
	}
	xfree(vals);
}

static void emit_compound_graph_init(be_gas_decl_env_t *env,
                                     const ir_entity *ent)
{
	normal_or_bitfield *vals;
	int i, j, n;
	unsigned k, last_ofs;

	if (entity_is_string_const(ent)) {
		emit_string_cst(ent);
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
	vals = XMALLOCNZ(normal_or_bitfield, last_ofs);

	/* collect the values and store them at the offsets */
	for (i = 0; i < n; ++i) {
		unsigned offset      = get_compound_ent_value_offset_bytes(ent, i);
		int      offset_bits = get_compound_ent_value_offset_bit_remainder(ent, i);
		ir_node  *value      = get_compound_ent_value(ent, i);
		int      value_len   = get_mode_size_bits(get_irn_mode(value));

		assert(offset_bits >= 0);

		if (offset_bits != 0 ||
				(value_len != 8 && value_len != 16 && value_len != 32 && value_len != 64)) {
			ir_tarval *tv = get_atomic_init_tv(value);
			unsigned char curr_bits, last_bits = 0;
			if (tv == NULL) {
				panic("Couldn't get numeric value for bitfield initializer '%s'",
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
			if (vals[k].v.value != NULL) {
				emit_atomic_init(env, vals[k].v.value);
				skip = get_mode_size_bytes(get_irn_mode(vals[k].v.value)) - 1;
			} else {
				space = 1;
			}
		} else {
			assert(vals[k].kind == BITFIELD);
			be_emit_irprintf("\t.byte\t%d\n", vals[k].v.bf_val);
		}

		++k;
		while (k < last_ofs && vals[k].kind == NORMAL && vals[k].v.value == NULL) {
			++space;
			++k;
		}
		space -= skip;
		assert(space >= 0);

		/* a gap */
		if (space > 0) {
			be_emit_irprintf("\t.space\t%d, 0\n", space);
			be_emit_write_line();
		}
	}
	xfree(vals);
}

static void emit_align(unsigned p2alignment)
{
	be_emit_irprintf("\t.p2align\t%u\n", log2_floor(p2alignment));
	be_emit_write_line();
}

static unsigned get_effective_entity_alignment(const ir_entity *entity)
{
	unsigned alignment = get_entity_alignment(entity);
	if (alignment == 0) {
		ir_type *type = get_entity_type(entity);
		alignment     = get_type_alignment_bytes(type);
	}
	return alignment;
}

static void emit_common(const ir_entity *entity)
{
	unsigned size      = get_type_size_bytes(get_entity_type(entity));
	unsigned alignment = get_effective_entity_alignment(entity);

	if (get_entity_linkage(entity) & IR_LINKAGE_WEAK) {
		emit_weak(entity);
	}

	switch (be_gas_object_file_format) {
	case OBJECT_FILE_FORMAT_MACH_O:
		be_emit_cstring("\t.comm ");
		be_gas_emit_entity(entity);
		be_emit_irprintf(",%u,%u\n", size, log2_floor(alignment));
		be_emit_write_line();
		return;
	case OBJECT_FILE_FORMAT_ELF:
	case OBJECT_FILE_FORMAT_ELF_SPARC:
		be_emit_cstring("\t.comm ");
		be_gas_emit_entity(entity);
		be_emit_irprintf(",%u,%u\n", size, alignment);
		be_emit_write_line();
		return;
	case OBJECT_FILE_FORMAT_COFF:
		be_emit_cstring("\t.comm ");
		be_gas_emit_entity(entity);
		be_emit_irprintf(",%u # %u\n", size, alignment);
		be_emit_write_line();
		return;
	}
	panic("invalid object file format");
}

static void emit_local_common(const ir_entity *entity)
{
	unsigned size      = get_type_size_bytes(get_entity_type(entity));
	unsigned alignment = get_effective_entity_alignment(entity);

	if (get_entity_linkage(entity) & IR_LINKAGE_WEAK) {
		emit_weak(entity);
	}

	switch (be_gas_object_file_format) {
	case OBJECT_FILE_FORMAT_MACH_O:
		be_emit_cstring("\t.lcomm ");
		be_gas_emit_entity(entity);
		be_emit_irprintf(",%u,%u\n", size, log2_floor(alignment));
		be_emit_write_line();
		return;
	case OBJECT_FILE_FORMAT_ELF:
	case OBJECT_FILE_FORMAT_ELF_SPARC:
		be_emit_cstring("\t.local ");
		be_gas_emit_entity(entity);
		be_emit_cstring("\n");
		be_emit_write_line();
		be_emit_cstring("\t.comm ");
		be_gas_emit_entity(entity);
		be_emit_irprintf(",%u,%u\n", size, alignment);
		be_emit_write_line();
		return;
	case OBJECT_FILE_FORMAT_COFF:
		be_emit_cstring("\t.lcomm ");
		be_gas_emit_entity(entity);
		be_emit_irprintf(",%u # %u\n", size, alignment);
		be_emit_write_line();
		return;
	}
	panic("invalid object file format");
}

static void emit_indirect_symbol(const ir_entity *entity, be_gas_section_t section)
{
	/* we can only do PIC code on macho so far */
	assert(be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O);

	be_gas_emit_entity(entity);
	be_emit_cstring(":\n");
	be_emit_write_line();
	be_emit_cstring("\t.indirect_symbol ");
	be_emit_ident(get_entity_ident(entity));
	be_emit_char('\n');
	be_emit_write_line();
	if (section == GAS_SECTION_PIC_TRAMPOLINES) {
		be_emit_cstring("\thlt ; hlt ; hlt ; hlt ; hlt\n");
		be_emit_write_line();
	} else {
		assert(section == GAS_SECTION_PIC_SYMBOLS);
		be_emit_cstring("\t.long 0\n");
		be_emit_write_line();
	}
}

char const *be_gas_get_private_prefix(void)
{
	return be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O ? "L" : ".L";
}

void be_gas_emit_entity(const ir_entity *entity)
{
	if (entity->type == firm_code_type) {
		ir_label_t label = get_entity_label(entity);
		be_emit_irprintf("%s_%lu", be_gas_get_private_prefix(), label);
		return;
	}

	if (get_entity_visibility(entity) == ir_visibility_private) {
		be_emit_string(be_gas_get_private_prefix());
	}
	be_emit_ident(get_entity_ld_ident(entity));
}

void be_gas_emit_block_name(const ir_node *block)
{
	if (has_Block_entity(block)) {
		ir_entity *entity = get_Block_entity(block);
		be_gas_emit_entity(entity);
	} else {
		be_emit_irprintf("%s%ld", be_gas_get_private_prefix(), get_irn_node_nr(block));
	}
}

/**
 * Dump a global entity.
 *
 * @param env  the gas output environment
 * @param ent  the entity to be dumped
 */
static void emit_global(be_gas_decl_env_t *env, const ir_entity *entity)
{
	ir_type          *type       = get_entity_type(entity);
	ident            *ld_ident   = get_entity_ld_ident(entity);
	unsigned          alignment  = get_effective_entity_alignment(entity);
	be_gas_section_t  section    = determine_section(env, entity);
	ir_visibility     visibility = get_entity_visibility(entity);
	ir_linkage        linkage    = get_entity_linkage(entity);

	/* block labels are already emittet in the code */
	if (type == firm_code_type)
		return;

	/* we already emitted all methods. Except for the trampolines which
	 * the assembler/linker generates */
	if (is_Method_type(type) && section != GAS_SECTION_PIC_TRAMPOLINES) {
		/* functions with graph are already emitted with
		 * be_gas_emit_function_prolog */
		if (get_entity_irg(entity) == NULL) {
			emit_visibility(entity);
		}
		return;
	}

	be_dbg_variable(entity);

	if (section == GAS_SECTION_BSS) {
		switch (visibility) {
		case ir_visibility_local:
		case ir_visibility_private:
			emit_local_common(entity);
			return;
		case ir_visibility_default:
			if (linkage & IR_LINKAGE_MERGE) {
				emit_common(entity);
				return;
			}
			break;
		case ir_visibility_external:
			if (linkage & IR_LINKAGE_MERGE)
				panic("merge link semantic not supported for extern entities");
			break;
		}
	}

	emit_visibility(entity);
	if (visibility == ir_visibility_external) {
		/* nothing to do for externally defined values */
		return;
	}

	if (!is_po2(alignment))
		panic("alignment not a power of 2");

	emit_section(section, entity);

	if (section == GAS_SECTION_PIC_TRAMPOLINES
			|| section == GAS_SECTION_PIC_SYMBOLS) {
		emit_indirect_symbol(entity, section);
		return;
	}

	/* alignment */
	if (alignment > 1) {
		emit_align(alignment);
	}
	if (be_gas_object_file_format == OBJECT_FILE_FORMAT_ELF
			&& be_gas_emit_types
			&& visibility != ir_visibility_private) {
		be_emit_cstring("\t.type\t");
		be_gas_emit_entity(entity);
		be_emit_cstring(", ");
		be_emit_char(be_gas_elf_type_char);
		be_emit_cstring("object\n\t.size\t");\
		be_gas_emit_entity(entity);
		be_emit_irprintf(", %u\n", get_type_size_bytes(type));
	}

	if (get_id_str(ld_ident)[0] != '\0') {
	    be_gas_emit_entity(entity);
		be_emit_cstring(":\n");
		be_emit_write_line();
	}

	if (entity_is_null(entity)) {
		unsigned size = get_type_size_bytes(type);
		if (size > 0) {
			be_emit_irprintf("\t.space %u, 0\n", get_type_size_bytes(type));
			be_emit_write_line();
		}
	} else if (entity_has_compound_ent_values(entity)) {
		emit_compound_graph_init(env, entity);
	} else {
		assert(entity->initializer != NULL);
		emit_initializer(env, entity);
	}
}

/**
 * Dumps declarations of global variables and the initialization code.
 *
 * @param gt                a global like type, either the global or the TLS one
 * @param env               an environment
 */
static void be_gas_emit_globals(ir_type *gt, be_gas_decl_env_t *env)
{
	size_t i, n = get_compound_n_members(gt);

	for (i = 0; i < n; i++) {
		ir_entity *ent = get_compound_member(gt, i);
		emit_global(env, ent);
	}
}

/* Generate all entities. */
void be_gas_emit_decls(const be_main_env_t *main_env)
{
	be_gas_decl_env_t env;
	memset(&env, 0, sizeof(env));

	/* dump global type */
	env.main_env = main_env;
	env.section  = (be_gas_section_t) -1;

	be_gas_emit_globals(get_glob_type(), &env);
	be_gas_emit_globals(get_tls_type(), &env);
	be_gas_emit_globals(get_segment_type(IR_SEGMENT_CONSTRUCTORS), &env);
	be_gas_emit_globals(get_segment_type(IR_SEGMENT_DESTRUCTORS), &env);
	be_gas_emit_globals(main_env->pic_symbols_type, &env);
	be_gas_emit_globals(main_env->pic_trampolines_type, &env);

	/**
	 * ".subsections_via_symbols marks object files which are OK to divide
	 * their section contents into individual blocks".
	 * From my understanding this means no label points in the middle of an
	 * object which we want to address as a whole. Firm code should be fine
	 * with this.
	 */
	if (be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O) {
		be_emit_cstring("\t.subsections_via_symbols\n");
		be_emit_write_line();
	}
}
