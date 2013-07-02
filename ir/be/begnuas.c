/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Dumps global variables and constants as gas assembler.
 * @author      Christian Wuerdig, Matthias Braun
 * @date        04.11.2005
 */
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
#include "execfreq.h"

#include "be_t.h"
#include "bearch.h"
#include "beemitter.h"
#include "bedwarf.h"

/** by default, we generate assembler code for the Linux gas */
object_file_format_t  be_gas_object_file_format = OBJECT_FILE_FORMAT_ELF;
elf_variant_t         be_gas_elf_variant        = ELF_VARIANT_NORMAL;
bool                  be_gas_emit_types         = true;
char                  be_gas_elf_type_char      = '@';

static be_gas_section_t current_section = (be_gas_section_t) -1;
static pmap            *block_numbers;
static unsigned         next_block_nr;

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

	if (current_section == section)
		return;
	current_section = section;

	/* shortforms */
	const char *name;
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
		case GAS_SECTION_DEBUG_INFO:      name = "section __DWARF,__debug_info,regular,debug"; break;
		case GAS_SECTION_DEBUG_ABBREV:    name = "section __DWARF,__debug_abbrev,regular,debug"; break;
		case GAS_SECTION_DEBUG_LINE:      name = "section __DWARF,__debug_line,regular,debug"; break;
		case GAS_SECTION_DEBUG_PUBNAMES:  name = "section __DWARF,__debug_pubnames,regular,debug"; break;
		case GAS_SECTION_DEBUG_FRAME:     name = "section __DWARF,__debug_frame,regular,debug"; break;
		default: panic("unsupported scetion type 0x%X", section);
		}
	} else if (flags & GAS_SECTION_FLAG_COMDAT) {
		switch (base) {
		case GAS_SECTION_TEXT:            name = "section __TEXT,__textcoal_nt,coalesced,pure_instructions"; break;
		case GAS_SECTION_BSS:
		case GAS_SECTION_DATA:            name = "section __DATA,__datacoal_nt,coalesced"; break;
		case GAS_SECTION_RODATA:          name = "section __TEXT,__const_coal,coalesced"; break;
		case GAS_SECTION_CSTRING:         name = "section __TEXT,__const_coal,coalesced"; break;
		default: panic("unsupported scetion type 0x%X", section);
		}
	} else if (flags & GAS_SECTION_FLAG_TLS) {
		panic("thread local storage not supported on macho (section 0x%X)", section);
	} else {
		panic("unsupported section type 0x%X", section);
	}
	be_emit_irprintf("\t.%s\n", name);
	be_emit_write_line();
}

static void emit_section_sparc(be_gas_section_t section,
                               const ir_entity *entity)
{
	be_gas_section_t base  = section & GAS_SECTION_TYPE_MASK;
	be_gas_section_t flags = section & ~GAS_SECTION_TYPE_MASK;
	static const char *const basename[GAS_SECTION_LAST+1] = {
		"text",
		"data",
		"rodata",
		"bss",
		"ctors",
		"dtors",
		NULL, /* cstring */
		NULL, /* pic trampolines */
		NULL, /* pic symbols */
		"debug_info",
		"debug_abbrev",
		"debug_line",
		"debug_pubnames"
		"debug_frame",
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
	const char *f;
	static const struct {
		const char *name;
		const char *type;
		const char *flags;
	} sectioninfos[GAS_SECTION_LAST+1] = {
		{ "text",           "progbits", "ax" },
		{ "data",           "progbits", "aw" },
		{ "rodata",         "progbits", "a"  },
		{ "bss",            "nobits",   "aw" },
		{ "ctors",          "progbits", "aw" },
		{ "dtors",          "progbits", "aw" },
		{ NULL,             NULL,       NULL }, /* cstring */
		{ NULL,             NULL,       NULL }, /* pic trampolines */
		{ NULL,             NULL,       NULL }, /* pic symbols */
		{ "debug_info",     "progbits", ""   },
		{ "debug_abbrev",   "progbits", ""   },
		{ "debug_line",     "progbits", ""   },
		{ "debug_pubnames", "progbits", ""   },
		{ "debug_frame",    "progbits", ""   },
	};

	if (be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O) {
		emit_section_macho(section);
		return;
	} else if(be_gas_elf_variant == ELF_VARIANT_SPARC) {
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

	assert(base < (be_gas_section_t) ARRAY_SIZE(sectioninfos));
	be_emit_cstring("\t.section\t.");
	/* section name */
	if (flags & GAS_SECTION_FLAG_TLS)
		be_emit_char('t');
	be_emit_string(sectioninfos[base].name);
	if (flags & GAS_SECTION_FLAG_COMDAT) {
		be_emit_char('.');
		be_gas_emit_entity(entity);
	}

	/* section flags */
	be_emit_cstring(",\"");
	for (f = sectioninfos[base].flags; *f != '\0'; ++f) {
		be_emit_char(*f);
	}
	if (flags & GAS_SECTION_FLAG_TLS)
		be_emit_char('T');
	if (flags & GAS_SECTION_FLAG_COMDAT)
		be_emit_char('G');

	/* section type */
	if (be_gas_object_file_format != OBJECT_FILE_FORMAT_COFF) {
		be_emit_cstring("\",");
		be_emit_char(be_gas_elf_type_char);
		be_emit_string(sectioninfos[base].type);
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
	assert(!(section & GAS_SECTION_FLAG_COMDAT));

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

static bool initializer_is_string_const(const ir_initializer_t *initializer,
                                        bool only_suffix_null)
{
	if (initializer->kind != IR_INITIALIZER_COMPOUND)
		return false;

	size_t len = initializer->compound.n_initializers;
	if (len < 1)
		return false;

	bool found_printable = false;
	for (size_t i = 0; i < len; ++i) {
		ir_initializer_t *sub_initializer
			= initializer->compound.initializers[i];

		ir_tarval *tv = get_initializer_tarval(sub_initializer);
		if (!tarval_is_constant(tv))
			return false;

		ir_mode *mode = get_tarval_mode(tv);
		if (!mode_is_int(mode) || get_mode_size_bits(mode) != 8)
			return false;

		int c = get_tarval_long(tv);
		if (i == len-1) {
			if (c != '\0')
				return false;
		} else {
			if (isgraph(c) || isspace(c))
				found_printable = true;
			else if (c != 0 || only_suffix_null)
				return false;
		}
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
		for (size_t i = 0; i < initializer->compound.n_initializers; ++i) {
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
 * @param only_suffix_null  if true '\0' is only legal at the end of the
 *                          string.
 */
static bool entity_is_string_const(const ir_entity *ent, bool only_suffix_null)
{
	/* if it's an array */
	ir_type *type = get_entity_type(ent);
	if (!is_Array_type(type))
		return false;

	/* and the array's element type is primitive */
	ir_type *element_type = get_array_element_type(type);
	if (!is_Primitive_type(element_type))
		return false;

	/* and the mode of the element type is an int of
	 * the same size as the byte mode */
	ir_mode *mode = get_type_mode(element_type);
	if (!mode_is_int(mode) || get_mode_size_bits(mode) != 8)
		return false;

	if (ent->initializer == NULL)
		return false;

	return initializer_is_string_const(ent->initializer, only_suffix_null);
}

static bool entity_is_null(const ir_entity *entity)
{
	ir_initializer_t *initializer = get_entity_initializer(entity);
	return initializer == NULL || initializer_is_null(initializer);
}

static bool is_comdat(const ir_entity *entity)
{
	ir_linkage linkage = get_entity_linkage(entity);
	return (linkage & IR_LINKAGE_MERGE)
		&& (linkage & IR_LINKAGE_GARBAGE_COLLECT);
}

static be_gas_section_t determine_basic_section(const ir_entity *entity)
{
	if (is_method_entity(entity))
		return GAS_SECTION_TEXT;

	ir_linkage linkage = get_entity_linkage(entity);
	if (linkage & IR_LINKAGE_CONSTANT) {
		/* mach-o is the only one with a cstring section */
		if (be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O
		    && entity_is_string_const(entity, true))
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
	ir_linkage const linkage = get_entity_linkage(entity);

	if (linkage & IR_LINKAGE_WEAK) {
		emit_weak(entity);
		/* Note: .weak seems to imply .globl so no need to output .globl */
	} else if (get_entity_visibility(entity) == ir_visibility_external
	           && entity_has_definition(entity)) {
		be_emit_cstring("\t.globl ");
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

void be_gas_emit_function_prolog(const ir_entity *entity, unsigned po2alignment,
                                 const parameter_dbg_info_t *parameter_infos)
{
	be_dwarf_method_before(entity, parameter_infos);

	be_gas_section_t section = determine_section(NULL, entity);
	emit_section(section, entity);

	/* write the begin line (makes the life easier for scripts parsing the
	 * assembler) */
	if (be_options.verbose_asm) {
		be_emit_cstring("# -- Begin  ");
		be_gas_emit_entity(entity);
		be_emit_char('\n');
		be_emit_write_line();
	}

	if (po2alignment > 0) {
		const char *fill_byte    = "";
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

	be_dwarf_method_begin();
}

void be_gas_emit_function_epilog(const ir_entity *entity)
{
	be_dwarf_method_end();

	if (be_gas_object_file_format == OBJECT_FILE_FORMAT_ELF) {
		be_emit_cstring("\t.size\t");
		be_gas_emit_entity(entity);
		be_emit_cstring(", .-");
		be_gas_emit_entity(entity);
		be_emit_char('\n');
		be_emit_write_line();
	}

	if (be_options.verbose_asm) {
		be_emit_cstring("# -- End  ");
		be_gas_emit_entity(entity);
		be_emit_char('\n');
		be_emit_write_line();
	}

	be_emit_char('\n');
	be_emit_write_line();

	next_block_nr += 199;
	next_block_nr -= next_block_nr % 100;
}

/**
 * Output a tarval.
 *
 * @param tv     the tarval
 * @param bytes  the width of the tarvals value in bytes
 */
static void emit_arith_tarval(ir_tarval *tv, unsigned bytes)
{
	switch (bytes) {
	case 1:
		be_emit_irprintf("0x%02x", get_tarval_sub_bits(tv, 0));
		return;

	case 2:
		be_emit_irprintf("0x%02x%02x",
			get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		return;

	case 4:
		be_emit_irprintf("0x%02x%02x%02x%02x",
			get_tarval_sub_bits(tv, 3), get_tarval_sub_bits(tv, 2),
			get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		return;

	case 8:
		be_emit_irprintf("0x%02x%02x%02x%02x%02x%02x%02x%02x",
			get_tarval_sub_bits(tv, 7), get_tarval_sub_bits(tv, 6),
			get_tarval_sub_bits(tv, 5), get_tarval_sub_bits(tv, 4),
			get_tarval_sub_bits(tv, 3), get_tarval_sub_bits(tv, 2),
			get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
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
 * Dump an atomic value.
 *
 * @param env   the gas output environment
 * @param init  a node representing the atomic value (on the const code irg)
 */
static void emit_init_expression(be_gas_decl_env_t *env, ir_node *init)
{
	ir_mode *mode  = get_irn_mode(init);
	int      bytes = get_mode_size_bytes(mode);

	switch (get_irn_opcode(init)) {
	case iro_Id:
		emit_init_expression(env, get_Id_pred(init));
		return;

	case iro_Conv:
		emit_init_expression(env, get_Conv_op(init));
		return;

	case iro_Const: {
		ir_tarval *tv = get_Const_tarval(init);

		/* it's an arithmetic value */
		emit_arith_tarval(tv, bytes);
		return;
	}

	case iro_SymConst:
		switch (get_SymConst_kind(init)) {
		case symconst_addr_ent: {
			ir_entity *ent = get_SymConst_entity(init);
			be_gas_emit_entity(ent);
			break;
		}

		case symconst_ofs_ent: {
			ir_entity *ent = get_SymConst_entity(init);
			be_emit_irprintf("%d", get_entity_offset(ent));
			break;
		}

		case symconst_type_size:
			be_emit_irprintf("%u", get_type_size_bytes(get_SymConst_type(init)));
			break;

		case symconst_type_align:
			be_emit_irprintf("%u", get_type_alignment_bytes(get_SymConst_type(init)));
			break;

		case symconst_enum_const: {
			ir_tarval *tv = get_enumeration_value(get_SymConst_enum(init));
			emit_arith_tarval(tv, bytes);
			break;
		}

		default:
			assert(!"emit_atomic_init(): don't know how to init from this SymConst");
		}
		return;

	case iro_Add:
		if (!mode_is_int(mode) && !mode_is_reference(mode)) {
			panic("Constant must be int or pointer for '+' to work");
		}
		emit_init_expression(env, get_Add_left(init));
		be_emit_cstring(" + ");
		emit_init_expression(env, get_Add_right(init));
		return;

	case iro_Sub:
		if (!mode_is_int(mode) && !mode_is_reference(mode)) {
			panic("Constant must be int or pointer for '-' to work");
		}
		emit_init_expression(env, get_Sub_left(init));
		be_emit_cstring(" - ");
		emit_init_expression(env, get_Sub_right(init));
		return;

	case iro_Mul:
		if (!mode_is_int(mode) && !mode_is_reference(mode)) {
			panic("Constant must be int or pointer for '*' to work");
		}
		emit_init_expression(env, get_Mul_left(init));
		be_emit_cstring(" * ");
		emit_init_expression(env, get_Mul_right(init));
		return;

	case iro_Unknown:
		be_emit_cstring("0");
		return;

	default:
		panic("unsupported IR-node %+F", init);
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
	case 1: be_emit_cstring("\t.byte\t");  break;
	case 2: be_emit_cstring("\t.short\t"); break;
	case 4: be_emit_cstring("\t.long\t");  break;
	case 8: be_emit_cstring("\t.quad\t");  break;

	default:
		panic("Try to dump a type with %u bytes", (unsigned)size);
	}
}

static void emit_string_char(int c)
{
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
			be_emit_irprintf("\\%03o", c);
		break;
	}
}

static size_t emit_string_initializer(const ir_initializer_t *initializer)
{
	be_emit_cstring("\t.asciz \"");

	size_t len = initializer->compound.n_initializers;
	for (size_t i = 0; i < len-1; ++i) {
		const ir_initializer_t *sub_initializer
			= get_initializer_compound_value(initializer, i);

		ir_tarval *tv = get_initializer_tarval(sub_initializer);
		int        c  = get_tarval_long(tv);
		emit_string_char(c);
	}
	be_emit_cstring("\"\n");
	be_emit_write_line();

	return initializer->compound.n_initializers;
}

void be_gas_emit_string_literal(const char *string)
{
	be_emit_char('"');
	for (const char *c = string; *c != '\0'; ++c) {
		emit_string_char(*c);
	}
	be_emit_char('"');
}

void be_gas_emit_cstring(const char *string)
{
	be_emit_cstring("\t.asciz \"");
	for (const char *c = string; *c != '\0'; ++c) {
		emit_string_char(*c);
	}
	be_emit_cstring("\"\n");
	be_emit_write_line();
}

typedef enum normal_or_bitfield_kind {
	NORMAL = 0,
	TARVAL,
	STRING,
	BITFIELD
} normal_or_bitfield_kind;

typedef struct {
	normal_or_bitfield_kind kind;
	ir_type                *type;
	union {
		ir_node                *value;
		ir_tarval              *tarval;
		unsigned char           bf_val;
		const ir_initializer_t *string;
	} v;
} normal_or_bitfield;

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
		if (is_Array_type(type)) {
			if (is_array_variable_size(type)) {
				ir_type   *element_type = get_array_element_type(type);
				unsigned   element_size = get_type_size_bytes(element_type);
				unsigned   element_align
					= get_type_alignment_bytes(element_type);
				unsigned   misalign     = element_size % element_align;
				size_t     n_inits
					= get_initializer_compound_n_entries(initializer);
				element_size += element_align - misalign;
				return n_inits * element_size;
			} else {
				return get_type_size_bytes(type);
			}
		} else {
			assert(is_compound_type(type));
			size_t size = get_type_size_bytes(type);
			if (is_compound_variable_size(type)) {
				/* last initializer has to be an array of variable size */
				size_t l = get_initializer_compound_n_entries(initializer)-1;
				const ir_initializer_t *last
					= get_initializer_compound_value(initializer, l);
				const ir_entity *last_ent  = get_compound_member(type, l);
				ir_type         *last_type = get_entity_type(last_ent);
				assert(is_array_variable_size(last_type));
				size += get_initializer_size(last, last_type);
			}
			return size;
		}
	}

	panic("found invalid initializer");
}

#ifndef NDEBUG
static normal_or_bitfield *glob_vals;
static size_t              max_vals;
#endif

static void emit_bitfield(normal_or_bitfield *vals, unsigned offset_bits,
                          unsigned bitfield_size,
                          const ir_initializer_t *initializer, ir_type *type)
{
	static const size_t BITS_PER_BYTE = 8;

	ir_tarval *tv = NULL;
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
	if (tv == NULL || tv == tarval_bad) {
		panic("Couldn't get numeric value for bitfield initializer");
	}

	int    value_len  = get_type_size_bytes(type);
	size_t bit_offset = 0;
	size_t end        = bitfield_size;
	bool   big_endian = be_get_backend_param()->byte_order_big_endian;
	while (bit_offset < end) {
		size_t src_offset      = bit_offset / BITS_PER_BYTE;
		size_t src_offset_bits = bit_offset % BITS_PER_BYTE;
		size_t dst_offset      = (bit_offset+offset_bits) / BITS_PER_BYTE;
		size_t dst_offset_bits = (bit_offset+offset_bits) % BITS_PER_BYTE;
		size_t src_bits_len    = end-bit_offset;
		size_t dst_bits_len    = BITS_PER_BYTE-dst_offset_bits;
		if (src_bits_len > dst_bits_len)
			src_bits_len = dst_bits_len;

		normal_or_bitfield *val;
		if (big_endian) {
			val = &vals[value_len - dst_offset - 1];
		} else {
			val = &vals[dst_offset];
		}

		assert((val-glob_vals) < (ptrdiff_t) max_vals);
		assert(val->kind == BITFIELD ||
				(val->kind == NORMAL && val->v.value == NULL));
		val->kind = BITFIELD;
		unsigned char curr_bits = get_tarval_sub_bits(tv, src_offset);
		curr_bits = curr_bits >> src_offset_bits;
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
	assert((size_t) (vals - glob_vals) <= max_vals);

	if (initializer_is_string_const(initializer, false)) {
		assert(vals->kind != BITFIELD);
		vals->kind     = STRING;
		vals->v.string = initializer;
		return;
	}

	switch (get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL:
		return;
	case IR_INITIALIZER_TARVAL:
		assert(vals->kind != BITFIELD);
		vals->kind     = TARVAL;
		vals->type     = type;
		vals->v.tarval = get_initializer_tarval_value(initializer);
		assert(get_type_mode(type) == get_tarval_mode(vals->v.tarval));
		for (size_t i = 1; i < get_type_size_bytes(type); ++i) {
			vals[i].kind    = NORMAL;
			vals[i].type    = NULL;
			vals[i].v.value = NULL;
		}
		return;

	case IR_INITIALIZER_CONST:
		assert(vals->kind != BITFIELD);
		vals->kind    = NORMAL;
		vals->type    = type;
		vals->v.value = get_initializer_const_value(initializer);
		for (size_t i = 1; i < get_type_size_bytes(type); ++i) {
			vals[i].kind    = NORMAL;
			vals[i].type    = NULL;
			vals[i].v.value = NULL;
		}
		return;

	case IR_INITIALIZER_COMPOUND:
		if (is_Array_type(type)) {
			ir_type *element_type = get_array_element_type(type);
			size_t   skip         = get_type_size_bytes(element_type);
			size_t   alignment    = get_type_alignment_bytes(element_type);
			size_t   misalign     = skip % alignment;
			if (misalign != 0) {
				skip += alignment - misalign;
			}

			for (size_t i = 0,
			     n = get_initializer_compound_n_entries(initializer);
			     i < n; ++i) {
				ir_initializer_t *sub_initializer
					= get_initializer_compound_value(initializer, i);

				emit_ir_initializer(vals, sub_initializer, element_type);

				vals += skip;
			}
		} else {
			assert(is_compound_type(type));
			for (size_t i = 0, n_members = get_compound_n_members(type);
			     i < n_members; ++i) {
				ir_entity *member = get_compound_member(type, i);
				size_t     offset = get_entity_offset(member);

				assert(i < get_initializer_compound_n_entries(initializer));
				ir_initializer_t *sub_initializer
					= get_initializer_compound_value(initializer, i);

				ir_type *subtype       = get_entity_type(member);
				unsigned bitfield_size = get_entity_bitfield_size(member);
				if (bitfield_size > 0) {
					unsigned offset_bits = get_entity_bitfield_offset(member);
					emit_bitfield(&vals[offset], offset_bits, bitfield_size,
								  sub_initializer, subtype);
					continue;
				}

				emit_ir_initializer(&vals[offset], sub_initializer, subtype);
			}
		}

		return;
	}
	panic("invalid ir_initializer kind found");
}

static void emit_tarval_data(ir_type *type, ir_tarval *tv)
{
	size_t size = get_type_size_bytes(type);
	if (size == 12) {
		/* this should be an x86 extended float */
		assert(be_get_backend_param()->byte_order_big_endian == 0);

		/* Beware: Mixed endian output!  One little endian number emitted as
		 * three longs.  Each long initializer is written in big endian. */
		be_emit_irprintf(
			"\t.long\t0x%02x%02x%02x%02x\n"
			"\t.long\t0x%02x%02x%02x%02x\n"
			"\t.long\t0x%02x%02x%02x%02x\n",
			get_tarval_sub_bits(tv,  3), get_tarval_sub_bits(tv,  2),
			get_tarval_sub_bits(tv,  1), get_tarval_sub_bits(tv,  0),
			get_tarval_sub_bits(tv,  7), get_tarval_sub_bits(tv,  6),
			get_tarval_sub_bits(tv,  5), get_tarval_sub_bits(tv,  4),
			get_tarval_sub_bits(tv, 11), get_tarval_sub_bits(tv, 10),
			get_tarval_sub_bits(tv,  9), get_tarval_sub_bits(tv,  8)
		);
		be_emit_write_line();
	} else if (size == 16) {
		if (be_get_backend_param()->byte_order_big_endian) {
			be_emit_irprintf(
				"\t.long\t0x%02x%02x%02x%02x\n"
				"\t.long\t0x%02x%02x%02x%02x\n"
				"\t.long\t0x%02x%02x%02x%02x\n"
				"\t.long\t0x%02x%02x%02x%02x\n",
				get_tarval_sub_bits(tv, 15), get_tarval_sub_bits(tv, 14),
				get_tarval_sub_bits(tv, 13), get_tarval_sub_bits(tv, 12),
				get_tarval_sub_bits(tv, 11), get_tarval_sub_bits(tv, 10),
				get_tarval_sub_bits(tv,  9), get_tarval_sub_bits(tv,  8),
				get_tarval_sub_bits(tv,  7), get_tarval_sub_bits(tv,  6),
				get_tarval_sub_bits(tv,  5), get_tarval_sub_bits(tv,  4),
				get_tarval_sub_bits(tv,  3), get_tarval_sub_bits(tv,  2),
				get_tarval_sub_bits(tv,  1), get_tarval_sub_bits(tv,  0)
			);
		} else {
			/* Beware: Mixed endian output! One little endian number emitted as
			 * three longs.  Each long initializer is written in big endian. */
			be_emit_irprintf(
				"\t.long\t0x%02x%02x%02x%02x\n"
				"\t.long\t0x%02x%02x%02x%02x\n"
				"\t.long\t0x%02x%02x%02x%02x\n"
				"\t.long\t0x%02x%02x%02x%02x\n",
				get_tarval_sub_bits(tv,  3), get_tarval_sub_bits(tv,  2),
				get_tarval_sub_bits(tv,  1), get_tarval_sub_bits(tv,  0),
				get_tarval_sub_bits(tv,  7), get_tarval_sub_bits(tv,  6),
				get_tarval_sub_bits(tv,  5), get_tarval_sub_bits(tv,  4),
				get_tarval_sub_bits(tv, 11), get_tarval_sub_bits(tv, 10),
				get_tarval_sub_bits(tv,  9), get_tarval_sub_bits(tv,  8),
				get_tarval_sub_bits(tv, 15), get_tarval_sub_bits(tv, 14),
				get_tarval_sub_bits(tv, 13), get_tarval_sub_bits(tv, 12)
			);
		}
		be_emit_write_line();
		return;
	} else {
		/* default case */
		emit_size_type(size);
		emit_arith_tarval(tv, size);
		be_emit_char('\n');
		be_emit_write_line();
	}
}

/**
 * Emit an atomic value.
 *
 * @param env   the gas output environment
 * @param init  a node representing the atomic value (on the const code irg)
 */
static void emit_node_data(be_gas_decl_env_t *env, ir_node *init, ir_type *type)
{
	size_t size = get_type_size_bytes(type);
	if (size == 12 || size == 16) {
		if (!is_Const(init)) {
			panic("12/16byte initializers only support Const nodes yet");
		}
		ir_tarval *tv = get_Const_tarval(init);
		emit_tarval_data(type, tv);
		return;
	}

	emit_size_type(size);
	emit_init_expression(env, init);
	be_emit_char('\n');
	be_emit_write_line();
}

static void emit_initializer(be_gas_decl_env_t *env, const ir_entity *entity)
{
	const ir_initializer_t *initializer = entity->initializer;
	if (initializer_is_string_const(initializer, false)) {
		emit_string_initializer(initializer);
		return;
	}

	ir_type *type = get_entity_type(entity);
	size_t   size = get_initializer_size(initializer, type);
	if (size == 0)
		return;

	/*
	 * In the worst case, every initializer allocates one byte.
	 * Moreover, initializer might be big, do not allocate on stack.
	 */
	normal_or_bitfield *vals = XMALLOCNZ(normal_or_bitfield, size);

#ifndef NDEBUG
	glob_vals = vals;
	max_vals  = size;
#endif

	emit_ir_initializer(vals, initializer, type);

	/* now write values sorted */
	for (size_t k = 0; k < size; ) {
		int                     space     = 0;
		normal_or_bitfield_kind kind      = vals[k].kind;
		int                     elem_size;
		switch (kind) {
		case NORMAL:
			if (vals[k].v.value != NULL) {
				emit_node_data(env, vals[k].v.value, vals[k].type);
				elem_size = get_type_size_bytes(vals[k].type);
			} else {
				elem_size = 0;
			}
			break;
		case TARVAL:
			emit_tarval_data(vals[k].type, vals[k].v.tarval);
			elem_size = get_type_size_bytes(vals[k].type);
			break;
		case STRING:
			elem_size = emit_string_initializer(vals[k].v.string);
			break;
		case BITFIELD:
			be_emit_irprintf("\t.byte\t%d\n", vals[k].v.bf_val);
			be_emit_write_line();
			elem_size = 1;
			break;
		default:
			panic("internal compiler error (invalid normal_or_bitfield_kind");
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
	free(vals);
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

static void emit_indirect_symbol(const ir_entity *entity,
                                 be_gas_section_t section)
{
	/* we can only do PIC code on macho so far */
	assert(be_gas_object_file_format == OBJECT_FILE_FORMAT_MACH_O);

	be_gas_emit_entity(entity);
	be_emit_cstring(":\n");
	be_emit_write_line();
	be_emit_irprintf("\t.indirect_symbol %I\n", get_entity_ident(entity));
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
	if (entity->type == get_code_type()) {
		ir_label_t label = get_entity_label(entity);
		be_emit_irprintf("%s_%lu", be_gas_get_private_prefix(), label);
		return;
	}

	if (get_entity_visibility(entity) == ir_visibility_private) {
		be_emit_string(be_gas_get_private_prefix());
	}
	be_emit_irprintf("%I", get_entity_ld_ident(entity));
}

void be_gas_emit_block_name(const ir_node *block)
{
	ir_entity *entity = get_Block_entity(block);
	if (entity != NULL) {
		be_gas_emit_entity(entity);
	} else {
		void *nr_val = pmap_get(void, block_numbers, block);
		int   nr;
		if (nr_val == NULL) {
			nr = next_block_nr++;
			pmap_insert(block_numbers, block, INT_TO_PTR(nr+1));
		} else {
			nr = PTR_TO_INT(nr_val)-1;
		}
		be_emit_irprintf("%s%d", be_gas_get_private_prefix(), nr);
	}
}

void be_gas_begin_block(const ir_node *block, bool needs_label)
{
	if (needs_label) {
		be_gas_emit_block_name(block);
		be_emit_char(':');
	} else {
		if (!be_options.verbose_asm)
			return;
		be_emit_cstring("/*");
		be_gas_emit_block_name(block);
		be_emit_cstring(":*/");
	}

	if (be_options.verbose_asm) {
		be_emit_pad_comment();
		be_emit_irprintf("/* %+F preds:", block);

		int arity = get_irn_arity(block);
		if (arity == 0) {
			be_emit_cstring(" none");
		} else {
			for (int i = 0; i < arity; ++i) {
				ir_node *predblock = get_Block_cfgpred_block(block, i);
				be_emit_char(' ');
				be_gas_emit_block_name(predblock);
			}
		}
		be_emit_irprintf(", freq: %.3f */", get_block_execfreq(block));
	}
	be_emit_char('\n');
	be_emit_write_line();
}

/**
 * Dump a global entity.
 *
 * @param env  the gas output environment
 * @param ent  the entity to be dumped
 */
static void emit_global(be_gas_decl_env_t *env, const ir_entity *entity)
{
	/* Block labels are already emitted in the code. */
	ir_type *type = get_entity_type(entity);
	if (type == get_code_type())
		return;

	/* we already emitted all methods with graphs in other functions like
	 * be_gas_emit_function_prolog(). All others don't need to be emitted. */
	be_gas_section_t section = determine_section(env, entity);
	if (is_Method_type(type) && section != GAS_SECTION_PIC_TRAMPOLINES) {
		return;
	}

	be_dwarf_variable(entity);

	ir_visibility visibility = get_entity_visibility(entity);
	ir_linkage    linkage    = get_entity_linkage(entity);
	if (section == GAS_SECTION_BSS) {
		switch (visibility) {
		case ir_visibility_local:
		case ir_visibility_private:
			emit_local_common(entity);
			return;
		case ir_visibility_external:
			if (linkage & IR_LINKAGE_MERGE) {
				emit_common(entity);
				return;
			}
			break;
		}
	}

	emit_visibility(entity);

	unsigned alignment = get_effective_entity_alignment(entity);
	if (!is_po2(alignment))
		panic("alignment not a power of 2");

	emit_section(section, entity);

	if (section == GAS_SECTION_PIC_TRAMPOLINES
			|| section == GAS_SECTION_PIC_SYMBOLS) {
		emit_indirect_symbol(entity, section);
		return;
	}

	/* nothing left to do without an initializer */
	if (!entity_has_definition(entity))
		return;

	/* alignment */
	if (alignment > 1) {
		emit_align(alignment);
	}
	if (be_gas_object_file_format == OBJECT_FILE_FORMAT_ELF
	    && be_gas_emit_types && visibility != ir_visibility_private) {
		be_emit_cstring("\t.type\t");
		be_gas_emit_entity(entity);
		be_emit_cstring(", ");
		be_emit_char(be_gas_elf_type_char);
		be_emit_cstring("object\n\t.size\t");\
		be_gas_emit_entity(entity);
		be_emit_irprintf(", %u\n", get_type_size_bytes(type));
	}

	ident *ld_ident = get_entity_ld_ident(entity);
	if (get_id_str(ld_ident)[0] != '\0') {
		be_gas_emit_entity(entity);
		be_emit_cstring(":\n");
		be_emit_write_line();
	}

	if (entity_is_null(entity)) {
		/* we should use .space for stuff in the bss segment */
		unsigned size = get_type_size_bytes(type);
		if (size > 0) {
			be_emit_irprintf("\t.space %u, 0\n", get_type_size_bytes(type));
			be_emit_write_line();
		}
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
	for (size_t i = 0, n = get_compound_n_members(gt); i < n; i++) {
		ir_entity *ent = get_compound_member(gt, i);
		emit_global(env, ent);
	}
}

/* Generate all entities. */
static void emit_global_decls(const be_main_env_t *main_env)
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

void be_emit_jump_table(const ir_node *node, const ir_switch_table *table,
                        ir_entity *entity, get_cfop_target_func get_cfop_target)
{
	/* go over all proj's and collect their jump targets */
	unsigned        n_outs  = arch_get_irn_n_outs(node);
	const ir_node **targets = XMALLOCNZ(const ir_node*, n_outs);
	foreach_out_edge(node, edge) {
		ir_node *proj   = get_edge_src_irn(edge);
		long     pn     = get_Proj_proj(proj);
		ir_node *target = get_cfop_target(proj);
		assert(targets[pn] == NULL);
		targets[pn] = target;
	}

	/* go over table to determine max value (note that we normalized the
	 * ranges so that the minimum is 0) */
	size_t        n_entries = ir_switch_table_get_n_entries(table);
	unsigned long length    = 0;
	for (size_t e = 0; e < n_entries; ++e) {
		const ir_switch_table_entry *entry
			= ir_switch_table_get_entry_const(table, e);
		ir_tarval *max = entry->max;
		unsigned long val;
		if (entry->pn == 0)
			continue;
		if (!tarval_is_long(max))
			panic("switch case overflow (%+F)", node);
		val = (unsigned long) get_tarval_long(max);
		if (val > length) {
			length = val;
		}
	}

	/* the 16000 isn't a real limit of the architecture. But should protect us
	 * from seamingly endless compiler runs */
	if (length > 16000) {
		/* switch lowerer should have broken this monster to pieces... */
		panic("too large switch encountered (%+F)", node);
	}
	++length;

	const ir_node **labels = XMALLOCNZ(const ir_node*, length);
	for (size_t e = 0; e < n_entries; ++e) {
		const ir_switch_table_entry *entry
			= ir_switch_table_get_entry_const(table, e);
		ir_tarval     *min    = entry->min;
		ir_tarval     *max    = entry->max;
		const ir_node *target = targets[entry->pn];
		assert(entry->pn < (long)n_outs);
		if (min == max) {
			unsigned long val = (unsigned long)get_tarval_long(max);
			labels[val] = target;
		} else {
			unsigned long min_val;
			unsigned long max_val;
			unsigned long i;
			if (!tarval_is_long(min))
				panic("switch case overflow (%+F)", node);
			min_val = (unsigned long)get_tarval_long(min);
			max_val = (unsigned long)get_tarval_long(max);
			assert(min_val <= max_val);
			for (i = min_val; i <= max_val; ++i) {
				labels[i] = target;
			}
		}
	}

	/* emit table */
	unsigned pointer_size = get_mode_size_bytes(mode_P);
	if (entity != NULL) {
		be_gas_emit_switch_section(GAS_SECTION_RODATA);
		be_emit_irprintf("\t.align %u\n", pointer_size);
		be_gas_emit_entity(entity);
		be_emit_cstring(":\n");
	}

	for (unsigned long i = 0; i < length; ++i) {
		const ir_node *block = labels[i];
		if (block == NULL)
			block = targets[0];
		emit_size_type(pointer_size);
		be_gas_emit_block_name(block);
		be_emit_char('\n');
		be_emit_write_line();
	}

	if (entity != NULL)
		be_gas_emit_switch_section(GAS_SECTION_TEXT);

	free(labels);
	free(targets);
}

static void emit_global_asms(void)
{
	be_gas_emit_switch_section(GAS_SECTION_TEXT);
	for (size_t i = 0, n = get_irp_n_asms(); i < n; ++i) {
		ident *asmtext = get_irp_asm(i);

		be_emit_cstring("#APP\n");
		be_emit_write_line();
		be_emit_irprintf("%I\n", asmtext);
		be_emit_write_line();
		be_emit_cstring("#NO_APP\n");
		be_emit_write_line();
	}
}

void be_gas_begin_compilation_unit(const be_main_env_t *env)
{
	be_dwarf_open();
	be_dwarf_unit_begin(env->cup_name);

	block_numbers = pmap_create();
	next_block_nr = 0;

	emit_global_asms();
}

void be_gas_end_compilation_unit(const be_main_env_t *env)
{
	emit_global_decls(env);

	pmap_destroy(block_numbers);

	be_dwarf_unit_end();
	be_dwarf_close();
}
