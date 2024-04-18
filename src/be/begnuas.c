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

#include "be_t.h"
#include "bearch.h"
#include "beemithlp.h"
#include "beemitter.h"
#include "bemodule.h"
#include "betranshlp.h"
#include "dbginfo.h"
#include "entity_t.h"
#include "execfreq.h"
#include "iredges_t.h"
#include "irnode_t.h"
#include "irtools.h"
#include "lc_opts_enum.h"
#include "panic.h"
#include "platform_t.h"
#include "util.h"
#include <assert.h>
#include <ctype.h>

/** by default, we generate assembler code for GNU as */
elf_variant_t          be_gas_elf_variant   = ELF_VARIANT_NORMAL;
bool                   be_gas_emit_types    = true;
char                   be_gas_elf_type_char = '@';

static be_gas_section_t current_section = (be_gas_section_t) -1;
static pmap            *block_numbers;
static unsigned         next_block_nr;

static bool is_macho(void)
{
	return ir_platform.object_format == OBJECT_FORMAT_MACH_O;
}

static void emit_section_macho(be_gas_section_t const section)
{
	if (current_section == section)
		return;
	current_section = section;

	be_gas_section_t const base  = section &  GAS_SECTION_TYPE_MASK;
	be_gas_section_t const flags = section & ~GAS_SECTION_TYPE_MASK;

	static char const *const macho_shortforms[] = {
		[GAS_SECTION_CONSTRUCTORS] = "mod_init_func",
		[GAS_SECTION_DESTRUCTORS]  = "mod_term_func",
	};
	if (flags == 0 && base < ARRAY_SIZE(macho_shortforms)) {
		char const *const shortform = macho_shortforms[base];
		if (shortform) {
			be_emit_irprintf("\t.%s\n", shortform);
			be_emit_write_line();
			return;
		}
	}

	typedef struct {
		const char *segment_section;
		const char *flags;
	} macho_sectioninfo_t;

	static const macho_sectioninfo_t macho_sectioninfos[] = {
		[GAS_SECTION_TEXT]            = { "__TEXT,__text",            "regular,pure_instructions" },
		[GAS_SECTION_DATA]            = { "__DATA,__data",            NULL },
		[GAS_SECTION_RODATA]          = { "__TEXT,__const",           NULL },
		[GAS_SECTION_REL_RO]          = { "__DATA,__const",           NULL },
		[GAS_SECTION_REL_RO_LOCAL]    = { "__DATA,__const",           NULL },
		[GAS_SECTION_BSS]             = { "__DATA,__bss",             NULL },
		[GAS_SECTION_CSTRING]         = { "__TEXT,__cstring",         "cstring_literals" },
		[GAS_SECTION_PIC_TRAMPOLINES] = { "__IMPORT,__jump_table",    "symbol_stubs,self_modifying_code+pure_instructions,5" },
		[GAS_SECTION_PIC_SYMBOLS]     = { "__IMPORT,__pointers",      "non_lazy_symbol_pointers" },
		[GAS_SECTION_DEBUG_INFO]      = { "__DWARF,__debug_info",     "regular,debug" },
		[GAS_SECTION_DEBUG_ABBREV]    = { "__DWARF,__debug_abbrev",   "regular,debug" },
		[GAS_SECTION_DEBUG_LINE]      = { "__DWARF,__debug_line",     "regular,debug" },
		[GAS_SECTION_DEBUG_PUBNAMES]  = { "__DWARF,__debug_pubnames", "regular,debug" },
		[GAS_SECTION_DEBUG_FRAME]     = { "__DWARF,__debug_frame",    "regular,debug" },
	};
	static const macho_sectioninfo_t macho_sectioninfos_coalesce[] = {
		[GAS_SECTION_TEXT]    = { "__TEXT,__textcoal_nt", "coalesced,pure_instructions" },
		[GAS_SECTION_DATA]    = { "__DATA,__datacoal_nt", "coalesced" },
		[GAS_SECTION_BSS]     = { "__DATA,__datacoal_nt", "coalesced" },
		[GAS_SECTION_RODATA]  = { "__TEXT,__const_coal",  "coalesced" },
		[GAS_SECTION_CSTRING] = { "__TEXT,__const_coal",  "coalesced" },
	};

	if (flags & GAS_SECTION_FLAG_TLS)
		panic("thread local storage not supported on macho (section 0x%X)", section);

	macho_sectioninfo_t const *info;
	if (flags & GAS_SECTION_FLAG_COMDAT) {
		assert(base < ARRAY_SIZE(macho_sectioninfos_coalesce));
		info = &macho_sectioninfos_coalesce[base];
	} else {
		assert(base < ARRAY_SIZE(macho_sectioninfos));
		info = &macho_sectioninfos[base];
	}
	if (!info->segment_section)
		panic("Unsupported macho section requested");

	be_emit_irprintf("\t.section\t%s", info->segment_section);
	if (info->flags)
		be_emit_irprintf(",%s", info->flags);
	be_emit_char('\n');
	be_emit_write_line();
}

typedef struct {
	const char *name;
	const char *type;
	const char *flags;
} elf_sectioninfo_t;

static const elf_sectioninfo_t elf_sectioninfos[] = {
	[GAS_SECTION_TEXT]           = { "text",              "progbits", "ax" },
	[GAS_SECTION_DATA]           = { "data",              "progbits", "aw" },
	[GAS_SECTION_RODATA]         = { "rodata",            "progbits", "a"  },
	[GAS_SECTION_REL_RO_LOCAL]   = { "data.rel.ro.local", "progbits", "aw" },
	[GAS_SECTION_REL_RO]         = { "data.rel.ro",       "progbits", "aw" },
	[GAS_SECTION_BSS]            = { "bss",               "nobits",   "aw" },
	[GAS_SECTION_CONSTRUCTORS]   = { "ctors",             "progbits", "aw" },
	[GAS_SECTION_DESTRUCTORS]    = { "dtors",             "progbits", "aw" },
	[GAS_SECTION_JCR]            = { "jcr",               "progbits", "aw" },
	[GAS_SECTION_DEBUG_INFO]     = { "debug_info",        "progbits", ""   },
	[GAS_SECTION_DEBUG_ABBREV]   = { "debug_abbrev",      "progbits", ""   },
	[GAS_SECTION_DEBUG_LINE]     = { "debug_line",        "progbits", ""   },
	[GAS_SECTION_DEBUG_PUBNAMES] = { "debug_pubnames",    "progbits", ""   },
	[GAS_SECTION_DEBUG_FRAME]    = { "debug_frame",       "progbits", ""   },
};

static void emit_section_sparc(be_gas_section_t section,
                               const ir_entity *entity)
{
	if (current_section == section && !(section & GAS_SECTION_FLAG_COMDAT))
		return;
	current_section = section;

	be_gas_section_t const base  = section &  GAS_SECTION_TYPE_MASK;
	be_gas_section_t const flags = section & ~GAS_SECTION_TYPE_MASK;

	be_emit_cstring("\t.section\t\".");

	/* Part1: section-name */
	if (flags & GAS_SECTION_FLAG_TLS)
		be_emit_char('t');
	assert((size_t)base < ARRAY_SIZE(elf_sectioninfos));
	char const *const name = elf_sectioninfos[base].name;
	assert(name != NULL);
	be_emit_string(name);

	if (flags & GAS_SECTION_FLAG_COMDAT) {
		be_emit_char('.');
		be_gas_emit_entity(entity);
	}
	be_emit_char('"');

	/* for the simple sections we're done here */
	if (flags != 0) {
		be_emit_cstring(",#alloc");

		switch (base) {
		case GAS_SECTION_TEXT: be_emit_cstring(",#execinstr"); break;
		case GAS_SECTION_DATA:
		case GAS_SECTION_BSS:  be_emit_cstring(",#write"); break;
		default:               /* nothing */ break;
		}
		if (flags & GAS_SECTION_FLAG_TLS)
			be_emit_cstring(",#tls");
	}

	be_emit_char('\n');
	be_emit_write_line();
}

static void emit_section_elf_coff(be_gas_section_t const section, ir_entity const *const entity)
{
	if (current_section == section && !(section & GAS_SECTION_FLAG_COMDAT))
		return;
	current_section = section;

	be_gas_section_t const base  = section &  GAS_SECTION_TYPE_MASK;
	be_gas_section_t const flags = section & ~GAS_SECTION_TYPE_MASK;

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

	assert((size_t)base < ARRAY_SIZE(elf_sectioninfos));
	elf_sectioninfo_t const *const info = &elf_sectioninfos[base];
	assert(info->name != NULL);
	be_emit_cstring("\t.section\t.");
	/* section name */
	if (flags & GAS_SECTION_FLAG_TLS)
		be_emit_char('t');
	be_emit_string(info->name);
	if (flags & GAS_SECTION_FLAG_COMDAT) {
		be_emit_char('.');
		be_gas_emit_entity(entity);
	}

	/* section flags */
	be_emit_irprintf(",\"%s", info->flags);
	if (flags & GAS_SECTION_FLAG_TLS)
		be_emit_char('T');
	if (flags & GAS_SECTION_FLAG_COMDAT)
		be_emit_char('G');

	/* section type */
	if (ir_platform.object_format != OBJECT_FORMAT_PE_COFF)
		be_emit_irprintf("\",%c%s", be_gas_elf_type_char, info->type);

	if (flags & GAS_SECTION_FLAG_COMDAT) {
		be_emit_char(',');
		be_gas_emit_entity(entity);
		be_emit_cstring(",comdat");
	}
	be_emit_char('\n');
	be_emit_write_line();
}

static void emit_section(be_gas_section_t const section, ir_entity const *const entity)
{
	if (is_macho()) {
		emit_section_macho(section);
	} else if (be_gas_elf_variant == ELF_VARIANT_SPARC) {
		emit_section_sparc(section, entity);
	} else {
		emit_section_elf_coff(section, entity);
	}
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
	return get_tarval_unknown();
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

	ir_initializer_t const *const init = get_entity_initializer(ent);
	if (!init)
		return false;

	return initializer_is_string_const(init, only_suffix_null);
}

static bool entity_is_zero_initialized(ir_entity const *entity)
{
	if (is_alias_entity(entity))
		return false;
	ir_initializer_t *initializer = get_entity_initializer(entity);
	return initializer != NULL && initializer_is_null(initializer);
}

static bool is_comdat(const ir_entity *entity)
{
	ir_linkage linkage = get_entity_linkage(entity);
	return (linkage & IR_LINKAGE_MERGE)
		&& (linkage & IR_LINKAGE_GARBAGE_COLLECT);
}

typedef enum reloc_class_t {
	NO_RELOCATIONS,
	ONLY_LOCAL_RELOCATIONS,
	ANY_RELOCATIONS,
} reloc_class_t;

static reloc_class_t classify_expr_relocs(ir_node const *const node)
{
	switch (get_irn_opcode(node)) {
	case iro_Conv:
		return classify_expr_relocs(get_Conv_op(node));
	case iro_Address: {
		ir_entity const *const entity     = get_Address_entity(node);
		ir_visibility    const visibility = get_entity_visibility(entity);
		return visibility == ir_visibility_local
		    || visibility == ir_visibility_private ? ONLY_LOCAL_RELOCATIONS
		                                           : ANY_RELOCATIONS;
	}
	case iro_Offset:
	case iro_Align:
	case iro_Const:
	case iro_Size:
	case iro_Unknown:
		return NO_RELOCATIONS;
	case iro_Add:
	case iro_Sub:
	case iro_Mul:
		return MAX(classify_expr_relocs(get_binop_left(node)),
		           classify_expr_relocs(get_binop_right(node)));
	default:
		panic("unsupported IR-node %+F in initializer", node);
	}
}

static reloc_class_t classify_initializer_relocs(
		ir_initializer_t const *const init)
{
	switch (get_initializer_kind(init)) {
	case IR_INITIALIZER_NULL:
	case IR_INITIALIZER_TARVAL:
		return NO_RELOCATIONS;
	case IR_INITIALIZER_CONST:
		return classify_expr_relocs(get_initializer_const_value(init));
	case IR_INITIALIZER_COMPOUND: {
		reloc_class_t result = NO_RELOCATIONS;
		for (size_t i = 0, n = get_initializer_compound_n_entries(init); i < n;
		     ++i) {
			ir_initializer_t const *const subinit
				= get_initializer_compound_value(init, i);
			result = MAX(result, classify_initializer_relocs(subinit));
		}
		return result;
	}
	}
	panic("invalid initializer");
}

static be_gas_section_t determine_basic_section(const ir_entity *entity)
{
	if (is_method_entity(entity) || is_alias_entity(entity))
		return GAS_SECTION_TEXT;

	if (get_entity_linkage(entity) & IR_LINKAGE_CONSTANT) {
		/* mach-o is the only one with a cstring section */
		if (is_macho()
		    && get_entity_linkage(entity) & IR_LINKAGE_NO_IDENTITY
		    && entity_is_string_const(entity, true))
			return GAS_SECTION_CSTRING;

		if (ir_platform.pic_style != BE_PIC_NONE) {
			ir_initializer_t const *const init = get_entity_initializer(entity);
			switch (classify_initializer_relocs(init)) {
			case ONLY_LOCAL_RELOCATIONS: return GAS_SECTION_REL_RO_LOCAL;
			case ANY_RELOCATIONS:        return GAS_SECTION_REL_RO;
			case NO_RELOCATIONS:         return GAS_SECTION_RODATA;
			}
			panic("invalid relocation class");
		} else {
			return GAS_SECTION_RODATA;
		}
	}
	if (entity_is_zero_initialized(entity))
		return GAS_SECTION_BSS;

	return GAS_SECTION_DATA;
}

static be_gas_section_t determine_section(be_main_env_t const *const main_env, ir_entity const *const entity)
{
	ir_type *owner = get_entity_owner(entity);

	if (owner == get_segment_type(IR_SEGMENT_GLOBAL)) {
		be_gas_section_t section = determine_basic_section(entity);
		if (is_comdat(entity))
			section |= GAS_SECTION_FLAG_COMDAT;
		return section;
	} else if (main_env && owner == main_env->pic_symbols_type) {
		return GAS_SECTION_PIC_SYMBOLS;
	} else if (main_env && owner == main_env->pic_trampolines_type) {
		return GAS_SECTION_PIC_TRAMPOLINES;
	} else if (owner == get_segment_type(IR_SEGMENT_CONSTRUCTORS)) {
		return GAS_SECTION_CONSTRUCTORS;
	} else if (owner == get_segment_type(IR_SEGMENT_DESTRUCTORS)) {
		return GAS_SECTION_DESTRUCTORS;
	} else if (owner == get_segment_type(IR_SEGMENT_JCR)) {
		return GAS_SECTION_JCR;
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

	panic("couldn't determine section for %+F", entity);
}

static void emit_symbol_directive(const char *directive,
                                  const ir_entity *entity)
{
	be_emit_irprintf("\t%s ", directive);
	be_gas_emit_entity(entity);
	be_emit_char('\n');
	be_emit_write_line();
}

static void emit_weak(const ir_entity *entity)
{
	char const *const directive =
		!is_macho()                   ? ".weak" :
		entity_has_definition(entity) ? ".weak_definition" :
		/*                           */ ".weak_reference";
	emit_symbol_directive(directive, entity);
}

static const char *get_visibility_directive(const ir_entity *entity,
                                            bool *output_global)
{
	switch (get_entity_visibility(entity)) {
	case ir_visibility_external: return NULL;
	case ir_visibility_external_private: {
		switch (ir_platform.object_format) {
		case OBJECT_FORMAT_MACH_O:
			*output_global = false;
			return ".private_extern";
		case OBJECT_FORMAT_ELF:
			return ".hidden";
		case OBJECT_FORMAT_PE_COFF:
			panic("ir_visibility_external_private not supported for COFF");
		}
		panic("invalid object file format");
	}
	case ir_visibility_external_protected: {
		switch (ir_platform.object_format) {
		case OBJECT_FORMAT_MACH_O:
			return NULL;
		case OBJECT_FORMAT_ELF:
			return ".protected";
		case OBJECT_FORMAT_PE_COFF:
			panic("ir_visibility_external_protected not supported for COFF");
		}
		panic("invalid object file format");
	}
	case ir_visibility_local:
	case ir_visibility_private:
		*output_global = false;
		return NULL;
	}
	panic("invalid visibility");
}

static void emit_visibility(const ir_entity *entity, bool implicit_globl)
{
	ir_linkage const linkage = get_entity_linkage(entity);

	if (linkage & IR_LINKAGE_WEAK)
		emit_weak(entity);

	bool output_global = !implicit_globl;
	const char *const directive
		= get_visibility_directive(entity, &output_global);
	if (output_global)
		emit_symbol_directive(".globl", entity);
	if (directive != NULL)
		emit_symbol_directive(directive, entity);

	if (is_macho()
	    && (linkage & IR_LINKAGE_HIDDEN_USER)
	    && get_entity_ld_name(entity)[0] != '\0') {
		emit_symbol_directive(".no_dead_strip", entity);
	}
}

void be_gas_emit_function_prolog(const ir_entity *entity, unsigned po2alignment,
                                 const parameter_dbg_info_t *parameter_infos)
{
	be_dwarf_function_before(entity, parameter_infos);

	be_gas_section_t const section = determine_section(NULL, entity);
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
		/* gcc fills space between function with 0x90... */
		char const *const fill_byte    = is_macho() ? "0x90" : "";
		unsigned    const maximum_skip = (1U << po2alignment) - 1;
		be_emit_irprintf("\t.p2align %u,%s,%u\n", po2alignment, fill_byte, maximum_skip);
		be_emit_write_line();
	}
	emit_visibility(entity, false);

	switch (ir_platform.object_format) {
	case OBJECT_FORMAT_ELF:
		be_emit_cstring("\t.type\t");
		be_gas_emit_entity(entity);
		be_emit_irprintf(", %cfunction\n", be_gas_elf_type_char);
		be_emit_write_line();
		break;
	case OBJECT_FORMAT_PE_COFF:
		be_emit_cstring("\t.def\t");
		be_gas_emit_entity(entity);
		char const vis = get_entity_visibility(entity) == ir_visibility_local ? '3' : '2';
		be_emit_irprintf(";\t.scl\t%c;\t.type\t32;\t.endef\n", vis);
		be_emit_write_line();
		break;
	case OBJECT_FORMAT_MACH_O:
		if (section == (GAS_SECTION_TEXT | GAS_SECTION_FLAG_COMDAT))
			emit_symbol_directive(".weak_definition", entity);
		break;
	}
	be_gas_emit_entity(entity);
	be_emit_cstring(":\n");
	be_emit_write_line();

	be_dwarf_function_begin();
}

void be_gas_emit_function_epilog(ir_entity const *const entity)
{
	be_dwarf_function_end();

	if (ir_platform.object_format == OBJECT_FORMAT_ELF) {
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
 * Output parts of a tarval.
 *
 * @param tv      The tarval
 * @param offset  The byte offst to start at
 * @param n       How many bytes to output
 */
static void emit_tv(ir_tarval *const tv, unsigned const offset, unsigned const n)
{
	be_emit_cstring("0x");
	for (unsigned i = n; i-- != 0;) {
		be_emit_irprintf("%02x", get_tarval_sub_bits(tv, offset + i));
	}
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
 * @param init  a node representing the atomic value (on the const code irg)
 */
static void emit_init_expression(ir_node *const init)
{
	ir_mode *mode = get_irn_mode(init);

	switch (get_irn_opcode(init)) {
	case iro_Conv:
		emit_init_expression(get_Conv_op(init));
		return;

	case iro_Const: {
		/* it's an arithmetic value */
		int        const bytes = get_mode_size_bytes(mode);
		ir_tarval *const tv    = get_Const_tarval(init);
		emit_tv(tv, 0, bytes);
		return;
	}

	case iro_Address:
		be_gas_emit_entity(get_Address_entity(init));
		return;

	case iro_Offset:
		be_emit_irprintf("%d", get_entity_offset(get_Offset_entity(init)));
		return;

	case iro_Align:
		be_emit_irprintf("%u", get_type_alignment(get_Align_type(init)));
		return;

	case iro_Size:
		be_emit_irprintf("%u", get_type_size(get_Size_type(init)));
		return;

	case iro_Add:
		if (!be_mode_needs_gp_reg(mode))
			panic("constant must be int or pointer for '+' to work");
		emit_init_expression(get_Add_left(init));
		be_emit_cstring(" + ");
		emit_init_expression(get_Add_right(init));
		return;

	case iro_Sub:
		if (!be_mode_needs_gp_reg(mode))
			panic("constant must be int or pointer for '-' to work");
		emit_init_expression(get_Sub_left(init));
		be_emit_cstring(" - ");
		emit_init_expression(get_Sub_right(init));
		return;

	case iro_Mul:
		if (!mode_is_int(mode))
			panic("constant must be int for '*' to work");
		emit_init_expression(get_Mul_left(init));
		be_emit_cstring(" * ");
		emit_init_expression(get_Mul_right(init));
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
		panic("try to dump a type with %u bytes", (unsigned)size);
	}
}

static void emit_string_char(int c)
{
	switch (c) {
	case '"' : be_emit_cstring("\\\""); break;
	case '\n': be_emit_cstring("\\n");  break;
	case '\r': be_emit_cstring("\\r");  break;
	case '\t': be_emit_cstring("\\t");  break;
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

	size_t const len = initializer->compound.n_initializers - 1;
	for (size_t i = 0; i < len; ++i) {
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
	be_emit_cstring("\t.asciz ");
	be_gas_emit_string_literal(string);
	be_emit_char('\n');
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
		return get_type_size(type);
	case IR_INITIALIZER_CONST:
	case IR_INITIALIZER_NULL:
		return get_type_size(type);
	case IR_INITIALIZER_COMPOUND:
		if (is_Array_type(type)) {
			if (get_array_size(type) == 0) {
				ir_type  *element_type  = get_array_element_type(type);
				unsigned  element_size  = get_type_size(element_type);
				unsigned  element_align = get_type_alignment(element_type);
				unsigned  misalign      = element_size % element_align;
				size_t    n_inits
					= get_initializer_compound_n_entries(initializer);
				element_size += element_align - misalign;
				return n_inits * element_size;
			} else {
				return get_type_size(type);
			}
		} else {
			assert(is_compound_type(type));
			size_t size = get_type_size(type);
			/* Last initializer may be an array of flexible size. */
			size_t const n = get_initializer_compound_n_entries(initializer);
			if (n != 0) {
				ir_entity *const last_ent  = get_compound_member(type, n - 1);
				ir_type   *const last_type = get_entity_type(last_ent);
				if (is_Array_type(last_type) && get_array_size(last_type) == 0) {
					ir_initializer_t const *const last = get_initializer_compound_value(initializer, n - 1);
					size += get_initializer_size(last, last_type);
				}
			}
			return size;
		}
	}

	panic("found invalid initializer");
}

static unsigned long compute_entity_size(ir_entity const *const entity)
{
	ir_type *const type = get_entity_type(entity);
	unsigned long  size = get_type_size(type);
	if (is_alias_entity(entity))
		return size;

	/* Note that for variable array/compound types we may have to inspect the
	 * initializer to get the actual size */
	ir_initializer_t const *const initializer = get_entity_initializer(entity);
	if (initializer != NULL)
		size = get_initializer_size(initializer, type);
	return size;
}

#ifndef NDEBUG
static normal_or_bitfield *glob_vals;
static unsigned long       max_vals;
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
		ir_node *const node = get_initializer_const_value(initializer);
		if (!is_Const(node))
			panic("bitfield initializer not a Const node");
		tv = get_Const_tarval(node);
		break;
	}
	case IR_INITIALIZER_COMPOUND:
		panic("bitfield initializer is compound");
	}
	if (!tv || tv == tarval_bad)
		panic("couldn't get numeric value for bitfield initializer");

	int    value_len  = get_type_size(type);
	size_t bit_offset = 0;
	size_t end        = bitfield_size;
	bool   big_endian = ir_target_big_endian();
	while (bit_offset < end) {
		size_t src_offset      = bit_offset / BITS_PER_BYTE;
		size_t src_offset_bits = bit_offset % BITS_PER_BYTE;
		size_t dst_offset      = (bit_offset + offset_bits) / BITS_PER_BYTE;
		size_t dst_offset_bits = (bit_offset + offset_bits) % BITS_PER_BYTE;
		size_t src_bits_len    = end - bit_offset;
		size_t dst_bits_len    = BITS_PER_BYTE - dst_offset_bits;
		src_bits_len = MIN(dst_bits_len, src_bits_len);

		normal_or_bitfield *const val = big_endian ?
			&vals[value_len - dst_offset - 1] :
			&vals[dst_offset];

		assert(val - glob_vals < (ptrdiff_t)max_vals);
		assert(val->kind == BITFIELD || (val->kind == NORMAL && !val->v.value));
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
	assert((unsigned long) (vals - glob_vals) <= max_vals);

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
		for (size_t i = 1; i < get_type_size(type); ++i) {
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
		for (size_t i = 1; i < get_type_size(type); ++i) {
			vals[i].kind    = NORMAL;
			vals[i].type    = NULL;
			vals[i].v.value = NULL;
		}
		return;

	case IR_INITIALIZER_COMPOUND:
		if (is_Array_type(type)) {
			ir_type *element_type = get_array_element_type(type);
			size_t   skip         = get_type_size(element_type);
			size_t   alignment    = get_type_alignment(element_type);
			size_t   misalign     = skip % alignment;
			if (misalign != 0)
				skip += alignment - misalign;

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
	size_t size = get_type_size(type);
	if (size > 8) {
		assert(size % 4 == 0);
		if (ir_target_big_endian()) {
			for (unsigned i = size; i != 0;) {
				emit_size_type(4);
				emit_tv(tv, i -= 4, 4);
				be_emit_char('\n');
			}
		} else {
			/* Beware: Mixed endian output! One little endian number emitted as
			 * longs.  Each long initializer is written in big endian. */
			for (unsigned i = 0; i != size; i += 4) {
				emit_size_type(4);
				emit_tv(tv, i, 4);
				be_emit_char('\n');
			}
		}
	} else {
		/* default case */
		emit_size_type(size);
		emit_tv(tv, 0, size);
		be_emit_char('\n');
	}
	be_emit_write_line();
}

/**
 * Emit an atomic value.
 *
 * @param init  a node representing the atomic value (on the const code irg)
 */
static void emit_node_data(ir_node *const init, ir_type *const type)
{
	size_t const size = get_type_size(type);
	if (size == 12 || size == 16) {
		if (!is_Const(init))
			panic("12/16byte initializers only support Const nodes yet");
		ir_tarval *const tv = get_Const_tarval(init);
		emit_tarval_data(type, tv);
		return;
	}

	emit_size_type(size);
	emit_init_expression(init);
	be_emit_char('\n');
	be_emit_write_line();
}

static void emit_initializer(ir_entity const *const entity,
                             unsigned long const size)
{
	ir_initializer_t const *const initializer = get_entity_initializer(entity);
	if (initializer_is_string_const(initializer, false)) {
		emit_string_initializer(initializer);
		return;
	}

	assert(size > 0);

	/* In the worst case, every initializer allocates one byte.
	 * Moreover, initializer might be big, do not allocate on stack. */
	normal_or_bitfield *const vals = XMALLOCNZ(normal_or_bitfield, size);

#ifndef NDEBUG
	glob_vals = vals;
	max_vals  = size;
#endif

	ir_type *const type = get_entity_type(entity);
	emit_ir_initializer(vals, initializer, type);

	/* now write values sorted */
	for (size_t k = 0; k < size;) {
		int elem_size;
		switch (vals[k].kind) {
		case NORMAL:
			if (vals[k].v.value != NULL) {
				emit_node_data(vals[k].v.value, vals[k].type);
				elem_size = get_type_size(vals[k].type);
			} else {
				elem_size = 0;
			}
			break;
		case TARVAL:
			emit_tarval_data(vals[k].type, vals[k].v.tarval);
			elem_size = get_type_size(vals[k].type);
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
		int space = 0;
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
		alignment     = get_type_alignment(type);
	}
	return alignment;
}

static void emit_common(const ir_entity *entity, unsigned long size,
                        bool is_local)
{
	unsigned const alignment = get_effective_entity_alignment(entity);

	switch (ir_platform.object_format) {
	case OBJECT_FORMAT_MACH_O:
		assert(!is_local);
		be_emit_cstring("\t.comm ");
		be_gas_emit_entity(entity);
		be_emit_irprintf(",%lu,%u\n", size, log2_floor(alignment));
		be_emit_write_line();
		return;
	case OBJECT_FORMAT_ELF:
		if (is_local)
			emit_symbol_directive(".local", entity);
		be_emit_cstring("\t.comm ");
		be_gas_emit_entity(entity);
		be_emit_irprintf(",%lu,%u\n", size, alignment);
		be_emit_write_line();
		return;
	case OBJECT_FORMAT_PE_COFF:
		be_emit_string(is_local ? "\t.lcomm " : "\t.comm ");
		be_gas_emit_entity(entity);
		be_emit_irprintf(",%lu # %u\n", size, alignment);
		be_emit_write_line();
		return;
	}
	panic("invalid object file format");
}

static void emit_zerofill(const ir_entity *entity, const char *section_segment,
                          unsigned long const size)
{
	be_emit_cstring("\t.zerofill\t");
	be_emit_string(section_segment);
	be_emit_char(',');
	be_gas_emit_entity(entity);
	unsigned const alignment = get_effective_entity_alignment(entity);
	be_emit_irprintf(",%lu,%u\n", size, log2_floor(alignment));
	be_emit_write_line();
}

static void emit_indirect_symbol(const ir_entity *entity,
                                 be_gas_section_t section)
{
	if (!is_macho())
		panic("indirect_symbol entities only supported for ELF");

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

static void emit_alias(const ir_entity *entity)
{
	if (ir_platform.object_format != OBJECT_FORMAT_ELF)
		panic("alias entities only supported for ELF");

	be_emit_cstring("\t.set ");
	be_gas_emit_entity(entity);
	be_emit_char(',');
	be_gas_emit_entity(get_entity_alias(entity));
	be_emit_char('\n');
	be_emit_write_line();
}

char const *be_gas_get_private_prefix(void)
{
	return is_macho() ? "L" : ".L";
}

static bool check_needs_quotes(char const *const s)
{
	if (is_macho()) {
		for (char const *i = s; *i != '\0'; ++i) {
			if ((unsigned char)*i >= 128)
				return true;
		}
	}
	return false;
}

void be_gas_emit_entity(const ir_entity *entity)
{
	if (entity->kind == IR_ENTITY_LABEL) {
		ir_label_t label = get_entity_label(entity);
		be_emit_irprintf("%s_%lu", be_gas_get_private_prefix(), label);
		return;
	}

	char const *const name         = get_entity_ld_name(entity);
	bool        const needs_quotes = check_needs_quotes(name);
	if (needs_quotes)
		be_emit_char('"');
	if (get_entity_visibility(entity) == ir_visibility_private)
		be_emit_string(be_gas_get_private_prefix());
	be_emit_string(name);
	if (needs_quotes)
		be_emit_char('"');
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
			pmap_insert(block_numbers, block, INT_TO_PTR(nr + 1));
		} else {
			nr = PTR_TO_INT(nr_val) - 1;
		}
		be_emit_irprintf("%s%d", be_gas_get_private_prefix(), nr);
	}
}

static bool block_needs_label(ir_node const *const block)
{
	if (get_Block_entity(block))
		return true;

	int const n_cfgpreds = get_Block_n_cfgpreds(block);
	if (n_cfgpreds == 0) {
		return false;
	} else if (n_cfgpreds == 1) {
		ir_node *const cfgpred       = get_Block_cfgpred(block, 0);
		ir_node *const cfgpred_block = get_nodes_block(cfgpred);
		if (be_emit_get_prev_block(block) != cfgpred_block)
			return true;

		if (is_x_regular_Proj(cfgpred))
			return false;

		ir_node const *const pred = skip_Proj(cfgpred);
		return !(arch_get_irn_flags(pred) & arch_irn_flag_fallthrough);
	} else {
		return true;
	}
}

void be_gas_begin_block(ir_node const *const block)
{
	if (block_needs_label(block)) {
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
				if (predblock == NULL)
					continue;
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
 */
static void emit_global(be_main_env_t const *const main_env,
                        ir_entity const *const entity)
{
	ir_entity_kind kind = get_entity_kind(entity);

	/* Block labels are already emitted in the code. */
	if (kind == IR_ENTITY_LABEL)
		return;

	/* we already emitted all functions with graphs in other functions like
	 * be_gas_emit_function_prolog(). All others don't need to be emitted. */
	be_gas_section_t const section = determine_section(main_env, entity);
	if (kind == IR_ENTITY_METHOD && section != GAS_SECTION_PIC_TRAMPOLINES)
		return;

	if (section == GAS_SECTION_PIC_TRAMPOLINES
	 || section == GAS_SECTION_PIC_SYMBOLS) {
		emit_section(section, entity);
		emit_indirect_symbol(entity, section);
		return;
	}

	be_dwarf_variable(entity);

	ir_visibility const visibility       = get_entity_visibility(entity);
	ir_linkage    const linkage          = get_entity_linkage(entity);
	bool          const zero_initializer = entity_is_zero_initialized(entity);
	unsigned long       size             = compute_entity_size(entity);

	/* We need to output at least 1 byte, otherwise macho will merge
	 * the label with the next thing */
	if (size == 0)
		size = 1;

	if ((linkage & IR_LINKAGE_MERGE || zero_initializer)
	  && !(section & GAS_SECTION_FLAG_TLS)) {
		switch (visibility) {
		case ir_visibility_external:
		case ir_visibility_external_private:
		case ir_visibility_external_protected:
			if (linkage & IR_LINKAGE_MERGE) {
				emit_visibility(entity, true);
				emit_common(entity, size, false);
				return;
			} else if (is_macho() && !(linkage & IR_LINKAGE_CONSTANT)) {
				emit_visibility(entity, false);
				emit_zerofill(entity, "__DATA,__common", size);
				return;
			}
			break;
		case ir_visibility_local:
		case ir_visibility_private:
			if (!(linkage & IR_LINKAGE_CONSTANT)) {
				switch (ir_platform.object_format) {
				case OBJECT_FORMAT_ELF:
				case OBJECT_FORMAT_PE_COFF:
					emit_visibility(entity, true);
					emit_common(entity, size, true);
					return;

				case OBJECT_FORMAT_MACH_O:
					emit_visibility(entity, false);
					emit_zerofill(entity, "__DATA,__bss", size);
					return;
				}
			}
			break;
		}
	}

	/* nothing left to do without an initializer */
	if (!entity_has_definition(entity))
		return;

	emit_section(section, entity);
	emit_visibility(entity, false);

	if (kind == IR_ENTITY_ALIAS) {
		emit_alias(entity);
		return;
	}

	/* alignment */
	unsigned alignment = get_effective_entity_alignment(entity);
	if (!is_po2_or_zero(alignment))
		panic("alignment not a power of 2");
	if (alignment > 1)
		emit_align(alignment);
	if (ir_platform.object_format == OBJECT_FORMAT_ELF
		&& be_gas_emit_types && visibility != ir_visibility_private) {
		be_emit_cstring("\t.type\t");
		be_gas_emit_entity(entity);
		be_emit_irprintf(", %cobject\n\t.size\t", be_gas_elf_type_char);
		be_gas_emit_entity(entity);
		ir_type *const type = get_entity_type(entity);
		be_emit_irprintf(", %u\n", get_type_size(type));
	}

	ident *ld_ident = get_entity_ld_ident(entity);
	if (get_id_str(ld_ident)[0] != '\0') {
		be_gas_emit_entity(entity);
		be_emit_cstring(":\n");
		be_emit_write_line();
	}

	if (!zero_initializer) {
		emit_initializer(entity, size);
	} else if (size > 0) {
		/* use .space for stuff in the bss segment */
		be_emit_irprintf("\t.space %u, 0\n", size);
		be_emit_write_line();
	}
}

/**
 * Dumps declarations of global variables and the initialization code.
 *
 * @param gt                a global like type, either the global or the TLS one
 */
static void be_gas_emit_globals(ir_type *const gt, be_main_env_t const *const main_env)
{
	for (size_t i = 0, n = get_compound_n_members(gt); i < n; i++) {
		ir_entity *const ent = get_compound_member(gt, i);
		if (!(get_entity_linkage(ent) & IR_LINKAGE_NO_CODEGEN))
			emit_global(main_env, ent);
	}
}

/* Generate all entities. */
static void emit_global_decls(be_main_env_t const *const main_env)
{
	be_gas_emit_globals(get_glob_type(), main_env);
	be_gas_emit_globals(get_tls_type(), main_env);
	be_gas_emit_globals(get_segment_type(IR_SEGMENT_CONSTRUCTORS), main_env);
	be_gas_emit_globals(get_segment_type(IR_SEGMENT_DESTRUCTORS), main_env);
	be_gas_emit_globals(get_segment_type(IR_SEGMENT_JCR), main_env);
	be_gas_emit_globals(main_env->pic_symbols_type, main_env);
	be_gas_emit_globals(main_env->pic_trampolines_type, main_env);

	/**
	 * ".subsections_via_symbols marks object files which are OK to divide
	 * their section contents into individual blocks".
	 * From my understanding this means no label points in the middle of an
	 * object which we want to address as a whole. Firm code should be fine
	 * with this.
	 */
	if (is_macho()) {
		be_emit_cstring("\t.subsections_via_symbols\n");
		be_emit_write_line();
	}
}

void be_emit_jump_table(ir_node const *const node, be_switch_attr_t const *const swtch, ir_mode *const entry_mode, emit_target_func const emit_target)
{
	/* go over all proj's and collect their jump targets */
	unsigned        n_outs  = arch_get_irn_n_outs(node);
	const ir_node **targets = XMALLOCNZ(const ir_node*, n_outs);
	foreach_out_edge(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		unsigned pn   = get_Proj_num(proj);
		targets[pn]   = proj;
	}

	ir_switch_table const *const table = swtch->table;

	/* go over table to determine max value (note that we normalized the
	 * ranges so that the minimum is 0) */
	size_t        n_entries = ir_switch_table_get_n_entries(table);
	unsigned long length    = 0;
	for (size_t e = 0; e < n_entries; ++e) {
		const ir_switch_table_entry *entry
			= ir_switch_table_get_entry_const(table, e);
		ir_tarval *max = entry->max;
		if (entry->pn == 0)
			continue;
		if (!tarval_is_long(max))
			panic("switch case overflow (%+F)", node);
		unsigned long const val = (unsigned long)get_tarval_long(max);
		length = MAX(length, val);
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
		assert(entry->pn < n_outs);
		if (min == max) {
			unsigned long val = (unsigned long)get_tarval_long(max);
			labels[val] = target;
		} else {
			if (!tarval_is_long(min))
				panic("switch case overflow (%+F)", node);
			unsigned long const min_val = (unsigned long)get_tarval_long(min);
			unsigned long const max_val = (unsigned long)get_tarval_long(max);
			assert(min_val <= max_val);
			for (unsigned long i = min_val; i <= max_val; ++i) {
				labels[i] = target;
			}
		}
	}

	/* emit table */
	unsigned         const pointer_size = get_mode_size_bytes(entry_mode);
	ir_entity const *const entity       = swtch->table_entity;
	if (entity) {
		if (!is_macho())
			be_gas_emit_switch_section(GAS_SECTION_RODATA);
		be_emit_irprintf("\t.align %u\n", pointer_size);
		be_gas_emit_entity(entity);
		be_emit_cstring(":\n");
	}

	for (unsigned long i = 0; i < length; ++i) {
		ir_node const *target = labels[i];
		if (target == NULL)
			target = targets[0];
		emit_size_type(pointer_size);
		emit_target(entity, target);
		be_emit_char('\n');
		be_emit_write_line();
	}

	if (entity && !is_macho())
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

bool be_gas_produces_dwarf_line_info(void)
{
	return ir_platform.object_format == OBJECT_FORMAT_ELF;
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

void be_emit_finish_line_gas(const ir_node *node)
{
	if (node && be_options.verbose_asm) {
		be_emit_pad_comment();
		dbg_info   *const dbg = get_irn_dbg_info(node);
		src_loc_t   const loc = ir_retrieve_dbg_info(dbg);
		char const *const fmt =
			!loc.file       ? "/* %+F */\n"       :
			loc.line   == 0 ? "/* %+F %s */\n"    :
			loc.column == 0 ? "/* %+F %s:%u */\n" :
			/*             */ "/* %+F %s:%u:%u */\n";
		be_emit_irprintf(fmt, node, loc.file, loc.line, loc.column);
	} else {
		be_emit_char('\n');
	}
	be_emit_write_line();
}
