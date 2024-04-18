/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   DWARF debugging info support
 * @author  Matthias Braun
 */
#include "bedwarf_t.h"

#include "array.h"
#include "bearch.h"
#include "beemitter.h"
#include "begnuas.h"
#include "bemodule.h"
#include "dbginfo.h"
#include "irprog.h"
#include "irtools.h"
#include "lc_opts.h"
#include "lc_opts_enum.h"
#include "obst.h"
#include "obst.h"
#include "panic.h"
#include "pmap.h"
#include "pset_new.h"
#include "target_t.h"
#include "tv.h"
#include "typerep.h"
#include "util.h"
#include "xmalloc.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

enum {
	LEVEL_NONE,
	LEVEL_BASIC,
	LEVEL_LOCATIONS,
	LEVEL_FRAMEINFO
};
static int debug_level = LEVEL_NONE;

static bool   has_cfi_sections = true;

/**
 * Usually we simply use the DW_TAG_xxx numbers for our abbrev IDs, but for
 * the cases where we need multiple ids with the same DW_TAG we define new IDs
 * here
 */
typedef enum custom_abbrevs {
	abbrev_void_subprogram = 1,
	abbrev_subprogram,
	abbrev_formal_parameter,
	abbrev_unnamed_formal_parameter,
	abbrev_formal_parameter_no_location,
	abbrev_variable,
	abbrev_compile_unit,
	abbrev_base_type,
	abbrev_pointer_type,
	abbrev_void_pointer_type,
	abbrev_array_type,
	abbrev_subrange_type,
	abbrev_structure_type,
	abbrev_union_type,
	abbrev_class_type,
	abbrev_member,
	abbrev_bitfield_member,
	abbrev_subroutine_type,
	abbrev_void_subroutine_type,
} custom_abbrevs;

/**
 * The dwarf handle.
 */
typedef struct dwarf_t {
	const ir_entity  *cur_ent;      /**< current function entity */
	unsigned          next_type_nr; /**< next type number */
	pmap             *file_map;     /**< a map from file names to number in
	                                     file list */
	const char      **file_list;
	const ir_entity **pubnames_list;
	pset_new_t        emitted_types;
	const char       *main_file;    /**< name of the main source file */
	const char       *curr_file;    /**< name of the current source file */
	unsigned          label_num;
	unsigned          last_line;
} dwarf_t;

static dwarf_t               env;
static dwarf_source_language language;
static char                 *comp_dir;

static unsigned insert_file(const char *filename)
{
	void *entry = pmap_get(void, env.file_map, filename);
	if (entry != NULL) {
		return PTR_TO_INT(entry);
	}
	ARR_APP1(const char*, env.file_list, filename);
	unsigned num = (unsigned)ARR_LEN(env.file_list);
	pmap_insert(env.file_map, filename, INT_TO_PTR(num));

	be_emit_irprintf("\t.file %u ", num);
	be_gas_emit_string_literal(filename);
	be_emit_char('\n');
	be_emit_write_line();
	return num;
}

static void emit_int32(uint32_t value)
{
	be_emit_irprintf("\t.long %u\n", value);
	be_emit_write_line();
}

static void emit_int16(uint16_t value)
{
	be_emit_irprintf("\t.short %u\n", value);
	be_emit_write_line();
}

static void emit_int8(uint8_t value)
{
	be_emit_irprintf("\t.byte %u\n", value);
	be_emit_write_line();
}

static void emit_uleb128(unsigned value)
{
	be_emit_irprintf("\t.uleb128 0x%x\n", value);
	be_emit_write_line();
}

static unsigned get_uleb128_size(unsigned value)
{
	unsigned size = 0;
	do {
		value >>= 7;
		size += 1;
	} while (value != 0);
	return size;
}

static void emit_sleb128(long value)
{
	be_emit_irprintf("\t.sleb128 %ld\n", value);
	be_emit_write_line();
}

static unsigned get_sleb128_size(long value)
{
	unsigned size = 0;
	do {
		value >>= 7;
		size += 1;
	} while (value != 0 && value != -1);
	return size;
}

static void emit_ref(const ir_entity *entity)
{
	const char *directive = ir_target_pointer_size() == 8 ? "\t.quad "
	                                                    : "\t.long ";
	be_emit_string(directive);
	be_gas_emit_entity(entity);
	be_emit_char('\n');
	be_emit_write_line();
}

static void emit_string_printf(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	be_emit_cstring("\t.asciz \"");
	be_emit_irvprintf(fmt, ap);
	be_emit_cstring("\"\n");
	va_end(ap);

	be_emit_write_line();
}

static void emit_address(const char *name)
{
	be_emit_cstring("\t.long ");
	be_emit_string(be_gas_get_private_prefix());
	be_emit_string(name);
	be_emit_char('\n');
	be_emit_write_line();
}

static void emit_size(const char *from_label, const char *to_label)
{
	be_emit_cstring("\t.long ");
	be_emit_string(be_gas_get_private_prefix());
	be_emit_string(to_label);
	be_emit_cstring(" - ");
	be_emit_string(be_gas_get_private_prefix());
	be_emit_string(from_label);
	be_emit_char('\n');
	be_emit_write_line();
}

static void emit_label(const char *name)
{
	be_emit_string(be_gas_get_private_prefix());
	be_emit_string(name);
	be_emit_cstring(":\n");
	be_emit_write_line();
}

static void register_attribute(dwarf_attribute attribute, dwarf_form form)
{
	emit_uleb128(attribute);
	emit_uleb128(form);
}

static void begin_abbrev(custom_abbrevs code, dwarf_tag tag,
                         dw_children children)
{
	emit_uleb128(code);
	emit_uleb128(tag);
	emit_int8(children);
}

static void end_abbrev(void)
{
	emit_uleb128(0);
	emit_uleb128(0);
}

static void emit_line_info(void)
{
	be_gas_emit_switch_section(GAS_SECTION_DEBUG_LINE);

	emit_label("line_section_begin");
	/* on some systems gas handles producing the line info for us, and we don't
	 * have to do anything */
	if (!be_gas_produces_dwarf_line_info()) {
		emit_size("line_info_begin", "line_info_end");

		emit_label("line_info_begin");
		emit_int16(2); /* version */
		emit_size("line_info_prolog_begin", "line_info_prolog_end");
		emit_label("line_info_prolog_begin");
		emit_int8(1); /* len of smallest instruction TODO: query from backend */
		emit_int8(1); /* default is statement */
		emit_int8(246); /* line base */
		emit_int8(245); /* line range */
		emit_int8(10); /* opcode base */

		emit_uleb128(0);
		emit_uleb128(1);
		emit_uleb128(1);
		emit_uleb128(1);
		emit_uleb128(1);
		emit_uleb128(0);
		emit_uleb128(0);
		emit_uleb128(0);
		emit_uleb128(1);

		/* include directory list */
		emit_int8(0);

		/* file list */
		for (size_t i = 0; i < ARR_LEN(env.file_list); ++i) {
			be_gas_emit_cstring(env.file_list[i]);
			emit_uleb128(1); /* directory */
			emit_uleb128(0); /* modification time */
			emit_uleb128(0); /* file length */
		}
		emit_int8(0);

		emit_label("line_info_prolog_end");

		/* TODO: put the line_info program here */

		emit_label("line_info_end");
	}
}

static void emit_pubnames(void)
{
	be_gas_emit_switch_section(GAS_SECTION_DEBUG_PUBNAMES);

	emit_size("pubnames_begin", "pubnames_end");
	emit_label("pubnames_begin");

	emit_int16(2); /* version */
	emit_size("info_section_begin", "info_begin");
	emit_size("compile_unit_begin", "compile_unit_end");

	for (size_t i = 0; i < ARR_LEN(env.pubnames_list); ++i) {
		const ir_entity *entity = env.pubnames_list[i];
		be_emit_irprintf("\t.long %sE%ld - %sinfo_begin\n",
		                 be_gas_get_private_prefix(),
		                 get_entity_nr(entity), be_gas_get_private_prefix());
		be_gas_emit_cstring(get_entity_name(entity));
	}
	emit_int32(0);

	emit_label("pubnames_end");
}

void be_dwarf_location(dbg_info *dbgi)
{
	if (debug_level < LEVEL_LOCATIONS)
		return;
	src_loc_t loc = ir_retrieve_dbg_info(dbgi);
	if (!loc.file)
		return;

	unsigned filenum = insert_file(loc.file);
	be_emit_irprintf("\t.loc %u %u %u\n", filenum, loc.line, loc.column);
	be_emit_write_line();
}

void be_dwarf_callframe_register(const arch_register_t *reg)
{
	if (debug_level < LEVEL_FRAMEINFO)
		return;
	be_emit_cstring("\t.cfi_def_cfa_register ");
	be_emit_irprintf("%d\n", reg->dwarf_number);
	be_emit_write_line();
}

void be_dwarf_callframe_offset(int offset)
{
	if (debug_level < LEVEL_FRAMEINFO)
		return;
	be_emit_cstring("\t.cfi_def_cfa_offset ");
	be_emit_irprintf("%d\n", offset);
	be_emit_write_line();
}

void be_dwarf_callframe_spilloffset(const arch_register_t *reg, int offset)
{
	if (debug_level < LEVEL_FRAMEINFO)
		return;
	be_emit_cstring("\t.cfi_offset ");
	be_emit_irprintf("%d, %d\n", reg->dwarf_number, offset);
	be_emit_write_line();
}

static bool is_extern_entity(const ir_entity *entity)
{
	ir_visited_t visibility = get_entity_visibility(entity);
	return visibility == ir_visibility_external;
}

static void emit_entity_label(const ir_entity *entity)
{
	be_emit_irprintf("%sE%ld:\n", be_gas_get_private_prefix(),
	                 get_entity_nr(entity));
	be_emit_write_line();
}

static void register_dbginfo_attributes(void)
{
	register_attribute(DW_AT_decl_file,   DW_FORM_udata);
	register_attribute(DW_AT_decl_line,   DW_FORM_udata);
	register_attribute(DW_AT_decl_column, DW_FORM_udata);
}

static void emit_dbginfo(const dbg_info *dbgi)
{
	src_loc_t const loc  = ir_retrieve_dbg_info(dbgi);
	unsigned  const file = loc.file ? insert_file(loc.file) : 0;
	emit_uleb128(file);
	emit_uleb128(loc.line);
	emit_uleb128(loc.column);
}

static void emit_type_address(const ir_type *type)
{
	be_emit_irprintf("\t.long %sT%ld - %sinfo_begin\n",
	                 be_gas_get_private_prefix(),
	                 get_type_nr(type), be_gas_get_private_prefix());
	be_emit_write_line();
}

static void emit_subprogram_abbrev(void)
{
	begin_abbrev(abbrev_subprogram, DW_TAG_subprogram, DW_CHILDREN_yes);
	register_attribute(DW_AT_name,      DW_FORM_string);
	register_dbginfo_attributes();
	register_attribute(DW_AT_type,       DW_FORM_ref4);
	register_attribute(DW_AT_external,   DW_FORM_flag);
	register_attribute(DW_AT_low_pc,     DW_FORM_addr);
	register_attribute(DW_AT_high_pc,    DW_FORM_addr);
	//register_attribute(DW_AT_prototyped, DW_FORM_flag);
	if (debug_level >= LEVEL_FRAMEINFO)
		register_attribute(DW_AT_frame_base, DW_FORM_block1);
	end_abbrev();

	begin_abbrev(abbrev_void_subprogram, DW_TAG_subprogram, DW_CHILDREN_yes);
	register_attribute(DW_AT_name,       DW_FORM_string);
	register_dbginfo_attributes();
	register_attribute(DW_AT_external,   DW_FORM_flag);
	register_attribute(DW_AT_low_pc,     DW_FORM_addr);
	register_attribute(DW_AT_high_pc,    DW_FORM_addr);
	//register_attribute(DW_AT_prototyped, DW_FORM_flag);
	if (debug_level >= LEVEL_FRAMEINFO)
		register_attribute(DW_AT_frame_base, DW_FORM_block1);
	end_abbrev();

	begin_abbrev(abbrev_formal_parameter, DW_TAG_formal_parameter,
	             DW_CHILDREN_no);
	register_attribute(DW_AT_name,      DW_FORM_string);
	register_dbginfo_attributes();
	register_attribute(DW_AT_type,      DW_FORM_ref4);
	register_attribute(DW_AT_location,  DW_FORM_block1);
	end_abbrev();

	begin_abbrev(abbrev_formal_parameter_no_location, DW_TAG_formal_parameter,
	             DW_CHILDREN_no);
	register_attribute(DW_AT_name,      DW_FORM_string);
	register_dbginfo_attributes();
	register_attribute(DW_AT_type,      DW_FORM_ref4);
	end_abbrev();
}

static void emit_type(ir_type *type);

static void emit_stack_location(long offset)
{
	unsigned size = 1 + get_sleb128_size(offset);
	emit_int8(size);
	emit_int8(DW_OP_fbreg);
	emit_sleb128(offset);
}

static void emit_function_parameters(const ir_entity *entity,
                                     const parameter_dbg_info_t *infos)
{
	const ir_type *type = get_entity_type(entity);
	dbg_info      *dbgi = get_entity_dbg_info(entity);
	for (size_t i = 0, n_params = get_method_n_params(type);
	     i < n_params; ++i) {
		ir_type *param_type = get_method_param_type(type, i);

		if (infos != NULL && infos[i].entity != NULL) {
			long const offset = get_entity_offset(infos[i].entity);
			emit_uleb128(abbrev_formal_parameter);
			emit_string_printf("arg%u", (unsigned)i);
			emit_dbginfo(dbgi);
			emit_type_address(param_type);
			emit_stack_location(offset);
		} else {
			emit_uleb128(abbrev_formal_parameter_no_location);
			emit_string_printf("arg%u", (unsigned)i);
			emit_dbginfo(dbgi);
			emit_type_address(param_type);
		}
	}
}

void be_dwarf_function_before(const ir_entity *entity,
                              const parameter_dbg_info_t *parameter_infos)
{
	if (debug_level < LEVEL_BASIC)
		return;

	be_gas_emit_switch_section(GAS_SECTION_DEBUG_INFO);

	ir_type *type   = get_entity_type(entity);
	size_t   n_ress = get_method_n_ress(type);
	if (n_ress > 0) {
		ir_type *res = get_method_res_type(type, 0);
		emit_type(res);
	}
	for (size_t i = 0, n_params = get_method_n_params(type);
	     i < n_params; ++i) {
		ir_type *param_type = get_method_param_type(type, i);
		emit_type(param_type);
	}

	emit_entity_label(entity);
	emit_uleb128(n_ress == 0 ? abbrev_void_subprogram : abbrev_subprogram);
	be_gas_emit_cstring(get_entity_ld_name(entity));
	emit_dbginfo(get_entity_dbg_info(entity));
	if (n_ress > 0) {
		ir_type *res = get_method_res_type(type, 0);
		emit_type_address(res);
	}
	emit_int8(is_extern_entity(entity));
	emit_ref(entity);
	const char *directive = ir_target_pointer_size() == 8 ? ".quad" : ".long";
	be_emit_irprintf("\t%s %sfunction_end_%s\n", directive,
	                 be_gas_get_private_prefix(), get_entity_ld_name(entity));
	/* frame_base prog */
	emit_int8(1);
	emit_int8(DW_OP_call_frame_cfa);

	emit_function_parameters(entity, parameter_infos);
	emit_int8(0);

	ARR_APP1(const ir_entity*, env.pubnames_list, entity);

	env.cur_ent = entity;
}

void be_dwarf_function_begin(void)
{
	if (debug_level < LEVEL_FRAMEINFO)
		return;
	be_emit_cstring("\t.cfi_startproc\n");
	be_emit_write_line();
}

void be_dwarf_function_end(void)
{
	if (debug_level < LEVEL_BASIC)
		return;
	const ir_entity *entity = env.cur_ent;
	be_emit_irprintf("%sfunction_end_%s:\n", be_gas_get_private_prefix(),
	                 get_entity_ld_name(entity));

	if (debug_level >= LEVEL_FRAMEINFO) {
		be_emit_cstring("\t.cfi_endproc\n");
		be_emit_write_line();
	}
}

static void emit_base_type_abbrev(void)
{
	begin_abbrev(abbrev_base_type, DW_TAG_base_type, DW_CHILDREN_no);
	register_attribute(DW_AT_encoding,  DW_FORM_data1);
	register_attribute(DW_AT_byte_size, DW_FORM_data1);
	register_attribute(DW_AT_name,      DW_FORM_string);
	end_abbrev();
}

static void emit_type_label(const ir_type *type)
{
	be_emit_irprintf("%sT%ld:\n", be_gas_get_private_prefix(),
	                 get_type_nr(type));
	be_emit_write_line();
}

static void emit_base_type(const ir_type *type)
{
	char buf[128];
	ir_print_type(buf, sizeof(buf), type);

	emit_type_label(type);
	emit_uleb128(abbrev_base_type);
	ir_mode *mode = get_type_mode(type);
	if (mode_is_int(mode)) {
		/* bool hack */
		if (streq(buf, "_Bool") || streq(buf, "bool")) {
			emit_int8(DW_ATE_boolean);
		} else {
			emit_int8(mode_is_signed(mode) ? DW_ATE_signed : DW_ATE_unsigned);
		}
	} else if (mode_is_reference(mode)) {
		emit_int8(DW_ATE_address);
	} else if (mode_is_float(mode)) {
		emit_int8(DW_ATE_float);
	} else {
		panic("mode not implemented yet");
	}
	emit_int8(get_mode_size_bytes(mode));
	be_gas_emit_cstring(buf);
}

static void emit_pointer_type_abbrev(void)
{
	begin_abbrev(abbrev_pointer_type, DW_TAG_pointer_type, DW_CHILDREN_no);
	register_attribute(DW_AT_type,      DW_FORM_ref4);
	register_attribute(DW_AT_byte_size, DW_FORM_data1);
	end_abbrev();

	/* for void* pointer s*/
	begin_abbrev(abbrev_void_pointer_type, DW_TAG_pointer_type, DW_CHILDREN_no);
	register_attribute(DW_AT_byte_size, DW_FORM_data1);
	end_abbrev();
}

static void emit_pointer_type(const ir_type *type)
{
	ir_type *points_to = get_pointer_points_to_type(type);
	unsigned size      = get_type_size(type);
	assert(size < 256);

	if (!is_Primitive_type(points_to) || get_type_mode(points_to) != mode_ANY) {
		emit_type(points_to);

		emit_type_label(type);
		emit_uleb128(abbrev_pointer_type);
		emit_type_address(points_to);
	} else {
		emit_type_label(type);
		emit_uleb128(abbrev_void_pointer_type);
	}
	emit_int8(size);
}

static void emit_array_type_abbrev(void)
{
	begin_abbrev(abbrev_array_type, DW_TAG_array_type, DW_CHILDREN_yes);
	register_attribute(DW_AT_type, DW_FORM_ref4);
	end_abbrev();

	begin_abbrev(abbrev_subrange_type, DW_TAG_subrange_type, DW_CHILDREN_no);
	register_attribute(DW_AT_upper_bound, DW_FORM_udata);
	end_abbrev();
}

static void emit_array_type(const ir_type *type)
{
	ir_type *element_type = get_array_element_type(type);
	emit_type(element_type);

	emit_type_label(type);
	emit_uleb128(abbrev_array_type);
	emit_type_address(element_type);

	unsigned const bound = get_array_size(type);
	if (bound != 0) {
		emit_uleb128(abbrev_subrange_type);
		emit_uleb128(bound);
	}

	emit_uleb128(0);
}

static void emit_compound_type_abbrev(void)
{
	begin_abbrev(abbrev_structure_type, DW_TAG_structure_type, DW_CHILDREN_yes);
	register_attribute(DW_AT_byte_size,  DW_FORM_udata);
	// TODO register_dbginfo_attributes();
	end_abbrev();

	begin_abbrev(abbrev_union_type, DW_TAG_union_type, DW_CHILDREN_yes);
	register_attribute(DW_AT_byte_size,  DW_FORM_udata);
	// TODO register_dbginfo_attributes();
	end_abbrev();

	begin_abbrev(abbrev_class_type, DW_TAG_class_type, DW_CHILDREN_yes);
	register_attribute(DW_AT_byte_size,  DW_FORM_udata);
	// TODO register_dbginfo_attributes();
	end_abbrev();

	begin_abbrev(abbrev_member, DW_TAG_member, DW_CHILDREN_no);
	register_attribute(DW_AT_type,                 DW_FORM_ref4);
	register_attribute(DW_AT_name,                 DW_FORM_string);
	register_dbginfo_attributes();
	register_attribute(DW_AT_data_member_location, DW_FORM_block1);
	end_abbrev();

	begin_abbrev(abbrev_bitfield_member, DW_TAG_member, DW_CHILDREN_no);
	register_attribute(DW_AT_byte_size,            DW_FORM_udata);
	register_attribute(DW_AT_bit_size,             DW_FORM_udata);
	register_attribute(DW_AT_bit_offset,           DW_FORM_udata);
	register_attribute(DW_AT_type,                 DW_FORM_ref4);
	register_attribute(DW_AT_name,                 DW_FORM_string);
	register_dbginfo_attributes();
	register_attribute(DW_AT_data_member_location, DW_FORM_block1);
	end_abbrev();
}

static void emit_op_plus_uconst(unsigned value)
{
	emit_int8(DW_OP_plus_uconst);
	emit_uleb128(value);
}

static void emit_compound_type(const ir_type *type)
{
	size_t n_members = get_compound_n_members(type);
	for (size_t i = 0; i < n_members; ++i) {
		ir_entity *member      = get_compound_member(type, i);
		ir_type   *member_type = get_entity_type(member);
		emit_type(member_type);
	}

	emit_type_label(type);
	if (is_Struct_type(type)) {
		emit_uleb128(abbrev_structure_type);
	} else if (is_Union_type(type)) {
		emit_uleb128(abbrev_union_type);
	} else {
		assert(is_Class_type(type));
		emit_uleb128(abbrev_class_type);
	}
	emit_uleb128(get_type_size(type));
	for (size_t i = 0; i < n_members; ++i) {
		ir_entity *member      = get_compound_member(type, i);
		ir_type   *member_type = get_entity_type(member);
		int        offset      = get_entity_offset(member);

		if (get_entity_bitfield_size(member) > 0) {
			unsigned bit_offset = get_entity_bitfield_offset(member);
			unsigned bit_size   = get_entity_bitfield_size(member);
			unsigned base_size  = get_type_size(member_type);

			bit_offset = base_size*8 - bit_offset - bit_size;

			emit_uleb128(abbrev_bitfield_member);
			emit_uleb128(base_size);
			emit_uleb128(bit_size);
			emit_uleb128(bit_offset);
		} else {
			emit_uleb128(abbrev_member);
		}

		emit_type_address(member_type);
		be_gas_emit_cstring(get_entity_name(member));
		emit_dbginfo(get_entity_dbg_info(member));
		assert(offset >= 0);
		emit_int8(1 + get_uleb128_size(offset));
		emit_op_plus_uconst(offset);
	}

	emit_int8(0);
}

static void emit_subroutine_type_abbrev(void)
{
	begin_abbrev(abbrev_subroutine_type,
	             DW_TAG_subroutine_type, DW_CHILDREN_yes);
	register_attribute(DW_AT_prototyped,   DW_FORM_flag);
	register_attribute(DW_AT_type,         DW_FORM_ref4);
	end_abbrev();

	begin_abbrev(abbrev_void_subroutine_type,
	             DW_TAG_subroutine_type, DW_CHILDREN_yes);
	register_attribute(DW_AT_prototyped,   DW_FORM_flag);
	end_abbrev();

	begin_abbrev(abbrev_unnamed_formal_parameter,
	             DW_TAG_formal_parameter, DW_CHILDREN_no);
	register_attribute(DW_AT_type,  DW_FORM_ref4);
	end_abbrev();
}

static void emit_subroutine_type(const ir_type *type)
{
	size_t n_params = get_method_n_params(type);
	size_t n_ress   = get_method_n_ress(type);
	for (size_t i = 0; i < n_params; ++i) {
		ir_type *param_type = get_method_param_type(type, i);
		emit_type(param_type);
	}
	for (size_t i = 0; i < n_ress; ++i) {
		ir_type *res_type = get_method_res_type(type, i);
		emit_type(res_type);
	}

	emit_type_label(type);
	emit_uleb128(n_ress == 0 ? abbrev_void_subroutine_type
	                         : abbrev_subroutine_type);
	emit_int8(1); /* prototyped */
	if (n_ress > 0) {
		/* dwarf only supports 1 return type */
		ir_type *res_type = get_method_res_type(type, 0);
		emit_type_address(res_type);
	}

	for (size_t i = 0; i < n_params; ++i) {
		ir_type *param_type = get_method_param_type(type, i);
		emit_uleb128(abbrev_unnamed_formal_parameter);
		emit_type_address(param_type);
	}
	emit_int8(0);
}

static void emit_type(ir_type *type)
{
	if (!pset_new_insert(&env.emitted_types, type))
		return;

	switch (get_type_opcode(type)) {
	case tpo_primitive: emit_base_type(type);       return;
	case tpo_pointer:   emit_pointer_type(type);    return;
	case tpo_array:     emit_array_type(type);      return;
	case tpo_class:
	case tpo_struct:
	case tpo_union:     emit_compound_type(type);   return;
	case tpo_method:    emit_subroutine_type(type); return;
	case tpo_segment:
	case tpo_code:
	case tpo_unknown:
	case tpo_uninitialized:
		panic("unexpected type %+F", type);
	}
	panic("invalid type");
}

static void emit_op_addr(const ir_entity *entity)
{
	emit_int8(DW_OP_addr);
	be_emit_cstring("\t.long ");
	be_gas_emit_entity(entity);
	be_emit_char('\n');
	be_emit_write_line();
}

static void emit_variable_abbrev(void)
{
	begin_abbrev(abbrev_variable, DW_TAG_variable, DW_CHILDREN_no);
	register_attribute(DW_AT_name,      DW_FORM_string);
	register_attribute(DW_AT_type,      DW_FORM_ref4);
	register_attribute(DW_AT_external,  DW_FORM_flag);
	register_dbginfo_attributes();
	register_attribute(DW_AT_location,  DW_FORM_block1);
	end_abbrev();
}

void be_dwarf_variable(const ir_entity *entity)
{
	if (debug_level < LEVEL_BASIC)
		return;
	if (get_entity_ld_name(entity)[0] == '\0')
		return;
	if (!entity_has_definition(entity))
		return;

	be_gas_emit_switch_section(GAS_SECTION_DEBUG_INFO);

	ir_type *type = get_entity_type(entity);
	emit_type(type);

	emit_entity_label(entity);
	emit_uleb128(abbrev_variable);
	be_gas_emit_cstring(get_entity_ld_name(entity));
	emit_type_address(type);
	emit_int8(is_extern_entity(entity));
	emit_dbginfo(get_entity_dbg_info(entity));
	/* DW_AT_location */
	emit_int8(5); /* block length */
	emit_op_addr(entity);

	ARR_APP1(const ir_entity*, env.pubnames_list, entity);
}

static void emit_compile_unit_abbrev(void)
{
	begin_abbrev(abbrev_compile_unit, DW_TAG_compile_unit, DW_CHILDREN_yes);
	register_attribute(DW_AT_stmt_list, DW_FORM_data4);
	register_attribute(DW_AT_producer,  DW_FORM_string);
	register_attribute(DW_AT_name,      DW_FORM_string);
	if (language != 0)
		register_attribute(DW_AT_language,  DW_FORM_data2);
	if (comp_dir != NULL)
		register_attribute(DW_AT_comp_dir,  DW_FORM_string);
	end_abbrev();
}

static void emit_abbrev(void)
{
	/* create abbreviation for compile_unit */
	be_gas_emit_switch_section(GAS_SECTION_DEBUG_ABBREV);

	emit_label("abbrev_begin");

	emit_compile_unit_abbrev();
	emit_variable_abbrev();
	emit_subprogram_abbrev();
	emit_base_type_abbrev();
	emit_pointer_type_abbrev();
	emit_array_type_abbrev();
	emit_compound_type_abbrev();
	emit_subroutine_type_abbrev();
	emit_uleb128(0);
}

void be_dwarf_unit_begin(const char *filename)
{
	if (debug_level < LEVEL_BASIC)
		return;
	emit_abbrev();

	/* The DWARF emission code only handles 4 or 8 byte pointer sizes. */
	unsigned const pointer_size = ir_target_pointer_size();
	assert(pointer_size == 4 || pointer_size == 8);

	be_gas_emit_switch_section(GAS_SECTION_DEBUG_INFO);
	emit_label("info_section_begin");
	emit_label("info_begin");

	/* length of compilation unit info */
	emit_size("compile_unit_begin", "compile_unit_end");
	emit_label("compile_unit_begin");
	emit_int16(3);   /* dwarf version */
	emit_address("abbrev_begin");
	emit_int8(pointer_size);

	/* compile_unit die */
	emit_uleb128(abbrev_compile_unit);
	emit_address("line_section_begin");
	emit_string_printf("libFirm (%u.%u %s)", ir_get_version_major(),
	                   ir_get_version_minor(), ir_get_version_revision());
	be_gas_emit_cstring(filename);
	if (language != 0)
		emit_int16(language);
	if (comp_dir != NULL)
		be_gas_emit_cstring(comp_dir);

	if (has_cfi_sections) {
		/* tell gas to emit cfi in debug_frame
		 * TODO: if we produce exception handling code then this should be
		 *       .eh_frame (I also wonder if bad things happen if simply always
		 *       use eh_frame) */
		be_emit_cstring("\t.cfi_sections .debug_frame\n");
		be_emit_write_line();
	}
}

void be_dwarf_unit_end(void)
{
	if (debug_level < LEVEL_BASIC)
		return;
	be_gas_emit_switch_section(GAS_SECTION_TEXT);
	emit_label("section_end");

	be_gas_emit_switch_section(GAS_SECTION_DEBUG_INFO);
	emit_uleb128(0); /* end of compile_unit DIE */

	emit_label("compile_unit_end");

	emit_line_info();
	emit_pubnames();
}

void be_dwarf_close(void)
{
	if (debug_level < LEVEL_BASIC)
		return;
	pmap_destroy(env.file_map);
	DEL_ARR_F(env.file_list);
	DEL_ARR_F(env.pubnames_list);
	pset_new_destroy(&env.emitted_types);
}

/* Opens a dwarf handler */
void be_dwarf_open(void)
{
	if (debug_level < LEVEL_BASIC)
		return;
	env.file_map      = pmap_create();
	env.file_list     = NEW_ARR_F(const char*, 0);
	env.pubnames_list = NEW_ARR_F(const ir_entity*, 0);
	pset_new_init(&env.emitted_types);
}

void be_dwarf_set_source_language(dwarf_source_language new_language)
{
	language = new_language;
}

void be_dwarf_set_compilation_directory(const char *new_comp_dir)
{
	free(comp_dir);
	comp_dir = xstrdup(new_comp_dir);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_dwarf)
void be_init_dwarf(void)
{
	static const lc_opt_enum_int_items_t level_items[] = {
		{ "none",      LEVEL_NONE },
		{ "basic",     LEVEL_BASIC },
		{ "locations", LEVEL_LOCATIONS },
		{ "frameinfo", LEVEL_FRAMEINFO },
		{ NULL,        0 }
	};
	static lc_opt_enum_int_var_t debug_level_opt = {
		&debug_level, level_items
	};
	static lc_opt_table_entry_t be_main_options[] = {
		LC_OPT_ENT_ENUM_INT("debug", "debug output (dwarf) level",
		                    &debug_level_opt),
		LC_OPT_LAST
	};
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_add_table(be_grp, be_main_options);

	static lc_opt_table_entry_t dwarf_options[] = {
		LC_OPT_ENT_BOOL("has_cfi_sections",
		                "assembler supports .cfi_sections directive",
		                &has_cfi_sections),
		LC_OPT_LAST
	};
	lc_opt_entry_t *dwarf_grp = lc_opt_get_grp(be_grp, "dwarf");
	lc_opt_add_table(dwarf_grp, dwarf_options);
}
