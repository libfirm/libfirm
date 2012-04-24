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
 * @brief   DWARF debugging info support
 * @author  Matthias Braun
 */
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "bedwarf_t.h"
#include "obst.h"
#include "irprog.h"
#include "irgraph.h"
#include "tv.h"
#include "xmalloc.h"
#include "pmap.h"
#include "pdeq.h"
#include "pset_new.h"
#include "util.h"
#include "obst.h"
#include "array_t.h"
#include "be_dbgout_t.h"
#include "beabi.h"
#include "bemodule.h"
#include "beemitter.h"
#include "dbginfo.h"
#include "begnuas.h"

/**
 * The dwarf handle.
 */
typedef struct dwarf_t {
	dbg_handle               base;         /**< the base class */
	const ir_entity         *cur_ent;     /**< current method entity */
	const be_stack_layout_t *layout;      /**< current stack layout */
	unsigned                 next_type_nr; /**< next type number */
	pmap                    *file_map;    /**< a map from file names to number in file list */
	const char             **file_list;
	const ir_entity        **pubnames_list;
	pset_new_t               emitted_types;
	const char              *main_file;   /**< name of the main source file */
	const char              *curr_file;   /**< name of the current source file */
	unsigned                 label_num;
	unsigned                 last_line;
} dwarf_t;

static dwarf_source_language language;
static const char           *comp_dir;

static unsigned insert_file(dwarf_t *env, const char *filename)
{
	unsigned num;
	void    *entry = pmap_get(env->file_map, filename);
	if (entry != NULL) {
		return PTR_TO_INT(entry);
	}
	ARR_APP1(const char*, env->file_list, filename);
	num = (unsigned)ARR_LEN(env->file_list);
	pmap_insert(env->file_map, filename, INT_TO_PTR(num));
	/* TODO: quote chars in string */
	be_emit_irprintf("\t.file %u \"%s\"\n", num, filename);
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

static void emit_string(const char *string)
{
	be_emit_irprintf("\t.asciz \"%s\"\n", string);
	be_emit_write_line();
}

static void emit_ref(const ir_entity *entity)
{
	be_emit_cstring("\t.long ");
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

static void begin_abbrev(unsigned code, dwarf_tag tag, dw_children children)
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

static void emit_line_info(dwarf_t *env)
{
	be_gas_emit_switch_section(GAS_SECTION_DEBUG_LINE);

	emit_label("line_section_begin");
	/* on elf systems gas handles producing the line info for us, and we
	 * don't have to do anything */
	if (be_gas_object_file_format != OBJECT_FILE_FORMAT_ELF) {
		size_t i;
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
		emit_string("/foo/bar");
		emit_int8(0);

		/* file list */
		for (i = 0; i < ARR_LEN(env->file_list); ++i) {
			emit_string(env->file_list[0]);
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

static void emit_pubnames(dwarf_t *env)
{
	size_t i;

	be_gas_emit_switch_section(GAS_SECTION_DEBUG_PUBNAMES);

	emit_size("pubnames_begin", "pubnames_end");
	emit_label("pubnames_begin");

	emit_int16(2); /* version */
	emit_size("info_section_begin", "info_begin");
	emit_size("compile_unit_begin", "compile_unit_end");

	for (i = 0; i < ARR_LEN(env->pubnames_list); ++i) {
		const ir_entity *entity = env->pubnames_list[i];
		be_emit_irprintf("\t.long %sE%ld - %sinfo_begin\n",
		                 be_gas_get_private_prefix(),
		                 get_entity_nr(entity), be_gas_get_private_prefix());
		emit_string(get_entity_name(entity));
	}
	emit_int32(0);

	emit_label("pubnames_end");
}

static void dwarf_set_dbg_info(dbg_handle *h, dbg_info *dbgi)
{
	dwarf_t  *const env = (dwarf_t*)h;
	src_loc_t const loc = ir_retrieve_dbg_info(dbgi);
	unsigned        filenum;

	if (!loc.file)
		return;

	filenum = insert_file(env, loc.file);
	be_emit_irprintf("\t.loc %u %u %u\n", filenum, loc.line, loc.column);
	be_emit_write_line();
}

static bool is_extern_entity(const ir_entity *entity)
{
	ir_visited_t visibility = get_entity_visibility(entity);
	return visibility == ir_visibility_default
	    || visibility == ir_visibility_external;
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

/**
 * Emit values for DW_AT_decl_file, DW_AT_decl_line and DW_AT_decl_column.
 */
static void emit_dbginfo(dwarf_t *env, const dbg_info *dbgi)
{
	src_loc_t const loc  = ir_retrieve_dbg_info(dbgi);
	unsigned  const file = loc.file ? insert_file(env, loc.file) : 0;
	emit_uleb128(file);
	emit_uleb128(loc.line);
	emit_uleb128(loc.column);
}

static void emit_subprogram_abbrev(void)
{
	begin_abbrev(DW_TAG_subprogram, DW_TAG_subprogram, DW_CHILDREN_no);
	register_attribute(DW_AT_name,      DW_FORM_string);
	register_dbginfo_attributes();
	//register_attribute(DW_AT_prototyped, DW_FORM_flag);
	//register_attribute(DW_AT_type,       DW_FORM_ref4);
	register_attribute(DW_AT_external,  DW_FORM_flag);
	register_attribute(DW_AT_low_pc,    DW_FORM_addr);
	register_attribute(DW_AT_high_pc,   DW_FORM_addr);
	//register_attribute(DW_AT_frame_base, DW_FORM_block1);
	end_abbrev();
}

/**
 * dump the drwarf for a method begin
 */
static void dwarf_method_begin(dbg_handle *handle, const ir_entity *entity)
{
	dwarf_t *env = (dwarf_t*)handle;

	be_gas_emit_switch_section(GAS_SECTION_DEBUG_INFO);

	emit_entity_label(entity);
	emit_uleb128(DW_TAG_subprogram);
	emit_string(get_entity_ld_name(entity));
	emit_dbginfo(env, get_entity_dbg_info(entity));
	emit_int8(is_extern_entity(entity));
	emit_ref(entity);
	be_emit_irprintf("\t.long %smethod_end_%s\n", be_gas_get_private_prefix(),
	                 get_entity_ld_name(entity));

	ARR_APP1(const ir_entity*, env->pubnames_list, entity);

	env->cur_ent = entity;
}

/**
 * dump the drwarf for a method end
 */
static void dwarf_method_end(dbg_handle *handle)
{
	dwarf_t *env = (dwarf_t*)handle;
	const ir_entity *entity = env->cur_ent;

	be_emit_irprintf("%smethod_end_%s:\n", be_gas_get_private_prefix(),
	                 get_entity_ld_name(entity));
}

static void dwarf_types(dbg_handle *handle)
{
	(void)handle;
}

static void emit_type(dwarf_t *env, ir_type *type);

static void emit_base_type_abbrev(void)
{
	begin_abbrev(DW_TAG_base_type, DW_TAG_base_type, DW_CHILDREN_no);
	register_attribute(DW_AT_encoding,  DW_FORM_data1);
	register_attribute(DW_AT_byte_size, DW_FORM_data1);
	register_attribute(DW_AT_name,      DW_FORM_string);
	end_abbrev();
}

static void emit_type_label(const ir_type *type)
{
	be_emit_irprintf("%sT%ld:\n", be_gas_get_private_prefix(), get_type_nr(type));
	be_emit_write_line();
}

static void emit_type_address(const ir_type *type)
{
	be_emit_irprintf("\t.long %sT%ld - %sinfo_begin\n",
	                 be_gas_get_private_prefix(),
	                 get_type_nr(type), be_gas_get_private_prefix());
	be_emit_write_line();
}

static void emit_base_type(const ir_type *type)
{
	char buf[128];
	ir_mode *mode = get_type_mode(type);
	ir_print_type(buf, sizeof(buf), type);

	emit_type_label(type);
	emit_uleb128(DW_TAG_base_type);
	if (mode_is_int(mode)) {
		/* bool hack */
		if (strcmp(buf, "_Bool")==0 || strcmp(buf, "bool")==0) {
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
	emit_string(buf);
}

static void emit_pointer_type_abbrev(void)
{
	begin_abbrev(DW_TAG_pointer_type, DW_TAG_pointer_type, DW_CHILDREN_no);
	register_attribute(DW_AT_type,      DW_FORM_ref4);
	register_attribute(DW_AT_byte_size, DW_FORM_data1);
	end_abbrev();

	/* for void* pointer s*/
	begin_abbrev(abbrev_void_pointer_type, DW_TAG_pointer_type, DW_CHILDREN_no);
	register_attribute(DW_AT_byte_size, DW_FORM_data1);
	end_abbrev();
}

static void emit_pointer_type(dwarf_t *env, const ir_type *type)
{
	ir_type *points_to = get_pointer_points_to_type(type);
	unsigned size      = get_type_size_bytes(type);
	assert(size < 256);

	if (!is_Primitive_type(points_to) || get_type_mode(points_to) != mode_ANY) {
		emit_type(env, points_to);

		emit_type_label(type);
		emit_uleb128(DW_TAG_pointer_type);
		emit_type_address(points_to);
	} else {
		emit_type_label(type);
		emit_uleb128(abbrev_void_pointer_type);
	}
	emit_int8(size);
}

static void emit_array_type_abbrev(void)
{
	begin_abbrev(DW_TAG_array_type, DW_TAG_array_type, DW_CHILDREN_yes);
	register_attribute(DW_AT_type, DW_FORM_ref4);
	end_abbrev();

	begin_abbrev(DW_TAG_subrange_type, DW_TAG_subrange_type, DW_CHILDREN_no);
	register_attribute(DW_AT_upper_bound, DW_FORM_udata);
	end_abbrev();
}

static void emit_array_type(dwarf_t *env, const ir_type *type)
{
	ir_type *element_type = get_array_element_type(type);

	if (get_array_n_dimensions(type) != 1)
		panic("dwarf: multidimensional arrays no supported yet");

	emit_type(env, element_type);

	emit_type_label(type);
	emit_uleb128(DW_TAG_array_type);
	emit_type_address(element_type);

	if (has_array_upper_bound(type, 0)) {
		int bound = get_array_upper_bound_int(type, 0);
		emit_uleb128(DW_TAG_subrange_type);
		emit_uleb128(bound);
	}

	emit_uleb128(0);
}

static void emit_compound_type_abbrev(void)
{
	begin_abbrev(DW_TAG_structure_type, DW_TAG_structure_type, DW_CHILDREN_yes);
	register_attribute(DW_AT_byte_size,  DW_FORM_udata);
	// TODO register_dbginfo_attributes();
	end_abbrev();

	begin_abbrev(DW_TAG_union_type, DW_TAG_union_type, DW_CHILDREN_yes);
	register_attribute(DW_AT_byte_size,  DW_FORM_udata);
	// TODO register_dbginfo_attributes();
	end_abbrev();

	begin_abbrev(DW_TAG_class_type, DW_TAG_class_type, DW_CHILDREN_yes);
	register_attribute(DW_AT_byte_size,  DW_FORM_udata);
	// TODO register_dbginfo_attributes();
	end_abbrev();

	begin_abbrev(DW_TAG_member, DW_TAG_member, DW_CHILDREN_no);
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

static void emit_compound_type(dwarf_t *env, const ir_type *type)
{
	size_t i;
	size_t n_members = get_compound_n_members(type);

	for (i = 0; i < n_members; ++i) {
		ir_entity *member      = get_compound_member(type, i);
		ir_type   *member_type = get_entity_type(member);
		if (is_Primitive_type(member_type)) {
			ir_type *base = get_primitive_base_type(member_type);
			if (base != NULL)
				member_type = base;
		}
		emit_type(env, member_type);
	}

	emit_type_label(type);
	if (is_Struct_type(type)) {
		emit_uleb128(DW_TAG_structure_type);
	} else if (is_Union_type(type)) {
		emit_uleb128(DW_TAG_union_type);
	} else {
		assert(is_Class_type(type));
		emit_uleb128(DW_TAG_class_type);
	}
	emit_uleb128(get_type_size_bytes(type));
	for (i = 0; i < n_members; ++i) {
		ir_entity *member      = get_compound_member(type, i);
		ir_type   *member_type = get_entity_type(member);
		int        offset      = get_entity_offset(member);
		ir_type   *base;

		if (is_Primitive_type(member_type) &&
		    (base = get_primitive_base_type(member_type))) {
		    unsigned bit_offset = get_entity_offset_bits_remainder(member);
		    unsigned base_size  = get_type_size_bytes(base);
		    ir_mode *mode       = get_type_mode(member_type);
		    unsigned bit_size   = get_mode_size_bits(mode);

			bit_offset = base_size*8 - bit_offset - bit_size;

			emit_uleb128(abbrev_bitfield_member);
			emit_uleb128(base_size);
			emit_uleb128(bit_size);
			emit_uleb128(bit_offset);
			member_type = base;
		} else {
			emit_uleb128(DW_TAG_member);
		}

		emit_type_address(member_type);
		emit_string(get_entity_name(member));
		emit_dbginfo(env, get_entity_dbg_info(member));
		assert(offset >= 0);
		emit_int8(1 + get_uleb128_size(offset));
		emit_op_plus_uconst(offset);
	}

	emit_int8(0);
}

static void emit_subroutine_type_abbrev(void)
{
	begin_abbrev(DW_TAG_subroutine_type,
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

static void emit_subroutine_type(dwarf_t *env, const ir_type *type)
{
	size_t n_params = get_method_n_params(type);
	size_t n_ress   = get_method_n_ress(type);
	size_t i;
	for (i = 0; i < n_params; ++i) {
		ir_type *param_type = get_method_param_type(type, i);
		emit_type(env, param_type);
	}
	for (i = 0; i < n_ress; ++i) {
		ir_type *res_type = get_method_res_type(type, i);
		emit_type(env, res_type);
	}

	emit_type_label(type);
	emit_uleb128(n_ress == 0 ? abbrev_void_subroutine_type : DW_TAG_subroutine_type);
	emit_int8(1); /* prototyped */
	if (n_ress > 0) {
		/* dwarf only supports 1 return type */
		ir_type *res_type = get_method_res_type(type, 0);
		emit_type_address(res_type);
	}

	for (i = 0; i < n_params; ++i) {
		ir_type *param_type = get_method_param_type(type, i);
		emit_uleb128(abbrev_unnamed_formal_parameter);
		emit_type_address(param_type);
	}
	emit_int8(0);
}

static void emit_type(dwarf_t *env, ir_type *type)
{
	if (pset_new_insert(&env->emitted_types, type))
		return;

	switch (get_type_tpop_code(type)) {
	case tpo_primitive: emit_base_type(type);            break;
	case tpo_pointer:   emit_pointer_type(env, type);    break;
	case tpo_array:     emit_array_type(env, type);      break;
	case tpo_class:
	case tpo_struct:
	case tpo_union:     emit_compound_type(env, type);   break;
	case tpo_method:    emit_subroutine_type(env, type); break;
	default:
		panic("bedwarf: type %+F not implemented yet", type);
	}
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
	begin_abbrev(DW_TAG_variable, DW_TAG_variable, DW_CHILDREN_no);
	register_attribute(DW_AT_name,      DW_FORM_string);
	register_attribute(DW_AT_type,      DW_FORM_ref4);
	register_attribute(DW_AT_external,  DW_FORM_flag);
	register_dbginfo_attributes();
	register_attribute(DW_AT_location,  DW_FORM_block1);
	end_abbrev();
}

static void dwarf_variable(dbg_handle *handle, const ir_entity *entity)
{
	dwarf_t  *env  = (dwarf_t*) handle;
	ir_type  *type = get_entity_type(entity);

	if (get_entity_ld_name(entity)[0] == '\0')
		return;

	be_gas_emit_switch_section(GAS_SECTION_DEBUG_INFO);

	emit_type(env, type);

	emit_entity_label(entity);
	emit_uleb128(DW_TAG_variable);
	emit_string(get_entity_ld_name(entity));
	emit_type_address(type);
	emit_int8(is_extern_entity(entity));
	emit_dbginfo(env, get_entity_dbg_info(entity));
	/* DW_AT_location */
	emit_int8(5); /* block length */
	emit_op_addr(entity);

	ARR_APP1(const ir_entity*, env->pubnames_list, entity);
}

static void emit_compile_unit_abbrev(void)
{
	begin_abbrev(DW_TAG_compile_unit, DW_TAG_compile_unit, DW_CHILDREN_yes);
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

/**
 * start a new source object (compilation unit)
 */
static void dwarf_unit_begin(dbg_handle *handle, const char *filename)
{
	(void) handle;

	emit_abbrev();

	be_gas_emit_switch_section(GAS_SECTION_DEBUG_INFO);
	emit_label("info_section_begin");
	emit_label("info_begin");

	/* length of compilation unit info */
	emit_size("compile_unit_begin", "compile_unit_end");
	emit_label("compile_unit_begin");
	emit_int16(2);   /* dwarf version */
	emit_address("abbrev_begin");
	emit_int8(4);    /* pointer size */

	/* compile_unit die */
	emit_uleb128(DW_TAG_compile_unit);
	emit_address("line_section_begin");
	emit_string_printf("libFirm (%u.%u %s)", ir_get_version_major(),
	                   ir_get_version_minor(),
	                   ir_get_version_revision());
	emit_string(filename);
	if (language != 0)
		emit_int16(DW_LANG_C_plus_plus);
	if (comp_dir != NULL)
		emit_string(comp_dir);
}

static void dwarf_unit_end(dbg_handle *handle)
{
	dwarf_t *env = (dwarf_t*)handle;

	be_gas_emit_switch_section(GAS_SECTION_TEXT);
	emit_label("section_end");

	be_gas_emit_switch_section(GAS_SECTION_DEBUG_INFO);
	emit_uleb128(0); /* end of compile_unit DIE */

	emit_label("compile_unit_end");

	emit_line_info(env);
	emit_pubnames(env);
}

/**
 * Close the drwarf handler.
 */
static void dwarf_close(dbg_handle *handle)
{
	dwarf_t *h = (dwarf_t *)handle;
	pmap_destroy(h->file_map);
	DEL_ARR_F(h->file_list);
	DEL_ARR_F(h->pubnames_list);
	pset_new_destroy(&h->emitted_types);
	free(h);
}

/** The drwarf operations. */
static const debug_ops dwarf_ops = {
	dwarf_close,
	dwarf_unit_begin,
	dwarf_unit_end,
	dwarf_method_begin,
	dwarf_method_end,
	dwarf_types,
	dwarf_variable,
	dwarf_set_dbg_info
};

/* Opens a drwarf handler */
static dbg_handle *be_dwarf_open(void)
{
	dwarf_t *h = XMALLOCZ(dwarf_t);

	h->base.ops      = &dwarf_ops;
	h->file_map      = pmap_create();
	h->file_list     = NEW_ARR_F(const char*, 0);
	h->pubnames_list = NEW_ARR_F(const ir_entity*, 0);
	pset_new_init(&h->emitted_types);

	return &h->base;
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_dwarf)
void be_init_dwarf(void)
{
	be_register_dbgout_module("dwarf", be_dwarf_open);
}

void be_dwarf_set_source_language(dwarf_source_language new_language)
{
	language = new_language;
}

void be_dwarf_set_compilation_directory(const char *new_comp_dir)
{
	comp_dir = new_comp_dir;
}
