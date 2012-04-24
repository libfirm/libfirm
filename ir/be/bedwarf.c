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

/* Tag names and codes.  */
typedef enum dwarf_tag {
	DW_TAG_padding = 0x00,
	DW_TAG_array_type = 0x01,
	DW_TAG_class_type = 0x02,
	DW_TAG_entry_point = 0x03,
	DW_TAG_enumeration_type = 0x04,
	DW_TAG_formal_parameter = 0x05,
	DW_TAG_imported_declaration = 0x08,
	DW_TAG_label = 0x0a,
	DW_TAG_lexical_block = 0x0b,
	DW_TAG_member = 0x0d,
	DW_TAG_pointer_type = 0x0f,
	DW_TAG_reference_type = 0x10,
	DW_TAG_compile_unit = 0x11,
	DW_TAG_string_type = 0x12,
	DW_TAG_structure_type = 0x13,
	DW_TAG_subroutine_type = 0x15,
	DW_TAG_typedef = 0x16,
	DW_TAG_union_type = 0x17,
	DW_TAG_unspecified_parameters = 0x18,
	DW_TAG_variant = 0x19,
	DW_TAG_common_block = 0x1a,
	DW_TAG_common_inclusion = 0x1b,
	DW_TAG_inheritance = 0x1c,
	DW_TAG_inlined_subroutine = 0x1d,
	DW_TAG_module = 0x1e,
	DW_TAG_ptr_to_member_type = 0x1f,
	DW_TAG_set_type = 0x20,
	DW_TAG_subrange_type = 0x21,
	DW_TAG_with_stmt = 0x22,
	DW_TAG_access_declaration = 0x23,
	DW_TAG_base_type = 0x24,
	DW_TAG_catch_block = 0x25,
	DW_TAG_const_type = 0x26,
	DW_TAG_constant = 0x27,
	DW_TAG_enumerator = 0x28,
	DW_TAG_file_type = 0x29,
	DW_TAG_friend = 0x2a,
	DW_TAG_namelist = 0x2b,
	DW_TAG_namelist_item = 0x2c,
	DW_TAG_packed_type = 0x2d,
	DW_TAG_subprogram = 0x2e,
	DW_TAG_template_type_param = 0x2f,
	DW_TAG_template_value_param = 0x30,
	DW_TAG_thrown_type = 0x31,
	DW_TAG_try_block = 0x32,
	DW_TAG_variant_part = 0x33,
	DW_TAG_variable = 0x34,
	DW_TAG_volatile_type = 0x35,
	/* DWARF 3.  */
	DW_TAG_dwarf_procedure = 0x36,
	DW_TAG_restrict_type = 0x37,
	DW_TAG_interface_type = 0x38,
	DW_TAG_namespace = 0x39,
	DW_TAG_imported_module = 0x3a,
	DW_TAG_unspecified_type = 0x3b,
	DW_TAG_partial_unit = 0x3c,
	DW_TAG_imported_unit = 0x3d,
	DW_TAG_condition = 0x3f,
	DW_TAG_shared_type = 0x40,
} dwarf_tag;

typedef enum custom_abbrevs {
	abbrev_void_pointer_type = 100,
	abbrev_unnamed_formal_parameter,
	abbrev_void_subroutine_type,
	abbrev_bitfield_member,
} custom_abbrevs;

typedef enum dw_children {
	DW_CHILDREN_no  = 0x00,
	DW_CHILDREN_yes = 0x01
} dw_children;

typedef enum dwarf_form {
	DW_FORM_addr = 0x01,
	DW_FORM_block2 = 0x03,
	DW_FORM_block4 = 0x04,
	DW_FORM_data2 = 0x05,
	DW_FORM_data4 = 0x06,
	DW_FORM_data8 = 0x07,
	DW_FORM_string = 0x08,
	DW_FORM_block = 0x09,
	DW_FORM_block1 = 0x0a,
	DW_FORM_data1 = 0x0b,
	DW_FORM_flag = 0x0c,
	DW_FORM_sdata = 0x0d,
	DW_FORM_strp = 0x0e,
	DW_FORM_udata = 0x0f,
	DW_FORM_ref_addr = 0x10,
	DW_FORM_ref1 = 0x11,
	DW_FORM_ref2 = 0x12,
	DW_FORM_ref4 = 0x13,
	DW_FORM_ref8 = 0x14,
	DW_FORM_ref_udata = 0x15,
	DW_FORM_indirect = 0x16
} dwarf_form;

typedef enum dwarf_attribute {
	DW_AT_sibling = 0x01,
	DW_AT_location = 0x02,
	DW_AT_name = 0x03,
	DW_AT_ordering = 0x09,
	DW_AT_subscr_data = 0x0a,
	DW_AT_byte_size = 0x0b,
	DW_AT_bit_offset = 0x0c,
	DW_AT_bit_size = 0x0d,
	DW_AT_element_list = 0x0f,
	DW_AT_stmt_list = 0x10,
	DW_AT_low_pc = 0x11,
	DW_AT_high_pc = 0x12,
	DW_AT_language = 0x13,
	DW_AT_member = 0x14,
	DW_AT_discr = 0x15,
	DW_AT_discr_value = 0x16,
	DW_AT_visibility = 0x17,
	DW_AT_import = 0x18,
	DW_AT_string_length = 0x19,
	DW_AT_common_reference = 0x1a,
	DW_AT_comp_dir = 0x1b,
	DW_AT_const_value = 0x1c,
	DW_AT_containing_type = 0x1d,
	DW_AT_default_value = 0x1e,
	DW_AT_inline = 0x20,
	DW_AT_is_optional = 0x21,
	DW_AT_lower_bound = 0x22,
	DW_AT_producer = 0x25,
	DW_AT_prototyped = 0x27,
	DW_AT_return_addr = 0x2a,
	DW_AT_start_scope = 0x2c,
	DW_AT_bit_stride = 0x2e,
	DW_AT_stride_size = DW_AT_bit_stride,
	DW_AT_upper_bound = 0x2f,
	DW_AT_abstract_origin = 0x31,
	DW_AT_accessibility = 0x32,
	DW_AT_address_class = 0x33,
	DW_AT_artificial = 0x34,
	DW_AT_base_types = 0x35,
	DW_AT_calling_convention = 0x36,
	DW_AT_count = 0x37,
	DW_AT_data_member_location = 0x38,
	DW_AT_decl_column = 0x39,
	DW_AT_decl_file = 0x3a,
	DW_AT_decl_line = 0x3b,
	DW_AT_declaration = 0x3c,
	DW_AT_discr_list = 0x3d,
	DW_AT_encoding = 0x3e,
	DW_AT_external = 0x3f,
	DW_AT_frame_base = 0x40,
	DW_AT_friend = 0x41,
	DW_AT_identifier_case = 0x42,
	DW_AT_macro_info = 0x43,
	DW_AT_namelist_items = 0x44,
	DW_AT_priority = 0x45,
	DW_AT_segment = 0x46,
	DW_AT_specification = 0x47,
	DW_AT_static_link = 0x48,
	DW_AT_type = 0x49,
	DW_AT_use_location = 0x4a,
	DW_AT_variable_parameter = 0x4b,
	DW_AT_virtuality = 0x4c,
	DW_AT_vtable_elem_location = 0x4d,
	/* DWARF 3 values.  */
	DW_AT_allocated     = 0x4e,
	DW_AT_associated    = 0x4f,
	DW_AT_data_location = 0x50,
	DW_AT_byte_stride   = 0x51,
	DW_AT_stride        = DW_AT_byte_stride,
	DW_AT_entry_pc      = 0x52,
	DW_AT_use_UTF8      = 0x53,
	DW_AT_extension     = 0x54,
	DW_AT_ranges        = 0x55,
	DW_AT_trampoline    = 0x56,
	DW_AT_call_column   = 0x57,
	DW_AT_call_file     = 0x58,
	DW_AT_call_line     = 0x59,
	DW_AT_description   = 0x5a,
	DW_AT_binary_scale  = 0x5b,
	DW_AT_decimal_scale = 0x5c,
	DW_AT_small         = 0x5d,
	DW_AT_decimal_sign  = 0x5e,
	DW_AT_digit_count   = 0x5f,
	DW_AT_picture_string = 0x60,
	DW_AT_mutable       = 0x61,
	DW_AT_threads_scaled = 0x62,
	DW_AT_explicit      = 0x63,
	DW_AT_object_pointer = 0x64,
	DW_AT_endianity     = 0x65,
	DW_AT_elemental     = 0x66,
	DW_AT_pure          = 0x67,
	DW_AT_recursive     = 0x68,
} dwarf_attribute;

enum dwarf_type {
	DW_ATE_void = 0x0,
	DW_ATE_address = 0x1,
	DW_ATE_boolean = 0x2,
	DW_ATE_complex_float = 0x3,
	DW_ATE_float = 0x4,
	DW_ATE_signed = 0x5,
	DW_ATE_signed_char = 0x6,
	DW_ATE_unsigned = 0x7,
	DW_ATE_unsigned_char = 0x8,
	/* DWARF 3.  */
	DW_ATE_imaginary_float = 0x9,
	DW_ATE_packed_decimal = 0xa,
	DW_ATE_numeric_string = 0xb,
	DW_ATE_edited = 0xc,
	DW_ATE_signed_fixed = 0xd,
	DW_ATE_unsigned_fixed = 0xe,
	DW_ATE_decimal_float = 0xf,
};

typedef enum dwarf_line_number_x_ops {
	DW_LNE_end_sequence = 1,
	DW_LNE_set_address = 2,
	DW_LNE_define_file = 3,
} dwarf_line_number_x_ops;

typedef enum dwarf_location_op {
	DW_OP_addr = 0x03,
	DW_OP_deref = 0x06,
	DW_OP_const1u = 0x08,
	DW_OP_const1s = 0x09,
	DW_OP_const2u = 0x0a,
	DW_OP_const2s = 0x0b,
	DW_OP_const4u = 0x0c,
	DW_OP_const4s = 0x0d,
	DW_OP_const8u = 0x0e,
	DW_OP_const8s = 0x0f,
	DW_OP_constu = 0x10,
	DW_OP_consts = 0x11,
	DW_OP_dup = 0x12,
	DW_OP_drop = 0x13,
	DW_OP_over = 0x14,
	DW_OP_pick = 0x15,
	DW_OP_swap = 0x16,
	DW_OP_rot = 0x17,
	DW_OP_xderef = 0x18,
	DW_OP_abs = 0x19,
	DW_OP_and = 0x1a,
	DW_OP_div = 0x1b,
	DW_OP_minus = 0x1c,
	DW_OP_mod = 0x1d,
	DW_OP_mul = 0x1e,
	DW_OP_neg = 0x1f,
	DW_OP_not = 0x20,
	DW_OP_or = 0x21,
	DW_OP_plus = 0x22,
	DW_OP_plus_uconst = 0x23,
	DW_OP_shl = 0x24,
	DW_OP_shr = 0x25,
	DW_OP_shra = 0x26,
	DW_OP_xor = 0x27,
	DW_OP_bra = 0x28,
	DW_OP_eq = 0x29,
	DW_OP_ge = 0x2a,
	DW_OP_gt = 0x2b,
	DW_OP_le = 0x2c,
	DW_OP_lt = 0x2d,
	DW_OP_ne = 0x2e,
	DW_OP_skip = 0x2f,
	DW_OP_lit0 = 0x30,
	DW_OP_lit1 = 0x31,
	DW_OP_lit2 = 0x32,
	DW_OP_lit3 = 0x33,
	DW_OP_lit4 = 0x34,
	DW_OP_lit5 = 0x35,
	DW_OP_lit6 = 0x36,
	DW_OP_lit7 = 0x37,
	DW_OP_lit8 = 0x38,
	DW_OP_lit9 = 0x39,
	DW_OP_lit10 = 0x3a,
	DW_OP_lit11 = 0x3b,
	DW_OP_lit12 = 0x3c,
	DW_OP_lit13 = 0x3d,
	DW_OP_lit14 = 0x3e,
	DW_OP_lit15 = 0x3f,
	DW_OP_lit16 = 0x40,
	DW_OP_lit17 = 0x41,
	DW_OP_lit18 = 0x42,
	DW_OP_lit19 = 0x43,
	DW_OP_lit20 = 0x44,
	DW_OP_lit21 = 0x45,
	DW_OP_lit22 = 0x46,
	DW_OP_lit23 = 0x47,
	DW_OP_lit24 = 0x48,
	DW_OP_lit25 = 0x49,
	DW_OP_lit26 = 0x4a,
	DW_OP_lit27 = 0x4b,
	DW_OP_lit28 = 0x4c,
	DW_OP_lit29 = 0x4d,
	DW_OP_lit30 = 0x4e,
	DW_OP_lit31 = 0x4f,
	DW_OP_reg0 = 0x50,
	DW_OP_reg1 = 0x51,
	DW_OP_reg2 = 0x52,
	DW_OP_reg3 = 0x53,
	DW_OP_reg4 = 0x54,
	DW_OP_reg5 = 0x55,
	DW_OP_reg6 = 0x56,
	DW_OP_reg7 = 0x57,
	DW_OP_reg8 = 0x58,
	DW_OP_reg9 = 0x59,
	DW_OP_reg10 = 0x5a,
	DW_OP_reg11 = 0x5b,
	DW_OP_reg12 = 0x5c,
	DW_OP_reg13 = 0x5d,
	DW_OP_reg14 = 0x5e,
	DW_OP_reg15 = 0x5f,
	DW_OP_reg16 = 0x60,
	DW_OP_reg17 = 0x61,
	DW_OP_reg18 = 0x62,
	DW_OP_reg19 = 0x63,
	DW_OP_reg20 = 0x64,
	DW_OP_reg21 = 0x65,
	DW_OP_reg22 = 0x66,
	DW_OP_reg23 = 0x67,
	DW_OP_reg24 = 0x68,
	DW_OP_reg25 = 0x69,
	DW_OP_reg26 = 0x6a,
	DW_OP_reg27 = 0x6b,
	DW_OP_reg28 = 0x6c,
	DW_OP_reg29 = 0x6d,
	DW_OP_reg30 = 0x6e,
	DW_OP_reg31 = 0x6f,
	DW_OP_breg0 = 0x70,
	DW_OP_breg1 = 0x71,
	DW_OP_breg2 = 0x72,
	DW_OP_breg3 = 0x73,
	DW_OP_breg4 = 0x74,
	DW_OP_breg5 = 0x75,
	DW_OP_breg6 = 0x76,
	DW_OP_breg7 = 0x77,
	DW_OP_breg8 = 0x78,
	DW_OP_breg9 = 0x79,
	DW_OP_breg10 = 0x7a,
	DW_OP_breg11 = 0x7b,
	DW_OP_breg12 = 0x7c,
	DW_OP_breg13 = 0x7d,
	DW_OP_breg14 = 0x7e,
	DW_OP_breg15 = 0x7f,
	DW_OP_breg16 = 0x80,
	DW_OP_breg17 = 0x81,
	DW_OP_breg18 = 0x82,
	DW_OP_breg19 = 0x83,
	DW_OP_breg20 = 0x84,
	DW_OP_breg21 = 0x85,
	DW_OP_breg22 = 0x86,
	DW_OP_breg23 = 0x87,
	DW_OP_breg24 = 0x88,
	DW_OP_breg25 = 0x89,
	DW_OP_breg26 = 0x8a,
	DW_OP_breg27 = 0x8b,
	DW_OP_breg28 = 0x8c,
	DW_OP_breg29 = 0x8d,
	DW_OP_breg30 = 0x8e,
	DW_OP_breg31 = 0x8f,
	DW_OP_regx = 0x90,
	DW_OP_fbreg = 0x91,
	DW_OP_bregx = 0x92,
	DW_OP_piece = 0x93,
	DW_OP_deref_size = 0x94,
	DW_OP_xderef_size = 0x95,
	DW_OP_nop = 0x96,
} dwarf_location_op;

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
	register_attribute(DW_AT_decl_file, DW_FORM_udata);
	register_attribute(DW_AT_decl_line, DW_FORM_udata);
}

/**
 * emits values for DW_AT_decl_file then DW_AT_decl_line
 */
static void emit_dbginfo(dwarf_t *env, const dbg_info *dbgi)
{
	src_loc_t const loc  = ir_retrieve_dbg_info(dbgi);
	unsigned  const file = loc.file ? insert_file(env, loc.file) : 0;
	emit_uleb128(file);
	emit_uleb128(loc.line);
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
