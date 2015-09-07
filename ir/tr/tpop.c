/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Opcode of types.
 * @author  Goetz Lindenmaier, Michael Beck
 */
#include "ident_t.h"
#include "xmalloc.h"
#include "tpop_t.h"
#include "type_t.h"

const tp_op *type_class;     const tp_op *get_tpop_class    (void) { return type_class;     }
const tp_op *type_struct;    const tp_op *get_tpop_struct   (void) { return type_struct;    }
const tp_op *type_method;    const tp_op *get_tpop_method   (void) { return type_method;    }
const tp_op *type_union;     const tp_op *get_tpop_union    (void) { return type_union;     }
const tp_op *type_array;     const tp_op *get_tpop_array    (void) { return type_array;     }
const tp_op *type_pointer;   const tp_op *get_tpop_pointer  (void) { return type_pointer;   }
const tp_op *type_primitive; const tp_op *get_tpop_primitive(void) { return type_primitive; }
const tp_op *tpop_code;      const tp_op *get_tpop_code_type(void) { return tpop_code;      }
const tp_op *tpop_unknown;   const tp_op *get_tpop_unknown  (void) { return tpop_unknown;   }

const tp_op *new_tpop(tp_opcode code, ident *name, unsigned flags,
                      size_t attr_size, const tp_op_ops *ops)
{
	tp_op *res = XMALLOC(tp_op);
	res->code      = code;
	res->name      = name;
	res->flags     = flags;
	res->attr_size = attr_size;
	res->ops       = *ops;
	return res;
}

void free_tpop(const tp_op *tpop)
{
	free((void*)tpop);
}

static const tp_op_ops
	/** tpop operations for class types */
	class_ops = {
		.free_attrs         = free_class_attrs,
		.free_entities      = free_class_entities,
		.set_type_mode      = set_class_mode,
		.set_type_size      = set_default_size,
		.get_n_members      = get_class_n_members,
		.get_member         = get_class_member,
		.get_member_index   = get_class_member_index
	},
	/** tpop operations for struct types */
	struct_ops = {
		.free_attrs         = free_struct_attrs,
		.free_entities      = free_struct_entities,
		.set_type_mode      = set_struct_mode,
		.set_type_size      = set_default_size,
		.get_n_members      = get_struct_n_members,
		.get_member         = get_struct_member,
		.get_member_index   = get_struct_member_index
	},
	/** tpop operations for method types */
	method_ops = {
		.free_attrs = free_method_attrs,
	},
	/** tpop operations for union types */
	union_ops = {
		.free_attrs       = free_union_attrs,
		.free_entities    = free_union_entities,
		.set_type_size    = set_default_size,
		.get_n_members    = get_union_n_members,
		.get_member       = get_union_member,
		.get_member_index = get_union_member_index
	},
	/** tpop operations for array types */
	array_ops = {
		.set_type_size = set_default_size,
	},
	/** tpop operations for pointer types */
	pointer_ops = {
		.set_type_mode = set_pointer_mode,
	},
	primitive_ops = {
		.set_type_size = set_default_size,
	},
	/** tpop operations for primitive types */
	null_ops = {
		.free_attrs = NULL,
	}
;

void init_tpop(void)
{
	type_class     = new_tpop(tpo_class,     NEW_IDENT("class"),     TP_OP_FLAG_COMPOUND, sizeof(cls_attr),      &class_ops);
	type_struct    = new_tpop(tpo_struct,    NEW_IDENT("struct"),    TP_OP_FLAG_COMPOUND, sizeof(compound_attr), &struct_ops);
	type_method    = new_tpop(tpo_method,    NEW_IDENT("method"),    0,                   sizeof(mtd_attr),      &method_ops);
	type_union     = new_tpop(tpo_union,     NEW_IDENT("union"),     TP_OP_FLAG_COMPOUND, sizeof(compound_attr), &union_ops);
	type_array     = new_tpop(tpo_array,     NEW_IDENT("array"),     0,                   sizeof(arr_attr),      &array_ops);
	type_pointer   = new_tpop(tpo_pointer,   NEW_IDENT("pointer"),   0,                   sizeof(ptr_attr),      &pointer_ops);
	type_primitive = new_tpop(tpo_primitive, NEW_IDENT("primitive"), 0,                   0,                     &primitive_ops);
	tpop_code      = new_tpop(tpo_code,      NEW_IDENT("code"),      0,                   0,                     &null_ops);
	tpop_unknown   = new_tpop(tpo_unknown,   NEW_IDENT("Unknown"),   0,                   0,                     &null_ops);
}

void finish_tpop(void)
{
	free_tpop(type_class    ); type_class       = NULL;
	free_tpop(type_struct   ); type_struct      = NULL;
	free_tpop(type_method   ); type_method      = NULL;
	free_tpop(type_union    ); type_union       = NULL;
	free_tpop(type_array    ); type_array       = NULL;
	free_tpop(type_pointer  ); type_pointer     = NULL;
	free_tpop(type_primitive); type_primitive   = NULL;
	free_tpop(tpop_code     ); tpop_code        = NULL;
	free_tpop(tpop_unknown  ); tpop_unknown     = NULL;
}

const char *get_tpop_name(const tp_op *op)
{
	return get_id_str(op->name);
}

tp_opcode (get_tpop_code)(const tp_op *op)
{
	return _get_tpop_code(op);
}
