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
 * @brief   Opcode of types.
 * @author  Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "xmalloc.h"
#include "tpop_t.h"
#include "type_t.h"

tp_op *type_class;         tp_op *get_tpop_class      (void) { return type_class;       }
tp_op *type_struct;        tp_op *get_tpop_struct     (void) { return type_struct;      }
tp_op *type_method;        tp_op *get_tpop_method     (void) { return type_method;      }
tp_op *type_union;         tp_op *get_tpop_union      (void) { return type_union;       }
tp_op *type_array;         tp_op *get_tpop_array      (void) { return type_array;       }
tp_op *type_enumeration;   tp_op *get_tpop_enumeration(void) { return type_enumeration; }
tp_op *type_pointer;       tp_op *get_tpop_pointer    (void) { return type_pointer;     }
tp_op *type_primitive;     tp_op *get_tpop_primitive  (void) { return type_primitive;   }
tp_op *type_id;            tp_op *get_tpop_id         (void) { return type_id;          }
tp_op *tpop_none;          tp_op *get_tpop_none       (void) { return tpop_none;        }
tp_op *tpop_unknown;       tp_op *get_tpop_unknown    (void) { return tpop_unknown;     }

tp_op *
new_tpop(tp_opcode code, ident *name, unsigned flags, size_t attr_size,
         const tp_op_ops *ops)
{
	tp_op *res;

	res = xmalloc(sizeof(*res));
	res->code          = code;
	res->name          = name;
	res->flags         = flags;
	res->attr_size     = attr_size;

	if (ops)
		memcpy(&res->ops, ops, sizeof(res->ops));
	else
		memset(&res->ops, 0, sizeof(res->ops));

	return res;
}

void
free_tpop(tp_op *tpop) {
	free(tpop);
}

static const tp_op_ops
	/** tpop operations for class types */
	class_ops = {
		free_class_attrs,
		free_class_entities,
		NULL,
		set_class_mode,
		set_class_size_bits,
		get_class_n_members,
		get_class_member,
		get_class_member_index
	},
	/** tpop operations for struct types */
	struct_ops = {
		free_struct_attrs,
		free_struct_entities,
		NULL,
		set_struct_mode,
		set_struct_size_bits,
		get_struct_n_members,
		get_struct_member,
		get_struct_member_index
	},
	/** tpop operations for method types */
	method_ops = {
		free_method_attrs,
		free_method_entities,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL
		},
	/** tpop operations for union types */
	union_ops = {
		free_union_attrs,
		free_union_entities,
		NULL,
		NULL,
		set_union_size_bits,
		get_union_n_members,
		get_union_member,
		get_union_member_index
	},
	/** tpop operations for array types */
	array_ops = {
		free_array_attrs,
		free_array_entities,
		free_array_automatic_entities,
		NULL,
		set_array_size_bits,
		NULL,
		NULL,
		NULL
	},
	/** tpop operations for enumeration types */
	enum_ops = {
		free_enumeration_attrs,
		free_enumeration_entities,
		NULL,
		set_enumeration_mode,
		NULL,
		NULL,
		NULL,
		NULL
	},
	/** tpop operations for pointer types */
	pointer_ops = {
		free_pointer_attrs,
		free_pointer_entities,
		NULL,
		set_pointer_mode,
		NULL,
		NULL,
		NULL,
		NULL
	},
	/** tpop operations for pseudo types */
	pseudo_ops = {
		NULL,
		NULL,
		NULL,
		NULL,
		set_default_size_bits,
		NULL,
		NULL,
		NULL
	},
	/** tpop operations for primitive types */
	null_ops = {
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL
	};

#define C     TP_OP_FLAG_COMPOUND
#define ID(s) new_id_from_chars(s, sizeof(s) - 1)

void init_tpop(void) {
	type_class       = new_tpop(tpo_class      , ID("class"),       C, sizeof (cls_attr), &class_ops);
	type_struct      = new_tpop(tpo_struct     , ID("struct"),      C, sizeof (stc_attr), &struct_ops);
	type_method      = new_tpop(tpo_method     , ID("method"),      0, sizeof (mtd_attr), &method_ops);
	type_union       = new_tpop(tpo_union      , ID("union"),       C, sizeof (uni_attr), &union_ops);
	type_array       = new_tpop(tpo_array      , ID("array"),       C, sizeof (arr_attr), &array_ops);
	type_enumeration = new_tpop(tpo_enumeration, ID("enumeration"), 0, sizeof (enm_attr), &enum_ops);
	type_pointer     = new_tpop(tpo_pointer    , ID("pointer"),     0, sizeof (ptr_attr), &pointer_ops);
	type_primitive   = new_tpop(tpo_primitive  , ID("primitive"),   0, sizeof (pri_attr), &null_ops);
	type_id          = new_tpop(tpo_id         , ID("type_id"),     0, /* sizeof (id_attr)  */ 0, &null_ops);
	tpop_none        = new_tpop(tpo_none       , ID("None"),        0, /* sizeof (non_attr) */ 0, &pseudo_ops);
	tpop_unknown     = new_tpop(tpo_unknown    , ID("Unknown"),     0, /* sizeof (ukn_attr) */ 0, &pseudo_ops);
}
#undef ID
#undef C

/* Finalize the tpop module.
 * Frees all type opcodes.  */
void finish_tpop(void) {
	free_tpop(type_class      ); type_class       = NULL;
	free_tpop(type_struct     ); type_struct      = NULL;
	free_tpop(type_method     ); type_method      = NULL;
	free_tpop(type_union      ); type_union       = NULL;
	free_tpop(type_array      ); type_array       = NULL;
	free_tpop(type_enumeration); type_enumeration = NULL;
	free_tpop(type_pointer    ); type_pointer     = NULL;
	free_tpop(type_primitive  ); type_primitive   = NULL;
	free_tpop(type_id         ); type_id          = NULL;
	free_tpop(tpop_none       ); tpop_none        = NULL;
	free_tpop(tpop_unknown    ); tpop_unknown     = NULL;
	}

/* Returns the string for the tp_opcode. */
const char  *get_tpop_name(const tp_op *op) {
	return get_id_str(op->name);
}

tp_opcode (get_tpop_code)(const tp_op *op) {
	return _get_tpop_code(op);
}

ident *(get_tpop_ident)(const tp_op *op) {
	return _get_tpop_ident(op);
}

/* returns the attribute size of the operator. */
int (get_tpop_attr_size)(const tp_op *op) {
	return _get_tpop_attr_size(op);
}
