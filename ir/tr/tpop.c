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

const tp_op *new_tpop(tp_opcode code, ident *name, size_t attr_size)
{
	tp_op *res = XMALLOC(tp_op);
	res->code      = code;
	res->name      = name;
	res->attr_size = attr_size;
	return res;
}

void free_tpop(const tp_op *tpop)
{
	free((void*)tpop);
}

void init_tpop(void)
{
	type_class     = new_tpop(tpo_class,     NEW_IDENT("class"),     sizeof(cls_attr)     );
	type_struct    = new_tpop(tpo_struct,    NEW_IDENT("struct"),    sizeof(compound_attr));
	type_method    = new_tpop(tpo_method,    NEW_IDENT("method"),    sizeof(mtd_attr)     );
	type_union     = new_tpop(tpo_union,     NEW_IDENT("union"),     sizeof(compound_attr));
	type_array     = new_tpop(tpo_array,     NEW_IDENT("array"),     sizeof(arr_attr)     );
	type_pointer   = new_tpop(tpo_pointer,   NEW_IDENT("pointer"),   sizeof(ptr_attr)     );
	type_primitive = new_tpop(tpo_primitive, NEW_IDENT("primitive"), 0                    );
	tpop_code      = new_tpop(tpo_code,      NEW_IDENT("code"),      0                    );
	tpop_unknown   = new_tpop(tpo_unknown,   NEW_IDENT("Unknown"),   0                    );
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
