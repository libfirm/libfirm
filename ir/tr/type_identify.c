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
 * @file    type_identify.c
 * @brief   Representation of types.
 * @author  Goetz Lindenmaier
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "typerep.h"

#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#include "type_t.h"
#include "tpop_t.h"
#include "irprog_t.h"
#include "array.h"
#include "irprog_t.h"
#include "pset.h"
#include "irtools.h"

/* The hash set for types. */
static pset *type_table = NULL;

/* hash and compare types */
static hash_types_func_t    *hash_types_func;
static compare_types_func_t *compare_types_func;

int compare_names (const void *tp1, const void *tp2) {
	ir_type *t1 = (ir_type *) tp1;
	ir_type *t2 = (ir_type *) tp2;

	return (t1 != t2 &&
	        (t1->type_op !=  t2->type_op ||
	         t1->name    !=  t2->name      ) );
}

/* stuff for comparing two types. */
int compare_strict(const void *tp1, const void *tp2) {
	const ir_type *t1 = tp1;
	const ir_type *t2 = tp2;
	return t1 != t2;
}

/* stuff to compute a hash value for a type. */
int firm_hash_name(ir_type *tp) {
	unsigned h = (unsigned)PTR_TO_INT(tp->type_op);
	h = 9*h + (unsigned)PTR_TO_INT(tp->name);
	return h;
}

/* The function that hashes a type. */
ir_type *mature_type(ir_type *tp) {
	ir_type *o;

	assert(type_table);

	o = pset_insert (type_table, tp, hash_types_func(tp) );
	if (!o || o == tp) return tp;
	exchange_types(tp, o);

	return o;
}


/* The function that hashes a type. */
ir_type *mature_type_free(ir_type *tp) {
	ir_type *o;

	assert(type_table);

	o = pset_insert (type_table, tp, hash_types_func(tp) );
	if (!o || o == tp) return tp;

	free_type_entities(tp);
	free_type(tp);

	return o;
}

/* The function that hashes a type. */
ir_type *mature_type_free_entities(ir_type *tp) {
	ir_type *o;

	assert(type_table);

	o = pset_insert (type_table, tp, hash_types_func(tp) );
	if (!o || o == tp) return tp;

	free_type_entities(tp);
	exchange_types(tp, o);

	return o;
}

/* initialize this module */
void init_type_identify(type_identify_if_t *ti_if) {
	compare_types_func = ti_if && ti_if->cmp  ? ti_if->cmp  : compare_strict;
	hash_types_func    = ti_if && ti_if->hash ? ti_if->hash : firm_hash_name;

	type_table = new_pset (compare_types_func, 8);
}
