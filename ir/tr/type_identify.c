/*
 * Project:     libFIRM
 * File name:   ir/tr/type.c
 * Purpose:     Representation of types.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 *  file type.c - implementation of the datastructure to hold
 *  type information.
 *  (C) 2004 by Universitaet Karlsruhe
 *  Goetz Lindenmaier
 *
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "type_identify_t.h"

#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#include "type_t.h"
#include "tpop_t.h"
#include "irprog_t.h"
#include "typegmod.h"
#include "array.h"
#include "irprog_t.h"
#include "mangle.h"
#include "pset.h"

/* The hash set for types. */
static pset *type_table = NULL;

/* hash and compare types */
static hash_types_func_t    *hash_types_func;
static compare_types_func_t *compare_types_func;

int compare_names (const void *tp1, const void *tp2) {
  type *t1 = (type *) tp1;
  type *t2 = (type *) tp2;

  return (t1 != t2 &&
	  (t1->type_op !=  t2->type_op ||
	   t1->name    !=  t2->name      )  );
}

/* stuff for comparing two types. */
int compare_strict (const void *tp1, const void *tp2) {
  type *t1 = (type *) tp1;
  type *t2 = (type *) tp2;
  return t1 != t2;
}

/* stuff to compute a hash value for a type. */
int hash_name (type *tp) {
  unsigned h = (unsigned)tp->type_op;
  h = 9*h + (unsigned)tp->name;
  return h;
}

/* The function that hashes a type. */
type *mature_type(type *tp) {
  type *o;

  assert(type_table);

  o = pset_insert (type_table, tp, hash_types_func(tp) );

  if (!o || o == tp) return tp;

  exchange_types(tp, o);

  return o;
}


/* The function that hashes a type. */
type *mature_type_free(type *tp) {
  type *o;

  assert(type_table);

  o = pset_insert (type_table, tp, hash_types_func(tp) );

  if (!o || o == tp) return tp;

  free_type_entities(tp);
  free_type(tp);

  return o;
}

/* The function that hashes a type. */
type *mature_type_free_entities(type *tp) {
  type *o;

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
  hash_types_func    = ti_if && ti_if->hash ? ti_if->hash : hash_name;

  type_table = new_pset (compare_types_func, 8);
}
