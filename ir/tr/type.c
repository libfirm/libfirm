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
 *
 *   file type.c - implementation of the datastructure to hold
 *   type information.
 *  (C) 2001 by Universitaet Karlsruhe
 *  Goetz Lindenmaier
 *
 *  This module supplies a datastructure to represent all types
 *  known in the compiled program.  This includes types specified
 *  in the program as well as types defined by the language.  In the
 *  view of the intermediate representation there is no difference
 *  between these types.
 *
 *  There exist several kinds of types, arranged by the structure of
 *  the type.  A type is described by a set of attributes.  Some of
 *  these attributes are common to all types, others depend on the
 *  kind of the type.
 *
 *  Types are different from the modes defined in irmode:  Types are
 *  on the level of the programming language, modes at the level of
 *  the target processor.
 *
 * @see  type_t.h type tpop
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif


# include <stdlib.h>
# include <stddef.h>
# include <string.h>
# include "type_t.h"
# include "tpop_t.h"
# include "irprog_t.h"
# include "typegmod.h"
# include "array.h"
# include "irprog.h"
# include "mangle.h"
# include "tv.h"
# include "ircons.h"

/*******************************************************************/
/** TYPE                                                          **/
/*******************************************************************/

type *none_type;    type *get_none_type(void)    { return none_type;    }
type *unknown_type; type *get_unknown_type(void) { return unknown_type; }


#ifdef DEBUG_libfirm
/** Returns a new, unique number to number nodes or the like. */
int get_irp_new_node_nr(void);
#endif

/* Suffixes added to types used for pass-by-value representations. */
static ident *value_params_suffix = NULL;
static ident *value_ress_suffix = NULL;

void init_type(void) {
  value_params_suffix = new_id_from_str(VALUE_PARAMS_SUFFIX);
  value_ress_suffix   = new_id_from_str(VALUE_RESS_SUFFIX);

  /* construct none and unknown type. */
  none_type    = new_type(tpop_none,    mode_BAD, new_id_from_str("type_none"));
  set_type_size  (none_type, 0);
  set_type_state (none_type, layout_fixed);
  remove_irp_type(none_type);
  unknown_type = new_type(tpop_unknown, mode_ANY, new_id_from_str("type_unknown"));
  set_type_size  (unknown_type, 0);
  set_type_state (unknown_type, layout_fixed);
  remove_irp_type(unknown_type);
}

unsigned long type_visited;
INLINE void set_master_type_visited(unsigned long val) { type_visited = val; }
INLINE unsigned long get_master_type_visited() { return type_visited; }
INLINE void inc_master_type_visited() { type_visited++; }

void        free_type(type *tp) {
  if ((get_type_tpop(tp) == tpop_none) || (get_type_tpop(tp) == tpop_unknown))
    return;
  /* Remove from list of all types */
  remove_irp_type(tp);
  /* Free the attributes of the type. */
  free_type_attrs(tp);
  /* Free entities automatically allocated with the type */
  if (is_array_type(tp))
    free_entity(get_array_element_entity(tp));
  /* And now the type itself... */
  tp->kind = k_BAD;
  free(tp);
}

INLINE type *
new_type(tp_op *type_op, ir_mode *mode, ident* name) {
  type *res;
  int node_size ;

  assert(type_op != type_id);
  assert(!id_contains_char(name, ' ') && "type name should not contain spaces");

  node_size = offsetof (type, attr) +  type_op->attr_size;
  res = (type *) xmalloc (node_size);
  add_irp_type(res);   /* Remember the new type global. */

  res->kind = k_type;
  res->type_op = type_op;
  res->mode = mode;
  res->name = name;
  res->state = layout_undefined;
  res->size = -1;
  res->visit = 0;
  res -> link = NULL;
#ifdef DEBUG_libfirm
  res->nr = get_irp_new_node_nr();
#endif

  return res;
}

void free_type_attrs(type *tp) {
  switch(get_type_tpop_code(tp)) {
  case tpo_class:       { free_class_attrs(tp);       } break;
  case tpo_struct:      { free_struct_attrs(tp);      } break;
  case tpo_method:      { free_method_attrs(tp);      } break;
  case tpo_union:       { free_union_attrs(tp);       } break;
  case tpo_array:       { free_array_attrs(tp);       } break;
  case tpo_enumeration: { free_enumeration_attrs(tp); } break;
  case tpo_pointer:     { free_pointer_attrs(tp);     } break;
  case tpo_primitive:   { free_primitive_attrs(tp);   } break;
  default: break;
  }
}

/* set/get the link field */
void *get_type_link(type *tp)
{
  assert(tp && tp->kind == k_type);
  return(tp -> link);
}

void set_type_link(type *tp, void *l)
{
  assert(tp && tp->kind == k_type);
  tp -> link = l;
}

tp_op*      get_type_tpop(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->type_op;
}

ident*      get_type_tpop_nameid(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->type_op->name;
}

const char* get_type_tpop_name(type *tp) {
  assert(tp && tp->kind == k_type);
  return get_id_str(tp->type_op->name);
}

tp_opcode    get_type_tpop_code(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->type_op->code;
}

ir_mode*    get_type_mode(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->mode;
}

void        set_type_mode(type *tp, ir_mode* m) {
  assert(tp && tp->kind == k_type);

  assert(((tp->type_op != type_primitive)   || mode_is_data(m))     &&
	 /* Modes of primitives must be data */
	 ((tp->type_op != type_enumeration) || mode_is_int(m))      &&
         /* Modes of enumerations must be integers */
	 ((tp->type_op != type_pointer)     || mode_is_reference(m))   );
	 /* Modes of pointers must be references. */

  if ((tp->type_op == type_primitive)   ||
      (tp->type_op == type_enumeration) ||
      (tp->type_op == type_pointer)       ) {
    /* For pointer, primitive and enumeration size depends on the mode. */
    assert((get_mode_size_bytes(m) != -1) && "unorthodox modes not implemented");
    tp->size = get_mode_size_bytes(m);
    tp->mode = m;
  }
}

ident*      get_type_ident(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->name;
}

void        set_type_ident(type *tp, ident* id) {
  assert(tp && tp->kind == k_type);
  tp->name = id;
}

/* Outputs a unique number for this node */
INLINE long
get_type_nr(type *tp) {
  assert(tp);
#ifdef DEBUG_libfirm
  return tp->nr;
#else
  return (long)tp;
#endif
}

const char* get_type_name(type *tp) {
  assert(tp && tp->kind == k_type);
  return (get_id_str(tp->name));
}

int         get_type_size(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->size;
}

void
set_type_size(type *tp, int size) {
  assert(tp && tp->kind == k_type);
  /* For pointer enumeration and primitive size depends on the mode.
     Methods don't have a size. */
  if ((tp->type_op != type_pointer) && (tp->type_op != type_primitive) &&
      (tp->type_op != type_enumeration) && (tp->type_op != type_method))
    tp->size = size;
}

type_state
get_type_state(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->state;
}

void
set_type_state(type *tp, type_state state) {
  assert(tp && tp->kind == k_type);

  if ((tp->type_op == type_pointer) || (tp->type_op == type_primitive) ||
      (tp->type_op == type_method))
    return;

  /* Just a correctness check: */
  if (state == layout_fixed) {
    int i;
    switch (get_type_tpop_code(tp)) {
    case tpo_class:
      {
	assert(get_type_size(tp) > -1);
	if (tp != get_glob_type())
	  for (i = 0; i < get_class_n_members(tp); i++) {
	    if (get_entity_offset(get_class_member(tp, i)) <= -1)
	      { DDMT(tp); DDME(get_class_member(tp, i)); }
	    assert(get_entity_offset(get_class_member(tp, i)) > -1);
	    assert(is_method_type(get_entity_type(get_class_member(tp, i))) ||
		   (get_entity_allocation(get_class_member(tp, i)) == allocation_automatic));
	  }
      } break;
    case tpo_struct:
      {
	assert(get_type_size(tp) > -1);
	for (i = 0; i < get_struct_n_members(tp); i++) {
	  assert(get_entity_offset(get_struct_member(tp, i)) > -1);
	  assert((get_entity_allocation(get_struct_member(tp, i)) == allocation_automatic));
	}
      } break;
    case tpo_union:
      { /* ?? */
      } break;
    case tpo_array:
      { /* ??
	 Check order?
	 Assure that only innermost dimension is dynamic? */
      } break;
    case tpo_enumeration:
      {
	assert(get_type_mode != NULL);
	for (i = 0; i < get_enumeration_n_enums(tp); i++)
	  assert(get_enumeration_enum(tp, i) != NULL);
      } break;
    default: break;
    } /* switch (tp) */
  }
  tp->state = state;
}

unsigned long get_type_visited(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->visit;
}

void        set_type_visited(type *tp, unsigned long num) {
  assert(tp && tp->kind == k_type);
  tp->visit = num;
}
/* Sets visited field in type to type_visited. */
void        mark_type_visited(type *tp) {
  assert(tp && tp->kind == k_type);
  assert(tp->visit < type_visited);
  tp->visit = type_visited;
}
/* @@@ name clash with master flag
bool          type_visited(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->visit >= type_visited;
  }*/
bool          type_not_visited(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->visit  < type_visited;
}


int is_type            (void *thing) {
  assert(thing);
  if (get_kind(thing) == k_type)
    return 1;
  else
    return 0;
}


bool equal_type(type *typ1, type *typ2) {
  entity **m;
  type **t;
  int i, j;

  if (typ1 == typ2) return true;

  if ((get_type_tpop_code(typ1) != get_type_tpop_code(typ2)) ||
      (get_type_ident(typ1) != get_type_ident(typ2)) ||
      (get_type_mode(typ1) != get_type_mode(typ2)) ||
      (get_type_state(typ1) != get_type_state(typ2)))
    return false;
  if ((get_type_state(typ1) == layout_fixed) &&
      (get_type_size(typ1) != get_type_size(typ2)))
    return false;

  switch(get_type_tpop_code(typ1)) {
  case tpo_class:       {
    if (get_class_n_members(typ1) != get_class_n_members(typ2)) return false;
    if (get_class_n_subtypes(typ1) != get_class_n_subtypes(typ2)) return false;
    if (get_class_n_supertypes(typ1) != get_class_n_supertypes(typ2)) return false;
    if (get_class_peculiarity(typ1) != get_class_peculiarity(typ2)) return false;
    /** Compare the members **/
    m = alloca(sizeof(entity *) * get_class_n_members(typ1));
    memset(m, 0, sizeof(entity *) * get_class_n_members(typ1));
    /* First sort the members of typ2 */
    for (i = 0; i < get_class_n_members(typ1); i++) {
      entity *e1 = get_class_member(typ1, i);
      for (j = 0; j < get_class_n_members(typ2); j++) {
	entity *e2 = get_class_member(typ2, j);
	if (get_entity_name(e1) == get_entity_name(e2))
	  m[i] = e2;
      }
    }
    for (i = 0; i < get_class_n_members(typ1); i++) {
      if (!m[i]  ||  /* Found no counterpart */
	  !equal_entity(get_class_member(typ1, i), m[i]))
	return false;
    }
    /** Compare the supertypes **/
    t = alloca(sizeof(entity *) * get_class_n_supertypes(typ1));
    memset(t, 0, sizeof(entity *) * get_class_n_supertypes(typ1));
    /* First sort the supertypes of typ2 */
    for (i = 0; i < get_class_n_supertypes(typ1); i++) {
      type *t1 = get_class_supertype(typ1, i);
      for (j = 0; j < get_class_n_supertypes(typ2); j++) {
	type *t2 = get_class_supertype(typ2, j);
	if (get_type_ident(t2) == get_type_ident(t1))
	  t[i] = t2;
      }
    }
    for (i = 0; i < get_class_n_supertypes(typ1); i++) {
      if (!t[i]  ||  /* Found no counterpart */
	  get_class_supertype(typ1, i) != t[i])
	return false;
    }
  } break;
  case tpo_struct:      {
    if (get_struct_n_members(typ1) != get_struct_n_members(typ2)) return false;
    m = alloca(sizeof(entity *) * get_struct_n_members(typ1));
    memset(m, 0, sizeof(entity *) * get_struct_n_members(typ1));
    /* First sort the members of lt */
    for (i = 0; i < get_struct_n_members(typ1); i++) {
      entity *e1 = get_struct_member(typ1, i);
      for (j = 0; j < get_struct_n_members(typ2); j++) {
	entity *e2 = get_struct_member(typ2, j);
	if (get_entity_name(e1) == get_entity_name(e2))
	  m[i] = e2;
      }
    }
    for (i = 0; i < get_struct_n_members(typ1); i++) {
      if (!m[i]  ||  /* Found no counterpart */
	  !equal_entity(get_struct_member(typ1, i), m[i]))
	return false;
    }
  } break;
  case tpo_method:      {
    if (get_method_variadicity(typ1) != get_method_variadicity(typ2)) return false;
    if (get_method_n_params(typ1) != get_method_n_params(typ2)) return false;
    if (get_method_n_ress(typ1) != get_method_n_ress(typ2)) return false;
    for (i = 0; i < get_method_n_params(typ1); i++) {
      if (!equal_type(get_method_param_type(typ1, i), get_method_param_type(typ2, i)))
	return false;
    }
    for (i = 0; i < get_method_n_ress(typ1); i++) {
      if (!equal_type(get_method_res_type(typ1, i), get_method_res_type(typ2, i)))
	return false;
    }
  } break;
  case tpo_union:       {
    if (get_union_n_members(typ1) != get_union_n_members(typ2)) return false;
    m = alloca(sizeof(entity *) * get_union_n_members(typ1));
    memset(m, 0, sizeof(entity *) * get_union_n_members(typ1));
    /* First sort the members of lt */
    for (i = 0; i < get_union_n_members(typ1); i++) {
      entity *e1 = get_union_member(typ1, i);
      for (j = 0; j < get_union_n_members(typ2); j++) {
	entity *e2 = get_union_member(typ2, j);
	if (get_entity_name(e1) == get_entity_name(e2))
	  m[i] = e2;
      }
    }
    for (i = 0; i < get_union_n_members(typ1); i++) {
      if (!m[i]  ||  /* Found no counterpart */
	  !equal_entity(get_union_member(typ1, i), m[i]))
	return false;
    }
  } break;
  case tpo_array:       {
    if (get_array_n_dimensions(typ1) != get_array_n_dimensions(typ2))
      return false;
    if (!equal_type(get_array_element_type(typ1), get_array_element_type(typ2)))
      return false;
    for(i = 0; i < get_array_n_dimensions(typ1); i++) {
      if (get_array_lower_bound(typ1, i) != get_array_lower_bound(typ2, i) ||
	  get_array_upper_bound(typ1, i) != get_array_upper_bound(typ2, i))
	return false;
      if (get_array_order(typ1, i) != get_array_order(typ2, i))
	assert(0 && "type compare with different dimension orders not implemented");
    }
  } break;
  case tpo_enumeration: {
    assert(0 && "enumerations not implemented");
  } break;
  case tpo_pointer:     {
    if (get_pointer_points_to_type(typ1) != get_pointer_points_to_type(typ2))
      return false;
  } break;
  case tpo_primitive:   {
  } break;
  default: break;
  }
  return true;
}

bool smaller_type (type *st, type *lt) {
  entity **m;
  int i, j;

  if (st == lt) return true;

  if (get_type_tpop_code(st) != get_type_tpop_code(lt))
    return false;

  switch(get_type_tpop_code(st)) {
  case tpo_class:       {
    return is_subclass_of(st, lt);
  } break;
  case tpo_struct:      {
    if (get_struct_n_members(st) != get_struct_n_members(lt)) return false;
    m = alloca(sizeof(entity *) * get_struct_n_members(st));
    memset(m, 0, sizeof(entity *) * get_struct_n_members(st));
    /* First sort the members of lt */
    for (i = 0; i < get_struct_n_members(st); i++) {
      entity *se = get_struct_member(st, i);
      for (j = 0; j < get_struct_n_members(lt); j++) {
	entity *le = get_struct_member(lt, j);
	if (get_entity_name(le) == get_entity_name(se))
	  m[i] = le;
      }
    }
    for (i = 0; i < get_struct_n_members(st); i++) {
      if (!m[i]  ||  /* Found no counterpart */
	  !smaller_type(get_entity_type(get_struct_member(st, i)),
			get_entity_type(m[i])))
	return false;
    }
  } break;
  case tpo_method:      {
    if (get_method_variadicity(st) != get_method_variadicity(lt)) return false;
    if (get_method_n_params(st) != get_method_n_params(lt)) return false;
    if (get_method_n_ress(st) != get_method_n_ress(lt)) return false;
    for (i = 0; i < get_method_n_params(st); i++) {
      if (!smaller_type(get_method_param_type(st, i), get_method_param_type(lt, i)))
	return false;
    }
    for (i = 0; i < get_method_n_ress(st); i++) {
      if (!smaller_type(get_method_res_type(st, i), get_method_res_type(lt, i)))
	return false;
    }
  } break;
  case tpo_union:       {
    if (get_union_n_members(st) != get_union_n_members(lt)) return false;
    m = alloca(sizeof(entity *) * get_union_n_members(st));
    memset(m, 0, sizeof(entity *) * get_union_n_members(st));
    /* First sort the members of lt */
    for (i = 0; i < get_union_n_members(st); i++) {
      entity *se = get_union_member(st, i);
      for (j = 0; j < get_union_n_members(lt); j++) {
	entity *le = get_union_member(lt, j);
	if (get_entity_name(le) == get_entity_name(se))
	  m[i] = le;
      }
    }
    for (i = 0; i < get_union_n_members(st); i++) {
      if (!m[i]  ||  /* Found no counterpart */
	  !smaller_type(get_entity_type(get_union_member(st, i)),
			get_entity_type(m[i])))
	return false;
    }
  } break;
  case tpo_array:       {
    type *set, *let;  /* small/large elt. type */
    if (get_array_n_dimensions(st) != get_array_n_dimensions(lt))
      return false;
    set = get_array_element_type(st);
    let = get_array_element_type(lt);
    if (set != let) {
      /* If the elt types are different, set must be convertible
	 to let, and they must have the same size so that address
	 computations work out.  To have a size the layout must
	 be fixed. */
      if ((get_type_state(set) != layout_fixed) ||
	  (get_type_state(let) != layout_fixed))
	return false;
      if (!smaller_type(set, let) ||
	  get_type_size(set) != get_type_size(let))
	return false;
    }
    for(i = 0; i < get_array_n_dimensions(st); i++) {
      if (get_array_lower_bound(lt, i))
	if(get_array_lower_bound(st, i) != get_array_lower_bound(lt, i))
	  return false;
      if (get_array_upper_bound(lt, i))
	if(get_array_upper_bound(st, i) != get_array_upper_bound(lt, i))
	  return false;
    }
  } break;
  case tpo_enumeration: {
    assert(0 && "enumerations not implemented");
  } break;
  case tpo_pointer:     {
    if (!smaller_type(get_pointer_points_to_type(st),
		      get_pointer_points_to_type(lt)))
      return false;
  } break;
  case tpo_primitive:   {
    if (!smaller_mode(get_type_mode(st), get_type_mode(lt)))
      return false;
  } break;
  default: break;
  }
  return true;
}

/*******************************************************************/
/** TYPE_CLASS                                                    **/
/*******************************************************************/

/* create a new class type */
INLINE type   *new_type_class (ident *name) {
  type *res;

  res = new_type(type_class, NULL, name);

  res->attr.ca.members     = NEW_ARR_F (entity *, 1);
  res->attr.ca.subtypes    = NEW_ARR_F (type *, 1);
  res->attr.ca.supertypes  = NEW_ARR_F (type *, 1);
  res->attr.ca.peculiarity = peculiarity_existent;
  res->attr.ca.dfn         = 0;

  return res;
}
type   *new_d_type_class (ident *name, dbg_info* db) {
  type *res = new_type_class (name);
  set_type_dbg_info(res, db);
  return res;
}
INLINE void free_class_attrs(type *clss) {
  assert(clss && (clss->type_op == type_class));
  DEL_ARR_F(clss->attr.ca.members);
  DEL_ARR_F(clss->attr.ca.subtypes);
  DEL_ARR_F(clss->attr.ca.supertypes);
}

/* manipulate private fields of class type  */
void    add_class_member   (type *clss, entity *member) {
  assert(clss && (clss->type_op == type_class));
  ARR_APP1 (entity *, clss->attr.ca.members, member);
}
int     get_class_n_members (type *clss) {
  assert(clss && (clss->type_op == type_class));
  return (ARR_LEN (clss->attr.ca.members))-1;
}
int     get_class_member_index(type *clss, entity *mem) {
  int i;
  assert(clss && (clss->type_op == type_class));
  for (i = 0; i < get_class_n_members(clss); i++)
    if (get_class_member(clss, i) == mem)
      return i;
  return -1;
}
entity *get_class_member   (type *clss, int pos) {
  assert(clss && (clss->type_op == type_class));
  assert(pos >= 0 && pos < get_class_n_members(clss));
  return clss->attr.ca.members[pos+1];
}
void    set_class_member   (type *clss, entity *member, int pos) {
  assert(clss && (clss->type_op == type_class));
  assert(pos >= 0 && pos < get_class_n_members(clss));
  clss->attr.ca.members[pos+1] = member;
}
void    set_class_members  (type *clss, entity **members, int arity) {
  int i;
  assert(clss && (clss->type_op == type_class));
  DEL_ARR_F(clss->attr.ca.members);
  clss->attr.ca.members    = NEW_ARR_F (entity *, 1);
  for (i = 0; i < arity; i++) {
    set_entity_owner(members[i], clss);
    ARR_APP1 (entity *, clss->attr.ca.members, members[i]);
  }
}
void    remove_class_member(type *clss, entity *member) {
  int i;
  assert(clss && (clss->type_op == type_class));
  for (i = 1; i < (ARR_LEN (clss->attr.ca.members)); i++) {
    if (clss->attr.ca.members[i] == member) {
      for(; i < (ARR_LEN (clss->attr.ca.members)) - 1; i++)
	clss->attr.ca.members[i] = clss->attr.ca.members[i + 1];
      ARR_SETLEN(entity*, clss->attr.ca.members, ARR_LEN(clss->attr.ca.members) - 1);
      break;
    }
  }
}

void    add_class_subtype   (type *clss, type *subtype) {
  int i;
  assert(clss && (clss->type_op == type_class));
  ARR_APP1 (type *, clss->attr.ca.subtypes, subtype);
  for (i = 0; i < get_class_n_supertypes(subtype); i++)
    if (get_class_supertype(subtype, i) == clss)
      /* Class already registered */
      return;
  ARR_APP1 (type *, subtype->attr.ca.supertypes, clss);
}
int     get_class_n_subtypes (type *clss) {
  assert(clss && (clss->type_op == type_class));
  return (ARR_LEN (clss->attr.ca.subtypes))-1;
}
type   *get_class_subtype   (type *clss, int pos) {
  assert(clss && (clss->type_op == type_class));
  assert(pos >= 0 && pos < get_class_n_subtypes(clss));
  return clss->attr.ca.subtypes[pos+1] = skip_tid(clss->attr.ca.subtypes[pos+1]);
}
void    set_class_subtype   (type *clss, type *subtype, int pos) {
  assert(clss && (clss->type_op == type_class));
  assert(pos >= 0 && pos < get_class_n_subtypes(clss));
  clss->attr.ca.subtypes[pos+1] = subtype;
}
void    remove_class_subtype(type *clss, type *subtype) {
  int i;
  assert(clss && (clss->type_op == type_class));
  for (i = 1; i < (ARR_LEN (clss->attr.ca.subtypes)); i++)
    if (clss->attr.ca.subtypes[i] == subtype) {
      for(; i < (ARR_LEN (clss->attr.ca.subtypes))-1; i++)
	clss->attr.ca.subtypes[i] = clss->attr.ca.subtypes[i+1];
      ARR_SETLEN(entity*, clss->attr.ca.subtypes, ARR_LEN(clss->attr.ca.subtypes) - 1);
      break;
    }
}

void    add_class_supertype   (type *clss, type *supertype) {
  int i;
  assert(clss && (clss->type_op == type_class));
  assert(supertype && (supertype -> type_op == type_class));
  ARR_APP1 (type *, clss->attr.ca.supertypes, supertype);
  for (i = 0; i < get_class_n_subtypes(supertype); i++)
    if (get_class_subtype(supertype, i) == clss)
      /* Class already registered */
      return;
  ARR_APP1 (type *, supertype->attr.ca.subtypes, clss);
}
int     get_class_n_supertypes (type *clss) {
  assert(clss && (clss->type_op == type_class));
  return (ARR_LEN (clss->attr.ca.supertypes))-1;
}
int get_class_supertype_index(type *clss, type *super_clss) {
  int i;
  assert(clss && (clss->type_op == type_class));
  assert(super_clss && (super_clss->type_op == type_class));
  for (i = 0; i < get_class_n_supertypes(clss); i++)
    if (get_class_supertype(clss, i) == super_clss)
      return i;
  return -1;
}
type   *get_class_supertype   (type *clss, int pos) {
  assert(clss && (clss->type_op == type_class));
  assert(pos >= 0 && pos < get_class_n_supertypes(clss));
  return clss->attr.ca.supertypes[pos+1] = skip_tid(clss->attr.ca.supertypes[pos+1]);
}
void    set_class_supertype   (type *clss, type *supertype, int pos) {
  assert(clss && (clss->type_op == type_class));
  assert(pos >= 0 && pos < get_class_n_supertypes(clss));
  clss->attr.ca.supertypes[pos+1] = supertype;
}
void    remove_class_supertype(type *clss, type *supertype) {
  int i;
  assert(clss && (clss->type_op == type_class));
  for (i = 1; i < (ARR_LEN (clss->attr.ca.supertypes)); i++)
    if (clss->attr.ca.supertypes[i] == supertype) {
      for(; i < (ARR_LEN (clss->attr.ca.supertypes))-1; i++)
	clss->attr.ca.supertypes[i] = clss->attr.ca.supertypes[i+1];
      ARR_SETLEN(entity*, clss->attr.ca.supertypes, ARR_LEN(clss->attr.ca.supertypes) - 1);
      break;
    }
}

char *get_peculiarity_string(peculiarity p) {
  if (p == peculiarity_description)
    return "peculiarity_description";
  if (p == peculiarity_inherited)
    return "peculiarity_inherited";
  return "peculiarity_existent";
}

INLINE peculiarity get_class_peculiarity (type *clss) {
  assert(clss && (clss->type_op == type_class));
  return clss->attr.ca.peculiarity;
}
INLINE void        set_class_peculiarity (type *clss, peculiarity pec) {
  assert(clss && (clss->type_op == type_class));
  assert(pec != peculiarity_inherited);  /* There is no inheritance of types in libFirm. */
  clss->attr.ca.peculiarity = pec;
}

void set_class_dfn (type *clss, int dfn)
{
  clss->attr.ca.dfn        = dfn;
}

int get_class_dfn (type *clss)
{
  return (clss->attr.ca.dfn);
}

/* typecheck */
bool    is_class_type(type *clss) {
  assert(clss);
  if (clss->type_op == type_class) return 1; else return 0;
}

bool is_subclass_of(type *low, type *high) {
  int i;
  assert(is_class_type(low) && is_class_type(high));
  if (low == high) return true;
  /* depth first search from high downwards. */
  for (i = 0; i < get_class_n_subtypes(high); i++) {
    if (low == get_class_subtype(high, i))
      return true;
    if (is_subclass_of(low, get_class_subtype(high, i)))
      return true;
  }
  return false;
}

/*******************************************************************/
/** TYPE_STRUCT                                                   **/
/*******************************************************************/

/* create a new type struct */
INLINE type   *new_type_struct (ident *name) {
  type *res;
  res = new_type(type_struct, NULL, name);
  res->attr.sa.members = NEW_ARR_F (entity *, 1);
  return res;
}
type   *new_d_type_struct (ident *name, dbg_info* db) {
  type *res = new_type_struct (name);
  set_type_dbg_info(res, db);
  return res;
}
INLINE void free_struct_attrs (type *strct) {
  assert(strct && (strct->type_op == type_struct));
  DEL_ARR_F(strct->attr.sa.members);
}

/* manipulate private fields of struct */
int     get_struct_n_members (type *strct) {
  assert(strct && (strct->type_op == type_struct));
  return (ARR_LEN (strct->attr.sa.members))-1;
}
void    add_struct_member   (type *strct, entity *member) {
  assert(strct && (strct->type_op == type_struct));
  assert(get_type_tpop(get_entity_type(member)) != type_method);
    /*    @@@ lowerfirm geht nicht durch */
  ARR_APP1 (entity *, strct->attr.sa.members, member);
}
entity *get_struct_member   (type *strct, int pos) {
  assert(strct && (strct->type_op == type_struct));
  assert(pos >= 0 && pos < get_struct_n_members(strct));
  return strct->attr.sa.members[pos+1];
}
void    set_struct_member   (type *strct, int pos, entity *member) {
  assert(strct && (strct->type_op == type_struct));
  assert(pos >= 0 && pos < get_struct_n_members(strct));
  assert(get_entity_type(member)->type_op != type_method);/* @@@ lowerfirm !!*/
  strct->attr.sa.members[pos+1] = member;
}
void    remove_struct_member(type *strct, entity *member) {
  int i;
  assert(strct && (strct->type_op == type_struct));
  for (i = 1; i < (ARR_LEN (strct->attr.sa.members)); i++)
    if (strct->attr.sa.members[i] == member) {
      for(; i < (ARR_LEN (strct->attr.sa.members))-1; i++)
	strct->attr.sa.members[i] = strct->attr.sa.members[i+1];
      ARR_SETLEN(entity*, strct->attr.sa.members, ARR_LEN(strct->attr.sa.members) - 1);
      break;
    }
}

/* typecheck */
bool    is_struct_type(type *strct) {
  assert(strct);
  if (strct->type_op == type_struct) return 1; else return 0;
}

/*******************************************************************/
/** TYPE_METHOD                                                   **/
/*******************************************************************/

/* Lazy construction of value argument / result representation. */
static INLINE type *
build_value_type(ident *name, int len, type **tps) {
  int i;
  type *res = new_type_struct(name);
  /* Remove type from type list.  Must be treated differently than other types. */
  remove_irp_type_from_list(res);
  for (i = 0; i < len; i++) {
    type *elt_type = res;   /* use res as default if corresponding type is not yet set. */
    if (tps[i]) elt_type = tps[i];
    new_entity(res, mangle_u(name, get_type_ident(elt_type)), elt_type);
  }
  return res;
}

/* Create a new method type.
   N_param is the number of parameters, n_res the number of results.  */
INLINE type *new_type_method (ident *name, int n_param, int n_res) {
  type *res;
  res = new_type(type_method, mode_P_mach, name);
  res->state = layout_fixed;
  assert((get_mode_size_bytes(mode_P_mach) != -1) && "unorthodox modes not implemented");
  res->size = get_mode_size_bytes(mode_P_mach);
  res->attr.ma.n_params     = n_param;
  res->attr.ma.param_type   = (type **) xmalloc (sizeof (type *) * n_param);
  res->attr.ma.value_params = NULL;
  res->attr.ma.n_res        = n_res;
  res->attr.ma.res_type     = (type **) xmalloc (sizeof (type *) * n_res);
  res->attr.ma.value_ress   = NULL;
  res->attr.ma.variadicity  = variadicity_non_variadic;

  return res;
}

type *new_d_type_method (ident *name, int n_param, int n_res, dbg_info* db) {
  type *res = new_type_method (name, n_param, n_res);
  set_type_dbg_info(res, db);
  return res;
}

INLINE void free_method_attrs(type *method) {
  assert(method && (method->type_op == type_method));
  free(method->attr.ma.param_type);
  free(method->attr.ma.res_type);
}

/* manipulate private fields of method. */
int   get_method_n_params  (type *method) {
  assert(method && (method->type_op == type_method));
  return method->attr.ma.n_params;
}
type *get_method_param_type(type *method, int pos) {
  type *res;
  assert(method && (method->type_op == type_method));
  assert(pos >= 0 && pos < get_method_n_params(method));
  res = method->attr.ma.param_type[pos];
  assert(res != NULL && "empty method param type");
  return method->attr.ma.param_type[pos] = skip_tid(res);
}
void  set_method_param_type(type *method, int pos, type* tp) {
  assert(method && (method->type_op == type_method));
  assert(pos >= 0 && pos < get_method_n_params(method));
  method->attr.ma.param_type[pos] = tp;
  /* If information constructed set pass-by-value representation. */
  if (method->attr.ma.value_params) {
    assert(get_method_n_params(method) == get_struct_n_members(method->attr.ma.value_params));
    set_entity_type(get_struct_member(method->attr.ma.value_params, pos), tp);
  }
}
/* Returns an entity that represents the copied value argument.  Only necessary
   for compounds passed by value. */
entity *get_method_value_param_ent(type *method, int pos) {
  assert(method && (method->type_op == type_method));
  assert(pos >= 0 && pos < get_method_n_params(method));
  if (!method->attr.ma.value_params)
    method->attr.ma.value_params
      = build_value_type(mangle_u(get_type_ident(method), value_params_suffix),
			 get_method_n_params(method), method->attr.ma.param_type);
  assert((get_entity_type(get_struct_member(method->attr.ma.value_params, pos))
	  != method->attr.ma.value_params)
	 && "param type not yet set");
  return get_struct_member(method->attr.ma.value_params, pos);
}

type *get_method_value_res_type(type *method) {
  assert(method && (method->type_op == type_method));
  return method->attr.ma.value_params;
}


int   get_method_n_ress   (type *method) {
  assert(method && (method->type_op == type_method));
  return method->attr.ma.n_res;
}
type *get_method_res_type(type *method, int pos) {
  type *res;
  assert(method && (method->type_op == type_method));
  assert(pos >= 0 && pos < get_method_n_ress(method));
  res = method->attr.ma.res_type[pos];
  assert(res != NULL && "empty method return type");
  return method->attr.ma.res_type[pos] = skip_tid(res);
}
void  set_method_res_type(type *method, int pos, type* tp) {
  assert(method && (method->type_op == type_method));
  assert(pos >= 0 && pos < get_method_n_ress(method));
  /* set the result type */
  method->attr.ma.res_type[pos] = tp;
  /* If information constructed set pass-by-value representation. */
  if (method->attr.ma.value_ress) {
    assert(get_method_n_ress(method) == get_struct_n_members(method->attr.ma.value_ress));
    set_entity_type(get_struct_member(method->attr.ma.value_ress, pos), tp);
  }
}
/* Returns an entity that represents the copied value result.  Only necessary
   for compounds passed by value. */
entity *get_method_value_res_ent(type *method, int pos) {
  assert(method && (method->type_op == type_method));
  assert(pos >= 0 && pos < get_method_n_ress(method));
  if (!method->attr.ma.value_ress)
    method->attr.ma.value_ress
      = build_value_type(mangle_u(get_type_ident(method), value_ress_suffix),
			 get_method_n_ress(method), method->attr.ma.res_type);
  assert((get_entity_type(get_struct_member(method->attr.ma.value_ress, pos)) != method->attr.ma.value_ress)
	 && "result type not yet set");
  return get_struct_member(method->attr.ma.value_ress, pos);
}

/* Returns the null-terminated name of this variadicity. */
const char *get_variadicity_name(variadicity vari)
{
#define X(a)	case a: return #a
  switch (vari) {
    X(variadicity_non_variadic);
    X(variadicity_variadic);
    default:
      return "BAD VALUE";
  }
#undef X
}

variadicity get_method_variadicity(type *method)
{
  assert(method && (method->type_op == type_method));
  return method->attr.ma.variadicity;
}

void set_method_variadicity(type *method, variadicity vari)
{
  assert(method && (method->type_op == type_method));
  method->attr.ma.variadicity = vari;
}

/* typecheck */
bool  is_method_type     (type *method) {
  assert(method);
  if (method->type_op == type_method) return 1; else return 0;
}

/*******************************************************************/
/** TYPE_UNION                                                    **/
/*******************************************************************/

/* create a new type uni */
INLINE type  *new_type_union (ident *name) {
  type *res;
  res = new_type(type_union, NULL, name);
  /*res->attr.ua.unioned_type = (type **)  xmalloc (sizeof (type *)  * n_types);
    res->attr.ua.delim_names  = (ident **) xmalloc (sizeof (ident *) * n_types); */
  res->attr.ua.members = NEW_ARR_F (entity *, 1);
  return res;
}
type  *new_d_type_union (ident *name, dbg_info* db) {
  type *res = new_type_union (name);
  set_type_dbg_info(res, db);
  return res;
}
INLINE void free_union_attrs (type *uni) {
  assert(uni && (uni->type_op == type_union));
  DEL_ARR_F(uni->attr.ua.members);
}
/* manipulate private fields of union */
#if 0
int    get_union_n_types      (type *uni) {
  assert(uni && (uni->type_op == type_union));
  return uni->attr.ua.n_types;
}
type  *get_union_unioned_type (type *uni, int pos) {
  assert(uni && (uni->type_op == type_union));
  assert(pos >= 0 && pos < get_union_n_types(uni));
  return uni->attr.ua.unioned_type[pos] = skip_tid(uni->attr.ua.unioned_type[pos]);
}
void   set_union_unioned_type (type *uni, int pos, type *tp) {
  assert(uni && (uni->type_op == type_union));
  assert(pos >= 0 && pos < get_union_n_types(uni));
  uni->attr.ua.unioned_type[pos] = tp;
}
ident *get_union_delim_nameid (type *uni, int pos) {
  assert(uni && (uni->type_op == type_union));
  assert(pos >= 0 && pos < get_union_n_types(uni));
  return uni->attr.ua.delim_names[pos];
}
const char *get_union_delim_name (type *uni, int pos) {
  assert(uni && (uni->type_op == type_union));
  assert(pos >= 0 && pos < get_union_n_types(uni));
  return get_id_str(uni->attr.ua.delim_names[pos]);
}
void   set_union_delim_nameid (type *uni, int pos, ident *id) {
  assert(uni && (uni->type_op == type_union));
  assert(pos >= 0 && pos < get_union_n_types(uni));
  uni->attr.ua.delim_names[pos] = id;
}
#endif
int    get_union_n_members      (type *uni) {
  assert(uni && (uni->type_op == type_union));
  return (ARR_LEN (uni->attr.ua.members))-1;
}
void    add_union_member   (type *uni, entity *member) {
  assert(uni && (uni->type_op == type_union));
  ARR_APP1 (entity *, uni->attr.ua.members, member);
}
entity  *get_union_member (type *uni, int pos) {
  assert(uni && (uni->type_op == type_union));
  assert(pos >= 0 && pos < get_union_n_members(uni));
  return uni->attr.ua.members[pos+1];
}
void   set_union_member (type *uni, int pos, entity *member) {
  assert(uni && (uni->type_op == type_union));
  assert(pos >= 0 && pos < get_union_n_members(uni));
  uni->attr.ua.members[pos+1] = member;
}
void   remove_union_member(type *uni, entity *member) {
  int i;
  assert(uni && (uni->type_op == type_union));
  for (i = 1; i < (ARR_LEN (uni->attr.ua.members)); i++)
    if (uni->attr.ua.members[i] == member) {
      for(; i < (ARR_LEN (uni->attr.ua.members))-1; i++)
	uni->attr.ua.members[i] = uni->attr.ua.members[i+1];
      ARR_SETLEN(entity*, uni->attr.ua.members, ARR_LEN(uni->attr.ua.members) - 1);
      break;
    }
}

/* typecheck */
bool   is_union_type         (type *uni) {
  assert(uni);
  if (uni->type_op == type_union) return 1; else return 0;
}

/*******************************************************************/
/** TYPE_ARRAY                                                    **/
/*******************************************************************/


/* create a new type array -- set dimension sizes independently */
INLINE type *new_type_array         (ident *name, int n_dimensions,
			      type *element_type) {
  type *res;
  int i;
  ir_graph *rem = current_ir_graph;
  assert(!is_method_type(element_type));

  res = new_type(type_array, NULL, name);
  res->attr.aa.n_dimensions = n_dimensions;
  res->attr.aa.lower_bound  = (ir_node **) xmalloc (sizeof (ir_node *) * n_dimensions);
  res->attr.aa.upper_bound  = (ir_node **) xmalloc (sizeof (ir_node *) * n_dimensions);
  res->attr.aa.order  = (int *) xmalloc (sizeof (int) * n_dimensions);

  current_ir_graph = get_const_code_irg();
  for (i = 0; i < n_dimensions; i++) {
    res->attr.aa.lower_bound[i]  = new_Unknown();
    res->attr.aa.upper_bound[i]  = new_Unknown();
    res->attr.aa.order[i] = i;
  }
  current_ir_graph = rem;

  res->attr.aa.element_type = element_type;
  new_entity(res, mangle_u(name, id_from_str("elem_ent", 8)), element_type);

  return res;
}
type *new_d_type_array (ident *name, int n_dimensions,
			type *element_type, dbg_info* db) {
  type *res = new_type_array (name, n_dimensions, element_type);
  set_type_dbg_info(res, db);
  return res;
}

INLINE void free_array_attrs (type *array) {
  assert(array && (array->type_op == type_array));
  free(array->attr.aa.lower_bound);
  free(array->attr.aa.upper_bound);
}

/* manipulate private fields of array type */
int   get_array_n_dimensions (type *array) {
  assert(array && (array->type_op == type_array));
  return array->attr.aa.n_dimensions;
}

INLINE void
set_array_bounds (type *array, int dimension, ir_node * lower_bound,
		  ir_node * upper_bound) {
  assert(array && (array->type_op == type_array));
  assert(lower_bound && "lower_bound node may not be NULL.");
  assert(upper_bound && "upper_bound node may not be NULL.");
  assert(dimension < array->attr.aa.n_dimensions && dimension >= 0);
  array->attr.aa.lower_bound[dimension] = lower_bound;
  array->attr.aa.upper_bound[dimension] = upper_bound;
}
void
set_array_bounds_int (type *array, int dimension, int lower_bound,
		      int upper_bound) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = get_const_code_irg();
  set_array_bounds (array, dimension,
		    new_Const(mode_Iu, new_tarval_from_long (lower_bound, mode_Iu)),
		    new_Const(mode_Iu, new_tarval_from_long (upper_bound, mode_Iu )));
  current_ir_graph = rem;
}
INLINE void
set_array_lower_bound  (type *array, int dimension, ir_node * lower_bound) {
  assert(array && (array->type_op == type_array));
  assert(lower_bound && "lower_bound node may not be NULL.");
  array->attr.aa.lower_bound[dimension] = lower_bound;
}
void  set_array_lower_bound_int (type *array, int dimension, int lower_bound) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = get_const_code_irg();
  set_array_lower_bound  (array, dimension,
			  new_Const(mode_Iu, new_tarval_from_long (lower_bound, mode_Iu)));
  current_ir_graph = rem;
}
INLINE void
set_array_upper_bound  (type *array, int dimension, ir_node * upper_bound) {
  assert(array && (array->type_op == type_array));
  assert(upper_bound && "upper_bound node may not be NULL.");
  array->attr.aa.upper_bound[dimension] = upper_bound;
}
void  set_array_upper_bound_int (type *array, int dimension, int upper_bound) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = get_const_code_irg();
  set_array_upper_bound  (array, dimension,
			  new_Const(mode_Iu, new_tarval_from_long (upper_bound, mode_Iu)));
  current_ir_graph = rem;
}
int       has_array_lower_bound  (type *array, int dimension) {
  assert(array && (array->type_op == type_array));
  return (get_irn_op(array->attr.aa.lower_bound[dimension]) != op_Unknown);
}
ir_node * get_array_lower_bound  (type *array, int dimension) {
  assert(array && (array->type_op == type_array));
  return array->attr.aa.lower_bound[dimension];
}
int       has_array_upper_bound  (type *array, int dimension) {
  assert(array && (array->type_op == type_array));
  return (get_irn_op(array->attr.aa.upper_bound[dimension]) != op_Unknown);
}
ir_node * get_array_upper_bound  (type *array, int dimension) {
  assert(array && (array->type_op == type_array));
  return array->attr.aa.upper_bound[dimension];
}

void set_array_order (type *array, int dimension, int order) {
  assert(array && (array->type_op == type_array));
  array->attr.aa.order[dimension] = order;
}
int  get_array_order (type *array, int dimension) {
  assert(array && (array->type_op == type_array));
  return array->attr.aa.order[dimension];
}

void  set_array_element_type (type *array, type *tp) {
  assert(array && (array->type_op == type_array));
  assert(!is_method_type(tp));
  array->attr.aa.element_type = tp;
}
type *get_array_element_type (type *array) {
  assert(array && (array->type_op == type_array));
  return array->attr.aa.element_type = skip_tid(array->attr.aa.element_type);
}

void  set_array_element_entity (type *array, entity *ent) {
  assert(array && (array->type_op == type_array));
  assert((get_entity_type(ent)->type_op != type_method));
  array->attr.aa.element_ent = ent;
  array->attr.aa.element_type = get_entity_type(ent);
}
entity *get_array_element_entity (type *array) {
  assert(array && (array->type_op == type_array));
  return array->attr.aa.element_ent;
}

/* typecheck */
bool   is_array_type         (type *array) {
  assert(array);
  if (array->type_op == type_array) return 1; else return 0;
}

/*******************************************************************/
/** TYPE_ENUMERATION                                              **/
/*******************************************************************/

/* create a new type enumeration -- set the enumerators independently */
INLINE type   *new_type_enumeration    (ident *name, int n_enums) {
  type *res;
  int i;
  res = new_type(type_enumeration, NULL, name);
  res->attr.ea.n_enums     = n_enums;
  res->attr.ea.enumer      = (tarval **) xmalloc (sizeof (tarval *) * n_enums);
  res->attr.ea.enum_nameid = (ident  **) xmalloc (sizeof (ident  *) * n_enums);
  for (i = 0; i < n_enums; i++) {
    res->attr.ea.enumer[i] = NULL;
    res->attr.ea.enum_nameid  = NULL;
  }
  return res;
}
type   *new_d_type_enumeration    (ident *name, int n_enums, dbg_info* db) {
  type *res = new_type_enumeration (name, n_enums);
  set_type_dbg_info(res, db);
  return res;
}

INLINE void free_enumeration_attrs(type *enumeration) {
  assert(enumeration && (enumeration->type_op == type_enumeration));
  free(enumeration->attr.ea.enumer);
  free(enumeration->attr.ea.enum_nameid);
}

/* manipulate fields of enumeration type. */
int     get_enumeration_n_enums (type *enumeration) {
  assert(enumeration && (enumeration->type_op == type_enumeration));
  return enumeration->attr.ea.n_enums;
}
void    set_enumeration_enum    (type *enumeration, int pos, tarval *con) {
  assert(enumeration && (enumeration->type_op == type_enumeration));
  assert(pos >= 0 && pos < get_enumeration_n_enums(enumeration));
  enumeration->attr.ea.enumer[pos] = con;
}
tarval *get_enumeration_enum    (type *enumeration, int pos) {
  assert(enumeration && (enumeration->type_op == type_enumeration));
  assert(pos >= 0 && pos < get_enumeration_n_enums(enumeration));
  return enumeration->attr.ea.enumer[pos];
}
void    set_enumeration_nameid  (type *enumeration, int pos, ident *id) {
  assert(enumeration && (enumeration->type_op == type_enumeration));
  assert(pos >= 0 && pos < get_enumeration_n_enums(enumeration));
  enumeration->attr.ea.enum_nameid[pos] = id;
}
ident  *get_enumeration_nameid  (type *enumeration, int pos) {
  assert(enumeration && (enumeration->type_op == type_enumeration));
  assert(pos >= 0 && pos < get_enumeration_n_enums(enumeration));
  return enumeration->attr.ea.enum_nameid[pos];
}
const char *get_enumeration_name(type *enumeration, int pos) {
  assert(enumeration && (enumeration->type_op == type_enumeration));
  assert(pos >= 0 && pos < get_enumeration_n_enums(enumeration));
  return get_id_str(enumeration->attr.ea.enum_nameid[pos]);
}

/* typecheck */
bool    is_enumeration_type     (type *enumeration) {
  assert(enumeration);
  if (enumeration->type_op == type_enumeration) return 1; else return 0;
}

/*******************************************************************/
/** TYPE_POINTER                                                  **/
/*******************************************************************/

/* Create a new type pointer */
INLINE type *new_type_pointer_mode (ident *name, type *points_to, ir_mode *ptr_mode) {
  type *res;
  assert(mode_is_reference(ptr_mode));
  res = new_type(type_pointer, ptr_mode, name);
  res->attr.pa.points_to = points_to;
  assert((get_mode_size_bytes(res->mode) != -1) && "unorthodox modes not implemented");
  res->size = get_mode_size_bytes(res->mode);
  res->state = layout_fixed;
  return res;
}
type *new_d_type_pointer (ident *name, type *points_to, ir_mode *ptr_mode, dbg_info* db) {
  type *res = new_type_pointer_mode (name, points_to, ptr_mode);
  set_type_dbg_info(res, db);
  return res;
}
INLINE void free_pointer_attrs (type *pointer) {
  assert(pointer && (pointer->type_op == type_pointer));
}
/* manipulate fields of type_pointer */
void  set_pointer_points_to_type (type *pointer, type *tp) {
  assert(pointer && (pointer->type_op == type_pointer));
  pointer->attr.pa.points_to = tp;
}
type *get_pointer_points_to_type (type *pointer) {
  assert(pointer && (pointer->type_op == type_pointer));
  return pointer->attr.pa.points_to = skip_tid(pointer->attr.pa.points_to);
}

/* typecheck */
bool  is_pointer_type            (type *pointer) {
  assert(pointer);
  if (pointer->type_op == type_pointer) return 1; else return 0;
}

/* Returns the first pointer type that has as points_to tp.
 *  Not efficient: O(#types).
 *  If not found returns unknown_type. */
type *find_pointer_type_to_type (type *tp) {
  int i;
  for (i = 0; i < get_irp_n_types(); ++i) {
    type *found = get_irp_type(i);
    if (is_pointer_type(found) && get_pointer_points_to_type(found) == tp)
      return (found);
  }
  return unknown_type;
}



/*******************************************************************/
/** TYPE_PRIMITIVE                                                **/
/*******************************************************************/

/* create a new type primitive */
INLINE type *new_type_primitive (ident *name, ir_mode *mode) {
  type *res;
  /* @@@ assert( mode_is_data(mode) && (!mode_is_reference(mode))); */
  res = new_type(type_primitive, mode, name);
  assert((get_mode_size_bytes(mode) != -1) && "unorthodox modes not implemented");
  res->size = get_mode_size_bytes(mode);
  res->state = layout_fixed;
  return res;
}
type *new_d_type_primitive (ident *name, ir_mode *mode, dbg_info* db) {
  type *res = new_type_primitive (name, mode);
  set_type_dbg_info(res, db);
  return res;
}
INLINE void free_primitive_attrs (type *primitive) {
  assert(primitive && (primitive->type_op == type_primitive));
}

/* typecheck */
bool  is_primitive_type  (type *primitive) {
  assert(primitive && primitive->kind == k_type);
  if (primitive->type_op == type_primitive) return 1; else return 0;
}

/*******************************************************************/
/** common functionality                                          **/
/*******************************************************************/


INLINE int is_atomic_type(type *tp) {
  assert(tp && tp->kind == k_type);
  return (is_primitive_type(tp) || is_pointer_type(tp) ||
	  is_enumeration_type(tp));
}

/*
 * Gets the number of elements in a firm compound type.
 */
int get_compound_n_members(type *tp)
{
  int res = 0;

  if (is_struct_type(tp))
    res = get_struct_n_members(tp);
  else if (is_class_type(tp))
    res = get_class_n_members(tp);
  else if (is_union_type(tp))
    res = get_union_n_members(tp);
  else
    assert(0 && "need struct, union or class for member count");

  return res;
}

/*
 * Gets the member of a firm compound type at position pos.
 */
entity *get_compound_member(type *tp, int pos)
{
  entity *res;

  if (is_struct_type(tp))
    res = get_struct_member(tp, pos);
  else if (is_class_type(tp))
    res = get_class_member(tp, pos);
  else if (is_union_type(tp))
    res = get_union_member(tp, pos);
  else
  {
    assert(0 && "need struct, union or class to get a member");
    res=NULL;
  }

  return res;
}


INLINE int is_compound_type(type *tp) {
  assert(tp && tp->kind == k_type);
  return (is_class_type(tp) || is_struct_type(tp) ||
	  is_array_type(tp) || is_union_type(tp));
}
