/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <stdlib.h>
# include <stddef.h>
# include "entity_t.h"
# include "mangle.h"
# include "typegmod_t.h"
# include "array.h"
/* All this is needed to build the constant node for methods: */
# include "irprog.h"
# include "ircons.h"

/*******************************************************************/
/** general                                                       **/
/*******************************************************************/

void
init_entity (void)
{
}

/*******************************************************************/
/** ENTITY                                                        **/
/*******************************************************************/

inline void insert_entity_in_owner (entity *ent) {
  type *owner = ent->owner;
  switch (get_type_tpop_code(owner)) {
  case tpo_class: {
    add_class_member (owner, ent);
  } break;
  case tpo_struct: {
    add_struct_member (owner, ent);
  } break;
  case tpo_union: {
    add_union_member (owner, ent);
  } break;
  case tpo_array: {
    set_array_element_entity(owner, ent);
  } break;
  default: assert(0);
  }
}

entity *
new_entity (type *owner, ident *name, type *type)
{
  entity *res;
  ir_graph *rem;

  res = (entity *) malloc (sizeof (entity));
  res->kind = k_entity;
  assert_legal_owner_of_ent(owner);
  res->owner = owner;
  res->name = name;
  res->type = type;
  if (get_type_tpop(type) == type_method)
    res->allocation = static_allocated;
  else
    res->allocation = automatic_allocated;
  res->visibility = local;
  res->offset = -1;
  if (is_method_type(type)) {
    res->variability = constant;
    rem = current_ir_graph;
    current_ir_graph = get_const_code_irg();
    res->value = new_Const(mode_p, tarval_p_from_entity(res));
    current_ir_graph = rem;
  } else {
    res->variability = uninitialized;
  }
  res->peculiarity = existent;
  res->volatility = non_volatile;
  res->ld_name = NULL;
  res->overwrites = NEW_ARR_F(entity *, 1);
  res->overwrittenby = NEW_ARR_F(entity *, 1);

  res->irg = NULL;

  res->visit = 0;

  /* Remember entity in it's owner. */
  insert_entity_in_owner (res);
  return res;
}
inline void free_entity_attrs(entity *ent) {
  assert(ent);
  DEL_ARR_F(ent->overwrites);
  DEL_ARR_F(ent->overwrittenby);
}

entity *
copy_entity_own (entity *old, type *new_owner) {
  entity *new;

  assert_legal_owner_of_ent(new_owner);
  if (old->owner == new_owner) return old;
  new = (entity *) malloc (sizeof (entity));
  memcpy (new, old, sizeof (entity));
  new->owner = new_owner;
  new->overwrites = DUP_ARR_F(entity *, old->overwrites);
  new->overwrittenby = DUP_ARR_F(entity *, old->overwrittenby);

  insert_entity_in_owner (new);

  return new;
}

entity *
copy_entity_name (entity *old, ident *new_name) {
  entity *new;

  if (old->name == new_name) return old;
  new = (entity *) malloc (sizeof (entity));
  memcpy (new, old, sizeof (entity));
  new->name = new_name;
  new->ld_name = NULL;
  new->overwrites = DUP_ARR_F(entity *, old->overwrites);
  new->overwrittenby = DUP_ARR_F(entity *, old->overwrittenby);

  insert_entity_in_owner (new);

  return new;
}

inline const char *
get_entity_name (entity *ent) {
  assert (ent);
  return id_to_str(get_entity_ident(ent));
}

ident *
get_entity_ident    (entity *ent) {
  assert(ent);
  return ent->name;
}

/*
void   set_entity_ld_name  (entity *, char *ld_name);
void   set_entity_ld_ident (entity *, ident *ld_ident);
*/

inline type *
get_entity_owner (entity *ent) {
  return ent->owner = skip_tid(ent->owner);
}

inline void
set_entity_owner (entity *ent, type *owner) {
  assert_legal_owner_of_ent(owner);
  ent->owner = owner;
}

inline void   /* should this go into type.c? */
assert_legal_owner_of_ent(type *owner) {
  assert (get_type_tpop_code(owner) == tpo_class ||
          get_type_tpop_code(owner) == tpo_union ||
          get_type_tpop_code(owner) == tpo_struct ||
	  get_type_tpop_code(owner) == tpo_array);   /* Yes, array has an entity
							-- to select fields! */
}

inline ident *
get_entity_ld_ident (entity *ent)
{
  if (ent->ld_name == NULL)
    ent->ld_name = mangle_entity (ent);
  return ent->ld_name;
}

inline void
set_entity_ld_ident (entity *ent, ident *ld_ident) {
  ent->ld_name = ld_ident;
}

inline const char *
get_entity_ld_name (entity *ent) {
  return id_to_str(get_entity_ld_ident(ent));
}

/*
char  *get_entity_ld_name  (entity *);
void   set_entity_ld_name  (entity *, char *ld_name);
*/

inline type *
get_entity_type (entity *ent) {
  return ent->type = skip_tid(ent->type);
}

inline void
set_entity_type (entity *ent, type *type) {
  ent->type = type;
}


inline ent_allocation
get_entity_allocation (entity *ent) {
  return ent->allocation;
}

inline void
set_entity_allocation (entity *ent, ent_allocation al) {
  ent->allocation = al;
}


inline ent_visibility
get_entity_visibility (entity *ent) {
  return ent->visibility;
}

inline void
set_entity_visibility (entity *ent, ent_visibility vis) {
  if (vis != local) assert(ent->allocation == static_allocated);
  ent->visibility = vis;
}

inline ent_variability
get_entity_variability (entity *ent) {
  return ent->variability;
}

inline void
set_entity_variability (entity *ent, ent_variability var){
  if (var == part_constant)
    assert(is_class_type(ent->type) || is_struct_type(ent->type));
  if ((is_compound_type(ent->type)) &&
      (ent->variability == uninitialized) && (var != uninitialized)) {
    /* Allocate datastructures for constant values */
    ent->values = NEW_ARR_F(ir_node *, 1);
    ent->val_ents = NEW_ARR_F(entity *, 1);
  }
  if ((is_compound_type(ent->type)) &&
      (var == uninitialized) && (ent->variability != uninitialized)) {
    /* Free datastructures for constant values */
    DEL_ARR_F(ent->values);
    DEL_ARR_F(ent->val_ents);
  }
  ent->variability = var;
}


inline ent_volatility
get_entity_volatility (entity *ent) {
  return ent->volatility;
}

inline void
set_entity_volatility (entity *ent, ent_volatility vol) {
  ent->volatility = vol;
}

inline peculiarity
get_entity_peculiarity (entity *ent) {
  assert (ent);
  //assert (is_method_type(ent->type));
  return ent->peculiarity;
}

inline void
set_entity_peculiarity (entity *ent, peculiarity pec) {
  assert (ent);
  assert (is_method_type(ent->type));
  ent->peculiarity = pec;
}

/* Set has no effect for entities of type method. */
inline ir_node *
get_atomic_ent_value(entity *ent) {
  assert(ent); assert(is_atomic_entity(ent));
  assert((ent->variability != uninitialized));
  return ent->value;
}

inline void
set_atomic_ent_value(entity *ent, ir_node *val) {
  assert(ent && is_atomic_entity(ent) && (ent->variability != uninitialized));
  if (is_method_type(ent->type)) return;
  ent->value = val;
}


ir_node *copy_const_value(ir_node *n) {
  ir_node *nn;
  ir_mode *m;

  m = get_irn_mode(n);
  switch(get_irn_opcode(n)) {
  case iro_Const:
    nn = new_Const(m, get_Const_tarval(n)); break;
  case iro_SymConst:
    nn = new_SymConst(get_SymConst_type_or_id(n), get_SymConst_kind(n)); break;
  case iro_Add:
    nn = new_Add(copy_const_value(get_Add_left(n)), copy_const_value(get_Add_right(n)), m); break;
  default:
    assert(0 && "opdope invalid or not implemented"); break;
  }
  return nn;
}

/* Copies the value represented by the entity to current_block
   in current_ir_graph. */
ir_node *copy_atomic_ent_value(entity *ent) {
  assert(ent && is_atomic_entity(ent) && (ent->variability != uninitialized));
  return copy_const_value(ent->value);
}

/* A value of a compound entity is a pair of value and the corresponding member of
   the compound. */
inline void
add_compound_ent_value(entity *ent, ir_node *val, entity *member) {
  assert(ent && is_compound_entity(ent) && (ent->variability != uninitialized));
  ARR_APP1 (ir_node *, ent->values, val);
  ARR_APP1 (entity *, ent->val_ents, member);
}

/* Copies the firm subgraph referenced by val to const_code_irg and adds
   the node as constant initialization to ent.
   The subgraph may not contain control flow operations. */
inline void
copy_and_add_compound_ent_value(entity *ent, ir_node *val, entity *member) {
  ir_graph *rem = current_ir_graph;

  assert(get_entity_variability(ent) != uninitialized);
  current_ir_graph = get_const_code_irg();

  val = copy_const_value(val);
  add_compound_ent_value(ent, val, member);
  current_ir_graph = rem;
}

inline int
get_compound_ent_n_values(entity *ent) {
  assert(ent && is_compound_entity(ent) && (ent->variability != uninitialized));
  return (ARR_LEN (ent->values))-1;
}

inline ir_node  *
get_compound_ent_value(entity *ent, int pos) {
  assert(ent && is_compound_entity(ent) && (ent->variability != uninitialized));
  return ent->values[pos+1];
}

/* Copies the value i of the entity to current_block in current_ir_graph. */
ir_node *copy_compound_ent_value(entity *ent, int pos) {
  assert(ent && is_compound_entity(ent) && (ent->variability != uninitialized));
  return copy_const_value(ent->values[pos+1]);
}

inline entity   *
get_compound_ent_value_member(entity *ent, int pos) {
  assert(ent && is_compound_entity(ent) && (ent->variability != uninitialized));
  return ent->val_ents[pos+1];
}

inline void
set_compound_ent_value(entity *ent, ir_node *val, entity *member, int pos) {
  assert(ent && is_compound_entity(ent) && (ent->variability != uninitialized));
  ent->values[pos+1] = val;
  ent->val_ents[pos+1] = member;
}

void
set_array_entity_values(entity *ent, tarval **values, int num_vals) {
  int i;
  ir_graph *rem = current_ir_graph;
  type *arrtp = get_entity_type(ent);
  ir_node *val;

  assert(is_array_type(arrtp));
  assert(get_array_n_dimensions(arrtp) == 1);
  /* One bound is sufficient, the nunmber of constant fields makes the
     size. */
  assert(get_array_lower_bound (arrtp, 0) || get_array_upper_bound (arrtp, 0));
  assert(get_entity_variability(ent) != uninitialized);
  current_ir_graph = get_const_code_irg();

  for (i = 0; i < num_vals; i++) {
    val = new_Const(get_tv_mode (values[i]), values[i]);
    add_compound_ent_value(ent, val, get_array_element_entity(arrtp));
  }
  current_ir_graph = rem;
}

inline int
get_entity_offset (entity *ent) {
  return ent->offset;
}

inline void
set_entity_offset (entity *ent, int offset) {
  ent->offset = offset;
}

inline void
add_entity_overwrites   (entity *ent, entity *overwritten) {
  assert(ent);
  assert(is_class_type(get_entity_owner(ent)));
  ARR_APP1 (entity *, ent->overwrites, overwritten);
  ARR_APP1 (entity *, overwritten->overwrittenby, ent);
}

inline int
get_entity_n_overwrites (entity *ent) {
  assert(ent);
  assert(is_class_type(get_entity_owner(ent)));
  return (ARR_LEN (ent->overwrites))-1;
}

inline entity *
get_entity_overwrites   (entity *ent, int pos) {
  assert(ent);
  assert(is_class_type(get_entity_owner(ent)));
  assert(pos < get_entity_n_overwrites(ent));
  return ent->overwrites[pos+1];
}

inline void
set_entity_overwrites   (entity *ent, int pos, entity *overwritten) {
  assert(ent);
  assert(is_class_type(get_entity_owner(ent)));
  assert(pos < get_entity_n_overwrites(ent));
  ent->overwrites[pos+1] = overwritten;
}

inline void
add_entity_overwrittenby   (entity *ent, entity *overwrites) {
  assert(ent);
  assert(is_class_type(get_entity_owner(ent)));
  add_entity_overwrites(overwrites, ent);
}

inline int
get_entity_n_overwrittenby (entity *ent) {
  assert(ent);
  assert(is_class_type(get_entity_owner(ent)));
  return (ARR_LEN (ent->overwrittenby))-1;
}

inline entity *
get_entity_overwrittenby   (entity *ent, int pos) {
  assert(ent);
  assert(is_class_type(get_entity_owner(ent)));
  assert(pos < get_entity_n_overwrittenby(ent));
  return ent->overwrittenby[pos+1];
}

inline void
set_entity_overwrittenby   (entity *ent, int pos, entity *overwrites) {
  assert(ent);
  assert(is_class_type(get_entity_owner(ent)));
  assert(pos < get_entity_n_overwrittenby(ent));
  ent->overwrittenby[pos+1] = overwrites;
}

/* A link to store intermediate information */
void *
get_entity_link(entity *ent) {
  assert(ent);
  return ent->link;
}

void
set_entity_link(entity *ent, void *l) {
  assert(ent);
  ent->link = l;
}

inline ir_graph *
get_entity_irg(entity *ent) {
  assert (ent);
  assert (is_method_type(ent->type));
  return ent->irg;
}

inline void
set_entity_irg(entity *ent, ir_graph *irg) {
  assert (ent && ent->type);
  assert (irg);
  assert (is_method_type(ent->type));
  assert (ent->peculiarity == existent);
  ent->irg = irg;
}

int is_atomic_entity(entity *ent) {
  type* t = get_entity_type(ent);
  return (is_primitive_type(t) || is_pointer_type(t) ||
	  is_enumeration_type(t) || is_method_type(t));
}

int is_compound_entity(entity *ent) {
  type* t = get_entity_type(ent);
  return (is_class_type(t) || is_struct_type(t) ||
	  is_array_type(t) || is_union_type(t));
}
