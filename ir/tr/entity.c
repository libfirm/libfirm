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

  res = (entity *) malloc (sizeof (entity));
  res->kind = k_entity;
  assert_legal_owner_of_ent(owner);
  res->owner = owner;
  res->name = name;
  res->type = type;
  res->allocation = dynamic_allocated;
  res->visibility = local;
  res->ld_name = NULL;
  res->overwrites = NEW_ARR_F(entity *, 1);

  res->visit = 0;

  /* Remember entity in it's owner. */
  insert_entity_in_owner (res);
  return res;
}
inline void free_entity_attrs(entity *ent) {
  assert(ent);
  DEL_ARR_F(ent->overwrites);
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
  ARR_APP1 (entity *, ent->overwrites, overwritten);
}

inline int
get_entity_n_overwrites (entity *ent){
  assert(ent);
  return (ARR_LEN (ent->overwrites))-1;
}

inline entity *
get_entity_overwrites   (entity *ent, int pos){
  assert(ent);
  return ent->overwrites[pos+1];
}

inline void
set_entity_overwrites   (entity *ent, int pos, entity *overwritten) {
  assert(ent);
  ent->overwrites[pos+1] = overwritten;
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
  ent->irg = irg;
}
