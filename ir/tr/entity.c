/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <stdlib.h>
# include <stddef.h>
# include "entity_t.h"
# include "entity.h"
# include "mangle.h"

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
  res->ld_name = NULL;

  res->visit = 0;

  switch (get_type_tpop_code(owner)) {
  case tpo_class: {
    add_class_member (owner, res);
  } break;
  case tpo_struct: {
    add_struct_member (owner, res);
  } break;
  case tpo_union: {
    add_union_member (owner, res);
  } break;
  default: assert(0);
  }

  return res;
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
  return ent->owner;
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
          get_type_tpop_code(owner) == tpo_struct);
}

inline ident *
get_entity_ld_name (entity *ent)
{
  if (ent->ld_name != NULL) return ent->ld_name;
  return mangle_entity (ent);
}

/*
char  *get_entity_ld_name  (entity *);
void   set_entity_ld_name  (entity *, char *ld_name);
void   set_entity_ld_ident (entity *, ident *ld_ident);
*/

inline type *
get_entity_type (entity *ent) {
  return ent->type;
}

inline void
set_entity_type (entity *ent, type *type) {
  ent->type = type;
}

inline int
get_entity_offset (entity *ent) {
  return ent->offset;
}

inline void
set_entity_offset (entity *ent, int offset) {
  ent->offset = offset;
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
