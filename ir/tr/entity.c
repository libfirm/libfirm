/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
*/

# include <stdlib.h>
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

  switch (get_kind(owner)) {
  case k_type_class: {
    add_class_member ((type_class *) owner, res);
  } break;
  case k_type_strct: {
    add_strct_member ((type_strct *) owner, res);
  } break;
  case k_type_union: {
    /* not implemented */
  } break;
  case k_type_method: {
    /* not implemented */
  } break;
  default: ;
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
  assert (owner->clss.kind   == k_type_class ||
          owner->uni.kind    == k_type_union ||
          owner->array.kind  == k_type_array ||
          owner->method.kind == k_type_method );
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

inline ir_graph *
get_entity_irg(entity *ent) {
  assert (ent);
  assert (get_kind(ent->type) == k_type_method);
  return ent->irg;
}

inline void
set_entity_irg(entity *ent, ir_graph *irg) {
  assert (ent && ent->type);
  assert (irg);
  assert (get_kind(ent->type) == k_type_method);
  ent->irg = irg;
}
