/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
*/

# include <stdlib.h>
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
// new_entity (type_class *owner, ident *name, type *type)
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

  /* add entity to the list of entities of the owner. */
  // res->owner->member[res->owner->n_members] = res;
  // res->owner->n_members ++;

  return res;
}

/*
  char  *get_entity_name     (entity *);  */

ident *
get_entity_ident    (entity *ent) {
  assert(ent);
  return ent->name;
}
/*
void   set_entity_ld_name  (entity *, char *ld_name);
void   set_entity_ld_ident (entity *, ident *ld_ident);
*/

//inline type_class *
inline type *
get_entity_owner (entity *entity) {
  return entity->owner;
}

inline void
// set_entity_owner (entity *entity, type_class *owner) {
set_entity_owner (entity *entity, type *owner) {
  assert_legal_owner_of_ent(owner);
  entity->owner = owner;
}

inline void   /* should this go into type.c? */
assert_legal_owner_of_ent(type *type) {
  assert (type->clss.kind   == k_type_class ||
          type->uni.kind    == k_type_union ||
          type->array.kind  == k_type_array ||
          type->method.kind == k_type_method );
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
get_entity_type (entity *entity) {
  return entity->type;
}

inline void
set_entity_type (entity *entity, type *type) {
  entity->type = type;
}
