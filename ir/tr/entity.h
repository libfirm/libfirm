/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer,
**          Goetz Lindenmaier
**
** entity.h: declarations for entity
*/

# ifndef _ENTITY_H_
# define _ENTITY_H_

# include "ident.h"
# include "type.h"

/*******************************************************************/
/** general                                                       **/
/*******************************************************************/

/* initalize entity */
void init_entity (void);


/*******************************************************************/
/** ENTITY                                                        **/
/*******************************************************************/

typedef struct entity {
  firm_kind kind;
  ident *name;          /* name of this entity */
  type *owner;          /* The class this entity belongs to */
  ident *ld_name;       /* Unique name of this entity, i.e., the mangled
                           name.  E.g., for a class `A' with field `a' this
                           is the ident for `A_a'. */
  type *type;           /* The type of this entity, e.g., a method type, a
                           basic type of the language or a class itself */
  unsigned long visit;  /* visited counter for walks of the type information */
} entity;

/* create a new entity */
entity      *new_entity (type *owner, ident *name, type *type);

/* manipulate fields of entity */
/*
  char        *get_entity_name     (entity *);  */
ident       *get_entity_ident    (entity *);
/*
void         set_entity_ld_name  (entity *, char *ld_name);
void         set_entity_ld_ident (entity *, ident *ld_ident);
*/

// type_class *get_entity_owner (entity *);
type  *get_entity_owner (entity *);
// void  set_entity_owner (entity *, union type *);
void   set_entity_owner (entity *, type *);
inline void  assert_legal_owner_of_ent(type *);

ident *get_entity_ld_name  (entity *); /** resolve name conflict!! standard: ident **/
/*
char        *get_entity_ld_ident (entity *);
void         set_entity_ld_name  (entity *, char *ld_name);
void         set_entity_ld_ident (entity *, ident *ld_ident);
*/

type  *get_entity_type (entity *);
void   set_entity_type (entity *, type *);

# endif /* _ENTITY_H_ */
