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

/* initalize entity module */
void init_entity (void);

/*******************************************************************/
/** ENTITY                                                        **/
/*******************************************************************/

#ifndef _IR_GRAPH_TYPEDEF_
#define _IR_GRAPH_TYPEDEF_
/* to resolve recursion between entity.h and irgraph.h */
typedef struct ir_graph ir_graph;
#endif

struct entity {
  firm_kind kind;
  ident *name;          /* name of this entity */
  ident *ld_name;       /* Unique name of this entity, i.e., the mangled
                           name.  E.g., for a class `A' with field `a' this
                           is the ident for `A_a'. */
  type *type;           /* The type of this entity, e.g., a method type, a
                           basic type of the language or a class itself */
  type *owner;          /* The class this entity belongs to */
  /* for methods */
  ir_graph *irg;        /* If (type == method_type) this is the corresponding irg.
			   The ir_graph constructor automatically sets this field.
		 	   @@@ Does this go here, or should it be in type_mehtod,
			   or should Call have an attribute ent?? */
  /* Do we need to remember the initializer of fields? */
  unsigned long visit;  /* visited counter for walks of the type information */
};

#ifndef _ENTITY_TYPEDEF_
#define _ENTITY_TYPEDEF_
/* to resolve recursion between entity.h and irgraph.h */
typedef struct entity entity;
#endif

/* create a new entity */
entity   *new_entity (type *owner, ident *name, type *type);

/* manipulate fields of entity */
char     *get_entity_name     (entity *);
ident    *get_entity_ident    (entity *);

ident    *get_entity_ld_name  (entity *);
/*
char     *get_entity_ld_name  (entity *);
ident    *get_entity_ld_ident (entity *);
void      set_entity_ld_name  (entity *, char *ld_name);
void      set_entity_ld_ident (entity *, ident *ld_ident);
*/

type     *get_entity_owner (entity *);
void      set_entity_owner (entity *, type *);
inline void  assert_legal_owner_of_ent(type *);

type     *get_entity_type (entity *);
void      set_entity_type (entity *, type *);

/* The entity knows the corresponding irg if the entity is a method.
   This allows to get from a Call to the called irg. *
ir_graph *get_entity_irg(entity *ent);
void      set_entity_irg(entity *ent, ir_graph *irg);
*/

# endif /* _ENTITY_H_ */
