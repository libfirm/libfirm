/*
**  Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
**  All rights reserved.
**
**  Authors: Martin Trapp, Christian Schaefer,
**           Goetz Lindenmaier
**
**  entity.h:  entities represent all program known objects.
**
**  An entity is the representation of program known objects in Firm.
**  The primary concept of entities is to represent members of complex
**  types, i.e., fields and methods of classes.  As not all programming
**  language model all variables and methods as members of some class,
**  the concept of entities is extended to cover also local and global
**  variables, and arbitrary procedures.
**
**  An entity always specifies the type of the object it represents and
**  the type of the object it is a part of, the owner of the entity.
**  Originally this is the type of the class of which the entity is a
**  member.
**  The owner of local variables is the procedure they are defined in.
**  The owner of global variables and procedures visible in the whole
**  program is a universally defined class type "GlobalType".  The owner
**  of procedures defined in the scope of an other procedure is the
**  enclosing procedure.
**
**  In detail the datastructure entity has the following fields:
**
**  ident *name     Name of this entity as specified in the source code.
**                  Only unequivocal in conjuction with scope.
**  ident *ld_name  Unique name of this entity, i.e., the mangled
**                  name.  E.g., for a class `A' with field `a' this
**                  is the ident for `A_a'.
**  type *type      The type of this entity, e.g., a method type, a
**                  basic type of the language or a class itself.
**  type *owner;    The class this entity belongs to.  In case of local
**		    variables the method they are defined in.
**  int offset;     Offset in byte for this entity.  Fixed when layout
**		    of owner is determined.
**  ir_graph *irg;  If (type == method_type) this is the corresponding irg.
**		    The ir_graph constructor automatically sets this field.
**                  If (type !- method_type) access of this field will cause
**                  an assertion.
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

#ifndef _ENTITY_TYPEDEF_
#define _ENTITY_TYPEDEF_
/* to resolve recursion between entity.h and type.h */
typedef struct entity entity;
#endif

/* Creates a new entity.
   Automatically inserts the entity as a member of owner. */
entity     *new_entity (type *owner, ident *name, type *type);

/* manipulate fields of entity */
const char *get_entity_name     (entity *ent);
ident      *get_entity_ident    (entity *ent);
/* returns the mangled name of the entity */
ident      *get_entity_ld_name  (entity *ent);

/*
char     *get_entity_ld_name  (entity *ent);
void      set_entity_ld_name  (entity *ent, char *ld_name);

ident    *get_entity_ld_ident (entity *ent);
void      set_entity_ld_ident (entity *ent, ident *ld_ident);
*/

type     *get_entity_owner (entity *ent);
void      set_entity_owner (entity *ent, type *owner);
inline void  assert_legal_owner_of_ent(type *owner);

type     *get_entity_type (entity *ent);
void      set_entity_type (entity *ent, type *type);

typedef enum {
  dynamic_allocated,  /* The entity is allocated during runtime, either explicitly
			 by an Alloc node or implicitly as component of a compound
			 type.  This is the default. */
  static_allocated    /* The entity is allocated statically.  We can use a
			  SymConst as address of the entity. */
} ent_allocation;

ent_allocation get_entity_allocation (entity *ent);
void           set_entity_allocation (entity *ent, ent_allocation al);

/* This enumeration flags the visibility of entities.  This is necessary
   for partial compilation. */
typedef enum {
  local,              /* The entity is only visible locally.  This is the default. */
  external_visible,   /* The entity is visible to other external program parts, but
			 it is defined here.  It may not be optimized away.  The entity must
		         be static_allocated. */
  external_allocated  /* The entity is defined and allocated externaly.  This compilation
			 must not allocate memory for this entity. The entity must
		         be static_allocated. */
} ent_visibility;

ent_visibility get_entity_visibility (entity *ent);
void           set_entity_visibility (entity *ent, ent_visibility vis);

int       get_entity_offset (entity *ent);
void      set_entity_offset (entity *ent, int offset);

/* The entity knows the corresponding irg if the entity is a method.
   This allows to get from a Call to the called irg. */
ir_graph *get_entity_irg(entity *ent);
void      set_entity_irg(entity *ent, ir_graph *irg);


# endif /* _ENTITY_H_ */
