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

/*CS*/
//#ifndef _TYPE_TYPEDEF_
//#define _TYPE_TYPEDEF_
/* to resolve recursion between entity.h and irgraph.h */
//typedef union type type;
//#endif

//typedef struct entity entity;

/* create a new entity */
entity   *new_entity (type *owner, ident *name, type *type);

/* manipulate fields of entity */
char     *get_entity_name     (entity *ent);
ident    *get_entity_ident    (entity *ent);

ident    *get_entity_ld_name  (entity *ent);

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

/* The entity knows the corresponding irg if the entity is a method.
   This allows to get from a Call to the called irg. */
ir_graph *get_entity_irg(entity *ent);
void      set_entity_irg(entity *ent, ir_graph *irg);


# endif /* _ENTITY_H_ */
