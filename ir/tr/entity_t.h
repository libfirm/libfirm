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

# ifndef _ENTITY_T_H_
# define _ENTITY_T_H_

# include "entity.h"

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
  type *owner;          /* The class this entity belongs to.  In case of local
			   variables the method they are defined in. */
  ent_allocation allocation;  /* Distinguishes static and dynamically allocated
				 entities. */
  ent_visibility visibility;  /* Specifies visibility to external program
				 fragments */
  int  offset;          /* Offset in byte for this entity.  Fixed when layout
			   of owner is determined.  */
  /* for methods */
  ir_graph *irg;        /* If (type == method_type) this is the corresponding irg.
			   The ir_graph constructor automatically sets this field.
		 	   @@@ Does this go here, or should it be in type_method,
			   or should Call have an attribute ent?? */
  /* Do we need to remember the initializer of fields? */
  unsigned long visit;  /* visited counter for walks of the type information */
};


# endif /* _ENTITY_T_H_ */
