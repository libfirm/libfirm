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

/* $Id$ */

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
  entity **overwrites;  /* A list of entities this entity overwrites.  */
  ent_allocation allocation;  /* Distinguishes static and dynamically allocated
				 entities. */
  ent_visibility visibility;  /* Specifies visibility to external program
				 fragments */
  ent_variability variability;  /* Specifies variability of entities content */
  ent_volatility volatility;    /* Specifies volatility of entities content */
  ir_node *value;            /* value of atomic entity */
  ir_node **values;     /* values of compound entities */
  entity **val_ents;    /* entities corresponding to constant values */
  int  offset;          /* Offset in byte for this entity.  Fixed when layout
			   of owner is determined.  */
  void *link;           /* To store some intermediate information */
  /* for methods */
  ir_graph *irg;        /* If (type == method_type) this is the corresponding irg.
			   The ir_graph constructor automatically sets this field.
		 	   @@@ Does this go here, or should it be in type_method,
			   or should Call have an attribute ent??
			   Yes, it must be here. */
  unsigned long visit;  /* visited counter for walks of the type information */
};


# endif /* _ENTITY_T_H_ */
