/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe

* All rights reserved.
*
* Author: Goetz Lindenmaier
*
* traverse the type information.  The walker walks the whole ir graph
* to find the distinct type trees in the type graph forest.
* - execute the pre function before recursion
* - execute the post function after recursion
*/

/* $Id$ */


/* walk over all type information reachable from the ir graph. */

#ifndef _TYPEWALK_H_
#define _TYPEWALK_H_

# include "type_or_entity.h"


typedef void (type_walk_func)(type_or_ent *, void *);
typedef void (class_walk_func)(type *, void *);

/**
    Touches every type and entity in unspecified order.  If new
    types/entities are created during the traversal these will
    be visited, too. **/
void type_walk(type_walk_func *pre,
	       type_walk_func *post,
	       void *env);

/** walks over all type information reachable from irg **/
void type_walk_irg(ir_graph *irg,
		   type_walk_func *pre,
		   type_walk_func *post,
		   void *env);

/**
    Touches every class in specified order:
    - first the super class
    - second the class itself
    - third the sub classes.  If new classes are created
    during the traversal these will be visited, too. **/
/** @@@ should be named class-walk **/
/*  @@@ will be removed? */
void type_walk_super2sub(type_walk_func *pre,
			 type_walk_func *post,
			 void *env);

/**
    Touches every class in specified order:
    - first the super class
    - second the class itself
    If new classes are created during the traversal these
    will be visited, too. **/
void type_walk_super(type_walk_func *pre,
		     type_walk_func *post,
		     void *env);

/* Same as type_walk_super2sub, but visits only class types.
   Executes pre for a class if all superclasses have been visited.
   Then iterates to subclasses.  Executes post after return from
   subclass.
   Does not visit global type, frame types.
*/
/* @@@ ?? something is wrong with this. */
void class_walk_super2sub(class_walk_func *pre,
			  class_walk_func *post,
			  void *env);


/* Walks over all entities in the type */
typedef void (entity_walk_func)(entity *, void *);
void walk_types_entities(type *tp,
			 entity_walk_func *doit,
			 void *env);
#endif /* _TYPEWALK_H_ */
