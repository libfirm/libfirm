/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe

** All rights reserved.
**
** Author: Goetz Lindenmaier
**
** traverse the type information.  The walker walks the whole ir graph
** to find the distinct type trees in the type graph forest.
** - execute the pre function before recursion
** - execute the post function after recursion
*/

/* $Id$ */


/* walk over all type information reachable from the ir graph. */

#ifndef _TYPEWALK_H_
#define _TYPEWALK_H_

# include "type_or_entity.h"


/** Walks over all type information reachable from global roots.
    Touches every type and entity in unspecified order.  If new
    types/entities are created during the traversal these will
    be visited, too. **/
void type_walk(void (pre)(type_or_ent*, void*),
	       void (post)(type_or_ent*, void*),
	       void *env);

/** walks over all type information reachable from irg **/
void type_walk_irg(ir_graph *irg,
		   void (pre)(type_or_ent*, void*),
		   void (post)(type_or_ent*, void*),
		   void *env);

/** Walks over all type information reachable from global roots.
    Touches every class in specified order:
    - first the super class
    - second the class itself
    - third the sub classes.  If new classes are created
    during the traversal these will be visited, too. **/
/** @@@ should be named class-walk **/
/*  @@@ will be removed? */
void type_walk_super2sub(void (pre)(type_or_ent*, void*),
			 void (post)(type_or_ent*, void*),
			 void *env);

/** Walks over all type information reachable from global roots.
    Touches every class in specified order:
    - first the super class
    - second the class itself
    If new classes are created during the traversal these
    will be visited, too. **/
void type_walk_super(void (pre)(type_or_ent*, void*),
		     void (post)(type_or_ent*, void*),
		     void *env);

/* Same as type_walk_super2sub, but visits only class types.
   Executes pre for a class if all superclasses have been visited.
   Then iterates to subclasses.  Executes post after return from
   subclass.
   Does not visit global type, frame types.
*/
/* @@@ ?? something is wrong with this. */
void class_walk_super2sub(void (pre)(type*, void*),
			  void (post)(type*, void*),
			  void *env);
#endif /* _TYPEWALK_H_ */
