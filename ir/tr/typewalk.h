/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*/

/**
* @file typewalk.h
*
* Traverse the type information.
*
* @author Goetz Lindenmaier
*
* The walker walks the whole ir graph
* to find the distinct type trees in the type graph forest.
* - execute the pre() function before recursion
* - execute the post() function after recursion
*/

/* $Id$ */


/* walk over all type information reachable from the ir graph. */

#ifndef _TYPEWALK_H_
#define _TYPEWALK_H_

# include "type_or_entity.h"

/**
 * the type walk function
 *
 * @param tore    points to the visited type or entity
 * @param env     free environment pointer
 */
typedef void type_walk_func(type_or_ent *tore, void *env);

/**
 * the class walk function
 *
 * @param clss    points to the visited class
 * @param env     free environment pointer
 */
typedef void class_walk_func(type *clss, void *env);

/**
    Touches every type and entity in unspecified order.  If new
    types/entities are created during the traversal these will
    be visited, too. */
void type_walk(type_walk_func *pre,
	       type_walk_func *post,
	       void *env);

/** Walks over all type information reachable from an ir graph.
 *
 *  Walks over all type information reachable from irg, i.e., starts a
 *  type walk at the irgs entity, the irgs frame type and all types and
 *  entities that are attributes to firm nodes. */
void type_walk_irg(ir_graph *irg,
		   type_walk_func *pre,
		   type_walk_func *post,
		   void *env);

/**
    Touches every class in specified order:
    - first the super class
    - second the class itself
    - third the sub classes.  If new classes are created
    during the traversal these will be visited, too.

    @todo should be named class-walk

    @deprecated will be removed?
*/
void type_walk_super2sub(type_walk_func *pre,
			 type_walk_func *post,
			 void *env);

/**
    Touches every class in specified order:
    - first the super class
    - second the class itself
    If new classes are created during the traversal these
    will be visited, too. */
void type_walk_super(type_walk_func *pre,
		     type_walk_func *post,
		     void *env);

/** Same as type_walk_super2sub, but visits only class types.
   Executes pre for a class if all superclasses have been visited.
   Then iterates to subclasses.  Executes post after return from
   subclass.
   Does not visit global type, frame types.

   @bug ?? something is wrong with this.
*/
void class_walk_super2sub(class_walk_func *pre,
			  class_walk_func *post,
			  void *env);

/**
 * the entity walk function
 *
 * @param ent     points to the visited entity
 * @param env     free environment pointer
 */
typedef void entity_walk_func(entity *ent, void *env);

/**
 * Walks over all entities in the type.
 *
 * @param tp    the type
 * @param doit  the entity walker function
 * @param env   environment, wil be passed to the walker function
 */
void walk_types_entities(type *tp,
			 entity_walk_func *doit,
			 void *env);
#endif /* _TYPEWALK_H_ */
