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


#endif /* _TYPEWALK_H_ */
