/*
 * Project:     libFIRM
 * File name:   ir/ana/trouts.h
 * Purpose:     Reverse edges that reference types/entities.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     29.10.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
* @file trouts.h
*
* Trouts list all uses of types and entities.
* Each type gets a list of all Alloc nodes allocating it.
* Each entity gets two lists:
*   - one containing all accesses (Load, (Call), Store),
*   - and one containing all uses to get a reference (Sel, SymConst).
*
* @todo
*   To list all uses of entities of a type, we also should list all
*   static/automatic allocated entities in types.  The Alloc nodes
*   represent only the dynamic allocated entities.
*
* @author Goetz Lindenmaier
*
*/

# ifndef _TROUTS_H_
# define _TROUTS_H_

#include "type.h"
#include "entity.h"
#include "irnode.h"


/*------------------------------------------------------------------*/
/* Accessing the trout datastructures.                              */
/* These routines only work properly if firm is in state            */
/* trouts_consistent or trouts_inconsistent.                        */
/*------------------------------------------------------------------*/


/** Number of Load/Store nodes that possibly access this entity. */
int get_entity_n_accesses(entity *ent);
/** Load/Store node that possibly access this entity. */
ir_node *get_entity_access(entity *ent, int pos);


/** Number of references to an entity, in form of SymConst/Sel.
 *  Including references from constant entities and the like. */
int get_entity_n_references(entity *ent);
/** References to an entity, in form of SymConst/Sel
 *  Including references from constants. */
ir_node *get_entity_reference(entity *ent, int pos);


/** Number of Alloc nodes that create an instance of this type */
int get_type_n_allocations(type *tp);
/** Alloc node that create an instance of this type */
ir_node *get_type_allocation(type *tp, int pos);



/*------------------------------------------------------------------*/
/* Building and Removing the trout datastructure                    */
/*------------------------------------------------------------------*/


/** Compute the outs of types and entities.
 *
 *  Collects all reference from irnodes to types or entities in the
 *  corresponding types/entities.
 *
 *  @todo @@@ We need a flag that signs the consistency of the out information. */
void compute_trouts(void);

/** Free trout data. */
void free_trouts(void);


#endif /* _TROUTS_H_ */
