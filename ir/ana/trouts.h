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


/** Number of Alloc nodes that create an instance of this type. */
int get_type_n_allocs(type *tp);
/** Alloc node that create an instance of this type. */
ir_node *get_type_alloc(type *tp, int pos);

/** Number of Cast nodes that cast a pointer to this type. */
int get_type_n_casts(type *tp);
/** Cast node that cast a pointer to this type. */
ir_node *get_type_cast(type *tp, int pos);
/** Return number of upcasts. O(#casts). */
int get_class_n_upcasts(type *clss);
/** Return number of downcasts. O(#casts). */
int get_class_n_downcasts(type *clss);

/* Access all pointer types that point to tp. */
int get_type_n_pointertypes_to(type *tp);
type *get_type_pointertype_to(type *tp, int pos);


/*------------------------------------------------------------------*/
/* Building and Removing the trout datastructure                    */
/*------------------------------------------------------------------*/

/** The state of the tr_out datastructure.
 *
 *  We reuse the enum of irouts. */
irg_outs_state get_trouts_state(void);
void           set_trouts_inconsistent(void);

/** Compute the outs of types and entities.
 *
 *  Collects all reference from irnodes to types or entities in the
 *  corresponding types/entities.  Further reverses references between
 *  types and entities.
 *
 *  Annotates the following nodes:
 *    Alloc    --> get_Alloc_type()
 *    Cast     --> get_Cast_type()
 *    Sel      --> get_Sel_entity()
 *    SymConst --> get_SymConst_entity()
 *    Load(addr)  --> get_addr_entity() \  ent von SymConst, oder falls Sel: ent von
 *    Store(addr) --> get_addr_entity() /  outermost im compound.  Ansonsten: nirgends.
 *                                         d.h. wir bekommen die array Elementzugriffe
 *                                         an die jack array Klasse annotiert.
 *    Call(Sel)   --> get_Sel_entity()  // ev. Tabellenzugriff --> Load.
 *
 *   type --> pointer type refering to this type.
 *   type --> entity of this type. @@@ to be implemented.
 *
 *  Sets trout state to outs_consistent.
 *
 *  @todo @@@ We need a flag that signs the consistency of the out information. */
void compute_trouts(void);

/** Free trout data. */
void free_trouts(void);


#endif /* _TROUTS_H_ */
