/**
 *
 * @file tp_inheritance.h
 *
 * Project:     libFIRM                                                   <br>
 * File name:   ir/tr/tp_inheritance.h                                    <br>
 * Purpose:     Utility routines for inheritance representation           <br>
 * Author:      Goetz Lindenmaier                                         <br>
 * Modified by:                                                           <br>
 * Created:                                                               <br>
 * Copyright:   (c) 2001-2005 Universität Karlsruhe                       <br>
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE. <br>
 * CVS-ID:      $Id$
 *
 * This file supplies a set of utility routines for the inheritance
 * representation.
 *
 * Inheritance is represented in Firm by two relations: sub/supertype
 * between class types, overwrites/ovwerwrittenby between entities.
 *
 * - Classify pairs of types/entities in the inheritance relations.
 * - Resolve implicit inheritance.
 * - Compute the transitive closure of the subclass/superclass and
 *   overwrites/overwrittenby relation.
 *
 *  @see  type.h entity.h
 */

#ifndef _TR_INHERITANCE_H_
#define _TR_INHERITANCE_H_

#include "type.h"
/* #include "entity.h" */
#include "ident.h"

/* ----------------------------------------------------------------------- */
/* Classify pairs of types/entities in the inheritance relations.          */
/* ----------------------------------------------------------------------- */

/** Returns true if low is subclass of high.
 *
 *  Low is a subclass of high if low == high or if low is a subclass of
 *  a subclass of high.  I.e, we search in all subtypes of high for low.
 *  @@@ this can be implemented more efficient if we know the set of all
 *  subclasses of high.  */
int is_subclass_of(type *low, type *high);

/** Returns true if high is (transitiv) overwritten by low.
 *
 *  Returns false if high == low. */
int is_overwritten_by(entity *high, entity *low);

/** Resolve polymorphy in the inheritance relation.
 *
 * Returns the dynamically referenced entity if the static entity and the
 * dynamic type are given.
 * Searches downwards in overwritten tree. */
entity *resolve_ent_polymorphy(type *dynamic_class, entity* static_ent);

/* ----------------------------------------------------------------------- */
/* Resolve implicit inheritance.                                           */
/* ----------------------------------------------------------------------- */

/** Default name mangling for inherited entities.
 *
 *  Returns an ident that consists of the name of type followed by an
 *  underscore and the name (not ld_name) of the entity. */
ident *default_mangle_inherited_name(entity *super, type *clss);

/** Type of argument functions for inheritance resolver.
 *
 * @param super   The entity in the super type that will be overwritten
 *                by the newly generated entity, for which this name is
 *                used.
 * @param clss    The class type in which the new entity will be placed.
 */
typedef ident *mangle_inherited_name_func(entity *super, type *clss);

/** Resolve implicit inheritance.
 *
 *  Resolves the implicit inheritance supplied by firm.  Firm defines,
 *  that each entity that is not overwritten in a subclass is
 *  inherited to this subclass without change implicitly.  This
 *  function generates entities that explicitly represent this
 *  inheritance.  It generates for each entity overwriting entities in
 *  all subclasses of the owner of the entity, if the entity is not
 *  overwritten in that sublclass.
 *
 *  The name of the new entity is generated with the function passed.
 *  If the function is NULL, the default_mangle_inherited_name() is
 *  used.
 *
 *  This function was moved here from firmlower 3/2005.
 */
void resolve_inheritance(mangle_inherited_name_func *mfunc);


/* ----------------------------------------------------------------------- */
/* The transitive closure of the subclass/superclass and                   */
/* overwrites/overwrittenby relation.                                      */
/*                                                                         */
/* A walk over the ir (O(#types+#entities)) computes the transitive        */
/* closure.  Adding a new type/entity or changing the basic relations in   */
/* some other way invalidates the transitive closure, i.e., it is not      */
/* updated by the basic functions.                                         */
/*                                                                         */
/* The transitive edges are held in a set, not in an array as the          */
/* underlying relation.                                                    */
/* ----------------------------------------------------------------------- */

/** The state of the transitive closure.
 *
 *  @TODO: we could manage the state for each relation separately.  Invalidating
 *  the entity relations does not mean invalidating the class relation. */
typedef enum {
  inh_transitive_closure_none,       /**<  Closure is not computed, can not be accessed. */
  inh_transitive_closure_valid,      /**<  Closure computed and valid. */
  inh_transitive_closure_invalid,    /**<  Closure invalid, but can be accessed. */
  inh_transitive_closure_max         /**<  Invalid value. */
} inh_transitive_closure_state;

void                        set_irp_inh_transitive_closure_state(inh_transitive_closure_state s);
void                        invalidate_irp_inh_transitive_closure_state(void);
inh_transitive_closure_state get_irp_inh_transitive_closure_state(void);


/** Compute transitive closure of the subclass/superclass and
* overwrites/overwrittenby relation.
*
* This function walks over the ir (O(#types+#entities)) to compute the
* transitive closure.    */
void compute_inh_transitive_closure(void);

/** Free memory occupied by the transitive closure information. */
void free_inh_transitive_closure(void);


/* - subtype ------------------------------------------------------------- */

/** Iterate over all transitive subtypes. */
type *get_class_trans_subtype_first(type *tp);
type *get_class_trans_subtype_next (type *tp);

/* - supertype ----------------------------------------------------------- */

/** Iterate over all transitive supertypes. */
type *get_class_trans_supertype_first(type *tp);
type *get_class_trans_supertype_next (type *tp);

/* - overwrittenby ------------------------------------------------------- */

/** Iterate over all entities that transitive overwrite this entities. */
entity *get_entity_trans_overwrittenby_first(entity *ent);
entity *get_entity_trans_overwrittenby_next (entity *ent);

/* - overwrites ---------------------------------------------------------- */

/** Iterate over all transitive overwritten entities. */
entity *get_entity_trans_overwrites_first(entity *ent);
entity *get_entity_trans_overwrites_next (entity *ent);

#endif  /* _TR_INHERITANCE_H_ */
