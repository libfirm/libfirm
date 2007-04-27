/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file tr_inheritance.h
 *
 * Project:     libFIRM                                                   <br>
 * File name:   ir/tr/tp_inheritance.h                                    <br>
 * Purpose:     Utility routines for inheritance representation           <br>
 * Author:      Goetz Lindenmaier                                         <br>
 * Modified by:                                                           <br>
 * Created:                                                               <br>
 * Copyright:   (c) 2001-2005 Universität Karlsruhe                       <br>
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
#ifndef _FIRM_TR_INHERITANCE_H_
#define _FIRM_TR_INHERITANCE_H_

#include "firm_types.h"
#include "type.h"
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
int is_SubClass_of(ir_type *low, ir_type *high);

/** Subclass check for pointers to classes.
 *
 *  Dereferences at both types the same amount of pointer types (as
 *  many as possible).  If the remaining types are both class types
 *  and subclasses, returns true, else false.  Can also be called with
 *  two class types.  */
int is_SubClass_ptr_of(ir_type *low, ir_type *high);

/** Returns true if high is superclass of low.
 *
 *  Low is a subclass of high if low == high or if low is a subclass of
 *  a subclass of high.  I.e, we search in all subtypes of high for low.
 *  @@@ this can be implemented more efficient if we know the set of all
 *  subclasses of high.  */
#define is_SuperClass_of(high, low) is_SubClass_of(low, high)

/** Superclass check for pointers to classes.
 *
 *  Dereferences at both types the same amount of pointer types (as
 *  many as possible).  If the remaining types are both class types
 *  and superclasses, returns true, else false.  Can also be called with
 *  two class types.  */
#define is_SuperClass_ptr_of(low, high) is_SubClass_ptr_of(high, low)

/** Returns true if high is (transitive) overwritten by low.
 *
 *  Returns false if high == low. */
int is_overwritten_by(ir_entity *high, ir_entity *low);

/** Resolve polymorphism in the inheritance relation.
 *
 *  Returns the dynamically referenced entity if the static entity and the
 *  dynamic type are given.
 *  Searches downwards in overwritten tree. */
ir_entity *resolve_ent_polymorphy(ir_type *dynamic_class, ir_entity* static_ent);

/* ----------------------------------------------------------------------- */
/* Resolve implicit inheritance.                                           */
/* ----------------------------------------------------------------------- */

/** Default name mangling for inherited entities.
 *
 *  Returns an ident that consists of the name of type followed by an
 *  underscore and the name (not ld_name) of the entity. */
ident *default_mangle_inherited_name(ir_entity *ent, ir_type *clss);

/** Type of argument functions for inheritance resolver.
 *
 * @param ent     The entity in the super type that will be overwritten
 *                by the newly generated entity, for which this name is
 *                used.
 * @param clss    The class type in which the new entity will be placed.
 */
typedef ident *mangle_inherited_name_func(ir_entity *ent, ir_type *clss);

/** Resolve implicit inheritance.
 *
 *  Resolves the implicit inheritance supplied by firm.  Firm defines,
 *  that each entity that is not overwritten in a subclass is
 *  inherited to this subclass without change implicitly.  This
 *  function generates entities that explicitly represent this
 *  inheritance.  It generates for each entity overwriting entities in
 *  all subclasses of the owner of the entity, if the entity is not
 *  overwritten in that subclass.
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
/*                                                                         */
/* Do the sets contain the node itself?  I assume NOT!                     */
/* ----------------------------------------------------------------------- */

/** The state of the transitive closure.
 *
 *  @todo: we could manage the state for each relation separately.  Invalidating
 *  the entity relations does not mean invalidating the class relation. */
typedef enum {
  inh_transitive_closure_none,       /**<  Closure is not computed, can not be accessed. */
  inh_transitive_closure_valid,      /**<  Closure computed and valid. */
  inh_transitive_closure_invalid,    /**<  Closure invalid, but can be accessed. */
  inh_transitive_closure_max         /**<  Invalid value. */
} inh_transitive_closure_state;

void                         set_irp_inh_transitive_closure_state(inh_transitive_closure_state s);
void                         invalidate_irp_inh_transitive_closure_state(void);
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
ir_type *get_class_trans_subtype_first(ir_type *tp);
ir_type *get_class_trans_subtype_next (ir_type *tp);
int   is_class_trans_subtype (ir_type *tp, ir_type *subtp);

/* - supertype ----------------------------------------------------------- */

/** Iterate over all transitive supertypes. */
ir_type *get_class_trans_supertype_first(ir_type *tp);
ir_type *get_class_trans_supertype_next (ir_type *tp);

/* - overwrittenby ------------------------------------------------------- */

/** Iterate over all entities that transitive overwrite this entities. */
ir_entity *get_entity_trans_overwrittenby_first(ir_entity *ent);
ir_entity *get_entity_trans_overwrittenby_next (ir_entity *ent);

/* - overwrites ---------------------------------------------------------- */

/** Iterate over all transitive overwritten entities. */
ir_entity *get_entity_trans_overwrites_first(ir_entity *ent);
ir_entity *get_entity_trans_overwrites_next (ir_entity *ent);


/* ----------------------------------------------------------------------- */
/** The state of Cast operations that cast class types or pointers to class
 *  types.
 *
 * The state expresses, how far Cast operations conform with the class
 * hierarchy.
 *
 *   class A {}
 *   class B1 extends A {}
 *   class B2 extends A {}
 *   class C  extends B1 {}
 * normalized:  Cast operations conform with the inheritance relation.
 *   I.e., the type of the operand of a Cast is either a super= or a sub-
 *   type of the type casted to. Example: (A)((B2) (new C())).
 * transitive:  Cast operations conform with the transitive inheritance
 *   relation. Example: (A)(new C()).
 * any:  Cast operations do not conform with the transitive inheritance
 *   relation.  Example: (B2)(new B1())
 *
 *  @see: tropt.h
 */
/* ----------------------------------------------------------------------- */

/** Flags for class cast state.
 *
 * The state in irp is always smaller or equal to the state of any
 * irg.
 *
 * We rely on the ordering of the enum. */
typedef enum {
  ir_class_casts_any        = 0, /**< There are class casts that do not cast in conformance with
				      the class hierarchy.  @@@ So far this does not happen in Firm. */
  ir_class_casts_transitive = 1, /**< Class casts conform to transitive inheritance edges. Default. */
  ir_class_casts_normalized = 2, /**< Class casts conform to inheritance edges. */
  ir_class_casts_state_max
} ir_class_cast_state;
char *get_class_cast_state_string(ir_class_cast_state s);

void                set_irg_class_cast_state(ir_graph *irg, ir_class_cast_state s);
ir_class_cast_state get_irg_class_cast_state(ir_graph *irg);
void                set_irp_class_cast_state(ir_class_cast_state s);
ir_class_cast_state get_irp_class_cast_state(void);

/** Verify the class cast state of an irg.
 *
 *  Asserts if state is to high, outputs warning if state is to low
 *  and firm verbosity is set.
 */
void verify_irg_class_cast_state(ir_graph *irg);
#endif  /* _FIRM_TR_INHERITANCE_H_ */
