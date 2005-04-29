/**
 *
 * @file tp_inheritance.c
 *
 * Project:     libFIRM                                                   <br>
 * File name:   ir/tr/tp_inheritance.c                                    <br>
 * Purpose:     Utility routines for inheritance representation           <br>
 * Author:      Goetz Lindenmaier                                         <br>
 * Modified by:                                                           <br>
 * Created:                                                               <br>
 * Copyright:   (c) 2001-2005 Universität Karlsruhe                       <br>
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE. <br>
 * CVS-ID:      $Id$
 *
 *
 *
 *  @see  type.h entity.h
 */

#include "type.h"
#include "entity.h"
#include "typewalk.h"
#include "irgraph_t.h"
#include "irprog_t.h"
#include "pset.h"
#include "set.h"
#include "mangle.h"
#include "irgwalk.h"
#include "irflag.h"
//#include ".h"


/* ----------------------------------------------------------------------- */
/* Resolve implicit inheritance.                                           */
/* ----------------------------------------------------------------------- */

ident *default_mangle_inherited_name(entity *super, type *clss) {
  return mangle_u(new_id_from_str("inh"), mangle_u(get_type_ident(clss), get_entity_ident(super)));
}

/** Replicates all entities in all super classes that are not overwritten
   by an entity of this class. */
static void copy_entities_from_superclass(type *clss, void *env)
{
  int i, j, k, l;
  int overwritten;
  type *super, *inhenttype;
  entity *inhent, *thisent;
  mangle_inherited_name_func *mfunc = (mangle_inherited_name_func *)env;

  for(i = 0; i < get_class_n_supertypes(clss); i++) {
    super = get_class_supertype(clss, i);
    assert(is_Class_type(super) && "not a class");
    for(j = 0; j < get_class_n_members(super); j++) {
      inhent = get_class_member(super, j);
      inhenttype = get_entity_type(inhent);
      /* check whether inhent is already overwritten */
      overwritten = 0;
      for (k = 0; (k < get_class_n_members(clss)) && (overwritten == 0); k++) {
	      thisent = get_class_member(clss, k);
	      for(l = 0; l < get_entity_n_overwrites(thisent); l++) {
	        if(inhent == get_entity_overwrites(thisent, l)) {
	          /* overwritten - do not copy */
	          overwritten = 1;
	          break;
	        }
	      }
      }
      /* Inherit entity */
      if (!overwritten) {
	      thisent = copy_entity_own(inhent, clss);
	      add_entity_overwrites(thisent, inhent);
	      set_entity_peculiarity(thisent, peculiarity_inherited);
	      set_entity_ld_ident(thisent, mfunc(inhent, clss));
	      if (get_entity_variability(inhent) == variability_constant) {
	        assert(is_atomic_entity(inhent) &&  /* @@@ */
		       "Inheritance of constant, compound entities not implemented");
	        set_entity_variability(thisent, variability_constant);
	        set_atomic_ent_value(thisent, get_atomic_ent_value(inhent));
	      }
      }
    }
  }
}

/* Resolve implicit inheritance.
 *
 *  Resolves the implicit inheritance supplied by firm.
 */
void resolve_inheritance(mangle_inherited_name_func *mfunc) {
  if (!mfunc)
    mfunc = default_mangle_inherited_name;
  class_walk_super2sub(copy_entities_from_superclass, NULL, (void *)mfunc);
}


/* ----------------------------------------------------------------------- */
/* The transitive closure of the subclass/superclass and                   */
/* overwrites/overwrittenby relation.                                      */
/*                                                                         */
/* A walk over the ir (O(#types+#entities)) computes the transitive        */
/* closure.  Adding a new type/entity or changing the basic relations in   */
/* some other way invalidates the transitive closure, i.e., it is not      */
/* updated by the basic functions.                                         */
/*                                                                         */
/* All functions are named as their counterparts for the basic relations,  */
/* adding the infix 'trans_'.                                              */
/* ----------------------------------------------------------------------- */

void                        set_irp_inh_transitive_closure_state(inh_transitive_closure_state s) {
  irp->inh_trans_closure_state = s;
}
void                        invalidate_irp_inh_transitive_closure_state(void) {
  if (irp->inh_trans_closure_state == inh_transitive_closure_valid)
    irp->inh_trans_closure_state = inh_transitive_closure_invalid;
}
inh_transitive_closure_state get_irp_inh_transitive_closure_state(void) {
  return irp->inh_trans_closure_state;
}

static void assert_valid_state(void) {
  assert(irp->inh_trans_closure_state == inh_transitive_closure_valid ||
	 irp->inh_trans_closure_state == inh_transitive_closure_invalid);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* There is a set that extends each entity/type with two new               */
/* fields:  one for the upwards directed relation: 'up' (supertype,        */
/* overwrites) and one for the downwards directed relation: 'down' (sub-   */
/* type, overwrittenby.  These fields contain psets (and maybe later       */
/* arrays) listing all subtypes...                                         */
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct {
  firm_kind *kind;   /* An entity or type. */
  pset *up;
  pset *down;
} tr_inh_trans_tp;

/* We use this set for all types and entities.  */
static set *tr_inh_trans_set = NULL;

static int tr_inh_trans_cmp(const void *e1, const void *e2, size_t size) {
  tr_inh_trans_tp *ef1 = (tr_inh_trans_tp *)e1;
  tr_inh_trans_tp *ef2 = (tr_inh_trans_tp *)e2;
  return (ef1->kind != ef2->kind);
}

static INLINE unsigned int tr_inh_trans_hash(void *e) {
  void *v = (void *) ((tr_inh_trans_tp *)e)->kind;
  return HASH_PTR(v);
}

typedef enum {
  d_up,
  d_down,
} dir;

/* This always completes successfully. */
static tr_inh_trans_tp* get_firm_kind_entry(firm_kind *k) {
  tr_inh_trans_tp a, *found;
  a.kind = k;

  if (!tr_inh_trans_set) tr_inh_trans_set = new_set(tr_inh_trans_cmp, 128);

  found = set_find(tr_inh_trans_set, &a, sizeof(a), tr_inh_trans_hash(&a));
  if (!found) {
    a.up   = pset_new_ptr(16);
    a.down = pset_new_ptr(16);
    found = set_insert(tr_inh_trans_set, &a, sizeof(a), tr_inh_trans_hash(&a));
  }
  return found;
}

static pset *get_entity_map(entity *ent, dir d) {
  tr_inh_trans_tp *found;

  assert(is_entity(ent));
  found = get_firm_kind_entry((firm_kind *)ent);
  return (d == d_up) ? found->up : found->down;
}
/*
static void  add_entity_map(entity *ent, dir d, entity *new) {
  tr_inh_trans_tp *found;

  assert(is_entity(ent) && is_entity(new));
  tr_inh_trans_tp *found = get_firm_kind_entry((firm_kind *)ent);
  if (d == d_up)
    pset_insert_ptr(found->up,   new);
  else
    pset_insert_ptr(found->down, new);
}
*/
static pset *get_type_map(type *tp, dir d) {
  tr_inh_trans_tp *found;

  assert(is_type(tp));
  found = get_firm_kind_entry((firm_kind *)tp);
  return (d == d_up) ? found->up : found->down;
}
/*
static void  add_type_map(type *tp, dir d, type *new) {
  tr_inh_trans_tp *found;

  assert(is_type(tp) && is_type(new));
  found = get_firm_kind_entry((firm_kind *)tp);
  if (d == d_up) pset_insert_ptr(found->up,   new);
  else           pset_insert_ptr(found->down, new);
}
*/


/**
 * Walk over all types reachable from tp in the sub/supertype
 * relation and compute the closure for the two downwards directed
 * relations.
 *
 * The walk in the dag formed by the relation is tricky:  We must visit
 * all subtypes before visiting the supertypes.  So we first walk down.
 * Then we can compute the closure for this type.  Then we walk up.
 * As we call ourselves recursive, and walk in both directions, there
 * can be cycles.  So we have to make sure, that if we visit a node
 * a second time (in a walk up) we do nothing.  For this we increment
 * the master visited flag twice.
 * If the type is marked with master_flag_visited-1 it is on the stack.
 * If it is marked with master_flag_visited it is fully processed.
 *
 * Well, we still miss some candidates ... */
static void compute_down_closure(type *tp) {
  pset *myset, *subset;
  int i, n_subtypes, n_members, n_supertypes;
  unsigned long master_visited = get_master_type_visited();

  assert(is_Class_type(tp));

  set_type_visited(tp, master_visited-1);

  /* Recursive descend. */
  n_subtypes = get_class_n_subtypes(tp);
  for (i = 0; i < n_subtypes; ++i) {
    type *stp = get_class_subtype(tp, i);
    if (get_type_visited(stp) < master_visited-1) {
      compute_down_closure(stp);
    }
  }

  /* types */
  myset = get_type_map(tp, d_down);
  for (i = 0; i < n_subtypes; ++i) {
    type *stp = get_class_subtype(tp, i);
    subset = get_type_map(stp, d_down);
    pset_insert_ptr(myset, stp);
    pset_insert_pset_ptr(myset, subset);
  }

  /* entities */
  n_members = get_class_n_members(tp);
  for (i = 0; i < n_members; ++i) {
    entity *mem = get_class_member(tp, i);
    int j, n_overwrittenby = get_entity_n_overwrittenby(mem);

    myset = get_entity_map(mem, d_down);
    for (j = 0; j > n_overwrittenby; ++j) {
      entity *ov = get_entity_overwrittenby(mem, j);
      subset = get_entity_map(ov, d_down);
      pset_insert_pset_ptr(myset, subset);
      pset_insert_ptr(myset, ov);
    }
  }

  mark_type_visited(tp);

  /* Walk up. */
  n_supertypes = get_class_n_supertypes(tp);
  for (i = 0; i < n_supertypes; ++i) {
    type *stp = get_class_supertype(tp, i);
    if (get_type_visited(stp) < master_visited-1) {
      compute_down_closure(stp);
    }
  }
}

static void compute_up_closure(type *tp) {
  pset *myset, *subset;
  int i, n_subtypes, n_members, n_supertypes;
  int master_visited = get_master_type_visited();

  assert(is_Class_type(tp));

  set_type_visited(tp, master_visited-1);

  /* Recursive descend. */
  n_supertypes = get_class_n_supertypes(tp);
  for (i = 0; i < n_supertypes; ++i) {
    type *stp = get_class_supertype(tp, i);
    if (get_type_visited(stp) < get_master_type_visited()-1) {
      compute_up_closure(stp);
    }
  }

  /* types */
  myset = get_type_map(tp, d_up);
  for (i = 0; i < n_supertypes; ++i) {
    type *stp = get_class_supertype(tp, i);
    subset = get_type_map(stp, d_up);
    pset_insert_ptr(myset, stp);
    pset_insert_pset_ptr(myset, subset);
  }

  /* entities */
  n_members = get_class_n_members(tp);
  for (i = 0; i < n_members; ++i) {
    entity *mem = get_class_member(tp, i);
    int j, n_overwrites = get_entity_n_overwrites(mem);

    myset = get_entity_map(mem, d_up);
    for (j = 0; j > n_overwrites; ++j) {
      entity *ov = get_entity_overwrites(mem, j);
      subset = get_entity_map(ov, d_up);
      pset_insert_pset_ptr(myset, subset);
      pset_insert_ptr(myset, ov);
    }
  }

  mark_type_visited(tp);

  /* Walk down. */
  n_subtypes = get_class_n_subtypes(tp);
  for (i = 0; i < n_subtypes; ++i) {
    type *stp = get_class_subtype(tp, i);
    if (get_type_visited(stp) < master_visited-1) {
      compute_up_closure(stp);
    }
  }
}

/** Compute the transitive closure of the subclass/superclass and
 *  overwrites/overwrittenby relation.
 *
 *  This function walks over the ir (O(#types+#entities)) to compute the
 *  transitive closure.    */
void compute_inh_transitive_closure(void) {
  int i, n_types = get_irp_n_types();
  free_inh_transitive_closure();

  /* The 'down' relation */
  inc_master_type_visited();  /* Inc twice: one if on stack, second if values computed. */
  inc_master_type_visited();
  for (i = 0; i < n_types; ++i) {
    type *tp = get_irp_type(i);
    if (is_Class_type(tp) && type_not_visited(tp)) { /* For others there is nothing to accumulate. */
      int j, n_subtypes = get_class_n_subtypes(tp);
      int has_unmarked_subtype = false;

      assert(get_type_visited(tp) < get_master_type_visited()-1);
      for (j = 0; j < n_subtypes && !has_unmarked_subtype; ++j) {
	type *stp = get_class_subtype(tp, j);
	if (type_not_visited(stp)) has_unmarked_subtype = true;
      }

      /* This is a good starting point. */
      if (!has_unmarked_subtype)
	compute_down_closure(tp);
    }
  }

  /* The 'up' relation */
  inc_master_type_visited();
  inc_master_type_visited();
  for (i = 0; i < n_types; ++i) {
    type *tp = get_irp_type(i);
    if (is_Class_type(tp) && type_not_visited(tp)) { /* For others there is nothing to accumulate. */
      int j, n_supertypes = get_class_n_supertypes(tp);
      int has_unmarked_supertype = false;

      assert(get_type_visited(tp) < get_master_type_visited()-1);
      for (j = 0; j < n_supertypes && !has_unmarked_supertype; ++j) {
	      type *stp = get_class_supertype(tp, j);
	      if (type_not_visited(stp)) has_unmarked_supertype = true;
      }

      /* This is a good starting point. */
      if (!has_unmarked_supertype)
	      compute_up_closure(tp);
    }
  }

  irp->inh_trans_closure_state = inh_transitive_closure_valid;
}

/** Free memory occupied by the transitive closure information. */
void free_inh_transitive_closure(void) {
  if (tr_inh_trans_set) {
    tr_inh_trans_tp *elt;
    for (elt = set_first(tr_inh_trans_set); elt; elt = set_next(tr_inh_trans_set)) {
      del_pset(elt->up);
      del_pset(elt->down);
    }
    del_set(tr_inh_trans_set);
    tr_inh_trans_set = NULL;
  }
  irp->inh_trans_closure_state = inh_transitive_closure_none;
}

/* - subtype ------------------------------------------------------------- */

type *get_class_trans_subtype_first(type *tp) {
  assert_valid_state();
  return pset_first(get_type_map(tp, d_down));
}

type *get_class_trans_subtype_next (type *tp) {
  assert_valid_state();
  return pset_next(get_type_map(tp, d_down));
}

int is_class_trans_subtype (type *tp, type *subtp) {
  assert_valid_state();
  return (pset_find_ptr(get_type_map(tp, d_down), subtp) != NULL);
}

/* - supertype ----------------------------------------------------------- */

type *get_class_trans_supertype_first(type *tp) {
  assert_valid_state();
  return pset_first(get_type_map(tp, d_up));
}

type *get_class_trans_supertype_next (type *tp) {
  assert_valid_state();
  return pset_next(get_type_map(tp, d_up));
}

/* - overwrittenby ------------------------------------------------------- */

entity *get_entity_trans_overwrittenby_first(entity *ent) {
  assert_valid_state();
  return pset_first(get_entity_map(ent, d_down));
}

entity *get_entity_trans_overwrittenby_next (entity *ent) {
  assert_valid_state();
  return pset_next(get_entity_map(ent, d_down));
}

/* - overwrites ---------------------------------------------------------- */


/** Iterate over all transitive overwritten entities. */
entity *get_entity_trans_overwrites_first(entity *ent) {
  assert_valid_state();
  return pset_first(get_entity_map(ent, d_up));
}

entity *get_entity_trans_overwrites_next (entity *ent) {
  assert_valid_state();
  return pset_next(get_entity_map(ent, d_up));
}





/* ----------------------------------------------------------------------- */
/* Classify pairs of types/entities in the inheritance relations.          */
/* ----------------------------------------------------------------------- */

/* Returns true if low is subclass of high. */
int is_subclass_of(type *low, type *high) {
  int i, n_subtypes;
  assert(is_Class_type(low) && is_Class_type(high));

  if (low == high) return 1;

  if (get_irp_inh_transitive_closure_state() == inh_transitive_closure_valid) {
    pset *m = get_type_map(high, d_down);
    return pset_find_ptr(m, low) ? 1 : 0;
  }

  /* depth first search from high downwards. */
  n_subtypes = get_class_n_subtypes(high);
  for (i = 0; i < n_subtypes; i++) {
    type *stp = get_class_subtype(high, i);
    if (low == stp) return 1;
    if (is_subclass_of(low, stp))
      return 1;
  }
  return 0;
}

int is_superclass_of(type *high, type *low) {
  return is_subclass_of(low, high);
}

int is_overwritten_by(entity *high, entity *low) {
  int i, n_overwrittenby;
  assert(is_entity(low) && is_entity(high));

  if (get_irp_inh_transitive_closure_state() == inh_transitive_closure_valid) {
    pset *m = get_entity_map(high, d_down);
    return pset_find_ptr(m, low) ? 1 : 0;
  }

  /* depth first search from high downwards. */
  n_overwrittenby = get_entity_n_overwrittenby(high);
  for (i = 0; i < n_overwrittenby; i++) {
    entity *ov = get_entity_overwrittenby(high, i);
    if (low == ov) return 1;
    if (is_overwritten_by(low, ov))
      return 1;
  }
  return 0;
}


/** Need two routines because I want to assert the result. */
static entity *resolve_ent_polymorphy2 (type *dynamic_class, entity *static_ent) {
  int i, n_overwrittenby;
  entity *res = NULL;

  if (get_entity_owner(static_ent) == dynamic_class) return static_ent;

  n_overwrittenby = get_entity_n_overwrittenby(static_ent);
  for (i = 0; i < n_overwrittenby; ++i) {
    res = resolve_ent_polymorphy2(dynamic_class, get_entity_overwrittenby(static_ent, i));
    if (res)
      break;
  }

  return res;
}

/* Resolve polymorphy in the inheritance relation.
 *
 * Returns the dynamically referenced entity if the static entity and the
 * dynamic type are given.
 * Search downwards in overwritten tree. */
entity *resolve_ent_polymorphy(type *dynamic_class, entity *static_ent) {
  entity *res;
  assert(static_ent && is_entity(static_ent));

  res = resolve_ent_polymorphy2(dynamic_class, static_ent);
  assert(res);

  return res;
}



/* ----------------------------------------------------------------------- */
/* Class cast state handling.                                              */
/* ----------------------------------------------------------------------- */

/* - State handling. ----------------------------------------- */

void set_irg_class_cast_state(ir_graph *irg, ir_class_cast_state s) {
  if (get_irp_class_cast_state() > s) set_irp_class_cast_state(s);
  irg->class_cast_state = s;
}

ir_class_cast_state get_irg_class_cast_state(ir_graph *irg) {
  return irg->class_cast_state;
}

void set_irp_class_cast_state(ir_class_cast_state s) {
  int i;
  for (i = 0; i < get_irp_n_irgs(); ++i)
    assert(get_irg_class_cast_state(get_irp_irg(i)) >= s);
  irp->class_cast_state = s;
}

ir_class_cast_state get_irp_class_cast_state(void) {
  return irp->class_cast_state;
}

char *get_class_cast_state_string(ir_class_cast_state s) {
#define X(a)    case a: return #a
  switch(s) {
    X(ir_class_casts_any);
    X(ir_class_casts_transitive);
    X(ir_class_casts_normalized);
    X(ir_class_casts_state_max);
  default: return "invalid class cast state";
  }
#undef X
}

/* - State verification. ------------------------------------- */

typedef struct ccs_env {
  ir_class_cast_state expected_state;
  ir_class_cast_state worst_situation;
} ccs_env;

void verify_irn_class_cast_state(ir_node *n, void *env) {
  ccs_env *ccs = (ccs_env *)env;
  ir_class_cast_state this_state = ir_class_casts_any;

  type *fromtype = get_irn_typeinfo_type(get_Cast_op(n));
  type *totype   = get_Cast_type(n);
  int ref_depth = 0;

  while (is_Pointer_type(totype) && is_Pointer_type(fromtype)) {
    totype   = get_pointer_points_to_type(totype);
    fromtype = get_pointer_points_to_type(fromtype);
    ref_depth++;
  }

  if (!is_Class_type(totype)) return;

  if (is_subclass_of(totype, fromtype) ||
      is_subclass_of(fromtype, totype)   ) {
    this_state = ir_class_casts_transitive;
    if ((get_class_supertype_index(totype, fromtype) == -1) &&
	(get_class_supertype_index(fromtype, totype) == -1) ) {
      this_state = ir_class_casts_normalized;
    }
  }

  assert(this_state >= ccs->expected_state &&
	 "invalid state class cast state setting in graph");

  if (this_state < ccs->worst_situation)
    ccs->worst_situation = this_state;
}


/** Verify that the graph meets reqirements of state set. */
void verify_irg_class_cast_state(ir_graph *irg) {
  ccs_env env;

  env.expected_state  = get_irg_class_cast_state(irg);
  env.worst_situation = ir_class_casts_normalized;

  irg_walk_graph(irg, NULL, verify_irn_class_cast_state, &env);

  if ((env.worst_situation > env.expected_state) && get_firm_verbosity()) {
    printf("Note:  class cast state is set lower than reqired in graph\n       ");
    DDMG(irg);
    printf("       state is %s, reqired is %s\n",
	   get_class_cast_state_string(env.expected_state),
	   get_class_cast_state_string(env.worst_situation));
  }
}
