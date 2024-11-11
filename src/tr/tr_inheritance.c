/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Utility routines for inheritance representation
 * @author  Goetz Lindenmaier
 */
#include "irflag.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irprog_t.h"
#include "pset.h"
#include "set.h"
#include "typerep.h"

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

void set_irp_inh_transitive_closure_state(inh_transitive_closure_state s)
{
	irp->inh_trans_closure_state = s;
}
void invalidate_irp_inh_transitive_closure_state(void)
{
	if (irp->inh_trans_closure_state == inh_transitive_closure_valid)
		irp->inh_trans_closure_state = inh_transitive_closure_invalid;
}
inh_transitive_closure_state get_irp_inh_transitive_closure_state(void)
{
	return irp->inh_trans_closure_state;
}

static void assert_valid_state(void)
{
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

typedef enum {
	d_up   = 0,
	d_down = 1,
} dir;

typedef struct {
	const firm_kind *kind;   /**< An entity or type. */
	pset            *directions[2];
} tr_inh_trans_tp;

/* We use this set for all types and entities.  */
static set *tr_inh_trans_set = NULL;

/**
 * Compare two tr_inh_trans_tp entries.
 */
static int tr_inh_trans_cmp(const void *e1, const void *e2, size_t size)
{
	(void) size;
	const tr_inh_trans_tp *ef1 = (const tr_inh_trans_tp*)e1;
	const tr_inh_trans_tp *ef2 = (const tr_inh_trans_tp*)e2;
	return ef1->kind != ef2->kind;
}

/**
 * calculate the hash value of an tr_inh_trans_tp
 */
static inline unsigned int tr_inh_trans_hash(const tr_inh_trans_tp *v)
{
	return hash_ptr(v->kind);
}

/* This always completes successfully. */
static tr_inh_trans_tp *get_firm_kind_entry(const firm_kind *k)
{
	if (tr_inh_trans_set == NULL)
		tr_inh_trans_set = new_set(tr_inh_trans_cmp, 128);

	tr_inh_trans_tp a;
	a.kind = k;
	tr_inh_trans_tp *found = set_find(tr_inh_trans_tp, tr_inh_trans_set, &a, sizeof(a), tr_inh_trans_hash(&a));
	if (!found) {
		a.directions[d_up]   = pset_new_ptr(16);
		a.directions[d_down] = pset_new_ptr(16);
		found = set_insert(tr_inh_trans_tp, tr_inh_trans_set, &a, sizeof(a), tr_inh_trans_hash(&a));
	}
	return found;
}

static pset *get_entity_map(const ir_entity *ent, dir d)
{
	assert(is_entity(ent));
	tr_inh_trans_tp *found = get_firm_kind_entry((const firm_kind *)ent);
	return found->directions[d];
}

static pset *get_type_map(const ir_type *tp, dir d)
{
	tr_inh_trans_tp *found = get_firm_kind_entry((const firm_kind *)tp);
	return found->directions[d];
}


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
static void compute_down_closure(ir_type *tp)
{
	ir_visited_t master_visited = get_master_type_visited();
	set_type_visited(tp, master_visited-1);

	/* Recursive descend. */
	size_t n_subtypes = get_class_n_subtypes(tp);
	for (size_t i = 0; i < n_subtypes; ++i) {
		ir_type *stp = get_class_subtype(tp, i);
		if (get_type_visited(stp) < master_visited-1) {
			compute_down_closure(stp);
		}
	}

	/* types */
	pset *myset = get_type_map(tp, d_down);
	for (size_t i = 0; i < n_subtypes; ++i) {
		ir_type *stp    = get_class_subtype(tp, i);
		pset    *subset = get_type_map(stp, d_down);
		pset_insert_ptr(myset, stp);
		pset_insert_pset_ptr(myset, subset);
	}

	/* entities */
	for (size_t i = 0, n_members = get_compound_n_members(tp); i < n_members;
	     ++i) {
		ir_entity *mem = get_compound_member(tp, i);
		size_t j, n_overwrittenby = get_entity_n_overwrittenby(mem);

		myset = get_entity_map(mem, d_down);
		for (j = 0; j < n_overwrittenby; ++j) {
			ir_entity *ov     = get_entity_overwrittenby(mem, j);
			pset      *subset = get_entity_map(ov, d_down);
			pset_insert_ptr(myset, ov);
			pset_insert_pset_ptr(myset, subset);
		}
	}

	mark_type_visited(tp);

	/* Walk up. */
	for (size_t i = 0, n_supertypes = get_class_n_supertypes(tp);
	     i < n_supertypes; ++i) {
		ir_type *stp = get_class_supertype(tp, i);
		if (get_type_visited(stp) < master_visited-1) {
			compute_down_closure(stp);
		}
	}
}

static void compute_up_closure(ir_type *tp)
{
	ir_visited_t master_visited = get_master_type_visited();
	set_type_visited(tp, master_visited-1);

	/* Recursive descend. */
	size_t n_supertypes = get_class_n_supertypes(tp);
	for (size_t i = 0; i < n_supertypes; ++i) {
		ir_type *stp = get_class_supertype(tp, i);
		if (get_type_visited(stp) < get_master_type_visited()-1) {
			compute_up_closure(stp);
		}
	}

	/* types */
	pset *myset = get_type_map(tp, d_up);
	for (size_t i = 0; i < n_supertypes; ++i) {
		ir_type *stp    = get_class_supertype(tp, i);
		pset    *subset = get_type_map(stp, d_up);
		pset_insert_ptr(myset, stp);
		pset_insert_pset_ptr(myset, subset);
	}

	/* entities */
	for (size_t i = 0, n_members = get_compound_n_members(tp); i < n_members;
	     ++i) {
		ir_entity *mem   = get_compound_member(tp, i);
		pset      *myset = get_entity_map(mem, d_up);
		for (size_t j = 0, n_overwrites = get_entity_n_overwrites(mem);
		     j < n_overwrites; ++j) {
			ir_entity *ov     = get_entity_overwrites(mem, j);
			pset      *subset = get_entity_map(ov, d_up);
			pset_insert_pset_ptr(myset, subset);
			pset_insert_ptr(myset, ov);
		}
	}

	mark_type_visited(tp);

	/* Walk down. */
	for (size_t i = 0, n_subtypes = get_class_n_subtypes(tp);
	     i < n_subtypes; ++i) {
		ir_type *stp = get_class_subtype(tp, i);
		if (get_type_visited(stp) < master_visited-1) {
			compute_up_closure(stp);
		}
	}
}

void compute_inh_transitive_closure(void)
{
	free_inh_transitive_closure();

	/* The 'down' relation */
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();  /* Inc twice: one if on stack, second if values computed. */
	inc_master_type_visited();
	size_t n_types = get_irp_n_types();
	for (size_t i = 0; i < n_types; ++i) {
		ir_type *tp = get_irp_type(i);
		if (is_Class_type(tp) && !type_visited(tp)) { /* For others there is nothing to accumulate. */
			size_t n_subtypes = get_class_n_subtypes(tp);
			int has_unmarked_subtype = 0;

			assert(get_type_visited(tp) < get_master_type_visited()-1);
			for (size_t j = 0; j < n_subtypes; ++j) {
				ir_type *stp = get_class_subtype(tp, j);
				if (!type_visited(stp)) {
					has_unmarked_subtype = 1;
					break;
				}
			}

			/* This is a good starting point. */
			if (!has_unmarked_subtype)
				compute_down_closure(tp);
		}
	}

	/* The 'up' relation */
	inc_master_type_visited();
	inc_master_type_visited();
	for (size_t i = 0; i < n_types; ++i) {
		ir_type *tp = get_irp_type(i);
		if (is_Class_type(tp) && !type_visited(tp)) { /* For others there is nothing to accumulate. */
			size_t n_supertypes = get_class_n_supertypes(tp);
			int has_unmarked_supertype = 0;

			assert(get_type_visited(tp) < get_master_type_visited()-1);
			for (size_t j = 0; j < n_supertypes; ++j) {
				ir_type *stp = get_class_supertype(tp, j);
				if (!type_visited(stp)) {
					has_unmarked_supertype = 1;
					break;
				}
			}

			/* This is a good starting point. */
			if (!has_unmarked_supertype)
				compute_up_closure(tp);
		}
	}

	irp->inh_trans_closure_state = inh_transitive_closure_valid;
	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
}

void free_inh_transitive_closure(void)
{
	if (tr_inh_trans_set) {
		foreach_set(tr_inh_trans_set, tr_inh_trans_tp, elt) {
			del_pset(elt->directions[d_up]);
			del_pset(elt->directions[d_down]);
		}
		del_set(tr_inh_trans_set);
		tr_inh_trans_set = NULL;
	}
	irp->inh_trans_closure_state = inh_transitive_closure_none;
}

/* - subtype ------------------------------------------------------------- */

ir_type *get_class_trans_subtype_first(const ir_type *tp)
{
	assert_valid_state();
	return pset_first(ir_type, get_type_map(tp, d_down));
}

ir_type *get_class_trans_subtype_next(const ir_type *tp)
{
	assert_valid_state();
	return pset_next(ir_type, get_type_map(tp, d_down));
}

int is_class_trans_subtype(const ir_type *tp, const ir_type *subtp)
{
	assert_valid_state();
	return (pset_find_ptr(get_type_map(tp, d_down), subtp) != NULL);
}

/* - supertype ----------------------------------------------------------- */

ir_type *get_class_trans_supertype_first(const ir_type *tp)
{
	assert_valid_state();
	return pset_first(ir_type, get_type_map(tp, d_up));
}

ir_type *get_class_trans_supertype_next(const ir_type *tp)
{
	assert_valid_state();
	return pset_next(ir_type, get_type_map(tp, d_up));
}

/* - overwrittenby ------------------------------------------------------- */

ir_entity *get_entity_trans_overwrittenby_first(const ir_entity *ent)
{
	assert_valid_state();
	return pset_first(ir_entity, get_entity_map(ent, d_down));
}

ir_entity *get_entity_trans_overwrittenby_next(const ir_entity *ent)
{
	assert_valid_state();
	return pset_next(ir_entity, get_entity_map(ent, d_down));
}

/* - overwrites ---------------------------------------------------------- */


ir_entity *get_entity_trans_overwrites_first(const ir_entity *ent)
{
	assert_valid_state();
	return pset_first(ir_entity, get_entity_map(ent, d_up));
}

ir_entity *get_entity_trans_overwrites_next(const ir_entity *ent)
{
	assert_valid_state();
	return pset_next(ir_entity, get_entity_map(ent, d_up));
}


/* ----------------------------------------------------------------------- */
/* Classify pairs of types/entities in the inheritance relations.          */
/* ----------------------------------------------------------------------- */

/** Returns true if low is subclass of high. */
static int check_is_SubClass_of(const ir_type *low, const ir_type *high)
{
	/* depth first search from high downwards. */
	for (size_t i = 0, n_subtypes = get_class_n_subtypes(high);
	     i < n_subtypes; i++) {
		ir_type *stp = get_class_subtype(high, i);
		if (low == stp) return 1;
		if (is_SubClass_of(low, stp))
			return 1;
	}
	return 0;
}

int is_SubClass_of(const ir_type *low, const ir_type *high)
{
	assert(is_Class_type(low) && is_Class_type(high));
	if (low == high)
		return 1;

	if (get_irp_inh_transitive_closure_state() == inh_transitive_closure_valid) {
		pset *m = get_type_map(high, d_down);
		return pset_find_ptr(m, low) ? 1 : 0;
	}
	return check_is_SubClass_of(low, high);
}

int is_SubClass_ptr_of(ir_type *low, ir_type *high)
{
	while (is_Pointer_type(low) && is_Pointer_type(high)) {
		low  = get_pointer_points_to_type(low);
		high = get_pointer_points_to_type(high);
	}

	if (is_Class_type(low) && is_Class_type(high))
		return is_SubClass_of(low, high);
	return 0;
}

int is_overwritten_by(ir_entity *high, ir_entity *low)
{
	assert(is_entity(low) && is_entity(high));
	if (get_irp_inh_transitive_closure_state() == inh_transitive_closure_valid) {
		pset *m = get_entity_map(high, d_down);
		return pset_find_ptr(m, low) ? 1 : 0;
	}

	/* depth first search from high downwards. */
	for (size_t i = 0, n_overwrittenby = get_entity_n_overwrittenby(high);
	     i < n_overwrittenby; i++) {
		ir_entity *ov = get_entity_overwrittenby(high, i);
		if (low == ov) return 1;
		if (is_overwritten_by(low, ov))
			return 1;
	}
	return 0;
}

/** Resolve polymorphy in the inheritance relation.
 *
 * Returns the dynamically referenced entity if the static entity and the
 * dynamic type are given.
 * Search downwards in overwritten tree.
 *
 * Need two routines because I want to assert the result.
 */
static ir_entity *do_resolve_ent_polymorphy(ir_type *dynamic_class, ir_entity *static_ent)
{
	ir_type *owner = get_entity_owner(static_ent);
	if (owner == dynamic_class)
		return static_ent;

	// if the owner of the static_ent already is more special than the dynamic
	// type to check against - stop here.
	if (!is_SubClass_of(dynamic_class, owner))
		return NULL;

	for (size_t i = 0, n_overwrittenby = get_entity_n_overwrittenby(static_ent);
	     i < n_overwrittenby; ++i) {
		ir_entity *ent = get_entity_overwrittenby(static_ent, i);
		ent = do_resolve_ent_polymorphy(dynamic_class, ent);
		if (ent) return ent;
	}

	// No further specialization of static_ent has been found
	return static_ent;
}

ir_entity *resolve_ent_polymorphy(ir_type *dynamic_class, ir_entity *static_ent)
{
	assert(is_entity(static_ent));
	ir_entity *res = do_resolve_ent_polymorphy(dynamic_class, static_ent);
	assert(res != NULL);
	return res;
}
