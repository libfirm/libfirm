/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @file
 * @brief    Reverse edges that reference types/entities.
 * @author   Goetz Lindenmaier
 * @date     29.10.2004
 */
#include "config.h"

#include "trouts_t.h"

#include "array.h"
#include "pmap.h"

#include "irnode_t.h"
#include "irprog_t.h"
#include "irgwalk.h"
#include "irnode.h"


/*------------------------------------------------------------------*/
/* We represent the fields in entities/types by hashmaps.           */
/*------------------------------------------------------------------*/

static pmap *entity_access_map = NULL;
static pmap *entity_reference_map = NULL;
static pmap *type_alloc_map = NULL;
static pmap *type_cast_map = NULL;
static pmap *type_pointertype_map = NULL;
static pmap *type_arraytype_map = NULL;

/**
 * Return a flexible array containing all IR-nodes
 * that access a given entity.
 */
static ir_node **get_entity_access_array(const ir_entity *ent)
{
	ir_node **res;
	if (!entity_access_map) entity_access_map = pmap_create();

	if (pmap_contains(entity_access_map, ent)) {
		res = (ir_node **) pmap_get(entity_access_map, ent);
	} else {
		res = NEW_ARR_F(ir_node *, 0);
		pmap_insert(entity_access_map, ent, (void *)res);
	}

	return res;
}

static void set_entity_access_array(const ir_entity *ent, ir_node **accs)
{
	pmap_insert(entity_access_map, ent, (void *)accs);
}

/**
 * Return a flexible array containing all IR-nodes
 * that reference a given entity.
 */
static ir_node **get_entity_reference_array(const ir_entity *ent)
{
	ir_node **res;
	if (!entity_reference_map) entity_reference_map = pmap_create();

	if (pmap_contains(entity_reference_map, ent)) {
		res = (ir_node **) pmap_get(entity_reference_map, ent);
	} else {
		res = NEW_ARR_F(ir_node *, 0);
		pmap_insert(entity_reference_map, ent, (void *)res);
	}

	return res;
}

static void set_entity_reference_array(const ir_entity *ent, ir_node **refs)
{
	pmap_insert(entity_reference_map, ent, (void *)refs);
}

/**
 * Return a flexible array containing all IR-nodes
 * that allocate a given type.
 */
static ir_node **get_type_alloc_array(const ir_type *tp)
{
	ir_node **res;
	if (!type_alloc_map) type_alloc_map = pmap_create();

	if (pmap_contains(type_alloc_map, tp)) {
		res = (ir_node **) pmap_get(type_alloc_map, tp);
	} else {
		res = NEW_ARR_F(ir_node *, 0);
		pmap_insert(type_alloc_map, tp, (void *)res);
	}

	return res;
}

static void set_type_alloc_array(const ir_type *tp, ir_node **alls)
{
	pmap_insert(type_alloc_map, tp, (void *)alls);
}

/**
 * Return a flexible array containing all Cast-nodes
 * that "create" a given type.
 */
static ir_node **get_type_cast_array(const ir_type *tp)
{
	ir_node **res;
	if (!type_cast_map) type_cast_map = pmap_create();

	if (pmap_contains(type_cast_map, tp)) {
		res = (ir_node **) pmap_get(type_cast_map, tp);
	} else {
		res = NEW_ARR_F(ir_node *, 0);
		pmap_insert(type_cast_map, tp, (void *)res);
	}
	return res;
}

static void set_type_cast_array(const ir_type *tp, ir_node **alls)
{
	pmap_insert(type_cast_map, tp, (void *)alls);
}

/**
 * Return a flexible array containing all pointer
 * types that points-to a given type.
 */
static ir_type **get_type_pointertype_array(const ir_type *tp)
{
	ir_type **res;
	if (!type_pointertype_map) type_pointertype_map = pmap_create();

	if (pmap_contains(type_pointertype_map, tp)) {
		res = (ir_type **) pmap_get(type_pointertype_map, tp);
	} else {
		res = NEW_ARR_F(ir_type *, 0);
		pmap_insert(type_pointertype_map, tp, (void *)res);
	}

	return res;
}

static void set_type_pointertype_array(const ir_type *tp, ir_type **pts)
{
	pmap_insert(type_pointertype_map, tp, (void *)pts);
}

/**
 * Return a flexible array containing all array
 * types that have a given type as element type.
 */
static ir_type **get_type_arraytype_array(const ir_type *tp)
{
	ir_type **res;
	if (!type_arraytype_map) type_arraytype_map = pmap_create();

	if (pmap_contains(type_arraytype_map, tp)) {
		res = (ir_type **) pmap_get(type_arraytype_map, tp);
	} else {
		res = NEW_ARR_F(ir_type *, 0);
		pmap_insert(type_arraytype_map, tp, (void *)res);
	}

	return res;
}

static void set_type_arraytype_array(const ir_type *tp, ir_type **pts)
{
	pmap_insert(type_arraytype_map, tp, (void *)pts);
}

/*------------------------------------------------------------------*/
/* Accessing the out data structures.                               */
/* These routines only work properly if firm is in state            */
/* trouts_consistent or trouts_inconsistent.                        */
/*------------------------------------------------------------------*/

/**------------------------------------------------------------------*/
/*   Access routines for entities                                    */
/**------------------------------------------------------------------*/

size_t get_entity_n_accesses(const ir_entity *ent)
{
	ir_node ** accs;

	assert(ent && is_entity(ent));

	accs = get_entity_access_array(ent);
	return ARR_LEN(accs);
}

ir_node *get_entity_access(const ir_entity *ent, size_t pos)
{
	ir_node ** accs;

	assert(pos < get_entity_n_accesses(ent));

	accs = get_entity_access_array(ent);
	return accs[pos];
}

static void add_entity_access(const ir_entity *ent, ir_node *n)
{
	ir_node ** accs;

	assert(ent && is_entity(ent));
	assert(n && is_ir_node(n));

	accs = get_entity_access_array(ent);
	ARR_APP1(ir_node *, accs, n);
	set_entity_access_array(ent, accs);
}

#if 0
void set_entity_access(const ir_entity *ent, int pos, ir_node *n)
{
	ir_node ** accs;

	assert(0 <= pos && pos < get_entity_n_accesses(ent));
	assert(n && is_ir_node(n));

	accs = get_entity_access_array(ent);
	accs[pos] = n;
}
#endif

/*------------------------------------------------------------------*/

size_t get_entity_n_references(const ir_entity *ent)
{
	ir_node ** refs;

	assert(ent && is_entity(ent));

	refs = get_entity_reference_array(ent);
	return ARR_LEN(refs);
}

ir_node *get_entity_reference(const ir_entity *ent, size_t pos)
{
	ir_node ** refs;

	assert( pos < get_entity_n_references(ent));

	refs = get_entity_reference_array(ent);
	return refs[pos];
}

static void add_entity_reference(const ir_entity *ent, ir_node *n)
{
	ir_node ** refs;

	assert(ent && is_entity(ent));
	assert(n && is_ir_node(n));

	refs = get_entity_reference_array(ent);
	ARR_APP1(ir_node *, refs, n);
	set_entity_reference_array(ent, refs);
}

#if 0
void set_entity_reference(const ir_entity *ent, int pos, ir_node *n)
{
	ir_node ** refs;

	assert(0 <= pos && pos < get_entity_n_references(ent));
	assert(n && is_ir_node(n));

	refs = get_entity_reference_array(ent);
	refs[pos] = n;
}
#endif

/**------------------------------------------------------------------*/
/*   Access routines for types                                       */
/**------------------------------------------------------------------*/

/* Number of Alloc nodes that create an instance of this type */
size_t get_type_n_allocs(const ir_type *tp)
{
	ir_node **allocs;

	assert(tp && is_type(tp));

	allocs = get_type_alloc_array(tp);
	return ARR_LEN(allocs);
}

/* Alloc node that creates an instance of this type */
ir_node *get_type_alloc(const ir_type *tp, size_t pos)
{
	ir_node **allocs;
	assert( pos < get_type_n_allocs(tp));

	allocs = get_type_alloc_array(tp);
	return allocs[pos];
}

static void add_type_alloc(const ir_type *tp, ir_node *n)
{
	ir_node **allocs;

	assert(tp && is_type(tp));
	assert(n && is_ir_node(n));

	allocs = get_type_alloc_array(tp);
	ARR_APP1(ir_node *, allocs, n);
	set_type_alloc_array(tp, allocs);
}

#if 0
void set_type_alloc(const ir_type *tp, int pos, ir_node *n)
{
	ir_node **allocs;

	assert(0 <= pos && pos < get_type_n_allocs(tp));
	assert(n && is_ir_node(n));

	allocs = get_type_alloc_array(tp);
	allocs[pos] = n;
}
#endif

/* Number of Cast nodes that create an instance of this type */
size_t get_type_n_casts(const ir_type *tp)
{
	ir_node **casts;

	assert(tp && is_type(tp));

	casts = get_type_cast_array(tp);
	return ARR_LEN(casts);
}


size_t get_class_n_upcasts(const ir_type *clss)
{
	size_t i, n_casts = get_type_n_casts(clss);
	size_t n_instances = 0;
	for (i = 0; i < n_casts; ++i) {
		ir_node *cast = get_type_cast(clss, i);
		if (is_Cast_upcast(cast))
			++n_instances;
	}
	return n_instances;
}

size_t get_class_n_downcasts(const ir_type *clss)
{
	size_t i, n_casts = get_type_n_casts(clss);
	size_t n_instances = 0;
	for (i = 0; i < n_casts; ++i) {
		ir_node *cast = get_type_cast(clss, i);
		if (is_Cast_downcast(cast))
			++n_instances;
	}
	return n_instances;
}

ir_node *get_type_cast(const ir_type *tp, size_t pos)
{
	ir_node **casts;
	assert(pos < get_type_n_casts(tp));

	casts = get_type_cast_array(tp);
	return casts[pos];
}

void add_type_cast(const ir_type *tp, ir_node *n)
{
	ir_node **casts;

	assert(tp && is_type(tp));
	assert(n && is_ir_node(n));

	casts = get_type_cast_array(tp);
	ARR_APP1(ir_node *, casts, n);
	set_type_cast_array(tp, casts);
}

#if 0
void set_type_cast(const ir_type *tp, size_t pos, ir_node *n)
{
	ir_node **casts;

	assert(pos < get_type_n_casts(tp));
	assert(n && is_ir_node(n));

	casts = get_type_cast_array(tp);
	casts[pos] = n;
}
#endif

/*------------------------------------------------------------------*/

size_t get_type_n_pointertypes_to(const ir_type *tp)
{
	ir_type ** pts;

	assert(tp && is_type(tp));

	pts = get_type_pointertype_array(tp);
	return ARR_LEN(pts);
}

ir_type *get_type_pointertype_to(const ir_type *tp, size_t pos)
{
	ir_type ** pts;

	assert(pos < get_type_n_pointertypes_to(tp));

	pts = get_type_pointertype_array(tp);
	return pts[pos];
}

void add_type_pointertype_to(const ir_type *tp, ir_type *ptp)
{
	ir_type ** pts;

	assert(tp && is_type(tp));
	assert(ptp && is_Pointer_type(ptp));

	pts = get_type_pointertype_array(tp);
	ARR_APP1(ir_type*, pts, ptp);
	set_type_pointertype_array(tp, pts);
}

#if 0
void set_type_pointertype_to(const ir_type *tp, int pos, ir_type *ptp)
{
	ir_type ** pts;

	assert(0 <= pos && pos < get_type_n_pointertypes_to(tp));
	assert(ptp && is_Pointer_type(ptp));

	pts = get_type_pointertype_array(tp);
	pts[pos] = ptp;
}
#endif

/*------------------------------------------------------------------*/

size_t get_type_n_arraytypes_of(const ir_type *tp)
{
	ir_type ** pts;

	assert(tp && is_type(tp));

	pts = get_type_arraytype_array(tp);
	return ARR_LEN(pts);
}

ir_type *get_type_arraytype_of(const ir_type *tp, size_t pos)
{
	ir_type ** pts;

	assert(pos < get_type_n_arraytypes_of(tp));

	pts = get_type_arraytype_array(tp);
	return pts[pos];
}

void  add_type_arraytype_of(const ir_type *tp, ir_type *atp)
{
	ir_type ** pts;

	assert(tp && is_type(tp));
	assert(atp && is_Array_type(atp));

	pts = get_type_arraytype_array(tp);
	ARR_APP1(ir_type*, pts, atp);
	set_type_arraytype_array(tp, pts);
}

#if 0
void  set_type_arraytype_of(const ir_type *tp, int pos, ir_type *atp)
{
	ir_type ** pts;

	assert(0 <= pos && pos < get_type_n_arraytypes_of(tp));
	assert(atp && is_Array_type(atp));

	pts = get_type_arraytype_array(tp);
	pts[pos] = atp;
}
#endif

/*------------------------------------------------------------------*/
/* Building and Removing the out datastructure                      */
/*------------------------------------------------------------------*/

/** Initialize the trouts handling. */
static void init_trouts(void)
{
}

/** The number of entities that can be accessed by this Sel node. */
static int get_Sel_n_accessed_entities(const ir_node *sel)
{
	(void) sel;
	return 1;
}

/** The entity that cat be accessed by this Sel node. */
static ir_entity *get_Sel_accessed_entity(const ir_node *sel)
{
	return get_Sel_entity(sel);
}

/** An addr node is a SymConst or a Sel. */
static int get_addr_n_entities(const ir_node *addr)
{
	switch (get_irn_opcode(addr)) {
	case iro_Sel:
		/* Treat jack array sels? */
		return get_Sel_n_accessed_entities(addr);
	case iro_SymConst:
		if (get_SymConst_kind(addr) == symconst_addr_ent)
			return 1;
		return 0;
	default:
		return 0;
	}
}

/** An addr node is a SymConst or a Sel.
    If Sel follow to outermost of compound. */
static ir_entity *get_addr_entity(const ir_node *addr, int pos)
{
	ir_node *ptr;
	(void) pos;

	switch (get_irn_opcode(addr)) {
	case iro_Sel:
		/* Treat jack array sels? They are compounds!  Follow to outermost entity.  */
		ptr = get_Sel_ptr(addr);
		while (is_Sel(ptr)) {
			addr = ptr;
			ptr  = get_Sel_ptr(addr);
		}
		assert(0 <= pos && pos < get_Sel_n_accessed_entities(addr));
		return get_Sel_accessed_entity(addr);
	case iro_SymConst:
		if (get_SymConst_kind(addr) == symconst_addr_ent) {
			assert(pos == 0);
			return get_SymConst_entity(addr);
		}
		return NULL;
	default:
		return NULL;
	}
}

static void chain_accesses(ir_node *n, void *env)
{
	int i, n_ents;
	ir_node *addr;

	(void) env;
	if (is_Alloc(n)) {
		add_type_alloc(get_Alloc_type(n), n);
		return;
	} else if (is_Cast(n)) {
		add_type_cast(get_Cast_type(n), n);
		return;
	} else if (is_Sel(n)) {
		add_entity_reference(get_Sel_entity(n), n);
		return;
	} else if (is_SymConst_addr_ent(n)) {
		add_entity_reference(get_SymConst_entity(n), n);
		return;
	} else if (is_Store(n)) {
		addr = get_Store_ptr(n);
	} else if (is_Load(n)) {
		addr = get_Load_ptr(n);
	} else if (is_Call(n)) {
		addr = get_Call_ptr(n);
		if (! is_Sel(addr)) return;  /* Sels before Calls mean a Load / polymorphic Call. */
	} else {
		return;
	}

	n_ents = get_addr_n_entities(addr);  /* == 1 */
	for (i = 0; i < n_ents; ++i) {
		ir_entity *ent = get_addr_entity(addr, i);
		if (ent)
			add_entity_access(ent, n);
		//else
		//add_unrecognized_access(n);
	}
}

/**
 * Handle chain types (pointer, array) by adding them to
 * its "inner" type.
 */
static void chain_types(ir_type *tp)
{
	if (is_Pointer_type(tp)) {
		add_type_pointertype_to(get_pointer_points_to_type(tp), tp);
	} else if (is_Array_type(tp)) {
		add_type_arraytype_of(get_array_element_type(tp), tp);
	}
}

void compute_trouts(void)
{
	size_t i;

	free_trouts();
	init_trouts();

	/* Compute outs for IR nodes. */
	for (i = get_irp_n_irgs(); i > 0;) {
		ir_graph *irg = get_irp_irg(--i);
		irg_walk_graph(irg, NULL, chain_accesses, NULL);
	}
	walk_const_code(NULL, chain_accesses, NULL);

	/* Compute outs for types */
	for (i = get_irp_n_types(); i > 0;) {
		ir_type *type = get_irp_type(--i);
		chain_types(type);
	}
}

void free_trouts(void)
{
	if (entity_access_map) {
		ir_node **accs;
		for (accs = (ir_node **)pmap_first(entity_access_map);
			accs;
			accs = (ir_node **)pmap_next(entity_access_map)) {
			/* DEL_ARR_F(accs); */
		}
		pmap_destroy(entity_access_map);
		entity_access_map = NULL;
	}

	if (entity_reference_map) {
		ir_node **refs;
		for (refs = (ir_node **)pmap_first(entity_reference_map);
			refs;
			refs = (ir_node **)pmap_next(entity_reference_map)) {
			/* DEL_ARR_F(refs); */
		}
		pmap_destroy(entity_reference_map);
		entity_reference_map = NULL;
	}

	if (type_alloc_map) {
		ir_node **alls;
		for (alls = (ir_node **)pmap_first(type_alloc_map);
			alls;
			alls = (ir_node **)pmap_next(type_alloc_map)) {
			/* DEL_ARR_F(alls); */
		}
		pmap_destroy(type_alloc_map);
		type_alloc_map = NULL;
	}

	if (type_cast_map) {
		ir_node **casts;
		for (casts = (ir_node **)pmap_first(type_cast_map);
			casts;
			casts = (ir_node **)pmap_next(type_cast_map)) {
			/* DEL_ARR_F(alls); */
		}
		pmap_destroy(type_cast_map);
		type_cast_map = NULL;
	}

	if (type_pointertype_map) {
		ir_node **pts;
		for (pts = (ir_node **)pmap_first(type_pointertype_map);
			pts;
			pts = (ir_node **)pmap_next(type_pointertype_map)) {
			/* DEL_ARR_F(pts); */
		}
		pmap_destroy(type_pointertype_map);
		type_pointertype_map = NULL;
	}

	if (type_arraytype_map) {
		ir_node **pts;
		for (pts = (ir_node **)pmap_first(type_arraytype_map);
			pts;
			pts = (ir_node **)pmap_next(type_arraytype_map)) {
			/* DEL_ARR_F(pts); */
		}
		pmap_destroy(type_arraytype_map);
		type_arraytype_map = NULL;
	}
}
