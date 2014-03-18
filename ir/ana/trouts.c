/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Reverse edges that reference types/entities.
 * @author   Goetz Lindenmaier
 * @date     29.10.2004
 */
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

static pmap *entity_access_map;
static pmap *entity_reference_map;
static pmap *type_pointertype_map;
static pmap *type_arraytype_map;

/**
 * Return a flexible array containing all IR-nodes
 * that access a given entity.
 */
static ir_node **get_entity_access_array(const ir_entity *ent)
{
	if (!entity_access_map) entity_access_map = pmap_create();

	ir_node **res = pmap_get(ir_node*, entity_access_map, ent);
	if (!res) {
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
	if (!entity_reference_map) entity_reference_map = pmap_create();

	ir_node **res = pmap_get(ir_node*, entity_reference_map, ent);
	if (!res) {
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
 * Return a flexible array containing all pointer
 * types that points-to a given type.
 */
static ir_type **get_type_pointertype_array(const ir_type *tp)
{
	if (!type_pointertype_map) type_pointertype_map = pmap_create();

	ir_type **res = pmap_get(ir_type*, type_pointertype_map, tp);
	if (!res) {
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
	if (!type_arraytype_map) type_arraytype_map = pmap_create();

	ir_type **res = pmap_get(ir_type*, type_arraytype_map, tp);
	if (!res) {
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
	ir_node **accs = get_entity_access_array(ent);
	return ARR_LEN(accs);
}

ir_node *get_entity_access(const ir_entity *ent, size_t pos)
{
	assert(pos < get_entity_n_accesses(ent));

	ir_node **accs = get_entity_access_array(ent);
	return accs[pos];
}

static void add_entity_access(const ir_entity *ent, ir_node *n)
{
	ir_node **accs = get_entity_access_array(ent);
	ARR_APP1(ir_node *, accs, n);
	set_entity_access_array(ent, accs);
}

/*------------------------------------------------------------------*/

size_t get_entity_n_references(const ir_entity *ent)
{
	ir_node **refs = get_entity_reference_array(ent);
	return ARR_LEN(refs);
}

ir_node *get_entity_reference(const ir_entity *ent, size_t pos)
{
	ir_node **refs = get_entity_reference_array(ent);
	assert(pos < get_entity_n_references(ent));
	return refs[pos];
}

static void add_entity_reference(const ir_entity *ent, ir_node *n)
{
	ir_node **refs = get_entity_reference_array(ent);
	ARR_APP1(ir_node *, refs, n);
	set_entity_reference_array(ent, refs);
}

/**------------------------------------------------------------------*/
/*   Access routines for types                                       */
/**------------------------------------------------------------------*/

size_t get_type_n_pointertypes_to(const ir_type *tp)
{
	ir_type **pts = get_type_pointertype_array(tp);
	return ARR_LEN(pts);
}

ir_type *get_type_pointertype_to(const ir_type *tp, size_t pos)
{
	ir_type **pts = get_type_pointertype_array(tp);
	assert(pos < get_type_n_pointertypes_to(tp));
	return pts[pos];
}

void add_type_pointertype_to(const ir_type *tp, ir_type *ptp)
{
	ir_type **pts = get_type_pointertype_array(tp);
	ARR_APP1(ir_type*, pts, ptp);
	set_type_pointertype_array(tp, pts);
}

/*------------------------------------------------------------------*/

size_t get_type_n_arraytypes_of(const ir_type *tp)
{
	ir_type **pts = get_type_arraytype_array(tp);
	return ARR_LEN(pts);
}

ir_type *get_type_arraytype_of(const ir_type *tp, size_t pos)
{
	ir_type **pts = get_type_arraytype_array(tp);
	assert(pos < get_type_n_arraytypes_of(tp));
	return pts[pos];
}

void  add_type_arraytype_of(const ir_type *tp, ir_type *atp)
{
	ir_type **pts = get_type_arraytype_array(tp);
	ARR_APP1(ir_type*, pts, atp);
	set_type_arraytype_array(tp, pts);
}

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
	(void)sel;
	return 1;
}

/** The entity that cat be accessed by this Sel node. */
static ir_entity *get_Sel_accessed_entity(const ir_node *sel)
{
	return get_Sel_entity(sel);
}

/** An addr node is an Address or a Sel. */
static int get_addr_n_entities(const ir_node *addr)
{
	switch (get_irn_opcode(addr)) {
	case iro_Sel:
		/* Treat jack array sels? */
		return get_Sel_n_accessed_entities(addr);
	case iro_Address:
		return 1;
	default:
		return 0;
	}
}

/** An addr node is an Address or a Sel.
    If Sel follow to outermost of compound. */
static ir_entity *get_addr_entity(const ir_node *addr, int pos)
{
	(void)pos;
	switch (get_irn_opcode(addr)) {
	case iro_Sel: {
		/* Treat jack array sels? They are compounds!  Follow to outermost entity.  */
		ir_node *ptr = get_Sel_ptr(addr);
		while (is_Sel(ptr)) {
			addr = ptr;
			ptr  = get_Sel_ptr(addr);
		}
		assert(0 <= pos && pos < get_Sel_n_accessed_entities(addr));
		return get_Sel_accessed_entity(addr);
	}
	case iro_Address:
		assert(pos == 0);
		return get_Address_entity(addr);
	default:
		return NULL;
	}
}

static void chain_accesses(ir_node *n, void *env)
{
	(void) env;
	ir_node *addr;
	if (is_Sel(n)) {
		add_entity_reference(get_Sel_entity(n), n);
		return;
	} else if (is_Address(n)) {
		add_entity_reference(get_Address_entity(n), n);
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

	for (int i = 0, n_ents = get_addr_n_entities(addr); i < n_ents; ++i) {
		ir_entity *ent = get_addr_entity(addr, i);
		if (ent != NULL)
			add_entity_access(ent, n);
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
	free_trouts();
	init_trouts();

	/* Compute outs for IR nodes. */
	foreach_irp_irg_r(i, irg) {
		irg_walk_graph(irg, NULL, chain_accesses, NULL);
	}
	walk_const_code(NULL, chain_accesses, NULL);

	/* Compute outs for types */
	for (size_t i = get_irp_n_types(); i-- > 0;) {
		ir_type *type = get_irp_type(i);
		chain_types(type);
	}
}

void free_trouts(void)
{
	if (entity_access_map != NULL) {
		/*
		for (ir_node **accs = (ir_node **)pmap_first(entity_access_map);
		     accs != NULL; accs = (ir_node **)pmap_next(entity_access_map)) {
			DEL_ARR_F(accs);
		}
		*/
		pmap_destroy(entity_access_map);
		entity_access_map = NULL;
	}

	if (entity_reference_map != NULL) {
		/*
		for (ir_node **refs = (ir_node **)pmap_first(entity_reference_map);
		     refs != NULL; refs = (ir_node **)pmap_next(entity_reference_map)) {
			DEL_ARR_F(refs);
		}
		*/
		pmap_destroy(entity_reference_map);
		entity_reference_map = NULL;
	}

	if (type_pointertype_map != NULL) {
		/*
		for (ir_node **pts = (ir_node **)pmap_first(type_pointertype_map);
		     pts != NULL; pts = (ir_node **)pmap_next(type_pointertype_map)) {
			DEL_ARR_F(pts);
		}
		*/
		pmap_destroy(type_pointertype_map);
		type_pointertype_map = NULL;
	}

	if (type_arraytype_map) {
		/*
		for (ir_node **pts = (ir_node **)pmap_first(type_arraytype_map);
		     pts != NULL; pts = (ir_node **)pmap_next(type_arraytype_map)) {
			DEL_ARR_F(pts);
		}
		*/
		pmap_destroy(type_arraytype_map);
		type_arraytype_map = NULL;
	}
}
