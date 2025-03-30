/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief      Intraprozedural analyses to estimate the call graph.
 * @author     Hubert Schmid
 * @date       09.06.2002
 * @brief
 *  Interprocedural analysis to estimate the calling relation.
 *
 *  This analysis computes all entities representing methods that
 *  can be called at a Call node.  Further it computes a set of
 *  methods that are 'free', i.e., their adress is handled by
 *  the program directly, or they are visible external.
 */
#include "cgana.h"

#include "array.h"
#include "dbginfo_t.h"
#include "ircons.h"
#include "irdump.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iropt.h"
#include "iropt_dbg.h"
#include "irprog_t.h"
#include "irtools.h"
#include "panic.h"
#include "pmap.h"
#include "xmalloc.h"

/* unambiguous address used as a mark. */
static void *MARK = &MARK;

static pset *entities = NULL;

int cg_call_has_callees(const ir_node *node)
{
	assert(is_Call(node));
	return ((get_irg_callee_info_state(get_irn_irg(node)) != irg_callee_info_none) &&
	        (node->attr.call.callee_arr != NULL));
}

size_t cg_get_call_n_callees(const ir_node *node)
{
  assert(is_Call(node) && node->attr.call.callee_arr);
  return ARR_LEN(node->attr.call.callee_arr);
}

ir_entity *cg_get_call_callee(const ir_node *node, size_t pos)
{
	assert(pos < cg_get_call_n_callees(node));
	return node->attr.call.callee_arr[pos];
}

void cg_set_call_callee_arr(ir_node *node, size_t n, ir_entity **arr)
{
	assert(is_Call(node));
	if (node->attr.call.callee_arr==NULL || cg_get_call_n_callees(node) != n) {
		ir_graph *const irg = get_irn_irg(node);
		node->attr.call.callee_arr = NEW_ARR_D(ir_entity*, get_irg_obstack(irg), n);
	}
	MEMCPY(node->attr.call.callee_arr, arr, n);
}

void cg_remove_call_callee_arr(ir_node *node)
{
	assert(is_Call(node));
	node->attr.call.callee_arr = NULL;
}

/*--------------------------------------------------------------------------*/
/* The analysis                                                             */
/*--------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------*/
/* Initialize data structures, remove unwanted constructs, optimize         */
/* call target computations.                                                */
/*--------------------------------------------------------------------------*/

/** Collect the entity representing the implementation of this
 *  method (not the same if inherited) and all entities for overwriting
 *  implementations in parameter set.
 *  A recursive descend in the overwritten relation.
 *  Cycle-free, therefore must terminate.
 *
 * @param method   the overwritten method
 * @param set      A set of entities.
 *
 * @return Number of entities in set.
 */
static size_t collect_impls(ir_entity *method, pset *set)
{
	size_t size = 0;
	if (get_entity_irg(method) != NULL) {
		/* has an implementation */
		pset_insert_ptr(set, method);
		++size;
	}

	/*- recursive descent -*/
	for (size_t i = get_entity_n_overwrittenby(method); i-- > 0;) {
		size += collect_impls(get_entity_overwrittenby(method, i), set);
	}
	return size;
}

/**
 * Determine all methods that overwrite the given method (and implement it).
 * The returned array must be freed by the caller (see DEL_ARR_F).
 * If the set of overwriting methods is empty, returns NULL.
 *
 * @param method  the method
 */
static ir_entity **get_impl_methods(ir_entity *method)
{
	assert(is_method_entity(method));
	/* Collect all method entities that can be called here */
	ir_entity  **arr = NULL;
	pset       *set  = pset_new_ptr_default();
	size_t      size = collect_impls(method, set);
	if (size > 0) {
		arr = NEW_ARR_F(ir_entity *, size);
		foreach_pset(set, ir_entity, ent) {
			arr[--size] = ent;
		}
	}
	del_pset(set);
	return arr;
}

/** Analyze address computations.
 *
 *  Compute for all Member nodes the set of methods that can be selected.
 *  For each entity we store the set of subentities in the link field.
 *
 *  Further do some optimizations:
 *  - Call standard optimizations for Member nodes: this removes polymorphic
 *    calls.
 *
 *  @param node  The node to analyze
 */
static void sel_methods_walker(ir_node *node, void *env)
{
	(void)env;
	if (!is_Member(node))
		return;

	/* Call standard optimizations */
	ir_node *new_node = optimize_in_place(node);
	if (node != new_node) {
		exchange(node, new_node);
		node = new_node;
		if (!is_Member(node))
			return;
	}

	ir_entity *const entity = get_Member_entity(node);
	if (!is_method_entity(entity))
		return;
	if (!pset_find_ptr(entities, entity)) {
		/* Entity not yet handled. Find all (internal or external)
		 * implemented methods that overwrites this entity.
		 * This set is stored in the entity link. */
		set_entity_link(entity, get_impl_methods(entity));
		pset_insert_ptr(entities, entity);
	}
}

/**
 * Initialize auxiliary data structures.
 *
 * Computes a set of entities that overwrite an entity and contain
 * an implementation. The set is stored in the entity's link field.
 *
 * Further replaces Member nodes where this set contains exactly one
 * method by Address nodes.
 */
static void sel_methods_init(void)
{
	assert(entities == NULL);
	entities = pset_new_ptr_default();
	all_irg_walk(sel_methods_walker, NULL, NULL);
}

/*--------------------------------------------------------------------------*/
/* Find free methods.
 *
 * We expect that each entity has an array with all implementations in its
 * link field.                                                              */
/*--------------------------------------------------------------------------*/

/**
 * Returns an array of all methods that could be called at a Member node.
 * This array contains every entry only once.
 *
 * @param member  the Member node
 */
static ir_entity **get_member_arr(ir_node *member)
{
	ir_entity *const entity = get_Member_entity(member);
	assert(is_Method_type(get_entity_type(entity))); /* what else? */
	return (ir_entity**)get_entity_link(entity);
}

/**
 * Returns the number of possible called methods at a Member node.
 *
 * @param member  the Member node
 */
static size_t get_member_n_methods(ir_node *member)
{
	ir_entity **const arr = get_member_arr(member);
	if (arr == NULL)
		return 0;
	return ARR_LEN(arr);
}

/**
 * Returns the ith possible called method entity at a Member node.
 */
static ir_entity *get_member_method(ir_node *member, size_t pos)
{
	ir_entity **arr = get_member_arr(member);
	assert(pos < ARR_LEN(arr));
	return arr[pos];
}

/* forward */
static void free_mark(ir_node *node, pset *set);

static void free_mark_proj(ir_node *node, unsigned n, pset *set)
{
	assert(get_irn_mode(node) == mode_T);
	if (get_irn_link(node) == MARK) {
		/* already visited */
		return;
	}
	set_irn_link(node, MARK);
	switch (get_irn_opcode(node)) {
	case iro_Proj: {
		/* proj_proj: in a correct graph we now find an op_Tuple or something
		 * which is handled by free_ana_walker(). */
		ir_node *pred = get_Proj_pred(node);
		if (get_irn_link(pred) != MARK && is_Tuple(pred)) {
			free_mark_proj(get_Tuple_pred(pred, get_Proj_num(node)), n, set);
		}
		break;
	}

	case iro_Tuple:
		free_mark(get_Tuple_pred(node, n), set);
		break;

	case iro_Start:
	case iro_Alloc:
	case iro_Load:
	case iro_Builtin:
		/* nothing: operations are handled in free_ana_walker() */
		break;

	default:
		panic("unexpected opcode or opcode not implemented");
	}
}

/**
 * Called for predecessors nodes of "interesting" ones.
 * Interesting ones include all nodes that can somehow make
 * a method visible.
 *
 * If a method (or a set of methods in case of polymorph calls) gets visible,
 * add it to the set of 'free' methods
 *
 * @param node  the current visited node
 * @param set   the set of all free methods
 */
static void free_mark(ir_node *node, pset *set)
{
	if (get_irn_link(node) == MARK)
		return; /* already visited */

	set_irn_link(node, MARK);

	switch (get_irn_opcode(node)) {
	case iro_Member: {
		const ir_entity *ent = get_Member_entity(node);
		if (is_method_entity(ent)) {
			for (size_t i = 0, n = get_member_n_methods(node); i < n; ++i) {
				pset_insert_ptr(set, get_member_method(node, i));
			}
		}
		break;
	}

	case iro_Address: {
		const ir_entity *ent = get_Address_entity(node);
		if (is_method_entity(ent)) {
			pset_insert_ptr(set, ent);
		}
		break;
	}

	case iro_Phi:
		for (int i = 0, n = get_Phi_n_preds(node); i < n; ++i) {
			free_mark(get_Phi_pred(node, i), set);
		}
		break;

	case iro_Proj:
		free_mark_proj(get_Proj_pred(node), get_Proj_num(node), set);
		break;
	default:
		break;
	}
}

/**
 * post-walker. Find method addresses.
 */
static void free_ana_walker(ir_node *node, void *env)
{
	if (get_irn_link(node) == MARK) {
		/* already visited */
		return;
	}

	pset *set = (pset*) env;
	switch (get_irn_opcode(node)) {
		/* special nodes */
	case iro_Address:
	case iro_Align:
	case iro_Member:
	case iro_Const:
	case iro_Offset:
	case iro_Phi:
	case iro_Id:
	case iro_Proj:
	case iro_Size:
	case iro_Tuple:
		/* nothing */
		break;
	case iro_Call:
		/* we must handle Call nodes specially, because their call address input
		   do not expose a method address. */
		set_irn_link(node, MARK);
		for (size_t i = 0, n = get_Call_n_params(node); i < n; ++i) {
			ir_node *pred = get_Call_param(node, i);
			if (mode_is_reference(get_irn_mode(pred))) {
				free_mark(pred, set);
			}
		}
		break;

	default:
		/* other nodes: Alle anderen Knoten nehmen wir als Verr�ter an, bis
		 * jemand das Gegenteil implementiert. */
		set_irn_link(node, MARK);
		foreach_irn_in_r(node, i, pred) {
			if (mode_is_reference(get_irn_mode(pred))) {
				free_mark(pred, set);
			}
		}
		break;
	}
}

/**
 * Add all method addresses in global new style initializers to the set.
 *
 * @note
 * We do NOT check the type here, just if it's an entity address.
 * The reason for this is code like:
 *
 * void *p = function;
 *
 * which is sometimes used to anchor functions.
 */
static void add_method_address_inititializer(ir_initializer_t const *const initializer, pset *const set)
{
	switch (initializer->kind) {
	case IR_INITIALIZER_CONST: {
		ir_node *n = initializer->consti.value;

		/* let's check if it's the address of a function */
		if (is_Address(n)) {
			ir_entity *ent = get_Address_entity(n);

			if (is_Method_type(get_entity_type(ent)))
				pset_insert_ptr(set, ent);
		}
		return;
	}
	case IR_INITIALIZER_TARVAL:
	case IR_INITIALIZER_NULL:
		return;
	case IR_INITIALIZER_COMPOUND:
		for (size_t i = 0; i < initializer->compound.n_initializers; ++i) {
			ir_initializer_t *sub_initializer
				= initializer->compound.initializers[i];
			add_method_address_inititializer(sub_initializer, set);
		}
		return;
	}
	panic("invalid initializer found");
}

/**
 * Add all method addresses in global initializers to the set.
 *
 * @note
 * We do NOT check the type here, just if it's an entity address.
 * The reason for this is code like:
 *
 * void *p = function;
 *
 * which is sometimes used to anchor functions.
 */
static void add_method_address(ir_entity *ent, pset *set)
{
	if (get_entity_kind(ent) == IR_ENTITY_NORMAL) {
		ir_initializer_t const *const init = get_entity_initializer(ent);
		if (init)
			add_method_address_inititializer(init, set);
	}
}

/**
 * returns a list of 'free' methods, i.e., the methods that can be called
 * from external or via function pointers.
 *
 * the data structures for sel_methods must be constructed before calling
 * get_free_methods().
 */
static size_t get_free_methods(ir_entity ***free_methods)
{
	pset *free_set = pset_new_ptr_default();

	foreach_irp_irg(i, irg) {
		ir_entity *const ent = get_irg_entity(irg);
		if (entity_is_externally_visible(ent))
			pset_insert_ptr(free_set, ent);

		ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
		/* Find all method entities that gets "visible" through this graphs,
		 * for instance because their address is stored. */
		irg_walk_graph(irg, firm_clear_link, free_ana_walker, free_set);
		ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	}

	/* insert all methods that are used in global variables initializers */
	ir_type *global_tp = get_glob_type();
	for (size_t j = 0, m = get_compound_n_members(global_tp); j < m; ++j) {
		ir_entity *const ent = get_compound_member(global_tp, j);
		add_method_address(ent, free_set);
	}
	ir_type *tls_tp = get_tls_type();
	for (size_t j = 0, m = get_compound_n_members(tls_tp); j < m; ++j) {
		ir_entity *const ent = get_compound_member(tls_tp, j);
		add_method_address(ent, free_set);
	}

	/* the main program is even then "free", if it's not external visible. */
	ir_graph *irg = get_irp_main_irg();
	if (irg != NULL)
		pset_insert_ptr(free_set, get_irg_entity(irg));

	/* Finally, transform the set into an array. */
	size_t      length = pset_count(free_set);
	ir_entity **arr    = XMALLOCN(ir_entity*, length);
	size_t      i      = 0;
	foreach_pset(free_set, ir_entity, ent) {
		arr[i++] = ent;
	}
	del_pset(free_set);

	*free_methods = arr;
	return length;
}

/*--------------------------------------------------------------------------*/
/* Callee analysis.                                                         */
/*--------------------------------------------------------------------------*/

static void callee_ana_node(ir_node *node, pset *methods);

static void callee_ana_proj(ir_node *node, unsigned n, pset *methods)
{
	assert(get_irn_mode(node) == mode_T);
	if (get_irn_link(node) == MARK) {
		/* already visited */
		return;
	}
	set_irn_link(node, MARK);

	switch (get_irn_opcode(node)) {
	case iro_Proj: {
		/* proj_proj: in a correct graph we now get an op_Tuple or a node
		 * returning a free method. */
		ir_node *pred = get_Proj_pred(node);
		if (get_irn_link(pred) != MARK) {
			if (is_Tuple(pred)) {
				callee_ana_proj(get_Tuple_pred(pred, get_Proj_num(node)), n, methods);
			} else {
				pset_insert_ptr(methods, get_unknown_entity()); /* free method -> unknown */
			}
		}
		break;
	}

	case iro_Tuple:
		callee_ana_node(get_Tuple_pred(node, n), methods);
		break;

	default:
		pset_insert_ptr(methods, get_unknown_entity()); /* free method -> unknown */
		break;
	}
}

/**
 * Analyse a Call address.
 *
 * @param node     the node representing the call address
 * @param methods  after call contains the set of all possibly called entities
 */
static void callee_ana_node(ir_node *node, pset *methods)
{
	assert(mode_is_reference(get_irn_mode(node)) || is_Bad(node));
	/* Beware of recursion */
	if (get_irn_link(node) == MARK) {
		/* already visited */
		return;
	}
	set_irn_link(node, MARK);

	switch (get_irn_opcode(node)) {
	case iro_Const:
		/* A direct address call. We treat this as an external
		   call and ignore it completely. */
		pset_insert_ptr(methods, get_unknown_entity()); /* free method -> unknown */
		break;

	case iro_Address: {
		ir_entity *ent = get_Address_entity(node);
		if (is_method_entity(ent))
			pset_insert_ptr(methods, ent);
		break;
	}

	case iro_Member: {
		ir_entity *entity = get_Member_entity(node);
		if (!is_method_entity(entity))
			break;
		/* polymorphic method */
		for (size_t i = 0, n = get_member_n_methods(node); i < n; ++i) {
			ir_entity *ent = get_member_method(node, i);
			if (ent != NULL) {
				pset_insert_ptr(methods, ent);
			} else {
				pset_insert_ptr(methods, get_unknown_entity());
			}
		}
		break;
	}

	case iro_Bad:
		break;

	case iro_Phi:
		for (int i = get_Phi_n_preds(node) - 1; i >= 0; --i) {
			callee_ana_node(get_Phi_pred(node, i), methods);
		}
		break;

	case iro_Mux:
		callee_ana_node(get_Mux_false(node), methods);
		callee_ana_node(get_Mux_true(node), methods);
		break;

	case iro_Proj:
		callee_ana_proj(get_Proj_pred(node), get_Proj_num(node), methods);
		break;

	case iro_Add:
	case iro_Sub:
	case iro_Conv:
		/* extern */
		pset_insert_ptr(methods, get_unknown_entity()); /* free method -> unknown */
		break;

	default:
		panic("invalid opcode or opcode not implemented");
	}
}

/**
 * Walker: Analyses every Call node and calculates an array of possible
 * callees for that call.
 */
static void callee_walker(ir_node *call, void *env)
{
	(void)env;
	if (!is_Call(call))
		return;

	pset *methods = pset_new_ptr_default();
	callee_ana_node(get_Call_ptr(call), methods);
	ir_entity **arr = NEW_ARR_F(ir_entity*, pset_count(methods));
	size_t      i   = 0;
	foreach_pset(methods, ir_entity, ent) {
		arr[i] = ent;
		/* we want the unknown_entity on the zero position for easy tests later */
		if (is_unknown_entity(ent)) {
			arr[i] = arr[0];
			arr[0] = get_unknown_entity();
		}
		++i;
	}
	cg_set_call_callee_arr(call, ARR_LEN(arr), arr);
	DEL_ARR_F(arr);
	del_pset(methods);
}

/**
 * Determine for every Call the set of possibly called methods and stores it
 * inside the Call (@see set_Call_callee()).
 * Uses the sel_methods set with much be already calculated.
 */
static void callee_ana(void)
{
	/* analyse all graphs */
	foreach_irp_irg(i, irg) {
		assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_TUPLES);
		ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
		irg_walk_graph(irg, callee_walker, NULL, NULL);
		ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
		set_irg_callee_info_state(irg, irg_callee_info_consistent);
	}
	set_irp_callee_info_state(irg_callee_info_consistent);
}

/*--------------------------------------------------------------------------*/
/* Cleanup after analyses.                                                  */
/*--------------------------------------------------------------------------*/

/** Frees intermediate data structures. */
static void sel_methods_dispose(void)
{
	assert(entities);
	foreach_pset(entities, ir_entity, ent) {
		ir_entity **arr = (ir_entity**) get_entity_link(ent);
		if (arr != NULL) {
			DEL_ARR_F(arr);
		}
		set_entity_link(ent, NULL);
	}
	del_pset(entities);
	entities = NULL;
}

static void destruct_walker(ir_node *node, void *env)
{
	(void)env;
	if (is_Call(node))
		cg_remove_call_callee_arr(node);
}

size_t cgana(ir_entity ***free_methods)
{
	/* Optimize Address/Member nodes and compute all methods that implement an
	 * entity. */
	sel_methods_init();
	size_t length = get_free_methods(free_methods);
	callee_ana();
	sel_methods_dispose();
	return length;
}

void free_callee_info(ir_graph *irg)
{
	irg_walk_graph(irg, destruct_walker, NULL, NULL);
	set_irg_callee_info_state(irg, irg_callee_info_none);
}

void free_irp_callee_info(void)
{
	foreach_irp_irg(i, irg) {
		free_callee_info(irg);
	}
}

void opt_call_addrs(void)
{
	/* Optimize the address expressions passed to call nodes.
	 *
	 * This optimization performs the following transformations for
	 * all ir graphs:
	 * - All Address operations that refer to intern methods are replaced
	 *   by Const operations referring to the corresponding entity.
	 * - Member nodes, that select entities that are not overwritten are
	 *   replaced by Const nodes referring to the selected entity.
	 * - Member nodes, for which no method exists at all are replaced by Bad
	 *   nodes.
	 * - Member nodes with a pointer input that is an Alloc node are replaced
	 *   by Const nodes referring to the entity that implements the method in
	 *   the type given by the Alloc node.
	 */
	sel_methods_init();
	sel_methods_dispose();
}
