/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Entry point to the representation of procedure code.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#include "irgraph_t.h"

#include "array.h"
#include "irbackedge_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irmemory.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "iroptimize.h"
#include "irouts.h"
#include "irprog_t.h"
#include "irtools.h"
#include "type_t.h"
#include "util.h"
#include "xmalloc.h"

#define INITIAL_IDX_IRN_MAP_SIZE 1024

ir_graph *current_ir_graph;

ir_graph *get_current_ir_graph(void)
{
	return current_ir_graph;
}

void set_current_ir_graph(ir_graph *graph)
{
	assert(graph == NULL
	       || irg_is_constrained(graph, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	current_ir_graph = graph;
}

/**
 * Allocate a new IR graph.
 * This function respects the registered graph data. The only reason for
 * this function is, that there are two locations, where graphs are
 * allocated (new_r_ir_graph, new_const_code_irg).
 * @return Memory for a new graph.
 */
static ir_graph *alloc_graph(void)
{
	ir_graph *const res = XMALLOCZ(ir_graph);
	res->kind = k_ir_graph;

	/* initialize the idx->node map. */
	res->idx_irn_map = NEW_ARR_FZ(ir_node*, INITIAL_IDX_IRN_MAP_SIZE);

	obstack_init(&res->obst);

	/* value table for global value numbering for optimizing use in iropt.c */
	new_identities(res);

	return res;
}

/**
 * Frees an allocated IR graph
 */
static void free_graph(ir_graph *irg)
{
	for (ir_edge_kind_t i = EDGE_KIND_FIRST; i <= EDGE_KIND_LAST; ++i)
		edges_deactivate_kind(irg, i);
	DEL_ARR_F(irg->idx_irn_map);
	free(irg);
}

void irg_set_nloc(ir_graph *res, int n_loc)
{
	assert(irg_is_constrained(res, IR_GRAPH_CONSTRAINT_CONSTRUCTION));

	/* number of local variables that are never dereferenced in this graph plus
	 * one for the store. This is not the number of parameters to the
	 * procedure! */
	res->n_loc = n_loc + 1;

	if (res->loc_descriptions) {
		free(res->loc_descriptions);
		res->loc_descriptions = NULL;
	}
}

static ir_graph *new_r_ir_graph(ir_entity *ent, int n_loc)
{
	ir_graph *const res = alloc_graph();

	/* Inform statistics here, as blocks will be already built on this graph. */
	hook_new_graph(res, ent);

	/* graphs are in construction mode by default */
	add_irg_constraints(res, IR_GRAPH_CONSTRAINT_CONSTRUCTION);
	irg_set_nloc(res, n_loc);

	res->irg_pinned_state  = op_pin_state_pinned;
	res->callee_info_state = irg_callee_info_none;
	res->mem_disambig_opt  = aa_opt_inherited;

	/*-- Type information for the procedure of the graph --*/
	res->ent = ent;
	if (ent)
		set_entity_irg(ent, res);

	/*--  a class type so that it can contain "inner" methods as in Pascal. --*/
	res->frame_type = new_type_frame();

	/* the Anchor node must be created first */
	res->anchor = new_r_Anchor(res);

	/*-- Nodes needed in every graph --*/
	set_irg_end_block(res, new_r_immBlock(res));
	set_irg_end(res, new_r_End(res, 0, NULL));

	ir_node *const start_block = new_r_Block_noopt(res, 0, NULL);
	set_irg_start_block(res, start_block);
	set_irg_no_mem(res, new_r_NoMem(res));

	res->index = get_irp_new_irg_idx();
#ifdef DEBUG_libfirm
	res->graph_nr = get_irp_new_node_nr();
#endif

	set_r_cur_block(res, start_block);

	return res;
}

ir_graph *new_ir_graph(ir_entity *ent, int n_loc)
{
	/* We cannot create graphs before setting mode_P. */
	assert(mode_P != NULL && "mode_P is not set (target not initialized?)");

	ir_graph *res = new_r_ir_graph(ent, n_loc);

	ir_node *const start = new_r_Start(res);
	set_irg_start(res, start);

	/* Proj results of start node */
	set_irg_frame(res, new_r_Proj(start, mode_P, pn_Start_P_frame_base));
	set_irg_args(res, new_r_Proj(start, mode_T, pn_Start_T_args));
	ir_node *const initial_mem = new_r_Proj(start, mode_M, pn_Start_M);
	set_irg_initial_mem(res, initial_mem);

	set_r_store(res, initial_mem);

	add_irp_irg(res);
	return res;
}

ir_graph *new_const_code_irg(void)
{
	ir_graph *const res = new_r_ir_graph(NULL, 0);
	mature_immBlock(get_irg_end_block(res));

	/* There is no Start node in the const_code_irg */
	set_irg_start(res, new_r_Bad(res, mode_T));
	set_irg_frame(res, new_r_Bad(res, mode_BAD));
	set_irg_args(res, new_r_Bad(res, mode_T));
	set_irg_initial_mem(res, new_r_Bad(res, mode_M));
	set_r_store(res, get_irg_no_mem(res));

	/* Set the visited flag high enough that the blocks will never be
	 * visited. */
	ir_node *const body_block = get_r_cur_block(res);
	set_irn_visited(body_block, -1);
	set_Block_block_visited(body_block, -1);
	ir_node *const start_block = get_irg_start_block(res);
	set_Block_block_visited(start_block, -1);
	set_irn_visited(start_block, -1);

	return res;
}

/**
 * Pre-Walker: Copies blocks and nodes from the original method graph
 * to the copied graph.
 *
 * @param n    A node from the original method graph.
 * @param env  The copied graph.
 */
static void copy_all_nodes(ir_node *node, void *env)
{
	ir_graph *irg      = (ir_graph*)env;
	ir_node  *new_node = irn_copy_into_irg(node, irg);

	set_irn_link(node, new_node);

	/* fix access to entities on the stack frame */
	if (is_Member(new_node)) {
		ir_entity *ent = get_Member_entity(new_node);
		ir_type   *tp  = get_entity_owner(ent);

		if (is_frame_type(tp)) {
			/* replace by the copied entity */
			ent = (ir_entity*)get_entity_link(ent);

			assert(is_entity(ent));
			assert(get_entity_owner(ent) == get_irg_frame_type(irg));
			set_Member_entity(new_node, ent);
		}
	}
}

/**
 * Post-walker: Set the predecessors of the copied nodes.
 * The copied nodes are set as link of their original nodes. The links of
 * "irn" predecessors are the predecessors of copied node.
 */
static void rewire(ir_node *irn, void *env)
{
	(void)env;
	irn_rewire_inputs(irn);
}

static ir_node *get_new_node(const ir_node *old_node)
{
	return (ir_node*)get_irn_link(old_node);
}

ir_graph *create_irg_copy(ir_graph *irg)
{
	ir_graph *res = alloc_graph();

	res->irg_pinned_state = irg->irg_pinned_state;

	/* clone the frame type here for safety */
	irp_reserve_resources(irp, IRP_RESOURCE_ENTITY_LINK);
	res->frame_type  = clone_frame_type(irg->frame_type);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	/* copy all nodes from the graph irg to the new graph res */
	irg_walk_anchors(irg, copy_all_nodes, rewire, res);

	/* copy the Anchor node */
	res->anchor = get_new_node(irg->anchor);

	/* -- The end block -- */
	set_irg_end_block (res, get_new_node(get_irg_end_block(irg)));
	set_irg_end       (res, get_new_node(get_irg_end(irg)));

	/* -- The start block -- */
	set_irg_start_block(res, get_new_node(get_irg_start_block(irg)));
	set_irg_no_mem     (res, get_new_node(get_irg_no_mem(irg)));
	set_irg_start      (res, get_new_node(get_irg_start(irg)));

	/* Proj results of start node */
	set_irg_initial_mem(res, get_new_node(get_irg_initial_mem(irg)));

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	irp_free_resources(irp, IRP_RESOURCE_ENTITY_LINK);

	return res;
}

void free_ir_graph(ir_graph *irg)
{
	assert(irg->kind == k_ir_graph);

	remove_irp_irg(irg);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);

	free_irg_outs(irg);
	del_identities(irg);
	if (irg->ent) {
		set_entity_irg(irg->ent, NULL);  /* not set in const code irg */
	}

	free_End(get_irg_end(irg));
	obstack_free(&irg->obst, NULL);
	if (irg->loc_descriptions)
		free(irg->loc_descriptions);
	irg->kind = k_BAD;
	free_graph(irg);
}

long get_irg_graph_nr(const ir_graph *irg)
{
#ifdef DEBUG_libfirm
	return irg->graph_nr;
#else
	return PTR_TO_INT(irg);
#endif
}

size_t get_irg_idx(const ir_graph *irg)
{
	return irg->index;
}

ir_node *(get_idx_irn)(const ir_graph *irg, unsigned idx)
{
	return get_idx_irn_(irg, idx);
}

ir_node *(get_irg_start_block)(const ir_graph *irg)
{
	return get_irg_start_block_(irg);
}

void (set_irg_start_block)(ir_graph *irg, ir_node *node)
{
	set_irg_start_block_(irg, node);
}

ir_node *(get_irg_start)(const ir_graph *irg)
{
	return get_irg_start_(irg);
}

void (set_irg_start)(ir_graph *irg, ir_node *node)
{
	set_irg_start_(irg, node);
}

ir_node *(get_irg_end_block)(const ir_graph *irg)
{
	return get_irg_end_block_(irg);
}

void (set_irg_end_block)(ir_graph *irg, ir_node *node)
{
	set_irg_end_block_(irg, node);
}

ir_node *(get_irg_end)(const ir_graph *irg)
{
	return get_irg_end_(irg);
}

void (set_irg_end)(ir_graph *irg, ir_node *node)
{
	set_irg_end_(irg, node);
}

ir_node *(get_irg_frame)(const ir_graph *irg)
{
	return get_irg_frame_(irg);
}

void (set_irg_frame)(ir_graph *irg, ir_node *node)
{
	set_irg_frame_(irg, node);
}

ir_node *(get_irg_initial_mem)(const ir_graph *irg)
{
	return get_irg_initial_mem_(irg);
}

void (set_irg_initial_mem)(ir_graph *irg, ir_node *node)
{
	set_irg_initial_mem_(irg, node);
}

ir_node *(get_irg_args)(const ir_graph *irg)
{
	return get_irg_args_(irg);
}

void (set_irg_args)(ir_graph *irg, ir_node *node)
{
	set_irg_args_(irg, node);
}

ir_node *(get_irg_no_mem)(const ir_graph *irg)
{
	return get_irg_no_mem_(irg);
}

void (set_irg_no_mem)(ir_graph *irg, ir_node *node)
{
	set_irg_no_mem_(irg, node);
}

ir_entity *(get_irg_entity)(const ir_graph *irg)
{
	return get_irg_entity_(irg);
}

void (set_irg_entity)(ir_graph *irg, ir_entity *ent)
{
	set_irg_entity_(irg, ent);
}

ir_type *(get_irg_frame_type)(ir_graph *irg)
{
	return get_irg_frame_type_(irg);
}

void (set_irg_frame_type)(ir_graph *irg, ir_type *ftp)
{
	set_irg_frame_type_(irg, ftp);
}

int get_irg_n_locs(ir_graph *irg)
{
	return irg->n_loc - 1;
}

int node_is_in_irgs_storage(const ir_graph *irg, const ir_node *n)
{
	/* Check whether the ir_node pointer is on the obstack.
	 * A more sophisticated check would test the "whole" ir_node. */
	for (struct _obstack_chunk const *p = irg->obst.chunk; p; p = p->prev) {
		if (((char *)p->contents <= (char *)n) && ((char *)n < (char *)p->limit))
			return 1;
	}

	return 0;
}

op_pin_state (get_irg_pinned)(const ir_graph *irg)
{
	return get_irg_pinned_(irg);
}

irg_callee_info_state (get_irg_callee_info_state)(const ir_graph *irg)
{
	return get_irg_callee_info_state_(irg);
}

void (set_irg_callee_info_state)(ir_graph *irg, irg_callee_info_state s)
{
	set_irg_callee_info_state_(irg, s);
}

void (set_irg_link)(ir_graph *irg, void *thing)
{
	set_irg_link_(irg, thing);
}

void *(get_irg_link)(const ir_graph *irg)
{
	return get_irg_link_(irg);
}

ir_visited_t (get_irg_visited)(const ir_graph *irg)
{
	return get_irg_visited_(irg);
}

/** maximum visited flag content of all ir_graph visited fields. */
static ir_visited_t max_irg_visited = 0;

void set_irg_visited(ir_graph *irg, ir_visited_t visited)
{
	irg->visited = visited;
	if (irg->visited > max_irg_visited) {
		max_irg_visited = irg->visited;
	}
}

void inc_irg_visited(ir_graph *irg)
{
	++irg->visited;
	if (irg->visited > max_irg_visited) {
		max_irg_visited = irg->visited;
	}
}

ir_visited_t get_max_irg_visited(void)
{
	return max_irg_visited;
}

void set_max_irg_visited(int val)
{
	max_irg_visited = val;
}

ir_visited_t inc_max_irg_visited(void)
{
#ifndef NDEBUG
	foreach_irp_irg(i, irg) {
		assert(max_irg_visited >= get_irg_visited(irg));
	}
#endif
	return ++max_irg_visited;
}

ir_visited_t (get_irg_block_visited)(const ir_graph *irg)
{
	return get_irg_block_visited_(irg);
}

void (set_irg_block_visited)(ir_graph *irg, ir_visited_t visited)
{
	set_irg_block_visited_(irg, visited);
}

void (inc_irg_block_visited)(ir_graph *irg)
{
  inc_irg_block_visited_(irg);
}

void set_irg_loc_description(ir_graph *irg, int n, void *description)
{
	assert(0 <= n && n < irg->n_loc);

	if (!irg->loc_descriptions)
		irg->loc_descriptions = XMALLOCNZ(void*, irg->n_loc);

	irg->loc_descriptions[n] = description;
}

void *get_irg_loc_description(ir_graph *irg, int n)
{
	assert(0 <= n && n < irg->n_loc);
	return irg->loc_descriptions ? irg->loc_descriptions[n] : NULL;
}

void (ir_reserve_resources)(ir_graph *irg, ir_resources_t resources)
{
	ir_reserve_resources_(irg, resources);
}

void (ir_free_resources)(ir_graph *irg, ir_resources_t resources)
{
	ir_free_resources_(irg, resources);
}

ir_resources_t (ir_resources_reserved)(const ir_graph *irg)
{
	return ir_resources_reserved_(irg);
}

unsigned get_irg_last_idx(const ir_graph *irg)
{
	return irg->last_node_idx;
}

void add_irg_constraints(ir_graph *irg, ir_graph_constraints_t constraints)
{
	irg->constraints |= constraints;
}

void clear_irg_constraints(ir_graph *irg, ir_graph_constraints_t constraints)
{
	irg->constraints &= ~constraints;
}

int (irg_is_constrained)(const ir_graph *irg, ir_graph_constraints_t constraints)
{
	return irg_is_constrained_(irg, constraints);
}

void (add_irg_properties)(ir_graph *irg, ir_graph_properties_t props)
{
	add_irg_properties_(irg, props);
}

void (clear_irg_properties)(ir_graph *irg, ir_graph_properties_t props)
{
	clear_irg_properties_(irg, props);
}

int (irg_has_properties)(const ir_graph *irg, ir_graph_properties_t props)
{
	return irg_has_properties_(irg, props);
}

typedef void (*assure_property_func)(ir_graph *irg);

void assure_irg_properties(ir_graph *irg, ir_graph_properties_t props)
{
	static struct {
		ir_graph_properties_t property;
		assure_property_func  func;
	} property_functions[] = {
		{ IR_GRAPH_PROPERTY_ONE_RETURN,               normalize_one_return },
		{ IR_GRAPH_PROPERTY_MANY_RETURNS,             normalize_n_returns },
		{ IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES,        remove_critical_cf_edges },
		{ IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE,      remove_unreachable_code },
		{ IR_GRAPH_PROPERTY_NO_BADS,                  remove_bads },
		{ IR_GRAPH_PROPERTY_NO_TUPLES,                remove_tuples },
		{ IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE,     compute_doms },
		{ IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE, compute_postdoms },
		{ IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES,     assure_edges },
		{ IR_GRAPH_PROPERTY_CONSISTENT_OUTS,          assure_irg_outs },
		{ IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO,      assure_loopinfo },
		{ IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE,  assure_irg_entity_usage_computed },
		{ IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE_FRONTIERS, ir_compute_dominance_frontiers },
	};
	for (size_t i = 0; i < ARRAY_SIZE(property_functions); ++i) {
		ir_graph_properties_t missing = props & ~irg->properties;
		if (missing & property_functions[i].property)
			property_functions[i].func(irg);
	}
	assert((props & ~irg->properties) == IR_GRAPH_PROPERTIES_NONE);
}

void confirm_irg_properties(ir_graph *irg, ir_graph_properties_t props)
{
	clear_irg_properties(irg, ~props);
	if (!(props & IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES))
		edges_deactivate(irg);
	if (!(props & IR_GRAPH_PROPERTY_CONSISTENT_OUTS)
	    && (irg->properties & IR_GRAPH_PROPERTY_CONSISTENT_OUTS))
	    free_irg_outs(irg);
	if (!(props & IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE))
		set_irp_globals_entity_usage_state(ir_entity_usage_not_computed);
	if (!(props & IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE_FRONTIERS))
		ir_free_dominance_frontiers(irg);
}
