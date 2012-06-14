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
 * @brief    Entry point to the representation of procedure code.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#include "config.h"

#include <string.h>
#include <stddef.h>

#include "xmalloc.h"
#include "ircons_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "irflag_t.h"
#include "array.h"
#include "irgmod.h"
#include "irouts.h"
#include "irhooks.h"
#include "irtools.h"
#include "util.h"
#include "irgwalk.h"
#include "irbackedge_t.h"
#include "iredges_t.h"
#include "type_t.h"
#include "irmemory.h"

#define INITIAL_IDX_IRN_MAP_SIZE 1024

/**
 * Indicates, whether additional data can be registered to graphs.
 * If set to 1, this is not possible anymore.
 */
static int forbid_new_data = 0;

/**
 * The amount of additional space for custom data to be allocated upon
 * creating a new graph.
 */
static size_t additional_graph_data_size = 0;

ir_graph *current_ir_graph;
ir_graph *get_current_ir_graph(void)
{
	return current_ir_graph;
}

void set_current_ir_graph(ir_graph *graph)
{
	current_ir_graph = graph;
}

/** contains the suffix for frame type names */
static ident *frame_type_suffix = NULL;

void firm_init_irgraph(void)
{
	frame_type_suffix = new_id_from_str(FRAME_TP_SUFFIX);
	forbid_new_data   = 1;
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
	ir_graph *res;
	size_t   size = sizeof(ir_graph) + additional_graph_data_size;
	char     *ptr = XMALLOCNZ(char, size);

	res = (ir_graph *)(ptr + additional_graph_data_size);
	res->kind = k_ir_graph;

	/* initialize the idx->node map. */
	res->idx_irn_map = NEW_ARR_F(ir_node *, INITIAL_IDX_IRN_MAP_SIZE);
	memset(res->idx_irn_map, 0, INITIAL_IDX_IRN_MAP_SIZE * sizeof(res->idx_irn_map[0]));

	return res;
}

/**
 * Frees an allocated IR graph
 */
static void free_graph(ir_graph *irg)
{
	char           *ptr = (char *)irg;
	ir_edge_kind_t  i;

	for (i = EDGE_KIND_FIRST; i < EDGE_KIND_LAST; ++i)
		edges_deactivate_kind(irg, i);
	DEL_ARR_F(irg->idx_irn_map);
	free(ptr - additional_graph_data_size);
}

void irg_set_nloc(ir_graph *res, int n_loc)
{
	assert(res->phase_state == phase_building);

	res->n_loc = n_loc + 1;     /* number of local variables that are never
	                               dereferenced in this graph plus one for
	                               the store. This is not the number of
	                               parameters to the procedure!  */

	if (res->loc_descriptions) {
		xfree(res->loc_descriptions);
		res->loc_descriptions = NULL;
	}
}

ir_graph *new_r_ir_graph(ir_entity *ent, int n_loc)
{
	ir_graph *res;
	ir_node  *first_block;
	ir_node  *start, *start_block, *initial_mem, *projX;

	res = alloc_graph();

	/* inform statistics here, as blocks will be already build on this graph */
	hook_new_graph(res, ent);

	/*-- initialized for each graph. --*/
	res->kind = k_ir_graph;
	res->obst = XMALLOC(struct obstack);
	obstack_init(res->obst);

	res->phase_state = phase_building;
	irg_set_nloc(res, n_loc);

	/* descriptions will be allocated on demand */
	res->loc_descriptions = NULL;

	res->visited       = 0; /* visited flag, for the ir walker */
	res->block_visited = 0; /* visited flag, for the 'block'-walker */

	res->last_node_idx = 0;

	new_identities(res);
	res->outs = NULL;

	res->inline_property       = irg_inline_any;
	res->additional_properties = mtp_property_inherited;  /* inherited from type */

	res->irg_pinned_state    = op_pin_state_pinned;
	res->typeinfo_state      = ir_typeinfo_none;
	set_irp_typeinfo_inconsistent();           /* there is a new graph with typeinfo_none. */
	res->callee_info_state   = irg_callee_info_none;
	res->class_cast_state    = ir_class_casts_transitive;
	res->fp_model            = fp_model_precise;
	res->mem_disambig_opt    = aa_opt_inherited;

	/*-- Type information for the procedure of the graph --*/
	res->ent = ent;
	set_entity_irg(ent, res);

	/*--  a class type so that it can contain "inner" methods as in Pascal. --*/
	res->frame_type = new_type_frame();

	/* the Anchor node must be created first */
	res->anchor = new_r_Anchor(res);

	/*-- Nodes needed in every graph --*/
	set_irg_end_block(res, new_r_immBlock(res));
	set_irg_end(res, new_r_End(res, 0, NULL));

	start_block = new_r_Block_noopt(res, 0, NULL);
	set_irg_start_block(res, start_block);
	set_irg_no_mem     (res, new_r_NoMem(res));
	start = new_r_Start(res);
	set_irg_start      (res, start);

	/* Proj results of start node */
	projX                   = new_r_Proj(start, mode_X, pn_Start_X_initial_exec);
	set_irg_initial_exec    (res, projX);
	set_irg_frame           (res, new_r_Proj(start, mode_P_data, pn_Start_P_frame_base));
	set_irg_args            (res, new_r_Proj(start, mode_T,      pn_Start_T_args));
	initial_mem             = new_r_Proj(start, mode_M, pn_Start_M);
	set_irg_initial_mem(res, initial_mem);

	res->index       = get_irp_new_irg_idx();
#ifdef DEBUG_libfirm
	res->graph_nr    = get_irp_new_node_nr();
#endif

	set_r_cur_block(res, start_block);
	set_r_store(res, initial_mem);

	/*-- Make a block to start with --*/
	first_block = new_r_Block(res, 1, &projX);
	set_r_cur_block(res, first_block);

	res->method_execution_frequency = -1.0;
	res->estimated_node_count       = 0;

	return res;
}

ir_graph *new_ir_graph(ir_entity *ent, int n_loc)
{
	ir_graph *res = new_r_ir_graph(ent, n_loc);
	add_irp_irg(res);          /* remember this graph global. */
	return res;
}

ir_graph *new_const_code_irg(void)
{
	ir_graph *res = alloc_graph();
	ir_node  *body_block;
	ir_node  *end;
	ir_node  *end_block;
	ir_node  *no_mem;
	ir_node  *projX;
	ir_node  *start_block;
	ir_node  *start;

	/* inform statistics here, as blocks will be already build on this graph */
	hook_new_graph(res, NULL);

	res->n_loc         = 1; /* Only the memory. */
	res->visited       = 0; /* visited flag, for the ir walker */
	res->block_visited = 0; /* visited flag, for the 'block'-walker */
	res->obst          = XMALLOC(struct obstack);
	obstack_init(res->obst);

	res->last_node_idx = 0;

	res->phase_state      = phase_building;
	res->irg_pinned_state = op_pin_state_pinned;
	res->fp_model         = fp_model_precise;

	/* value table for global value numbering for optimizing use in iropt.c */
	new_identities(res);
	res->ent         = NULL;
	res->frame_type  = NULL;

	/* the Anchor node must be created first */
	res->anchor = new_r_Anchor(res);

	/* -- The end block -- */
	end_block = new_r_Block_noopt(res, 0, NULL);
	set_irg_end_block(res, end_block);
	end = new_r_End(res, 0, NULL);
	set_irg_end(res, end);

	/* -- The start block -- */
	start_block = new_r_Block_noopt(res, 0, NULL);
	set_irg_start_block(res, start_block);
	no_mem = new_r_NoMem(res);
	set_irg_no_mem(res, no_mem);
	start = new_r_Start(res);
	set_irg_start(res, start);

	/* Proj results of start node */
	set_irg_initial_mem(res, new_r_Proj(start, mode_M, pn_Start_M));
	projX = new_r_Proj(start, mode_X, pn_Start_X_initial_exec);

	body_block = new_r_Block(res, 1, &projX);

	set_r_cur_block(res, body_block);

	/* Set the visited flag high enough that the blocks will never be visited. */
	set_irn_visited(body_block, -1);
	set_Block_block_visited(body_block, -1);
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
	if (is_Sel(new_node)) {
		ir_entity *ent = get_Sel_entity(new_node);
		ir_type   *tp  = get_entity_owner(ent);

		if (is_frame_type(tp)) {
			/* replace by the copied entity */
			ent = (ir_entity*)get_entity_link(ent);

			assert(is_entity(ent));
			assert(get_entity_owner(ent) == get_irg_frame_type(irg));
			set_Sel_entity(new_node, ent);
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
	(void) env;
	irn_rewire_inputs(irn);
}

static ir_node *get_new_node(const ir_node *old_node)
{
	return (ir_node*) get_irn_link(old_node);
}

ir_graph *create_irg_copy(ir_graph *irg)
{
	ir_graph *res;

	res = alloc_graph();

	res->n_loc = 0;
	res->visited = 0;       /* visited flag, for the ir walker */
	res->block_visited = 0; /* visited flag, for the 'block'-walker */
	res->obst       = XMALLOC(struct obstack);
	obstack_init(res->obst);

	res->last_node_idx = 0;

	res->phase_state      = irg->phase_state;
	res->irg_pinned_state = irg->irg_pinned_state;
	res->fp_model         = irg->fp_model;

	new_identities(res);

	/* clone the frame type here for safety */
	irp_reserve_resources(irp, IRP_RESOURCE_ENTITY_LINK);
	res->frame_type  = clone_frame_type(irg->frame_type);

	res->phase_state = irg->phase_state;

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

	/* Copy the node count estimation. Would be strange if this
	   is different from the original one. */
	res->estimated_node_count = irg->estimated_node_count;

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	irp_free_resources(irp, IRP_RESOURCE_ENTITY_LINK);

	return res;
}

void free_ir_graph(ir_graph *irg)
{
	assert(is_ir_graph(irg));

	edges_deactivate(irg);

	hook_free_graph(irg);
	free_irg_outs(irg);
	del_identities(irg);
	if (irg->ent) {
		set_entity_irg(irg->ent, NULL);  /* not set in const code irg */
	}

	free_End(get_irg_end(irg));
	obstack_free(irg->obst, NULL);
	free(irg->obst);
	if (irg->loc_descriptions)
		free(irg->loc_descriptions);
	irg->kind = k_BAD;
	free_graph(irg);
}

int (is_ir_graph)(const void *thing)
{
	return is_ir_graph_(thing);
}

#ifdef DEBUG_libfirm
long get_irg_graph_nr(const ir_graph *irg)
{
	return irg->graph_nr;
}
#else
long get_irg_graph_nr(const ir_graph *irg)
{
	return PTR_TO_INT(irg);
}
#endif

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

ir_node *(get_irg_initial_exec)(const ir_graph *irg)
{
	return get_irg_initial_exec_(irg);
}

void (set_irg_initial_exec)(ir_graph *irg, ir_node *node)
{
	set_irg_initial_exec_(irg, node);
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

struct obstack *(get_irg_obstack)(const ir_graph *irg)
{
	return get_irg_obstack_(irg);
}

int node_is_in_irgs_storage(const ir_graph *irg, const ir_node *n)
{
	struct _obstack_chunk *p;

	/*
	 * checks weather the ir_node pointer is on the obstack.
	 * A more sophisticated check would test the "whole" ir_node
	 */
	for (p = irg->obst->chunk; p; p = p->prev) {
		if (((char *)p->contents <= (char *)n) && ((char *)n < (char *)p->limit))
			return 1;
	}

	return 0;
}

irg_phase_state (get_irg_phase_state)(const ir_graph *irg)
{
	return get_irg_phase_state_(irg);
}

void (set_irg_phase_state)(ir_graph *irg, irg_phase_state state)
{
	set_irg_phase_state_(irg, state);
}

op_pin_state (get_irg_pinned)(const ir_graph *irg)
{
	return get_irg_pinned_(irg);
}

void (set_irg_pinned)(ir_graph *irg, op_pin_state p)
{
	set_irg_pinned_(irg, p);
}

irg_callee_info_state (get_irg_callee_info_state)(const ir_graph *irg)
{
	return get_irg_callee_info_state_(irg);
}

void (set_irg_callee_info_state)(ir_graph *irg, irg_callee_info_state s)
{
	set_irg_callee_info_state_(irg, s);
}

irg_inline_property (get_irg_inline_property)(const ir_graph *irg)
{
	return get_irg_inline_property_(irg);
}

void (set_irg_inline_property)(ir_graph *irg, irg_inline_property s)
{
	set_irg_inline_property_(irg, s);
}

mtp_additional_properties (get_irg_additional_properties)(const ir_graph *irg)
{
	return get_irg_additional_properties_(irg);
}

void (set_irg_additional_properties)(ir_graph *irg, mtp_additional_properties property_mask)
{
	set_irg_additional_properties_(irg, property_mask);
}

void (add_irg_additional_properties)(ir_graph *irg, mtp_additional_properties flag)
{
	add_irg_additional_properties_(irg, flag);
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
	size_t i;
	for (i = 0; i < get_irp_n_irgs(); i++)
		assert(max_irg_visited >= get_irg_visited(get_irp_irg(i)));
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

unsigned (get_irg_fp_model)(const ir_graph *irg)
{
	return get_irg_fp_model_(irg);
}

void set_irg_fp_model(ir_graph *irg, unsigned model)
{
	irg->fp_model = model;
}

void set_irg_loc_description(ir_graph *irg, int n, void *description)
{
	assert(0 <= n && n < irg->n_loc);

	if (! irg->loc_descriptions)
		irg->loc_descriptions = XMALLOCNZ(void*, irg->n_loc);

	irg->loc_descriptions[n] = description;
}

void *get_irg_loc_description(ir_graph *irg, int n)
{
	assert(0 <= n && n < irg->n_loc);
	return irg->loc_descriptions ? irg->loc_descriptions[n] : NULL;
}

#ifndef NDEBUG
void ir_reserve_resources(ir_graph *irg, ir_resources_t resources)
{
	assert((irg->reserved_resources & resources) == 0);
	irg->reserved_resources |= resources;
}

void ir_free_resources(ir_graph *irg, ir_resources_t resources)
{
	assert((irg->reserved_resources & resources) == resources);
	irg->reserved_resources &= ~resources;
}

ir_resources_t ir_resources_reserved(const ir_graph *irg)
{
	return irg->reserved_resources;
}
#endif

unsigned (get_irg_estimated_node_cnt)(const ir_graph *irg)
{
	return get_irg_estimated_node_cnt_(irg);
}

unsigned get_irg_last_idx(const ir_graph *irg)
{
	return irg->last_node_idx;
}

size_t register_additional_graph_data(size_t size)
{
	assert(!forbid_new_data && "Too late to register additional node data");

	if (forbid_new_data)
		return 0;

	return additional_graph_data_size += size;
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
