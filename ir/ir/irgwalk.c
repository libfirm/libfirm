/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   Functions for traversing ir graphs
 * @author  Boris Boesler, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 * @summary
 *  traverse an ir graph
 *  - execute the pre function before recursion
 *  - execute the post function after recursion
 */
#include "config.h"

#include <stdlib.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "ircgcons.h"
#include "entity_t.h"

#include "error.h"
#include "pset_new.h"
#include "array.h"

#ifdef INTERPROCEDURAL_VIEW
/**
 * Walk over an interprocedural graph (callgraph).
 * Visits only graphs in irg_set.
 */
static void irg_walk_cg(ir_node * node, ir_visited_t visited,
                        pset_new_t *irg_set, irg_walk_func *pre,
                        irg_walk_func *post, void * env)
{
	int i;
	ir_graph * rem = current_ir_graph;
	ir_node * pred;

	assert(node && node->kind == k_ir_node);
	if (get_irn_visited(node) >= visited) return;

	set_irn_visited(node, visited);

	if (pre) pre(node, env);

	pred = skip_Proj(node);
	if (is_CallBegin(pred)            ||
	    get_irn_op(pred) == op_EndReg ||
	    get_irn_op(pred) == op_EndExcept) {
		current_ir_graph = get_irn_irg(pred);
	}

	if (is_no_Block(node)) { /* not block */
		irg_walk_cg(get_nodes_block(node), visited, irg_set, pre, post, env);
	}

	if (is_Block(node)) { /* block */
		for (i = get_irn_arity(node) - 1; i >= 0; --i) {
			ir_node * exec = get_irn_n(node, i);
			ir_node * pred = skip_Proj(exec);
			if ((
			      !is_CallBegin(pred)           &&
			      get_irn_op(pred) != op_EndReg &&
			      get_irn_op(pred) != op_EndExcept
			    ) || pset_new_contains(irg_set, get_irn_irg(pred))) {
				irg_walk_cg(exec, visited, irg_set, pre, post, env);
			}
		}
	} else if (is_Filter(node)) { /* filter */
		for (i = get_irn_arity(node) - 1; i >= 0; --i) {
			ir_node * pred = get_irn_n(node, i);
			if (is_Unknown(pred) || is_Bad(pred)) {
				irg_walk_cg(pred, visited, irg_set, pre, post, env);
			} else {
				ir_node * exec;
				exec = skip_Proj(get_Block_cfgpred(get_nodes_block(node), i));

				if (is_Bad(exec)) {
					continue;
				}

				assert(is_CallBegin(exec)            ||
				       get_irn_op(exec) == op_EndReg ||
				       get_irn_op(exec) == op_EndExcept);
				if (pset_new_contains(irg_set, get_irn_irg(exec))) {
					current_ir_graph = get_irn_irg(exec);
					irg_walk_cg(pred, visited, irg_set, pre, post, env);
					current_ir_graph = rem;
				}
			}
		}
	} else {                      /* everything else */
		for (i = get_irn_arity(node) - 1; i >= 0; --i) {
			irg_walk_cg(get_irn_n(node, i), visited, irg_set, pre, post, env);
		}
	}

	if (post) post(node, env);

	current_ir_graph = rem;
}
#endif

/**
 * Insert all ir_graphs in irg_set, that are (transitive) reachable.
 */
static void collect_irgs(ir_node * node, pset_new_t *irg_set) {
	if (is_Call(node)) {
		int i;
		for (i = get_Call_n_callees(node) - 1; i >= 0; --i) {
			ir_entity * ent = get_Call_callee(node, i);
			ir_graph * irg = get_entity_irg(ent);
			if (irg && !pset_new_contains(irg_set, irg)) {
				pset_new_insert(irg_set, irg);
				irg_walk_graph(irg, (irg_walk_func *) collect_irgs, NULL, irg_set);
			}
		}
	}
}

/**
 * specialized version of irg_walk_2, called if only pre callback exists
 *
 * @return number of visited nodes
 */
static unsigned
irg_walk_2_pre(ir_node *node, irg_walk_func *pre, void * env) {
	int i;
	unsigned cnt = 1;
	ir_graph *irg = current_ir_graph;

	set_irn_visited(node, irg->visited);

	pre(node, env);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
			cnt += irg_walk_2_pre(pred, pre, env);
	}
	for (i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);
		if (pred->visited < irg->visited)
			cnt += irg_walk_2_pre(pred, pre, env);
	}
	return cnt;
}

/**
 * specialized version of irg_walk_2, called if only post callback exists
 *
 * @return number of visited nodes
 */
static unsigned
irg_walk_2_post(ir_node *node, irg_walk_func *post, void * env) {
	int i;
	unsigned cnt = 1;
	ir_graph *irg = current_ir_graph;

	set_irn_visited(node, irg->visited);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
			cnt += irg_walk_2_post(pred, post, env);
	}
	for (i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);
		if (pred->visited < irg->visited)
			cnt += irg_walk_2_post(pred, post, env);
	}

	post(node, env);

	return cnt;
}

/**
 * specialized version of irg_walk_2, called if pre and post callbacks exist
 *
 * @return number of visited nodes
 */
static unsigned
irg_walk_2_both(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void * env) {
	int i;
	unsigned cnt = 1;
	ir_graph *irg = current_ir_graph;

	set_irn_visited(node, irg->visited);

	pre(node, env);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
			cnt += irg_walk_2_both(pred, pre, post, env);
	}
	for (i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);
		if (pred->visited < irg->visited)
			cnt += irg_walk_2_both(pred, pre, post, env);
	}

	post(node, env);

	return cnt;
}

/**
 * Intraprozedural graph walker.
 *
 * @return number of visited nodes
 */
static unsigned
irg_walk_2(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void * env)
{
	if (node->visited < current_ir_graph->visited) {
		if      (!post) return irg_walk_2_pre (node, pre, env);
		else if (!pre)  return irg_walk_2_post(node, post, env);
		else            return irg_walk_2_both(node, pre, post, env);
	}
	return 0;
}

/* a counter */
static unsigned nodes_touched = 0;

/*
 * generic graph walker
 */
void irg_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
	assert(is_ir_node(node));

#ifdef INTERPROCEDURAL_VIEW
	if (get_interprocedural_view()) {
		pset_new_t           irg_set;
		pset_new_iterator_t  iter;
		ir_visited_t         visited;
		ir_graph            *irg;
		assert(get_irp_ip_view_state() == ip_view_valid);

		pset_new_init(&irg_set);
		set_interprocedural_view(0);
		pset_new_insert(&irg_set, current_ir_graph);
		irg_walk(node, (irg_walk_func *) collect_irgs, NULL, &irg_set);
		set_interprocedural_view(1);
		visited = get_max_irg_visited() + 1;

		foreach_pset_new(&irg_set, irg, iter) {
			set_irg_visited(irg, visited);
		}
		irg_walk_cg(node, visited, &irg_set, pre, post, env);
		pset_new_destroy(&irg_set);
	} else {
#endif
		ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);
		inc_irg_visited(current_ir_graph);
		nodes_touched = irg_walk_2(node, pre, post, env);
		ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);
#ifdef INTERPROCEDURAL_VIEW
	}
#endif
}

/*
 * walk over a graph
 */
void irg_walk_graph(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post, void *env) {
	ir_graph * rem = current_ir_graph;

	hook_irg_walk(irg, (generic_func *)pre, (generic_func *)post);
	current_ir_graph = irg;
	irg_walk(get_irg_end(irg), pre, post, env);
	irg->estimated_node_count = nodes_touched;
	current_ir_graph = rem;
}

/* Executes irg_walk(end, pre, post, env) for all irgraphs in irprog.
   Sets current_ir_graph properly for each walk.  Conserves current
   current_ir_graph. */
void all_irg_walk(irg_walk_func *pre, irg_walk_func *post, void *env) {
	int i, n;
	ir_graph *irg;

	for (i = 0, n = get_irp_n_irgs(); i < n; i++) {
		irg = get_irp_irg(i);
		irg_walk_graph(irg, pre, post, env);
	}
}

/***************************************************************************/

/**
 * specialized version of irg_walk_in_or_dep_2, called if only pre callback exists
 *
 * @return number of visited nodes
 */
static unsigned
irg_walk_in_or_dep_2_pre(ir_node *node, irg_walk_func *pre, void *env) {
	int i;
	unsigned cnt = 1;
	ir_graph *irg = current_ir_graph;

	set_irn_visited(node, irg->visited);

	pre(node, env);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
			cnt += irg_walk_in_or_dep_2_pre(pred, pre, env);
	}
	for (i = get_irn_ins_or_deps(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_in_or_dep(node, i);
		if (pred->visited < irg->visited)
			cnt += irg_walk_in_or_dep_2_pre(pred, pre, env);
	}
	return cnt;
}

/**
 * specialized version of irg_walk_in_or_dep_2, called if only post callback exists
 *
 * @return number of visited nodes
 */
static unsigned
irg_walk_in_or_dep_2_post(ir_node *node, irg_walk_func *post, void *env) {
	int i;
	unsigned cnt = 1;
	ir_graph *irg = current_ir_graph;

	set_irn_visited(node, irg->visited);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
			cnt += irg_walk_in_or_dep_2_post(pred, post, env);
	}
	for (i = get_irn_ins_or_deps(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_in_or_dep(node, i);
		if (pred->visited < irg->visited)
			cnt += irg_walk_in_or_dep_2_post(pred, post, env);
	}

	post(node, env);

	return cnt;
}

/**
 * specialized version of irg_walk_in_or_dep_2, called if pre and post callbacks exist
 *
 * @return number of visited nodes
 */
static unsigned
irg_walk_in_or_dep_2_both(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env) {
	int i;
	unsigned cnt = 1;
	ir_graph *irg = current_ir_graph;

	set_irn_visited(node, irg->visited);

	pre(node, env);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
			cnt += irg_walk_in_or_dep_2_both(pred, pre, post, env);
	}
	for (i = get_irn_ins_or_deps(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_in_or_dep(node, i);
		if (pred->visited < irg->visited)
			cnt += irg_walk_in_or_dep_2_both(pred, pre, post, env);
	}

	post(node, env);

	return cnt;
}

/**
 * Intraprozedural graph walker. Follows dependency edges as well.
 *
 * @return number of visited nodes
 */
static unsigned
irg_walk_in_or_dep_2(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
	if (node->visited < current_ir_graph->visited) {
		if      (! post) return irg_walk_in_or_dep_2_pre (node, pre, env);
		else if (! pre)  return irg_walk_in_or_dep_2_post(node, post, env);
		else             return irg_walk_in_or_dep_2_both(node, pre, post, env);
	}
	return 0;
}

/*
 * Generic graph walker. Follows dependency edges as well.
 */
void irg_walk_in_or_dep(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
	assert(is_ir_node(node));

	if (get_interprocedural_view()) {
		assert(0 && "This is not yet implemented.");
	} else {
		ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);
		inc_irg_visited(current_ir_graph);
		nodes_touched = irg_walk_in_or_dep_2(node, pre, post, env);
		ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);
	}
}

/*
 * Walk over a graph. Follow all edges (including dependencies)
 */
void irg_walk_in_or_dep_graph(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post, void *env) {
	ir_graph * rem = current_ir_graph;

	hook_irg_walk(irg, (generic_func *)pre, (generic_func *)post);
	current_ir_graph = irg;
	irg_walk_in_or_dep(get_irg_end(irg), pre, post, env);
	irg->estimated_node_count = nodes_touched;
	current_ir_graph = rem;
}

/***************************************************************************/

/**
 * Returns current_ir_graph and sets it to the irg of predecessor index
 * of node n.
 */
static inline ir_graph *
switch_irg(ir_node *n, int index) {
	ir_graph *old_current = current_ir_graph;

	if (get_interprocedural_view()) {
		/* Only Filter and Block nodes can have predecessors in other graphs. */
		if (is_Filter(n))
			n = get_nodes_block(n);
		if (is_Block(n)) {
			ir_node *cfop = skip_Proj(get_Block_cfgpred(n, index));
			if (is_ip_cfop(cfop)) {
				current_ir_graph = get_irn_irg(cfop);
			}
		}
	}

	return old_current;
}

#ifdef INTERPROCEDURAL_VIEW
static void
cg_walk_2(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void * env)
{
	int i;
	ir_graph *rem = NULL;
	assert(node);

	if (get_irn_visited(node) < get_irg_visited(current_ir_graph)) {
		set_irn_visited(node, get_irg_visited(current_ir_graph));

		if (pre) pre(node, env);

		if (is_no_Block(node))
			cg_walk_2(get_nodes_block(node), pre, post, env);
		for (i = get_irn_arity(node) - 1; i >= 0; --i) {
			rem = switch_irg(node, i);  /* @@@ AS: Is this wrong? We do have to
						    switch to the irg of the predecessor, don't we? */
			cg_walk_2(get_irn_n(node, i), pre, post, env);
			current_ir_graph = rem;
		}

		if (post) post(node, env);
	}
}

/* Walks all irgs in interprocedural view.  Visits each node only once. */
void cg_walk(irg_walk_func *pre, irg_walk_func *post, void *env) {
	int i;
	ir_graph *rem = current_ir_graph;
	int rem_view = get_interprocedural_view();

	set_interprocedural_view(1);

	inc_max_irg_visited();
	/* Fix all irg_visited flags */
	for (i = 0; i < get_irp_n_irgs(); i++)
		set_irg_visited(get_irp_irg(i), get_max_irg_visited());

	/* Walk starting at unreachable procedures. Only these
	 * have End blocks visible in interprocedural view. */
	for (i = 0; i < get_irp_n_irgs(); i++) {
		ir_node *sb;
		current_ir_graph = get_irp_irg(i);

		sb = get_irg_start_block(current_ir_graph);

		if ((get_Block_n_cfgpreds(sb) > 1) ||
			(get_nodes_block(get_Block_cfgpred(sb, 0)) != sb)) continue;

		cg_walk_2(get_irg_end(current_ir_graph), pre, post, env);
	}

	/* Check whether we walked all procedures: there could be procedures
	   with cyclic calls but no call from the outside. */
	for (i = 0; i < get_irp_n_irgs(); i++) {
		ir_node *sb;
		current_ir_graph = get_irp_irg(i);

		/* Test start block: if inner procedure end and end block are not
		* visible and therefore not marked. */
		sb = get_irg_start_block(current_ir_graph);
		if (get_irn_visited(sb) < get_irg_visited(current_ir_graph)) {
			cg_walk_2(sb, pre, post, env);
		}
	}

	/* Walk all endless loops in inner procedures.
	 * We recognize an inner procedure if the End node is not visited. */
	for (i = 0; i < get_irp_n_irgs(); i++) {
		ir_node *e;
		current_ir_graph = get_irp_irg(i);
		e = get_irg_end(current_ir_graph);
		if (get_irn_visited(e) < get_irg_visited(current_ir_graph)) {
			int j;
			/* Don't visit the End node. */
			for (j = 0; j < get_End_n_keepalives(e); j++)
				cg_walk_2(get_End_keepalive(e, j), pre, post, env);
		}
	}

	set_interprocedural_view(rem_view);
	current_ir_graph = rem;
}
#endif


/***************************************************************************/

/* Walks back from n until it finds a real cf op. */
static ir_node *get_cf_op(ir_node *n) {
	while (!is_cfop(n) && !is_fragile_op(n) && !is_Bad(n)) {
		n = skip_Id(n);
		n = skip_Tuple(n);
		n = skip_Proj(n);
	}
	return n;
}

static void irg_block_walk_2(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
	int i;

	if (!Block_block_visited(node)) {
		mark_Block_block_visited(node);

		if (pre) pre(node, env);

		for(i = get_Block_n_cfgpreds(node) - 1; i >= 0; --i) {
			/* find the corresponding predecessor block. */
			ir_node *pred = get_cf_op(get_Block_cfgpred(node, i));
			pred = get_nodes_block(pred);
			if (get_irn_opcode(pred) == iro_Block) {
				/* recursion */
				irg_block_walk_2(pred, pre, post, env);
			}
			else {
				assert(get_irn_opcode(pred) == iro_Bad);
			}
		}

		if (post) post(node, env);
	}
}


/* walks only over Block nodes in the graph.  Has it's own visited
   flag, so that it can be interleaved with the other walker.         */
void irg_block_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
	ir_graph *irg = current_ir_graph;
	ir_node *block, *pred;
	int i;

	hook_irg_block_walk(irg, node, (generic_func *)pre, (generic_func *)post);

	assert(node);
	assert(!get_interprocedural_view());   /* interprocedural_view not implemented, because it
	                                        * interleaves with irg_walk */
	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	inc_irg_block_visited(irg);
	block = is_Block(node) ? node : get_nodes_block(node);
	assert(is_Block(block));
	irg_block_walk_2(block, pre, post, env);

	/* keepalive: the endless loops ... */
	if (is_End(node)) {
		int arity = get_irn_arity(node);
		for (i = 0; i < arity; i++) {
			pred = get_irn_n(node, i);
			if (!is_Block(pred)) {
				pred = get_nodes_block(pred);
				if (!is_Block(pred)) {
					/* if rare cases a kept node might have a bad block input */
					continue;
				}
			}
			/* Sometimes the blocks died, but are still reachable through kept nodes.
			 * Make sure the algorithms that try to remove these reach them. */
			irg_block_walk_2(pred, pre, post, env);
		}
	}

	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED);
}

/*
 * walk over a graph block wise
 */
void irg_block_walk_graph(ir_graph *irg, irg_walk_func *pre,
              irg_walk_func *post, void *env) {
	ir_graph * rem = current_ir_graph;
	current_ir_graph = irg;
	irg_block_walk(get_irg_end(irg), pre, post, env);
	current_ir_graph = rem;
}

/*
 * Additionally walk over all anchors. Do NOT increase the visit flag.
 */
void irg_walk_anchors(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post, void *env) {
	ir_graph * rem = current_ir_graph;
	current_ir_graph = irg;

	inc_irg_visited(irg);
	irg_walk_2(irg->anchor, pre, post, env);

	current_ir_graph = rem;
}

/********************************************************************/

typedef struct walk_env {
	irg_walk_func *pre;
	irg_walk_func *post;
	void *env;
} walk_env;

static void walk_initializer(ir_initializer_t *initializer, walk_env *env)
{
	switch(initializer->kind) {
    case IR_INITIALIZER_CONST:
    	irg_walk(initializer->consti.value, env->pre, env->post, env->env);
        return;
    case IR_INITIALIZER_TARVAL:
    case IR_INITIALIZER_NULL:
        return;

    case IR_INITIALIZER_COMPOUND: {
        size_t i;
        for(i = 0; i < initializer->compound.n_initializers; ++i) {
            ir_initializer_t *subinitializer
                = initializer->compound.initializers[i];
            walk_initializer(subinitializer, env);
        }
        return;
    }
	}
	panic("invalid initializer found");
}

/**
 * Walk to all constant expressions in this entity.
 */
static void walk_entity(ir_entity *ent, void *env)
{
	walk_env *my_env = (walk_env *)env;

	if (get_entity_variability(ent) != variability_uninitialized) {
		if (ent->has_initializer) {
			walk_initializer(ent->attr.initializer, my_env);
		} else if (is_atomic_entity(ent)) {
			irg_walk(get_atomic_ent_value(ent), my_env->pre, my_env->post, my_env->env);
		} else {
			int i, n_vals = get_compound_ent_n_values(ent);

			for (i = 0; i < n_vals; i++)
				irg_walk(get_compound_ent_value(ent, i), my_env->pre, my_env->post, my_env->env);
		}
	}
}

/* Walks over all code in const_code_irg. */
void walk_const_code(irg_walk_func *pre, irg_walk_func *post, void *env) {
	int i, j, n_types;
	walk_env my_env;

	ir_graph *rem = current_ir_graph;
	current_ir_graph = get_const_code_irg();
	inc_irg_visited(current_ir_graph);

	my_env.pre = pre;
	my_env.post = post;
	my_env.env = env;

	/* Walk all types that can contain constant entities.  */
	walk_types_entities(get_glob_type(), &walk_entity, &my_env);
	n_types = get_irp_n_types();
	for (i = 0; i < n_types; i++)
		walk_types_entities(get_irp_type(i), &walk_entity, &my_env);
	for (i = 0; i < get_irp_n_irgs(); i++)
		walk_types_entities(get_irg_frame_type(get_irp_irg(i)), &walk_entity, &my_env);

	/* Walk constant array bounds. */
	for (i = 0; i < n_types; i++) {
		ir_type *tp = get_irp_type(i);
		if (is_Array_type(tp)) {
			int n_dim = get_array_n_dimensions(tp);
			for (j = 0; j < n_dim; j++) {
				ir_node *n = get_array_lower_bound(tp, j);
				if (n) irg_walk(n, pre, post, env);
				n = get_array_upper_bound(tp, j);
				if (n) irg_walk(n, pre, post, env);
			}
		}
	}

	current_ir_graph = rem;
}
