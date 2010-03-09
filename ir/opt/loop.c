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
 * @author   Christian Helmer
 * @brief    loop inversion and loop unrolling, loop peeling
 *
 * @version  $Id$
 */
#include "config.h"

#include "iroptimize.h"
#include "opt_init.h"
#include "irnode.h"
#include "debug.h"

#include "ircons.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irouts.h"
#include "iredges.h"
#include "irtools.h"
#include "array_t.h"
#include "beutil.h"
#include "irloop_t.h"
#include "irpass.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg);

/**
 * Convenience macro for iterating over every phi node of the given block.
 * Requires phi list per block.
 */
#define for_each_phi(block, phi) \
	for ( (phi) = get_Block_phis( (block) ); (phi) ; (phi) = get_Phi_next( (phi) ) )

/* current loop */
static ir_loop *cur_loop;

/* abortable walker function */
typedef unsigned irg_walk_func_abortable(ir_node *, void *);

/* condition for walking a node during a copy_walk */
typedef unsigned walker_condition(ir_node *);

/* node and position of a predecessor */
typedef struct out_edges {
	ir_node *node;
	int pred_irn_n;
} out_edge;

/* access complex values through the nodes links */
typedef struct node_info {
	unsigned invariant:1;
	ir_node *copy;
	ir_node *link;					/* temporary links for ssa creation */
	ir_node **ins;					/* ins for phi nodes, during rewiring of blocks */
	unsigned done;
	struct node_info *freelistnext; /* linked list to free all node_infos */
} node_info;

static node_info *link_node_state_list;		/* head of the linked list to free all node_infos */

static out_edge *cur_loop_outs;				/* A walker may start visiting the current loop with these nodes. */
static out_edge *cur_head_outs;				/* A walker may start visiting the cur head with these nodes. */

static ir_node *loop_cf_head = NULL;				/* Loop head node */
static unsigned loop_cf_head_valid = 1;				/* A loop may have one head, otherwise we do not touch it. */

ir_node **loops;

/* Inverted head */
static ir_node *loop_inv_head = NULL;
/* Peeled head */
static ir_node *loop_peeled_head = NULL;

/* Loop analysis informations */
typedef struct loop_info_t {
	unsigned blocks;			/* number of blocks in the loop */
	unsigned calls;				/* number of calls */
	unsigned loads;				/* number of load nodes */
	unsigned outs;				/* outs without keepalives */
#if 0
	unsigned invariant_loads;
	unsigned stores;			/* number of store nodes */
	unsigned opnodes_n;			/* nodes that probably result in an instruction */
	unsigned do_invariant_opt;
#endif
} loop_info_t;

/* Information about the current loop */
static loop_info_t loop_info;

/* A walker may start visiting a condition chain with these nodes. */
static out_edge *cond_chain_entries;

/* Number of unrolling */
int unroll_times;

static unsigned head_inversion_node_count;
static unsigned inversion_head_node_limit;
static unsigned head_inversion_block_count;

static unsigned enable_peeling;
static unsigned enable_inversion;
static unsigned enable_unrolling;

/**
 *
 * ============= AUXILIARY FUNCTIONS =====================================
 */


/**
 * Creates object on the heap, and adds it to a linked list to free it later.
 */
static node_info *new_node_info(void)
{
	node_info *l = XMALLOCZ(node_info);
	l->freelistnext = link_node_state_list;
	link_node_state_list = l;
	l->copy = NULL;
	l->invariant = 0;
	return l;
}

static node_info *get_node_info(ir_node *n)
{
	return ((node_info *)get_irn_link(n));
}

/* Allocates a node_info struct for the given node. For use with a walker. */
static void alloc_node_info(ir_node *node, void *env)
{
	node_info *state;
	(void) env;
	state = new_node_info();
	set_irn_link(node, (void *)state);
}

static void free_node_info(void)
{
	int a = 0;
	node_info *n;
	n = link_node_state_list;
	while (n) {
		node_info *next = n->freelistnext;
		++a;
		xfree(n);
		n = next;
	}
	link_node_state_list = NULL;
}

/**
 * Use the linked list to reset the reused values of all node_infos
 * Reset in particular the copy attribute as copy_walk uses it to determine a present copy
 */
static void reset_node_infos(void)
{
	node_info *next;
	next = link_node_state_list;
	while (next->freelistnext) {
		node_info *cur = next;
		next = cur->freelistnext;
		cur->copy = NULL;
		cur->ins = NULL;
		cur->link = NULL;
	}
}

/* Returns the nodes node_info link. */
static ir_node *get_link(ir_node *n)
{
	return ((node_info *)get_irn_link(n))->link;
}

/* Sets the nodes node_info link. */
static void set_link(ir_node *n, ir_node *link)
{
	((node_info *)get_irn_link(n))->link = link;
}

/* Returns a nodes copy. */
static ir_node *get_copy(ir_node *n)
{
	return ((node_info *)get_irn_link(n))->copy;
}

/* Sets a nodes copy. */
static void set_copy(ir_node *n, ir_node *copy)
{
	((node_info *)get_irn_link(n) )->copy = copy;
}

/**
 * Convenience macro for iterating over every copy in a linked list
 * of copies.
 */
#define for_each_copy(node) \
	for ( ; (node) ; (node) = get_copy(node))

/**
 * Convenience macro for iterating over every copy in 2 linked lists
 * of copies in parallel.
 */
#define for_each_copy2(high1, low1, high2, low2) \
	for ( ; (low1) && (low2); (high1) = (low1), (low1) = get_copy(low1), \
							(high2) = (low2), (low2) = get_copy(low2))

/*
 * Returns 0 if the node or block is not in cur_loop.
 */
static unsigned is_in_loop(ir_node *node)
{
	return (get_irn_loop(get_block(node)) == cur_loop);
}

/* Returns if the given be is an alien edge. This is the case when the pred is not in the loop. */
static unsigned is_alien_edge(ir_node *n, int i)
{
	return(!is_in_loop(get_irn_n(n, i)));
}

/* used for block walker */
static void reset_block_mark(ir_node *node, void * env)
{
	(void) env;

	if (is_Block(node))
		set_Block_mark(node, 0);
}

static unsigned is_nodesblock_marked(ir_node* node)
{
	return (get_Block_mark(get_block(node)));
}

/* Returns the number of blocks in a loop. */
static int get_loop_n_blocks(ir_loop *loop)
{
	int elements, e;
	int blocks = 0;
	elements = get_loop_n_elements(loop);

	for (e=0; e<elements; e++) {
		loop_element elem = get_loop_element(loop, e);
		if (is_ir_node(elem.kind) && is_Block(elem.node))
			++blocks;
	}
	return blocks;
}

/**
 * Add newpred at position pos to node and also add the corresponding value to the phis.
 * Requires block phi list.
 */
static int duplicate_preds(ir_node* block, unsigned pos, ir_node* newpred)
{
	ir_node **ins;
	/*int *is_be;*/
	ir_node *phi;
	int block_arity;
	int i;

	assert(is_Block(block) && "duplicate_preds may be called for blocks only");

	DB((dbg, LEVEL_5, "duplicate_preds(node %N, pos %d, newpred %N)\n", block, pos, newpred));

	block_arity = get_irn_arity(block);

	NEW_ARR_A(ir_node*, ins, block_arity + 1);

	for (i = 0; i < block_arity; ++i) {
		ins[i] = get_irn_n(block, i);
	}
	ins[block_arity] = newpred;

	set_irn_in(block, block_arity + 1, ins);

	/* LDBG 1 */
#if 1
	for_each_phi(block, phi) {
		int phi_arity = get_irn_arity(phi);
		DB((dbg, LEVEL_5, "duplicate_preds: fixing phi %N\n", phi));

		NEW_ARR_A(ir_node *, ins, block_arity + 1);
		for (i = 0; i < phi_arity; ++i) {
			DB((dbg, LEVEL_5, "pos %N\n", get_irn_n(phi, i)));
			ins[i] = get_irn_n(phi, i);
		}
		ins[block_arity] = get_copy(get_irn_n(phi, pos));
		set_irn_in(phi, block_arity + 1, ins);
	}
#endif
	return block_arity;
}

/**
 * Finds loop head and loop_info.
 */
static void get_loop_info(ir_node *node, void *env)
{
	unsigned node_in_loop, pred_in_loop;
	int i, arity;
	(void) env;

	arity = get_irn_arity(node);
	for (i = 0; i < arity; i++) {
		ir_node *pred = get_irn_n(node, i);

		pred_in_loop = is_in_loop(pred);
		node_in_loop = is_in_loop(node);

		/* collect some loop information */
		if (node_in_loop) {
			if (is_Call(node))
				++loop_info.calls;
		}

		/* Find the loops head/the blocks with cfpred outside of the loop */
		if (is_Block(node) && node_in_loop && !pred_in_loop && loop_cf_head_valid) {
			ir_node *cfgpred = get_Block_cfgpred(node, i);

			if (!is_in_loop(cfgpred)) {
				DB((dbg, LEVEL_5, "potential head %+F because inloop and pred %+F not inloop\n", node, pred));
				/* another head? We do not touch this. */
				if (loop_cf_head && loop_cf_head != node) {
					loop_cf_head_valid = 0;
				} else {
					loop_cf_head = node;
				}
			}
		}
	}
}

/* Adds all nodes pointing into the loop to loop_entries and also finds the loops head */
static void get_loop_outs(ir_node *node, void *env)
{
	unsigned node_in_loop, pred_in_loop;
	int i, arity;
	(void) env;

	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);

		pred_in_loop = is_in_loop(pred);
		node_in_loop = is_in_loop(node);

		if (pred_in_loop && !node_in_loop) {
			out_edge entry;
			entry.node = node;
			entry.pred_irn_n = i;
			ARR_APP1(out_edge, cur_loop_outs, entry);
		}
	}
}

static ir_node *ssa_second_def;
static ir_node *ssa_second_def_block;

/**
 * Walks the graph bottom up, searching for definitions and creates phis.
 */
static ir_node *search_def_and_create_phis(ir_node *block, ir_mode *mode)
{
	int i;
	int n_cfgpreds;
	ir_graph *irg;
	ir_node *phi;
	ir_node **in;

	DB((dbg, LEVEL_5, "ssa search_def_and_create_phis: block %N\n", block));

	/* Prevents creation of phi that would be bad anyway.
	 * Dead and bad blocks. */
	if (get_irn_arity(block) < 1 || is_Bad(block))
		return new_Bad();

	if (block == ssa_second_def_block) {
		DB((dbg, LEVEL_5, "ssa found second definition: use second def %N\n", ssa_second_def));
		return ssa_second_def;
	}

	/* already processed this block? */
	if (irn_visited(block)) {
		ir_node *value = get_link(block);
		DB((dbg, LEVEL_5, "ssa already visited: use linked %N\n", value));
		return value;
	}

	irg = get_irn_irg(block);
	assert(block != get_irg_start_block(irg));

	/* a Block with only 1 predecessor needs no Phi */
	n_cfgpreds = get_Block_n_cfgpreds(block);
	if (n_cfgpreds == 1) {
		ir_node *pred_block = get_Block_cfgpred_block(block, 0);
		ir_node *value;

		DB((dbg, LEVEL_5, "ssa 1 pred: walk pred %N\n", pred_block));

		value = search_def_and_create_phis(pred_block, mode);
		set_link(block, value);
		mark_irn_visited(block);

		return value;
	}

	/* create a new Phi */
	NEW_ARR_A(ir_node*, in, n_cfgpreds);
	for (i = 0; i < n_cfgpreds; ++i)
		in[i] = new_Unknown(mode);

	phi = new_r_Phi(block, n_cfgpreds, in, mode);

	/* Important: always keep block phi list up to date. */
	add_Block_phi(block, phi);
	/* EVERY node is assumed to have a node_info linked. */
	alloc_node_info(phi, NULL);

	DB((dbg, LEVEL_5, "ssa phi creation: link new phi %N to block %N\n", phi, block));

	set_link(block, phi);
	mark_irn_visited(block);

	/* set Phi predecessors */
	for (i = 0; i < n_cfgpreds; ++i) {
		ir_node *pred_val;
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		assert(pred_block != NULL);
		pred_val = search_def_and_create_phis(pred_block, mode);
		assert(pred_val != NULL);

		DB((dbg, LEVEL_5, "ssa phi pred:phi %N, pred %N\n", phi, pred_val));
		set_irn_n(phi, i, pred_val);
	}
	return phi;
}

/**
 * Given a set of values this function constructs SSA-form for the users of the
 * first value (the users are determined through the out-edges of the value).
 * Works without using the dominance tree.
 */
static void construct_ssa(ir_node *orig_block, ir_node *orig_val,
                          ir_node *second_block, ir_node *second_val)
{
	ir_graph *irg;
	ir_mode *mode;
	const ir_edge_t *edge;
	const ir_edge_t *next;

	assert(orig_block && orig_val && second_block && second_val &&
			"no parameter of construct_ssa may be NULL");

	/* no need to do anything */
	if (orig_val == second_val)
		return;

	irg = get_irn_irg(orig_val);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);

	mode = get_irn_mode(orig_val);
	set_link(orig_block, orig_val);
	mark_irn_visited(orig_block);

	ssa_second_def_block = second_block;
	ssa_second_def       = second_val;

	/* Only fix the users of the first, i.e. the original node */
	foreach_out_edge_safe(orig_val, edge, next) {
		ir_node *user = get_edge_src_irn(edge);
		int j = get_edge_src_pos(edge);
		ir_node *user_block = get_nodes_block(user);
		ir_node *newval;

		/* ignore keeps */
		if (is_End(user))
			continue;

		DB((dbg, LEVEL_5, "original user %N\n", user));

		if (is_Phi(user)) {
			ir_node *pred_block = get_Block_cfgpred_block(user_block, j);
			newval = search_def_and_create_phis(pred_block, mode);
		} else {
			newval = search_def_and_create_phis(user_block, mode);
		}

		/* If we get a bad node the user keeps the original in. No second definition needed. */
		if (newval != user && !is_Bad(newval))
			set_irn_n(user, j, newval);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
}

/*
 * Construct SSA for def and all of its copies.
 */
static void construct_ssa_n(ir_node *def, ir_node *user)
{
	ir_graph *irg;
	ir_mode *mode;
	ir_node *iter = def;
	const ir_edge_t *edge;
	const ir_edge_t *next;
	irg = get_irn_irg(def);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);

	mode = get_irn_mode(def);

	for_each_copy(iter) {
		set_link(get_nodes_block(iter), iter);
		mark_irn_visited(get_nodes_block(iter));

		DB((dbg, LEVEL_5, "ssa_n:  Link def %N to block %N\n",
									iter, get_nodes_block(iter)));
	}

	/* Need to search the outs, because we need the in-pos on the user node. */
	foreach_out_edge_safe(def, edge, next) {
		ir_node *edge_user = get_edge_src_irn(edge);
		int edge_src = get_edge_src_pos(edge);
		ir_node *user_block = get_nodes_block(user);
		ir_node *newval;

		if (edge_user != user)
			continue;

		if (is_Phi(user)) {
			ir_node *pred_block = get_Block_cfgpred_block(user_block, edge_src);
			newval = search_def_and_create_phis(pred_block, mode);
		} else {
			newval = search_def_and_create_phis(user_block, mode);
		}

		if (newval != user && !is_Bad(newval))
			set_irn_n(user, edge_src, newval);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
}

/**
 * Construct SSA for all definitions in arr.
 */
static void construct_ssa_foreach(ir_node **arr, int arr_n)
{
	int i;
	for (i = 0; i < arr_n ; ++i) {
		ir_node *cppred, *block, *cpblock, *pred;

		pred = arr[i];
		cppred = get_copy(pred);
		block = get_nodes_block(pred);
		cpblock = get_nodes_block(cppred);
		construct_ssa(block, pred, cpblock, cppred);
	}
}

/* get the number of backedges without alien bes */
static int get_backedge_n(ir_node *loophead, unsigned with_alien)
{
	int i;
	int be_n = 0;
	int arity = get_irn_arity(loophead);
	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(loophead, i);
		if (is_backedge(loophead, i) && (with_alien || is_in_loop(pred)))
			++be_n;
	}
	return be_n;
}

/**
 * Rewires the heads after peeling.
 */
static void peel_fix_heads(void)
{
	ir_node **loopheadnins, **peelheadnins;
	ir_node *loophead = loop_cf_head;
	ir_node *peelhead = get_copy(loophead);

	int headarity = get_irn_arity(loophead);
	ir_node *phi;
	int i;

	int lheadin_c = 0;
	int pheadin_c = 0;

	int backedges_n = get_backedge_n(loophead, 0);

	int lhead_arity = 2 * backedges_n;
	int phead_arity = headarity - backedges_n;

	/* new in arrays */
	NEW_ARR_A(ir_node *, loopheadnins, lhead_arity );
	NEW_ARR_A(ir_node *, peelheadnins, phead_arity );

	for_each_phi(loophead, phi) {
		NEW_ARR_A(ir_node *, get_node_info(phi)->ins, lhead_arity);
	}
	for_each_phi(peelhead, phi) {
		NEW_ARR_A(ir_node *, get_node_info(phi)->ins, phead_arity);
	}

	for (i = 0; i < headarity; i++)
	{
		ir_node *orgjmp = get_irn_n(loophead, i);
		ir_node *copyjmp = get_copy(orgjmp);

		/**
		 * Rewire the head blocks ins and their phi ins.
		 * Requires phi list per block.
		 */
		if (is_backedge(loophead, i) && !is_alien_edge(loophead, i)) {
			loopheadnins[lheadin_c] = orgjmp;
			for_each_phi(loophead, phi) {
				get_node_info( phi )->ins[lheadin_c] =	get_irn_n( phi, i) ;
			}
			++lheadin_c;

			/* former bes of the peeled code origin now from the loophead */
			loopheadnins[lheadin_c] = copyjmp;

			/* get_irn_n( get_copy_of(phi), i ) <!=> get_copy_of( get_irn_n( phi, i) )
			 * Order is crucial! Predecessors outside of the loop are non existent.
			 * The copy (cloned with its ins!) has pred i,
			 * but phis pred i might not have a copy of itself.
			 */
			for_each_phi(loophead, phi) {
				get_node_info( phi )->ins[lheadin_c] =	get_irn_n( get_copy(phi), i) ;
			}
			++lheadin_c;
		} else {
			peelheadnins[pheadin_c] = orgjmp;
			for_each_phi(peelhead, phi) {
				get_node_info( phi )->ins[pheadin_c] = get_irn_n(phi, i);
			}
			++pheadin_c;
		}
	}/* for */

	assert(pheadin_c == ARR_LEN(peelheadnins) &&
			lheadin_c == ARR_LEN(loopheadnins) &&
			"the constructed head arities do not match the predefined arities");

	/* assign the ins to the nodes */
	set_irn_in(loophead, ARR_LEN(loopheadnins), loopheadnins);
	set_irn_in(peelhead, ARR_LEN(peelheadnins), peelheadnins);

	for_each_phi(loophead, phi) {
		ir_node **ins = get_node_info( phi )->ins;
		set_irn_in(phi, lhead_arity, ins);
	}

	for_each_phi(peelhead, phi) {
		ir_node **ins = get_node_info( phi )->ins;
		set_irn_in(phi, phead_arity, ins);
	}
}

/**
 * Create a raw copy (ins are still the old ones) of the given node.
 * We rely on copies to be NOT visited.
 */
static ir_node *rawcopy_node(ir_node *node)
{
	int i, arity;
	ir_node *cp;
	node_info *cpstate;

	cp = exact_copy(node);

	arity = get_irn_arity(node);

	for (i = 0; i < arity; ++i) {
		if (is_backedge(node, i))
			set_backedge(cp, i);
	}

	set_copy(node, cp);
	cpstate = new_node_info();
	set_irn_link(cp, cpstate);


	if (is_Block(cp)) {
		/* TODO
		 * exact_copy already sets Macroblock.
		 * Why should we do this anyway? */
		set_Block_MacroBlock(cp, cp);
	}

	return cp;
}

/**
 * This walker copies all walked nodes.
 * If the walk_condition is true for a node, it is walked.
 * All nodes node_info->copy attributes have to be NULL prior to every walk.
 */
static void copy_walk(ir_node *node, walker_condition *walk_condition, ir_loop *set_loop)
{
	int i;
	int arity;
	ir_node *cp;
	ir_node **cpin;
	ir_graph *irg = current_ir_graph;
	node_info *node_info = get_node_info(node);

	/**
	 * break condition and cycle resolver, creating temporary node copies
	 */
	if (get_irn_visited(node) >= get_irg_visited(irg)) {
		/* Here we rely on nodestate's copy being initialized with NULL */
		DB((dbg, LEVEL_5, "copy_walk: We have already visited %N\n", node));
		if (node_info->copy == NULL) {
			cp = rawcopy_node(node);
			DB((dbg, LEVEL_5, "The TEMP copy of %N is created %N\n", node, cp));
		}
		return;
	}

	/* Walk */
	mark_irn_visited(node);

	if (!is_Block(node)) {
		ir_node *pred = get_nodes_block(node);
		if (walk_condition(pred))
			DB((dbg, LEVEL_5, "walk block %N\n", pred));
			copy_walk(pred, walk_condition, set_loop);
	}

	arity = get_irn_arity(node);

	NEW_ARR_A(ir_node *, cpin, arity);

	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);

		if (walk_condition(pred)) {
			DB((dbg, LEVEL_5, "walk node %N\n", pred));
			copy_walk(pred, walk_condition, set_loop);
			cpin[i] = get_copy(pred);
			DB((dbg, LEVEL_5, "copy of %N gets new in %N which is copy of %N\n",
			        node, get_copy(pred), pred));
		} else {
			cpin[i] = pred;
		}
	}

	/* copy node / finalize temp node */
	if (node_info->copy == NULL) {
		/* No temporary copy existent */
		cp = rawcopy_node(node);
		DB((dbg, LEVEL_5, "The FINAL copy of %N is CREATED %N\n", node, cp));
	} else {
		/* temporary copy is existent but without correct ins */
		cp = get_copy(node);
		DB((dbg, LEVEL_5, "The FINAL copy of %N is EXISTENT %N\n", node, cp));
	}

	if (!is_Block(node)) {
		ir_node *cpblock = get_copy(get_nodes_block(node));

		set_nodes_block(cp, cpblock );
		if (is_Phi(cp))
			add_Block_phi(cpblock, cp);
	}

	set_irn_loop(cp, set_loop);
	set_irn_in(cp, ARR_LEN(cpin), cpin);
}

/**
 * Loop peeling, and fix the cf for the loop entry nodes, which have now more preds
 */
static void peel(out_edge *loop_outs)
{
	int i;
	ir_node **entry_buffer;
	int entry_c = 0;

	ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	NEW_ARR_A(ir_node *, entry_buffer, ARR_LEN(loop_outs));

	/* duplicate loop walk */
	inc_irg_visited(current_ir_graph);

	for (i = 0; i < ARR_LEN(loop_outs); i++) {
		out_edge entry = loop_outs[i];
		ir_node *node = entry.node;
		ir_node *pred = get_irn_n(entry.node, entry.pred_irn_n);

		if (is_Block(node)) {
			copy_walk(pred, is_in_loop, NULL);
			duplicate_preds(node, entry.pred_irn_n, get_copy(pred) );
		} else {
			copy_walk(pred, is_in_loop, NULL);
			/* leave out keepalives */
			if (!is_End(node)) {
				/* Node is user of a value defined inside the loop.
				 * We'll need a phi since we duplicated the loop. */
				/* Cannot construct_ssa here, because it needs another walker. */
				entry_buffer[entry_c] = pred;
				++entry_c;
			}
		}
	}

	ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	/* Rewires the 2 heads */
	peel_fix_heads();

	/* Generate phis for values from peeled code and original loop */
	construct_ssa_foreach(entry_buffer, entry_c);
	/*for (i = 0; i < entry_c; i++)
	{
		ir_node *cppred, *block, *cpblock, *pred;

		pred = entry_buffer[i];
		cppred = get_copy(pred);
		block = get_nodes_block(pred);
		cpblock = get_nodes_block(cppred);
		construct_ssa(block, pred, cpblock, cppred);
	}*/
}

/**
 * Populates head_entries with (node, pred_pos) tuple
 * whereas the node's pred at pred_pos is in the head but not the node itself.
 * Head and condition chain blocks must be marked.
 */
static void get_head_outs(ir_node *node, void *env)
{
	int i;
	int arity = get_irn_arity(node);
	(void) env;

	DB((dbg, LEVEL_5, "get head entries %N \n", node));

	for (i = 0; i < arity; ++i) {
		/* node is not in the head, but the predecessor is.
		 * (head or loop chain nodes are marked) */

		DB((dbg, LEVEL_5, "... "));
		DB((dbg, LEVEL_5, "node %N  marked %d (0)  pred %d marked %d (1) \n",
		    node->node_nr, is_nodesblock_marked(node),i, is_nodesblock_marked(get_irn_n(node, i))));

		if (!is_nodesblock_marked(node) && is_nodesblock_marked(get_irn_n(node, i))) {
			out_edge entry;
			entry.node = node;
			entry.pred_irn_n = i;
			DB((dbg, LEVEL_5,
			        "Found head chain entry %N @%d because !inloop %N and inloop %N\n",
			        node, i, node, get_irn_n(node, i)));
			ARR_APP1(out_edge, cur_head_outs, entry);
		}
	}
}

/**
 * Find condition chains, and add them to be inverted, until the node count exceeds the limit.
 * A block belongs to the chain if a condition branches out of the loop.
 * Returns 1 if the given block belongs to the condition chain.
 */
static unsigned find_condition_chains(ir_node *block)
{
	const ir_edge_t *edge;
	unsigned mark = 0;
	int nodes_n = 0;

	DB((dbg, LEVEL_5, "condition_chains for block %N\n", block));

	/* Collect all outs, including keeps.
	 * (TODO firm function for number of out edges?) */
	foreach_out_edge_kind(block, edge, EDGE_KIND_NORMAL) {
		++nodes_n;
	}

	/* We do not want to collect more nodes from condition chains, than the limit allows us to.
	 * Also, leave at least one block as body. */
	if (head_inversion_node_count + nodes_n > inversion_head_node_limit
		    || head_inversion_block_count + 1 == loop_info.blocks) {
		set_Block_mark(block, 0);

		return 0;
	}

	/* First: check our successors, and add all succs that are outside of the loop to the list */
	foreach_block_succ(block, edge) {
		ir_node *src = get_edge_src_irn( edge );
		int pos = get_edge_src_pos( edge );

		if (!is_in_loop(src)) {
			out_edge entry;

			mark = 1;
			entry.node = src;
			entry.pred_irn_n = pos;
			ARR_APP1(out_edge, cond_chain_entries, entry);
			mark_irn_visited(src);
		}
	}

	if (mark == 0) {
		/* this block is not part of the chain,
		 * because the chain would become too long or we have no successor outside of the loop */

		set_Block_mark(block, 0);
		return 0;
	} else {
		set_Block_mark(block, 1);
		++head_inversion_block_count;
		DB((dbg, LEVEL_5, "block %N is part of condition chain\n", block));
		head_inversion_node_count += nodes_n;
	}

	/* Second: walk all successors, and add them to the list if they are not part of the chain */
	foreach_block_succ(block, edge) {
		unsigned inchain;
		ir_node *src = get_edge_src_irn( edge );
		int pos = get_edge_src_pos( edge );

		/* already done cases */
		if (!is_in_loop( src ) || (get_irn_visited(src) >= get_irg_visited(current_ir_graph))) {
			continue;
		}

		mark_irn_visited(src);
		DB((dbg, LEVEL_5, "condition chain walk %N\n", src));
		inchain = find_condition_chains(src);

		/* if successor is not part of chain we need to collect its outs */
		if (!inchain) {
			out_edge entry;
			entry.node = src;
			entry.pred_irn_n = pos;
			ARR_APP1(out_edge, cond_chain_entries, entry);
		}
	}
	return mark;
}

/**
 * Rewire the loop head and inverted head for loop inversion.
 */
static void inversion_fix_heads(void)
{
	ir_node **loopheadnins, **invheadnins;
	ir_node *loophead = loop_cf_head;
	ir_node *invhead = 	get_copy(loophead);

	int headarity = 	get_irn_arity(loophead);
	ir_node *phi;
	int i;

	int lheadin_c = 0;
	int iheadin_c = 0;

	int backedges_n = get_backedge_n(loophead, 0);
	int lhead_arity = backedges_n;
	int ihead_arity = headarity - backedges_n;

	assert(lhead_arity != 0 && "Loophead has arity 0. Probably wrong backedge informations.");
	assert(ihead_arity != 0 && "Inversionhead has arity 0. Probably wrong backedge informations.");

	/* new in arrays for all phis in the head blocks */
	NEW_ARR_A(ir_node *, loopheadnins, lhead_arity);
	NEW_ARR_A(ir_node *, invheadnins, ihead_arity);

	for_each_phi(loophead, phi) {
		NEW_ARR_A(ir_node *, get_node_info(phi)->ins, lhead_arity);
	}
	for_each_phi(invhead, phi) {
		NEW_ARR_A(ir_node *, get_node_info(phi)->ins, ihead_arity);
	}

	for (i = 0; i < headarity; i++) {
		ir_node *pred = get_irn_n(loophead, i);

		/**
		 * Rewire the head blocks ins and their phi ins.
		 * Requires phi list per block.
		 */
		if (is_backedge(loophead, i) && !is_alien_edge(loophead, i)) {
			/* just copy these edges */
			loopheadnins[lheadin_c] = pred;
			for_each_phi(loophead, phi) {
				get_node_info(phi)->ins[lheadin_c] = get_irn_n(phi, i);
			}
			++lheadin_c;
		} else {
			invheadnins[iheadin_c] = pred;
			for_each_phi(invhead, phi) {
				get_node_info(phi)->ins[iheadin_c] = get_irn_n(phi, i) ;
			}
			++iheadin_c;
		}
	}

	/* assign the ins to the head blocks */
	set_irn_in(loophead, ARR_LEN(loopheadnins), loopheadnins);
	set_irn_in(invhead, ARR_LEN(invheadnins), invheadnins);

	/* assign the ins for the phis */
	for_each_phi(loophead, phi) {
		ir_node **ins = get_node_info(phi)->ins;
		set_irn_in(phi, lhead_arity, ins);
	}

	for_each_phi(invhead, phi) {
		ir_node **ins = get_node_info(phi)->ins;
		set_irn_in(phi, ihead_arity, ins);
	}
}

static void inversion_walk(out_edge *head_entries)
{
	int i;
	ir_node *phi;
	int entry_c = 0;
	ir_node **entry_buffer;
	ir_node **head_phi_assign;

	NEW_ARR_A(ir_node *, entry_buffer, ARR_LEN(head_entries));

	head_phi_assign = NEW_ARR_F(ir_node *, 0);

	/* Find assignments in the condition chain,
	 * to construct_ssa for them after the loop inversion. */
	for_each_phi(loop_cf_head , phi) {
		int arity = get_irn_arity(phi);
		for (i = 0; i < arity; ++i) {
			ir_node *def = get_irn_n(phi, i);
			if (is_nodesblock_marked(def)) {
				ARR_APP1(ir_node *, head_phi_assign, def);
			}
		}
	}

	ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	/**
	 * duplicate condition chain
	 **/
	inc_irg_visited(current_ir_graph);

	for (i = 0; i < ARR_LEN(head_entries); ++i) {
		out_edge entry = head_entries[i];
		ir_node *node = entry.node;
		ir_node *pred = get_irn_n(entry.node, entry.pred_irn_n);

		if (is_Block(node)) {
			DB((dbg, LEVEL_5, "\nInit walk block %N\n", pred));

			copy_walk(pred, is_nodesblock_marked, cur_loop);
			duplicate_preds(node, entry.pred_irn_n, get_copy(pred) );
		} else {
			DB((dbg, LEVEL_5, "\nInit walk node  %N\n", pred));

			copy_walk(pred, is_nodesblock_marked, cur_loop);

			/* ignore keepalives */
			if (!is_End(node)) {
				/* Node is user of a value assigned inside the loop.
				 * We will need a phi since we duplicated the head. */
				entry_buffer[entry_c] = pred;
				++entry_c;
			}
		}
	}

	ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	inversion_fix_heads();

	/* Generate phis for users of values assigned in the condition chain
	 * and read in the loops body */
	construct_ssa_foreach(entry_buffer, entry_c);

	/* Generate phis for values that are assigned in the condition chain
	 * but not read in the loops body. */
	construct_ssa_foreach(head_phi_assign, ARR_LEN(head_phi_assign));

	loop_cf_head = get_copy(loop_cf_head);
}

/* Loop peeling */
static void loop_peeling(void)
{
	cur_loop_outs = NEW_ARR_F(out_edge, 0);
	irg_walk_graph( current_ir_graph, get_loop_outs, NULL, NULL );

	peel(cur_loop_outs);

	/* clean up */
	reset_node_infos();

	set_irg_doms_inconsistent(current_ir_graph);
	set_irg_loopinfo_inconsistent(current_ir_graph);
	set_irg_outs_inconsistent(current_ir_graph);

	DEL_ARR_F(cur_loop_outs);
}

/* Loop inversion */
static void loop_inversion(void)
{
	unsigned do_inversion = 1;

	inversion_head_node_limit = INT_MAX;

	/* Search for condition chains. */
	ir_reserve_resources(current_ir_graph, IR_RESOURCE_BLOCK_MARK);

	irg_walk_graph(current_ir_graph, reset_block_mark, NULL, NULL);

	loop_info.blocks = get_loop_n_blocks(cur_loop);
	cond_chain_entries = NEW_ARR_F(out_edge, 0);

	head_inversion_node_count = 0;
	head_inversion_block_count = 0;

	set_Block_mark(loop_cf_head, 1);
	mark_irn_visited(loop_cf_head);
	inc_irg_visited(current_ir_graph);

	find_condition_chains(loop_cf_head);

	DB((dbg, LEVEL_3, "Loop contains %d blocks.\n", loop_info.blocks));
	if (loop_info.blocks < 2) {
		do_inversion = 0;
		DB((dbg, LEVEL_3, "Loop contains %d (less than 2) blocks => No Inversion done.\n", loop_info.blocks));
	}

	/* We also catch endless loops here,
	 * because they do not have a condition chain. */
	if (head_inversion_block_count < 1) {
		do_inversion = 0;
		DB((dbg, LEVEL_3, "Loop contains %d (less than 1) invertible blocks => No Inversion done.\n", head_inversion_block_count));
	}

	if (do_inversion) {
		cur_head_outs = NEW_ARR_F(out_edge, 0);

		/* Get all edges pointing into the head or condition chain (outs). */
		irg_walk_graph(current_ir_graph, get_head_outs, NULL, NULL);
		inversion_walk(cur_head_outs);

		DEL_ARR_F(cur_head_outs);

		set_irg_doms_inconsistent(current_ir_graph);
		set_irg_loopinfo_inconsistent(current_ir_graph);
		set_irg_outs_inconsistent(current_ir_graph);
	}

	/* free */
	DEL_ARR_F(cond_chain_entries);
	ir_free_resources(current_ir_graph, IR_RESOURCE_BLOCK_MARK);
}

/**
 * Returns last element of linked list of copies by
 * walking the linked list.
 */
static ir_node *get_last_copy(ir_node *node)
{
	ir_node *copy, *cur;
	cur = node;
	while ((copy = get_copy(cur))) {
		cur = copy;
	}
	return cur;
}

/**
 * Rewire floating copies of the current loop.
 */
static void unrolling_fix_cf(void)
{
	ir_node *loophead = loop_cf_head;
	int headarity = 	get_irn_arity(loophead);
	ir_node *phi, *headnode;
	/*ir_node *high, *low;*/
	int i;

	int uhead_in_n = 0;
	int backedges_n = get_backedge_n(loophead, 0);
	int unroll_arity = backedges_n;

	/* Create ins for all heads and their phis */
	headnode = get_copy(loophead);
	for_each_copy(headnode) {
		NEW_ARR_A(ir_node *, get_node_info(headnode)->ins, unroll_arity);
		for_each_phi(headnode, phi) {
			NEW_ARR_A(ir_node *, get_node_info(phi)->ins, unroll_arity);
		}
	}

	/* Append the copies to the existing loop. */
	for (i = 0; i < headarity; i++) {
		ir_node *upper_head = loophead;
		ir_node *lower_head = get_copy(loophead);

		ir_node *upper_pred = get_irn_n(loophead, i);
		ir_node *lower_pred = get_copy(get_irn_n(loophead, i));

		ir_node *last_pred;

		/**
		 * Build unrolled loop top down
		 */
		if (is_backedge(loophead, i) && !is_alien_edge(loophead, i)) {
			for_each_copy2(upper_head, lower_head, upper_pred, lower_pred) {
				get_node_info(lower_head)->ins[uhead_in_n] = upper_pred;

				for_each_phi(upper_head, phi) {
					ir_node *phi_copy = get_copy(phi);
					get_node_info(phi_copy)->ins[uhead_in_n] = get_irn_n(phi, i);
				}
			}

			last_pred = upper_pred;
			++uhead_in_n;

			/* Fix the topmost loop heads backedges. */
			set_irn_n(loophead, i, last_pred);
			for_each_phi(loophead, phi) {
				ir_node *last_phi = get_last_copy(phi);
				ir_node *pred = get_irn_n(last_phi, i);
				set_irn_n(phi, i, pred);
			}
		}
	}

	headnode = get_copy(loophead);
	for_each_copy(headnode) {
		set_irn_in(headnode, unroll_arity, get_node_info(headnode)->ins);
		for_each_phi(headnode, phi) {
			set_irn_in(phi, unroll_arity, get_node_info(phi)->ins);
		}
	}
}

#if 0
static ir_node *add_phi(ir_node *node, int phi_pos)
{
	ir_mode *mode;
	ir_node *phi;
	ir_node **in;
	mode = get_irn_mode(get_irn_n(node, phi_pos));
	ir_node *block = get_nodes_block(node);
	int n_cfgpreds = get_irn_arity(block);
	ir_node *pred = get_irn_n(node, phi_pos);
	int i;

	/* create a new Phi */
	NEW_ARR_A(ir_node*, in, n_cfgpreds);
	for (i = 0; i < n_cfgpreds; ++i)
		in[i] = new_Unknown(mode);  /*pred;*/

	phi = new_r_Phi(block, n_cfgpreds, in, mode);

	assert(phi && "phi null");
	assert(is_Bad(phi) && "phi bad");

	/* Important: always keep block phi list up to date. */
	add_Block_phi(block, phi);
	/* EVERY node is assumed to have a node_info linked. */
	alloc_node_info(phi, NULL);

	set_irn_n(node, phi_pos, phi);
	return phi;
}
#endif


/**
 * Loop unrolling
 * Could be improved with variable range informations.
 */
static void loop_unrolling(void)
{
	int i, j;

	unroll_times = 7;

	cur_loop_outs = NEW_ARR_F(out_edge, 0);
	irg_walk_graph( current_ir_graph, get_loop_outs, NULL, NULL );

	ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	/* duplicate whole loop content */
	inc_irg_visited(current_ir_graph);

	for (i = 0; i < ARR_LEN(cur_loop_outs); ++i) {
		out_edge entry = cur_loop_outs[i];
		ir_node *node = entry.node;
		ir_node *pred = get_irn_n(entry.node, entry.pred_irn_n);
		if (!is_Block(node)) {
			for (j = 0; j < unroll_times - 1; ++j) {
				copy_walk(pred, is_in_loop, cur_loop);
				pred = get_copy(pred);
			}
		}
	}

	for (i = 0; i < ARR_LEN(cur_loop_outs); ++i) {
		out_edge entry = cur_loop_outs[i];
		ir_node *node = entry.node;
		ir_node *pred = get_irn_n(entry.node, entry.pred_irn_n);

		if (is_Block(node)) {
			for (j = 0; j < unroll_times - 1; ++j) {
				copy_walk(pred, is_in_loop, cur_loop);
				duplicate_preds(node, entry.pred_irn_n, get_copy(pred));

				pred = get_copy(pred);
			}
		}
	}

	ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	/*dump_ir_graph(current_ir_graph, "-raw");*/

	/* LDBG 2 */
#if 1
	/* Line up the floating copies. */
	unrolling_fix_cf();

	/* Generate phis for all loop outs */
	for (i = 0; i < ARR_LEN(cur_loop_outs); ++i) {
		out_edge entry = cur_loop_outs[i];
		ir_node *node = entry.node;
		ir_node *pred = get_irn_n(entry.node, entry.pred_irn_n);

		if (!is_Block(node) && !is_End(node)) {
			DB((dbg, LEVEL_5, "  construct_ssa_n def %N  node %N  pos %d\n",
					pred, node, entry.pred_irn_n));
			construct_ssa_n(pred, node);
		}
	}
#endif

	DEL_ARR_F(cur_loop_outs);

	set_irg_doms_inconsistent(current_ir_graph);
	set_irg_loopinfo_inconsistent(current_ir_graph);
	set_irg_outs_inconsistent(current_ir_graph);
}

/* Initialization and */
static void init_analyze(ir_loop *loop)
{
	/* Init new for every loop */
	cur_loop = loop;

	loop_cf_head = NULL;
	loop_cf_head_valid = 1;
	loop_inv_head = NULL;
	loop_peeled_head = NULL;

	loop_info.outs = 0;
	loop_info.calls = 0;
	loop_info.loads = 0;
	loop_info.blocks = 0;

	DB((dbg, LEVEL_2, "  >>>> current loop includes node %N <<<\n", get_loop_node(loop, 0)));

	irg_walk_graph(current_ir_graph, get_loop_info, NULL, NULL);

	/* RETURN if there is no valid head */
	if (!loop_cf_head || !loop_cf_head_valid) {
		DB((dbg, LEVEL_2,   "No valid loop head. Nothing done.\n"));
		return;
	}

	if (enable_peeling)
		loop_peeling();

	if (enable_inversion)
		loop_inversion();
	if (enable_unrolling)
		loop_unrolling();

#if 0
	/* RETURN if there is a call in the loop */
	if (loop_info.calls)
		return;
#endif

	DB((dbg, LEVEL_2, "      <<<< end of loop with node %N >>>>\n", get_loop_node(loop, 0)));
}

/* Find most inner loops and send them to analyze_loop */
static void find_most_inner_loop(ir_loop *loop)
{
	/* descend into sons */
	int sons = get_loop_n_sons(loop);

	if (sons == 0) {
		loop_element elem;
		int el_n, i;

		el_n = get_loop_n_elements(loop);

		for (i=0; i < el_n; ++i) {
			elem = get_loop_element(loop, i);
			/* We can only rely on the blocks,
			 * as the loop attribute of the nodes seems not to be set. */
			if (is_ir_node(elem.kind) && is_Block(elem.node)) {
				ARR_APP1(ir_node *, loops, elem.node);
				DB((dbg, LEVEL_5, "Found most inner loop (contains block %+F)\n", elem.node));
				break;
			}
		}
	} else {
		int s;
		for (s=0; s<sons; s++) {
			find_most_inner_loop(get_loop_son(loop, s));
		}
	}
}

/**
 * Assure preconditions are met and go through all loops.
 */
void loop_optimization(ir_graph *irg)
{
	ir_loop *loop;
	int     i, sons, nr;

	/* Init */
	link_node_state_list = NULL;
	set_current_ir_graph(irg);

	/* preconditions */
	edges_assure(irg);
	assure_irg_outs(irg);

	/* NOTE: sets only the loop attribute of blocks, not nodes */
	/* NOTE: Kills links */
	assure_cf_loop(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);
	collect_phiprojs(irg);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	/* allocate node_info for additional information on nodes */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_walk_graph(current_ir_graph, alloc_node_info, NULL, NULL);

	loop = get_irg_loop(irg);
	sons = get_loop_n_sons(loop);

	loops = NEW_ARR_F(ir_node *, 0);

	for (nr = 0; nr < sons; ++nr) {
		find_most_inner_loop(get_loop_son(loop, nr));
	}

/* TODO Keep backedges during optimization to avoid
 * this ugly allocation and deallocation.
 * (set_irn_in seems to destroy them)
 */
#if 0
	for (i = 0; i < ARR_LEN(loops); ++i) {
		ir_loop *loop;

		loop = get_irn_loop(loops[i]);
		init_analyze(loop);
	}
#else
	/* This part is useful for testing
	 * or has to be used if the backedge information is destroyed.
	 * Which is the case at the moment, because the backedge information gets lost
	 * before inversion_fix_heads/unrolling_fix_cf, which results in bads.
	 * NOTE!: Testsuite runs successfully nevertheless...
	 */

	/**
	 * assure_cf_loop() creates a completely new loop tree.
	 * Thus we cannot optimize a loop, assure_cf_loop() and continue with the next loop,
	 * as the next loop must be searched because it is not distinguishable from the
	 * already done loops.
	 * The links of the loops are also not available anymore (to store a "loop done" flag).
	 * Therefore we save a block per loop.
	 * NOTE: We rely on the loop optimizations not to remove any block from the loop.
	 * Later, we fetch the blocks loop attribute, as it is updated by assure_cf_loop.
	 */
	for (i = 0; i < ARR_LEN(loops); ++i) {
		ir_loop *loop;

		free_node_info();
		ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

		edges_assure(current_ir_graph);
		assure_irg_outs(current_ir_graph);

		/* NOTE: sets only the loop attribute of blocks */
		/* NOTE: Kills links */
		assure_cf_loop(current_ir_graph);

		irg_walk_graph(current_ir_graph, alloc_node_info, NULL, NULL);
		ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

		/* Get loop from block */
		loop = get_irn_loop(loops[i]);
		init_analyze(loop);
	}
#endif

	/* Free */
	DEL_ARR_F(loops);

	free_node_info();
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	ir_free_resources(irg, IR_RESOURCE_PHI_LIST);
}

void do_loop_unrolling(ir_graph *irg)
{
	enable_unrolling = 1;
	enable_peeling = 0;
	enable_inversion = 0;

	DB((dbg, LEVEL_2, " >>> unrolling (Startnode %N) <<<\n",
	        get_irg_start(irg)));

	loop_optimization(irg);

	DB((dbg, LEVEL_2, " >>> unrolling done (Startnode %N) <<<\n",
	        get_irg_start(irg)));
}

void do_loop_inversion(ir_graph *irg)
{
	enable_unrolling = 0;
	enable_peeling = 0;
	enable_inversion = 1;

	DB((dbg, LEVEL_2, " >>> inversion (Startnode %N) <<<\n",
	        get_irg_start(irg)));

	loop_optimization(irg);

	DB((dbg, LEVEL_2, " >>> inversion done (Startnode %N) <<<\n",
	        get_irg_start(irg)));
}

void do_loop_peeling(ir_graph *irg)
{
	enable_unrolling = 0;
	enable_peeling = 1;
	enable_inversion = 0;

	DB((dbg, LEVEL_2, " >>> peeling (Startnode %N) <<<\n",
	        get_irg_start(irg)));

	loop_optimization(irg);

	DB((dbg, LEVEL_2, " >>> peeling done (Startnode %N) <<<\n",
	        get_irg_start(irg)));

}

ir_graph_pass_t *loop_inversion_pass(const char *name)
{
	return def_graph_pass(name ? name : "loop_inversion", do_loop_inversion);
}

ir_graph_pass_t *loop_unroll_pass(const char *name)
{
	return def_graph_pass(name ? name : "loop_unroll", do_loop_unrolling);
}

ir_graph_pass_t *loop_peeling_pass(const char *name)
{
	return def_graph_pass(name ? name : "loop_peeling", do_loop_peeling);
}

void firm_init_loop_opt(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.loop");
}
