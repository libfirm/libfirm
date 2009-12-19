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
 * @brief    Loop peeling and unrolling
 * @author   Christian Helmer
 * @version  $Id$
 */
#include "config.h"

#include "irnode.h"
#include "debug.h"

#include "ircons.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irouts.h"
#include "iredges.h"
#include "irtools.h"
#include "array_t.h"	/* automatic array */
#include "beutil.h"		/* get_block */
#include "irloop_t.h"	/* set_irn_loop */

// TODO during DBG
//#include "irnode_t.h"
#include "irdump.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg);

/**
 * Convenience macro for iterating over every phi node of the given block.
 * Requires phi list per block.
 */
#define for_each_phi(block, phi) \
	for ( (phi) = get_Block_phis( (block) ); (phi) ; (phi) = get_Phi_next( (phi) ) )

/* current loop */
static ir_loop *cur_loop;

/* The loop walker should be possible to abort if nothing can be done anymore */
typedef unsigned irg_walk_func_abortable(ir_node *, void *);

/* condition for breaking a copy_walk */
typedef unsigned walker_condition(ir_node *);

/* stores node and position of a predecessor */
typedef struct out_edges {
	ir_node *node;
	int pred_irn_n;
} out_edges;

/* access complex values through the nodes links */
typedef struct node_info {
	unsigned invariant:1;
	ir_node *copy;
	ir_node *link;					/* temporary links for ssa creation */
	ir_node **ins;					/* ins for phi nodes, during rewiring of blocks */
	struct node_info *freelistnext; /* linked list to free all node_infos */
} node_info;

static node_info *link_node_state_list;		/* head of the linked list to free all node_infos */

static out_edges *cur_loop_outs;				/* A walker may start visiting the current loop with these nodes. */
static out_edges *cur_head_outs;				/* A walker may start visiting the cur head with these nodes. */

static ir_node *loop_cf_head = NULL;				/* Loop head node */
static unsigned loop_cf_head_valid = 1;				/* A loop may have one head, otherwise we do not touch it. */

/* Inverted head */
static ir_node *loop_inv_head = NULL;
/* Peeled head */
static ir_node *loop_peeled_head = NULL;

/* Loop analysis informations */
typedef struct loop_info_t {
	unsigned calls;
	unsigned loads;
	unsigned invariant_loads;	/* number of load nodes */
	unsigned stores;			/* number of store nodes */
	unsigned blocks;			/* number of blocks in the loop */
	unsigned opnodes_n;			/* nodes that should result in an instruction */
	unsigned opnodes_head;
} loop_info_t;

/* Information about the current loop */
static loop_info_t loop_info;

/* A walker may start visiting a condition chain with these nodes. */
static out_edges *cond_chain_entries;

static unsigned head_inversion_node_count;
static unsigned head_inversion_node_limit;
static unsigned head_inversion_block_count;

/**
 *
 * ============= AUXILIARY FUNCTIONS =====================================
 */

/**
 * Creates object on the heap, and adds it to a linked list to free it later.
 */
static node_info *new_node_info(void) {
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
	node_info *state = new_node_info();
	(void) env;
	set_irn_link(node, (void *)state);
}

static void free_node_info(void)
{
	node_info *next;
	next = link_node_state_list;
	while(next->freelistnext) {
		node_info *cur = next;
		next = cur->freelistnext;
		xfree( cur );
	}
}

/**
 * Use the linked list to reset the reused values of all node_infos
 * Reset in particular the copy attribute as copy_walk uses it to determine a present copy
 */
static void reset_node_infos(void)
{
	node_info *next;
	next = link_node_state_list;
	while(next->freelistnext) {
		node_info *cur = next;
		next = cur->freelistnext;
		cur->copy = NULL;
		cur->ins = NULL;
		cur->link = NULL;
	}
}

/* Returns the  */
static ir_node *get_copy(ir_node *n)
{
	return ((node_info *)get_irn_link(n))->copy;
}

/* Links the node to its copy */
static void set_copy(ir_node *n, ir_node *copy)
{
	((node_info *)get_irn_link(n) )->copy = copy;
}

/* Returns 0 if the node or block is not in cur_loop */
static unsigned is_in_loop(ir_node *node)
{
	return (get_irn_loop(get_block(node)) == cur_loop);
}

/* Returns if the given be is an alien edge. This is the case when the pred is not in the loop. */
static unsigned is_alien_edge(ir_node *n, int i)
{
	return(!is_in_loop(get_irn_n(n, i)));
}

/* used for walker */
static void unmark_block(ir_node *node, void * env)
{
	(void) env;
	DB((dbg, LEVEL_4, "UNMARK ..."));
	DB((dbg, LEVEL_4, " UNMARK %ld\n", get_irn_node_nr(node)));
	if(is_Block(node))
		set_Block_mark(node, 0);
}

static unsigned is_nodesblock_marked(ir_node* node)
{
	return (get_Block_mark(get_block(node)));
}

/* Returns the number of blocks in a loop. */
int get_loop_n_blocks(ir_loop *loop) {
	int elements, e;
	int blocks = 0;
	elements = get_loop_n_elements(loop);

	for(e=0; e<elements; e++) {
		loop_element elem = get_loop_element(loop, e);
		if  (is_ir_node(elem.kind) && is_Block(elem.node) )
			++blocks;
	}
	return blocks;
}

/**
 * Add newpred at position pos to node and also add the corresponding value to the phis.
 * Requires block phi list.
 */
static void duplicate_preds(ir_node* node, unsigned pos, ir_node* newpred)
{
	ir_node** ins;
	ir_node *phi;
	int block_arity;
	int i;

	assert(is_Block(node) && "duplicate_preds is only allowed for blocks");

	DB((dbg, LEVEL_4, "duplicate_preds(node %ld, pos %d, newpred %ld)\n", get_irn_node_nr(node), pos, get_irn_node_nr(newpred)));

	block_arity = get_irn_arity(node);

	NEW_ARR_A(ir_node*, ins, block_arity + 1 );
	for (i = 0; i < block_arity; ++i)
		ins[i] = get_irn_n(node, i);
	ins[block_arity] = newpred;

	set_irn_in(node, block_arity + 1, ins);

	for_each_phi(node, phi) {
		int phi_arity = get_irn_arity(phi);
		DB((dbg, LEVEL_4, "duplicate_preds: fixing phi %ld\n", get_irn_node_nr(phi)));

		NEW_ARR_A(ir_node *, ins, block_arity + 1);
		for(i=0; i < phi_arity; ++i) {
			DB((dbg, LEVEL_4, "in %ld\n", get_irn_node_nr(get_irn_n(phi, i))));
			ins[i] = get_irn_n(phi, i);
		}
		ins[block_arity] = get_irn_n(phi, pos);
		set_irn_in(phi, block_arity + 1, ins);
	}
}

/* Adds all nodes pointing into the loop to loop_entries and also finds the loops head */
static void get_loop_outs_and_info(ir_node *node, void *env)
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
			if ( !is_Store(node) )
				++loop_info.stores;
			if ( !is_Load(node) )
				++loop_info.loads;
			if ( !is_Call(node) )
				++loop_info.calls;
			if ( !is_Block(node) && !is_Proj(node) && !is_Phi(node) )
				++loop_info.opnodes_n;
		}

		//Find the loops head/the blocks with cfpred outside of the loop
		if (is_Block(node) && node_in_loop && !pred_in_loop && loop_cf_head_valid) {
			ir_node *cfgpred = get_Block_cfgpred(node, i);
			if ( !is_in_loop(cfgpred) ) {
				//DB((dbg, LEVEL_1, "potential head %+F\n", node));
				/* another head? We do not touch this. */
				if (loop_cf_head && loop_cf_head != node) {
					loop_cf_head_valid = 0;
				} else {
					loop_cf_head = node;
				}
			}
		}

		if ( pred_in_loop && !node_in_loop ) {
			out_edges entry;
			entry.node = node;
			entry.pred_irn_n = i;
			ARR_APP1(out_edges, cur_loop_outs, entry);
		}
	}
}

/**
 * Finds invariant loads and marks them as invariant.
 * (has to be post walk)
 */
static unsigned get_invariants(ir_node *node, void *env)
{
	unsigned invar = 1;
	int arity = get_irn_arity(node);
	(void) env;

	/* RETURN, no preds to visit */
	if (arity == 0) return 0;

	if (is_Load(node)) {
		ir_node *pred = get_Load_ptr(node);
		if ( (get_Load_volatility(node) == volatility_non_volatile) &
				(!is_in_loop(pred)
				|| is_Const(pred)
				|| is_SymConst(pred)
				|| get_node_info(node)->invariant ) ) {
			get_node_info(node)->invariant = 1;
			++loop_info.invariant_loads;
		} else
		{
			get_node_info(node)->invariant = 0;
		}
	} else {
		int i;
		invar = 1;
		/* find loop variant preds */
		for(i = 0; i < arity; ++i) {
			ir_node *pred = get_irn_n(node, i);

			if ( is_in_loop(pred)							/* outside loop is loop invariant */
					&& !is_Const(pred)						/* constants */
					&& !is_SymConst(pred)					/* SymConst */
					&& !get_node_info(node)->invariant ) {	/* pred is marked as invariant */
				invar = 0;
			}
		}

		if (invar) {
			get_node_info(node)->invariant = 1;
		} else {
			get_node_info(node)->invariant = 0;
		}
	}
	return 0;
}


static ir_node *ssa_second_def;
static ir_node *ssa_second_def_block;

/**
 * Walks the graph bottom up, searching for definitions and create phis.
 * (Does not handle the special case where the second definition is in the block of the user
 * of the original definition because it is not necessary here.)
 */
static ir_node *search_def_and_create_phis(ir_node *block, ir_mode *mode)
{
	int i;
	int n_cfgpreds;
	ir_graph *irg;
	ir_node *phi;
	ir_node **in;

	DB((dbg, LEVEL_4, "ssa sdacp: block %ld\n", get_irn_node_nr(block)));

	/* Prevents creation of phi that would be bad anyway.
	 * Dead and bad blocks. */
	if (get_irn_arity(block) < 1 || is_Bad(block))
		return new_Bad();

	if (block == ssa_second_def_block) {
		DB((dbg, LEVEL_4, "ssa found second definition: use second def %ld\n", get_irn_node_nr(ssa_second_def)));
		return ssa_second_def;
	}

	/* already processed this block? */
	if (irn_visited(block)) {
		ir_node *value = get_node_info(block)->link;
		DB((dbg, LEVEL_4, "ssa already visited: use linked %ld\n", get_irn_node_nr(value)));
		return value;
	}

	irg = get_irn_irg(block);
	assert(block != get_irg_start_block(irg));

	/* a Block with only 1 predecessor needs no Phi */
	n_cfgpreds = get_Block_n_cfgpreds(block);
	if (n_cfgpreds == 1) {
		ir_node *pred_block = get_Block_cfgpred_block(block, 0);
		ir_node *value;

		DB((dbg, LEVEL_4, "ssa 1 pred: walk pred %ld\n", get_irn_node_nr(pred_block)));

		value = search_def_and_create_phis(pred_block, mode);
		get_node_info(block)->link = value;
		mark_irn_visited(block);

		return value;
	}

	/* create a new Phi */
	NEW_ARR_A(ir_node*, in, n_cfgpreds);
	for(i = 0; i < n_cfgpreds; ++i)
		in[i] = new_Unknown(mode);

	phi = new_r_Phi(block, n_cfgpreds, in, mode);

	/* Important: always keep block phi list up to date. */
	add_Block_phi(block, phi);
	/* EVERY node is assumed to have a node_info linked. */
	alloc_node_info(phi, NULL);

	DB((dbg, LEVEL_4, "ssa phi creation: link new phi %ld to block %ld\n", get_irn_node_nr(phi), get_irn_node_nr(block)));

	get_node_info(block)->link = phi;
	mark_irn_visited(block);

	/* set Phi predecessors */
	for(i = 0; i < n_cfgpreds; ++i) {
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		ir_node *pred_val   = search_def_and_create_phis(pred_block, mode);
		DB((dbg, LEVEL_4, "ssa phi pred:phi %ld, pred %ld\n", get_irn_node_nr(phi), get_irn_node_nr(pred_val)));
		set_irn_n(phi, i, pred_val);
	}

	return phi;
}

/**
 * Given a set of values this function constructs SSA-form for the users of the
 * first value (the users are determined through the out-edges of the value).
 * Uses the irn_visited flags. Works without using the dominance tree.
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
	get_node_info(orig_block)->link = orig_val;
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

		DB((dbg, LEVEL_4, "original user %ld\n", get_irn_node_nr(user)));

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

/* get the number of backedges without alien bes */
static int get_backedge_n(ir_node *loophead, unsigned with_alien)
{
	int i;
	int be_n = 0;
	int arity = get_irn_arity(loophead);
	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(loophead, i);
		if (is_backedge(loophead, i) && ( with_alien || is_in_loop(pred)) )
			++be_n;
	}
	return be_n;
}

/**
 * Sets the nodes backedges, according to its predecessors link.
 */
static void fix_backedge_info(ir_node *node)
{
	int i;
	for (i = 0; i < get_irn_arity(node); ++i)
	{
		ir_node *pred = get_irn_n(node, i);
		if (get_node_info(pred)->link != NULL)
			set_backedge(node, i);
		else
			set_not_backedge(node, i);
	}
}

/**
 *
 * ============= PEELING =====================================
 *
 */

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
			/* marks out edge as backedge */
			get_node_info(orgjmp)->link = orgjmp;
			for_each_phi(loophead, phi) {
				get_node_info( phi )->ins[lheadin_c] =	get_irn_n( phi, i) ;
			}
			++lheadin_c;

			loopheadnins[lheadin_c] = copyjmp;	/* former bes of the peeled code origin now from the loophead */
			/* marks out edge as normal edge */
			get_node_info(copyjmp)->link = NULL;
			/* get_irn_n( get_copy_of(phi), i) <!=> get_copy_of(get_irn_n( phi, i))
			 * Order is crucial! Predecessors outside of the loop are non existent.
			 * The copy (cloned with its ins!) has pred i,
			 * but phis pred i might not have a copy of itself.
			 */
			for_each_phi(loophead, phi) {
				//printf("normalbe phi %ld @ %d -> %ld\n", phi->node_nr, i,  get_irn_n( get_copy_of(phi), i)->node_nr);
				get_node_info( phi )->ins[lheadin_c] =	get_irn_n( get_copy(phi), i) ;
			}
			++lheadin_c;
		} else {
			peelheadnins[pheadin_c] = orgjmp;
			/* marks out edge as normal edge */
			get_node_info(orgjmp)->link = NULL;
			for_each_phi(peelhead, phi) {
				get_node_info( phi )->ins[pheadin_c] = get_irn_n(phi, i);
			}
			++pheadin_c;
		}
	}/* for */

	//DBG
	assert(pheadin_c == ARR_LEN(peelheadnins) &&
			lheadin_c == ARR_LEN(loopheadnins) &&
			"the constructed head arities do not match the predefined arities");

	/**
	 * assign the ins to the nodes
	 */
	set_irn_in(loophead, ARR_LEN(loopheadnins), loopheadnins);
	set_irn_in(peelhead, ARR_LEN(peelheadnins), peelheadnins);

	/* Fixes the backedge information according to the link.
	 * Following loop optimizations might depend on it. */
	fix_backedge_info(loophead);
	fix_backedge_info(peelhead);

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
 */
static ir_node *rawcopy_node(ir_node *node)
{
	ir_node *cp;
	node_info *cpstate;

	cp = exact_copy(node);
	set_copy(node, cp);
	cpstate = new_node_info();
	set_irn_link(cp, cpstate);
	mark_irn_visited(cp);
	return cp;
}

//int temp = 0;
//
///* This walker copies all walked nodes. The walk_condition determines the nodes to walk. */
//static void keepalives_walk(ir_node *node, walker_condition *walk_condition)
//{
//	int i;
//	int arity;
//	ir_graph *irg = current_ir_graph;
//
//	/**
//	 * break condition and cycle resolver, creating temporary node copies
//	 */
//	if (get_irn_visited(node) >= get_irg_visited(irg)) {
//		return;
//	}
//
//	/* Walk */
//	mark_irn_visited(node);
//
//	if (!is_Block(node)) {
//		ir_node *pred = get_nodes_block(node);
//		if (walk_condition(pred))
//			keepalives_walk( pred, walk_condition );
//	}
//
//	arity = get_irn_arity(node);
//
//	for (i = get_irn_arity(node) - 1; i >= 0; --i) {
//		ir_node *pred = get_irn_n(node, i);
//
//		if (walk_condition(pred))
//			keepalives_walk( pred, walk_condition );
//	}
//
//	add_End_keepalive(get_irg_end(current_ir_graph), node);
//}


/**
 * This walker copies all walked nodes.
 * If the walk_condition is true for a node, it is walked.
 * All nodes node_info->copy attributes has to be NULL prior to every to every walk.
 */
static void copy_walk(ir_node *node, walker_condition *walk_condition)
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
		DB((dbg, LEVEL_4, "copy_walk: We have already visited %ld\n", get_irn_node_nr(node)));
		if (node_info->copy == NULL) {
//			if (!is_Const(node) && !is_SymConst(node)) {
				cp = rawcopy_node(node);
//			} else {
//				cp = node;
//				node_info->copy = cp;
//			}
			DB((dbg, LEVEL_4, "The TEMP copy of %ld is created %ld\n", get_irn_node_nr(node), get_irn_node_nr(cp)));
		}
		return;
	}

//	add_End_keepalive(get_irg_end(current_ir_graph), node);

	/* Walk */
	mark_irn_visited(node);

	if (!is_Block(node)) {
		ir_node *pred = get_nodes_block(node);
		if (walk_condition(pred))
			DB((dbg, LEVEL_4, "walk block %ld\n", get_irn_node_nr(pred)));
			copy_walk( pred, walk_condition );
	}

	arity = get_irn_arity(node);

	NEW_ARR_A(ir_node *, cpin, arity);

	for (i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);

		if (walk_condition(pred)) {
			DB((dbg, LEVEL_4, "walk node %ld\n", get_irn_node_nr(pred)));
			copy_walk( pred, walk_condition );
			cpin[i] = get_copy(pred);
			DB((dbg, LEVEL_4, "copy of %ld gets new in %ld which is copy of %ld\n",
					get_irn_node_nr(node), get_irn_node_nr(get_copy(pred)), get_irn_node_nr(pred)));
		} else {
			cpin[i] = pred;
		}
	}

	/* copy node / finalize temp node */
	if (node_info->copy == NULL) {
		/* No temporary copy existent */

		/* Do not copy constants TODO right? */
//		if (!is_Const(node) && !is_SymConst(node)) {
			cp = rawcopy_node(node);
//		} else {
//			cp = node;
//			node_info->copy = cp;
//		}
		DB((dbg, LEVEL_4, "The FINAL copy of %ld is CREATED %ld\n", get_irn_node_nr(node), get_irn_node_nr(cp)));
	} else {
		/* temporary copy is existent but without correct ins */
		cp = get_copy(node);
		DB((dbg, LEVEL_4, "The FINAL copy of %ld is EXISTENT %ld\n", get_irn_node_nr(node), get_irn_node_nr(cp)));
	}

	if (!is_Block(node)) {
		ir_node *cpblock = get_copy(get_nodes_block(node));

		set_nodes_block(cp, cpblock );
		/* fix the phi information in attr.phis */
		if( is_Phi(cp) )
			add_Block_phi(cpblock, cp);
	} else {
		/* macroblock info has not been copied */
		set_Block_MacroBlock(cp, cp);
	}

	//TODO do?
	//set_irn_loop(cp, cur_loop);
	set_irn_in(cp, ARR_LEN(cpin), cpin);
}

/* Loop peeling, and fix the cf for the loop entry nodes, which have now more preds */
static void peel(out_edges *loop_outs)
{
	int i;
	ir_node **entry_buffer;
	int entry_c = 0;

	ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	NEW_ARR_A(ir_node *, entry_buffer, ARR_LEN(loop_outs));

	/* duplicate loop walk */
//	cur_head = loop_cf_head;
	inc_irg_visited(current_ir_graph);

	for(i = 0; i < ARR_LEN(loop_outs); i++) {
		out_edges entry = loop_outs[i];
		ir_node *node = entry.node;
		ir_node *pred = get_irn_n(entry.node, entry.pred_irn_n);

		if (is_Block(node)) {
			copy_walk( pred, is_in_loop );
			duplicate_preds(node, entry.pred_irn_n, get_copy(pred) );
		} else {
			copy_walk( pred, is_in_loop );
			if (!is_End(node))		/* leave out keepalives */
				/* Node is user of a value defined inside the loop.
				 * We'll need a phi since we duplicated the loop. */
				/* cannot construct_ssa here, because it needs another walker */
				entry_buffer[entry_c++] = pred;
		}
	}

	ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	/* Rewires the 2 heads */
	peel_fix_heads();

	/* Generate phis for values from peeled code and original loop */
	for(i = 0; i < entry_c; i++)
	{
		ir_node *cppred, *block, *cpblock, *pred;

		/* It is not possible to use
		 * pred = get_irn_n(entry.node, entry.pred_irn_n);
		 * because we might have changed the nodes predecessors in construct_ssa
		 */
		pred = entry_buffer[i];
		cppred = get_copy(pred);
		block = get_nodes_block(pred);
		cpblock = get_nodes_block(cppred);
		construct_ssa(block, pred, cpblock, cppred);
	}
}

/*
 * Populates head_entries with (node, pred_pos) tuple
 * whereas the node's pred at pred_pos is in the head but not the node itself.
 * Head and condition chain blocks must be marked.
 */
static void get_head_entries(ir_node *node, void *env)
{
	int i;
	int arity = get_irn_arity(node);
	(void) env;

	DB((dbg, LEVEL_5, "get head entries \n"));

	for(i = 0; i < arity; ++i) {
		/* node is not in the head, but the predecessor is.
		 * (head or loop chain nodes are marked) */
		DB((dbg, LEVEL_5, "... "));
		DB((dbg, LEVEL_5, "node %ld  marked %d (0)  pred %d marked %d (1) \n",
				node->node_nr, is_nodesblock_marked(node),i, is_nodesblock_marked(get_irn_n(node, i))));
		if (!is_nodesblock_marked(node) && is_nodesblock_marked(get_irn_n(node, i))) {
			out_edges entry;
			entry.node = node;
			entry.pred_irn_n = i;
			DB((dbg, LEVEL_4,
					"Found head chain entry %ld @%d because !inloop %ld and inloop %ld\n",
					node->node_nr, i, node->node_nr, get_irn_n(node, i)->node_nr));
			ARR_APP1(out_edges, cur_head_outs, entry);
		}
	}
}

/**
 * Find condition chains, and add them to be inverted, until the node count exceeds the limit.
 * A block belongs to the chain if a condition branches out of the loop.
 * Returns if the given block belongs to the condition chain.
 * FIXME prevent collecting ALL loop blocks (may happen if all blocks jump out of the loop)
 */
static unsigned condition_chains(ir_node *block) {
	const ir_edge_t *edge;
	unsigned mark = 0;
	int nodes_n = 0;

	printf("cd %ld\n", block->node_nr);

	/* we need all outs, including keeps (TODO firm function for that??) */
	foreach_out_edge_kind(block, edge, EDGE_KIND_NORMAL) {
		++nodes_n;
	}

	/* We do not want to collect more nodes from condition chains, than the limit allows us to.
	 * Also, leave at least one block as body. */
	if (head_inversion_node_count + nodes_n > head_inversion_node_limit
			|| loop_info.blocks == head_inversion_block_count + 1) {
		set_Block_mark(block, 0);
		printf(" %ld over limit\n", block->node_nr);
		return 0;
	}

	printf("blocks ++ %ld\n", block->node_nr);
//	++loop_info.blocks;

	/* First: check our successors, and add all succs that are outside of the loop to the list */
	foreach_block_succ(block, edge) {
		ir_node *src = get_edge_src_irn( edge );
		int pos = get_edge_src_pos( edge );

		printf("check %ld\n", src->node_nr);

		if (src->loop)
			printf(" src %ld in loop %ld  curlooop %ld \n", src->node_nr, src->loop->loop_nr, cur_loop->loop_nr);
		if (!is_in_loop(src)) {
			out_edges entry;
			printf(" src %ld @ %d into block %ld \n", src->node_nr, pos, block->node_nr);

			mark = 1;
			entry.node = src;
			entry.pred_irn_n = pos;
			ARR_APP1(out_edges, cond_chain_entries, entry);
			mark_irn_visited(src);
		}
	}

	/* this block is not part of the chain,
	 * because the chain would become too big or we have no succ outside of the loop */
	if (mark == 0) {
		printf("mark is 0 %ld\n", block->node_nr);
		set_Block_mark(block, 0);
		return 0;
	} else {
		printf("mark is 1 %ld\n", block->node_nr);
		set_Block_mark(block, 1);
		++head_inversion_block_count;
		DB((dbg, LEVEL_4, "block %ld is part of condition chain\n", get_irn_node_nr(block)));
		head_inversion_node_count += nodes_n;
	}

	/* Second: walk all successors, and add them to the list if they are not part of the chain */
	foreach_block_succ(block, edge) {
		unsigned inchain;
		ir_node *src = get_edge_src_irn( edge );
		int pos = get_edge_src_pos( edge );

		/* already done cases */
		if (!is_in_loop( src ) || (get_irn_visited(src) >= get_irg_visited(current_ir_graph))) {
//			printf("!inloop || visited %ld\n", block->node_nr);
			continue;
		}

		mark_irn_visited(src);
		DB((dbg, LEVEL_4, "condition chain walk %ld\n", get_irn_node_nr(src)));
		inchain = condition_chains( src );

		/* if successor is not part of chain we need to collect its outs */
		if ( !inchain ) {
			out_edges entry;
			entry.node = src;
			entry.pred_irn_n = pos;
			ARR_APP1(out_edges, cond_chain_entries, entry);
		}
	}
	return mark;
}

/**
 *
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
	int lhead_arity = headarity - backedges_n;
	int ihead_arity = backedges_n;

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
		if ( is_backedge(loophead, i) && !is_alien_edge(loophead, i) ) {
			invheadnins[iheadin_c] = pred;
			for_each_phi(invhead, phi) {
				get_node_info( phi )->ins[iheadin_c] =	get_irn_n( phi, i) ;
			}
			++iheadin_c;
		} else {
			/* just copy these edges */
			loopheadnins[lheadin_c] = pred;
			for_each_phi(loophead, phi) {
				get_node_info( phi )->ins[lheadin_c] = get_irn_n(phi, i);
			}
			++lheadin_c;
		}
	}/* for */

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


static void loop_inversion_walk(out_edges *head_entries)
{
	int i;
	ir_node *phi;
	int entry_c = 0;
	ir_node **entry_buffer;
	ir_node **head_phi_assign;

	NEW_ARR_A(ir_node *, entry_buffer, ARR_LEN(head_entries));

	head_phi_assign = NEW_ARR_F(ir_node *, 0);

	/* Find assignments in the condition chain, to construct_ssa for them after the loop inversion. */
	for_each_phi( loop_cf_head , phi) {
		for(i=0; i<get_irn_arity(phi); ++i) {
			ir_node *def = get_irn_n(phi, i);
			if ( is_nodesblock_marked(def) ) {
				ARR_APP1(ir_node *, head_phi_assign, def);
			}
		}
	}

	ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	/* duplicate condition chain */
	inc_irg_visited(current_ir_graph);

	for(i = 0; i < ARR_LEN(head_entries); ++i) {
		out_edges entry = head_entries[i];
		ir_node *node = entry.node;
		ir_node *pred = get_irn_n(entry.node, entry.pred_irn_n);

//		add_End_keepalive(get_irg_end(current_ir_graph), pred);

		if (is_Block(node)) {
			DB((dbg, LEVEL_4, "\nINIT walk block %ld\n", get_irn_node_nr(pred)));
			copy_walk(pred, is_nodesblock_marked);
			duplicate_preds(node, entry.pred_irn_n, get_copy(pred) );
		} else {
			DB((dbg, LEVEL_4, "\nInit walk node  %ld\n", get_irn_node_nr(pred)));
			copy_walk( pred, is_nodesblock_marked );

			/* ignore keepalives */
			if (!is_End(node))
				/* Node is user of a value assigned inside the loop.
				 * We'll need a phi since we duplicated the head. */
				entry_buffer[entry_c++] = pred;
		}
	}

	ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	inversion_fix_heads();

	/* Generate phis for users of values assigned in the condition chain and read in the loops body */
	for(i = 0; i < entry_c; i++) {
		ir_node *cppred, *block, *cpblock, *pred;

		/* It is not possible to use
		 * pred = get_irn_n(entry.node, entry.pred_irn_n);
		 * because we might have changed the nodes predecessors in construct_ssa
		 */
		pred = entry_buffer[i];
		cppred = get_copy(pred);
		block = get_nodes_block(pred);
		cpblock = get_nodes_block(cppred);
		DB((dbg, LEVEL_4,
				"construct_ssa (loop out value) original %ld and clone %ld\n",
				get_irn_node_nr(pred), get_irn_node_nr(cppred)));
		construct_ssa(block, pred, cpblock, cppred);


//		char *res;
//		char *s = "-SSA_";
//		char *n = strdup(" ");
//		n[0] = 'a' + (char)i;
//		res = strdup(s);
//		strcat(res, n);
//		dump_ir_block_graph(current_ir_graph, res );
	}

	/* Generate phis for values that are assigned in the condition chain
	 * but not read in the loops body.
	 */
	for(i = 0; i < ARR_LEN(head_phi_assign); ++i) {
		ir_node *def_block, *inhead_phi_def, *inv_def_block, *inv_inhead_phi_def;
		/* Note: construct_ssa only fixes the FIRST nodes usage. */
		inhead_phi_def = head_phi_assign[i];
		inv_inhead_phi_def = get_copy(inhead_phi_def);
		def_block = get_nodes_block(inhead_phi_def);
		inv_def_block = get_nodes_block(inv_inhead_phi_def);
		DB((dbg, LEVEL_4,
				"construct_ssa (condition chain out values) original %ld and clone %ld\n",
				get_irn_node_nr(inv_inhead_phi_def), get_irn_node_nr(inhead_phi_def)));
		construct_ssa(inv_def_block, inv_inhead_phi_def, def_block, inhead_phi_def);
	}
	loop_cf_head = get_copy(loop_cf_head);
}

/**
 * Decide if loop inversion, peeling or unrolling should be performed.
 * Inversion creates abnormal looking loops. Be careful with optimizations after that.
 */
static void decision_maker(void)
{
	unsigned do_peel = 0;
	unsigned do_inversion = 1;

	/* unsigned max_loop_opnodes = 2000000; */

	head_inversion_node_limit = 99910;

	cur_loop_outs = NEW_ARR_F(out_edges, 0);

	/* Find loop entries walk, find head */
	inc_irg_visited( current_ir_graph );
	irg_walk_graph( current_ir_graph, get_loop_outs_and_info, NULL, NULL );

	/* RETURN if there is no valid head */
	if (!loop_cf_head || !loop_cf_head_valid) {
		DB((dbg, LEVEL_1, "No valid loop head. Nothing done.\n"));
		return;
	}
#if 0
	/* RETURN if there is a call in the loop */
	if (loop_info.calls)
		return;

	/* Loop complexity too high */
	if (loop_info.opnodes_n > max_loop_opnodes)
		return;

//	foreach_out_edge(loop_cf_head, edge) {
//		ir_node *node = get_edge_src_irn(edge);
//		if ( !is_Block(node) && !is_Proj(node) && !is_Phi(node) )
//			++loop_info.opnodes_head;
//	}

	inc_irg_visited(current_ir_graph);
	loop_walker( loop_outs, NULL, get_invariants, NULL );

	/* This could be improved with knowledge about variable range. */
	if (loop_info.stores == 0 && loop_info.invariant_loads > 0)
		do_peel = 1;

#else
	(void) get_invariants;
#endif

	do_peel = 0;
	do_inversion = 1;

	/* Loop peeling */
	if (do_peel) {
		peel(cur_loop_outs);
		reset_node_infos();
	}

	DEBUG_ONLY(dump_ir_block_graph(current_ir_graph, "-peeled1"));

	DEL_ARR_F(cur_loop_outs);

	/* Loop inversion */
	/* Search for condition chains. We may not do this before peeling, as peeling changes things. */
	ir_reserve_resources(current_ir_graph, IR_RESOURCE_BLOCK_MARK);
	irg_walk_graph(current_ir_graph, unmark_block, NULL, NULL);

	loop_info.blocks = get_loop_n_blocks(cur_loop);
	cond_chain_entries = NEW_ARR_F(out_edges, 0);
	head_inversion_node_count = 0;
	head_inversion_block_count = 0;
	inc_irg_visited(current_ir_graph);
	set_Block_mark(loop_cf_head, 1);
	mark_irn_visited(loop_cf_head);
	/* find condition chains */
	condition_chains(loop_cf_head);

	DEBUG_ONLY(dump_ir_block_graph(current_ir_graph, "-pre_inversion"));

	// TODO assume number of phis to be created. prevent inversion in case ...

	/* Loop inversion */
	/* We catch endless loops here too,
	 * because they do not have a condition chain and a maximum of 1 block. */
	if (loop_info.blocks < 2) {
		do_inversion = 0;
		DB((dbg, LEVEL_1, "Loop contains %d (less than 2) blocks => No Inversion done.\n", loop_info.blocks));
	}

	if (head_inversion_block_count < 1) {
		do_inversion = 0;
		DB((dbg, LEVEL_1, "Loop contains %d (less than 1) invertible blocks => No Inversion done.\n", head_inversion_block_count));
	}


	if (do_inversion) {
		cur_head_outs = NEW_ARR_F(out_edges, 0);

		/* get all edges pointing into the head or condition chain */
		irg_walk_graph(current_ir_graph, get_head_entries, NULL, NULL);
		loop_inversion_walk(cur_head_outs);

		DEL_ARR_F(cur_head_outs);
	}

	DEBUG_ONLY(dump_ir_block_graph(current_ir_graph, "-inversed2"));

	/* FREE */
	DEL_ARR_F(cond_chain_entries);
	ir_free_resources(current_ir_graph, IR_RESOURCE_BLOCK_MARK);
}

/*  */
static void analyze_loop(ir_loop *loop)
{
	/* Init new for every loop */
	cur_loop = loop;

	loop_cf_head = NULL;
	loop_cf_head_valid = 1;
	loop_inv_head = NULL;
	loop_peeled_head = NULL;

	loop_info.calls = 0;
	loop_info.invariant_loads = 0;
	loop_info.loads = 0;
	loop_info.stores = 0;
	loop_info.opnodes_n = 0;
	loop_info.blocks = 0;

	DB((dbg, LEVEL_1, "  >>>> current loop includes node %ld <<<\n", get_irn_node_nr(get_loop_node(loop, 0))));

	decision_maker();

	DB((dbg, LEVEL_1, "    <<<< end of loop with node %ld >>>>\n", get_irn_node_nr(get_loop_node(loop, 0))));
}

/* Find most inner loops and send them to analyze_loop */
static void analyze_inner_loop(ir_loop *loop)
{
	/* descend into sons */
	int sons = get_loop_n_sons(loop);

	if (sons==0) {
		analyze_loop(loop);
	} else {
		int s;
		for(s=0; s<sons; s++) {
			analyze_inner_loop( get_loop_son(loop, s) );
		}
	}
}

/**
 *
 */
void loop_optimization(ir_graph *irg)
{
	ir_loop *loop;
	int     sons, nr;

	FIRM_DBG_REGISTER(dbg, "firm.opt.loop");

	DB((dbg, LEVEL_1, " >>> loop optimization (Startnode %ld) <<<\n", get_irn_node_nr(get_irg_start(irg))));

	/* Init */
	link_node_state_list = NULL;

	/* preconditions */
	edges_assure(irg);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);
	collect_phiprojs(irg);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	set_current_ir_graph(irg);
	assure_cf_loop(irg);

	/* allocate node_info for additional information on nodes */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_walk_graph(current_ir_graph, alloc_node_info, NULL, NULL);

	loop = get_irg_loop(irg);
	sons = get_loop_n_sons(loop);

	for (nr=0; nr<sons; nr++) {
		analyze_inner_loop(get_loop_son(loop, nr));
	}

	/* Free */
	free_node_info();
	ir_free_resources(irg, IR_RESOURCE_PHI_LIST|IR_RESOURCE_IRN_LINK);

	DB((dbg, LEVEL_1, " >>> loop optimization done (Startnode %ld) <<<\n", get_irn_node_nr(get_irg_start(irg))));
}

void do_loop_inversion(ir_graph *irg)
{
	/* TODO: add the code here that performs loop inversion only */
	loop_optimization(irg);
}

void do_loop_peeling(ir_graph *irg)
{
	/* TODO: add the code here that performs loop peeling only */
	loop_optimization(irg);
}

void firm_init_loop_opt(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.loop");
}
