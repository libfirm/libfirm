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

//#include "config.h"

//#include <limits.h>
#include <assert.h>

#include "irnode.h"
#include "irnode_t.h"
#include "irgraph_t.h"
//#include "irprog_t.h"

//#include "iroptimize.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irgopt.h"
//#include "irgmod.h"
#include "irgwalk.h"

//#include "array_t.h"
#include "list.h"
//#include "pset.h"
//#include "pmap.h"
//#include "pdeq.h"
//#include "xmalloc.h"
//#include "pqueue.h"

#include "irouts.h"
#include "irloop_t.h"
#include "irbackedge_t.h"
//#include "opt_inline_t.h"
//#include "cgana.h"
//#include "trouts.h"
//#include "error.h"

//#include "analyze_irg_args.h"
#include "iredges_t.h"
//#include "irflag_t.h"
//#include "irhooks.h"
#include "irtools.h"
//#include "iropt_dbg.h"
#include "irpass_t.h"
#include "irloop.h"

#include "array_t.h"
#include "irdump.h"


/* convenience macro iterating over every phi node of the block */
#define for_each_phi(block, phi) \
	for ( (phi) = get_Block_phis( (block) ); (phi) ; (phi) = get_Phi_next( (phi) ) )

ir_loop *cur_loop;

/* The loop walker should be possible to abort if nothing can be done anymore */
typedef unsigned irg_walk_func_abortable(ir_node *, void *);

/* stores pair of node and number for nodes predecessor */
typedef struct loop_entry_t {
	ir_node *node;			/* node outside of the loop */
	int pred_irn_n;			/* with pred_irn_n pointing inside loop */
} loop_entry_t;

/* Store complex values in the nodes link */
//TODO optimize. Every node has these values and seldom many otm are used.
typedef struct link_node_state_t {
	unsigned cloned:1;
	unsigned temp:1;
	unsigned invariant:1;
	ir_node *link;
	ir_node *ssalink;	/* we will have to keep the link to the copies, as well as have temporary links for ssa creation */
	ir_node **ins;		/* ins for phi nodes, during rewiring of blocks */
	// TODO omit ins. can be replaced by new ins and newunknown ins for each
} link_node_state_t;


loop_entry_t *loop_entries;		/* loop entries (from below) in the node graph */
loop_entry_t *backedges;		/* backedges exclusively from the current loop */
loop_entry_t *alien_backedges;	/* The head can be head of several loops. */
loop_entry_t *head_edges;	/* The head can be head of several loops. */

ir_node *loop_cf_head = NULL;		/* loop exit in the node graph */
unsigned loop_cf_head_valid = 1;	/* a loop may/must have one head, otherwise invalid */

unsigned has_sto = 0;				/* If we store inside the loop we might
									 * have disambiguation problems */
//DBG
//void arrdump(ir_node **arr)
//{
//	int i;
//	for (i=0; i<ARR_LEN(arr); i++)
//	{
//		printf("inarr: %ld          is block %d \n", (arr[i]->node_nr), is_Block(arr[i]));
//	}
//}

/**
 * Returns the state of the given node.
 */
link_node_state_t *get_lstate(ir_node *n)
{
	return ((link_node_state_t *)n->link);
}

/**
 * Returns the link inside of the nodes state which is pointing to its copy
 * most of the time during loop peeling.
 */
ir_node *get_copy_of(ir_node *n)
{
	return ((link_node_state_t *)n->link)->link;
}

/**
 * Returns true if the node or block is in cur_loop.
 */
unsigned is_in_loop(ir_node *node)
{
//	if (is_Block(node)) {
//		if (node->loop == cur_loop) {
//			printf("                                   INLOOP %ld \n", node->node_nr);
//		}
//		return (node->loop == cur_loop);
//	} else {
//		if ( get_nodes_block(node)->loop == cur_loop ) {
//			printf("                                   INLOOP %ld \n", node->node_nr);
//		}
//		return ( get_nodes_block(node)->loop == cur_loop );
//	}
	if (is_Block(node)) {
		return (node->loop == cur_loop);
	} else {
		return ( get_nodes_block(node)->loop == cur_loop );
	}
}

/**
 * Returns if the given be is an alien edge
 */
unsigned is_alien_edge(ir_node *n, int i)
{
	return(	!is_in_loop( get_irn_n( n, i ) ) );
}

static void add_pred(ir_node* node, ir_node* x)
{
	ir_node** ins;
	int n;
	int i;

//	if(!node)
//		printf("NONODE\n");

	//printf("addpred %ld   pred %ld \n", node->node_nr, x->node_nr);

	// WHY limit it to blocks and phi?
	//assert(is_Block(node) || is_Phi(node));

	n = get_irn_arity(node);
	NEW_ARR_A(ir_node*, ins, n + 1);
	for (i = 0; i < n; i++)
		ins[i] = get_irn_n(node, i);
	ins[n] = x;
	set_irn_in(node, n + 1, ins);
}

void block_phi_walker(ir_node *n, void *env)
{
	const ir_edge_t *edge;
	(void) env;

	/* RETURN */
	if (!is_Block(n))
		return;

	/* generate phi list for every block */
	n->attr.block.phis = NULL;

	foreach_out_edge(n, edge) {
		ir_node *src = get_edge_src_irn(edge);
		if (is_Phi(src))
		{
			//printf("%ld has phi %ld \n", block->node_nr, src->node_nr);
			add_Block_phi(n, src);
		}
	}
}

/**
 * Calls func() for every block in the given loop.
 */
void for_each_loop_block(ir_loop *loop, irg_walk_func *func, void *env)
{
	int elements, e;
	elements = get_loop_n_elements(loop);

	for(e=0; e<elements; e++)
	{
		loop_element elem = get_loop_element(loop, e);
		if  (is_ir_node(elem.kind) && is_Block(elem.node) )
		{
			//printf(" foreach LOOPBLOCK %ld \n", elem.node->node_nr);
			func(elem.node, env);
		}
	}
}

/**
 * collects the blocks backedges and creates the phi list for every block
 */
void collect_backedges(ir_node *block, void *env)
{
	(void) env;

	printf("LOOP BLOCK %ld\n", block->node_nr);

	/* collect backedges */
	if (has_backedges(block))
	{
		int i;
		int arity = get_irn_arity(block);

		for(i = 0; i < arity; ++i) {
			ir_node *pred = get_irn_n(block, i);

			loop_entry_t be;
			be.node = block;
			be.pred_irn_n = i;

			ARR_APP1(loop_entry_t, head_edges, be);

			if (is_backedge(block, i) )
			{
				if ( is_in_loop(pred) ) {
					//printf("be: %ld --> %ld \n", block->node_nr, pred->node_nr);
					ARR_APP1(loop_entry_t, backedges, be);
				} else {
					//printf("alien be: %ld --> %ld \n", block->node_nr, pred->node_nr);
					ARR_APP1(loop_entry_t, alien_backedges, be);
				}
			}
//			else {
//				if ( !is_in_loop(pred) ) {
//					ARR_APP1(loop_entry_t, head_edges, be);
//			}

		}
	}
}

/**
 *	Walks through all loop nodes.
 */
unsigned loop_walker_rec(ir_node *node,
		irg_walk_func_abortable *pre,
		irg_walk_func_abortable *post, void * env)
{
	int i;
	unsigned stop = 0;

	ir_graph *irg = current_ir_graph;

	/* RETURN if we walked out of the loop*/
	if (!is_in_loop(node))
		return 0;

	if (pre)
	{
		unsigned stop = pre(node, env);
		if (stop)
			return stop;
	}

	set_irn_visited(node, irg->visited);

	if (node->op != op_Block) {
		ir_node *pred = get_irn_n(node, -1);
		if (pred->visited < irg->visited)
		{
			stop = loop_walker_rec(pred, pre, post, env);
			if (stop)
				return stop;
		}
	}

	for (i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);
		if (pred->visited < irg->visited)
		{
			stop = loop_walker_rec(pred, pre, post, env);
			if (stop)
				return stop;
		}
	}

	if (post)
		return post(node, env);
	return 0;
}

/**
 * Walks through loop nodes.
 * The entries of the loop (all edges pointing into the loop) have to be given.
 */
unsigned loop_walker(loop_entry_t *entries,
		irg_walk_func_abortable *pre, irg_walk_func_abortable *post, void * env)
{
	int i;
	int stop = 0;

	for (i=0; !stop && i<ARR_LEN(entries); i++)
	{
		ir_node *pred;
		loop_entry_t entry;
		entry = entries[i];

		pred = get_irn_n( entry.node , entry.pred_irn_n);

		stop = loop_walker_rec( pred, pre, post, env);
	}
	return stop;
}

/**
 *
 */
void find_loop_entries_walk(ir_node *node, void *env)
{
	unsigned node_in_loop, pred_in_loop;
	(void) env;

	link_node_state_t *state = XMALLOCZ(link_node_state_t);
	node->link = (void *)state;

	int i, arity;
	arity = get_irn_arity(node);
	for (i = 0; i < arity; i++) {
		ir_node *pred = get_irn_n(node, i);

		pred_in_loop = is_in_loop(pred);
		node_in_loop = is_in_loop(node);

		//Find the loops head/the blocks with cfpred outside of the loop
		if (is_Block(node) && node_in_loop
				&& !pred_in_loop && loop_cf_head_valid)
		{
			ir_node *cfgpred = get_Block_cfgpred(node, i);

			if ( !is_in_loop(cfgpred) )
			{
				//another head? We do not touch this.
				if (loop_cf_head && loop_cf_head != node)
				{
					loop_cf_head_valid = 0;
				}
				else
				{
					loop_cf_head = node;
				}
			}
		}

		if ( pred_in_loop && !node_in_loop )
		{
			/* we walked right into the loop. */
			loop_entry_t entry;
			entry.node = node;
			entry.pred_irn_n = i;

			//DBG
//			printf("inloop: %ld --> inloop %ld (@ %d)  \n",
//					node->node_nr, pred->node_nr, i);

			ARR_APP1(loop_entry_t, loop_entries, entry);
		}
	}
}

// TODO needed?
///**
// * Finds invariant nodes and marks them as invariant.
// * (Post walk)
// */
//unsigned get_invariants(ir_node *node, void *env)
//{
//	unsigned invar = 1;
//	(void) env;
//
//	if (is_Store(node))
//	{
//		has_sto = 1;
//		/* RETURN and abort walker */
//		return 1;
//	}
//
//	int arity = get_irn_arity(node);
//
//	/* RETURN, no preds to visit */
//	if (arity == 0) return 0;
//
//	if (is_Load(node))
//	{
//		assert(arity>=2 && "expected load to have edge nr 1 (address)");
//
//		ir_node *pred = get_irn_n(node, 1);
//		if (!is_in_loop(pred)			/* Everything outside the loop is considered invariant */
//				|| is_Const(pred)		/* This is not true, but we also want the quasi-invariants. */
//				|| is_SymConst(pred)
//				|| get_lstate(node)->invariant)
//		{
//			//printf("## CONSTLOAD: %ld \n", node->node_nr);
//			get_lstate(node)->invariant = 1;
//		} else
//		{
//			get_lstate(node)->invariant = 0;
//		}
//	}
//	else
//	{
//		int i;
//		invar = 1;
//		/* find loop variant preds */
//		for(i = 0; i < arity; ++i)
//		{
//			ir_node *pred = get_irn_n(node, i);
//
//			if ( !(!is_in_loop(pred)	/* outside loop is loop invariant */
//					|| is_Const(pred)			/* constants */
//					|| is_SymConst(pred)		/* SymConst, if no Store */
//					|| get_lstate(node)->invariant /* pred is marked as invariant */
//					) )
//			{
//				invar = 0;
//			}
//		}
//
//		if (invar) {
//			printf("const: %ld \n", node->node_nr);
//			get_lstate(node)->invariant = 1;
//		} else {
//			get_lstate(node)->invariant = 0;
//		}
//// DBG
////				if (!is_nodes_block_marked(pred)) {
////					//printf("pred outloop: %ld, pred %ld (const)\n", node->node_nr, pred->node_nr);
////				} else if (is_Const(pred) || is_SymConst(pred)) // || is_Phi(pred)) {
////					//printf("predconst: %ld, pred %ld CONST\n", node->node_nr, pred->node_nr);
////				} else if (pred->link == MARKED_CONST) {
////					//printf("predmarked: %ld, pred %ld const\n", node->node_nr, pred->node_nr);
////				} else {
////					mark=0;
////				}
//	}
//	return 0;
//}

////TODO DBG Remove
void phifix(ir_node *node, ir_node *newpred)
{
	ir_node *phi=get_Block_phis(node);
	while(phi)
	{
		int pa = get_irn_arity(phi);
		int ba = get_irn_arity(node);



		while(ba>pa)
		{
			printf("!!!!!!!!!! block has %d, phi had %d\n", ba, pa );
			add_pred(phi, newpred);
			pa++;
			printf("!!!!!!!!!! block has %d, phi has now %d\n", ba, pa );
		}
		phi=get_Phi_next(phi);
	}
}

static ir_node *ssa_second_def;
static ir_node *ssa_second_def_block;

static ir_node *search_def_and_create_phis(ir_node *block, ir_mode *mode,
                                           int first)
{
	int i;
	int n_cfgpreds;
	ir_graph *irg;
	ir_node *phi;
	ir_node **in;

	/* This is needed because we create bads sometimes */
	if (is_Bad(block))
		return new_Bad();

	/* the other defs can't be marked for cases where a user of the original
	 * value is in the same block as the alternative definition.
	 * In this case we mustn't use the alternative definition.
	 * So we keep a flag that indicated wether we walked at least 1 block
	 * away and may use the alternative definition */
	if (block == ssa_second_def_block && !first) {
		return ssa_second_def;
	}

	/* already processed this block? */
	if (irn_visited(block)) {
		ir_node *value = get_lstate(block)->ssalink;
		return value;
	}

	irg = get_irn_irg(block);
	assert(block != get_irg_start_block(irg));

	/* a Block with only 1 predecessor needs no Phi */
	n_cfgpreds = get_Block_n_cfgpreds(block);
	if (n_cfgpreds == 1) {
		ir_node *pred_block = get_Block_cfgpred_block(block, 0);
		ir_node *value      = search_def_and_create_phis(pred_block, mode, 0);

		get_lstate(block)->ssalink = value;
		//set_irn_link(block, value);
		mark_irn_visited(block);
		return value;
	}

	/* create a new Phi */
	NEW_ARR_A(ir_node*, in, n_cfgpreds);
	for(i = 0; i < n_cfgpreds; ++i)
		in[i] = new_Unknown(mode);

	phi = new_r_Phi(block, n_cfgpreds, in, mode);
	//set_irn_link(block, phi);
	get_lstate(block)->ssalink = phi;
	mark_irn_visited(block);

	/* set Phi predecessors */
	for(i = 0; i < n_cfgpreds; ++i) {
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		ir_node *pred_val   = search_def_and_create_phis(pred_block, mode, 0);

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

	/* no need to do anything */
	if (orig_val == second_val)
		return;

	irg = get_irn_irg(orig_val);
	inc_irg_visited(irg);

	mode = get_irn_mode(orig_val);
	get_lstate(orig_block)->ssalink = orig_val;
	//set_irn_link(orig_block, orig_val);
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

		//DB((dbg, LEVEL_3, ">>> Fixing user %+F (pred %d == %+F)\n", user, j, get_irn_n(user, j)));

		if (is_Phi(user)) {
			ir_node *pred_block = get_Block_cfgpred_block(user_block, j);
			newval = search_def_and_create_phis(pred_block, mode, 1);
		} else {
			newval = search_def_and_create_phis(user_block, mode, 1);
		}

		/* don't fix newly created Phis from the SSA construction */
		if (newval != user) {
			//DB((dbg, LEVEL_4, ">>>> Setting input %d of %+F to %+F\n", j, user, newval));
			set_irn_n(user, j, newval);
		}
	}
}



/**
 * Rewires the heads after peeling
 */
void fix_head(ir_node *loophead)
{
	int headarity = get_irn_arity(loophead);
	int i;
	ir_node **loopheadnins;
	ir_node **peelheadnins;
	ir_node *phi;
	ir_node *peelhead = get_copy_of(loophead);
	int lheadin_c = 0;
	int pheadin_c = 0;

	/**
	 * the loopheads new preds are:
	 * its own backedge(s) and the former backedge(s) of the peeled code
	 */
	int lhead_arity = 2 * ARR_LEN(backedges);
	int phead_arity = headarity - ARR_LEN(backedges);

	NEW_ARR_A(ir_node *, loopheadnins, lhead_arity );
	NEW_ARR_A(ir_node *, peelheadnins, phead_arity );

	phi = get_Block_phis(loophead);
	while(phi) {
		NEW_ARR_A(ir_node *, get_lstate(phi)->ins, lhead_arity);
		phi=get_Phi_next(phi);
	}

	phi = get_Block_phis(peelhead);
	while(phi)
	{
		NEW_ARR_A(ir_node *, get_lstate(phi)->ins, phead_arity);
		phi=get_Phi_next(phi);
	}

	for (i = 0; i < headarity; i++)
	{
		ir_node *phi;
		ir_node *orgjmp = get_irn_n(loophead, i);
		ir_node *copyjmp = get_copy_of(orgjmp);

		/**
		 * Rewire the head blocks ins and their phi ins.
		 * Requires blocks phi list.
		 *
		 * 1. Alien bes origin from the peeled head (new head of the whole loop)
		 * 2. Loops own bes must be kept/copied to the loophead.
		 * 3. All other edges origin from the peeled head (new head of the loop)
		 */


		//printf("head i %d\n", i);

		if (is_backedge(loophead, i))
		{
			if (is_alien_edge(loophead, i)) {
				peelheadnins[pheadin_c] = orgjmp;	/* alien bes go to the peeled head */
				//set_backedge(peelhead, pheadin_c);

				// alien bes origin at the peeled head
				for_each_phi(peelhead, phi)
				{
					//printf("alienbe phi %ld @ %d -> %ld\n", phi->node_nr, i, get_irn_n(phi, i)->node_nr);
					get_lstate( phi )->ins[pheadin_c] = get_irn_n(phi, i);
				}
				//printf("alienbe %ld @ %d -> add to peelhead orgjump %ld\n", peelhead->node_nr, i, orgjmp->node_nr);
				++pheadin_c;
			} else {
				loopheadnins[lheadin_c] = orgjmp;	/* keep/copy the loops own bes */
				//set_backedge(loophead, lheadin_c);

				for_each_phi(loophead, phi) {
					//printf("normalbe phi %ld @ %d -> %ld\n", phi->node_nr, i, get_irn_n(phi, i)->node_nr);
					get_lstate( phi )->ins[lheadin_c] = get_irn_n(phi, i);
				}
				//printf("normalbe %ld @ %d -> add to loophead orgjump %ld\n", loophead->node_nr, i, orgjmp->node_nr);
				++lheadin_c;

				loopheadnins[lheadin_c] = copyjmp;	/* former bes of the peeled code origin now from the loophead */
				//set_not_backedge(loophead, lheadin_c);

				/* get_irn_n( get_copy_of(phi), i) <!=> get_copy_of(get_irn_n( phi, i))
				 * Order is crucial! Preds outside of the loop are non existent, like Const.
				 */
				for_each_phi(loophead, phi) {
					//printf("normalbe phi %ld @ %d -> %ld\n", phi->node_nr, i,  get_irn_n( get_copy_of(phi), i)->node_nr);
					get_lstate( phi )->ins[lheadin_c] =	get_irn_n( get_copy_of(phi), i) ;
				}
				//printf("normalbe %ld @ %d -> add to loophead copyjump %ld\n", loophead->node_nr, i, copyjmp->node_nr);
				++lheadin_c;
			}
		} else {
			peelheadnins[pheadin_c] = orgjmp;
			//set_not_backedge(peelhead, pheadin_c);

			for_each_phi(peelhead, phi) {
				//printf("edge phi %ld @ %d -> %ld\n", phi->node_nr, i, get_irn_n( phi, i)->node_nr);
				get_lstate( phi )->ins[pheadin_c] = get_irn_n(phi, i);
			}
			//printf("edge %ld @ %d -> add to peelhead orgjump %ld\n", peelhead->node_nr, i, orgjmp->node_nr);
			++pheadin_c;
		}
	}/* for */

//	printf("pheadin %d  arr %d            lheadin %d arr %d \n",
//			pheadin_c, ARR_LEN(peelheadnins),
//			lheadin_c, ARR_LEN(loopheadnins));

	assert(pheadin_c == ARR_LEN(peelheadnins) &&
			lheadin_c == ARR_LEN(loopheadnins) &&
			"the number of head elements does not match the predefined one");

	set_irn_in(loophead, ARR_LEN(loopheadnins), loopheadnins);
	set_irn_in(peelhead, ARR_LEN(peelheadnins), peelheadnins);

	for_each_phi(loophead, phi) {
		ir_node **ins = get_lstate( phi )->ins;
		set_irn_in(phi, lhead_arity, ins);
	}

	for_each_phi(peelhead, phi) {
		ir_node **ins = get_lstate( phi )->ins;
		set_irn_in(phi, phead_arity, ins);
	}
}


/**
 * Peels the loop by copying the contents. Graph needs some rewiring after that.
 */
void peel_walk(ir_node *node, void *env)
{
	int i;
	int arity;
	ir_node *cp;
	ir_node **cpin;
	ir_graph *irg = current_ir_graph;
	link_node_state_t *cpstate;
	(void) env;

	link_node_state_t *nodestate = get_lstate(node);

	/**
	 * break condition and cycle resolver, creating temporary node copies
	 */
	if (node->visited >= irg->visited)
	{
		if (!nodestate->cloned && !nodestate->temp)
		{
			/** temporary clone this node
			 * because we were here before and would walk into a cycle
			 */
			cp = exact_copy(node);
			//DBG
			//printf("COPY TEMP : %ld -T> %ld \n", node->node_nr, cp->node_nr);
			nodestate->link = cp;
			if (is_Block(cp))
				cp->loop = NULL;
			cpstate = XMALLOCZ(link_node_state_t);
			cp->link = cpstate;
			nodestate->temp=1;
			set_irn_visited(cp, irg->visited);
		}
		return;
	}
	//printf("  -----  WALK %ld -----  \n", node->node_nr);

	/**
	 * WALK
	 */
	set_irn_visited(node, irg->visited);

	if ( !is_Block(node) ) {
		ir_node *pred = get_irn_n(node, -1);
		if (is_in_loop(pred))
			peel_walk(pred, NULL);
	}

	arity = get_irn_arity(node);

	NEW_ARR_A(ir_node *, cpin, arity);

	for (i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);

		if (is_in_loop(pred))
		{
			peel_walk(pred, NULL);
			cpin[i] = get_lstate(pred)->link;
			//printf("copy of %ld gets in %ld", node->node_nr, cpin[i]->node_nr);
		} else {
			cpin[i] = pred;

		}
		//printf("copy of %ld gets in %ld \n", node->node_nr, cpin[i]->node_nr);
	}

	/**
	 *  copy node / finalize temp node
	 */
	if (!nodestate->temp)
	{
//		if (!is_Const(node) && !is_SymConst(node)) {
			cp = exact_copy(node);
			//DBG
			//printf("COPY FINAL: %ld -F> %ld \n", node->node_nr, cp->node_nr);
			nodestate->link = cp;
			cpstate = XMALLOCZ(link_node_state_t);
			cp->link = cpstate;
			if (is_Block(cp))
				cp->loop = NULL;
			set_irn_visited(cp, irg->visited);
//		} else {
//			cp = node;
//			//DBG
//			printf("CONST FINAL: %ld -F> %ld \n", node->node_nr, cp->node_nr);
//			nodestate->link = cp;
//		}
	} else {
		/* temporary copy is existent but without correct ins */
		cp = nodestate->link;
		//printf("FINALIZE: %ld \n", cp->node_nr);
	}

	//TODO REM
	//add_End_keepalive(get_irg_end(current_ir_graph), cp );

	if (!is_Block(node))
	{
		ir_node *cpblock = get_copy_of(get_nodes_block(node));

		/* set the block of the copy to the copied block */
		//printf("    PRE  NODE %ld   BLOCK %ld \n", cp->node_nr, get_nodes_block(cp)->node_nr);
		set_nodes_block(cp, cpblock );
		//printf("    POST NODE %ld   BLOCK %ld \n", cp->node_nr, get_nodes_block(cp)->node_nr);

		/* fix the phi information in attr.phis (does not add the phi node to the block) */
		if( is_Phi(cp) )
		{
			add_Block_phi(cpblock, cp);
			//printf("PHI-BLOCK block %ld got its phi %ld\n", cpblock->node_nr, cp->node_nr);
		}
	}
	else {
		/* macroblock info is not copied */
		set_Block_MacroBlock(cp, cp);
	}

	//dbg valid ins?
//	for(i=0; i<ARR_LEN(cpin); i++)
//		printf(" cpin of %ld (cp %ld):  %ld \n ", node->node_nr, cp->node_nr, cpin[i]->node_nr);

	set_irn_in(cp, ARR_LEN(cpin), cpin);

//	for(i=0; i< ARR_LEN(cpin); i++)
//	{
//		printf("ins %ld: %ld \n", cp->node_nr, cpin[i]->node_nr);
//	}

//TODO REM
//	if (!nodestate->temp)
//	{
//		nodestate->link = cp;
//		cpstate = XMALLOCZ(link_node_state_t);
//		cp->link = cpstate;
//	} else {
//		/* temporary copy is existent but without correct ins */
//		cp = nodestate->link;
//	}


	nodestate->temp = 0;
	nodestate->cloned = 1;
}

//void chklink (ir_node *n, void * e)
//{
//	ir_node *link = n->link;
//	link_node_state_t *l = (link_node_state_t *)link;
//
//	printf("n %ld\n", n->node_nr);
//	printf("l p %ld\n", l->link);
//	if (l->link)
//		printf("l %ld\n", l->link->node_nr);
//
//}

/**
 * Loop peeling, and fix the cf for the loop entry nodes, which have now more preds
 */
void peel(void)
{
	int i;
	ir_node **entry_buffer;
	int entry_c = 0;
	int entry_i;

	NEW_ARR_A(ir_node *, entry_buffer, ARR_LEN(loop_entries));

	for(i = 0; i < ARR_LEN(loop_entries); i++)
	{
		loop_entry_t entry = loop_entries[i];
		ir_node *node = entry.node;
		ir_node *pred = get_irn_n(entry.node, entry.pred_irn_n);

		if (is_Block(node)) {
			/* node is block and the given pred points inside the loop  */
			ir_node *cppred;

			peel_walk( pred, 0);

			// leave keepalives out
			if (is_End(node) && (is_Block(pred) || is_Phi(pred)) ) {
				//add_End_keepalive(get_irg_end(current_ir_graph), get_copy_of(pred) );
			} else {
				cppred = get_copy_of(pred);
				//printf("fix block entry %ld to cp %ld\n", node->node_nr, cppred->node_nr);
				add_pred( node, cppred );
				//printf("fix block entry %ld to cp %ld\n", node->node_nr, cppred->node_nr);
			}

			//add_End_keepalive(get_irg_end(current_ir_graph), get_copy_of(pred) );

			//DBG
			//phifix(node, cppred);
		} else {
			/* node is somewhere in the graph, outside of the loop */
			//ir_node *cppred;
			//ir_node *block;
			//ir_node *cpblock;
			peel_walk( pred, 0);

			// no ssa for keepalives
			if (is_End(node) && (is_Block(pred) || is_Phi(pred)) ) {
				//add_End_keepalive(get_irg_end(current_ir_graph), get_copy_of(pred) );
			} else {
				//printf("fix entry %ld to %ld\n", node->node_nr, pred->node_nr);
				entry_buffer[entry_c++] = pred;
			}

			//add_End_keepalive(get_irg_end(current_ir_graph), get_copy_of(pred) );

			// cannot construct_ssa here, because it needs another walker

		} /* is block */
	} /* for */

	//irg_walk_graph(current_ir_graph, chklink, NULL, NULL);

	fix_head(loop_cf_head);

	//printf (" FIXHEAD DONE :D \n");

	entry_i = 0;

	/* Generate phis for values from peeled code and original loop */
	for(i = 0; entry_i < entry_c; i++)
	{
		loop_entry_t entry = loop_entries[i];
		ir_node *node = entry.node;

		if (is_Block(node))
		{
			/* block */
			ir_node *phi=get_Block_phis(node);

			while(phi)
			{
				add_pred(phi, entry_buffer[entry_i++]);
				phi=get_Phi_next(phi);
			}
		} else {
			/* not block */

			ir_node *cppred, *block, *cpblock, *pred;

			/**
			 * pred = get_irn_n(entry.node, entry.pred_irn_n);
			 * does not work, because we could have changed the nodes preds in construct_ssa
			 */

			pred = entry_buffer[entry_i++];

			//printf("pred %ld\n", pred->node_nr);
			cppred = get_copy_of(pred);
			//printf("cppred %ld\n", cppred->node_nr);
			block = get_nodes_block(pred);
			//printf("block %ld\n", block->node_nr);
			cpblock = get_nodes_block(cppred);
			//printf("cpblock %ld\n", cpblock->node_nr);


			//dump_ir_block_graph(current_ir_graph, "vorher");
			construct_ssa(block, pred, cpblock, cppred);
			//add_End_keepalive(get_irg_end(current_ir_graph), cppred);


			//add_pred(get_irg_end(current_ir_graph), cppred);
			//dump_ir_block_graph(current_ir_graph, "nachher");


		}
	}
}

void decision_maker(void)
{
	//inc_irg_visited(current_ir_graph);
	//loop_walker( loop_entries, NULL, get_invariants, NULL );

	inc_irg_visited(current_ir_graph);
	peel();

}


/**
 * TODO use list , not arr_F
 */
void analyze_loop(ir_loop *loop)
{
	/* Init new for every loop */
	loop_cf_head = NULL;
	loop_cf_head_valid = 1;
	has_sto = 0;

	cur_loop = loop;

	/* arrays */
	backedges = NEW_ARR_F(loop_entry_t, 0);
	alien_backedges = NEW_ARR_F(loop_entry_t, 0);
	loop_entries = NEW_ARR_F(loop_entry_t, 0);
	head_edges = NEW_ARR_F(loop_entry_t, 0);

	inc_irg_visited( current_ir_graph );
	irg_walk_graph( current_ir_graph, block_phi_walker, NULL, NULL );

	/* Collect all backedges */
	for_each_loop_block(loop, collect_backedges, NULL );

	/* Find loop entries walk, find head */
	inc_irg_visited( current_ir_graph );
	irg_walk_graph( current_ir_graph, find_loop_entries_walk, NULL, NULL );

	/* RETURN if there is no valid head */
	if (!loop_cf_head || !loop_cf_head_valid)
	{
		//DBG printf("NOTE: There is no valid loop head. Nothing done.\n");
		return;
	}

	decision_maker();

	// TODO free all link states... or better put them on functionstack

	/* FREE */
	DEL_ARR_F(loop_entries);
	DEL_ARR_F(backedges);
	DEL_ARR_F(alien_backedges);
	DEL_ARR_F(head_edges);

	//dump_ir_block_graph(current_ir_graph, "-lu1");
}

/**
 *	Find most inner loops and send them to analyze_loop
 */
void analyze_inner_loop(ir_loop *loop)
{
	/* descend into sons */
	int sons = get_loop_n_sons(loop);

	//printf("found %d loops \n", sons);

	if (sons==0)
	{
		//printf("analyze loop %ld\n", loop->loop_nr);
		analyze_loop(loop);
	}
	else
	{
		int s;
		for(s=0; s<sons; s++)
		{
			//printf("descend into loop son %ld\n", get_loop_son(loop, s)->loop_nr);
			analyze_inner_loop( get_loop_son(loop, s) );
		}
	}
}



//
//void phicheck(ir_node *node, void * env)
//{
//	if (!is_Block(node)) return;
//
//	ir_node *phi=get_Block_phis(node);
//	while(phi)
//	{
//		if (!is_Phi(phi))
//		{
//			printf("NOT PHI %ld\n", phi->node_nr);
//			phi = NULL;
//		} else {
//			phi=get_Phi_next(phi);
//		}
//	}
//}

void loop_unroll(ir_graph *irg)
{
	//printf(" --- loop unroll start --- \n");

	//irg_walk_graph(irg, phicheck, NULL, NULL);

	ir_loop *loop;

	assure_cf_loop(irg);

	loop = get_irg_loop(irg);
	int sons = get_loop_n_sons(loop);
	//printf("FOUND %d LOOPS \n", sons);
	int nr;
	for (nr=0; nr<sons; nr++)
	{
		analyze_inner_loop( get_loop_son(loop, nr) );
	}

	//DBG
	//printf(" --- loop unroll done --- \n");
}

struct loop_unroll_pass_t {
	ir_graph_pass_t pass;
};

/**
 * Wrapper to run ...() as a ir_prog pass.
 */
static int loop_unroll_wrapper(ir_graph *irg, void *context) {

	(void)context;
	loop_unroll(irg);
	return 0;
}

//TODO ??
ir_graph_pass_t *loop_unroll_pass(const char *name)
{
	struct loop_unroll_pass_t *pass =
		XMALLOCZ(struct loop_unroll_pass_t);

	return def_graph_pass_constructor(
		&pass->pass, name ? name : "loop_unroll",
		loop_unroll_wrapper);
}

/*
void firm_init_loopunroll(void) {
	FIRM_DBG_REGISTER(dbg, "firm.opt.loopunroll");
}*/
