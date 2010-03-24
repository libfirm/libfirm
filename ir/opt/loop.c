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
 * @brief    loop inversion and loop unrolling
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
#include "irpass.h"
#include "irdom.h"

#include "irbackedge_t.h"
#include "irphase_t.h"
#include "irloop_t.h"


DEBUG_ONLY(static firm_dbg_module_t *dbg);

/**
 * Convenience macro for iterating over every phi node of the given block.
 * Requires phi list per block.
 */
#define for_each_phi(block, phi) \
	for ((phi) = get_Block_phis( (block) ); (phi) ; (phi) = get_Phi_next((phi)))

#define for_each_phi_safe(head, phi, next) \
	for ((phi) = (head), (next) = (head) ? get_Phi_next((head)) : NULL; \
			(phi) ; (phi) = (next), (next) = (next) ? get_Phi_next((next)) : NULL)

/* currently processed loop */
static ir_loop *cur_loop;

/* Condition for performing copy_walk. */
typedef unsigned walker_condition(ir_node *);

/* Node and position of a predecessor */
typedef struct out_edge {
	ir_node *node;
	int pos;
} out_edge;

/**/
typedef struct inversion_node_info {
	ir_node *copy;
	ir_node **ins;
	struct inversion_node_info *free_list_next; 	/* linked list to free all node_infos */
} inversion_node_info;

/*   */
typedef struct node_info {
	ir_node **copy;
	ir_node **ins;
	struct ir_node *free_list_next; 	/* linked list to free all node_infos */
} node_info;

/* Head of the list for freeing node_infos */
static ir_node *free_list;

/* A walker may start visiting the current loop with these nodes. */
static out_edge *cur_loop_outs;

/* Outs of the nodes head. */
static out_edge *cur_head_outs;

/* Information about the loop head */
static ir_node *loop_head = NULL;
static unsigned loop_head_valid = 1;

/* List of all inner loops, that are processed. */
static ir_loop **loops;

/* Inverted head */
static ir_node *loop_inv_head = NULL;
/* Peeled head */
static ir_node *loop_peeled_head = NULL;

/* Loop analysis informations */
typedef struct loop_info_t {
	unsigned blocks;			/* number of blocks in the loop */
	unsigned calls;				/* number of calls */
	unsigned cf_outs;
	out_edge cf_out;
	ir_node *new_loop_head;
} loop_info_t;

/* Information about the current loop */
static loop_info_t loop_info;

/* Outs of the condition chain (loop inversion). */
static out_edge *cond_chain_entries;

/* Arity of the unrolled loop heads. */
static int unroll_arity;

static ir_phase *phase;



/* TODO */
static unsigned head_inversion_node_count;
static unsigned inversion_head_node_limit;
static unsigned head_inversion_block_count;

static unsigned enable_peeling;
static unsigned enable_inversion;
static unsigned enable_unrolling;


/* Creates node_info. A linked list keeps all node_infos to free them again. */
static node_info *new_node_info(ir_node *node)
{
	node_info *l = XMALLOCZ(node_info);
	l->free_list_next = free_list;
	free_list = node;
	l->copy = NULL;
	return l;
}

/* Returns linked node_info for given node. */
static node_info *get_node_info(ir_node *n)
{
	return ((node_info *)get_irn_link(n));
}

/* Allocates a node_info struct for the given node */
static void alloc_node_info(ir_node *node)
{
	node_info *state;
	state = new_node_info(node);
	set_irn_link(node, (void *)state);
}

/* Frees node info and copy array for given node. Returns next node of free_list. */
static ir_node *free_info(ir_node *node)
{
	node_info *info = get_node_info(node);
	ir_node *next = info->free_list_next;
	/*DB((dbg, LEVEL_1, "FREE %N\n", node));*/
	DEL_ARR_F(info->copy);
	xfree(info);
	set_irn_link(node, NULL);
	return next;
}

/* Free all created node_infos, including the copy arrays. */
static void free_node_info(void)
{
	ir_node *node = free_list;
	while (node) {
		node = free_info(node);
	}
	free_list = NULL;
}

/* Free node_infos of blocks only, including the copy arrays. */
static void free_node_info_blocks(void)
{
	ir_node *node = free_list;
	ir_node *new_freelist = NULL;
	while (node) {
		ir_node *next;
		if (is_Block(node)) {
			next = free_info(node);
		} else {
			next = get_node_info(node)->free_list_next;
			get_node_info(node)->free_list_next = new_freelist;
			new_freelist = node;
		}
		node = next;
	}
	free_list = new_freelist;
}

/* Reset nodes link. For use with a walker. */
static void reset_link(ir_node *node, void *env)
{
	(void)env;
	set_irn_link(node, NULL);
}

static void set_inversion_copy(ir_node *n, ir_node *cp)
{
	phase_set_irn_data(phase, n, cp);
}

/* Returns a nodes copy. */
static ir_node *get_copy(ir_node *n, int index)
{
	ir_node *cp = ((node_info *)get_irn_link(n))->copy[index];
	return cp;
}

/* Returns a nodes copy if it exists, or else the node. */
static ir_node *get_inversion_copy(ir_node *n)
{
	ir_node *cp = (ir_node *)phase_get_irn_data(phase, n);
	return cp;
}

/* Returns a nodes copy, or node if there is no node info linked. */
static ir_node *get_copy_safe(ir_node *node, int index)
{
	node_info *info = (node_info *)get_irn_link(node);
	if (info != NULL)
		return get_copy(node, index);
	else
		return node;
}

/* Assigns the given array to a nodes node_info */
static void set_copy_arr(ir_node *n, ir_node **arr)
{
	((node_info *)get_irn_link(n))->copy = arr;
}

/* Assigns a copy with given index to a node.
 * Prerequirements are existing node_info and copy array. */
static void set_copy(ir_node *n, int index, ir_node *copy)
{
	((node_info *)get_irn_link(n))->copy[index] = copy;
}

/* Returns 0 if the node or block is not in cur_loop. */
static unsigned is_in_loop(ir_node *node)
{
	return (get_irn_loop(get_block(node)) == cur_loop);
}

/* Returns if the given be is an alien edge.
 * This is the case when the pred is not in the loop. */
static unsigned is_alien_edge(ir_node *n, int i)
{
	return(!is_in_loop(get_irn_n(n, i)));
}

static unsigned is_own_backedge(ir_node *n, int pos)
{
	return (is_backedge(n, pos) && is_in_loop(get_irn_n(n, pos)));
}

/* Resets block mark for given node. For use with walker */
static void reset_block_mark(ir_node *node, void * env)
{
	(void) env;

	if (is_Block(node))
		set_Block_mark(node, 0);
}

static unsigned is_nodes_block_marked(ir_node* node)
{
	if (is_Block(node))
		return get_Block_mark(node);
	else
		return get_Block_mark(get_block(node));
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

/* */
static void extend_ins_by_copy(ir_node *block, int pos)
{
	ir_node **ins;
	ir_node *phi;
	int i;
	int arity = get_irn_arity(block);
	int new_arity = arity + 1;
	assert(is_Block(block));

	NEW_ARR_A(ir_node *, ins, new_arity);

	/* Extend block by copy of definition at pos */
	for(i = 0; i < arity; ++i) {
		ins[i] = get_irn_n(block, i);
	}
	ins[i] = get_inversion_copy(get_irn_n(block, pos));

	DB((dbg, LEVEL_5, "Extend block %N by %N\n", block, ins[i]));

	/* Fixes backedges. We rely on correct backedges. */
	set_irn_in(block, new_arity, ins);

	/* Extend block phis by copy of definition at pos */
	for_each_phi(block, phi) {
		ir_node *cp, *pred;
		for(i = 0; i < arity; ++i)
			ins[i] = get_irn_n(phi, i);
		pred = get_irn_n(phi, pos);
		cp = get_inversion_copy(pred);
		/* If the phis in is not in the condition chain (eg. a constant),
		 * there is no copy. */
		if (cp == NULL)
			ins[i] = pred;
		else
			ins[i] = cp;

		DB((dbg, LEVEL_5, "Extend phi %N by %N\n", phi, ins[i]));
		set_irn_in(phi, new_arity, ins);
	}
}

/*
 * Extends all blocks which succeed the loop with the copied loops outs.
 * Also extends the blocks phis. Requires block phi list.
 * Returns an updated list of loop outs of the original loop outs.
 */
static out_edge *extend_ins(out_edge *outs, int copies)
{
	ir_node **ins;
	ir_node **phi_ins;
	int *bes, *phi_bes;
	ir_node *phi, *next_phi, *new_phi, *head, *new_block;
	int old_arity, new_arity;
	int i, c, p;
	out_edge *new_outs;

	inc_irg_visited(current_ir_graph);

	new_outs = NEW_ARR_F(out_edge, 0);

#if 0
	for (i = 0; i < ARR_LEN(outs); ++i) {
		out_edge edge = outs[i];
		ir_node *node = edge.node;
		DB((dbg, LEVEL_5, "outs %N %d\n", node, edge.pos));
	}
#endif

	for (i = 0; i < ARR_LEN(outs); ++i) {
		out_edge edge = outs[i];
		ir_node *node = edge.node;
		int in_c, positions_n;
		int *positions;

		if (!is_Block(node) && !is_Bad(node)) {
#if 0
			if (is_Phi(node)) {
				int c;
				unsigned b = 0;
				ir_node *p = get_nodes_block(node);
				for(c = 0; c < get_irn_arity(p); ++c) {
					if (is_in_loop(get_irn_n(p, c))) {
						b=1;
						break;
					}
				}
				if (!b) {
					ARR_APP1(out_edge, new_outs, edge);
					DB((dbg, LEVEL_5, "new out %N\n", node));
				}

			} else {
#endif
			ARR_APP1(out_edge, new_outs, edge);
			DB((dbg, LEVEL_5, "new out %N\n", node));

		}

		/* CONTINUE if node already processed. */
		if (irn_visited(node))
			continue;

		if (!is_Block(node))
			continue;

		positions = NEW_ARR_F(int, 0);
		positions_n = 0;

		/* Search for all occurences of the current node, and save the in positions. */
		for (p = 0; p < ARR_LEN(outs); ++p) {
			ir_node *cur = outs[p].node;
			int d_pos = outs[p].pos;

			if (node==cur) {
				DB((dbg, LEVEL_5, "Loop successor %N with pred @%d in loop\n",
							node, d_pos));
				ARR_APP1(int, positions, d_pos);
				mark_irn_visited(cur);
				++positions_n;
			}
		}

		old_arity = get_irn_arity(node);
		new_arity = old_arity + (positions_n * copies);

		ins = NEW_ARR_F(ir_node*, new_arity);
		bes = NEW_ARR_F(int, new_arity);

		/* extend block ins */
		for (in_c = 0; in_c < old_arity; ++in_c) {
			ins[in_c] = get_irn_n(node, in_c);
			bes[in_c] = is_backedge(node, in_c);
		}
		for (p = 0; p < positions_n; ++p) {
			for (c = 0; c < copies; ++c) {
				ir_node *cp = get_copy_safe(get_irn_n(node, positions[p]), c);
				DB((dbg, LEVEL_5, "%N (new_arity %d) @%d = %N\n",
							node, new_arity, in_c, cp));
				ins[in_c] = cp;
				bes[in_c] = is_backedge(node, positions[p]);
				++in_c;
			}
		}

		assert(new_arity == in_c);

		head = get_Block_phis(node);
		new_block = new_r_Block(get_irn_irg(node), new_arity, ins);
		set_irn_loop(new_block, get_irn_loop(node));

		for (in_c = 0; in_c < new_arity; ++in_c) {
			if (bes[in_c])
				set_backedge(new_block, in_c);
		}
		DEL_ARR_F(ins);
		DEL_ARR_F(bes);

#if 1
		set_Block_phis(node, NULL);

		phi_ins = NEW_ARR_F(ir_node *, new_arity);
		phi_bes = NEW_ARR_F(int, new_arity);

		for_each_phi_safe(head, phi, next_phi) {
			for (in_c = 0; in_c < old_arity; ++in_c) {
				phi_ins[in_c] = get_irn_n(phi, in_c);
				phi_bes[in_c] = is_backedge(phi, in_c);
			}

			for (p = 0; p < positions_n; ++p) {
				for(c = 0; c < copies; ++c) {
					ir_node *cp, *pred;
					pred = get_irn_n(phi, positions[p]);
					DB((dbg, LEVEL_5, "Phi %N pred @%d is %N\n",
								phi, positions[p], pred));
					cp = get_copy_safe(pred, c);
					phi_ins[in_c] = cp;
					DB((dbg, LEVEL_5, "Phi %N in %d is %N\n",
								phi, in_c, cp));
					phi_bes[in_c] = is_backedge(phi, positions[p]);
					++in_c;
				}
			}

			new_phi = new_r_Phi(new_block, new_arity, phi_ins, get_irn_mode(phi));

			for (in_c = 0; in_c < new_arity; ++in_c) {
				if (phi_bes[in_c])
					set_backedge(new_phi, in_c);
			}
			exchange(phi, new_phi);
			DB((dbg, LEVEL_5, "exch Phi %N  %N\n",phi, new_phi));

			add_Block_phi(new_block, new_phi);
		}

		DEL_ARR_F(phi_ins);
		DEL_ARR_F(phi_bes);
#endif
		exchange(node, new_block);
		set_Block_MacroBlock(new_block, new_block);
		DB((dbg, LEVEL_5, "exch block %N  %N\n", node, new_block));

		DEL_ARR_F(positions);
	}
	return new_outs;
}

/**
 * Finds loop head and loop_info.
 */
static void get_loop_info(ir_node *node, void *env)
{
	unsigned node_in_loop, pred_in_loop;
	int i, arity;
	(void)env;

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
		if (is_Block(node) && node_in_loop && !pred_in_loop && loop_head_valid) {
			ir_node *cfgpred = get_Block_cfgpred(node, i);

			if (!is_in_loop(cfgpred)) {
				DB((dbg, LEVEL_5, "potential head %+F because inloop and pred %+F not inloop\n",
							node, pred));
				/* another head? We do not touch this. */
				if (loop_head && loop_head != node) {
					loop_head_valid = 0;
				} else {
					loop_head = node;
				}
			}
		}
	}
}
/**/
static void correct_phis(ir_node *node, void *env)
{
	(void)env;

	if (is_Phi(node) && get_irn_arity(node) == 1) {
		ir_node *exch;
		ir_node *in[1];
		in[0] = get_irn_n(node, 0);

		exch = new_rd_Phi(get_irn_dbg_info(node),
		        get_nodes_block(node), 1, in,
		        get_irn_mode(node));

		exchange(node, exch);
		/* TODO make sure visited is copied */
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
			entry.pos = i;
			ARR_APP1(out_edge, cur_loop_outs, entry);
			if (is_Block(node)) {
				++loop_info.cf_outs;
				loop_info.cf_out = entry;
			}
			DB((dbg, LEVEL_5, "loop outs %N\n", node));
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
	if (get_irn_arity(block) < 1 || is_Bad(block)) {
		DB((dbg, LEVEL_5, "ssa bad %N\n", block));
		return new_Bad();
	}

	if (block == ssa_second_def_block) {
		DB((dbg, LEVEL_5, "ssa found second definition: use second def %N\n", ssa_second_def));
		return ssa_second_def;
	}

	/* already processed this block? */
	if (irn_visited(block)) {
		ir_node *value = get_irn_link(block);
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
		set_irn_link(block, value);
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

	DB((dbg, LEVEL_5, "ssa phi creation: link new phi %N to block %N\n", phi, block));

	set_irn_link(block, phi);
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
	set_irn_link(orig_block, orig_val);
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


/* Construct ssa for def and all of its copies.
 * NOTE: Overwrites links of blocks.
*/
static ir_node *construct_ssa_n(ir_node *def, ir_node *user, int copies)
{
	ir_graph *irg;
	ir_mode *mode;
	irg = get_irn_irg(def);
	int i, user_arity;
	ir_node *newval;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(current_ir_graph);
	mode = get_irn_mode(def);

	set_irn_link(get_nodes_block(def), def);
	mark_irn_visited(get_nodes_block(def));

	for (i = 0; i < copies; ++i) {
		ir_node *cp = get_copy(def, i);
		ir_node *block = get_nodes_block(cp);
		set_irn_link(block, cp);
		mark_irn_visited(block);

		DB((dbg, LEVEL_5, "ssa mark cp %N of def %N  usr %N\n", cp, def, user));
	}

	/* TODO BUG BUG */
	newval = user;
	user_arity = get_irn_arity(user);
	for (i = 0; i < user_arity; ++i) {
		ir_node *user_block = get_nodes_block(user);
		if (get_irn_n(user, i) != def)
			continue;

		DB((dbg, LEVEL_5, "ssa_n found edge of user %N\n", user));

		if (is_Phi(user)) {
			ir_node *pred_block = get_Block_cfgpred_block(user_block, i);
			newval = search_def_and_create_phis(pred_block, mode);
		} else {
			newval = search_def_and_create_phis(user_block, mode);
		}
		/*TODO bads should not happen! */
		if (newval != user) {
			DB((dbg, LEVEL_5, "=> ssa_n new in is %N\n", newval));
			set_irn_n(user, i, newval);
			/*if (is_Phi(newval) && get_nodes_block(newval)== user_block) {
			ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
				return newval;
			}*/
		}

		/* BREAK, as we found and handled the definition */
		break;

#if 0
		if (is_Bad(newval)) {
			DB((dbg, LEVEL_5, "=> ssa_n new in is BAD %N in graph %N\n", newval, current_ir_graph));
			add_End_keepalive(get_irg_end(current_ir_graph), newval);
			dump_ir_block_graph(current_ir_graph, "-bad");
			assert(0);

		}
#endif
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
	return NULL;
}

/* Returns the number of backedges with or without alien bes. */
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
 * Returns a raw copy of the given node.
 * Attributes are kept/set according to the needs of loop inversion.
 */
static ir_node *copy_node_inversion(ir_node *node)
{
	int i, arity;
	ir_node *cp;

	cp = exact_copy(node);
	arity = get_irn_arity(node);

	for (i = 0; i < arity; ++i) {
		if (is_backedge(node, i))
			set_backedge(cp, i);
	}

	set_inversion_copy(node, cp);
	DB((dbg, LEVEL_5, "set copy of %N to cp %N \n", node, cp));

	if (is_Block(cp)) {
		/* We may not keep the old macroblock. */
		set_Block_MacroBlock(cp, cp);
		set_Block_mark(cp, 0);
	}
	return cp;
}


/**
 * This walker copies all walked nodes.
 * If the walk_condition is true for a node, it is copied.
 * All nodes node_info copy attributes have to be NULL prior to every walk.
 */
static void copy_walk(ir_node *node, walker_condition *walk_condition,
		ir_loop *set_loop)
{
	int i;
	int arity;
	ir_node *cp;
	ir_node **cpin;
	ir_graph *irg = current_ir_graph;

	/**
	 * break condition and cycle resolver, creating temporary node copies
	 */
	if (get_irn_visited(node) >= get_irg_visited(irg)) {
		/* Here we rely on nodestate's copy being initialized with NULL */
		DB((dbg, LEVEL_5, "copy_walk: We have already visited %N\n", node));
		if (get_inversion_copy(node) == NULL) {
			cp = copy_node_inversion(node);
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
			cpin[i] = get_inversion_copy(pred);
			DB((dbg, LEVEL_5, "copy of %N gets new in %N which is copy of %N\n",
						node, get_inversion_copy(pred), pred));
		} else {
			cpin[i] = pred;
		}
	}

	/* copy node / finalize temp node */
	if (get_inversion_copy(node) == NULL) {
		/* No temporary copy existent */
		cp = copy_node_inversion(node);
		DB((dbg, LEVEL_5, "The FINAL copy of %N is CREATED %N\n", node, cp));
	} else {
		/* temporary copy is existent but without correct ins */
		cp = get_inversion_copy(node);
		DB((dbg, LEVEL_5, "The FINAL copy of %N is EXISTENT %N\n", node, cp));
	}

	if (!is_Block(node)) {
		ir_node *cpblock = get_inversion_copy(get_nodes_block(node));

		set_nodes_block(cp, cpblock );
		if (is_Phi(cp))
			add_Block_phi(cpblock, cp);
	}

	set_irn_loop(cp, set_loop);

	/* Keeps phi list of temporary node. */
	set_irn_in(cp, ARR_LEN(cpin), cpin);
}

/* Creates a new node from a given one, but with custom ins. */
static ir_node *copy_node_changed(
		ir_node *node, ir_node *block, int arity, ir_node **ins, ir_loop *loop)
{
	ir_node *cp;
	DB((dbg, LEVEL_5, "New node from %N in %N block %N arity %d\n",
				node, get_irn_irg(node), block, arity));
#if 0
	if (is_Phi(node)) {
		cp = new_rd_Phi(get_irn_dbg_info(node),
				block,
				arity,
				ins,
				get_irn_mode(node));

	} else {
#endif
	/* We want to keep phi nodes with arity 1, as it makes rewiring a lot easier. */
	cp = new_ir_node(get_irn_dbg_info(node),
			get_irn_irg(node),
			block,
			get_irn_op(node),
			get_irn_mode(node),
			arity,
			ins);

	/* By any means, keep the blocks phi list up-to-date,
	   because regaining it would destroy the links. */
	if (is_Phi(cp))
		add_Block_phi(block, cp);
/* TODO */
	if (get_irn_op(node) == get_irn_op(cp))
		copy_node_attr(get_irn_irg(node), node, cp);

	/*new_backedge_info(cp); function removed */
	if (is_Block(cp))
		set_Block_MacroBlock(cp, cp);

	set_irn_loop(cp, loop);

	/* Blocks are copied first, so that future phi nodes populate the phi list. */
	if (is_Block(cp))
		set_Block_phis(cp, NULL);

	DB((dbg, LEVEL_5, "New node %N in %N block %N arity %d\n",
				cp, get_irn_irg(cp), block, arity));
	return cp;
}

/* To be exchangeable, the unknown needs to have proper graph set. */
static ir_node *new_node_unknown(ir_node *block, ir_mode *mode)
{
	ir_node *new = new_ir_node(NULL, get_irn_irg(block), block, op_Unknown, mode, 0, NULL);
	return new;
}

/**
 * Walker, to copy alle nodes that meet the walk_condition.
 * Blocks are copied first, which is important for the creation of the phi lists.
 *
 */
static ir_node *copy_walk_n(ir_node *node, walker_condition *walk_condition, int copy_index, int alloc_cp_arr)
{
	int 		i;
	int 		arity;
	unsigned	headblock = 0;
	ir_node 	*block, *cp, *new_cp;
	ir_node 	**ins = NULL;

	if (irn_visited(node)) {
		cp = get_copy(node, copy_index);

		DB((dbg, LEVEL_5, "Visited %N\n", node));

		if (cp != NULL) {
			DB((dbg, LEVEL_5, "Visited. Return copy %N\n", cp));
			return cp;
		}

		if (!is_Block(node)) {
			ir_node *unknown = new_node_unknown(
					get_nodes_block(node),
					get_irn_mode(node));
			DB((dbg, LEVEL_5, " #### Create Unknown %N for %N\n", unknown, node));
			set_copy(node, copy_index, unknown);
			return unknown;
		}
	} else {
		DB((dbg, LEVEL_5, "Walk %N\n", node));
		mark_irn_visited(node);

		/* Allocate array of copies */
		if (alloc_cp_arr > 0) {
			ir_node **arr = NEW_ARR_F(ir_node *, alloc_cp_arr);
			alloc_node_info(node);
			for (i = 0; i < alloc_cp_arr; ++i)
				arr[i] = NULL;
			set_copy_arr(node, arr);
		}
	}

	block = NULL;
	if (!is_Block(node)) {
		ir_node *orig_block = get_nodes_block(node);
		DB((dbg, LEVEL_5, "current node: %N about to walk block %N\n", node, orig_block));
		/* Check walk_condition for blocks not necessary */

		if (orig_block == loop_head && is_Phi(node))
			headblock = 1;

		block = copy_walk_n(orig_block, walk_condition, copy_index, alloc_cp_arr);
		DB((dbg, LEVEL_5, "current node: %N block %N blocks copy %N\n",
					node, orig_block, block));
	} else {
		if (node == loop_head)
			headblock = 1;
	}


	arity = get_irn_arity(node);

	if (headblock) {
		/* Headblock and headblock-phi case */
		int in_c = 0;
		DB((dbg, LEVEL_5, "Headblock/Phi from headblock\n"));
		NEW_ARR_A(ir_node *, ins, unroll_arity);

		for (i = 0; i < arity; ++i) {
			ir_node *ret;
			ir_node *pred = get_irn_n(node, i);
			if (is_backedge(loop_head, i)) {
				if (walk_condition(pred)) {
					ret = copy_walk_n(pred, walk_condition, copy_index, alloc_cp_arr);
					DB((dbg, LEVEL_5, "in %d = %N walked\n", i, ret));
				} else {
					ret = pred;
					DB((dbg, LEVEL_5, "in %d = %N not walked\n", i, ret));
				}
				ins[in_c++] = ret;
			}
		}
		arity = unroll_arity;
	} else {
		/* Normal case */
		NEW_ARR_A(ir_node *, ins, arity);

		for (i = 0; i < arity; ++i) {
			ir_node *ret;
			ir_node *pred = get_irn_n(node, i);
			if (walk_condition(pred)) {
				ret = copy_walk_n(pred, walk_condition, copy_index, alloc_cp_arr);
				DB((dbg, LEVEL_5, "in %d = %N walked\n", i, ret));
			} else {
				ret = pred;
				DB((dbg, LEVEL_5, "in %d = %N not walked\n", i, ret));
			}
			ins[i] = ret;
		}
	}

	/* NOW, after the walk, we may check if the blocks copy is already present. */
	cp = get_copy(node, copy_index);
	if (cp != NULL && !is_Unknown(cp)) {
		DB((dbg, LEVEL_5, "Must be Block %N copy is already present\n", node, cp));
		assert(is_Block(cp) &&
				"sanity: The copy should have been a block.");
		return cp;
	}

	new_cp = copy_node_changed(node, block, arity, ins, cur_loop);
	set_copy(node, copy_index, new_cp);

	if (cp != NULL && is_Unknown(cp)) {
		DB((dbg, LEVEL_5, "Found unknown %N in %N now exchanged by %N in %N\n",
					cp, get_irn_irg(cp), new_cp, get_irn_irg(new_cp)));
		exchange(cp, new_cp);
	}
	return new_cp;
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
					node, is_nodes_block_marked(node), i,
					is_nodes_block_marked(get_irn_n(node, i))));

		if (!is_nodes_block_marked(node) && is_nodes_block_marked(get_irn_n(node, i))) {
			out_edge entry;
			entry.node = node;
			entry.pos = i;
			ARR_APP1(out_edge, cur_head_outs, entry);

			if (is_in_loop(node) && is_Block(node)) {
				loop_info.new_loop_head = node;
				DB((dbg, LEVEL_5, " SET NEW LOOPHEAD to %N\n", node));
			}
		}
	}
}

/**
 * Find condition chains, and add them to be inverted
 * A block belongs to the chain if a condition branches out of the loop.
 * Returns 1 if the given block belongs to the condition chain.
 */
static unsigned find_condition_chains(ir_node *block)
{
	const ir_edge_t *edge;
	unsigned mark = 0;
	int nodes_n = 0;

	DB((dbg, LEVEL_5, "condition_chains for block %N\n", block));

	/* Collect all outs, including keeps. */
	foreach_out_edge_kind(block, edge, EDGE_KIND_NORMAL) {
		++nodes_n;
	}

	/* Leave at least one block as body. */
	if (head_inversion_node_count + nodes_n > inversion_head_node_limit
			|| head_inversion_block_count + 1 == loop_info.blocks) {
		set_Block_mark(block, 0);

		return 0;
	}

	/* First: check our successors,
	 * and add all of them, which are outside of the loop, to the list */
	foreach_block_succ(block, edge) {
		ir_node *src = get_edge_src_irn( edge );
		int pos = get_edge_src_pos( edge );

		if (!is_in_loop(src)) {
			out_edge entry;

			mark = 1;
			entry.node = src;
			entry.pos = pos;
			ARR_APP1(out_edge, cond_chain_entries, entry);
			mark_irn_visited(src);
		}
	}

	if (mark == 0) {
		set_Block_mark(block, 0);
		return 0;
	} else {
		set_Block_mark(block, 1);
		++head_inversion_block_count;
		DB((dbg, LEVEL_5, "block %N is part of condition chain\n", block));
		head_inversion_node_count += nodes_n;
	}

	/* Second: walk all successors,
	 * and add them to the list if they are not part of the chain */
	foreach_block_succ(block, edge) {
		unsigned inchain;
		ir_node *src = get_edge_src_irn( edge );
		int pos = get_edge_src_pos( edge );

		/* already done */
		if (!is_in_loop( src ) || irn_visited(src)) {
			continue;
		}

		mark_irn_visited(src);
		DB((dbg, LEVEL_5, "condition chain walk %N\n", src));
		inchain = find_condition_chains(src);

		/* if successor is not part of chain we need to collect its outs */
		if (!inchain) {
			out_edge entry;
			entry.node = src;
			entry.pos = pos;
			ARR_APP1(out_edge, cond_chain_entries, entry);
		}
	}
	return mark;
}

/**
 * Rewires the copied condition chain. Removes backedges.
 * as this condition chain is prior to the loop.
 * Copy of loop_head must have phi list and old (unfixed) backedge info of the loop head.
 * (loop_head is already fixed, we cannot rely on it.)
 */
static void fix_copy_inversion(void)
{
	ir_node *new_head;
	ir_node **ins;
	ir_node **phis;
	ir_node *phi, *next;
	ir_node *head_cp 	= get_inversion_copy(loop_head);
	int arity 			= get_irn_arity(head_cp);
	int backedges	 	= get_backedge_n(head_cp, 0);
	int new_arity 		= arity - backedges;
	int pos;
	int i;

	NEW_ARR_A(ir_node *, ins, new_arity);

	pos = 0;
	/* Remove block backedges */
	for(i = 0; i < arity; ++i) {
		if (!is_backedge(head_cp, i))
			ins[pos++] = get_irn_n(head_cp, i);
	}

	new_head = new_Block(new_arity, ins);

	phis = NEW_ARR_F(ir_node *, 0);

	for_each_phi_safe(get_Block_phis(head_cp), phi, next) {
		ir_node *new_phi;
		NEW_ARR_A(ir_node *, ins, new_arity);
		pos = 0;
		for(i = 0; i < arity; ++i) {
			if (!is_backedge(head_cp, i))
				ins[pos++] = get_irn_n(phi, i);
		}
		new_phi = new_rd_Phi(get_irn_dbg_info(phi),
				new_head, new_arity, ins,
				get_irn_mode(phi));
		ARR_APP1(ir_node *, phis, new_phi);
	}

	pos = 0;
	for_each_phi_safe(get_Block_phis(head_cp), phi, next) {
		exchange(phi, phis[pos++]);
	}

	exchange(head_cp, new_head);

	DEL_ARR_F(phis);
}

/**/
static ir_node *fix_inner_cc_definitions(ir_node *phi, ir_node *pred)
{
	int i;
	ir_node *head_phi;
	ir_node **head_phi_ins;
	ir_node *new_loop_head = loop_info.new_loop_head;
	int new_loop_head_arity = get_irn_arity(new_loop_head);
	DB((dbg, LEVEL_5, "NEW HEAD %N arity %d\n", new_loop_head, new_loop_head_arity));

	NEW_ARR_A(ir_node *, head_phi_ins, new_loop_head_arity);

	for(i = 0; i < new_loop_head_arity; ++i) {
		/* Rely on correct backedge info for the new loop head
		 * is set by extend_ins_by_copy. */
		if(is_nodes_block_marked(get_irn_n(new_loop_head, i))) {
			head_phi_ins[i] = pred;
		} else {
			head_phi_ins[i] = get_inversion_copy(pred);
		}
		DB((dbg, LEVEL_5, "NEW HEAD %N in %N %N\n", new_loop_head, head_phi_ins[i], current_ir_graph));
	}

	head_phi= new_r_Phi(new_loop_head,
			new_loop_head_arity,
			head_phi_ins,
			get_irn_mode(phi));

	add_Block_phi(new_loop_head, head_phi);

	DB((dbg, LEVEL_5, "fix inner cc definitions: new head %N has new phi %N from cc phi %N @%N\n",
				new_loop_head, head_phi, phi, pred));

	return head_phi;
}


/* Puts the original condition chain at the end of the loop, subsequently to the body.
 * Relies on block phi list and correct backedges.
 */
static void fix_head_inversion(void)
{
	ir_node *new_head;
	ir_node **ins;
	ir_node *phi, *next;
	ir_node **phis;
	int arity 			= get_irn_arity(loop_head);
	int backedges	 	= get_backedge_n(loop_head, 0);
	int new_arity 		= backedges;
	int pos;
	int i;

	NEW_ARR_A(ir_node *, ins, new_arity);

	pos = 0;
	/* Keep only backedges */
	for(i = 0; i < arity; ++i) {
		if (is_own_backedge(loop_head, i))
			ins[pos++] = get_irn_n(loop_head, i);
	}

	new_head = new_Block(new_arity, ins);

	phis = NEW_ARR_F(ir_node *, 0);

	for_each_phi(loop_head, phi) {
		ir_node *new_phi;

		NEW_ARR_A(ir_node *, ins, new_arity);

		pos = 0;
		for(i = 0; i < arity; ++i) {
			ir_node *pred = get_irn_n(phi, i);

			if (is_own_backedge(loop_head, i)) {
				DB((dbg, LEVEL_5, " !!! PHI not useful %N\n", phi));

				/* If definition is in the condition chain,
				 * we need to create a phi in the new loop head */
				if (is_nodes_block_marked(pred))
					ins[pos++] = fix_inner_cc_definitions(phi, pred);
				else
					ins[pos++] = pred;
			}
		}
		new_phi = new_rd_Phi(get_irn_dbg_info(phi),
			new_head, new_arity, ins,
			get_irn_mode(phi));

		ARR_APP1(ir_node *, phis, new_phi);

		DB((dbg, LEVEL_5, "fix inverted head should exch %N by %N\n", phi, new_phi));
	}

	pos = 0;
	for_each_phi_safe(get_Block_phis(loop_head), phi, next) {
		DB((dbg, LEVEL_5, "fix inverted exch phi %N by %N\n", phi, phis[pos]));
		if (phis[pos] != phi)
			exchange(phi, phis[pos++]);
	}

	DEL_ARR_F(phis);

	DB((dbg, LEVEL_5, "fix inverted head exch head block %N by %N\n", loop_head, new_head));
	exchange(loop_head, new_head);
}

/* Does the loop inversion.  */
static void inversion_walk(out_edge *head_entries)
{
	int i;

	/*
	 * The order of rewiring bottom-up is crucial.
	 * Any change of the order leads to lost information that would be needed later.
	 */

	ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	/* 1. clone condition chain */
	inc_irg_visited(current_ir_graph);

	for (i = 0; i < ARR_LEN(head_entries); ++i) {
		out_edge entry = head_entries[i];
		ir_node *pred = get_irn_n(entry.node, entry.pos);

		DB((dbg, LEVEL_5, "\nInit walk block %N\n", pred));

		copy_walk(pred, is_nodes_block_marked, cur_loop);
	}

	ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	/* 2. Extends the head control flow successors ins
	 *    by the definitions of the copied head node. */
	for (i = 0; i < ARR_LEN(head_entries); ++i) {
		out_edge head_out = head_entries[i];

		if (is_Block(head_out.node))
			extend_ins_by_copy(head_out.node, head_out.pos);
	}

	/* 3. construct_ssa for users of definitions in the condition chain,
	 *    as there is now a second definition. */
	for (i = 0; i < ARR_LEN(head_entries); ++i) {
		out_edge head_out = head_entries[i];
		/* Ignore keepalives */
		if (is_End(head_out.node))
			continue;

		ir_node *is_in_cc = get_nodes_block(get_irn_n(head_out.node, head_out.pos));
		/* Construct ssa for definitions in the condition chain. */
		/* TODO Why do we need the check for is_in_cc? All definitions should be there... */
		if (!is_Block(head_out.node) && is_nodes_block_marked(is_in_cc)) {
			ir_node *pred, *cppred, *block, *cpblock;

			pred = get_irn_n(head_out.node, head_out.pos);
			cppred = get_inversion_copy(pred);
			block = get_nodes_block(pred);
			cpblock = get_nodes_block(cppred);
			construct_ssa(block, pred, cpblock, cppred);
		}
	}

	/* 4. Remove the ins which are no backedges from the original condition chain,
	 *    as the cc is now subsequently to the body. */
	fix_head_inversion();

	/* 5. Remove the backedges of the copied condition chain,
	 *    because it is going to be the new 'head' in advance to the loop */
	fix_copy_inversion();
}

/* Analyse loop, if inversion is possible or reasonable. Then do the inversion. */
static void loop_inversion(void)
{
	unsigned do_inversion = 1;

	inversion_head_node_limit = INT_MAX;
	ir_reserve_resources(current_ir_graph, IR_RESOURCE_BLOCK_MARK);

	/* Reset block marks.
	 * We use block marks to flag blocks of the original condition chain. */
	irg_walk_graph(current_ir_graph, reset_block_mark, NULL, NULL);

	loop_info.blocks = get_loop_n_blocks(cur_loop);
	cond_chain_entries = NEW_ARR_F(out_edge, 0);

	head_inversion_node_count = 0;
	head_inversion_block_count = 0;

	set_Block_mark(loop_head, 1);
	mark_irn_visited(loop_head);

	/* Use phase to keep copy of nodes from the condition chain. */
	phase = new_phase(current_ir_graph, phase_irn_init_default);

	inc_irg_visited(current_ir_graph);

	/* Search for condition chains. */
	find_condition_chains(loop_head);

	DB((dbg, LEVEL_3, "Loop contains %d blocks.\n", loop_info.blocks));

	if (loop_info.blocks < 2) {
		do_inversion = 0;
		DB((dbg, LEVEL_3,
			"Loop contains %d (less than 2) blocks => No Inversion done.\n",
			loop_info.blocks));
	}

	/* We also catch endless loops here,
	 * because they do not have a condition chain. */
	if (head_inversion_block_count < 1) {
		do_inversion = 0;
		DB((dbg, LEVEL_3,
			"Loop contains %d (less than 1) invertible blocks => No Inversion done.\n",
			head_inversion_block_count));
	}

	if (do_inversion) {
		cur_head_outs = NEW_ARR_F(out_edge, 0);

		/* Get all edges pointing into the condition chain. */
		irg_walk_graph(current_ir_graph, get_head_outs, NULL, NULL);

		/* Do the inversion */
		inversion_walk(cur_head_outs);

		DEL_ARR_F(cur_head_outs);
		/* Duplicated blocks changed doms */
		set_irg_doms_inconsistent(current_ir_graph);
		/* Loop content changed */
		set_irg_loopinfo_inconsistent(current_ir_graph);
		/* TODO are they? */
		set_irg_outs_inconsistent(current_ir_graph);
	}

	/* free */
	phase_free(phase);
	DEL_ARR_F(cond_chain_entries);
	ir_free_resources(current_ir_graph, IR_RESOURCE_BLOCK_MARK);
}

/**
 * Rewire floating copies of the current loop.
 */
static void place_copies(int copies)
{
	ir_node *loophead = loop_head;
	int headarity = 	get_irn_arity(loophead);
	ir_node *phi;
	int i, pos, c;

	pos = 0;
	/* Append the copies to the existing loop. */
	for (i = 0; i < headarity; ++i) {
		/* Build unrolled loop top down */
		if (is_backedge(loophead, i) && !is_alien_edge(loophead, i)) {
			for (c = 0; c < copies; ++c) {
				ir_node *upper_head;
				ir_node *lower_head = get_copy(loophead, c);
				ir_node *upper_pred;

				if (c == 0) {
					upper_head = loophead;
					upper_pred = get_irn_n(loophead, i);
				} else {
					upper_head = get_copy(loophead, c - 1);
					upper_pred = get_copy_safe(get_irn_n(loophead, i), c - 1);
				}
				set_irn_n(lower_head, pos, upper_pred);
				for_each_phi(loophead, phi) {
					ir_node *lower_phi, *upper_phi, *upper_pred_phi;
					lower_phi = get_copy(phi, c);
					if (c == 0) {
						upper_phi = phi;
						upper_pred_phi = get_irn_n(phi, i);
					} else {
						upper_phi = get_copy(phi, c - 1);
						upper_pred_phi = get_copy_safe(get_irn_n(phi, i), c - 1);
					}

					DB((dbg, LEVEL_5, "Rewire %N @%d to %N\n",
								lower_phi, pos, upper_pred_phi));

					/* Relying on phi nodes with 1 in to be present. */
					assert(is_Phi(lower_phi));

/* Do not fix phis here. Chained phis will cause a defective copy array. */
#if 0
					if (get_irn_arity(lower_phi) == 1) {
						ir_node *single_in[1];
						ir_node *cp;
						single_in[0] = upper_pred_phi;

						cp = new_rd_Phi(get_irn_dbg_info(lower_phi),
								get_nodes_block(lower_phi), 1, single_in,
								get_irn_mode(lower_phi));

						set_copy(phi, c, cp);
						exchange(lower_phi, cp);
					} else
#endif
						set_irn_n(lower_phi, pos, upper_pred_phi);

				}
			}
			++pos;
			/* Fix the topmost loop heads backedges. */
			set_irn_n(loophead, i, get_copy(get_irn_n(loophead, i), copies - 1));
			for_each_phi(loophead, phi) {
				ir_node *last_cp = get_copy_safe(get_irn_n(phi, i), copies - 1);
				set_irn_n(phi, i, last_cp);
			}
		}
	}
}

/* Copies the cur_loop */
static void copy_loop(out_edge *cur_loop_outs, int copies)
{
	int i, c;
	unroll_arity = get_backedge_n(loop_head, 0);

	ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);

	/* copy loop */
	for (c = 0; c < copies; ++c) {

		inc_irg_visited(current_ir_graph);

		DB((dbg, LEVEL_5, "          ### Copy_loop  copy n: %d ###\n", c));
		for (i = 0; i < ARR_LEN(cur_loop_outs); ++i) {
			out_edge entry = cur_loop_outs[i];
			ir_node *pred = get_irn_n(entry.node, entry.pos);

			copy_walk_n(pred, is_in_loop, c, c ? 0 : copies);
		}
	}

	ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_VISITED);
}

/**/
static void create_duffs_device(int unroll_number)
{
	(void)unroll_number;
}

#if 1

static unsigned is_loop_invariant_val(ir_node *node)
{
	int i;

	if (! is_in_loop(node))
		return 1;

	/* TODO all cases? and how to use the dom tree for that. */

	if (is_Phi(node)) {
		for (i = 0; i < get_irn_arity(node); ++i) {
			if (is_own_backedge(get_nodes_block(node), i) && get_irn_n(node, i) != node)
				return 0;
		}
	}
	return 1;
}

/* TODO name */
static ir_node *is_loop_iteration(ir_node *node)
{
	int i;
	ir_node *found_add = NULL;

	if (!is_in_loop(node))
		return NULL;

	if (is_Add(node))
		return node;

	/*TODO all cases? and how do i use the dom tree for that.*/

	if (is_Phi(node)) {
		for (i = 0; i < get_irn_arity(node); ++i) {
			ir_node *new_found;
			if (! is_own_backedge(get_nodes_block(node), i))
				continue;
			new_found = get_irn_n(node,i);

			if (found_add && found_add != new_found)
				return NULL;

			found_add = new_found;
		}
	}
	return found_add;
}

/* counting loop */
static unsigned get_unroll_decision(out_edge *cur_loop_outs)
{
	int i;
	ir_node *pred0, *pred1, *projx, *cond, *projres, *loop_condition;
	ir_node *iteration, *add, *iteration_phi, *end_val, *step, *add_in0, *add_in1;

	(void) cur_loop_outs;
	(void) i;


	/* There is more than 1 condition for the loop exit. We cannot cope this with duffs device. */
	if (loop_info.cf_outs != 1)
		return 0;

	/*TODO test for impact on performance */
	if (loop_info.calls > 0)
		return 0;


	projx = get_irn_n(loop_info.cf_out.node, loop_info.cf_out.pos);
	cond = get_irn_n(projx, 0);
	projres = get_irn_n(cond, 0);
	loop_condition = get_irn_n(projres, 0);

	DB((dbg, LEVEL_2, " loop condition %N\n", loop_condition));

	/* One in of the loop condition needs to be loop invariant.
	 * The other in is assigned by an add.
	 * The add uses a loop invariant value
	 * and another value that depends on a loop invariant start value and the add itself.

	   ^   ^
	   |   | .-,
	   |   Phi |
		\  |   |
		 Add  /
	      |__/
	*/

	end_val = NULL;
	/* Find the loop invariant in */
	/* Assumption that condition is cmp or mod.
	   TODO other? */
	pred0 = get_irn_n(loop_condition, 0);
	pred1 = get_irn_n(loop_condition, 1);

	if (is_loop_invariant_val(pred0)) {
		end_val = pred0;
		iteration = pred1;
	}


	if (is_loop_invariant_val(pred1)) {
		if (end_val != NULL)
			/* RETURN. loop condition does not change during the loop. */
			return 0;

		end_val = pred1;
		iteration = pred0;
	}

	DB((dbg, LEVEL_2, " end_val %N\n", end_val));

	/* Check for constraint of remaining in. */
	add = is_loop_iteration(iteration);
	if (! add)
		return 0;

	DB((dbg, LEVEL_2, " add %N\n", add));

	add_in0 = get_irn_n(add, 0);
	add_in1 = get_irn_n(add, 1);

	if (is_loop_invariant_val(add_in0)) {
		step = add_in0;
		iteration_phi = add_in1;
	}

	if (is_loop_invariant_val(add_in1)) {
		if (step != NULL)
			return 0;
		step = add_in1;
		iteration_phi = add_in0;
	}

	DB((dbg, LEVEL_2, " step %N\n", step));

	if (! is_Phi(iteration_phi)) {
		if (is_Phi(iteration))
			iteration_phi = iteration;
		else
			return 0;
	}

	DB((dbg, LEVEL_2, " #### COUNTING LOOP step %N end %N phi %N\n", step, end_val, iteration_phi));

#if 0
	phi for startval

	mode = get_irn_mode(add);

	block = new_Block(0, NULL);
	/*set_irg_current_Block(current_ir_graph, block);*/

	/*TODO*/
	unroll_mod_const = new_Const(unroll_number);
	module = new_r_Mod(block, new_NoMem(), end_val, step, mode, op_pin_state_pinned);
	/*new_Proj();*/


#endif
	return 0;
}
#endif

/**
 * Loop unrolling
 */
static void unroll_loop(void)
{
	int i;
	unsigned unroll_number;
	out_edge *ssa_outs;
	cur_loop_outs = NEW_ARR_F(out_edge, 0);

	/* Get loop outs */
	irg_walk_graph(current_ir_graph, get_loop_outs, NULL, NULL);

	unroll_number = get_unroll_decision(cur_loop_outs);

	/*dump_ir_block_graph(current_ir_graph, "-pre");*/

	if (unroll_number) {
		create_duffs_device(unroll_number);

		/* Copies the loop and allocs node_info */
		copy_loop(cur_loop_outs, unroll_number - 1);
		/*dump_ir_block_graph(current_ir_graph, "-cp");*/

		/* Line up the floating copies. */
		place_copies(unroll_number - 1);
		/*dump_ir_block_graph(current_ir_graph, "-ord");*/

		/* Extend loop successors */
		ssa_outs = extend_ins(cur_loop_outs, unroll_number - 1);
		/*dump_ir_block_graph(current_ir_graph, "-fixed")*/ ;

		/* Links are going to be overwritten */
		free_node_info_blocks();

		/* Generate phis for all loop outs */
		for (i = 0; i < ARR_LEN(ssa_outs); ++i) {
			out_edge entry = ssa_outs[i];
			ir_node *node = entry.node;
			ir_node *pred = get_irn_n(entry.node, entry.pos);

			DB((dbg, LEVEL_5, "Do? construct_ssa_n def %N  node %N  pos %d\n",
					pred, node, entry.pos));

			if (!is_End(node)) {
				DB((dbg, LEVEL_5, "construct_ssa_n def %N  node %N  pos %d\n",
							pred, node, entry.pos));

				construct_ssa_n(pred, node, unroll_number - 1);
				/*if (new_phi)
					construct_ssa_n(pred, new_phi, unroll_number - 1);*/
			}
		}
		/*dump_ir_block_graph(current_ir_graph, "-ssa-done");*/

		/* FREE */
		free_node_info();

		irg_walk_graph(current_ir_graph, correct_phis, NULL, NULL);

		DEL_ARR_F(ssa_outs);

		set_irg_doms_inconsistent(current_ir_graph);
		set_irg_loopinfo_inconsistent(current_ir_graph);
		set_irg_outs_inconsistent(current_ir_graph);
	}
	DEL_ARR_F(cur_loop_outs);
}

/* Initialization and */
static void init_analyze(ir_loop *loop)
{
	/* Init new for every loop */
	cur_loop = loop;

	loop_head = NULL;
	loop_head_valid = 1;
	loop_inv_head = NULL;
	loop_peeled_head = NULL;

	loop_info.calls = 0;
	loop_info.blocks = 0;
	loop_info.cf_outs = 0;

	DB((dbg, LEVEL_2, "          >>>> current loop includes node %N <<<\n",
		get_loop_node(loop, 0)));

	irg_walk_graph(current_ir_graph, get_loop_info, NULL, NULL);

	/* RETURN if there is no valid head */
	if (!loop_head || !loop_head_valid) {
		DB((dbg, LEVEL_2,   "No valid loop head. Nothing done.\n"));
		return;
	}

	DB((dbg, LEVEL_2,   "Loophead: %N\n", loop_head));

	if (enable_inversion)
		loop_inversion();

	if (enable_unrolling)
		unroll_loop();

	DB((dbg, LEVEL_2, "             <<<< end of loop with node %N >>>>\n",
		get_loop_node(loop, 0)));
}

/* Find most inner loops and send them to analyze_loop */
static void find_most_inner_loop(ir_loop *loop)
{
	/* descend into sons */
	int sons = get_loop_n_sons(loop);

	if (sons == 0) {
		ARR_APP1(ir_loop *, loops, loop);
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
	free_list = NULL;
	set_current_ir_graph(irg);

	edges_assure(irg);
	assure_irg_outs(irg);

	/* NOTE: sets only the loop attribute of blocks, not nodes */
	/* NOTE: Kills links */
	assure_cf_loop(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);
	collect_phiprojs(irg);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	loop = get_irg_loop(irg);
	sons = get_loop_n_sons(loop);

	loops = NEW_ARR_F(ir_loop *, 0);
	/* List all inner loops */
	for (nr = 0; nr < sons; ++nr) {
		find_most_inner_loop(get_loop_son(loop, nr));
	}

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	/* Set all links NULL */
	irg_walk_graph(current_ir_graph, reset_link, NULL, NULL);

	for (i = 0; i < ARR_LEN(loops); ++i) {
		ir_loop *loop = loops[i];
		init_analyze(loop);

		/*dump_ir_block_graph(current_ir_graph, "-part");*/
		collect_phiprojs(irg);
		irg_walk_graph(current_ir_graph, reset_link, NULL, NULL);
	}

	/*dump_ir_block_graph(current_ir_graph, "-full");*/

	DEL_ARR_F(loops);
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
