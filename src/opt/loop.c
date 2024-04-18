/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author   Christian Helmer
 * @brief    loop inversion and loop unrolling
 *
 */

#include "array.h"
#include "debug.h"
#include "irbackedge_t.h"
#include "ircons_t.h"
#include "irdom.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irloop_t.h"
#include "irnode_t.h"
#include "irnodemap.h"
#include "iroptimize.h"
#include "irouts.h"
#include "irtools.h"
#include "opt_init.h"
#include "panic.h"
#include "util.h"
#include <math.h>
#include <stdbool.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * Convenience macro for iterating over every phi node of the given block.
 * Requires phi list per block.
 */
#define for_each_phi(block, phi) \
	for (ir_node *phi = get_Block_phis((block)); phi; phi = get_Phi_next(phi))

#define for_each_phi_safe(block, phi, next) \
	for (ir_node *phi = get_Block_phis((block)), *next = NULL; phi ? next = get_Phi_next(phi), true : false; phi = next)

/* Currently processed loop. */
static ir_loop *cur_loop;

/* Flag for kind of unrolling. */
typedef enum unrolling_kind_flag {
	constant,
	invariant
} unrolling_kind_flag;

/* Condition for performing visiting a node during copy_walk. */
typedef bool walker_condition(const ir_node *);

/* Node and position of a predecessor. */
typedef struct entry_edge {
	ir_node *node;
	int      pos;
	ir_node *pred;
} entry_edge;

/* Node info for unrolling. */
typedef struct unrolling_node_info {
	ir_node **copies;
} unrolling_node_info;

/* Outs of the nodes head. */
static entry_edge *cur_head_outs;

/* Information about the loop head */
static ir_node *loop_head       = NULL;
static bool     loop_head_valid = true;

/* List of all inner loops, that are processed. */
static ir_loop **loops;

/* Stats */
typedef struct loop_stats_t {
	unsigned loops;
	unsigned inverted;
	unsigned too_large;
	unsigned too_large_adapted;
	unsigned cc_limit_reached;
	unsigned calls_limit;

	unsigned u_simple_counting_loop;
	unsigned constant_unroll;
	unsigned invariant_unroll;

	unsigned unhandled;
} loop_stats_t;

static loop_stats_t stats;

/* Set stats to sero */
static void reset_stats(void)
{
	memset(&stats, 0, sizeof(loop_stats_t));
}

/* Print stats */
static void print_stats(void)
{
	DB((dbg, LEVEL_2, "---------------------------------------\n"));
	DB((dbg, LEVEL_2, "loops             :   %d\n", stats.loops));
	DB((dbg, LEVEL_2, "inverted          :   %d\n", stats.inverted));
	DB((dbg, LEVEL_2, "too_large         :   %d\n", stats.too_large));
	DB((dbg, LEVEL_2, "too_large_adapted :   %d\n", stats.too_large_adapted));
	DB((dbg, LEVEL_2, "cc_limit_reached  :   %d\n", stats.cc_limit_reached));
	DB((dbg, LEVEL_2, "calls_limit       :   %d\n", stats.calls_limit));
	DB((dbg, LEVEL_2, "u_simple_counting :   %d\n", stats.u_simple_counting_loop));
	DB((dbg, LEVEL_2, "constant_unroll   :   %d\n", stats.constant_unroll));
	DB((dbg, LEVEL_2, "invariant_unroll  :   %d\n", stats.invariant_unroll));
	DB((dbg, LEVEL_2, "=======================================\n"));
}

/* Commandline parameters */
typedef struct loop_opt_params_t {
	unsigned max_loop_size;     /* Maximum number of nodes  [nodes]*/
	int      depth_adaption;    /* Loop nest depth adaption [percent] */
	unsigned allowed_calls;     /* Number of calls allowed [number] */
	bool     count_phi;         /* Count phi nodes */

	unsigned max_cc_size;       /* Maximum condition chain size [nodes] */
	unsigned max_branches;

	unsigned max_unrolled_loop_size;    /* [nodes] */
	bool     allow_const_unrolling;
	bool     allow_invar_unrolling;
	unsigned invar_unrolling_min_size;  /* [nodes] */
} loop_opt_params_t;

static loop_opt_params_t opt_params;

/* Loop analysis informations */
typedef struct loop_info_t {
	unsigned   nodes;      /* node count */
	unsigned   branches;   /* number of conditions */
	unsigned   calls;      /* number of calls */
	unsigned   cf_outs;    /* number of cf edges which leave the loop */
	ir_node   *cf_out;     /* single loop leaving cf edge */
	int        be_src_pos; /* position of the single own backedge in the head */

	/* for inversion */
	unsigned cc_size; /* nodes in the condition chain */

	/* for unrolling */
	unsigned max_unroll;       /* Number of unrolls satisfying max_loop_size */
	unsigned exit_cond;        /* 1 if condition==true exits the loop.  */
	unsigned latest_value:1;   /* 1 if condition is checked against latest counter value */
	unsigned decreasing:1;     /* Step operation is_Sub, or step is<0 */

	/* IV informations of a simple loop */
	ir_node *start_val;
	ir_node *step;
	ir_node *end_val;
	ir_node *iteration_phi;
	ir_node *add;

	ir_node            *duff_cond;   /* Duff mod */
	unrolling_kind_flag unroll_kind; /* constant or invariant unrolling */
} loop_info_t;

/* Information about the current loop */
static loop_info_t loop_info;

/* Outs of the condition chain (loop inversion). */
static ir_node **cc_blocks;
/* Array of df loops found in the condition chain. */
static entry_edge *head_df_loop;
/* Number of blocks in cc */
static unsigned inversion_blocks_in_cc;


/* Cf/df edges leaving the loop.
 * Called entries here, as they are used to enter the loop with walkers. */
static entry_edge *loop_entries;
/* Number of unrolls to perform */
static int unroll_nr;
/* Phase is used to keep copies of nodes. */
static ir_nodemap     map;
static struct obstack obst;

/* Loop operations.  */
typedef enum loop_op_t {
	loop_op_inversion,
	loop_op_unrolling,
	loop_op_peeling
} loop_op_t;

/* Returns the maximum nodes for the given nest depth */
static unsigned get_max_nodes_adapted(unsigned const depth)
{
	double perc = 100.0 + (double)opt_params.depth_adaption;
	double factor = pow(perc / 100.0, depth);

	return (int)((double)opt_params.max_loop_size * factor);
}

/* Returns 0 if the node or block is not in cur_loop. */
static bool is_in_loop(const ir_node *const node)
{
	return get_irn_loop(get_block_const(node)) == cur_loop;
}

/* Returns 0 if the given edge is not a backedge
 * with its pred in the cur_loop. */
static bool is_own_backedge(const ir_node *const n, int const pos)
{
	return is_backedge(n, pos) && is_in_loop(get_Block_cfgpred(n, pos));
}

/* Finds loop head and some loop_info as calls or else if necessary. */
static void get_loop_info(ir_node *const node, void *const env)
{
	(void)env;

	bool const node_in_loop = is_in_loop(node);

	/* collect some loop information */
	if (node_in_loop) {
		switch (get_irn_opcode(node)) {
		case iro_Call:
			++loop_info.calls;
			goto count;

		case iro_Phi:
			if (opt_params.count_phi)
				goto count;
			break;

		case iro_Address:
		case iro_Align:
		case iro_Confirm:
		case iro_Const:
		case iro_Offset:
		case iro_Proj:
		case iro_Size:
			break;

		default:
count:
			++loop_info.nodes;
			break;
		}
	}

	foreach_irn_in_r(node, i, pred) {
		bool pred_in_loop = is_in_loop(pred);

		if (is_Block(node) && !node_in_loop && pred_in_loop) {
			/* Count cf outs */
			++loop_info.cf_outs;
			loop_info.cf_out = pred;
		}

		/* Find the loops head/the blocks with cfpred outside of the loop */
		if (is_Block(node)) {
			unsigned outs_n = 0;

			/* Count innerloop branches */
			foreach_block_succ(node, edge) {
				ir_node *succ = get_edge_src_irn(edge);
				if (is_Block(succ) && is_in_loop(succ))
					++outs_n;
			}
			if (outs_n > 1)
				++loop_info.branches;

			if (node_in_loop && !pred_in_loop && loop_head_valid) {
				ir_node *cfgpred = get_Block_cfgpred(node, i);

				if (!is_in_loop(cfgpred)) {
					DB((dbg, LEVEL_5, "potential head %+F because inloop and pred %+F not inloop\n",
					    node, pred));
					/* another head? We do not touch this. */
					if (loop_head && loop_head != node) {
						loop_head_valid = false;
					} else {
						loop_head = node;
					}
				}
			}
		}
	}
}

/* Finds all edges with users outside of the loop
 * and definition inside the loop. */
static void get_loop_entries(ir_node *const node, void *const env)
{
	(void)env;

	foreach_irn_in(node, i, pred) {
		if (is_in_loop(pred) && !is_in_loop(node)) {
			entry_edge const entry = { .node = node, .pos = i, .pred = pred };
			ARR_APP1(entry_edge, loop_entries, entry);
		}
	}
}

/* ssa */
static ir_node *ssa_second_def;
static ir_node *ssa_second_def_block;

/**
 * Walks the graph bottom up, searching for definitions and creates phis.
 */
static ir_node *search_def_and_create_phis(ir_node *const block, ir_mode *const mode, int const first)
{
	ir_graph *const irg = get_irn_irg(block);

	DB((dbg, LEVEL_5, "ssa search_def_and_create_phis: block %N\n", block));

	/* Prevents creation of phi that would be bad anyway.
	 * Dead and bad blocks. */
	if (get_irn_arity(block) < 1 || is_Bad(block)) {
		DB((dbg, LEVEL_5, "ssa bad %N\n", block));
		return new_r_Bad(irg, mode);
	}

	if (block == ssa_second_def_block && !first) {
		DB((dbg, LEVEL_5, "ssa found second definition: use second def %N\n", ssa_second_def));
		return ssa_second_def;
	}

	/* already processed this block? */
	if (irn_visited(block)) {
		ir_node *const value = (ir_node *)get_irn_link(block);
		DB((dbg, LEVEL_5, "ssa already visited: use linked %N\n", value));
		return value;
	}

	assert(block != get_irg_start_block(irg));

	/* a Block with only 1 predecessor needs no Phi */
	int const n_cfgpreds = get_Block_n_cfgpreds(block);
	if (n_cfgpreds == 1) {
		ir_node *pred_block = get_Block_cfgpred_block(block, 0);
		ir_node *value;

		DB((dbg, LEVEL_5, "ssa 1 pred: walk pred %N\n", pred_block));

		value = search_def_and_create_phis(pred_block, mode, 0);
		set_irn_link(block, value);
		mark_irn_visited(block);

		return value;
	}

	/* create a new Phi */
	ir_node **const in = ALLOCAN(ir_node*, n_cfgpreds);
	for (int i = 0; i < n_cfgpreds; ++i)
		in[i] = new_r_Dummy(irg, mode);

	ir_node *const phi = mode == mode_M ? new_r_Phi_loop(block, n_cfgpreds, in)
	                                    : new_r_Phi(block, n_cfgpreds, in, mode);
	/* Important: always keep block phi list up to date. */
	add_Block_phi(block, phi);
	DB((dbg, LEVEL_5, "ssa phi creation: link new phi %N to block %N\n", phi, block));
	set_irn_link(block, phi);
	mark_irn_visited(block);

	/* set Phi predecessors */
	for (int i = 0; i < n_cfgpreds; ++i) {
		ir_node *const pred_block = get_Block_cfgpred_block(block, i);
		assert(pred_block != NULL);
		ir_node *const pred_val = search_def_and_create_phis(pred_block, mode, 0);

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
static void construct_ssa(ir_node *const orig_block, ir_node *const orig_val, ir_node *const second_block, ir_node *const second_val)
{
	assert(orig_block && orig_val && second_block && second_val &&
	       "no parameter of construct_ssa may be NULL");

	if (orig_val == second_val)
		return;

	ir_graph *const irg = get_irn_irg(orig_val);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);

	set_irn_link(orig_block, orig_val);
	mark_irn_visited(orig_block);

	ssa_second_def_block = second_block;
	ssa_second_def       = second_val;

	/* Only fix the users of the first, i.e. the original node */
	ir_mode *const mode = get_irn_mode(orig_val);
	foreach_out_edge_safe(orig_val, edge) {
		ir_node *const user = get_edge_src_irn(edge);

		/* Ignore keeps. */
		if (is_End(user))
			continue;

		DB((dbg, LEVEL_5, "original user %N\n", user));

		ir_node       *newval;
		ir_node *const user_block = get_nodes_block(user);
		int      const j          = get_edge_src_pos(edge);
		if (is_Phi(user)) {
			ir_node *pred_block = get_Block_cfgpred_block(user_block, j);
			newval = search_def_and_create_phis(pred_block, mode, 1);
		} else {
			newval = search_def_and_create_phis(user_block, mode, 1);
		}
		if (newval != user && !is_Bad(newval))
			set_irn_n(user, j, newval);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
}


/***** Unrolling Helper Functions *****/

/* Assign the copy with index nr to node n */
static void set_unroll_copy(ir_node *const n, int const nr, ir_node *const cp)
{
	assert(nr != 0 && "0 reserved");

	unrolling_node_info *info = ir_nodemap_get(unrolling_node_info, &map, n);
	if (!info) {
		info = OALLOCZ(&obst, unrolling_node_info);
		info->copies = NEW_ARR_DZ(ir_node*, &obst, unroll_nr);
		ir_nodemap_insert(&map, n, info);
	}
	/* Original node */
	info->copies[0]  = n;
	info->copies[nr] = cp;
}

/* Returns a nodes copy if it exists, else NULL. */
static ir_node *get_unroll_copy(ir_node *const n, int const nr)
{
	unrolling_node_info *info = ir_nodemap_get(unrolling_node_info, &map, n);
	if (!info)
		return NULL;

	return info->copies[nr];
}


/***** Inversion Helper Functions *****/

/* Sets copy cp of node n. */
static void set_inversion_copy(ir_node *const n, ir_node *const cp)
{
	ir_nodemap_insert(&map, n, cp);
}

/* Getter of copy of n for inversion */
static ir_node *get_inversion_copy(ir_node *const n)
{
	return ir_nodemap_get(ir_node, &map, n);
}

/* Resets block mark for given node. For use with walker */
static void reset_block_mark(ir_node *const node, void *const env)
{
	(void)env;

	if (is_Block(node))
		set_Block_mark(node, 0);
}

/* Returns mark of node, or its block if node is not a block.
 * Used in this context to determine if node is in the condition chain. */
static bool is_nodes_block_marked(const ir_node *const node)
{
	return get_Block_mark(get_block_const(node));
}

/* Extends a nodes ins by node new.
 * NOTE: This is slow if a node n needs to be extended more than once. */
static void extend_irn(ir_node *const n, ir_node *const newnode, bool const new_is_backedge)
{
	int       const arity     = get_irn_arity(n);
	int       const new_arity = arity + 1;
	ir_node **const ins       = XMALLOCN(ir_node*, new_arity);
	bool     *const bes       = XMALLOCN(bool,     new_arity);

	/* save bes */
	/* Bes are important!
	 * Another way would be recreating the looptree,
	 * but after that we cannot distinguish already processed loops
	 * from not yet processed ones. */
	if (is_Block(n)) {
		for (int i = 0; i < arity; ++i) {
			bes[i] = is_backedge(n, i);
		}
		bes[arity] = new_is_backedge;
	}

	for (int i = 0; i < arity; ++i) {
		ins[i] = get_irn_n(n, i);
	}
	ins[arity] = newnode;

	set_irn_in(n, new_arity, ins);

	/* restore bes  */
	if (is_Block(n)) {
		for (int i = 0; i < new_arity; ++i) {
			if (bes[i])
				set_backedge(n, i);
		}
	}
	free(ins);
	free(bes);
}

/* Extends a block by a copy of its pred at pos,
 * fixing also the phis in the same way. */
static void extend_ins_by_copy(ir_node *const block, int const pos)
{
	/* Extend block by copy of definition at pos */
	ir_node *const pred   = get_Block_cfgpred(block, pos);
	ir_node *const new_in = get_inversion_copy(pred);
	DB((dbg, LEVEL_5, "Extend block %N by %N cp of %N\n", block, new_in, pred));
	extend_irn(block, new_in, false);

	/* Extend block phis by copy of definition at pos */
	for_each_phi(block, phi) {
		ir_node *const pred = get_Phi_pred(phi, pos);
		ir_node *const cp   = get_inversion_copy(pred);
		/* If the phis in is not in the condition chain (eg. a constant),
		 * there is no copy. */
		ir_node *const new_phi_in = cp ? cp : pred;

		DB((dbg, LEVEL_5, "Extend phi %N by %N cp of %N\n", phi, new_in, pred));
		extend_irn(phi, new_phi_in, false);
	}
}

/* Returns the number of blocks backedges. With or without alien bes. */
static int get_backedge_n(ir_node *const block, bool const with_alien)
{
	int       be_n  = 0;
	int const arity = get_Block_n_cfgpreds(block);
	for (int i = 0; i < arity; ++i) {
		ir_node *const pred = get_Block_cfgpred(block, i);
		if (is_backedge(block, i) && (with_alien || is_in_loop(pred)))
			++be_n;
	}
	return be_n;
}

/* Returns a raw copy of the given node.
 * Attributes are kept/set according to the needs of loop inversion. */
static ir_node *copy_node(ir_node *const node)
{
	ir_node *const cp    = exact_copy(node);
	int      const arity = get_irn_arity(node);

	/* Keep backedge info */
	for (int i = 0; i < arity; ++i) {
		if (is_backedge(node, i))
			set_backedge(cp, i);
	}

	if (is_Block(cp))
		set_Block_mark(cp, 0);

	return cp;
}


/**
 * This walker copies all walked nodes.
 * If the walk_condition is true for a node, it is copied.
 * All nodes node_info->copy have to be NULL prior to every walk.
 * Order of ins is important for later usage.
 */
static void copy_walk(ir_node *const node, walker_condition *const walk_condition, ir_loop *const set_loop)
{
	/**
	 * break condition and cycle resolver, creating temporary node copies
	 */
	if (irn_visited(node)) {
		/* Here we rely on nodestate's copy being initialized with NULL */
		DB((dbg, LEVEL_5, "copy_walk: We have already visited %N\n", node));
		if (get_inversion_copy(node) == NULL) {
			ir_node *const cp = copy_node(node);
			set_inversion_copy(node, cp);

			DB((dbg, LEVEL_5, "The TEMP copy of %N is created %N\n", node, cp));
		}
		return;
	}

	/* Walk */
	mark_irn_visited(node);

	if (!is_Block(node)) {
		ir_node *const pred = get_nodes_block(node);
		if (walk_condition(pred))
			DB((dbg, LEVEL_5, "walk block %N\n", pred));
		copy_walk(pred, walk_condition, set_loop);
	}

	int       const arity = get_irn_arity(node);
	ir_node **const cpin  = ALLOCAN(ir_node*, arity);

	foreach_irn_in(node, i, pred) {
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
	ir_node *cp;
	if (get_inversion_copy(node) == NULL) {
		/* No temporary copy existent */
		cp = copy_node(node);
		set_inversion_copy(node, cp);
		DB((dbg, LEVEL_5, "The FINAL copy of %N is CREATED %N\n", node, cp));
	} else {
		/* temporary copy is existent but without correct ins */
		cp = get_inversion_copy(node);
		DB((dbg, LEVEL_5, "The FINAL copy of %N is EXISTENT %N\n", node, cp));
	}

	if (!is_Block(node)) {
		ir_node *const cpblock = get_inversion_copy(get_nodes_block(node));
		set_nodes_block(cp, cpblock);
		if (is_Phi(cp))
			add_Block_phi(cpblock, cp);
	}

	/* Keeps phi list of temporary node. */
	set_irn_in(cp, arity, cpin);
}

/**
 * This walker copies all walked nodes.
 * If the walk_condition is true for a node, it is copied.
 * All nodes node_info->copy have to be NULL prior to every walk.
 * Order of ins is important for later usage.
 * Takes copy_index, to phase-link copy at specific index.
 */
static void copy_walk_n(ir_node *const node, walker_condition *const walk_condition, int const copy_index)
{
	/* break condition and cycle resolver, creating temporary node copies */
	if (irn_visited(node)) {
		/* Here we rely on nodestate's copy being initialized with NULL */
		DB((dbg, LEVEL_5, "copy_walk: We have already visited %N\n", node));
		if (get_unroll_copy(node, copy_index) == NULL) {
			ir_node *const u = copy_node(node);
			set_unroll_copy(node, copy_index, u);
			DB((dbg, LEVEL_5, "The TEMP unknown of %N is created %N\n", node, u));
		}
		return;
	}

	/* Walk */
	mark_irn_visited(node);

	if (!is_Block(node)) {
		ir_node *const block = get_nodes_block(node);
		if (walk_condition(block))
			DB((dbg, LEVEL_5, "walk block %N\n", block));
		copy_walk_n(block, walk_condition, copy_index);
	}

	int       const arity = get_irn_arity(node);
	ir_node **const cpin  = ALLOCAN(ir_node*, arity);

	foreach_irn_in(node, i, pred) {
		if (walk_condition(pred)) {
			DB((dbg, LEVEL_5, "walk node %N\n", pred));
			copy_walk_n(pred, walk_condition, copy_index);
			cpin[i] = get_unroll_copy(pred, copy_index);
		} else {
			cpin[i] = pred;
		}
	}

	/* copy node / finalize temp node */
	ir_node *cp = get_unroll_copy(node, copy_index);
	if (cp == NULL || is_Unknown(cp)) {
		cp = copy_node(node);
		set_unroll_copy(node, copy_index, cp);
		DB((dbg, LEVEL_5, "The FINAL copy of %N is CREATED %N\n", node, cp));
	} else {
		/* temporary copy is existent but without correct ins */
		cp = get_unroll_copy(node, copy_index);
		DB((dbg, LEVEL_5, "The FINAL copy of %N is EXISTENT %N\n", node, cp));
	}

	if (!is_Block(node)) {
		ir_node *const block   = get_nodes_block(node);
		ir_node *const cpblock = get_unroll_copy(block, copy_index);
		set_nodes_block(cp, cpblock);
		if (is_Phi(cp))
			add_Block_phi(cpblock, cp);
	}

	/* Keeps phi list of temporary node. */
	set_irn_in(cp, arity, cpin);
}

/* Removes all Blocks with non marked predecessors from the condition chain. */
static void unmark_not_allowed_cc_blocks(void)
{
	size_t const blocks = ARR_LEN(cc_blocks);
	for (size_t i = 0; i < blocks; ++i) {
		ir_node *const block = cc_blocks[i];

		/* Head is an exception. */
		if (block == loop_head)
			continue;

		int const arity = get_Block_n_cfgpreds(block);
		for (int a = 0; a < arity; ++a) {
			if (!is_nodes_block_marked(get_Block_cfgpred(block, a))) {
				set_Block_mark(block, 0);
				--inversion_blocks_in_cc;
				DB((dbg, LEVEL_5, "Removed %N from cc (blocks in cc %d)\n",
				    block, inversion_blocks_in_cc));

				break;
			}
		}
	}
}

/* Unmarks all cc blocks using cc_blocks except head.
 * TODO: invert head for unrolling? */
static void unmark_cc_blocks(void)
{
	size_t const blocks = ARR_LEN(cc_blocks);
	for (size_t i = 0; i < blocks; ++i) {
		ir_node *const block = cc_blocks[i];

		/* TODO Head is an exception. */
		/*if (block != loop_head)*/
		set_Block_mark(block, 0);
	}
	/*inversion_blocks_in_cc = 1;*/
	inversion_blocks_in_cc = 0;

	/* invalidate */
	loop_info.cc_size = 0;
}

/**
 * Populates head_entries with (node, pred_pos) tuple
 * whereas the node's pred at pred_pos is in the cc but not the node itself.
 * Also finds df loops inside the cc.
 * Head and condition chain blocks have been marked previously.
 */
static void get_head_outs(ir_node *const node, void *const env)
{
	(void)env;

	foreach_irn_in(node, i, pred) {
		if (!is_nodes_block_marked(node) && is_nodes_block_marked(pred)) {
			/* Saving also predecessor seems redundant, but becomes
			 * necessary when changing position of it, before
			 * dereferencing it.*/
			entry_edge const entry = { .node = node, .pos = i, .pred = pred };
			ARR_APP1(entry_edge, cur_head_outs, entry);
		}
	}

	/* Find df loops inside the cc */
	if (is_Phi(node) && get_nodes_block(node) == loop_head) {
		foreach_irn_in(node, i, pred) {
			if (is_own_backedge(loop_head, i) && is_nodes_block_marked(pred)) {
				entry_edge const entry = { .node = node, .pos = i, .pred = pred };
				ARR_APP1(entry_edge, head_df_loop, entry);
				DB((dbg, LEVEL_5, "Found incc assignment node %N @%d is pred %N, graph %N %N\n",
				    node, i, entry.pred, get_irn_irg(node), get_irg_start_block(get_irn_irg(node))));
			}
		}
	}
}

/**
 * Find condition chains, and add them to be inverted.
 * A block belongs to the chain if a condition branches out of the loop.
 * (Some blocks need to be removed once again.)
 * Returns 1 if the given block belongs to the condition chain.
 */
static void find_condition_chain(ir_node *const block)
{
	mark_irn_visited(block);

	DB((dbg, LEVEL_5, "condition_chains for block %N\n", block));

	/* Get node count */
	unsigned nodes_n = 0;
	foreach_out_edge(block, edge) {
		++nodes_n;
	}

	/* Check if node count would exceed maximum cc size.
	 * TODO
	 * This is not optimal, as we search depth-first and break here,
	 * continuing with another subtree. */
	if (loop_info.cc_size + nodes_n > opt_params.max_cc_size) {
		set_Block_mark(block, 0);
		return;
	}

	/* Check if block only has a jmp instruction. */
	bool jmp_only = true;
	foreach_out_edge(block, edge) {
		ir_node *const src = get_edge_src_irn(edge);
		if (!is_Block(src) && !is_Jmp(src)) {
			jmp_only = false;
			break;
		}
	}

	/* Check cf outs if one is leaving the loop,
	 * or if this node has a backedge. */
	bool has_be = false;
	bool mark   = false;
	foreach_block_succ(block, edge) {
		ir_node *const src = get_edge_src_irn(edge);
		if (!is_in_loop(src))
			mark = true;

		/* Inverting blocks with backedge outs leads to a cf edge
		 * from the inverted head, into the inverted head (skipping the body).
		 * As the body becomes the new loop head,
		 * this would introduce another loop in the existing loop.
		 * This loop inversion cannot cope with this case. */
		int const pos = get_edge_src_pos(edge);
		if (is_backedge(src, pos)) {
			has_be = true;
			break;
		}
	}

	/* We need all predecessors to already belong to the condition chain.
	 * Example of wrong case:  * == in cc
	 *
	 *     Head*             ,--.
	 *    /|   \            B   |
	 *   / A*  B           /    |
	 *  / /\   /          ?     |
	 *   /   C*      =>      D  |
	 *      /  D           Head |
	 *     /               A  \_|
	 *                      C
	 */
	/* Collect blocks containing only a Jmp.
	 * Do not collect blocks with backedge outs. */
	if ((jmp_only || mark) && !has_be) {
		set_Block_mark(block, 1);
		++inversion_blocks_in_cc;
		loop_info.cc_size += nodes_n;
		DB((dbg, LEVEL_5, "block %N is part of condition chain\n", block));
		ARR_APP1(ir_node *, cc_blocks, block);
	} else {
		set_Block_mark(block, 0);
	}

	foreach_block_succ(block, edge) {
		ir_node *const src = get_edge_src_irn(edge);
		if (is_in_loop(src) && !irn_visited(src))
			find_condition_chain(src);
	}
}

/**
 * Rewires the copied condition chain. Removes backedges
 * as this condition chain is prior to the loop.
 * Copy of loop_head must have phi list and old (unfixed) backedge info of the loop head.
 * (loop_head is already fixed, we cannot rely on it.)
 */
static void fix_copy_inversion(void)
{
	ir_node  *const head_cp   = get_inversion_copy(loop_head);
	int       const arity     = get_irn_arity(head_cp);
	int       const backedges = get_backedge_n(head_cp, false);
	int       const new_arity = arity - backedges;
	ir_node **const ins       = ALLOCAN(ir_node*, new_arity);

	int pos = 0;
	/* Remove block backedges */
	foreach_irn_in(head_cp, i, pred) {
		if (!is_backedge(head_cp, i))
			ins[pos++] = pred;
	}

	ir_graph *const irg      = get_irn_irg(head_cp);
	ir_node  *const new_head = new_r_Block(irg, new_arity, ins);
	ir_node       **phis     = NEW_ARR_F(ir_node*, 0);

	for_each_phi(head_cp, phi) {
		pos = 0;
		foreach_irn_in(phi, i, pred) {
			if (!is_backedge(head_cp, i))
				ins[pos++] = pred;
		}
		dbg_info *const dbgi    = get_irn_dbg_info(phi);
		ir_mode  *const mode    = get_irn_mode(phi);
		ir_node  *const new_phi = new_rd_Phi(dbgi, new_head, new_arity, ins, mode);
		ARR_APP1(ir_node*, phis, new_phi);
	}

	pos = 0;
	for_each_phi_safe(head_cp, phi, next) {
		exchange(phi, phis[pos++]);
	}

	exchange(head_cp, new_head);

	DEL_ARR_F(phis);
}


/* Puts the original condition chain at the end of the loop,
 * subsequently to the body.
 * Relies on block phi list and correct backedges.
 */
static void fix_head_inversion(void)
{
	int       const backedges = get_backedge_n(loop_head, false);
	int       const new_arity = backedges;
	ir_node **const ins       = ALLOCAN(ir_node*, new_arity);

	int pos = 0;
	/* Keep only backedges */
	foreach_irn_in(loop_head, i, pred) {
		if (is_own_backedge(loop_head, i))
			ins[pos++] = pred;
	}

	ir_graph *const irg      = get_irn_irg(loop_head);
	ir_node  *const new_head = new_r_Block(irg, new_arity, ins);
	ir_node       **phis     = NEW_ARR_F(ir_node *, 0);

	for_each_phi(loop_head, phi) {
		DB((dbg, LEVEL_5, "Fixing phi %N of loop head\n", phi));

		pos = 0;
		foreach_irn_in(phi, i, pred) {
			if (is_own_backedge(loop_head, i)) {
				/* If assignment is in the condition chain,
				 * we need to create a phi in the new loop head.
				 * This can only happen for df, not cf. See find_condition_chains. */
				/*if (is_nodes_block_marked(pred)) {
					ins[pos++] = pred;
				} else {*/
				ins[pos++] = pred;
			}
		}

		dbg_info *const dbgi    = get_irn_dbg_info(phi);
		ir_mode  *const mode    = get_irn_mode(phi);
		ir_node  *const new_phi = new_rd_Phi(dbgi, new_head, new_arity, ins, mode);
		ARR_APP1(ir_node*, phis, new_phi);

		DB((dbg, LEVEL_5, "fix inverted head should exchange %+F by %+F (pos %d)\n", phi, new_phi, pos));
	}

	pos = 0;
	for_each_phi_safe(loop_head, phi, next) {
		DB((dbg, LEVEL_5, "fix inverted head exchange %+F by %+F\n", phi, phis[pos]));
		if (phis[pos] != phi) {
			if (get_Phi_loop(phi)) {
				remove_keep_alive(phi);
				set_Phi_loop(phi, false);
			}
			exchange(phi, phis[pos++]);
		}
	}

	DEL_ARR_F(phis);

	DB((dbg, LEVEL_5, "fix inverted head exchange head block %N by %N\n", loop_head, new_head));
	exchange(loop_head, new_head);
}

/* Does the loop inversion. */
static void inversion_walk(ir_graph *const irg, entry_edge *const head_entries)
{
	/* The order of rewiring bottom-up is crucial.
	 * Any change of the order leads to lost information that would be needed later.
	 */

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);

	/* 1. clone condition chain */
	inc_irg_visited(irg);

	for (size_t i = 0; i < ARR_LEN(head_entries); ++i) {
		entry_edge const entry = head_entries[i];
		ir_node   *const pred  = get_irn_n(entry.node, entry.pos);

		DB((dbg, LEVEL_5, "\nInit walk block %N\n", pred));

		copy_walk(pred, is_nodes_block_marked, cur_loop);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);

	/* 2. Extends the head control flow successors ins
	 *    with the definitions of the copied head node. */
	for (size_t i = 0; i < ARR_LEN(head_entries); ++i) {
		entry_edge const head_out = head_entries[i];
		if (is_Block(head_out.node))
			extend_ins_by_copy(head_out.node, head_out.pos);
	}

	/* 3. construct_ssa for users of definitions in the condition chain,
	 *    as there is now a second definition. */
	for (size_t i = 0; i < ARR_LEN(head_entries); ++i) {
		entry_edge const head_out = head_entries[i];

		/* Ignore keepalives */
		if (is_End(head_out.node))
			continue;

		/* Construct ssa for assignments in the condition chain. */
		if (!is_Block(head_out.node)) {
			ir_node *const pred    = head_out.pred;
			ir_node *const cppred  = get_inversion_copy(pred);
			ir_node *const block   = get_nodes_block(pred);
			ir_node *const cpblock = get_nodes_block(cppred);
			construct_ssa(block, pred, cpblock, cppred);
		}
	}

	/*
	 * If there is an assignment in the condition chain
	 * with a user also in the condition chain,
	 * the dominance frontier is in the new loop head.
	 * The dataflow loop is completely in the condition chain.
	 * Goal:
	 *  To be wired: >|
	 *
	 *  | ,--.   |
	 * Phi_cp |  | copied condition chain
	 * >| |   |  |
	 * >| ?__/   |
	 * >| ,-.
	 *  Phi* |   | new loop head with newly created phi.
	 *   |   |
	 *  Phi  |   | original, inverted condition chain
	 *   |   |   |
	 *   ?__/    |
	 *
	 */
	for (size_t i = 0; i < ARR_LEN(head_df_loop); ++i) {
		entry_edge const head_out = head_df_loop[i];

		/* Construct ssa for assignments in the condition chain. */
		ir_node *const pred   = head_out.pred;
		ir_node *const cppred = get_inversion_copy(pred);
		assert(cppred && pred);
		ir_node *const block   = get_nodes_block(pred);
		ir_node *const cpblock = get_nodes_block(cppred);
		construct_ssa(block, pred, cpblock, cppred);
	}

	/* 4. Remove the ins which are no backedges from the original condition chain
	 *    as the cc is now subsequent to the body. */
	fix_head_inversion();

	/* 5. Remove the backedges of the copied condition chain,
	 *    because it is going to be the new 'head' in advance to the loop. */
	fix_copy_inversion();
}

/* Performs loop inversion of cur_loop if possible and reasonable. */
static void loop_inversion(ir_graph *const irg)
{
	unsigned const max_loop_nodes = opt_params.max_loop_size;

	/* Depth of 0 is the procedure and 1 a topmost loop. */
	int const loop_depth = get_loop_depth(cur_loop) - 1;

	/* Calculating in per mil. */
	unsigned const max_loop_nodes_adapted = get_max_nodes_adapted(loop_depth);

	DB((dbg, LEVEL_1, "max_nodes: %d\nmax_nodes_adapted %d at depth of %d (adaption %d)\n",
	    max_loop_nodes, max_loop_nodes_adapted, loop_depth, opt_params.depth_adaption));

	if (loop_info.nodes == 0)
		return;

	if (loop_info.nodes > max_loop_nodes) {
		/* Only for stats */
		DB((dbg, LEVEL_1, "Nodes %d > allowed nodes %d\n",
			loop_info.nodes, loop_depth, max_loop_nodes));
		++stats.too_large;
		/* no RETURN */
		/* Adaption might change it */
	}

	/* Limit processing to loops smaller than given parameter. */
	if (loop_info.nodes > max_loop_nodes_adapted) {
		DB((dbg, LEVEL_1, "Nodes %d > allowed nodes (depth %d adapted) %d\n",
			loop_info.nodes, loop_depth, max_loop_nodes_adapted));
		++stats.too_large_adapted;
		return;
	}

	if (loop_info.calls > opt_params.allowed_calls) {
		DB((dbg, LEVEL_1, "Calls %d > allowed calls %d\n",
			loop_info.calls, opt_params.allowed_calls));
		++stats.calls_limit;
		return;
	}

	/*inversion_head_node_limit = INT_MAX;*/
	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_MARK);

	/* Reset block marks.
	 * We use block marks to flag blocks of the original condition chain. */
	irg_walk_graph(irg, reset_block_mark, NULL, NULL);

	head_df_loop = NEW_ARR_F(entry_edge, 0);

	/*head_inversion_node_count = 0;*/
	inversion_blocks_in_cc = 0;

	/* Use phase to keep copy of nodes from the condition chain. */
	ir_nodemap_init(&map, irg);
	obstack_init(&obst);

	/* Search for condition chains and temporarily save the blocks in an array. */
	cc_blocks = NEW_ARR_F(ir_node*, 0);
	inc_irg_visited(irg);
	find_condition_chain(loop_head);

	unmark_not_allowed_cc_blocks();
	DEL_ARR_F(cc_blocks);

	bool do_inversion = true;

	/* Condition chain too large.
	 * Loop should better be small enough to fit into the cache. */
	/* TODO Of course, we should take a small enough cc in the first place,
	 * which is not that simple. (bin packing)  */
	if (loop_info.cc_size > opt_params.max_cc_size) {
		++stats.cc_limit_reached;

		do_inversion = false;

		/* Unmark cc blocks except the head.
		 * Invert head only for possible unrolling. */
		unmark_cc_blocks();
	}

	/* We also catch endless loops here,
	 * because they do not have a condition chain. */
	if (inversion_blocks_in_cc < 1) {
		do_inversion = false;
		DB((dbg, LEVEL_3,
			"Loop contains %d (less than 1) invertible blocks => No Inversion done.\n",
			inversion_blocks_in_cc));
	}

	if (do_inversion) {
		cur_head_outs = NEW_ARR_F(entry_edge, 0);

		/* Get all edges pointing into the condition chain. */
		irg_walk_graph(irg, get_head_outs, NULL, NULL);

		/* Do the inversion */
		inversion_walk(irg, cur_head_outs);

		DEL_ARR_F(cur_head_outs);

		/* Duplicated blocks changed doms */
		clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		                   | IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);

		++stats.inverted;
	}

	/* free */
	obstack_free(&obst, NULL);
	ir_nodemap_destroy(&map);
	DEL_ARR_F(head_df_loop);

	ir_free_resources(irg, IR_RESOURCE_BLOCK_MARK);
}

/* Fix the original loop_heads ins for invariant unrolling case. */
static void unrolling_fix_loop_head_inv(void)
{

	/* Original loop_heads ins are:
	 * duff block and the own backedge */

	ir_node *const proj            = new_r_Proj(loop_info.duff_cond, mode_X, 0);
	ir_node *const head_pred       = get_Block_cfgpred(loop_head, loop_info.be_src_pos);
	ir_node *const loop_condition  = get_unroll_copy(head_pred, unroll_nr - 1);
	ir_node *const loop_head_ins[] = { loop_condition, proj };
	set_irn_in(loop_head, ARRAY_SIZE(loop_head_ins), loop_head_ins);
	DB((dbg, LEVEL_4, "Rewire ins of block loophead %N to pred %N and duffs entry %N \n" , loop_head, loop_head_ins[0], loop_head_ins[1]));

	for_each_phi(loop_head, phi) {
		ir_node *const pred      = get_Phi_pred(phi, loop_info.be_src_pos);
		/* TODO we think it is a phi, but for Mergesort it is not the case.*/
		ir_node *const last_pred = get_unroll_copy(pred, unroll_nr - 1);

		ir_node *const phi_ins[] = { last_pred, (ir_node*)get_irn_link(phi) };
		set_irn_in(phi, ARRAY_SIZE(phi_ins), phi_ins);
		DB((dbg, LEVEL_4, "Rewire ins of loophead phi %N to pred %N and duffs entry %N \n" , phi, phi_ins[0], phi_ins[1]));
	}
}

/* Removes previously created phis with only 1 in. */
static void correct_phis(ir_node *const node, void *const env)
{
	(void)env;

	if (is_Phi(node) && get_irn_arity(node) == 1) {
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = get_nodes_block(node);
		ir_node  *const in[]  = { get_Phi_pred(node, 0) };
		ir_mode  *const mode  = get_irn_mode(node);
		ir_node  *const exch  = new_rd_Phi(dbgi, block, ARRAY_SIZE(in), in, mode);
		exchange(node, exch);
	}
}

/* Unrolling: Rewire floating copies. */
static void place_copies(int const copies)
{
	ir_node *const loophead   = loop_head;
	int      const be_src_pos = loop_info.be_src_pos;

	/* Serialize loops by fixing their head ins.
	 * Processed are the copies.
	 * The original loop is done after that, to keep backedge infos. */
	for (int c = 0; c < copies; ++c) {
#ifdef DEBUG_libfirm
		ir_node *const upper            = get_unroll_copy(loophead, c);
#endif
		ir_node *const lower            = get_unroll_copy(loophead, c + 1);
		ir_node *const topmost_be_block = get_nodes_block(get_Block_cfgpred(loophead, be_src_pos));

		/* Important: get the preds first and then their copy. */
		ir_node *const upper_be_block = get_unroll_copy(topmost_be_block, c);
		ir_node *const new_jmp        = new_r_Jmp(upper_be_block);
		DB((dbg, LEVEL_5, " place_copies upper %N lower %N\n", upper, lower));

		DB((dbg, LEVEL_5, "topmost be block %N \n", topmost_be_block));

		if (loop_info.unroll_kind == constant) {
			ir_node *ins[] = { new_jmp };
			set_irn_in(lower, ARRAY_SIZE(ins), ins);

			for_each_phi(loophead, phi) {
				ir_node *const topmost_def = get_Phi_pred(phi, be_src_pos);
				ir_node *const upper_def   = get_unroll_copy(topmost_def, c);
				ir_node *const lower_phi   = get_unroll_copy(phi, c + 1);

				/* It is possible, that the value used
				 * in the OWN backedge path is NOT defined in this loop. */
				if (is_in_loop(topmost_def))
					ins[0] = upper_def;
				else
					ins[0] = topmost_def;

				set_irn_in(lower_phi, ARRAY_SIZE(ins), ins);
				/* Need to replace phis with 1 in later. */
			}
		} else {
			/* Calculate corresponding projection of mod result for this copy c */
			ir_node *proj = new_r_Proj(loop_info.duff_cond, mode_X, unroll_nr - c - 1);
			DB((dbg, LEVEL_4, "New duff proj %N\n" , proj));

			/* Invariant case */
			/* Every node has 2 ins. One from the duff blocks
			 * and one from the previously unrolled loop. */
			ir_node *ins[] = { new_jmp, proj };
			set_irn_in(lower, ARRAY_SIZE(ins), ins);
			DB((dbg, LEVEL_4, "Rewire ins of Block %N to pred %N and duffs entry %N \n" , lower, ins[0], ins[1]));

			for_each_phi(loophead, phi) {
				ir_node *const lower_phi = get_unroll_copy(phi, c + 1);
				ir_node *const duff_phi  = (ir_node*)get_irn_link(phi);
				DB((dbg, LEVEL_4, "DD Link of %N is %N\n" , phi, duff_phi));

				ir_node *const topmost_phi_pred = get_Phi_pred(phi, be_src_pos);
				ir_node *const upper_phi_pred   = is_in_loop(topmost_phi_pred) ?
					get_unroll_copy(topmost_phi_pred, c) : topmost_phi_pred;

				ir_node *phi_ins[] = { upper_phi_pred, duff_phi };
				set_irn_in(lower_phi, ARRAY_SIZE(phi_ins), phi_ins);
				DB((dbg, LEVEL_4, "Rewire ins of %N to pred %N and duffs entry %N \n" , lower_phi, phi_ins[0], phi_ins[1]));
			}
		}
	}

	/* Reconnect last copy. */
	for (size_t i = 0; i < ARR_LEN(loop_entries); ++i) {
		entry_edge const edge     = loop_entries[i];
		/* Last copy is at the bottom */
		ir_node   *const new_pred = get_unroll_copy(edge.pred, copies);
		set_irn_n(edge.node, edge.pos, new_pred);
	}

	/* Fix original loops head.
	 * Done in the end, as ins and be info were needed before. */
	if (loop_info.unroll_kind == constant) {
		ir_node *const head_pred      = get_Block_cfgpred(loop_head, be_src_pos);
		ir_node *const loop_condition = get_unroll_copy(head_pred, unroll_nr - 1);

		set_irn_n(loop_head, loop_info.be_src_pos, loop_condition);

		for_each_phi(loop_head, phi) {
			ir_node *const pred = get_Phi_pred(phi, be_src_pos);

			/* It is possible, that the value used
			 * in the OWN backedge path is NOT assigned in this loop. */
			ir_node *const last_pred = is_in_loop(pred) ?
				get_unroll_copy(pred, copies) : pred;
			set_irn_n(phi, be_src_pos, last_pred);
		}
	} else {
		unrolling_fix_loop_head_inv();
	}
}

/* Copies the cur_loop several times. */
static void copy_loop(ir_graph *const irg, entry_edge *const cur_loop_outs, int const copies)
{
	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);

	for (int c = 0; c < copies; ++c) {
		inc_irg_visited(irg);

		DB((dbg, LEVEL_5, "         ### Copy_loop  copy nr: %d ###\n", c));
		for (size_t i = 0; i < ARR_LEN(cur_loop_outs); ++i) {
			entry_edge const entry = cur_loop_outs[i];
			ir_node   *const pred  = get_irn_n(entry.node, entry.pos);
			copy_walk_n(pred, is_in_loop, c + 1);
		}
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
}


/* Creates a new phi from the given phi node omitting own bes,
 * using be_block as supplier of backedge informations. */
static ir_node *clone_phis_sans_bes(ir_node *const phi, ir_node *const be_block, ir_node *const dest_block)
{
	int const arity = get_Phi_n_preds(phi);
	assert(arity == get_Block_n_cfgpreds(be_block));

	int             c   = 0;
	ir_node **const ins = ALLOCAN(ir_node*, arity);
	foreach_irn_in(phi, i, pred) {
		if (!is_own_backedge(be_block, i))
			ins[c++] = pred;
	}

	ir_mode *const mode   = get_irn_mode(phi);
	ir_node *const newphi = new_r_Phi(dest_block, c, ins, mode);

	set_irn_link(phi, newphi);
	DB((dbg, LEVEL_4, "Linking for duffs device %N to %N\n", phi, newphi));

	return newphi;
}

/* Creates a new block from the given block node omitting own bes,
 * using be_block as supplier of backedge informations. */
static ir_node *clone_block_sans_bes(ir_graph *const irg, ir_node *const node, ir_node *const be_block)
{
	int const arity = get_Block_n_cfgpreds(node);
	assert(arity == get_irn_arity(be_block));

	int             c   = 0;
	ir_node **const ins = ALLOCAN(ir_node*, arity);
	foreach_irn_in(node, i, pred) {
		if (!is_own_backedge(be_block, i))
			ins[c++] = pred;
	}

	return new_r_Block(irg, c, ins);
}

/* Creates a structure to calculate absolute value of node op.
 * Returns mux node with absolute value. */
static ir_node *new_Abs(ir_node *const op, ir_mode *const mode)
{
	ir_node  *const block    = get_nodes_block(op);
	ir_graph *const irg      = get_irn_irg(op);
	ir_node  *const zero     = new_r_Const_null(irg, mode);
	ir_node  *const cmp      = new_r_Cmp(block, op, zero, ir_relation_less);
	ir_node  *const minus_op = new_r_Minus(block, op);
	ir_node  *const mux      = new_r_Mux(block, cmp, op, minus_op);
	return mux;
}


/* Creates blocks for duffs device, using previously obtained
 * informations about the iv.
 * TODO split */
static void create_duffs_block(ir_graph *const irg)
{
	/* TODO naming
	 * 1. Calculate first approach to count.
	 *    Condition: (end - start) % step == 0 */
	ir_node *const block1 = clone_block_sans_bes(irg, loop_head, loop_head);
	DB((dbg, LEVEL_4, "Duff block 1 %N\n", block1));

	/* Create loop entry phis in first duff block
	 * as it becomes the loops preheader */
	for_each_phi(loop_head, phi) {
		/* Returns phis pred if phi would have arity 1*/
		ir_node *const new_phi = clone_phis_sans_bes(phi, loop_head, block1);

		(void)new_phi;
		DB((dbg, LEVEL_4, "HEAD %N phi %N\n", loop_head, phi));
		DB((dbg, LEVEL_4, "BLOCK1 %N phi %N\n", block1, new_phi));
	}

	ir_mode *const mode = get_irn_mode(loop_info.end_val);
	ir_node *const ems  = new_r_Sub(block1, loop_info.end_val, loop_info.start_val);
	DB((dbg, LEVEL_4, "BLOCK1 sub %N\n", ems));

	DB((dbg, LEVEL_4, "mod ins %N %N\n", ems, loop_info.step));
	ir_node *const nomem   = get_irg_no_mem(irg);
	ir_node *const ems_mod = new_r_Mod(block1, nomem, ems, loop_info.step, true);
	ir_node *const mod_res = new_r_Proj(ems_mod, mode_Iu, pn_Mod_res);

	ir_node *const ems_div = new_r_Div(block1, nomem, ems, loop_info.step, true);
	ir_node *const div_res = new_r_Proj(ems_div, mode, pn_Div_res);

	DB((dbg, LEVEL_4, "New modulus node %N\n", ems_mod));

	ir_node *const null          = new_r_Const_null(irg, mode);
	ir_node *const cmp_null      = new_r_Cmp(block1, mod_res, null, ir_relation_less);
	ir_node *const ems_mode_cond = new_r_Cond(block1, cmp_null);

	/* ems % step == 0 */
	ir_node *const x_true  = new_r_Proj(ems_mode_cond, mode_X, pn_Cond_true);
	/* ems % step != 0 */
	ir_node *const x_false = new_r_Proj(ems_mode_cond, mode_X, pn_Cond_false);

	/* 2. Second block.
	 * Assures, duffs device receives a valid count.
	 * Condition:
	 *     decreasing: count < 0
	 *     increasing: count > 0
	 */
	ir_node *const count_block_ins[] = { x_true, x_false };
	ir_node *const count_block       = new_r_Block(irg, ARRAY_SIZE(count_block_ins), count_block_ins);
	DB((dbg, LEVEL_4, "Duff block 2 %N\n", count_block));


	/* Increase loop-taken-count depending on the loop condition
	 * uses the latest iv to compare to. */
	ir_node       *true_val;
	ir_node       *false_val;
	ir_node *const one = new_r_Const_one(irg, mode);
	if (loop_info.latest_value == 1) {
		/* ems % step == 0 :  +0 */
		true_val  = null;
		/* ems % step != 0 :  +1 */
		false_val = one;
	} else {
		/* ems % step == 0 :  +1 */
		true_val  = one;
		/* ems % step != 0 :  +2 */
		false_val = new_r_Const_long(irg, mode, 2);
	}

	ir_node *const correction_ins[] = { true_val, false_val };
	ir_node *const correction       = new_r_Phi(count_block, ARRAY_SIZE(correction_ins), correction_ins, mode);

	/* (end - start) / step  +  correction */
	ir_node *const count = new_r_Add(count_block ,div_res, correction);

	/* We preconditioned the loop to be tail-controlled.
	 * So, if count is something 'wrong' like 0,
	 * negative/positive (depending on step direction),
	 * we may take the loop once (tail-contr.) and leave it
	 * to the existing condition, to break; */

	/* Depending on step direction, we have to check for > or < 0 */
	ir_relation const rel           = loop_info.decreasing == 1 ? ir_relation_less : ir_relation_greater;
	ir_node    *const cmp_bad_count = new_r_Cmp(count_block, count, null, rel);
	ir_node    *const bad_count_neg = new_r_Cond(count_block, cmp_bad_count);
	ir_node    *const good_count    = new_r_Proj(bad_count_neg, mode_X, pn_Cond_true);
	ir_node    *const bad_count     = new_r_Proj(ems_mode_cond, mode_X, pn_Cond_false);

	/* 3. Duff Block
	 *    Contains module to decide which loop to start from. */

	ir_node *const duff_block_ins[] = { good_count, bad_count };
	ir_node *const duff_block       = new_r_Block(irg, ARRAY_SIZE(duff_block_ins), duff_block_ins);
	DB((dbg, LEVEL_4, "Duff block 3 %N\n", duff_block));

	/* Get absolute value */
	ir_node *const abs = new_Abs(count, mode);
	/* Manually feed the aforementioned count = 1 (bad case)*/
	ir_node *const count_phi_ins[] = { abs, one };
	ir_node *const count_phi       = new_r_Phi(duff_block, ARRAY_SIZE(count_phi_ins), count_phi_ins, mode);

	/* count % unroll_nr */
	ir_node *const unroll_c = new_r_Const_long(irg, mode, (long)unroll_nr);
	ir_node *const duff_mod = new_r_Mod(duff_block, nomem, count_phi, unroll_c, true);
	ir_node *const proj     = new_r_Proj(duff_mod, mode, pn_Mod_res);
	/* condition does NOT create itself in the block of the proj! */
	ir_node *const cond     = new_r_Cond(duff_block, proj);

	loop_info.duff_cond = cond;
}

/* Returns 1 if given node is not in loop,
 * or if it is a phi of the loop head with only loop invariant defs.
 */
static unsigned is_loop_invariant_def(ir_node *const node)
{
	if (!is_in_loop(node)) {
		DB((dbg, LEVEL_4, "Not in loop %N\n", node));
		return 1;
	}

	/* If this is a phi of the loophead shared by more than 1 loop,
	 * we need to check if all defs are not in the loop.  */
	if (is_Phi(node)) {
		ir_node *const block = get_nodes_block(node);

		/* To prevent unexpected situations. */
		if (block != loop_head)
			return 0;

		foreach_irn_in(node, i, pred) {
			/* Check if all bes are just loopbacks. */
			if (is_own_backedge(block, i) && pred != node)
				return 0;
		}
		DB((dbg, LEVEL_4, "invar %N\n", node));
		return 1;
	}

	DB((dbg, LEVEL_4, "Not invar %N\n", node));
	return 0;
}

/* Returns 1 if one pred of node is invariant and the other is not.
 * invar_pred and other are set analogously. */
static unsigned get_invariant_pred(ir_node *const node, ir_node **const invar_pred, ir_node **const other)
{
	ir_node *const pred0 = get_Cmp_left(node);
	ir_node *const pred1 = get_Cmp_right(node);

	*invar_pred = NULL;
	*other      = NULL;

	if (is_loop_invariant_def(pred0)) {
		DB((dbg, LEVEL_4, "pred0 invar %N\n", pred0));
		*invar_pred = pred0;
		*other      = pred1;
	}

	if (is_loop_invariant_def(pred1)) {
		DB((dbg, LEVEL_4, "pred1 invar %N\n", pred1));

		/* We do not want both preds to be invariant. */
		if (*invar_pred != NULL)
			return 0;

		*other      = pred0;
		*invar_pred = pred1;
		return 1;
	} else {
		DB((dbg, LEVEL_4, "pred1 not invar %N\n", pred1));

		if (*invar_pred != NULL)
			return 1;
		else
			return 0;
	}
}

static bool is_const(ir_node *const node)
{
	switch (get_irn_opcode(node)) {
	case iro_Address:
	case iro_Align:
	case iro_Const:
	case iro_Offset:
	case iro_Size:
		return true;
	default:
		return false;
	}
}

/* Starts from a phi that may belong to an iv.
 * If an add forms a loop with iteration_phi,
 * and add uses a constant, 1 is returned
 * and 'start' as well as 'add' are sane. */
static unsigned get_start_and_add(unrolling_kind_flag const role)
{
	ir_node *const iteration_phi = loop_info.iteration_phi;

	DB((dbg, LEVEL_4, "Find start and add from %N\n", iteration_phi));

	ir_node *found_add = loop_info.add;
	foreach_irn_in(iteration_phi, i, found_start_val) {
		ir_node *const block = get_nodes_block(iteration_phi);

		/* Find start_val which needs to be pred of the iteration_phi.
		 * If start_val already known, sanity check. */
		if (!is_backedge(block, i)) {
			DB((dbg, LEVEL_4, "found_start_val %N\n", found_start_val));

			/* We already found a start_val it has to be always the same. */
			if (loop_info.start_val && found_start_val != loop_info.start_val)
				return 0;

			switch (role) {
			case constant:  if (!is_const(found_start_val))              return 0; break;
			case invariant: if (!is_loop_invariant_def(found_start_val)) return 0; break;
			}

			loop_info.start_val = found_start_val;
		}

		/* The phi has to be in the loop head.
		 * Follow all own backedges. Every value supplied from these preds of the phi
		 * needs to origin from the same add. */
		if (is_own_backedge(block, i)) {
			DB((dbg, LEVEL_4, "is add? %N\n", found_start_val));

			if (!is_Add(found_start_val) && !is_Sub(found_start_val))
				return 0;
			if (found_add && found_add != found_start_val)
				return 0;
			found_add = found_start_val;
		}
	}

	loop_info.add = found_add;
	return 1;
}


/* Returns 1 if one pred of node is a const value and the other is not.
 * const_pred and other are set analogously. */
static unsigned get_const_pred(ir_node *const node, ir_node **const const_pred, ir_node **const other)
{
	ir_node *pred0 = get_irn_n(node, 0);
	ir_node *pred1 = get_irn_n(node, 1);

	DB((dbg, LEVEL_4, "Checking for constant pred of %N\n", node));

	*const_pred = NULL;
	*other      = NULL;

	/*DB((dbg, LEVEL_4, "is %N const\n", pred0));*/
	if (is_const(pred0)) {
		*const_pred = pred0;
		*other      = pred1;
	}

	/*DB((dbg, LEVEL_4, "is %N const\n", pred1));*/
	if (is_const(pred1)) {
		if (*const_pred != NULL) {
			/* RETURN. We do not want both preds to be constant. */
			return 0;
		}

		*other      = pred0;
		*const_pred = pred1;
	}

	if (*const_pred == NULL)
		return 0;
	else
		return 1;
}

/* Returns 1 if loop exits within 2 steps of the iv.
 * Norm_proj means we do not exit the loop.*/
static unsigned simulate_next(ir_tarval **const count_tar, ir_tarval *const stepped, ir_tarval *const step_tar, ir_tarval *const end_tar, ir_relation const norm_proj)
{
	DB((dbg, LEVEL_4, "Loop taken if (stepped)%ld %s (end)%ld ",
	    get_tarval_long(stepped),
	    get_relation_string((norm_proj)),
	    get_tarval_long(end_tar)));
	DB((dbg, LEVEL_4, "comparing latest value %d\n", loop_info.latest_value));

	/* If current iv does not stay in the loop,
	 * this run satisfied the exit condition. */
	if (!(tarval_cmp(stepped, end_tar) & norm_proj))
		return 1;

	DB((dbg, LEVEL_4, "Result: (stepped)%ld IS %s (end)%ld\n",
	    get_tarval_long(stepped),
	    get_relation_string(tarval_cmp(stepped, end_tar)),
	    get_tarval_long(end_tar)));

	/* next step */
	ir_tarval *const next = is_Add(loop_info.add) ?
		tarval_add(stepped, step_tar) :
		tarval_sub(stepped, step_tar);

	DB((dbg, LEVEL_4, "Loop taken if %ld %s %ld ",
	    get_tarval_long(next),
	    get_relation_string(norm_proj),
	    get_tarval_long(end_tar)));
	DB((dbg, LEVEL_4, "comparing latest value %d\n", loop_info.latest_value));

	/* Increase steps. */
	*count_tar = tarval_add(*count_tar, get_mode_one(get_tarval_mode(*count_tar)));

	/* Next has to fail the loop condition, or we will never exit. */
	if (!(tarval_cmp(next, end_tar) & norm_proj))
		return 1;
	else
		return 0;
}

/* Check if loop meets requirements for a 'simple loop':
 * - Exactly one cf out
 * - Allowed calls
 * - Max nodes after unrolling
 * - tail-controlled
 * - exactly one be
 * - cmp
 * Returns cmp node or NULL; */
static ir_node *is_simple_loop(void)
{
	/* Maximum of one condition, and no endless loops. */
	if (loop_info.cf_outs != 1)
		return NULL;

	DB((dbg, LEVEL_4, "1 loop exit\n"));

	/* Calculate maximum unroll_nr keeping node count below limit. */
	loop_info.max_unroll = (int)((double)opt_params.max_unrolled_loop_size / (double)loop_info.nodes);
	if (loop_info.max_unroll < 2) {
		++stats.too_large;
		return NULL;
	}

	DB((dbg, LEVEL_4, "maximum unroll factor %u, to not exceed node limit \n", opt_params.max_unrolled_loop_size));

	/* RETURN if we have more than 1 be. */
	/* Get my backedges without alien bes. */
	ir_node *loop_block = NULL;
	foreach_irn_in(loop_head, i, pred) {
		if (is_own_backedge(loop_head, i)) {
			/* Our simple loops may have only one backedge. */
			if (loop_block)
				return NULL;
			loop_block           = get_nodes_block(pred);
			loop_info.be_src_pos = i;
		}
	}

	DB((dbg, LEVEL_4, "loop has 1 own backedge.\n"));

	ir_node *const exit_block = get_nodes_block(loop_info.cf_out);
	/* The loop has to be tail-controlled.
	 * This can be changed/improved,
	 * but we would need a duff iv. */
	if (exit_block != loop_block)
		return NULL;

	DB((dbg, LEVEL_4, "tail-controlled loop.\n"));

	/* find value on which loop exit depends */
	ir_node *const projx = loop_info.cf_out;
	ir_node *const cond  = get_Proj_pred(projx);
	ir_node *const cmp   = get_Cond_selector(cond);
	if (!is_Cmp(cmp))
		return NULL;

	DB((dbg, LEVEL_5, "projection is %s\n", get_relation_string(get_Cmp_relation(cmp))));

	switch (get_Proj_num(projx)) {
	case pn_Cond_false: loop_info.exit_cond = 0; break;
	case pn_Cond_true:  loop_info.exit_cond = 1; break;
	default: panic("Cond Proj_proj other than true/false");
	}

	DB((dbg, LEVEL_4, "Valid Cmp.\n"));
	return cmp;
}

/* Returns 1 if all nodes are mode_Iu or mode_Is. */
static unsigned are_mode_I(ir_node *const n1, ir_node *const n2, ir_node *const n3)
{
	ir_mode *const m1 = get_irn_mode(n1);
	ir_mode *const m2 = get_irn_mode(n2);
	ir_mode *const m3 = get_irn_mode(n3);

	if (m1 != m2 || m1 != m3)
		return 0;
	if (m1 != mode_Iu && m1 != mode_Is)
		return 0;
	return 1;
}

/* Checks if cur_loop is a simple tail-controlled counting loop
 * with start and end value loop invariant, step constant. */
static unsigned get_unroll_decision_invariant(ir_graph *const irg)
{
	/* RETURN if loop is not 'simple' */
	ir_node *const loop_condition = is_simple_loop();
	if (!loop_condition)
		return 0;

	/* Use a minimal size for the invariant unrolled loop,
	 * as duffs device produces overhead */
	if (loop_info.nodes < opt_params.invar_unrolling_min_size)
		return 0;

	ir_node *iteration_path;
	unsigned const success = get_invariant_pred(loop_condition, &loop_info.end_val, &iteration_path);
	DB((dbg, LEVEL_4, "pred invar %d\n", success));
	if (!success)
		return 0;

	DB((dbg, LEVEL_4, "Invariant End_val %N, other %N\n", loop_info.end_val, iteration_path));

	/* We may find the add or the phi first.
	 * Until now we only have end_val. */
	if (is_Add(iteration_path) || is_Sub(iteration_path)) {
		loop_info.add = iteration_path;
		DB((dbg, LEVEL_4, "Case 1: Got add %N (maybe not sane)\n", loop_info.add));

		/* Preds of the add should be step and the iteration_phi */
		if (!get_const_pred(loop_info.add, &loop_info.step, &loop_info.iteration_phi))
			return 0;

		DB((dbg, LEVEL_4, "Got step %N\n", loop_info.step));

		if (!is_Phi(loop_info.iteration_phi))
			return 0;

		DB((dbg, LEVEL_4, "Got phi %N\n", loop_info.iteration_phi));

		/* Find start_val.
		 * Does necessary sanity check of add, if it is already set.  */
		if (!get_start_and_add(invariant))
			return 0;

		DB((dbg, LEVEL_4, "Got start A  %N\n", loop_info.start_val));

	} else if (is_Phi(iteration_path)) {
		loop_info.iteration_phi = iteration_path;
		DB((dbg, LEVEL_4, "Case 2: Got phi %N\n", loop_info.iteration_phi));

		/* Find start_val and add-node.
		 * Does necessary sanity check of add, if it is already set.  */
		if (!get_start_and_add(invariant))
			return 0;

		DB((dbg, LEVEL_4, "Got start B %N\n", loop_info.start_val));
		DB((dbg, LEVEL_4, "Got add or sub %N\n", loop_info.add));

		ir_node *new_iteration_phi;
		if (!get_const_pred(loop_info.add, &loop_info.step, &new_iteration_phi))
			return 0;

		DB((dbg, LEVEL_4, "Got step (B) %N\n", loop_info.step));

		if (loop_info.iteration_phi != new_iteration_phi)
			return 0;
	} else {
		return 0;
	}

	DB((dbg, LEVEL_4, "start %N, end %N, step %N\n",
	    loop_info.start_val, loop_info.end_val, loop_info.step));

	ir_mode *const mode = get_irn_mode(loop_info.end_val);
	if (mode != mode_Is && mode != mode_Iu)
		return 0;

	/* TODO necessary? */
	if (!are_mode_I(loop_info.start_val, loop_info.step, loop_info.end_val))
		return 0;

	DB((dbg, LEVEL_4, "mode integer\n"));

	ir_tarval *const step_tar = get_Const_tarval(loop_info.step);
	if (tarval_is_null(step_tar)) {
		/* TODO Might be worth a warning. */
		return 0;
	}

	DB((dbg, LEVEL_4, "step is not 0\n"));

	create_duffs_block(irg);

	return loop_info.max_unroll;
}

/* Returns unroll factor,
 * given maximum unroll factor and number of loop passes. */
static unsigned get_preferred_factor_constant(ir_tarval *const count_tar)
{
	/* If our preference is greater than the allowed unroll factor
	 * we either might reduce the preferred factor and prevent a duffs device block,
	 * or create a duffs device block, from which in this case (constants only)
	 * we know the startloop at compiletime.
	 * The latter yields the following graphs.
	 * but for code generation we would want to use graph A.
	 * The graphs are equivalent. So, we can only reduce the preferred factor.
	 * A)                   B)
	 *     PreHead             PreHead
	 *        |      ,--.         |   ,--.
	 *         \ Loop1   \        Loop2   \
	 *          \  |     |       /  |     |
	 *           Loop2   /      / Loop1   /
	 *           |   `--'      |      `--'
	 */
	/* loop passes % {6, 5, 4, 3, 2} == 0  */
	ir_mode *const mode = get_irn_mode(loop_info.end_val);
	for (unsigned prefer = MIN(loop_info.max_unroll, 6); prefer != 1; --prefer) {
		ir_tarval *const prefer_tv = new_tarval_from_long(prefer, mode);
		if (tarval_is_null(tarval_mod(count_tar, prefer_tv))) {
			DB((dbg, LEVEL_4, "preferred unroll factor %d\n", prefer));
			return prefer;
		}
	}

	/* gcd(max_unroll, count_tar) */
	int a = loop_info.max_unroll;
	int b = (int)get_tarval_long(count_tar);

	DB((dbg, LEVEL_4, "gcd of max_unroll %d and count_tar %d: ", a, b));

	for (;;) {
		int const c = a % b;
		if (c == 0)
			break;
		a = b;
		b = c;
	}

	DB((dbg, LEVEL_4, "%d\n", b));
	return b;
}

/* Check if cur_loop is a simple counting loop.
 * Start, step and end are constants.
 * TODO The whole constant case should use procedures similar to
 * the invariant case, as they are more versatile. */
/* TODO split. */
static unsigned get_unroll_decision_constant(void)
{
	/* RETURN if loop is not 'simple' */
	ir_node *const cmp = is_simple_loop();
	if (cmp == NULL)
		return 0;

	/* One in of the loop condition needs to be loop invariant. => end_val
	 * The other in is assigned by an add. => add
	 * The add uses a loop invariant value => step
	 * and a phi with a loop invariant start_val and the add node as ins.
	 *
	 *  ^   ^
	 *  |   | .-,
	 *  |   Phi |
	 *   \  |   |
	 * ^  Add   |
	 *  \  | \__|
	 *   cond
	 *    /\
	*/

	ir_node *iteration_path;
	if (!get_const_pred(cmp, &loop_info.end_val, &iteration_path))
		return 0;

	DB((dbg, LEVEL_4, "End_val %N, other %N\n", loop_info.end_val, iteration_path));

	/* We may find the add or the phi first.
	 * Until now we only have end_val. */
	unsigned is_latest_val;
	if (is_Add(iteration_path) || is_Sub(iteration_path)) {
		/* We test against the latest value of the iv. */
		is_latest_val = 1;

		loop_info.add = iteration_path;
		DB((dbg, LEVEL_4, "Case 2: Got add %N (maybe not sane)\n", loop_info.add));

		/* Preds of the add should be step and the iteration_phi */
		if (!get_const_pred(loop_info.add, &loop_info.step, &loop_info.iteration_phi))
			return 0;

		DB((dbg, LEVEL_4, "Got step %N\n", loop_info.step));

		if (!is_Phi(loop_info.iteration_phi))
			return 0;

		DB((dbg, LEVEL_4, "Got phi %N\n", loop_info.iteration_phi));

		/* Find start_val.
		 * Does necessary sanity check of add, if it is already set.  */
		if (!get_start_and_add(constant))
			return 0;

		DB((dbg, LEVEL_4, "Got start %N\n", loop_info.start_val));
	} else if (is_Phi(iteration_path)) {
		/* We compare with the value the iv had entering this run. */
		is_latest_val = 0;

		loop_info.iteration_phi = iteration_path;
		DB((dbg, LEVEL_4, "Case 1: Got phi %N \n", loop_info.iteration_phi));

		/* Find start_val and add-node.
		 * Does necessary sanity check of add, if it is already set.  */
		if (!get_start_and_add(constant))
			return 0;

		DB((dbg, LEVEL_4, "Got start %N\n", loop_info.start_val));
		DB((dbg, LEVEL_4, "Got add or sub %N\n", loop_info.add));

		ir_node *new_iteration_phi;
		if (!get_const_pred(loop_info.add, &loop_info.step, &new_iteration_phi))
			return 0;

		DB((dbg, LEVEL_4, "Got step %N\n", loop_info.step));

		if (loop_info.iteration_phi != new_iteration_phi)
			return 0;
	} else {
		return 0;
	}

	DB((dbg, LEVEL_4, "start %N, end %N, step %N\n", loop_info.start_val, loop_info.end_val, loop_info.step));

	ir_mode *const mode = get_irn_mode(loop_info.end_val);
	if (mode != mode_Is && mode != mode_Iu)
		return 0;

	/* TODO necessary? */
	if (!are_mode_I(loop_info.start_val, loop_info.step, loop_info.end_val))
		return 0;

	DB((dbg, LEVEL_4, "mode integer\n"));

	ir_tarval *const end_tar   = get_Const_tarval(loop_info.end_val);
	ir_tarval *const start_tar = get_Const_tarval(loop_info.start_val);
	ir_tarval *const step_tar  = get_Const_tarval(loop_info.step);

	if (tarval_is_null(step_tar))
		/* TODO Might be worth a warning. */
		return 0;

	DB((dbg, LEVEL_4, "step is not 0\n"));

	if (!tarval_is_negative(step_tar) ^ !is_Sub(loop_info.add))
		loop_info.decreasing = 1;

	ir_tarval *const diff_tar = tarval_sub(end_tar, start_tar);

	/* We need at least count_tar steps to be close to end_val, maybe more.
	 * No way, that we have gone too many steps.
	 * This represents the 'latest value'.
	 * (If condition checks against latest value, is checked later) */
	ir_tarval *count_tar = tarval_div(diff_tar, step_tar);

	/* Iv will not pass end_val (except overflows).
	 * Nothing done, as it would yield to no advantage. */
	if (tarval_is_negative(count_tar)) {
		DB((dbg, LEVEL_4, "Loop is endless or never taken."));
		/* TODO Might be worth a warning. */
		return 0;
	}

	++stats.u_simple_counting_loop;

	loop_info.latest_value = is_latest_val;

	/* stepped can be negative, if step < 0 */
	ir_tarval *stepped = tarval_mul(count_tar, step_tar);

	/* step as close to end_val as possible, */
	/* |stepped| <= |end_tar|, and dist(stepped, end_tar) is smaller than a step. */
	stepped = is_Add(loop_info.add) ? tarval_add(start_tar, stepped)
	                                : tarval_sub(start_tar, stepped);

	DB((dbg, LEVEL_4, "stepped to %ld\n", get_tarval_long(stepped)));

	ir_relation const relation      = get_Cmp_relation(cmp);
	/* Assure that norm_relation is the stay-in-loop case. */
	ir_relation const norm_relation = loop_info.exit_cond == 1 ?
		get_negated_relation(relation) : relation;

	DB((dbg, LEVEL_4, "normalized projection %s\n", get_relation_string(norm_relation)));
	/* Executed at most once (stay in counting loop if a Eq b) */
	if (norm_relation == ir_relation_equal)
		/* TODO Might be worth a warning. */
		return 0;

	/* calculates next values and increases count_tar according to it */
	if (!simulate_next(&count_tar, stepped, step_tar, end_tar, norm_relation))
		return 0;

	/* We run loop once more, if we compare to the
	 * not yet in-/decreased iv. */
	if (is_latest_val == 0) {
		DB((dbg, LEVEL_4, "condition uses not latest iv value\n"));
		count_tar = tarval_add(count_tar, get_mode_one(mode));
	}

	DB((dbg, LEVEL_4, "loop taken %ld times\n", get_tarval_long(count_tar)));

	/* Assure the loop is taken at least 1 time. */
	if (tarval_is_null(count_tar)) {
		/* TODO Might be worth a warning. */
		return 0;
	}

	return get_preferred_factor_constant(count_tar);
}

/**
 * Loop unrolling
 */
static void unroll_loop(ir_graph *const irg)
{
	if (loop_info.nodes <= 0)
		return;

	if (loop_info.nodes > opt_params.max_unrolled_loop_size) {
		DB((dbg, LEVEL_2, "Nodes %d > allowed nodes %d\n",
			loop_info.nodes, opt_params.max_unrolled_loop_size));
		++stats.too_large;
		return;
	}

	if (loop_info.calls > 0) {
		DB((dbg, LEVEL_2, "Calls %d > allowed calls 0\n", loop_info.calls));
		++stats.calls_limit;
		return;
	}

	unroll_nr = 0;

	/* get_unroll_decision_constant and invariant are completely
	 * independent for flexibility.
	 * Some checks may be performed twice. */

	/* constant case? */
	if (opt_params.allow_const_unrolling)
		unroll_nr = get_unroll_decision_constant();
	if (unroll_nr > 1) {
		loop_info.unroll_kind = constant;
	} else {
		/* invariant case? */
		if (opt_params.allow_invar_unrolling)
			unroll_nr = get_unroll_decision_invariant(irg);
		if (unroll_nr > 1)
			loop_info.unroll_kind = invariant;
	}

	DB((dbg, LEVEL_2, " *** Unrolling %d times ***\n", unroll_nr));

	if (unroll_nr > 1) {
		loop_entries = NEW_ARR_F(entry_edge, 0);

		/* Get loop outs */
		irg_walk_graph(irg, get_loop_entries, NULL, NULL);

		/* Use phase to keep copy of nodes from the condition chain. */
		ir_nodemap_init(&map, irg);
		obstack_init(&obst);

		/* Copies the loop */
		copy_loop(irg, loop_entries, unroll_nr - 1);

		/* Line up the floating copies. */
		place_copies(unroll_nr - 1);

		/* Remove phis with 1 in
		 * If there were no nested phis, this would not be necessary.
		 * Avoiding the creation in the first place
		 * leads to complex special cases. */
		irg_walk_graph(irg, correct_phis, NULL, NULL);

		if (loop_info.unroll_kind == constant)
			++stats.constant_unroll;
		else
			++stats.invariant_unroll;

		clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

		DEL_ARR_F(loop_entries);
		obstack_free(&obst, NULL);
		ir_nodemap_destroy(&map);
	}
}

/* Analyzes the loop, and checks if size is within allowed range.
 * Decides if loop will be processed. */
static void init_analyze(ir_graph *const irg, ir_loop *const loop, loop_op_t const loop_op)
{
	cur_loop        = loop;
	loop_head       = NULL;
	loop_head_valid = true;

	/* Reset loop info */
	memset(&loop_info, 0, sizeof(loop_info));

	DB((dbg, LEVEL_1, "    >>>> current loop %ld <<<\n", get_loop_loop_nr(loop)));

	/* Collect loop informations: head, node counts. */
	irg_walk_graph(irg, get_loop_info, NULL, NULL);

	/* RETURN if there is no valid head */
	if (!loop_head || !loop_head_valid) {
		DB((dbg, LEVEL_1, "No valid loop head. Nothing done.\n"));
		return;
	}
	DB((dbg, LEVEL_1, "Loophead: %N\n", loop_head));

	if (loop_info.branches > opt_params.max_branches) {
		DB((dbg, LEVEL_1, "Branches %d > allowed branches %d\n",
			loop_info.branches, opt_params.max_branches));
		++stats.calls_limit;
		return;
	}

	switch (loop_op) {
		case loop_op_inversion: loop_inversion(irg); break;
		case loop_op_unrolling: unroll_loop(irg);    break;
		default: panic("loop optimization not implemented");
	}
	DB((dbg, LEVEL_1, "       <<<< end of loop with node %ld >>>>\n", get_loop_loop_nr(loop)));
}

/* Find innermost loops and add them to loops. */
static void find_innermost_loop(ir_loop *const loop)
{
	bool         had_sons   = false;
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t e = 0; e < n_elements; ++e) {
		loop_element element = get_loop_element(loop, e);
		if (*element.kind == k_ir_loop) {
			find_innermost_loop(element.son);
			had_sons = true;
		}
	}

	if (!had_sons)
		ARR_APP1(ir_loop*, loops, loop);
}

static void set_loop_params(void)
{
	opt_params.max_loop_size            =  100;
	opt_params.depth_adaption           =  -50;
	opt_params.count_phi                = true;
	opt_params.allowed_calls            =    0;
	opt_params.max_cc_size              =    5;
	opt_params.allow_const_unrolling    = true;
	opt_params.allow_invar_unrolling    = false;
	opt_params.invar_unrolling_min_size =   20;
	opt_params.max_unrolled_loop_size   =  400;
	opt_params.max_branches             = 9999;
}

/**
 * Optimize loops by peeling or unrolling them if beneficial.
 *
 * @param irg  The graph whose loops will be processed
 *
 * This function did not change the graph, only its frame type.
 * The layout state of the frame type will be set to layout_undefined
 * if entities were removed.
 */
static void loop_optimization(ir_graph *const irg, loop_op_t const loop_op)
{
	/* Assure preconditions are met and go through all loops. */
	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
		| IR_GRAPH_PROPERTY_CONSISTENT_OUTS
		| IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);

	set_loop_params();

	/* Reset stats for this procedure */
	reset_stats();

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);
	collect_phiprojs_and_start_block_nodes(irg);

	ir_loop *const loop = get_irg_loop(irg);

	loops = NEW_ARR_F(ir_loop *, 0);
	/* List all inner loops */
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element element = get_loop_element(loop, i);
		if (*element.kind != k_ir_loop)
			continue;
		find_innermost_loop(element.son);
	}

	/* Set all links to NULL */
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);

	for (size_t i = 0; i < ARR_LEN(loops); ++i) {
		ir_loop *const loop = loops[i];

		++stats.loops;

		/* Analyze and handle loop */
		init_analyze(irg, loop, loop_op);

		/* Copied blocks do not have their phi list yet */
		collect_phiprojs_and_start_block_nodes(irg);

		/* Set links to NULL
		 * TODO Still necessary? */
		irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	}

	print_stats();

	DEL_ARR_F(loops);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
}

void do_loop_unrolling(ir_graph *const irg)
{
	loop_optimization(irg, loop_op_unrolling);
}

void do_loop_inversion(ir_graph *const irg)
{
	loop_optimization(irg, loop_op_inversion);
}

void do_loop_peeling(ir_graph *const irg)
{
	loop_optimization(irg, loop_op_peeling);
}

void firm_init_loop_opt(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.loop");
}
