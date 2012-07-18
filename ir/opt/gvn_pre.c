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
 * @brief   Global Value Numbering Partial Redundancy Elimination
 *          (VanDrunen Hosking 2004)
 * @author  Michael Beck
 * @brief
 */
#include "config.h"

#include "debug.h"
#include "ircons.h"
#include "irdom.h"
#include "iredges.h"
#include "irflag.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnodehashmap.h"
#include "irnodeset.h"
#include "iropt_dbg.h"
#include "iroptimize.h"
#include "irouts.h"
#include "irpass.h"
#include "valueset.h"
#include "irloop.h"

#include "irgraph_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "plist.h"

#define MAX_ANTIC_ITER 10
#define MAX_INSERT_ITER 3

#define HOIST_HIGH 0
#define BETTER_GREED 0
#define LOADS 0
#define DIVMODS 0
#define NO_INF_LOOPS 0
#define NO_INF_LOOPS2 0

/** Additional info we need for every block. */
typedef struct block_info {
	ir_valueset_t     *exp_gen;    /* contains this blocks clean expressions */
	ir_valueset_t     *avail_out;  /* available values at block end */
	ir_valueset_t     *antic_in;   /* clean anticipated values at block entry */
#if HOIST_HIGH
	ir_valueset_t     *antic_done; /* keeps elements of antic_in after insert_nodes() */
#endif
	ir_valueset_t     *new_set;    /* new by hoisting made available values */
	ir_nodehashmap_t  *trans;      /* contains translated nodes translated into block */
	ir_node           *avail;      /* saves available node for insert_nodes */
	int                found;      /* saves kind of availability for insert_nodes */
	ir_node           *block;      /* block of the block_info */
	struct block_info *next;       /* links all instances for easy access */
} block_info;

/**
 * A pair of nodes to be exchanged.
 * We have to defer the exchange because our hash-sets cannot
 * find an already replaced node.
 */
typedef struct elim_pair {
	ir_node *old_node;      /* node that will be replaced */
	ir_node *new_node;      /* replacement for old_node */
	struct elim_pair *next; /* links all instances for easy access */
	int     reason;         /* reason for the replacement */
} elim_pair;

/** environment for the GVN-PRE algorithm */
typedef struct pre_env {
	ir_graph       *graph;        /* current graph */
	struct obstack *obst;         /* obstack to allocate on */
	ir_node        *start_block;  /* start block of the current graph */
	ir_node        *end_block;    /* end block of the current graph */
	ir_node        *end_node;     /* end node of the current graph */
	block_info     *list;         /* block_info list head */
	elim_pair      *pairs;        /* elim_pair list head */
	ir_nodeset_t   *keeps;        /* a list of to be removed phis to kill their keep alive edges */
	unsigned        last_idx;     /* last node index of input graph */
	char            changes;      /* flag for fixed point iterations - non-zero if changes occurred */
	char            first_iter;   /* non-zero for first fixed point iteration */
	char            insert_phase; /* non-zero for first fixed point iteration */
} pre_env;

static pre_env *environ;
/* custom GVN value map */
static ir_nodehashmap_t value_map;

/* debug module handle */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

#ifdef DEBUG_libfirm

/* --------------------------------------------------------
 * Statistics
 * --------------------------------------------------------
 */

typedef struct gvnpre_statistics {
	int replaced;
	int partially;
	int fully;
	int loads;
	int divmods;
	int hoist_high;
	int first_iter_found;
	int antic_iterations;
	int insert_iterations;
	int infinite_loops;
} gvnpre_statistics;

gvnpre_statistics *gvnpre_stats = NULL;

static void init_stats()
{
	gvnpre_stats = XMALLOCZ(gvnpre_statistics);
}

static void free_stats()
{
	free(gvnpre_stats);
	gvnpre_stats = NULL;
}

static void print_stats()
{
	gvnpre_statistics *stats = gvnpre_stats;
	DB((dbg, LEVEL_1, "replaced             : %d\n", stats->replaced));
	DB((dbg, LEVEL_1, "antic_in iterations  : %d\n", stats->antic_iterations));
	DB((dbg, LEVEL_1, "insert iterations    : %d\n", stats->insert_iterations));
	DB((dbg, LEVEL_1, "infinite loops       : %d\n", stats->infinite_loops));
	DB((dbg, LEVEL_1, "fully redundant      : %d\n", stats->fully));
	DB((dbg, LEVEL_1, "partially redundant  : %d\n", stats->partially));
	DB((dbg, LEVEL_1, "  loads                : %d\n", stats->loads));
	DB((dbg, LEVEL_1, "  Divs/Mods            : %d\n", stats->divmods));
	DB((dbg, LEVEL_1, "  hoist high           : %d\n", stats->hoist_high));
	DB((dbg, LEVEL_1, "  first iteration      : %d\n", stats->first_iter_found));
}

#define set_stats(var, value) (var)=(value)
#define inc_stats(var)        ((var)+=1)

/* --------------------------------------------------------
 * Dump valuesets
 * --------------------------------------------------------
 */

/**
 * Dump a value set.
 *
 * @param set    the set to dump
 * @param txt    a text to describe the set
 * @param block  the owner block of the set
 */
static void dump_value_set(ir_valueset_t *set, const char *txt, ir_node *block)
{
	ir_valueset_iterator_t iter;
	ir_node *value, *expr;
	int i;

	DB((dbg, LEVEL_2, "%s(%+F) = {\n", txt, block));
	i = 0;
	foreach_valueset(set, value, expr, iter) {
		if ((i & 3) == 3)
			DB((dbg, LEVEL_2, "\n"));
		if (value != expr)
			DB((dbg, LEVEL_2, " %+F(%+F),", expr, value));
		else
			DB((dbg, LEVEL_2, " %+F,", expr));
		++i;
	}
	DB((dbg, LEVEL_2, "\n}\n"));
}  /* dump_value_set */

/**
 * Dump all exp_gen value sets.
 *
 * @param list  the list of block infos to retrieve the sets from
 */
static void dump_all_expgen_sets(block_info *list)
{
	block_info *block_info;

	for (block_info = list; block_info != NULL; block_info = block_info->next) {
		dump_value_set(block_info->exp_gen, "[Exp_gen]", block_info->block);
	}
}

#else

#define dump_all_expgen_sets(list)
#define dump_value_set(set, txt, block)

#endif /* DEBUG_libfirm */

/* --------------------------------------------------------
 * GVN Functions
 * --------------------------------------------------------
 */

/**
 * Compares node collisions in valuetable.
 * Modified identities_cmp().
 */
static int compare_gvn_identities(const void *elt, const void *key)
{
	ir_node *a = (ir_node *)elt;
	ir_node *b = (ir_node *)key;
	int i, irn_arity_a;

	if (a == b) return 0;

	/* phi nodes kill predecessor values and are always different */
	if (is_Phi(a) || is_Phi(b))
		return 1;

	if ((get_irn_op(a) != get_irn_op(b)) ||
	    (get_irn_mode(a) != get_irn_mode(b))) return 1;

	/* compare if a's in and b's in are of equal length */
	irn_arity_a = get_irn_arity(a);
	if (irn_arity_a != get_irn_arity(b))
		return 1;

	/* blocks are never the same */
	if (is_Block(a) || is_Block(b))
		return 1;

	/* TODO depends on load optimization */
	if (get_irn_pinned(a) == op_pin_state_pinned) {
		/* for pinned nodes, the block inputs must be equal */
		if (get_irn_n(a, -1) != get_irn_n(b, -1))
			return 1;
	} else {
		/* we need global values independent from their blocks */
		assert(get_opt_global_cse());
	}

	/* compare a->in[0..ins] with b->in[0..ins] */
	for (i = 0; i < irn_arity_a; ++i) {
		ir_node *pred_a = get_irn_n(a, i);
		ir_node *pred_b = get_irn_n(b, i);
		if (pred_a != pred_b) {
			/* if both predecessors are CSE neutral they might be different */
			if (!is_irn_cse_neutral(pred_a) || !is_irn_cse_neutral(pred_b))
				return 1;
		}
	}

	/*
	 * here, we already now that the nodes are identical except their
	 * attributes
	 */
	if (a->op->ops.node_cmp_attr)
		return a->op->ops.node_cmp_attr(a, b);

	return 0;
}

/**
 * Identify does a lookup in the GVN valuetable.
 * To be used when no new GVN values are to be created.
 *
 * @param e  a node representing an expression
 * @return a node representing the value
 */
static ir_node *identify(ir_node *irn)
{
	ir_node *value = ir_nodehashmap_get(ir_node, &value_map, irn);
	if (value)
		return value;
	/* irn represents a new value */
	return identify_remember(irn);
}

/**
 * remember() adds node irn to the GVN valuetable.
 * Identify_remember only identifies values of nodes with the
 * same predecessor nodes (not values). By creating a node from the predecessor
 * values, a true valuetree is built. Phis kill their predecessor value,
 * so no circular dependencies need to be resolved.
 *
 * TODO Improvement:
 *      Maybe this could be implemented with a custom node hash that takes
 *      phi nodes and true values (instead of predecessors) into account,
 *      resulting in value numbers.
 *
 * @param irn  a node representing an expression
 * @return     the value of the expression
 */
static ir_node *remember(ir_node *irn)
{
	int       arity   = get_irn_arity(irn);
	int       i;
	int       changed = 0;
	ir_node **in      = XMALLOCN(ir_node *, arity);
	ir_node  *value;

	DB((dbg, LEVEL_4, "Remember %+F\n", irn));

	for (i = 0; i < arity; ++i) {
		ir_node *pred       = get_irn_n(irn, i);
		ir_node *pred_value = identify(pred);

		/* phi will be translated anyway, so kill the predecessor values
		   also prevents circular dependencies */
		if (is_Phi(pred)) {
			/* every phi represents its own value */
			in[i] = pred;
			continue;
		}

		/* predecessor is not its value representation */
		if (pred != pred_value)
			changed = 1;
		in[i] = pred_value;
	}

	if (changed) {
		/* create representative for */
		ir_node *nn = new_ir_node(
			get_irn_dbg_info(irn),
			get_irn_irg(irn),
			get_nodes_block(irn),
			get_irn_op(irn),
			get_irn_mode(irn),
			get_irn_arity(irn),
			in);
		copy_node_attr(environ->graph, irn, nn);

		/* now the value can be determined */
		value = identify_remember(nn);
	} else {
		value = identify_remember(irn);
	}
	free(in);

	ir_nodehashmap_insert(&value_map, irn, value);

	return value;
}

/** When the value map has been built we may lookup expressions
 *  and remember them if new.
 */
static ir_node *identify_or_remember(ir_node *irn)
{
	ir_node *value = ir_nodehashmap_get(ir_node, &value_map, irn);
	if (value)
		return value;
	else
		return remember(irn);
}

/* --------------------------------------------------------
 * Block info
 * --------------------------------------------------------
 */

/**
 * Allocate block info for block block.
 *
 * @param block   the block
 * @param env     the environment
 */
static void alloc_block_info(ir_node *block, pre_env *env)
{
	block_info *info = OALLOC(env->obst, block_info);

	set_irn_link(block, info);
	info->exp_gen    = ir_valueset_new(16);
	info->avail_out  = ir_valueset_new(16);
	info->antic_in   = ir_valueset_new(16);
#if HOIST_HIGH
	info->antic_done = ir_valueset_new(16);
#endif
	info->trans = XMALLOC(ir_nodehashmap_t);
	ir_nodehashmap_init(info->trans);

	info->new_set = NULL;
	info->avail   = NULL;
	info->block   = block;
	info->found   = 1;

	info->next = env->list;
	env->list  = info;
}  /* alloc_block_info */

/**
 * Bottom up walker that ensures that every block gets a block info.
 *
 * @param irn   the node
 * @param ctx   the environment
 */
static void block_info_walker(ir_node *irn, void *ctx)
{
	if (is_Block(irn)) {
		pre_env *env = (pre_env*)ctx;
		alloc_block_info(irn, env);
	}
}

/**
 * Returns the block info of a block.
 */
static block_info *get_block_info(ir_node *block)
{
	return (block_info*)get_irn_link(block);
}

/* --------------------------------------------------------
 * Infinite loop analysis
 * --------------------------------------------------------
 */

/**
 * Walker to set block marks and loop links to 0.
 */
static void clear_block_mark_loop_link(ir_node *block, void *env)
{
	(void) env;

	if (is_Block(block)) {
		set_Block_mark(block, 0);
		set_loop_link(get_irn_loop(block), NULL);
	}
}

/**
 * Returns non-zero if block is part of real loop loop.
 */

static unsigned in_loop(ir_node *block, ir_loop *loop)
{
	ir_loop *l     = get_irn_loop(block);
	ir_loop *outer = get_irg_loop(environ->graph);

	while (l != loop) {
		/* loop tree root is not a loop */
		if (l == NULL || l == outer)
			return 0;
		l = get_loop_outer_loop(l);
	}
	return 1;
}

/**
 * Returns the outermost real loop of loop.
 */
static ir_loop *get_loop_outermost(ir_loop *loop)
{
	ir_loop *outer = get_irg_loop(environ->graph);
	ir_loop *l     = loop;
	ir_loop *last  = NULL;

	while(l != outer) {
		last = l;
		l = get_loop_outer_loop(l);
	}
	return last;
}

/**
 * Topologic bottom-up walker sets links of infinite loops to non-zero.
 * Block marks are used to flag blocks reachable (from end) on the one hand,
 * on the other hand they are set if the block is not part of an infinite loop.
 */
static void infinite_loop_walker(ir_node *block, void *env)
{
	int arity;
	int i;
	(void) env;

	if (! is_Block(block))
		return;

	/* start block has no predecessors */
	if (block == environ->start_block)
		return;

	arity = get_irn_arity(block);

	/* block not part of a real loop: no infinite loop */
	if (get_irn_loop(block) == get_irg_loop(environ->graph))
		set_Block_mark(block, 1);

	if (get_Block_mark(block)) {
		/* reachable block: mark all cf predecessors */
		for (i = 0; i < arity; ++i) {
			ir_node *pred = get_Block_cfgpred_block(block, i);
			if (is_Bad(pred))
				continue;
			set_Block_mark(pred, 1);
		}
	} else {
		/* We are in a real loop and see an unreachable block. */
		ir_loop *outermost_loop = get_loop_outermost(get_irn_loop(block));

		/* flag loop as infinite */
		set_loop_link(outermost_loop, outermost_loop);
		DEBUG_ONLY(inc_stats(gvnpre_stats->infinite_loops);)

		/* The cf predecessors are unreachable, but can never be part of
		   an infinite loop, because we just reached them. So we set the
		   blockmark to prevent triggering the infinite loop detection. */

		/* passing information to the cf predecessors */
		for (i = 0; i < arity; ++i) {
			ir_node *pred = get_Block_cfgpred_block(block, i);

			if (is_Bad(pred))
				continue;

			/* If our cf predecessor is in the same endless loop,
			   it is also unreachable. */
			if (in_loop(pred, outermost_loop)) {
				set_Block_mark(pred, 0);
			} else {
				/* When we leave the unreachable loop, we artificially
				   declare the cf predecessor reachable. */
				set_Block_mark(pred, 1);
			}
		}
	}
}

/**
 * Sets loop links of outermost infinite loops to non-zero.
 */
static void analyse_loops(ir_graph *irg)
{
	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_MARK);

	/* reset block mark and loop links */
	irg_walk_blkwise_graph(irg, clear_block_mark_loop_link, NULL, NULL);

	/* mark end block reachable */
	set_Block_mark(get_irg_end_block(irg), 1);
	irg_walk_blkwise_graph(irg, infinite_loop_walker, NULL, NULL);

	ir_free_resources(irg, IR_RESOURCE_BLOCK_MARK);
}

#if NO_INF_LOOPS || NO_INF_LOOPS2
/**
 * Returns non-zero if block is part of an infinite loop.
 */
static unsigned is_in_infinite_loop(ir_node *block)
{
	ir_loop *loop;

	assert(is_Block(block));
	loop = get_irn_loop(block);
	assert(loop);

	loop = get_loop_outermost(loop);
	if (loop)
		return (get_loop_link(loop) != NULL);
	else
		return 0;
}
#endif

/* --------------------------------------------------------
 * GVN-PRE Exp_gen
 * --------------------------------------------------------
 */

/**
 * Returns non-zero if a node is movable and a possible candidate for PRE.
 */
static unsigned is_nice_value(ir_node *n)
{
	ir_mode *mode = get_irn_mode(n);

	/* positive group */

	if (is_Phi(n))
		return 1;

#if LOADS || DIVMODS
	if (is_Proj(n))
		return 1;
#endif

#if LOADS
	if (is_Load(n))
		return (get_Load_volatility(n) == volatility_non_volatile);
#endif

	/* negative group */

	if (get_irn_pinned(n) == op_pin_state_pinned)
		return 0;

	if (! mode_is_data(mode)) {
		if (! is_Div(n) && ! is_Mod(n))
			return 0;
	}
	return 1;
}

/**
 * Checks if a node n is clean in block block for exp_gen.
 *
 * @param n      the node
 * @param block  the block
 * @return non-zero value for clean node
 */
static unsigned is_clean_in_block_expgen(ir_node *n, ir_node *block)
{
	int i, arity;

	if (is_Phi(n))
		return 1;

	if (! is_nice_value(n))
		return 0;

	arity = get_irn_arity(n);
	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(n, i);

		/* sufficient for exp_gen because block is always node's block */
		if (get_nodes_block(pred) != block)
			continue;

		/* predecessor is in block,
		   so it needs to be clean */
		if (get_irn_link(pred)) {
			continue;
		} else {
			DB((dbg, LEVEL_3, "unclean %+F because pred %+F unclean\n", n, pred));
			return 0;
		}
	}
	return 1;
}

/**
 * Topological walker puts nodes in top-down topological order into exp_gen set.
 * Assumed to walk blockwise and nodewise topologically top-down.
 *
 * @param irn    the node
 * @param ctx    the environment
 */
static void topo_walker(ir_node *irn, void *ctx)
{
	ir_node    *block;
	block_info *info;
	ir_node    *value;
	(void) ctx;

	if (is_Block(irn))
		return;

	/* GVN step: remember the value. Predecessors need to be visited. */
	value = remember(irn);

	block = get_nodes_block(irn);
	info  = get_block_info(block);

	/* values for avail_out do not need to be clean */
	ir_valueset_insert(info->avail_out, value, irn);

	/* no need to put constants into the sets: they are always redundant */
	if (! is_nice_value(irn) || is_irn_constlike(irn))
		return;

	if (is_clean_in_block_expgen(irn, block)) {
		DB((dbg, LEVEL_3, "%+F clean in block %+F\n", irn, block));

		ir_valueset_insert(info->exp_gen, value, irn);
		/* flag irn as clean*/
		set_irn_link(irn, irn);
	} else {
		/* flag irn as not clean */
		set_irn_link(irn, NULL);
	}
}

/* --------------------------------------------------------
 * GVN-PRE Antic_in
 * --------------------------------------------------------
 */

/**
 * Gets result of nodes phi translation into block.
 *
 * @param node   the node
 * @param block  the target block
 *
 * @return a phi translation of node node into block block or NULL
 */
static ir_node *get_translated(ir_node *block, ir_node *node)
{
	if (is_irn_constlike(node))
		return node;

	return ir_nodehashmap_get(ir_node, get_block_info(block)->trans, node);
}

/**
 * Saves result of phi translation of node into predecessor
 * at pos of block succ.
 *
 * @param node   the node
 * @param succ   the successor of the translation target block
 * @param pos    the position of the predecessor block
 * @param trans  the translation result
 *
 */
static void set_translated(ir_nodehashmap_t *map, ir_node *node, ir_node *trans)
{
	if (is_irn_constlike(node))
		return;
	/* insert or replace */
	ir_nodehashmap_insert(map, node, trans);
}

/**
 * Checks if a node node is clean in block block for use in antic_in.
 *
 * A clean node in block block can be hoisted above block block.
 * A node is not clean if its representative is killed in block block.
 * This reveals that antic_in is basically antic_out, because if the
 * removal of TMP_GEN which are the representatives in the current block.
 * The node can still be hoisted into block block.
 *
 *
 * @param n      the phi translated or not translated node
 * @param block  the block
 * @return non-zero value for clean node
 */
static unsigned is_clean_in_block_antic(ir_node *node, ir_node *block)
{
	int i;
	int arity = get_irn_arity(node);

	/* make sure that node can be hoisted above block */

	if (is_irn_constlike(node))
		return 1;

	for (i = 0; i < arity; ++i) {
		block_info *info       = get_block_info(block);
		ir_node    *pred       = get_irn_n(node, i);
		ir_node    *pred_value = identify(pred);
		ir_node    *pred_block = get_nodes_block(pred);

		/* remove TMP_GEN */
		/* is predecessor not known to be clean */
		if (! ir_valueset_lookup(info->antic_in, pred_value)) {
			/* is it possible to hoist node above block? */
			if (! block_strictly_dominates(pred_block, block)) {
				DB((dbg, LEVEL_3, "unclean %+F: %+F (%+F) not antic\n", node, pred, pred_value));
				return 0;
			}
		}
		/* predecessor value to be antic in is not enough.
		   if we didn't translate the exact representative we cannot translate */
		if (! get_translated(pred_block, pred)) {
			if (! block_strictly_dominates(pred_block, block)) {
				DB((dbg, LEVEL_3, "unclean %+F: %+F (%+F) lost of representative\n", node, pred, pred_value));
				return 0;
			}

		}
	}
	return 1;
}

#if LOADS || DIVMODS
/* Helper function to compare the values of pred and avail_pred. */
static unsigned match_pred(ir_node *pred, ir_node *avail_pred, ir_node *block, int pos)
{
	ir_node *avail_value  = identify(avail_pred);
	ir_node *trans_pred   = get_translated_pred(pred, block, pos);
	ir_node *value;

	if (trans_pred == NULL)
		trans_pred = pred;
	value = identify(trans_pred);

	DB((dbg, LEVEL_3, "manual compare %+F  %+F\n", pred, avail_pred));

	return (value == avail_value);
}
#endif

#if LOADS
/**
 * Does phi translation for redundant load nodes only.
 * Returns NULL for non-redundant loads, which need to be phi translated.
 * Loads are compared by comparing their pointer values,
 * and assuring that they are adjacent.
 * This is equivalent to what phi_translation does,
 * when a new node is created and then GCSE optimized resulting
 * in an already available node.
 */
static ir_node *phi_translate_load(ir_node *load, ir_node *block, int pos)
{
	ir_node *mem   = get_Load_mem(load);
	ir_node *trans = get_translated_pred(mem, block, pos);

	if (trans == NULL)
		trans = mem;

	/* no partial redundancy if this is a mode_M phi */
	if (is_Proj(trans)) {
		/* The last memory operation in predecessor block */
		ir_node *avail_load = get_Proj_pred(trans);

		/* memop is a load with matching type */
		if (is_Load(avail_load) &&
			    get_Load_mode(load) == get_Load_mode(avail_load)) {

			unsigned match = match_pred(get_Load_ptr(load), get_Load_ptr(avail_load), block, pos);

			if (match)
				return avail_load;
		}
	}
	return NULL;
}
#endif

#if DIVMODS
/**
 * Does phi translation for redundant Div/Mod nodes only.
 * Returns NULL for non-redundant node, which needs to be phi translated.
 */
static ir_node *phi_translate_divmod(ir_node *divmod, ir_node *block, int pos)
{
	ir_node *mem   = get_memop_mem(divmod);
	ir_node *trans = get_translated_pred(mem, block, pos);

	if (trans == NULL)
		trans = mem;

	/* no partial redundancy if this is a mode_M phi */
	if (is_Proj(trans)) {
		/* The last memory operation in predecessor block */
		ir_node *avail_op = get_Proj_pred(trans);

		if (get_irn_op(divmod) == get_irn_op(avail_op)) {
			unsigned left, right;

			if (is_Div(avail_op)) {
				if (get_Div_resmode(divmod) == get_Div_resmode(avail_op) &&
				    get_Div_no_remainder(divmod) == get_Div_no_remainder(avail_op)) {

					left  = match_pred(get_Div_left(divmod), get_Div_left(avail_op), block, pos);
					right = match_pred(get_Div_right(divmod), get_Div_right(avail_op), block, pos);

					if (left && right)
						return avail_op;
				}
			} else if (is_Mod(avail_op)) {
			    if (get_Mod_resmode(divmod) == get_Mod_resmode(avail_op)) {

					left  = match_pred(get_Mod_left(divmod), get_Mod_left(avail_op), block, pos);
					right = match_pred(get_Mod_right(divmod), get_Mod_right(avail_op), block, pos);

					if (left && right)
						return avail_op;
				}
			}
		}
	}
	return NULL;
}
#endif

/**
 * Translates an expression above a Phi.
 *
 * @param node        the node
 * @param block       the block the node is translated into
 * @param pos         the input number of the destination block
 *
 * @return a node representing the translated value
 */
static ir_node *phi_translate(ir_node *node, ir_node *block, int pos, ir_node *pred_block)
{
	int       i;
	int       arity;
	ir_node **in;
	ir_node  *nn;
	int       needed;

	if (is_Phi(node)) {
		if (get_nodes_block(node) == block)
			return get_Phi_pred(node, pos);
		/* this phi does not need translation */
		return node;
	}

	arity = get_irn_arity(node);

#if LOADS
	if (is_Load(node)) {
		ir_node *avail_load = phi_translate_load(node, block, pos);
		if (avail_load)
			return avail_load;
	}
#endif

#if DIVMODS
	if (is_Div(node) || is_Mod(node)) {
		ir_node *avail_op = phi_translate_divmod(node, block, pos);
		if (avail_op)
			return avail_op;
	}
#endif

	needed = 0;
	/* insert phase enforces translation for previously not translated nodes */
	if (environ->insert_phase)
		needed = 1;

	in = XMALLOCN(ir_node *, arity);

	for (i = 0; i < arity; ++i) {
		ir_node *pred       = get_irn_n(node, i);
		/* we cannot find this value in antic_in, because the value
		   has (possibly) changed! */
		ir_node *pred_trans = get_translated(pred_block, pred);
		ir_node *expr;

		if (pred_trans == NULL) {
			/* reasons for this are:
			   1. pred not dominated by block: use predecessor.
			   2. dangling-represenatative: predecessor not translated.
			   We cannot phi translate, it will be the wrong value. */
			expr = pred;
		} else {
			expr = pred_trans;
			/* predecessor value changed, so translation is needed */
			if (identify(expr) != identify(pred))
				needed |= 1;
		}

		/* We need to build a value tree. But values cannot be translated,
		   expressions can be. So we translate the expressions and let GVN
		   identify their values. */
		in[i] = expr;
	}

	if (! needed)
		return node;

	DB((dbg, LEVEL_3, "Translate\n"));
	/* copy node to represent the new value.
	   We do not translate nodes that do not need translation,
	   so we use the newly created nodes as value representatives only.
	   Their block is not important, because we create new ones during
	   insert_nodes(). */
	nn = new_ir_node(
		get_irn_dbg_info(node),
		environ->graph,
		get_Block_cfgpred_block(block, pos),
		get_irn_op(node),
		get_irn_mode(node),
		arity,
		in);
	free(in);
	/* We need the attribute copy here, because the Hash value of a
	   node might depend on it. */
	copy_node_attr(environ->graph, node, nn);
	/* Optimizing nn here is tempting but might be against the GVN-PRE algorithm
	   because it already uses availability. */

	DB((dbg, LEVEL_3, "New node %+F in %+F origin %+F\n", nn, get_Block_cfgpred_block(block, pos), node));
	return nn;
}

/**
 * Block-walker, computes Antic_in(block).
 * Builds a value tree out of the graph by translating values
 * over phi nodes.
 * Although the algorithm works on values, constructing the value tree
 * depends on actual representations through nodes and their actual
 * predecessors.
 * By using only one representative (GVN-PRE) for every value, we have to be
 * careful not to break the topological order during translation. If a node
 * needs to be translated, but its predecessor - a representative in the same
 * antic_in scope - has not been, then it has to be killed.
 * one-representative-problem: Two blocks yield the same value through
 *    completely different calculations. The value is common, but the
 *    representative cannot be phi translated, because the predecessors
 *    haven't been.
 *    If a value is in exp_gen and also in antic_in of the successor,
 *    a similar situation sets in.
 *
 * By using antic_in exclusively, we lose information about translated nodes.
 * We need to permanently keep the translated nodes list.
 * For insert_nodes() we actually need antic_out. But antic_out is not usable,
 * because every value had to be cross-looked up in every available set.
 *
 * If we used antic_out, the translated nodes list would not be necessary
 * permanently, because instead of looking for translated(antic_in) we could
 * just use antic_out of the predecessor block.
 *
 * @param block  the block
 * @param ctx    the walker environment
 */
static void compute_antic(ir_node *block, void *ctx)
{
	pre_env                *env       = (pre_env*)ctx;
	block_info             *succ_info;
	block_info             *info;
	ir_node                *succ;
	ir_node                *value;
	ir_node                *expr;
	size_t                  size;
	ir_valueset_iterator_t  iter;
	int                     n_succ;

	/* filter blocks from topological walker */
	if (! is_Block(block))
		return;

	/* the end block has no successor */
	if (block == env->end_block)
		return;

	info = get_block_info(block);
	/* track changes */
	size = ir_valueset_size(info->antic_in);
	n_succ = get_Block_n_cfg_outs(block);

	if (env->first_iter) {
		foreach_valueset(info->exp_gen, value, expr, iter) {
			ir_valueset_insert(info->antic_in, value, expr);
		}
	}

	/* successor might have phi nodes */
	if (n_succ == 1 && get_irn_arity(get_Block_cfg_out(block, 0)) > 1) {
		succ      = get_Block_cfg_out(block, 0);
		int pos   = get_Block_cfgpred_pos(succ, block);
		succ_info = get_block_info(succ);

		/* initialize translated set */
		if (env->first_iter) {
			info->trans = XMALLOC(ir_nodehashmap_t);
			ir_nodehashmap_init(info->trans);
		}

		foreach_valueset(succ_info->antic_in, value, expr, iter) {
			/* we translate the expression over the phi node,
			   remember() builds the value tree */
			ir_node *trans       = phi_translate(expr, succ, pos, block);
			/* create new value if necessary */
			ir_node *trans_value = identify_or_remember(trans);
			ir_node *represent;

			DB((dbg, LEVEL_3, "Translate %+F %+F to %d = %+F (%+F)\n", expr, succ, pos, trans, trans_value));

			/* on value change (phi present) we need the translated node
			   to represent the new value for possible further translation. */
			if (value != trans_value)
				represent = trans;
			else
				represent = expr;

			if (is_clean_in_block_antic(expr, block)) {
				/* Where is the difference between replace and insert?
				   If we replace, we might use a representative with non translated*/
				ir_valueset_insert(info->antic_in, trans_value, represent);
				DB((dbg, LEVEL_3, "Translated %+F repr %+F\n", expr, represent));
			}
			/* during insert we use the translated node, because it may be
			   hoisted into block whilst being not anticipated there. */
			set_translated(info->trans, expr, represent);
		}
	} else if (n_succ > 1) {
		int         i;
		ir_node    *common     = NULL;
		ir_node    *succ0      = get_Block_cfg_out(block, 0);
		block_info *succ0_info = get_block_info(succ0);

		/* disjoint of antic_ins */
		foreach_valueset(succ0_info->antic_in, value, expr, iter) {
			/* iterate over remaining successors */
			for (i = 1; i < n_succ; ++i) {
				ir_node    *succ      = get_Block_cfg_out(block, i);
				block_info *succ_info = get_block_info(succ);

				/* value in antic_in? */
				common = ir_valueset_lookup(succ_info->antic_in, value);
				if (common == NULL)
					break;
			}

			if (common && is_clean_in_block_antic(expr, block)) {
				ir_valueset_insert(info->antic_in, value, expr);
				DB((dbg, LEVEL_3, "common and clean %+F(%+F) in %+F\n", expr, value, block));
			}
		}
	}

	DEBUG_ONLY(dump_value_set(info->antic_in, "Antic_in", block);)

	if (size != ir_valueset_size(info->antic_in))
		env->changes |= 1;
}

/* --------------------------------------------------------
 * Main algorithm Avail_out
 * --------------------------------------------------------
 */

/**
 * Computes Avail_out(block):
 *
 * Avail_in(block)  = Avail_out(dom(block))
 * Avail_out(block) = Avail_in(block) \/ Nodes(block)
 *
 * Precondition:
 *  This function must be called in the top-down topological order:
 *  Then it computes Leader(Nodes(block)) instead of Nodes(block) !
 *
 * @param block   the block
 * @param ctx     walker context
 */
static void compute_avail_top_down(ir_node *block, void *ctx)
{
	pre_env    *env   = (pre_env*)ctx;
	block_info *info;

	if (block == env->end_block)
		return;

	info  = get_block_info(block);

	/* Add all nodes from the immediate dominator.
	   This ensures that avail_out contains the leader. */
	if (block != env->start_block) {
		ir_node                *dom_block = get_Block_idom(block);
		block_info             *dom_info  = get_block_info(dom_block);
		ir_node                *value;
		ir_node                *expr;
		ir_valueset_iterator_t  iter;

		foreach_valueset(dom_info->avail_out, value, expr, iter) {
			/* use first available expr as leader */
			ir_valueset_replace(info->avail_out, value, expr);
		}
	}

	DEBUG_ONLY(dump_value_set(info->avail_out, "Avail_out", block);)
}

/* --------------------------------------------------------
 * Main algorithm redundancy detection
 * --------------------------------------------------------
 */

/**
 * Flags node irn redundant depending on redundant parameter.
 */
static void flag_redundant(ir_node *irn, unsigned redundant)
{
	if (redundant) {
		set_irn_link(irn, irn);
	} else {
		set_irn_link(irn, NULL);
	}
}

/*
 * Returns redundant flag of node irn.
 */
static unsigned is_redundant(ir_node *irn)
{
	return (get_irn_link(irn) != NULL);
}

/**
 * Returns a valid mode if the value of expr is a partially redundant value.
 *
 * @param block   the block
 * @param expr    the expression
 *
 * @return mode of the expression if it is partially redundant else NULL
 */
static ir_mode *is_partially_redundant(ir_node *block, ir_node *expr, ir_node *value)
{
	ir_node *first_avail         = NULL;
	int      pos;
	int      arity               = get_irn_arity(block);
	int      fully_redundant     = 1;
	int      partially_redundant = 0;
	ir_mode *mode                = NULL;

	DB((dbg, LEVEL_3, "is partially redundant %+F(%+F) of %+F\n", expr, value, block));

	/* for each predecessor blocks */
	for (pos = 0; pos < arity; ++pos) {
		block_info *pred_info;
		ir_node    *pred_block  = get_Block_cfgpred_block(block, pos);
		ir_node    *trans_expr;
		ir_node    *trans_value;
		ir_node    *avail_expr;

		/* ignore bad blocks. */
		if (is_Bad(pred_block))
			continue;

		pred_info  = get_block_info(pred_block);
		trans_expr = get_translated(pred_block, expr);
		trans_value = identify(trans_expr);

		/* value might be available through a constant */
		if (is_irn_constlike(trans_expr)) {
			avail_expr = trans_expr;
			if (get_irn_idx(trans_expr) > environ->last_idx) {
				/* limit new constants */
				ir_mode   *cmode  = get_irn_mode(trans_expr);
				ir_tarval *upper = new_tarval_from_long(127, cmode);
				ir_tarval *lower = new_tarval_from_long(-127, cmode);
				ir_tarval *c     = get_Const_tarval(trans_expr);

				if (tarval_cmp(lower, c) != ir_relation_greater_equal &&
				    tarval_cmp(c, upper) != ir_relation_greater_equal) {
					avail_expr = NULL;
				}
			}
	    } else {
			avail_expr = (ir_node*)ir_valueset_lookup(pred_info->avail_out, trans_value);
		}
		DB((dbg, LEVEL_3, "avail_expr %+F\n", avail_expr));

		if (avail_expr == NULL) {
			pred_info->avail = trans_expr;
			pred_info->found = 0;
			fully_redundant  = 0;
		} else {
			/* expr is available */
			pred_info->avail    = avail_expr;
			pred_info->found    = 1;
			mode                = get_irn_mode(avail_expr);
			partially_redundant = 1;

			if (first_avail == NULL)
				first_avail = avail_expr;
			else if (first_avail != avail_expr)
				/* Multiple different expressions are available */
				fully_redundant = 0;

			DB((dbg, LEVEL_2, "Found %+F from block %+F as %+F in pred %+F\n", expr, block, avail_expr, pred_block));
		}
	}

#if BETTER_GREED
	/* value is redundant from last iteration,
	   but has not been removed from antic_in (is not optimized) */
	if (! environ->first_iter && is_redundant(expr))
		return mode;
#endif

	/* If it is not the same value already existing along every predecessor
       and it is defined by some predecessor then it is partially redundant. */
	if (! partially_redundant || fully_redundant)
		return NULL;
	return mode;
}

/**
 * Updates the new_set of a block by adding the new_set of
 * the immediate dominating block.
 *
 * @param  the block
 */
static void update_new_set(ir_node *block, ir_node *idom)
{
	ir_node                *value;
	ir_node                *expr;
	ir_valueset_iterator_t  iter;
	block_info             *curr_info = get_block_info(block);
	block_info             *idom_info = get_block_info(idom);
	int                     updated   = 0;

	DEBUG_ONLY(dump_value_set(idom_info->new_set, "[New Set]", idom);)
	foreach_valueset(idom_info->new_set, value, expr, iter) {
		/* inherit new_set from immediate dominator */
		ir_valueset_insert(curr_info->new_set, value, expr);
		/* replace in avail_out */
		updated |= ir_valueset_replace(curr_info->avail_out, value, expr);
	}
#ifdef DEBUG_libfirm
	if (updated)
		dump_value_set(curr_info->avail_out, "Updated [Avail_out]", block);
#endif
} /* update_new_set */

/**
 * Checks if hoisting irn is greedy.
 * Greedy hoisting means that there are non partially redundant nodes
 * hoisted. This happens if a partially redundant node has
 * non redundant predecessors.
 */
static unsigned is_hoisting_greedy(ir_node *irn, ir_node *block)
{
	int arity = get_irn_arity(irn);
	int i;

	for (i = 0; i < arity; ++i) {
		ir_node *pred  = get_irn_n(irn, i);

		if (! block_strictly_dominates(get_nodes_block(pred), block) && ! is_redundant(pred))
			return 1;
	}
	return 0;
}

/**
 * Perform insertion of partially redundant values.
 * For every block node, do the following:
 * 1.  Propagate the NEW_SETS of the dominator into the current block.
 * If the block has multiple predecessors,
 *     2a. Iterate over the ANTIC expressions for the block to see if
 *         any of them are partially redundant.
 *     2b. If so, insert them into the necessary predecessors to make
 *         the expression fully redundant.
 *     2c. Insert a new Phi merging the values of the predecessors.
 *     2d. Insert the new Phi, and the new expressions, into the
 *         NEW_SETS set.
 *
 * @param block  the block
 * @param ctx    the walker environment
 */
static void insert_nodes(ir_node *block, void *ctx)
{
	pre_env                *env    = (pre_env*)ctx;
	int                     arity  = get_irn_arity(block);
	ir_node                *value;
	ir_node                *expr;
	block_info             *info;
	ir_node                *idom;
	int                     pos;
	ir_valueset_iterator_t  iter;

	/* only blocks */
	if (! is_Block(block))
		return;

	/* ensure that even the start block has a new_set */
	info = get_block_info(block);
	if (info->new_set)
		ir_valueset_del(info->new_set);
	info->new_set = ir_valueset_new(16);

	if (block == env->start_block)
		return;

	DB((dbg, LEVEL_2, "Insert operation of %+F\n", block));

	idom = get_Block_idom(block);
	update_new_set(block, idom);

	/* process only path joining blocks */
	if (arity < 2) {
		return;
	}

#if BETTER_GREED
	plist_t *stack = plist_new();
#endif

	/* This is the main reason we choose to use antic_in over antic_out;
	   we may iterate over every anticipated value first and not
	   over the predecessor blocks. */
	foreach_valueset(info->antic_in, value, expr, iter) {
		ir_mode  *mode;
		ir_node  *phi;
		ir_node **phi_in;

#if BETTER_GREED
		plist_insert_front(stack, expr);
#endif

		/* filter phi nodes from antic_in */
		if (is_Phi(expr)) {
			flag_redundant(expr, 1);
			continue;
		}

		DB((dbg, LEVEL_2, "Insert for %+F (value %+F) in %+F\n", expr, value, block));

		/* A value computed in the dominator is totally redundant.
		   Hence we have nothing to insert. */
		if (ir_valueset_lookup(get_block_info(idom)->avail_out, value)) {
			DB((dbg, LEVEL_2, "Fully redundant expr %+F value %+F\n", expr, value));
			DEBUG_ONLY(inc_stats(gvnpre_stats->fully);)

			flag_redundant(expr, 1);
			continue;
		}

#if !BETTER_GREED
		if (is_hoisting_greedy(expr, block))
			continue;
#endif

		mode = is_partially_redundant(block, expr, value);
		if (mode == NULL) {
			flag_redundant(expr, 0);
			continue;
		} else {
			flag_redundant(expr, 1);
		}

#if BETTER_GREED
		if (is_hoisting_greedy(expr, block))
			continue;
#endif

#if LOADS || DIVMODS
		/* save old mode_M phis to remove keepalive edges later */
		if (is_memop(expr)) {
			ir_node *mem = get_memop_mem(expr);
			if (is_Phi(mem) && get_nodes_block(mem) == get_nodes_block(expr)) {
				ir_nodeset_insert(env->keeps, mem);
			}
		}
#endif

#ifdef DEBUG_libfirm
		if (! is_Proj(expr)) {
			if (env->first_iter)
				inc_stats(gvnpre_stats->first_iter_found);
			inc_stats(gvnpre_stats->partially);
		}
		if (is_Load(expr))
			inc_stats(gvnpre_stats->loads);
		else if (is_Div(expr) || is_Mod(expr))
			inc_stats(gvnpre_stats->divmods);
#endif

		phi_in = XMALLOCN(ir_node *, arity);

		/* for each predecessor block */
		for (pos = 0; pos < arity; ++pos) {
			ir_node    *pred_block = get_Block_cfgpred_block(block, pos);
			block_info *pred_info;

			/* ignore bad blocks. */
			if (is_Bad(pred_block)) {
				ir_graph *irg = get_irn_irg(pred_block);
				phi_in[pos] = new_r_Bad(irg, mode);
				continue;
			}
			pred_info = get_block_info(pred_block);

			if (! pred_info->found) {
				ir_node *trans = get_translated(pred_block, expr);

				assert(trans);
				if (trans == expr) {
					/* has been translated if ancestor had a phi and was translated */
					/* also non phi descendants can be redundant, but have
					   not yet been (phi) translated. */
					trans = phi_translate(expr, block, pos, pred_block);
					set_translated(pred_info->trans, expr, trans);
				}

				DB((dbg, LEVEL_3, "Use new %+F in %+F because expr %+F not available\n", trans, pred_block, expr));

				/* value is now available in target block through trans
				   insert (not replace) because it has not been available */
				ir_valueset_insert(pred_info->avail_out, value, trans);
				phi_in[pos] = trans;
			} else {
				/* value available */
				phi_in[pos] = pred_info->avail;
			}
			DB((dbg, LEVEL_3, "phi_in %+F\n", phi_in[pos]));
		}

		/* We do not connect tuples as they will be connected automatically
		   by the corresponding projections. */
		if (get_irn_mode(expr) != mode_T) {

			phi = new_r_Phi(block, arity, phi_in, mode);
			DB((dbg, LEVEL_3, "New %+F for redundant %+F created\n", phi, expr));

			/* this value is now available through the new phi */
			ir_valueset_replace(info->avail_out, value, phi);
			ir_valueset_insert(info->new_set, value, phi);
		}
		free(phi_in);

		/* remove from antic_in, because expr is not anticipated
		   anymore in this block */
		ir_valueset_remove_iterator(info->antic_in, &iter);
#if HOIST_HIGH
		ir_valueset_insert(info->antic_done, value, expr);
#endif

		env->changes |= 1;
	}

#if BETTER_GREED
	if (env->changes) {
		/* iterate in inverse topological order */
		plist_element_t *it;
		foreach_plist(stack, it) {
			ir_node *irn = (ir_node *)plist_element_get_value(it);
			int j;
			char redundant = 1;
			ir_node *block = get_nodes_block(irn);

			/* does irn only have redundant successors? */

			if (! get_irn_outs_computed(irn))
				continue;

			for (j = get_irn_n_outs(irn) - 1; j >= 0; --j) {
				ir_node *succ = get_irn_out(irn, j);

				/* if succ and irn are in the same block */
				if (get_nodes_block(succ) == block && is_redundant(succ)) {
					continue;
				} else {
					redundant = 0;
					break;
				}
			}

			if (redundant)
				flag_redundant(irn, 1);
		}
	}
	plist_free(stack);
#endif

}

#if HOIST_HIGH
static void hoist_high(ir_node *block, void *ctx)
{
	pre_env                *env        = (pre_env*)ctx;
	block_info             *curr_info;
	ir_valueset_iterator_t  iter;
	ir_node                *expr;
	ir_node                *value;

	if (! is_Block(block))
		return;

	if (block == env->start_block)
		return;

	curr_info = get_block_info(block);

	DB((dbg, LEVEL_2, "High hoisting %+F\n", block));

	/* foreach antic in */
	foreach_valueset(curr_info->antic_done, value, expr, iter) {
		int arity = get_irn_arity(block);
		int pos;

		if (is_memop(expr) || is_Proj(expr))
			continue;

		/* for each path to block */
		for (pos = 0; pos < arity; ++pos) {
			/* standard target is predecessor block */
			ir_node    *target     = get_Block_cfgpred_block(block, pos);
			block_info *pred_info  = get_block_info(target);
			ir_node    *avail;
			ir_node    *new_target;
			ir_node    *trans_expr;
			ir_node    *trans_value;
			ir_node    *dom;
			int         avail_arity;
			int         i;
			unsigned    nest_depth;

			trans_expr  = get_translated(target, expr);
			trans_value = identify(trans_expr);
			avail = (ir_node*)ir_valueset_lookup(pred_info->avail_out, trans_value);

			if (avail == NULL)
				avail = trans_expr;

			value = identify(avail);
			avail_arity = get_irn_arity(avail);

			/* anticipation border */
			new_target = NULL;
			nest_depth = get_loop_depth(get_irn_loop(target));
			dom        = target;

			while(dom != environ->start_block) {
				dom = get_Block_idom(dom);

				if (is_Bad(dom))
					break;

				/* do not hoist into loops */
				if (get_loop_depth(get_irn_loop(dom)) > nest_depth)
					break;

				nest_depth = get_loop_depth(get_irn_loop(dom));

				/* antic_in means that the expression is clean to be
				   hoisted above block, but still into */
				new_target = dom;
				/* check if available node ist still anticipated and clean
				   (clean is part of antic) */
				if (! ir_valueset_lookup(get_block_info(dom)->antic_in, value))
					break;
			}

			/* No new target or does the available node already dominate the new_target? */
			if (new_target) {
				DB((dbg, LEVEL_2, "leader block %+F\n", get_nodes_block(avail)));
				/* already same block or dominating?*/
				if (block_striclty_dominates(new_target, get_nodes_block(avail)))
					new_target = NULL;
			}

			DB((dbg, LEVEL_2, "dom border %+F\n", new_target));

			/* check for uses of available ins on our path*/
			for (i = 0; i < avail_arity; i++) {
				ir_node *pred       = get_irn_n(avail, i);
				ir_node *pred_block = get_nodes_block(avail);
				int      j;

				if (new_target == NULL)
					break;

				if (! get_irn_outs_computed(pred)) {
					new_target = NULL;
					break;
				}

				/**/
				if (! block_strictly_dominates(pred_block, new_target)) {
					new_target = pred_block;
				}

				/* outs of def*/
				for (j = get_irn_n_outs(pred) - 1; j >= 0; --j) {
					ir_node *succ = get_irn_out(pred, j);

					/* on our path?*/
					/* is succ on this path? */
					if (block_strictly_dominates(get_nodes_block(succ), new_target)) {
						ir_node *succ_value = identify(succ);

						/* pred not dead? */
						if (succ_value != value) {
							continue;
						} else {
							new_target = NULL;
							break;
						}
					}
				}
			}

			/* only one usage on our path */
			if (new_target) {
				/* push the available node up into */
				set_nodes_block(avail, new_target);

				DEBUG_ONLY(inc_stats(gvnpre_stats->hoist_high);)
			}
		}
	}
}
#endif

/* --------------------------------------------------------
 * Elimination of fully redundant nodes
 * --------------------------------------------------------
 */

/**
 * Walker which finds redundant nodes using avail_out sets
 * and exchanges them for existing ones.
 * We cannot change the graph here as this would affect
 * the hash values of the nodes.
 *
 * @param irn  the node
 * @param ctx  the walker environment
 */
static void eliminate(ir_node *irn, void *ctx)
{
	pre_env *env = (pre_env*)ctx;

	if (! is_Block(irn)) {
		ir_node    *block = get_nodes_block(irn);
		block_info *info  = get_block_info(block);
		ir_node    *value = identify(irn);

		if (value != NULL) {
			ir_node *expr = (ir_node*)ir_valueset_lookup(info->avail_out, value);

			if (expr != NULL && expr != irn) {
				elim_pair *p = OALLOC(env->obst, elim_pair);

				p->old_node = irn;
				p->new_node = expr;
				p->next     = env->pairs;
				if (get_irn_idx(expr) >= env->last_idx)
					p->reason = FS_OPT_GVN_PARTLY;
				else
					p->reason = FS_OPT_GVN_FULLY;
				env->pairs = p;
				DEBUG_ONLY(inc_stats(gvnpre_stats->replaced);)
			}
		}
	}
}  /* eliminate */

/**
 * Do all the recorded changes and optimize
 * newly created Phi's.
 *
 * @param pairs  list of elimination pairs
 */
static void eliminate_nodes(elim_pair *pairs, ir_nodeset_t *keeps)
{
	elim_pair             *p;
	ir_nodeset_iterator_t iter;
	ir_node               *m_phi;
	ir_node               *end = environ->end_node;

	for (p = pairs; p != NULL; p = p->next) {
		/* might be already changed */
		p->new_node = skip_Id(p->new_node);

		DB((dbg, LEVEL_2, "Replacing %+F by %+F\n", p->old_node, p->new_node));

		/* PRE tends to create Phi(self, self, ... , x, self, self, ...)
		 * which we can optimize here */
		if (is_Phi(p->new_node)) {
			int      i;
			ir_node *res = NULL;

			for (i = get_irn_arity(p->new_node) - 1; i >= 0; --i) {
				ir_node *pred = get_irn_n(p->new_node, i);

				if (pred != p->old_node) {
					if (res) {
						res = NULL;
						break;
					}
					res = pred;
				}
			}
			if (res) {
				exchange(p->new_node, res);
				p->new_node = res;
			}
		}
		DBG_OPT_GVN_PRE(p->old_node, p->new_node, p->reason);

		exchange(p->old_node, p->new_node);
	}

	/* remove keep alive edges of unused mode_M phis */
	foreach_ir_nodeset(keeps, m_phi, iter) {
		remove_End_keepalive(end, m_phi);
	}
}  /* eliminate_nodes */

/* --------------------------------------------------------
 * GVN PRE pass
 * --------------------------------------------------------
 */

/**
 * Gvn_Pre algorithm.
 *
 * @param irg   the graph
 */
static void gvn_pre(ir_graph *irg, pre_env *env)
{
	unsigned              antic_iter;
	unsigned              insert_iter;

	DB((dbg, LEVEL_1, "Doing GVN-PRE for %+F\n", irg));

	/* allocate block info */
	irg_walk_blkwise_graph(irg, block_info_walker, NULL, env);

	ir_nodehashmap_init(&value_map);

	/* generate exp_gen */
	irg_walk_blkwise_graph(irg, NULL, topo_walker, env);
	dump_all_expgen_sets(env->list);

	/* compute the avail_out sets for all blocks */
	dom_tree_walk_irg(irg, compute_avail_top_down, NULL, env);

	/* compute the anticipated value sets for all blocks */
	antic_iter      = 0;
	env->first_iter = 1;

	/* antic_in passes */
	do {
		++antic_iter;
		DB((dbg, LEVEL_2, "= Antic_in Iteration %d ========================\n", antic_iter));
		env->changes = 0;
		irg_walk_blkwise_graph(irg, compute_antic, NULL, env);
		env->first_iter = 0;
		DB((dbg, LEVEL_2, "----------------------------------------------\n"));
	} while (env->changes != 0 && antic_iter < MAX_ANTIC_ITER);

	DEBUG_ONLY(set_stats(gvnpre_stats->antic_iterations, antic_iter);)

	ir_nodeset_init(env->keeps);
	insert_iter       = 0;
	env->insert_phase = 1;
	env->first_iter   = 1;
	env->last_idx     = get_irg_last_idx(irg);
	/* compute redundant expressions */
	do {
		++insert_iter;
		DB((dbg, LEVEL_2, "= Insert Iteration %d ==========================\n", insert_iter));
		env->changes = 0;
		/* TODO topologically top down would be better; fewer iterations. */
		dom_tree_walk_irg(irg, insert_nodes, NULL, env);
		env->first_iter = 0;
		DB((dbg, LEVEL_2, "----------------------------------------------\n"));
	} while (env->changes != 0 && insert_iter < MAX_INSERT_ITER);
	DEBUG_ONLY(set_stats(gvnpre_stats->insert_iterations, insert_iter);)

#if HOIST_HIGH
	dom_tree_walk_irg(irg, hoist_high, NULL, env);
#endif

	/* last step: eliminate nodes */
	irg_walk_graph(irg, NULL, eliminate, env);
	eliminate_nodes(env->pairs, env->keeps);

	ir_nodeset_destroy(env->keeps);
}

/**
 * Gvn_Pre pass for graph irg.
 *
 * @param irg   the graph
 */
void do_gvn_pre(ir_graph *irg)
{
	struct obstack        obst;
	pre_env               env;
	ir_nodeset_t          keeps;
	optimization_state_t  state;
	block_info           *block_info;

	/* bads and unreachables cause too much trouble with dominance
	   dominance
	   loop info for endless loop detection
	   no critical edges is GVN-PRE precondition
	 */
	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
		| IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO
		| IR_GRAPH_PROPERTY_CONSISTENT_OUTS
		| IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
		| IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	/* register a debug mask */
	FIRM_DBG_REGISTER(dbg, "firm.opt.gvn_pre");

	save_optimization_state(&state);
	environ = &env;

	/* edges will crash if enabled due to our allocate on other obstack trick */
	edges_deactivate(irg);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_LOOP_LINK);

	DEBUG_ONLY(init_stats();)

	/* setup environment */
	obstack_init(&obst);
	env.graph        = irg;
	env.obst         = &obst;
	env.list         = NULL;
	env.start_block  = get_irg_start_block(irg);
	env.end_block    = get_irg_end_block(irg);
	env.end_node     = get_irg_end(irg);
	env.pairs        = NULL;
	env.keeps        = &keeps;
	env.insert_phase = 0;

	/* Detect and set links of infinite loops to non-zero. */
	analyse_loops(irg);

	/* Switch on GCSE. We need it to correctly compute
	   the value of a node, which is independent from
	   its block. */
	set_opt_global_cse(1);
	/* new_identities */
	if (irg->value_table != NULL)
		del_pset(irg->value_table);
	/* initially assumed nodes in pset are 512 */
	irg->value_table = new_pset(compare_gvn_identities, 512);

	/* do GVN-PRE pass */
	gvn_pre(irg, &env);
	DEBUG_ONLY(print_stats();)

	/* clean up: delete all sets */
	for (block_info = env.list; block_info != NULL; block_info = block_info->next) {
		ir_valueset_del(block_info->exp_gen);
		ir_valueset_del(block_info->avail_out);
		ir_valueset_del(block_info->antic_in);
		if (block_info->trans) {
			ir_nodehashmap_destroy(block_info->trans);
			free(block_info->trans);
		}
		if (block_info->new_set)
			ir_valueset_del(block_info->new_set);
	}

	DEBUG_ONLY(free_stats();)
	ir_nodehashmap_destroy(&value_map);
	obstack_free(&obst, NULL);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_LOOP_LINK);

	/* Pin the graph again.
	   This is needed due to the use of set_opt_global_cse(1) */
	set_irg_pinned(irg, op_pin_state_pinned);
	restore_optimization_state(&state);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);

	/* TODO there are optimizations that try to use the existing value_table */
	new_identities(irg);
}

/* Creates an ir_graph pass for do_gvn_pre. */
ir_graph_pass_t *do_gvn_pre_pass(const char *name)
{
	return def_graph_pass(name ? name : "gvn_pre", do_gvn_pre);
}
