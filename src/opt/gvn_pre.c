/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Global Value Numbering Partial Redundancy Elimination
 *          (VanDrunen Hosking 2004)
 * @author  Michael Beck
 * @brief
 */
#include "debug.h"
#include "ircons.h"
#include "irdom.h"
#include "iredges_t.h"
#include "irflag.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irloop.h"
#include "irnode_t.h"
#include "irnodehashmap.h"
#include "irnodeset.h"
#include "iropt_dbg.h"
#include "iropt_t.h"
#include "iroptimize.h"
#include "irouts.h"
#include "tv_t.h"
#include "valueset.h"

/* suggested by GVN-PRE authors */
#define MAX_ANTIC_ITER 10
#define MAX_INSERT_ITER 3

/* Stops antic iteration from processing endless loops. */
#define IGNORE_INF_LOOPS 1
/* Stops antic information to flow over infinite loop backedge */
#define NO_INF_LOOPS 0

/* Attempt to reduce register pressure and reduce code size
   for hoisted nodes. */
#define HOIST_HIGH 1
#define COMMON_DOM 1

/* Seamless implementation of handling loads and generally memory
   dependent nodes with GVN-PRE. */
#define LOADS 1
#define DIVMODS 0

/* Experimental */

/* Only optimize node directly after phi node if node is only successor. */
#define MIN_CUT 0

/* GVN is very limited. This enables optimize_node during value recognition.
   GVN is still very limited, since a+1+b+1 does not equal a+b+2.
   TODO Broken for yet unknown reasons. */
#define OPTIMIZE_NODES 0


/** Additional info we need for every block. */
typedef struct block_info {
	ir_valueset_t     *exp_gen;    /* contains this blocks clean expressions */
	ir_valueset_t     *avail_out;  /* available values at block end */
	ir_valueset_t     *antic_in;   /* clean anticipated values at block entry */
	ir_valueset_t     *antic_done; /* keeps elements of antic_in after insert nodes phase */
	ir_valueset_t     *new_set;    /* new by hoisting made available values */
	ir_nodehashmap_t  *trans;      /* contains translated nodes translated into block */
	ir_node           *avail;      /* saves available node for insert node phase */
	int                found;      /* saves kind of availability for insert_node phase */
	ir_node           *block;      /* block of the block_info */
	struct block_info *next;       /* links all instances for easy access */
} block_info;

/**
 * A pair of nodes to be exchanged.
 * We have to defer the exchange because there are still needed references
 * to certain nodes.
 */
typedef struct elim_pair {
	ir_node *old_node;      /* node that will be replaced */
	ir_node *new_node;      /* replacement for old_node */
	struct elim_pair *next; /* links all instances for easy access */
} elim_pair;

/** environment for the GVN-PRE algorithm */
typedef struct pre_env {
	ir_graph       *graph;        /* current graph */
	struct obstack  obst;         /* obstack to allocate on */
	ir_node        *start_block;  /* start block of the current graph */
	ir_node        *end_block;    /* end block of the current graph */
	ir_node        *end_node;     /* end node of the current graph */
	block_info     *list;         /* block_info list head */
	elim_pair      *pairs;        /* elim_pair list head */
	ir_nodeset_t   *keeps;        /* a list of to be removed phis to kill their keep alive edges */
	unsigned        last_idx;     /* last node index of input graph */
	char            changes;      /* flag for fixed point iterations - non-zero if changes occurred */
	char            first_iter;   /* non-zero for first fixed point iteration */
	int             iteration;    /* iteration counter */
#if OPTIMIZE_NODES
	pset           *value_table;   /* standard value table*/
	pset           *gvnpre_values; /* GVN-PRE value table */
#endif
} pre_env;

static pre_env *environment;

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

static gvnpre_statistics *gvnpre_stats = NULL;

static void init_stats(void)
{
	gvnpre_stats = XMALLOCZ(gvnpre_statistics);
}

static void free_stats(void)
{
	free(gvnpre_stats);
	gvnpre_stats = NULL;
}

static void print_stats(void)
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
 * Dump value sets
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
}

/**
 * Dump all exp_gen value sets.
 *
 * @param list  the list of block infos to retrieve the sets from
 */
static void dump_all_expgen_sets(block_info *list)
{
	for (block_info *block_info = list; block_info != NULL; block_info = block_info->next) {
		dump_value_set(block_info->exp_gen, "[Exp_gen]", block_info->block);
	}
}

#else

#define dump_all_expgen_sets(list) ((void)(list))
#define dump_value_set(set, txt, block)

#endif /* DEBUG_libfirm */

/* --------------------------------------------------------
 * GVN Functions
 * --------------------------------------------------------
 */

/**
 * Compares node collisions in value table.
 * Modified identities_cmp().
 */
static int compare_gvn_identities(const void *elt, const void *key)
{
	ir_node *a = (ir_node *)elt;
	ir_node *b = (ir_node *)key;

	if (a == b)
		return 0;

	/* phi nodes kill predecessor values and are always different */
	if (is_Phi(a) || is_Phi(b))
		return 1;

	/* memops are not the same, even if we want to optimize them
	   we have to take the order in account */
	if (is_memop(a) || is_memop(b) ||
	    get_irn_mode(a) == mode_T ||
	    get_irn_mode(b) == mode_T) {
		/* Loads with the same predecessors are the same value;
		   this should only happen after phi translation. */
		if ((!is_Load(a) || !is_Load(b)) && (!is_Store(a) || !is_Store(b)))
			return 1;
	}

	if ((get_irn_op(a) != get_irn_op(b)) ||
	    (get_irn_mode(a) != get_irn_mode(b)))
		return 1;

	/* compare if a's in and b's in are of equal length */
	int irn_arity_a = get_irn_arity(a);
	if (irn_arity_a != get_irn_arity(b))
		return 1;

	/* blocks are never the same */
	if (is_Block(a) || is_Block(b))
		return 1;

	/* should only be used with GCSE enabled */
	assert(get_opt_global_cse());

	/* compare a->in[0..ins] with b->in[0..ins] */
	for (int i = 0; i < irn_arity_a; ++i) {
		ir_node *pred_a = get_irn_n(a, i);
		ir_node *pred_b = get_irn_n(b, i);
		if (pred_a != pred_b)
			return 1;
	}

	/* here, we already now that the nodes are identical except their
	 * attributes */
	return !a->op->ops.attrs_equal(a, b);
}

/**
 * Identify does a lookup in the GVN value table.
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
	/* irn represents a new value, so return the leader */
	return identify_remember(irn);
}

/**
 * remember() adds node irn to the GVN value table.
 * Identify_remember only identifies values of nodes with the
 * same predecessor nodes (not values). By creating a node from the predecessor
 * values/leaders, a true value tree is built. Phis kill their predecessor value,
 * so no circular dependencies need to be resolved.
 *
 * TODO Improvement:
 *      Maybe this could be implemented with a custom node hash that takes
 *      phi nodes and true values (instead of predecessors) into account,
 *      resulting in value numbers.
 * TODO This unnecessarily also handles nodes like calls, which are never equal.
 *
 * @param irn  a node representing an expression
 * @return     the value of the expression
 */
static ir_node *remember(ir_node *irn)
{
	int       arity   = get_irn_arity(irn);
	int       changed = 0;
	ir_node **in      = XMALLOCN(ir_node *, arity);
	ir_node  *value;

	foreach_irn_in(irn, i, pred) {
		/* value and leader at the same time */
		ir_node *pred_value = identify(pred);

		/* phi will be translated anyway, so kill the predecessor values
		   this also prevents circular dependencies */
		if (is_Phi(pred)) {
			/* every phi represents its own value */
			in[i] = pred;
			continue;
		}

		/* predecessor is not its value representation/the leader */
		if (pred != pred_value)
			changed = 1;
		in[i] = pred_value;
	}

	if (changed && !is_memop(irn) && get_irn_mode(irn) != mode_X) {
		/* create representative for */
		ir_node *nn = new_similar_node(irn, get_nodes_block(irn), in);

#if OPTIMIZE_NODES
		/* TODO optimize_node() uses the value table and thus the custom
		   identify_cmp() and will fail trying. */
		environment->graph->value_table = environment->value_table;
		nn = optimize_node(nn);
		environment->graph->value_table = environment->gvnpre_values;
#endif

		/* now the value can be determined because the
		   predecessors are the leaders */
		value = identify_remember(nn);
	} else {
		value = identify_remember(irn);
	}
	free(in);

	DB((dbg, LEVEL_4, "Remember %+F as value %+F\n", irn, value));
	ir_nodehashmap_insert(&value_map, irn, value);

	return value;
}

/**
 * When the value map has been built we may lookup expressions
 * and remember them if new.
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
	block_info *info = OALLOC(&env->obst, block_info);

	set_irn_link(block, info);
	info->exp_gen    = ir_valueset_new(16);
	info->avail_out  = ir_valueset_new(16);
	info->antic_in   = ir_valueset_new(16);
	info->antic_done = ir_valueset_new(16);
	info->trans = XMALLOC(ir_nodehashmap_t);
	ir_nodehashmap_init(info->trans);

	info->new_set = NULL;
	info->avail   = NULL;
	info->block   = block;
	info->found   = 1;

	info->next = env->list;
	env->list  = info;
}

static void free_block_info(block_info *block_info)
{
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
	(void)env;

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
	ir_loop *outer = get_irg_loop(environment->graph);

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
	ir_loop *outer = get_irg_loop(environment->graph);
	ir_loop *l     = loop;
	ir_loop *last  = NULL;

	while (l != outer) {
		last = l;
		l    = get_loop_outer_loop(l);
	}
	return last;
}

/**
 * Topological bottom-up walker sets links of infinite loops to non-zero.
 * Block marks are used to flag blocks reachable (from end) on the one hand,
 * on the other hand they are set if the block is not part of an infinite loop.
 */
static void infinite_loop_walker(ir_node *block, void *env)
{
	(void)env;

	if (!is_Block(block))
		return;

	/* start block has no predecessors */
	if (block == environment->start_block)
		return;

	int arity = get_irn_arity(block);

	/* block not part of a real loop: no infinite loop */
	if (get_irn_loop(block) == get_irg_loop(environment->graph))
		set_Block_mark(block, 1);

	if (get_Block_mark(block)) {
		/* reachable block: mark all cf predecessors */
		for (int i = 0; i < arity; ++i) {
			ir_node *pred = get_Block_cfgpred_block(block, i);
			if (pred == NULL)
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
		   an infinite loop, because we just reached them. So we mark the
		   block to prevent triggering the infinite loop detection. */

		/* passing information to the cf predecessors */
		for (int i = 0; i < arity; ++i) {
			ir_node *pred = get_Block_cfgpred_block(block, i);
			if (pred == NULL)
				continue;

			/* If our control flow predecessor is in the same endless loop,
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

#if IGNORE_INF_LOOPS || NO_INF_LOOPS
/**
 * Returns non-zero if block is part of an infinite loop.
 */
static unsigned is_in_infinite_loop(ir_node *block)
{
	assert(is_Block(block));
	ir_loop *loop = get_irn_loop(block);
	assert(loop);

	loop = get_loop_outermost(loop);
	if (loop)
		return (get_loop_link(loop) != NULL);

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

	if (is_Phi(n))
		return 1;

#if LOADS || DIVMODS
	if (is_Proj(n) && mode != mode_X && mode != mode_T)
		return 1;
#else
	if (is_Proj(n))
		return 0;
#endif

#if LOADS
	if (is_Load(n))
		return get_Load_volatility(n) == volatility_non_volatile;
	if (is_Store(n))
		return get_Store_volatility(n) == volatility_non_volatile;
#endif

	if (get_irn_pinned(n))
		return 0;

	if (!mode_is_data(mode)) {
		if (!is_Div(n) && !is_Mod(n))
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
static unsigned is_clean_in_block(ir_node *n, ir_node *block, ir_valueset_t *valueset)
{
	if (is_Phi(n))
		return 1;

	if (!is_nice_value(n))
		return 0;

#if LOADS
	/* filter loads with no phi predecessor from antic_in */
	if (is_Load(n) && !is_Phi(get_Load_mem(n)))
		return 0;
	if (is_Store(n) && !is_Phi(get_Store_mem(n)))
		return 0;
#endif

#if DIVMODS
	if (is_Div(n)) {
		ir_node *mem = get_Div_mem(n);

		mem = skip_Pin(mem);

		if (!is_Phi(mem) && !is_NoMem(mem))
			return 0;
	}

	if (is_Mod(n) && !is_Phi(get_Mod_mem(n)))
		return 0;
#endif

	foreach_irn_in(n, i, pred) {
		if (is_Phi(pred))
			continue;

		/* we only handle current block */
		if (get_nodes_block(pred) != block)
			continue;

		if (!is_nice_value(pred))
			return 0;

		ir_node *const value = identify(pred);
		if (!ir_valueset_lookup(valueset, value))
			return 0;
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
	(void)ctx;

	if (is_Block(irn))
		return;

#if OPTIMIZE_NODES
	environment->graph->value_table = environment->value_table;
	identify_remember(irn);
	environment->graph->value_table = environment->gvnpre_values;
#endif

	/* GVN step: remember the value. */
	ir_node *value = remember(irn);

	if (is_irn_constlike(irn))
		return;

	ir_node    *block = get_nodes_block(irn);
	block_info *info  = get_block_info(block);

	if (get_irn_mode(irn) != mode_X)
		ir_valueset_insert(info->avail_out, value, irn);

	/* values that are not in antic_in also don't need to be in any other set */

	if (!is_nice_value(irn))
		return;

	if (is_clean_in_block(irn, block, info->exp_gen)) {
		DB((dbg, LEVEL_3, "%+F clean in block %+F\n", irn, block));

		ir_valueset_insert(info->exp_gen, value, irn);
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
 * Translates an expression above a Phi.
 *
 * @param node        the node
 * @param block       the block the node is translated into
 * @param pos         the input number of the destination block
 *
 * @return a node representing the translated value
 */
static ir_node *phi_translate(ir_node *node, ir_node *block, int pos, ir_valueset_t *leaderset)
{
	ir_node *pred_block = get_Block_cfgpred_block(block, pos);

	if (is_Phi(node)) {
		if (get_nodes_block(node) == block)
			return get_Phi_pred(node, pos);
		/* this phi does not need translation */
		return node;
	}

	int        arity  = get_irn_arity(node);
	unsigned   needed = 0;
	ir_node  **in     = ALLOCANZ(ir_node *, arity);

	/* A value has several representatives. The anti leader is chosen to be
	   the main representative. If we access a node as representative of a
	   value we always use the anti leader. The anti leader can be found by
	   antic_in(identify(node)). */
	foreach_irn_in(node, i, pred) {
		ir_node *value  = identify(pred);
		/* get leader for pred to lookup its translated value */
		ir_node *leader = ir_valueset_lookup(leaderset, value);
		ir_node *pred_trans;
		ir_node *new_pred;

		if (!leader)
			leader = pred;

		/* we cannot find this value in antic_in, because the value
		   has (possibly) changed! */
		pred_trans = get_translated(pred_block, leader);


#if DIVMODS
		if (is_Div(node)) {
			ir_node *mem = get_Div_mem(node);

			mem = skip_Pin(mem);

			if (!is_Phi(mem))
				pred_trans = get_Div_mem(node);
		}
#endif

		DB((dbg, LEVEL_3, "trans %+F of %+F is  %+F\n", leader, pred_block, pred_trans));
		if (pred_trans == NULL) {
			new_pred = pred;
		} else {
			new_pred = pred_trans;

			/* loads: Predecessor is a memory phi, which translated yields a proj or
			   another phi. In case of projection and a load predecessor,
			   skip them and use the loads memory. */
			if (is_Proj(pred_trans) && get_irn_mode(pred_trans) == mode_M) {
#if LOADS || DIVMODS
				ir_node *loadstore = get_Proj_pred(pred_trans);
				/* If we do not translate this node, we will get its value wrong. */
				needed |= 1;

				if (is_Load(loadstore)) {
					/* Put new load under the adjacent loads memory edge
					   such that GVN may compare them. */
					new_pred = get_Load_mem(loadstore);
				} else if (is_Store(loadstore)) {
					new_pred = get_Store_mem(loadstore);
				}
#endif
			} else {
				/* predecessor value changed, so translation is needed */
				if (identify(new_pred) != identify(pred))
					needed |= 1;
			}
		}

		DB((dbg, LEVEL_4, "in %+F\n", new_pred));
		in[i] = new_pred;
	}

	if (!needed)
		return node;

	DB((dbg, LEVEL_3, "Translate\n"));

	if (is_Proj(node))
		pred_block = get_nodes_block(in[0]);

	/* copy node to represent the new value.
	   We do not translate nodes that do not need translation,
	   so we use the newly created nodes as value representatives only.
	   Their block is not important, because we create new ones during
	   the insert node phase. */
	ir_node *const nn = new_similar_node(node, pred_block, in);
	/* Optimizing nn here is tempting but might be against the GVN-PRE algorithm
	   because it already uses availability. */

	DB((dbg, LEVEL_3, "New node %+F in %+F origin %+F\n", nn, get_Block_cfgpred_block(block, pos), node));
	return nn;
}

/**
 * Block-walker, computes Antic_in(block).
 * Builds a value tree out of the graph by translating values
 * over phi nodes.
 *
 * @param block  the block
 * @param ctx    the walker environment
 */
static void compute_antic(ir_node *block, void *ctx)
{
	pre_env                *env   = (pre_env *)ctx;
	ir_node                *value;
	ir_node                *expr;
	ir_valueset_iterator_t  iter;

	/* filter blocks from topological walker */
	if (!is_Block(block))
		return;

	/* the end block has no successor */
	if (block == env->end_block)
		return;

	block_info *info = get_block_info(block);
	/* track changes */
	size_t size   = ir_valueset_size(info->antic_in);
	int    n_succ = get_Block_n_cfg_outs(block);

	/* add exp_gen */
	if (env->first_iter) {
#if IGNORE_INF_LOOPS
		/* keep antic_in of infinite loops empty */
		if (!is_in_infinite_loop(block)) {
			foreach_valueset(info->exp_gen, value, expr, iter) {
				ir_valueset_insert(info->antic_in, value, expr);
			}
		}
#else
		foreach_valueset(info->exp_gen, value, expr, iter) {
			ir_valueset_insert(info->antic_in, value, expr);
		}
#endif
	}

	/* successor might have phi nodes */
	if (n_succ == 1 && get_irn_arity(get_Block_cfg_out(block, 0)) > 1) {
		int         pos;
		ir_node    *succ      = get_Block_cfg_out_ex(block, 0, &pos);
		block_info *succ_info = get_block_info(succ);

		/* initialize translated set */
		if (env->first_iter) {
			info->trans = XMALLOC(ir_nodehashmap_t);
			ir_nodehashmap_init(info->trans);
		}

		foreach_valueset(succ_info->antic_in, value, expr, iter) {
			ir_node *trans = get_translated(block, expr);
			ir_node *trans_value;
			ir_node *represent;

			if (trans == NULL)
				trans = phi_translate(expr, succ, pos, get_block_info(succ)->antic_in);

			/* create new value if necessary */
			trans_value = identify_or_remember(trans);

			DB((dbg, LEVEL_3, "Translate %+F %+F to %d = %+F (%+F)\n", expr, succ, pos, trans, trans_value));

			/* On value change (phi present) we need the translated node
			   to represent the new value for possible further translation. */
			represent = value != trans_value ? trans : expr;

			if (is_clean_in_block(expr, block, info->antic_in)) {
#if NO_INF_LOOPS
				/* Prevent information flow over the backedge of endless loops. */
				if (env->iteration <= 2 || (is_backedge(succ, pos) && !is_in_infinite_loop(succ))) {
					ir_valueset_replace(info->antic_in, trans_value, represent);
				}
#else
				ir_valueset_replace(info->antic_in, trans_value, represent);
#endif
			}
			set_translated(info->trans, expr, represent);
		}

	} else if (n_succ > 1) {
		ir_node    *common     = NULL;
		ir_node    *succ0      = get_Block_cfg_out(block, 0);
		block_info *succ0_info = get_block_info(succ0);

		/* disjoint of antic_ins */
		foreach_valueset(succ0_info->antic_in, value, expr, iter) {
			/* iterate over remaining successors */
			for (int i = 1; i < n_succ; ++i) {
				ir_node    *succ      = get_Block_cfg_out(block, i);
				block_info *succ_info = get_block_info(succ);

				/* value in antic_in? */
				common = ir_valueset_lookup(succ_info->antic_in, value);
				if (common == NULL)
					break;
			}

			if (common && is_clean_in_block(expr, block, info->antic_in))
				ir_valueset_replace(info->antic_in, value, expr);
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
	pre_env *env = (pre_env *)ctx;

	if (block == env->end_block)
		return;

	block_info *info = get_block_info(block);

	/* Add all nodes from the immediate dominator.
	   This ensures that avail_out contains the leader. */
	if (block != env->start_block) {
		ir_node                *dom_block = get_Block_idom(block);
		block_info             *dom_info  = get_block_info(dom_block);
		ir_node                *value;
		ir_node                *expr;
		ir_valueset_iterator_t  iter;

		foreach_valueset(dom_info->avail_out, value, expr, iter)
			/* replace: use the leader from dominator, not local exp_gen */
			ir_valueset_replace(info->avail_out, value, expr);
	}

	DEBUG_ONLY(dump_value_set(info->avail_out, "Avail_out", block);)
}

/* --------------------------------------------------------
 * Main algorithm redundancy detection
 * --------------------------------------------------------
 */

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
	int      arity               = get_irn_arity(block);
	bool     fully_redundant     = true;
	bool     partially_redundant = false;
	ir_mode *mode                = NULL;

	(void)value;
	DB((dbg, LEVEL_3, "is partially redundant %+F(%+F) of %+F\n", expr, value, block));

	/* for each predecessor blocks */
	for (int pos = 0; pos < arity; ++pos) {
		ir_node    *pred_block  = get_Block_cfgpred_block(block, pos);
		ir_node    *avail_expr;

		block_info *pred_info  = get_block_info(pred_block);
		ir_node    *trans_expr = get_translated(pred_block, expr);
		ir_node    *trans_value = identify(trans_expr);

		if (is_Const(trans_expr))
			avail_expr = trans_expr;
		else
			avail_expr = (ir_node*)ir_valueset_lookup(pred_info->avail_out, trans_value);

		/* value might be available through a not yet existing constant */
		if (avail_expr == NULL && is_Const(trans_expr)) {
			/* limit range of new constants */
			ir_mode   *cmode = get_irn_mode(trans_expr);
			ir_tarval *upper = new_tarval_from_long(127, cmode);
			ir_tarval *lower = new_tarval_from_long(-127, cmode);
			ir_tarval *c     = get_Const_tarval(trans_expr);

			/* tarval within range? */
			if (tarval_in_range(lower, c, upper))
				avail_expr = trans_expr;
		}

		DB((dbg, LEVEL_3, "avail_expr %+F  trans_expr %+F\n", avail_expr, trans_expr));

		if (avail_expr == NULL) {
			pred_info->avail = trans_expr;
			pred_info->found = 0;
			fully_redundant  = false;
		} else {
			/* expr is available, use the leader */
			pred_info->avail    = avail_expr;
			pred_info->found    = 1;
			mode                = get_irn_mode(avail_expr);
			partially_redundant = true;

			if (first_avail == NULL)
				first_avail = avail_expr;
			else if (first_avail != avail_expr)
				/* Multiple different expressions are available,
				   This is why we need no cut over avail_out sets. */
				fully_redundant = false;

			DB((dbg, LEVEL_2, "Found %+F from block %+F as %+F in pred %+F\n", expr, block, avail_expr, pred_block));
		}
	}

	/* If it is not the same value already existing along every predecessor
	   and it is defined by some predecessor then it is partially redundant. */
	if (!partially_redundant || fully_redundant)
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
#else
	(void)updated;
#endif
}

/**
 * Checks if hoisting irn is greedy.
 * Greedy hoisting means that there are non partially redundant nodes
 * hoisted. This happens if a partially redundant node has
 * non redundant predecessors.
 */
static unsigned is_hoisting_greedy(ir_node *irn, ir_node *block)
{
	int         block_arity = get_irn_arity(block);
	block_info *info        = get_block_info(block);

	/* As long as the predecessor values are available in all predecessor blocks,
	   we can hoist this value. */
	for (int pos = 0; pos < block_arity; ++pos) {
		ir_node    *pred_block = get_Block_cfgpred_block(block, pos);
		block_info *pred_info  = get_block_info(pred_block);

		foreach_irn_in(irn, i, pred) {
#if MIN_CUT
			/* Very conservative min cut. Phi might only have 1 user. */
			if (is_Phi(pred) && get_irn_n_edges(pred) != 1)
				return 1;
#endif

			if (is_Phi(pred) && get_nodes_block(pred) == block)
				continue;

			DB((dbg, LEVEL_3, "pred %+F\n", pred));
			ir_node *value = identify(pred);
			ir_node *leader = ir_valueset_lookup(info->antic_in, value);
			if (!leader)
				leader = pred;
			DB((dbg, LEVEL_3, "lead %+F\n", leader));
			ir_node *trans = get_translated(pred_block, leader);
			if (!trans)
				trans = pred;
			DB((dbg, LEVEL_3, "trans %+F\n", trans));

			ir_node *trans_val = identify(trans);
			DB((dbg, LEVEL_3, "value %+F\n", trans_val));

			if (is_Address(trans_val) || is_Align(trans_val) || is_Const(trans_val) || is_Offset(trans_val) || is_Size(trans_val)) {
				/* existing constant */
				if (get_irn_idx(trans_val) < environment->last_idx) {
					continue;
				} else {
					/* limit range of new constants */
					ir_mode   *cmode = get_irn_mode(trans);
					ir_tarval *upper = new_tarval_from_long( 127, cmode);
					ir_tarval *lower = new_tarval_from_long(-127, cmode);
					ir_tarval *c     = get_Const_tarval(trans);

					/* tarval within range? */
					if (tarval_in_range(lower, c, upper)) {
						continue;
					} else {
						return 1;
					}
				}
			}

			if (is_irn_constlike(trans_val))
				continue;

			ir_node *avail = ir_valueset_lookup(pred_info->avail_out, trans_val);

			DB((dbg, LEVEL_3, "avail %+F\n", avail));
			if (!avail)
				return 1;
#if MIN_CUT
			/* only optimize if predecessors have been optimized */
			if (ir_valueset_lookup(info->antic_done, value) == NULL)
				return 1;
#endif
		}
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
static void insert_nodes_walker(ir_node *block, void *ctx)
{
	pre_env *env = (pre_env *)ctx;

	/* ensure that even the start block has a new_set */
	block_info *info = get_block_info(block);
	if (info->new_set)
		ir_valueset_del(info->new_set);
	info->new_set = ir_valueset_new(16);

	if (block == env->start_block)
		return;

	DB((dbg, LEVEL_2, "Insert operation of %+F\n", block));

	ir_node *idom = get_Block_idom(block);
	update_new_set(block, idom);

	/* process only path joining blocks */
	int arity = get_irn_arity(block);
	if (arity < 2) {
		return;
	}

	ir_node                *value;
	ir_node                *expr;
	ir_valueset_iterator_t  iter;

	/* This is the main reason antic_in is preferred over antic_out;
	   we may iterate over every anticipated value first and not
	   over the predecessor blocks. */
	foreach_valueset(info->antic_in, value, expr, iter) {
		/* already done? */
		if (ir_valueset_lookup(info->antic_done, value))
			continue;

		/* filter phi nodes from antic_in */
		if (is_Phi(expr))
			continue;

		DB((dbg, LEVEL_2, "Insert for %+F (value %+F) in %+F\n", expr, value, block));

		/* A value computed in the dominator is totally redundant.
		   Hence we have nothing to insert. */
		if (ir_valueset_lookup(get_block_info(idom)->avail_out, value)) {
			DB((dbg, LEVEL_2, "Fully redundant expr %+F value %+F\n", expr, value));
			DEBUG_ONLY(inc_stats(gvnpre_stats->fully);)

			ir_valueset_insert(info->antic_done, value, expr);
			continue;
		}

		if (is_hoisting_greedy(expr, block)) {
			DB((dbg, LEVEL_2, "greedy\n"));
			continue;
		}

		ir_mode  *mode = is_partially_redundant(block, expr, value);
		if (mode == NULL)
			continue;

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
		if (!is_Proj(expr)) {
			if (env->first_iter)
				inc_stats(gvnpre_stats->first_iter_found);
			inc_stats(gvnpre_stats->partially);
		}
		if (is_Load(expr) || is_Store(expr))
			inc_stats(gvnpre_stats->loads);
		else if (is_Div(expr) || is_Mod(expr))
			inc_stats(gvnpre_stats->divmods);
#endif

		ir_node **phi_in = XMALLOCN(ir_node *, arity);

		/* for each predecessor block */
		for (int pos = 0; pos < arity; ++pos) {
			ir_node    *pred_block = get_Block_cfgpred_block(block, pos);
			block_info *pred_info;

			pred_info = get_block_info(pred_block);

			if (!pred_info->found) {
				int       node_arity   = get_irn_arity(expr);
				ir_node **in           = XMALLOCNZ(ir_node *, node_arity);
				ir_node  *target_block = pred_block;

				foreach_irn_in(expr, i, pred) {
					const ir_node *value = identify(pred);

					/* transform knowledge over the predecessor from
					   anti-leader world into leader world. */

					DB((dbg, LEVEL_3, "pred %+F\n", pred));

					/* get leader for pred to lookup its translated value */
					ir_node *leader = ir_valueset_lookup(info->antic_in, value);
					if (!leader)
						leader = pred;
					DB((dbg, LEVEL_3, "lead %+F\n", leader));

					ir_node *trans = get_translated(pred_block, leader);
					if (!trans)
						trans = pred;
					DB((dbg, LEVEL_3, "trans %+F\n", trans));

					/* in case of phi, we are done */
					if (is_Phi(pred) && get_nodes_block(pred) == block) {
						in[i] = trans;
						continue;
					}

					ir_node *trans_val = identify(trans);
					DB((dbg, LEVEL_3, "value %+F\n", trans_val));

					/* constants are always available but not in avail set */
					if (is_irn_constlike(trans_val)) {
						in[i] = trans;
						continue;
					}

					/* use the leader
					   In case of loads we need to make sure the hoisted
					   loads are found despite their unique value. */
					ir_node *avail = ir_valueset_lookup(pred_info->avail_out, trans_val);
					DB((dbg, LEVEL_3, "avail %+F\n", avail));

					assert(avail && "predecessor has to be available");
					in[i] = avail;
				}

				if (is_Proj(expr))
					target_block = get_nodes_block(in[0]);

				/* Copy node to represent the new value.
				   We use translated nodes as value representatives only.
				   They have anti leaders as predecessors, not leaders!
				   So we have to create a new node using leaders. */
				ir_node *trans = new_similar_node(expr, target_block, in);
				free(in);

				/* value is now available in target block through trans
				   insert (not replace) because it has not been available */
				ir_node *new_value = identify_or_remember(trans);
				ir_valueset_insert(pred_info->avail_out, new_value, trans);
				DB((dbg, LEVEL_4, "avail%+F+= trans %+F(%+F)\n", pred_block, trans, new_value));

				ir_node *new_value2 = identify(get_translated(pred_block, expr));
				ir_valueset_insert(pred_info->avail_out, new_value2, trans);
				DB((dbg, LEVEL_4, "avail%+F+= trans %+F(%+F)\n", pred_block, trans, new_value2));

				DB((dbg, LEVEL_3, "Use new %+F in %+F because %+F(%+F) not available\n", trans, pred_block, expr, value));

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
			ir_node *phi = new_r_Phi(block, arity, phi_in, mode);
			DB((dbg, LEVEL_3, "New %+F for redundant %+F created\n", phi, expr));

			/* This value is now available through the new phi.
			   insert || replace in avail_out */
			ir_valueset_replace(info->avail_out, value, phi);
			ir_valueset_insert(info->new_set, value, phi);
		}
		free(phi_in);

		/* already optimized this value in this block */
		ir_valueset_insert(info->antic_done, value, expr);
		env->changes |= 1;
	}
}

#if HOIST_HIGH
static void update_new_set_walker(ir_node *block, void *ctx)
{
	pre_env *env = (pre_env*)ctx;

	if (!is_Block(block))
		return;
	if (block == env->start_block)
		return;

	update_new_set(block, get_Block_idom(block));
}

/**
 * Domtree block walker to insert nodes with dying operands
 * into the highest possible block whilst still being anticipated.
 */
static void hoist_high(ir_node *block, void *ctx)
{
	(void)ctx;

	ir_valueset_iterator_t  iter;
	ir_node                *expr;
	ir_node                *value;
	int                     arity = get_irn_arity(block);

	if (!is_Block(block))
		return;

	block_info *curr_info = get_block_info(block);

	if (curr_info->new_set)
		ir_valueset_del(curr_info->new_set);
	curr_info->new_set = ir_valueset_new(16);

	if (arity < 2)
		return;

	DB((dbg, LEVEL_2, "High hoisting %+F\n", block));

	/* foreach entry optimized by insert node phase */
	foreach_valueset(curr_info->antic_done, value, expr, iter) {
		int pos;

		/* TODO currently we cannot handle load and their projections */
		if (is_memop(expr) || is_Proj(expr))
			continue;

		DB((dbg, LEVEL_4, "leader %+F value %+F\n", expr, value));

		/* visit hoisted expressions */
		for (pos = 0; pos < arity; ++pos) {
			/* standard target is predecessor block */
			ir_node    *target     = get_Block_cfgpred_block(block, pos);
			block_info *pred_info  = get_block_info(target);

			/* get phi translated value */
			ir_node *trans_expr  = get_translated(target, expr);
			ir_node *trans_value = identify(trans_expr);
			ir_node *avail       = (ir_node*)ir_valueset_lookup(pred_info->avail_out, trans_value);

			/* get the used expr on this path */

			/* TODO when does this happen? */
			if (avail == NULL)
				continue;

			value = identify(avail);

			/* anticipation border */
			ir_node  *new_target = NULL;
			unsigned  nest_depth = get_loop_depth(get_irn_loop(target));

			/* Either push the hoisted nodes up their path,
			   or try to put them directly into their common dominator. */
#if COMMON_DOM
			/* By using block (instead of target) as initial block,
			   we only allow hoisting into a common block of
			   both predecessor blocks. */
			ir_node *dom = block;
#else
			ir_node *dom = target;
#endif

			while (dom && dom != get_Block_idom(block)) {
				dom = get_Block_idom(dom);
				block_info *dom_info = get_block_info(dom);
				DB((dbg, LEVEL_4, "testing dom %+F\n", dom));

				/* TODO Being in antic_in means hoistable above block,
				   but we need 'hoistable into block'.
				   This could be achieved by a flag for each pair of value sets,
				   being set during antic computation. */

				/* check if available node is still anticipated and clean */
				if (!ir_valueset_lookup(dom_info->antic_in, value)) {
					DB((dbg, LEVEL_4, "%+F not antic in %+F\n", value, dom));
					break;
				}

				nest_depth = get_loop_depth(get_irn_loop(dom));

				/* do not hoist into loops */
				if (get_loop_depth(get_irn_loop(dom)) > nest_depth) {
					DB((dbg, LEVEL_4, "%+F deeper nested\n", dom));
					/* not a suitable location */
					continue;
				}

				/* check if operands die */

				/* check for uses on current path */
				foreach_irn_in(avail, i, pred) {
					ir_node *const pred_value = identify(pred);

					if (dom == NULL)
						break;

					DB((dbg, LEVEL_4, "testing pred %+F\n", pred));

					if (!ir_valueset_lookup(dom_info->avail_out, pred_value)) {
						DB((dbg, LEVEL_4, "pred %+F not available\n", pred));
						dom = NULL;
						break;
					}

					/* check every successor */
					foreach_out_edge(pred, edge) {
						ir_node *succ = get_edge_src_irn(edge);
						DB((dbg, LEVEL_4, "testing succ %+F\n", succ));

						/* check only successors on current path to end */
						if (block_dominates(dom, get_nodes_block(succ))) {
							ir_node *succ_value = identify(succ);

							/* Do we have another user than avail?
							   Then predecessor is not dead after removal of avail. */
							if (succ_value != value) {
								DB((dbg, LEVEL_4, "still used in %+F\n", succ));
								dom = NULL;
								break;
							}
						}
					}
				}
				if (dom)
					new_target = dom;

#if COMMON_DOM
				/* only try common dominator */
				break;
#endif
			}

			/* put node into new target block */
			if (new_target) {
				block_info *target_info = get_block_info(new_target);
				int         nn_arity    = get_irn_arity(avail);
				ir_node   **in          = XMALLOCN(ir_node *, nn_arity);

				DB((dbg, LEVEL_2, "Hoisting %+F into %+F\n", avail, new_target));
				DEBUG_ONLY(inc_stats(gvnpre_stats->hoist_high);)

				foreach_irn_in(avail, i, pred) {
					ir_node *avail_pred = ir_valueset_lookup(target_info->avail_out, identify(pred));
					assert(avail_pred);
					in[i] = avail_pred;
				}
				ir_node *const nn = new_similar_node(avail, new_target, in);
				free(in);

				identify_or_remember(nn);
				/* TODO Nodes are inserted into a dominating block and should
				   be available from this point on. Currently we do not push
				   the availability information through during the walk. */
				ir_valueset_insert(target_info->new_set, value, nn);
				ir_valueset_insert(target_info->avail_out, value, nn);
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

	if (!is_Block(irn)) {
		ir_node *value = identify(irn);

		if (value != NULL) {
			ir_node    *block = get_nodes_block(irn);
			block_info *info  = get_block_info(block);
			ir_node    *expr  = (ir_node *)ir_valueset_lookup(info->avail_out, value);
			DB((dbg, LEVEL_3, "Elim %+F(%+F) avail %+F\n", irn, value, expr));

			if (expr != NULL && expr != irn) {
				elim_pair *p = OALLOC(&env->obst, elim_pair);

				p->old_node = irn;
				p->new_node = expr;
				p->next     = env->pairs;
				env->pairs = p;
				DEBUG_ONLY(inc_stats(gvnpre_stats->replaced);)
			}
		}
	}
}

/**
 * Do all the recorded changes and optimize
 * newly created Phi's.
 *
 * @param pairs  list of elimination pairs
 */
static void eliminate_nodes(elim_pair *pairs, ir_nodeset_t *keeps)
{
	for (elim_pair *p = pairs; p != NULL; p = p->next) {
		/* might be already changed */
		p->new_node = skip_Id(p->new_node);

		DB((dbg, LEVEL_2, "Replacing %+F by %+F\n", p->old_node, p->new_node));

		/* PRE tends to create Phi(self, self, ... , x, self, self, ...)
		 * which we can optimize here */
		if (is_Phi(p->new_node)) {
			ir_node *res = NULL;
			foreach_irn_in(p->new_node, i, pred) {
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
		DBG_OPT_GVN_PRE(p->old_node, p->new_node);

		exchange(p->old_node, p->new_node);
	}

	/* remove keep alive edges of unused mode_M phis */
	ir_node *end = environment->end_node;
	foreach_ir_nodeset(keeps, m_phi, iter) {
		remove_End_keepalive(end, m_phi);
	}
}


/* --------------------------------------------------------
 * GVN PRE pass
 * --------------------------------------------------------
 */

/**
 * Gvn_Pre algorithm.
 *
 * @param irg   the graph
 * @param env   the environment
 */
static void gvn_pre(ir_graph *irg, pre_env *env)
{
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
	unsigned antic_iter = 0;
	env->first_iter = 1;

	env->iteration = 1;
	/* antic_in passes */
	do {
		++antic_iter;
		DB((dbg, LEVEL_2, "= Antic_in Iteration %d ========================\n", antic_iter));
		env->changes = 0;
		irg_walk_blkwise_graph(irg, compute_antic, NULL, env);
		env->first_iter = 0;
		DB((dbg, LEVEL_2, "----------------------------------------------\n"));
		env->iteration ++;
	} while (env->changes != 0 && antic_iter < MAX_ANTIC_ITER);

	DEBUG_ONLY(set_stats(gvnpre_stats->antic_iterations, antic_iter);)

	ir_nodeset_init(env->keeps);
	unsigned insert_iter = 0;
	env->first_iter = 1;
	/* compute redundant expressions */
	do {
		++insert_iter;
		DB((dbg, LEVEL_2, "= Insert Iteration %d ==========================\n", insert_iter));
		env->changes = 0;
		/* TODO topologically top down would be better; fewer iterations. */
		dom_tree_walk_irg(irg, insert_nodes_walker, NULL, env);
		env->first_iter = 0;
		DB((dbg, LEVEL_2, "----------------------------------------------\n"));
	} while (env->changes != 0 && insert_iter < MAX_INSERT_ITER);
	DEBUG_ONLY(set_stats(gvnpre_stats->insert_iterations, insert_iter);)

#if HOIST_HIGH
	/* An attempt to reduce lifetimes by hoisting already hoisted values
	   even higher if their operands die. */
	dom_tree_walk_irg(irg, hoist_high, NULL, NULL);
	/* update avail_out for elimination */
	dom_tree_walk_irg(irg, update_new_set_walker, NULL, env);
#endif

	/* Deactivate edges to prevent intelligent removal of nodes,
	   or else we will get deleted nodes which we try to exchange. */
	edges_deactivate(environment->graph);

	/* eliminate nodes */
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
	pre_env               env;
	ir_nodeset_t          keeps;
	optimization_state_t  state;

	/* bads and unreachables cause too much trouble with dominance,
	   loop info for endless loop detection,
	   no critical edges is PRE precondition
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
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_LOOP_LINK);

	edges_activate(irg);

	environment = &env;
	DEBUG_ONLY(init_stats();)

	/* setup environment */
	env.graph        = irg;
	env.list         = NULL;
	env.start_block  = get_irg_start_block(irg);
	env.end_block    = get_irg_end_block(irg);
	env.end_node     = get_irg_end(irg);
	env.pairs        = NULL;
	env.keeps        = &keeps;
	env.last_idx     = get_irg_last_idx(irg);
	obstack_init(&env.obst);

	/* Detect and set links of infinite loops to non-zero. */
	analyse_loops(irg);

#if OPTIMIZE_NODES
	new_identities(irg);
	env.value_table = irg->value_table;
	irg->value_table = NULL;
#endif

	/* Switch on GCSE. We need it to correctly compute
	   the value of a node, which is independent from
	   its block. */
	set_opt_global_cse(1);
	/* new_identities() */
	if (irg->value_table != NULL)
		del_pset(irg->value_table);
	/* initially assumed nodes in pset are 512 */
	irg->value_table = new_pset(compare_gvn_identities, 512);
#if OPTIMIZE_NODES
	env.gvnpre_values = irg->value_table;
#endif

	/* do GVN-PRE pass */
	gvn_pre(irg, &env);
	DEBUG_ONLY(print_stats();)

	/* clean up: delete all sets */
	for (block_info *block_info = env.list; block_info != NULL; block_info = block_info->next) {
		free_block_info(block_info);
	}

	DEBUG_ONLY(free_stats();)
	ir_nodehashmap_destroy(&value_map);
	obstack_free(&env.obst, NULL);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_LOOP_LINK);

	/* Pin the graph again.
	   This is needed due to the use of set_opt_global_cse(1) */
	set_irg_pinned(irg, op_pin_state_pinned);
	restore_optimization_state(&state);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);

#if OPTIMIZE_NODES
	irg->value_table = env.value_table;
	del_pset(irg->value_table);
	irg->value_table = env.gvnpre_values;
#endif

	/* TODO There seem to be optimizations that try to use the existing
	   value_table. */
	new_identities(irg);

	/* TODO assure nothing else breaks. */
	set_opt_global_cse(0);
	edges_activate(irg);
}
