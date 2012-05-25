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

#include "irgraph_t.h"
#include "irnode_t.h"
#include "iropt_t.h"

/** Additional info we need for every block. */
typedef struct block_info {
	ir_valueset_t     *exp_gen;   /**< contains this blocks clean expressions */
	ir_valueset_t     *avail_out; /**< The Avail_out set for a block. */
	ir_valueset_t     *antic_in;  /**< The Antic_in set for a block. */
	ir_valueset_t     *new_set;   /**< The set of all new values for a block. */
	ir_nodehashmap_t  *trans;     /**< contains translated nodes in block */
	ir_node           *avail;     /**< The get_map(avail, block) result. */
	ir_node           *block;     /**< The Block of the block info. */
	struct block_info *next;      /**< Links all entries, so we can recover the sets easily. */
	int                found;     /**< Non-zero, if avail was found in this block. */
} block_info;

/**
 * A pair of nodes that must be exchanged.
 * We must defer the exchange because our hash-sets cannot
 * find an already replace node else.
 */
typedef struct elim_pair {
	ir_node *old_node;      /**< The old node that will be replaced. */
	ir_node *new_node;      /**< The new node. */
	struct elim_pair *next; /**< Links all entries in a list. */
	int     reason;         /**< The reason for the replacement. */
} elim_pair;

/** The environment for the GVN-PRE algorithm */
typedef struct pre_env {
	struct obstack *obst;        /**< The obstack to allocate on. */
	ir_node        *start_block; /**< The start block of the current graph. */
	ir_node        *end_block;   /**< The end block of the current graph */
	block_info     *list;        /**< Links all block info entries for easier recovery. */
	elim_pair      *pairs;       /**< A list of node pairs that must be eliminated. */
	unsigned        last_idx;    /**< last node index of "old" nodes, all higher indexes are newly created once. */
	char            changes;     /**< Non-zero, if calculation of Antic_in has changed. */
	char            first_iter;  /**< non-zero for first iteration */
} pre_env;

/** The debug module handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/* ----------  Functions for Value sets ---------- */


/**
 * computes dst = dst \/ src for value sets
 *
 * @param dst    the union result set
 * @param src    the source set
 */
static void value_union(ir_valueset_t *dst, ir_valueset_t *src)
{
	ir_valueset_iterator_t  iter;
	ir_node                *value;
	ir_node                *expr;

	foreach_valueset(src, value, expr, iter) {
		/* dominator tree walk; use first available expr as leader */
		ir_valueset_insert(dst, value, expr);
	}
}


/* ----------  Functions for Values ---------- */

/**
 * Remember adds a node e to the GCSE valuetable.
 *
 * @param e  a node representing an expression
 * @return the final value for the expression e
 */
static ir_node *remember(ir_node *e)
{
	ir_node *value;

	if (is_Proj(e)) {
		ir_node *pred   = get_Proj_pred(e);
		ir_node *v_pred = identify_remember(pred);

		if (v_pred != pred) {
			ir_node *proj = new_r_Proj(v_pred, get_irn_mode(e), get_Proj_proj(e));
			value = identify_remember(proj);
			return value;
		}
	}

	value = identify_remember(e);
	return value;
}  /* identify */

/**
 * Identify does a lookup in the GCSE valuetable.
 *
 * @param e  a node representing an expression
 * @return a node representing the value or NULL if no identified
 */
static ir_node *identify(ir_node *e)
{
	return identify_remember(e);
}

/**
 * Returns the block info of a block.
 *
 * @param block  the block
 * @return block info of block
 */
static block_info *get_block_info(ir_node *block)
{
	return (block_info*)get_irn_link(block);
}

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
	info->exp_gen   = ir_valueset_new(16);
	info->avail_out = ir_valueset_new(16);
	info->antic_in  = ir_valueset_new(16);
	/* valueset has much nicer interface */
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
 * Returns non-zero if a node is movable and a possible candidate for PRE.
 *
 * @param n  the node
 * @return non-zero if value is nice
 */
static int is_nice_value(ir_node *n)
{
	ir_mode *mode = get_irn_mode(n);

	if (mode == mode_M)
		return 0;

	if (is_Phi(n))
		return 1;

	while (is_Proj(n))
		n = get_Proj_pred(n);

	/* we may not move pinned nodes */
	if (get_irn_pinned(n) == op_pin_state_pinned)
		return 0;

	if (!mode_is_data(mode)) {
		/* Div and Mod are only nice if they do not use memory. */
		if (! is_Div(n) && ! is_Mod(n))
			return 0;
		if (! is_NoMem(get_memop_mem(n)))
			return 0;
	}
	return 1;
}  /* is_nice_value */


#ifdef DEBUG_libfirm
/**
 * Dump a value set.
 *
 * @param set    the set to dump
 * @param txt    a text to describe the set
 * @param block  the owner block of the set
 */
static void dump_value_set(ir_valueset_t *set, const char *txt, ir_node *block)
{
	ir_valueset_iterator_t  iter;
	ir_node                *value;
	ir_node                *expr;
	int                     i     = 0;

	DB((dbg, LEVEL_2, "%s(%+F) = {\n", txt, block));
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
	block_info *bl_info;

	for (bl_info = list; bl_info != NULL; bl_info = bl_info->next) {
		dump_value_set(bl_info->exp_gen, "[Exp_gen]", bl_info->block);
	}
}

#else
#define dump_value_set(set, txt, block)
#define dump_all_expgen_sets(list)
#endif /* DEBUG_libfirm */

/**
 * Gets result of nodes phi translation into block.
 *
 * @param node   the node
 * @param block  the target block
 *
 * @return a phi translation of node node into block block or NULL
 */
static ir_node *get_translated(ir_node *node, ir_node *block)
{
	block_info *bi;
	ir_node    *trans;

	if (is_irn_constlike(node))
		return node;

	bi    = get_block_info(block);
	trans = ir_nodehashmap_get(ir_node, bi->trans, node);
	return trans;
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
static void set_translated(ir_node *node, ir_node *succ, int pos, ir_node *trans)
{
	ir_node    *pred = get_Block_cfgpred_block(succ, pos);
	block_info *bi   = get_block_info(pred);

	ir_nodehashmap_insert(bi->trans, node, trans);
}

/**
 * Checks if a node node is clean in block block for use in antic_in.
 *
 * A clean node in block block can be hoisted above block block.
 * A node is not clean if its value is killed in block block.
 * The node can still be hoisted into block block.
 *
 * @param n      the phi translated or not translated node
 * @param block  the block
 * @return non-zero value for clean node
 */
static int is_clean_in_block_antic(ir_node *node, ir_node *block)
{
	int i;

	if (get_irn_mode(node) == mode_M)
		return 0;

	/* a phi only has predecessors in other blocks */
	if (is_Phi(node))
		return 1;

	/* constants are in start block */
	if (is_irn_constlike(node))
		return 1;

	/* what we really want to check
	   Only for node is translated case; other are clean anyway */
	if (! is_nice_value(node)) {
		return 0;
	}

	/* cleanliness depends on nodes predecessors
	   At least if node is translated. */
	for (i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);
		ir_node *trans;
		ir_node *value;

		if (is_irn_constlike(pred))
			continue;

		/* exp_gen only contains clean nodes */
		if (ir_valueset_lookup(get_block_info(block)->exp_gen, pred))
			continue;

		/* block of pred strictly dominates target block. pred irrelevant. */
		if (block_strictly_dominates(get_nodes_block(pred), block))
			continue;

		/* --- pred neither in block, nor dominating -- */

		/* This pred is in antic_in and such clean.
		   Not every clean pred is in antic_in though.
		   Predecessor might be translated or not */
		value = identify(pred);
		if (ir_valueset_lookup(get_block_info(block)->antic_in, value))
			continue;

		/* This check is not redundant for translated nodes;
		   non translated ones are already nice. */
		if (! is_nice_value(pred)) {
			DB((dbg, LEVEL_5, "unclean %+F because pred %+F not nice\n", node, pred));
			return 0;
		}

		/* predecessor is not translated. This is legal if
		   predecessor is dominating or in target block (already checked). */
		trans = get_translated(pred, block);
		if (trans == NULL) {
			DB((dbg, LEVEL_5, "unclean %+F because pred %+F unclean (not translated)\n", node, pred));
			return 0;

		} else {
			/* Node and predecessor are translated, but is pred clean?
			   The value of the translated predecessor has to be in antic_in. */
			ir_node *value = identify(trans);
			if (! ir_valueset_lookup(get_block_info(block)->antic_in, value)) {
				DB((dbg, LEVEL_5, "unclean %+F because pred %+F value %+F not antic\n", node, pred, value));
				return 0;
			}
		}

		assert(0 && "should have been catched");
	}

	/* clean */
	return 1;
}  /* is_clean_in_block */

/**
 * Checks if a node n is clean in block block for exp_gen.
 *
 * @param n      the node
 * @param block  the block
 * @return non-zero value for clean node
 */
static int is_clean_in_block_expgen(ir_node *n, ir_node *block)
{
	int i;

	if (get_irn_mode(n) == mode_M)
		return 0;

	if (is_Phi(n))
		return 1;

	if (! is_nice_value(n))
		return 0;

	for (i = get_irn_arity(n) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(n, i);

		/* sufficient for exp_gen because block is always block of node */
		if (get_nodes_block(pred) != block)
			continue;

		/* pred is in block,
		   so it needs to be clean (already in exp_gen) */
		if (! get_irn_link(pred)) {
			DB((dbg, LEVEL_5, "unclean %+F because pred %+F unclean\n", n, pred));
			return 0;
		} else {
			continue;
		}
	}
	return 1;
}  /* is_clean_in_block */

/**
 * Does blocklocal common subexpression elimination (CSE).
 *
 * @param irn   the node
 * @param ctx   the environment
 */
static void cse_walker(ir_node *irn, void *ctx)
{
	ir_node *opt = identify_remember(irn);
	(void) ctx;

	if (opt != irn) {
		DB((dbg, LEVEL_5, "CSE %+F to %+F\n", irn, opt));
		exchange(irn, opt);
	}
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
 * Topological walker puts nodes in top-down topological order into exp_gen set.
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

	/* GVN step: remember the value */
	value = remember(irn);

	/* no need to put constants into the sets: they are always redundant */
	if (! is_nice_value(irn) || is_irn_constlike(irn))
		return;

	/* Do not put mode_T nodes info the sets, or PhiT will be created
	  (which are not allowed in Firm). Instead, put the Proj's here only. */
	if (get_irn_mode(irn) == mode_T)
		return;

	block = get_nodes_block(irn);
	info  = get_block_info(block);

	if (is_clean_in_block_expgen(irn, block)) {
		/* two expressions with same value in block;
		   should have been fixed by CSE pass */
		assert(get_nodes_block(irn) == block &&
		    (! ir_valueset_lookup(info->exp_gen, value)));

		DB((dbg, LEVEL_5, "%+F clean in block %+F\n", irn, block));

		ir_valueset_insert(info->exp_gen, value, irn);
		/* flag irn as clean*/
		set_irn_link(irn, irn);
	} else {
		/* flag irn as not clean */
		set_irn_link(irn, NULL);
	}
}

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
	pre_env    *env       = (pre_env*)ctx;
	block_info *dom_info;
	block_info *info      = get_block_info(block);
	ir_node    *dom_block;

	/* filter blocks from topological walker */
	if (! is_Block(block))
		return;

	if (block == env->end_block)
		return;

	/* First, add all nodes from the immediate dominator.
	   This ensures that avail_out contains the leader.
	   The start block has no immediate dominator. */
	if (block != env->start_block) {
		dom_block = get_Block_idom(block);
		assert(is_Block(dom_block));
		dom_info = get_block_info(dom_block);

		value_union(info->avail_out, dom_info->avail_out);
	}
	/* Second, add values from exp_gen. */
	value_union(info->avail_out, info->exp_gen);

	dump_value_set(info->avail_out, "Avail_out", block);
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
static ir_node *phi_translate(ir_node *node, ir_node *block, int pos)
{
	int       i;
	int       arity;
	ir_node **in;
	ir_node  *nn;
	ir_node  *target_block;

	if (is_Phi(node)) {
		if (get_nodes_block(node) == block) {
			/* a Phi inside target block */
			return get_Phi_pred(node, pos);
		}
		/* already outside */
		return node;
	}

	arity = get_irn_arity(node);
	in    = XMALLOCN(ir_node *, arity);

	for (i = 0; i < arity; ++i) {
		ir_node *pred       = get_irn_n(node, i);
		ir_node *pred_block = get_Block_cfgpred_block(block,pos);
		ir_node *trans      = get_translated(pred, pred_block);

		/* if node is topologically first in block then
	       there is no translated predecessor.
	       We do not check cleanliness here, so pred might be not clean. */
		if (trans == NULL)
			in[i] = pred;
		else
			in[i] = trans;
	}

	target_block = get_Block_cfgpred_block(block, pos);
	if (is_Proj(node)) {
		/* Projections are the sole case where we have to ensure
		   that they are in the same block as their tuple node. */
		target_block = get_nodes_block(in[0]);
	}

	nn = new_ir_node(
		get_irn_dbg_info(node),
		get_irn_irg(node),
		target_block,
		get_irn_op(node),
		get_irn_mode(node),
		arity,
		in);
	free(in);
	/* We need the attribute copy here, because the Hash value of a
	   node might depend on that. */
	copy_node_attr(get_irn_irg(node), node, nn);
	DB((dbg, LEVEL_5, "New node %+F in %+F origin %+F\n", nn, get_Block_cfgpred_block(block, pos), node));


	nn = optimize_node(nn);
	DB((dbg, LEVEL_5, "New GCSE-optimized node %+F origin %+F\n", nn, node));

	/* During the insert phase we need to compare the global value numbers
	   of blocks that do not dominate each other. 'Blocksafe' GCSE requires
	   the two equivalent nodes to be in blocks that dominate each other.
	   (see identities_cmp() in iropt.c)
	   If we do not translate a node into the predecessor block, their values
	   will not be considered equivalent. (we are at a merging block.)
	   So we have to translate a node into its predecessor block.
	   If we switched off blocksafety we will find matching values that are
	   not dominating (in loops) which we cannot use.

	   Also, blocksafe GCSE does not kill nn even if its value is already
	   present in the successor because the predecessor blocks do not dominate.
	   This is required for antic_in.

	   The nodes produced here are not necessarily in the designated block.
	   They are used to determine the value of node node.
	   If we use them for hoisting, we need to make sure that they are in the
	   designated block. fix_translated() does this job. */

	return nn;
}  /* phi_translate */

/**
 * Block-walker, computes Antic_in(block).
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

	/* filter blocks from topological walker */
	if (! is_Block(block))
		return;

	/* no need for computations in start block */
	if (block == env->start_block)
		return;

	/* the end block has no successor */
	if (block == env->end_block)
		return;

	info = get_block_info(block);
	size = ir_valueset_size(info->antic_in);

	/* This step puts all generated expression from the
	   current block into antic_in.
	   This is needs to be done in the first iteration only. */
	if (env->first_iter) {
		foreach_valueset(info->exp_gen, value, expr, iter) {
			/* We will have phi nodes in antic in.
			   This should prevent special cases in several places. */
			ir_valueset_insert(info->antic_in, value, expr);
		}
	}

	/* TODO handle endless loops. */

	int n_succ = get_Block_n_cfg_outs(block);
	if (n_succ == 1) {
		int pos = -1;

		/* find blocks position in succ's block predecessors */
		succ = get_Block_cfg_out(block, 0);
		pos  = get_Block_cfgpred_pos(succ, block);
		assert(pos >= 0);

		succ_info = get_block_info(succ);
		/* translate into list: we cannot insert into a set we iterate
		 * and succ might be equal to block for endless loops */
		foreach_valueset(succ_info->antic_in, value, expr, iter) {
			ir_node *trans;
			ir_node *newval;

			DB((dbg, LEVEL_5, "Begin phi translate antic: expr %+F from %+F to %d\n", expr, succ, pos));

			/* TODO if successor block has 1 predecessor we need no phi translation.
			   But the clean_in_block check is still needed! */
			/* TODO phi translation and clean in block are overlapping,
			   because phi trans perhaps should know in advance if predecessors are clean. */
			trans = phi_translate(expr, succ, pos);
			newval = remember(trans);

			DB((dbg, LEVEL_5, "----> phi translate antic: expr %+F from %+F to %d is trans %+F\n", expr, succ, pos, trans));

			if (is_clean_in_block_antic(trans, block)) {
				if (! is_irn_constlike(trans)) {
					ir_valueset_insert(info->antic_in, newval, trans);
				}
				DB((dbg, LEVEL_5, " translated %+F clean in %+F\n", trans, block));

			} else {
				DB((dbg, LEVEL_5, " translated %+F not clean in %+F\n", trans, block));
			}

			/* We have to set translated anyway
			   because expr might still be hoisted _into_ block. */
			set_translated(expr, succ, pos, trans);

			DB((dbg, LEVEL_5, "- end: expr %+F -----\n\n", expr));
		}

	} else if (n_succ > 1) {
		ir_node    *succ0;
		block_info *succ0_info;
		int         i;
		int         common     = 1;

		/* Select a successor to compute the disjoint of all nodes
		   sets, it might be useful to select the block with the
		   smallest number of nodes. For simplicity we choose the
		   first one. */
		succ0      = get_Block_cfg_out(block, 0);
		succ0_info = get_block_info(succ0);

		foreach_valueset(succ0_info->antic_in, value, expr, iter) {
			/* we need the disjoint */
			for (i = 1; i < n_succ; ++i) {
				ir_node    *succ      = get_Block_cfg_out(block, i);
				block_info *succ_info = get_block_info(succ);

				if (ir_valueset_lookup(succ_info->antic_in, value) == NULL) {
					common = 0;
					break;
				}
			}

			/* we found a value that is common in all Antic_in(succ(b)),
			   put it in Antic_in(b) if the value is not already represented. */
			if (common && is_clean_in_block_antic(expr, block)) {
				ir_valueset_insert(info->antic_in, value, expr);
			}
			set_translated(expr, succ0, 0, expr);

		}
	}

	dump_value_set(info->antic_in, "Antic_in", block);
	if (size != ir_valueset_size(info->antic_in)) {
		env->changes |= 1;
	}

}  /* compute_antic */

/**
 * Finds if the value of expr is a partially redundant value in block.
 *
 * @param block   the block
 * @param expr    the expression
 *
 * @return mode of the expression if it is partially redundant else NULL
 */
static ir_mode *find_partially_redundant(ir_node *block, ir_node *expr)
{
	ir_node *first_avail         = NULL;
	int      pos;
	int      arity               = get_irn_arity(block);
	int      fully_redundant     = 1;
	int      partially_redundant = 0;
	ir_mode *mode                = NULL;

	DB((dbg, LEVEL_3, "Examine expr %+F of %+F\n", expr, block));

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

		trans_expr = get_translated(expr, get_Block_cfgpred_block(block,pos));
		DB((dbg, LEVEL_2, "expr %+F trans @ %d is translated %+F\n", expr, pos, trans_expr));
		/* exp in antic in, so pred is clean
		   uncover when it is not */
		assert(trans_expr);

		trans_value = identify(trans_expr);
		DB((dbg, LEVEL_2, "trans_value %+F\n", trans_value));
		assert(trans_value);

		pred_info  = get_block_info(pred_block);
		avail_expr = (ir_node*)ir_valueset_lookup(pred_info->avail_out, trans_value);
		DB((dbg, LEVEL_2, "avail_expr %+F\n", avail_expr));

		if (avail_expr == NULL) {
			/* expr not available */
			pred_info->avail = expr;
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
		}  /* if */
	}  /* for */

	/* If it is not the same value already existing along every predecessor
           and it is defined by some predecessor then it is partially redundant. */
	if (! fully_redundant && partially_redundant)
		return mode;

	return NULL;
}

/**
 * Copies node and its predecessors to a block that dominates the target block.
 *
 * @param node     the node
 * @param target   the target block
 *
 * @return copy of node node dominating target block
 */
static ir_node *fix_translation(ir_node *node, ir_node *target)
{
	ir_node  *nn;
	int       i;
	int       arity;
	ir_node **ins;

	DB((dbg, LEVEL_1, "Fix_translation %+F into %+F\n", node, target));

	/* identifies unreachable blocks using domination */
	if (get_Block_dom_depth(get_nodes_block(node)) < 0 ||
	    (get_Block_dom_depth(target) < 0))
		return new_r_Bad(get_irn_irg(node), get_irn_mode(node));

	/* Walk upwards until the node dominates its use in target block.
	   Precondition is that the node is clean. */
	if (block_dominates(get_nodes_block(node), target))
		return node;

	DB((dbg, LEVEL_1, "Fix_translation%+F of node %+F does not dominate target %+F\n", get_nodes_block(node), node, target));

	arity = get_irn_arity(node);
	ins   = XMALLOCN(ir_node*, arity);

	for (i = arity - 1; i >= 0; --i) {
		ir_node *pred  = get_irn_n(node, i);
		ir_node *fixed = fix_translation(pred, target);

		DB((dbg, LEVEL_1, "Fixed %+F to %+F for node %+F\n", pred, fixed, node));
		ins[i] = fixed;
	}

	nn = new_ir_node(
		get_irn_dbg_info(node),
		get_irn_irg(node),
		target,
		get_irn_op(node),
		get_irn_mode(node),
		arity,
		ins);
	free(ins);
	copy_node_attr(get_irn_irg(node), node, nn);

	DB((dbg, LEVEL_1, "New fixed node %+F from translated %+F. target %+F\n", nn, node, target));

	nn = optimize_node(nn);
	remember(nn);
	return nn;
} /* fix_translation */

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

	dump_value_set(idom_info->new_set, "[New Set]", idom);
	foreach_valueset(idom_info->new_set, value, expr, iter) {
		ir_valueset_insert(curr_info->new_set, value, expr);
		updated |= ir_valueset_replace(curr_info->avail_out, value, expr);
	}
	if (updated) {
		dump_value_set(curr_info->avail_out, "Updated [Avail_out]", block);
	}
} /* update_new_set */

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
	pre_env                *env       = (pre_env*)ctx;
	ir_node                *value;
	ir_node                *expr;
	ir_node                *idom;
	block_info             *curr_info;
	int                     pos;
	int                     arity     = get_irn_arity(block);
	ir_valueset_iterator_t  iter;

	/* filter only blocks */
	if (! is_Block(block))
		return;

	/* ensure that even the start block has a new_set */
	curr_info = get_block_info(block);
	if (curr_info->new_set)
		ir_valueset_del(curr_info->new_set);
	curr_info->new_set = ir_valueset_new(16);

	if (block == env->start_block)
		return;

	DB((dbg, LEVEL_2, "Insert operation of %+F\n", block));

	idom = get_Block_idom(block);
	update_new_set(block, idom);

	/* process only merge blocks */
	if (arity < 2)
		return;

	/* for each antic_in */
	foreach_valueset(curr_info->antic_in, value, expr, iter) {
		ir_mode  *mode;
		ir_node  *phi;
		ir_node  *phi_value;
		ir_node **phi_in;

		/* filter phi nodes from antic in */
		if (is_Phi(expr))
			continue;

		/* A value computed in the dominator is totally redundant.
		   Hence we have nothing to insert. */
		if (ir_valueset_lookup(get_block_info(idom)->avail_out, value)) {
			DB((dbg, LEVEL_2, "Fully redundant expr %+F value %+F\n", expr, value));
			continue;
		}

		mode = find_partially_redundant(block, expr);
		if (mode == NULL)
			continue;

		DB((dbg, LEVEL_1, "Partial redundant %+F from block %+F found\n", expr, block));

		phi_in = XMALLOCN(ir_node *, arity);

		/* for all predecessor blocks */
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

			/* ignore blocks that already have the expression */
			if (! pred_info->found) {
				ir_node *translated  = get_translated(expr, pred_block);
				ir_node *trans_value;

				/* make sure translated dominates its use */
				translated = fix_translation(translated, pred_block);
				DB((dbg, LEVEL_3, "Use translated %+F in %+F because expr %+F not available\n", translated, pred_block, expr));

				/* make the new node available  */
				trans_value = remember(translated);
				ir_valueset_insert(pred_info->avail_out, trans_value, translated);
				phi_in[pos] = translated;
				DB((dbg, LEVEL_5, "phi_in %+F\n", translated));
			} else {
				phi_in[pos] = pred_info->avail;
				DB((dbg, LEVEL_5, "phi_in %+F\n", pred_info->avail));
			}

		}  /* for */

		phi = new_r_Phi(block, arity, phi_in, mode);
		free(phi_in);
		DB((dbg, LEVEL_1, "New %+F for redundant %+F created\n", phi, expr));

		phi_value = remember(phi);

		/* this 'global' value is now available through the new phi */
		ir_valueset_replace(curr_info->avail_out, value, phi);
		/* add this phi and its 'blocklocal' value */
		ir_valueset_insert(curr_info->avail_out, phi_value, phi);

		ir_valueset_insert(curr_info->new_set, value, phi);
		ir_valueset_insert(curr_info->new_set, phi_value, phi);

		/* remove from antic_in to prevent reprocessing */
		ir_valueset_remove_iterator(curr_info->antic_in, &iter);

		env->changes |= 1;

  }  /* node_set_foreach */
}  /* insert_nodes */

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
		block_info *bl    = get_block_info(block);
		ir_node    *value = identify(irn);

		if (value != NULL) {
			ir_node *expr = (ir_node*)ir_valueset_lookup(bl->avail_out, value);

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
static void eliminate_nodes(elim_pair *pairs)
{
	elim_pair *p;

	for (p = pairs; p != NULL; p = p->next) {
		/* might be already changed */
		p->new_node = skip_Id(p->new_node);

		DB((dbg, LEVEL_1, "Replacing %+F by %+F\n", p->old_node, p->new_node));
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
}  /* eliminate_nodes */

/**
 * Gvn_Pre pass for graph irg.
 *
 * @param irg   the graph
 */
void do_gvn_pre(ir_graph *irg)
{
	struct obstack        obst;
	pre_env               a_env;
	optimization_state_t  state;
	block_info           *bl_info;
	unsigned              antic_iter;
	unsigned              insert_iter;

	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_CONSISTENT_OUTS
		| IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
		| IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		| IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE);

	/* register a debug mask */
	FIRM_DBG_REGISTER(dbg, "firm.opt.gvn_pre");
	/* edges will crash if enabled due to our allocate on other obstack trick */
	edges_deactivate(irg);

	save_optimization_state(&state);

	/* CSE pass
	 * If there are two nodes with the same value in one block,
	 * the exp_gen valueset can only contain one of them. */
	set_opt_global_cse(0);
	new_identities(irg);
	irg_walk_graph(irg, NULL, cse_walker, &a_env);

	DB((dbg, LEVEL_1, "Doing GVN-PRE for %+F\n", irg));

	/* Switch on GCSE. We need it to correctly compute
	   the value of a node, which is independent from
	   its block. */
	set_opt_global_cse(1);
	new_identities(irg);

	/* setup environment */
	obstack_init(&obst);
	a_env.obst        = &obst;
	a_env.list        = NULL;
	a_env.start_block = get_irg_start_block(irg);
	a_env.end_block   = get_irg_end_block(irg);
	a_env.pairs       = NULL;

	/* allocate block info */
	irg_walk_blkwise_graph(irg, block_info_walker, NULL, &a_env);

	/* generate exp_gen */
	irg_walk_blkwise_dom_top_down(irg, NULL, topo_walker, &a_env);
	dump_all_expgen_sets(a_env.list);

	/* compute the avail_out sets for all blocks */
	dom_tree_walk_irg(irg, compute_avail_top_down, NULL, &a_env);

	/* compute the anticipated value sets for all blocks */
	antic_iter       = 0;
	a_env.first_iter = 1;

	/* antic_in passes */
	do {
		DB((dbg, LEVEL_1, "= Antic_in Iteration %d ========================\n",
		    ++antic_iter));
		a_env.changes = 0;
		irg_walk_blkwise_graph(irg, compute_antic, NULL, &a_env);
		a_env.first_iter = 0;
		DB((dbg, LEVEL_1, "----------------------------------------------\n"));
	/* TODO bad endless loop protection */
	} while (a_env.changes != 0 && antic_iter < 40);

	/* compute redundant expressions */
	insert_iter = 0;
	a_env.last_idx = get_irg_last_idx(irg);
	do {
		++insert_iter;
		DB((dbg, LEVEL_1, "= Insert Iteration %d ==========================\n", insert_iter));
		a_env.changes = 0;
		/* TODO topologically top down would be better; fewer iterations. */
		dom_tree_walk_irg(irg, insert_nodes, NULL, &a_env);
		DB((dbg, LEVEL_1, "----------------------------------------------\n"));
	} while (a_env.changes != 0);

	/* last step: eliminate nodes */
	irg_walk_graph(irg, NULL, eliminate, &a_env);
	eliminate_nodes(a_env.pairs);

	/* clean up: delete all sets */
	for (bl_info = a_env.list; bl_info != NULL; bl_info = bl_info->next) {
		ir_valueset_del(bl_info->exp_gen);
		ir_valueset_del(bl_info->avail_out);
		ir_valueset_del(bl_info->antic_in);
		ir_nodehashmap_destroy(bl_info->trans);
		free(bl_info->trans);
		if (bl_info->new_set)
			ir_valueset_del(bl_info->new_set);
	}

	obstack_free(&obst, NULL);

	/* pin the graph again.
	   This is needed due to the use of set_opt_global_cse(1) */
	set_irg_pinned(irg, op_pin_state_pinned);
	restore_optimization_state(&state);

	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
}

/* Creates an ir_graph pass for do_gvn_pre. */
ir_graph_pass_t *do_gvn_pre_pass(const char *name)
{
	return def_graph_pass(name ? name : "gvn_pre", do_gvn_pre);
}
