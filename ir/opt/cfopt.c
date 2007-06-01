/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief   Control flow optimizations.
 * @author  Goetz Lindenmaier, Michael Beck, Sebastian Hack
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "iroptimize.h"

#include <assert.h>

#include "plist.h"
#include "xmalloc.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"

#include "ircons.h"
#include "iropt_t.h"
#include "irgwalk.h"
#include "irgmod.h"
#include "irdump.h"
#include "irvrfy.h"
#include "iredges.h"

#include "array.h"

#include "irouts.h"
#include "irbackedge_t.h"

#include "irflag_t.h"
#include "firmstat.h"

#include "iropt_dbg.h"

/*------------------------------------------------------------------*/
/* Control flow optimization.                                       */
/*                                                                  */
/* Removes Bad control flow predecessors and empty blocks.  A block */
/* is empty if it contains only a Jmp node.                         */
/* Blocks can only be removed if they are not needed for the        */
/* semantics of Phi nodes.                                          */
/*------------------------------------------------------------------*/

/**
 * Block walker, replacing binary Conds that jumps twice into the same block
 * by a simple Jmp.
 * E.g.
 * @verbatim
 *               Cond                     Jmp  Bad
 *             /       \                   |   /
 *       ProjX True   ProjX False  ==>     |  /
 *             \       /                   | /
 *               Block                    Block
 * @endverbatim
 *
 * Such pattern are the result of if-conversion.
 *
 * Note that the simple case that Block has only these two
 * predecessors are already handled in equivalent_node_Block().
 */
static void remove_senseless_conds(ir_node *bl, void *env) {
	int i, j;
	int n = get_Block_n_cfgpreds(bl);
	int *changed = env;

	assert(is_Block(bl));

	for (i = 0; i < n; ++i) {
		ir_node *pred_i = get_Block_cfgpred(bl, i);
		ir_node *cond_i = skip_Proj(pred_i);

		/* binary Cond */
		if (is_Cond(cond_i) && get_irn_mode(get_Cond_selector(cond_i)) == mode_b) {

			for (j = i + 1; j < n; ++j) {
				ir_node *pred_j = get_Block_cfgpred(bl, j);
				ir_node *cond_j = skip_Proj(pred_j);

				if (cond_j == cond_i) {
					ir_node *jmp = new_r_Jmp(current_ir_graph, get_nodes_block(cond_i));
					set_irn_n(bl, i, jmp);
					set_irn_n(bl, j, new_Bad());

					DBG_OPT_IFSIM2(cond_i, jmp);
					*changed = 1;
					break;
				}
			}
		}
	}
}

/** An environment for merge_blocks and collect nodes. */
typedef struct _merge_env {
	int changed;    /**< Set if the graph was changed. */
	int phis_moved; /**< Set if Phi nodes were moved. */
	plist_t *list;  /**< Helper list for all found Switch Conds. */
} merge_env;

/**
 * Removes Tuples from Block control flow predecessors.
 * Optimizes blocks with equivalent_node().  This is tricky,
 * as we want to avoid nodes that have as block predecessor Bads.
 * Therefore we also optimize at control flow operations, depending
 * how we first reach the Block.
 */
static void merge_blocks(ir_node *node, void *ctx) {
	int i;
	ir_node *new_block;
	merge_env *env = ctx;

	/* clear the link field for ALL nodes first */
	set_irn_link(node, NULL);

	if (is_Block(node)) {
		/* Remove Tuples */
		for (i = get_Block_n_cfgpreds(node) - 1; i >= 0; --i) {
			ir_node *pred = get_Block_cfgpred(node, i);
			ir_node *skipped = skip_Tuple(pred);
			if (pred != skipped) {
				set_Block_cfgpred(node, i, skipped);
				env->changed = 1;
			}
		}

		/* see below */
		new_block = equivalent_node(node);
		if (new_block != node && ! is_Block_dead(new_block)) {
			exchange(node, new_block);
			env->changed = 1;
		}

	} else if (get_opt_optimize() && (get_irn_mode(node) == mode_X)) {
		/* We will soon visit a block.  Optimize it before visiting! */
		ir_node *b = get_nodes_block(skip_Proj(node));

		if (!is_Block_dead(b)) {
			new_block = equivalent_node(b);

			while (irn_not_visited(b) && (!is_Block_dead(new_block)) && (new_block != b)) {
				/* We would have to run gigo() if new is bad, so we
				   promote it directly below. Nevertheless, we sometimes reach a block
				   the first time through a dataflow node.  In this case we optimized the
				   block as such and have to promote the Bad here. */
				assert((get_opt_control_flow_straightening() ||
					get_opt_control_flow_weak_simplification()) &&
					("strange flag setting"));
				exchange(b, new_block);
				env->changed = 1;
				b = new_block;
				new_block = equivalent_node(b);
			}

			/* normally, we would create a Bad block here, but this must be
			 * prevented, so just set it's cf to Bad.
			 */
			if (is_Block_dead(new_block)) {
				exchange(node, new_Bad());
				env->changed = 1;
			}
		}
	}
}


/**
 * Block walker removing control flow from dead block by
 * inspecting dominance info.
 * Do not replace blocks by Bad.  This optimization shall
 * ensure, that all Bad control flow predecessors are
 * removed, and no new other Bads are introduced.
 *
 * Must be run in the post walker.
 */
static void remove_dead_block_cf(ir_node *block, void *env) {
	int i;
	int *changed = env;

	/* check block predecessors and turn control flow into bad */
	for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
		ir_node *pred_X = get_Block_cfgpred(block, i);

		if (! is_Bad(pred_X)) {
			ir_node *pred_bl = get_nodes_block(skip_Proj(pred_X));

			if (is_Block_dead(pred_bl) || (get_Block_dom_depth(pred_bl) < 0)) {
				set_Block_dead(pred_bl);
				exchange(pred_X, new_Bad());
				*changed = 1;
			}
		}
	}
}

/**
 * Collects all Phi nodes in link list of Block.
 * Marks all blocks "block_visited" if they contain a node other
 * than Jmp (and Proj).
 * Links all Proj nodes to their predecessors.
 * Collects all switch-Conds in a list.
 */
static void collect_nodes(ir_node *n, void *ctx) {
	ir_op *op = get_irn_op(n);
	merge_env *env = ctx;

	if (op != op_Block) {
		ir_node *b  = get_nodes_block(n);

		if (op == op_Phi) {
			/* Collect Phi nodes to compact ins along with block's ins. */
			set_irn_link(n, get_irn_link(b));
			set_irn_link(b, n);
		} else if (op != op_Jmp && !is_Bad(b)) {  /* Check for non empty block. */
			mark_Block_block_visited(b);

			if (op == op_Proj) {               /* link Proj nodes */
				ir_node *pred = get_Proj_pred(n);

				set_irn_link(n, get_irn_link(pred));
				set_irn_link(pred, n);
			} else if (op == op_Cond) {
				ir_node *sel = get_Cond_selector(n);
				if (mode_is_int(get_irn_mode(sel))) {
					/* found a switch-Cond, collect */
					plist_insert_back(env->list, n);
				}
			}
		}
	}
}

/** Returns true if pred is predecessor of block. */
static int is_pred_of(ir_node *pred, ir_node *b) {
	int i, n;

	for (i = 0, n = get_Block_n_cfgpreds(b); i < n; ++i) {
		ir_node *b_pred = get_Block_cfgpred_block(b, i);
		if (b_pred == pred) return 1;
	}
	return 0;
}

/** Test whether we can optimize away pred block pos of b.
 *
 *  @param  b    A block node.
 *  @param  pos  The position of the predecessor block to judge about.
 *
 *  @returns     The number of predecessors
 *
 *  The test is rather tricky.
 *
 *  The situation is something like the following:
 *  @verbatim
 *                 if-block
 *                  /   \
 *              then-b  else-b
 *                  \   /
 *                    b
 *  @endverbatim
 *
 *  b merges the control flow of an if-then-else.  We may not remove
 *  the 'then' _and_ the 'else' block of an 'if' if there is a Phi
 *  node in b, even if both are empty.  The destruction of this Phi
 *  requires that a copy is added before the merge.  We have to
 *  keep one of the case blocks to place the copies in.
 *
 *  To perform the test for pos, we must regard predecessors before pos
 *  as already removed.
 **/
static int test_whether_dispensable(ir_node *b, int pos) {
	int i, j, n_preds = 1;
	ir_node *pred = get_Block_cfgpred_block(b, pos);

	/* Bad blocks will be optimized away, so we don't need space for them */
	if (is_Block_dead(pred))
		return 0;

	if (get_Block_block_visited(pred) + 1
		< get_irg_block_visited(current_ir_graph)) {

		if (!get_opt_optimize() || !get_opt_control_flow_strong_simplification()) {
			/* Mark block so that is will not be removed: optimization is turned off. */
			set_Block_block_visited(pred, get_irg_block_visited(current_ir_graph)-1);
			return 1;
		}

		/* Seems to be empty. At least we detected this in collect_nodes. */
		if (!get_irn_link(b)) {
			/* There are no Phi nodes ==> all predecessors are dispensable. */
			n_preds = get_Block_n_cfgpreds(pred);
		} else {
			/* b's pred blocks and pred's pred blocks must be pairwise disjunct.
			   Handle all pred blocks with preds < pos as if they were already removed. */
			for (i = 0; i < pos; i++) {
				ir_node *b_pred = get_Block_cfgpred_block(b, i);
				if (! is_Block_dead(b_pred) &&
					get_Block_block_visited(b_pred) + 1
					< get_irg_block_visited(current_ir_graph)) {
					for (j = 0; j < get_Block_n_cfgpreds(b_pred); j++) {
						ir_node *b_pred_pred = get_Block_cfgpred_block(b_pred, j);
						if (is_pred_of(b_pred_pred, pred))
							goto non_dispensable;
					}
				} else {
					if (is_pred_of(b_pred, pred))
						goto non_dispensable;
				}
			}
			for (i = pos +1; i < get_Block_n_cfgpreds(b); i++) {
				ir_node *b_pred = get_Block_cfgpred_block(b, i);
				if (is_pred_of(b_pred, pred))
					goto non_dispensable;
			}
			/* if we get here, the block is dispensable */
			n_preds = get_Block_n_cfgpreds(pred);
		}
	}

	return n_preds;

non_dispensable:
	set_Block_block_visited(pred, get_irg_block_visited(current_ir_graph)-1);
	return 1;
}

/**
 * This method removed Bad cf predecessors from Blocks and Phis, and removes
 * empty blocks.  A block is empty if it only contains Phi and Jmp nodes.
 *
 * We first adapt Phi nodes, then Block nodes, as we need the old ins
 * of the Block to adapt the Phi nodes.  We do this by computing new
 * in arrays, and then replacing the old ones.  So far we compute new in arrays
 * for all nodes, not regarding whether there is a possibility for optimization.
 *
 * For each predecessor p of a Block b there are three cases:
 *  -1. The predecessor p is a Bad node:  just skip it.  The in array of b shrinks by one.
 *  -2. The predecessor p is empty.  Remove p.  All predecessors of p are now
 *      predecessors of b.
 *  -3. The predecessor p is a block containing useful code.  Just keep p as is.
 *
 * For Phi nodes f we have to check the conditions at the Block of f.
 * For cases 1 and 3 we proceed as for Blocks.  For case 2 we can have two
 * cases:
 *  -2a: The old predecessor of the Phi f is a Phi pred_f IN THE BLOCK REMOVED.  In this
 *      case we proceed as for blocks. We remove pred_f.  All
 *      predecessors of pred_f now are predecessors of f.
 *  -2b: The old predecessor of f is NOT in the block removed. It might be a Phi, too.
 *      We have to replicate f for each predecessor of the removed block. Or, with
 *      other words, the removed predecessor block has exactly one predecessor.
 *
 * Further there is a special case for self referencing blocks:
 * @verbatim
 *
 *    then_b     else_b                              then_b  else_b
 *       \      /                                      \      /
 *        \    /                                        |    /
 *        pred_b                                        |   /
 *         |   ____                                     |  /  ____
 *         |  |    |                                    |  | |    |
 *         |  |    |       === optimized to ===>        \  | |    |
 *        loop_b   |                                     loop_b   |
 *         |  |    |                                      |  |    |
 *         |  |____|                                      |  |____|
 *         |                                              |
 * @endverbatim
 *
 * If there is a Phi in pred_b, but we remove pred_b, we have to generate a
 * Phi in loop_b, that has the ins of the Phi in pred_b and a self referencing
 * backedge.
 * @@@ It is negotiable whether we should do this ... there might end up a copy
 * from the Phi in the loop when removing the Phis.
 */
static void optimize_blocks(ir_node *b, void *ctx) {
	int i, j, k, n, max_preds, n_preds, p_preds = -1;
	ir_node *pred, *phi;
	ir_node **in;
	merge_env *env = ctx;

	/* Count the number of predecessor if this block is merged with pred blocks
	   that are empty. */
	max_preds = 0;
	for (i = 0, k = get_Block_n_cfgpreds(b); i < k; ++i) {
		max_preds += test_whether_dispensable(b, i);
	}
	in = xmalloc(max_preds * sizeof(*in));

	/*- Fix the Phi nodes of the current block -*/
	for (phi = get_irn_link(b); phi; ) {
		assert(get_irn_op(phi) == op_Phi);

		/* Find the new predecessors for the Phi */
		p_preds = 0;
		for (i = 0, n = get_Block_n_cfgpreds(b); i < n; ++i) {
			pred = get_Block_cfgpred_block(b, i);

			if (is_Bad(get_Block_cfgpred(b, i))) {
				/* case Phi 1: Do nothing */
			}
			else if (get_Block_block_visited(pred) + 1
			         < get_irg_block_visited(current_ir_graph)) {
				/* case Phi 2: It's an empty block and not yet visited. */
				ir_node *phi_pred = get_Phi_pred(phi, i);

				for (j = 0, k = get_Block_n_cfgpreds(pred); j < k; j++) {
					/* because of breaking loops, not all predecessors are Bad-clean,
					 * so we must check this here again */
					if (! is_Bad(get_Block_cfgpred(pred, j))) {
						if (get_nodes_block(phi_pred) == pred) {
							/* case Phi 2a: */
							assert(get_irn_op(phi_pred) == op_Phi);  /* Block is empty!! */

							in[p_preds++] = get_Phi_pred(phi_pred, j);
						} else {
							/* case Phi 2b: */
							in[p_preds++] = phi_pred;
						}
					}
				}
			} else {
				/* case Phi 3: */
				in[p_preds++] = get_Phi_pred(phi, i);
			}
		}
		assert(p_preds <= max_preds);

		/* Fix the node */
		if (p_preds == 1)
			/* By removal of Bad ins the Phi might be degenerated. */
			exchange(phi, in[0]);
		else
			set_irn_in(phi, p_preds, in);
		env->changed = 1;

		phi = get_irn_link(phi);
	}

	/*- This happens only if merge between loop backedge and single loop entry.
	    Moreover, it is only needed if predb is the direct dominator of b, else there can be no uses
	    of the Phi's in predb ... -*/
	for (k = 0, n = get_Block_n_cfgpreds(b); k < n; ++k) {
		ir_node *predb = get_nodes_block(get_Block_cfgpred(b, k));

		if (get_Block_block_visited(predb) + 1 < get_irg_block_visited(current_ir_graph)) {
			ir_node *next_phi;

			/* we found a predecessor block at position k that will be removed */
			for (phi = get_irn_link(predb); phi; phi = next_phi) {
				int q_preds = 0;
				next_phi = get_irn_link(phi);

				assert(is_Phi(phi));

				if (get_Block_idom(b) != predb) {
					/* predb is not the dominator. There can't be uses of pred's Phi nodes, kill them .*/
					exchange(phi, new_Bad());
				} else {
					/* predb is the direct dominator of b. There might be uses of the Phi nodes from
					   predb in further block, so move this phi from the predecessor into the block b */
					set_nodes_block(phi, b);
					set_irn_link(phi, get_irn_link(b));
					set_irn_link(b, phi);
					env->phis_moved = 1;

					/* first, copy all 0..k-1 predecessors */
					for (i = 0; i < k; i++) {
						pred = get_Block_cfgpred_block(b, i);

						if (is_Bad(pred)) {
							/* Do nothing */
						} else if (get_Block_block_visited(pred) + 1
							< get_irg_block_visited(current_ir_graph)) {
							/* It's an empty block and not yet visited. */
							for (j = 0; j < get_Block_n_cfgpreds(pred); j++) {
								if (! is_Bad(get_Block_cfgpred(pred, j)))
									in[q_preds++] = phi;
							}
						} else {
							in[q_preds++] = phi;
						}
					}

					/* now we are at k, copy the phi predecessors */
					pred = get_nodes_block(get_Block_cfgpred(b, k));
					for (i = 0; i < get_Phi_n_preds(phi); i++) {
						if (! is_Bad(get_Block_cfgpred(pred, i)))
							in[q_preds++] = get_Phi_pred(phi, i);
					}

					/* and now all the rest */
					for (i = k+1; i < get_Block_n_cfgpreds(b); i++) {
						pred = get_nodes_block(get_Block_cfgpred(b, i));

						if (is_Bad(get_Block_cfgpred(b, i))) {
							/* Do nothing */
						} else if (get_Block_block_visited(pred) +1
							< get_irg_block_visited(current_ir_graph)) {
							/* It's an empty block and not yet visited. */
							for (j = 0; j < get_Block_n_cfgpreds(pred); j++) {
								if (! is_Bad(get_Block_cfgpred(pred, j)))
									in[q_preds++] = phi;
							}
						} else {
							in[q_preds++] = phi;
						}
					}

					/* Fix the node */
					if (q_preds == 1)
						exchange(phi, in[0]);
					else
						set_irn_in(phi, q_preds, in);
					env->changed = 1;

					assert(q_preds <= max_preds);
					// assert(p_preds == q_preds && "Wrong Phi Fix");
				}
			}
		}
	}

	/*- Fix the block -*/
	n_preds = 0;
	for (i = 0; i < get_Block_n_cfgpreds(b); i++) {
		pred = get_Block_cfgpred_block(b, i);

		if (is_Bad(pred)) {
			/* case 1: Do nothing */
		} else if (get_Block_block_visited(pred) +1
			< get_irg_block_visited(current_ir_graph)) {
			/* case 2: It's an empty block and not yet visited. */
			assert(get_Block_n_cfgpreds(b) > 1);
			/* Else it should be optimized by equivalent_node. */
			for (j = 0; j < get_Block_n_cfgpreds(pred); j++) {
				ir_node *pred_block = get_Block_cfgpred(pred, j);

				/* because of breaking loops, not all predecessors are Bad-clean,
				 * so we must check this here again */
				if (! is_Bad(pred_block))
					in[n_preds++] = pred_block;
			}
			/* Remove block as it might be kept alive. */
			exchange(pred, b/*new_Bad()*/);
		} else {
			/* case 3: */
			in[n_preds++] = get_Block_cfgpred(b, i);
		}
	}
	assert(n_preds <= max_preds);

	set_irn_in(b, n_preds, in);
	env->changed = 1;

	assert(get_irn_link(b) == NULL || p_preds == -1 || (n_preds == p_preds && "Wrong Phi Fix"));
	xfree(in);
}

/**
 * Block walker: optimize all blocks using the default optimizations.
 * This removes Blocks that with only a Jmp predecessor.
 */
static void remove_simple_blocks(ir_node *block, void *ctx) {
	ir_node *new_blk = equivalent_node(block);
	merge_env *env = ctx;

	if (new_blk != block) {
		exchange(block, new_blk);
		env->changed = 1;
	}
}

/**
 * Handle pre-optimized table switch Cond's.
 * During iropt, all Projs from a switch-Cond are already removed except
 * the defProj and maybe the taken one.
 * The defProj cannot be removed WITHOUT looking backwards, so we do this here.
 *
 * @param cond the switch-Cond
 *
 * @return non-zero if a switch-Cond was optimized
 *
 * Expects all Proj's linked to the cond node
 */
static int handle_switch_cond(ir_node *cond) {
	ir_node *sel = get_Cond_selector(cond);

	ir_node *proj1 = get_irn_link(cond);
	ir_node *proj2 = get_irn_link(proj1);
	ir_node *jmp, *blk;

	blk = get_nodes_block(cond);

	if (proj2 == NULL) {
		/* this Cond has only one Proj: must be the defProj */
		assert(get_Cond_defaultProj(cond) == get_Proj_proj(proj1));
		/* convert it into a Jmp */
		jmp = new_r_Jmp(current_ir_graph, blk);
		exchange(proj1, jmp);
		return 1;
	} else if (get_irn_link(proj2) == NULL) {
		/* We have two Proj's here. Check if the Cond has
		   a constant argument */
		tarval *tv = value_of(sel);

		if (tv != tarval_bad) {
			/* we have a constant switch */
			long num     = get_tarval_long(tv);
			long def_num = get_Cond_defaultProj(cond);

			if (def_num == get_Proj_proj(proj1)) {
				/* first one is the defProj */
				if (num == get_Proj_proj(proj2)) {
					jmp = new_r_Jmp(current_ir_graph, blk);
					exchange(proj2, jmp);
					exchange(proj1, new_Bad());
					return 1;
				}
			} else if (def_num == get_Proj_proj(proj2)) {
				/* second one is the defProj */
				if (num == get_Proj_proj(proj1)) {
					jmp = new_r_Jmp(current_ir_graph, blk);
					exchange(proj1, jmp);
					exchange(proj2, new_Bad());
					return 1;
				}
			} else {
				/* neither: strange, Cond was not optimized so far */
				if (num == get_Proj_proj(proj1)) {
					jmp = new_r_Jmp(current_ir_graph, blk);
					exchange(proj1, jmp);
					exchange(proj2, new_Bad());
					return 1;
				} else if (num == get_Proj_proj(proj2)) {
					jmp = new_r_Jmp(current_ir_graph, blk);
					exchange(proj2, jmp);
					exchange(proj1, new_Bad());
					return 1;
				}
			}
		}
	}
	return 0;
}

/* Optimizations of the control flow that also require changes of Phi nodes.
 *
 * This optimization performs two passes over the graph.
 *
 * The first pass collects all Phi nodes in a link list in the block
 * nodes.  Further it performs simple control flow optimizations.
 * Finally it marks all blocks that do not contain useful
 * computations, i.e., these blocks might be removed.
 *
 * The second pass performs the optimizations intended by this algorithm.
 * It walks only over block nodes and adapts these and the Phi nodes in these blocks,
 * which it finds in a linked list computed by the first pass.
 *
 * We use the block_visited flag to mark empty blocks in the first
 * phase.
 * @@@ It would be better to add a struct in the link field
 * that keeps the Phi list and the mark.  Place it on an obstack, as
 * we will lose blocks and thereby generate memory leaks.
 */
void optimize_cf(ir_graph *irg) {
	int i, j, n;
	ir_node **in = NULL;
	ir_node *cond, *end = get_irg_end(irg);
	ir_graph *rem = current_ir_graph;
	plist_element_t *el;
	merge_env env;

	assert(get_irg_phase_state(irg) != phase_building);

	/* if the graph is not pinned, we cannot determine empty blocks */
	assert(get_irg_pinned(irg) != op_pin_state_floats &&
	       "Control flow optimization need a pinned graph");

	current_ir_graph = irg;

	/* FIXME: is this still needed? */
	edges_deactivate(irg);

	env.changed    = 0;
	env.phis_moved = 0;

	if (get_opt_optimize() && get_opt_unreachable_code()) {
		ir_node *end;

		/* kill dead blocks using dom info */
		assure_doms(irg);
		irg_block_walk_graph(irg, NULL, remove_dead_block_cf, &env.changed);

		/* fix the keep-alives */
		end = get_irg_end(irg);
		for (i = 0, n = get_End_n_keepalives(end); i < n; ++i) {
			ir_node *ka = get_End_keepalive(end, i);

			if (is_Block(ka)) {
				/* do NOT keep dead blocks */
				if (get_Block_dom_depth(ka) < 0) {
					set_End_keepalive(end, i, new_Bad());
					env.changed = 1;
				}
			} else if (is_Block_dead(get_nodes_block(ka)) ||
			           get_Block_dom_depth(get_nodes_block(ka)) < 0) {
				/* do NOT keep nodes in dead blocks */
				set_End_keepalive(end, i, new_Bad());
				env.changed = 1;
			}
		}
	}
	irg_block_walk_graph(irg, NULL, remove_senseless_conds, &env.changed);

	/* Use block visited flag to mark non-empty blocks. */
	inc_irg_block_visited(irg);
	set_using_block_visited(irg);
	set_using_irn_link(irg);

	env.list = plist_new();
	irg_walk(end, merge_blocks, collect_nodes, &env);

	clear_using_block_visited(irg);
	clear_using_irn_link(irg);

	/* handle all collected switch-Conds */
	foreach_plist(env.list, el) {
		cond = plist_element_get_value(el);
		env.changed |= handle_switch_cond(cond);
	}
	plist_free(env.list);

	if (env.changed) {
		/* Handle graph state if was changed. */
		set_irg_outs_inconsistent(irg);
		set_irg_doms_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	}

	/* Optimize the standard code. */
	env.changed = 0;
	assure_doms(irg);
	irg_block_walk(get_irg_end_block(irg), optimize_blocks, remove_simple_blocks, &env);

	/* Walk all keep alives, optimize them if block, add to new in-array
	   for end if useful. */
	n  = get_End_n_keepalives(end);
	if (n > 0)
		NEW_ARR_A(ir_node *, in, n);

	/* in rare cases a node may be kept alive more than once, use the visited flag to detect this */
	inc_irg_visited(irg);
	set_using_visited(irg);

	/* fix the keep alive */
	for (i = j = 0; i < n; i++) {
		ir_node *ka = get_End_keepalive(end, i);

		if (irn_not_visited(ka)) {
			ir_op *op = get_irn_op(ka);

			if ((op == op_Block) && Block_not_block_visited(ka)) {
				/* irg_block_walk() will increase the block visited flag, but we must visit only
				   these blocks that are not visited yet, so decrease it first. */
				set_irg_block_visited(irg, get_irg_block_visited(irg) - 1);
				irg_block_walk(ka, optimize_blocks, remove_simple_blocks, &env.changed);
				mark_irn_visited(ka);
				in[j++] = ka;
			} else if (op == op_Phi) {
				mark_irn_visited(ka);
				/* don't keep alive dead blocks */
				if (! is_Block_dead(get_nodes_block(ka)))
					in[j++] = ka;
			} else if (is_op_keep(op)) {
				mark_irn_visited(ka);
				if (! is_Block_dead(get_nodes_block(ka)))
					in[j++] = ka;
			}
		}
	}
	if (j != n) {
		set_End_keepalives(end, j, in);
		env.changed = 1;
	}

	clear_using_visited(irg);

	if (env.phis_moved) {
		/* Bad: when we moved Phi's, we might produce dead Phi nodes
		   that are kept-alive.
		   Some other phases cannot copy with this, so will them.
		 */
		n = get_End_n_keepalives(end);
		if (n > 0) {
			if (env.changed) {
				/* Handle graph state if was changed. */
				set_irg_outs_inconsistent(irg);
			}
			assure_irg_outs(irg);

			for (i = j = 0; i < n; ++i) {
				ir_node *ka = get_End_keepalive(end, i);

				if (is_Phi(ka)) {
					int k;

					for (k = get_irn_n_outs(ka) - 1; k >= 0; --k) {
						ir_node *user = get_irn_out(ka, k);

						if (user != ka && user != end) {
							/* Is it a real user or just a self loop ? */
							break;
						}
					}
					if (k >= 0)
						in[j++] = ka;
				} else
					in[j++] = ka;
			}
			if (j != n) {
				set_End_keepalives(end, j, in);
				env.changed = 1;
			}
		}
	}

	if (env.changed) {
		/* Handle graph state if was changed. */
		set_irg_outs_inconsistent(irg);
		set_irg_doms_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	}


	/* the verifier doesn't work yet with floating nodes */
	if (get_irg_pinned(irg) == op_pin_state_pinned) {
		/* after optimize_cf(), only Bad data flow may remain. */
		if (irg_vrfy_bads(irg, BAD_DF | BAD_BLOCK | TUPLE)) {
			dump_ir_block_graph(irg, "-vrfy-cf");
			dump_ir_graph(irg, "-vrfy-cf");
			fprintf(stderr, "VRFY_BAD in optimize_cf()\n");
		}
	}

	current_ir_graph = rem;
}
