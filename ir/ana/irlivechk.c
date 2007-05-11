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
 * @file    livechk.c
 * @date    21.04.2007
 * @author  Sebastian Hack
 * @version $Id$
 * @summary
 *
 * Liveness checks as developed by Benoit Boissinot, Fabrice Rastello and myself.
 *
 * The speciality here is, that nothing has to be recomputed if new nodes are created
 * or old ones deleted.
 *
 * This algo has one core routine check_live_end_internal() which performs the liveness check.
 * It only relies on the precomputation done in the constructor, which in turn needs:
 * - out edges
 * - the dominance tree
 * - data obtained from a depth-first-search
 *
 * The precomputation remains valid as long as the CFG is not altered.
 */

#include <stdio.h>

#include "irgraph_t.h"
#include "irphase_t.h"
#include "iredges_t.h"
#include "irprintf.h"
#include "irdump.h"

#include "dfs_t.h"
#include "bitset.h"
#include "util.h"

#include "irlivechk.h"

typedef struct _bl_info_t {
	ir_node *block;            /**< The block. */

	int id;                    /**< a tight number for the block.
								 we're just reusing the pre num from
								 the DFS. */

	bitset_t *red_reachable;   /**< Holds all id's if blocks reachable
								 in the CFG modulo back edges. */

	bitset_t *be_tgt_reach;	   /**< target blocks of back edges whose
								 sources are reachable from this block
								 in the reduced graph. */

	bitset_t *be_tgt_dom;      /**< target blocks of back edges which
								 are dominated by this block. */
} bl_info_t;

#define get_block_info(lv, bl) ((bl_info_t *) phase_get_irn_data(&(lv)->ph, bl))

struct _lv_chk_t {
	ir_phase ph;
	dfs_t *dfs;
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
	int n_blocks;
	bitset_t *back_edge_src;
	bitset_t *back_edge_tgt;
	bl_info_t **map;
};

static void *init_block_data(ir_phase *ph, ir_node *irn, void *old)
{
	lv_chk_t *lv      = container_of(ph, lv_chk_t, ph);
	bl_info_t *bi     = phase_alloc(ph, sizeof(bi[0]));

	bi->id            = dfs_get_pre_num(lv->dfs, irn);
	bi->block         = irn;
	bi->red_reachable = bitset_obstack_alloc(phase_obst(ph), lv->n_blocks);
	bi->be_tgt_reach  = bitset_obstack_alloc(phase_obst(ph), lv->n_blocks);
	bi->be_tgt_dom    = bitset_obstack_alloc(phase_obst(ph), lv->n_blocks);
	return bi;
}

/**
 * Filter function to select all nodes for which liveness is computed.
 * @param irn A node.
 * @return    1 if the node shall be considered in liveness, 0 if not.
 */
static INLINE int is_liveness_node(const ir_node *irn)
{
	switch(get_irn_opcode(irn)) {
	case iro_Block:
	case iro_Bad:
	case iro_End:
		return 0;
	default:;
	}

	return 1;
}

/**
 * Compute the transitive closure on the reduced graph.
 * The reduced graph is the original graph without back edges.
 * Since that is a DAG, a reverse post order of the graph gives a toposort
 * which is ideally suited to compute the transitive closure.
 * Note also, that the DFS tree of the reduced graph is the same than the one
 * of the original graph. This saves us computing a new reverse post order.
 * We also can re-use the DFS tree of the original graph.
 */
static void red_trans_closure(lv_chk_t *lv)
{
	int i, n;

	for (i = 0, n = dfs_get_n_nodes(lv->dfs); i < n; ++i) {
		ir_node *bl   = dfs_get_post_num_node(lv->dfs, i);
		bl_info_t *bi = get_block_info(lv, bl);

		const ir_edge_t *edge;

		foreach_block_succ (bl, edge) {
			ir_node *succ = get_edge_src_irn(edge);
			bl_info_t *si = get_block_info(lv, succ);
			dfs_edge_kind_t kind = dfs_get_edge_kind(lv->dfs, bl, succ);

			/*
			 * if the successor is no back edge, include all reachable
			 * blocks from there into the reachable set of the current node
			 */
			if (kind != DFS_EDGE_BACK) {
				assert(dfs_get_post_num(lv->dfs, bl)
						> dfs_get_post_num(lv->dfs, succ));
				bitset_or(bi->red_reachable, si->red_reachable);
				bitset_set(bi->red_reachable, si->id);
			}

			/* mark the block as a back edge src and succ as back edge tgt. */
			else {
				bitset_set(lv->back_edge_src, bi->id);
				bitset_set(lv->back_edge_tgt, si->id);
			}
		}

	}

}

/**
 * Compute the two back edge sets for each block.
 * <code>be_tgt_reach</code> contains all target blocks of a back edges reachable from a node.
 * <code>be_tgt_dom</code> contains all target blocks of back edges strictly dominated
 * by a node.
 */
static void compute_back_edge_sets(lv_chk_t *lv, ir_node *bl)
{
	bl_info_t *bi = get_block_info(lv, bl);
	bitset_t *tmp = bitset_alloca(lv->n_blocks);

	bitset_pos_t elm;
	ir_node *n;

	dominates_for_each (bl, n) {
		bl_info_t *ni = get_block_info(lv, n);

		/* compute information for dominance sub tree */
		compute_back_edge_sets(lv, n);

		/*
		 * of course all blocks dominated by blocks in the
		 * subtree are also dominated by bl.
		 */
		bitset_or(bi->be_tgt_dom, ni->be_tgt_dom);

		/*
		 * add the immeditate dominee to the back edge tgt dominance
		 * bitset if it is the target node of a back edge.
		 */
		if (bitset_is_set(lv->back_edge_tgt, ni->id))
			bitset_set(bi->be_tgt_dom, ni->id);
	}

	/*
	 * iterate over all back edge src nodes which are reachable from
	 * this nodes and put the targets of the back edges in the be_tgt_reach
	 * bitset of the node.
	 */
	bitset_copy(tmp, bi->red_reachable);
	bitset_set(tmp, bi->id);
	bitset_and(tmp, lv->back_edge_src);
	bitset_foreach (tmp, elm) {
		ir_node *src = lv->map[elm]->block;
		const ir_edge_t *edge;

		foreach_block_succ (src, edge) {
			ir_node *succ        = get_edge_src_irn(edge);
			dfs_edge_kind_t kind = dfs_get_edge_kind(lv->dfs, src, succ);

			if (kind == DFS_EDGE_BACK) {
				bl_info_t *si = get_block_info(lv, succ);
				bitset_set(bi->be_tgt_reach, si->id);
			}
		}
	}
}

lv_chk_t *lv_chk_new(ir_graph *irg)
{
	lv_chk_t *res = xmalloc(sizeof(res[0]));
	struct obstack *obst;
	int i;

	phase_init(&res->ph, "liveness check", irg, PHASE_DEFAULT_GROWTH, init_block_data, NULL);
	obst = phase_obst(&res->ph);

	FIRM_DBG_REGISTER(res->dbg, "ir.ana.lvchk");

	res->dfs           = dfs_new(&absgraph_irg_cfg_succ, irg);
	res->n_blocks      = dfs_get_n_nodes(res->dfs);
	res->back_edge_src = bitset_obstack_alloc(obst, res->n_blocks);
	res->back_edge_tgt = bitset_obstack_alloc(obst, res->n_blocks);
	res->map           = obstack_alloc(obst, res->n_blocks * sizeof(res->map[0]));

#if 1
	{
		char name[256];
		FILE *f;
		ir_snprintf(name, sizeof(name), "dfs_%F.dot", irg);
		if ((f = fopen(name, "wt")) != NULL) {
			dfs_dump(res->dfs, f);
			fclose(f);
		}
		dump_ir_block_graph(irg, "-lvchk");
	}
#endif

	/* fill the map which maps pre_num to block infos */
	for (i = res->n_blocks - 1; i >= 0; --i) {
		ir_node *irn = dfs_get_pre_num_node(res->dfs, i);
		res->map[i]  = phase_get_or_set_irn_data(&res->ph, irn);
	}

	/* first of all, compute the transitive closure of the CFG *without* back edges */
	red_trans_closure(res);

	/* now fill the two remaining bitsets concerning back edges */
	compute_back_edge_sets(res, get_irg_start_block(irg));

	DBG((res->dbg, LEVEL_1, "liveness chk in %+F\n", irg));
	for (i = res->n_blocks - 1; i >= 0; --i) {
		ir_node *irn  = dfs_get_pre_num_node(res->dfs, i);
		bl_info_t *bi = get_block_info(res, irn);
		DBG((res->dbg, LEVEL_1, "lv_chk for %d -> %+F\n", i, irn));
		DBG((res->dbg, LEVEL_1, "\tred reach: %B\n", bi->red_reachable));
		DBG((res->dbg, LEVEL_1, "\ttgt reach: %B\n", bi->be_tgt_reach));
		DBG((res->dbg, LEVEL_1, "\ttgt dom:   %B\n", bi->be_tgt_dom));
	}

	DBG((res->dbg, LEVEL_1, "back edge src: %B\n", res->back_edge_src));
	DBG((res->dbg, LEVEL_1, "back edge tgt: %B\n", res->back_edge_tgt));

	return res;
}

void lv_chk_free(lv_chk_t *lv)
{
	obstack_free(phase_obst(&lv->ph), NULL);
	dfs_free(lv->dfs);
	xfree(lv);
}

/**
 * Check if a node is live at the end of a block.
 * This function is for internal use as its code is shared between
 * the in/end routines below. It is almost the "live_end" routine
 * but passing in the bitset for recording the blocks where the variable
 * is used saves some effort in the "live_in" routine. See below for
 * details.
 *
 * @param lv    The liveness check environment.
 * @param what  The node to check for.
 * @param bl    The block under investigation.
 * @param end   If 1, it is tested if the node is live at the end.
 *              If 0, it is only tested if the node is live out.
 * @param uses  A bitset where this routine records all ids of blocks
 *              where this variable is used. Note that the bitset
 *              is only guaranteed to be filled if the node was not
 *              live at the end of the block.
 * @return      1, if @p what is live at the end at @p bl.
 */
static int check_live_internal(const lv_chk_t *lv, const ir_node *what, const ir_node *bl, int end, bitset_t *uses)
{
	ir_node *what_bl;

	assert(is_Block(bl) && "can only check for liveness in a block");

	if (!is_liveness_node(what))
		return 0;

	what_bl = get_nodes_block(what);
	if (!block_dominates(what_bl, bl))
		return 0;

	/*
	 * If the block in question is the same as the definition block,
	 * the algorithm is simple. JUst check for uses not inside this block.
	 */
	if (what_bl == bl) {
		const ir_edge_t *edge;

		DBG((lv->dbg, LEVEL_2, "lv check same block %+F in %+F\n", what, bl));
		foreach_out_edge (what, edge) {
			ir_node *use    = get_edge_src_irn(edge);
			ir_node *use_bl;

			if (!is_liveness_node(use))
				continue;

			use_bl = get_nodes_block(use);
			if (is_Phi(use)) {
				int pos = get_edge_src_pos(edge);
				use_bl  = get_Block_cfgpred_block(use_bl, pos);

				if (end && use_bl == bl) {
					DBG((lv->dbg, LEVEL_2, "\tphi %+F in succ %+F,%d -> live end\n", use, use_bl, pos));
					return 1;
				}
			}

			if (use_bl != what_bl)
				return 1;
		}

		return 0;
	}

	/* this is the complicated case */
	else {
		bitset_t *visited   = bitset_alloca(lv->n_blocks);
		bitset_t *to_visit  = bitset_alloca(lv->n_blocks);
		bitset_t *next      = bitset_alloca(lv->n_blocks);
		bl_info_t *def      = get_block_info(lv, what_bl);
		bl_info_t *bli      = get_block_info(lv, bl);

		const ir_edge_t *edge;

		foreach_out_edge (what, edge) {
			ir_node *user   = get_edge_src_irn(edge);
			ir_node *use_bl;

			if (!is_liveness_node(user))
				continue;

			use_bl = get_nodes_block(user);
			if (is_Phi(user)) {
				int pos          = get_edge_src_pos(edge);
				ir_node *pred_bl = get_Block_cfgpred_block(use_bl, pos);
				bl_info_t *bi    = get_block_info(lv, pred_bl);

				if (end && pred_bl == bl)
					return 1;

				bitset_set(uses, bi->id);
			}

			else {
				bl_info_t *bi   = get_block_info(lv, use_bl);
				bitset_set(uses, bi->id);
			}
		}
		DBG((lv->dbg, LEVEL_2, "\tuses: %B\n", uses));

		bitset_clear(uses, def->id);
		bitset_set(to_visit, bli->id);
		do {
			int id        = bitset_next_set(to_visit, 0);
			bl_info_t *bi = lv->map[id];

			DBG((lv->dbg, LEVEL_2, "\tto visit: %B\n", to_visit));
			DBG((lv->dbg, LEVEL_2, "\tvisited:  %B\n", visited));

			/*
			 * if one of the blocks is reachable, the node must be live there.
			 * Not that this is not sufficient, since the nodes reachable
			 * via back edges are not contained in the red_reachable set.
			 */
			if (bitset_intersect(bi->red_reachable, uses))
				return 1;

			/*
			 * if not, we have to check the back edges in question, if
			 * they lead to places which are reachable.
			 */
			else {
				bitset_set(visited, id);
				bitset_or(visited, bi->red_reachable);

				bitset_copy(next, bi->be_tgt_reach);
				bitset_and(next, def->be_tgt_dom);
				DBG((lv->dbg, LEVEL_2, "\tnext: %B\n----\n", next));

				if (bitset_intersect(uses, next))
					return 1;

				bitset_or(to_visit, next);
				bitset_andnot(to_visit, visited);

			}
		} while (!bitset_is_empty(to_visit));
	}

	return 0;
}

int lv_chk_bl_end(const lv_chk_t *lv, const ir_node *bl, const ir_node *what)
{
	bitset_t *uses = bitset_alloca(lv->n_blocks);
	return check_live_internal(lv, what, bl, 1, uses);
}

int lv_chk_bl_out(const lv_chk_t *lv, const ir_node *bl, const ir_node *what)
{
	bitset_t *uses = bitset_alloca(lv->n_blocks);
	return check_live_internal(lv, what, bl, 0, uses);
}

int lv_chk_bl_in(const lv_chk_t *lv, const ir_node *bl, const ir_node *what)
{
	/*
	 * only check, if the node is not defined in this block.
	 * Under SSA, a node can never be live in at its definition block.
	 */
	if (get_nodes_block(what) != bl) {
		bl_info_t *bi   = get_block_info(lv, bl);
		int id          = bi->id;
		bitset_t *uses  = bitset_alloca(lv->n_blocks);
		int live_at_end = check_live_internal(lv, what, bl, 1, uses);

		/* to be live in, the value must be live at the end or have a use in this block */
		return live_at_end || bitset_is_set(uses, id);
	}

	return 0;
}
