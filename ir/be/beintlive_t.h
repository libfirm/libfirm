/**
 * @file   beintlive_t.h
 * @date   10.05.2007
 * @author Sebastian Hack
 *
 * Principal routines for liveness and interference checks.
 *
 * Copyright (C) 2007 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _BELIVECHK_T_H
#define _BELIVECHK_T_H

#include "irgraph_t.h"
#include "irphase_t.h"
#include "iredges_t.h"

#include "statev.h"

#include "beirg_t.h"
#include "besched_t.h"
#include "belive_t.h"

/**
 * Check dominance of two nodes in the same block.
 * @param a The first node.
 * @param b The second node.
 * @return 1 if a comes before b in the same block or if a == b, 0 else.
 */
static INLINE int _value_dominates_intrablock(const ir_node *a, const ir_node *b)
{
	/* TODO: ? :  can be removed?! */
	sched_timestep_t as = is_Phi(a) ? 0 : sched_get_time_step(a);
	sched_timestep_t bs = is_Phi(b) ? 0 : sched_get_time_step(b);
	return as <= bs;
}

/**
 * Check strict dominance of two nodes in the same block.
 * @param a The first node.
 * @param b The second node.
 * @return 1 if a comes before b in the same block, 0 else.
 */
static INLINE int _value_strictly_dominates_intrablock(const ir_node *a, const ir_node *b)
{
	/* TODO: ? :  can be removed?! */
	sched_timestep_t as = is_Phi(a) ? 0 : sched_get_time_step(a);
	sched_timestep_t bs = is_Phi(b) ? 0 : sched_get_time_step(b);
	return as < bs;
}

/**
 * Check, if one value dominates the other.
 * The dominance is not strict here.
 * @param a The first node.
 * @param b The second node.
 * @return 1 if a dominates b or if a == b, 0 else.
 */
static INLINE int _value_dominates(const ir_node *a, const ir_node *b)
{
	const ir_node *block_a = get_block(a);
	const ir_node *block_b = get_block(b);

	/*
	 * a and b are not in the same block,
	 * so dominance is determined by the dominance of the blocks.
	 */
	if(block_a != block_b) {
		return block_dominates(block_a, block_b);
	}

	/*
	 * Dominance is determined by the time steps of the schedule.
	 */
	return _value_dominates_intrablock(a, b);
}

/**
 * Check, if one value dominates the other.
 * The dominance is strict here.
 * @param a The first node.
 * @param b The second node.
 * @return 1 if a dominates b, 0 else.
 */
static INLINE int _value_strictly_dominates(const ir_node *a, const ir_node *b)
{
	const ir_node *block_a = get_block(a);
	const ir_node *block_b = get_block(b);

	/*
	 * a and b are not in the same block,
	 * so dominance is determined by the dominance of the blocks.
	 */
	if(block_a != block_b) {
		return block_dominates(block_a, block_b);
	}

	/*
	 * Dominance is determined by the time steps of the schedule.
	 */
	return _value_strictly_dominates_intrablock(a, b);
}

/**
 * Check, if two values interfere.
 * @param lv Liveness information (in the future we should use a be_irg_t here).
 * @param a The first value.
 * @param b The second value.
 * @return 1, if a and b interfere, 0 if not.
 */
static INLINE int _lv_values_interfere(const be_lv_t *lv, const ir_node *a, const ir_node *b)
{
	int a2b = _value_dominates(a, b);
	int b2a = _value_dominates(b, a);
	int res = 0;

	stat_ev_ctx_push("beintlive");

	/*
	 * Adjust a and b so, that a dominates b if
	 * a dominates b or vice versa.
	 */
	if(b2a) {
		const ir_node *t = a;
		a = b;
		b = t;
		a2b = 1;
	}

	/* If there is no dominance relation, they do not interfere. */
	if(a2b) {
		const ir_edge_t *edge;
		ir_node *bb = get_nodes_block(b);

		stat_ev_dbl("beintlive_ignore", arch_irn_is(lv->birg->main_env->arch_env, a, ignore));

		/*
		 * If a is live end in b's block it is
		 * live at b's definition (a dominates b)
		 */
		if(be_is_live_end(lv, bb, a)) {
			res = 1;
			goto end;
		}

		/*
		 * Look at all usages of a.
		 * If there's one usage of a in the block of b, then
		 * we check, if this use is dominated by b, if that's true
		 * a and b interfere. Note that b must strictly dominate the user,
		 * since if b is the last user of in the block, b and a do not
		 * interfere.
		 * Uses of a not in b's block can be disobeyed, because the
		 * check for a being live at the end of b's block is already
		 * performed.
		 */
		foreach_out_edge(a, edge) {
			const ir_node *user = get_edge_src_irn(edge);
			if(get_nodes_block(user) == bb && !is_Phi(user) && _value_strictly_dominates(b, user)) {
				res = 1;
				goto end;
			}
		}
  	}

end:
	stat_ev_ctx_pop("beintlive");
	return res;
}



/**
 * Check if a node dominates a use.
 * Note that the use of a phi is in its corresponding predecessor.
 * @param irn  The node.
 * @param edge The use.
 * @return     1, if @p irn dominates the use @p edge.
 */
static INLINE int _dominates_use(const ir_node *irn, const ir_edge_t *edge)
{
	ir_node *use = get_edge_src_irn(edge);

	if (is_Phi(use)) {
		int pos         = get_edge_src_pos(edge);
		ir_node *phi_bl = get_nodes_block(use);
		ir_node *use_bl = get_Block_cfgpred_block(phi_bl, pos);
		ir_node *irn_bl = get_nodes_block(irn);
		return block_dominates(irn_bl, use_bl);
	}

	return _value_dominates(irn, use);
}

/**
 * Check if a node strictly dominates a use.
 * Note that the use of a phi is in its corresponding predecessor.
 * @param irn  The node.
 * @param edge The use.
 * @return     1, if @p irn strictly dominates the use @p edge.
 */
static INLINE int _strictly_dominates_use(const ir_node *irn, const ir_edge_t *edge)
{
	return get_edge_src_irn(edge) != irn && _dominates_use(irn, edge);
}

/**
 * Check, if a node is live in front of another.
 * @param birg  The backend irg.
 * @param irn   The node.
 * @param where The location to check for.
 * @return      1, if @p irn is live in front of @p where.
 */
static INLINE int _be_lv_chk_before_irn(const be_irg_t *birg, const ir_node *irn, const ir_node *where)
{
	const be_lv_t *lv = be_get_birg_liveness(birg);
	const ir_edge_t *edge;

	/* the node must strictly dominate the location, else it cannot be live there. */
	if (!_value_dominates(irn, where) || irn == where)
		return 0;

	/*
	 * now that it is clear that it strictly dominates the location it is surely live
	 * if it is also live end at the block.
	 */
	if (be_is_live_end(lv, get_nodes_block(where), irn))
		return 1;

	/*
	 * If the node is not live out, we have to check if there
	 * is a use which is dominated by the location.
	 */
	foreach_out_edge (irn, edge) {
		if (_dominates_use(where, edge))
			return 1;
	}

	return 0;
}

/**
 * Check, if a node is live after another node.
 * @param birg  The backend irg.
 * @param irn   The node.
 * @param where The location to check for.
 * @return      1, if @p irn is live after @p where.
 */
static INLINE int _be_lv_chk_after_irn(const be_irg_t *birg, const ir_node *irn, const ir_node *where)
{
	const be_lv_t *lv = be_get_birg_liveness(birg);
	const ir_edge_t *edge;

	if (!_value_dominates(irn, where))
		return 0;

	if (be_is_live_end(lv, get_nodes_block(where), irn))
		return 1;

	foreach_out_edge (irn, edge) {
		if (_strictly_dominates_use(where, edge))
			return 1;
	}

	return 0;
}

#define value_dominates_intrablock(a, b)         _value_dominates_intrablock(a, b)
#define value_dominates(a, b)                    _value_dominates(a, b)
#define values_interfere(birg, a, b)             _lv_values_interfere(be_get_birg_liveness(birg), a, b)
#define dominates_use(a, e)                      _dominates_use(a, e)
#define strictly_dominates_use(a, e)             _strictly_dominates_use(a, e)
#define be_lv_chk_before_irn(birg, a, b)         _be_lv_chk_before_irn(birg, a, b)
#define be_lv_chk_after_irn(birg, a, b)          _be_lv_chk_after_irn(birg, a, b)

#endif /* _BELIVECHK_T_H */
