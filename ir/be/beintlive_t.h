/*
 * This file is part of libFirm.
 * Copyright (C) 2012 Universitaet Karlsruhe
 */
#ifndef _BELIVECHK_T_H
#define _BELIVECHK_T_H

#include "besched.h"
#include "belive_t.h"
#include "beutil.h"
#include "iredges_t.h"

/**
 * Check, if one value dominates the other.
 * The dominance is not strict here.
 * @param a The first node.
 * @param b The second node.
 * @return true if a dominates b or if a == b.
 */
static inline bool value_strictly_dominates(const ir_node *a,
                                            const ir_node *b)
{
	/* if a and b are not in the same block, dominance is determined by the
	 * dominance of the blocks. */
	const ir_node *block_a = get_block_const(a);
	const ir_node *block_b = get_block_const(b);
	if (block_a != block_b)
		return block_dominates(block_a, block_b);

	/* Dominance is determined by schedule. */
	return sched_comes_before(a, b);
}

/**
 * Check, if two values interfere.
 * @param lv Liveness information
 * @param a The first value.
 * @param b The second value.
 * @return true, if a and b interfere, false if not.
 */
static inline bool be_values_interfere(be_lv_t const *lv, ir_node const *a,
                                       ir_node const *b)
{
	assert(a != b);
	if (value_strictly_dominates(b, a)) {
		/* Adjust a and b so, that a dominates b if
		 * a dominates b or vice versa. */
		ir_node const *const t = a;
		a = b;
		b = t;
	} else if (!value_strictly_dominates(a, b)) {
		/* If there is no dominance relation, they do not interfere. */
		return false;
	}

	ir_node *const bb = get_nodes_block(b);

	/* If a is live end in b's block it is
	 * live at b's definition (a dominates b) */
	if (be_is_live_end(lv, bb, a))
		return true;

	/* Look at all usages of a.
	 * If there's one usage of a in the block of b, then
	 * we check, if this use is dominated by b, if that's true
	 * a and b interfere. Note that b must strictly dominate the user,
	 * since if b is the last user of in the block, b and a do not
	 * interfere.
	 * Uses of a not in b's block can be disobeyed, because the
	 * check for a being live at the end of b's block is already
	 * performed. */
	foreach_out_edge(a, edge) {
		ir_node const *const user = get_edge_src_irn(edge);
		if (get_nodes_block(user) == bb && !is_Phi(user)
		    && sched_comes_before(b, user))
			return true;
	}

	return false;
}

#endif
