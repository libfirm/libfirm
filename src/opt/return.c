/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Normalize returns.
 * @author  Michael Beck
 */
#include "ircons_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "raw_bitset.h"
#include "util.h"
#include <stdbool.h>

/*
 * Normalize the Returns of a graph by creating a new End block
 * with One Return(Phi).
 * This is the preferred input for the if-conversion.
 *
 * In pseudocode, it means:
 *
 * if (a)
 *   return b;
 * else
 *   return c;
 *
 * is transformed into
 *
 * if (a)
 *   res = b;
 * else
 *   res = c;
 * return res;
 */
void normalize_one_return(ir_graph *irg)
{
	/* look, if we have more than one return */
	ir_node *endbl = get_irg_end_block(irg);
	int      n     = get_Block_n_cfgpreds(endbl);
	if (n <= 0) {
		/* The end block has no predecessors, we have an endless
		   loop. In that case, no returns exists. */
		confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_ALL);
		add_irg_properties(irg, IR_GRAPH_PROPERTY_ONE_RETURN);
		return;
	}

	unsigned *const returns       = rbitset_alloca(n);
	unsigned        n_rets        = 0;
	bool            filter_dbgi   = false;
	dbg_info       *combined_dbgi = NULL;
	for (int i = 0; i < n; ++i) {
		ir_node *node = get_Block_cfgpred(endbl, i);

		if (is_Return(node)) {
			dbg_info *dbgi = get_irn_dbg_info(node);

			if (dbgi != NULL && dbgi != combined_dbgi) {
				if (filter_dbgi) {
					combined_dbgi = NULL;
				} else {
					combined_dbgi = dbgi;
					filter_dbgi   = true;
				}
			}

			++n_rets;
			rbitset_set(returns, i);
		}
	}

	if (n_rets <= 1) {
		confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_ALL);
		add_irg_properties(irg, IR_GRAPH_PROPERTY_ONE_RETURN);
		return;
	}

	ir_entity *entity     = get_irg_entity(irg);
	ir_type   *type       = get_entity_type(entity);
	unsigned   n_ret_vals = get_method_n_ress(type) + 1;
	ir_node  **in         = ALLOCAN(ir_node*, MAX(n_rets, n_ret_vals));
	ir_node  **retvals    = ALLOCAN(ir_node*, n_rets * n_ret_vals);
	ir_node  **endbl_in   = ALLOCAN(ir_node*, n);

	int last_idx = 0;
	for (int i = 0, j = 0; i < n; ++i) {
		ir_node *ret = get_Block_cfgpred(endbl, i);

		if (rbitset_is_set(returns, i)) {
			/* create a new Jmp for every Ret and place the in in */
			ir_node *block = get_nodes_block(ret);
			in[j] = new_r_Jmp(block);

			/* save the return values and shuffle them */
			for (unsigned k = 0; k < n_ret_vals; ++k)
				retvals[j + k*n_rets] = get_irn_n(ret, k);

			++j;
		} else {
			endbl_in[last_idx++] = ret;
		}
	}

	/* ok, create a new block with all created in's */
	ir_node *block = new_r_Block(irg, n_rets, in);

	/* now create the Phi nodes */
	for (unsigned i = 0, j = 0; i < n_ret_vals; ++i, j += n_rets) {
		ir_mode *mode = get_irn_mode(retvals[j]);
		in[i] = new_r_Phi(block, n_rets, &retvals[j], mode);
	}

	endbl_in[last_idx++] = new_rd_Return(combined_dbgi, block, in[0],
	                                     n_ret_vals-1, &in[1]);

	set_irn_in(endbl, last_idx, endbl_in);

	/* invalidate analysis information:
	 * a new Block was added, so dominator, outs and loop are inconsistent,
	 * callee-state should be still valid */
	confirm_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_NO_TUPLES
		| IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
		| IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
		| IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE);
	add_irg_properties(irg, IR_GRAPH_PROPERTY_ONE_RETURN);
}

/**
 * Check, whether a Return can be moved on block upwards.
 *
 * In a block with a Return, all live nodes must be linked
 * with the Return, otherwise they are dead (because the Return leaves
 * the graph, so no more users of the other nodes can exists.
 *
 * We can move a Return, if its predecessors are Phi nodes or
 * comes from another block. In the later case, it is always possible
 * to move the Return one block up, because the predecessor block must
 * dominate the Return block (SSA) and then it dominates the predecessor
 * block of the Return block as well.
 *
 * All predecessors of the Return block must be Jmp's of course, or we
 * cannot move it up, so we add blocks if needed.
 */
static bool can_move_ret(ir_node *ret)
{
	ir_node *retbl = get_nodes_block(ret);
	foreach_irn_in(ret, i, pred) {
		if (!is_Phi(pred) && retbl == get_nodes_block(pred)) {
			/* first condition failed, found a non-Phi predecessor
			 * then is in the Return block */
			return false;
		}
	}

	/* check, that predecessors are Jmps */
	int n = get_Block_n_cfgpreds(retbl);
	/* we cannot move above a labeled block, as this might kill the block */
	if (n <= 1 || get_Block_entity(retbl) != NULL)
		return false;
	for (int i = 0; i < n; ++i) {
		ir_node *pred = get_Block_cfgpred(retbl, i);

		pred = skip_Tuple(pred);
		if (!is_Jmp(pred) && !is_Bad(pred)) {
			/* simply place a new block here */
			ir_graph *irg   = get_irn_irg(retbl);
			ir_node  *block = new_r_Block(irg, 1, &pred);
			ir_node  *jmp   = new_r_Jmp(block);
			set_Block_cfgpred(retbl, i, jmp);
		}
	}
	return true;
}

/*
 * Normalize the Returns of a graph by moving
 * the Returns upwards as much as possible.
 * This might be preferred for code generation.
 *
 * In pseudocode, it means:
 *
 * if (a)
 *   res = b;
 * else
 *   res = c;
 * return res;
 *
 * is transformed into
 *
 * if (a)
 *   return b;
 * else
 *   return c;
 */
void normalize_n_returns(ir_graph *irg)
{
	/* First, link all returns:
	 * These must be predecessors of the endblock.
	 * Place Returns that can be moved on list, all others
	 * on final. */
	unsigned  n_rets   = 0;
	unsigned  n_finals = 0;
	ir_node  *endbl    = get_irg_end_block(irg);
	ir_node  *final    = NULL;
	ir_node  *list     = NULL;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	for (int i = 0, n = get_Block_n_cfgpreds(endbl); i < n; ++i) {
		ir_node *ret = get_Block_cfgpred(endbl, i);

		if (is_Bad(ret)) {
			continue;
		} else if (is_Return(ret) && can_move_ret(ret)) {
			/* Ok, all conditions met, we can move this Return, put it
			 * on our work list. */
			set_irn_link(ret, list);
			list = ret;
			++n_rets;
		} else {
			/* Put all nodes that are not changed on the final list. */
			set_irn_link(ret, final);
			final = ret;
			++n_finals;
		}
	}

	if (n_rets == 0) {
		ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
		confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_ALL);
		add_irg_properties(irg, IR_GRAPH_PROPERTY_MANY_RETURNS);
		return;
	}

	/* Now move the Returns upwards. We move always one block up (and create n
	 * new Returns), than we check if a newly created Return can be moved even
	 * further. If yes, we simply add it to our work list, else to the final
	 * list. */
	ir_node  *end        = get_irg_end(irg);
	int       n_ret_vals = get_irn_arity(list);
	ir_node **in         = ALLOCAN(ir_node*, n_ret_vals);
	while (list != NULL) {
		ir_node  *ret   = list;
		ir_node  *block = get_nodes_block(ret);
		dbg_info *dbgi  = get_irn_dbg_info(ret);
		ir_node  *phiM;

		list = (ir_node*)get_irn_link(ret);
		--n_rets;

		for (int i = 0, n = get_Block_n_cfgpreds(block); i < n; ++i) {
			ir_node *jmp = get_Block_cfgpred(block, i);
			if (is_Bad(jmp))
				continue;
			assert(is_Jmp(jmp));

			ir_node *new_bl = get_nodes_block(jmp);

			/* create the in-array for the new Return */
			for (int j = 0; j < n_ret_vals; ++j) {
				ir_node *pred = get_irn_n(ret, j);
				in[j] = (is_Phi(pred) && get_nodes_block(pred) == block)
				      ? get_Phi_pred(pred, i) : pred;
			}

			ir_node *new_ret = new_rd_Return(dbgi, new_bl, in[0], n_ret_vals-1,
			                                 &in[1]);
			if (!is_Bad(new_ret)) {
				/*
				 * The newly created node might be bad, if we
				 * create it in a block with only Bad predecessors.
				 * In that case ignore this block.
				 *
				 * We could even kill the jmp then ...
				 */
				if (can_move_ret(new_ret)) {
					set_irn_link(new_ret, list);
					list = new_ret;
					++n_rets;
				} else {
					set_irn_link(new_ret, final);
					final = new_ret;
					++n_finals;
				}
			}

			/* remove the Jmp, we have placed a Return here */
			exchange(jmp, new_r_Bad(irg, mode_X));
		}

		/*
		 * if the memory of the old Return is a PhiM, remove it
		 * from the keep-alives, or it will keep the block which
		 * will crash the dominator algorithm.
		 */
		phiM = get_Return_mem(ret);
		if (is_Phi(phiM)) {
			remove_End_keepalive(end, phiM);
		}
	}

	/*
	 * Last step: Create a new endblock, with all nodes on the final list as
	 * predecessors.
	 */
	ir_node **endbl_in = ALLOCAN(ir_node*, n_finals);
	for (int i = 0; final != NULL; ++i, final = (ir_node*)get_irn_link(final)) {
		endbl_in[i] = final;
	}

	set_irn_in(endbl, n_finals, endbl_in);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	/* Invalidate analysis information:
	 * Blocks become dead and new Returns were deleted, so dominator, outs and
	 * loop are inconsistent, callee-state should be still valid */
	confirm_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_TUPLES
		| IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
		| IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE);
	add_irg_properties(irg, IR_GRAPH_PROPERTY_MANY_RETURNS);
}
