/**
 * @file
 * @brief    Replaces Const nodes with PinnedConst nodes in the block(s) in which they are needed.
 *           In case of Phis, places PinnedConsts in the appropriate predecessor.
 * @author   Andreas Fried
 *
 */

#include <firm.h>

static ir_node *pin_const(ir_node *konst, ir_node *where)
{
	dbg_info  *dbgi = get_irn_dbg_info(konst);
	ir_tarval *tv   = get_Const_tarval(konst);
	return new_rd_PinnedConst(dbgi, where, tv);
}

static void do_place_consts(ir_node *node, void *env)
{
	(void)env;

	int arity = get_irn_arity(node);

	for (int i = 0; i < arity; i++) {
		ir_node *op = get_irn_n(node, i);

		if (is_Const(op)) {
			ir_node *block = get_nodes_block(node);
			if (is_Phi(node)) {
				// Place i-th argument of Phi in i-th predecessor block.
				block = get_irn_n(block, i);
			}

			ir_node *new_op = pin_const(op, block);
			set_irn_n(node, i, new_op);
		}
	}
}

void place_consts(ir_graph *irg)
{
	irg_walk_graph(irg, NULL, do_place_consts, NULL);
}
