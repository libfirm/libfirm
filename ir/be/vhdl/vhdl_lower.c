/**
 * @file
 * @brief    Replaces Const nodes with PinnedConst nodes in the block(s) in which they are needed.
 *           In case of Phis, places PinnedConsts in the appropriate predecessor.
 * @author   Andreas Fried
 *
 */

#include "vhdl_lower.h"

#include "irdump.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irnode.h"
#include "iropt_t.h"

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
				block = get_nodes_block(get_irn_n(block, i));
			}

			ir_node *new_op = pin_const(op, block);
			set_irn_n(node, i, new_op);
		}
	}
}

static void place_consts(ir_graph *irg)
{
	irg_walk_graph(irg, NULL, do_place_consts, NULL);
}

static void do_insert_phi1(ir_node *node, void *env)
{
	(void)env;

	if (is_Phi(node) || is_Block(node)) {
		return;
	}

	int arity = get_irn_arity(node);
	ir_node *block = get_nodes_block(node);

	for (int i = 0; i < arity; i++) {
		ir_node *op = get_irn_n(node, i);

		if (is_Block(op)) {
			return;
		}

		if (get_nodes_block(op) != block) {
			// We use a value from outside our block => insert a Phi
			int block_arity = get_irn_arity(block);

			dbg_info *dbgi = get_irn_dbg_info(node);
			ir_mode *mode = get_irn_mode(op);
			ir_node *phi_ins[block_arity];
			for (int in = 0; in < block_arity; in++) {
				phi_ins[in] = op;
			}
			ir_node *phi = new_rd_Phi(dbgi, block, block_arity, phi_ins, mode);

			set_irn_n(node, i, phi);
		}
	}
}

static void insert_phi1(ir_graph *irg)
{
	int before = get_optimize();
	set_optimize(0);
	irg_walk_graph(irg, NULL, do_insert_phi1, NULL);
	set_optimize(before);
}

void lower_for_vhdl(ir_graph *irg)
{
	place_consts(irg);
	dump_ir_graph(irg, "place-consts");

	insert_phi1(irg);
	dump_ir_graph(irg, "insert-phi1");

	//TODO
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);
}
