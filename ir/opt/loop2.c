#include "firm.h"
#include "xmalloc.h"
#include "debug.h"
#include <stdbool.h>
#include <assert.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static bool is_inside_loop(ir_node const *const node)
{
	ir_graph const *const graph = get_irn_irg(node);
	ir_node const *const block = is_Block(node) ? node : get_nodes_block(node);
	ir_loop const *const loop = get_irn_loop(block);
	return loop && loop != get_irg_loop(graph);
}

// insert a phi node between node and its nth predecessor in block
static ir_node *insert_phi(ir_node *const node, int const n, ir_node *const block)
{
	ir_node *const pred = get_irn_n(node, n);
	int const block_arity = get_irn_arity(block);
	ir_node **const in = ALLOCAN(ir_node *, block_arity);
	for (int i = 0; i < block_arity; ++i) {
		in[i] = pred;
	}
	ir_mode *const mode = get_irn_mode(pred);
	int opt = get_optimize();
	set_optimize(0);
	ir_node *const phi = new_r_Phi(block, block_arity, in, mode);
	set_optimize(opt);
	set_irn_n(node, n, phi);
	mark_irn_visited(phi);
	DB((dbg, LEVEL_3, "inserting phi %ld\n", get_irn_node_nr(phi)));
	return phi;
}

// insert phi nodes for the edge between node and its nth predecessor
static void insert_phis_for_edge(ir_node *node, int n)
{
	ir_node *const pred = get_irn_n(node, n);
	if (!mode_is_data(get_irn_mode(pred)))
		return;
	ir_node *const pred_block = get_nodes_block(pred);
	if (!is_inside_loop(pred_block))
		return;
	ir_node *block = get_nodes_block(node);
	ir_loop *loop = get_irn_loop(block);
	// walk up the dominance tree
	if (is_Phi(node)) {
		// if node is a phi, start the walk at the nth predecessor of block
		block = get_nodes_block(get_irn_n(block, n));
	}
	while (block != pred_block) {
		ir_node *const idom = get_Block_idom(block);
		// insert phi nodes whenever the loop changes
		if (get_irn_loop(idom) != loop) {
			node = insert_phi(node, n, block);
			n = 0;
			loop = get_irn_loop(idom);
		}
		block = idom;
	}
}

static void insert_phis_for_node(ir_node *const node, void *const env)
{
	(void)env;
	if (is_Block(node))
		return;
	int const arity = get_irn_arity(node);
	for (int i = 0; i < arity; ++i) {
		insert_phis_for_edge(node, i);
	}
}

#ifdef DEBUG_libfirm

static bool is_inner_loop(ir_loop *const outer_loop, ir_loop *inner_loop)
{
	ir_loop *old_inner_loop;
	do {
		old_inner_loop = inner_loop;
		inner_loop = get_loop_outer_loop(inner_loop);
	} while (inner_loop != old_inner_loop && inner_loop != outer_loop);
	return inner_loop != old_inner_loop;
}

static void verify_lcssa_node(ir_node *const node, void *const env)
{
	(void)env;
	if (is_Block(node))
		return;
	ir_loop *const loop = get_irn_loop(get_nodes_block(node));
	int const arity = get_irn_arity(node);
	for (int i = 0; i < arity; ++i) {
		ir_node *const pred = get_irn_n(node, i);
		if (!mode_is_data(get_irn_mode(pred)))
			return;
		ir_loop *const pred_loop = get_irn_loop(get_nodes_block(pred));
		if (is_inner_loop(loop, pred_loop)) {
			assert(is_Phi(node));
		}
	}
}

static void verify_lcssa(ir_graph *const irg)
{
	irg_walk_graph(irg, verify_lcssa_node, NULL, NULL);
}

#endif /* DEBUG_libfirm */

static void assure_lcssa(ir_graph *const irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.lcssa");
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	irg_walk_graph(irg, insert_phis_for_node, NULL, NULL);
	DEBUG_ONLY(verify_lcssa(irg);)
}

void do_loop_unrolling2(ir_graph *const irg)
{
	assure_lcssa(irg);
}
