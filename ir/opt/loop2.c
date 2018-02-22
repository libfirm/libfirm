#include "lcssa_t.h"
#include "irtools.h"
#include <assert.h>

static void duplicate_node(ir_node *const node, ir_node *const new_block)
{
	ir_printf("duplicate node %n (%d)\n", node, get_irn_node_nr(node));
	//int const opt = get_optimize();
	//set_optimize(0);
	ir_node *const new_node = exact_copy(node);
	//set_optimize(opt);
	set_nodes_block(new_node, new_block);
	set_irn_link(node, new_node);
	printf("new node %ld\n", get_irn_node_nr(new_node));
}

static void rewire_node(ir_node *const node)
{
	ir_node *const new_node = get_irn_link(node);
	assert(new_node);
	assert(get_irn_arity(node) == get_irn_arity(new_node));
	// TODO: use foreach_irn_in
	int const arity = get_irn_arity(new_node);
	for (int i = 0; i < arity; ++i) {
		ir_node *const pred     = get_irn_n(new_node, i);
		ir_node *const new_pred = get_irn_link(pred);
		if (is_backedge(node, i)) {
			assert(new_pred != NULL);
			set_irn_n(node, i, new_pred);
		}
		else if (new_pred != NULL) {
			set_irn_n(new_node, i, new_pred);
		}
	}
}

static void duplicate_block(ir_node *const block)
{
	ir_node *const new_block = exact_copy(block);
	set_irn_link(block, new_block);
	unsigned int const n_outs = get_irn_n_outs(block);
	for (unsigned int i = 0; i < n_outs; ++i) {
		ir_node *const node = get_irn_out(block, i);
		assert(!is_Block(node));
		duplicate_node(node, new_block);
	}
}

static void rewire_block(ir_node *const block)
{
	rewire_node(block);
	unsigned int const n_outs = get_irn_n_outs(block);
	for (unsigned int i = 0; i < n_outs; ++i) {
		ir_node *const node = get_irn_out(block, i);
		assert(!is_Block(node));
		rewire_node(node);
	}
}

static void duplicate_loop(ir_loop *const loop)
{
	printf("duplicating loop\n");
	size_t const n_elements = get_loop_n_elements(loop);
	// step 1: duplicate blocks
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_node) {
			assert(is_Block(element.node));
			duplicate_block(element.node);
		}
	}
	// step 2: rewire the edges
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_node) {
			assert(is_Block(element.node));
			rewire_block(element.node);
		}
	}
}

// duplicate one innermost loop, for testing purposes
static void duplicate_one_loop(ir_loop *const loop)
{
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_loop) {
			duplicate_one_loop(element.son);
			return;
		}
	}
	duplicate_loop(loop);
}

void do_loop_unrolling2(ir_graph *const irg)
{
	assure_lcssa(irg);
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO | IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	duplicate_one_loop(get_irg_loop(irg));
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	dump_ir_graph(irg, "loop-unrolling");
}
