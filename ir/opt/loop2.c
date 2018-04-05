#include "lcssa_t.h"
#include "irtools.h"
#include "xmalloc.h"
#include "debug.h"
#include <assert.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static void walk_loop(ir_loop *const loop, irg_walk_func *const func, void *const env)
{
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_node) {
			func(element.node, env);
		} else if (*element.kind == k_ir_loop) {
			walk_loop(element.son, func, env);
		}
	}
}

static bool is_inner_loop(ir_loop *const outer_loop, ir_loop *inner_loop)
{
	ir_loop *old_inner_loop;
	do {
		old_inner_loop = inner_loop;
		inner_loop = get_loop_outer_loop(inner_loop);
	} while (inner_loop != old_inner_loop && inner_loop != outer_loop);
	return inner_loop != old_inner_loop;
}

static bool block_is_inside_loop(ir_node *const block, ir_loop *const loop)
{
	ir_loop *const block_loop = get_irn_loop(block);
	if (block_loop == NULL)
		return false;
	return block_loop == loop || is_inner_loop(loop, block_loop);
}

static bool block_dominates_loop(ir_node *const block, ir_loop *const loop)
{
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_node) {
			assert(is_Block(element.node));
			if (!block_dominates(block, element.node))
				return false;
		} else if (*element.kind == k_ir_loop) {
			if (!block_dominates_loop(block, element.son))
				return false;
		}
	}
	return true;
}

// returns the block that dominates all blocks in the loop or NULL
static ir_node *get_loop_header(ir_loop *const loop)
{
	// pick a random block
	ir_node *header = NULL;
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_node) {
			header = element.node;
			break;
		}
	}
	assert(header && is_Block(header));

	// walk up the dominance tree
	ir_node *idom = get_Block_idom(header);
	while (idom && block_is_inside_loop(idom, loop)) {
		header = idom;
		idom = get_Block_idom(header);
	}

	return block_dominates_loop(header, loop) ? header : NULL;
}

static void duplicate_node(ir_node *const node, ir_node *const new_block)
{
	ir_node *const new_node = exact_copy(node);
	set_nodes_block(new_node, new_block);
	set_irn_link(node, new_node);
	set_irn_link(new_node, node);
	DB((dbg, LEVEL_3, "duplicating node %N (%n), new node %N\n", node, node, new_node));
}

static void rewire_successor_phi(ir_node *const phi, ir_node *const block, int arity, int new_arity)
{
	ir_node **const in = ALLOCAN(ir_node *, new_arity);
	for (int i = 0, j = arity; i < arity; ++i) {
		ir_node *const pred_phi       = get_irn_n(phi, i);
		ir_node *const new_pred_phi   = get_irn_link(pred_phi);
		ir_node *const pred_block     = get_irn_n(block, i);
		ir_node *const new_pred_block = get_irn_link(pred_block);

		in[i] = pred_phi;
		if (new_pred_block && new_pred_block != pred_block) {
			in[j++] = new_pred_phi ? new_pred_phi : pred_phi;
		}
	}
	set_irn_in(phi, new_arity, in);
}

static void rewire_successor_block(ir_node *const block)
{
	if (irn_visited(block))
		return;

	int const arity = get_irn_arity(block);

	// find the new arity
	int new_arity = arity;
	for (int i = 0; i < arity; ++i) {
		ir_node *const pred_block     = get_irn_n(block, i);
		ir_node *const new_pred_block = get_irn_link(pred_block);
		if (new_pred_block && new_pred_block != pred_block)
			++new_arity;
	}

	// rewire phis inside the block
	unsigned int const n_outs = get_irn_n_outs(block);
	for (unsigned int i = 0; i < n_outs; ++i) {
		ir_node *const node = get_irn_out(block, i);
		if (is_Phi(node))
			rewire_successor_phi(node, block, arity, new_arity);
	}

	ir_node **const in = ALLOCAN(ir_node *, new_arity);
	for (int i = 0, j = arity; i < arity; ++i) {
		ir_node *const pred_block     = get_irn_n(block, i);
		ir_node *const new_pred_block = get_irn_link(pred_block);

		in[i] = pred_block;
		if (new_pred_block && new_pred_block != pred_block) {
			in[j++] = new_pred_block;
		}
	}
	set_irn_in(block, new_arity, in);

	mark_irn_visited(block);
}

static void rewire_node(ir_node *const node, ir_node *const header)
{
	ir_node *const new_node = get_irn_link(node);
	assert(new_node);
	assert(get_irn_arity(node) == get_irn_arity(new_node));

	// rewire the successors outside the loop
	unsigned int const n_outs = get_irn_n_outs(node);
	for (unsigned int i = 0; i < n_outs; ++i) {
		ir_node *const succ = get_irn_out(node, i);
		if (get_irn_link(succ) == NULL && is_Block(succ)) {
			rewire_successor_block(succ);
		}
	}

	// loop header block
	if (node == header) {
		assert(is_Block(node));
		int const arity = get_irn_arity(node);
		int new_arity = 0;
		for (int i = 0; i < arity; ++i) {
			if (is_backedge(node, i)) {
				++new_arity;
			}
		}
		ir_node **const in = ALLOCAN(ir_node *, new_arity);
		for (int i = 0, j = 0; i < arity; ++i) {
			if (is_backedge(node, i)) {
				ir_node *const pred     = get_irn_n(new_node, i);
				ir_node *const new_pred = get_irn_link(pred);
				assert(new_pred);
				// jump to the old node from outside and from the new node
				set_irn_n(node, i, new_pred);
				// jump to the new node only from the old node
				in[j++] = pred;
			}
		}
		set_irn_in(new_node, new_arity, in);
		return;
	}

	// phi node inside loop header
	if (is_Phi(node) && get_nodes_block(node) == header) {
		int const arity = get_irn_arity(node);
		int new_arity = 0;
		for (int i = 0; i < arity; ++i) {
			if (is_backedge(get_nodes_block(node), i)) {
				++new_arity;
			}
		}
		ir_node **const in = ALLOCAN(ir_node *, 1);
		for (int i = 0, j = 0; i < arity; ++i) {
			if (is_backedge(get_nodes_block(node), i)) {
				ir_node *const pred     = get_irn_n(node, i);
				ir_node *const new_pred = get_irn_link(pred);
				if (new_pred) {
					set_irn_n(node, i, new_pred);
				}
				in[j++] = pred;
			}
		}
		set_irn_in(new_node, new_arity, in);
		return;
	}

	// TODO: use foreach_irn_in
	int const arity = get_irn_arity(new_node);
	for (int i = 0; i < arity; ++i) {
		ir_node *const pred     = get_irn_n(new_node, i);
		ir_node *const new_pred = get_irn_link(pred);
		assert(!is_backedge(node, i));
		if (new_pred != NULL) {
			set_irn_n(new_node, i, new_pred);
		}
	}
}

static void duplicate_block(ir_node *const block)
{
	ir_node *const new_block = exact_copy(block);
	set_irn_link(block, new_block);
	set_irn_link(new_block, block);
	DB((dbg, LEVEL_3, "duplicating block %N, new block %N\n", block, new_block));

	unsigned int const n_outs = get_irn_n_outs(block);
	for (unsigned int i = 0; i < n_outs; ++i) {
		ir_node *const node = get_irn_out(block, i);
		assert(!is_Block(node));
		if (get_nodes_block(node) != block)
			continue;
		duplicate_node(node, new_block);
	}
}

static void rewire_block(ir_node *const block, ir_node *const header)
{
	rewire_node(block, header);
	unsigned int const n_outs = get_irn_n_outs(block);
	for (unsigned int i = 0; i < n_outs; ++i) {
		ir_node *const node = get_irn_out(block, i);
		assert(!is_Block(node));
		if (get_nodes_block(node) != block)
			continue;
		rewire_node(node, header);
	}
}

static void duplicate_loop(ir_loop *const loop)
{
	ir_node *const header = get_loop_header(loop);
	if (header == NULL)
		return;
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
	inc_irg_visited(get_irn_irg(header));
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_node) {
			assert(is_Block(element.node));
			rewire_block(element.node, header);
		}
	}
}

// duplicate one innermost loop, for testing purposes
static void duplicate_one_loop(ir_loop *const loop, bool outermost)
{
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_loop) {
			duplicate_one_loop(element.son, false);
			return;
		}
	}
	if (!outermost)
		duplicate_loop(loop);
}

void do_loop_unrolling2(ir_graph *const irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.loop-unrolling");
	assure_lcssa(irg);
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO | IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	duplicate_one_loop(get_irg_loop(irg), true);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	dump_ir_graph(irg, "loop-unrolling");
}
