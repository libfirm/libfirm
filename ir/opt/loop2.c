#include "firm.h"
#include "xmalloc.h"
#include "debug.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static char const *kind_get_string(firm_kind const *const kind)
{
	switch (*kind) {
	case k_BAD: return "k_BAD";
	case k_entity: return "k_entity";
	case k_type: return "k_type";
	case k_ir_graph: return "k_ir_graph";
	case k_ir_node: return "k_ir_node";
	case k_ir_mode: return "k_ir_mode";
	case k_tarval: return "k_tarval";
	case k_ir_loop: return "k_ir_loop";
	case k_ir_max: return "k_ir_max";
	default: return "";
	}
}

static void assure_dominance(ir_graph *irg)
{
	if (irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE))
		return;
	compute_doms(irg);
}

static int is_inside_loop(ir_node const *const node)
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
	DB((dbg, LEVEL_3, "inserting phi %ld\n", get_irn_node_nr(phi)));
	return phi;
}

// insert phi nodes for the edge between node and its nth predecessor
static void insert_phis_for_edge(ir_node *node, int n)
{
	ir_node *const pred = get_irn_n(node, n);
	if (!mode_is_data(get_irn_mode(pred))) return;
	ir_node *const pred_block = get_nodes_block(pred);
	if (!is_inside_loop(pred_block)) return;
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

static void insert_phis(ir_node *const node, void *const env)
{
	(void)env;
	if (is_Block(node))
		return;
	int const arity = get_irn_arity(node);
	for (int i = 0; i < arity; ++i) {
		insert_phis_for_edge(node, i);
	}
}

static void print_loop(ir_loop const *const loop, int const indentation)
{
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		for (int j = 0; j < indentation; ++j) printf("  ");
		printf("%s\n", kind_get_string(element.kind));
		if (*element.kind == k_ir_loop) {
			print_loop(element.son, indentation + 1);
		}
	}
}

void do_loop_unrolling2(ir_graph *const irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.lcssa");
	assure_loopinfo(irg);
	assure_dominance(irg);
	irg_walk_graph(irg, insert_phis, NULL, NULL);
}
