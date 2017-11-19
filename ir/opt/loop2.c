#include "iroptimize.h"
#include "irgraph_t.h"
#include "irloop_t.h"

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
	assure_loopinfo(irg);
	ir_loop *loop = get_irg_loop(irg);
	if (loop) {
		print_loop(loop, 0);
	}
}
