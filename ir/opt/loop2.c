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

void do_loop_unrolling2(ir_graph *const irg)
{
	printf("test\n");
}
