
#include <stdio.h>

#include "irgraph.h"
#include "irgwalk.h"
#include "irprintf.h"

#include "beutil.h"
#include "besched_t.h"
#include "bera_t.h"

static void dump_allocated_block(ir_node *block, void *env)
{
	int i, n;
	const ir_node *irn;
	FILE *f = env;

	ir_fprintf(f, "node:{title:\"b%N\"\nlabel:\"%n\n", block, block);
	sched_foreach(block, irn) {
		const char *prefix = "";

		ir_fprintf(f, "\n");
		if(is_color(get_irn_color(irn)))
			ir_fprintf(f, "r%d = ", get_irn_color(irn));
		ir_fprintf(f, "%n(", irn);

		if(block != get_irg_start_block(get_irn_irg(block))) {
			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);
				if(is_allocatable_irn(op)) {
					ir_fprintf(f, "%sr%d", prefix, get_irn_color(op));
					prefix = ", ";
				}
			}
		}

		ir_fprintf(f, ")");
	}
	ir_fprintf(f, "\"}\n");

	if(get_irg_start_block(get_irn_irg(block)) != block) {
		for(i = 0, n = get_irn_arity(block); i < n; ++i) {
			ir_node *pred_bl = get_nodes_block(get_irn_n(block, i));
			ir_fprintf(f, "edge:{sourcename:\"b%N\" targetname:\"b%N\"}\n", pred_bl, block);
		}
	}
}

void dump_allocated_irg(ir_graph *irg)
{
	char buf[1024];
	FILE *f;

	snprintf(buf, sizeof(buf), "%s-alloc.vcg", get_entity_name(get_irg_entity(irg)));

	if((f = fopen(buf, "wt")) != NULL) {
		fprintf(f, "graph:{title:\"prg\"\n");
		irg_block_walk_graph(irg, dump_allocated_block, NULL, f);
		fprintf(f, "}\n");
		fclose(f);
	}
}
