
#include "irprintf.h"
#include "irgwalk.h"
#include "irnode.h"

#include "besched.h"
#include "belistsched.h"

size_t sched_irn_data_offset = 0;

static void block_sched_dumper(ir_node *block, void *env)
{
	FILE *f = env;
	const ir_node *curr;

	ir_fprintf(f, "%n:\n", block);
	sched_foreach(block, curr) {
		ir_fprintf(f, "\t%n\n", curr);
	}
}

void be_sched_dump(FILE *f, const ir_graph *irg)
{
	irg_block_walk_graph((ir_graph *) irg, block_sched_dumper, NULL, f);
}

void be_sched_init(void)
{
	sched_irn_data_offset = register_additional_node_data(sizeof(sched_info_t));
}

void be_sched_test(void)
{
	int i, n;
	struct obstack obst;

	obstack_init(&obst);

	for(i = 0, n = get_irp_n_irgs(); i < n; ++i) {
		ir_graph *irg = get_irp_irg(i);

		list_sched(irg, trivial_selector, NULL);
		be_sched_dump(stdout, irg);
	}

	obstack_free(&obst, NULL);
}
