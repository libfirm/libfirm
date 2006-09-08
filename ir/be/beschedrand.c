/**
 * Trivial node selector.
 * @author Christian Wuerdig
 * @date   29.08.2006
 * @cvs-id $Id$
 */

#include <stdlib.h>

#include "besched_t.h"
#include "belistsched.h"

/**
 * The random selector:
 * Just assure that branches are executed last, otherwise select a random node
 */
static ir_node *random_select(void *block_env, nodeset *ready_set, nodeset *live_set)
{
	const arch_env_t *arch_env = block_env;
	ir_node          *irn      = NULL;
	int only_branches_left = 1;

	/* assure that branches and constants are executed last */
	for (irn = nodeset_first(ready_set); irn; irn = nodeset_next(ready_set)) {
		if (! arch_irn_class_is(arch_env, irn, branch)) {
			only_branches_left = 0;
			nodeset_break(ready_set);
			break;
		}
	}

	if(only_branches_left) {
		/* at last: schedule branches */
		irn = nodeset_first(ready_set);
		nodeset_break(ready_set);
	} else {
		do {
			// take 1 random node
			int n = rand() % pset_count(ready_set);
			int i = 0;
			for(irn = nodeset_first(ready_set); irn; irn = nodeset_next(ready_set)) {
				if(i == n) {
					nodeset_break(ready_set);
					break;
				}
				++i;
			}
		} while(arch_irn_class_is(arch_env, irn, branch));
	}

	return irn;
}

static void *random_init_graph(const list_sched_selector_t *vtab, const arch_env_t *arch_env, ir_graph *irg)
{
	srand(time(0));
	return (void *)arch_env;
}

static void *random_init_block(void *graph_env, ir_node *bl)
{
	return graph_env;
}

static const list_sched_selector_t random_selector_struct = {
	random_init_graph,
	random_init_block,
	random_select,
	NULL,                /* to_appear_in_schedule */
	NULL,                /* node_ready */
	NULL,                /* node_selected */
	NULL,                /* exectime */
	NULL,                /* latency */
	NULL,                /* finish_block */
	NULL                 /* finish_graph */
};

const list_sched_selector_t *random_selector = &random_selector_struct;
