#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "debug.h"

#include "irnodeset.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "iredges_t.h"

#include "beirg.h"
#include "bespilloptions.h"
#include "bespill.h"
#include "bemodule.h"
#include "besched.h"
#include "bearch_t.h"
#include "be_t.h"
#include "beirg.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct daemel_env_t daemel_env_t;
struct daemel_env_t {
	spill_env_t                 *spill_env;
	int                          n_regs;
	const arch_env_t            *arch_env;
	const arch_register_class_t *cls;
	const be_lv_t               *lv;
	bitset_t                    *spilled_nodes;
};

typedef struct spill_candidate_t spill_candidate_t;
struct spill_candidate_t {
	double   costs;
	ir_node *node;
};

static
int compare_spill_candidates_desc(const void *d1, const void *d2)
{
	const spill_candidate_t *c1 = d1;
	const spill_candidate_t *c2 = d2;

	return (int) (c2->costs - c1->costs);
}

static
double get_spill_costs(daemel_env_t *env, ir_node *node)
{
	const ir_edge_t *edge;
	spill_env_t     *spill_env = env->spill_env;
	double           costs     = be_get_spill_costs(spill_env, node, node);

	foreach_out_edge(node, edge) {
		ir_node *use = get_edge_src_irn(edge);

		if(is_Phi(use)) {
			int      in         = get_edge_src_pos(edge);
			ir_node *block      = get_nodes_block(use);

			costs += be_get_reload_costs_on_edge(spill_env, node, block, in);
		} else {
			costs += be_get_reload_costs(spill_env, node, use);
		}
	}

	return costs;
}

/**
 * spills a node by placing a reload before each usage
 */
static
void spill_node(daemel_env_t *env, ir_node *node)
{
	const ir_edge_t *edge;
	spill_env_t     *spill_env       = env->spill_env;
	const arch_register_class_t *cls = env->cls;

	foreach_out_edge(node, edge) {
		ir_node *use = get_edge_src_irn(edge);

		if(is_Phi(use)) {
			int      in         = get_edge_src_pos(edge);
			ir_node *block      = get_nodes_block(use);

			be_add_reload_on_edge(spill_env, node, block, in, cls, 1);
		} else {
			be_add_reload(spill_env, node, use, cls, 1);
		}
	}

	bitset_set(env->spilled_nodes, get_irn_idx(node));
}

/**
 * spill @p n nodes from a nodeset. Removes the nodes from the nodeset and
 * sets the spilled bits in env->spilled_nodes.
 */
static
void spill_nodes(daemel_env_t *env, ir_nodeset_t *nodes)
{
	size_t                 node_count = ir_nodeset_size(nodes);
	int                    registers  = env->n_regs;
	spill_candidate_t     *candidates;
	ir_nodeset_iterator_t  iter;
	ir_node               *node;
	size_t                 i;
	int                    spills_needed = node_count - registers;

	if(spills_needed <= 0)
		return;

	candidates = malloc(node_count * sizeof(candidates[0]));

	/* construct array with spill candidates and calculate their costs */
	i = 0;
	foreach_ir_nodeset(nodes, node, iter) {
		spill_candidate_t *candidate = & candidates[i];

		candidate->node  = node;
		candidate->costs = get_spill_costs(env, node);
		i++;
	}
	assert(i == node_count);

	/* sort spill candidates */
	qsort(candidates, node_count, sizeof(candidates[0]),
	      compare_spill_candidates_desc);

	/* spill cheapest ones */
	DBG((dbg, LEVEL_2, "\tspills needed: %d\n", spills_needed));
	for(i = registers; i < node_count; ++i) {
		spill_candidate_t *candidate = &candidates[i];
		ir_node           *node      = candidate->node;

		DBG((dbg, LEVEL_3, "\tspilling %+F (costs %f)\n",
		     node, candidate->costs));

		spill_node(env, node);
	}

	free(candidates);
}

/**
 * make sure register pressure in a block is always equal or below the number
 * of available registers
 */
static
void spill_block(ir_node *block, void *data)
{
	daemel_env_t                *env           = data;
	const arch_env_t            *arch_env      = env->arch_env;
	const arch_register_class_t *cls           = env->cls;
	const be_lv_t               *lv            = env->lv;
	ir_nodeset_t                 live_nodes;
	ir_nodeset_iterator_t        iter;
	ir_node                     *node;
	bitset_t                    *spilled_nodes = env->spilled_nodes;

	DBG((dbg, LEVEL_1, "spilling block %+F\n", block));

	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block_ir_nodeset(lv, arch_env, cls, block, &live_nodes);

	foreach_ir_nodeset(&live_nodes, node, iter) {
		DBG((dbg, LEVEL_2, "\t%+F is live-in... ", node));
		if(bitset_is_set(spilled_nodes, get_irn_idx(node))) {
			DBG((dbg, LEVEL_2, "but spilled; removing.\n"));
		} else {
			DBG((dbg, LEVEL_2, "keeping.\n"));
		}
	}

	sched_foreach_reverse(block, node) {
		if(is_Phi(node))
			break;

		be_liveness_transfer_ir_nodeset(arch_env, cls, node, &live_nodes);

		spill_nodes(env, &live_nodes);
	}

	/* TODO: spill phis... */

	ir_nodeset_destroy(&live_nodes);
}

void be_spill_daemel(be_irg_t *birg, const arch_register_class_t *cls)
{
	daemel_env_t  env;
	ir_graph     *irg    = be_get_birg_irg(birg);
	int           n_regs = cls->n_regs - be_put_ignore_regs(birg, cls, NULL);

	if(n_regs == 0)
		return;

	be_invalidate_liveness(birg);
	be_assure_liveness(birg);

	env.spill_env     = be_new_spill_env(birg);
	env.n_regs        = n_regs;
	env.arch_env      = be_get_birg_arch_env(birg);
	env.cls           = cls;
	env.lv            = be_get_birg_liveness(birg);
	env.spilled_nodes = bitset_malloc(get_irg_last_idx(irg));

	irg_block_walk_graph(irg, spill_block, NULL, &env);

	bitset_free(env.spilled_nodes);

	be_insert_spills_reloads(env.spill_env);

	be_delete_spill_env(env.spill_env);
}

void be_init_daemelspill(void)
{
	static be_spiller_t daemel_spiller = {
		be_spill_daemel
	};

	be_register_spiller("daemel", &daemel_spiller);
	FIRM_DBG_REGISTER(dbg, "ir.be.spilldaemel");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_doedelspill);
