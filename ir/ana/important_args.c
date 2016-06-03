#include "important_args.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irnode.h"
#include "irnode_t.h"
#include "irouts.h"
#include "irouts_t.h"
#include "irnodehashmap.h"
#include "pdeq.h"

static unsigned n_args;
static ir_nodehashmap_t arg_deps;
static struct obstack obst;

static void init_constants(ir_node *node, void *env)
{
	if (!is_Const(node)) return;

	bitset_t *const empty_set = bitset_obstack_alloc(&obst, n_args);
	ir_nodehashmap_insert((ir_nodehashmap_t *)env, node, empty_set);
}

static void worklist_add_outs(pdeq *worklist, const ir_node *node)
{
	foreach_irn_out (node, i, user) {
		pdeq_putr(worklist, user);
	}
}

static bool needs_more_info(ir_node *const node)
{
	if (is_Phi(node)) return false;

	foreach_irn_in (node, i, dep_node) {
		if (ir_nodehashmap_get(bitset_t, &arg_deps, dep_node) == NULL) {
			return true;
		}
	}
	return false;
}

static bool is_important_node(ir_node *const node)
{
	return is_Cond(node);
}

bitset_t *local_important_args(ir_graph *irg)
{
	assure_irg_outs(irg);
	obstack_init(&obst);

	const ir_node *const args      = get_irg_args(irg);
	n_args                         = get_irn_n_outs(args);
	bitset_t *const important_args = bitset_malloc(n_args);

	ir_nodehashmap_init(&arg_deps);
	irg_walk_graph(irg, init_constants, NULL, &arg_deps);

	pdeq *const worklist = new_pdeq();
	foreach_irn_out (args, i, arg) {
		bitset_t *const deps = bitset_obstack_alloc(&obst, n_args);
		bitset_set(deps, get_Proj_num(arg));
		ir_nodehashmap_insert(&arg_deps, arg, deps);

		worklist_add_outs(worklist, arg);
	}

	while (!pdeq_empty(worklist)) {
		ir_node *const node = pdeq_getl(worklist);

		if (needs_more_info(node)) continue;

		bitset_t *deps = ir_nodehashmap_get(bitset_t, &arg_deps, node);
		if (deps == NULL) {
			deps = bitset_obstack_alloc(&obst, n_args);
			ir_nodehashmap_insert(&arg_deps, node, deps);
		}
		const size_t old_size = bitset_popcount(deps);

		foreach_irn_in (node, i, dep_node) {
			bitset_t *const dep_deps =
			    ir_nodehashmap_get(bitset_t, &arg_deps, dep_node);
			if (dep_deps == NULL) continue;
			bitset_or(deps, dep_deps);
		}
		if (bitset_popcount(deps) == old_size) continue;

		if (is_important_node(node)) {
			bitset_or(important_args, deps);
		}
		worklist_add_outs(worklist, node);
	}

	obstack_free(&obst, 0);
	del_pdeq(worklist);
	return important_args;
}
