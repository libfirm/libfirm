#include "important_args.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irnode.h"
#include "irnode_t.h"
#include "irouts.h"
#include "irouts_t.h"
#include "irnodehashmap.h"
#include "pdeq.h"

#define foreach_irn_data_in(node, i, operand)                                  \
	foreach_irn_in ((node), i, operand)                                        \
		if (get_irn_mode(operand) == mode_M) {                                 \
		} else

static size_t n_args;
static ir_nodehashmap_t arg_deps;
static struct obstack obst;

static bitset_t *bitset_safe_or(bitset_t *tgt, const bitset_t *src)
{
	assert(tgt != NULL);
	if (src != NULL)
		bitset_or(tgt, src);
	return tgt;
}

static bitset_t *arg_deps_get(const ir_node *node)
{
	return ir_nodehashmap_get(bitset_t, &arg_deps, node);
}

static bitset_t *arg_deps_create(ir_node *node)
{
	bitset_t *const empty_set = bitset_obstack_alloc(&obst, n_args);
	ir_nodehashmap_insert(&arg_deps, node, empty_set);
	return empty_set;
}

static bitset_t *arg_deps_get_or_create(ir_node *node)
{
	bitset_t *deps = arg_deps_get(node);
	if (deps == NULL) {
		deps = arg_deps_create(node);
	}
	return deps;
}

static size_t arg_deps_update(ir_node *node)
{
	bitset_t *deps        = arg_deps_get_or_create(node);
	const size_t old_size = bitset_popcount(deps);

	foreach_irn_data_in (node, i, operand) {
		bitset_safe_or(deps, arg_deps_get(operand));
	}

	return bitset_popcount(deps) - old_size;
}

static void init_constants(ir_node *node, UNUSED void *env)
{
	if (!is_irn_constlike(node)) return;
	arg_deps_create(node);
}

static void worklist_add_outs(pdeq *worklist, const ir_node *node)
{
	foreach_irn_out (node, i, user) {
		if (get_irn_mode(user) == mode_M) continue;
		pdeq_putr(worklist, user);
	}
}

static bool node_needs_more_info(const ir_node *node)
{
	if (is_Phi(node)) return false;

	foreach_irn_data_in (node, i, operand) {
		if (arg_deps_get(operand) == NULL) return true;
	}
	return false;
}

static bool node_is_important(const ir_node *node)
{
	return is_Cond(node) || is_Load(node) || is_Store(node) || is_Call(node) ||
	       is_Switch(node);
}

static void important_args_update(bitset_t *important_args, const ir_node *node)
{
	if (node_is_important(node)) {
		bitset_or(important_args, arg_deps_get(node));
	}
}

bitset_t *local_important_args(ir_graph *irg)
{
	assure_irg_outs(irg);
	obstack_init(&obst);
	ir_nodehashmap_init(&arg_deps);

	const ir_node *const args      = get_irg_args(irg);
	n_args                         = get_irn_n_outs(args);
	bitset_t *const important_args = bitset_malloc(n_args);
	pdeq *const worklist           = new_pdeq();

	// Set arg_deps(c) to {} for all constant nodes c
	irg_walk_graph(irg, init_constants, NULL, NULL);

	// Initialize worklist with all argument users
	foreach_irn_out (args, i, arg) {
		bitset_t *const deps = arg_deps_create(arg);
		bitset_set(deps, get_Proj_num(arg));

		worklist_add_outs(worklist, arg);
	}

	// Do a fixpoint iteration
	while (!pdeq_empty(worklist)) {
		ir_node *const node = pdeq_getl(worklist);

		if (node_needs_more_info(node)) continue;

		// Update arg_deps(node) and continue if it didn't change
		if (!arg_deps_update(node)) continue;

		important_args_update(important_args, node);
		worklist_add_outs(worklist, node);
	}

	// Cleanup
	obstack_free(&obst, 0);
	del_pdeq(worklist);

	return important_args;
}
