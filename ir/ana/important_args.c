#include "important_args.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irnode.h"
#include "irnode_t.h"
#include "irouts.h"
#include "irouts_t.h"
#include "irnodehashmap.h"
#include "pdeq.h"
#include "pmap.h"
#include "callgraph.h"
#include "cgana.h"
#include "call_sites.h"
#include "debug.h"

#define foreach_irn_data_in(node, i, operand)                                  \
	foreach_irn_in ((node), i, operand)                                        \
		if (get_irn_mode(operand) == mode_M) {                                 \
		} else

static size_t n_args;
static ir_nodehashmap_t arg_deps;
static struct obstack obst;

static size_t get_irg_n_args(const ir_graph *irg)
{
	const ir_entity *const ent = get_irg_entity(irg);
	const ir_type *const mtp   = get_entity_type(ent);
	return get_method_n_params(mtp);
}

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

static bool node_produces_value(const ir_node *node)
{
	return mode_is_data(get_irn_mode(node)) || is_Div(node) || is_Load(node);
}

static void worklist_add_outs(deq_t *worklist, const ir_node *node)
{
	foreach_irn_out (node, i, user) {
		if (node_produces_value(user))
			deq_push_pointer_right(worklist, user);
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
	foreach_irn_out (node, i, user) {
		if (is_Cond(user) || is_Load(user) || is_Switch(user) ||
		    (is_Call(user) && get_Call_ptr(user) == node) ||
		    (is_Store(user) && get_Store_ptr(user) == node)) {
			return true;
		}
	}
	return false;
}

static void important_args_update(bitset_t *important_args, const ir_node *node)
{
	if (node_is_important(node)) {
		bitset_or(important_args, arg_deps_get(node));
	}
}

// Complexity: O(n|V|), where n is the number of parameters of the procedure.
// There is potentially a set for every node in the graph. New items are only
// added to the work list if a set changes. Each set changes a maximum of n
// times.
bitset_t *local_important_args(ir_graph *irg)
{
	// TODO remove when we get an instance passed in
	obstack_init(&obst);

	assure_irg_outs(irg);
	ir_nodehashmap_init(&arg_deps);

	const ir_node *const args      = get_irg_args(irg);
	n_args                         = get_irg_n_args(irg);
	bitset_t *const important_args = bitset_malloc(n_args);

	deq_t wl;
	deq_t *const worklist = &wl;
	deq_init(worklist);

	// Set arg_deps(c) to {} for all constant nodes c
	irg_walk_graph(irg, init_constants, NULL, NULL);

	// Initialize worklist with all argument users
	foreach_irn_out (args, i, arg) {
		bitset_t *const deps = arg_deps_create(arg);
		bitset_set(deps, get_Proj_num(arg));

		important_args_update(important_args, arg);
		worklist_add_outs(worklist, arg);
	}

	// Do a fixpoint iteration
	while (!deq_empty(worklist)) {
		ir_node *const node = deq_pop_pointer_left(ir_node, worklist);

		if (node_needs_more_info(node)) continue;

		// Update arg_deps(node) and continue if it didn't change
		if (!arg_deps_update(node)) continue;

		important_args_update(important_args, node);
		worklist_add_outs(worklist, node);
	}

	// Cleanup
	deq_free(worklist);

	return important_args;
}

// Interprocedural part of the analysis

typedef struct order_t {
	ir_graph **irgs;
	size_t last;
} order_t;

static size_t n_irgs = 0;

static void build_topo_order(ir_graph *g, void *env)
{
	assert(g != NULL);
	order_t *order             = (order_t *)env;
	order->irgs[--order->last] = g;
}

pmap *important_args_get()
{
	// register a debug mask
	DEBUG_ONLY(firm_dbg_module_t *dbg = NULL);
	FIRM_DBG_REGISTER(dbg, "ir.ana.important_args");

	obstack_init(&obst);
	n_irgs                     = get_irp_n_irgs();
	pmap *const important_args = pmap_create_ex(n_irgs);
	assure_callgraph_consistent();

	// Get call site info
	call_sites_t call_sites;
	call_sites_init(&call_sites);

	// Create topological ordering of the call graph
	order_t order = {NEW_ARR_DZ(ir_graph *, &obst, n_irgs), n_irgs};
	callgraph_walk(NULL, build_topo_order, &order);
	assert(order.last == 0);

	// Walk the call graph in reverse topological order
	for (size_t i = n_irgs - 1; i != (size_t)-1; --i) {
		ir_graph *const irg  = order.irgs[i];
		bitset_t *const vips = local_important_args(irg);
		pmap_insert(important_args, irg, vips);
		DB((dbg, LEVEL_2, "Local important args of proc %s: %B\n",
		    get_entity_name(get_irg_entity(irg)), vips));

		foreach_call_from (&call_sites, irg, call) {
			assert(call != NULL);
			const ir_graph *const callee = call_site_get_callee_irg(call);
			// The callee might be from a different compilation unit or even
			// completely unknown (e.g. a call using a function pointer)
			if (callee == NULL) continue;

			const bitset_t *const callee_vips =
			    pmap_get(const bitset_t, important_args, callee);
			// This is safe to do since we do not optimize non-trivial cycles
			if (callee_vips == NULL) continue;

			bitset_foreach (callee_vips, p) {
				const ir_node *const argument = get_Call_param(call, p);
				bitset_safe_or(vips, arg_deps_get(argument));
			}
		}
		DB((dbg, LEVEL_1, "Important args of proc %s: %B\n",
		    get_entity_name(get_irg_entity(irg)), vips));
	}

	call_sites_destroy(&call_sites);
	obstack_free(&obst, 0);

	return important_args;
}
