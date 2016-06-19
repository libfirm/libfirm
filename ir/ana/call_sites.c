#include "call_sites.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irnode.h"
#include "array.h"
#include "irprog.h"
#include "pdeq.h"

// Add `call` to the collection of calls stored in `map` under `key`
// Expected O(1)
static void call_map_add(pmap *map, const ir_graph *key, ir_node *call)
{
	ir_node **call_sites = pmap_get(ir_node *, map, key);
	if (call_sites == NULL) {
		call_sites = NEW_ARR_F(ir_node *, 0);
	}
	ARR_APP1(ir_node *, call_sites, call);
	pmap_insert(map, key, call_sites);
}

// O(n), with n = size(pmap)
static void call_map_destroy(pmap *map)
{
	foreach_pmap (map, entry) {
		DEL_ARR_F(entry->value);
	}
}

// Apparently O(1)
ir_graph *call_site_get_callee_irg(const ir_node *call)
{
	assert(call != NULL);
	const ir_entity *const callee_ent = get_Call_callee(call);
	if (callee_ent == NULL) return NULL;

	return get_entity_linktime_irg(callee_ent);
}

// Expected O(1)
static void register_call(ir_node *node, void *env)
{
	assert(node != NULL);
	assert(env != NULL);
	if (!is_Call(node)) return;

	call_sites_register_call((call_sites_t *)env, node);
}

// Expected O(n) with n = count of all ir_nodes in all ir_graphs
call_sites_t *call_sites_get()
{
	return call_sites_init(xmalloc(sizeof(call_sites_t)));
}

// Expected O(n) with n = count of all ir_nodes in all ir_graphs
call_sites_t *call_sites_init(call_sites_t *call_sites)
{
	const size_t n_irgs = get_irp_n_irgs();
	*call_sites = (call_sites_t){.by_caller = pmap_create_ex(n_irgs),
	                             .by_callee = pmap_create_ex(n_irgs),
	                             .all       = NEW_ARR_F(ir_node *, 0)};
	all_irg_walk(register_call, NULL, call_sites);
	return call_sites;
}

// Free all members. `call_sites` itself has to be freed by user
// O(n), where n = number of irgs in the irp
void call_sites_destroy(call_sites_t *call_sites)
{
	call_map_destroy(call_sites->by_caller);
	call_map_destroy(call_sites->by_callee);
	DEL_ARR_F(call_sites->all);
}

// Expected O(1)
static ir_node **call_map_get_calls(const pmap *map, const ir_graph *key)
{
	assert(map != NULL);
	assert(key != NULL);
	return pmap_get(ir_node *, map, key);
}

// Expected O(1)
static size_t call_map_size(const pmap *map, const ir_graph *key)
{
	ir_node *const *const call_sites = call_map_get_calls(map, key);
	return call_sites == NULL ? 0 : ARR_LEN(call_sites);
}

// Expected O(1)
static ir_node *call_map_get_call(const pmap *map, const ir_graph *key,
                                  size_t pos)
{
	assert(pos < call_map_size(map, key));
	ir_node *const *const call_sites = call_map_get_calls(map, key);
	assert(call_sites != NULL);
	return call_sites[pos];
}

// Expected O(1)
size_t call_sites_get_n_calls_from(const call_sites_t *self,
                                   const ir_graph *caller)
{
	assert(self != NULL);
	return call_map_size(self->by_caller, caller);
}

// Expected O(1)
ir_node *call_sites_get_call_from(const call_sites_t *self,
                                  const ir_graph *caller, size_t pos)
{
	assert(self != NULL);
	return call_map_get_call(self->by_caller, caller, pos);
}

// Expected O(1)
ir_node **call_sites_get_calls_from(const call_sites_t *self,
                                    const ir_graph *caller)
{
	assert(self != NULL);
	return call_map_get_calls(self->by_caller, caller);
}

// Expected O(1)
size_t call_sites_get_n_calls_to(const call_sites_t *self,
                                 const ir_graph *callee)
{
	assert(self != NULL);
	return call_map_size(self->by_callee, callee);
}

// Expected O(1)
ir_node *call_sites_get_call_to(const call_sites_t *self,
                                const ir_graph *callee, size_t pos)
{
	assert(self != NULL);
	return call_map_get_call(self->by_callee, callee, pos);
}

// Expected O(1)
ir_node **call_sites_get_calls_to(const call_sites_t *self,
                                  const ir_graph *callee)
{
	assert(self != NULL);
	return call_map_get_calls(self->by_callee, callee);
}

// Expected O(1)
void call_sites_register_call(call_sites_t *call_sites, ir_node *call)
{
	assert(call_sites != NULL);
	assert(is_Call(call));

	const ir_graph *const caller = get_irn_irg(call);
	const ir_graph *const callee = call_site_get_callee_irg(call);

	call_map_add(call_sites->by_caller, caller, call);
	call_map_add(call_sites->by_callee, callee, call);
	ARR_APP1(ir_node *, call_sites->all, call);
}

// Expected O(n) with n = count of ir_nodes in irg
void call_sites_register_irg_calls(call_sites_t *call_sites, ir_graph *irg)
{
	assert(call_sites != NULL);
	assert(irg != NULL);
	irg_walk_graph(irg, register_call, NULL, call_sites);
}
