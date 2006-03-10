
#include "pmap.h"

#include "irnode_t.h"
#include "irhooks.h"
#include "irgwalk.h"

#include "survive_dce.h"

struct _survive_dce_t {
	pmap *places;
	pmap *new_places;
	hook_entry_t dead_node_elim;
	hook_entry_t dead_node_elim_subst;
};

static void dead_node_hook(void *context, ir_graph *irg, int start)
{
	survive_dce_t *sd = context;

	/* Create a new map before the dead node elimination is performed. */
	if(start) {
		sd->new_places = pmap_create_ex(pmap_count(sd->places));
	}

	/* Patch back all nodes if dead node elimination is over and something is to be done. */
	else {
		pmap_destroy(sd->places);
		sd->places     = sd->new_places;
		sd->new_places = NULL;
	}
}

static void dead_node_subst_hook(void *context, ir_graph *irg, ir_node *old, ir_node *nw)
{
	survive_dce_t *sd = context;
	ir_node **place    = (ir_node **) pmap_get(sd->places, old);

	/* If the node is to be patched back, do it. */
	if(place) {
		*place = nw;
		pmap_insert(sd->new_places, nw, (void *) place);
	}
}

survive_dce_t *new_survive_dce(void)
{
	survive_dce_t *res = xmalloc(sizeof(res[0]));
	res->places     = pmap_create();
	res->new_places = NULL;

	res->dead_node_elim.hook._hook_dead_node_elim = dead_node_hook;
	res->dead_node_elim.context                   = res;
	res->dead_node_elim.next                      = NULL;

	res->dead_node_elim_subst.hook._hook_dead_node_elim_subst = dead_node_subst_hook;
	res->dead_node_elim_subst.context = res;
	res->dead_node_elim_subst.next    = NULL;

	register_hook(hook_dead_node_elim, &res->dead_node_elim);
	register_hook(hook_dead_node_elim_subst, &res->dead_node_elim_subst);
	return res;
}

void free_survive_dce(survive_dce_t *sd)
{
	pmap_destroy(sd->places);
	unregister_hook(hook_dead_node_elim, &sd->dead_node_elim);
	unregister_hook(hook_dead_node_elim_subst, &sd->dead_node_elim_subst);
	free(sd);
}

void survive_dce_register_irn(survive_dce_t *sd, ir_node **place)
{
	if(*place != NULL)
		pmap_insert(sd->places, *place, (void *) place);
}

void survive_dce_register_pmap(survive_dce_t *sd, pmap *m)
{
	pmap_entry *ent;

	for(ent = pmap_first(m); ent; ent = pmap_next(m))
		survive_dce_register_irn(sd, (ir_node **) &ent->value);
}
