#include "obst.h"
#include "pset.h"
#include "irprintf_t.h"
#include "irgraph.h"
#include "irnode.h"
#include "irmode.h"
#include "irgwalk.h"
#include "iredges_t.h"
#include "ircons_t.h"
#include "irprintf.h"

#include "obst.h"
#include "bitset.h"

#include "irprog.h"
#include "irgopt.h"
#include "irdump.h"
#include "phiclass.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "irloop_t.h"
#include "irtools.h"
#include "return.h"

#include "bearch.h"
#include "firm/bearch_firm.h"
#include "ia32/bearch_ia32.h"
#include "arm/bearch_arm.h"
#include "ppc32/bearch_ppc32.h"
#include "mips/bearch_mips.h"

#include "be_t.h"
#include "benumb_t.h"
#include "beutil.h"
#include "benode_t.h"
#include "beirgmod.h"
#include "besched_t.h"
#include "belistsched.h"
#include "belive_t.h"
#include "bespillilp.h"
#include "bespillbelady.h"
#include "bera.h"
#include "beraextern.h"
#include "bechordal_t.h"
#include "beifg.h"
#include "beifg_impl.h"
#include "becopyopt.h"
#include "becopystat.h"
#include "bessadestr.h"
#include "beabi.h"
#include "belower.h"
#include "bestat.h"
#include "typewalk.h"
#include "error.h"



typedef struct _spilloc_env_t {
	struct obstack ob;
	pset *ents;
	int entcnt;
	ir_graph *irg;
	pmap *nodents;
	pmap *afgraph;
} spilloc_env_t;


typedef struct _vertex {
	entity *ent;
	int node_nr;
	pset *edges;
} vertex;



typedef struct _edge {
	entity *ent;
	unsigned int wt_val_interfere;
	unsigned int wt_con_interfere;
	struct edge *next;
} edge;


#define STRUCTSIZE sizeof(vertex)
#define NIL (-1)
#define ABSENT (-2)

static void add_ent(entity *ent, void *env) {
	spilloc_env_t *spi = env;
	entity *e = obstack_alloc(&spi->ob, sizeof(*e));

	e = ent;
	pset_insert_ptr(spi->ents, e);
	spi->entcnt++;
}

static void data_init(spilloc_env_t *env) {
	pset *ps = pset_new_ptr_default();
	entity *e;

	foreach_pset(env->ents, e) {
		pmap_insert(env->nodents, e, ps);
	}
}

static void get_entity_nodes(ir_node *irn, void *env) {
	entity *e, *ent;
	spilloc_env_t *spi = env;

	e = get_irn_entity_attr(irn);
	//get_entity_additional_properties();
	if (e)
	{
		foreach_pset(spi->ents, ent) {

 			if ((ent->nr == e->nr) && (ent->ld_name == e->ld_name) )
			{
				pset *ps = pmap_get(spi->nodents, ent);
				if (ps && (!pset_find_ptr(pmap_get(spi->nodents, ent),irn))) {
					pset_insert_ptr(pmap_get(spi->nodents, ent), irn);
				}
			}
		}
	}
}

pset *get_entity_irn(entity *ent, void *env) {
	spilloc_env_t *spi = env;

	pset *pirn = pmap_get(spi->nodents, ent);
	if (pirn)
	{
		return pirn;
	}
	return NULL;
}

int entities_interfere(entity *e1, entity *e2, void *env) {
	spilloc_env_t *spi = env;
	ir_node *n1, *n2;

	pset *pe1 = pmap_get(spi->nodents, e1);
	pset *pe2 = pmap_get(spi->nodents, e2);

	foreach_pset(pe1, n1) {
		foreach_pset(pe2, n2) {
			if (values_interfere(n1, n2)) return 1;
		}
	}
	return 0;
}

static void set_aff_graph(void *env) {
	spilloc_env_t *spi = env;
	entity *e, *cmp;
	vertex *v;
	int i = 1;

	foreach_pset(spi->ents, e) {
		vertex *ver = obstack_alloc(&spi->ob, sizeof(*ver));
		ver->edges = pset_new_ptr_default();
		ver->ent = e;
		ver->node_nr = i;
		pmap_insert(spi->afgraph, e, ver);
		i++;
	}

	foreach_pset(spi->ents, e) {
		foreach_pset(spi->ents, cmp) {
			if (entities_interfere(e, cmp, &spi) == 1) {
				v = pmap_get(spi->afgraph, e);
				pset_insert_ptr(v->edges, cmp);
			}
		}
	}
}

void be_spill_loc(const be_chordal_env_t *chordal_env) {

	spilloc_env_t spi;

	// create initial graph representing data structure
	obstack_init(&spi.ob);
	spi.ents = pset_new_ptr_default();
	spi.entcnt = 0;
	spi.irg = chordal_env->irg;
	spi.nodents = pmap_create();
	spi.afgraph = pmap_create();


	/* Walks over all entities in the type */
	walk_types_entities(get_irg_frame_type(chordal_env->irg), add_ent, &spi);

	data_init(&spi);
	irg_walk_blkwise_graph(chordal_env->irg, NULL, get_entity_nodes, &spi);


	/*clean*/
	del_pset(spi.ents);
	pmap_destroy(spi.nodents);
	pmap_destroy(spi.afgraph);
	obstack_free(&spi.ob, NULL);
}
