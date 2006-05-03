#include <assert.h>
#include <stdlib.h>
#include "irdom.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irnode.h"
#include "pmap.h"
#include "cdep.h"
#include "irprintf.h"

typedef unsigned int uint;

static pmap* cdep_map;

cdep* find_cdep(const ir_node* block)
{
  return pmap_get(cdep_map, (void*)block);
}


static void add_cdep(ir_node* node, ir_node* dep_on)
{
  cdep* dep = find_cdep(node);
#if 0
	ir_fprintf(stderr, "Adding cdep of %+F on %+F\n", node, dep_on);
#endif

  if (dep == NULL) {
    cdep* newdep = malloc(sizeof(*newdep));

    newdep->node = dep_on;
    newdep->next = NULL;
    pmap_insert(cdep_map, node, newdep);
  } else {
    cdep* newdep;

    for (;;) {
      if (dep->node == dep_on) return;
      if (dep->next == NULL) break;
      dep = dep->next;
    }
    newdep = malloc(sizeof(*newdep));
    newdep->node = dep_on;
    newdep->next = NULL;
    dep->next = newdep;
  }
}


static void cdep_pre(ir_node* node, void* env)
{
  uint n;
  uint i;

  /* special case:
   * start and end block have no control depency
   */
  if (node == get_irg_start_block(get_irn_irg(node))) return;
  if (node == get_irg_end_block(get_irn_irg(node))) return;

  n = get_Block_n_cfgpreds(node);
  for (i = 0; i < n; i++) {
    ir_node* pred = get_Block_cfgpred_block(node, i);
    ir_node* pdom;
    ir_node* dependee;

    if (is_Bad(pred)) continue;

    pdom = get_Block_ipostdom(pred);
    for (dependee = node; dependee != pdom; dependee = get_Block_ipostdom(dependee)) {
      assert(!is_Bad(pdom));
      add_cdep(dependee, pred);
    }
  }
}


#include "irdump.h"


static int cdep_edge_hook(FILE* F, ir_node* block)
{
#if 0
	ir_node* pdom;
#endif
  cdep* cd;

#if 0
	pdom = get_Block_ipostdom(block);
	if (pdom != NULL) {
		fprintf(
			F,
			"edge:{sourcename:\"n%ld\" targetname:\"n%ld\" color:gold}\n",
			get_irn_node_nr(pdom), get_irn_node_nr(block)
		);
	}
#endif

  for (cd = find_cdep(block); cd != NULL; cd = cd->next) {
    fprintf(
      F,
			"edge:{sourcename:\"n%ld\" targetname:\"n%ld\" "
			"linestyle:dashed color:gold}\n",
			get_irn_node_nr(block), get_irn_node_nr(cd->node)
		);
  }

	return 0;
}


void compute_cdep(ir_graph* irg)
{
  cdep_map = pmap_create();

  compute_postdoms(irg);
	set_Block_ipostdom(get_irg_start_block(irg), get_irg_end_block(irg));

  irg_block_walk_graph(irg, cdep_pre, NULL, NULL);

#if 1
	set_dump_block_edge_hook(cdep_edge_hook);
  dump_ir_block_graph(irg, "_cdep");
	set_dump_block_edge_hook(NULL);
#endif

  free_postdom(irg);
}


void free_cdep(ir_graph* irg)
{
  // TODO atm leaking more memory than a small memory leaking animal
}


int is_iterated_cdep_on(ir_node* dependee, ir_node* candidate)
{
	const cdep* dep;

	while ((dep = find_cdep(dependee)) != NULL) {
		if (dep->next != NULL) return 0;
		if (dep->node == candidate) return 1;
		dependee = dep->node;
	}
	return 0;
}


ir_node* get_unique_cdep(const ir_node* block)
{
	cdep* cdep = find_cdep(block);

	return cdep != NULL && cdep->next == NULL ? cdep->node : NULL;
}


int has_multiple_cdep(const ir_node* block)
{
	cdep* cdep = find_cdep(block);

	return cdep != NULL && cdep->next != NULL;
}
