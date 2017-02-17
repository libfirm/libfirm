/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Implementation of cdep
 * @author  Christoph Mallon
 */
#include "cdep_t.h"

#include "irdom_t.h"
#include "irdump.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irprintf.h"
#include "obst.h"
#include "pmap.h"
#include "xmalloc.h"
#include <assert.h>
#include <stdlib.h>

typedef struct cdep_info {
	pmap          *cdep_map; /**< A map to find the list of all control dependence nodes for a block. */
	struct obstack obst;     /**< An obstack where all cdep data lives on. */
} cdep_info;

static cdep_info *cdep_data;

ir_node *(get_cdep_node)(const ir_cdep *cdep)
{
	return _get_cdep_node(cdep);
}

ir_cdep *(get_cdep_next)(const ir_cdep *cdep)
{
	return _get_cdep_next(cdep);
}

ir_cdep *find_cdep(const ir_node *block)
{
	assert(is_Block(block));
	return pmap_get(ir_cdep, cdep_data->cdep_map, block);
}

void exchange_cdep(ir_node *old, const ir_node *nw)
{
	ir_cdep *cdep = find_cdep(nw);
	assert(is_Block(old));
	pmap_insert(cdep_data->cdep_map, old, cdep);
}

/**
 * Adds a control dependence from node to dep_on.
 */
static void add_cdep(ir_node *node, ir_node *dep_on)
{
	ir_cdep *dep = find_cdep(node);

	assert(is_Block(dep_on));
	if (dep == NULL) {
		ir_cdep *newdep = OALLOC(&cdep_data->obst, ir_cdep);

		newdep->node = dep_on;
		newdep->next = NULL;
		pmap_insert(cdep_data->cdep_map, node, newdep);
	} else {
		ir_cdep *newdep;

		for (;;) {
			if (get_cdep_node(dep) == dep_on)
				return;
			if (dep->next == NULL)
				break;
			dep = dep->next;
		}
		newdep = OALLOC(&cdep_data->obst, ir_cdep);
		newdep->node = dep_on;
		newdep->next = NULL;
		dep->next = newdep;
	}
}

/**
 * Pre-block-walker: calculate the control dependence
 */
static void cdep_pre(ir_node *node, void *ctx)
{
	(void)ctx;
	for (int i = get_Block_n_cfgpreds(node); i-- > 0; ) {
		ir_node *pred = get_Block_cfgpred_block(node, i);
		if (pred == NULL)
			continue;

		ir_node *pdom = get_Block_ipostdom(pred);
		for (ir_node *dependee = node; dependee != pdom;
		     dependee = get_Block_ipostdom(dependee)) {
			assert(!is_Bad(pdom));
			add_cdep(dependee, pred);
		}
	}
}


/**
 * A block edge hook: add all cdep edges of block.
 */
static int cdep_edge_hook(FILE *F, ir_node *block)
{
	for (ir_cdep *cd = find_cdep(block); cd != NULL; cd = cd->next) {
		fprintf(F, "edge:{sourcename:\"n%ld\" targetname:\"n%ld\" "
		           "linestyle:dashed color:gold}\n",
		        get_irn_node_nr(block), get_irn_node_nr(cd->node));
	}

	return 0;
}

void compute_cdep(ir_graph *irg)
{
	free_cdep(irg);
	cdep_data = XMALLOC(cdep_info);
	obstack_init(&cdep_data->obst);

	cdep_data->cdep_map = pmap_create();

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE);

	/* we must temporary change the post dominator relation:
	   the ipdom of the startblock is the end block.
	   Firm does NOT add the phantom edge from Start to End.
	 */
	ir_node *const start_block = get_irg_start_block(irg);
	ir_node *const end_block   = get_irg_end_block(irg);
	ir_node *const rem         = get_Block_ipostdom(start_block);
	set_Block_ipostdom(start_block, end_block);

	irg_block_walk_graph(irg, cdep_pre, NULL, NULL);

	(void)cdep_edge_hook;

	/* restore the post dominator relation */
	set_Block_ipostdom(start_block, rem);
}

void free_cdep(ir_graph *irg)
{
	(void)irg;
	if (cdep_data != NULL) {
		pmap_destroy(cdep_data->cdep_map);
		obstack_free(&cdep_data->obst, NULL);
		free(cdep_data);
		cdep_data = NULL;
	}
}

int is_cdep_on(const ir_node *dependee, const ir_node *candidate)
{
	for (const ir_cdep *dep = find_cdep(dependee); dep != NULL;
	     dep = dep->next) {
		if (get_cdep_node(dep) == candidate)
			return 1;
	}
	return 0;
}

ir_node *get_unique_cdep(const ir_node *block)
{
	ir_cdep *cdep = find_cdep(block);
	return cdep != NULL && cdep->next == NULL ? get_cdep_node(cdep) : NULL;
}

int has_multiple_cdep(const ir_node *block)
{
	ir_cdep *cdep = find_cdep(block);
	return cdep != NULL && cdep->next != NULL;
}
