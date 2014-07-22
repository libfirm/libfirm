/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Contains some useful function for the backend.
 * @author      Sebastian Hack
 */
#include <stdio.h>

#include "irgwalk.h"
#include "irdom_t.h"
#include "ircons.h"
#include "iropt.h"
#include "irgopt.h"
#include "irtools.h"
#include "iredges_t.h"

#include "beutil.h"
#include "besched.h"
#include "bearch.h"

void be_clear_links(ir_graph *irg)
{
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
}

/**
 * Gets the Proj with number pn from irn.
 */
ir_node *be_get_Proj_for_pn(const ir_node *irn, long pn)
{
	assert(get_irn_mode(irn) == mode_T && "need mode_T");
	foreach_out_edge(irn, edge) {
		ir_node *proj = get_edge_src_irn(edge);

		if (is_Proj(proj) && get_Proj_proj(proj) == pn)
			return proj;
	}

	return NULL;
}

/**
 * Block-walker: adds the visited block to a flexible array.
 */
static void add_to_postorder(ir_node *block, void *data)
{
	ir_node ***list = (ir_node***) data;
	ARR_APP1(ir_node*, *list, block);
}

ir_node **be_get_cfgpostorder(ir_graph *irg)
{
	ir_node **list      = NEW_ARR_F(ir_node*, 0);
	ir_node  *end_block = get_irg_end_block(irg);

	/* end block may be unreachable in case of endless loops */
	if (get_Block_n_cfgpreds(end_block) == 0)
		ARR_APP1(ir_node*, list, end_block);

	/* walk blocks */
	irg_block_edges_walk(get_irg_start_block(irg), NULL, add_to_postorder,
	                     &list);

	return list;
}
