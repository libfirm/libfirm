/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       Contains some useful function for the backend.
 * @author      Sebastian Hack
 * @version     $Id$
 */
#include "config.h"

#include <stdio.h>

#include "pset.h"

#include "irgraph.h"
#include "irgwalk.h"
#include "irdump_t.h"
#include "irdom_t.h"
#include "ircons.h"
#include "iropt.h"
#include "irgopt.h"
#include "irtools.h"
#include "irprintf.h"
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
	const ir_edge_t *edge;
	ir_node         *proj;
	assert(get_irn_mode(irn) == mode_T && "need mode_T");

	foreach_out_edge(irn, edge) {
		proj = get_edge_src_irn(edge);

		if (is_Proj(proj) && get_Proj_proj(proj) == pn)
			return proj;
	}

	return NULL;
}

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

ir_node *get_first_block_succ(const ir_node *block)
{
	const ir_edge_t *edge = get_irn_out_edge_first_kind(block, EDGE_KIND_BLOCK);
	assert(edge != NULL);
	return get_edge_src_irn(edge);
}
