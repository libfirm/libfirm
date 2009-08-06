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
 * @brief       Trivial node selector.
 * @author      Sebastian Hack
 * @date        29.08.2006
 * @version     $Id$
 */
#include "config.h"

#include <stdlib.h>

#include "irgraph.h"
#include "irnode.h"
#include "irnodeset.h"

#include "bearch.h"
#include "belistsched.h"

/**
 * The trivial selector:
 * Just assure that branches are executed last, otherwise select
 * the first node ready.
 */
static ir_node *trivial_select(void *block_env, ir_nodeset_t *ready_set,
                               ir_nodeset_t *live_set)
{
	ir_node               *irn;
	ir_nodeset_iterator_t  iter;
	(void)block_env;
	(void)live_set;

	/* assure that branches and constants are executed last */
	foreach_ir_nodeset(ready_set, irn, iter) {
		if (!is_cfop(irn)) {
			return irn;
		}
	}

	/* at last: schedule branches */
	ir_nodeset_iterator_init(&iter, ready_set);
	irn = ir_nodeset_iterator_next(&iter);

	return irn;
}

static void *trivial_init_graph(const list_sched_selector_t *vtab, const be_irg_t *birg)
{
	(void)vtab;
	(void)birg;
	return NULL;
}

static void *trivial_init_block(void *graph_env, ir_node *block)
{
	(void)graph_env;
	(void)block;
	return NULL;
}

const list_sched_selector_t trivial_selector = {
	trivial_init_graph,
	trivial_init_block,
	trivial_select,
	NULL,                /* to_appear_in_schedule */
	NULL,                /* node_ready */
	NULL,                /* node_selected */
	NULL,                /* exectime */
	NULL,                /* latency */
	NULL,                /* finish_block */
	NULL                 /* finish_graph */
};
