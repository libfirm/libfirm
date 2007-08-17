/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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
	const arch_env_t *arch_env = block_env;
	ir_node          *irn      = NULL;
	ir_nodeset_iterator_t iter;
	(void) live_set;

	/* assure that branches and constants are executed last */
	foreach_ir_nodeset(ready_set, irn, iter) {
		if (! arch_irn_class_is(arch_env, irn, branch)) {
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
	(void) vtab;
	return (void *) be_get_birg_arch_env(birg);
}

static void *trivial_init_block(void *graph_env, ir_node *block)
{
	(void) block;
	return graph_env;
}

static const list_sched_selector_t trivial_selector_struct = {
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

const list_sched_selector_t *trivial_selector = &trivial_selector_struct;
