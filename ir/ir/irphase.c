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
 * @brief    Phase information handling using node indexes.
 * @author   Sebastian Hack
 * @version  $Id$
 * @brief
 *  A phase contains a link to private data for each node in an ir graph.
 *  A phase is independent from the globally visible link field of ir nodes.
 */
#include "config.h"

#include "array.h"
#include "util.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irphase_t.h"

void *phase_irn_init_default(ir_phase *ph, const ir_node *irn, void *old)
{
	(void) ph;
	(void) irn;
	(void) old;
	return NULL;
}

void phase_init(ir_phase *phase, ir_graph *irg, phase_irn_init *data_init)
{
	memset(phase, 0, sizeof(*phase));

	obstack_init(&phase->obst);
	phase->data_init  = data_init;
	phase->irg        = irg;
	phase->n_data_ptr = 0;
	phase->data_ptr   = NULL;
}

ir_phase *new_phase(ir_graph *irg, phase_irn_init *data_init)
{
	ir_phase *phase = xmalloc(sizeof(*phase));
	phase_init(phase, irg, data_init);
	return phase;
}

void phase_deinit(ir_phase *phase)
{
	obstack_free(&phase->obst, NULL);
	if (phase->data_ptr)
		xfree(phase->data_ptr);
}

void phase_free(ir_phase *phase)
{
	phase_deinit(phase);
	xfree(phase);
}

phase_stat_t *phase_stat(const ir_phase *phase, phase_stat_t *stat)
{
	int i, n;
	memset(stat, 0, sizeof(stat[0]));

	stat->node_map_bytes = phase->n_data_ptr * sizeof(phase->data_ptr[0]);
	stat->node_slots     = phase->n_data_ptr;
	for (i = 0, n = phase->n_data_ptr; i < n; ++i) {
		if (phase->data_ptr[i] != NULL) {
			stat->node_slots_used++;
		}
	}
	stat->overall_bytes = stat->node_map_bytes + obstack_memory_used(&((ir_phase *)phase)->obst);
	return stat;
}

void phase_reinit_irn_data(ir_phase *phase)
{
	int i, n;

	if (! phase->data_init)
		return;

	for (i = 0, n = phase->n_data_ptr; i < n; ++i) {
		if (phase->data_ptr[i])
			phase->data_init(phase, get_idx_irn(phase->irg, i), phase->data_ptr[i]);
	}
}

void phase_reinit_block_irn_data(ir_phase *phase, ir_node *block)
{
	int i, n;

	if (! phase->data_init)
		return;

	for (i = 0, n = phase->n_data_ptr; i < n; ++i) {
		if (phase->data_ptr[i]) {
			ir_node *irn = get_idx_irn(phase->irg, i);
			if (! is_Block(irn) && get_nodes_block(irn) == block)
				phase->data_init(phase, irn, phase->data_ptr[i]);
		}
	}
}

ir_node *phase_get_first_node(const ir_phase *phase)
{
	unsigned i;

	for (i = 0; i < phase->n_data_ptr;  ++i)
		if (phase->data_ptr[i])
			return get_idx_irn(phase->irg, i);

	return NULL;
}

ir_node *phase_get_next_node(const ir_phase *phase, ir_node *start)
{
	unsigned i;

	for (i = get_irn_idx(start) + 1; i < phase->n_data_ptr; ++i)
		if (phase->data_ptr[i])
			return get_idx_irn(phase->irg, i);

	return NULL;
}
