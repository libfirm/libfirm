/*
 * Project:     libFIRM
 * File name:   ir/ir/irphase.c
 * Purpose:     Phase information handling using node indexes.
 * Author:      Sebastian Hack
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2006 Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "array.h"
#include "irnode_t.h"
#include "irphase_t.h"

phase_t *phase_init(phase_t *ph, const char *name, ir_graph *irg, unsigned growth_factor, phase_irn_data_init_t *data_init)
{
	assert(growth_factor >= 1.0 && "growth factor must greater or equal to 1.0");
	assert(data_init && "You must provide a data constructor");

	obstack_init(&ph->obst);

	ph->name          = name;
	ph->growth_factor = growth_factor;
	ph->data_init     = data_init;
	ph->irg           = irg;
	ph->n_data_ptr    = 0;
	ph->data_ptr      = NULL;

	return ph;
}

void phase_free(phase_t *phase)
{
	obstack_free(&phase->obst, NULL);
	if(phase->data_ptr)
		xfree(phase->data_ptr);
}

phase_stat_t *phase_stat(const phase_t *phase, phase_stat_t *stat)
{
	int i, n;
	memset(stat, 0, sizeof(stat[0]));

	stat->node_map_bytes = phase->n_data_ptr * sizeof(phase->data_ptr[0]);
	stat->node_slots     = phase->n_data_ptr;
	for(i = 0, n = phase->n_data_ptr; i < n; ++i) {
		if(phase->data_ptr[i] != NULL) {
			stat->node_slots_used++;
		}
	}
	stat->overall_bytes = stat->node_map_bytes + obstack_memory_used(&((phase_t *)phase)->obst);
	return stat;
}

void phase_reinit_irn_data(phase_t *phase)
{
	int i, n;

	if (! phase->data_init)
		return;

	for (i = 0, n = phase->n_data_ptr; i < n; ++i) {
		if (phase->data_ptr[i])
			phase->data_init(phase, get_idx_irn(phase->irg, i), phase->data_ptr[i]);
	}
}

void phase_reinit_block_irn_data(phase_t *phase, ir_node *block)
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

ir_node *phase_get_first_node(phase_t *phase) {
	int i;

	for (i = 0; i < phase->n_data_ptr;  ++i)
		if (phase->data_ptr[i])
			return get_idx_irn(phase->irg, i);

	return NULL;
}

ir_node *phase_get_next_node(phase_t *phase, ir_node *start) {
	int i;

	for (i = get_irn_idx(start) + 1; i < phase->n_data_ptr; ++i)
		if (phase->data_ptr[i])
			return get_idx_irn(phase->irg, i);

	return NULL;
}
