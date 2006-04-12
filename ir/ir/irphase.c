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

#ifdef _HAVE_CONFIG_H
#include "config.h"
#endif

#include "array.h"
#include "irnode_t.h"
#include "irphase_t.h"

phase_t *phase_new(const char *name, ir_graph *irg, size_t data_size, unsigned growth_factor, phase_irn_data_init_t *data_init)
{
	phase_t *ph;

	assert(growth_factor >= 1.0 && "growth factor must greater or equal to 1.0");

	ph = xmalloc(sizeof(ph[0]));

	obstack_init(&ph->obst);

	ph->name          = name;
	ph->growth_factor = growth_factor;
	ph->data_init     = data_init;
	ph->data_size     = data_size;
	ph->irg           = irg;
	ph->n_data_ptr    = 0;
	ph->data_ptr      = NULL;

	return ph;
}

void phase_free(phase_t *phase)
{
	obstack_free(&phase->obst, NULL);
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
			stat->node_data_bytes += phase->data_size;
		}
	}
	stat->overall_bytes = stat->node_map_bytes + obstack_memory_used(&phase->obst);
	return stat;
}
