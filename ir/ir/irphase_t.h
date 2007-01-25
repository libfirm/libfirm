/*
 * Project:     libFIRM
 * File name:   ir/ir/irphase_t.c
 * Purpose:     Phase information handling using node indexes.
 * Author:      Sebastian Hack
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2007 Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _FIRM_IR_PHASE_T_H
#define _FIRM_IR_PHASE_T_H

#include "obstack.h"
#include "irgraph_t.h"
#include "irtools.h"
#include "irphase.h"
#include "irtools.h"

typedef struct {
	unsigned node_slots;
	unsigned node_slots_used;
	unsigned node_data_bytes;
	unsigned node_map_bytes;
	unsigned overall_bytes;
} phase_stat_t;

/**
 * Phase statistics.
 */
phase_stat_t *phase_stat(const phase_t *phase, phase_stat_t *stat);

typedef void *(phase_irn_data_init_t)(phase_t *phase, ir_node *irn, void *old);

/**
 * The default grow factor.
 * The node => data map does not speculatively allocate more slots.
 */
#define PHASE_DEFAULT_GROWTH (256)

/**
 * A phase object.
 */
struct _phase_t {
	struct obstack         obst;           /**< The obstack where the irn phase data will be stored on. */
	const char            *name;           /**< The name of the phase. */
	ir_graph              *irg;            /**< The irg this phase will we applied to. */
	unsigned               growth_factor;  /**< factor to leave room for add. nodes. 256 means 1.0. */
	void                  *priv;           /**< Some pointer private to the user of the phase. */
	size_t                 n_data_ptr;     /**< The length of the data_ptr array. */
	void                 **data_ptr;       /**< Map node indexes to irn data on the obstack. */
	phase_irn_data_init_t *data_init;      /**< A callback that is called to initialize newly created node data. */
};

/**
 * Initialize a phase object.
 * @param name          The name of the phase.
 * @param irg           The graph the phase will run on.
 * @param growth_factor A factor denoting how many node slots will be additionally allocated,
 *                      if the node => data is full. The factor is given in units of 1/256, so
 *                      256 means 1.0.
 * @param irn_data_init A callback that is called to initialize newly created node data.
 *                      Must be non-null.
 * @param priv          Some private pointer which is kept in the phase and can be retrieved with phase_get_private().
 * @return              A new phase object.
 */
phase_t *phase_init(phase_t *ph, const char *name, ir_graph *irg, unsigned growth_factor, phase_irn_data_init_t *data_init);

/**
 * Free the phase and all node data associated with it.
 * @param phase The phase.
 */
void phase_free(phase_t *phase);

/**
 * Re-initialize the irn data for all nodes in the node => data map using the given callback.
 * @param phase The phase.
 */
void phase_reinit_irn_data(phase_t *phase);

/**
 * Re-initialize the irn data for all nodes in the given block.
 * @param phase The phase.
 * @param block The block.
 */
void phase_reinit_block_irn_data(phase_t *phase, ir_node *block);

/**
 * Re-initialize the irn data for the given node.
 * @param phase The phase.
 * @param irn   The irn.
 */
#define phase_reinit_single_irn_data(phase, irn) _phase_reinit_single_irn_data((phase), (irn))

/**
 * Returns the first node of the phase having some data assigned.
 * @param phase The phase.
 * @return The first irn having some data assigned, NULL otherwise
 */
ir_node *phase_get_first_node(phase_t *phase);

/**
 * Returns the next node after @p start having some data assigned.
 * @param phase The phase.
 * @param start The node to start from
 * @return The next node after start having some data assigned, NULL otherwise
 */
ir_node *phase_get_next_node(phase_t *phase, ir_node *start);

/**
 * Convenience macro to iterate over all nodes of a phase
 * having some data assigned.
 */
#define foreach_phase_irn(phase, irn) \
	for (irn = phase_get_first_node(phase); irn; irn = phase_get_next_node(phase, irn))

/**
 * Get the name of the phase.
 */
#define phase_get_name(ph)                 ((ph)->name)

/**
 * Get the irg the phase runs on.
 */
#define phase_get_irg(ph)                  ((ph)->irg)

/**
 * Get private data pointer as passed on creating the phase.
 */
#define phase_get_private(ph)              ((ph)->priv)

/**
 * Allocate memory in the phase's memory pool.
 */
#define phase_alloc(ph, size)              obstack_alloc(phase_obst(ph), (size))

/**
 * Get the obstack of the phase.
 */
#define phase_obst(ph)                     (&(ph)->obst)

/**
 * Get the phase data for an irn.
 * @param ph   The phase.
 * @param irn  The irn to get data for.
 * @return     A pointer to the data or NULL if the irn has no phase data.
 */
#define phase_get_irn_data(ph, irn)        _phase_get_irn_data((ph), (irn))

/**
 * Get or set phase data for an irn.
 * @param ph   The phase.
 * @param irn  The irn to get (or set) node data for.
 * @return     A (non-NULL) pointer to phase data for the irn. Either existent one or newly allocated one.
 */
#define phase_get_or_set_irn_data(ph, irn) _phase_get_or_set_irn_data((ph), (irn))

/**
 * Set the data for an irn.
 * @param ph The phase.
 * @param irn The node.
 * @param data The data.
 * @return The old data or NULL if there was none.
 */
#define phase_set_irn_data(ph, irn, data)  _phase_set_irn_data((ph), (irn), (data))

/**
 * This is private and only here for performance reasons.
 */
static INLINE void _phase_reinit_single_irn_data(phase_t *phase, ir_node *irn)
{
	int idx;

	if (! phase->data_init)
		return;

	idx = get_irn_idx(irn);
	if (phase->data_ptr[idx])
		phase->data_init(phase, irn, phase->data_ptr[idx]);
}


/**
 * This is private and just here for performance reasons.
 */
static INLINE void _private_phase_enlarge(phase_t *phase, unsigned max_idx)
{
	unsigned last_irg_idx = get_irg_last_idx(phase->irg);
	size_t old_cap        = phase->n_data_ptr;
	size_t new_cap;

	/* make the maximum index at least as big as the largest index in the graph. */
	max_idx = MAX(max_idx, last_irg_idx);
	new_cap = (size_t) (max_idx * phase->growth_factor / 256);

	phase->data_ptr = (void **)xrealloc(phase->data_ptr, new_cap * sizeof(phase->data_ptr[0]));

	/* initialize the newly allocated memory. */
	memset(phase->data_ptr + old_cap, 0, (new_cap - old_cap) * sizeof(phase->data_ptr[0]));
	phase->n_data_ptr = new_cap;
}

/**
 * This is private and only here for performance reasons.
 */
#define _private_phase_assure_capacity(ph, max_idx) ((max_idx) >= (ph)->n_data_ptr ? (_private_phase_enlarge((ph), (max_idx)), 1) : 1)

static INLINE void *_phase_get_irn_data(const phase_t *ph, const ir_node *irn)
{
	unsigned idx = get_irn_idx(irn);
	return idx < ph->n_data_ptr ? ph->data_ptr[idx] : NULL;
}

static INLINE void *_phase_set_irn_data(phase_t *ph, const ir_node *irn, void *data)
{
	unsigned idx = get_irn_idx(irn);
	void *res;

	/* Assure that there's a sufficient amount of slots. */
	_private_phase_assure_capacity(ph, idx);

	res = ph->data_ptr[idx];
	ph->data_ptr[idx] = data;

	return res;
}


static INLINE void *_phase_get_or_set_irn_data(phase_t *ph, ir_node *irn)
{
	unsigned idx = get_irn_idx(irn);
	void *res;

	/* Assure that there's a sufficient amount of slots. */
	_private_phase_assure_capacity(ph, idx);

	res = ph->data_ptr[idx];

	/* If there has no irn data allocated yet, do that now. */
	if(!res) {
		phase_irn_data_init_t *data_init = ph->data_init;

		/* call the node data structure allocator/constructor. */
		res = ph->data_ptr[idx] = data_init(ph, irn, NULL);

	}
	return res;
}

#endif /* _FIRM_IR_PHASE_T_H */
