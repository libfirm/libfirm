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
 * @brief   Phase information handling using node indexes.
 * @author  Sebastian Hack
 * @version $Id$
 */
#ifndef FIRM_IR_PHASE_T_H
#define FIRM_IR_PHASE_T_H

#include "firm_types.h"
#include "obst.h"
#include "irgraph_t.h"
#include "irtools.h"
#include "irphases_t.h"

struct _ir_phase_info {
	ir_phase_id      id;
	const char       buf[128];
};

typedef struct _ir_phase_info ir_phase_info;

typedef void *(phase_irn_init)(ir_phase *phase, const ir_node *irn, void *old);

/**
 * A default node initializer.
 * It does nothing and returns NULL.
 */
extern phase_irn_init phase_irn_init_default;

/**
 * A phase object.
 */
struct _ir_phase {
	struct obstack     obst;           /**< The obstack where the irn phase data will be stored on. */
	ir_phase_id        id;             /**< The phase ID. */
	const char        *name;           /**< The name of the phase. */
	ir_graph          *irg;            /**< The irg this phase will we applied to. */
	unsigned           growth_factor;  /**< The factor to leave room for additional nodes. 256 means 1.0. */
	void              *priv;           /**< Some pointer private to the user of the phase. */
	size_t             n_data_ptr;     /**< The length of the data_ptr array. */
	void             **data_ptr;       /**< Map node indexes to irn data on the obstack. */
	phase_irn_init    *data_init;      /**< A callback that is called to initialize newly created node data. */
};

#define PHASE_DEFAULT_GROWTH (256)


/**
 * For statistics: A type containing statistic data of a phase object.
 */
typedef struct {
	unsigned node_slots;       /**< The number of allocated node slots. */
	unsigned node_slots_used;  /**< The number of used node slots, ie. nodes that have node data. */
	unsigned node_map_bytes;   /**< Number of used bytes for the node map. */
	unsigned overall_bytes;    /**< Overall number of used bytes for the phase. */
} phase_stat_t;

/**
 * Collect Phase statistics.
 *
 * @param phase  The phase.
 * @param stat   Will be filled with the statistical data.
 */
phase_stat_t *phase_stat(const ir_phase *phase, phase_stat_t *stat);

/**
 * Initialize a phase object.
 *
 * @param name          The name of the phase. Just for debugging.
 * @param irg           The graph the phase will run on.
 * @param growth_factor A factor denoting how many node slots will be additionally allocated,
 *                      if the node => data is full. The factor is given in units of 1/256, so
 *                      256 means 1.0.
 * @param irn_data_init A callback that is called to initialize newly created node data.
 *                      Must be non-null.
 * @param priv          Some private pointer which is kept in the phase and can be retrieved with phase_get_private().
 * @return              A new phase object.
 */
ir_phase *phase_init(ir_phase *ph, const char *name, ir_graph *irg, unsigned growth_factor, phase_irn_init *data_init, void *priv);

/**
 * Init an irg managed phase.
 *
 * The first sizeof(ir_phase) bytes will be considered to be a phase object;
 * they will be properly initialized. The remaining bytes are at the user's disposal.
 * The returned phase object will be inserted in the phase slot of the @p irg designated by the phase ID (@p id).
 * Note that you cannot allocate phases with an ID <code>PHASE_NOT_IRG_MANAGED</code>.
 *
 * @param ph        The memory of the phase to initialize.
 * @param irg       The irg.
 * @param id        The ID of the irg-managed phase (see irphaselist.h).
 * @param data_init The node data initialization function.
 * @return          The allocated phase object.
 */
ir_phase *init_irg_phase(ir_graph *irg, ir_phase_id id, size_t size, phase_irn_init *data_init);

void free_irg_phase(ir_graph *irg, ir_phase_id id);

/**
 * Free the phase and all node data associated with it.
 *
 * @param phase  The phase.
 */
void phase_free(ir_phase *phase);

/**
 * Re-initialize the irn data for all nodes in the node => data map using the given callback.
 *
 * @param phase  The phase.
 */
void phase_reinit_irn_data(ir_phase *phase);

/**
 * Re-initialize the irn data for all nodes having phase data in the given block.
 *
 * @param phase  The phase.
 * @param block  The block.
 *
 * @note Beware: iterates over all nodes in the graph to find the nodes of the given block.
 */
void phase_reinit_block_irn_data(ir_phase *phase, ir_node *block);

/**
 * Re-initialize the irn data for the given node.
 *
 * @param phase  The phase.
 * @param irn    The irn.
 */
#define phase_reinit_single_irn_data(phase, irn) _phase_reinit_single_irn_data((phase), (irn))

/**
 * Returns the first node of the phase having some data assigned.
 *
 * @param phase  The phase.
 *
 * @return The first irn having some data assigned, NULL otherwise
 */
ir_node *phase_get_first_node(const ir_phase *phase);

/**
 * Returns the next node after @p start having some data assigned.
 *
 * @param phase  The phase.
 * @param start  The node to start from
 *
 * @return The next node after start having some data assigned, NULL otherwise
 */
ir_node *phase_get_next_node(const ir_phase *phase, ir_node *start);

/**
 * Convenience macro to iterate over all nodes of a phase
 * having some data assigned.
 *
 * @param phase  The phase.
 * @param irn    A local variable that will hold the current node inside the loop.
 */
#define foreach_phase_irn(phase, irn) \
	for (irn = phase_get_first_node(phase); irn; irn = phase_get_next_node(phase, irn))

/**
 * Get the name of the phase.
 *
 * @param phase  The phase.
 */
#define phase_get_name(phase)                 ((phase)->name)

/**
 * Get the irg the phase runs on.
 *
 * @param phase  The phase.
 */
#define phase_get_irg(phase)                  ((phase)->irg)

/**
 * Get private data pointer as passed on creating the phase.
 *
 * @param phase  The phase.
 */
#define phase_get_private(phase)              ((phase)->priv)

/**
 * Allocate memory in the phase's memory pool.
 *
 * @param phase  The phase.
 * @param size   Number of bytes to allocate.
 */
#define phase_alloc(phase, size)              obstack_alloc(phase_obst(phase), (size))

/**
 * Get the obstack of a phase.
 *
 * @param phase  The phase.
 */
#define phase_obst(phase)                     (&(phase)->obst)

/**
 * Get the phase node data for an irn.
 *
 * @param phase   The phase.
 * @param irn     The irn to get data for.
 *
 * @return A pointer to the node data or NULL if the irn has no phase data allocated yet.
 */
#define phase_get_irn_data(phase, irn)        _phase_get_irn_data((phase), (irn))

/**
 * Get or set phase data for an irn.
 *
 * @param phase  The phase.
 * @param irn    The irn to get (or set) node data for.
 *
 * @return A (non-NULL) pointer to phase data for the irn. Either existent one or newly allocated one.
 */
#define phase_get_or_set_irn_data(phase, irn) _phase_get_or_set_irn_data((phase), (irn))

/**
 * Set the node data for an irn.
 *
 * @param phase  The phase.
 * @param irn    The node.
 * @param data   The node data.
 *
 * @return The old data or NULL if there was none.
 */
#define phase_set_irn_data(phase, irn, data)  _phase_set_irn_data((phase), (irn), (data))

/**
 * Get the irg-managed phase for a givedn phase ID.
 * @param irg The irg.
 * @param id  The ID.
 * @return The corresponding phase, or NULL if there is none.
 */
#define get_irg_phase(irg, id)                _get_irg_phase((irg), (id))

/**
 * Get the information a phase holds about a node.
 * @param irn The node.
 * @param id  The ID of the phase.
 */
#define get_irn_phase_info(irn, id)           _get_phase_irn_info((irn), (id))

/**
 * Get or set information a phase holds about a node.
 * If the given phase does not hold information of the node,
 * the information structure will be created, initialized (see the data_init function of ir_phase), and returned.
 * @param irn The node.
 * @param id  The ID of the phase.
 */
#define get_or_set_irn_phase_info(irn, id)    _get_or_set_irn_phase_info((irn), (id))

#define set_irn_phase_info(irn, id)           _set_irn_phase_info((irn), (id))

/**
 * This is private and only here for performance reasons.
 */
static INLINE void _phase_reinit_single_irn_data(ir_phase *phase, ir_node *irn)
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
static INLINE void _private_phase_enlarge(ir_phase *phase, unsigned max_idx)
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

static INLINE void *_phase_get_irn_data(const ir_phase *ph, const ir_node *irn)
{
	unsigned idx = get_irn_idx(irn);
	return idx < ph->n_data_ptr ? ph->data_ptr[idx] : NULL;
}

static INLINE void *_phase_set_irn_data(ir_phase *ph, const ir_node *irn, void *data)
{
	unsigned idx = get_irn_idx(irn);
	void *res;

	/* Assure that there's a sufficient amount of slots. */
	_private_phase_assure_capacity(ph, idx);

	res = ph->data_ptr[idx];
	ph->data_ptr[idx] = data;

	return res;
}


static INLINE void *_phase_get_or_set_irn_data(ir_phase *ph, const ir_node *irn)
{
	unsigned idx = get_irn_idx(irn);
	void *res;

	/* Assure that there's a sufficient amount of slots. */
	_private_phase_assure_capacity(ph, idx);

	res = ph->data_ptr[idx];

	/* If there has no irn data allocated yet, do that now. */
	if(!res) {
		phase_irn_init *data_init = ph->data_init;

		/* call the node data structure allocator/constructor. */
		res = ph->data_ptr[idx] = data_init(ph, irn, NULL);

	}
	return res;
}

static INLINE ir_phase *_get_irg_phase(const ir_graph *irg, ir_phase_id id)
{
	return irg->phases[id];
}

static INLINE void *_get_irn_phase_info(const ir_node *irn, ir_phase_id id)
{
	const ir_graph *irg = get_irn_irg(irn);
	const ir_phase *ph  = get_irg_phase(irg, id);
	assert(ph && "phase info has to be computed");
	return _phase_get_irn_data(ph, irn);
}

static INLINE void *_get_or_set_irn_phase_info(const ir_node *irn, ir_phase_id id)
{
	const ir_graph *irg = get_irn_irg(irn);
	ir_phase *ph  = get_irg_phase(irg, id);
	assert(ph && "phase info has to be computed");
	return _phase_get_or_set_irn_data(ph, irn);
}

static INLINE void *_set_irn_phase_info(const ir_node *irn, ir_phase_id id, void *data)
{
	const ir_graph *irg = get_irn_irg(irn);
	ir_phase *ph  = get_irg_phase(irg, id);
	assert(ph && "phase info has to be computed");
	return _phase_set_irn_data(ph, irn, data);
}


#endif
