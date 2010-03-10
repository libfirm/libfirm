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
 * @brief   Phase information handling using node indexes.
 * @author  Sebastian Hack
 * @version $Id: irphase_t.h 27270 2010-03-07 22:20:43Z matze $
 */
#ifndef FIRM_IR_PHASE_H
#define FIRM_IR_PHASE_H

#include "firm_types.h"

typedef struct ir_phase ir_phase;
typedef void *(phase_irn_init)(ir_phase *phase, const ir_node *irn, void *old);

/**
 * Allocate and initialize a new phase object
 *
 * @param irg           The graph the phase will run on.
 * @param irn_data_init A callback that is called to initialize newly created
 *                      node data. Must be non-null. You could use
 * @return              A new phase object.
 */
ir_phase *new_phase(ir_graph *irg, phase_irn_init *data_init);

/**
 * Variant for custom memory-management/classes. Just initialize given phase
 * structure (performs no allocation, you do not need to call this for phases
 * allocated wiht new_phase)
 */
void phase_init(ir_phase *phase, ir_graph *irg, phase_irn_init *data_init);

/**
 * frees all internal memory used by the phase but does not free the
 * phase struct itself.
 */
void phase_deinit(ir_phase *phase);

/**
 * free memory allocated by a phase
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
 * A default node initializer.
 * It does nothing and returns NULL.
 */
extern phase_irn_init phase_irn_init_default;

#endif
