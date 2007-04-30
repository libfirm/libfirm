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
 * @brief       ILP based instruction scheduling.
 * @author      Christian Wuerdig
 * @date        22.10.2006
 * @version     $Id$
 *
 * An ILP scheduler based on
 * "ILP-based Instruction Scheduling for IA-64"
 * by Daniel Kaestner and Sebastian Winkel
 * extended with register pressure constraints by Christian Wuerdig
 */
#ifndef FIRM_BE_BEILPSCHED_H
#define FIRM_BE_BEILPSCHED_H

#include "irgraph.h"
#include "irnode.h"

typedef struct _ilp_sched_selector_t    ilp_sched_selector_t;
typedef struct _ilp_sched_selector_if_t ilp_sched_selector_if_t;

/**
 * A selector interface which is used by the ILP schedule framework.
 * These functions provide the ILP scheduler with necessary information
 * from the backend or they pass back information to the backend about
 * the state of scheduling.
 */
struct _ilp_sched_selector_if_t {

	/**
	 * This function is called before the scheduling of the irg.
	 * @param self    The this pointer.
	 * @param irg     The irg being scheduled.
	 * @return An irg scheduling environment.
	 */
	void *(*init_irg_ilp_schedule)(const void *self, ir_graph *irg);

	/**
	 * This functions is called after an irg has been scheduled.
	 * @param self    The this pointer.
	 * @param irg     The irg which has been scheduled.
	 * @param irg_env The irg scheduling environment.
	 */
	void (*finish_irg_ilp_schedule)(const void *self, ir_graph *irg, void *irg_env);

	/**
	 * This function is called before the scheduling of a block starts.
	 * @param self       The this pointer.
	 * @param block      The block being scheduled.
	 * @return A block scheduling environment.
	 */
	void *(*init_block_ilp_schedule)(const void *self, ir_node *block);

	/**
	 * This functions is called after a block has been scheduled.
	 * @param self      The this pointer.
	 * @param block     The block which has been scheduled.
	 * @param block_env The block scheduling environment.
	 */
	void (*finish_block_ilp_schedule)(const void *self, ir_node *block, void *block_env);

	/**
	 * Calculates the latency of node @p irn.
	 * @param self       The this pointer.
	 * @param irn        The node.
	 * @param block_env  The block scheduling environment.
	 * @return The latency in cycles of node @p irn.
	 */
	unsigned (*latency)(const void *self, ir_node *irn, void *block_env);

	/**
	 * This function is called after a certain node has been scheduled.
	 * @param self       The this pointer.
	 * @param irn        The node which has been scheduled.
	 * @param cycle      The cycle at which the node is scheduled.
	 * @param block_env  The block scheduling environment.
	 */
	void (*node_scheduled)(const void *self, ir_node *irn, unsigned cycle, void *block_env);
};

/**
 * The actual ILP schedule selector.
 */
struct _ilp_sched_selector_t {
	ilp_sched_selector_if_t *impl;
};

/**
 * Some helper macros.
 */
#define BE_ILP_SCHED_CALL(func, self, obj, env)       \
	do {                                              \
		if ((self) && (self)->impl->func)             \
			(self)->impl->func((self), (obj), (env)); \
	} while (0)

#define BE_ILP_SCHED_CALL2(func, self, obj, obj2, env)        \
	do {                                                      \
		if ((self) && (self)->impl->func)                     \
			(self)->impl->func((self), (obj), (obj2), (env)); \
	} while (0)

#define BE_ILP_SCHED_CALL_ENVRET(func, self, obj, defret) \
	((self) && (self)->impl->func ? (self)->impl->func((self), (obj)) : (defret))

#define BE_ILP_SCHED_CALL_RET(func, self, obj, env, defret) \
	((self) && (self)->impl->func ? (self)->impl->func((self), (obj), (env)) : (defret))

/**
 * Convenience macros for all functions.
 */

#define be_ilp_sched_init_irg_ilp_schedule(self, irg) \
	BE_ILP_SCHED_CALL_ENVRET(init_irg_ilp_schedule, self, irg, NULL)

#define be_ilp_sched_finish_irg_ilp_schedule(self, irg, irg_env) \
	BE_ILP_SCHED_CALL(finish_irg_ilp_schedule, self, irg, irg_env)

#define be_ilp_sched_init_block_ilp_schedule(self, block) \
	BE_ILP_SCHED_CALL_ENVRET(init_block_ilp_schedule, self, block, NULL)

#define be_ilp_sched_finish_block_ilp_schedule(self, block, block_env) \
	BE_ILP_SCHED_CALL(finish_block_ilp_schedule, self, block, block_env)

#define be_ilp_sched_latency(self, irn, block_env) \
	BE_ILP_SCHED_CALL_RET(latency, self, irn, block_env, 0)

#define be_ilp_sched_node_scheduled(self, irn, cycle, block_env) \
	BE_ILP_SCHED_CALL2(node_scheduled, self, irn, cycle, block_env)

/**
 * Perform ILP scheduling on given birg.
 */
void be_ilp_sched(const be_irg_t *birg, be_options_t *be_opts);

#endif /* FIRM_BE_BEILPSCHED_H */
