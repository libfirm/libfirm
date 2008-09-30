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
 * @brief       Provides several statistic functions for the backend.
 * @author      Christian Wuerdig, Matthias Braun
 * @version     $Id$
 */
#ifndef FIRM_BE_BESTAT_H
#define FIRM_BE_BESTAT_H

#include "firm_config.h"

#include "firm_types.h"
#include "irnodeset.h"

#include "beirg.h"
#include "bestatevent.h"
#include "bearch.h"

enum be_stat_tag_t {
	BE_STAT_PHIS,      /**< phi count (excluding mem-phis) */
	BE_STAT_MEM_PHIS,  /**< memory-phi count */
	BE_STAT_COPIES,    /**< copies */
	BE_STAT_PERMS,     /**< perms */
	BE_STAT_SPILLS,    /**< spills (also folded spills) */
	BE_STAT_RELOADS,   /**< reloads (also folded reloads) */
	BE_STAT_REMATS,    /**< rematerialized nodes */
	BE_STAT_COUNT
};
typedef unsigned long be_node_stats_t[BE_STAT_COUNT];

/**
 * Collect statistics about node types
 */
void be_collect_node_stats(be_node_stats_t *stats, be_irg_t *birg);

void be_subtract_node_stats(be_node_stats_t *stats, be_node_stats_t *sub);

void be_copy_node_stats(be_node_stats_t *dest, be_node_stats_t *src);

void be_emit_node_stats(be_node_stats_t *stats, const char *prefix);

/**
 * Collects statistics information about register pressure.
 * @param birg The be irg object containing the irg
 */
void be_do_stat_reg_pressure(be_irg_t *birg, const arch_register_class_t *cls);

/**
 * Gives a cost estimate for the program (based on execution frequencies)
 * and backend op_estimated_cost
 */
double be_estimate_irg_costs(ir_graph *irg, const arch_env_t *arch_env,
                             ir_exec_freq *execfreqs);

/**
 * return number of "instructions" (=nodes without some virtual nodes like Proj,
 * Start, End)
 */
unsigned long be_count_insns(ir_graph *irg);

/**
 * return number of basic blocks (without the end block)
 */
unsigned long be_count_blocks(ir_graph *irg);


#endif
