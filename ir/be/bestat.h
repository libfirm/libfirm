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
 * @brief       Provides several statistic functions for the backend.
 * @author      Christian Wuerdig
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

enum {
	STAT_TAG_FILE = 0,  /**< tag for source file name */
	STAT_TAG_TIME = 1,  /**< tag for time */
	STAT_TAG_IRG  = 2,  /**< tag for function name (irg) */
	STAT_TAG_CLS  = 3,  /**< tag for register class (or "<all>") */
	STAT_TAG_LAST
};

extern FILE *be_stat_file;
extern const char *be_stat_tags[STAT_TAG_LAST];


#ifdef FIRM_STATISTICS

/**
 * Collects statistics information about register pressure.
 * @param birg The be irg object containing the irg
 */
void be_do_stat_reg_pressure(be_irg_t *birg);

/**
 * Collect statistics about amount of ready nodes per block
 * @param block     The block
 * @param ready_set A set of ready nodes
 */
void be_do_stat_sched_ready(ir_node *block, const ir_nodeset_t *ready_set);

/**
 * Pass information about a perm to the statistic module.
 *
 * @param class_name the name of the register class
 * @param n_regs     number of registers in the register class
 * @param perm       the perm node
 * @param block      the block containing the perm
 * @param size       the size of the perm
 * @param real_size  number of pairs with different registers
 */
void be_do_stat_perm(const char *class_name, int n_regs, ir_node *perm, ir_node *block, int n, int real_size);

/**
 * Pass information about a cycle or chain in a perm to the statistic module.
 *
 * @param class_name the name of the register class
 * @param perm       the perm node
 * @param block      the block containing the perm
 * @param is_chain   1 if chain, 0 if cycle
 * @param size       length of the cycle/chain
 * @param n_ops      the number of ops representing this cycle/chain after lowering
 */
void be_do_stat_permcycle(const char *class_name, ir_node *perm, ir_node *block, int is_chain, int n_elems, int n_ops);

/**
 * Collects node statistics.
 *
 * @param irg      the to do statistics for
 * @param phase    the phase to collect the statistic for
 */
void be_do_stat_nodes(ir_graph *irg, const char *phase);

/**
 * Performs initialization for be node statistics.
 */
void be_stat_init_irg(const arch_env_t *arch_env, ir_graph *irg);

void be_init_stat_file(const char *filename, const char *sourcefilename);
void be_close_stat_file(void);

#else /* ! FIRM_STATISTICS */

#define be_stat_init_irg(arch_env, irg)
#define be_do_stat_nodes(irg, phase)
#define be_do_stat_reg_pressure(birg)
#define be_do_stat_sched_ready(block, ready_set)
#define be_do_stat_perm(class_name, n_regs, perm, block, n, real_size)
#define be_do_stat_permcycle(class_name, perm, block, is_chain, n_elems, n_ops)

#endif /* FIRM_STATISTICS */

/**
 * Gives a cost estimate for the program (based on execution frequencies)
 * and backend op_estimated_cost
 */
double be_estimate_irg_costs(ir_graph *irg, const arch_env_t *arch_env, ir_exec_freq *execfreqs);

#endif /* FIRM_BE_BESTAT_H */
