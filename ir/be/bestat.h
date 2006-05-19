/**
 * Provides several statistic functions for the backend.
 * @author Christian Wuerdig
 * $Id$
 */

#ifndef _BESTAT_H_
#define _BESTAT_H_

#include "firm_config.h"
#include "be_t.h"
#include "benodesets.h"

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
void be_do_stat_sched_ready(ir_node *block, nodeset *ready_set);

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

#else

#define be_stat_init_irg(arch_env, irg)
#define be_do_stat_nodes(irg, phase)
#define be_do_stat_reg_pressure(birg)
#define be_do_stat_sched_ready(block, ready_set)
#define be_do_stat_perm(class_name, n_regs, perm, block, n, real_size)
#define be_do_stat_permcycle(class_name, perm, block, is_chain, n_elems, n_ops)

#endif /* FIRM_STATISTICS */

#endif /* _BESTAT_H_ */
