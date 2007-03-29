/**
 * This file implements the x87 support and virtual to stack
 * register translation.
 *
 * $Id$
 */
#ifndef _IA32_X87_H_
#define _IA32_X87_H_

#include "../bearch.h"

/**
 * Run a simulation and fix all virtual instructions for a graph.
 * Replaces all virtual floating point instructions and registers
 * by real ones.
 *
 * @param env       architecture environment
 * @param birg      the graph to simulate and patch
 *
 * Registers must be allocated.		Needs a block-schedule.
 */
void x87_simulate_graph(const arch_env_t *env, be_irg_t *birg);

#endif /* _IA32_X87_H_ */
