/*
 * Project:     libFIRM
 * File name:   ir/ana/execution_frequency.h
 * Purpose:     Compute an estimate of basic block executions.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     5.11.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _EXECUTION_FREQUENCY_H_
#define _EXECUTION_FREQUENCY_H_

/** @file execution_frequency.h
 *
 * Estimate exectution freqencies of blocks.
 *
 * @author Goetz Lindenmaier
 *
 * We assume the start block of a procedure is executed once.  Based on this we
 * compute the execution freqency of all blocks.
 *
 *
 */

#include "irnode.h"
#include "irgraph.h"


/** Returns the nunmber of times the block/region is executed according to
 *  out estimate. */
double     get_Block_exec_freq(ir_node *b);
double     get_region_exec_freq(void *reg);

/** Compute the execution frequency for all blocks in the given
 *  graph.
 *
 * @param irg                  The graph to be analyzed.
 * @param default_loop_weight  The default number of executions of a loop.
 */
void compute_execution_frequency(ir_graph *irg, int default_loop_weight, double exception_probability);
void compute_execution_frequencies(int default_loop_weight, double exception_probability);

/** Free occupied memory, reset. */
void free_execution_frequency(void);


#endif /* _EXECUTION_FREQUENCY_H_ */
