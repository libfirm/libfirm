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
 * The computation of the freqencies depends on the count of exception control
 * flow computed during the interval analysis.  The interval analysis again
 * depends on stuff computed here.
 */

#include "irnode.h"
#include "irgraph.h"

/* A proj from a Cond that goes to an exception handler. */
int is_fragile_Proj(ir_node *n);

/** Returns the number of times the block/region is executed according to
 *  our estimate. Gives a number relative to the Start node of the procedure
 *  the block is in, which is weighted with 1. */
double get_irn_exec_freq   (ir_node *n);
double get_Block_exec_freq (ir_node *b);
double get_region_exec_freq(void *reg);

/** Compute the execution frequency for all blocks in the given
 *  graph.
 *
 * @param irg                   The graph to be analyzed.
 * @param default_loop_weight   The default number of executions of a loop.
 * @param exception_probability The probability that a fragile operation causes an exception.
 *
 * Uses link field.
 */
void compute_execution_frequency(ir_graph *irg, int default_loop_weight, double exception_probability);

/** Compute the execution frequency for all graphs.
 *
 * @param default_loop_weight   The default number of executions of a loop.
 * @param exception_probability The probability that a fragile operation causes an exception.
 *
 */
void compute_execution_frequencies(int default_loop_weight, double exception_probability);

/** Free occupied memory, reset for all graphs. */
void free_execution_frequency(void);

/** State of execution freqencies for graphs and the whole program.
 *
 * The exec_freq_state in irp is consistent, if the state of all graphs is consistent.
 * It is none, if the state of all graphs is none.  Else it is inconsistent. */
typedef enum {
  exec_freq_none,             /**< Execution frequencies are not computed, no memory is
				   allocated, access fails. */
  exec_freq_consistent,       /**< Execution frequency information is computed and correct. */
  exec_freq_inconsistent      /**< Execution frequency is computed but the graph has been
				   changed since. */
} exec_freq_state;

exec_freq_state get_irg_exec_freq_state(ir_graph *irg);
void            set_irg_exec_freq_state(ir_graph *irg, exec_freq_state s);
/* Sets irg and irp exec freq state to inconsistent if it is set to consistent. */
void            set_irg_exec_freq_state_inconsistent(ir_graph *irg);

exec_freq_state get_irp_exec_freq_state(void);
/* Sets irp and all irg exec freq states to inconsistent if it is set to consistent. */
void            set_irp_exec_freq_state_inconsistent(void);




#endif /* _EXECUTION_FREQUENCY_H_ */
