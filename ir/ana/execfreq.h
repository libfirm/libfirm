#ifndef EXECFREQ_H_
#define EXECFREQ_H_
/*
 * Project:     libFIRM
 * File name:   ir/ana/execfreq.h
 * Purpose:     Compute an estimate of basic block executions.
 * Author:      Adam M. Szalkowski
 * Modified by:
 * Created:     28.05.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 2006 Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#include "firm_types.h"

struct _exec_freq_t;

#ifndef _EXECFREQ_TYPEDEF
#define _EXECFREQ_TYPEDEF
typedef struct _exec_freq_t exec_freq_t;
#endif

/**
 * Create execfreq structure (to be used with set_execfreq)
 */
exec_freq_t *create_execfreq(ir_graph *irg);

/**
 * Set execution frequency of a basic block
 */
void set_execfreq(exec_freq_t *ef, const ir_node *block, double freq);

/**
 * Create execfreq structure and initialze with estimated frequencies
 */
exec_freq_t *compute_execfreq(ir_graph *irg, double loop_weight);

void free_execfreq(exec_freq_t *ef);

double get_block_execfreq(const exec_freq_t *ef, const ir_node *block);
unsigned long get_block_execfreq_ulong(const exec_freq_t *ef, const ir_node *block);

#endif /* EXECFREQ_H_ */
