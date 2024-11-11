/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Compute an estimate of basic block executions.
 * @author      Adam M. Szalkowski
 * @date        28.05.2006
 */
#ifndef FIRM_ANA_EXECFREQ_T_H
#define FIRM_ANA_EXECFREQ_T_H

#include "execfreq.h"

void init_execfreq(void);

void exit_execfreq(void);

void set_block_execfreq(ir_node *block, double freq);

typedef struct ir_execfreq_int_factors {
	double min_non_zero;
	double m;
	double b;
} ir_execfreq_int_factors;

void ir_calculate_execfreq_int_factors(ir_execfreq_int_factors *factors,
                                       ir_graph *irg);

int get_block_execfreq_int(const ir_execfreq_int_factors *factors,
                           const ir_node *block);

#endif
