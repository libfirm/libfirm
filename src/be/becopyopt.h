/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Copy minimization driver.
 * @author      Daniel Grund
 * @date        11.04.2005
 *
 * Main file for the optimization reducing the copies needed for:
 * - Phi coalescing
 * - Register-constrained nodes
 * - Two-address code instructions
 */
#ifndef FIRM_BE_BECOPYOPT_H
#define FIRM_BE_BECOPYOPT_H

#include <stdbool.h>

#include "firm_types.h"
#include "bechordal.h"

typedef int(*cost_fct_t)(const ir_node *node, int input);

typedef struct {
	int (*copyopt)(copy_opt_t *co); /**< function ptr to run copyopt */
} co_algo_info;

/**
 * Register a new copy optimization algorithm.
 *
 * @param name     the name of the copy optimazation algorithm,
 *                 used to select it
 * @param copyopt  a copy optimazation entry
 */
void be_register_copyopt(const char *name, co_algo_info *copyopt);

/** The driver for copy minimization. */
void co_driver(be_chordal_env_t *cenv);

#endif
