/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Common stuff used by all ILP formulations.
 * @author      Daniel Grund
 * @date        28.02.2006
 */
#ifndef FIRM_BE_BECOPYILP_T_H
#define FIRM_BE_BECOPYILP_T_H

#include "be_types.h"
#include "bearch.h"
#include "firm_types.h"
#include "irnodeset.h"
#include "lpp.h"

/******************************************************************************
    _____                      _        _____ _      _____
   / ____|                    (_)      |_   _| |    |  __ \
  | |  __  ___ _ __   ___ _ __ _  ___    | | | |    | |__) |
  | | |_ |/ _ \ '_ \ / _ \ '__| |/ __|   | | | |    |  ___/
  | |__| |  __/ | | |  __/ |  | | (__   _| |_| |____| |
   \_____|\___|_| |_|\___|_|  |_|\___| |_____|______|_|

 *****************************************************************************/

#define EPSILON 0.00001

typedef struct ilp_env_t ilp_env_t;

typedef void (*ilp_callback)(ilp_env_t*);

struct ilp_env_t {
	copy_opt_t const *co;          /**< the copy opt problem */
	ir_node         **col_suff;    /**< Coloring suffix for size reduction. A PEO prefix. */
	ir_nodeset_t      all_removed; /**< All nodes removed during problem size reduction */
	lpp_t            *lp;          /**< the linear programming problem */
	void             *env;
	ilp_callback      build;
	ilp_callback      apply;
};

ilp_env_t *new_ilp_env(copy_opt_t *co, ilp_callback build, ilp_callback apply, void *env);

lpp_sol_state_t ilp_go(ilp_env_t *ienv);

void free_ilp_env(ilp_env_t *ienv);

/**
 * Checks if a node has already been removed
 */
static inline bool sr_is_removed(ilp_env_t const *const ienv, ir_node const *const irn)
{
	return ir_nodeset_contains(&ienv->all_removed, irn);
}

static inline unsigned get_irn_col(ir_node const *const node)
{
	return arch_get_irn_register(node)->index;
}

#endif
