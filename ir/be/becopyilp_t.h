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

#include "firm_types.h"
#include "becopyopt_t.h"

/******************************************************************************
    _____ _                        _            _   _
   / ____(_)                      | |          | | (_)
  | (___  _ _______   _ __ ___  __| |_   _  ___| |_ _  ___  _ __
   \___ \| |_  / _ \ | '__/ _ \/ _` | | | |/ __| __| |/ _ \| '_ \
   ____) | |/ /  __/ | | |  __/ (_| | |_| | (__| |_| | (_) | | | |
  |_____/|_/___\___| |_|  \___|\__,_|\__,_|\___|\__|_|\___/|_| |_|

 *****************************************************************************/

typedef struct coloring_suffix_t coloring_suffix_t;

struct coloring_suffix_t {
	coloring_suffix_t *next;
	ir_node           *irn;
};

typedef struct size_red_t {
	copy_opt_t        *co;
	ir_nodeset_t       all_removed;   /**< All nodes removed during problem size reduction */
	coloring_suffix_t *col_suff;      /**< Coloring suffix. Reverse would be a PEO prefix */
	struct obstack    ob;
} size_red_t;

/**
 * Checks if a node has already been removed
 */
static inline bool sr_is_removed(size_red_t const *const sr, ir_node const *const irn)
{
	return ir_nodeset_contains(&sr->all_removed, irn);
}

/**
 * TODO: This search is necessary because during the construction of the
 *       units (ou's) args could be merged and weights are accumulated.
 *       Is this necessary?
 */
static inline int co_ilp_get_costs(copy_opt_t *co, ir_node *root, ir_node *arg) {
	int i;

	/* search optimization unit for phi */
	list_for_each_entry(unit_t, curr, &co->units, units)
		if (curr->nodes[0] == root) {

			for (i = 1; i < curr->node_count; ++i)
				if (curr->nodes[i] == arg)
					return curr->costs[i];

				assert(0 && "irn must occur in this ou");
		}

	assert(0 && "phi must be found in a ou");
	return 0;
}

/******************************************************************************
    _____                      _        _____ _      _____
   / ____|                    (_)      |_   _| |    |  __ \
  | |  __  ___ _ __   ___ _ __ _  ___    | | | |    | |__) |
  | | |_ |/ _ \ '_ \ / _ \ '__| |/ __|   | | | |    |  ___/
  | |__| |  __/ | | |  __/ |  | | (__   _| |_| |____| |
   \_____|\___|_| |_|\___|_|  |_|\___| |_____|______|_|

 *****************************************************************************/

#include "lpp.h"
#include "lpp_net.h"

#define EPSILON 0.00001

typedef struct ilp_env_t ilp_env_t;

typedef void(*ilp_callback)(ilp_env_t*);

struct ilp_env_t {
	const copy_opt_t *co;   /**< the copy opt problem */
	size_red_t       *sr;   /**< problem size reduction. removes simple nodes */
	lpp_t            *lp;   /**< the linear programming problem */
	void             *env;
	ilp_callback     build;
	ilp_callback     apply;
};

ilp_env_t *new_ilp_env(copy_opt_t *co, ilp_callback build, ilp_callback apply, void *env);

lpp_sol_state_t ilp_go(ilp_env_t *ienv);

void free_ilp_env(ilp_env_t *ienv);

#endif
