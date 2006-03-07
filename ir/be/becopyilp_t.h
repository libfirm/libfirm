/**
 * Author:      Daniel Grund
 * Date:		28.02.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Common stuff used by all ILP fomulations.
 *
 */

#ifndef _BECOPYILP_T_H
#define _BECOPYILP_T_H

#include "firm_config.h"

#ifndef _WIN32
 #ifndef HAVE_ALLOCA_H
  #define HAVE_ALLOCA_H 1
 #endif /* HAVE_ALLOC_H */
#endif /* _WIN32 */

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "irnode_t.h"
#include "pset.h"
#include "becopyopt_t.h"

/******************************************************************************
    _____ _                        _            _   _
   / ____(_)                      | |          | | (_)
  | (___  _ _______   _ __ ___  __| |_   _  ___| |_ _  ___  _ __
   \___ \| |_  / _ \ | '__/ _ \/ _` | | | |/ __| __| |/ _ \| '_ \
   ____) | |/ /  __/ | | |  __/ (_| | |_| | (__| |_| | (_) | | | |
  |_____/|_/___\___| |_|  \___|\__,_|\__,_|\___|\__|_|\___/|_| |_|

 *****************************************************************************/

typedef struct _coloring_suffix_t coloring_suffix_t;

struct _coloring_suffix_t {
	coloring_suffix_t *next;
	ir_node *irn;
};

typedef struct _size_red_t {
	copy_opt_t *co;
	pset *all_removed;				/**< All nodes removed during problem size reduction */
	coloring_suffix_t *col_suff;	/**< Coloring suffix. Reverse would be a PEO prefix */
	struct obstack ob;
} size_red_t;

/**
 * Just prepare. Do nothing yet.
 */
size_red_t *new_size_red(copy_opt_t *co);

/**
 * Checks if a node has already been removed
 */
#define sr_is_removed(sr, irn)		pset_find_ptr((sr)->all_removed, irn)

/**
 * Virtually remove all nodes not related to the problem
 * (simplicial AND not adjacent to a equal-color-edge)
 */
void sr_remove(size_red_t *sr);

/**
 * Virtually reinsert the nodes removed before and color them
 */
void sr_reinsert(size_red_t *sr);

/**
 * Free all space...
 */
void free_size_red(size_red_t *sr);

/**
 * TODO: This search is necessary because during the construction of the
 *       units (ou's) args could be merged and weights are accumulated.
 *       Is this necessary?
 */
static INLINE int co_ilp_get_costs(copy_opt_t *co, ir_node *root, ir_node *arg) {
	int i;
	unit_t *curr;

	/* search optimization unit for phi */
	list_for_each_entry(unit_t, curr, &co->units, units)
		if (curr->nodes[0] == root) {

			for (i=1; i<curr->node_count; ++i)
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

#include <lpp/lpp.h>

#undef LPP_SOLVE_NET

#ifdef LPP_SOLVE_NET
#  include <lpp/lpp_net.h>
#  define LPP_HOST "i44pc52"
#  define LPP_SOLVER "cplex"
#else
#  include <lpp/lpp_cplex.h>
#endif

#define EPSILON 0.00001

typedef struct _ilp_env_t ilp_env_t;

typedef void(*ilp_callback)(ilp_env_t*);

struct _ilp_env_t {
	firm_dbg_module_t *dbg;
	const copy_opt_t *co;			/**< the copy opt problem */
	size_red_t *sr;					/**< problem size reduction. removes simple nodes */
	lpp_t *lp;						/**< the linear programming problem */
	void *env;
	ilp_callback build;
	ilp_callback apply;

};

ilp_env_t *new_ilp_env(copy_opt_t *co, firm_dbg_module_t *dbg, ilp_callback build, ilp_callback apply, void *env);

lpp_sol_state_t ilp_go(ilp_env_t *ienv, double time_limit);

void free_ilp_env(ilp_env_t *ienv);


/******************************************************************************


 *****************************************************************************/

#endif /* _BECOPYILP_T_H */
