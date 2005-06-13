/**
 * Author:      Daniel Grund
 * Date:		12.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Header for copy optimization problem. Analysis and set up of the problem.
 */

#ifndef _BECOPYOPT_H
#define _BECOPYOPT_H

#include "debug.h"
#include "obst.h"
#include "list.h"
#include "set.h"
#include "pset.h"
#include "bitset.h"
#include "sp_matrix.h"

#include "irgraph.h"
#include "irgwalk.h"
#include "irnode.h"
#include "irdom.h"
#include "irouts.h"

#include "beutil.h"
#include "benumb_t.h"
#include "belive_t.h"
#include "bechordal_t.h"
#include "bearch.h"

#define DEBUG_IRG "-scanner.c__init_td__gp"
//TODO is_Perm
#define is_Perm(irn) 0

/**
 * Data representing the problem of copy minimization.
 */
typedef struct _copy_opt_t {
	be_chordal_env_t *chordal_env;
	char *name;							/**< ProgName__IrgName__RegClass */
	struct list_head units;				/**< all units to optimize in right order */
	pset *roots;						/**< used only temporary for detecting multiple appends */
	struct obstack ob;
} copy_opt_t;

/**
 * A single unit of optimization. Lots of these form a copy-opt problem
 */
typedef struct _unit_t {
	struct list_head units;		/**< chain for all units */
	copy_opt_t *co;				/**< the copy_opt this unit belongs to */
	int interf;					/**< number of nodes dropped due to interference */
	int node_count;				/**< size of the nodes array */
	ir_node **nodes;			/**< [0] is the root-node, others are non interfering args of it. */
	int ifg_mis_size;			/**< size of a mis considering only ifg (not coloring conflicts) */

	/* for heuristic */
	struct list_head queue;		/**< list of (mis/color) sorted by size of mis */
} unit_t;

/* Helpers */
#define set_irn_col(co, irn, col) \
	arch_set_irn_register(co->chordal_env->arch_env, irn, 0, arch_register_for_index(co->chordal_env->cls, col))

#define get_irn_col(co, irn) \
	arch_register_get_index(arch_get_irn_register(co->chordal_env->arch_env, irn, 0))


/**
 * Generate the problem. Collect all infos and optimizable nodes.
 */
copy_opt_t *new_copy_opt(be_chordal_env_t *chordal_env);

/**
 * Free the space...
 */
void free_copy_opt(copy_opt_t *co);

/**
 * A copy is a proj haning out of perm node
 */
#define is_Copy(irn) (is_Proj(irn) && is_Perm(get_Proj_pred(irn)))

/**
 * returns the corresponding argument of the perm node for a copy
 */
#define get_Copy_src(irn) (get_irn_n(get_Proj_pred(irn), get_Proj_proj(irn)))

/**
 * Checks if a node is optimizable, viz. is a target of a 'copy-op'
 */
#define is_optimizable(irn) (is_Phi(irn) || is_Copy(irn))

/**
 * Checks if the irn is a non-interfering argument of a node which 'is_optimizable'
 */
int is_optimizable_arg(const copy_opt_t *co, ir_node *irn);


/**
 * Returns the current number of copies needed
 */
int co_get_copy_count(const copy_opt_t *co);

/**
 * Returns a lower bound for the number of copies needed based on interfering
 * arguments and the size of a max indep. set (only ifg-edges) of the other args.
 */
int co_get_lower_bound(const copy_opt_t *co);

/**
 * Returns the number of arguments interfering with their root node. This also
 * is a (worse) lower bound for the number of copies needed.
 */
int co_get_interferer_count(const copy_opt_t *co);

/**
 * Solves the problem using a heuristic approach
 */
void co_heur_opt(copy_opt_t *co);

/**
 * Solves the problem using mixed integer programming
 */
void co_ilp_opt(copy_opt_t *co);

/**
 * Checks the register allocation for correctness
 */
void co_check_allocation(copy_opt_t *co);

#endif
