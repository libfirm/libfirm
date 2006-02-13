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

#define MIS_HEUR_TRIGGER 8

typedef int(*cost_fct_t)(ir_node*, ir_node*, int);

/**
 * Data representing the problem of copy minimization.
 */
typedef struct _copy_opt_t {
	be_chordal_env_t *chordal_env;
	char *name;						/**< ProgName__IrgName__RegClass */
	struct list_head units;			/**< all units to optimize in specific order */
	cost_fct_t get_costs;			/**< function ptr used to get costs for copies */
	struct obstack ob;
} copy_opt_t;

/**
 * A single unit of optimization. Lots of these form a copy-opt problem
 */
typedef struct _unit_t {
	struct list_head units;		/**< chain for all units */
	copy_opt_t *co;				/**< the copy_opt this unit belongs to */
	int node_count;				/**< size of the nodes array */
	ir_node **nodes;			/**< [0] is the root-node, others are non interfering args of it. */
	int *costs;					/**< costs[i] are arising, if nodes[i] has a different color */
	int inevitable_costs;		/**< sum of costs of all args interfering with root */
	int all_nodes_costs;		/**< sum of all costs[i] */
	int min_nodes_costs;		/**< a lower bound for the costs in costs[], determined by a max indep. set */
	int sort_key;				/**< maximum costs. controls the order of ou's in the struct list_head units. */

	/* for heuristic */
	struct list_head queue;		/**< list of qn's sorted by weight of qn-mis */
} unit_t;

/* Helpers */
#define get_arch_env(co) ((co)->chordal_env->main_env->arch_env)
#define get_irg(co)      ((co)->chordal_env->irg)
#define get_irn_col(co, irn) \
	arch_register_get_index(arch_get_irn_register(get_arch_env(co), irn))
#define set_irn_col(co, irn, col) \
	arch_set_irn_register(get_arch_env(co), irn, arch_register_for_index(co->chordal_env->cls, col))


#define list_entry_units(lh) list_entry(lh, unit_t, units)

#define get_Copy_src(irn) (get_irn_n(get_Proj_pred(irn), get_Proj_proj(irn)))
#define is_Perm(arch_env, irn)				(arch_irn_classify(arch_env, irn) == arch_irn_class_perm)
#define is_Reg_Phi(irn)						(is_Phi(irn) && mode_is_data(get_irn_mode(irn)))
#define is_Perm_Proj(arch_env, irn)			(is_Proj(irn) && is_Perm(arch_env, get_Proj_pred(irn)))
#define is_2addr_code(arch_env, irn, req)	(arch_get_register_req(arch_env, req, irn, -1)->type == arch_register_req_type_should_be_same)


/**
 * Generate the problem. Collect all infos and optimizable nodes.
 */
copy_opt_t *new_copy_opt(be_chordal_env_t *chordal_env, int (*get_costs)(ir_node*, ir_node*, int));

/**
 * Free the space...
 */
void free_copy_opt(copy_opt_t *co);

/**
 * Checks if a node is optimizable, viz. has somthing to do with coalescing
 * @param arch The architecture environment
 * @param irn  The irn to check
 * @param req  A register_requirement structure (used to check for 2-addr-code)
 */
#define is_optimizable(arch, irn, req) (!arch_irn_is_ignore(arch, irn) && (is_Reg_Phi(irn) || is_Perm_Proj(arch, irn) || is_2addr_code(arch, irn, req)))

/**
 * Checks if the irn is a non-interfering argument of a node which 'is_optimizable'
 */
int is_optimizable_arg(const copy_opt_t *co, ir_node *irn);

/**
 * Computes the costs of a copy according to loop depth
 * @param pos:	the argument position of arg in the root arguments
 * @return Must be >= 0 in all cases.
 */
int get_costs_loop_depth(ir_node *root, ir_node* arg, int pos);

/**
 * All costs equal 1. Using this will reduce the number of copies.
 * @return Must be >= 0 in all cases.
 */
int get_costs_all_one(ir_node *root, ir_node* arg, int pos);

/**
 * Returns the maximal costs possible, i.e. the costs if all
 * pairs would be assigned different registers.
 */
int co_get_max_copy_costs(const copy_opt_t *co);

/**
 * Returns the inevitable costs, i.e. the costs of
 * all copy pairs which interfere.
 */
int co_get_inevit_copy_costs(const copy_opt_t *co);

/**
 * Returns the current costs the copies are causing.
 * The result includes inevitable costs and the costs
 * of the copies regarding the current register allocation
 */
int co_get_copy_costs(const copy_opt_t *co);

/**
 * Returns a lower bound for the costs of copies in this ou.
 * The result includes inevitable costs and the costs of a
 * minimal costs caused by the nodes of the ou.
 */
int co_get_lower_bound(const copy_opt_t *co);

/**
 * Solves the problem using a heuristic approach
 */
void co_heur_opt(copy_opt_t *co);

/**
 * Solves the problem using mixed integer programming
 * @returns 1 iff solution state was optimal
 */
int co_ilp_opt(copy_opt_t *co, double time_limit);

#endif
