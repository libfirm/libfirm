/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Internal header for copy optimization problem.
 * @author      Daniel Grund
 * @date        12.04.2005
 */
#ifndef FIRM_BE_BECOPYOPT_T_H
#define FIRM_BE_BECOPYOPT_T_H

#include "obst.h"
#include "list.h"
#include "set.h"
#include "irnode_t.h"
#include "bechordal_t.h"
#include "becopyopt.h"

/**
 * Data representing the problem of copy minimization.
 */
struct copy_opt_t {
	be_chordal_env_t            *cenv;
	const arch_register_class_t *cls;
	ir_graph                    *irg;
	cost_fct_t                   get_costs; /**< function ptr used to get costs for copies */

	/** Representation as optimization units */
	struct list_head units;  /**< all units to optimize in specific order */

	/** Representation in graph structure. Only build on demand */
	struct obstack obst;
	set           *nodes;
};

#define ASSERT_OU_AVAIL(co)     assert((co)->units.next && "Representation as optimization-units not built")
#define ASSERT_GS_AVAIL(co)     assert((co)->nodes && "Representation as graph not built")

#define get_Perm_src(irn) (get_irn_n(get_Proj_pred(irn), get_Proj_num(irn)))
#define is_Perm_Proj(irn) (is_Proj(irn) && be_is_Perm(get_Proj_pred(irn)))

/**
 * Returns the inevitable costs, i.e. the costs of
 * all copy pairs which interfere.
 * Uses the OU data structure
 */
int co_get_inevit_copy_costs(const copy_opt_t *co);

/**
 * Returns a lower bound for the costs of copies in this ou.
 * The result includes inevitable costs and the costs of a
 * minimal costs caused by the nodes of the ou.
 * Uses the OU data structure
 */
int co_get_lower_bound(const copy_opt_t *co);

/**
 * Checks if a node is optimizable, viz has something to do with coalescing.
 * Uses the GRAPH data structure
 */
bool co_gs_is_optimizable(copy_opt_t const *co, ir_node *irn);

typedef struct unit_t {
	struct list_head units;            /**< chain for all units */
	int              node_count;       /**< size of the nodes array */
	ir_node        **nodes;            /**< [0] is the root-node, others are non interfering args of it. */
	int             *costs;            /**< costs[i] are incurred, if nodes[i] has a different color */
	int              inevitable_costs; /**< sum of costs of all args interfering with root */
	int              all_nodes_costs;  /**< sum of all costs[i] */
	int              min_nodes_costs;  /**< a lower bound for the costs in costs[], determined by a max independent set */
	int              sort_key;         /**< maximum costs. controls the order of ou's in the struct list_head units. */

	/* for heuristic */
	struct list_head queue;            /**< list of qn's sorted by weight of qn-mis */
} unit_t;

typedef struct neighb_t        neighb_t;
typedef struct affinity_node_t affinity_node_t;

struct neighb_t {
	neighb_t      *next;  /** the next neighbour entry*/
	const ir_node *irn;   /** the neighbour itself */
	int            costs; /** the costs of the edge (affinity_node_t->irn, neighb_t->irn) */
};

struct affinity_node_t {
	const ir_node  *irn;        /** a node with affinity edges */
	neighb_t       *neighbours; /** a linked list of all affinity neighbours */
};


static inline affinity_node_t *get_affinity_info(const copy_opt_t *co, const ir_node *irn)
{
	ASSERT_GS_AVAIL(co);

	affinity_node_t find;
	find.irn = irn;
	return set_find(affinity_node_t, co->nodes, &find, sizeof(find), hash_irn(irn));
}

#define co_gs_foreach_aff_node(co, aff_node)     foreach_set((co)->nodes, affinity_node_t, (aff_node))
#define co_gs_foreach_neighb(aff_node, neighb)   for (neighb_t *neighb = aff_node->neighbours; neighb; neighb = neighb->next)

#endif
