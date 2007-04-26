/**
 * Author:      Daniel Grund
 * Date:		12.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Internal header for copy optimization problem.
 */

#ifndef _BECOPYOPT_T_H
#define _BECOPYOPT_T_H

#include "obst.h"
#include "list.h"
#include "bearch_t.h"
#include "bechordal_t.h"
#include "becopyopt.h"
#include "benodesets.h"

/**
 * Data representing the problem of copy minimization.
 */
struct _copy_opt_t {
	be_chordal_env_t *cenv;
	const arch_register_class_t *cls;
	const arch_env_t *aenv;
	ir_graph *irg;
	char *name;						/**< ProgName__IrgName__RegClassName */
	cost_fct_t get_costs;			/**< function ptr used to get costs for copies */

	/** Representation as optimization units */
	struct list_head units;			/**< all units to optimize in specific order */

	/** Representation in graph structure. Only build on demand */
	struct obstack obst;
	set *nodes;
};

/* Helpers */
#define ASSERT_OU_AVAIL(co)		assert((co)->units.next && "Representation as optimization-units not build")
#define ASSERT_GS_AVAIL(co)		assert((co)->nodes && "Representation as graph not build")

#define get_irn_col(co, irn)		arch_register_get_index(arch_get_irn_register((co)->aenv, irn))
#define set_irn_col(co, irn, col)	arch_set_irn_register((co)->aenv, irn, arch_register_for_index((co)->cls, col))
#define is_curr_reg_class(co, irn)	(arch_get_irn_reg_class((co)->aenv, irn, -1) == (co)->cls)

#define list_entry_units(lh) list_entry(lh, unit_t, units)

#define is_Reg_Phi(irn)						(is_Phi(irn) && mode_is_data(get_irn_mode(irn)))

#define get_Perm_src(irn)                   (get_irn_n(get_Proj_pred(irn), get_Proj_proj(irn)))
#define is_Perm(arch_env, irn)				(arch_irn_classify(arch_env, irn) == arch_irn_class_perm)
#define is_Perm_Proj(arch_env, irn)			(is_Proj(irn) && is_Perm(arch_env, get_Proj_pred(irn)))

static INLINE int is_2addr_code(const arch_register_req_t *req)
{
	return req->type == arch_register_req_type_should_be_same;
}

/******************************************************************************
   ____        _   _    _       _ _          _____ _
  / __ \      | | | |  | |     (_) |        / ____| |
 | |  | |_ __ | |_| |  | |_ __  _| |_ ___  | (___ | |_ ___  _ __ __ _  __ _  ___
 | |  | | '_ \| __| |  | | '_ \| | __/ __|  \___ \| __/ _ \| '__/ _` |/ _` |/ _ \
 | |__| | |_) | |_| |__| | | | | | |_\__ \  ____) | || (_) | | | (_| | (_| |  __/
  \____/| .__/ \__|\____/|_| |_|_|\__|___/ |_____/ \__\___/|_|  \__,_|\__, |\___|
        | |                                                            __/ |
        |_|                                                           |___/
 ******************************************************************************/

#define MIS_HEUR_TRIGGER 8

typedef struct _unit_t {
	struct list_head units;		/**< chain for all units */
	copy_opt_t *co;				/**< the copy opt this unit belongs to */
	int node_count;				/**< size of the nodes array */
	ir_node **nodes;			/**< [0] is the root-node, others are non interfering args of it. */
	int *costs;					/**< costs[i] are incurred, if nodes[i] has a different color */
	int inevitable_costs;		/**< sum of costs of all args interfering with root */
	int all_nodes_costs;		/**< sum of all costs[i] */
	int min_nodes_costs;		/**< a lower bound for the costs in costs[], determined by a max independent set */
	int sort_key;				/**< maximum costs. controls the order of ou's in the struct list_head units. */

	/* for heuristic */
	struct list_head queue;		/**< list of qn's sorted by weight of qn-mis */
} unit_t;



/******************************************************************************
   _____                 _        _____ _
  / ____|               | |      / ____| |
 | |  __ _ __ __ _ _ __ | |__   | (___ | |_ ___  _ __ __ _  __ _  ___
 | | |_ | '__/ _` | '_ \| '_ \   \___ \| __/ _ \| '__/ _` |/ _` |/ _ \
 | |__| | | | (_| | |_) | | | |  ____) | || (_) | | | (_| | (_| |  __/
  \_____|_|  \__,_| .__/|_| |_| |_____/ \__\___/|_|  \__,_|\__, |\___|
                  | |                                       __/ |
                  |_|                                      |___/
 ******************************************************************************/

typedef struct _neighb_t neighb_t;
typedef struct _affinity_node_t affinity_node_t;

struct _neighb_t {
	neighb_t *next;			/** the next neighbour entry*/
	ir_node *irn;			/** the neighbour itself */
	int costs;				/** the costs of the edge (affinity_node_t->irn, neighb_t->irn) */
};

struct _affinity_node_t {
	ir_node *irn;			/** a node with affinity edges */
	int degree;				/** number of affinity edges in the linked list below */
	neighb_t *neighbours;	/** a linked list of all affinity neighbours */
	void *data;             /** stuff that is attachable. */
};


static INLINE affinity_node_t *get_affinity_info(const copy_opt_t *co, ir_node *irn) {
	affinity_node_t find;

	ASSERT_GS_AVAIL(co);

	find.irn = irn;
	return set_find(co->nodes, &find, sizeof(find), nodeset_hash(irn));
}

#define co_gs_nodes_begin(co)			set_first((co)->nodes)
#define co_gs_nodes_next(co)			set_next((co)->nodes)
#define co_gs_nodes_break(co)			set_break((co)->nodes)

#define co_gs_foreach_aff_node(co, aff_node)	for (aff_node = co_gs_nodes_begin(co); aff_node; aff_node = co_gs_nodes_next(co))
#define co_gs_foreach_neighb(aff_node, neighb)	for (neighb = aff_node->neighbours; neighb; neighb = neighb->next)


#endif
