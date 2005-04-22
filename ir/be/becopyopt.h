/**
 * Header for copy optimization problem. Analysis and set up of the problem.
 * @author Daniel Grund
 * @date 12.04.2005
 */

#ifndef _BECOPYOPT_H
#define _BECOPYOPT_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <alloca.h>
#include <sys/types.h>
#include <sys/stat.h>

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
#include "bera_t.h"
#include "bechordal_t.h"
#include "bearch.h"

/**
 * TODO external things
 * define get_weight to sth representing the _gain_ if node n and m
 * have the same color. Must return values MIN_WEIGHT <= . <= MAX_WEIGHT.
 */
#define get_weight(n,m) 1
#define is_possible_color(irn, col) 1
#define is_Copy(irn) 0
#define MAX_COLORS 32

/**
 * A single unit of optimization. Lots of these form a copy-opt problem
 */
typedef struct _unit_t {
	struct list_head units;		/**< chain for all units */
	int interf;					/**< number of nodes dropped due to interference */
	int node_count;				/**< size of the nodes array */
	const ir_node **nodes;		/**< [0] is the root-node, others are non interfering args of it. */

	/* for heuristic */
	int mis_size;				/**< size of a mis considering only ifg (not coloring conflicts) */
	struct list_head queue;		/**< list of (mis/color) sorted by size of mis */
} unit_t;

/**
 * Data representing the problem of copy minimization.
 */
typedef struct _copy_opt_t {
	ir_graph *irg;						/**< the irg to process */
	const arch_isa_if_t *isa;
	const arch_register_class_t *cls;	/**< the registerclass to optimize */
	struct list_head units;				/**< all units to optimize in right oreder */
	pset *roots;						/**< used only temporary for detecting multiple appends */
	struct obstack ob;
} copy_opt_t;


/**
 * Generate the problem. Collect all infos and optimizable nodes.
 */
copy_opt_t *new_copy_opt(ir_graph *irg, const arch_isa_if_t *isa, const arch_register_class_t *cls);

/**
 * Free the space...
 */
void free_copy_opt(copy_opt_t *co);

/**
 * Returns the current number of copies needed
 */
int co_get_copy_count(copy_opt_t *co);

/**
 * IMPORTANT: Available only iff heuristic has run!
 * Returns a lower bound for the number of copies needed based on interfering
 * arguments and the size of a max indep. set (only ifg-edges) of the other args.
 */
int co_get_lower_bound(copy_opt_t *co);

/**
 * Returns the number of arguments interfering with their root node. This also
 * is a (worse) lower bound for the number of copies needed.
 */
int co_get_interferer_count(copy_opt_t *co);

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
