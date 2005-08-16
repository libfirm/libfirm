
/**
 * Internal datastructures for the chordal register allocator.
 * @author Sebastian Hack
 * @date 25.1.2005
 */

#ifndef _BECHORDAL_T_H
#define _BECHORDAL_T_H

#include <stdlib.h>

#include "bitset.h"
#include "list.h"
#include "obst.h"
#include "pset.h"
#include "pmap.h"
#include "set.h"

#include "irnode.h"
#include "irgraph.h"

#include "be_t.h"
#include "bearch.h"

/** Defines an invalid register index. */
#define NO_COLOR (-1)

#define BUILD_GRAPH

#define DBG_CHORDAL "firm.be.ra.chordal"

/**
 * A liveness interval border.
 */
typedef struct _border_t {
	unsigned magic;								/**< A magic number for checking. */
	struct list_head list;				/**< list head for queuing. */
	struct _border_t *other_end;	/**< The other end of the border. */
	ir_node *irn;						      /**< The node. */
	unsigned step;								/**< The number equal to the interval border. */
	unsigned pressure;						/**< The pressure at this interval border.
																	(The border itself is counting). */
	unsigned is_def : 1;					/**< Does this border denote a use or a def. */
	unsigned is_real : 1;					/**< Is the def/use real? Or is it just inserted
																	at block beginnings or ends to ensure that inside
																	a block, each value has one begin and one end. */
} border_t;

/**
 * Environment for each of the chordal register allocator phases
 */
struct _be_chordal_env_t {
	struct obstack obst;	/**< An obstack for temporary storage. */
	const be_main_session_env_t *session_env; /**< The current session. */
	pmap *border_heads;   /**< Maps blocks to border heads. */

#ifdef BUILD_GRAPH
	set *nodes;						/**< The interference graph nodes. */
	set *edges;						/**< The interference graph edges. */
#endif

	bitset_t *live;				/**< A liveness bitset. */
	bitset_t *colors;			/**< The color mask. */
	bitset_t *in_colors;	/**< Colors used by live in values. */
	int colors_n;					/**< The number of colors. */
	const arch_register_class_t *cls;   /**< The current register class. */
	void *data;           /**< Some pointer, to which different
                          phases can attach data to. */
};

typedef struct _be_chordal_env_t be_chordal_env_t;

static INLINE struct list_head *_get_block_border_head(const be_chordal_env_t *inf, ir_node *bl) {
  return pmap_get(inf->border_heads, bl);
}

#define get_block_border_head(info, bl)     _get_block_border_head(info, bl)

int nodes_interfere(const be_chordal_env_t *env, const ir_node *a, const ir_node *b);

#ifdef BUILD_GRAPH
typedef struct _if_node_t {
	int nnr;
	pset *neighb;
} if_node_t;

typedef struct _if_edge_t {
	int src, tgt;
} if_edge_t;

set *be_ra_get_ifg_edges(const be_chordal_env_t *env);
set *be_ra_get_ifg_nodes(const be_chordal_env_t *env);

int ifg_has_edge(const be_chordal_env_t *env, const if_node_t *n1, const if_node_t* n2);

#define ifn_get_degree(ifnode) pset_count(ifnode->neighb)
#define foreach_neighb(ifnode, curr) \
			for(curr=pset_first(ifnode->neighb); curr; curr=pset_next(ifnode->neighb))
#endif

extern void be_ra_chordal_spill(be_chordal_env_t *env);

/**
 * Allocate registers for an ir graph.
 * @param irg The graph.
 * @return Some internal data to be freed with be_ra_chordal_done().
 */
be_chordal_env_t *be_ra_chordal(
    const be_main_session_env_t *env,
    const arch_register_class_t *cls);

/**
 * Check current register allocation for correctness.
 * Interfering nodes have different colors
 * Register constraints
 * O(n^2)
 */
void be_ra_chordal_check(be_chordal_env_t *chordal_env);

/**
 * Free data from the chordal register allocation.
 * @param irg The graph.
 */
void be_ra_chordal_done(be_chordal_env_t *info);

/**
 * Init some things for the chordal register allocator.
 * This must be called before Firm is inited.
 */
void be_ra_chordal_init(void);

/**
 * Check the register pressure in a graph.
 * @param env The sesion env.
 * @param cls The register class to consider.
 */
void be_check_pressure(const be_main_session_env_t *env, const arch_register_class_t *cls);


#endif /* _BECHORDAL_T_H */
