
/**
 * Internal datastructures for the chordal register allocator.
 * @author Sebastian Hack
 * @date 25.1.2005
 */

#ifndef _BECHORDAL_T_H
#define _BECHORDAL_T_H

#include "firm_types.h"
#include "firm_config.h"

#include <stdlib.h>

#include "bitset.h"
#include "list.h"
#include "obst.h"
#include "pset.h"
#include "pmap.h"
#include "set.h"

#include "be_t.h"
#include "beifg.h"
#include "bera.h"
#include "bearch.h"
#include "bechordal.h"
#include "beirgmod.h"

typedef struct _be_ra_chordal_opts_t be_ra_chordal_opts_t;

/** Defines an invalid register index. */
#define NO_COLOR (-1)

/**
 * A liveness interval border.
 */
typedef struct _border_t {
	unsigned magic;                 /**< A magic number for checking. */
	struct list_head list;          /**< list head for queuing. */
	struct _border_t *other_end;    /**< The other end of the border. */
	ir_node *irn;                   /**< The node. */
	unsigned step;                  /**< The number equal to the interval border. */
	unsigned pressure;              /**< The pressure at this interval border. (The border itself is counting). */
	unsigned is_def : 1;            /**< Does this border denote a use or a def. */
	unsigned is_real : 1;           /**< Is the def/use real? Or is it just inserted
                                        at block beginnings or ends to ensure that inside
                                        a block, each value has one begin and one end. */
} border_t;

/**
 * Environment for each of the chordal register allocator phases
 */
struct _be_chordal_env_t {
	struct obstack obst;                /**< An obstack for temporary storage. */
	be_ra_chordal_opts_t *opts;         /**< A pointer to the chordal ra options. */
	const be_irg_t *birg;               /**< Back-end IRG session. */
	dom_front_info_t *dom_front;        /**< Dominance frontiers. */
	ir_graph *irg;                      /**< The graph under examination. */
	const arch_register_class_t *cls;   /**< The current register class. */
	pmap *border_heads;                 /**< Maps blocks to border heads. */
	be_ifg_t *ifg;                      /**< The interference graph. */
	void *data;                         /**< Some pointer, to which different phases can attach data to. */
	bitset_t *ignore_colors;            /**< A set of colors which shall be ignored in register allocation. */
	DEBUG_ONLY(firm_dbg_module_t *dbg;) /**< Debug module for the chordal register allocator. */
};

static INLINE struct list_head *_get_block_border_head(const be_chordal_env_t *inf, ir_node *bl) {
  return pmap_get(inf->border_heads, bl);
}

#define get_block_border_head(info, bl)     _get_block_border_head(info, bl)

#define foreach_border_head(head, pos)		list_for_each_entry_reverse(border_t, pos, head, list)
#define border_next(b)                      (list_entry((b)->list.next, border_t, list))
#define border_prev(b)                      (list_entry((b)->list.prev, border_t, list))

#define chordal_has_class(chordal_env, irn) \
	arch_irn_consider_in_reg_alloc(chordal_env->birg->main_env->arch_env, chordal_env->cls, irn)

int nodes_interfere(const be_chordal_env_t *env, const ir_node *a, const ir_node *b);

void be_ra_chordal_color(be_chordal_env_t *chordal_env);

/**
 * Check a register allocation obtained with the chordal register allocator.
 * @param chordal_env The chordal environment.
 */
void be_ra_chordal_check(be_chordal_env_t *chordal_env);

enum {
	/* spill method */
	BE_CH_SPILL_BELADY    = 1,
	BE_CH_SPILL_ILP       = 2,

	/* Dump flags */
	BE_CH_DUMP_NONE       = (1 << 0),
	BE_CH_DUMP_SPILL      = (1 << 1),
	BE_CH_DUMP_LIVE       = (1 << 2),
	BE_CH_DUMP_COLOR      = (1 << 3),
	BE_CH_DUMP_COPYMIN    = (1 << 4),
	BE_CH_DUMP_SSADESTR   = (1 << 5),
	BE_CH_DUMP_TREE_INTV  = (1 << 6),
	BE_CH_DUMP_CONSTR     = (1 << 7),
	BE_CH_DUMP_LOWER      = (1 << 8),
	BE_CH_DUMP_ALL        = 2 * BE_CH_DUMP_LOWER - 1,

	/* copymin method */
	BE_CH_COPYMIN_HEUR    = 1,
	BE_CH_COPYMIN_ILP     = 2,

	/* ifg flavor */
	BE_CH_IFG_STD         = 1,
	BE_CH_IFG_FAST        = 2,

	/* lower perm options */
	BE_CH_LOWER_PERM_SWAP   = (1 << 0),
	BE_CH_LOWER_PERM_COPY   = (1 << 1),
	BE_CH_LOWER_PERM_STAT   = (1 << 2)
};

struct _be_ra_chordal_opts_t {
	int dump_flags;
	int spill_method;
	int copymin_method;
	int ifg_flavor;
	int lower_perm_opt;

	char ilp_server[128];
	char ilp_solver[128];
};


#endif /* _BECHORDAL_T_H */
