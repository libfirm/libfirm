/*
 * Project:     libFIRM
 * File name:   ir/ir/firmstat.h
 * Purpose:     Statistics for Firm.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# ifndef _FIRMSTAT_H_
# define _FIRMSTAT_H_

#ifdef FIRM_STATISTICS

typedef enum {
  STAT_OPT_STG        = 0,		/**< straightening optimization */
  STAT_OPT_IFSIM      = 1,		/**< if simplification */
  STAT_OPT_ALGSIM     = 2,		/**< algebraic simplification */
  STAT_OPT_PHI        = 3,		/**< Phi optmization */
  STAT_OPT_WAW        = 4,		/**< Write-After-Write optimization */
  STAT_OPT_WAR        = 5,		/**< Write-After-Read optimization */
  STAT_OPT_TUPLE      = 6,		/**< Tuple optimization */
  STAT_OPT_ID         = 7,		/**< ID optimization */
  STAT_OPT_CONST_EVAL = 8,		/**< constant evaluation */
  STAT_LOWERED        = 9,		/**< lowered */

  STAT_OPT_MAX        = 10
}
stat_opt_kind;

/**
 * initialize the statistics module.
 */
void stat_init(void);

/**
 * Finish the statistics .
 */
void stat_finish(void);

/**
 * A new IR op is registered.
 */
void stat_new_ir_op(const ir_op *op);

/**
 * An IR op is freed.
 */
void stat_free_ir_op(const ir_op *op);

/**
 * A new node is created.
 */
void stat_new_node(const ir_node *node);

/**
 * A node is changed into a Id node
 */
void stat_turn_into_id(const ir_node *node);

/**
 * A new graph was created
 */
void stat_new_graph(const ir_graph *irg);

/**
 * A walk over a graph is initiated
 */
void stat_irg_walk(const ir_graph *irg, void *pre, void *post);

/**
 * A walk over the graph's blocks is initiated
 */
void stat_irg_block_walk(const ir_graph *irg, const ir_node *node, void *pre, void *post);

/**
 * Some nodes were optimized into some others due to an optimization
 */
void stat_merge_nodes(
    ir_node **new_node_array, int new_num_entries,
    ir_node **old_node_array, int old_num_entries,
    stat_opt_kind opt);

/**
 * A node was lowered into other nodes
 */
void stat_lower(ir_node *node);

#else

#define stat_init()
#define stat_finish()
#define stat_new_ir_op(op)
#define stat_free_ir_op(op)
#define stat_new_node(node)
#define stat_turn_into_id(node)
#define stat_new_graph(irg)
#define stat_irg_walk(irg, pre, post)
#define stat_irg_block_walk(irg, node, pre, post)
#define stat_merge_nodes(new_node_array, new_num_entries, old_node_array, old_num_entries, opt)
#define stat_lower(node)

#endif

#endif /* _FIRMSTAT_H_ */
