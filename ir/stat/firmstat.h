/*
 * Project:     libFIRM
 * File name:   ir/stat/firmstat.h
 * Purpose:     Statistics for Firm.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FIRMSTAT_H_
#define _FIRMSTAT_H_

/**
 * @file firmstat.h
 */
#include "irop.h"
#include "irnode.h"
#include "irgraph.h"

/**
 * Statistic options, can be or'ed.
 */
enum firmstat_options_t {
  FIRMSTAT_ENABLED         = 0x00000001,	/**< enable statistics */
  FIRMSTAT_PATTERN_ENABLED = 0x00000002		/**< enable pattern calculation */
};

/**
 * Finish the statistics.
 * Never called from libFirm should be called from user.
 *
 * @param name   basename of the statistic output file
 */
void stat_finish(const char *name);

#ifdef FIRM_STATISTICS

typedef enum {
  STAT_OPT_STG,			/**< straightening optimization */
  STAT_OPT_IFSIM,		/**< if simplification */
  STAT_OPT_ALGSIM,		/**< algebraic simplification */
  STAT_OPT_PHI,			/**< Phi optmization */
  STAT_OPT_WAW,			/**< Write-After-Write optimization */
  STAT_OPT_WAR,			/**< Write-After-Read optimization */
  STAT_OPT_RAW,			/**< Read-After-Write optimization */
  STAT_OPT_RAR,			/**< Read-After-Read optimization */
  STAT_OPT_TUPLE,		/**< Tuple optimization */
  STAT_OPT_ID,			/**< ID optimization */
  STAT_OPT_CONST_EVAL,		/**< constant evaluation */
  STAT_OPT_STRENGTH_RED,	/**< strenght reduction */
  STAT_LOWERED,			/**< lowered */

  STAT_OPT_MAX
} stat_opt_kind;

/**
 * initialize the statistics module.
 *
 * @param enable_options  Bitmask containing the statistic options
 */
void init_stat(unsigned enable_options);

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
void stat_new_graph(ir_graph *irg, entity *ent);

/**
 * A graph was deleted
 */
void stat_free_graph(ir_graph *irg);

/**
 * A walk over a graph is initiated
 */
void stat_irg_walk(ir_graph *irg, void *pre, void *post);

/**
 * A walk over a graph in block-wise order is initiated
 */
void stat_irg_walk_blkwise(ir_graph *irg, void *pre, void *post);

/**
 * A walk over the graph's blocks is initiated
 */
void stat_irg_block_walk(ir_graph *irg, const ir_node *node, void *pre, void *post);

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

/**
 * A graph was inlined
 */
void stat_inline(ir_node *call, ir_graph *irg);

/**
 * A graph with tail-recursions was optimized.
 */
void stat_tail_rec(ir_graph *irg);

/**
 * Strength reduction was performed on an iteration variable.
 */
void stat_strength_red(ir_graph *irg, ir_node *strong, ir_node *cmp);

/**
 * Start the dead node elimination.
 */
void stat_dead_node_elim_start(ir_graph *irg);

/**
 * Stops the dead node elimination.
 */
void stat_dead_node_elim_stop(ir_graph *irg);

/**
 * helper: get an ir_op from an opcode
 *
 * @param code  the opcode
 *
 * @return  The associated ir_op or NULL if the opcode could not be found.
 */
ir_op *stat_get_op_from_opcode(opcode code);

#else

#define init_stat(enable_options)
#define stat_finish(name)
#define stat_new_ir_op(op)
#define stat_free_ir_op(op)
#define stat_new_node(node)
#define stat_turn_into_id(node)
#define stat_new_graph(irg, ent)
#define stat_free_graph(irg)
#define stat_irg_walk(irg, pre, post)
#define stat_irg_walk_blkwise(irg, pre, post)
#define stat_irg_block_walk(irg, node, pre, post)
#define stat_merge_nodes(new_node_array, new_num_entries, old_node_array, old_num_entries, opt)
#define stat_lower(node)
#define stat_inline(call, irg)
#define stat_tail_rec(irg)
#define stat_strength_red(irg, strong, cmp)
#define stat_dead_node_elim_start(irg)
#define stat_dead_node_elim_stop(irg)

#endif

#endif /* _FIRMSTAT_H_ */
