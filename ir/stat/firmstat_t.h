/*
 * Project:     libFIRM
 * File name:   ir/stat/firmstat_t.h
 * Purpose:     Statistics for Firm. Internal data structures.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FIRMSTAT_T_H_
#define _FIRMSTAT_T_H_

/**
 * @file firmstat_t.h
 */
#include "firmstat.h"

#include "irop_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "pset.h"
#include "irprog.h"
#include "irgwalk.h"
#include "counter.h"

/*
 * just be make some things clear :-), the
 * poor man "generics"
 */
#define HASH_MAP(type) hmap_##type

typedef pset hmap_node_entry_t;
typedef pset hmap_graph_entry_t;
typedef pset hmap_opt_entry_t;
typedef pset hmap_block_entry_t;
typedef pset hmap_ir_op;
typedef pset hmap_distrib_entry_t;


/**
 * An entry for ir_nodes, used in ir_graph statistics.
 */
typedef struct _node_entry_t {
  counter_t   cnt_alive;		/**< amount of nodes in this entry */
  counter_t   new_node;			/**< amount of new nodes for this entry */
  counter_t   into_Id;			/**< amount of nodes that turned into Id's for this entry */
  const ir_op *op;			/**< the op for this entry */
} node_entry_t;

/**
 * An entry for ir_graphs
 */
typedef struct _graph_entry_t {
  HASH_MAP(node_entry_t)  *opcode_hash;			/**< hash map containing the opcode counter */
  HASH_MAP(block_entry_t) *block_hash;			/**< hash map countaining the block counter */
  counter_t               cnt_walked;			/**< walker walked over the graph */
  counter_t               cnt_walked_blocks;		/**< walker walked over the graph blocks */
  counter_t               cnt_was_inlined;		/**< number of times other graph were inlined */
  counter_t               cnt_got_inlined;		/**< number of times this graph was inlined */
  counter_t               cnt_strength_red;		/**< number of times strength reduction was successful on this graph */
  counter_t               cnt_edges;			/**< number of DF edges in this graph */
  counter_t               cnt_all_calls;                /**< number of all calls */
  counter_t               cnt_indirect_calls;           /**< number of indirect calls */
  HASH_MAP(opt_entry_t)   *opt_hash[STAT_OPT_MAX];	/**< hash maps containing opcode counter for optimizations */
  ir_graph                *irg;				/**< the graph of this object */
  entity                  *ent;				/**< the entity of this graph if one exists */
  unsigned                is_deleted:1;			/**< set if this irg was deleted */
  unsigned                is_leaf:1;			/**< set, if this irg is a leaf function */
  unsigned                is_recursive:1;		/**< set, if this irg has recursive calls */
  unsigned                is_chain_call:1;		/**< set, if this irg is a chain call */
} graph_entry_t;

/**
 * An entry for optimized ir_nodes
 */
typedef struct _opt_entry_t {
  counter_t   count;			/**< optimization counter */
  const ir_op *op;			/**< the op for this entry */
} opt_entry_t;

/**
 * An entry for a block in a ir-graph
 */
typedef struct _block_entry_t {
  counter_t  cnt_nodes;			/**< the counter of nodes in this block */
  counter_t  cnt_edges;			/**< the counter of edges in this block */
  counter_t  cnt_in_edges;		/**< the counter of edges incoming from other blocks to this block */
  counter_t  cnt_out_edges;		/**< the counter of edges outgoing from this block to other blocks */
  long       block_nr;			/**< block nr */
} block_entry_t;

/** forward */
typedef struct _dumper_t dumper_t;

/**
 * handler for dumping an IRG
 *
 * @param dmp   the dumper
 * @param entry the IR-graph hash map entry
 */
typedef void (*dump_graph_FUNC)(dumper_t *dmp, graph_entry_t *entry);

/**
 * handler for dumper init
 *
 * @param dmp   the dumper
 * @param name  name of the file to dump to
 */
typedef void (*dump_init_FUNC)(dumper_t *dmp, const char *name);

/**
 * handler for dumper finish
 *
 * @param dmp   the dumper
 */
typedef void (*dump_finish_FUNC)(dumper_t *dmp);


/**
 * a dumper description
 */
struct _dumper_t {
  dump_graph_FUNC         dump_graph;		/**< handler for dumping an irg */
  dump_init_FUNC          init;			/**< handler for init */
  dump_finish_FUNC        finish;		/**< handler for finish */
  FILE                    *f;			/**< the file to dump to */
  dumper_t                *next;		/**< link to the next dumper */
};

/**
 * statistics info
 */
typedef struct _statistic_info_t {
  struct obstack          cnts;			/**< obstack containing the counters */
  HASH_MAP(graph_entry_t) *irg_hash;		/**< hash map containing the counter for irgs */
  HASH_MAP(ir_op)         *ir_op_hash;		/**< hash map containing all ir_ops (accessible by op_codes) */
  int                     recursive;		/**< flag for detecting recursive hook calls */
  int                     in_dead_node_elim;	/**< set, if dead node elimination runs */
  ir_op                   *op_Phi0;		/**< pseudo op for Phi0 */
  ir_op                   *op_PhiM;		/**< pseudo op for memory Phi */
  ir_op                   *op_MulC;		/**< pseudo op for multiplication by const */
  ir_op                   *op_DivC;		/**< pseudo op for division by const */
  ir_op                   *op_ModC;		/**< pseudo op for modulo by const */
  ir_op                   *op_DivModC;		/**< pseudo op for DivMod by const */
  dumper_t                *dumper;		/**< list of dumper */
  int                     reassoc_run;          /**< if set, reassociation is running */
  int                     enable;		/**< if set, statistic is enabled */
} stat_info_t;

/**
 * An entry in a distribution table
 */
typedef struct _distrib_entry_t {
  counter_t	cnt;		/**< the current count */
  const void	*object;	/**< the object which is counted */
} distrib_entry_t;

/** The type of the hash function for objects in distribution tables. */
typedef unsigned (*distrib_hash_fun)(const void *object);

/**
 * The distribution table.
 */
typedef struct _distrib_tbl_t {
  struct obstack          	cnts;		/**< obstack containing the distrib_entry_t entries */
  HASH_MAP(distrib_entry_t)	*hash_map;	/**< the hash map containing the distribution */
  distrib_hash_fun              hash_func;	/**< the hash function for object in this distribution */
  unsigned			int_dist;	/**< non-zero, if it's a integer distribution */
} distrib_tbl_t;

/* API for distribution tables */

/**
 * creates a new distribution table
 *
 * @param cmp_func   Compare function for objects in the distribution
 * @param hash_func  Hash function for objects in the distribution
 */
distrib_tbl_t *stat_new_distrib_tbl(pset_cmp_fun cmp_func, distrib_hash_fun hash_func);

/**
 * creates a new distribution table for an integer distribution
 */
distrib_tbl_t *stat_new_int_distrib_tbl(void);

/**
 * destroys a distribution table
 */
void stat_delete_distrib_tbl(distrib_tbl_t *tbl);

/**
 * adds a new object count into the distribution table
 */
void stat_add_distrib_tbl(distrib_tbl_t *tbl, const void *object, const counter_t *cnt);

/**
 * adds a new key count into the integer distribution table
 */
void stat_add_int_distrib_tbl(distrib_tbl_t *tbl, int key, const counter_t *cnt);

/**
 * calculates the mean value of a distribution
 */
double stat_calc_mean_distrib_tbl(distrib_tbl_t *tbl);

/** evaluates each entry of a distribution table */
typedef void (*eval_distrib_entry_fun)(const distrib_entry_t *entry);

/**
 * iterates over all entries in a distribution table
 */
void stat_iterate_distrib_tbl(distrib_tbl_t *tbl, eval_distrib_entry_fun eval);

#endif /* _FIRMSTAT_T_H_ */
