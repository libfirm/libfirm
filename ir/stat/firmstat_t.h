/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Statistics for Firm. Internal data structures.
 * @author  Michael Beck
 * @version $Id$
 */
#ifndef FIRM_STAT_FIRMSTAT_T_H
#define FIRM_STAT_FIRMSTAT_T_H

#include "firmstat.h"

#include "irop_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "pset.h"
#include "pdeq.h"
#include "irprog.h"
#include "irgwalk.h"
#include "counter.h"
#include "irhooks.h"

/* some useful macro. */
#define ARR_SIZE(a)   (sizeof(a)/sizeof((a)[0]))

/*
 * just be make some things clear :-), the
 * poor man "generics"
 */
#define HASH_MAP(type) hmap_##type

typedef pset hmap_node_entry_t;
typedef pset hmap_graph_entry_t;
typedef pset hmap_opt_entry_t;
typedef pset hmap_block_entry_t;
typedef pset hmap_be_block_entry_t;
typedef pset hmap_reg_pressure_entry_t;
typedef pset hmap_perm_stat_entry_t;
typedef pset hmap_perm_class_entry_t;
typedef pset hmap_ir_op;
typedef pset hmap_distrib_entry_t;

/**
 * An entry in a distribution table
 */
typedef struct _distrib_entry_t {
	counter_t	 cnt;       /**< the current count */
	const void *object;   /**< the object which is counted */
} distrib_entry_t;

/** The type of the hash function for objects in distribution tables. */
typedef unsigned (*distrib_hash_fun)(const void *object);

/**
 * The distribution table.
 */
typedef struct _distrib_tbl_t {
	struct obstack            cnts;       /**< obstack containing the distrib_entry_t entries */
	HASH_MAP(distrib_entry_t) *hash_map;  /**< the hash map containing the distribution */
	distrib_hash_fun          hash_func;  /**< the hash function for object in this distribution */
	unsigned                  int_dist;   /**< non-zero, if it's a integer distribution */
} distrib_tbl_t;

/**
 * possible address marker values
 */
enum adr_marker_t {
	MARK_ADDRESS_CALC     = 1,    /**< the node is an address expression */
	MARK_REF_ADR          = 2,    /**< the node is referenced by an address expression */
	MARK_REF_NON_ADR      = 4,    /**< the node is referenced by a non-address expression */
};

/**
 * An entry in the address_mark set
 */
typedef struct _address_mark_entry_t {
  ir_node  *node;               /**< the node which this entry belongs to, needed for compare */
  unsigned mark;                /**< the mark, a bitmask of enum adr_marker_t */
} address_mark_entry_t;

/**
 * An entry for ir_nodes, used in ir_graph statistics.
 */
typedef struct _node_entry_t {
	counter_t   cnt_alive;    /**< amount of nodes in this entry */
	counter_t   new_node;     /**< amount of new nodes for this entry */
	counter_t   into_Id;      /**< amount of nodes that turned into Id's for this entry */
	const ir_op *op;          /**< the op for this entry */
} node_entry_t;

enum leaf_call_state_t {
	LCS_UNKNOWN       = 0,      /**< state is unknown yet */
	LCS_LEAF_CALL     = 1,      /**< only leaf functions will be called */
	LCS_NON_LEAF_CALL = 2,      /**< at least one non-leaf function will be called or indetermined */
};

/**
 * Graph counter indexes. The first one are accumulated once, the other are always deleted before an
 * snapshot is taken.
 */
enum graph_counter_names {
	gcnt_acc_walked,               /**< walker walked over the graph, accumulated */
	gcnt_acc_walked_blocks,        /**< walker walked over the graph blocks, accumulated */
	gcnt_acc_was_inlined,          /**< number of times other graph were inlined, accumulated */
	gcnt_acc_got_inlined,          /**< number of times this graph was inlined, accumulated */
	gcnt_acc_strength_red,         /**< number of times strength reduction was successful on this graph, accumulated */
	gcnt_acc_real_func_call,       /**< number real function call optimization, accumulated */

	/* --- non-accumulated values from here */
	_gcnt_non_acc,                 /**< first non-accumulated counter */

	gcnt_edges = _gcnt_non_acc,    /**< number of DF edges in this graph */
	gcnt_all_calls,                /**< number of all calls */
	gcnt_call_with_cnst_arg,       /**< number of calls with const args */
	gcnt_call_with_all_cnst_arg,   /**< number of calls with all const args */
	gcnt_call_with_local_adr,      /**< number of calls with address of local var args */
	gcnt_indirect_calls,           /**< number of indirect calls */
	gcnt_pure_adr_ops,             /**< number of pure address operation */
	gcnt_all_adr_ops,              /**< number of all address operation */
	gcnt_global_adr,               /**< number of global load/store addresses. */
	gcnt_local_adr,                /**< number of local load/store addresses. */
	gcnt_param_adr,                /**< number of parameter load/store addresses. */
	gcnt_this_adr,                 /**< number of this load/store addresses. */
	gcnt_other_adr,                /**< number of other load/store addresses. */
	gcnt_if_conv,                  /**< number of if conversions */

	/* --- must be the last enum constant --- */
	_gcnt_last = gcnt_if_conv + IF_RESULT_LAST                 /**< number of counters */
};

/**
 * An entry for ir_graphs. These numbers are calculated for every IR graph.
 */
typedef struct _graph_entry_t {
	struct obstack             recalc_cnts;                  /**< obstack containing the counters that are recalculated */
	HASH_MAP(node_entry_t)     *opcode_hash;                 /**< hash map containing the opcode counter */
	HASH_MAP(block_entry_t)    *block_hash;                  /**< hash map containing the block counter */
	HASH_MAP(block_entry_t)    *extbb_hash;                  /**< hash map containing the extended block counter */
	HASH_MAP(be_block_entry_t) *be_block_hash;               /**< hash map containing backend block information */
	counter_t                  cnt[_gcnt_last];               /**< counter */
	unsigned                   num_tail_recursion;           /**< number of tail recursion optimizations */
	HASH_MAP(opt_entry_t)      *opt_hash[FS_OPT_MAX];        /**< hash maps containing opcode counter for optimizations */
	ir_graph                   *irg;                         /**< the graph of this object */
	ir_entity                  *ent;                         /**< the entity of this graph if one exists */
	set                        *address_mark;                /**< a set containing the address marks of the nodes */
	unsigned                   is_deleted:1;                 /**< set if this irg was deleted */
	unsigned                   is_leaf:1;                    /**< set, if this irg is a leaf function */
	unsigned                   is_leaf_call:2;               /**< set, if this irg calls only leaf functions */
	unsigned                   is_recursive:1;               /**< set, if this irg has recursive calls */
	unsigned                   is_chain_call:1;              /**< set, if this irg is a chain call */
	unsigned                   is_analyzed:1;                /**< helper: set, if this irg was already analysed */
} graph_entry_t;

/**
 * An entry for optimized ir_nodes
 */
typedef struct _opt_entry_t {
	counter_t   count;    /**< optimization counter */
	const ir_op *op;      /**< the op for this entry */
} opt_entry_t;

/**
 * An entry for register pressure.
 */
typedef struct _reg_pressure_entry_t {
	const char *class_name; /**< name of the register class */
	int         pressure;   /**< the register pressure for this class */
} reg_pressure_entry_t;

/**
 * An entry for permutation statistics.
 */
typedef struct _perm_stat_entry_t {
	ir_node       *perm;       /**< the perm node */
	int            size;       /**< complete size */
	int            real_size;  /**< number of pairs with different registers */
	int            n_copies;   /**< number of copies created for lowering */
	int            n_exchg;    /**< number of exchanges created for lowering */
	distrib_tbl_t *cycles;     /**< distribution of cycle lengths */
	distrib_tbl_t *chains;     /**< distribution of chain lengths */
} perm_stat_entry_t;

/**
 * An entry for permutation statistics per class.
 */
typedef struct _perm_class_entry_t {
	const char                  *class_name; /**< name of the register class */
	int                          n_regs;     /**< number of register in this class */
	HASH_MAP(perm_stat_entry_t) *perm_stat;  /**< statistics about all perm nodes of this class */
} perm_class_entry_t;

/**
 * An entry for a block or extended block in a ir-graph
 */
typedef struct _be_block_entry_t {
	long                           block_nr;         /**< block nr */
	distrib_tbl_t                  *sched_ready;     /**< distribution of ready nodes per block */
	/**< the highest register pressures for this block for each register class */
	HASH_MAP(reg_pressure_entry_t) *reg_pressure;
	HASH_MAP(perm_class_entry_t)   *perm_class_stat; /**< statistics about perm nodes for each register class */
} be_block_entry_t;

/**
 * Block counter indexes. The first one are accumulated once, the other are always deleted before an
 * snapshot is taken.
 */
enum block_counter_names {
	bcnt_nodes,     /**< the counter of nodes in this block */
	bcnt_edges,     /**< the counter of edges in this block */
	bcnt_in_edges,  /**< the counter of edges incoming from other blocks to this block */
	bcnt_out_edges, /**< the counter of edges outgoing from this block to other blocks */
	bcnt_phi_data,  /**< the counter of data Phi nodes in this block */

	/* --- must be the last enum constant --- */
	_bcnt_last      /**< number of counters */
};

/**
 * An entry for a block or extended block in a ir-graph
 */
typedef struct _block_entry_t {
  counter_t       cnt[_bcnt_last];  /**< counter */
  long            block_nr;         /**< block nr */
} block_entry_t;

/** An entry for an extended block in a ir-graph */
typedef block_entry_t extbb_entry_t;

/**
 * Some potential interesting float values
 */
typedef enum _float_classify_t {
	STAT_FC_0,                /**< the float value 0.0 */
	STAT_FC_1,                /**< the float value 1.0 */
	STAT_FC_2,                /**< the float value 2.0 */
	STAT_FC_0_5,              /**< the float value 0.5 */
	STAT_FC_EXACT,            /**< an exact value */
	STAT_FC_OTHER,            /**< all other values */
	STAT_FC_MAX               /**< last value */
} float_classify_t;

/**
 * constant info
 */
typedef struct _constant_info_t {
	counter_t  int_bits_count[32];  /**< distribution of bit sizes of integer constants */
	counter_t  floats[STAT_FC_MAX]; /**< floating point constants */
	counter_t  others;              /**< all other constants */
} constant_info_t;

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
 * handler for dumper a constant info table
 *
 * @param dmp   the dumper
 */
typedef void (*dump_const_table_FUNC)(dumper_t *dmp, const constant_info_t *tbl);

/**
 * dumps the parameter distribution table
 */
typedef void (*dump_param_tbl_FUNC)(dumper_t *dmp, const distrib_tbl_t *tbl, graph_entry_t *global);

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
 * statistics info
 */
typedef struct _statistic_info_t {
	unsigned                stat_options;        /**< statistic options: field must be first */
	struct obstack          cnts;                /**< obstack containing the counters that are incremented */
	struct obstack          be_data;             /**< obstack containing backend statistics data */
	HASH_MAP(graph_entry_t) *irg_hash;           /**< hash map containing the counter for irgs */
	HASH_MAP(ir_op)         *ir_op_hash;         /**< hash map containing all ir_ops (accessible by op_codes) */
	pdeq                    *wait_q;             /**< wait queue for leaf call decision */
	unsigned                recursive:1;         /**< flag for detecting recursive hook calls */
	unsigned                in_dead_node_elim:1; /**< flag for dead node elimination runs */
	ir_op                   *op_Phi0;            /**< pseudo op for Phi0 */
	ir_op                   *op_PhiM;            /**< pseudo op for memory Phi */
	ir_op                   *op_ProjM;           /**< pseudo op for memory Proj */
	ir_op                   *op_MulC;            /**< pseudo op for multiplication by const */
	ir_op                   *op_DivC;            /**< pseudo op for division by const */
	ir_op                   *op_ModC;            /**< pseudo op for modulo by const */
	ir_op                   *op_DivModC;         /**< pseudo op for DivMod by const */
	ir_op                   *op_SelSel;          /**< pseudo op for Sel(Sel) */
	ir_op                   *op_SelSelSel;       /**< pseudo op for Sel(Sel(Sel)) */
	dumper_t                *dumper;             /**< list of dumper */
	int                     reassoc_run;         /**< if set, reassociation is running */
	constant_info_t         const_info;          /**< statistic info for constants */
	distrib_tbl_t           *dist_param_cnt;     /**< distributation table for call parameters */
} stat_info_t;

/**
 * a dumper description
 */
struct _dumper_t {
	dump_graph_FUNC         dump_graph;     /**< handler for dumping an irg */
	dump_const_table_FUNC   dump_const_tbl; /**< handler for dumping a const table */
	dump_param_tbl_FUNC     dump_param_tbl; /**< handler for dumper the Call parameter table */
	dump_init_FUNC          init;           /**< handler for init */
	dump_finish_FUNC        finish;         /**< handler for finish */
	FILE                    *f;             /**< the file to dump to */
	stat_info_t             *status;        /**< access to the global status */
	dumper_t                *next;          /**< link to the next dumper */
	pset                    *func_map;      /**< pset containing all registered functions */
	unsigned                tag;            /**< the id tag of the dumper */
};

/**
 * helper: get an ir_op from an opcode
 */
ir_op *stat_get_op_from_opcode(ir_opcode code);

/* API for distribution tables */

/**
 * creates a new distribution table.
 *
 * @param cmp_func   Compare function for objects in the distribution
 * @param hash_func  Hash function for objects in the distribution
 */
distrib_tbl_t *stat_new_distrib_tbl(pset_cmp_fun cmp_func, distrib_hash_fun hash_func);

/**
 * creates a new distribution table for an integer distribution.
 */
distrib_tbl_t *stat_new_int_distrib_tbl(void);

/**
 * destroys a distribution table.
 */
void stat_delete_distrib_tbl(distrib_tbl_t *tbl);

/**
 * adds a new object count into the distribution table.
 */
void stat_add_distrib_tbl(distrib_tbl_t *tbl, const void *object, const counter_t *cnt);

/**
 * adds a new key count into the integer distribution table.
 */
void stat_add_int_distrib_tbl(distrib_tbl_t *tbl, int key, const counter_t *cnt);

/**
 * increases object count by one
 */
void stat_inc_distrib_tbl(distrib_tbl_t *tbl, const void *object);

/**
 * increases key count by one
 */
void stat_inc_int_distrib_tbl(distrib_tbl_t *tbl, int key);

/**
 * inserts a new object with count 0 into the distribution table
 * if object is already present, nothing happens
 */
void stat_insert_distrib_tbl(distrib_tbl_t *tbl, const void *object);

/**
 * inserts a new key with count 0 into the integer distribution table
 * if key is already present, nothing happens
 */
void stat_insert_int_distrib_tbl(distrib_tbl_t *tbl, int key);

/**
 * returns the sum over all counters in a distribution table
 */
int stat_get_count_distrib_tbl(distrib_tbl_t *tbl);

/**
 * calculates the mean value of a distribution.
 */
double stat_calc_mean_distrib_tbl(distrib_tbl_t *tbl);

/**
 * calculates the average value of a distribution
 */
double stat_calc_avg_distrib_tbl(distrib_tbl_t *tbl);

/** evaluates each entry of a distribution table. */
typedef void (*eval_distrib_entry_fun)(const distrib_entry_t *entry, void *env);

/**
 * iterates over all entries in a distribution table
 */
void stat_iterate_distrib_tbl(const distrib_tbl_t *tbl, eval_distrib_entry_fun eval, void *env);

/**
 * update info on Consts.
 *
 * @param node   The Const node
 * @param graph  The graph entry containing the call
 */
void stat_update_const(stat_info_t *status, ir_node *node, graph_entry_t *graph);

/**
 * clears the const statistics for a new snapshot.
 */
void stat_const_clear(stat_info_t *status);

/**
 * initialize the Const statistic.
 */
void stat_init_const_cnt(stat_info_t *status);

/**
 * return a human readable name for an float classification
 */
const char *stat_fc_name(float_classify_t classification);

/**
 * Update the register pressure of a block
 *
 * @param irg        the irg containing the block
 * @param block      the block for which the reg pressure should be set
 * @param pressure   the pressure
 * @param class_name the name of the register class
 */
void stat_be_block_regpressure(ir_graph *irg, ir_node *block, int pressure, const char *class_name);

/**
 * Update the distribution of ready nodes of a block
 *
 * @param irg        the irg containing the block
 * @param block      the block for which the reg pressure should be set
 * @param num_ready  the number of ready nodes
 */
void stat_be_block_sched_ready(ir_graph *irg, ir_node *block, int num_ready);

/**
 * Update the permutation statistic of a block
 *
 * @param class_name the name of the register class
 * @param perm       the perm node
 * @param block      the block containing the perm
 * @param size       the size of the perm
 * @param real_size  number of pairs with different registers
 */
void stat_be_block_stat_perm(const char *class_name, int n_regs, ir_node *perm, ir_node *block,
                             int size, int real_size);

/**
 * Update the permutation statistic of a single perm
 *
 * @param class_name the name of the register class
 * @param perm       the perm node
 * @param block      the block containing the perm
 * @param is_chain   1 if chain, 0 if cycle
 * @param size       length of the cycle/chain
 * @param n_ops      the number of ops representing this cycle/chain after lowering
 */
void stat_be_block_stat_permcycle(const char *class_name, ir_node *perm, ir_node *block,
                                  int is_chain, int size, int n_ops);

/**
 * Register an additional function for all dumper.  This function
 * is called in dump_snapshot once for each graph_entry and dumper.
 *
 * @param func  the dump function to register
 */
void stat_register_dumper_func(dump_graph_FUNC func);

#endif /* FIRM_STAT_FIRMSTAT_T_H */
