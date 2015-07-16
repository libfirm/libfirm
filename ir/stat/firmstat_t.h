/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Statistics for Firm. Internal data structures.
 * @author  Michael Beck
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
typedef pset hmap_ir_op;
typedef pset hmap_distrib_entry_t;

/**
 * Statistic options, can be or'ed.
 */
enum firmstat_options_t {
	FIRMSTAT_ENABLED         = 0x00000001,    /**< enable statistics */
	FIRMSTAT_PATTERN_ENABLED = 0x00000002,    /**< enable pattern calculation */
	FIRMSTAT_COUNT_STRONG_OP = 0x00000004,    /**< if set, count Mul/Div/Mod by constant */
	FIRMSTAT_COUNT_DAG       = 0x00000008,    /**< if set, count DAG statistics */
	FIRMSTAT_COUNT_DELETED   = 0x00000010,    /**< if set, count deleted graphs */
	FIRMSTAT_COUNT_SELS      = 0x00000020,    /**< if set, count Sel(Sel(..)) differently */
	FIRMSTAT_COUNT_CONSTS    = 0x00000040,    /**< if set, count Const statistics */
	FIRMSTAT_CSV_OUTPUT      = 0x10000000     /**< CSV output of some mini-statistic */
};

/**
 * Additional flags for statistics.
 */
enum firmstat_optimizations_t {
	FS_OPT_NEUTRAL_0  = HOOK_OPT_LAST,        /**< a op 0 = 0 op a = a */
	FS_OPT_NEUTRAL_1,                         /**< a op 1 = 1 op a = a */
	FS_OPT_ADD_A_A,                           /**< a + a = a * 2 */
	FS_OPT_ADD_A_MINUS_B,                     /**< a + -b = a - b */
	FS_OPT_ADD_SUB,                           /**< (a + x) - x = (a - x) + x */
	FS_OPT_ADD_MUL_A_X_A,                     /**< a * x + a = a * (x + 1) */
	FS_OPT_SUB_0_A,                           /**< 0 - a = -a */
	FS_OPT_MINUS_SUB,                         /**< - (a - b) = b - a */
	FS_OPT_SUB_MINUS,                         /**< a - (-b) = a + b */
	FS_OPT_SUB_MUL_A_X_A,                     /**< a * x - a = a * (x - 1) */
	FS_OPT_SUB_SUB_X_Y_Z,                     /**< (x - y) - z = x - (y + z) */
	FS_OPT_SUB_C_NOT_X,                       /**< c - ~a = a + (c+1) */
	FS_OPT_SUB_TO_ADD,                        /**< (-a) - b = -(a + b), a - (b - c) = a + (c - b), a - (b * C) -> a + (b * -C) */
	FS_OPT_SUB_TO_NOT,                        /**< -1 - x -> ~x on two's complement */
	FS_OPT_SUB_TO_CONV,                       /**< a - NULL = (int)a */
	FS_OPT_MUL_MINUS,                         /**< (-a) * (b - c) -> a * (c - b) */
	FS_OPT_MUL_MINUS_1,                       /**< a * -1 = -a */
	FS_OPT_MINUS_MUL_C,                       /**< (-a) * C = a * (-C) */
	FS_OPT_MUL_MINUS_MINUS,                   /**< (-a) * (-b) = a * b */
	FS_OPT_OR,                                /**< a | a = a | 0 = 0 | a = a */
	FS_OPT_AND,                               /**< a & 0b1...1 = 0b1...1 & a =  a & a = a */
	FS_OPT_TO_EOR,                            /**< (a|b) & ~(a&b) = a^b */
	FS_OPT_EOR_A_A,                           /**< a ^ a = 0 */
	FS_OPT_EOR_A_B_A,                         /**< (a ^ b) ^ a = b */
	FS_OPT_EOR_TO_NOT_BOOL,                   /**< bool ^ 1 = !bool */
	FS_OPT_EOR_TO_NOT,                        /**< x ^ 0b1..1 = ~x, (a ^ b) & b -> ~a & b */
	FS_OPT_NOT_CMP,                           /**< !(a cmp b) = a !cmp b */
	FS_OPT_REASSOC_SHIFT,                     /**< (x SHF c1) SHF c2 = x SHF (c1+c2) */
	FS_OPT_SHIFT_AND,                         /**< (a SHF c) AND (b SHF c) = (a AND b) SHF c */
	FS_OPT_SHIFT_OR,                          /**< (a SHF c) OR (b SHF c) = (a OR b) SHF c */
	FS_OPT_SHIFT_EOR,                         /**< (a SHF c) XOR (b SHF c) = (a XOR b) SHF c */
	FS_OPT_CONV,                              /**< a Conv could be removed */
	FS_OPT_MIN_MAX_EQ,                        /**< Min(a,a) = Max(a,a) = a */
	FS_OPT_MUX_COMBINE,                       /**< two Mux nodes where combined into one */
	FS_OPT_MUX_CONV,                          /**< MuxI(sel, 1, 0) = (I)sel */
	FS_OPT_MUX_BOOL,                          /**< Muxb(sel, true, false) = sel */
	FS_OPT_MUX_NOT_BOOL,                      /**< Muxb(sel, false, true) = Not(sel) */
	FS_OPT_MUX_OR_BOOL,                       /**< Muxb(sel, true, x) = Or(sel, x) */
	FS_OPT_MUX_ORNOT_BOOL,                    /**< Muxb(sel, x, true) = Or(Not(sel), x) */
	FS_OPT_MUX_AND_BOOL,                      /**< Muxb(sel, x, false) = And(sel, x) */
	FS_OPT_MUX_ANDNOT_BOOL,                   /**< Muxb(sel, false, x) = And(Not(sel), x) */
	FS_OPT_MUX_C,                             /**< Mux(C, f, t) = C ? t : f */
	FS_OPT_MUX_EQ,                            /**< Mux(v, x, x) = x */
	FS_OPT_MUX_TRANSFORM,                     /**< Mux(t ==/!= f, t, f) = f/t, Mux(t ==/!= 0, -t, t) = -t/t */
	FS_OPT_MUX_TO_MIN,                        /**< Mux(a < b, a, b) = Min(a,b) */
	FS_OPT_MUX_TO_MAX,                        /**< Mux(a > b, a, b) = Max(a,b) */
	FS_OPT_MUX_TO_BITOP,                      /**< Mux((a & 2^x) ==/!= 0, 2^x, 0) = (a & 2^x) (xor 2^x) */
	FS_OPT_INVOLUTION,                        /**< OP(OP(x)) = x */
	FS_OPT_MINUS_NOT,                         /**< -(~x) = x + 1 */
	FS_OPT_NOT_MINUS_1,                       /**< ~(x - 1) = -x */
	FS_OPT_NOT_PLUS_C,                        /**< ~x + C = (C - 1) - x */
	FS_OPT_ADD_X_NOT_X,                       /**< ~x + x = -1 */
	FS_OPT_FP_INV_MUL,                        /**< x / y = x * (1.0/y) */
	FS_OPT_CONST_PHI,                         /**< Constant evaluation on Phi */
	FS_OPT_PREDICATE,                         /**< Predicate optimization */
	FS_OPT_DEMORGAN,                          /**< optimization using DeMorgan's law */
	FS_OPT_CMP_OP_OP,                         /**< CMP optimization: Cmp(OP(x), OP(y)) = Cmp(x, y) */
	FS_OPT_CMP_OP_C,                          /**< CMP optimization: Cmp(OP(x), c1) = Cmp(x, c2) */
	FS_OPT_CMP_CONV_CONV,                     /**< CMP optimization: Cmp(Conv(x), Conv(y)) = Cmp(x, y) */
	FS_OPT_CMP_CONV,                          /**< CMP optimization: Cmp(Conv(x), Conv(y)) = Cmp(Conv(x), y) */
	FS_OPT_CMP_TO_BOOL,                       /**< CMP optimization: Cmp(x, y) = BoolOP(x, y) */
	FS_OPT_CMP_CNST_MAGN,                     /**< CMP optimization: reduced magnitude of a const */
	FS_OPT_CMP_SHF_TO_AND,                    /**< CMP optimization: transformed shift into And */
	FS_OPT_CMP_MOD_TO_AND,                    /**< CMP optimization: transformed Mod into And */
	FS_OPT_NOP,                               /**< the operation is a NOP */
	FS_OPT_GVN_FOLLOWER,                      /**< GVN-PRE: replaced a follower */
	FS_OPT_GVN_FULLY,                         /**< GVN-PRE: replaced by fully redundant value */
	FS_OPT_GVN_PARTLY,                        /**< GVN-PRE: replaced by partly redundant value */
	FS_OPT_COMBO_CONST,                       /**< Combo: evaluated into Constant */
	FS_OPT_COMBO_CF,                          /**< Combo: removed conditional control flow */
	FS_OPT_COMBO_FOLLOWER,                    /**< Combo: replaced a follower */
	FS_OPT_COMBO_CONGRUENT,                   /**< Combo: replaced by congruent */
	FS_OPT_JUMPTHREADING,                     /**< Jump threading: removed conditional control flow */
	FS_OPT_RTS_ABS,                           /**< RTS optimization: call to abs() replaced */
	FS_OPT_RTS_ALLOCA,                        /**< RTS optimization: call to alloca() replaced */
	FS_OPT_RTS_SQRT,                          /**< RTS optimization: call to sqrt() replaced */
	FS_OPT_RTS_CBRT,                          /**< RTS optimization: call to cbrt() replaced */
	FS_OPT_RTS_POW,                           /**< RTS optimization: call to pow() replaced */
	FS_OPT_RTS_EXP,                           /**< RTS optimization: call to exp() replaced */
	FS_OPT_RTS_LOG,                           /**< RTS optimization: call to log() replaced */
	FS_OPT_RTS_SIN,                           /**< RTS optimization: call to sin() replaced */
	FS_OPT_RTS_COS,                           /**< RTS optimization: call to cos() replaced */
	FS_OPT_RTS_TAN,                           /**< RTS optimization: call to tan() replaced */
	FS_OPT_RTS_ASIN,                          /**< RTS optimization: call to asin() replaced */
	FS_OPT_RTS_ACOS,                          /**< RTS optimization: call to acos() replaced */
	FS_OPT_RTS_ATAN,                          /**< RTS optimization: call to atan() replaced */
	FS_OPT_RTS_SINH,                          /**< RTS optimization: call to sinh() replaced */
	FS_OPT_RTS_COSH,                          /**< RTS optimization: call to cosh() replaced */
	FS_OPT_RTS_TANH,                          /**< RTS optimization: call to tanh() replaced */
	FS_OPT_RTS_SYMMETRIC,                     /**< RTS optimization: call to symmetric function f(-x) replaced by f(x) */
	FS_OPT_RTS_STRCMP,                        /**< RTS optimization: call to strcmp() replaced */
	FS_OPT_RTS_STRNCMP,                       /**< RTS optimization: call to strncmp() replaced */
	FS_OPT_RTS_STRCPY,                        /**< RTS optimization: call to strcpy() replaced */
	FS_OPT_RTS_STRLEN,                        /**< RTS optimization: call to strlen() replaced */
	FS_OPT_RTS_MEMCPY,                        /**< RTS optimization: call to memcpy() replaced */
	FS_OPT_RTS_MEMPCPY,                       /**< RTS optimization: call to mempcpy() replaced */
	FS_OPT_RTS_MEMMOVE,                       /**< RTS optimization: call to memmove() replaced */
	FS_OPT_RTS_MEMSET,                        /**< RTS optimization: call to memset() replaced */
	FS_OPT_RTS_MEMCMP,                        /**< RTS optimization: call to memcmp() replaced */
	FS_OPT_MAX
};

/**
 * An entry in a distribution table
 */
typedef struct distrib_entry_t {
	counter_t   cnt;      /**< the current count */
	const void *object;   /**< the object which is counted */
} distrib_entry_t;

/** The type of the hash function for objects in distribution tables. */
typedef unsigned (*distrib_hash_fun)(const void *object);

/**
 * The distribution table.
 */
typedef struct distrib_tbl_t {
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
typedef struct address_mark_entry_t {
  ir_node  *node;               /**< the node which this entry belongs to, needed for compare */
  unsigned mark;                /**< the mark, a bitmask of enum adr_marker_t */
} address_mark_entry_t;

typedef char const *op_id_t;

/**
 * An entry for ir_nodes, used in ir_graph statistics.
 */
typedef struct node_entry_t {
	counter_t cnt_alive;  /**< amount of nodes in this entry */
	counter_t new_node;   /**< amount of new nodes for this entry */
	counter_t into_Id;    /**< amount of nodes that turned into Id's for this entry */
	counter_t normalized; /**< amount of nodes that normalized for this entry */
	op_id_t   op_id;      /**< the op for this entry */
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
	gcnt_external_calls,           /**< number of external calls */
	gcnt_pure_adr_ops,             /**< number of pure address operation */
	gcnt_all_adr_ops,              /**< number of all address operation */
	gcnt_global_adr,               /**< number of global load/store addresses. */
	gcnt_local_adr,                /**< number of local load/store addresses. */
	gcnt_param_adr,                /**< number of parameter load/store addresses. */
	gcnt_this_adr,                 /**< number of this load/store addresses. */
	gcnt_other_adr,                /**< number of other load/store addresses. */

	/* --- must be the last enum constant --- */
	_gcnt_last = gcnt_other_adr    /**< number of counters */
};

/**
 * An entry for ir_graphs. These numbers are calculated for every IR graph.
 */
typedef struct graph_entry_t {
	struct obstack             recalc_cnts;                  /**< obstack containing the counters that are recalculated */
	HASH_MAP(node_entry_t)     *opcode_hash;                 /**< hash map containing the opcode counter */
	HASH_MAP(block_entry_t)    *block_hash;                  /**< hash map containing the block counter */
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
	unsigned                   is_strict:1;                  /**< set, if this irg represents a strict program */
	unsigned                   is_analyzed:1;                /**< helper: set, if this irg was already analysed */
} graph_entry_t;

/**
 * An entry for optimized ir_nodes
 */
typedef struct opt_entry_t {
	counter_t count; /**< optimization counter */
	op_id_t   op_id; /**< the op for this entry */
} opt_entry_t;

/**
 * An entry for register pressure.
 */
typedef struct reg_pressure_entry_t {
	const char *class_name; /**< name of the register class */
	int         pressure;   /**< the register pressure for this class */
} reg_pressure_entry_t;

/**
 * An entry for a block or extended block in a ir-graph
 */
typedef struct be_block_entry_t {
	long                           block_nr;         /**< block nr */
	distrib_tbl_t                  *sched_ready;     /**< distribution of ready nodes per block */
	/**< the highest register pressures for this block for each register class */
	HASH_MAP(reg_pressure_entry_t) *reg_pressure;
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
typedef struct block_entry_t {
	counter_t       cnt[_bcnt_last];  /**< counter */
	long            block_nr;         /**< block nr */
	unsigned        is_start:1;       /**< set, if it's the Start block. */
	unsigned        is_end:1;         /**< set, if it's the End block. */
} block_entry_t;

/**
 * Some potential interesting float values
 */
typedef enum float_classify_t {
	STAT_FC_0,                /**< the float value 0.0 */
	STAT_FC_1,                /**< the float value 1.0 */
	STAT_FC_2,                /**< the float value 2.0 */
	STAT_FC_0_5,              /**< the float value 0.5 */
	STAT_FC_POWER_OF_TWO,     /**< another 2^x value */
	STAT_FC_OTHER,            /**< all other values */
	STAT_FC_MAX               /**< last value */
} float_classify_t;

/**
 * constant info
 */
typedef struct constant_info_t {
	counter_t  int_bits_count[32];  /**< distribution of bit sizes of integer constants */
	counter_t  floats[STAT_FC_MAX]; /**< floating point constants */
	counter_t  others;              /**< all other constants */
} constant_info_t;

/** forward */
typedef struct dumper_t dumper_t;

/**
 * handler for dumping an IRG
 *
 * @param dmp   the dumper
 * @param entry the IR-graph hash map entry
 */
typedef void dump_graph_FUNC(dumper_t *dmp, graph_entry_t *entry);

/**
 * handler for dumper a constant info table
 *
 * @param dmp   the dumper
 */
typedef void dump_const_table_FUNC(dumper_t *dmp, const constant_info_t *tbl);

/**
 * dumps the parameter distribution table
 */
typedef void dump_param_tbl_FUNC(dumper_t *dmp, const distrib_tbl_t *tbl, graph_entry_t *global);

/**
 * dumps the optimizations counter
 */
typedef void dump_opt_cnt_FUNC(dumper_t *dumper, const counter_t *tbl, unsigned len);

/**
 * handler for dumper init
 *
 * @param dmp   the dumper
 * @param name  name of the file to dump to
 */
typedef void dump_init_FUNC(dumper_t *dmp, const char *name);

/**
 * handler for dumper finish
 *
 * @param dmp   the dumper
 */
typedef void dump_finish_FUNC(dumper_t *dmp);

/**
 * statistics info
 */
typedef struct statistic_info_t {
	unsigned                stat_options;        /**< statistic options: field must be first */
	struct obstack          cnts;                /**< obstack containing the counters that are incremented */
	struct obstack          be_data;             /**< obstack containing backend statistics data */
	HASH_MAP(graph_entry_t) *irg_hash;           /**< hash map containing the counter for irgs */
	HASH_MAP(ir_op)         *ir_op_hash;         /**< hash map containing all ir_ops (accessible by op_codes) */
	pdeq                    *wait_q;             /**< wait queue for leaf call decision */
	unsigned                recursive:1;         /**< flag for detecting recursive hook calls */
	unsigned                in_dead_node_elim:1; /**< flag for dead node elimination runs */
	op_id_t                 op_Phi0;             /**< pseudo op for Phi0 */
	op_id_t                 op_PhiM;             /**< pseudo op for memory Phi */
	op_id_t                 op_ProjM;            /**< pseudo op for memory Proj */
	op_id_t                 op_MulC;             /**< pseudo op for multiplication by const */
	op_id_t                 op_DivC;             /**< pseudo op for division by const */
	op_id_t                 op_ModC;             /**< pseudo op for modulo by const */
	op_id_t                 op_SelSel;           /**< pseudo op for Sel(Sel) */
	op_id_t                 op_SelSelSel;        /**< pseudo op for Sel(Sel(Sel)) */
	dumper_t                *dumper;             /**< list of dumper */
	int                     reassoc_run;         /**< if set, reassociation is running */
	constant_info_t         const_info;          /**< statistic info for constants */
	distrib_tbl_t           *dist_param_cnt;     /**< distribution table for call parameters */

	counter_t               num_opts[FS_OPT_MAX];/**< count optimizations */
} stat_info_t;

/**
 * a dumper description
 */
struct dumper_t {
	dump_graph_FUNC       *dump_graph;     /**< handler for dumping an irg */
	dump_const_table_FUNC *dump_const_tbl; /**< handler for dumping a const table */
	dump_param_tbl_FUNC   *dump_param_tbl; /**< handler for dumping the Call parameter table */
	dump_opt_cnt_FUNC     *dump_opt_cnt;   /**< handler for dumping the optimization table. */
	dump_init_FUNC        *init;           /**< handler for init */
	dump_finish_FUNC      *finish;         /**< handler for finish */
	FILE                  *f;             /**< the file to dump to */
	stat_info_t           *status;        /**< access to the global status */
	dumper_t              *next;          /**< link to the next dumper */
	unsigned               tag;            /**< the id tag of the dumper */
};

/**
 * internal init function, mainly registers commandline arguments.
 * (The use still has to call firm_init_stat() later
 */
void init_stat(void);

/**
 * helper: get an ir_op from an opcode
 */
ir_op *stat_get_op_from_opcode(unsigned code);

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

#endif
