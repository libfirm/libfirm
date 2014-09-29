/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Definition of the Firm IR base types, concentrated here
 * @author  Michael Beck
 */
#ifndef FIRM_IR_IRDEFS_H
#define FIRM_IR_IRDEFS_H

#include <stdbool.h>

#include "firm_types.h"
#include "irdom_t.h"
#include "irmode.h"
#include "irnode.h"
#include "iredgekinds.h"
#include "irop.h"
#include "irmemory.h"
#include "callgraph.h"
#include "irprog.h"
#include "bitset.h"

#include "pset.h"
#include "pmap.h"
#include "list.h"
#include "obst.h"
#include "vrp.h"

struct ir_nodemap {
	void **data;  /**< maps node indices to void* */
};

typedef struct arch_irn_ops_t arch_irn_ops_t;

/**
 * Operation specific callbacks.
 */
typedef struct {
	hash_func             hash;                 /**< Calculate a hash value for an IR node. */
	computed_value_func   computed_value;       /**< Evaluates a node into a tarval if possible. */
	computed_value_func   computed_value_Proj;  /**< Evaluates a Proj node into a tarval if possible. */
	equivalent_node_func  equivalent_node;      /**< Optimizes the node by returning an equivalent one. */
	equivalent_node_func  equivalent_node_Proj; /**< Optimizes the Proj node by returning an equivalent one. */
	transform_node_func   transform_node;       /**< Optimizes the node by transforming it. */
	transform_node_func   transform_node_Proj;  /**< Optimizes the Proj node by transforming it. */
	node_attrs_equal_func  attrs_equal;         /**< Compares two node attributes. */
	reassociate_func      reassociate;          /**< Reassociate a tree. */
	copy_attr_func        copy_attr;            /**< Copy node attributes. */
	get_type_attr_func    get_type_attr;        /**< Returns the type attribute of a node. */
	get_entity_attr_func  get_entity_attr;      /**< Returns the entity attribute of a node. */
	verify_node_func      verify_node;          /**< Verify the node. */
	verify_proj_node_func verify_proj_node;     /**< Verify the Proj node. */
	dump_node_func        dump_node;            /**< Dump a node. */
	op_func               generic;              /**< A generic function pointer. */
	op_func               generic1;             /**< A generic function pointer. */
	op_func               generic2;             /**< A generic function pointer. */
	const arch_irn_ops_t *be_ops;               /**< callbacks used by the backend. */
} ir_op_ops;

/** The type of an ir_op. */
struct ir_op {
	unsigned code;            /**< The unique opcode of the op. */
	ident *name;              /**< The name of the op. */
	size_t attr_size;         /**< Space needed in memory for private attributes
	                               */
	op_pin_state pin_state;   /**< How to deal with the node in CSE, PRE. */
	op_arity opar;            /**< The arity of operator. */
	int op_index;             /**< The index of the first data operand, 0 for
	                               most cases, 1 for Div etc. */
	int memory_index;         /**< index of memory input for memory nodes */
	unsigned pn_x_regular;    /**< for fragile ops the position of the
	                               X_regular output */
	unsigned pn_x_except;     /**< for fragile ops the position of the
	                               X_except output */
	unsigned flags;           /**< Flags describing the behavior of the ir_op,
	                               a bitmasks of irop_flags. */
	unsigned tag;             /**< Some custom TAG value the op's creator set */
	void *attr;               /**< custom pointer where op's creator can attach
	                               attribute stuff to. */
	ir_op_ops ops;            /**< The operations of the this op. */
};

/** Helper values for ir_mode_sort. */
enum ir_mode_sort_helper {
	irmsh_is_num   = 0x10, /**< mode represents a number */
	irmsh_is_data  = 0x20, /**< mode represents data (can be carried in registers) */
};

/**
 * These values represent the different mode classes of value representations.
 */
typedef enum ir_mode_sort {
	irms_auxiliary        = 0,
	irms_internal_boolean = 1 | irmsh_is_data,
	irms_data             = 2 | irmsh_is_data,

	/** A mode to represent entities.
	    Restricted int computations can be performed */
	irms_reference        = 3 | irmsh_is_data,
	/** A mode to represent int numbers.
	    Integer computations can be performed. */
	irms_int_number       = 4 | irmsh_is_data | irmsh_is_num,
	/** A mode to represent float numbers.
	    Floating point computations can be performed. */
	irms_float_number     = 5 | irmsh_is_data | irmsh_is_num,
} ir_mode_sort;

/**
 * A descriptor for an IEEE754 float value.
 */
typedef struct float_descriptor_t {
	unsigned char exponent_size;    /**< size of exponent in bits */
	unsigned char mantissa_size;    /**< size of mantissa in bits */
	bool          explicit_one;     /**< set if the leading one is explicit */
} float_descriptor_t;

/**
 * Contains relevant information about a mode.
 *
 * Necessary information about a mode is stored in this struct
 * which is used by the tarval module to perform calculations
 * and comparisons of values of a such described mode.
 *
 * ATTRIBUTES:
 *  -  ident *name:             Name of this mode. Two modes are different if the name is different.
 *  -  ir_mode_sort sort:       sort of mode specifying possible usage categories
 *  -  int    size:             size of the mode in Bits.
 *  -  unsigned sign:1:         signedness of this mode
 *  -  ... more to come
 *  -  modulo_shift             specifies for modes of kind irms_int_number
 *                              whether shift applies modulo to value of bits to shift
 *
 * SEE ALSO:
 *    The tech report 1999-44 describing FIRM and predefined modes
 *    tarval.h
 */
struct ir_mode {
	firm_kind         kind;       /**< distinguishes this node from others */
	ident             *name;      /**< Name ident of this mode */
	ir_type           *type;      /**< corresponding primitive type */

	/* ---------------------------------------------------------------------- */
	/* On changing this struct you have to evaluate the mode_are_equal function!*/
	ir_mode_sort       sort;          /**< coarse classification of this mode:
                                           int, float, reference ...
                                           (see irmode.h) */
	ir_mode_arithmetic arithmetic;    /**< different arithmetic operations possible with a mode */
	unsigned           size;          /**< size of the mode in Bits. */
	bool               sign:1;        /**< signedness of this mode */
	ENUMBF(float_int_conversion_overflow_style_t)
	                   int_conv_overflow:1;
	unsigned           modulo_shift;  /**< number of bits a values of this mode will be shifted */
	float_descriptor_t float_desc;

	/* ---------------------------------------------------------------------- */
	ir_tarval         *min;         /**< the minimum value that can be expressed */
	ir_tarval         *max;         /**< the maximum value that can be expressed */
	ir_tarval         *null;        /**< the value 0 */
	ir_tarval         *one;         /**< the value 1 */
	ir_tarval         *all_one;     /**< the value ~0 */
	ir_tarval         *infinity;    /**< the infinity value */
	ir_tarval         *nan;         /** the not a number (NaN) value */
	ir_mode           *eq_unsigned; /**< For pointer modes, the equivalent unsigned integer one. */
};

/* note: we use "long" here because that is the type used for Proj-Numbers */
typedef struct ir_switch_table_entry {
	ir_tarval *min;
	ir_tarval *max;
	unsigned   pn;
} ir_switch_table_entry;

struct ir_switch_table {
	size_t                n_entries;
	ir_switch_table_entry entries[];
};

/* ir node attributes */

/** Attributes for Block nodes. */
typedef struct block_attr {
	/* General attributes */
	ir_visited_t block_visited; /**< For the walker that walks over all blocks. */
	/* Attributes private to construction: */
	unsigned is_matured:1;      /**< If set, all in-nodes of the block are fixed. */
	unsigned dynamic_ins:1;     /**< if set in-array is an ARR_F on the heap. */
	unsigned marked:1;          /**< Can be set/unset to temporary mark a block. */
	ir_node **graph_arr;        /**< An array to store all parameters. */
	/* Attributes holding analyses information */
	ir_dom_info dom;            /**< Datastructure that holds information about dominators. */
	ir_dom_info pdom;           /**< Datastructure that holds information about post-dominators. */
	bitset_t *backedge;         /**< Bitfield n set to true if pred n is backedge.*/
	ir_entity *entity;          /**< entity representing this block */
	ir_node  *phis;             /**< The list of Phi nodes in this block. */
	double    execfreq;         /**< block execution frequency */
} block_attr;

/** Attributes for Cond nodes. */
typedef struct cond_attr {
	cond_jmp_predicate jmp_pred; /**< only for binary Conds: The jump predication. */
} cond_attr;

/** Attributes for Const nodes. */
typedef struct const_attr {
	ir_tarval *tarval;  /**< the target value */
} const_attr;

/** Attributes for Address and Offset nodes. */
typedef struct entconst_attr {
	ir_entity *entity;
} entconst_attr;

/** Attributes for TypeConst nodes. */
typedef struct typeconst_attr {
	ir_type *type;
} typeconst_attr;

/** Attributes for Member nodes. */
typedef struct member_attr {
	ir_entity *entity; /**< entity to select */
} member_attr;

/** Attributes for Sel nodes. */
typedef struct sel_attr {
	ir_type *type;
} sel_attr;

/** Attributes for nodes with exceptions (fragile flag). */
typedef struct except_attr {
	unsigned  pin_state : 2;         /**< the pin state for operations with
	                                      variable pinned state. Contains a
	                                      op_pin_state */
	unsigned  throws_exception : 1; /**< if true a fragile op throws and
	                                     must produce X_except and X_regular
	                                     values */
} except_attr;

/** Attributes for Call nodes. */
typedef struct call_attr {
	except_attr exc;               /**< the exception attribute. MUST be the first one. */
	ir_type     *type;             /**< type of called procedure */
	ir_entity   **callee_arr;      /**< result of callee analysis */
} call_attr;

/** Attributes for Builtin nodes. */
typedef struct builtin_attr {
	except_attr     exc;           /**< the exception attribute. MUST be the first one. */
	ir_builtin_kind kind;          /**< kind of the called builtin procedure */
	ir_type         *type;         /**< type of called builtin procedure */
} builtin_attr;

/** Attributes for Alloc nodes. */
typedef struct alloc_attr {
	unsigned alignment;
} alloc_attr;

/** Attributes for Load nodes. */
typedef struct load_attr {
	except_attr   exc;            /**< The exception attribute. MUST be the first one. */
	ENUMBF(ir_volatility) volatility:1;   /**< The volatility of this Load operation. */
	ENUMBF(ir_align)      unaligned:1;    /**< The align attribute of this Load operation. */
	ir_mode       *mode;          /**< The mode of this Load operation. */
	ir_type       *type;          /**< The type of the object loaded. */
} load_attr;

/** Attributes for Store nodes. */
typedef struct store_attr {
	except_attr   exc;            /**< the exception attribute. MUST be the first one. */
	ir_type       *type;          /**< The type of the object stored at the node's address. */
	ENUMBF(ir_volatility) volatility:1;   /**< The volatility of this Store operation. */
	ENUMBF(ir_align)      unaligned:1;    /**< The align attribute of this Store operation. */
} store_attr;

/** Attributes for Phi nodes. */
typedef struct phi_attr {
	ir_node *next; /**< Points to the next Phi in the Phi list of a block. */
	union {
		bitset_t *backedge; /**< Raw Bitset: pred n is backedge iff n is set. */
		/** For Phi0. Used to remember the value defined by this Phi node.
		 * Needed when the Phi is completed to call get_r_internal_value() to
		 * find the predecessors. If this attribute is set, the Phi node takes
		 * the role of the obsolete Phi0 node, therefore the name. */
		int       pos;
	} u;
	unsigned loop:1; /**< Set to true if this is a loop PhiM node. */
} phi_attr;

/** Attributes for Cmp nodes. */
typedef struct cmp_attr {
	ir_relation relation; /**< comparison condition. */
} cmp_attr;

/** Attributes for Confirm nodes. */
typedef struct confirm_attr {
	ir_relation relation; /**< relation between value and bound */
} confirm_attr;

/** Attributes for CopyB nodes. */
typedef struct copyb_attr {
	ir_type *type;                      /**< Type of the copied entity. */
	ENUMBF(ir_volatility) volatility:1; /**< Volatility of this CopyB. */
} copyb_attr;

/** Attributes for Div nodes. */
typedef struct div_attr {
	except_attr exc;          /**< The exception attribute. MUST be first. */
	ir_mode    *resmode;      /**< Result mode for the division. */
	char        no_remainder; /**< Set, if known that a division can be done
	                               without a remainder. */
} div_attr;

/** Attributes for Mod nodes. */
typedef struct mod_attr {
	except_attr exc;     /**< The exception attribute. MUST be first. */
	ir_mode    *resmode; /**< Result mode for the division. */
} mod_attr;

/** Attributes for ASM nodes. */
typedef struct asm_attr {
	except_attr        exc; /**< The exception attribute. MUST be first. */
	ident             *text;               /**< The inline assembler text. */
	ir_asm_constraint *input_constraints;  /**< Input constraints. */
	ir_asm_constraint *output_constraints; /**< Output constraints. */
	ident            **clobbers;           /**< List of clobbered registers. */
} asm_attr;

/** Attributes for Proj nodes. */
typedef struct proj_attr {
	unsigned num; /**< number of tuple sub-value which is projected */
} proj_attr;

/** Attributes for Switch nodes. */
typedef struct switch_attr {
	unsigned         n_outs;
	ir_switch_table *table;
} switch_attr;

/** Union with all possible node attributes. */
typedef union ir_attr {
	block_attr     block;
	cmp_attr       cmp;
	cond_attr      cond;
	const_attr     con;
	entconst_attr  entc;
	typeconst_attr typec;
	sel_attr       sel;
	member_attr    member;
	call_attr      call;
	builtin_attr   builtin;
	alloc_attr     alloc;
	load_attr      load;
	store_attr     store;
	phi_attr       phi;
	proj_attr      proj;
	confirm_attr   confirm;
	except_attr    except;
	copyb_attr     copyb;
	div_attr       div;
	mod_attr       mod;
	asm_attr       assem;
	switch_attr    switcha;
} ir_attr;

/**
 * Edge info to put into an irn.
 */
typedef struct irn_edge_kind_info_t {
	struct list_head outs_head;  /**< The list of all outs. */
	unsigned edges_built : 1;    /**< Set edges where built for this node. */
	unsigned out_count : 31;     /**< Number of outs in the list. */
} irn_edge_info_t;

typedef irn_edge_info_t irn_edges_info_t[EDGE_KIND_LAST+1];

/**
 * A Def-Use edge.
 */
typedef struct ir_def_use_edge {
	ir_node *use;            /** The use node of that edge. */
	int     pos;             /** The position of this edge in use's input array. */
} ir_def_use_edge;

typedef struct ir_def_use_edges {
	unsigned        n_edges;
	ir_def_use_edge edges[];
} ir_def_use_edges;

/**
 * The common structure of an irnode.
 * If the node has some attributes, they are stored in the attr field.
 */
struct ir_node {
	/* ------- Basics of the representation  ------- */
	firm_kind kind;          /**< Distinguishes this node from others. */
	unsigned node_idx;       /**< The node index of this node in its graph. */
	ir_op *op;               /**< The Opcode of this node. */
	ir_mode *mode;           /**< The Mode of this node. */
	struct ir_node **in;     /**< The array of predecessors / operands. */
	ir_graph *irg;
	ir_visited_t visited;    /**< The visited counter for walks of the graph. */
	void *link;              /**< To attach additional information to the node, e.g.
	                              used during optimization to link to nodes that
	                              shall replace a node. */
	long node_nr;            /**< A globally unique node number for each node. */
	/* ------- Fields for optimizations / analysis information ------- */
	union {
		ir_def_use_edges *out;    /**< array of def-use edges. */
		unsigned          n_outs; /**< number of def-use edges (temporarily used
		                               during construction of datastructure ) */
	} o;
	struct dbg_info  *dbi;   /**< A pointer to information for debug support. */
	/* ------- For analyses -------- */
	ir_loop *loop;           /**< the loop the node is in. Access routines in irloop.h */
	struct ir_node **deps;   /**< Additional dependencies induced by state. */
	void            *backend_info;
	irn_edges_info_t edge_info;  /**< Everlasting out edges. */

	/* ------- Opcode depending fields -------- */
	ir_attr attr;            /**< The set of attributes of this node. Depends on opcode.
	                              Must be last field of struct ir_node. */
};

#include "iredgeset.h"

/**
 * Edge info to put into an irg.
 */
typedef struct irg_edge_info_t {
	ir_edgeset_t     edges;          /**< A set containing all edges of the current graph. */
	struct list_head free_edges;     /**< list of all free edges. */
	struct obstack   edges_obst;     /**< Obstack, where edges are allocated on. */
	unsigned         allocated : 1;  /**< Set if edges are allocated on the obstack. */
	unsigned         activated : 1;  /**< Set if edges are activated for the graph. */
} irg_edge_info_t;

typedef irg_edge_info_t irg_edges_info_t[EDGE_KIND_LAST+1];

/**
 * Index constants for nodes that can be accessed through the graph anchor node.
 */
typedef enum irg_anchors {
	anchor_first,
	anchor_end_block = anchor_first, /**< block the end node will belong to,
	                                      same as Anchors block */
	anchor_start_block,      /**< block the start node will belong to */
	anchor_end,              /**< end node of this ir_graph */
	anchor_start,            /**< start node of this ir_graph */
	anchor_frame,            /**< methods frame */
	anchor_initial_mem,      /**< initial memory of this graph */
	anchor_args,             /**< methods arguments */
	anchor_no_mem,           /**< NoMem node of this ir_graph, the one and only in this graph */
	anchor_last = anchor_no_mem
} irg_anchors;
ENUM_COUNTABLE(irg_anchors)

/** A callgraph entry for callees. */
typedef struct cg_callee_entry {
	ir_graph  *irg;        /**< The called irg. */
	ir_node  **call_list;  /**< The list of all calls to the irg. */
	size_t     max_depth;  /**< Maximum depth of all Call nodes to irg. */
} cg_callee_entry;

typedef struct ir_bitinfo {
	struct ir_nodemap map;
	struct obstack    obst;
} ir_bitinfo;

typedef struct ir_vrp_info {
	struct ir_nodemap infos;
	struct obstack    obst;
} ir_vrp_info;

/**
 * An ir_graph holds all information for a procedure.
 */
struct ir_graph {
	firm_kind   kind;          /**< Always set to k_ir_graph. */
	unsigned    last_node_idx; /**< The last IR node index for this graph. */
	/** The entity of this procedure, i.e., the type of the procedure and the
	 * class it belongs to. */
	ir_entity  *ent;
	/** A type representing the stack frame. Can include "inner" functions. */
	ir_type    *frame_type;
	ir_node    *anchor;        /**< Pointer to the anchor node. */
	struct obstack obst;       /**< obstack allocator for nodes. */

	/* -- Fields indicating different states of irgraph -- */
	ir_graph_properties_t  properties;
	ir_graph_constraints_t constraints;
	op_pin_state           irg_pinned_state;  /**< Flag for status of nodes. */
	irg_callee_info_state  callee_info_state; /**< Validity of callee information. */

	/* -- Helpers for walking/analysis of the graph -- */
	/** this flag is an identifier for ir walk. it will be incremented every
	 * time someone walks through the graph */
	ir_visited_t     visited;
	ir_visited_t     block_visited; /**< Visited flag for block nodes. */
	ir_visited_t     self_visited;  /**< Visited flag of the irg */
	ir_node        **idx_irn_map;   /**< Map of node indexes to nodes. */
	size_t           index;         /**< a unique number for each graph */
	/** A void* field to link any information to the graph. */
	void            *link;
	void            *be_data; /**< backend can put in private data here */
	unsigned short   dump_nr;       /**< number of graph dumps */

	/* -- Semantic Settings -- */
	unsigned char    mem_disambig_opt;

	/* -- Fields for construction -- */
	/** Number of local variables in this function including parameters. */
	int      n_loc;
	void   **loc_descriptions; /**< Descriptions for variables. */
	ir_node *current_block;    /**< Block for new_*()ly created nodes. */

	/* -- Fields for optimizations / analysis information -- */
	/** Hash table for global value numbering (cse) */
	pset               *value_table;
	struct obstack      out_obst;    /**< Space for the Def-Use arrays. */
	bool                out_obst_allocated;
	ir_bitinfo          bitinfo;     /**< bit info */
	ir_vrp_info         vrp;         /**< vrp info */
	ir_loop            *loop;        /**< The outermost loop for this graph. */
	ir_dom_front_info_t domfront;    /**< dominance frontier analysis data */
	irg_edges_info_t    edge_info;   /**< edge info for automatic outs */
	ir_graph          **callers;     /**< Callgraph: list of callers. */
	unsigned           *caller_isbe; /**< Callgraph: bitset if backedge info calculated. */
	cg_callee_entry   **callees;     /**< Callgraph: list of callee calls */
	unsigned           *callee_isbe; /**< Callgraph: bitset if backedge info calculated. */
	ir_loop            *l;           /**< For callgraph analysis. */

#ifdef DEBUG_libfirm
	/** Unique graph number for each graph to make output readable. */
	long             graph_nr;
#endif
#ifndef NDEBUG
	/** Debug helper: Phases/Analysis can indicate here which exclusive
	 * resources (e.g. link fields of the graph nodes) they are using. */
	ir_resources_t   reserved_resources;
#endif
};

/**
 * Data structure that holds central information about a program
 * or a module.
 * One irp is created by libFirm on construction, so irp should never be NULL.
 *
 * - main_irg:  The ir graph that is the entry point to the program.
 *              (Anything not reachable from here may be optimized away
 *              if this irp represents a whole program.
 * - graphs:    List of all ir graphs in the program or module.
 * - types:     A list containing all types known to the translated program.
 *              Some types can have several entries in this list (as a result of
 *              using exchange_types()).
 */
struct ir_prog {
	ident     *name;                /**< A file name or the like. */
	ir_graph  *main_irg;            /**< The entry point to the compiled program
	                                     or NULL if no point exists. */
	ir_graph **graphs;              /**< A list of all graphs in the ir. */
	ir_graph  *const_code_irg;      /**< This ir graph gives the proper environment
	                                     to allocate nodes the represent values
	                                     of constant entities. It is not meant as
	                                     a procedure.  */
	ir_entity *unknown_entity;      /**< unique 'unknown'-entity */
	ir_type   *segment_types[IR_SEGMENT_LAST+1];
	ir_type  **types;               /**< A list of all types in the ir. */
	ir_type   *code_type;           /**< unique 'code'-type */
	ir_type   *unknown_type;        /**< unique 'unknown'-type */
	ir_type   *dummy_owner;         /**< owner for internal entities */
	ir_type   *byte_type;           /**< type for a 'byte' */
	ident    **global_asms;         /**< An array of global ASM insertions. */

	/* -- states of and access to generated information -- */
	irg_callee_info_state callee_info_state; /**< Validity of callee information.
	                                              Contains the lowest value or all irgs.  */
	inh_transitive_closure_state inh_trans_closure_state;  /**< State of transitive closure
	                                                            of inheritance relations. */

	irp_callgraph_state callgraph_state; /**< The state of the callgraph. */
	ir_loop *outermost_cg_loop;          /**< For callgraph analysis: entry point
	                                              to looptree over callgraph. */
	loop_nesting_depth_state lnd_state;  /**< The state of loop nesting depth information. */
	ir_entity_usage_computed_state globals_entity_usage_state;

	ir_label_t last_label_nr;            /**< The highest label number for generating unique labels. */
	size_t max_irg_idx;                  /**< highest unused irg index */
	long max_node_nr;                    /**< to generate unique numbers for nodes. */
	unsigned dump_nr;                    /**< number of program info dumps */
	pmap *compilerlib_entities;          /**< maps ident* to ir_entity* of the compilerlib */
#ifndef NDEBUG
	irp_resources_t reserved_resources;  /**< Bitset for tracking used global resources. */
#endif
};

#endif
