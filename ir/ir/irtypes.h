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
#include "irtypeinfo.h"
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
	node_cmp_attr_func    node_cmp_attr;        /**< Compares two node attributes. */
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
	int pn_x_regular;         /**< for fragile ops the position of the
	                               X_regular output */
	int pn_x_except;          /**< for fragile ops the position of the
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
	irmsh_is_datab = 0x40, /**< mode represents data or is internal boolean */
	irmsh_is_dataM = 0x80, /**< mode represents data or is memory */
};

/**
 * These values represent the different mode classes of value representations.
 * Beware: do not change the order of these values without checking
 * the mode_is
 */
typedef enum ir_mode_sort {
	irms_control_flow     = 0, /**< Marks all control flow modes. */
	irms_block            = 1,
	irms_tuple            = 2,
	irms_any              = 3,
	irms_bad              = 4,
	irms_memory           = 5 | irmsh_is_dataM, /**< Marks the memory mode.  Not extensible. (irm_M) */

	/** Internal boolean representation.
	     Storing to memory impossible, convert first. (irm_b) */
	irms_internal_boolean = 6 | irmsh_is_datab,

	/** A mode to represent entities.
	    Restricted int computations can be performed */
	irms_reference        = 7 | irmsh_is_data | irmsh_is_datab | irmsh_is_dataM,
	/** A mode to represent int numbers.
	    Integer computations can be performed. */
	irms_int_number       = 8 | irmsh_is_data | irmsh_is_datab | irmsh_is_dataM | irmsh_is_num,
	/** A mode to represent float numbers.
	    Floating point computations can be performed. */
	irms_float_number     = 9 | irmsh_is_data | irmsh_is_datab | irmsh_is_dataM | irmsh_is_num,
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
	unsigned           sign:1;        /**< signedness of this mode */
	unsigned int       modulo_shift;  /**< number of bits a values of this mode will be shifted */
	float_descriptor_t float_desc;

	/* ---------------------------------------------------------------------- */
	ir_tarval         *min;         /**< the minimum value that can be expressed */
	ir_tarval         *max;         /**< the maximum value that can be expressed */
	ir_tarval         *null;        /**< the value 0 */
	ir_tarval         *one;         /**< the value 1 */
	ir_tarval         *minus_one;   /**< the value -1 */
	ir_tarval         *all_one;     /**< the value ~0 */
	ir_mode           *eq_signed;   /**< For pointer modes, the equivalent signed integer one. */
	ir_mode           *eq_unsigned; /**< For pointer modes, the equivalent unsigned integer one. */
	void              *link;        /**< To store some intermediate information */
};

/* note: we use "long" here because that is the type used for Proj-Numbers */
typedef struct ir_switch_table_entry {
	ir_tarval *min;
	ir_tarval *max;
	long       pn;
} ir_switch_table_entry;

struct ir_switch_table {
	size_t                n_entries;
	ir_switch_table_entry entries[];
};

/* ir node attributes */

/** first attribute of Bad, Block, Anchor nodes */
typedef struct irg_attr {
	ir_graph *irg;              /**< The graph this block like node belongs to. */
} irg_attr;

typedef struct bad_attr {
	irg_attr    irg;
} bad_attr;

typedef struct anchor_attr {
	irg_attr  irg;
} anchor_attr;

/** Block attributes */
typedef struct block_attr {
	/* General attributes */
	irg_attr     irg;           /**< The graph this block belongs to. */
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
	ir_entity *entity;          /**< entitiy representing this block */
	ir_node  *phis;             /**< The list of Phi nodes in this block. */
	double    execfreq;         /**< block execution frequency */
} block_attr;

/** Cond attributes. */
typedef struct cond_attr {
	cond_jmp_predicate jmp_pred; /**< only for binary Conds: The jump predication. */
} cond_attr;

/** Const attributes. */
typedef struct const_attr {
	ir_tarval *tarval;  /**< the target value */
} const_attr;

/** SymConst attributes. */
typedef struct symconst_attr {
	symconst_symbol sym;  // old tori
	symconst_kind   kind;
} symconst_attr;

/** Sel attributes. */
typedef struct sel_attr {
	ir_entity *entity;    /**< entity to select */
} sel_attr;

/** Exception attributes. */
typedef struct except_attr {
	unsigned  pin_state : 2;         /**< the pin state for operations with
	                                      variable pinned state. Contains a
	                                      op_pin_state */
	unsigned  throws_exception : 1; /**< if true a fragile op throws and
	                                     must produce X_except and X_regular
	                                     values */
} except_attr;

/** Call attributes. */
typedef struct call_attr {
	except_attr exc;               /**< the exception attribute. MUST be the first one. */
	ir_type     *type;             /**< type of called procedure */
	ir_entity   **callee_arr;      /**< result of callee analysis */
} call_attr;

/** Builtin attributes. */
typedef struct builtin_attr {
	except_attr     exc;           /**< the exception attribute. MUST be the first one. */
	ir_builtin_kind kind;          /**< kind of the called builtin procedure */
	ir_type         *type;         /**< type of called builtin procedure */
} builtin_attr;

/** Alloc attributes. */
typedef struct alloc_attr {
	unsigned alignment;
} alloc_attr;

/** Load attributes. */
typedef struct load_attr {
	except_attr   exc;            /**< The exception attribute. MUST be the first one. */
	ENUMBF(ir_volatility) volatility:1;   /**< The volatility of this Load operation. */
	ENUMBF(ir_align)      unaligned:1;    /**< The align attribute of this Load operation. */
	ir_mode       *mode;          /**< The mode of this Load operation. */
} load_attr;

/** Store attributes. */
typedef struct store_attr {
	except_attr   exc;            /**< the exception attribute. MUST be the first one. */
	ENUMBF(ir_volatility) volatility:1;   /**< The volatility of this Store operation. */
	ENUMBF(ir_align)      unaligned:1;    /**< The align attribute of this Store operation. */
} store_attr;

typedef struct phi_attr {
	ir_node        *next;         /**< Points to the next Phi in the Phi list of a block. */
	union {
		bitset_t      *backedge;     /**< Raw Bitfield: bit n is set to true if pred n is backedge. */
		int            pos;           /**< For Phi0. Used to remember the value defined by
		                               this Phi node.  Needed when the Phi is completed
		                               to call get_r_internal_value() to find the
		                               predecessors. If this attribute is set, the Phi
		                               node takes the role of the obsolete Phi0 node,
		                               therefore the name. */
	} u;
} phi_attr;

/**< Cmp attribute. */
typedef struct cmp_attr {
	ir_relation relation;         /**< comparison condition. */
} cmp_attr;

/**< Confirm attribute. */
typedef struct confirm_attr {
	ir_relation relation;         /**< relation between value and bound */
} confirm_attr;

/** CopyB attribute. */
typedef struct copyb_attr {
	ir_type      *type;           /**< Type of the copied entity. */
	ENUMBF(ir_volatility) volatility:1;  /**< The volatility of this CopyB operation. */
} copyb_attr;

/** Div attribute. */
typedef struct div_attr {
	except_attr    exc;           /**< The exception attribute. MUST be the first one. */
	ir_mode        *resmode;      /**< Result mode for the division. */
	char           no_remainder;  /**< Set, if known that a division can be done without a remainder. */
} div_attr;

/** Mod attribute. */
typedef struct mod_attr {
	except_attr    exc;           /**< The exception attribute. MUST be the first one. */
	ir_mode        *resmode;      /**< Result mode for the division. */
} mod_attr;

/** Inline Assembler support attribute. */
typedef struct asm_attr {
	/* BEWARE: pin state MUST be the first attribute */
	op_pin_state      pin_state;            /**< the pin state for operations that might generate a exception */
	ident             *text;                /**< The inline assembler text. */
	ir_asm_constraint *input_constraints;   /**< Input constraints. */
	ir_asm_constraint *output_constraints;  /**< Output constraints. */
	ident             **clobbers;           /**< List of clobbered registers. */
} asm_attr;

typedef struct proj_attr {
	long  proj;           /**< position of tuple sub-value which is projected */
} proj_attr;

typedef struct switch_attr {
	unsigned         n_outs;
	ir_switch_table *table;
} switch_attr;

/** Some IR-nodes just have one attribute, these are stored here,
   some have more. Their name is 'irnodename_attr' */
typedef union ir_attr {
	irg_attr       irg;           /**< For Blocks and Bad: its belonging irg */
	bad_attr       bad;           /**< for Bads: irg reference */
	anchor_attr    anchor;        /**< for Anchor: irg reference */
	block_attr     block;         /**< For Block: Fields needed to construct it */
	cmp_attr       cmp;           /**< For Cmp. */
	cond_attr      cond;          /**< For Cond. */
	const_attr     con;           /**< For Const: contains the value of the constant and a type */
	symconst_attr  symc;          /**< For SymConst. */
	sel_attr       sel;           /**< For Sel. */
	call_attr      call;          /**< For Call. */
	builtin_attr   builtin;       /**< For Builtin. */
	alloc_attr     alloc;         /**< For Alloc. */
	load_attr      load;          /**< For Load. */
	store_attr     store;         /**< For Store. */
	phi_attr       phi;           /**< For Phi. */
	proj_attr      proj;          /**< For Proj. */
	confirm_attr   confirm;       /**< For Confirm: compare operation and region. */
	except_attr    except;        /**< For Phi node construction in case of exceptions */
	copyb_attr     copyb;         /**< For CopyB operation */
	div_attr       div;           /**< For Div operation */
	mod_attr       mod;           /**< For Mod operation */
	asm_attr       assem;         /**< For ASM operation. */
	switch_attr    switcha;       /**< For Switch operation. */
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
	anchor_initial_exec,     /**< methods initial control flow */
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

typedef struct ir_vrp_info {
	struct ir_nodemap infos;
	struct obstack    obst;
} ir_vrp_info;

/**
 * An ir_graph holds all information for a procedure.
 */
struct ir_graph {
	firm_kind         kind;        /**< Always set to k_ir_graph. */
	/* --  Basics of the representation -- */
	unsigned last_node_idx;        /**< The last IR node index for this graph. */
	ir_entity  *ent;               /**< The entity of this procedure, i.e.,
	                                    the type of the procedure and the
	                                    class it belongs to. */
	ir_type *frame_type;           /**< A class type representing the stack frame.
	                                    Can include "inner" methods. */
	ir_node *anchor;               /**< Pointer to the anchor node of this graph. */
	struct obstack obst;           /**< The obstack where all of the ir_nodes live. */
	ir_node *current_block;        /**< Current block for new_*()ly created ir_nodes. */

	/* -- Fields indicating different states of irgraph -- */
	ir_graph_properties_t  properties;
	ir_graph_constraints_t constraints;
	op_pin_state           irg_pinned_state;  /**< Flag for status of nodes. */
	ir_typeinfo_state      typeinfo_state;    /**< Validity of type information. */
	irg_callee_info_state  callee_info_state; /**< Validity of callee information. */
	unsigned mem_disambig_opt;               /**< Options for the memory disambiguator. */

	/* -- Fields for construction -- */
	int n_loc;                         /**< Number of local variables in this
	                                        procedure including procedure parameters. */
	void **loc_descriptions;           /**< Storage for local variable descriptions. */

	/* -- Fields for optimizations / analysis information -- */
	pset *value_table;                 /**< Hash table for global value numbering (cse)
	                                        for optimizing use in iropt.c */
	struct obstack   out_obst;         /**< Space for the Def-Use arrays. */
	bool             out_obst_allocated;
	ir_vrp_info      vrp;              /**< vrp info */

	ir_loop *loop;                     /**< The outermost loop for this graph. */
	ir_dom_front_info_t domfront;      /**< dominance frontier analysis data */
	void *link;                        /**< A void* field to link any information to
	                                        the node. */

	ir_graph **callers;                /**< For callgraph analysis: list of caller graphs. */
	unsigned *caller_isbe;             /**< For callgraph analysis: raw bitset if backedge info calculated. */
	cg_callee_entry **callees;         /**< For callgraph analysis: list of callee calls */
	unsigned *callee_isbe;             /**< For callgraph analysis: raw bitset if backedge info calculated. */
	ir_loop   *l;                            /**< For callgraph analysis. */
	size_t     callgraph_loop_depth;         /**< For callgraph analysis */
	size_t     callgraph_recursion_depth;    /**< For callgraph analysis */
	double     method_execution_frequency;   /**< For callgraph analysis */


	/* -- Fields for Walking the graph -- */
	ir_visited_t visited;             /**< this flag is an identifier for
	                  ir walk. it will be incremented
	                  every time someone walks through
	                  the graph */
	ir_visited_t block_visited;        /**< same as visited, for a complete block */

	ir_visited_t self_visited;         /**< visited flag of the irg */

	irg_edges_info_t edge_info;        /**< edge info for automatic outs */
	ir_node **idx_irn_map;             /**< Array mapping node indexes to nodes. */

	size_t index;                      /**< a unique number for each graph */
	/** extra info which should survive accross multiple passes */
	void     *be_data;                 /**< backend can put in private data here */

	unsigned  dump_nr;                 /**< number of graph dumps */
#ifdef DEBUG_libfirm
	long graph_nr;                     /**< a unique graph number for each
	                                        graph to make output readable. */
#endif

#ifndef NDEBUG
	ir_resources_t reserved_resources; /**< Bitset for tracking used local resources. */
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
 * - irg:       List of all ir graphs in the program or module.
 * - type:      A list containing all types known to the translated program.
 *              Some types can have several entries in this list (as a result of
 *              using exchange_types()).
 * - glob_type: The unique global type that is owner of all global entities
 *              of this module.
 */
struct ir_prog {
	firm_kind kind;                 /**< must be k_ir_prog */
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
	ir_type   *byte_type;           /**< type for a 'byte' */
	ident    **global_asms;         /**< An array of global ASM insertions. */

	/* -- states of and access to generated information -- */
	ir_node **ip_outedges;          /**< A huge Array that contains all out edges
	                                     in interprocedural view. */

	irg_callee_info_state callee_info_state; /**< Validity of callee information.
	                                              Contains the lowest value or all irgs.  */
	ir_typeinfo_state typeinfo_state;    /**< Validity of type information. */
	inh_transitive_closure_state inh_trans_closure_state;  /**< State of transitive closure
	                                                            of inheritance relations. */

	irp_callgraph_state callgraph_state; /**< The state of the callgraph. */
	ir_loop *outermost_cg_loop;          /**< For callgraph analysis: entry point
	                                              to looptree over callgraph. */
	size_t max_callgraph_loop_depth;        /**< needed in callgraph. */
	size_t max_callgraph_recursion_depth;   /**< needed in callgraph. */
	double max_method_execution_frequency;  /**< needed in callgraph. */
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
