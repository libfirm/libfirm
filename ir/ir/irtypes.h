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
	bool      pinned : 1;
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
	dbg_info *dbi;           /**< A pointer to information for debug support. */
	/* ------- For analyses -------- */
	ir_loop *loop;           /**< the loop the node is in. Access routines in irloop.h */
	void            *backend_info;
	irn_edges_info_t edge_info;  /**< Everlasting out edges. */

	/* ------- Opcode depending fields -------- */
	ir_attr attr;            /**< The set of attributes of this node. Depends on opcode.
	                              Must be last field of struct ir_node. */
};

#endif
