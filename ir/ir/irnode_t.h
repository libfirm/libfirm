/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Representation of an intermediate operation -- private header.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#ifndef FIRM_IR_IRNODE_T_H
#define FIRM_IR_IRNODE_T_H

#include "array.h"
#include "bitset.h"
#include "irdom_t.h"
#include "iredgekinds.h"
#include "irflag_t.h"
#include "irgraph.h"
#include "irnode.h"
#include "irop_t.h"
#include "list.h"

/* This section MUST come first, so the inline functions get used in this header. */
#define get_irn_arity(node)                   get_irn_arity_(node)
#define get_irn_n(node, n)                    get_irn_n_(node, n)
#define get_irn_mode(node)                    get_irn_mode_(node)
#define set_irn_mode(node, mode)              set_irn_mode_(node, mode)
#define get_irn_irg(node)                     get_irn_irg_(node)
#define get_nodes_block(node)                 get_nodes_block_(node)
#define get_irn_op(node)                      get_irn_op_(node)
#define get_irn_opcode(node)                  get_irn_opcode_(node)
#define get_irn_visited(node)                 get_irn_visited_(node)
#define set_irn_visited(node, v)              set_irn_visited_(node, v)
#define mark_irn_visited(node)                mark_irn_visited_(node)
#define irn_visited(node)                     irn_visited_(node)
#define irn_visited_else_mark(node)           irn_visited_else_mark_(node)
#define set_irn_link(node, link)              set_irn_link_(node, link)
#define get_irn_link(node)                    get_irn_link_(node)
#define get_irn_pinned(node)                  get_irn_pinned_(node)
#define is_binop(node)                        is_binop_(node)
#define get_Block_n_cfgpreds(node)            get_Block_n_cfgpreds_(node)
#define get_Block_cfgpred(node, pos)          get_Block_cfgpred_(node, pos)
#define get_Block_cfgpred_block(node, pos)    get_Block_cfgpred_block_(node, pos)
#define get_Block_block_visited(node)         get_Block_block_visited_(node)
#define set_Block_block_visited(node, visit)  set_Block_block_visited_(node, visit)
#define mark_Block_block_visited(node)        mark_Block_block_visited_(node)
#define Block_block_visited(node)             Block_block_visited_(node)
#define is_Const_null(node)                   is_Const_null_(node)
#define is_Const_one(node)                    is_Const_one_(node)
#define is_Const_all_one(node)                is_Const_all_one_(node)
#define is_irn_forking(node)                  is_irn_forking_(node)
#define copy_node_attr(irg,oldn,newn)         copy_node_attr_(irg,oldn,newn)
#define get_irn_type_attr(node)               get_irn_type_attr_(node)
#define get_irn_entity_attr(node)             get_irn_entity_attr_(node)
#define is_irn_constlike(node)                is_irn_constlike_(node)
#define is_irn_keep(node)                     is_irn_keep_(node)
#define is_irn_start_block_placed(node)       is_irn_start_block_placed_(node)
#define get_irn_generic_attr(node)            get_irn_generic_attr_(node)
#define get_irn_generic_attr_const(node)      get_irn_generic_attr_const_(node)
#define get_irn_idx(node)                     get_irn_idx_(node)

#define get_irn_dbg_info(node)                get_irn_dbg_info_(node)
#define set_irn_dbg_info(node, db)            set_irn_dbg_info_(node, db)

#define set_Block_phis(block, phi)            set_Block_phis_(block, phi)
#define get_Block_phis(block)                 get_Block_phis_(block)
#define add_Block_phi(block, phi)             add_Block_phi_(block, phi)
#define get_Block_mark(block)                 get_Block_mark_(block)
#define set_Block_mark(block, mark)           set_Block_mark_(block, mark)

#define set_Phi_next(node, phi)               set_Phi_next_(node, phi)
#define get_Phi_next(node)                    get_Phi_next_(node)

#define ir_switch_table_get_n_entries(table)  ir_switch_table_get_n_entries_(table)

typedef struct ir_switch_table_entry {
	ir_tarval *min;
	ir_tarval *max;
	unsigned   pn;
} ir_switch_table_entry;

struct ir_switch_table {
	size_t                n_entries;
	ir_switch_table_entry entries[];
};

/** Attributes for Block nodes. */
typedef struct block_attr {
	ir_visited_t block_visited; /**< Visited flag for block walker. */
	unsigned    is_matured : 1; /**< If set, all inputs are fixed. */
	unsigned    dynamic_ins: 1; /**< If set in-array is an ARR_F on the heap. */
	unsigned    marked     : 1; /**< Can be used to temporary mark the block. */
	ir_node   **graph_arr;      /**< An array to store construction values. */
	ir_dom_info dom;            /**< Information about dominators. */
	ir_dom_info pdom;           /**< Information about post-dominators. */
	bitset_t   *backedge;       /**< Bit n set to true if pred n is backedge.*/
	ir_entity  *entity;         /**< entity representing this block */
	ir_node    *phis;           /**< The list of Phi nodes in this block. */
	double      execfreq;       /**< block execution frequency */
} block_attr;

/** Attributes for Cond nodes. */
typedef struct cond_attr {
	cond_jmp_predicate jmp_pred; /**< Jump predication. */
} cond_attr;

/** Attributes for Const nodes. */
typedef struct const_attr {
	ir_tarval *tarval;  /**< The value. */
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
	bool pinned             : 1;
	/**< Whether a fragile op produces X_except and X_regular values. */
	bool throws_exception   : 1;
	/**< Whether this node needs a label because of possible exception. */
	bool needs_except_label : 1;
	/**< Whether this node requires all callee-saves to be reloaded. */
	bool needs_reloaded_callee_saves : 1;
} except_attr;

/** Attributes for Call nodes. */
typedef struct call_attr {
	except_attr exc;          /**< Exception attribute. MUST be first. */
	ir_type     *type;        /**< type of called procedure */
	ir_entity   **callee_arr; /**< result of callee analysis */
} call_attr;

/** Attributes for Builtin nodes. */
typedef struct builtin_attr {
	except_attr     exc;   /**< Exception attribute. MUST be first. */
	ir_builtin_kind kind;  /**< kind of the called builtin procedure */
	ir_type         *type; /**< type of called builtin procedure */
} builtin_attr;

/** Attributes for Alloc nodes. */
typedef struct alloc_attr {
	unsigned alignment;
} alloc_attr;

/** Attributes for Load nodes. */
typedef struct load_attr {
	except_attr   exc;           /**< Exception attribute. MUST be first. */
	ENUMBF(ir_volatility) volatility:1; /**< Volatility of this Load */
	ENUMBF(ir_align)      unaligned:1;  /**< Address may be unaligend. */
	ir_mode       *mode;         /**< Mode of this Load operation. */
	ir_type       *type;         /**< Type of the object loaded. */
} load_attr;

/** Attributes for Store nodes. */
typedef struct store_attr {
	except_attr   exc;   /**< Exception attribute. MUST be first. */
	ir_type       *type; /**< Type of the object stored. */
	ENUMBF(ir_volatility) volatility:1;   /**< Volatility of this Store. */
	ENUMBF(ir_align)      unaligned:1;    /**< Address may be unaligned. */
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
	except_attr exc;          /**< Exception attribute. MUST be first. */
	ir_mode    *resmode;      /**< Result mode for the division. */
	unsigned    no_remainder:1; /**< Set, if known that a division can be done
	                                 without a remainder. */
} div_attr;

/** Attributes for Mod nodes. */
typedef struct mod_attr {
	except_attr exc;     /**< Exception attribute. MUST be first. */
	ir_mode    *resmode; /**< Result mode for the division. */
} mod_attr;

/** Attributes for ASM nodes. */
typedef struct asm_attr {
	except_attr        exc; /**< Exception attribute. MUST be first. */
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
	unsigned out_count   : 31;   /**< Number of outs in the list. */
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
 * Data of a function graph node.
 */
struct ir_node {
	firm_kind        kind;     /**< Distinguishes this node from others. */
	unsigned         node_idx; /**< The node index of this node in its graph. */
	ir_op           *op;       /**< The Opcode of this node. */
	ir_mode         *mode;     /**< The Mode of this node. */
	struct ir_node **in;       /**< The array of predecessors / operands. */
	ir_graph        *irg;
	ir_visited_t     visited;  /**< Visited counter for walks of the graph. */
	void            *link;     /**< To attach additional information to the
	                                node, e.g. used during optimization to link
	                                to nodes that shall replace a node. */
	dbg_info        *dbi;      /**< Information for debug support. */
	long             node_nr;  /**< Globally unique node number. */

	union {
		ir_def_use_edges *out;    /**< array of def-use edges. */
		unsigned          n_outs; /**< number of def-use edges (temporarily used
		                               during construction of data structure) */
	} o;
	ir_loop         *loop;         /**< Loop information. */
	void            *backend_info;
	irn_edges_info_t edge_info;    /**< Everlasting out edges. */

	/** Attributes of this node. Depends on opcode. Must be last field. */
	ir_attr attr;
};

/**
 * Returns the array with the ins.  The content of the array must not be
 * changed.
 * This is symmetric to set_irn_in().
 */
static inline ir_node **get_irn_in(ir_node const *const node)
{
	return node->in + 1;
}

/*-------------------------------------------------------------------*/
/*  These function are most used in libfirm.  Give them as static    */
/*  functions so they can be inlined.                                */
/*-------------------------------------------------------------------*/

static inline unsigned get_irn_idx_(const ir_node *node)
{
	return node->node_idx;
}

/**
 * Gets the op of a node.
 * Intern version for libFirm.
 */
static inline ir_op *get_irn_op_(const ir_node *node)
{
	return node->op;
}

/**
 * Sets the opcode struct of the node.
 */
static inline void set_irn_op(ir_node *node, ir_op *op)
{
	node->op = op;
}

/** Copies all attributes stored in the old node  to the new node.
    Assumes both have the same opcode and sufficient size. */
static inline void copy_node_attr_(ir_graph *irg, const ir_node *old_node,
                                   ir_node *new_node)
{
	ir_op *op = get_irn_op_(old_node);

	/* must always exist */
	op->ops.copy_attr(irg, old_node, new_node);
}

/**
 * Gets the opcode of a node.
 * Intern version for libFirm.
 */
static inline unsigned get_irn_opcode_(const ir_node *node)
{
	assert(k_ir_node == get_kind(node));
	return node->op->code;
}

/**
 * Returns the number of predecessors without the block predecessor.
 * Intern version for libFirm.
 */
static inline int get_irn_arity_(const ir_node *node)
{
	return (int)(ARR_LEN(node->in) - 1);
}

/**
 * Intern version for libFirm.
 */
static inline ir_node *get_irn_n_(const ir_node *node, int n)
{
	ir_node *nn;

	assert(-1 <= n && n < get_irn_arity_(node));

	nn = node->in[n + 1];
	if (!is_Id(nn)) return nn;

	return (node->in[n + 1] = skip_Id(nn));
}

/* include generated code */
#include "gen_irnode.h"

/**
 * returns a hash value for a node
 */
static inline unsigned hash_irn(const ir_node *node)
{
	return (unsigned) get_irn_idx(node);
}

/**
 * Gets the mode of a node.
 * Intern version for libFirm.
 */
static inline ir_mode *get_irn_mode_(const ir_node *node)
{
	return node->mode;
}

/**
 * Sets the mode of a node.
 * Intern version of libFirm.
 */
static inline void set_irn_mode_(ir_node *node, ir_mode *mode)
{
	node->mode = mode;
}

static inline ir_node *get_nodes_block_(const ir_node *node)
{
	assert(!is_Block(node));
	return get_irn_n(node, -1);
}

static inline ir_graph *get_irn_irg_(const ir_node *node)
{
	return node->irg;
}

/**
 * Gets the visited counter of a node.
 * Intern version for libFirm.
 */
static inline ir_visited_t get_irn_visited_(const ir_node *node)
{
	return node->visited;
}

/**
 * Sets the visited counter of a node.
 * Intern version for libFirm.
 */
static inline void set_irn_visited_(ir_node *node, ir_visited_t visited)
{
	node->visited = visited;
}

/**
 * Mark a node as visited in a graph.
 * Intern version for libFirm.
 */
static inline void mark_irn_visited_(ir_node *node)
{
	node->visited = get_irg_visited(get_irn_irg(node));
}

/**
 * Returns non-zero if a node of was visited.
 * Intern version for libFirm.
 */
static inline int irn_visited_(const ir_node *node)
{
	ir_graph *irg = get_irn_irg(node);
	return node->visited >= get_irg_visited(irg);
}

static inline int irn_visited_else_mark_(ir_node *node)
{
	if (irn_visited_(node))
		return 1;
	mark_irn_visited_(node);
	return 0;
}

/**
 * Sets the link of a node.
 * Intern version of libFirm.
 */
static inline void set_irn_link_(ir_node *node, void *link)
{
	assert(ir_resources_reserved(get_irn_irg(node)) & IR_RESOURCE_IRN_LINK);
	node->link = link;
}

/**
 * Returns the link of a node.
 * Intern version of libFirm.
 */
static inline void *get_irn_link_(const ir_node *node)
{
	assert(node->kind == k_ir_node);
	assert(ir_resources_reserved(get_irn_irg(node)) & IR_RESOURCE_IRN_LINK);
	return node->link;
}

/**
 * Returns whether the node _always_ must be pinned.
 * I.e., the node is not floating after global cse.
 *
 * Intern version of libFirm.
 */
static inline int get_irn_pinned_(const ir_node *node)
{
	assert(node->kind == k_ir_node);
	/* Check opcode */
	op_pin_state state = get_op_pinned_(get_irn_op_(node));
	if (state >= op_pin_state_exc_pinned)
		return (op_pin_state)node->attr.except.pinned;

	return state;
}

static inline int is_binop_(const ir_node *node)
{
	assert(node->kind == k_ir_node);
	return (node->op->opar == oparity_binary);
}

static inline bool is_irn_dynamic(ir_node const *const n)
{
	return get_irn_op(n)->opar == oparity_dynamic;
}

/**
 * Get the predecessor block.
 *
 * Returns the block corresponding to the predecessor pos.
 *
 * If we encounter the Bad node, this function returns NULL.
 */
static inline ir_node *get_Block_cfgpred_block_(const ir_node *node, int pos)
{
	ir_node *res = get_Block_cfgpred(node, pos);
	if (is_Bad(res)) {
		return NULL;
	} else {
		return get_nodes_block(res);
	}
}

static inline ir_visited_t get_Block_block_visited_(const ir_node *node)
{
	assert(is_Block(node));
	return node->attr.block.block_visited;
}

static inline void set_Block_block_visited_(ir_node *node, ir_visited_t visit)
{
	assert(is_Block(node));
	node->attr.block.block_visited = visit;
}

static inline void mark_Block_block_visited_(ir_node *node)
{
	node->attr.block.block_visited = get_irg_block_visited(get_irn_irg(node));
}

static inline int Block_block_visited_(const ir_node *node)
{
	ir_graph const *const irg = get_irn_irg(node);
	return node->attr.block.block_visited >= get_irg_block_visited(irg);
}

static inline long get_Const_long(ir_node const *const node)
{
	return get_tarval_long(get_Const_tarval_(node));
}

static inline int is_Const_null_(const ir_node *node)
{
	return tarval_is_null(get_Const_tarval_(node));
}

static inline int is_Const_one_(const ir_node *node)
{
	return tarval_is_one(get_Const_tarval_(node));
}

static inline int is_Const_all_one_(const ir_node *node)
{
	return tarval_is_all_one(get_Const_tarval_(node));
}

static inline int is_irn_forking_(const ir_node *node)
{
	return is_op_forking(get_irn_op_(node));
}

static inline ir_type *get_irn_type_attr_(ir_node *node)
{
	return get_irn_op_(node)->ops.get_type_attr(node);
}

static inline ir_entity *get_irn_entity_attr_(ir_node *node)
{
	return get_irn_op_(node)->ops.get_entity_attr(node);
}

static inline int is_irn_constlike_(const ir_node *node)
{
	return is_op_constlike(get_irn_op_(node));
}

static inline int is_irn_keep_(const ir_node *node)
{
	return is_op_keep(get_irn_op_(node));
}

static inline int is_irn_start_block_placed_(const ir_node *node)
{
	return is_op_start_block_placed(get_irn_op_(node));
}

/**
 * Tests whether a block can be reached only via an X_except Proj
 */
static inline int is_x_except_block(const ir_node *block)
{
	assert(is_Block(block));
	if (is_Block_end_block(block))
		return false;
	for (int i = 0; i < get_Block_n_cfgpreds(block); ++i) {
		if (is_x_except_Proj(get_Block_cfgpred(block, i)))
			return true;
	}
	return false;
}

/**
 * Tests whether a node is an X_except Proj, X_except block or the Return of an X_except block.
 */
static inline int is_x_except_branch(const ir_node *node)
{
	return is_x_except_Proj(node)
		|| (is_Block(node) && is_x_except_block(node))
		|| (is_Return(node) && is_x_except_block(get_nodes_block(node)));
}

/**
 * Tests whether a block can be reached only via an X_regular Proj
 */
static inline int is_x_regular_block(const ir_node *block)
{
	assert(is_Block(block));
	return get_Block_n_cfgpreds(block) == 1 &&
		is_x_regular_Proj(get_Block_cfgpred(block, 0));
}

static inline void *get_irn_generic_attr_(ir_node *node)
{
	return &node->attr;
}

static inline const void *get_irn_generic_attr_const_(const ir_node *node)
{
	return &node->attr;
}

static inline dbg_info *get_irn_dbg_info_(const ir_node *n)
{
	return n->dbi;
}

static inline void set_irn_dbg_info_(ir_node *n, dbg_info *db)
{
	n->dbi = db;
}

static inline int needs_exc_label(const ir_node *node)
{
	return is_fragile_op(node) && node->attr.except.needs_except_label;
}

static inline void set_needs_exc_label(ir_node *node, int flag)
{
	assert(is_fragile_op(node));
	node->attr.except.needs_except_label = flag;
}

/**
 * Sets the Phi list of a block.
 */
static inline void set_Block_phis_(ir_node *block, ir_node *phi)
{
	assert(ir_resources_reserved(get_irn_irg(block)) & IR_RESOURCE_PHI_LIST);
	assert(is_Block_(block));
	assert(phi == NULL || is_Phi_(phi));
	block->attr.block.phis = phi;
}

/**
 * Returns the link of a node.
 * Intern version of libFirm.
 */
static inline ir_node *get_Block_phis_(const ir_node *block)
{
	assert(ir_resources_reserved(get_irn_irg(block)) & IR_RESOURCE_PHI_LIST);
	assert(is_Block_(block));
	return block->attr.block.phis;
}

static inline void set_Phi_next_(ir_node *phi, ir_node *next)
{
	assert(ir_resources_reserved(get_irn_irg(phi)) & IR_RESOURCE_PHI_LIST);
	assert(is_Phi_(phi));
	phi->attr.phi.next = next;
}

static inline ir_node *get_Phi_next_(const ir_node *phi)
{
	assert(ir_resources_reserved(get_irn_irg(phi)) & IR_RESOURCE_PHI_LIST);
	assert(is_Phi_(phi));
	return phi->attr.phi.next;
}

/** Add a Phi node to the list of Block Phi's. */
static inline void add_Block_phi_(ir_node *block, ir_node *phi)
{
	assert(is_Block_(block));
	set_Phi_next_(phi, get_Block_phis_(block));
	set_Block_phis_(block, phi);
}

/** Get the Block mark (single bit). */
static inline unsigned get_Block_mark_(const ir_node *block)
{
	assert(is_Block_(block));
	return block->attr.block.marked;
}

/** Set the Block mark (single bit). */
static inline void set_Block_mark_(ir_node *block, unsigned mark)
{
	assert(is_Block_(block));
	block->attr.block.marked = mark;
}

static inline size_t ir_switch_table_get_n_entries_(const ir_switch_table *table)
{
	return table->n_entries;
}

static inline ir_switch_table_entry *ir_switch_table_get_entry(
		ir_switch_table *table, size_t entry)
{
	assert(entry < table->n_entries);
	return &table->entries[entry];
}

static inline const ir_switch_table_entry *ir_switch_table_get_entry_const(
		const ir_switch_table *table, size_t entry)
{
	assert(entry < table->n_entries);
	return &table->entries[entry];
}

void ir_register_getter_ops(void);

/** remove keep alive edge to node by rerouting the edge to a Bad node.
 * (rerouting is preferable to removing when we are in a walker which also
 *  accesses the End node) */
void remove_keep_alive(const ir_node *kept_node);

/**
 * Create a node similar to @p old.  Except for @p block and @p in all aspects
 * are copied from @p old.
 */
ir_node *new_similar_node(ir_node *old, ir_node *block, ir_node **in);

/**
 * Gets the Proj with number pn from irn.
 * Returns a null pointer, if no such Proj exists.
 */
ir_node *get_Proj_for_pn(ir_node const *irn, unsigned pn);

/**
 * Convenient block getter.
 * Works also, if the given node is a block.
 * @param  irn The node.
 * @return The block of the node, or the node itself, if the node is a
 *         block.
 */
static inline ir_node *get_block(ir_node *const irn)
{
	return is_Block(irn) ? irn : get_nodes_block(irn);
}

static inline ir_node const *get_block_const(ir_node const *const irn)
{
	return is_Block(irn) ? irn : get_nodes_block(irn);
}

/** Return whether a node is the 0 constant. */
static inline bool is_irn_null(ir_node const *const irn)
{
	return is_Const(irn) && is_Const_null(irn);
}

/** Return whether a node is the 1 constant. */
static inline bool is_irn_one(ir_node const *const irn)
{
	return is_Const(irn) && is_Const_one(irn);
}

#define foreach_irn_in(irn, idx, pred) \
	for (bool pred##__b = true; pred##__b;) \
		for (ir_node const *const pred##__irn = (irn); pred##__b; pred##__b = false) \
			for (int idx = 0, pred##__n = get_irn_arity(pred##__irn); pred##__b && idx != pred##__n; ++idx) \
				for (ir_node *const pred = (pred##__b = false, get_irn_n(pred##__irn, idx)); !pred##__b; pred##__b = true)

#define foreach_irn_in_r(irn, idx, pred) \
	for (bool pred##__b = true; pred##__b;) \
		for (ir_node const *const pred##__irn = (irn); pred##__b; pred##__b = false) \
			for (int idx = get_irn_arity(pred##__irn); pred##__b && idx-- != 0;) \
				for (ir_node *const pred = (pred##__b = false, get_irn_n(pred##__irn, idx)); !pred##__b; pred##__b = true)

/* pull in real inline definitions */
#include "irgraph_t.h"

#endif
