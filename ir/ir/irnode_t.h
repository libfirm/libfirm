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
 * @brief   Representation of an intermediate operation -- private header.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#ifndef FIRM_IR_IRNODE_T_H
#define FIRM_IR_IRNODE_T_H

#include "firm_config.h"
#include "irnode.h"
#include "irop_t.h"
#include "irgraph_t.h"
#include "irflag_t.h"
#include "firm_common_t.h"
#include "irdom_t.h" /* For size of struct dom_info. */
#include "dbginfo.h"
#include "irloop.h"
#include "iredgekinds.h"
#include "array.h"

#include "set.h"
#include "list.h"
#include "entity_t.h"
#include "type_t.h"
#include "tv_t.h"
#include "irextbb_t.h"


/** ir node attributes **/

/** Block attributes */
typedef struct {
	/* General attributes */
	ir_graph *irg;              /**< The graph this block belongs to. */
	unsigned long block_visited; /**< For the walker that walks over all blocks. */
	/* Attributes private to construction: */
	unsigned is_matured:1;      /**< If set, all in-nodes of the block are fixed. */
	unsigned is_dead:1;         /**< If set, the block is dead (and could be replace by a Bad. */
    unsigned is_mb_head:1;      /**< Set if this block is a macroblock head. */
	ir_node **graph_arr;        /**< An array to store all parameters. */
	/* Attributes holding analyses information */
	ir_dom_info dom;            /**< Datastructure that holds information about dominators.
	                                 @@@ @todo
	                                 Eventually overlay with graph_arr as only valid
	                                 in different phases.  Eventually inline the whole
	                                 datastructure. */
	ir_dom_info pdom;           /**< Datastructure that holds information about post-dominators. */
	ir_node ** in_cg;           /**< array with predecessors in
	                             * interprocedural_view, if they differ
	                             * from intraprocedural predecessors */
	int *backedge;              /**< Field n set to true if pred n is backedge.
	                                 @@@ @todo Ev. replace by bit field! */
	int *cg_backedge;           /**< Field n set to true if pred n is interprocedural backedge.
	                                 @@@ @todo Ev. replace by bit field! */
	ir_extblk *extblk;          /**< The extended basic block this block belongs to. */
	ir_region *region;          /**< The immediate structural region this block belongs to. */
    unsigned mb_depth;          /**< The macroblock depth: A distance from the macroblock header */

	struct list_head succ_head; /**< A list head for all successor edges of a block. */
} block_attr;

/** Cond attributes. */
typedef struct {
	cond_kind kind;           /**< flavor of Cond */
	long default_proj;        /**< only for non-binary Conds: biggest Proj number, i.e. the one used for default. */
	cond_jmp_predicate pred;  /**< only for binary Conds: The jump predication. */
} cond_attr;

/** Const attributes. */
typedef struct {
	tarval  *tv;       /**< the target value */
	ir_type *tp;       /**< the source type, for analyses. default: type_unknown. */
} const_attr;

/** SymConst attributes. */
typedef struct {
	symconst_symbol sym;  // old tori
	symconst_kind num;
	ir_type *tp;       /**< the source type, for analyses. default: type_unknown. */
} symconst_attr;

/** Sel attributes. */
typedef struct {
	ir_entity *ent;    /**< entity to select */
} sel_attr;

/** Exception attributes. */
typedef struct {
	op_pin_state   pin_state;     /**< the pin state for operations that might generate a exception:
									 If it's know that no exception will be generated, could be set to
									 op_pin_state_floats. */
#if PRECISE_EXC_CONTEXT
	struct ir_node **frag_arr;    /**< For Phi node construction in case of exception */
#endif
} except_attr;

/** Call attributes. */
typedef struct {
	except_attr    exc;           /**< the exception attribute. MUST be the first one. */
	ir_type *cld_tp;              /**< type of called procedure */
	ir_entity ** callee_arr;      /**< result of callee analysis */
} call_attr;

/** Alloc attributes. */
typedef struct {
	except_attr    exc;           /**< the exception attribute. MUST be the first one. */
	ir_type *type;                /**< Type of the allocated object.  */
	where_alloc where;            /**< stack, heap or other managed part of memory */
} alloc_attr;

/** Free attributes. */
typedef struct {
	ir_type *type;                /**< Type of the allocated object.  */
	where_alloc where;            /**< stack, heap or other managed part of memory */
} free_attr;

/** InstOf attributes. */
typedef struct {
	except_attr    exc;           /**< the exception attribute. MUST be the first one. */
	ir_type *type;                /**< the type of which the object pointer must be */
} io_attr;

/** Filter attributes. */
typedef struct {
	long proj;                 /**< contains the result position to project (Proj) */
	ir_node ** in_cg;          /**< array with interprocedural predecessors (Phi) */
	int *backedge;             /**< Field n set to true if pred n is backedge.
	                                @todo Ev. replace by bitfield! */
} filter_attr;

/** CallBegin attributes. */
typedef struct {
	ir_node * call;               /**< Associated Call-operation. */
} callbegin_attr;

/** Cast attributes. */
typedef struct {
	ir_type *totype;              /**< Type of the casted node. */
} cast_attr;

/** Load attributes. */
typedef struct {
	except_attr   exc;            /**< The exception attribute. MUST be the first one. */
	ir_mode       *load_mode;     /**< The mode of this Load operation. */
	ir_volatility volatility;     /**< The volatility of a Load/Store operation. */
} load_attr;

/** Store attributes. */
typedef struct {
	except_attr   exc;            /**< the exception attribute. MUST be the first one. */
	ir_volatility volatility;     /**< the volatility of a Store operation */
} store_attr;

typedef struct {
	int            pos;  /**< For Phi. Used to remember the value defined by
	                 this Phi node.  Needed when the Phi is completed
	                 to call get_r_internal_value to find the
	                 predecessors. If this attribute is set, the Phi
	                 node takes the role of the obsolete Phi0 node,
	                 therefore the name. */
} phi0_attr;


typedef pn_Cmp confirm_attr;    /**< Attribute to hold compare operation */

/** CopyB attribute. */
typedef struct {
	except_attr    exc;           /**< The exception attribute. MUST be the first one. */
	ir_type        *data_type;    /**< Type of the copied entity. */
} copyb_attr;

/** Bound attribute. */
typedef struct {
	except_attr exc;              /**< The exception attribute. MUST be the first one. */
} bound_attr;

/** Conv attribute. */
typedef struct {
	char           strict;        /**< If set, this is a strict Conv that cannot be removed. */
} conv_attr;

/** Div/Mod/DivMod/Quot attribute. */
typedef struct {
	except_attr    exc;           /**< The exception attribute. MUST be the first one. */
	ir_mode        *res_mode;     /**< Result mode for the division. */
} divmod_attr;

/** Inline Assembler support attribute. */
typedef struct {
	ident          *asm_text;     /**< The inline assembler text. */
} asm_attr;

/** Some IR-nodes just have one attribute, these are stored here,
   some have more. Their name is 'irnodename_attr' */
typedef union {
	block_attr     block;         /**< For Block: Fields needed to construct it */
	cond_attr      cond;          /**< For Cond. */
	const_attr     con;           /**< For Const: contains the value of the constant and a type */
	symconst_attr  symc;          /**< For SymConst. */
	sel_attr       sel;           /**< For Sel. */
	call_attr      call;          /**< For Call: pointer to the type of the method to call */
	callbegin_attr callbegin;     /**< For CallBegin */
	alloc_attr     alloc;         /**< For Alloc. */
	free_attr      free;          /**< For Free. */
	io_attr        instof;        /**< For InstOf */
	cast_attr      cast;          /**< For Cast. */
	load_attr      load;          /**< For Load. */
	store_attr     store;         /**< For Store. */
	phi0_attr      phi0;          /**< for Phi0 nodes. */
	int            *phi_backedge; /**< For Phi after construction.
	                                   Field n set to true if pred n is backedge.
	                                   @todo Ev. replace by bitfield! */
	long           proj;          /**< For Proj: contains the result position to project */
	confirm_attr   confirm_cmp;   /**< For Confirm: compare operation */
	filter_attr    filter;        /**< For Filter */
	except_attr    except;        /**< For Phi node construction in case of exceptions */
	copyb_attr     copyb;         /**< For CopyB operation */
	bound_attr     bound;         /**< For Bound operation */
	conv_attr      conv;          /**< For Conv operation */
	divmod_attr    divmod;        /**< For Div/Mod/DivMod operation */
	asm_attr       asm;           /**< For ASM operation. */
} attr;

/**
 * Edge info to put into an irn.
 */
typedef struct _irn_edge_kind_info_t {
	struct list_head outs_head;  /**< The list of all outs. */
	int out_count;               /**< Number of outs in the list. */
} irn_edge_info_t;

typedef irn_edge_info_t irn_edges_info_t[EDGE_KIND_LAST];

/**
 * The common structure of an irnode.
 * If the node has some attributes, they are stored in the attr field.
 */
struct ir_node {
	/* ------- Basics of the representation  ------- */
	firm_kind kind;          /**< Distinguishes this node from others. */
	ir_op *op;               /**< The Opcode of this node. */
	ir_mode *mode;           /**< The Mode of this node. */
	struct ir_node **in;     /**< The array of predecessors / operands. */
	unsigned long visited;   /**< The visited counter for walks of the graph. */
	unsigned node_idx;       /**< The node index of this node in its graph. */
	void *link;              /**< To attach additional information to the node, e.g.
	                              used while construction to link Phi0 nodes and
	                              during optimization to link to nodes that
	                              shall replace a node. */
	/* ------- Fields for optimizations / analysis information ------- */
	struct ir_node **out;    /**< @deprecated array of out edges. */
	struct dbg_info *dbi;    /**< A pointer to information for debug support. */
	/* ------- For debugging ------- */
#ifdef DEBUG_libfirm
	int out_valid;
	long node_nr;            /**< A unique node number for each node to make output
	                              readable. */
#endif
	/* ------- For analyses -------- */
	ir_loop *loop;           /**< the loop the node is in. Access routines in irloop.h */
#ifdef DO_HEAPANALYSIS
	struct abstval *av;      /**< Heapanalysis: The abstract value of this node. */
	struct section *sec;     /**< Heapanalysis: The section of this node. */
#endif
	struct ir_node **deps;   /**< Additional dependencies induced by state. */
	irn_edges_info_t edge_info;  /**< Everlasting out edges. */
	/* ------- Opcode depending fields -------- */
	attr attr;               /**< The set of attributes of this node. Depends on opcode.
	                              Must be last field of struct ir_node. */
};


/**
 * Returns the array with the ins.  The content of the array may not be
 * changed.
 * Note that this function returns the whole in array including the
 * block predecessor. So, it is NOT symmetric with set_irn_in().
 */
ir_node     **get_irn_in            (const ir_node *node);

/** @{ */
/** access attributes directly */
const_attr    get_irn_const_attr    (ir_node *node);
long          get_irn_proj_attr     (ir_node *node);
alloc_attr    get_irn_alloc_attr    (ir_node *node);
free_attr     get_irn_free_attr     (ir_node *node);
symconst_attr get_irn_symconst_attr (ir_node *node);
ir_type      *get_irn_call_attr     (ir_node *node);
ir_type      *get_irn_funccall_attr (ir_node *node);
sel_attr      get_irn_sel_attr      (ir_node *node);
int           get_irn_phi0_attr     (ir_node *node);
block_attr    get_irn_block_attr    (ir_node *node);
load_attr     get_irn_load_attr     (ir_node *node);
store_attr    get_irn_store_attr    (ir_node *node);
except_attr   get_irn_except_attr   (ir_node *node);
/** @} */

/**
 * The amount of additional space for custom data to be allocated upon creating a new node.
 */
extern unsigned firm_add_node_size;

/**
 * Sets the get_type operation for an ir_op_ops.
 *
 * @param code   the opcode for the default operation
 * @param ops    the operations initialized
 *
 * @return
 *    The operations.
 */
ir_op_ops *firm_set_default_get_type(ir_opcode code, ir_op_ops *ops);

/**
 * Sets the get_type_attr operation for an ir_op_ops.
 *
 * @param code   the opcode for the default operation
 * @param ops    the operations initialized
 *
 * @return
 *    The operations.
 */
ir_op_ops *firm_set_default_get_type_attr(ir_opcode code, ir_op_ops *ops);

/**
 * Sets the get_entity_attr operation for an ir_op_ops.
 *
 * @param code   the opcode for the default operation
 * @param ops    the operations initialized
 *
 * @return
 *    The operations.
 */
ir_op_ops *firm_set_default_get_entity_attr(ir_opcode code, ir_op_ops *ops);

/*-------------------------------------------------------------------*/
/*  These function are most used in libfirm.  Give them as static    */
/*  functions so they can be inlined.                                */
/*-------------------------------------------------------------------*/

/**
 * Checks whether a pointer points to a ir node.
 * Intern version for libFirm.
 */
static INLINE int
_is_ir_node(const void *thing) {
	return (get_kind(thing) == k_ir_node);
}

/**
 * Gets the op of a node.
 * Intern version for libFirm.
 */
static INLINE ir_op *
_get_irn_op(const ir_node *node) {
	assert(node);
	return node->op;
}

static INLINE void
_set_irn_op(ir_node *node, ir_op *op) {
	assert(node);
	node->op = op;
}

/** Copies all attributes stored in the old node  to the new node.
    Assumes both have the same opcode and sufficient size. */
static INLINE void
copy_node_attr(const ir_node *old_node, ir_node *new_node) {
	ir_op *op = _get_irn_op(old_node);

	/* must always exist */
	op->ops.copy_attr(old_node, new_node);
}

/**
 * Gets the opcode of a node.
 * Intern version for libFirm.
 */
static INLINE ir_opcode
_get_irn_opcode(const ir_node *node) {
	assert(k_ir_node == get_kind(node));
	assert(node->op);
	return node->op->code;
}

/**
 * Returns the number of predecessors without the block predecessor.
 * Intern version for libFirm.
 */
static INLINE int
_get_irn_intra_arity(const ir_node *node) {
	assert(node);
	return ARR_LEN(node->in) - 1;
}

/**
 * Returns the number of predecessors without the block predecessor.
 * Intern version for libFirm.
 */
static INLINE int
_get_irn_inter_arity(const ir_node *node) {
	assert(node);
	if (_get_irn_op(node) == op_Filter) {
		assert(node->attr.filter.in_cg);
		return ARR_LEN(node->attr.filter.in_cg) - 1;
	} else if (_get_irn_op(node) == op_Block && node->attr.block.in_cg) {
		return ARR_LEN(node->attr.block.in_cg) - 1;
	}
	return _get_irn_intra_arity(node);
}

/**
 * Returns the number of predecessors without the block predecessor.
 * Intern version for libFirm.
 */
extern int (*_get_irn_arity)(const ir_node *node);

/**
 * Intern version for libFirm.
 */
static INLINE ir_node *
_get_irn_intra_n(const ir_node *node, int n) {
	ir_node *nn;

	assert(node);
	assert(-1 <= n && n < _get_irn_intra_arity(node));

	nn = node->in[n + 1];
	if (nn == NULL) {
		/* only block inputs are allowed to be NULL */
		assert(n == -1 && "NULL input of a node");
		return NULL;
	}
	if (nn->op != op_Id) return nn;

	return (node->in[n + 1] = skip_Id(nn));
}

/**
 * Intern version for libFirm.
 */
static INLINE ir_node*
_get_irn_inter_n(const ir_node *node, int n) {
	assert(node); assert(-1 <= n && n < _get_irn_inter_arity(node));

	/* handle Filter and Block specially */
	if (_get_irn_op(node) == op_Filter) {
		assert(node->attr.filter.in_cg);
		return (node->attr.filter.in_cg[n + 1] = skip_Id(node->attr.filter.in_cg[n + 1]));
	} else if (_get_irn_op(node) == op_Block && node->attr.block.in_cg) {
		return (node->attr.block.in_cg[n + 1] = skip_Id(node->attr.block.in_cg[n + 1]));
	}

	return _get_irn_intra_n(node, n);
}

/**
 * Access to the predecessors of a node.
 * To iterate over the operands iterate from 0 to i < get_irn_arity(),
 * to iterate including the Block predecessor iterate from i = -1 to
 * i < get_irn_arity.
 * If it is a block, the entry -1 is NULL.
 * Intern version for libFirm.
 */
extern ir_node *(*_get_irn_n)(const ir_node *node, int n);

static INLINE int _get_irn_deps(const ir_node *node) {
	return node->deps ? ARR_LEN(node->deps) : 0;
}

static INLINE ir_node *_get_irn_dep(const ir_node *node, int pos) {
	assert(node->deps && "dependency array node yet allocated. use add_irn_dep()");
	assert(pos >= 0 && pos < ARR_LEN(node->deps) && "dependency index out of range");
	return node->deps[pos];
}

static INLINE void
_set_irn_dep(ir_node *node, int pos, ir_node *dep) {
	ir_node *old;

	assert(node->deps && "dependency array node yet allocated. use add_irn_dep()");
	assert(pos >= 0 && pos < ARR_LEN(node->deps) && "dependency index out of range");
	old = node->deps[pos];
	node->deps[pos] = dep;
	edges_notify_edge_kind(node, pos, dep, old, EDGE_KIND_DEP, get_irn_irg(node));
}


static INLINE int
_get_irn_ins_or_deps(const ir_node *irn) {
	return _get_irn_deps(irn) + _get_irn_arity(irn);
}

static INLINE ir_node *
_get_irn_in_or_dep(const ir_node *irn, int pos) {
	int n_in = get_irn_arity(irn);
	return pos < n_in ? get_irn_n(irn, pos) : get_irn_dep(irn, pos - n_in);
}

/**
 * Gets the mode of a node.
 * Intern version for libFirm.
 */
static INLINE ir_mode *
_get_irn_mode(const ir_node *node) {
	assert(node);
	return node->mode;
}

/**
 * Sets the mode of a node.
 * Intern version of libFirm.
 */
static INLINE void
_set_irn_mode(ir_node *node, ir_mode *mode) {
	assert(node);
	node->mode = mode;
}

/**
 * Gets the visited counter of a node.
 * Intern version for libFirm.
 */
static INLINE unsigned long
_get_irn_visited(const ir_node *node) {
	assert(node);
	return node->visited;
}

/**
 * Sets the visited counter of a node.
 * Intern version for libFirm.
 */
static INLINE void
_set_irn_visited(ir_node *node, unsigned long visited) {
	assert(node);
	node->visited = visited;
}

/**
 * Mark a node as visited in a graph.
 * Intern version for libFirm.
 */
static INLINE void
_mark_irn_visited(ir_node *node) {
	assert(node);
	node->visited = current_ir_graph->visited;
}

/**
 * Returns non-zero if a node of was visited.
 * Intern version for libFirm.
 */
static INLINE int
_irn_visited(const ir_node *node) {
	assert(node);
	return (node->visited >= current_ir_graph->visited);
}

/**
 * Returns non-zero if a node of was NOT visited.
 * Intern version for libFirm.
 */
static INLINE int
_irn_not_visited(const ir_node *node) {
	assert(node);
	return (node->visited < current_ir_graph->visited);
}

/**
 * Sets the link of a node.
 * Intern version of libFirm.
 */
static INLINE void
_set_irn_link(ir_node *node, void *link) {
  assert(node);
	/* Link field is used for Phi construction and various optimizations
	   in iropt. */
	assert(get_irg_phase_state(get_irn_irg(node)) != phase_building);

	node->link = link;
}

/**
 * Returns the link of a node.
 * Intern version of libFirm.
 */
static INLINE void *
_get_irn_link(const ir_node *node) {
	assert(node && _is_ir_node(node));
	return node->link;
}

/**
 * Returns whether the node _always_ must be pinned.
 * I.e., the node is not floating after global cse.
 *
 * Intern version of libFirm.
 */
static INLINE op_pin_state
_get_irn_pinned(const ir_node *node) {
	op_pin_state state;
	assert(node && _is_ir_node(node));
	/* Check opcode */
	state = _get_op_pinned(_get_irn_op(node));

	if (state >= op_pin_state_exc_pinned)
		return get_opt_fragile_ops() ? node->attr.except.pin_state : op_pin_state_pinned;
	return state;
}

static INLINE op_pin_state
_is_irn_pinned_in_irg(const ir_node *node) {
	if (get_irg_pinned(get_irn_irg(node)) == op_pin_state_floats)
		return get_irn_pinned(node);
	return op_pin_state_pinned;
}

static INLINE int
_is_unop(const ir_node *node) {
	assert(node && _is_ir_node(node));
	return (node->op->opar == oparity_unary);
}

static INLINE int
_is_binop(const ir_node *node) {
	assert(node && _is_ir_node(node));
	return (node->op->opar == oparity_binary);
}

static INLINE int
_is_Bad(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Bad);
}

static INLINE int
_is_NoMem(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_NoMem);
}

static INLINE int
_is_Mod(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Mod);
}

static INLINE int
_is_Div(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Div);
}

static INLINE int
_is_DivMod(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_DivMod);
}

static INLINE int
_is_Quot(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Quot);
}

static INLINE int
_is_Add(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Add);
}

static INLINE int
_is_Sub(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Sub);
}

static INLINE int
_is_Start(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Start);
}

static INLINE int
_is_End(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_End);
}

static INLINE int
_is_Const(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Const);
}

static INLINE int
_is_Conv(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Conv);
}

static INLINE int
_is_CopyB(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_CopyB);
}

static INLINE int
_is_Unknown(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Unknown);
}

static INLINE int
_is_Return(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Return);
}

static INLINE int
_is_Call(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Call);
}

static INLINE int
_is_Sel(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Sel);
}

static INLINE int
_is_Mul(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Mul);
}

static INLINE int
_is_Mux(const ir_node *node) {
	assert(node);
	if (node) {
		ir_op *op = _get_irn_op(node);
		return (op == op_Mux || ((op == op_Psi) && _get_irn_arity(node) == 3));
	}
	return 0;
}

static INLINE int
_is_Load(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Load);
}

static INLINE int
_is_Store(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Store);
}

static INLINE int
_is_Sync(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Sync);
}

static INLINE int
_is_Confirm(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Confirm);
}

static INLINE int
_is_Pin(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Pin);
}

static INLINE int
_is_SymConst(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_SymConst);
}

static INLINE int
_is_Cond(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Cond);
}

static INLINE int
_is_Cmp(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Cmp);
}

static INLINE int
_is_Alloc(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Alloc);
}

static INLINE int
_is_Jmp(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Jmp);
}

static INLINE int
_is_Raise(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Raise);
}

static INLINE int
_is_no_Block(const ir_node *node) {
	assert(node && _is_ir_node(node));
	return (_get_irn_op(node) != op_Block);
}

static INLINE int
_is_Block(const ir_node *node) {
	assert(node && _is_ir_node(node));
	return (_get_irn_op(node) == op_Block);
}

static INLINE int
_get_Block_n_cfgpreds(const ir_node *node) {
	assert(_is_Block(node));
	return _get_irn_arity(node);
}

static INLINE ir_node *
_get_Block_cfgpred(ir_node *node, int pos) {
	assert(0 <= pos && pos < get_irn_arity(node));
	assert(_is_Block(node));
	return _get_irn_n(node, pos);
}

/* Get the predecessor block.
 *
 *  Returns the block corresponding to the predecessor pos.
 *
 *  There are several ambiguities we resolve with this function:
 *  - The direct predecessor can be a Proj, which is not pinned.
 *    We walk from the predecessor to the next pinned node
 *    (skip_Proj) and return the block that node is in.
 *  - If we encounter the Bad node, this function does not return
 *    Start, but the Bad node.
 */
static INLINE ir_node  *
_get_Block_cfgpred_block(ir_node *node, int pos) {
	ir_node *res = skip_Proj(get_Block_cfgpred(node, pos));
	if (!is_Bad(res))
		res = get_nodes_block(res);
	return res;
}

static INLINE unsigned long
_get_Block_block_visited(const ir_node *node) {
	assert(node->op == op_Block);
	return node->attr.block.block_visited;
}

static INLINE void
_set_Block_block_visited(ir_node *node, unsigned long visit) {
	assert(node->op == op_Block);
	node->attr.block.block_visited = visit;
}

/* For this current_ir_graph must be set. */
static INLINE void
_mark_Block_block_visited(ir_node *node) {
	assert(node->op == op_Block);
	node->attr.block.block_visited = get_irg_block_visited(current_ir_graph);
}

static INLINE int
_Block_not_block_visited(const ir_node *node) {
	assert(node->op == op_Block);
	return (node->attr.block.block_visited < get_irg_block_visited(current_ir_graph));
}

static INLINE int
_Block_block_visited(const ir_node *node) {
	assert(node->op == op_Block);
	return (node->attr.block.block_visited >= get_irg_block_visited(current_ir_graph));
}

static INLINE ir_node *
_set_Block_dead(ir_node *block) {
	assert(_get_irn_op(block) == op_Block);
	block->attr.block.is_dead = 1;
	return block;
}

static INLINE int
_is_Block_dead(const ir_node *block) {
	ir_op *op = _get_irn_op(block);

	if (op == op_Bad)
		return 1;
	else {
		assert(op == op_Block);
		return block->attr.block.is_dead;
	}
}

static INLINE tarval *_get_Const_tarval(const ir_node *node) {
	assert(_get_irn_op(node) == op_Const);
	return node->attr.con.tv;
}

static INLINE cnst_classify_t _classify_Const(ir_node *node) {
	ir_op *op;
	assert(_is_ir_node(node));

	op = _get_irn_op(node);

	if (op == op_Const)
		return classify_tarval(_get_Const_tarval(node));
	else if(op == op_SymConst)
		return CNST_SYMCONST;

	return CNST_NO_CONST;
}

static INLINE int _is_irn_forking(const ir_node *node) {
	return is_op_forking(_get_irn_op(node));
}

static INLINE ir_type *_get_irn_type(ir_node *node) {
	return _get_irn_op(node)->ops.get_type(node);
}

static INLINE ir_type *_get_irn_type_attr(ir_node *node) {
	return _get_irn_op(node)->ops.get_type_attr(node);
}

static INLINE ir_entity *_get_irn_entity_attr(ir_node *node) {
  return _get_irn_op(node)->ops.get_entity_attr(node);
}

static INLINE int _is_irn_constlike(const ir_node *node) {
	return is_op_constlike(_get_irn_op(node));
}

static INLINE int _is_irn_always_opt(const ir_node *node) {
	return is_op_always_opt(_get_irn_op(node));
}

static INLINE int _is_irn_keep(const ir_node *node) {
	return is_op_keep(_get_irn_op(node));
}

static INLINE int _is_irn_start_block_placed(const ir_node *node) {
	return is_op_start_block_placed(_get_irn_op(node));
}

static INLINE int _is_irn_machine_op(const ir_node *node) {
	return is_op_machine(_get_irn_op(node));
}

static INLINE int _is_irn_machine_operand(const ir_node *node) {
	return is_op_machine_operand(_get_irn_op(node));
}

static INLINE int _is_irn_machine_user(const ir_node *node, unsigned n) {
	return is_op_machine_user(_get_irn_op(node), n);
}

static INLINE cond_jmp_predicate _get_Cond_jmp_pred(const ir_node *node) {
	assert(_get_irn_op(node) == op_Cond);
	return node->attr.cond.pred;
}

static INLINE void _set_Cond_jmp_pred(ir_node *node, cond_jmp_predicate pred) {
	assert(_get_irn_op(node) == op_Cond);
	node->attr.cond.pred = pred;
}

static INLINE int _get_Psi_n_conds(ir_node *node) {
	assert(_get_irn_op(node) == op_Psi);
	return _get_irn_arity(node) >> 1;
}

static INLINE void *_get_irn_generic_attr(ir_node *node) {
	return &node->attr;
}

static INLINE const void *_get_irn_generic_attr_const(const ir_node *node) {
	return &node->attr;
}

static INLINE unsigned _get_irn_idx(const ir_node *node) {
	return node->node_idx;
}

/* this section MUST contain all inline functions */
#define is_ir_node(thing)                     _is_ir_node(thing)
#define get_irn_intra_arity(node)             _get_irn_intra_arity(node)
#define get_irn_inter_arity(node)             _get_irn_inter_arity(node)
#define get_irn_arity(node)                   _get_irn_arity(node)
#define get_irn_intra_n(node, n)              _get_irn_intra_n(node, n)
#define get_irn_inter_n(node, n)              _get_irn_inter_n(node, n)
#define get_irn_n(node, n)                    _get_irn_n(node, n)
#define get_irn_mode(node)                    _get_irn_mode(node)
#define set_irn_mode(node, mode)              _set_irn_mode(node, mode)
#define get_irn_op(node)                      _get_irn_op(node)
#define set_irn_op(node, op)                  _set_irn_op(node, op)
#define get_irn_opcode(node)                  _get_irn_opcode(node)
#define get_irn_visited(node)                 _get_irn_visited(node)
#define set_irn_visited(node, v)              _set_irn_visited(node, v)
#define mark_irn_visited(node)                _mark_irn_visited(node)
#define irn_visited(node)                     _irn_visited(node)
#define irn_not_visited(node)                 _irn_not_visited(node)
#define set_irn_link(node, link)              _set_irn_link(node, link)
#define get_irn_link(node)                    _get_irn_link(node)
#define get_irn_pinned(node)                  _get_irn_pinned(node)
#define is_irn_pinned_in_irg(node)            _is_irn_pinned_in_irg(node)
#define is_unop(node)                         _is_unop(node)
#define is_binop(node)                        _is_binop(node)
#define is_Const(node)                        _is_Const(node)
#define is_Conv(node)                         _is_Conv(node)
#define is_Unknown(node)                      _is_Unknown(node)
#define is_Return(node)                       _is_Return(node)
#define is_Call(node)                         _is_Call(node)
#define is_Sel(node)                          _is_Sel(node)
#define is_Mul(node)                          _is_Mul(node)
#define is_Mux(node)                          _is_Mux(node)
#define is_Load(node)                         _is_Load(node)
#define is_Sync(node)                         _is_Sync(node)
#define is_Confirm(node)                      _is_Confirm(node)
#define is_Pin(node)                          _is_Pin(node)
#define is_SymConst(node)                     _is_SymConst(node)
#define is_Cond(node)                         _is_Cond(node)
#define is_CopyB(node)                        _is_CopyB(node)
#define is_Cmp(node)                          _is_Cmp(node)
#define is_Alloc(node)                        _is_Alloc(node)
#define is_Jmp(node)                          _is_Jmp(node)
#define is_Raise(node)                        _is_Raise(node)
#define is_Bad(node)                          _is_Bad(node)
#define is_NoMem(node)                        _is_NoMem(node)
#define is_Start(node)                        _is_Start(node)
#define is_End(node)                          _is_End(node)
#define is_Mod(node)                          _is_Mod(node)
#define is_Div(node)                          _is_Div(node)
#define is_DivMod(node)                       _is_DivMod(node)
#define is_Quot(node)                         _is_Quot(node)
#define is_Add(node)                          _is_Add(node)
#define is_Sub(node)                          _is_Sub(node)
#define is_no_Block(node)                     _is_no_Block(node)
#define is_Block(node)                        _is_Block(node)
#define get_Block_n_cfgpreds(node)            _get_Block_n_cfgpreds(node)
#define get_Block_cfgpred(node, pos)          _get_Block_cfgpred(node, pos)
#define get_Block_cfgpred_block(node, pos)    _get_Block_cfgpred_block(node, pos)
#define get_Block_block_visited(node)         _get_Block_block_visited(node)
#define set_Block_block_visited(node, visit)  _set_Block_block_visited(node, visit)
#define mark_Block_block_visited(node)        _mark_Block_block_visited(node)
#define Block_not_block_visited(node)         _Block_not_block_visited(node)
#define Block_block_visited(node)             _Block_block_visited(node)
#define set_Block_dead(block)                 _set_Block_dead(block)
#define is_Block_dead(block)                  _is_Block_dead(block)
#define get_Const_tarval(node)                _get_Const_tarval(node)
#define classify_Const(node)                  _classify_Const(node)
#define is_irn_forking(node)                  _is_irn_forking(node)
#define get_irn_type(node)                    _get_irn_type(node)
#define get_irn_type_attr(node)               _get_irn_type_attr(node)
#define get_irn_entity_attr(node)             _get_irn_entity_attr(node)
#define is_irn_constlike(node)                _is_irn_constlike(node)
#define is_irn_always_opt(node)               _is_irn_always_opt(node)
#define is_irn_keep(node)                     _is_irn_keep(node)
#define is_irn_start_block_placed(node)       _is_irn_start_block_placed(node)
#define is_irn_machine_op(node)               _is_irn_machine_op(node)
#define is_irn_machine_operand(node)          _is_irn_machine_operand(node)
#define is_irn_machine_user(node, n)          _is_irn_machine_user(node, n)
#define get_Cond_jmp_pred(node)               _get_Cond_jmp_pred(node)
#define set_Cond_jmp_pred(node, pred)         _set_Cond_jmp_pred(node, pred)
#define get_Psi_n_conds(node)                 _get_Psi_n_conds(node)
#define get_irn_generic_attr(node)            _get_irn_generic_attr(node)
#define get_irn_generic_attr_const(node)      _get_irn_generic_attr_const(node)
#define get_irn_idx(node)                     _get_irn_idx(node)

#define get_irn_deps(node)                    _get_irn_deps(node)
#define set_irn_dep(node, pos, dep)           _set_irn_dep(node, pos, dep)
#define get_irn_dep(node, pos)                _get_irn_dep(node, pos)

#define get_irn_ins_or_deps(node)             _get_irn_ins_or_deps(node)
#define get_irn_in_or_dep(node, pos)          _get_irn_in_or_dep(node, pos)

#endif
