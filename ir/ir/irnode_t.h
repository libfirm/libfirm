/*
 * Project:     libFIRM
 * File name:   ir/ir/irnode_t.h
 * Purpose:     Representation of an intermediate operation -- private header.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


/**
 * @file irnode_t.h
 *
 * Declarations of an ir node.
 *
 * @author Martin Trapp, Christian Schaefer
 */


# ifndef _IRNODE_T_H_
# define _IRNODE_T_H_

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

# include "irnode.h"
# include "irop_t.h"
# include "irgraph_t.h"
# include "irflag_t.h"
# include "firm_common_t.h"
# include "irdom_t.h" /* For size of struct dom_info. */
# include "dbginfo.h"
# include "irloop.h"
# include "array.h"

# include "exc.h"

#include "set.h"
#include "entity_t.h"
#include "type_t.h"

/** ir node attributes **/

/** Block attributes */
typedef struct {
  /* General attributes */
  ir_graph *irg;
  unsigned long block_visited;  /**< for the walker that walks over all blocks. */
  /* Attributes private to construction: */
  bool matured;               /**< if set, all in-nodes of the block are fixed */
  struct ir_node **graph_arr; /**< array to store all parameters */
  /* Attributes holding analyses information */
  struct dom_info dom;        /**< Datastructure that holds information about dominators.
                 @@@ @todo
                 Eventually overlay with graph_arr as only valid
                 in different phases.  Eventually inline the whole
                 datastructure. */
  /*   exc_t exc;  */            /**< role of this block for exception handling */
  /*   ir_node *handler_entry; */    /**< handler entry block iff this block is part of a region */
  ir_node ** in_cg;           /**< array with predecessors in
                   * interprocedural_view, if they differ
                   * from intraprocedural predecessors */
  int *backedge;              /**< Field n set to true if pred n is backedge.
                     @@@ @todo Ev. replace by bitfield! */
  int *cg_backedge;           /**< Field n set to true if pred n is interprocedural backedge.
                     @@@ @todo Ev. replace by bitfield! */
} block_attr;

/** Start attributes */
typedef struct {
  char dummy;
  /*   ir_graph *irg;   @@@ now in block */
} start_attr;

/** Cond attributes */
typedef struct {
  cond_kind kind;    /**< flavor of Cond */
  long default_proj; /**< for optimization: biggest Proj number, i.e. the one
              used for default. */
} cond_attr;

/** Const attributes */
typedef struct {
  tarval *tv;        /**< the target value */
  type   *tp;        /**< the source type, for analyses. default: type_unknown. */
} const_attr;

typedef struct {
  symconst_symbol sym;  // old tori
  symconst_kind num;
  type *tp;          /**< the source type, for analyses. default: type_unknown. */
} symconst_attr;

/** Sel attributes */
typedef struct {
  entity *ent;          /**< entity to select */
} sel_attr;

typedef struct {
  type *cld_tp;         /**< type of called procedure */
#if PRECISE_EXC_CONTEXT
  struct ir_node **frag_arr; /**< For Phi node construction in case of exceptions */
#endif
  entity ** callee_arr; /**< result of callee analysis */
} call_attr;

/** Alloc attributes */
typedef struct {
  type *type;           /**< Type of the allocated object.  */
  where_alloc where;    /**< stack, heap or other managed part of memory */
#if PRECISE_EXC_CONTEXT
  struct ir_node **frag_arr; /**< For Phi node construction in case of exceptions */
#endif
} alloc_attr;

/** InstOf attributes */
typedef struct
{
  type *ent;
  int dfn;
} io_attr;

/** Filter attributes */
typedef struct {
  long proj;                 /**< contains the result position to project (Proj) */
  ir_node ** in_cg;          /**< array with interprocedural predecessors (Phi) */
  int *backedge;              /**< Field n set to true if pred n is backedge.
                     @todo Ev. replace by bitfield! */
} filter_attr;

/** EndReg/EndExcept attributes */
typedef struct {
  char dummy;
  /*   ir_graph * irg; */           /**< ir_graph this node belongs to (for */
  /*                               * navigating in interprocedural graphs)  */
  /*                       @@@ now in block */
} end_attr;

/** CallBegin attributes */
typedef struct {
  /*   ir_graph * irg; */           /**< ir_graph this node belongs to (for */
  /*                   * navigating in interprocedural graphs) */
  /*                            @@@ now in block */
  ir_node * call;            /**< associated Call-operation */
} callbegin_attr;

/** Cast attributes */
typedef struct {
  type *totype;
} cast_attr;

typedef pn_Cmp confirm_attr; /** Attribute to hold compare operation */

/** Some irnodes just have one attribute, these are stored here,
   some have more. Their name is 'irnodename_attr' */
typedef union {
  start_attr     start; /**< For Start */
  block_attr     block; /**< For Block: Fields needed to construct it */
  cond_attr      c;     /**< For Cond. */
  const_attr     con;   /**< For Const: contains the value of the constant and a type */
  symconst_attr  i;     /**< For SymConst. */
  sel_attr       s;     /**< For Sel. */
  call_attr      call;  /**< For Call: pointer to the type of the method to call */
  callbegin_attr callbegin; /**< For CallBegin */
  alloc_attr     a;     /**< For Alloc. */
  io_attr        io;    /**< For InstOf */
  type          *f;     /**< For Free. */
  cast_attr      cast;  /**< For Cast. */
  int            phi0_pos;  /**< For Phi. Used to remember the value defined by
                   this Phi node.  Needed when the Phi is completed
                   to call get_r_internal_value to find the
                   predecessors. If this attribute is set, the Phi
                   node takes the role of the obsolete Phi0 node,
                   therefore the name. */
  int *phi_backedge;    /**< For Phi after construction.
               Field n set to true if pred n is backedge.
               @todo Ev. replace by bitfield! */
  long           proj;  /**< For Proj: contains the result position to project */
  confirm_attr   confirm_cmp;   /**< For Confirm: compare operation */
  filter_attr    filter;    /**< For Filter */
  end_attr       end;       /**< For EndReg, EndExcept */
#if PRECISE_EXC_CONTEXT
  struct ir_node **frag_arr; /**< For Phi node construction in case of exceptions
                for nodes Store, Load, Div, Mod, Quot, DivMod. */
#endif
} attr;


/** common structure of an irnode
    if the node has some attributes, they are stored in attr */
struct ir_node {
  /* ------- Basics of the representation  ------- */
  firm_kind kind;          /**< distinguishes this node from others */
  ir_op *op;               /**< Opcode of this node. */
  ir_mode *mode;           /**< Mode of this node. */
  unsigned long visited;   /**< visited counter for walks of the graph */
  struct ir_node **in;     /**< array with predecessors / operands */
  void *link;              /**< to attach additional information to the node, e.g.
                              used while construction to link Phi0 nodes and
                  during optimization to link to nodes that
                  shall replace a node. */
  /* ------- Fields for optimizations / analysis information ------- */
  struct ir_node **out;    /**< array of out edges */
  struct dbg_info* dbi;    /**< A pointer to information for debug support. */
  /* ------- For debugging ------- */
#ifdef DEBUG_libfirm
  int out_valid;                /** < indicate that out edges are valid */
  int node_nr;             /**< a unique node number for each node to make output
                  readable. */
#endif
  /* ------- For analyses -------- */
  ir_loop *loop;           /**< the loop the node is in. Access routines in irloop.h */
#ifdef  DO_HEAPANALYSIS
  struct abstval *av;
  struct section *sec;
#endif
  /* ------- Opcode depending fields -------- */
  attr attr;               /**< attribute of this node. Depends on opcode.
                              Must be last field of struct ir_node. */
};

/** Copies all attributes stored in the old node  to the new node.
    Assumes both have the same opcode and sufficient size. */
void
copy_attrs(const ir_node *old_node, ir_node *new_node);


/** Returns the array with the ins.  The content of the array may not be
   changed.  */
ir_node     **get_irn_in            (const ir_node *node);

/** @{ */
/** access attributes directly */
INLINE const_attr    get_irn_const_attr    (ir_node *node);
INLINE long          get_irn_proj_attr     (ir_node *node);
INLINE alloc_attr    get_irn_alloc_attr    (ir_node *node);
INLINE type         *get_irn_free_attr     (ir_node *node);
INLINE symconst_attr get_irn_symconst_attr (ir_node *node);
type         *get_irn_call_attr     (ir_node *node);
type         *get_irn_funccall_attr (ir_node *node);
sel_attr      get_irn_sel_attr      (ir_node *node);
int           get_irn_phi_attr      (ir_node *node);
block_attr    get_irn_block_attr   (ir_node *node);
/** @} */

/*-------------------------------------------------------------------*/
/*  These function are most used in libfirm.  Give them as static    */
/*  functions so they can be inlined.                                */
/*-------------------------------------------------------------------*/

/**
 * Checks whether a pointer points to a ir node.
 * Intern version for libFirm.
 */
static INLINE int
__is_ir_node (const void *thing) {
  return (get_kind(thing) == k_ir_node);
}

/**
 * Gets the op of a node.
 * Intern version for libFirm.
 */
static INLINE ir_op *
__get_irn_op (const ir_node *node)
{
  assert (node);
  return node->op;
}

/**
 * Gets the opcode of a node.
 * Intern version for libFirm.
 */
static INLINE opcode
__get_irn_opcode (const ir_node *node)
{
  assert (k_ir_node == get_kind(node));
  assert (node -> op);
  return node->op->code;
}

/**
 * Returns the number of predecessors without the block predecessor.
 * Intern version for libFirm.
 */
static INLINE int
__get_irn_intra_arity (const ir_node *node) {
  assert(node);
  return ARR_LEN(node->in) - 1;
}

/**
 * Returns the number of predecessors without the block predecessor.
 * Intern version for libFirm.
 */
static INLINE int
__get_irn_inter_arity (const ir_node *node) {
  assert(node);
  if (__get_irn_opcode(node) == iro_Filter) {
    assert(node->attr.filter.in_cg);
    return ARR_LEN(node->attr.filter.in_cg) - 1;
  } else if (__get_irn_opcode(node) == iro_Block && node->attr.block.in_cg) {
    return ARR_LEN(node->attr.block.in_cg) - 1;
  }
  return __get_irn_intra_arity(node);
}

/**
 * Returns the number of predecessors without the block predecessor.
 * Intern version for libFirm.
 */
static INLINE int
__get_irn_arity (const ir_node *node) {
  assert(node);
  if (interprocedural_view) return __get_irn_inter_arity(node);
  return __get_irn_intra_arity(node);
}

/**
 * Intern version for libFirm.
 */
static INLINE ir_node *
__get_irn_intra_n (ir_node *node, int n) {
  return (node->in[n + 1] = skip_Id(node->in[n + 1]));
}

/**
 * Intern version for libFirm.
 */
static INLINE ir_node*
__get_irn_inter_n (ir_node *node, int n) {
  /* handle Filter and Block specially */
  if (__get_irn_opcode(node) == iro_Filter) {
    assert(node->attr.filter.in_cg);
    return (node->attr.filter.in_cg[n + 1] = skip_Id(node->attr.filter.in_cg[n + 1]));
  } else if (__get_irn_opcode(node) == iro_Block && node->attr.block.in_cg) {
    return (node->attr.block.in_cg[n + 1] = skip_Id(node->attr.block.in_cg[n + 1]));
  }

  return __get_irn_intra_n (node, n);
}

/**
 * Access to the predecessors of a node.
 * To iterate over the operands iterate from 0 to i < get_irn_arity(),
 * to iterate including the Block predecessor iterate from i = -1 to
 * i < get_irn_arity.
 * If it is a block, the entry -1 is NULL.
 * Intern version for libFirm.
 */
static INLINE ir_node *
__get_irn_n (ir_node *node, int n) {
  assert(node); assert(-1 <= n && n < __get_irn_arity(node));
  if (interprocedural_view)  return __get_irn_inter_n (node, n);
  return __get_irn_intra_n (node, n);
}

/**
 * Gets the mode of a node.
 * Intern version for libFirm.
 */
static INLINE ir_mode *
__get_irn_mode (const ir_node *node)
{
  assert (node);
  return node->mode;
}

/**
 * Sets the mode of a node.
 * Intern version of libFirm.
 */
static INLINE void
__set_irn_mode (ir_node *node, ir_mode *mode)
{
  assert (node);
  node->mode = mode;
}

/**
 * Gets the visited counter of a node.
 * Intern version for libFirm.
 */
static INLINE unsigned long
__get_irn_visited (const ir_node *node)
{
  assert (node);
  return node->visited;
}

/**
 * Sets the visited counter of a node.
 * Intern version for libFirm.
 */
static INLINE void
__set_irn_visited (ir_node *node, unsigned long visited)
{
  assert (node);
  node->visited = visited;
}

/**
 * Mark a node as visited in a graph.
 * Intern version for libFirm.
 */
static INLINE void
__mark_irn_visited (ir_node *node) {
  assert (node);
  node->visited = current_ir_graph->visited;
}

/**
 * Returns non-zero if a node of was visited.
 * Intern version for libFirm.
 */
static INLINE int
__irn_visited(const ir_node *node) {
  assert (node);
  return (node->visited >= current_ir_graph->visited);
}

/**
 * Returns non-zero if a node of was NOT visited.
 * Intern version for libFirm.
 */
static INLINE int
__irn_not_visited(const ir_node *node) {
  assert (node);
  return (node->visited < current_ir_graph->visited);
}

/**
 * Sets the link of a node.
 * Intern version of libFirm.
 */
static INLINE void
__set_irn_link(ir_node *node, void *link) {
  assert (node);
  /* Link field is used for Phi construction and various optimizations
     in iropt. */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);

  node->link = link;
}

/**
 * Returns the link of a node.
 * Intern version of libFirm.
 */
static INLINE void *
__get_irn_link(const ir_node *node) {
  assert (node);
  return node->link;
}

/* this section MUST contain all inline functions */
#define is_ir_node(thing)          __is_ir_node(thing)
#define get_irn_intra_arity(node)  __get_irn_intra_arity(node)
#define get_irn_inter_arity(node)  __get_irn_inter_arity(node)
#define get_irn_arity(node)        __get_irn_arity(node)
#define get_irn_intra_n(node, n)   __get_irn_intra_n(node, n)
#define get_irn_inter_n(node, n)   __get_irn_inter_n(node, n)
#define get_irn_n(node, n)         __get_irn_n(node, n)
#define get_irn_mode(node)         __get_irn_mode(node)
#define set_irn_mode(node, mode)   __set_irn_mode(node, mode)
#define get_irn_op(node)           __get_irn_op(node)
#define get_irn_opcode(node)       __get_irn_opcode(node)
#define get_irn_visited(node)      __get_irn_visited(node)
#define set_irn_visited(node, v)   __set_irn_visited(node, v)
#define mark_irn_visited(node)     __mark_irn_visited(node)
#define irn_visited(node)          __irn_visited(node)
#define irn_not_visited(node)      __irn_not_visited(node)
#define set_irn_link(node, link)   __set_irn_link(node, link)
#define get_irn_link(node)         __get_irn_link(node)

# endif /* _IRNODE_T_H_ */
