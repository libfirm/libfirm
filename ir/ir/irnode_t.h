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
# include "firm_common_t.h"
# include "irdom_t.h" /* For size of struct dom_info. */
# include "dbginfo.h"

# include "exc.h"

/** ir node attributes **/

/** Block attributes */
typedef struct {
  unsigned long block_visited;  /**< for the walker that walks over all blocks. */
  /* Attributes private to construction: */
  bool matured;               /**< if set, all in-nodes of the block are fixed */
  struct ir_node **graph_arr; /**< array to store all parameters */
  struct dom_info dom;        /**< Datastructure that holds information about dominators.
				 @todo
           Eventually overlay with graph_arr as only valid
				   in different phases.  Eventually inline the whole
				   datastructure. */
  exc_t exc;		      /**< role of this block for exception handling */
  ir_node *handler_entry;     /**< handler entry block iff this block is part of a region */
  ir_node ** in_cg;           /**< array with predecessors in
			       * interprocedural_view, if they differ
			       * from intraprocedural predecessors */
  int *backedge;              /**< Field n set to true if pred n is backedge.
			         @todo Ev. replace by bitfield! */
  int *cg_backedge;           /**< Field n set to true if pred n is interprocedural backedge.
			         @todo Ev. replace by bitfield! */
} block_attr;

/** Start attributes */
typedef struct {
  ir_graph *irg;
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

/** SymConst attributes
    This union contains the symbolic information represented by the node */
typedef union type_or_id {
  type  *typ;
  ident *ptrinfo;
} type_or_id;

typedef struct {
  type_or_id tori;
  symconst_kind num;
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
  ir_graph * irg;            /**< ir_graph this node belongs to (for
			      * navigating in interprocedural graphs) */
} end_attr;

/** CallBegin attributes */
typedef struct {
  ir_graph * irg;            /**< ir_graph this node belongs to (for
			      * navigating in interprocedural graphs) */
  ir_node * call;            /**< associated Call-operation */
} callbegin_attr;

/** Cast attributes */
typedef struct {
  type *totype;
} cast_attr;

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
  io_attr	 io;	/**< For InstOf */
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
  int node_nr;             /**< a unique node number for each node to make output
			      readable. */
#endif
  attr attr;               /**< attribute of this node. Depends on opcode.
                              Must be last field of struct ir_node. */
};

/** Copies all attributes stored in the old node  to the new node.
    Assumes both have the same opcode and sufficient size. */
void
copy_attrs (ir_node *old, ir_node *new);


/** Returns the array with the ins.  The content of the array may not be
   changed.  */
ir_node     **get_irn_in            (const ir_node *node);

/*@{*/
/** access attributes directly */
INLINE const_attr    get_irn_const_attr    (ir_node *node);
INLINE long          get_irn_proj_attr     (ir_node *node);
INLINE alloc_attr    get_irn_alloc_attr    (ir_node *node);
INLINE type         *get_irn_free_attr     (ir_node *node);
INLINE symconst_attr get_irn_symconst_attr (ir_node *node);
type         *get_irn_call_attr     (ir_node *node);
sel_attr      get_irn_sel_attr      (ir_node *node);
int           get_irn_phi_attr      (ir_node *node);
block_attr    get_irn_block_attr   (ir_node *node);
/*@}*/

# endif /* _IRNODE_T_H_ */
