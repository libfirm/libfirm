/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** declarations of an ir node
*/

# ifndef _IRNODE_T_H_
# define _IRNODE_T_H_

# include "irnode.h"
# include "xprintf.h"

/** ir node attributes **/
/* Block attributes */
typedef struct {
  unsigned long block_visited;  /* for the walker that walks over all blocks. */
  /* Attributes private to construction: */
  bool matured;               /* if set, all in-nodes of the block are fixed */
  struct ir_node **graph_arr; /* array to store all parameters */
} block_attr;

/* SymConst attributes */
/*   This union contains the symbolic information represented by the node */
typedef union type_or_id {
  type *typ;
  ident *ptrinfo;
} type_or_id;

typedef struct {
  type_or_id tori;
  symconst_kind num;
} symconst_attr;

/* Sel attributes */
typedef struct {
  entity *ent;          /* entity to select */
  linkage_type ltyp;    /* linkage type of the entity */
} sel_attr;

/* Alloc attributes */
typedef struct {
  type *type;           /* Type of the allocated object.  */
  where_alloc where;    /* stack, heap or other managed part of memory */
} alloc_attr;

/* Some irnodes just have one attribute, these are stored here,
   some have more. Their name is 'irnodename_attr' */
typedef union {
  block_attr     block; /* For Block: Fields needed to construct it */
  struct tarval *con;   /* For Const: contains the value of the constant */
  symconst_attr  i;     /* For SymConst. */
  sel_attr       s;     /* For Sel. */
  type_method   *call;  /* For Call: pointer to the type of the method to call */
  long           proj;  /* For Proj: contains the result position to project */
  alloc_attr     a;     /* For Alloc. */
  type          *f;     /* For Free. */
  int            phi0_pos;  /* For Phi. Used to remember the value defined by
			       this Phi node.  Needed when the Phi is completed
			       to call get_r_internal_value to find the
			       predecessors. If this attribute is set, the Phi
			       node takes the role of the obsolete Phi0 node,
			       therefore the name. */
} attr;


/* common structure of an irnode */
/* if the node has some attributes, they are stored in attr */

struct ir_node {
  firm_kind kind;          /* distinguishes this node from others */
  ir_op *op;               /* Opcode of this node. */
  ir_mode *mode;           /* Mode of this node. */
  unsigned long visited;   /* visited counter for walks of the graph */
  struct ir_node **in;     /* array with predecessors / operands */
  struct ir_node *link;    /* for linking nodes somehow to a list, e.g.
                              used while construction to link Phi0 nodes and
			      during optimization to link to nodes that
			      shall replace a node. */
#ifdef DEBUG_libfirm
  int node_nr;             /* a unique node number for each node to make output
			      readable. */
#endif
  attr attr;               /* attribute of this node. Depends on opcode. */
                           /* Must be last field of struct ir_node. */
};

/* Copies all attributes stored in the old node  to the new node.
   Assumes both have the same opcode and sufficient size. */
void
copy_attrs (ir_node *old, ir_node *new);


/* Print IR-Nodes with attributes */
/* @@@@ brauchen wir dienoch? dann fliegt ev. das xprint raus?*/
int ir_node_print (XP_PAR1, const xprintf_info *, XP_PARN);


/** access attributes directly **/
inline tarval       *get_irn_const_attr    (ir_node *node);
inline long          get_irn_proj_attr     (ir_node *node);
inline alloc_attr    get_irn_alloc_attr    (ir_node *node);
inline type         *get_irn_free_attr     (ir_node *node);
inline symconst_attr get_irn_symconst_attr (ir_node *node);
type_method  *get_irn_call_attr     (ir_node *node);
sel_attr      get_irn_sel_attr      (ir_node *node);
int           get_irn_phi_attr      (ir_node *node);
block_attr    get_irn_return_attr   (ir_node *node);

# endif /* _IRNODE_T_H_ */
