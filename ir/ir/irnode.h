

/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** declarations of an ir node
*/

# ifndef _IRNODE_H_
# define _IRNODE_H_

# include "irgraph.h"
# include "entity.h"
# include "common.h"
# include "irop.h"
# include "irmode.h"
# include "tv.h"
# include "type.h"

/* projection numbers of compare. */
enum {
  False,		/* false */
  Eq,			/* equal */
  Lt,			/* less */
  Le,			/* less or equal */
  Gt,			/* greater */
  Ge,			/* greater or equal */
  Lg,			/* less or greater */
  Leg,			/* less, equal or greater = ordered */
  Uo,			/* unordered */
  Ue,			/* unordered or equal */
  Ul,			/* unordered or less */
  Ule,			/* unordered, less or equal */
  Ug,			/* unordered or greater */
  Uge,			/* unordered, greater or equal */
  Ne,			/* unordered, less or greater = not equal */
  True,		        /* true */
  not_mask = Leg	/* bits to flip to negate comparison */
} pnc_number;

inline char *
get_pnc_string(int pnc);

int
get_negated_pnc(int pnc);


/* projection numbers for result of Start node. */
enum {
  pns_initial_exec,     /* Projection on an executable, the initial control
			   flow. */
  pns_global_store,     /* Projection on the global store */
  pns_frame_base,       /* Projection on the frame base */
  pns_globals,          /* Projection on the pointer to the data segment
			   containing _all_ global entities. */
  pns_args              /* Projection on all arguments */
} pns_number;

/* ir node attributes */
typedef struct {
  unsigned long block_visit;  /* for the walker that walks over all blocks. */
  /* Attributes private to construction: */
  bool matured;               /* if set, all in-nodes of the block are fixed */
  struct ir_node **graph_arr; /* array to store all parameters */
} block_attr;

typedef enum {
  type_tag,          /* The SymConst is a type tag for the given type.
			Type_or_id is type */
  size,              /* The SymConst is the size of the given type.
			Type_or_id is type */
  linkage_ptr_info   /* The SymConst is a symbolic pointer to be filled in
			by the linker. Type_or_id is ident */
} symconst_kind;

typedef union {
  type *typ;
  ident *ptrinfo;
} type_or_id;

typedef struct {
  type_or_id tori;
  symconst_kind num;
} symconst_attr;

typedef enum {
  static_linkage,       /* entity is used internal and not visible out of this
			   file/class. */
  external_linkage,     /* */
  no_linkage
} linkage_type;

typedef struct {
  entity *ent;          /* entity to select */
  linkage_type *ltyp;   /* linkage type of the entity */
} sel_attr;

typedef enum {
  stack_alloc,          /* Alloc allocates the object on the stack. */
  heap_alloc            /* lloc allocates the object on the heap. */
} where_alloc;

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
  unsigned long visit;     /* visited counter for walks of the graph */
  struct ir_node **in;     /* array with predecessors / operands */
  struct ir_node *link;    /* for linking nodes somehow to a list, e.g.
                              used while construction to link Phi0 nodes and
			      during optimization to link to nodes that
			      shall replace a node. */
  attr attr;               /* attribute of this node. Depends on opcode. */
                           /* Must be last attribute of struct ir_node. */
} ;


/* The typedefiniton of ir_node is also in irgraph.h to resolve
   recursion between irnode.h and irgraph.h */
#ifndef _IR_NODE_TYPEDEF_
#define _IR_NODE_TYPEDEF_
typedef struct ir_node ir_node;
#endif

/* irnode constructor                                             */
/* Create a new irnode in irg, with an op, mode, arity and        */
/* some incoming irnodes.                                         */
/* If arity is negative, a node with a dynamic array is created.  */

inline ir_node *
new_ir_node (ir_graph *irg,
	     ir_node *block,
	     ir_op *op,
	     ir_mode *mode,
	     int arity,
	     ir_node **in);


/* Print IR-Nodes with attributes */
/* @@@@ brauchen wir dienoch? dann fliegt ev. das xprint raus?*/
int ir_node_print (XP_PAR1, const xprintf_info *, XP_PARN);


/** manipulate the fields of ir_node **/

/* returns the number of predecessors without the block predecessor: */
int                  get_irn_arity         (ir_node *node);
/* returns the array with the ins: */
inline ir_node     **get_irn_in            (ir_node *node);
/* to iterate through the predecessors without touching the array. No
   order of predecessors guaranteed.
   To iterate over the operands iterate from 0 to i < get_irn_arity(),
   to iterate includind the Block predecessor iterate from i = -1 to
   i < get_irn_arity. */
inline ir_node      *get_irn_n             (ir_node *node, int n);
inline void          set_irn_n             (ir_node *node, int n, ir_node *in);
inline ir_mode      *get_irn_mode          (ir_node *node);
inline modecode      get_irn_modecode      (ir_node *node);
inline ir_op        *get_irn_op            (ir_node *node);
/* should be private to the library: */
inline void          set_irn_op            (ir_node *node, ir_op *op);
inline opcode        get_irn_opcode        (ir_node *node);
inline void          set_irn_link          (ir_node *node, ir_node *link);
inline ir_node      *get_irn_link          (ir_node *node);


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

/** manipulate fields of individual nodes **/

/* this works for all except Block */
inline ir_node *get_nodes_Block (ir_node *node);
inline void     set_nodes_Block (ir_node *node, ir_node *block);

int             get_Block_n_cfgpreds (ir_node *node);
/* inline void    set_Block_n_cfgpreds (ir_node *node, int n_preds); */
inline ir_node *get_Block_cfgpred (ir_node *node, int pos);
inline void     set_Block_cfgpred (ir_node *node, int pos, ir_node *pred);
inline bool     get_Block_matured (ir_node *node);
inline void     set_Block_matured (ir_node *node, bool matured);
inline bool     get_Block_closed (ir_node *node);
inline void     set_Block_closed (ir_node *node, bool closed);
inline unsigned long get_Block_block_visit (ir_node *node);
inline void     set_Block_block_visit (ir_node *node, unsigned long visit);
inline ir_node *get_Block_graph_arr (ir_node *node, int pos);
inline void     set_Block_graph_arr (ir_node *node, int pos, ir_node *value);

inline ir_node *get_Cond_selector (ir_node *node);
inline void     set_Cond_selector (ir_node *node, ir_node *selector);

inline ir_node *get_Return_mem (ir_node *node);
inline void     set_Return_mem (ir_node *node, ir_node *mem);
inline int      get_Return_n_res (ir_node *node);
/*inline void     set_Return_n_res (ir_node *node, int results); */
inline ir_node *get_Return_res (ir_node *node, int pos);
inline void     set_Return_res (ir_node *node, int pos, ir_node *res);

inline ir_node *get_Raise_mem (ir_node *node);
inline void     set_Raise_mem (ir_node *node, ir_node *mem);
inline ir_node *get_Raise_exoptr (ir_node *node);  /* PoinTeR to EXception Object */
inline void     set_Raise_exoptr (ir_node *node, ir_node *exoptr);

inline tarval  *get_Const_tarval (ir_node *node);
inline void     set_Const_tarval (ir_node *node, tarval *con);

inline symconst_kind get_SymConst_kind (ir_node *node);
inline void          set_SymConst_kind (ir_node *node, symconst_kind num);
inline type    *get_SymConst_type (ir_node *node);
inline void     set_SymConst_type (ir_node *node, type *type);
inline ident   *get_SymConst_ptrinfo (ir_node *node);
inline void     set_SymConst_ptrinfo (ir_node *node, ident *ptrinfo);

inline ir_node *get_Sel_mem (ir_node *node);
inline void     set_Sel_mem (ir_node *node, ir_node *mem);
inline ir_node *get_Sel_ptr (ir_node *node);  /* ptr to the object to select from */
inline void     set_Sel_ptr (ir_node *node, ir_node *ptr);
inline int      get_Sel_n_index (ir_node *node);
/*inline void     set_Sel_n_index (ir_node *node, int n_index); */
inline ir_node *get_Sel_index (ir_node *node, int pos);
inline void     set_Sel_index (ir_node *node, int pos, ir_node *index);
inline entity  *get_Sel_entity (ir_node *node); /* entity to select */
inline void     set_Sel_entity (ir_node *node, entity *ent);
inline linkage_type *get_Sel_linkage_type (ir_node *node);
inline void     set_Sel_linkage_type (ir_node *node, linkage_type *lt);

inline ir_node *get_Call_mem (ir_node *node);
inline void     set_Call_mem (ir_node *node, ir_node *mem);
inline ir_node *get_Call_ptr (ir_node *node);
inline void     set_Call_ptr (ir_node *node, ir_node *ptr);
inline int      get_Call_arity (ir_node *node);
/* inline void     set_Call_arity (ir_node *node, ir_node *arity); */
inline ir_node *get_Call_param (ir_node *node, int pos);
inline void     set_Call_param (ir_node *node, int pos, ir_node *param);
inline type_method *get_Call_type (ir_node *node);
inline void         set_Call_type (ir_node *node, type_method *type);

/* For unary and binary arithmetic operations the access to the
   operands can be factored out.  Left is the first, right the
   second arithmetic value  as listed in tech report 1999-44.
   unops are: Minus, Abs, Not, Conv
   binops are: Add, Sub, Mul, Quot, DivMod, Div, Mod, And, Or, Eor, Shl,
   Shr, Shrs, Rot, Cmp */
inline int      is_unop (ir_node *node);
inline ir_node *get_unop_op (ir_node *node);
inline void     set_unop_op (ir_node *node, ir_node *op);
inline int      is_binop (ir_node *node);
inline ir_node *get_binop_left (ir_node *node);
inline void     set_binop_left (ir_node *node, ir_node *left);
inline ir_node *get_binop_right (ir_node *node);
inline void     set_binop_right (ir_node *node, ir_node *right);

inline ir_node *get_Add_left (ir_node *node);
inline void     set_Add_left (ir_node *node, ir_node *left);
inline ir_node *get_Add_right (ir_node *node);
inline void     set_Add_right (ir_node *node, ir_node *right);

inline ir_node *get_Sub_left (ir_node *node);
inline void     set_Sub_left (ir_node *node, ir_node *left);
inline ir_node *get_Sub_right (ir_node *node);
inline void     set_Sub_right (ir_node *node, ir_node *right);

inline ir_node *get_Minus_op (ir_node *node);
inline void     set_Minus_op (ir_node *node, ir_node *op);

inline ir_node *get_Mul_left (ir_node *node);
inline void     set_Mul_left (ir_node *node, ir_node *left);
inline ir_node *get_Mul_right (ir_node *node);
inline void     set_Mul_right (ir_node *node, ir_node *right);

inline ir_node *get_Quot_left (ir_node *node);
inline void     set_Quot_left (ir_node *node, ir_node *left);
inline ir_node *get_Quot_right (ir_node *node);
inline void     set_Quot_right (ir_node *node, ir_node *right);
inline ir_node *get_Quot_mem (ir_node *node);
inline void     set_Quot_mem (ir_node *node, ir_node *mem);

inline ir_node *get_DivMod_left (ir_node *node);
inline void     set_DivMod_left (ir_node *node, ir_node *left);
inline ir_node *get_DivMod_right (ir_node *node);
inline void     set_DivMod_right (ir_node *node, ir_node *right);
inline ir_node *get_DivMod_mem (ir_node *node);
inline void     set_DivMod_mem (ir_node *node, ir_node *mem);

inline ir_node *get_Div_left (ir_node *node);
inline void     set_Div_left (ir_node *node, ir_node *left);
inline ir_node *get_Div_right (ir_node *node);
inline void     set_Div_right (ir_node *node, ir_node *right);
inline ir_node *get_Div_mem (ir_node *node);
inline void     set_Div_mem (ir_node *node, ir_node *mem);

inline ir_node *get_Mod_left (ir_node *node);
inline void     set_Mod_left (ir_node *node, ir_node *left);
inline ir_node *get_Mod_right (ir_node *node);
inline void     set_Mod_right (ir_node *node, ir_node *right);
inline ir_node *get_Mod_mem (ir_node *node);
inline void     set_Mod_mem (ir_node *node, ir_node *mem);

inline ir_node *get_Abs_op (ir_node *node);
inline void     set_Abs_op (ir_node *node, ir_node *op);

inline ir_node *get_And_left (ir_node *node);
inline void     set_And_left (ir_node *node, ir_node *left);
inline ir_node *get_And_right (ir_node *node);
inline void     set_And_right (ir_node *node, ir_node *right);

inline ir_node *get_Or_left (ir_node *node);
inline void     set_Or_left (ir_node *node, ir_node *left);
inline ir_node *get_Or_right (ir_node *node);
inline void     set_Or_right (ir_node *node, ir_node *right);

inline ir_node *get_Eor_left (ir_node *node);
inline void     set_Eor_left (ir_node *node, ir_node *left);
inline ir_node *get_Eor_right (ir_node *node);
inline void     set_Eor_right (ir_node *node, ir_node *right);

inline ir_node *get_Not_op (ir_node *node);
inline void     set_Not_op (ir_node *node, ir_node *op);

inline ir_node *get_Cmp_left (ir_node *node);
inline void     set_Cmp_left (ir_node *node, ir_node *left);
inline ir_node *get_Cmp_right (ir_node *node);
inline void     set_Cmp_right (ir_node *node, ir_node *right);

inline ir_node *get_Shl_left (ir_node *node);
inline void     set_Shl_left (ir_node *node, ir_node *left);
inline ir_node *get_Shl_right (ir_node *node);
inline void     set_Shl_right (ir_node *node, ir_node *right);

inline ir_node *get_Shr_left (ir_node *node);
inline void     set_Shr_left (ir_node *node, ir_node *left);
inline ir_node *get_Shr_right (ir_node *node);
inline void     set_Shr_right (ir_node *node, ir_node *right);

inline ir_node *get_Shrs_left (ir_node *node);
inline void     set_Shrs_left (ir_node *node, ir_node *left);
inline ir_node *get_Shrs_right (ir_node *node);
inline void     set_Shrs_right (ir_node *node, ir_node *right);

inline ir_node *get_Rot_left (ir_node *node);
inline void     set_Rot_left (ir_node *node, ir_node *left);
inline ir_node *get_Rot_right (ir_node *node);
inline void     set_Rot_right (ir_node *node, ir_node *right);

inline ir_node *get_Conv_op (ir_node *node);
inline void     set_Conv_op (ir_node *node, ir_node *op);

inline int      get_Phi_n_preds (ir_node *node);
/* inline void     set_Phi_n_preds (ir_node *node, int n_preds); */
inline ir_node *get_Phi_pred (ir_node *node, int pos);
inline void     set_Phi_pred (ir_node *node, int pos, ir_node *pred);

inline ir_node *get_Load_mem (ir_node *node);
inline void     set_Load_mem (ir_node *node, ir_node *mem);
inline ir_node *get_Load_ptr (ir_node *node);
inline void     set_Load_ptr (ir_node *node, ir_node *ptr);

inline ir_node *get_Store_mem (ir_node *node);
inline void     set_Store_mem (ir_node *node, ir_node *mem);
inline ir_node *get_Store_ptr (ir_node *node);
inline void     set_Store_ptr (ir_node *node, ir_node *ptr);
inline ir_node *get_Store_value (ir_node *node);
inline void     set_Store_value (ir_node *node, ir_node *value);

inline ir_node *get_Alloc_mem (ir_node *node);
inline void     set_Alloc_mem (ir_node *node, ir_node *mem);
inline ir_node *get_Alloc_size (ir_node *node);
inline void     set_Alloc_size (ir_node *node, ir_node *size);
inline type    *get_Alloc_type (ir_node *node);
inline void     set_Alloc_type (ir_node *node, type *type);
inline where_alloc  get_Alloc_where (ir_node *node);
inline void         set_Alloc_where (ir_node *node, where_alloc where);

inline ir_node *get_Free_mem (ir_node *node);
inline void     set_Free_mem (ir_node *node, ir_node *mem);
inline ir_node *get_Free_ptr (ir_node *node);
inline void     set_Free_ptr (ir_node *node, ir_node *ptr);
inline ir_node *get_Free_size (ir_node *node);
inline void     set_Free_size (ir_node *node, ir_node *size);
inline type    *get_Free_type (ir_node *node);
inline void     set_Free_type (ir_node *node, type *type);

inline int      get_Sync_n_preds (ir_node *node);
/* inline void     set_Sync_n_preds (ir_node *node, int n_preds); */
inline ir_node *get_Sync_pred (ir_node *node, int pos);
inline void     set_Sync_pred (ir_node *node, int pos, ir_node *pred);

inline ir_node *get_Proj_pred (ir_node *node);
inline void     set_Proj_pred (ir_node *node, ir_node *pred);
inline long     get_Proj_proj (ir_node *node);
inline void     set_Proj_proj (ir_node *node, long proj);

inline int      get_Tuple_n_preds (ir_node *node);
/* inline void     set_Tuple_n_preds (ir_node *node, int n_preds); */
inline ir_node *get_Tuple_pred (ir_node *node, int pos);
inline void     set_Tuple_pred (ir_node *node, int pos, ir_node *pred);

inline ir_node *get_Id_pred (ir_node *node);
inline void     set_Id_pred (ir_node *node, ir_node *pred);



/******************************************************************/
/*  Auxiliary routines                                            */
/******************************************************************/

/* returns operand of node if node is a Proj. */
inline ir_node *skip_Proj (ir_node *node);
/* returns operand of node if node is a Id */
inline ir_node *skip_nop  (ir_node *node);
/* returns true if node is a Bad node. */
inline int      is_Bad    (ir_node *node);
/* returns true if the node is not a Block */
inline int      is_no_Block (ir_node *node);
/* Returns true if the operation manipulates control flow:
   Start, End, Jmp, Cond, Return, Raise */
int is_cfop(ir_node *node);
/* Returns true if the operation can change the control flow because
   of a exception. */
int is_fragile_op(ir_node *node);

# endif /* _IRNODE_H_ */
