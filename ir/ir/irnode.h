/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe      3 2002/02/28 13:33:52
 * All rights reserved.
 */

/* $Id$ */

# ifndef _IRNODE_H_
# define _IRNODE_H_

/* Projection numbers of compare: use for Proj nodes! */
typedef enum {
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
  True		        /* true */
  /* not_mask = Leg*/	/* bits to flip to negate comparison * @@ hack for jni interface */
} pnc_number;
#define not_mask Leg

# include "tv.h"
# include "irgraph.h"
# include "entity.h"
# include "firm_common.h"
# include "irop.h"
# include "irmode.h"
# include "type.h"
# include "dbginfo.h"
# include "exc.h"

/**
 * @file irnode.h
 *
 * @author Martin Trapp, Christian Schaefer
 *
 * Declarations of an ir node.
 */

/**
 * @defgroup ir_node Declarations of an ir node.
 *
 * The type definiton of ir_node is also in irgraph.h to resolve
 *  recursion between irnode.h and irgraph.h
 *
 * ir_node - a datatype representing a Firm node
 *
 *  The common fields are:
 *
 *  - firm_kind - A firm_kind tag containing k_type.  This is useful
 *                for dynamically checking whether a node is a ir_node.
 *  - arity     - The number of predecessors in the Firm graph.
 *  - in        - A list with the predecessors in the Firm graph.  There are
 *                routines to access individual elements and to obtain the
 *                array.  The method returning the array should not be used.
 *  - mode      - The mode of the node.  There are routines to get the mode
 *                but also to access the mode's fields directly.
 *  - opcode    - The opcode of the node. There are routines to get the opcode
 *                but also to access the opcode's fields directly.
 *  - node_nr   - A unique number for the node.  Available only if debugging
 *                is turned on.
 * @{
 */

#ifndef _IR_NODE_TYPEDEF_
#define _IR_NODE_TYPEDEF_
typedef struct ir_node ir_node;
#endif

/**
 *   you can work on the graph without considering the different types
 *   of nodes, it's just a big graph.
 */

/** returns the number of predecessors without the block predecessor: */
int                  get_irn_arity         (const ir_node *node);

/** Replaces the old in array by a new one that will contain the ins given in
   the parameters.  Conserves the block predecessor.  It copies the array passed.
   This function is necessary to ajust in arrays of blocks, calls and phis.
   Assumes that current_ir_graph is set to the graph containing "node".
   "in" must contain all predecessors except the block that are required for
   the nodes opcode. */
INLINE void          set_irn_in            (ir_node *node, int arity,
					    ir_node *in[]);
/* to iterate through the predecessors without touching the array. No
   order of predecessors guaranteed.
   To iterate over the operands iterate from 0 to i < get_irn_arity(),
   to iterate including the Block predecessor iterate from i = -1 to
   i < get_irn_arity. */
/* Access predecessor n */
/* get_irn_n removes Id predecessors. */
INLINE ir_node      *get_irn_n             (ir_node *node, int n);
INLINE void          set_irn_n             (ir_node *node, int n, ir_node *in);
/* Get the mode struct. */
INLINE ir_mode      *get_irn_mode          (const ir_node *node);
/* Get the mode-enum modecode */
INLINE modecode      get_irn_modecode      (const ir_node *node);
/* Get the ident for a string representation of the mode */
INLINE ident        *get_irn_modeident     (const ir_node *node);
/* Access the opcode struct of the node */
INLINE ir_op        *get_irn_op            (const ir_node *node);
INLINE void          set_irn_op            (ir_node *node, ir_op *op);
/* Get the opcode-enum of the node */
INLINE opcode        get_irn_opcode        (const ir_node *node);
/* Get the string representation of the opcode */
INLINE const char   *get_irn_opname        (const ir_node *node);
/* Get the ident for a string representation of the opcode */
INLINE ident        *get_irn_opident       (const ir_node *node);
INLINE unsigned long get_irn_visited (const ir_node *node);
INLINE void          set_irn_visited (ir_node *node, unsigned long visited);
/* Sets visited to get_irg_visited(current_ir_graph) */
INLINE void          mark_irn_visited (ir_node *node);
/* Returns 1 if visited < get_irg_visited(current_ir_graph).  */
INLINE int           irn_not_visited  (const ir_node *node);
/* Returns 1 if visited >= get_irg_visited(current_ir_graph).  */
INLINE int           irn_visited      (const ir_node *node);
INLINE void          set_irn_link          (ir_node *node, void *link);
INLINE void         *get_irn_link          (const ir_node *node);

/** Outputs a unique number for this node if libfirm is compiled for
   debugging, (configure with --enable-debug) else returns 0. */
INLINE long get_irn_node_nr(const ir_node *node);

/** Returns the ir_graph this node belongs to. Only valid for
 * CallBegin, EndReg and EndExcept */
INLINE ir_graph *get_irn_irg(ir_node *node);

/**
 * irnode constructor.
 * Create a new irnode in irg, with an op, mode, arity and
 * some incoming irnodes.
 * If arity is negative, a node with a dynamic array is created.
 */
INLINE ir_node *
new_ir_node (dbg_info *db,
	     ir_graph *irg,
	     ir_node *block,
	     ir_op *op,
	     ir_mode *mode,
	     int arity,
	     ir_node *in[]);

/*
 *
 * NAME access functions for node fields.
 *
 *  Not properly documented ;-)
 *
 */

/* this works for all except Block */
INLINE ir_node  *get_nodes_Block (ir_node *node);
INLINE void      set_nodes_Block (ir_node *node, ir_node *block);

/** Projection numbers for result of Start node: use for Proj nodes! */
typedef enum {
  pns_initial_exec,     /**< Projection on an executable, the initial control
			   flow. */
  pns_global_store,     /**< Projection on the global store */
  pns_frame_base,       /**< Projection on the frame base */
  pns_globals,          /**< Projection on the pointer to the data segment
			   containing _all_ global entities. */
  pns_args              /**< Projection on all arguments */
} pns_number;

/* @@@ no more supported  */
INLINE ir_node **get_Block_cfgpred_arr (ir_node *node);
int              get_Block_n_cfgpreds (ir_node *node);
INLINE ir_node  *get_Block_cfgpred (ir_node *node, int pos);
INLINE void      set_Block_cfgpred (ir_node *node, int pos, ir_node *pred);
INLINE bool      get_Block_matured (ir_node *node);
INLINE void      set_Block_matured (ir_node *node, bool matured);
INLINE unsigned long get_Block_block_visited (ir_node *node);
INLINE void      set_Block_block_visited (ir_node *node, unsigned long visit);
/* For this current_ir_graph must be set. */
INLINE void      mark_Block_block_visited(ir_node *node);
INLINE int       Block_not_block_visited(ir_node *node);

/* Set and remove interprocedural predecessors. If the interprocedural
 * predecessors are removed, the node has the same predecessors in
 * both views.
 * @@@ Maybe better:  arity is zero if no cg preds. */
void set_Block_cg_cfgpred_arr(ir_node * node, int arity, ir_node ** in);
void set_Block_cg_cfgpred(ir_node * node, int pos, ir_node * pred);
/* @@@ not supported */
ir_node ** get_Block_cg_cfgpred_arr(ir_node * node);
/* Returns the number of interproc predecessors.  0 if none. */
int get_Block_cg_n_cfgpreds(ir_node * node);
ir_node * get_Block_cg_cfgpred(ir_node * node, int pos);
/* frees the memory. */
void remove_Block_cg_cfgpred_arr(ir_node * node);

INLINE int  get_End_n_keepalives(ir_node *end);
INLINE ir_node *get_End_keepalive(ir_node *end, int pos);
INLINE void add_End_keepalive (ir_node *end, ir_node *ka);
INLINE void set_End_keepalive(ir_node *end, int pos, ir_node *ka);
/* Some parts of the End node are allocated seperately -- their memory
   is not recovered by dead_node_elimination if a End node is dead.
   free_End frees these data structures. */
INLINE void free_End (ir_node *end);

ir_graph *get_EndReg_irg (const ir_node *end);
ir_graph *get_EndExcept_irg  (const ir_node *end);

/* We distinguish three kinds of Cond nodes.  These can be distinguished
   by the mode of the selector operand and an internal flag of type cond_kind.
   First we distinguish binary Conds and switch Conds.
   A binary Cond has as selector a boolean value.  Proj(0) projects the control
   flow for case "False", Proj(1) the control flow for "True".  A binary Cond
   is recognized by the boolean selector.
   The switch Cond has as selector an unsigned integer.  It produces as result
   an n+1 Tuple (cf0, ... , cfn) of control flows.
   We differ two flavours of this Cond.  The first, the dense Cond, passes
   control along output i if the selector value is i, 0 <= i <= n.  If the
   selector value is >n it passes control along output n.
   The second Cond flavor differes in the treatment of cases not specified in
   the source program.  It magically knows about the existence of Proj nodes.
   It only passes control along output i, 0 <= i <= n, if a node Proj(Cond, i)
   exists.  Else it passes control along output n (even if this Proj does not
   exist.)  This Cond we call "fragmentary".  There is a special constructor
   new_defaultProj that automatically sets the flavor.
   The two switch flavors are distinguished by a flag of type cond_kind.
   Default flavor is "dense"
*/
typedef enum {
  dense,        /**< Default. Missing Proj nodes are dead control flow. */
  fragmentary   /**< Special. No control flow optimizations allowed.  Missing
		   Proj nodes mean default control flow, i.e., Proj(n). */
} cond_kind;

INLINE ir_node  *get_Cond_selector (ir_node *node);
INLINE void      set_Cond_selector (ir_node *node, ir_node *selector);
INLINE cond_kind get_Cond_kind (ir_node *node);
INLINE void      set_Cond_kind (ir_node *node, cond_kind kind);

INLINE ir_node  *get_Return_mem (ir_node *node);
INLINE void      set_Return_mem (ir_node *node, ir_node *mem);
INLINE ir_node **get_Return_res_arr (ir_node *node);
INLINE int       get_Return_n_ress (ir_node *node);
INLINE ir_node  *get_Return_res (ir_node *node, int pos);
INLINE void      set_Return_res (ir_node *node, int pos, ir_node *res);

INLINE ir_node *get_Raise_mem (ir_node *node);
INLINE void     set_Raise_mem (ir_node *node, ir_node *mem);
INLINE ir_node *get_Raise_exo_ptr (ir_node *node);  /* PoinTeR to EXception Object */
INLINE void     set_Raise_exo_ptr (ir_node *node, ir_node *exoptr);

INLINE tarval  *get_Const_tarval (ir_node *node);
INLINE void     set_Const_tarval (ir_node *node, tarval *con);

/**  This enum names the three different kinds of symbolic Constants
     represented by SymConst.  The content of the attribute type_or_id
     depends on this tag.  Use the proper access routine after testing
     this flag. */
typedef enum {
  type_tag,          /**< The SymConst is a type tag for the given type.
			Type_or_id_p is type *. */
  size,              /**< The SymConst is the size of the given type.
			Type_or_id_p is type *. */
  linkage_ptr_info   /**< The SymConst is a symbolic pointer to be filled in
			by the linker. Type_or_id_p is ident *. */
} symconst_kind;
typedef union type_or_id * type_or_id_p;
INLINE symconst_kind get_SymConst_kind (const ir_node *node);
INLINE void          set_SymConst_kind (ir_node *node, symconst_kind num);
/* Only to access SymConst of kind type_tag or size.  Else assertion: */
INLINE type    *get_SymConst_type (ir_node *node);
INLINE void     set_SymConst_type (ir_node *node, type *tp);
/* Only to access SymConst of kind linkage_ptr_info.  Else assertion: */
INLINE ident   *get_SymConst_ptrinfo (ir_node *node);
INLINE void     set_SymConst_ptrinfo (ir_node *node, ident *ptrinfo);
/* Sets both: type and ptrinfo.  Needed to treat the node independent of
   its semantics.  Does a memcpy for the memory tori points to. */
INLINE type_or_id_p get_SymConst_type_or_id (ir_node *node);
INLINE void set_SymConst_type_or_id (ir_node *node, type_or_id_p tori);

INLINE ir_node *get_Sel_mem (ir_node *node);
INLINE void     set_Sel_mem (ir_node *node, ir_node *mem);
INLINE ir_node *get_Sel_ptr (ir_node *node);  /* ptr to the object to select from */
INLINE void     set_Sel_ptr (ir_node *node, ir_node *ptr);
INLINE ir_node **get_Sel_index_arr (ir_node *node);
INLINE int      get_Sel_n_indexs (ir_node *node);
INLINE ir_node *get_Sel_index (ir_node *node, int pos);
INLINE void     set_Sel_index (ir_node *node, int pos, ir_node *index);
INLINE entity  *get_Sel_entity (ir_node *node); /* entity to select */
INLINE void     set_Sel_entity (ir_node *node, entity *ent);

/* @@@ ajacs specific node -- not supported */
type           *get_InstOf_ent   (ir_node *node);
void            set_InstOf_ent   (ir_node *node, type *ent);
ir_node        *get_InstOf_obj   (ir_node *node);
void            set_InstOf_obj   (ir_node *node, ir_node *obj);
ir_node        *get_InstOf_store (ir_node *node);
void            set_InstOf_store (ir_node *node, ir_node *obj);

INLINE ir_node *get_Call_mem (ir_node *node);
INLINE void     set_Call_mem (ir_node *node, ir_node *mem);
INLINE ir_node *get_Call_ptr (ir_node *node);
INLINE void     set_Call_ptr (ir_node *node, ir_node *ptr);
INLINE ir_node **get_Call_param_arr (ir_node *node);
INLINE int      get_Call_n_params (ir_node *node);
INLINE ir_node *get_Call_param (ir_node *node, int pos);
INLINE void     set_Call_param (ir_node *node, int pos, ir_node *param);
INLINE type    *get_Call_type (ir_node *node);
INLINE void     set_Call_type (ir_node *node, type *tp);
INLINE int      get_Call_arity (ir_node *node);

/* Set, get and remove the callee-analysis. */
int get_Call_n_callees(ir_node * node);
entity * get_Call_callee(ir_node * node, int pos);
void set_Call_callee_arr(ir_node * node, int n, entity ** arr);
void remove_Call_callee_arr(ir_node * node);

ir_node * get_CallBegin_ptr (ir_node *node);
void set_CallBegin_ptr (ir_node *node, ir_node *ptr);
ir_graph *get_CallBegin_irg (ir_node *node);
ir_node *get_CallBegin_call (ir_node *node);
void set_CallBegin_call (ir_node *node, ir_node *call);

/* For unary and binary arithmetic operations the access to the
   operands can be factored out.  Left is the first, right the
   second arithmetic value  as listed in tech report 1999-44.
   unops are: Minus, Abs, Not, Conv
   binops are: Add, Sub, Mul, Quot, DivMod, Div, Mod, And, Or, Eor, Shl,
   Shr, Shrs, Rot, Cmp */
INLINE int      is_unop (ir_node *node);
INLINE ir_node *get_unop_op (ir_node *node);
INLINE void     set_unop_op (ir_node *node, ir_node *op);
INLINE int      is_binop (ir_node *node);
INLINE ir_node *get_binop_left (ir_node *node);
INLINE void     set_binop_left (ir_node *node, ir_node *left);
INLINE ir_node *get_binop_right (ir_node *node);
INLINE void     set_binop_right (ir_node *node, ir_node *right);

INLINE ir_node *get_Add_left (ir_node *node);
INLINE void     set_Add_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Add_right (ir_node *node);
INLINE void     set_Add_right (ir_node *node, ir_node *right);

INLINE ir_node *get_Sub_left (ir_node *node);
INLINE void     set_Sub_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Sub_right (ir_node *node);
INLINE void     set_Sub_right (ir_node *node, ir_node *right);

INLINE ir_node *get_Minus_op (ir_node *node);
INLINE void     set_Minus_op (ir_node *node, ir_node *op);

INLINE ir_node *get_Mul_left (ir_node *node);
INLINE void     set_Mul_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Mul_right (ir_node *node);
INLINE void     set_Mul_right (ir_node *node, ir_node *right);

INLINE ir_node *get_Quot_left (ir_node *node);
INLINE void     set_Quot_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Quot_right (ir_node *node);
INLINE void     set_Quot_right (ir_node *node, ir_node *right);
INLINE ir_node *get_Quot_mem (ir_node *node);
INLINE void     set_Quot_mem (ir_node *node, ir_node *mem);

INLINE ir_node *get_DivMod_left (ir_node *node);
INLINE void     set_DivMod_left (ir_node *node, ir_node *left);
INLINE ir_node *get_DivMod_right (ir_node *node);
INLINE void     set_DivMod_right (ir_node *node, ir_node *right);
INLINE ir_node *get_DivMod_mem (ir_node *node);
INLINE void     set_DivMod_mem (ir_node *node, ir_node *mem);

INLINE ir_node *get_Div_left (ir_node *node);
INLINE void     set_Div_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Div_right (ir_node *node);
INLINE void     set_Div_right (ir_node *node, ir_node *right);
INLINE ir_node *get_Div_mem (ir_node *node);
INLINE void     set_Div_mem (ir_node *node, ir_node *mem);

INLINE ir_node *get_Mod_left (ir_node *node);
INLINE void     set_Mod_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Mod_right (ir_node *node);
INLINE void     set_Mod_right (ir_node *node, ir_node *right);
INLINE ir_node *get_Mod_mem (ir_node *node);
INLINE void     set_Mod_mem (ir_node *node, ir_node *mem);

INLINE ir_node *get_Abs_op (ir_node *node);
INLINE void     set_Abs_op (ir_node *node, ir_node *op);

INLINE ir_node *get_And_left (ir_node *node);
INLINE void     set_And_left (ir_node *node, ir_node *left);
INLINE ir_node *get_And_right (ir_node *node);
INLINE void     set_And_right (ir_node *node, ir_node *right);

INLINE ir_node *get_Or_left (ir_node *node);
INLINE void     set_Or_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Or_right (ir_node *node);
INLINE void     set_Or_right (ir_node *node, ir_node *right);

INLINE ir_node *get_Eor_left (ir_node *node);
INLINE void     set_Eor_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Eor_right (ir_node *node);
INLINE void     set_Eor_right (ir_node *node, ir_node *right);

INLINE ir_node *get_Not_op (ir_node *node);
INLINE void     set_Not_op (ir_node *node, ir_node *op);

INLINE const char *get_pnc_string(int pnc);

INLINE int   get_negated_pnc(int pnc);
INLINE ir_node *get_Cmp_left (ir_node *node);
INLINE void     set_Cmp_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Cmp_right (ir_node *node);
INLINE void     set_Cmp_right (ir_node *node, ir_node *right);

INLINE ir_node *get_Shl_left (ir_node *node);
INLINE void     set_Shl_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Shl_right (ir_node *node);
INLINE void     set_Shl_right (ir_node *node, ir_node *right);

INLINE ir_node *get_Shr_left (ir_node *node);
INLINE void     set_Shr_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Shr_right (ir_node *node);
INLINE void     set_Shr_right (ir_node *node, ir_node *right);

INLINE ir_node *get_Shrs_left (ir_node *node);
INLINE void     set_Shrs_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Shrs_right (ir_node *node);
INLINE void     set_Shrs_right (ir_node *node, ir_node *right);

INLINE ir_node *get_Rot_left (ir_node *node);
INLINE void     set_Rot_left (ir_node *node, ir_node *left);
INLINE ir_node *get_Rot_right (ir_node *node);
INLINE void     set_Rot_right (ir_node *node, ir_node *right);

INLINE ir_node *get_Conv_op (ir_node *node);
INLINE void     set_Conv_op (ir_node *node, ir_node *op);

INLINE ir_node **get_Phi_preds_arr (ir_node *node);
INLINE int       get_Phi_n_preds (ir_node *node);
INLINE ir_node  *get_Phi_pred (ir_node *node, int pos);
INLINE void      set_Phi_pred (ir_node *node, int pos, ir_node *pred);

INLINE ir_node  *get_Filter_pred(ir_node *node);
INLINE void      set_Filter_pred(ir_node *node, ir_node *pred);
INLINE long      get_Filter_proj(ir_node *node);
INLINE void      set_Filter_proj(ir_node *node, long proj);
/* set the interprocedural predecessors, ...d_arr uses current_ir_graph.
 * @@@ Maybe better:  arity is zero if no cg preds. */
void             set_Filter_cg_pred_arr(ir_node * node, int arity, ir_node ** in);
void             set_Filter_cg_pred(ir_node * node, int pos, ir_node * pred);
int              get_Filter_n_cg_preds(ir_node *node);
ir_node *        get_Filter_cg_pred(ir_node *node, int pos);

INLINE ir_node *get_Load_mem (ir_node *node);
INLINE void     set_Load_mem (ir_node *node, ir_node *mem);
INLINE ir_node *get_Load_ptr (ir_node *node);
INLINE void     set_Load_ptr (ir_node *node, ir_node *ptr);

INLINE ir_node *get_Store_mem (ir_node *node);
INLINE void     set_Store_mem (ir_node *node, ir_node *mem);
INLINE ir_node *get_Store_ptr (ir_node *node);
INLINE void     set_Store_ptr (ir_node *node, ir_node *ptr);
INLINE ir_node *get_Store_value (ir_node *node);
INLINE void     set_Store_value (ir_node *node, ir_node *value);

INLINE ir_node *get_Alloc_mem (ir_node *node);
INLINE void     set_Alloc_mem (ir_node *node, ir_node *mem);
INLINE ir_node *get_Alloc_size (ir_node *node);
INLINE void     set_Alloc_size (ir_node *node, ir_node *size);
INLINE type    *get_Alloc_type (ir_node *node);
INLINE void     set_Alloc_type (ir_node *node, type *tp);

/** allocation place. */
typedef enum {
  stack_alloc,          /**< Alloc allocates the object on the stack. */
  heap_alloc            /**< Alloc allocates the object on the heap. */
} where_alloc;

INLINE where_alloc  get_Alloc_where (ir_node *node);
INLINE void         set_Alloc_where (ir_node *node, where_alloc where);

INLINE ir_node *get_Free_mem (ir_node *node);
INLINE void     set_Free_mem (ir_node *node, ir_node *mem);
INLINE ir_node *get_Free_ptr (ir_node *node);
INLINE void     set_Free_ptr (ir_node *node, ir_node *ptr);
INLINE ir_node *get_Free_size (ir_node *node);
INLINE void     set_Free_size (ir_node *node, ir_node *size);
INLINE type    *get_Free_type (ir_node *node);
INLINE void     set_Free_type (ir_node *node, type *tp);

INLINE ir_node **get_Sync_preds_arr (ir_node *node);
INLINE int       get_Sync_n_preds (ir_node *node);
INLINE ir_node  *get_Sync_pred (ir_node *node, int pos);
INLINE void      set_Sync_pred (ir_node *node, int pos, ir_node *pred);

INLINE ir_node  *get_Proj_pred (ir_node *node);
INLINE void      set_Proj_pred (ir_node *node, ir_node *pred);
INLINE long      get_Proj_proj (ir_node *node);
INLINE void      set_Proj_proj (ir_node *node, long proj);

INLINE ir_node **get_Tuple_preds_arr (ir_node *node);
INLINE int       get_Tuple_n_preds (ir_node *node);
INLINE ir_node  *get_Tuple_pred (ir_node *node, int pos);
INLINE void      set_Tuple_pred (ir_node *node, int pos, ir_node *pred);

INLINE ir_node  *get_Id_pred (ir_node *node);
INLINE void      set_Id_pred (ir_node *node, ir_node *pred);


/*
 *
 * NAME Auxiliary routines
 *
 *  Not properly documented ;-)
 *
 */

/** returns operand of node if node is a Proj. */
INLINE ir_node *skip_Proj (ir_node *node);
/** returns operand of node if node is a Id */
INLINE ir_node *skip_nop  (ir_node *node);
INLINE ir_node *skip_Id  (ir_node *node);   /* Same as skip_nop. */
/* returns corresponding operand of Tuple if node is a Proj from
   a Tuple. */
INLINE ir_node *skip_Tuple (ir_node *node);
/** returns true if node is a Bad node. */
INLINE int      is_Bad    (ir_node *node);
/** returns true if the node is not a Block */
INLINE int      is_no_Block (ir_node *node);
/** returns true if the node is a Block */
INLINE int      is_Block (ir_node *node);
/** returns true if node is a Proj node or a Filter node in
 * intraprocedural view */
INLINE int      is_Proj (const ir_node *node);
/** Returns true if the operation manipulates control flow:
   Start, End, Jmp, Cond, Return, Raise, Bad, CallBegin, EndReg, EndExcept */
int is_cfop(ir_node *node);

/** Returns true if the operation manipulates interprocedural control flow:
   CallBegin, EndReg, EndExcept */
int is_ip_cfop(ir_node *node);
/** Returns true if the operation can change the control flow because
   of an exception: Call, Quot, DivMod, Div, Mod, Load, Store, Alloc,
   Bad. */
ir_graph *get_ip_cfop_irg(ir_node *n);

int is_fragile_op(ir_node *node);
/** Returns the memory operand of fragile operations. */
ir_node *get_fragile_op_mem(ir_node *node);

#include "ident.h"

#ifdef __GNUC__
/* GNU C has the __FUNCTION__ extension */
#define __MYFUNC__ __FUNCTION__
#else
/* use Filename instead */
#define __MYFUNC__ __FILE__
#endif

/* !!!!!!!!! @@@
   Don't format with "\", firmjni gets problems */

/** Output the firm kind of the node */
#define DDMK(X)  printf("%s(l.%i) %s: %p\n",                 __MYFUNC__, __LINE__,  print_firm_kind(X), (X));
/** Output information about a node */
#define DDMN(X)  printf("%s(l.%i) %s%s: %ld (%p)\n",         __MYFUNC__, __LINE__,  get_irn_opname(X), get_mode_name(get_irn_mode(X)), get_irn_node_nr(X), (X))
/** Output information about a node and its block */
#define DDMNB(X) printf("%s%s: %ld (in block %ld)\n", get_irn_opname(X),  get_mode_name(get_irn_mode(X)), get_irn_node_nr(X), get_irn_node_nr(get_nodes_Block(X)))
/** Output information about a type */
#define DDMT(X)  printf("%s(l.%i) %s %s: %ld (%p)\n",        __MYFUNC__, __LINE__, get_type_tpop_name(X), get_type_name(X), get_type_nr(X), (X))
/** Output information about an entity */
#define DDME(X)  printf("%s(l.%i) %s: %ld (%p\n",            __MYFUNC__, __LINE__, get_entity_name(X), get_entity_nr(X), (X))
/** Output information about an entity and its owner */
#define DDMEO(X) printf("%s(l.%i) %s (own: %s): %ld (%p)\n", __MYFUNC__, __LINE__, get_entity_name(X), get_type_name(get_entity_owner(X)), get_entity_nr(X), (X))
/** Output information about a graph */
#define DDMG(X)  printf("%s(l.%i) %s: %ld (%p)\n",           __MYFUNC__, __LINE__, get_entity_name(get_irg_ent(X)), get_irg_graph_nr(X), (X))
/** Output information about an ident */
#define DDMI(X)  printf("%s(l.%i) %s: %p\n",                 __MYFUNC__, __LINE__, id_to_str(X), (X))
/** Output information about a mode */
#define DDMM(X)  printf("%s(l.%i) %s: %p\n",                 __MYFUNC__, __LINE__, get_mode_name(X), (X))
/** Output information about a loope */
#define DDML(X)  printf("%s(l.%i) loop with depth %d: %p\n", __MYFUNC__, __LINE__, get_loop_depth(X), (X))

/*@}*/ /* end of ir_node group definition */


# endif /* _IRNODE_H_ */
