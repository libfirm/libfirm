/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe      3 2002/02/28 13:33:52
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** declarations of an ir node
*/

/* $Id$ */

# ifndef _IRNODE_H_
# define _IRNODE_H_

# include "irgraph.h"
# include "entity.h"
# include "common.h"
# include "irop.h"
# include "irmode.h"
# include "tv.h"
# include "type.h"
# include "dbginfo.h"
# include "exc.h"

/* The typedefiniton of ir_node is also in irgraph.h to resolve
   recursion between irnode.h and irgraph.h */
#ifndef _IR_NODE_TYPEDEF_
#define _IR_NODE_TYPEDEF_
/****s* irnode/irnode
 *
 * NAME ir_node - a datatype representing a Firm node
 *
 * NOTE Not properly documented ;-)
 *
 * ATTRIBUTES
 *  The common fields are:
 *
 *  firm_kind    A firm_kind tag containing k_type.  This is useful
 *               for dynamically checking whether a node is a ir_node.
 *  arity        The number of predecessors in the Firm graph.
 *  in           A list with the predecessors in the Firm graph.  There are
 *               routines to access individual elements and to obtain the
 *               array.  The method returning the array should not be used.
 *  mode         The mode of the node.  There are routines to get the mode but
 *               also to access the mode's fields directly.
 *  opcode       The opcode of the node.  There are routines to get the opcode but
 *               also to access the opcode's fields directly.
 *  node_nr      A unique number for the node.  Available only if debugging is
 *               turned on.
 * SOURCE
 */
typedef struct ir_node ir_node;
#endif

/** Manipulate the fields of ir_node.  With these access routines
    you can work on the graph without considering the different types
    of nodes, it's just a big graph. **/

/* returns the number of predecessors without the block predecessor: */
int                  get_irn_arity         (ir_node *node);
/* returns the array with the ins: */
inline ir_node     **get_irn_in            (ir_node *node);
/* Replaces the old in array by a new one that will contain the ins given in
   the parameters.  Conserves the block predecessor.  It copies the array passed.
   This function is necessary to ajust in arrays of blocks, calls and phis.
   Assumes that current_ir_graph is set to the graph containing "node".
   "in" must contain all predecessors except the block that are required for
   the nodes opcode. */
inline void          set_irn_in            (ir_node *node, int arity,
					    ir_node **in);
/* to iterate through the predecessors without touching the array. No
   order of predecessors guaranteed.
   To iterate over the operands iterate from 0 to i < get_irn_arity(),
   to iterate including the Block predecessor iterate from i = -1 to
   i < get_irn_arity. */
/* Access predecessor n */
/* get_irn_n removes Id predecessors. */
inline ir_node      *get_irn_n             (ir_node *node, int n);
inline void          set_irn_n             (ir_node *node, int n, ir_node *in);
/* Get the mode struct. */
inline ir_mode      *get_irn_mode          (ir_node *node);
/* Get the mode-enum modecode */
inline modecode      get_irn_modecode      (ir_node *node);
/* Get the ident for a string representation of the mode */
inline ident        *get_irn_modeident     (ir_node *node);
/* Access the opcode struct of the node */
inline ir_op        *get_irn_op            (ir_node *node);
inline void          set_irn_op            (ir_node *node, ir_op *op);
/* Get the opcode-enum of the node */
inline opcode        get_irn_opcode        (ir_node *node);
/* Get the ident for a string representation of the opcode */
inline ident        *get_irn_opident       (ir_node *node);
/* Get the string representation of the opcode */
inline const char   *get_irn_opname        (ir_node *node);
inline void          set_irn_visited (ir_node *node, unsigned long visited);
inline unsigned long get_irn_visited (ir_node *node);
/* Sets visited to get_irg_visited(current_ir_graph) */
inline void          mark_irn_visited (ir_node *node);
/* Returns 1 of visited < get_irg_visited(current_ir_graph).  */
inline int           irn_not_visited  (ir_node *node);
inline void          set_irn_link          (ir_node *node, ir_node *link);
inline ir_node      *get_irn_link          (ir_node *node);
/* Outputs a unique number for this node if libfirm is compiled for
   debugging, else returns 0. */
inline long get_irn_node_nr(ir_node *node);
/*****/

/* irnode constructor                                             */
/* Create a new irnode in irg, with an op, mode, arity and        */
/* some incoming irnodes.                                         */
/* If arity is negative, a node with a dynamic array is created.  */

inline ir_node *
new_ir_node (dbg_info *db,
	     ir_graph *irg,
	     ir_node *block,
	     ir_op *op,
	     ir_mode *mode,
	     int arity,
	     ir_node **in);

/****s* irnode/other
 *
 * NAME access functions for node fields.
 *
 * NOTE Not properly documented ;-)
 *
 * SOURCE
 */

/* this works for all except Block */
inline ir_node  *get_nodes_Block (ir_node *node);
inline void      set_nodes_Block (ir_node *node, ir_node *block);

/* Projection numbers for result of Start node: use for Proj nodes! */
typedef enum {
  pns_initial_exec,     /* Projection on an executable, the initial control
			   flow. */
  pns_global_store,     /* Projection on the global store */
  pns_frame_base,       /* Projection on the frame base */
  pns_globals,          /* Projection on the pointer to the data segment
			   containing _all_ global entities. */
  pns_args              /* Projection on all arguments */
} pns_number;

inline ir_node **get_Block_cfgpred_arr (ir_node *node);
int              get_Block_n_cfgpreds (ir_node *node);
/* inline void    set_Block_n_cfgpreds (ir_node *node, int n_preds); */
inline ir_node  *get_Block_cfgpred (ir_node *node, int pos);
inline void      set_Block_cfgpred (ir_node *node, int pos, ir_node *pred);
inline bool      get_Block_matured (ir_node *node);
inline void      set_Block_matured (ir_node *node, bool matured);
inline unsigned long get_Block_block_visited (ir_node *node);
inline void      set_Block_block_visited (ir_node *node, unsigned long visit);
/* For this current_ir_graph must be set. */
inline void      mark_Block_block_visited(ir_node *node);
inline int       Block_not_block_visited(ir_node *node);
inline ir_node  *get_Block_graph_arr (ir_node *node, int pos);
inline void      set_Block_graph_arr (ir_node *node, int pos, ir_node *value);

void  set_Block_exc (ir_node*, exc_t);
exc_t get_Block_exc (ir_node*);


inline int  get_End_n_keepalives(ir_node *end);
inline ir_node *get_End_keepalive(ir_node *end, int pos);
inline void add_End_keepalive (ir_node *end, ir_node *ka);
/* Some parts of the End node are allocated seperately -- their memory
   is not recovered by dead_node_elimination if a End node is dead.
   free_End frees these data structures. */
inline void free_End (ir_node *end);

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
  dense,        /* Default. Missing Proj nodes are dead control flow. */
  fragmentary   /* Special. No control flow optimizations allowed.  Missing
		   Proj nodes mean default control flow, i.e., Proj(n). */
} cond_kind;

inline ir_node  *get_Cond_selector (ir_node *node);
inline void      set_Cond_selector (ir_node *node, ir_node *selector);
inline cond_kind get_Cond_kind (ir_node *node);
inline void      set_Cond_kind (ir_node *node, cond_kind kind);

inline ir_node  *get_Return_mem (ir_node *node);
inline void      set_Return_mem (ir_node *node, ir_node *mem);
inline ir_node **get_Return_res_arr (ir_node *node);
inline int       get_Return_n_res (ir_node *node);
/*inline void     set_Return_n_res (ir_node *node, int results); */
inline ir_node  *get_Return_res (ir_node *node, int pos);
inline void      set_Return_res (ir_node *node, int pos, ir_node *res);

inline ir_node *get_Raise_mem (ir_node *node);
inline void     set_Raise_mem (ir_node *node, ir_node *mem);
inline ir_node *get_Raise_exo_ptr (ir_node *node);  /* PoinTeR to EXception Object */
inline void     set_Raise_exo_ptr (ir_node *node, ir_node *exoptr);

inline tarval  *get_Const_tarval (ir_node *node);
inline void     set_Const_tarval (ir_node *node, tarval *con);

/*   This enum names the three different kinds of symbolic Constants
     represented by SymConst.  The content of the attribute type_or_id
     depends on this tag.  Use the proper access routine after testing
     this flag. */
typedef enum {
  type_tag,          /* The SymConst is a type tag for the given type.
			Type_or_id_p is type *. */
  size,              /* The SymConst is the size of the given type.
			Type_or_id_p is type *. */
  linkage_ptr_info   /* The SymConst is a symbolic pointer to be filled in
			by the linker. Type_or_id_p is ident *. */
} symconst_kind;
typedef union type_or_id * type_or_id_p;
inline symconst_kind get_SymConst_kind (ir_node *node);
inline void          set_SymConst_kind (ir_node *node, symconst_kind num);
/* Only to access SymConst of kind type_tag or size.  Else assertion: */
inline type    *get_SymConst_type (ir_node *node);
inline void     set_SymConst_type (ir_node *node, type *type);
/* Only to access SymConst of kind linkage_ptr_info.  Else assertion: */
inline ident   *get_SymConst_ptrinfo (ir_node *node);
inline void     set_SymConst_ptrinfo (ir_node *node, ident *ptrinfo);
/* Sets both: type and ptrinfo.  Needed to treat the node independent of
   its semantics.  Does a memcpy for the memory tori points to. */
inline type_or_id_p get_SymConst_type_or_id (ir_node *node);
inline void set_SymConst_type_or_id (ir_node *node, type_or_id_p tori);

inline ir_node *get_Sel_mem (ir_node *node);
inline void     set_Sel_mem (ir_node *node, ir_node *mem);
inline ir_node *get_Sel_ptr (ir_node *node);  /* ptr to the object to select from */
inline void     set_Sel_ptr (ir_node *node, ir_node *ptr);
inline ir_node **get_Sel_index_arr (ir_node *node);
inline int      get_Sel_n_index (ir_node *node);
/*inline void     set_Sel_n_index (ir_node *node, int n_index); */
inline ir_node *get_Sel_index (ir_node *node, int pos);
inline void     set_Sel_index (ir_node *node, int pos, ir_node *index);
inline entity  *get_Sel_entity (ir_node *node); /* entity to select */
inline void     set_Sel_entity (ir_node *node, entity *ent);
typedef enum {
  static_linkage,       /* entity is used internal and not visible out of this
			   file/class. */
  external_linkage,     /* */
  no_linkage
} linkage_type;
inline linkage_type get_Sel_linkage_type (ir_node *node);
inline void     set_Sel_linkage_type (ir_node *node, linkage_type lt);

inline ir_node *get_Call_mem (ir_node *node);
inline void     set_Call_mem (ir_node *node, ir_node *mem);
inline ir_node *get_Call_ptr (ir_node *node);
inline void     set_Call_ptr (ir_node *node, ir_node *ptr);
inline ir_node **get_Call_param_arr (ir_node *node);
inline int      get_Call_n_params (ir_node *node);
inline int      get_Call_arity (ir_node *node);
/* inline void     set_Call_arity (ir_node *node, ir_node *arity); */
inline ir_node *get_Call_param (ir_node *node, int pos);
inline void     set_Call_param (ir_node *node, int pos, ir_node *param);
inline type    *get_Call_type (ir_node *node);
inline void     set_Call_type (ir_node *node, type *type);

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
  True,		        /* true */
  not_mask = Leg	/* bits to flip to negate comparison */
} pnc_number;
inline char *get_pnc_string(int pnc);
inline int   get_negated_pnc(int pnc);
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

inline ir_node **get_Phi_preds_arr (ir_node *node);
inline int       get_Phi_n_preds (ir_node *node);
/* inline void     set_Phi_n_preds (ir_node *node, int n_preds); */
inline ir_node  *get_Phi_pred (ir_node *node, int pos);
inline void      set_Phi_pred (ir_node *node, int pos, ir_node *pred);

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
typedef enum {
  stack_alloc,          /* Alloc allocates the object on the stack. */
  heap_alloc            /* Alloc allocates the object on the heap. */
} where_alloc;
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

inline ir_node **get_Sync_preds_arr (ir_node *node);
inline int       get_Sync_n_preds (ir_node *node);
/* inline void     set_Sync_n_preds (ir_node *node, int n_preds); */
inline ir_node  *get_Sync_pred (ir_node *node, int pos);
inline void      set_Sync_pred (ir_node *node, int pos, ir_node *pred);

inline ir_node  *get_Proj_pred (ir_node *node);
inline void      set_Proj_pred (ir_node *node, ir_node *pred);
inline long      get_Proj_proj (ir_node *node);
inline void      set_Proj_proj (ir_node *node, long proj);

inline ir_node **get_Tuple_preds_arr (ir_node *node);
inline int       get_Tuple_n_preds (ir_node *node);
/* inline void     set_Tuple_n_preds (ir_node *node, int n_preds); */
inline ir_node  *get_Tuple_pred (ir_node *node, int pos);
inline void      set_Tuple_pred (ir_node *node, int pos, ir_node *pred);

inline ir_node  *get_Id_pred (ir_node *node);
inline void      set_Id_pred (ir_node *node, ir_node *pred);

/*****/

/****s* irnode/other2
 *
 * NAME Auxiliary routines
 *
 * NOTE Not properly documented ;-)
 *
 * SOURCE
 */

/* returns operand of node if node is a Proj. */
inline ir_node *skip_Proj (ir_node *node);
/* returns operand of node if node is a Id */
inline ir_node *skip_nop  (ir_node *node);
inline ir_node *skip_Id  (ir_node *node);   /* Same as skip_nop. */
/* returns corresponding operand of Tuple if node is a Proj from
   a Tuple. */
inline ir_node *skip_Tuple (ir_node *node);
/* returns true if node is a Bad node. */
inline int      is_Bad    (ir_node *node);
/* returns true if the node is not a Block */
inline int      is_no_Block (ir_node *node);
/* Returns true if the operation manipulates control flow:
   Start, End, Jmp, Cond, Return, Raise, Bad */
int is_cfop(ir_node *node);
/* Returns true if the operation can change the control flow because
   of an exception: Call, Quot, DivMod, Div, Mod, Load, Store, Alloc,
   Bad. */
int is_fragile_op(ir_node *node);
/* Returns the memory operand of fragile operations. */
ir_node *get_fragile_op_mem(ir_node *node);

/*****/

/* Makros for debugging the libfirm */
#include "ident.h"

#define DDMSG        printf("%s(l.%i)\n", __FUNCTION__, __LINE__)
#define DDMSG1(X)    printf("%s(l.%i) %s\n", __FUNCTION__, __LINE__,         \
                            id_to_str(get_irn_opident(X)))
#define DDMSG2(X)    printf("%s(l.%i) %s%s: %ld\n", __FUNCTION__, __LINE__,          \
                     id_to_str(get_irn_opident(X)), id_to_str(get_irn_modeident(X)), \
                     get_irn_node_nr(X))
#define DDMSG3(X)    printf("%s(l.%i) %s: %p\n", __FUNCTION__, __LINE__,     \
                     print_firm_kind(X), (X))
#define DDMSG4(X)    xprintf("%s(l.%i) %I %I: %p\n", __FUNCTION__, __LINE__,     \
                     get_type_tpop_nameid(X), get_type_ident(X), (X))
#define DDMSG5(X)    printf("%s%s: %ld",          \
                     id_to_str(get_irn_opident(X)), id_to_str(get_irn_modeident(X)), \
                     get_irn_node_nr(X))


#define DDMN(X)      xprintf("%s(l.%i) %I%I: %ld (%p)\n", __FUNCTION__, __LINE__,      \
                     get_irn_opident(X), get_irn_modeident(X), get_irn_node_nr(X), (X))
#define DDMNB(X)     xprintf("%I%I: %ld (in block %ld)\n",                             \
		     get_irn_opident(X), get_irn_modeident(X), get_irn_node_nr(X),     \
		     get_irn_node_nr(get_nodes_Block(X)))
#define DDMT(X)      xprintf("%s(l.%i) %I %I: %p\n", __FUNCTION__, __LINE__,           \
                     get_type_tpop_nameid(X), get_type_ident(X), (X))
#define DDME(X)      xprintf("%s(l.%i) %I: %p\n", __FUNCTION__, __LINE__,              \
                     get_entity_ident(X), (X))


# endif /* _IRNODE_H_ */
