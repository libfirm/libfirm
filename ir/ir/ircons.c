/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** ircons.c: basic and more detailed irnode constructors
**           store, block and parameter administration.
** Adapted to extended FIRM nodes (exceptions...) and commented
**   by Goetz Lindenmaier
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irgraph_t.h"
# include "irnode_t.h"
# include "irmode_t.h"
# include "ircons.h"
# include "common.h"
# include "irvrfy.h"
# include "irop.h"
# include "iropt.h"
# include "irgmod.h"
# include "array.h"
/* memset belongs to string.h */
# include "string.h"

#if USE_EXPICIT_PHI_IN_STACK
/* A stack needed for the automatic Phi node construction in constructor
   Phi_in. Redefinition in irgraph.c!! */
struct Phi_in_stack {
  ir_node **stack;
  int       pos;
};
typedef struct Phi_in_stack Phi_in_stack;
#endif

/*** ******************************************** */
/** privat interfaces, for professional use only */

/* Constructs a Block with a fixed number of predecessors.
   Does not set current_block.  Can not be used with automatic
   Phi node construction. */
inline ir_node *
new_r_Block (ir_graph *irg,  int arity, ir_node **in)
{
  ir_node *res;

  res = new_ir_node (irg, NULL, op_Block, mode_R, arity, in);
  set_Block_matured(res, 1);
  set_Block_block_visited(res, 0);

  irn_vrfy (res);
  return res;
}

ir_node *
new_r_Start (ir_graph *irg, ir_node *block)
{
  ir_node *res;

  res = new_ir_node (irg, block, op_Start, mode_T, 0, NULL);

  irn_vrfy (res);
  return res;
}

ir_node *
new_r_End (ir_graph *irg, ir_node *block)
{
  ir_node *res;

  res = new_ir_node (irg, block, op_End, mode_X, -1, NULL);

  irn_vrfy (res);
  return res;
}

/* Creates a Phi node with all predecessors.  Calling this constructor
   is only allowed if the corresponding block is mature.  */
ir_node *
new_r_Phi (ir_graph *irg, ir_node *block, int arity, ir_node **in, ir_mode *mode)
{
  ir_node *res;

  assert( get_Block_matured(block) );
  assert( get_irn_arity(block) == arity );

  res = new_ir_node (irg, block, op_Phi, mode, arity, in);

  res = optimize (res);
  irn_vrfy (res);
  return res;
}

ir_node *
new_r_Const (ir_graph *irg, ir_node *block, ir_mode *mode, tarval *con)
{
  ir_node *res;
  res = new_ir_node (irg, block, op_Const, mode, 0, NULL);
  res->attr.con = con;
  res = optimize (res);
  irn_vrfy (res);

#if 0
  res = local_optimize_newby (res);
# endif

  return res;
}

ir_node *
new_r_Id (ir_graph *irg, ir_node *block, ir_node *val, ir_mode *mode)
{
  ir_node *in[1] = {val};
  ir_node *res;
  res = new_ir_node (irg, block, op_Id, mode, 1, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

ir_node *
new_r_Proj (ir_graph *irg, ir_node *block, ir_node *arg, ir_mode *mode,
	    long proj)
{
  ir_node *in[1] = {arg};
  ir_node *res;
  res = new_ir_node (irg, block, op_Proj, mode, 1, in);
  res->attr.proj = proj;

  assert(res);
  assert(get_Proj_pred(res));
  assert(get_nodes_Block(get_Proj_pred(res)));

  res = optimize (res);

  irn_vrfy (res);
  return res;

}

ir_node *
new_r_defaultProj (ir_graph *irg, ir_node *block, ir_node *arg,
		   long max_proj)
{
  ir_node *res;
  assert((arg->op==op_Cond) && (get_irn_mode(arg->in[1]) == mode_I));
  arg->attr.c.kind = fragmentary;
  arg->attr.c.default_proj = max_proj;
  res = new_r_Proj (irg, block, arg, mode_X, max_proj);
  return res;
}

ir_node *
new_r_Conv (ir_graph *irg, ir_node *block, ir_node *op, ir_mode *mode)
{
  ir_node *in[1] = {op};
  ir_node *res;
  res = new_ir_node (irg, block, op_Conv, mode, 1, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;

}

ir_node *
new_r_Tuple (ir_graph *irg, ir_node *block, int arity, ir_node **in)
{
  ir_node *res;

  res = new_ir_node (irg, block, op_Tuple, mode_T, arity, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Add (ir_graph *irg, ir_node *block,
	   ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2] = {op1, op2};
  ir_node *res;
  res = new_ir_node (irg, block, op_Add, mode, 2, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Sub (ir_graph *irg, ir_node *block,
	   ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2] = {op1, op2};
  ir_node *res;
  res = new_ir_node (irg, block, op_Sub, mode, 2, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Minus (ir_graph *irg, ir_node *block,
	     ir_node *op,  ir_mode *mode)
{
  ir_node *in[1] = {op};
  ir_node *res;
  res = new_ir_node (irg, block, op_Minus, mode, 1, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Mul (ir_graph *irg, ir_node *block,
	   ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2] = {op1, op2};
  ir_node *res;
  res = new_ir_node (irg, block, op_Mul, mode, 2, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Quot (ir_graph *irg, ir_node *block,
     	    ir_node *memop, ir_node *op1, ir_node *op2)
{
  ir_node *in[3] = {memop, op1, op2};
  ir_node *res;
  res = new_ir_node (irg, block, op_Quot, mode_T, 3, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_DivMod (ir_graph *irg, ir_node *block,
	      ir_node *memop, ir_node *op1, ir_node *op2)
{
  ir_node *in[3] = {memop, op1, op2};
  ir_node *res;
  res = new_ir_node (irg, block, op_DivMod, mode_T, 3, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Div (ir_graph *irg, ir_node *block,
    	   ir_node *memop, ir_node *op1, ir_node *op2)
{
  ir_node *in[3] = {memop, op1, op2};
  ir_node *res;
  res = new_ir_node (irg, block, op_Div, mode_T, 3, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Mod (ir_graph *irg, ir_node *block,
     	   ir_node *memop, ir_node *op1, ir_node *op2)
{
  ir_node *in[3] = {memop, op1, op2};
  ir_node *res;
  res = new_ir_node (irg, block, op_Mod, mode_T, 3, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_And (ir_graph *irg, ir_node *block,
     	   ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2] = {op1, op2};
  ir_node *res;
  res = new_ir_node (irg, block, op_And, mode, 2, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Or (ir_graph *irg, ir_node *block,
     	  ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2] = {op1, op2};
  ir_node *res;
  res = new_ir_node (irg, block, op_Or, mode, 2, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Eor (ir_graph *irg, ir_node *block,
     	  ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2] = {op1, op2};
  ir_node *res;
  res = new_ir_node (irg, block, op_Eor, mode, 2, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Not    (ir_graph *irg, ir_node *block,
	      ir_node *op, ir_mode *mode)
{
  ir_node *in[1] = {op};
  ir_node *res;
  res = new_ir_node (irg, block, op_Not, mode, 1, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Shl (ir_graph *irg, ir_node *block,
     	  ir_node *op, ir_node *k, ir_mode *mode)
{
  ir_node *in[2] = {op, k};
  ir_node *res;
  res = new_ir_node (irg, block, op_Shl, mode, 2, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Shr (ir_graph *irg, ir_node *block,
	   ir_node *op, ir_node *k, ir_mode *mode)
{
  ir_node *in[2] = {op, k};
  ir_node *res;
  res = new_ir_node (irg, block, op_Shr, mode, 2, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Shrs (ir_graph *irg, ir_node *block,
	   ir_node *op, ir_node *k, ir_mode *mode)
{
  ir_node *in[2] = {op, k};
  ir_node *res;
  res = new_ir_node (irg, block, op_Shrs, mode, 2, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Rot (ir_graph *irg, ir_node *block,
	   ir_node *op, ir_node *k, ir_mode *mode)
{
  ir_node *in[2] = {op, k};
  ir_node *res;
  res = new_ir_node (irg, block, op_Rot, mode, 2, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Abs (ir_graph *irg, ir_node *block,
	   ir_node *op, ir_mode *mode)
{
  ir_node *in[1] = {op};
  ir_node *res;
  res = new_ir_node (irg, block, op_Abs, mode, 1, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Cmp (ir_graph *irg, ir_node *block,
	   ir_node *op1, ir_node *op2)
{
  ir_node *in[2] = {op1, op2};
  ir_node *res;
  res = new_ir_node (irg, block, op_Cmp, mode_T, 2, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Jmp (ir_graph *irg, ir_node *block)
{
  ir_node *in[0] = {};
  ir_node *res;
  res = new_ir_node (irg, block, op_Jmp, mode_X, 0, in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Cond (ir_graph *irg, ir_node *block, ir_node *c)
{
  ir_node *in[1] = {c};
  ir_node *res;
  res = new_ir_node (irg, block, op_Cond, mode_T, 1, in);
  res->attr.c.kind = dense;
  res->attr.c.default_proj = 0;
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

ir_node *
new_r_Call (ir_graph *irg, ir_node *block, ir_node *store,
	    ir_node *callee, int arity, ir_node **in, type *type)
{
  ir_node **r_in;
  ir_node *res;
  int r_arity;

  r_arity = arity+2;
  NEW_ARR_A (ir_node *, r_in, r_arity);
  r_in[0] = store;
  r_in[1] = callee;
  memcpy (&r_in[2], in, sizeof (ir_node *) * arity);

  res = new_ir_node (irg, block, op_Call, mode_T, r_arity, r_in);

  assert(is_method_type(type));
  set_Call_type(res, type);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

ir_node *
new_r_Return (ir_graph *irg, ir_node *block,
              ir_node *store, int arity, ir_node **in)
{
  ir_node **r_in;
  ir_node *res;
  int r_arity;

  r_arity = arity+1;
  NEW_ARR_A (ir_node *, r_in, r_arity);
  r_in[0] = store;
  memcpy (&r_in[1], in, sizeof (ir_node *) * arity);
  res = new_ir_node (irg, block, op_Return, mode_X, r_arity, r_in);
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Raise (ir_graph *irg, ir_node *block, ir_node *store, ir_node *obj)
{
  ir_node *in[2] = {store, obj};
  ir_node *res;
  res = new_ir_node (irg, block, op_Raise, mode_X, 2, in);

  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Load (ir_graph *irg, ir_node *block,
	    ir_node *store, ir_node *adr)
{
  ir_node *in[2] = {store, adr};
  ir_node *res;
  res = new_ir_node (irg, block, op_Load, mode_T, 2, in);

  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Store (ir_graph *irg, ir_node *block,
	     ir_node *store, ir_node *adr, ir_node *val)
{
  ir_node *in[3] = {store, adr, val};
  ir_node *res;
  res = new_ir_node (irg, block, op_Store, mode_T, 3, in);

  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Alloc (ir_graph *irg, ir_node *block, ir_node *store,
	    ir_node *size, type *alloc_type, where_alloc where)
{
  ir_node *in[2] = {store, size};
  ir_node *res;
  res = new_ir_node (irg, block, op_Alloc, mode_T, 2, in);

  res->attr.a.where = where;
  res->attr.a.type = alloc_type;

  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Free (ir_graph *irg, ir_node *block, ir_node *store,
	    ir_node *ptr, ir_node *size, type *free_type)
{
  ir_node *in[3] = {store, ptr, size};
  ir_node *res;
  res = new_ir_node (irg, block, op_Free, mode_T, 3, in);

  res->attr.f = free_type;

  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_Sel (ir_graph *irg, ir_node *block, ir_node *store, ir_node *objptr,
           int arity, ir_node **in, entity *ent)
{
  ir_node **r_in;
  ir_node *res;
  int r_arity;

  r_arity = arity + 2;
  NEW_ARR_A (ir_node *, r_in, r_arity);
  r_in[0] = store;
  r_in[1] = objptr;
  memcpy (&r_in[2], in, sizeof (ir_node *) * arity);
  res = new_ir_node (irg, block, op_Sel, mode_p, r_arity, r_in);

  res->attr.s.ltyp = static_linkage;
  res->attr.s.ent = ent;

  res = optimize (res);
  irn_vrfy (res);
  return res;
}

inline ir_node *
new_r_SymConst (ir_graph *irg, ir_node *block, type_or_id_p value,
                symconst_kind symkind)
{
  ir_node *in[0] = {};
  ir_node *res;
  ir_mode *mode;
  if (symkind == linkage_ptr_info)
    mode = mode_p;
  else
    mode = mode_I;
  res = new_ir_node (irg, block, op_SymConst, mode, 0, in);

  res->attr.i.num = symkind;
  if (symkind == linkage_ptr_info) {
    res->attr.i.tori.ptrinfo = (ident *)value;
  } else {
    assert (   (   (symkind == type_tag)
	        || (symkind == size))
            && (is_type(value)));
    res->attr.i.tori.typ = (type *)value;
  }
  res = optimize (res);
  irn_vrfy (res);
  return res;
}

ir_node *
new_r_Sync (ir_graph *irg, ir_node *block, int arity, ir_node **in)
{
  ir_node *res;

  res = new_ir_node (irg, block, op_Sync, mode_M, arity, in);

  res = optimize (res);
  irn_vrfy (res);
  return res;
}

ir_node *
new_r_Bad ()
{
  return current_ir_graph->bad;
}

/** ********************/
/** public interfaces  */
/** construction tools */

/****f* ircons/new_Start
 *
 * NAME
 *   new_Start -- create a new Start node in the current block
 *
 * SYNOPSIS
 *   s = new_Start(void);
 *   ir_node* new_Start(void);
 *
 * RESULT
 *   s - pointer to the created Start node
 *
 ****
 */
ir_node *
new_Start (void)
{
  ir_node *res;

  res = new_ir_node (current_ir_graph, current_ir_graph->current_block,
		     op_Start, mode_T, 0, NULL);

  res = optimize (res);
  irn_vrfy (res);
  return res;
}

ir_node *
new_End (void)
{
  ir_node *res;

  res = new_ir_node (current_ir_graph,  current_ir_graph->current_block,
		     op_End, mode_X, -1, NULL);

  res = optimize (res);
  irn_vrfy (res);

  return res;
}

/* Constructs a Block with a fixed number of predecessors.
   Does set current_block.  Can be used with automatic Phi
   node construction. */
ir_node *
new_Block (int arity, ir_node **in)
{
  ir_node *res;

  res = new_r_Block (current_ir_graph, arity, in);
  current_ir_graph->current_block = res;

  /* Create and initialize array for Phi-node construction. */
  res->attr.block.graph_arr = NEW_ARR_D (ir_node *, current_ir_graph->obst,
                                         current_ir_graph->n_loc);
  memset(res->attr.block.graph_arr, 0, sizeof(ir_node *)*current_ir_graph->n_loc);

  res = optimize (res);
  irn_vrfy (res);

  return res;
}

/* ***********************************************************************/
/* Methods necessary for automatic Phi node creation                     */
/*
  ir_node *phi_merge            (ir_node *block, int pos, ir_mode *mode, ir_node **nin, int ins)
  ir_node *get_r_value_internal (ir_node *block, int pos, ir_mode *mode);
  ir_node *new_r_Phi0           (ir_graph *irg, ir_node *block, ir_mode *mode)
  ir_node *new_r_Phi_in         (ir_graph *irg, ir_node *block, ir_mode *mode,  ir_node **in, int ins)

  Call Graph:   ( A ---> B == A "calls" B)

       get_value         mature_block
          |                   |
          |                   |
          |                   |
          |          ---> phi_merge
          |         /       /   \
          |        /       /     \
         \|/      /      |/_      \
       get_r_value_internal        |
                |                  |
	        |                  |
	       \|/                \|/
  	    new_r_Phi0          new_r_Phi_in

* *************************************************************************** */

/* Creates a Phi node with 0 predecessors */
inline ir_node *
new_r_Phi0 (ir_graph *irg, ir_node *block, ir_mode *mode)
{
  ir_node *res;
  res = new_ir_node (irg, block, op_Phi, mode, 0, NULL);
  irn_vrfy (res);
  return res;
}

/* There are two implementations of the Phi node construction.  The first
   is faster, but does not work for blocks with more than 2 predecessors.
   The second works always but is slower and causes more unnecessary Phi
   nodes.
   Select the implementations by the following preprocessor flag set in
   common/common.h: */
#if USE_FAST_PHI_CONSTRUCTION

/* This is a stack used for allocating and deallocating nodes in
   new_r_Phi_in.  The original implementation used the obstack
   to model this stack, now it is explicit.  This reduces side effects.
*/
#if USE_EXPICIT_PHI_IN_STACK
Phi_in_stack *
new_Phi_in_stack() {
  Phi_in_stack *res;

  res = (Phi_in_stack *) malloc ( sizeof (Phi_in_stack));

  res->stack = NEW_ARR_F (ir_node *, 1);
  res->pos = 0;

  return res;
}

void
free_Phi_in_stack(Phi_in_stack *s) {
  DEL_ARR_F(s->stack);
  free(s);
}

void free_to_Phi_in_stack(ir_node *phi) {
  assert(get_irn_opcode(phi) == iro_Phi);

  if (ARR_LEN(current_ir_graph->Phi_in_stack->stack) ==
      current_ir_graph->Phi_in_stack->pos)
    ARR_APP1 (ir_node *, current_ir_graph->Phi_in_stack->stack, phi);
  else
    current_ir_graph->Phi_in_stack->stack[current_ir_graph->Phi_in_stack->pos] = phi;

  (current_ir_graph->Phi_in_stack->pos)++;
}

ir_node *
alloc_or_pop_from_Phi_in_stack(ir_graph *irg, ir_node *block, ir_mode *mode,
	     int arity, ir_node **in) {
  ir_node *res;
  ir_node **stack = current_ir_graph->Phi_in_stack->stack;
  int pos = current_ir_graph->Phi_in_stack->pos;


  if (pos == 0) {
    /* We need to allocate a new node */
    res = new_ir_node (irg, block, op_Phi, mode, arity, in);
  } else {
    /* reuse the old node and initialize it again. */
    res = stack[pos-1];

    assert (res->kind == k_ir_node);
    assert (res->op == op_Phi);
    res->mode = mode;
    res->visited = 0;
    res->link = NULL;
    assert (arity >= 0);
    /* ???!!! How to free the old in array??  */
    res->in = NEW_ARR_D (ir_node *, irg->obst, (arity+1));
    res->in[0] = block;
    memcpy (&res->in[1], in, sizeof (ir_node *) * arity);

    (current_ir_graph->Phi_in_stack->pos)--;
  }
  return res;
}
#endif /* USE_EXPICIT_PHI_IN_STACK */

/* Creates a Phi node with a given, fixed array **in of predecessors.
   If the Phi node is unnecessary, as the same value reaches the block
   through all control flow paths, it is eliminated and the value
   returned directly.  This constructor is only intended for use in
   the automatic Phi node generation triggered by get_value or mature.
   The implementation is quite tricky and depends on the fact, that
   the nodes are allocated on a stack:
   The in array contains predecessors and NULLs.  The NULLs appear,
   if get_r_value_internal, that computed the predecessors, reached
   the same block on two paths.  In this case the same value reaches
   this block on both paths, there is no definition in between.  We need
   not allocate a Phi where these path's merge, but we have to communicate
   this fact to the caller.  This happens by returning a pointer to the
   node the caller _will_ allocate.  (Yes, we predict the address. We can
   do so because the nodes are allocated on the obstack.)  The caller then
   finds a pointer to itself and, when this routine is called again,
   eliminates itself.
   */
inline ir_node *
new_r_Phi_in (ir_graph *irg, ir_node *block, ir_mode *mode,
	      ir_node **in, int ins)
{
  int i;
  ir_node *res, *known;

  /* allocate a new node on the obstack.
     This can return a node to which some of the pointers in the in-array
     already point.
     Attention: the constructor copies the in array, i.e., the later changes
     to the array in this routine do not affect the constructed node!  If
     the in array contains NULLs, there will be missing predecessors in the
     returned node.
     Is this a possible internal state of the Phi node generation? */
#if USE_EXPICIT_PHI_IN_STACK
  res = known = alloc_or_pop_from_Phi_in_stack(irg, block, mode, ins, in);
#else
  res = known = new_ir_node (irg, block, op_Phi, mode, ins, in);
#endif
  /* The in-array can contain NULLs.  These were returned by
     get_r_value_internal if it reached the same block/definition on a
     second path.
     The NULLs are replaced by the node itself to simplify the test in the
     next loop. */
  for (i=0;  i < ins;  ++i)
    if (in[i] == NULL) in[i] = res;

  /* This loop checks whether the Phi has more than one predecessor.
     If so, it is a real Phi node and we break the loop.  Else the
     Phi node merges the same definition on several paths and therefore
     is not needed. */
  for (i=0;  i < ins;  ++i)
  {
    if (in[i]==res || in[i]==known) continue;

    if (known==res)
      known = in[i];
    else
      break;
  }

  /* i==ins: there is at most one predecessor, we don't need a phi node. */
  if (i==ins) {
#if USE_EXPICIT_PHI_IN_STACK
    free_to_Phi_in_stack(res);
#else
    obstack_free (current_ir_graph->obst, res);
#endif
    res = known;
  } else {
    res = optimize (res);
    irn_vrfy (res);
  }

  /* return the pointer to the Phi node.  This node might be deallocated! */
  return res;
}

inline ir_node *
get_r_value_internal (ir_node *block, int pos, ir_mode *mode);

/** This function computes the predecessors for a real Phi node, and then
    allocates and returns this node.  The routine called to allocate the
    node might optimize it away and return a real value, or even a pointer
    to a deallocated Phi node on top of the obstack!
    This function is called with an in-array of proper size. **/
static inline ir_node *
phi_merge (ir_node *block, int pos, ir_mode *mode, ir_node **nin, int ins)
{
  ir_node *prevBlock, *res;
  int i;

  /* This loop goes to all predecessor blocks of the block the Phi node is in
     and there finds the operands of the Phi node by calling
     get_r_value_internal. */
  for (i = 1;  i <= ins;  ++i) {
    assert (block->in[i]);
    prevBlock = block->in[i]->in[0]; /* go past control flow op to prev block */
    assert (prevBlock);
    nin[i-1] = get_r_value_internal (prevBlock, pos, mode);
  }

  /* After collecting all predecessors into the array nin a new Phi node
     with these predecessors is created.  This constructor contains an
     optimization: If all predecessors of the Phi node are identical it
     returns the only operand instead of a new Phi node.  If the value
     passes two different control flow edges without being defined, and
     this is the second path treated, a pointer to the node that will be
     allocated for the first path (recursion) is returned.  We already
     know the address of this node, as it is the next node to be allocated
     and will be placed on top of the obstack. (The obstack is a _stack_!) */
  res = new_r_Phi_in (current_ir_graph, block, mode, nin, ins);

  /* Now we now the value for "pos" and can enter it in the array with
     all known local variables.  Attention: this might be a pointer to
     a node, that later will be allocated!!! See new_r_Phi_in.
     If this is called in mature, after some set_value in the same block,
     the proper value must not be overwritten:
     The call order
       get_value    (makes Phi0, put's it into graph_arr)
       set_value    (overwrites Phi0 in graph_arr)
       mature_block (upgrades Phi0, puts it again into graph_arr, overwriting
                     the proper value.)
     fails. */
  if (!block->attr.block.graph_arr[pos]) {
    block->attr.block.graph_arr[pos] = res;
  } else {
    /*  printf(" value already computed by %s\n",
        id_to_str(block->attr.block.graph_arr[pos]->op->name));  */
  }

  return res;
}

/* This function returns the last definition of a variable.  In case
   this variable was last defined in a previous block, Phi nodes are
   inserted.  If the part of the firm graph containing the definition
   is not yet constructed, a dummy Phi node is returned. */
inline ir_node *
get_r_value_internal (ir_node *block, int pos, ir_mode *mode)
{
  ir_node *res;
  /* There are 4 cases to treat.

     1. The block is not mature and we visit it the first time.  We can not
        create a proper Phi node, therefore a Phi0, i.e., a Phi without
        predecessors is returned.  This node is added to the linked list (field
        "link") of the containing block to be completed when this block is
        matured. (Comlpletion will add a new Phi and turn the Phi0 into an Id
        node.)

     2. The value is already known in this block, graph_arr[pos] is set and we
        visit the block the first time.  We can return the value without
        creating any new nodes.

     3. The block is mature and we visit it the first time.  A Phi node needs
        to be created (phi_merge).  If the Phi is not needed, as all it's
        operands are the same value reaching the block through different
        paths, it's optimized away and the value itself is returned.

     4. The block is mature, and we visit it the second time.  Now two
        subcases are possible:
        * The value was computed completely the last time we were here. This
          is the case if there is no loop.  We can return the proper value.
        * The recursion that visited this node and set the flag did not
          return yet.  We are computing a value in a loop and need to
          break the recursion without knowing the result yet.
	  @@@ strange case.  Straight forward we would create a Phi before
	  starting the computation of it's predecessors.  In this case we will
	  find a Phi here in any case.  The problem is that this implementation
	  only creates a Phi after computing the predecessors, so that it is
	  hard to compute self references of this Phi.  @@@
        There is no simple check for the second subcase.  Therefore we check
        for a second visit and treat all such cases as the second subcase.
        Anyways, the basic situation is the same:  we reached a block
        on two paths without finding a definition of the value:  No Phi
        nodes are needed on both paths.
        We return this information "Two paths, no Phi needed" by a very tricky
        implementation that relies on the fact that an obstack is a stack and
        will return a node with the same address on different allocations.
        Look also at phi_merge and new_r_phi_in to understand this.
	@@@ Unfortunately this does not work, see testprogram
	three_cfpred_example.

  */

  /* case 4 -- already visited. */
  if (get_irn_visited(block) == get_irg_visited(current_ir_graph)) return NULL;

  /* visited the first time */
  set_irn_visited(block, get_irg_visited(current_ir_graph));

  /* Get the local valid value */
  res = block->attr.block.graph_arr[pos];

  /* case 2 -- If the value is actually computed, return it. */
  if (res) { return res;};

  if (block->attr.block.matured) { /* case 3 */

    /* The Phi has the same amount of ins as the corresponding block. */
    int ins = get_irn_arity(block);
    ir_node **nin;
    NEW_ARR_A (ir_node *, nin, ins);

    /* Phi merge collects the predecessors and then creates a node. */
    res = phi_merge (block, pos, mode, nin, ins);

  } else {  /* case 1 */
    /* The block is not mature, we don't know how many in's are needed.  A Phi
       with zero predecessors is created.  Such a Phi node is called Phi0
       node.  (There is also an obsolete Phi0 opcode.) The Phi0 is then added
       to the list of Phi0 nodes in this block to be matured by mature_block
       later.
       The Phi0 has to remember the pos of it's internal value.  If the real
       Phi is computed, pos is used to update the array with the local
       values. */

    res = new_r_Phi0 (current_ir_graph, block, mode);
    res->attr.phi0_pos = pos;
    res->link = block->link;
    block->link = res;
  }

  /* If we get here, the frontend missed a use-before-definition error */
  if (!res) {
    /* Error Message */
    printf("Error: no value set.  Use of undefined variable.  Initializing
            to zero.\n");
    assert (mode->code >= irm_f && mode->code <= irm_p);
    res = new_r_Const (current_ir_graph, block, mode,
		       tarval_mode_null[mode->code]);
  }

  /* The local valid value is available now. */
  block->attr.block.graph_arr[pos] = res;

  return res;
}

#else /* if 0 */

/** This is the simple algorithm.  If first generates a Phi0, then
    it starts the recursion.  This causes an Id at the entry of
    every block that has no definition of the value! **/

#if USE_EXPICIT_PHI_IN_STACK
/* Just dummies */
Phi_in_stack * new_Phi_in_stack() {  return NULL; }
void free_Phi_in_stack(Phi_in_stack *s) { }
#endif

inline ir_node *
new_r_Phi_in (ir_graph *irg, ir_node *block, ir_mode *mode,
	      ir_node **in, int ins)
{
  int i;
  ir_node *res, *known;

  /* Allocate a new node on the obstack.  The allocation copies the in
     array. */
  res = new_ir_node (irg, block, op_Phi, mode, ins, in);

  /* This loop checks whether the Phi has more than one predecessor.
     If so, it is a real Phi node and we break the loop.  Else the
     Phi node merges the same definition on several paths and therefore
     is not needed. Don't consider Bad nodes! */
  known = res;
  for (i=0;  i < ins;  ++i)
  {
    if (in[i]==res || in[i]==known || is_Bad(in[i])) continue;

    if (known==res)
      known = in[i];
    else
      break;
  }

  /* i==ins: there is at most one predecessor, we don't need a phi node. */
  if (i==ins) {
    if (res != known) {
      obstack_free (current_ir_graph->obst, res);
      res = known;
    } else {
      /* A undefined value, e.g., in unreachable code. */
      res = new_Bad();
    }
  } else {
    res = optimize (res);
    irn_vrfy (res);
  }

  return res;
}

inline ir_node *
get_r_value_internal (ir_node *block, int pos, ir_mode *mode);

/** This function allocates a dummy Phi node to break recursions,
    computes the predecessors for the real phi node, and then
    allocates and returns this node.  The routine called to allocate the
    node might optimize it away and return a real value.
    This function is called with an in-array of proper size. **/
static inline ir_node *
phi_merge (ir_node *block, int pos, ir_mode *mode, ir_node **nin, int ins)
{
  ir_node *prevBlock, *res, *phi0;
  int i;


  /* If this block has no value at pos create a Phi0 and remember it
     in graph_arr to break recursions. */
  phi0 = NULL;
  if (!block->attr.block.graph_arr[pos]) {
    /* This is commented out as collapsing to Bads is no good idea.
       Either we need an assert here, or we need to call a routine
       that deals with this case as appropriate for the given language.
       Right now a self referencing Id is created which will crash irg_vryfy().

       Even if all variables are defined before use, it can happen that
       we get to the start block, if a cond has been replaced by a tuple
       (bad, jmp).  As the start has a self referencing control flow edge,
       we get a self referencing Id, which is hard to optimize away.  We avoid
       this by defining the value as a Bad node.
       Returning a const with tarval_bad is a preliminary solution.  In some
       situations we might want a Warning or an Error. */

    if (block == get_irg_start_block(current_ir_graph)) {
      block->attr.block.graph_arr[pos] = new_Const(mode, tarval_bad);
      return block->attr.block.graph_arr[pos];
      } else  {
      phi0 = new_r_Phi0(current_ir_graph, block, mode);
      block->attr.block.graph_arr[pos] = phi0;
    }
  }

  /* This loop goes to all predecessor blocks of the block the Phi node
     is in and there finds the operands of the Phi node by calling
     get_r_value_internal.  */
  for (i = 1;  i <= ins;  ++i) {
    assert (block->in[i]);
    if (is_Bad(block->in[i])) {
      /* In case a Cond has been optimized we would get right to the start block
	 with an invalid definition. */
      nin[i-1] = new_Bad();
      continue;
    }
    prevBlock = block->in[i]->in[0]; /* go past control flow op to prev block */
    assert (prevBlock);
    if (!is_Bad(prevBlock)) {
      nin[i-1] = get_r_value_internal (prevBlock, pos, mode);
    } else {
      nin[i-1] = new_Bad();
    }
  }

  /* After collecting all predecessors into the array nin a new Phi node
     with these predecessors is created.  This constructor contains an
     optimization: If all predecessors of the Phi node are identical it
     returns the only operand instead of a new Phi node.  */
  res = new_r_Phi_in (current_ir_graph, block, mode, nin, ins);

  /* In case we allocated a Phi0 node at the beginning of this procedure,
     we need to exchange this Phi0 with the real Phi. */
  if (phi0) {
    exchange(phi0, res);
    block->attr.block.graph_arr[pos] = res;
  }

  return res;
}

/* This function returns the last definition of a variable.  In case
   this variable was last defined in a previous block, Phi nodes are
   inserted.  If the part of the firm graph containing the definition
   is not yet constructed, a dummy Phi node is returned. */
inline ir_node *
get_r_value_internal (ir_node *block, int pos, ir_mode *mode)
{
  ir_node *res;
  /* There are 4 cases to treat.

     1. The block is not mature and we visit it the first time.  We can not
        create a proper Phi node, therefore a Phi0, i.e., a Phi without
        predecessors is returned.  This node is added to the linked list (field
        "link") of the containing block to be completed when this block is
        matured. (Comlpletion will add a new Phi and turn the Phi0 into an Id
        node.)

     2. The value is already known in this block, graph_arr[pos] is set and we
        visit the block the first time.  We can return the value without
        creating any new nodes.

     3. The block is mature and we visit it the first time.  A Phi node needs
        to be created (phi_merge).  If the Phi is not needed, as all it's
        operands are the same value reaching the block through different
        paths, it's optimized away and the value itself is returned.

     4. The block is mature, and we visit it the second time.  Now two
        subcases are possible:
        * The value was computed completely the last time we were here. This
          is the case if there is no loop.  We can return the proper value.
        * The recursion that visited this node and set the flag did not
          return yet.  We are computing a value in a loop and need to
          break the recursion.  This case only happens if we visited
	  the same block with phi_merge before, which inserted a Phi0.
	  So we return the Phi0.
  */

  /* case 4 -- already visited. */
  if (get_irn_visited(block) == get_irg_visited(current_ir_graph)) {
    /* As phi_merge allocates a Phi0 this value is always defined. Here
     is the critical difference of the two algorithms. */
    assert(block->attr.block.graph_arr[pos]);
    return block->attr.block.graph_arr[pos];
  }

  /* visited the first time */
  set_irn_visited(block, get_irg_visited(current_ir_graph));

  /* Get the local valid value */
  res = block->attr.block.graph_arr[pos];

  /* case 2 -- If the value is actually computed, return it. */
  if (res) { return res; };

  if (block->attr.block.matured) { /* case 3 */

    /* The Phi has the same amount of ins as the corresponding block. */
    int ins = get_irn_arity(block);
    ir_node **nin;
    NEW_ARR_A (ir_node *, nin, ins);

    /* Phi merge collects the predecessors and then creates a node. */
    res = phi_merge (block, pos, mode, nin, ins);

  } else {  /* case 1 */
    /* The block is not mature, we don't know how many in's are needed.  A Phi
       with zero predecessors is created.  Such a Phi node is called Phi0
       node.  The Phi0 is then added to the list of Phi0 nodes in this block
       to be matured by mature_block later.
       The Phi0 has to remember the pos of it's internal value.  If the real
       Phi is computed, pos is used to update the array with the local
       values. */
    res = new_r_Phi0 (current_ir_graph, block, mode);
    res->attr.phi0_pos = pos;
    res->link = block->link;
    block->link = res;
  }

  /* If we get here, the frontend missed a use-before-definition error */
  if (!res) {
    /* Error Message */
    printf("Error: no value set.  Use of undefined variable.  Initializing
            to zero.\n");
    assert (mode->code >= irm_f && mode->code <= irm_p);
    res = new_r_Const (current_ir_graph, block, mode,
		       tarval_mode_null[mode->code]);
  }

  /* The local valid value is available now. */
  block->attr.block.graph_arr[pos] = res;

  return res;
}

#endif /* USE_FAST_PHI_CONSTRUCTION */

/* ************************************************************************** */

/** Finalize a Block node, when all control flows are known.  */
/** Acceptable parameters are only Block nodes.               */
void
mature_block (ir_node *block)
{

  int ins;
  ir_node *n, **nin;
  ir_node *next;

  assert (get_irn_opcode(block) == iro_Block);

  if (!get_Block_matured(block)) {

    /* turn the dynamic in-array into a static one. */
    ins = ARR_LEN (block->in)-1;
    NEW_ARR_A (ir_node *, nin, ins);
    /*  @@@ something is strange here... why isn't the array copied? */

    /* Traverse a chain of Phi nodes attached to this block and mature
       these, too. **/
    for (n = block->link;  n;  n=next) {
      inc_irg_visited(current_ir_graph);
      next = n->link;
      exchange (n, phi_merge (block, n->attr.phi0_pos, n->mode, nin, ins));
    }

    block->attr.block.matured = 1;

    /* Now, as the block is a finished firm node, we can optimize it.
       Since other nodes have been allocated since the block was created
       we can not free the node on the obstack.  Therefore we have to call
       optimize_in_place.
       Unfortunately the optimization does not change a lot, as all allocated
       nodes refer to the unoptimized node. */
    block = optimize_in_place(block);
    irn_vrfy(block);
  }
}

ir_node *
new_Phi (int arity, ir_node **in, ir_mode *mode)
{
  return new_r_Phi (current_ir_graph, current_ir_graph->current_block,
		    arity, in, mode);
}

ir_node *
new_Const (ir_mode *mode, tarval *con)
{
  return new_r_Const (current_ir_graph, current_ir_graph->start_block,
		      mode, con);
}

ir_node *
new_Id (ir_node *val, ir_mode *mode)
{
  return new_r_Id (current_ir_graph, current_ir_graph->current_block,
		   val, mode);
}

ir_node *
new_Proj (ir_node *arg, ir_mode *mode, long proj)
{
  return new_r_Proj (current_ir_graph, current_ir_graph->current_block,
		     arg, mode, proj);
}

ir_node *
new_defaultProj (ir_node *arg, long max_proj)
{
  ir_node *res;
  assert((arg->op==op_Cond) && (get_irn_mode(arg->in[1]) == mode_I));
  arg->attr.c.kind = fragmentary;
  arg->attr.c.default_proj = max_proj;
  res = new_Proj (arg, mode_X, max_proj);
  return res;
}

ir_node *
new_Conv (ir_node *op, ir_mode *mode)
{
  return new_r_Conv (current_ir_graph, current_ir_graph->current_block,
		     op, mode);
}

ir_node *
new_Tuple (int arity, ir_node **in)
{
  return new_r_Tuple (current_ir_graph, current_ir_graph->current_block,
		      arity, in);
}

ir_node *
new_Add (ir_node *op1, ir_node *op2, ir_mode *mode)
{
  return new_r_Add (current_ir_graph, current_ir_graph->current_block,
		    op1, op2, mode);
}

ir_node *
new_Sub (ir_node *op1, ir_node *op2, ir_mode *mode)
{
  return new_r_Sub (current_ir_graph, current_ir_graph->current_block,
		    op1, op2, mode);
}


ir_node *
new_Minus  (ir_node *op,  ir_mode *mode)
{
  return new_r_Minus (current_ir_graph, current_ir_graph->current_block,
		      op, mode);
}

ir_node *
new_Mul (ir_node *op1, ir_node *op2, ir_mode *mode)
{
  return new_r_Mul (current_ir_graph, current_ir_graph->current_block,
		    op1, op2, mode);
}

ir_node *
new_Quot (ir_node *memop, ir_node *op1, ir_node *op2)
{
  return new_r_Quot (current_ir_graph, current_ir_graph->current_block,
		     memop, op1, op2);
}

ir_node *
new_DivMod (ir_node *memop, ir_node *op1, ir_node *op2)
{
  return new_r_DivMod (current_ir_graph, current_ir_graph->current_block,
		       memop, op1, op2);
}

ir_node *
new_Div (ir_node *memop, ir_node *op1, ir_node *op2)
{
  return new_r_Div (current_ir_graph, current_ir_graph->current_block,
		    memop, op1, op2);
}

ir_node *
new_Mod (ir_node *memop, ir_node *op1, ir_node *op2)
{
  return new_r_Mod (current_ir_graph, current_ir_graph->current_block,
		    memop, op1, op2);
}

ir_node *
new_And (ir_node *op1, ir_node *op2, ir_mode *mode)
{
  return new_r_And (current_ir_graph, current_ir_graph->current_block,
		    op1, op2, mode);
}

ir_node *
new_Or (ir_node *op1, ir_node *op2, ir_mode *mode)
{
  return new_r_Or (current_ir_graph, current_ir_graph->current_block,
		   op1, op2, mode);
}

ir_node *
new_Eor (ir_node *op1, ir_node *op2, ir_mode *mode)
{
  return new_r_Eor (current_ir_graph, current_ir_graph->current_block,
		    op1, op2, mode);
}

ir_node *
new_Not (ir_node *op, ir_mode *mode)
{
  return new_r_Not (current_ir_graph, current_ir_graph->current_block,
		    op, mode);
}

ir_node *
new_Shl (ir_node *op, ir_node *k, ir_mode *mode)
{
  return new_r_Shl (current_ir_graph, current_ir_graph->current_block,
		    op, k, mode);
}

ir_node *
new_Shr (ir_node *op, ir_node *k, ir_mode *mode)
{
  return new_r_Shr (current_ir_graph, current_ir_graph->current_block,
		    op, k, mode);
}

ir_node *
new_Shrs (ir_node *op, ir_node *k, ir_mode *mode)
{
  return new_r_Shrs (current_ir_graph, current_ir_graph->current_block,
		     op, k, mode);
}

ir_node *
new_Rotate (ir_node *op, ir_node *k, ir_mode *mode)
{
  return new_r_Rot (current_ir_graph, current_ir_graph->current_block,
		     op, k, mode);
}

ir_node *
new_Abs (ir_node *op, ir_mode *mode)
{
  return new_r_Abs (current_ir_graph, current_ir_graph->current_block,
		    op, mode);
}

ir_node *
new_Cmp (ir_node *op1, ir_node *op2)
{
  return new_r_Cmp (current_ir_graph, current_ir_graph->current_block,
		    op1, op2);
}

ir_node *
new_Jmp (void)
{
  return new_r_Jmp (current_ir_graph, current_ir_graph->current_block);
}

ir_node *
new_Cond (ir_node *c)
{
  return new_r_Cond (current_ir_graph, current_ir_graph->current_block, c);
}

ir_node *
new_Call (ir_node *store, ir_node *callee, int arity, ir_node **in,
	  type *type)
{
  return new_r_Call (current_ir_graph, current_ir_graph->current_block,
		     store, callee, arity, in, type);
}

ir_node *
new_Return (ir_node* store, int arity, ir_node **in)
{
  return new_r_Return (current_ir_graph, current_ir_graph->current_block,
		       store, arity, in);
}

ir_node *
new_Raise (ir_node *store, ir_node *obj)
{
  return new_r_Raise (current_ir_graph, current_ir_graph->current_block,
		      store, obj);
}

ir_node *
new_Load (ir_node *store, ir_node *addr)
{
  return new_r_Load (current_ir_graph, current_ir_graph->current_block,
		     store, addr);
}

ir_node *
new_Store (ir_node *store, ir_node *addr, ir_node *val)
{
  return new_r_Store (current_ir_graph, current_ir_graph->current_block,
		      store, addr, val);
}

ir_node *
new_Alloc (ir_node *store, ir_node *size, type *alloc_type,
           where_alloc where)
{
  return new_r_Alloc (current_ir_graph, current_ir_graph->current_block,
		      store, size, alloc_type, where);
}

ir_node *
new_Free (ir_node *store, ir_node *ptr, ir_node *size, type *free_type)
{
  return new_r_Free (current_ir_graph, current_ir_graph->current_block,
		     store, ptr, size, free_type);
}

ir_node *
new_simpleSel (ir_node *store, ir_node *objptr, entity *ent)
/* GL: objptr was called frame before.  Frame was a bad choice for the name
   as the operand could as well be a pointer to a dynamic object. */
{
  return new_r_Sel (current_ir_graph, current_ir_graph->current_block,
		    store, objptr, 0, NULL, ent);
}

ir_node *
new_Sel (ir_node *store, ir_node *objptr, int n_index, ir_node **index, entity *sel)
{
  return new_r_Sel (current_ir_graph, current_ir_graph->current_block,
		    store, objptr, n_index, index, sel);
}

ir_node *
new_SymConst (type_or_id_p value, symconst_kind kind)
{
  return new_r_SymConst (current_ir_graph, current_ir_graph->current_block,
                         value, kind);
}

ir_node *
new_Sync (int arity, ir_node** in)
{
  return new_r_Sync (current_ir_graph, current_ir_graph->current_block,
		     arity, in);
}


ir_node *
new_Bad (void)
{
  return current_ir_graph->bad;
}

/* ********************************************************************* */
/* Comfortable interface with automatic Phi node construction.           */
/* (Uses also constructors of ?? interface, except new_Block.            */
/* ********************************************************************* */

/** Block construction **/
/* immature Block without predecessors */
ir_node *new_immBlock (void) {
  ir_node *res;

  /* creates a new dynamic in-array as length of in is -1 */
  res = new_ir_node (current_ir_graph, NULL, op_Block, mode_R, -1, NULL);
  current_ir_graph->current_block = res;
  res->attr.block.matured = 0;
  set_Block_block_visited(res, 0);

  /* Create and initialize array for Phi-node construction. */
  res->attr.block.graph_arr = NEW_ARR_D (ir_node *, current_ir_graph->obst,
                                         current_ir_graph->n_loc);
  memset(res->attr.block.graph_arr, 0, sizeof(ir_node *)*current_ir_graph->n_loc);

  /* Immature block may not be optimized! */
  irn_vrfy (res);

  return res;
}

/* add an adge to a jmp/control flow node */
void
add_in_edge (ir_node *block, ir_node *jmp)
{
  if (block->attr.block.matured) {
    printf("Error: Block already matured!\n");
  }
  else {
    assert (jmp != NULL);
    ARR_APP1 (ir_node *, block->in, jmp);
  }
}

/* changing the current block */
void
switch_block (ir_node *target)
{
  current_ir_graph->current_block = target;
}

/* ************************ */
/* parameter administration */

/* get a value from the parameter array from the current block by its index */
ir_node *
get_value (int pos, ir_mode *mode)
{
  inc_irg_visited(current_ir_graph);
  return get_r_value_internal (current_ir_graph->current_block, pos + 1, mode);
}


/* set a value at position pos in the parameter array from the current block */
inline void
set_value (int pos, ir_node *value)
{
  current_ir_graph->current_block->attr.block.graph_arr[pos + 1] = value;
}

/* get the current store */
inline ir_node *
get_store (void)
{
  /* GL: one could call get_value instead */
  inc_irg_visited(current_ir_graph);
  return get_r_value_internal (current_ir_graph->current_block, 0, mode_M);
}

/* set the current store */
inline void
set_store (ir_node *store)
{
  /* GL: one could call set_value instead */
  current_ir_graph->current_block->attr.block.graph_arr[0] = store;
}

/* ********************************************************************* */
/* initialize */

/* call once for each run of the library */
void
init_cons (void)
{
}
