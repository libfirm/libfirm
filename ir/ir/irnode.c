/*
 * Project:     libFIRM
 * File name:   ir/ir/irnode.c
 * Purpose:     Representation of an intermediate operation.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <string.h>

#include "ident.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "typegmod_t.h"
#include "array.h"
#include "irbackedge_t.h"
#include "irdump.h"
#include "irflag.h"

#ifdef DEBUG_libfirm
#include "irprog_t.h"
#endif

/* some constants fixing the positions of nodes predecessors
   in the in array */
#define CALL_PARAM_OFFSET 2
#define SEL_INDEX_OFFSET 2
#define RETURN_RESULT_OFFSET 1  /* mem is not a result */
#define END_KEEPALIVE_OFFSET 0

/* Declarations for inlineing */
INLINE ir_node ** get_irn_in (const ir_node *node);
INLINE ir_mode *get_irn_mode (const ir_node *node);
INLINE ir_op *get_irn_op (const ir_node *node);
INLINE opcode get_irn_opcode (const ir_node *node);
INLINE ident *get_irn_opident (const ir_node *node);
INLINE type *get_SymConst_type (ir_node *node);
INLINE ir_node *skip_nop (ir_node *node);
INLINE int is_Proj (const ir_node *node);


static const char *pnc_name_arr [] = {
  "False", "Eq", "Lt", "Le",
  "Gt", "Ge", "Lg", "Leg", "Uo",
  "Ue", "Ul", "Ule", "Ug", "Uge",
  "Ne", "True"
};

INLINE const char *get_pnc_string(int pnc) {
  return pnc_name_arr[pnc];
}


int
get_negated_pnc(int pnc) {
  switch (pnc) {
  case False: return True;  break;
  case Eq:    return Ne;    break;
  case Lt:    return Uge;   break;
  case Le:    return Ug;    break;
  case Gt:    return Ule;   break;
  case Ge:    return Ul;    break;
  case Lg:    return Ue;    break;
  case Leg:   return Uo;    break;
  case Uo:    return Leg;   break;
  case Ue:    return Lg;    break;
  case Ul:    return Ge;    break;
  case Ule:   return Gt;    break;
  case Ug:    return Le;    break;
  case Uge:   return Lt;    break;
  case Ne:    return Eq;    break;
  case True:  return False; break;
  }
  return 99; /* to shut up gcc */
}

const char *pns_name_arr [] = {
  "initial_exec", "global_store",
  "frame_base", "globals", "args"
};

const char *symconst_name_arr [] = {
  "type_tag", "size", "linkage_ptr_info"
};

void
init_irnode (void)
{
}

/* irnode constructor                                             */
/* create a new irnode in irg, with an op, mode, arity and        */
/* some incoming irnodes                                          */
/* this constructor is used in every specified irnode constructor */
INLINE ir_node *
new_ir_node (dbg_info *db, ir_graph *irg, ir_node *block, ir_op *op, ir_mode *mode,
	     int arity, ir_node **in)
{
  ir_node *res;
  int node_size = offsetof (ir_node, attr) +  op->attr_size;

  res = (ir_node *) obstack_alloc (irg->obst, node_size);

  res->kind = k_ir_node;
  res->op = op;
  res->mode = mode;
  res->visited = 0;
  res->link = NULL;
  if (arity < 0) {
    res->in = NEW_ARR_F (ir_node *, 1);  /* 1: space for block */
  } else {
    res->in = NEW_ARR_D (ir_node *, irg->obst, (arity+1));
    memcpy (&res->in[1], in, sizeof (ir_node *) * arity);
  }
  res->in[0] = block;
  set_irn_dbg_info(res, db);
  res->out = NULL;

#ifdef DEBUG_libfirm
  res->node_nr = get_irp_new_node_nr();
#endif

  return res;
}

/* Copies all attributes stored in the old node to the new node.
   Assumes both have the same opcode and sufficient size. */
void
copy_attrs (ir_node *old, ir_node *new) {
  assert (get_irn_op(old) == get_irn_op(new));
  memcpy (&new->attr, &old->attr, get_op_attr_size(get_irn_op(old)));
}

/** getting some parameters from ir_nodes **/

int
is_ir_node (void *thing) {
  assert(thing);
  if (get_kind(thing) == k_ir_node)
    return 1;
  else
    return 0;
}

/* returns the number of predecessors without the block predecessor. */
INLINE int
get_irn_arity (const ir_node *node) {
  assert(node);
  if (interprocedural_view) { /* handle Filter and Block specially */
    if (get_irn_opcode(node) == iro_Filter) {
      assert(node->attr.filter.in_cg);
      return ARR_LEN(node->attr.filter.in_cg) - 1;
    } else if (get_irn_opcode(node) == iro_Block && node->attr.block.in_cg) {
      return ARR_LEN(node->attr.block.in_cg) - 1;
    }
    /* else fall through */
  }
  return ARR_LEN(node->in) - 1;
}

/* Returns the array with ins. This array is shifted with respect to the
   array accessed by get_irn_n: The block operand is at position 0 not -1.
   (@@@ This should be changed.)
   The order of the predecessors in this array is not guaranteed, except that
   lists of operands as predecessors of Block or arguments of a Call are
   consecutive. */
INLINE ir_node **
get_irn_in (const ir_node *node) {
  assert(node);
  if (interprocedural_view) { /* handle Filter and Block specially */
    if (get_irn_opcode(node) == iro_Filter) {
      assert(node->attr.filter.in_cg);
      return node->attr.filter.in_cg;
    } else if (get_irn_opcode(node) == iro_Block && node->attr.block.in_cg) {
      return node->attr.block.in_cg;
    }
    /* else fall through */
  }
  return node->in;
}

INLINE void
set_irn_in (ir_node *node, int arity, ir_node **in) {
  ir_node *** arr;
  assert(node);
  if (interprocedural_view) { /* handle Filter and Block specially */
    if (get_irn_opcode(node) == iro_Filter) {
      assert(node->attr.filter.in_cg);
      arr = &node->attr.filter.in_cg;
    } else if (get_irn_opcode(node) == iro_Block && node->attr.block.in_cg) {
      arr = &node->attr.block.in_cg;
    } else {
      arr = &node->in;
    }
  } else {
    arr = &node->in;
  }
  if (arity != ARR_LEN(*arr) - 1) {
    ir_node * block = (*arr)[0];
    *arr = NEW_ARR_D(ir_node *, current_ir_graph->obst, arity + 1);
    (*arr)[0] = block;
  }
  fix_backedges(current_ir_graph->obst, node);
  memcpy((*arr) + 1, in, sizeof(ir_node *) * arity);
}

/* to iterate through the predecessors without touching the array */
/* To iterate over the operands iterate from 0 to i < get_irn_arity(),
   to iterate includind the Block predecessor iterate from i = -1 to
   i < get_irn_arity.
   If it is a block, the entry -1 is NULL. */
INLINE ir_node *
get_irn_n (ir_node *node, int n) {
  /* debug @@@
  if (-1 > n || get_irn_arity(node) <= n) {
    printf("pos: %d, arity: %d ", n, get_irn_arity(node));
    DDMN(node);
    } */
  assert(node); assert(-1 <= n && n < get_irn_arity(node));
  if (interprocedural_view) { /* handle Filter and Block specially */
    if (get_irn_opcode(node) == iro_Filter) {
      assert(node->attr.filter.in_cg);
      return (node->attr.filter.in_cg[n + 1] = skip_nop(node->attr.filter.in_cg[n + 1]));
    } else if (get_irn_opcode(node) == iro_Block && node->attr.block.in_cg) {
      return (node->attr.block.in_cg[n + 1] = skip_nop(node->attr.block.in_cg[n + 1]));
    }
    /* else fall through */
  }
  return (node->in[n + 1] = skip_nop(node->in[n + 1]));
}

INLINE void
set_irn_n (ir_node *node, int n, ir_node *in) {
  assert(node && -1 <= n && n < get_irn_arity(node));
  if ((n == -1) && (get_irn_opcode(node) == iro_Filter)) {
    /* Change block pred in both views! */
    node->in[n + 1] = in;
    assert(node->attr.filter.in_cg);
    node->attr.filter.in_cg[n + 1] = in;
    return;
  }
  if (interprocedural_view) { /* handle Filter and Block specially */
    if (get_irn_opcode(node) == iro_Filter) {
      assert(node->attr.filter.in_cg);
      node->attr.filter.in_cg[n + 1] = in;
      return;
    } else if (get_irn_opcode(node) == iro_Block && node->attr.block.in_cg) {
      node->attr.block.in_cg[n + 1] = in;
      return;
    }
    /* else fall through */
  }
  node->in[n + 1] = in;
}

INLINE ir_mode *
get_irn_mode (const ir_node *node)
{
  assert (node);
  return node->mode;
}

INLINE void
set_irn_mode (ir_node *node, ir_mode *mode)
{
  assert (node);
  node->mode=mode;
  return;
}

INLINE modecode
get_irn_modecode (const ir_node *node)
{
  assert (node);
  return node->mode->code;
}


INLINE ident *
get_irn_modeident (const ir_node *node)
{
  assert(node);
  return get_mode_ident(node->mode);
}

INLINE ir_op *
get_irn_op (const ir_node *node)
{
  assert (node);
  return node->op;
}

/* should be private to the library: */
INLINE void
set_irn_op (ir_node *node, ir_op *op)
{
  assert (node);
  node->op = op;
}

INLINE opcode
get_irn_opcode (const ir_node *node)
{
  assert (node);
  assert (k_ir_node == get_kind(node));
  assert (node -> op);
  return node->op->code;
}

INLINE const char *
get_irn_opname (const ir_node *node)
{
  assert(node);
  return get_id_str(node->op->name);
}

INLINE ident *
get_irn_opident (const ir_node *node)
{
  assert(node);
  return node->op->name;
}

INLINE unsigned long
get_irn_visited (const ir_node *node)
{
  assert (node);
  return node->visited;
}

INLINE void
set_irn_visited (ir_node *node, unsigned long visited)
{
  assert (node);
  node->visited = visited;
}

INLINE void
mark_irn_visited (ir_node *node) {
  assert (node);
  node->visited = current_ir_graph->visited;
}

INLINE int
irn_not_visited  (const ir_node *node) {
  assert (node);
  return (node->visited < current_ir_graph->visited);
}

INLINE int
irn_visited  (const ir_node *node) {
  assert (node);
  return (node->visited >= current_ir_graph->visited);
}

INLINE void
set_irn_link (ir_node *node, void *link) {
  assert (node);
  /* Link field is used for Phi construction and various optimizations
     in iropt. */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);

  node->link = link;
}

INLINE void *
get_irn_link (const ir_node *node) {
  assert (node);
  return node->link;
}

/* Outputs a unique number for this node */
INLINE long
get_irn_node_nr(const ir_node *node) {
  assert(node);
#ifdef DEBUG_libfirm
  return node->node_nr;
#else
  return 0;
#endif
}

INLINE tarval *
get_irn_const_attr (ir_node *node)
{
  assert (node->op == op_Const);
  return node->attr.con;
}

INLINE long
get_irn_proj_attr (ir_node *node)
{
  assert (node->op == op_Proj);
  return node->attr.proj;
}

INLINE alloc_attr
get_irn_alloc_attr (ir_node *node)
{
  assert (node->op == op_Alloc);
  return node->attr.a;
}

INLINE type *
get_irn_free_attr     (ir_node *node)
{
  assert (node->op == op_Free);
  return node->attr.f = skip_tid(node->attr.f);
}

INLINE symconst_attr
get_irn_symconst_attr (ir_node *node)
{
  assert (node->op == op_SymConst);
  return node->attr.i;
}

INLINE type *
get_irn_call_attr (ir_node *node)
{
  assert (node->op == op_Call);
  return node->attr.call.cld_tp = skip_tid(node->attr.call.cld_tp);
}

INLINE sel_attr
get_irn_sel_attr (ir_node *node)
{
  assert (node->op == op_Sel);
  return node->attr.s;
}

INLINE int
get_irn_phi_attr (ir_node *node)
{
  assert (node->op == op_Phi);
  return node->attr.phi0_pos;
}

INLINE block_attr
get_irn_block_attr (ir_node *node)
{
  assert (node->op == op_Block);
  return node->attr.block;
}

/** manipulate fields of individual nodes **/

/* this works for all except Block */
ir_node *
get_nodes_Block (ir_node *node) {
  assert (!(node->op == op_Block));
  return get_irn_n(node, -1);
}

INLINE void
set_nodes_Block (ir_node *node, ir_node *block) {
  assert (!(node->op == op_Block));
  set_irn_n(node, -1, block);
}

/* Returns an array with the predecessors of the Block. Depending on
   the implementation of the graph datastructure this can be a copy of
   the internal representation of predecessors as well as the internal
   array itself. Therefore writing to this array might obstruct the ir. */
INLINE ir_node **
get_Block_cfgpred_arr (ir_node *node)
{
  assert ((node->op == op_Block));
  return (ir_node **)&(get_irn_in(node)[1]);
}


INLINE int
get_Block_n_cfgpreds (ir_node *node) {
  assert ((node->op == op_Block));
  return (get_irn_arity(node));
}

INLINE ir_node *
get_Block_cfgpred (ir_node *node, int pos) {
  assert (node->op == op_Block);
  /* debug @@@
  if (-1 > pos || get_irn_arity(node) <= pos) {
    dump_ir_block_graph(current_ir_graph);
    printf("pos: %d, arity: %d ", pos, get_irn_arity(node));
    DDMN(node);
    } */
  assert(node); assert(-1 <= pos && pos < get_irn_arity(node));
  return get_irn_n(node, pos);
}

INLINE void
set_Block_cfgpred (ir_node *node, int pos, ir_node *pred) {
  assert (node->op == op_Block);
  set_irn_n(node, pos, pred);
}

INLINE bool
get_Block_matured (ir_node *node) {
  assert (node->op == op_Block);
  return node->attr.block.matured;
}

INLINE void
set_Block_matured (ir_node *node, bool matured) {
  assert (node->op == op_Block);
  node->attr.block.matured = matured;
}
INLINE unsigned long
get_Block_block_visited (ir_node *node) {
  assert (node->op == op_Block);
  return node->attr.block.block_visited;
}

INLINE void
set_Block_block_visited (ir_node *node, unsigned long visit) {
  assert (node->op == op_Block);
  node->attr.block.block_visited = visit;
}

/* For this current_ir_graph must be set. */
INLINE void
mark_Block_block_visited (ir_node *node) {
  assert (node->op == op_Block);
  node->attr.block.block_visited = get_irg_block_visited(current_ir_graph);
}

INLINE int
Block_not_block_visited(ir_node *node) {
  assert (node->op == op_Block);
  return (node->attr.block.block_visited < get_irg_block_visited(current_ir_graph));
}

INLINE ir_node *
get_Block_graph_arr (ir_node *node, int pos) {
  assert (node->op == op_Block);
  return node->attr.block.graph_arr[pos+1];
}

INLINE void
set_Block_graph_arr (ir_node *node, int pos, ir_node *value) {
  assert (node->op == op_Block);
  node->attr.block.graph_arr[pos+1] = value;
}

/* handler handling for Blocks */
void
set_Block_handler (ir_node *block, ir_node *handler)  {
  assert ((block->op == op_Block));
  assert ((handler->op == op_Block));
  block->attr.block.handler_entry = handler;
}

ir_node *
get_Block_handler (ir_node *block) {
  assert ((block->op == op_Block));
  return (block->attr.block.handler_entry);
}

/* handler handling for Nodes */
void
set_Node_handler (ir_node *node, ir_node *handler) {
  set_Block_handler (get_nodes_Block (node), handler);
}

ir_node *
get_Node_handler (ir_node *node) {
  return (get_Block_handler (get_nodes_Block (node)));
}

/* exc_t handling for Blocks */
void set_Block_exc (ir_node *block, exc_t exc) {
  assert ((block->op == op_Block));
  block->attr.block.exc = exc;
}

exc_t get_Block_exc (ir_node *block) {
  assert ((block->op == op_Block));

  return (block->attr.block.exc);
}

/* exc_t handling for Nodes */
void set_Node_exc (ir_node *node, exc_t exc) {
  set_Block_exc (get_nodes_Block (node), exc);
}

exc_t get_Node_exc (ir_node *node) {
  return (get_Block_exc (get_nodes_Block (node)));
}

void set_Block_cg_cfgpred_arr(ir_node * node, int arity, ir_node ** in) {
  assert(node->op == op_Block);
  if (node->attr.block.in_cg == NULL || arity != ARR_LEN(node->attr.block.in_cg) - 1) {
    node->attr.block.in_cg = NEW_ARR_D(ir_node *, current_ir_graph->obst, arity + 1);
    node->attr.block.in_cg[0] = NULL;
    node->attr.block.cg_backedge = new_backedge_arr(current_ir_graph->obst, arity);
    {
      /* Fix backedge array.  fix_backedges operates depending on
	 interprocedural_view. */
      bool ipv = interprocedural_view;
      interprocedural_view = true;
      fix_backedges(current_ir_graph->obst, node);
      interprocedural_view = ipv;
    }
  }
  memcpy(node->attr.block.in_cg + 1, in, sizeof(ir_node *) * arity);
}

void set_Block_cg_cfgpred(ir_node * node, int pos, ir_node * pred) {
  assert(node->op == op_Block &&
	 node->attr.block.in_cg &&
	 0 <= pos && pos < ARR_LEN(node->attr.block.in_cg) - 1);
  node->attr.block.in_cg[pos + 1] = pred;
}

ir_node ** get_Block_cg_cfgpred_arr(ir_node * node) {
  assert(node->op == op_Block);
  return node->attr.block.in_cg == NULL ? NULL : node->attr.block.in_cg  + 1;
}

int get_Block_cg_n_cfgpreds(ir_node * node) {
  assert(node->op == op_Block);
  return node->attr.block.in_cg == NULL ? 0 : ARR_LEN(node->attr.block.in_cg) - 1;
}

ir_node * get_Block_cg_cfgpred(ir_node * node, int pos) {
  assert(node->op == op_Block && node->attr.block.in_cg);
  return node->attr.block.in_cg[pos + 1];
}

void remove_Block_cg_cfgpred_arr(ir_node * node) {
  assert(node->op == op_Block);
  node->attr.block.in_cg = NULL;
}

/* Start references the irg it is in. */
INLINE ir_graph *
get_Start_irg(ir_node *node) {
  assert(node->op == op_Start);
  return node->attr.start.irg;
}

INLINE void
set_Start_irg(ir_node *node, ir_graph *irg) {
  assert(node->op == op_Start);
  assert(is_ir_graph(irg));
  node->attr.start.irg = irg;
}

INLINE int
get_End_n_keepalives(ir_node *end) {
  assert (end->op == op_End);
  return (get_irn_arity(end) - END_KEEPALIVE_OFFSET);
}

INLINE ir_node *
get_End_keepalive(ir_node *end, int pos) {
  assert (end->op == op_End);
  return get_irn_n(end, pos + END_KEEPALIVE_OFFSET);
}

INLINE void
add_End_keepalive (ir_node *end, ir_node *ka) {
  assert (end->op == op_End);
  ARR_APP1 (ir_node *, end->in, ka);
}

INLINE void
set_End_keepalive(ir_node *end, int pos, ir_node *ka) {
  assert (end->op == op_End);
  set_irn_n(end, pos + END_KEEPALIVE_OFFSET, ka);
}

INLINE void
free_End (ir_node *end) {
  assert (end->op == op_End);
  /* DEL_ARR_F(end->in);   GL @@@ tut nicht ! */
  end->in = NULL;   /* @@@ make sure we get an error if we use the
		       in array afterwards ... */
}

ir_graph *get_EndReg_irg (const ir_node *end) {
  assert (end->op == op_EndReg);
  return end->attr.end.irg;
}

ir_graph *get_EndExcept_irg  (const ir_node *end) {
  assert (end->op == op_EndReg);
  return end->attr.end.irg;
}

/*
> Implementing the case construct (which is where the constant Proj node is
> important) involves far more than simply determining the constant values.
> We could argue that this is more properly a function of the translator from
> Firm to the target machine.  That could be done if there was some way of
> projecting "default" out of the Cond node.
I know it's complicated.
Basically there are two proglems:
 - determining the gaps between the projs
 - determining the biggest case constant to konw the proj number for
   the default node.
I see several solutions:
1. Introduce a ProjDefault node.  Solves both problems.
   This means to extend all optimizations executed during construction.
2. Give the Cond node for switch two flavors:
   a) there are no gaps in the projs  (existing flavor)
   b) gaps may exist, default proj is still the Proj with the largest
      projection number.  This covers also the gaps.
3. Fix the semantic of the Cond to that of 2b)

Solution 2 seems to be the best:
Computing the gaps in the Firm representation is not too hard, i.e.,
libfirm can implement a routine that transforms betweeen the two
flavours.  This is also possible for 1) but 2) does not require to
change any existing optimization.
Further it should be far simpler to determine the biggest constant than
to compute all gaps.
I don't want to choose 3) as 2a) seems to have advantages for
dataflow analysis and 3) does not allow to convert the representation to
2a).
*/
INLINE ir_node *
get_Cond_selector (ir_node *node) {
  assert (node->op == op_Cond);
  return get_irn_n(node, 0);
}

INLINE void
set_Cond_selector (ir_node *node, ir_node *selector) {
  assert (node->op == op_Cond);
  set_irn_n(node, 0, selector);
}

INLINE cond_kind
get_Cond_kind (ir_node *node) {
  assert (node->op == op_Cond);
  return node->attr.c.kind;
}

INLINE void
set_Cond_kind (ir_node *node, cond_kind kind) {
  assert (node->op == op_Cond);
  node->attr.c.kind = kind;
}

INLINE ir_node *
get_Return_mem (ir_node *node) {
  assert (node->op == op_Return);
  return get_irn_n(node, 0);
}

INLINE void
set_Return_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Return);
  set_irn_n(node, 0, mem);
}

INLINE int
get_Return_n_ress (ir_node *node) {
  assert (node->op == op_Return);
  return (get_irn_arity(node) - RETURN_RESULT_OFFSET);
}

INLINE ir_node **
get_Return_res_arr (ir_node *node)
{
  assert ((node->op == op_Return));
  if (get_Return_n_ress(node) > 0)
    return (ir_node **)&(get_irn_in(node)[1 + RETURN_RESULT_OFFSET]);
  else
    return NULL;
}

/*
INLINE void
set_Return_n_res (ir_node *node, int results) {
  assert (node->op == op_Return);
}
*/

INLINE ir_node *
get_Return_res (ir_node *node, int pos) {
  assert (node->op == op_Return);
  assert (get_Return_n_ress(node) > pos);
  return get_irn_n(node, pos + RETURN_RESULT_OFFSET);
}

INLINE void
set_Return_res (ir_node *node, int pos, ir_node *res){
  assert (node->op == op_Return);
  set_irn_n(node, pos + RETURN_RESULT_OFFSET, res);
}

INLINE ir_node *
get_Raise_mem (ir_node *node) {
  assert (node->op == op_Raise);
  return get_irn_n(node, 0);
}

INLINE void
set_Raise_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Raise);
  set_irn_n(node, 0, mem);
}

INLINE ir_node *
get_Raise_exo_ptr (ir_node *node) {
  assert (node->op == op_Raise);
  return get_irn_n(node, 1);
}

INLINE void
set_Raise_exo_ptr (ir_node *node, ir_node *exo_ptr) {
  assert (node->op == op_Raise);
  set_irn_n(node, 1, exo_ptr);
}

INLINE tarval *get_Const_tarval (ir_node *node) {
  assert (node->op == op_Const);
  return get_irn_const_attr(node);
}

INLINE void
set_Const_tarval (ir_node *node, tarval *con) {
  assert (node->op == op_Const);
  node->attr.con = con;
}

INLINE symconst_kind
get_SymConst_kind (const ir_node *node) {
  assert (node->op == op_SymConst);
  return node->attr.i.num;
}

INLINE void
set_SymConst_kind (ir_node *node, symconst_kind num) {
  assert (node->op == op_SymConst);
  node->attr.i.num = num;
}

INLINE type *
get_SymConst_type (ir_node *node) {
  assert (   (node->op == op_SymConst)
          && (   get_SymConst_kind(node) == type_tag
              || get_SymConst_kind(node) == size));
  return node->attr.i.tori.typ = skip_tid(node->attr.i.tori.typ);
}

INLINE void
set_SymConst_type (ir_node *node, type *tp) {
  assert (   (node->op == op_SymConst)
          && (   get_SymConst_kind(node) == type_tag
              || get_SymConst_kind(node) == size));
  node->attr.i.tori.typ = tp;
}

INLINE ident *
get_SymConst_ptrinfo (ir_node *node) {
  assert (   (node->op == op_SymConst)
          && (get_SymConst_kind(node) == linkage_ptr_info));
  return node->attr.i.tori.ptrinfo;
}

INLINE void
set_SymConst_ptrinfo (ir_node *node, ident *ptrinfo) {
  assert (   (node->op == op_SymConst)
          && (get_SymConst_kind(node) == linkage_ptr_info));
  node->attr.i.tori.ptrinfo = ptrinfo;
}

INLINE type_or_id_p
get_SymConst_type_or_id (ir_node *node) {
  assert (node->op == op_SymConst);
  return &(node->attr.i.tori);
}

INLINE void
set_SymConst_type_or_id (ir_node *node, type_or_id_p tori) {
  assert (node->op == op_SymConst);
  memcpy (&(node->attr.i.tori), tori, sizeof(type_or_id));
}

INLINE ir_node *
get_Sel_mem (ir_node *node) {
  assert (node->op == op_Sel);
  return get_irn_n(node, 0);
}

INLINE void
set_Sel_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Sel);
  set_irn_n(node, 0, mem);
}

INLINE ir_node *
get_Sel_ptr (ir_node *node) {
  assert (node->op == op_Sel);
  return get_irn_n(node, 1);
}

INLINE void
set_Sel_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Sel);
  set_irn_n(node, 1, ptr);
}

INLINE int
get_Sel_n_indexs (ir_node *node) {
  assert (node->op == op_Sel);
  return (get_irn_arity(node) - SEL_INDEX_OFFSET);
}

INLINE ir_node **
get_Sel_index_arr (ir_node *node)
{
  assert ((node->op == op_Sel));
  if (get_Sel_n_indexs(node) > 0)
    return (ir_node **)& get_irn_in(node)[SEL_INDEX_OFFSET + 1];
  else
    return NULL;
}

INLINE ir_node *
get_Sel_index (ir_node *node, int pos) {
  assert (node->op == op_Sel);
  return get_irn_n(node, pos + SEL_INDEX_OFFSET);
}

INLINE void
set_Sel_index (ir_node *node, int pos, ir_node *index) {
  assert (node->op == op_Sel);
  set_irn_n(node, pos + SEL_INDEX_OFFSET, index);
}

INLINE entity *
get_Sel_entity (ir_node *node) {
  assert (node->op == op_Sel);
  return node->attr.s.ent;
}

INLINE void
set_Sel_entity (ir_node *node, entity *ent) {
  assert (node->op == op_Sel);
  node->attr.s.ent = ent;
}

type *
get_InstOf_ent (ir_node *node) {
  assert (node->op = op_InstOf);
  return (node->attr.io.ent);
}

void
set_InstOf_ent (ir_node *node, type *ent) {
  assert (node->op = op_InstOf);
  node->attr.io.ent = ent;
}

ir_node *
get_InstOf_store (ir_node *node) {
  assert (node->op = op_InstOf);
  return (get_irn_n (node, 0));
}

void
set_InstOf_store (ir_node *node, ir_node *obj) {
  assert (node->op = op_InstOf);
  set_irn_n (node, 0, obj);
}

ir_node *
get_InstOf_obj (ir_node *node) {
  assert (node->op = op_InstOf);
  return (get_irn_n (node, 1));
}

void
set_InstOf_obj (ir_node *node, ir_node *obj) {
  assert (node->op = op_InstOf);
  set_irn_n (node, 1, obj);
}


/* For unary and binary arithmetic operations the access to the
   operands can be factored out.  Left is the first, right the
   second arithmetic value  as listed in tech report 0999-33.
   unops are: Minus, Abs, Not, Conv, Cast
   binops are: Add, Sub, Mul, Quot, DivMod, Div, Mod, And, Or, Eor, Shl,
   Shr, Shrs, Rotate, Cmp */


INLINE ir_node *
get_Call_mem (ir_node *node) {
  assert (node->op == op_Call);
  return get_irn_n(node, 0);
}

INLINE void
set_Call_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Call);
  set_irn_n(node, 0, mem);
}

INLINE ir_node *
get_Call_ptr (ir_node *node) {
  assert (node->op == op_Call);
  return get_irn_n(node, 1);
}

INLINE void
set_Call_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Call);
  set_irn_n(node, 1, ptr);
}

INLINE ir_node **
get_Call_param_arr (ir_node *node) {
  assert (node->op == op_Call);
  return (ir_node **)&get_irn_in(node)[CALL_PARAM_OFFSET + 1];
}

INLINE int
get_Call_n_params (ir_node *node)  {
  assert (node->op == op_Call);
  return (get_irn_arity(node) - CALL_PARAM_OFFSET);
}

INLINE int
get_Call_arity (ir_node *node) {
  assert (node->op == op_Call);
  return get_Call_n_params(node);
}

/* INLINE void
set_Call_arity (ir_node *node, ir_node *arity) {
  assert (node->op == op_Call);
}
*/

INLINE ir_node *
get_Call_param (ir_node *node, int pos) {
  assert (node->op == op_Call);
  return get_irn_n(node, pos + CALL_PARAM_OFFSET);
}

INLINE void
set_Call_param (ir_node *node, int pos, ir_node *param) {
  assert (node->op == op_Call);
  set_irn_n(node, pos + CALL_PARAM_OFFSET, param);
}

INLINE type *
get_Call_type (ir_node *node) {
  assert (node->op == op_Call);
  return node->attr.call.cld_tp = skip_tid(node->attr.call.cld_tp);
}

INLINE void
set_Call_type (ir_node *node, type *tp) {
  assert (node->op == op_Call);
  assert (is_method_type(tp));
  node->attr.call.cld_tp = tp;
}

int get_Call_n_callees(ir_node * node) {
  assert(node->op == op_Call && node->attr.call.callee_arr);
  return ARR_LEN(node->attr.call.callee_arr);
}

entity * get_Call_callee(ir_node * node, int pos) {
  assert(node->op == op_Call && node->attr.call.callee_arr);
  return node->attr.call.callee_arr[pos];
}

void set_Call_callee_arr(ir_node * node, int n, entity ** arr) {
  assert(node->op == op_Call);
  if (node->attr.call.callee_arr == NULL || get_Call_n_callees(node) != n) {
    node->attr.call.callee_arr = NEW_ARR_D(entity *, current_ir_graph->obst, n);
  }
  memcpy(node->attr.call.callee_arr, arr, n * sizeof(entity *));
}

void remove_Call_callee_arr(ir_node * node) {
  assert(node->op == op_Call);
  node->attr.call.callee_arr = NULL;
}

ir_node * get_CallBegin_ptr (ir_node *node) {
  assert(node->op == op_CallBegin);
  return get_irn_n(node, 0);
}
void set_CallBegin_ptr (ir_node *node, ir_node *ptr) {
  assert(node->op == op_CallBegin);
  set_irn_n(node, 0, ptr);
}
ir_graph * get_CallBegin_irg (ir_node *node) {
  assert(node->op == op_CallBegin);
  return node->attr.callbegin.irg;
}
ir_node * get_CallBegin_call (ir_node *node) {
  assert(node->op == op_CallBegin);
  return node->attr.callbegin.call;
}
void  set_CallBegin_call (ir_node *node, ir_node *call) {
  assert(node->op == op_CallBegin);
  node->attr.callbegin.call = call;
}

INLINE ir_node *
get_Add_left (ir_node *node) {
  assert (node->op == op_Add);
  return get_irn_n(node, 0);
}

INLINE void
set_Add_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Add);
  set_irn_n(node, 0, left);
}

INLINE ir_node *
get_Add_right (ir_node *node) {
  assert (node->op == op_Add);
  return get_irn_n(node, 1);
}

INLINE void
set_Add_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Add);
  set_irn_n(node, 1, right);
}

INLINE ir_node *
get_Sub_left (ir_node *node) {
  assert (node->op == op_Sub);
  return get_irn_n(node, 0);
}

INLINE void
set_Sub_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Sub);
  set_irn_n(node, 0, left);
}

INLINE ir_node *
get_Sub_right (ir_node *node) {
  assert (node->op == op_Sub);
  return get_irn_n(node, 1);
}

INLINE void
set_Sub_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Sub);
  set_irn_n(node, 1, right);
}


INLINE ir_node *
get_Minus_op (ir_node *node) {
  assert (node->op == op_Minus);
  return get_irn_n(node, 0);
}

INLINE void
set_Minus_op (ir_node *node, ir_node *op) {
  assert (node->op == op_Minus);
  set_irn_n(node, 0, op);
}


INLINE ir_node *
get_Mul_left (ir_node *node) {
  assert (node->op == op_Mul);
  return get_irn_n(node, 0);
}

INLINE void
set_Mul_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Mul);
  set_irn_n(node, 0, left);
}

INLINE ir_node *
get_Mul_right (ir_node *node) {
  assert (node->op == op_Mul);
  return get_irn_n(node, 1);
}

INLINE void
set_Mul_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Mul);
  set_irn_n(node, 1, right);
}

INLINE ir_node *
get_Quot_left (ir_node *node) {
  assert (node->op == op_Quot);
  return get_irn_n(node, 1);
}

INLINE void
set_Quot_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Quot);
  set_irn_n(node, 1, left);
}

INLINE ir_node *
get_Quot_right (ir_node *node) {
  assert (node->op == op_Quot);
  return get_irn_n(node, 2);
}

INLINE void
set_Quot_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Quot);
  set_irn_n(node, 2, right);
}

INLINE ir_node *
get_Quot_mem (ir_node *node) {
  assert (node->op == op_Quot);
  return get_irn_n(node, 0);
}

INLINE void
set_Quot_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Quot);
  set_irn_n(node, 0, mem);
}

INLINE ir_node *
get_DivMod_left (ir_node *node) {
  assert (node->op == op_DivMod);
  return get_irn_n(node, 1);
}

INLINE void
set_DivMod_left (ir_node *node, ir_node *left) {
  assert (node->op == op_DivMod);
  set_irn_n(node, 1, left);
}

INLINE ir_node *
get_DivMod_right (ir_node *node) {
  assert (node->op == op_DivMod);
  return get_irn_n(node, 2);
}

INLINE void
set_DivMod_right (ir_node *node, ir_node *right) {
  assert (node->op == op_DivMod);
  set_irn_n(node, 2, right);
}

INLINE ir_node *
get_DivMod_mem (ir_node *node) {
  assert (node->op == op_DivMod);
  return get_irn_n(node, 0);
}

INLINE void
set_DivMod_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_DivMod);
  set_irn_n(node, 0, mem);
}

INLINE ir_node *
get_Div_left (ir_node *node) {
  assert (node->op == op_Div);
  return get_irn_n(node, 1);
}

INLINE void
set_Div_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Div);
  set_irn_n(node, 1, left);
}

INLINE ir_node *
get_Div_right (ir_node *node) {
  assert (node->op == op_Div);
  return get_irn_n(node, 2);
}

INLINE void
set_Div_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Div);
  set_irn_n(node, 2, right);
}

INLINE ir_node *
get_Div_mem (ir_node *node) {
  assert (node->op == op_Div);
  return get_irn_n(node, 0);
}

INLINE void
set_Div_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Div);
  set_irn_n(node, 0, mem);
}

INLINE ir_node *
get_Mod_left (ir_node *node) {
  assert (node->op == op_Mod);
  return get_irn_n(node, 1);
}

INLINE void
set_Mod_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Mod);
  set_irn_n(node, 1, left);
}

INLINE ir_node *
get_Mod_right (ir_node *node) {
  assert (node->op == op_Mod);
  return get_irn_n(node, 2);
}

INLINE void
set_Mod_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Mod);
  set_irn_n(node, 2, right);
}

INLINE ir_node *
get_Mod_mem (ir_node *node) {
  assert (node->op == op_Mod);
  return get_irn_n(node, 0);
}

INLINE void
set_Mod_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Mod);
  set_irn_n(node, 0, mem);
}

INLINE ir_node *
get_Abs_op (ir_node *node) {
  assert (node->op == op_Abs);
  return get_irn_n(node, 0);
}

INLINE void
set_Abs_op (ir_node *node, ir_node *op) {
  assert (node->op == op_Abs);
  set_irn_n(node, 0, op);
}

INLINE ir_node *
get_And_left (ir_node *node) {
  assert (node->op == op_And);
  return get_irn_n(node, 0);
}

INLINE void
set_And_left (ir_node *node, ir_node *left) {
  assert (node->op == op_And);
  set_irn_n(node, 0, left);
}

INLINE ir_node *
get_And_right (ir_node *node) {
  assert (node->op == op_And);
  return get_irn_n(node, 1);
}

INLINE void
set_And_right (ir_node *node, ir_node *right) {
  assert (node->op == op_And);
  set_irn_n(node, 1, right);
}

INLINE ir_node *
get_Or_left (ir_node *node) {
  assert (node->op == op_Or);
  return get_irn_n(node, 0);
}

INLINE void
set_Or_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Or);
  set_irn_n(node, 0, left);
}

INLINE ir_node *
get_Or_right (ir_node *node) {
  assert (node->op == op_Or);
  return get_irn_n(node, 1);
}

INLINE void
set_Or_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Or);
  set_irn_n(node, 1, right);
}

INLINE ir_node *
get_Eor_left (ir_node *node) {
  assert (node->op == op_Eor);
  return get_irn_n(node, 0);
}

INLINE void
set_Eor_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Eor);
  set_irn_n(node, 0, left);
}

INLINE ir_node *
get_Eor_right (ir_node *node) {
  assert (node->op == op_Eor);
  return get_irn_n(node, 1);
}

INLINE void
set_Eor_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Eor);
  set_irn_n(node, 1, right);
}


INLINE ir_node *
get_Not_op (ir_node *node) {
  assert (node->op == op_Not);
  return get_irn_n(node, 0);
}

INLINE void
set_Not_op (ir_node *node, ir_node *op) {
  assert (node->op == op_Not);
  set_irn_n(node, 0, op);
}


INLINE ir_node *
get_Shl_left (ir_node *node) {
  assert (node->op == op_Shl);
  return get_irn_n(node, 0);
}

INLINE void
set_Shl_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Shl);
  set_irn_n(node, 0, left);
}

INLINE ir_node *
get_Shl_right (ir_node *node) {
  assert (node->op == op_Shl);
  return get_irn_n(node, 1);
}

INLINE void
set_Shl_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Shl);
  set_irn_n(node, 1, right);
}

INLINE ir_node *
get_Shr_left (ir_node *node) {
  assert (node->op == op_Shr);
  return get_irn_n(node, 0);
}

INLINE void
set_Shr_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Shr);
  set_irn_n(node, 0, left);
}

INLINE ir_node *
get_Shr_right (ir_node *node) {
  assert (node->op == op_Shr);
  return get_irn_n(node, 1);
}

INLINE void
set_Shr_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Shr);
  set_irn_n(node, 1, right);
}

INLINE ir_node *
get_Shrs_left (ir_node *node) {
  assert (node->op == op_Shrs);
  return get_irn_n(node, 0);
}

INLINE void
set_Shrs_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Shrs);
  set_irn_n(node, 0, left);
}

INLINE ir_node *
get_Shrs_right (ir_node *node) {
  assert (node->op == op_Shrs);
  return get_irn_n(node, 1);
}

INLINE void
set_Shrs_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Shrs);
  set_irn_n(node, 1, right);
}

INLINE ir_node *
get_Rot_left (ir_node *node) {
  assert (node->op == op_Rot);
  return get_irn_n(node, 0);
}

INLINE void
set_Rot_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Rot);
  set_irn_n(node, 0, left);
}

INLINE ir_node *
get_Rot_right (ir_node *node) {
  assert (node->op == op_Rot);
  return get_irn_n(node, 1);
}

INLINE void
set_Rot_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Rot);
  set_irn_n(node, 1, right);
}

INLINE ir_node *
get_Cmp_left (ir_node *node) {
  assert (node->op == op_Cmp);
  return get_irn_n(node, 0);
}

INLINE void
set_Cmp_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Cmp);
  set_irn_n(node, 0, left);
}

INLINE ir_node *
get_Cmp_right (ir_node *node) {
  assert (node->op == op_Cmp);
  return get_irn_n(node, 1);
}

INLINE void
set_Cmp_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Cmp);
  set_irn_n(node, 1, right);
}

INLINE ir_node *
get_Conv_op (ir_node *node) {
  assert (node->op == op_Conv);
  return get_irn_n(node, 0);
}

INLINE void
set_Conv_op (ir_node *node, ir_node *op) {
  assert (node->op == op_Conv);
  set_irn_n(node, 0, op);
}

INLINE ir_node *
get_Cast_op (ir_node *node) {
  assert (node->op == op_Cast);
  return get_irn_n(node, 0);
}

INLINE void
set_Cast_op (ir_node *node, ir_node *op) {
  assert (node->op == op_Cast);
  set_irn_n(node, 0, op);
}

INLINE type *
get_Cast_type (ir_node *node) {
  assert (node->op == op_Cast);
  return node->attr.cast.totype;
}

INLINE void
set_Cast_type (ir_node *node, type *to_tp) {
  assert (node->op == op_Cast);
  node->attr.cast.totype = to_tp;
}

INLINE int
is_unop (ir_node *node) {
  return ( node->op == op_Minus ||
           node->op == op_Abs  ||
	   node->op == op_Not  ||
           node->op == op_Conv ||
           node->op == op_Cast );
}

INLINE ir_node *
get_unop_op (ir_node *node) {
  assert (is_unop(node));
  switch (get_irn_opcode (node)) {
    case iro_Minus: return get_Minus_op(node); break;
    case iro_Abs:   return get_Abs_op(node);   break;
    case iro_Not:   return get_Not_op(node);   break;
    case iro_Conv:  return get_Conv_op(node);  break;
    case iro_Cast:  return get_Cast_op(node);  break;
    default: return NULL;
  }
}

INLINE void
set_unop_op (ir_node *node, ir_node *op) {
  assert (is_unop(node));
  switch (get_irn_opcode (node)) {
    case iro_Minus:   set_Minus_op(node, op); break;
    case iro_Abs:     set_Abs_op(node, op);   break;
    case iro_Not:     set_Not_op(node, op);   break;
    case iro_Conv:    set_Conv_op(node, op);  break;
    case iro_Cast:    set_Cast_op(node, op);  break;
    default:  ;
  }

}

int
is_binop (ir_node *node) {
  return (node->op == op_Add    ||
          node->op == op_Sub    ||
          node->op == op_Mul    ||
          node->op == op_Quot   ||
          node->op == op_DivMod ||
          node->op == op_Div    ||
          node->op == op_Mod    ||
          node->op == op_And    ||
          node->op == op_Or     ||
          node->op == op_Eor    ||
          node->op == op_Shl    ||
          node->op == op_Shr    ||
          node->op == op_Shrs   ||
	  node->op == op_Rot    ||
          node->op == op_Cmp      );
}

INLINE ir_node *
get_binop_left (ir_node *node) {
  assert (node->op == op_Add    ||
          node->op == op_Sub    ||
          node->op == op_Mul    ||
          node->op == op_Quot   ||
          node->op == op_DivMod ||
          node->op == op_Div    ||
          node->op == op_Mod    ||
          node->op == op_And    ||
          node->op == op_Or     ||
          node->op == op_Eor    ||
          node->op == op_Shl    ||
          node->op == op_Shr    ||
          node->op == op_Shrs   ||
	  node->op == op_Rot    ||
          node->op == op_Cmp      );

    switch (get_irn_opcode (node)) {
      case iro_Add   :     return get_Add_left(node);  break;
      case iro_Sub   :     return get_Sub_left(node);  break;
      case iro_Mul   :     return get_Mul_left(node);  break;
      case iro_Quot  :     return get_Quot_left(node); break;
      case iro_DivMod:     return get_DivMod_left(node);  break;
      case iro_Div   :     return get_Div_left(node);  break;
      case iro_Mod   :     return get_Mod_left(node);  break;
      case iro_And   :     return get_And_left(node);  break;
      case iro_Or    :     return get_Or_left(node);   break;
      case iro_Eor   :     return get_Eor_left(node);  break;
      case iro_Shl   :     return get_Shl_left(node);  break;
      case iro_Shr   :     return get_Shr_left(node);  break;
      case iro_Shrs  :     return get_Shrs_left(node); break;
      case iro_Rot   :     return get_Rot_left(node);  break;
      case iro_Cmp   :     return get_Cmp_left(node);  break;
    default:  return NULL;
  };
}

INLINE void
set_binop_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Add    ||
          node->op == op_Sub    ||
          node->op == op_Mul    ||
          node->op == op_Quot   ||
          node->op == op_DivMod ||
          node->op == op_Div    ||
          node->op == op_Mod    ||
          node->op == op_And    ||
          node->op == op_Or     ||
          node->op == op_Eor    ||
          node->op == op_Shl    ||
          node->op == op_Shr    ||
	  node->op == op_Shrs   ||
	  node->op == op_Rot    ||
          node->op == op_Cmp      );

    switch (get_irn_opcode (node)) {
      case iro_Add   :     set_Add_left(node, left);  break;
      case iro_Sub   :     set_Sub_left(node, left);  break;
      case iro_Mul   :     set_Mul_left(node, left);  break;
      case iro_Quot  :     set_Quot_left(node, left); break;
      case iro_DivMod:     set_DivMod_left(node, left);  break;
      case iro_Div   :     set_Div_left(node, left);  break;
      case iro_Mod   :     set_Mod_left(node, left);  break;
      case iro_And   :     set_And_left(node, left);  break;
      case iro_Or    :     set_Or_left(node, left);   break;
      case iro_Eor   :     set_Eor_left(node, left);  break;
      case iro_Shl   :     set_Shl_left(node, left);  break;
      case iro_Shr   :     set_Shr_left(node, left);  break;
      case iro_Shrs  :     set_Shrs_left(node, left); break;
      case iro_Rot   :     set_Rot_left(node, left);  break;
      case iro_Cmp   :     set_Cmp_left(node, left);  break;
    default:  ;
  };
}

INLINE ir_node *
get_binop_right (ir_node *node) {
  assert (node->op == op_Add    ||
          node->op == op_Sub    ||
          node->op == op_Mul    ||
          node->op == op_Quot   ||
          node->op == op_DivMod ||
          node->op == op_Div    ||
          node->op == op_Mod    ||
          node->op == op_And    ||
          node->op == op_Or     ||
          node->op == op_Eor    ||
          node->op == op_Shl    ||
          node->op == op_Shr    ||
          node->op == op_Shrs   ||
          node->op == op_Rot    ||
          node->op == op_Cmp      );

    switch (get_irn_opcode (node)) {
      case iro_Add   :     return get_Add_right(node);  break;
      case iro_Sub   :     return get_Sub_right(node);  break;
      case iro_Mul   :     return get_Mul_right(node);  break;
      case iro_Quot  :     return get_Quot_right(node); break;
      case iro_DivMod:     return get_DivMod_right(node);  break;
      case iro_Div   :     return get_Div_right(node);  break;
      case iro_Mod   :     return get_Mod_right(node);  break;
      case iro_And   :     return get_And_right(node);  break;
      case iro_Or    :     return get_Or_right(node);   break;
      case iro_Eor   :     return get_Eor_right(node);  break;
      case iro_Shl   :     return get_Shl_right(node);  break;
      case iro_Shr   :     return get_Shr_right(node);  break;
      case iro_Shrs  :     return get_Shrs_right(node); break;
      case iro_Rot   :     return get_Rot_right(node);  break;
      case iro_Cmp   :     return get_Cmp_right(node);  break;
    default:  return NULL;
  };
}

INLINE void
set_binop_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Add    ||
          node->op == op_Sub    ||
          node->op == op_Mul    ||
          node->op == op_Quot   ||
          node->op == op_DivMod ||
          node->op == op_Div    ||
          node->op == op_Mod    ||
          node->op == op_And    ||
          node->op == op_Or     ||
          node->op == op_Eor    ||
          node->op == op_Shl    ||
          node->op == op_Shr    ||
          node->op == op_Shrs   ||
          node->op == op_Rot    ||
          node->op == op_Cmp      );

    switch (get_irn_opcode (node)) {
      case iro_Add   :     set_Add_right(node, right);  break;
      case iro_Sub   :     set_Sub_right(node, right);  break;
      case iro_Mul   :     set_Mul_right(node, right);  break;
      case iro_Quot  :     set_Quot_right(node, right); break;
      case iro_DivMod:     set_DivMod_right(node, right);  break;
      case iro_Div   :     set_Div_right(node, right);  break;
      case iro_Mod   :     set_Mod_right(node, right);  break;
      case iro_And   :     set_And_right(node, right);  break;
      case iro_Or    :     set_Or_right(node, right);   break;
      case iro_Eor   :     set_Eor_right(node, right);  break;
      case iro_Shl   :     set_Shl_right(node, right);  break;
      case iro_Shr   :     set_Shr_right(node, right);  break;
      case iro_Shrs  :     set_Shrs_right(node, right); break;
      case iro_Rot   :     set_Rot_right(node, right);  break;
      case iro_Cmp   :     set_Cmp_right(node, right);  break;
    default: ;
  };
}


INLINE ir_node **
get_Phi_preds_arr (ir_node *node) {
  assert (node->op == op_Phi);
  return (ir_node **)&(get_irn_in(node)[1]);
}

INLINE int
get_Phi_n_preds (ir_node *node) {
  assert (node->op == op_Phi);
  return (get_irn_arity(node));
}

/*
INLINE void set_Phi_n_preds (ir_node *node, int n_preds) {
  assert (node->op == op_Phi);
}
*/

INLINE ir_node *
get_Phi_pred (ir_node *node, int pos) {
  assert (node->op == op_Phi);
  return get_irn_n(node, pos);
}

INLINE void
set_Phi_pred (ir_node *node, int pos, ir_node *pred) {
  assert (node->op == op_Phi);
  set_irn_n(node, pos, pred);
}

INLINE ir_node *
get_Load_mem (ir_node *node) {
  assert (node->op == op_Load);
  return get_irn_n(node, 0);
}

INLINE void
set_Load_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Load);
  set_irn_n(node, 0, mem);
}

INLINE ir_node *
get_Load_ptr (ir_node *node) {
  assert (node->op == op_Load);
  return get_irn_n(node, 1);
}

INLINE void
set_Load_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Load);
  set_irn_n(node, 1, ptr);
}

INLINE
ir_node *
get_Store_mem (ir_node *node) {
  assert (node->op == op_Store);
  return get_irn_n(node, 0);
}

INLINE void
set_Store_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Store);
  set_irn_n(node, 0, mem);
}

INLINE ir_node *
get_Store_ptr (ir_node *node) {
  assert (node->op == op_Store);
  return get_irn_n(node, 1);
}

INLINE void
set_Store_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Store);
  set_irn_n(node, 1, ptr);
}

INLINE ir_node *
get_Store_value (ir_node *node) {
  assert (node->op == op_Store);
  return get_irn_n(node, 2);
}

INLINE void
set_Store_value (ir_node *node, ir_node *value) {
  assert (node->op == op_Store);
  set_irn_n(node, 2, value);
}

INLINE ir_node *
get_Alloc_mem (ir_node *node) {
  assert (node->op == op_Alloc);
  return get_irn_n(node, 0);
}

INLINE void
set_Alloc_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Alloc);
  set_irn_n(node, 0, mem);
}

INLINE ir_node *
get_Alloc_size (ir_node *node) {
  assert (node->op == op_Alloc);
  return get_irn_n(node, 1);
}

INLINE void
set_Alloc_size (ir_node *node, ir_node *size) {
  assert (node->op == op_Alloc);
  set_irn_n(node, 1, size);
}

INLINE type  *
get_Alloc_type (ir_node *node) {
  assert (node->op == op_Alloc);
  return node->attr.a.type = skip_tid(node->attr.a.type);
}

INLINE void
set_Alloc_type (ir_node *node, type *tp) {
  assert (node->op == op_Alloc);
  node->attr.a.type = tp;
}

INLINE where_alloc
get_Alloc_where (ir_node *node) {
  assert (node->op == op_Alloc);
  return node->attr.a.where;
}

INLINE void
set_Alloc_where (ir_node *node, where_alloc where) {
  assert (node->op == op_Alloc);
  node->attr.a.where = where;
}


INLINE ir_node *
get_Free_mem (ir_node *node) {
  assert (node->op == op_Free);
  return get_irn_n(node, 0);
}

INLINE void
set_Free_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Free);
  set_irn_n(node, 0, mem);
}

INLINE ir_node *
get_Free_ptr (ir_node *node) {
  assert (node->op == op_Free);
  return get_irn_n(node, 1);
}

INLINE void
set_Free_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Free);
  set_irn_n(node, 1, ptr);
}

INLINE ir_node *
get_Free_size (ir_node *node) {
  assert (node->op == op_Free);
  return get_irn_n(node, 2);
}

INLINE void
set_Free_size (ir_node *node, ir_node *size) {
  assert (node->op == op_Free);
  set_irn_n(node, 2, size);
}

INLINE type  *
get_Free_type (ir_node *node) {
  assert (node->op == op_Free);
  return node->attr.f = skip_tid(node->attr.f);
}

INLINE void
set_Free_type (ir_node *node, type *tp) {
  assert (node->op == op_Free);
  node->attr.f = tp;
}

INLINE ir_node **
get_Sync_preds_arr (ir_node *node) {
  assert (node->op == op_Sync);
  return (ir_node **)&(get_irn_in(node)[1]);
}

INLINE int
get_Sync_n_preds (ir_node *node) {
  assert (node->op == op_Sync);
  return (get_irn_arity(node));
}

/*
INLINE void
set_Sync_n_preds (ir_node *node, int n_preds) {
  assert (node->op == op_Sync);
}
*/

INLINE ir_node *
get_Sync_pred (ir_node *node, int pos) {
  assert (node->op == op_Sync);
  return get_irn_n(node, pos);
}

INLINE void
set_Sync_pred (ir_node *node, int pos, ir_node *pred) {
  assert (node->op == op_Sync);
  set_irn_n(node, pos, pred);
}

INLINE ir_node *
get_Proj_pred (ir_node *node) {
  assert (is_Proj(node));
  return get_irn_n(node, 0);
}

INLINE void
set_Proj_pred (ir_node *node, ir_node *pred) {
  assert (is_Proj(node));
  set_irn_n(node, 0, pred);
}

INLINE long
get_Proj_proj (ir_node *node) {
  assert (is_Proj(node));
  if (get_irn_opcode(node) == iro_Proj) {
    return node->attr.proj;
  } else {
    assert(get_irn_opcode(node) == iro_Filter);
    return node->attr.filter.proj;
  }
}

INLINE void
set_Proj_proj (ir_node *node, long proj) {
  assert (node->op == op_Proj);
  node->attr.proj = proj;
}

INLINE ir_node **
get_Tuple_preds_arr (ir_node *node) {
  assert (node->op == op_Tuple);
  return (ir_node **)&(get_irn_in(node)[1]);
}

INLINE int
get_Tuple_n_preds (ir_node *node) {
  assert (node->op == op_Tuple);
  return (get_irn_arity(node));
}

/*
INLINE void
set_Tuple_n_preds (ir_node *node, int n_preds) {
  assert (node->op == op_Tuple);
}
*/

INLINE ir_node *
get_Tuple_pred (ir_node *node, int pos) {
  assert (node->op == op_Tuple);
  return get_irn_n(node, pos);
}

INLINE void
set_Tuple_pred (ir_node *node, int pos, ir_node *pred) {
  assert (node->op == op_Tuple);
  set_irn_n(node, pos, pred);
}

INLINE ir_node *
get_Id_pred (ir_node *node) {
  assert (node->op == op_Id);
  return get_irn_n(node, 0);
}

INLINE void
set_Id_pred (ir_node *node, ir_node *pred) {
  assert (node->op == op_Id);
  set_irn_n(node, 0, pred);
}


INLINE ir_node *
get_Filter_pred (ir_node *node) {
  assert(node->op == op_Filter);
  return node->in[1];
}
INLINE void
set_Filter_pred (ir_node *node, ir_node *pred) {
  assert(node->op == op_Filter);
  node->in[1] = pred;
}
INLINE long
get_Filter_proj(ir_node *node) {
  assert(node->op == op_Filter);
  return node->attr.filter.proj;
}
INLINE void
set_Filter_proj (ir_node *node, long proj) {
  assert(node->op == op_Filter);
  node->attr.filter.proj = proj;
}

/* Don't use get_irn_arity, get_irn_n in implementation as access
   shall work independent of view!!! */
void set_Filter_cg_pred_arr(ir_node * node, int arity, ir_node ** in) {
  assert(node->op == op_Filter);
  if (node->attr.filter.in_cg == NULL || arity != ARR_LEN(node->attr.filter.in_cg) - 1) {
    node->attr.filter.in_cg = NEW_ARR_D(ir_node *, current_ir_graph->obst, arity + 1);
    node->attr.filter.backedge = NEW_ARR_D (int, current_ir_graph->obst, arity);
    memset(node->attr.filter.backedge, 0, sizeof(int) * arity);
    node->attr.filter.in_cg[0] = node->in[0];
  }
  memcpy(node->attr.filter.in_cg + 1, in, sizeof(ir_node *) * arity);
}

void set_Filter_cg_pred(ir_node * node, int pos, ir_node * pred) {
  assert(node->op == op_Filter && node->attr.filter.in_cg &&
	 0 <= pos && pos < ARR_LEN(node->attr.filter.in_cg) - 1);
  node->attr.filter.in_cg[pos + 1] = pred;
}
int get_Filter_n_cg_preds(ir_node *node) {
  assert(node->op == op_Filter && node->attr.filter.in_cg);
  return (ARR_LEN(node->attr.filter.in_cg) - 1);
}
ir_node *get_Filter_cg_pred(ir_node *node, int pos) {
  int arity;
  assert(node->op == op_Filter && node->attr.filter.in_cg &&
	 0 <= pos);
  arity = ARR_LEN(node->attr.filter.in_cg);
  assert(pos <  arity - 1);
  return node->attr.filter.in_cg[pos + 1];
}


INLINE ir_graph *
get_irn_irg(ir_node *node) {
  if (get_irn_op(node) == op_CallBegin) {
    return node->attr.callbegin.irg;
  } else if (get_irn_op(node) == op_EndReg ||
	     get_irn_op(node) == op_EndExcept) {
    return node->attr.end.irg;
  } else if (get_irn_op(node) == op_Start) {
    return node->attr.start.irg;
  } else {
    assert(0 && "no irg attr");
    return NULL;
  }
}


/******************************************************************/
/*  Auxiliary routines                                            */
/******************************************************************/

INLINE ir_node *
skip_Proj (ir_node *node) {
  /* don't assert node !!! */
  if (node && is_Proj(node)) {
    return get_Proj_pred(node);
  } else {
    return node;
  }
}

INLINE ir_node *
skip_Tuple (ir_node *node) {
  ir_node *pred;

  if (!get_opt_normalize()) return node;

  node = skip_nop(node);
  if (get_irn_op(node) == op_Proj) {
    pred = skip_nop(get_Proj_pred(node));
    if (get_irn_op(pred) == op_Proj) /* nested Tuple ? */
      pred = skip_nop(skip_Tuple(pred));
    if (get_irn_op(pred) == op_Tuple)
      return get_Tuple_pred(pred, get_Proj_proj(node));
  }
  return node;
}

/* This should compact Id-cycles to self-cycles. It has the same (or less?) complexity
   than any other approach, as Id chains are resolved and all point to the real node, or
   all id's are self loops. */
INLINE ir_node *
skip_nop (ir_node *node) {
  /* don't assert node !!! */

  if (!get_opt_normalize()) return node;

  /* Don't use get_Id_pred:  We get into an endless loop for
     self-referencing Ids. */
  if (node && (node->op == op_Id) && (node != node->in[0+1])) {
    ir_node *rem_pred = node->in[0+1];
    ir_node *res;

    assert (get_irn_arity (node) > 0);

    node->in[0+1] = node;
    res = skip_nop(rem_pred);
    if (res->op == op_Id) /* self-loop */ return node;

    node->in[0+1] = res;
    return res;
  } else {
    return node;
  }
}

INLINE ir_node *
skip_Id (ir_node *node) {
  return skip_nop(node);
}

INLINE int
is_Bad (ir_node *node) {
  assert(node);
  if ((node) && get_irn_opcode(node) == iro_Bad)
    return 1;
  return 0;
}

INLINE int
is_no_Block (ir_node *node) {
  assert(node);
  return (get_irn_opcode(node) != iro_Block);
}

INLINE int
is_Block (ir_node *node) {
  assert(node);
  return (get_irn_opcode(node) == iro_Block);
}

INLINE int
is_Proj (const ir_node *node) {
  assert(node);
  return node->op == op_Proj
    || (!interprocedural_view && node->op == op_Filter);
}

/* Returns true if the operation manipulates control flow. */
int
is_cfop(ir_node *node) {
  return is_cfopcode(get_irn_op(node));
}

/* Returns true if the operation manipulates interprocedural control flow:
   CallBegin, EndReg, EndExcept */
INLINE int is_ip_cfop(ir_node *node) {
  return is_ip_cfopcode(get_irn_op(node));
}

ir_graph *get_ip_cfop_irg(ir_node *n) {
  switch (get_irn_opcode(n)) {
  case iro_EndReg:
    return get_EndReg_irg(n);
  case iro_EndExcept:
    return get_EndExcept_irg(n);
  case iro_CallBegin:
    return get_CallBegin_irg(n);
  default:
    assert(is_ip_cfop(n));
  }
  return NULL; /* should never be reached */
}

/* Returns true if the operation can change the control flow because
   of an exception. */
int
is_fragile_op(ir_node *node) {
  return (   (get_irn_opcode(node) == iro_Call)
          || (get_irn_opcode(node) == iro_Quot)
          || (get_irn_opcode(node) == iro_DivMod)
          || (get_irn_opcode(node) == iro_Div)
          || (get_irn_opcode(node) == iro_Mod)
          || (get_irn_opcode(node) == iro_Load)
          || (get_irn_opcode(node) == iro_Store)
          || (get_irn_opcode(node) == iro_Alloc)
          || (get_irn_opcode(node) == iro_Bad)
          || (get_irn_opcode(node) == iro_Unknown));
}


/* Returns the memory operand of fragile operations. */
ir_node *get_fragile_op_mem(ir_node *node) {
  assert(node && is_fragile_op(node));

  switch (get_irn_opcode (node)) {
  case iro_Call  :
  case iro_Quot  :
  case iro_DivMod:
  case iro_Div   :
  case iro_Mod   :
  case iro_Load  :
  case iro_Store :
  case iro_Alloc :
    return get_irn_n(node, 0);
  case iro_Bad   :
  case iro_Unknown:
    return node;
  default: ;
    assert(0 && "not reached");
    return NULL;
  }
}
