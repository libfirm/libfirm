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
# include "config.h"
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif

#include "ident.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "typegmod.h"
#include "irbackedge_t.h"
#include "irdump.h"
#include "irop_t.h"
#include "irprog_t.h"
#include "iredges_t.h"

#include "irhooks.h"

/* some constants fixing the positions of nodes predecessors
   in the in array */
#define CALL_PARAM_OFFSET     2
#define FUNCCALL_PARAM_OFFSET 1
#define SEL_INDEX_OFFSET      2
#define RETURN_RESULT_OFFSET  1  /* mem is not a result */
#define END_KEEPALIVE_OFFSET  0

static const char *pnc_name_arr [] = {
  "False", "Eq", "Lt", "Le",
  "Gt", "Ge", "Lg", "Leg", "Uo",
  "Ue", "Ul", "Ule", "Ug", "Uge",
  "Ne", "True"
};

/**
 * returns the pnc name from an pnc constant
 */
const char *get_pnc_string(int pnc) {
  return pnc_name_arr[pnc];
}

/*
 * Calculates the negated pnc condition.
 */
int
get_negated_pnc(int pnc) {
  switch (pnc) {
  case pn_Cmp_False: return pn_Cmp_True;
  case pn_Cmp_Eq:    return pn_Cmp_Ne;
  case pn_Cmp_Lt:    return pn_Cmp_Uge;
  case pn_Cmp_Le:    return pn_Cmp_Ug;
  case pn_Cmp_Gt:    return pn_Cmp_Ule;
  case pn_Cmp_Ge:    return pn_Cmp_Ul;
  case pn_Cmp_Lg:    return pn_Cmp_Ue;
  case pn_Cmp_Leg:   return pn_Cmp_Uo;
  case pn_Cmp_Uo:    return pn_Cmp_Leg;
  case pn_Cmp_Ue:    return pn_Cmp_Lg;
  case pn_Cmp_Ul:    return pn_Cmp_Ge;
  case pn_Cmp_Ule:   return pn_Cmp_Gt;
  case pn_Cmp_Ug:    return pn_Cmp_Le;
  case pn_Cmp_Uge:   return pn_Cmp_Lt;
  case pn_Cmp_Ne:    return pn_Cmp_Eq;
  case pn_Cmp_True:  return pn_Cmp_False;
  }
  return 99; /* to shut up gcc */
}

/* Calculates the swapped pnc condition, i.e., "<" --> ">" */
int
get_swapped_pnc(int pnc) {
  int code    = pnc & ~(pn_Cmp_Lt|pn_Cmp_Gt);
  int lesser  = pnc & pn_Cmp_Lt;
  int greater = pnc & pn_Cmp_Gt;

  code |= (lesser ? pn_Cmp_Gt : 0) | (greater ? pn_Cmp_Lt : 0);

  return code;
}

const char *pns_name_arr [] = {
  "initial_exec", "global_store",
  "frame_base", "globals", "args"
};

const char *symconst_name_arr [] = {
  "type_tag", "size", "addr_name", "addr_ent"
};

/**
 * Indicates, whether additional data can be registered to ir nodes.
 * If set to 1, this is not possible anymore.
 */
static int forbid_new_data = 0;

/**
 * The amount of additional space for custom data to be allocated upon
 * creating a new node.
 */
unsigned firm_add_node_size = 0;


/* register new space for every node */
unsigned register_additional_node_data(unsigned size) {
  assert(!forbid_new_data && "Too late to register additional node data");

  if (forbid_new_data)
    return 0;

  return firm_add_node_size += size;
}


void
init_irnode(void) {
	/* Forbid the addition of new data to an ir node. */
	forbid_new_data = 1;
}

/*
 * irnode constructor.
 * Create a new irnode in irg, with an op, mode, arity and
 * some incoming irnodes.
 * If arity is negative, a node with a dynamic array is created.
 */
ir_node *
new_ir_node (dbg_info *db, ir_graph *irg, ir_node *block, ir_op *op, ir_mode *mode,
         int arity, ir_node **in)
{
  ir_node *res;
  size_t node_size = offsetof(ir_node, attr) + op->attr_size + firm_add_node_size;
	char *p;

  assert(irg && op && mode);
  p = obstack_alloc (irg->obst, node_size);
  memset(p, 0, node_size);
	res = (ir_node *) (p + firm_add_node_size);

  res->kind    = k_ir_node;
  res->op      = op;
  res->mode    = mode;
  res->visited = 0;
  res->link    = NULL;
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

#if FIRM_EDGES_INPLACE
  {
    int i, n;
    int not_a_block = is_no_Block(res);

    INIT_LIST_HEAD(&res->edge_info.outs_head);

    for (i = 0, n = arity + not_a_block; i < n; ++i)
      edges_notify_edge(res, i - not_a_block, res->in[i], NULL, irg);
  }
#endif

  hook_new_node(irg, res);

  return res;
}

/*-- getting some parameters from ir_nodes --*/

int
(is_ir_node)(const void *thing) {
  return _is_ir_node(thing);
}

int
(get_irn_intra_arity)(const ir_node *node) {
  return _get_irn_intra_arity(node);
}

int
(get_irn_inter_arity)(const ir_node *node) {
  return _get_irn_inter_arity(node);
}

int (*_get_irn_arity)(const ir_node *node) = _get_irn_intra_arity;

int
(get_irn_arity)(const ir_node *node) {
  return _get_irn_arity(node);
}

/* Returns the array with ins. This array is shifted with respect to the
   array accessed by get_irn_n: The block operand is at position 0 not -1.
   (@@@ This should be changed.)
   The order of the predecessors in this array is not guaranteed, except that
   lists of operands as predecessors of Block or arguments of a Call are
   consecutive. */
ir_node **
get_irn_in (const ir_node *node) {
  assert(node);
  if (get_interprocedural_view()) { /* handle Filter and Block specially */
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

void
set_irn_in (ir_node *node, int arity, ir_node **in) {
  ir_node *** arr;
  assert(node);
  if (get_interprocedural_view()) { /* handle Filter and Block specially */
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

ir_node *
(get_irn_intra_n)(const ir_node *node, int n) {
  return _get_irn_intra_n (node, n);
}

ir_node *
(get_irn_inter_n)(const ir_node *node, int n) {
  return _get_irn_inter_n (node, n);
}

ir_node *(*_get_irn_n)(const ir_node *node, int n) = _get_irn_intra_n;

ir_node *
(get_irn_n)(const ir_node *node, int n) {
  return _get_irn_n(node, n);
}

void
set_irn_n (ir_node *node, int n, ir_node *in) {
  assert(node && node->kind == k_ir_node);
  assert(-1 <= n);
  assert(n < get_irn_arity(node));
  assert(in && in->kind == k_ir_node);

  if ((n == -1) && (get_irn_opcode(node) == iro_Filter)) {
    /* Change block pred in both views! */
    node->in[n + 1] = in;
    assert(node->attr.filter.in_cg);
    node->attr.filter.in_cg[n + 1] = in;
    return;
  }
  if (get_interprocedural_view()) { /* handle Filter and Block specially */
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

  /* Call the hook */
  hook_set_irn_n(node, n, in, node->in[n + 1]);

  /* Here, we rely on src and tgt being in the current ir graph */
  edges_notify_edge(node, n, in, node->in[n + 1], current_ir_graph);

  node->in[n + 1] = in;
}

ir_mode *
(get_irn_mode)(const ir_node *node) {
  return _get_irn_mode(node);
}

void
(set_irn_mode)(ir_node *node, ir_mode *mode)
{
  _set_irn_mode(node, mode);
}

modecode
get_irn_modecode (const ir_node *node)
{
  assert (node);
  return node->mode->code;
}

/** Gets the string representation of the mode .*/
const char *
get_irn_modename (const ir_node *node)
{
  assert(node);
  return get_mode_name(node->mode);
}

ident *
get_irn_modeident (const ir_node *node)
{
  assert(node);
  return get_mode_ident(node->mode);
}

ir_op *
(get_irn_op)(const ir_node *node)
{
  return _get_irn_op(node);
}

/* should be private to the library: */
void
set_irn_op (ir_node *node, ir_op *op)
{
  assert (node);
  node->op = op;
}

opcode
(get_irn_opcode)(const ir_node *node)
{
  return _get_irn_opcode(node);
}

const char *
get_irn_opname (const ir_node *node)
{
  assert(node);
  if ((get_irn_op((ir_node *)node) == op_Phi) &&
      (get_irg_phase_state(get_irn_irg((ir_node *)node)) == phase_building) &&
      (get_irn_arity((ir_node *)node) == 0)) return "Phi0";
  return get_id_str(node->op->name);
}

ident *
get_irn_opident (const ir_node *node)
{
  assert(node);
  return node->op->name;
}

unsigned long
(get_irn_visited)(const ir_node *node)
{
  return _get_irn_visited(node);
}

void
(set_irn_visited)(ir_node *node, unsigned long visited)
{
  _set_irn_visited(node, visited);
}

void
(mark_irn_visited)(ir_node *node) {
  _mark_irn_visited(node);
}

int
(irn_not_visited)(const ir_node *node) {
  return _irn_not_visited(node);
}

int
(irn_visited)(const ir_node *node) {
  return _irn_visited(node);
}

void
(set_irn_link)(ir_node *node, void *link) {
  _set_irn_link(node, link);
}

void *
(get_irn_link)(const ir_node *node) {
  return _get_irn_link(node);
}

op_pin_state
(get_irn_pinned)(const ir_node *node) {
  return _get_irn_pinned(node);
}

void set_irn_pinned(ir_node *node, op_pin_state state) {
  /* due to optimization an opt may be turned into a Tuple */
  if (get_irn_op(node) == op_Tuple)
    return;

  assert(node && get_op_pinned(get_irn_op(node)) >= op_pin_state_exc_pinned);
  assert(state == op_pin_state_pinned || state == op_pin_state_floats);

  node->attr.except.pin_state = state;
}

#ifdef DO_HEAPANALYSIS
/* Access the abstract interpretation information of a node.
   Returns NULL if no such information is available. */
struct abstval *get_irn_abst_value(ir_node *n) {
  return n->av;
}
/* Set the abstract interpretation information of a node. */
void set_irn_abst_value(ir_node *n, struct abstval *os) {
  n->av = os;
}
struct section *firm_get_irn_section(ir_node *n) {
  return n->sec;
}
void firm_set_irn_section(ir_node *n, struct section *s) {
  n->sec = s;
}
#else
/* Dummies needed for firmjni. */
struct abstval *get_irn_abst_value(ir_node *n) { return NULL; }
void set_irn_abst_value(ir_node *n, struct abstval *os) {}
struct section *firm_get_irn_section(ir_node *n) { return NULL; }
void firm_set_irn_section(ir_node *n, struct section *s) {}
#endif /* DO_HEAPANALYSIS */


/* Outputs a unique number for this node */
long
get_irn_node_nr(const ir_node *node) {
  assert(node);
#ifdef DEBUG_libfirm
  return node->node_nr;
#else
  return (long)node;
#endif
}

const_attr
get_irn_const_attr (ir_node *node)
{
  assert (node->op == op_Const);
  return node->attr.con;
}

long
get_irn_proj_attr (ir_node *node)
{
  assert (node->op == op_Proj);
  return node->attr.proj;
}

alloc_attr
get_irn_alloc_attr (ir_node *node)
{
  assert (node->op == op_Alloc);
  return node->attr.a;
}

free_attr
get_irn_free_attr     (ir_node *node)
{
  assert (node->op == op_Free);
  return node->attr.f;
}

symconst_attr
get_irn_symconst_attr (ir_node *node)
{
  assert (node->op == op_SymConst);
  return node->attr.i;
}

type *
get_irn_call_attr (ir_node *node)
{
  assert (node->op == op_Call);
  return node->attr.call.cld_tp = skip_tid(node->attr.call.cld_tp);
}

sel_attr
get_irn_sel_attr (ir_node *node)
{
  assert (node->op == op_Sel);
  return node->attr.s;
}

int
get_irn_phi_attr (ir_node *node)
{
  assert (node->op == op_Phi);
  return node->attr.phi0_pos;
}

block_attr
get_irn_block_attr (ir_node *node)
{
  assert (node->op == op_Block);
  return node->attr.block;
}

load_attr
get_irn_load_attr (ir_node *node)
{
  assert (node->op == op_Load);
  return node->attr.load;
}

store_attr
get_irn_store_attr (ir_node *node)
{
  assert (node->op == op_Store);
  return node->attr.store;
}

except_attr
get_irn_except_attr (ir_node *node)
{
  assert (node->op == op_Div || node->op == op_Quot ||
          node->op == op_DivMod || node->op == op_Mod || node->op == op_Call || node->op == op_Alloc);
  return node->attr.except;
}

/** manipulate fields of individual nodes **/

/* this works for all except Block */
ir_node *
get_nodes_block (const ir_node *node) {
  assert (!(node->op == op_Block));
  return get_irn_n(node, -1);
}

void
set_nodes_block (ir_node *node, ir_node *block) {
  assert (!(node->op == op_Block));
  set_irn_n(node, -1, block);
}

/* Test whether arbitrary node is frame pointer, i.e. Proj(pn_Start_P_frame_base)
 * from Start.  If so returns frame type, else Null. */
type *is_frame_pointer(ir_node *n) {
  if ((get_irn_op(n) == op_Proj) &&
      (get_Proj_proj(n) == pn_Start_P_frame_base)) {
    ir_node *start = get_Proj_pred(n);
    if (get_irn_op(start) == op_Start) {
      return get_irg_frame_type(get_irn_irg(start));
    }
  }
  return NULL;
}

/* Test whether arbitrary node is globals pointer, i.e. Proj(pn_Start_P_globals)
 * from Start.  If so returns global type, else Null. */
type *is_globals_pointer(ir_node *n) {
  if ((get_irn_op(n) == op_Proj) &&
      (get_Proj_proj(n) == pn_Start_P_globals)) {
    ir_node *start = get_Proj_pred(n);
    if (get_irn_op(start) == op_Start) {
      return get_glob_type();
    }
  }
  return NULL;
}

/* Test whether arbitrary node is value arg base, i.e. Proj(pn_Start_P_value_arg_base)
 * from Start.  If so returns 1, else 0. */
int is_value_arg_pointer(ir_node *n) {
  if ((get_irn_op(n) == op_Proj) &&
      (get_Proj_proj(n) == pn_Start_P_value_arg_base) &&
      (get_irn_op(get_Proj_pred(n)) == op_Start))
    return 1;
  return 0;
}

/* Returns an array with the predecessors of the Block. Depending on
   the implementation of the graph data structure this can be a copy of
   the internal representation of predecessors as well as the internal
   array itself. Therefore writing to this array might obstruct the ir. */
ir_node **
get_Block_cfgpred_arr (ir_node *node)
{
  assert ((node->op == op_Block));
  return (ir_node **)&(get_irn_in(node)[1]);
}


int
get_Block_n_cfgpreds (ir_node *node) {
  assert ((node->op == op_Block));
  return get_irn_arity(node);
}

ir_node *
get_Block_cfgpred (ir_node *node, int pos) {
  assert(-1 <= pos && pos < get_irn_arity(node));
  assert(node->op == op_Block);
  return get_irn_n(node, pos);
}

void
set_Block_cfgpred (ir_node *node, int pos, ir_node *pred) {
  assert (node->op == op_Block);
  set_irn_n(node, pos, pred);
}

bool
get_Block_matured (ir_node *node) {
  assert (node->op == op_Block);
  return node->attr.block.matured;
}

void
set_Block_matured (ir_node *node, bool matured) {
  assert (node->op == op_Block);
  node->attr.block.matured = matured;
}

unsigned long
(get_Block_block_visited)(ir_node *node) {
  return _get_Block_block_visited(node);
}

void
(set_Block_block_visited)(ir_node *node, unsigned long visit) {
  _set_Block_block_visited(node, visit);
}

/* For this current_ir_graph must be set. */
void
(mark_Block_block_visited)(ir_node *node) {
  _mark_Block_block_visited(node);
}

int
(Block_not_block_visited)(ir_node *node) {
  return _Block_not_block_visited(node);
}

ir_node *
get_Block_graph_arr (ir_node *node, int pos) {
  assert (node->op == op_Block);
  return node->attr.block.graph_arr[pos+1];
}

void
set_Block_graph_arr (ir_node *node, int pos, ir_node *value) {
  assert (node->op == op_Block);
  node->attr.block.graph_arr[pos+1] = value;
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
      int ipv = get_interprocedural_view();
      set_interprocedural_view(true);
      fix_backedges(current_ir_graph->obst, node);
      set_interprocedural_view(ipv);
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

ir_node *(set_Block_dead)(ir_node *block) {
  return _set_Block_dead(block);
}

int (is_Block_dead)(const ir_node *block) {
  return _is_Block_dead(block);
}

void
set_Start_irg(ir_node *node, ir_graph *irg) {
  assert(node->op == op_Start);
  assert(is_ir_graph(irg));
  assert(0 && " Why set irg? -- use set_irn_irg");
}

int
get_End_n_keepalives(ir_node *end) {
  assert (end->op == op_End);
  return (get_irn_arity(end) - END_KEEPALIVE_OFFSET);
}

ir_node *
get_End_keepalive(ir_node *end, int pos) {
  assert (end->op == op_End);
  return get_irn_n(end, pos + END_KEEPALIVE_OFFSET);
}

void
add_End_keepalive (ir_node *end, ir_node *ka) {
  assert (end->op == op_End);
  ARR_APP1 (ir_node *, end->in, ka);
}

void
set_End_keepalive(ir_node *end, int pos, ir_node *ka) {
  assert (end->op == op_End);
  set_irn_n(end, pos + END_KEEPALIVE_OFFSET, ka);
}

void
free_End (ir_node *end) {
  assert (end->op == op_End);
  end->kind = k_BAD;
  DEL_ARR_F(end->in);  /* GL @@@ tut nicht ! */
  end->in = NULL;   /* @@@ make sure we get an error if we use the
               in array afterwards ... */
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
 - determining the biggest case constant to know the proj number for
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
libFIRM can implement a routine that transforms between the two
flavours.  This is also possible for 1) but 2) does not require to
change any existing optimization.
Further it should be far simpler to determine the biggest constant than
to compute all gaps.
I don't want to choose 3) as 2a) seems to have advantages for
dataflow analysis and 3) does not allow to convert the representation to
2a).
*/
ir_node *
get_Cond_selector (ir_node *node) {
  assert (node->op == op_Cond);
  return get_irn_n(node, 0);
}

void
set_Cond_selector (ir_node *node, ir_node *selector) {
  assert (node->op == op_Cond);
  set_irn_n(node, 0, selector);
}

cond_kind
get_Cond_kind (ir_node *node) {
  assert (node->op == op_Cond);
  return node->attr.c.kind;
}

void
set_Cond_kind (ir_node *node, cond_kind kind) {
  assert (node->op == op_Cond);
  node->attr.c.kind = kind;
}

long
get_Cond_defaultProj (ir_node *node) {
  assert (node->op == op_Cond);
  return node->attr.c.default_proj;
}

ir_node *
get_Return_mem (ir_node *node) {
  assert (node->op == op_Return);
  return get_irn_n(node, 0);
}

void
set_Return_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Return);
  set_irn_n(node, 0, mem);
}

int
get_Return_n_ress (ir_node *node) {
  assert (node->op == op_Return);
  return (get_irn_arity(node) - RETURN_RESULT_OFFSET);
}

ir_node **
get_Return_res_arr (ir_node *node)
{
  assert ((node->op == op_Return));
  if (get_Return_n_ress(node) > 0)
    return (ir_node **)&(get_irn_in(node)[1 + RETURN_RESULT_OFFSET]);
  else
    return NULL;
}

/*
void
set_Return_n_res (ir_node *node, int results) {
  assert (node->op == op_Return);
}
*/

ir_node *
get_Return_res (ir_node *node, int pos) {
  assert (node->op == op_Return);
  assert (get_Return_n_ress(node) > pos);
  return get_irn_n(node, pos + RETURN_RESULT_OFFSET);
}

void
set_Return_res (ir_node *node, int pos, ir_node *res){
  assert (node->op == op_Return);
  set_irn_n(node, pos + RETURN_RESULT_OFFSET, res);
}

ir_node *
get_Raise_mem (ir_node *node) {
  assert (node->op == op_Raise);
  return get_irn_n(node, 0);
}

void
set_Raise_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Raise);
  set_irn_n(node, 0, mem);
}

ir_node *
get_Raise_exo_ptr (ir_node *node) {
  assert (node->op == op_Raise);
  return get_irn_n(node, 1);
}

void
set_Raise_exo_ptr (ir_node *node, ir_node *exo_ptr) {
  assert (node->op == op_Raise);
  set_irn_n(node, 1, exo_ptr);
}

tarval *(get_Const_tarval)(ir_node *node) {
	return _get_Const_tarval(node);
}

void
set_Const_tarval (ir_node *node, tarval *con) {
  assert (node->op == op_Const);
  node->attr.con.tv = con;
}

cnst_classify_t (classify_Const)(ir_node *node)
{
	return _classify_Const(node);
}


/* The source language type.  Must be an atomic type.  Mode of type must
   be mode of node. For tarvals from entities type must be pointer to
   entity type. */
type *
get_Const_type (ir_node *node) {
  assert (node->op == op_Const);
  return node->attr.con.tp;
}

void
set_Const_type (ir_node *node, type *tp) {
  assert (node->op == op_Const);
  if (tp != firm_unknown_type) {
    assert (is_atomic_type(tp));
    assert (get_type_mode(tp) == get_irn_mode(node));
  }
  node->attr.con.tp = tp;
}


symconst_kind
get_SymConst_kind (const ir_node *node) {
  assert (node->op == op_SymConst);
  return node->attr.i.num;
}

void
set_SymConst_kind (ir_node *node, symconst_kind num) {
  assert (node->op == op_SymConst);
  node->attr.i.num = num;
}

type *
get_SymConst_type (ir_node *node) {
  assert (   (node->op == op_SymConst)
          && (   get_SymConst_kind(node) == symconst_type_tag
              || get_SymConst_kind(node) == symconst_size));
  return node->attr.i.sym.type_p = skip_tid(node->attr.i.sym.type_p);
}

void
set_SymConst_type (ir_node *node, type *tp) {
  assert (   (node->op == op_SymConst)
          && (   get_SymConst_kind(node) == symconst_type_tag
              || get_SymConst_kind(node) == symconst_size));
  node->attr.i.sym.type_p = tp;
}

ident *
get_SymConst_name (ir_node *node) {
  assert (   (node->op == op_SymConst)
          && (get_SymConst_kind(node) == symconst_addr_name));
  return node->attr.i.sym.ident_p;
}

void
set_SymConst_name (ir_node *node, ident *name) {
  assert (   (node->op == op_SymConst)
          && (get_SymConst_kind(node) == symconst_addr_name));
  node->attr.i.sym.ident_p = name;
}


/* Only to access SymConst of kind symconst_addr_ent.  Else assertion: */
entity   *get_SymConst_entity (ir_node *node) {
  assert (   (node->op == op_SymConst)
          && (get_SymConst_kind (node) == symconst_addr_ent));
  return node->attr.i.sym.entity_p;
}

void     set_SymConst_entity (ir_node *node, entity *ent) {
  assert (   (node->op == op_SymConst)
          && (get_SymConst_kind(node) == symconst_addr_ent));
  node->attr.i.sym.entity_p  = ent;
}

union symconst_symbol
get_SymConst_symbol (ir_node *node) {
  assert (node->op == op_SymConst);
  return node->attr.i.sym;
}

void
set_SymConst_symbol (ir_node *node, union symconst_symbol sym) {
  assert (node->op == op_SymConst);
  //memcpy (&(node->attr.i.sym), sym, sizeof(type_or_id));
  node->attr.i.sym = sym;
}

type *
get_SymConst_value_type (ir_node *node) {
  assert (node->op == op_SymConst);
  if (node->attr.i.tp) node->attr.i.tp = skip_tid(node->attr.i.tp);
  return node->attr.i.tp;
}

void
set_SymConst_value_type (ir_node *node, type *tp) {
  assert (node->op == op_SymConst);
  node->attr.i.tp = tp;
}

ir_node *
get_Sel_mem (ir_node *node) {
  assert (node->op == op_Sel);
  return get_irn_n(node, 0);
}

void
set_Sel_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Sel);
  set_irn_n(node, 0, mem);
}

ir_node *
get_Sel_ptr (ir_node *node) {
  assert (node->op == op_Sel);
  return get_irn_n(node, 1);
}

void
set_Sel_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Sel);
  set_irn_n(node, 1, ptr);
}

int
get_Sel_n_indexs (ir_node *node) {
  assert (node->op == op_Sel);
  return (get_irn_arity(node) - SEL_INDEX_OFFSET);
}

ir_node **
get_Sel_index_arr (ir_node *node)
{
  assert ((node->op == op_Sel));
  if (get_Sel_n_indexs(node) > 0)
    return (ir_node **)& get_irn_in(node)[SEL_INDEX_OFFSET + 1];
  else
    return NULL;
}

ir_node *
get_Sel_index (ir_node *node, int pos) {
  assert (node->op == op_Sel);
  return get_irn_n(node, pos + SEL_INDEX_OFFSET);
}

void
set_Sel_index (ir_node *node, int pos, ir_node *index) {
  assert (node->op == op_Sel);
  set_irn_n(node, pos + SEL_INDEX_OFFSET, index);
}

entity *
get_Sel_entity (ir_node *node) {
  assert (node->op == op_Sel);
  return node->attr.s.ent;
}

void
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


ir_node *
get_Call_mem (ir_node *node) {
  assert (node->op == op_Call);
  return get_irn_n(node, 0);
}

void
set_Call_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Call);
  set_irn_n(node, 0, mem);
}

ir_node *
get_Call_ptr (ir_node *node) {
  assert (node->op == op_Call);
  return get_irn_n(node, 1);
}

void
set_Call_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Call);
  set_irn_n(node, 1, ptr);
}

ir_node **
get_Call_param_arr (ir_node *node) {
  assert (node->op == op_Call);
  return (ir_node **)&get_irn_in(node)[CALL_PARAM_OFFSET + 1];
}

int
get_Call_n_params (ir_node *node)  {
  assert (node->op == op_Call);
  return (get_irn_arity(node) - CALL_PARAM_OFFSET);
}

int
get_Call_arity (ir_node *node) {
  assert (node->op == op_Call);
  return get_Call_n_params(node);
}

/* void
set_Call_arity (ir_node *node, ir_node *arity) {
  assert (node->op == op_Call);
}
*/

ir_node *
get_Call_param (ir_node *node, int pos) {
  assert (node->op == op_Call);
  return get_irn_n(node, pos + CALL_PARAM_OFFSET);
}

void
set_Call_param (ir_node *node, int pos, ir_node *param) {
  assert (node->op == op_Call);
  set_irn_n(node, pos + CALL_PARAM_OFFSET, param);
}

type *
get_Call_type (ir_node *node) {
  assert (node->op == op_Call);
  return node->attr.call.cld_tp = skip_tid(node->attr.call.cld_tp);
}

void
set_Call_type (ir_node *node, type *tp) {
  assert (node->op == op_Call);
  assert ((get_unknown_type() == tp) || is_Method_type(tp));
  node->attr.call.cld_tp = tp;
}

int Call_has_callees(ir_node *node) {
  assert(node && node->op == op_Call);
  return ((get_irg_callee_info_state(get_irn_irg(node)) != irg_callee_info_none) &&
      (node->attr.call.callee_arr != NULL));
}

int get_Call_n_callees(ir_node * node) {
  assert(node && node->op == op_Call && node->attr.call.callee_arr);
  return ARR_LEN(node->attr.call.callee_arr);
}

entity * get_Call_callee(ir_node * node, int pos) {
  assert(pos >= 0 && pos < get_Call_n_callees(node));
  return node->attr.call.callee_arr[pos];
}

void set_Call_callee_arr(ir_node * node, const int n, entity ** arr) {
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
ir_node * get_CallBegin_call (ir_node *node) {
  assert(node->op == op_CallBegin);
  return node->attr.callbegin.call;
}
void  set_CallBegin_call (ir_node *node, ir_node *call) {
  assert(node->op == op_CallBegin);
  node->attr.callbegin.call = call;
}


#define BINOP(OP)                   \
ir_node * get_##OP##_left(ir_node *node) {      \
  assert(node->op == op_##OP);              \
  return get_irn_n(node, node->op->op_index);       \
}                           \
void set_##OP##_left(ir_node *node, ir_node *left) {    \
  assert(node->op == op_##OP);              \
  set_irn_n(node, node->op->op_index, left);        \
}                           \
ir_node *get_##OP##_right(ir_node *node) {      \
  assert(node->op == op_##OP);              \
  return get_irn_n(node, node->op->op_index + 1);   \
}                           \
void set_##OP##_right(ir_node *node, ir_node *right) {  \
  assert(node->op == op_##OP);              \
  set_irn_n(node, node->op->op_index + 1, right);   \
}

#define UNOP(OP)                    \
ir_node *get_##OP##_op(ir_node *node) {         \
  assert(node->op == op_##OP);              \
  return get_irn_n(node, node->op->op_index);       \
}                           \
void set_##OP##_op (ir_node *node, ir_node *op) {   \
  assert(node->op == op_##OP);              \
  set_irn_n(node, node->op->op_index, op);      \
}

BINOP(Add)
BINOP(Sub)
UNOP(Minus)
BINOP(Mul)
BINOP(Quot)

ir_node *
get_Quot_mem (ir_node *node) {
  assert (node->op == op_Quot);
  return get_irn_n(node, 0);
}

void
set_Quot_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Quot);
  set_irn_n(node, 0, mem);
}

BINOP(DivMod)

ir_node *
get_DivMod_mem (ir_node *node) {
  assert (node->op == op_DivMod);
  return get_irn_n(node, 0);
}

void
set_DivMod_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_DivMod);
  set_irn_n(node, 0, mem);
}

BINOP(Div)

ir_node *
get_Div_mem (ir_node *node) {
  assert (node->op == op_Div);
  return get_irn_n(node, 0);
}

void
set_Div_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Div);
  set_irn_n(node, 0, mem);
}

BINOP(Mod)

ir_node *
get_Mod_mem (ir_node *node) {
  assert (node->op == op_Mod);
  return get_irn_n(node, 0);
}

void
set_Mod_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Mod);
  set_irn_n(node, 0, mem);
}

UNOP(Abs)
BINOP(And)
BINOP(Or)
BINOP(Eor)
UNOP(Not)
BINOP(Shl)
BINOP(Shr)
BINOP(Shrs)
BINOP(Rot)
BINOP(Cmp)
UNOP(Conv)
UNOP(Cast)

type *
get_Cast_type (ir_node *node) {
  assert (node->op == op_Cast);
  return node->attr.cast.totype;
}

void
set_Cast_type (ir_node *node, type *to_tp) {
  assert (node->op == op_Cast);
  node->attr.cast.totype = to_tp;
}


/* Checks for upcast.
 *
 * Returns true if the Cast node casts a class type to a super type.
 */
int is_Cast_upcast(ir_node *node) {
  type *totype   = get_Cast_type(node);
  type *fromtype = get_irn_typeinfo_type(get_Cast_op(node));
  ir_graph *myirg = get_irn_irg(node);

  assert(get_irg_typeinfo_state(myirg) == ir_typeinfo_consistent);
  assert(fromtype);

  while (is_Pointer_type(totype) && is_Pointer_type(fromtype)) {
    totype   = get_pointer_points_to_type(totype);
    fromtype = get_pointer_points_to_type(fromtype);
  }

  assert(fromtype);

  if (!is_Class_type(totype)) return false;
  return is_subclass_of(fromtype, totype);
}

/* Checks for downcast.
 *
 * Returns true if the Cast node casts a class type to a sub type.
 */
int is_Cast_downcast(ir_node *node) {
  type *totype   = get_Cast_type(node);
  type *fromtype = get_irn_typeinfo_type(get_Cast_op(node));

  assert(get_irg_typeinfo_state(get_irn_irg(node)) == ir_typeinfo_consistent);
  assert(fromtype);

  while (is_Pointer_type(totype) && is_Pointer_type(fromtype)) {
    totype   = get_pointer_points_to_type(totype);
    fromtype = get_pointer_points_to_type(fromtype);
  }

  assert(fromtype);

  if (!is_Class_type(totype)) return false;
  return is_subclass_of(totype, fromtype);
}

int
(is_unop)(const ir_node *node) {
  return _is_unop(node);
}

ir_node *
get_unop_op (ir_node *node) {
  if (node->op->opar == oparity_unary)
    return get_irn_n(node, node->op->op_index);

  assert(node->op->opar == oparity_unary);
  return NULL;
}

void
set_unop_op (ir_node *node, ir_node *op) {
  if (node->op->opar == oparity_unary)
    set_irn_n(node, node->op->op_index, op);

  assert(node->op->opar == oparity_unary);
}

int
(is_binop)(const ir_node *node) {
  return _is_binop(node);
}

ir_node *
get_binop_left (ir_node *node) {
  if (node->op->opar == oparity_binary)
    return get_irn_n(node, node->op->op_index);

  assert(node->op->opar == oparity_binary);
  return NULL;
}

void
set_binop_left (ir_node *node, ir_node *left) {
  if (node->op->opar == oparity_binary)
    set_irn_n(node, node->op->op_index, left);

  assert (node->op->opar == oparity_binary);
}

ir_node *
get_binop_right (ir_node *node) {
  if (node->op->opar == oparity_binary)
    return get_irn_n(node, node->op->op_index + 1);

  assert(node->op->opar == oparity_binary);
  return NULL;
}

void
set_binop_right (ir_node *node, ir_node *right) {
  if (node->op->opar == oparity_binary)
    set_irn_n(node, node->op->op_index + 1, right);

  assert (node->op->opar == oparity_binary);
}

int is_Phi (const ir_node *n) {
  ir_op *op;

  assert(n);
  op = get_irn_op(n);

  if (op == op_Filter) return get_interprocedural_view();

  if (op == op_Phi)
    return  ((get_irg_phase_state(get_irn_irg(n)) !=  phase_building) ||
         (get_irn_arity(n) > 0));

  return 0;
}

int is_Phi0 (const ir_node *n) {
  assert(n);

  return ((get_irn_op(n) == op_Phi) &&
      (get_irn_arity(n) == 0) &&
      (get_irg_phase_state(get_irn_irg(n)) ==  phase_building));
}

ir_node **
get_Phi_preds_arr (ir_node *node) {
  assert (node->op == op_Phi);
  return (ir_node **)&(get_irn_in(node)[1]);
}

int
get_Phi_n_preds (ir_node *node) {
  assert (is_Phi(node) || is_Phi0(node));
  return (get_irn_arity(node));
}

/*
void set_Phi_n_preds (ir_node *node, int n_preds) {
  assert (node->op == op_Phi);
}
*/

ir_node *
get_Phi_pred (ir_node *node, int pos) {
  assert (is_Phi(node) || is_Phi0(node));
  return get_irn_n(node, pos);
}

void
set_Phi_pred (ir_node *node, int pos, ir_node *pred) {
  assert (is_Phi(node) || is_Phi0(node));
  set_irn_n(node, pos, pred);
}


int is_memop(ir_node *node) {
  return ((get_irn_op(node) == op_Load) || (get_irn_op(node) == op_Store));
}

ir_node *get_memop_mem (ir_node *node) {
  assert(is_memop(node));
  return get_irn_n(node, 0);
}

void     set_memop_mem (ir_node *node, ir_node *mem) {
  assert(is_memop(node));
  set_irn_n(node, 0, mem);
}

ir_node *get_memop_ptr (ir_node *node) {
  assert(is_memop(node));
  return get_irn_n(node, 1);
}

void     set_memop_ptr (ir_node *node, ir_node *ptr) {
  assert(is_memop(node));
  set_irn_n(node, 1, ptr);
}

ir_node *
get_Load_mem (ir_node *node) {
  assert (node->op == op_Load);
  return get_irn_n(node, 0);
}

void
set_Load_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Load);
  set_irn_n(node, 0, mem);
}

ir_node *
get_Load_ptr (ir_node *node) {
  assert (node->op == op_Load);
  return get_irn_n(node, 1);
}

void
set_Load_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Load);
  set_irn_n(node, 1, ptr);
}

ir_mode *
get_Load_mode (ir_node *node) {
  assert (node->op == op_Load);
  return node->attr.load.load_mode;
}

void
set_Load_mode (ir_node *node, ir_mode *mode) {
  assert (node->op == op_Load);
  node->attr.load.load_mode = mode;
}

ent_volatility
get_Load_volatility (ir_node *node) {
  assert (node->op == op_Load);
  return node->attr.load.volatility;
}

void
set_Load_volatility (ir_node *node, ent_volatility volatility) {
  assert (node->op == op_Load);
  node->attr.load.volatility = volatility;
}


ir_node *
get_Store_mem (ir_node *node) {
  assert (node->op == op_Store);
  return get_irn_n(node, 0);
}

void
set_Store_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Store);
  set_irn_n(node, 0, mem);
}

ir_node *
get_Store_ptr (ir_node *node) {
  assert (node->op == op_Store);
  return get_irn_n(node, 1);
}

void
set_Store_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Store);
  set_irn_n(node, 1, ptr);
}

ir_node *
get_Store_value (ir_node *node) {
  assert (node->op == op_Store);
  return get_irn_n(node, 2);
}

void
set_Store_value (ir_node *node, ir_node *value) {
  assert (node->op == op_Store);
  set_irn_n(node, 2, value);
}

ent_volatility
get_Store_volatility (ir_node *node) {
  assert (node->op == op_Store);
  return node->attr.store.volatility;
}

void
set_Store_volatility (ir_node *node, ent_volatility volatility) {
  assert (node->op == op_Store);
  node->attr.store.volatility = volatility;
}


ir_node *
get_Alloc_mem (ir_node *node) {
  assert (node->op == op_Alloc);
  return get_irn_n(node, 0);
}

void
set_Alloc_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Alloc);
  set_irn_n(node, 0, mem);
}

ir_node *
get_Alloc_size (ir_node *node) {
  assert (node->op == op_Alloc);
  return get_irn_n(node, 1);
}

void
set_Alloc_size (ir_node *node, ir_node *size) {
  assert (node->op == op_Alloc);
  set_irn_n(node, 1, size);
}

type  *
get_Alloc_type (ir_node *node) {
  assert (node->op == op_Alloc);
  return node->attr.a.type = skip_tid(node->attr.a.type);
}

void
set_Alloc_type (ir_node *node, type *tp) {
  assert (node->op == op_Alloc);
  node->attr.a.type = tp;
}

where_alloc
get_Alloc_where (ir_node *node) {
  assert (node->op == op_Alloc);
  return node->attr.a.where;
}

void
set_Alloc_where (ir_node *node, where_alloc where) {
  assert (node->op == op_Alloc);
  node->attr.a.where = where;
}


ir_node *
get_Free_mem (ir_node *node) {
  assert (node->op == op_Free);
  return get_irn_n(node, 0);
}

void
set_Free_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Free);
  set_irn_n(node, 0, mem);
}

ir_node *
get_Free_ptr (ir_node *node) {
  assert (node->op == op_Free);
  return get_irn_n(node, 1);
}

void
set_Free_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Free);
  set_irn_n(node, 1, ptr);
}

ir_node *
get_Free_size (ir_node *node) {
  assert (node->op == op_Free);
  return get_irn_n(node, 2);
}

void
set_Free_size (ir_node *node, ir_node *size) {
  assert (node->op == op_Free);
  set_irn_n(node, 2, size);
}

type  *
get_Free_type (ir_node *node) {
  assert (node->op == op_Free);
  return node->attr.f.type = skip_tid(node->attr.f.type);
}

void
set_Free_type (ir_node *node, type *tp) {
  assert (node->op == op_Free);
  node->attr.f.type = tp;
}

where_alloc
get_Free_where (ir_node *node) {
  assert (node->op == op_Free);
  return node->attr.f.where;
}

void
set_Free_where (ir_node *node, where_alloc where) {
  assert (node->op == op_Free);
  node->attr.f.where = where;
}

ir_node **
get_Sync_preds_arr (ir_node *node) {
  assert (node->op == op_Sync);
  return (ir_node **)&(get_irn_in(node)[1]);
}

int
get_Sync_n_preds (ir_node *node) {
  assert (node->op == op_Sync);
  return (get_irn_arity(node));
}

/*
void
set_Sync_n_preds (ir_node *node, int n_preds) {
  assert (node->op == op_Sync);
}
*/

ir_node *
get_Sync_pred (ir_node *node, int pos) {
  assert (node->op == op_Sync);
  return get_irn_n(node, pos);
}

void
set_Sync_pred (ir_node *node, int pos, ir_node *pred) {
  assert (node->op == op_Sync);
  set_irn_n(node, pos, pred);
}

type *get_Proj_type(ir_node *n)
{
  type *tp      = NULL;
  ir_node *pred = get_Proj_pred(n);

  switch (get_irn_opcode(pred)) {
  case iro_Proj: {
    ir_node *pred_pred;
    /* Deal with Start / Call here: we need to know the Proj Nr. */
    assert(get_irn_mode(pred) == mode_T);
    pred_pred = get_Proj_pred(pred);
    if (get_irn_op(pred_pred) == op_Start)  {
      type *mtp = get_entity_type(get_irg_entity(get_irn_irg(pred_pred)));
      tp = get_method_param_type(mtp, get_Proj_proj(n));
    } else if (get_irn_op(pred_pred) == op_Call) {
      type *mtp = get_Call_type(pred_pred);
      tp = get_method_res_type(mtp, get_Proj_proj(n));
    }
  } break;
  case iro_Start: break;
  case iro_Call: break;
  case iro_Load: {
    ir_node *a = get_Load_ptr(pred);
    if (get_irn_op(a) == op_Sel)
      tp = get_entity_type(get_Sel_entity(a));
  } break;
  default:
    break;
  }
  return tp;
}

ir_node *
get_Proj_pred (ir_node *node) {
  assert (is_Proj(node));
  return get_irn_n(node, 0);
}

void
set_Proj_pred (ir_node *node, ir_node *pred) {
  assert (is_Proj(node));
  set_irn_n(node, 0, pred);
}

long
get_Proj_proj (ir_node *node) {
  assert (is_Proj(node));
  if (get_irn_opcode(node) == iro_Proj) {
    return node->attr.proj;
  } else {
    assert(get_irn_opcode(node) == iro_Filter);
    return node->attr.filter.proj;
  }
}

void
set_Proj_proj (ir_node *node, long proj) {
  assert (node->op == op_Proj);
  node->attr.proj = proj;
}

ir_node **
get_Tuple_preds_arr (ir_node *node) {
  assert (node->op == op_Tuple);
  return (ir_node **)&(get_irn_in(node)[1]);
}

int
get_Tuple_n_preds (ir_node *node) {
  assert (node->op == op_Tuple);
  return (get_irn_arity(node));
}

/*
void
set_Tuple_n_preds (ir_node *node, int n_preds) {
  assert (node->op == op_Tuple);
}
*/

ir_node *
get_Tuple_pred (ir_node *node, int pos) {
  assert (node->op == op_Tuple);
  return get_irn_n(node, pos);
}

void
set_Tuple_pred (ir_node *node, int pos, ir_node *pred) {
  assert (node->op == op_Tuple);
  set_irn_n(node, pos, pred);
}

ir_node *
get_Id_pred (ir_node *node) {
  assert (node->op == op_Id);
  return get_irn_n(node, 0);
}

void
set_Id_pred (ir_node *node, ir_node *pred) {
  assert (node->op == op_Id);
  set_irn_n(node, 0, pred);
}

ir_node *get_Confirm_value (ir_node *node) {
  assert (node->op == op_Confirm);
  return get_irn_n(node, 0);
}
void     set_Confirm_value (ir_node *node, ir_node *value) {
  assert (node->op == op_Confirm);
  set_irn_n(node, 0, value);
}
ir_node *get_Confirm_bound (ir_node *node) {
  assert (node->op == op_Confirm);
  return get_irn_n(node, 1);
}
void     set_Confirm_bound (ir_node *node, ir_node *bound) {
  assert (node->op == op_Confirm);
  set_irn_n(node, 0, bound);
}
pn_Cmp   get_Confirm_cmp   (ir_node *node) {
  assert (node->op == op_Confirm);
  return node->attr.confirm_cmp;
}
void     set_Confirm_cmp   (ir_node *node, pn_Cmp cmp) {
  assert (node->op == op_Confirm);
  node->attr.confirm_cmp = cmp;
}


ir_node *
get_Filter_pred (ir_node *node) {
  assert(node->op == op_Filter);
  return node->in[1];
}
void
set_Filter_pred (ir_node *node, ir_node *pred) {
  assert(node->op == op_Filter);
  node->in[1] = pred;
}
long
get_Filter_proj(ir_node *node) {
  assert(node->op == op_Filter);
  return node->attr.filter.proj;
}
void
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

/* Mux support */
ir_node *get_Mux_sel   (ir_node *node) {
  assert(node->op == op_Mux);
  return node->in[1];
}
void     set_Mux_sel   (ir_node *node, ir_node *sel) {
  assert(node->op == op_Mux);
  node->in[1] = sel;
}

ir_node *get_Mux_false (ir_node *node) {
  assert(node->op == op_Mux);
  return node->in[2];
}
void     set_Mux_false (ir_node *node, ir_node *ir_false) {
  assert(node->op == op_Mux);
  node->in[2] = ir_false;
}

ir_node *get_Mux_true  (ir_node *node) {
  assert(node->op == op_Mux);
  return node->in[3];
}
void     set_Mux_true  (ir_node *node, ir_node *ir_true) {
  assert(node->op == op_Mux);
  node->in[3] = ir_true;
}


ir_graph *
get_irn_irg(const ir_node *node) {
  if (! is_Block(node))
    node = get_nodes_block(node);
  if (is_Bad(node))  /* sometimes bad is predecessor of nodes instead of block: in case of optimization */
    node = get_nodes_block(node);
  assert(get_irn_op(node) == op_Block);
  return node->attr.block.irg;
}


/*----------------------------------------------------------------*/
/*  Auxiliary routines                                            */
/*----------------------------------------------------------------*/

ir_node *
skip_Proj (ir_node *node) {
  /* don't assert node !!! */
  if (node && is_Proj(node)) {
    return get_Proj_pred(node);
  } else {
    return node;
  }
}

ir_node *
skip_Tuple (ir_node *node) {
  ir_node *pred;

  if (!get_opt_normalize()) return node;

  node = skip_Id(node);
  if (get_irn_op(node) == op_Proj) {
    pred = skip_Id(get_Proj_pred(node));
    if (get_irn_op(pred) == op_Proj) /* nested Tuple ? */
      pred = skip_Id(skip_Tuple(pred));
    if (get_irn_op(pred) == op_Tuple)
      return get_Tuple_pred(pred, get_Proj_proj(node));
  }
  return node;
}

/** returns operand of node if node is a Cast */
ir_node *skip_Cast  (ir_node *node) {
  if (node && get_irn_op(node) == op_Cast) {
    return skip_Id(get_irn_n(node, 0));
  } else {
    return node;
  }
}

#if 0
/* This should compact Id-cycles to self-cycles. It has the same (or less?) complexity
   than any other approach, as Id chains are resolved and all point to the real node, or
   all id's are self loops. */
ir_node *
skip_Id (ir_node *node) {
  /* don't assert node !!! */

  if (!get_opt_normalize()) return node;

  /* Don't use get_Id_pred:  We get into an endless loop for
     self-referencing Ids. */
  if (node && (node->op == op_Id) && (node != node->in[0+1])) {
    ir_node *rem_pred = node->in[0+1];
    ir_node *res;

    assert (get_irn_arity (node) > 0);

    node->in[0+1] = node;
    res = skip_Id(rem_pred);
    if (res->op == op_Id) /* self-loop */ return node;

    node->in[0+1] = res;
    return res;
  } else {
    return node;
  }
}
#else
/* This should compact Id-cycles to self-cycles. It has the same (or less?) complexity
   than any other approach, as Id chains are resolved and all point to the real node, or
   all id's are self loops. */
ir_node *
skip_Id (ir_node *node) {
  ir_node *pred;
  /* don't assert node !!! */

  if (!node || (node->op != op_Id)) return node;

  if (!get_opt_normalize()) return node;

  /* Don't use get_Id_pred:  We get into an endless loop for
     self-referencing Ids. */
  pred = node->in[0+1];

  if (pred->op != op_Id) return pred;

  if (node != pred) {  /* not a self referencing Id. Resolve Id chain. */
    ir_node *rem_pred, *res;

    if (pred->op != op_Id) return pred; /* shortcut */
    rem_pred = pred;

    assert (get_irn_arity (node) > 0);

    node->in[0+1] = node;   /* turn us into a self referencing Id:  shorten Id cycles. */
    res = skip_Id(rem_pred);
    if (res->op == op_Id) /* self-loop */ return node;

    node->in[0+1] = res;    /* Turn Id chain into Ids all referencing the chain end. */
    return res;
  } else {
    return node;
  }
}
#endif

int
(is_Bad)(const ir_node *node) {
  return _is_Bad(node);
}

int
(is_Const)(const ir_node *node) {
	return _is_Const(node);
}

int
(is_no_Block)(const ir_node *node) {
  return _is_no_Block(node);
}

int
(is_Block)(const ir_node *node) {
  return _is_Block(node);
}

/* returns true if node is a Unknown node. */
int
is_Unknown (const ir_node *node) {
  assert(node);
  return (get_irn_op(node) == op_Unknown);
}

int
is_Proj (const ir_node *node) {
  assert(node);
  return node->op == op_Proj
    || (!get_interprocedural_view() && node->op == op_Filter);
}

/* Returns true if the operation manipulates control flow. */
int
is_cfop(const ir_node *node) {
  return is_cfopcode(get_irn_op(node));
}

/* Returns true if the operation manipulates interprocedural control flow:
   CallBegin, EndReg, EndExcept */
int is_ip_cfop(const ir_node *node) {
  return is_ip_cfopcode(get_irn_op(node));
}

/* Returns true if the operation can change the control flow because
   of an exception. */
int
is_fragile_op(const ir_node *node) {
  return is_op_fragile(get_irn_op(node));
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
    assert(0 && "should not be reached");
    return NULL;
  }
}

/* Returns true if the operation is a forking control flow operation. */
int
is_forking_op(const ir_node *node) {
  return is_op_forking(get_irn_op(node));
}

type *(get_irn_type)(ir_node *node) {
  return _get_irn_type(node);
}

/** the get_type operation must be always implemented */
static type *get_Null_type(ir_node *n) {
  return NULL;
}

/* set the get_type operation */
ir_op *firm_set_default_get_type(ir_op *op)
{
  switch (op->code) {
  case iro_Const:    op->get_type = get_Const_type; break;
  case iro_SymConst: op->get_type = get_SymConst_value_type; break;
  case iro_Cast:     op->get_type = get_Cast_type; break;
  case iro_Proj:     op->get_type = get_Proj_type; break;
  default:           op->get_type = get_Null_type; break;
  }
  return op;
}

#ifdef DEBUG_libfirm
void dump_irn (ir_node *n) {
  int i, arity = get_irn_arity(n);
  printf("%s%s: %ld (%p)\n", get_irn_opname(n), get_mode_name(get_irn_mode(n)), get_irn_node_nr(n), (void *)n);
  if (!is_Block(n)) {
    ir_node *pred = get_irn_n(n, -1);
    printf("  block: %s%s: %ld (%p)\n", get_irn_opname(pred), get_mode_name(get_irn_mode(pred)),
       get_irn_node_nr(pred), (void *)pred);
  }
  printf("  preds: \n");
  for (i = 0; i < arity; ++i) {
    ir_node *pred = get_irn_n(n, i);
    printf("    %d: %s%s: %ld (%p)\n", i, get_irn_opname(pred), get_mode_name(get_irn_mode(pred)),
       get_irn_node_nr(pred), (void *)pred);
  }
}

#else  /* DEBUG_libfirm */
void dump_irn (ir_node *n) {}
#endif /* DEBUG_libfirm */
