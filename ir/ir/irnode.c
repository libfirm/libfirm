/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer, Goetz Lindenmaier
**
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "irnode_t.h"
#include "irgraph_t.h"
#include "ident_t.h"
#include "irmode_t.h"
#include "array.h"

#ifdef DEBUG_libfirm
#include "irprog.h"
#endif

/* some constants fixing the positions of nodes predecessors
   in the in array */
#define CALL_PARAM_OFFSET 2
#define SEL_INDEX_OFFSET 2
#define RETURN_RESULT_OFFSET 1  /* mem is not a result */

static char *pnc_name_arr [] = {"False", "Eq", "Lt", "Le",
				"Gt", "Ge", "Lg", "Leg", "Uo",
				"Ue", "Ul", "Ule", "Ug", "Uge",
				"Ne", "True" };

inline char *get_pnc_string(int pnc) {
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

static char *pns_name_arr [] = {"initial_exec", "global_store",
				"frame_base", "globals", "args"};

static char *symconst_name_arr [] = {"type_tag", "size", "linkage_ptr_info"};

void
init_irnode (void)
{
}

/* irnode constructor                                             */
/* create a new irnode in irg, with an op, mode, arity and        */
/* some incoming irnodes                                          */
/* this constructor is used in every specified irnode constructor */
inline ir_node *
new_ir_node (ir_graph *irg, ir_node *block, ir_op *op, ir_mode *mode,
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
    res->in = NEW_ARR_F (ir_node *, 1);
  } else {
    res->in = NEW_ARR_D (ir_node *, irg->obst, (arity+1));
    memcpy (&res->in[1], in, sizeof (ir_node *) * arity);
  }
  res->in[0] = block;

#ifdef DEBUG_libfirm
  res->node_nr = get_irp_new_node_nr();
#endif

  return res;
}

/* Copies all attributes stored in the old node to the new node.
   Assumes both have the same opcode and sufficient size. */
void
copy_attrs (ir_node *old, ir_node *new) {
  assert (get_irn_opcode(old) == get_irn_opcode(new));
  memcpy (&new->attr, &old->attr, get_op_attr_size(get_irn_op(old)));
}

/* IR-Nodes with attributes */
int
ir_node_print (XP_PAR1, const xprintf_info *info ATTRIBUTE((unused)), XP_PARN)
{
  int printed = 0;
  ir_node *np = XP_GETARG (ir_node *, 0);

  if (!np) {
    XPS ("<null ir_node>");
    return printed;
  }

  XPF1 ("%I", get_irn_opident(np));

  switch (get_irn_opcode (np)) {	/* node label */
  case iro_Const:
    XPF1 ("%I", get_irn_mode(np)->name);
    XPS (" : ");
    XPF1 ("%v", get_irn_const_attr);
    break;
  case iro_Proj:
    if (get_irn_modecode (np) == irm_b) {
      XPC (" ");
      XP (pnc_name_arr[get_irn_proj_attr(np)]);
    } else if (get_irn_opcode (get_irn_in (np)[1]) == iro_Start) {
      XPC (" ");
      XP (pns_name_arr[get_irn_proj_attr(np)]);
    } else {
      XPF1 ("%I", get_irn_mode(np)->name);
      XPC (" ");
      XPF1 ("%d", get_irn_proj_attr(np));
    }
    break;
  case iro_SymConst:
    XPF1 ("%I", get_irn_mode(np)->name);
    XPC  (" ");
    XP   (symconst_name_arr[get_irn_symconst_attr(np).num]);
    XPF1 (" %#N", get_type_nameid(get_SymConst_type(np)));
    break;
  case iro_Start:		/* don't dump mode of these */
  case iro_Cond:
  case iro_Block:
  case iro_Call:
  case iro_Jmp:
  case iro_Return:
  case iro_End:
    break;
  default:
    XPF1 ("%I", get_irn_mode(np)->name);
  }

  return printed;
}

/** getting some parameters from ir_nodes **/

/* returns the number of predecessors without the block predecessor. */
inline int
get_irn_arity (ir_node *node)
{
  assert(node);
  return (ARR_LEN((node)->in)-1);
}

/* Returns the array with ins. This array is shifted with respect to the
   array accessed by get_irn_n: The block operand is at position 0 not -1.
   (@@@ This should be changed.)
   The order of the predecessors in this array is not guaranteed, except that
   lists of operands as predecessors of Block or arguments of a Call are
   consecutive. */
inline ir_node **
get_irn_in (ir_node *node)
{
  assert (node);
  return ((node)->in);
}

/* to iterate through the predecessors without touching the array */
/* To iterate over the operands iterate from 0 to i < get_irn_arity(),
   to iterate includind the Block predecessor iterate from i = -1 to
   i < get_irn_arity.
   If it is a block, the entry -1 is NULL. */
inline ir_node *
get_irn_n (ir_node *node, int n)
{
  assert (node);
  assert (get_irn_arity (node) > n);
  return skip_nop(node->in[n+1]);
}

inline void
set_irn_n (ir_node *node, int n, ir_node *in)
{
  assert (node);
  assert (get_irn_arity (node) > n);
  node->in[n+1] = in;
}

inline ir_mode *
get_irn_mode (ir_node *node)
{
  assert (node);
  return node->mode;
}

inline modecode
get_irn_modecode (ir_node *node)
{
  assert (node);
  return node->mode->code;
}


inline ident *
get_irn_modeident (ir_node *node)
{
  assert(node);
  return node->mode->name;
}

inline ir_op *
get_irn_op (ir_node *node)
{
  assert (node);
  return node->op;
}

/* should be private to the library: */
inline void
set_irn_op (ir_node *node, ir_op *op)
{
  assert (node);
  node->op = op;
}

inline opcode
get_irn_opcode (ir_node *node)
{
  assert (node);
  return node->op->code;
}

inline const char *
get_irn_opname (ir_node *node)
{
  assert(node);
  return id_to_str(node->op->name);
}

inline ident *
get_irn_opident (ir_node *node)
{
  assert(node);
  return node->op->name;
}

inline unsigned long
get_irn_visited (ir_node *node)
{
  assert (node);
  return node->visited;
}

inline void
set_irn_visited (ir_node *node, unsigned long visited)
{
  assert (node);
  node->visited = visited;
}
inline void
set_irn_link (ir_node *node, ir_node *link) {
  assert (node);
  node->link = link;
}

inline ir_node *
get_irn_link (ir_node *node) {
  assert (node);
  return node->link;
}

#ifdef DEBUG_libfirm
/* Outputs a unique number for this node */
inline long
get_irn_node_nr(ir_node *node) {
  assert(node);
  return node->node_nr;
}
#endif

inline tarval *
get_irn_const_attr (ir_node *node)
{
  assert (node->op == op_Const);
  return node->attr.con;
}

inline long
get_irn_proj_attr (ir_node *node)
{
  assert (node->op == op_Proj);
  return node->attr.proj;
}

inline alloc_attr
get_irn_alloc_attr (ir_node *node)
{
  assert (node->op == op_Alloc);
  return node->attr.a;
}

inline type *
get_irn_free_attr     (ir_node *node)
{
  assert (node->op == op_Free);
  return node->attr.f;
}

inline symconst_attr
get_irn_symconst_attr (ir_node *node)
{
  assert (node->op == op_SymConst);
  return node->attr.i;
}

inline type *
get_irn_call_attr (ir_node *node)
{
  assert (node->op == op_Call);
  return node->attr.call;
}

inline sel_attr
get_irn_sel_attr (ir_node *node)
{
  assert (node->op == op_Sel);
  return node->attr.s;
}

inline int
get_irn_phi_attr (ir_node *node)
{
  assert (node->op == op_Phi);
  return node->attr.phi0_pos;
}

inline block_attr
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

inline void
set_nodes_Block (ir_node *node, ir_node *block) {
  assert (!(node->op == op_Block));
  set_irn_n(node, -1, block);
}

/* Returns an array with the predecessors of the Block. Depending on
   the implementation of the graph datastructure this can be a copy of
   the internal representation of predecessors as well as the internal
   array itself. Therefore writing to this array might obstruct the ir. */
inline ir_node **
get_Block_cfgpred_arr (ir_node *node)
{
  assert ((node->op == op_Block));
  return (ir_node **)&(get_irn_in(node)[1]);
}


inline int
get_Block_n_cfgpreds (ir_node *node) {
  assert ((node->op == op_Block));
  return (get_irn_arity(node));
}

/*
inline void
set_Block_n_cfgpreds (ir_node *node, int n_preds) {
  assert ((node->op == op_Block));
}
*/

inline ir_node *
get_Block_cfgpred (ir_node *node, int pos) {
  assert (node->op == op_Block);
  return get_irn_n(node, pos);
}

inline void
set_Block_cfgpred (ir_node *node, int pos, ir_node *pred) {
  assert (node->op == op_Block);
  set_irn_n(node, pos, pred);
}

inline bool
get_Block_matured (ir_node *node) {
  assert (node->op == op_Block);
  return node->attr.block.matured;
}

inline void
set_Block_matured (ir_node *node, bool matured) {
  assert (node->op == op_Block);
  node->attr.block.matured = matured;
}
inline unsigned long
get_Block_block_visited (ir_node *node) {
  assert (node->op == op_Block);
  return node->attr.block.block_visited;
}

inline void
set_Block_block_visited (ir_node *node, unsigned long visit) {
  assert (node->op == op_Block);
  node->attr.block.block_visited = visit;
}

inline ir_node *
get_Block_graph_arr (ir_node *node, int pos) {
  assert (node->op == op_Block);
  return node->attr.block.graph_arr[pos+1];
}

inline void
set_Block_graph_arr (ir_node *node, int pos, ir_node *value) {
  assert (node->op == op_Block);
  node->attr.block.graph_arr[pos+1] = value;
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
inline ir_node *
get_Cond_selector (ir_node *node) {
  assert (node->op == op_Cond);
  return get_irn_n(node, 0);
}

inline void
set_Cond_selector (ir_node *node, ir_node *selector) {
  assert (node->op == op_Cond);
  set_irn_n(node, 0, selector);
}

inline cond_kind
get_Cond_kind (ir_node *node) {
  assert (node->op == op_Cond);
  return node->attr.c;
}

inline void
set_Cond_kind (ir_node *node, cond_kind kind) {
  assert (node->op == op_Cond);
  node->attr.c = kind;
}

inline ir_node *
get_Return_mem (ir_node *node) {
  assert (node->op == op_Return);
  return get_irn_n(node, 0);
}

inline void
set_Return_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Return);
  set_irn_n(node, 0, mem);
}

inline ir_node **
get_Return_res_arr (ir_node *node)
{
  assert ((node->op == op_Return));
  if (get_Return_n_res(node) > 0)
    return (ir_node **)&(get_irn_in(node)[1 + RETURN_RESULT_OFFSET]);
  else
    return NULL;
}

inline int
get_Return_n_res (ir_node *node) {
  assert (node->op == op_Return);
  return (get_irn_arity(node) - RETURN_RESULT_OFFSET);
}

/*
inline void
set_Return_n_res (ir_node *node, int results) {
  assert (node->op == op_Return);
}
*/

inline ir_node *
get_Return_res (ir_node *node, int pos) {
  assert (node->op == op_Return);
  assert (get_Return_n_res(node) > pos);
  return get_irn_n(node, pos + RETURN_RESULT_OFFSET);
}

inline void
set_Return_res (ir_node *node, int pos, ir_node *res){
  assert (node->op == op_Return);
  set_irn_n(node, pos + RETURN_RESULT_OFFSET, res);
}

inline ir_node *
get_Raise_mem (ir_node *node) {
  assert (node->op == op_Return);
  return get_irn_n(node, 0);
}

inline void
set_Raise_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Raise);
  set_irn_n(node, 0, mem);
}

inline ir_node *
get_Raise_exo_ptr (ir_node *node) {
  assert (node->op == op_Raise);
  return get_irn_n(node, 1);
}

inline void
set_Raise_exo_ptr (ir_node *node, ir_node *exo_ptr) {
  assert (node->op == op_Raise);
  set_irn_n(node, 1, exo_ptr);
}

inline tarval *get_Const_tarval (ir_node *node) {
  assert (node->op == op_Const);
  return get_irn_const_attr(node);
}

inline void
set_Const_tarval (ir_node *node, tarval *con) {
  assert (node->op == op_Const);
  node->attr.con = con;
}

inline symconst_kind
get_SymConst_kind (ir_node *node) {
  assert (node->op == op_SymConst);
  return node->attr.i.num;
}

inline void
set_SymConst_kind (ir_node *node, symconst_kind num) {
  assert (node->op == op_SymConst);
  node->attr.i.num = num;
}

inline type *
get_SymConst_type (ir_node *node) {
  assert (   (node->op == op_SymConst)
          && (   get_SymConst_kind(node) == type_tag
              || get_SymConst_kind(node) == size));
  return node->attr.i.tori.typ;
}

inline void
set_SymConst_type (ir_node *node, type *type) {
  assert (   (node->op == op_SymConst)
          && (   get_SymConst_kind(node) == type_tag
              || get_SymConst_kind(node) == size));
  node->attr.i.tori.typ = type;
}

inline ident *
get_SymConst_ptrinfo (ir_node *node) {
  assert (   (node->op == op_SymConst)
          && (get_SymConst_kind(node) == linkage_ptr_info));
  return node->attr.i.tori.ptrinfo;
}

inline void
set_SymConst_ptrinfo (ir_node *node, ident *ptrinfo) {
  assert (   (node->op == op_SymConst)
          && (get_SymConst_kind(node) == linkage_ptr_info));
  node->attr.i.tori.ptrinfo = ptrinfo;
}

inline type_or_id_p
get_SymConst_type_or_id (ir_node *node) {
  assert (node->op == op_SymConst);
  return &(node->attr.i.tori);
}

inline void
set_SymConst_type_or_id (ir_node *node, type_or_id_p tori) {
  assert (node->op == op_SymConst);
  memcpy (&(node->attr.i.tori), tori, sizeof(type_or_id));
}

inline ir_node *
get_Sel_mem (ir_node *node) {
  assert (node->op == op_Sel);
  return get_irn_n(node, 0);
}

inline void
set_Sel_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Sel);
  set_irn_n(node, 0, mem);
}

inline ir_node *
get_Sel_ptr (ir_node *node) {
  assert (node->op == op_Sel);
  return get_irn_n(node, 1);
}

inline void
set_Sel_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Sel);
  set_irn_n(node, 1, ptr);
}

inline ir_node **
get_Sel_index_arr (ir_node *node)
{
  assert ((node->op == op_Sel));
  if (get_Sel_n_index(node) > 0)
    return (ir_node **)& get_irn_in(node)[SEL_INDEX_OFFSET + 1];
  else
    return NULL;
}

inline int
get_Sel_n_index (ir_node *node) {
  assert (node->op == op_Sel);
  return (get_irn_arity(node) - SEL_INDEX_OFFSET);
}

/*
inline void
set_Sel_n_index (ir_node *node, int n_index) {
  assert (node->op == op_Sel);
}
*/

inline ir_node *
get_Sel_index (ir_node *node, int pos) {
  assert (node->op == op_Sel);
  return get_irn_n(node, pos + SEL_INDEX_OFFSET);
}

inline void
set_Sel_index (ir_node *node, int pos, ir_node *index) {
  assert (node->op == op_Sel);
  set_irn_n(node, pos + SEL_INDEX_OFFSET, index);
}

inline entity *
get_Sel_entity (ir_node *node) {
  assert (node->op == op_Sel);
  return node->attr.s.ent;
}

inline void
set_Sel_entity (ir_node *node, entity *ent) {
  assert (node->op == op_Sel);
  node->attr.s.ent = ent;
}

inline linkage_type
get_Sel_linkage_type (ir_node *node) {
  assert (node->op == op_Sel);
  return node->attr.s.ltyp;
}

inline void
set_Sel_linkage_type (ir_node *node, linkage_type lt) {
  assert (node->op == op_Sel);
  node->attr.s.ltyp = lt;
}

inline ir_node *
get_Call_mem (ir_node *node) {
  assert (node->op == op_Call);
  return get_irn_n(node, 0);
}

inline void
set_Call_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Call);
  set_irn_n(node, 0, mem);
}

inline ir_node *
get_Call_ptr (ir_node *node) {
  assert (node->op == op_Call);
  return get_irn_n(node, 1);
}

inline void
set_Call_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Call);
  set_irn_n(node, 1, ptr);
}

inline ir_node **
get_Call_param_arr (ir_node *node) {
  assert (node->op == op_Call);
  return (ir_node **)&get_irn_in(node)[CALL_PARAM_OFFSET + 1];
}

inline int
get_Call_n_params (ir_node *node)  {
  assert (node->op == op_Call);
  return (get_irn_arity(node) - CALL_PARAM_OFFSET);
}

inline int
get_Call_arity (ir_node *node) {
  return get_Call_n_params(node);
}

/* inline void
set_Call_arity (ir_node *node, ir_node *arity) {
  assert (node->op == op_Call);
}
*/

inline ir_node *
get_Call_param (ir_node *node, int pos) {
  assert (node->op == op_Call);
  return get_irn_n(node, pos + CALL_PARAM_OFFSET);
}

inline void
set_Call_param (ir_node *node, int pos, ir_node *param) {
  assert (node->op == op_Call);
  set_irn_n(node, pos + CALL_PARAM_OFFSET, param);
}

inline type *
get_Call_type (ir_node *node) {
  assert (node->op == op_Call);
  return node->attr.call;
}

inline void
set_Call_type (ir_node *node, type *type) {
  assert (node->op == op_Call);
  assert (is_method_type(type));
  node->attr.call = type;
}

/* For unary and binary arithmetic operations the access to the
   operands can be factored out.  Left is the first, right the
   second arithmetic value  as listed in tech report 0999-33.
   unops are: Minus, Abs, Not, Conv
   binops are: Add, Sub, Mul, Quot, DivMod, Div, Mod, And, Or, Eor, Shl,
   Shr, Shrs, Rotate, Cmp */

int
is_unop (ir_node *node) {
  return ( node->op == op_Minus ||
           node->op == op_Abs  ||
	   node->op == op_Not  ||
           node->op == op_Conv );
}

inline ir_node *
get_unop_op (ir_node *node) {
  assert ( node->op == op_Minus ||
           node->op == op_Abs  ||
	    node->op == op_Not  ||
           node->op == op_Conv );
  switch (get_irn_opcode (node)) {
    case iro_Minus: return get_Minus_op(node); break;
    case iro_Abs:   return get_Abs_op(node);   break;
    case iro_Not:   return get_Not_op(node);   break;
    case iro_Conv:  return get_Conv_op(node);  break;
    default: return NULL;
  }
}

inline void
set_unop_op (ir_node *node, ir_node *op) {
    assert (node->op == op_Minus ||
	    node->op == op_Abs   ||
	    node->op == op_Not   ||
	    node->op == op_Conv    );
    switch (get_irn_opcode (node)) {
    case iro_Minus:   set_Minus_op(node, op); break;
    case iro_Abs:     set_Abs_op(node, op);   break;
    case iro_Not:     set_Not_op(node, op);   break;
    case iro_Conv:    set_Conv_op(node, op);  break;
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

inline ir_node *
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

inline void
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

inline ir_node *
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

inline void
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

inline ir_node *
get_Add_left (ir_node *node) {
  assert (node->op == op_Add);
  return get_irn_n(node, 0);
}

inline void
set_Add_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Add);
  set_irn_n(node, 0, left);
}

inline ir_node *
get_Add_right (ir_node *node) {
  assert (node->op == op_Add);
  return get_irn_n(node, 1);
}

inline void
set_Add_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Add);
  set_irn_n(node, 1, right);
}

inline ir_node *
get_Sub_left (ir_node *node) {
  assert (node->op == op_Sub);
  return get_irn_n(node, 0);
}

inline void
set_Sub_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Sub);
  set_irn_n(node, 0, left);
}

inline ir_node *
get_Sub_right (ir_node *node) {
  assert (node->op == op_Sub);
  return get_irn_n(node, 1);
}

inline void
set_Sub_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Sub);
  set_irn_n(node, 1, right);
}


inline ir_node *
get_Minus_op (ir_node *node) {
  assert (node->op == op_Minus);
  return get_irn_n(node, 0);
}

inline void
set_Minus_op (ir_node *node, ir_node *op) {
  assert (node->op == op_Minus);
  set_irn_n(node, 0, op);
}


inline ir_node *
get_Mul_left (ir_node *node) {
  assert (node->op == op_Mul);
  return get_irn_n(node, 0);
}

inline void
set_Mul_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Mul);
  set_irn_n(node, 0, left);
}

inline ir_node *
get_Mul_right (ir_node *node) {
  assert (node->op == op_Mul);
  return get_irn_n(node, 1);
}

inline void
set_Mul_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Mul);
  set_irn_n(node, 1, right);
}

inline ir_node *
get_Quot_left (ir_node *node) {
  assert (node->op == op_Quot);
  return get_irn_n(node, 1);
}

inline void
set_Quot_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Quot);
  set_irn_n(node, 1, left);
}

inline ir_node *
get_Quot_right (ir_node *node) {
  assert (node->op == op_Quot);
  return get_irn_n(node, 2);
}

inline void
set_Quot_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Quot);
  set_irn_n(node, 2, right);
}

inline ir_node *
get_Quot_mem (ir_node *node) {
  assert (node->op == op_Quot);
  return get_irn_n(node, 0);
}

inline void
set_Quot_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Quot);
  set_irn_n(node, 0, mem);
}

inline ir_node *
get_DivMod_left (ir_node *node) {
  assert (node->op == op_DivMod);
  return get_irn_n(node, 1);
}

inline void
set_DivMod_left (ir_node *node, ir_node *left) {
  assert (node->op == op_DivMod);
  set_irn_n(node, 1, left);
}

inline ir_node *
get_DivMod_right (ir_node *node) {
  assert (node->op == op_DivMod);
  return get_irn_n(node, 2);
}

inline void
set_DivMod_right (ir_node *node, ir_node *right) {
  assert (node->op == op_DivMod);
  set_irn_n(node, 2, right);
}

inline ir_node *
get_DivMod_mem (ir_node *node) {
  assert (node->op == op_DivMod);
  return get_irn_n(node, 0);
}

inline void
set_DivMod_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_DivMod);
  set_irn_n(node, 0, mem);
}

inline ir_node *
get_Div_left (ir_node *node) {
  assert (node->op == op_Div);
  return get_irn_n(node, 1);
}

inline void
set_Div_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Div);
  set_irn_n(node, 1, left);
}

inline ir_node *
get_Div_right (ir_node *node) {
  assert (node->op == op_Div);
  return get_irn_n(node, 2);
}

inline void
set_Div_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Div);
  set_irn_n(node, 2, right);
}

inline ir_node *
get_Div_mem (ir_node *node) {
  assert (node->op == op_Div);
  return get_irn_n(node, 0);
}

inline void
set_Div_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Div);
  set_irn_n(node, 0, mem);
}

inline ir_node *
get_Mod_left (ir_node *node) {
  assert (node->op == op_Mod);
  return get_irn_n(node, 1);
}

inline void
set_Mod_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Mod);
  set_irn_n(node, 1, left);
}

inline ir_node *
get_Mod_right (ir_node *node) {
  assert (node->op == op_Mod);
  return get_irn_n(node, 2);
}

inline void
set_Mod_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Mod);
  set_irn_n(node, 2, right);
}

inline ir_node *
get_Mod_mem (ir_node *node) {
  assert (node->op == op_Mod);
  return get_irn_n(node, 0);
}

inline void
set_Mod_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Mod);
  set_irn_n(node, 0, mem);
}

inline ir_node *
get_Abs_op (ir_node *node) {
  assert (node->op == op_Abs);
  return get_irn_n(node, 0);
}

inline void
set_Abs_op (ir_node *node, ir_node *op) {
  assert (node->op == op_Abs);
  set_irn_n(node, 0, op);
}

inline ir_node *
get_And_left (ir_node *node) {
  assert (node->op == op_And);
  return get_irn_n(node, 0);
}

inline void
set_And_left (ir_node *node, ir_node *left) {
  assert (node->op == op_And);
  set_irn_n(node, 0, left);
}

inline ir_node *
get_And_right (ir_node *node) {
  assert (node->op == op_And);
  return get_irn_n(node, 1);
}

inline void
set_And_right (ir_node *node, ir_node *right) {
  assert (node->op == op_And);
  set_irn_n(node, 1, right);
}

inline ir_node *
get_Or_left (ir_node *node) {
  assert (node->op == op_Or);
  return get_irn_n(node, 0);
}

inline void
set_Or_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Or);
  set_irn_n(node, 0, left);
}

inline ir_node *
get_Or_right (ir_node *node) {
  assert (node->op == op_Or);
  return get_irn_n(node, 1);
}

inline void
set_Or_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Or);
  set_irn_n(node, 1, right);
}

inline ir_node *
get_Eor_left (ir_node *node) {
  assert (node->op == op_Eor);
  return get_irn_n(node, 0);
}

inline void
set_Eor_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Eor);
  set_irn_n(node, 0, left);
}

inline ir_node *
get_Eor_right (ir_node *node) {
  assert (node->op == op_Eor);
  return get_irn_n(node, 1);
}

inline void
set_Eor_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Eor);
  set_irn_n(node, 1, right);
}


inline ir_node *
get_Not_op (ir_node *node) {
  assert (node->op == op_Not);
  return get_irn_n(node, 0);
}

inline void
set_Not_op (ir_node *node, ir_node *op) {
  assert (node->op == op_Not);
  set_irn_n(node, 0, op);
}


inline ir_node *
get_Shl_left (ir_node *node) {
  assert (node->op == op_Shl);
  return get_irn_n(node, 0);
}

inline void
set_Shl_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Shl);
  set_irn_n(node, 0, left);
}

inline ir_node *
get_Shl_right (ir_node *node) {
  assert (node->op == op_Shl);
  return get_irn_n(node, 1);
}

inline void
set_Shl_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Shl);
  set_irn_n(node, 1, right);
}

inline ir_node *
get_Shr_left (ir_node *node) {
  assert (node->op == op_Shr);
  return get_irn_n(node, 0);
}

inline void
set_Shr_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Shr);
  set_irn_n(node, 0, left);
}

inline ir_node *
get_Shr_right (ir_node *node) {
  assert (node->op == op_Shr);
  return get_irn_n(node, 1);
}

inline void
set_Shr_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Shr);
  set_irn_n(node, 1, right);
}

inline ir_node *
get_Shrs_left (ir_node *node) {
  assert (node->op == op_Shrs);
  return get_irn_n(node, 0);
}

inline void
set_Shrs_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Shrs);
  set_irn_n(node, 0, left);
}

inline ir_node *
get_Shrs_right (ir_node *node) {
  assert (node->op == op_Shrs);
  return get_irn_n(node, 1);
}

inline void
set_Shrs_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Shrs);
  set_irn_n(node, 1, right);
}

inline ir_node *
get_Rot_left (ir_node *node) {
  assert (node->op == op_Rot);
  return get_irn_n(node, 0);
}

inline void
set_Rot_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Rot);
  set_irn_n(node, 0, left);
}

inline ir_node *
get_Rot_right (ir_node *node) {
  assert (node->op == op_Rot);
  return get_irn_n(node, 1);
}

inline void
set_Rot_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Rot);
  set_irn_n(node, 1, right);
}

inline ir_node *
get_Cmp_left (ir_node *node) {
  assert (node->op == op_Cmp);
  return get_irn_n(node, 0);
}

inline void
set_Cmp_left (ir_node *node, ir_node *left) {
  assert (node->op == op_Cmp);
  set_irn_n(node, 0, left);
}

inline ir_node *
get_Cmp_right (ir_node *node) {
  assert (node->op == op_Cmp);
  return get_irn_n(node, 1);
}

inline void
set_Cmp_right (ir_node *node, ir_node *right) {
  assert (node->op == op_Cmp);
  set_irn_n(node, 1, right);
}

inline ir_node *
get_Conv_op (ir_node *node) {
  assert (node->op == op_Conv);
  return get_irn_n(node, 0);
}

inline void
set_Conv_op (ir_node *node, ir_node *op) {
  assert (node->op == op_Conv);
  set_irn_n(node, 0, op);
}

inline ir_node **
get_Phi_preds_arr (ir_node *node) {
  assert (node->op == op_Phi);
  return (ir_node **)&(get_irn_in(node)[1]);
}

inline int
get_Phi_n_preds (ir_node *node) {
  assert (node->op == op_Phi);
  return (get_irn_arity(node));
}

/*
inline void set_Phi_n_preds (ir_node *node, int n_preds) {
  assert (node->op == op_Phi);
}
*/

inline ir_node *
get_Phi_pred (ir_node *node, int pos) {
  assert (node->op == op_Phi);
  return get_irn_n(node, pos);
}

inline void
set_Phi_pred (ir_node *node, int pos, ir_node *pred) {
  assert (node->op == op_Phi);
  set_irn_n(node, pos, pred);
}

inline ir_node *
get_Load_mem (ir_node *node) {
  assert (node->op == op_Load);
  return get_irn_n(node, 0);
}

inline void
set_Load_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Load);
  set_irn_n(node, 0, mem);
}

inline ir_node *
get_Load_ptr (ir_node *node) {
  assert (node->op == op_Load);
  return get_irn_n(node, 1);
}

inline void
set_Load_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Load);
  set_irn_n(node, 1, ptr);
}

inline
ir_node *
get_Store_mem (ir_node *node) {
  assert (node->op == op_Store);
  return get_irn_n(node, 0);
}

inline void
set_Store_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Store);
  set_irn_n(node, 0, mem);
}

inline ir_node *
get_Store_ptr (ir_node *node) {
  assert (node->op == op_Store);
  return get_irn_n(node, 1);
}

inline void
set_Store_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Store);
  set_irn_n(node, 1, ptr);
}

inline ir_node *
get_Store_value (ir_node *node) {
  assert (node->op == op_Store);
  return get_irn_n(node, 2);
}

inline void
set_Store_value (ir_node *node, ir_node *value) {
  assert (node->op == op_Store);
  set_irn_n(node, 2, value);
}

inline ir_node *
get_Alloc_mem (ir_node *node) {
  assert (node->op == op_Alloc);
  return get_irn_n(node, 0);
}

inline void
set_Alloc_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Alloc);
  set_irn_n(node, 0, mem);
}

inline ir_node *
get_Alloc_size (ir_node *node) {
  assert (node->op == op_Alloc);
  return get_irn_n(node, 1);
}

inline void
set_Alloc_size (ir_node *node, ir_node *size) {
  assert (node->op == op_Alloc);
  set_irn_n(node, 1, size);
}

inline type  *
get_Alloc_type (ir_node *node) {
  assert (node->op == op_Alloc);
  return node->attr.a.type;
}

inline void
set_Alloc_type (ir_node *node, type *type) {
  assert (node->op == op_Alloc);
  node->attr.a.type = type;
}

inline where_alloc
get_Alloc_where (ir_node *node) {
  assert (node->op == op_Alloc);
  return node->attr.a.where;
}

inline void
set_Alloc_where (ir_node *node, where_alloc where) {
  assert (node->op == op_Alloc);
  node->attr.a.where = where;
}


inline ir_node *
get_Free_mem (ir_node *node) {
  assert (node->op == op_Free);
  return get_irn_n(node, 0);
}

inline void
set_Free_mem (ir_node *node, ir_node *mem) {
  assert (node->op == op_Free);
  set_irn_n(node, 0, mem);
}

inline ir_node *
get_Free_ptr (ir_node *node) {
  assert (node->op == op_Free);
  return get_irn_n(node, 1);
}

inline void
set_Free_ptr (ir_node *node, ir_node *ptr) {
  assert (node->op == op_Free);
  set_irn_n(node, 1, ptr);
}

inline ir_node *
get_Free_size (ir_node *node) {
  assert (node->op == op_Free);
  return get_irn_n(node, 2);
}

inline void
set_Free_size (ir_node *node, ir_node *size) {
  assert (node->op == op_Free);
  set_irn_n(node, 2, size);
}

inline type  *
get_Free_type (ir_node *node) {
  assert (node->op == op_Free);
  return node->attr.f;
}

inline void
set_Free_type (ir_node *node, type *type) {
  assert (node->op == op_Free);
  node->attr.f = type;
}

inline ir_node **
get_Sync_preds_arr (ir_node *node) {
  assert (node->op == op_Sync);
  return (ir_node **)&(get_irn_in(node)[1]);
}

inline int
get_Sync_n_preds (ir_node *node) {
  assert (node->op == op_Sync);
  return (get_irn_arity(node));
}

/*
inline void
set_Sync_n_preds (ir_node *node, int n_preds) {
  assert (node->op == op_Sync);
}
*/

inline ir_node *
get_Sync_pred (ir_node *node, int pos) {
  assert (node->op == op_Sync);
  return get_irn_n(node, pos);
}

inline void
set_Sync_pred (ir_node *node, int pos, ir_node *pred) {
  assert (node->op == op_Sync);
  set_irn_n(node, pos, pred);
}

inline ir_node *
get_Proj_pred (ir_node *node) {
  assert (node->op == op_Proj);
  return get_irn_n(node, 0);
}

inline void
set_Proj_pred (ir_node *node, ir_node *pred) {
  assert (node->op == op_Proj);
  set_irn_n(node, 0, pred);
}

inline long
get_Proj_proj (ir_node *node) {
  assert (node->op == op_Proj);
  return node->attr.proj;
}

inline void
set_Proj_proj (ir_node *node, long proj) {
  assert (node->op == op_Proj);
  node->attr.proj = proj;
}

inline ir_node **
get_Tuple_preds_arr (ir_node *node) {
  assert (node->op == op_Tuple);
  return (ir_node **)&(get_irn_in(node)[1]);
}

inline int
get_Tuple_n_preds (ir_node *node) {
  assert (node->op == op_Tuple);
  return (get_irn_arity(node));
}

/*
inline void
set_Tuple_n_preds (ir_node *node, int n_preds) {
  assert (node->op == op_Tuple);
}
*/

inline ir_node *
get_Tuple_pred (ir_node *node, int pos) {
  assert (node->op == op_Tuple);
  return get_irn_n(node, pos);
}

inline void
set_Tuple_pred (ir_node *node, int pos, ir_node *pred) {
  assert (node->op == op_Tuple);
  set_irn_n(node, pos, pred);
}

inline ir_node *
get_Id_pred (ir_node *node) {
  assert (node->op == op_Id);
  return get_irn_n(node, 0);
}

inline void
set_Id_pred (ir_node *node, ir_node *pred) {
  assert (node->op == op_Id);
  set_irn_n(node, 0, pred);
}

/******************************************************************/
/*  Auxiliary routines                                            */
/******************************************************************/

inline ir_node *
skip_Proj (ir_node *node) {
  /* don't assert node !!! */
  if (node && (node->op == op_Proj)) {
    return get_Proj_pred(node);
  } else {
    return node;
  }
}

inline ir_node *
skip_nop (ir_node *node) {
  /* don't assert node !!! */

  if (node && (node->op == op_Id) && (node != get_Id_pred(node))) {
    /* Don't use get_Id_pred:  We get into an endless loop for
       self-referencing Ids. */
    assert (get_irn_arity (node) > 0);
    return node->in[0+1];
  } else {
    return node;
  }
}

inline int
is_Bad (ir_node *node) {
  assert(node);
  if ((node) && get_irn_opcode(node) == iro_Bad)
    return 1;
  return 0;
}

inline int
is_no_Block (ir_node *node) {
  assert(node);
  return (get_irn_opcode(node) != iro_Block);
}

/* Returns true if the operation manipulates control flow. */
int
is_cfop(ir_node *node) {
  return (   (get_irn_opcode(node) == iro_Start)
          || (get_irn_opcode(node) == iro_Jmp)
          || (get_irn_opcode(node) == iro_Cond)
          || (get_irn_opcode(node) == iro_Return)
          || (get_irn_opcode(node) == iro_Raise)
          || (get_irn_opcode(node) == iro_Bad));
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
          || (get_irn_opcode(node) == iro_Bad));
}
