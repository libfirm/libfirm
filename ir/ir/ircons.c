/*
 * Project:     libFIRM
 * File name:   ir/ir/ircons.c
 * Purpose:     Various irnode constructors.  Automatic construction
 *              of SSA representation.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier, Boris Boesler
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irgraph_t.h"
# include "irnode_t.h"
# include "irmode_t.h"
# include "ircons_t.h"
# include "firm_common_t.h"
# include "irvrfy.h"
# include "irop_t.h"
# include "iropt_t.h"
# include "irgmod.h"
# include "array.h"
/* memset belongs to string.h */
# include "string.h"
# include "irbackedge_t.h"
# include "irflag_t.h"

#if USE_EXPLICIT_PHI_IN_STACK
/* A stack needed for the automatic Phi node construction in constructor
   Phi_in. Redefinition in irgraph.c!! */
struct Phi_in_stack {
  ir_node **stack;
  int       pos;
};
typedef struct Phi_in_stack Phi_in_stack;
#endif

/* when we need verifying */
#ifdef NDEBUG
# define IRN_VRFY_IRG(res, irg)
#else
# define IRN_VRFY_IRG(res, irg)  irn_vrfy_irg(res, irg)
#endif

/*
 * language dependant initialization variable
 */
static default_initialize_local_variable_func_t *default_initialize_local_variable = NULL;

/* -------------------------------------------- */
/* privat interfaces, for professional use only */
/* -------------------------------------------- */

/* Constructs a Block with a fixed number of predecessors.
   Does not set current_block.  Can not be used with automatic
   Phi node construction. */
INLINE ir_node *
new_rd_Block (dbg_info* db, ir_graph *irg,  int arity, ir_node **in)
{
  ir_node *res;

  res = new_ir_node (db, irg, NULL, op_Block, mode_BB, arity, in);
  set_Block_matured(res, 1);
  set_Block_block_visited(res, 0);

  /* res->attr.block.exc = exc_normal; */
  /* res->attr.block.handler_entry = 0; */
  res->attr.block.dead        = 0;
  res->attr.block.irg         = irg;
  res->attr.block.backedge    = new_backedge_arr(irg->obst, arity);
  res->attr.block.in_cg       = NULL;
  res->attr.block.cg_backedge = NULL;

  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Start (dbg_info* db, ir_graph *irg, ir_node *block)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_Start, mode_T, 0, NULL);
  /* res->attr.start.irg = irg; */

  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_End (dbg_info* db, ir_graph *irg, ir_node *block)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_End, mode_X, -1, NULL);

  IRN_VRFY_IRG(res, irg);
  return res;
}

/* Creates a Phi node with all predecessors.  Calling this constructor
   is only allowed if the corresponding block is mature.  */
INLINE ir_node *
new_rd_Phi (dbg_info* db, ir_graph *irg, ir_node *block, int arity, ir_node **in, ir_mode *mode)
{
  ir_node *res;
  int i;
  bool has_unknown = false;

  /* Don't assert that block matured: the use of this constructor is strongly
     restricted ... */
  if ( get_Block_matured(block) )
    assert( get_irn_arity(block) == arity );

  res = new_ir_node(db, irg, block, op_Phi, mode, arity, in);

  res->attr.phi_backedge = new_backedge_arr(irg->obst, arity);

  for (i = arity-1; i >= 0; i--)
    if (get_irn_op(in[i]) == op_Unknown) {
      has_unknown = true;
      break;
    }

  if (!has_unknown) res = optimize_node (res);
  IRN_VRFY_IRG(res, irg);

  /* Memory Phis in endless loops must be kept alive.
     As we can't distinguish these easily we keep all of them alive. */
  if ((res->op == op_Phi) && (mode == mode_M))
    add_End_keepalive(irg->end, res);
  return res;
}

INLINE ir_node *
new_rd_Const_type (dbg_info* db, ir_graph *irg, ir_node *block, ir_mode *mode, tarval *con, type *tp)
{
  ir_node *res;

  res = new_ir_node (db, irg, irg->start_block, op_Const, mode, 0, NULL);
  res->attr.con.tv = con;
  set_Const_type(res, tp);  /* Call method because of complex assertion. */
  res = optimize_node (res);
  assert(get_Const_type(res) == tp);
  IRN_VRFY_IRG(res, irg);

  return res;
}

INLINE ir_node *
new_rd_Const (dbg_info* db, ir_graph *irg, ir_node *block, ir_mode *mode, tarval *con)
{
  type *tp = unknown_type;
  /* removing this somehow causes errors in jack. */
  return new_rd_Const_type (db, irg, block, mode, con, tp);
}

INLINE ir_node *
new_rd_Id (dbg_info* db, ir_graph *irg, ir_node *block, ir_node *val, ir_mode *mode)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_Id, mode, 1, &val);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Proj (dbg_info* db, ir_graph *irg, ir_node *block, ir_node *arg, ir_mode *mode,
        long proj)
{
  ir_node *res;

  res = new_ir_node (db, irg, block, op_Proj, mode, 1, &arg);
  res->attr.proj = proj;

  assert(res);
  assert(get_Proj_pred(res));
  assert(get_nodes_block(get_Proj_pred(res)));

  res = optimize_node(res);

  IRN_VRFY_IRG(res, irg);
  return res;

}

INLINE ir_node *
new_rd_defaultProj (dbg_info* db, ir_graph *irg, ir_node *block, ir_node *arg,
           long max_proj)
{
  ir_node *res;
  assert(arg->op == op_Cond);
  arg->attr.c.kind = fragmentary;
  arg->attr.c.default_proj = max_proj;
  res = new_rd_Proj (db, irg, block, arg, mode_X, max_proj);
  return res;
}

INLINE ir_node *
new_rd_Conv (dbg_info* db, ir_graph *irg, ir_node *block, ir_node *op, ir_mode *mode)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_Conv, mode, 1, &op);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Cast (dbg_info* db, ir_graph *irg, ir_node *block, ir_node *op, type *to_tp)
{
  ir_node *res;

  assert(is_atomic_type(to_tp));

  res = new_ir_node(db, irg, block, op_Cast, get_irn_mode(op), 1, &op);
  res->attr.cast.totype = to_tp;
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Tuple (dbg_info* db, ir_graph *irg, ir_node *block, int arity, ir_node **in)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_Tuple, mode_T, arity, in);
  res = optimize_node (res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Add (dbg_info* db, ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = op1;
  in[1] = op2;
  res = new_ir_node(db, irg, block, op_Add, mode, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Sub (dbg_info* db, ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = op1;
  in[1] = op2;
  res = new_ir_node (db, irg, block, op_Sub, mode, 2, in);
  res = optimize_node (res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Minus (dbg_info* db, ir_graph *irg, ir_node *block,
         ir_node *op, ir_mode *mode)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_Minus, mode, 1, &op);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Mul (dbg_info* db, ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = op1;
  in[1] = op2;
  res = new_ir_node(db, irg, block, op_Mul, mode, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Quot (dbg_info* db, ir_graph *irg, ir_node *block,
            ir_node *memop, ir_node *op1, ir_node *op2)
{
  ir_node *in[3];
  ir_node *res;

  in[0] = memop;
  in[1] = op1;
  in[2] = op2;
  res = new_ir_node(db, irg, block, op_Quot, mode_T, 3, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_DivMod (dbg_info* db, ir_graph *irg, ir_node *block,
          ir_node *memop, ir_node *op1, ir_node *op2)
{
  ir_node *in[3];
  ir_node *res;

  in[0] = memop;
  in[1] = op1;
  in[2] = op2;
  res = new_ir_node(db, irg, block, op_DivMod, mode_T, 3, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Div (dbg_info* db, ir_graph *irg, ir_node *block,
           ir_node *memop, ir_node *op1, ir_node *op2)
{
  ir_node *in[3];
  ir_node *res;

  in[0] = memop;
  in[1] = op1;
  in[2] = op2;
  res = new_ir_node(db, irg, block, op_Div, mode_T, 3, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Mod (dbg_info* db, ir_graph *irg, ir_node *block,
           ir_node *memop, ir_node *op1, ir_node *op2)
{
  ir_node *in[3];
  ir_node *res;

  in[0] = memop;
  in[1] = op1;
  in[2] = op2;
  res = new_ir_node(db, irg, block, op_Mod, mode_T, 3, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_And (dbg_info* db, ir_graph *irg, ir_node *block,
           ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = op1;
  in[1] = op2;
  res = new_ir_node(db, irg, block, op_And, mode, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Or (dbg_info* db, ir_graph *irg, ir_node *block,
          ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = op1;
  in[1] = op2;
  res = new_ir_node(db, irg, block, op_Or, mode, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Eor (dbg_info* db, ir_graph *irg, ir_node *block,
          ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = op1;
  in[1] = op2;
  res = new_ir_node (db, irg, block, op_Eor, mode, 2, in);
  res = optimize_node (res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Not    (dbg_info* db, ir_graph *irg, ir_node *block,
          ir_node *op, ir_mode *mode)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_Not, mode, 1, &op);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Shl (dbg_info* db, ir_graph *irg, ir_node *block,
          ir_node *op, ir_node *k, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = op;
  in[1] = k;
  res = new_ir_node(db, irg, block, op_Shl, mode, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Shr (dbg_info* db, ir_graph *irg, ir_node *block,
       ir_node *op, ir_node *k, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = op;
  in[1] = k;
  res = new_ir_node(db, irg, block, op_Shr, mode, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Shrs (dbg_info* db, ir_graph *irg, ir_node *block,
       ir_node *op, ir_node *k, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = op;
  in[1] = k;
  res = new_ir_node(db, irg, block, op_Shrs, mode, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Rot (dbg_info* db, ir_graph *irg, ir_node *block,
       ir_node *op, ir_node *k, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = op;
  in[1] = k;
  res = new_ir_node(db, irg, block, op_Rot, mode, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Abs (dbg_info* db, ir_graph *irg, ir_node *block,
       ir_node *op, ir_mode *mode)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_Abs, mode, 1, &op);
  res = optimize_node (res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Cmp (dbg_info* db, ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2)
{
  ir_node *in[2];
  ir_node *res;
  in[0] = op1;
  in[1] = op2;

  res = new_ir_node(db, irg, block, op_Cmp, mode_T, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Jmp (dbg_info* db, ir_graph *irg, ir_node *block)
{
  ir_node *res;

  res = new_ir_node (db, irg, block, op_Jmp, mode_X, 0, NULL);
  res = optimize_node (res);
  IRN_VRFY_IRG (res, irg);
  return res;
}

INLINE ir_node *
new_rd_Cond (dbg_info* db, ir_graph *irg, ir_node *block, ir_node *c)
{
  ir_node *res;

  res = new_ir_node (db, irg, block, op_Cond, mode_T, 1, &c);
  res->attr.c.kind         = dense;
  res->attr.c.default_proj = 0;
  res = optimize_node (res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

ir_node *
new_rd_Call (dbg_info* db, ir_graph *irg, ir_node *block, ir_node *store,
        ir_node *callee, int arity, ir_node **in, type *tp)
{
  ir_node **r_in;
  ir_node *res;
  int r_arity;

  r_arity = arity+2;
  NEW_ARR_A(ir_node *, r_in, r_arity);
  r_in[0] = store;
  r_in[1] = callee;
  memcpy(&r_in[2], in, sizeof(ir_node *) * arity);

  res = new_ir_node(db, irg, block, op_Call, mode_T, r_arity, r_in);

  assert((get_unknown_type() == tp) || is_method_type(tp));
  set_Call_type(res, tp);
  res->attr.call.exc.pin_state = op_pin_state_pinned;
  res->attr.call.callee_arr    = NULL;
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

ir_node *
new_rd_Return (dbg_info* db, ir_graph *irg, ir_node *block,
              ir_node *store, int arity, ir_node **in)
{
  ir_node **r_in;
  ir_node *res;
  int r_arity;

  r_arity = arity+1;
  NEW_ARR_A (ir_node *, r_in, r_arity);
  r_in[0] = store;
  memcpy(&r_in[1], in, sizeof(ir_node *) * arity);
  res = new_ir_node(db, irg, block, op_Return, mode_X, r_arity, r_in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Raise (dbg_info* db, ir_graph *irg, ir_node *block, ir_node *store, ir_node *obj)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = store;
  in[1] = obj;
  res = new_ir_node(db, irg, block, op_Raise, mode_T, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Load (dbg_info* db, ir_graph *irg, ir_node *block,
        ir_node *store, ir_node *adr, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = store;
  in[1] = adr;
  res = new_ir_node(db, irg, block, op_Load, mode_T, 2, in);
  res->attr.load.exc.pin_state = op_pin_state_pinned;
  res->attr.load.load_mode     = mode;
  res->attr.load.volatility    = volatility_non_volatile;
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Store (dbg_info* db, ir_graph *irg, ir_node *block,
         ir_node *store, ir_node *adr, ir_node *val)
{
  ir_node *in[3];
  ir_node *res;

  in[0] = store;
  in[1] = adr;
  in[2] = val;
  res = new_ir_node(db, irg, block, op_Store, mode_T, 3, in);
  res->attr.store.exc.pin_state = op_pin_state_pinned;
  res->attr.store.volatility    = volatility_non_volatile;
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Alloc (dbg_info* db, ir_graph *irg, ir_node *block, ir_node *store,
        ir_node *size, type *alloc_type, where_alloc where)
{
  ir_node *in[2];
  ir_node *res;

  in[0] = store;
  in[1] = size;
  res = new_ir_node(db, irg, block, op_Alloc, mode_T, 2, in);
  res->attr.a.exc.pin_state = op_pin_state_pinned;
  res->attr.a.where         = where;
  res->attr.a.type          = alloc_type;
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Free (dbg_info* db, ir_graph *irg, ir_node *block, ir_node *store,
        ir_node *ptr, ir_node *size, type *free_type)
{
  ir_node *in[3];
  ir_node *res;

  in[0] = store;
  in[1] = ptr;
  in[2] = size;
  res = new_ir_node (db, irg, block, op_Free, mode_T, 3, in);
  res->attr.f = free_type;
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

ir_node *
new_rd_Sel (dbg_info* db, ir_graph *irg, ir_node *block, ir_node *store, ir_node *objptr,
           int arity, ir_node **in, entity *ent)
{
  ir_node **r_in;
  ir_node *res;
  int r_arity;

  assert(ent != NULL && is_entity(ent) && "entity expected in Sel construction");

  r_arity = arity + 2;
  NEW_ARR_A(ir_node *, r_in, r_arity);  /* uses alloca */
  r_in[0] = store;
  r_in[1] = objptr;
  memcpy(&r_in[2], in, sizeof(ir_node *) * arity);
  res = new_ir_node(db, irg, block, op_Sel, mode_P_mach, r_arity, r_in);
  res->attr.s.ent = ent;
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

ir_node *
new_rd_InstOf (dbg_info *db, ir_graph *irg, ir_node *block, ir_node *store,
           ir_node *objptr, type *ent)
{
  ir_node **r_in;
  ir_node *res;
  int r_arity;

  r_arity = 2;
  NEW_ARR_A(ir_node *, r_in, r_arity);
  r_in[0] = store;
  r_in[1] = objptr;

  res = new_ir_node(db, irg, block, op_Sel, mode_T, r_arity, r_in);
  res->attr.io.ent = ent;

  /* res = optimize(res); */
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_SymConst_type (dbg_info* db, ir_graph *irg, ir_node *block, symconst_symbol value,
		      symconst_kind symkind, type *tp) {
  ir_node *res;
  ir_mode *mode;

  if ((symkind == symconst_addr_name) || (symkind == symconst_addr_ent))
    mode = mode_P_mach;
  else
    mode = mode_Iu;

  res = new_ir_node(db, irg, block, op_SymConst, mode, 0, NULL);

  res->attr.i.num = symkind;
  res->attr.i.sym = value;
  res->attr.i.tp  = tp;

  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_SymConst (dbg_info* db, ir_graph *irg, ir_node *block, symconst_symbol value,
		 symconst_kind symkind)
{
  ir_node *res = new_rd_SymConst_type(db, irg, block, value, symkind, unknown_type);
  return res;
}

ir_node *new_rd_SymConst_addr_ent (dbg_info *db, ir_graph *irg, entity *symbol, type *tp) {
  symconst_symbol sym = {(type *)symbol};
  return new_rd_SymConst_type(db, irg, irg->start_block, sym, symconst_addr_ent, tp);
}

ir_node *new_rd_SymConst_addr_name (dbg_info *db, ir_graph *irg, ident *symbol, type *tp) {
  symconst_symbol sym = {(type *)symbol};
  return new_rd_SymConst_type(db, irg, irg->start_block, sym, symconst_addr_name, tp);
}

ir_node *new_rd_SymConst_type_tag (dbg_info *db, ir_graph *irg, type *symbol, type *tp) {
  symconst_symbol sym = {symbol};
  return new_rd_SymConst_type(db, irg, irg->start_block, sym, symconst_type_tag, tp);
}

ir_node *new_rd_SymConst_size (dbg_info *db, ir_graph *irg, type *symbol, type *tp) {
  symconst_symbol sym = {symbol};
  return new_rd_SymConst_type(db, irg, irg->start_block, sym, symconst_size, tp);
}

INLINE ir_node *
new_rd_Sync (dbg_info* db, ir_graph *irg, ir_node *block, int arity, ir_node **in)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_Sync, mode_M, arity, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Bad (ir_graph *irg)
{
  return irg->bad;
}

INLINE ir_node *
new_rd_Confirm (dbg_info *db, ir_graph *irg, ir_node *block, ir_node *val, ir_node *bound, pn_Cmp cmp)
{
  ir_node *in[2], *res;

  in[0] = val;
  in[1] = bound;
  res = new_ir_node (db, irg, block, op_Confirm, get_irn_mode(val), 2, in);
  res->attr.confirm_cmp = cmp;
  res = optimize_node (res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Unknown (ir_graph *irg, ir_mode *m)
{
  return new_ir_node(NULL, irg, irg->start_block, op_Unknown, m, 0, NULL);
}

INLINE ir_node *
new_rd_CallBegin (dbg_info *db, ir_graph *irg, ir_node *block, ir_node *call)
{
  ir_node *in[1];
  ir_node *res;

  in[0] = get_Call_ptr(call);
  res = new_ir_node(db, irg, block, op_CallBegin, mode_T, 1, in);
  /* res->attr.callbegin.irg = irg; */
  res->attr.callbegin.call = call;
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_EndReg (dbg_info *db, ir_graph *irg, ir_node *block)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_EndReg, mode_T, -1, NULL);
  irg->end_reg = res;
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_EndExcept (dbg_info *db, ir_graph *irg, ir_node *block)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_EndExcept, mode_T, -1, NULL);
  irg->end_except = res;
  IRN_VRFY_IRG (res, irg);
  return res;
}

INLINE ir_node *
new_rd_Break (dbg_info *db, ir_graph *irg, ir_node *block)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_Break, mode_X, 0, NULL);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

INLINE ir_node *
new_rd_Filter (dbg_info *db, ir_graph *irg, ir_node *block, ir_node *arg, ir_mode *mode,
           long proj)
{
  ir_node *res;

  res = new_ir_node(db, irg, block, op_Filter, mode, 1, &arg);
  res->attr.filter.proj = proj;
  res->attr.filter.in_cg = NULL;
  res->attr.filter.backedge = NULL;

  assert(res);
  assert(get_Proj_pred(res));
  assert(get_nodes_block(get_Proj_pred(res)));

  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;

}

INLINE ir_node *
new_rd_NoMem (ir_graph *irg)
{
  return irg->no_mem;
}


INLINE ir_node *new_r_Block  (ir_graph *irg,  int arity, ir_node **in) {
  return new_rd_Block(NULL, irg, arity, in);
}
INLINE ir_node *new_r_Start  (ir_graph *irg, ir_node *block) {
  return new_rd_Start(NULL, irg, block);
}
INLINE ir_node *new_r_End    (ir_graph *irg, ir_node *block) {
  return new_rd_End(NULL, irg, block);
}
INLINE ir_node *new_r_Jmp    (ir_graph *irg, ir_node *block) {
  return new_rd_Jmp(NULL, irg, block);
}
INLINE ir_node *new_r_Cond   (ir_graph *irg, ir_node *block, ir_node *c) {
  return new_rd_Cond(NULL, irg, block, c);
}
INLINE ir_node *new_r_Return (ir_graph *irg, ir_node *block,
               ir_node *store, int arity, ir_node **in) {
  return new_rd_Return(NULL, irg, block, store, arity, in);
}
INLINE ir_node *new_r_Raise  (ir_graph *irg, ir_node *block,
               ir_node *store, ir_node *obj) {
  return new_rd_Raise(NULL, irg, block, store, obj);
}
INLINE ir_node *new_r_Const  (ir_graph *irg, ir_node *block,
               ir_mode *mode, tarval *con) {
  return new_rd_Const(NULL, irg, block, mode, con);
}
INLINE ir_node *new_r_SymConst (ir_graph *irg, ir_node *block,
                       symconst_symbol value, symconst_kind symkind) {
  return new_rd_SymConst(NULL, irg, block, value, symkind);
}
INLINE ir_node *new_r_Sel    (ir_graph *irg, ir_node *block, ir_node *store,
                  ir_node *objptr, int n_index, ir_node **index,
                  entity *ent) {
  return new_rd_Sel(NULL, irg, block, store, objptr, n_index, index, ent);
}
INLINE ir_node *new_r_InstOf (ir_graph *irg, ir_node *block, ir_node *store, ir_node *objptr,
                  type *ent) {
  return (new_rd_InstOf (NULL, irg, block, store, objptr, ent));
}
INLINE ir_node *new_r_Call   (ir_graph *irg, ir_node *block, ir_node *store,
                  ir_node *callee, int arity, ir_node **in,
                  type *tp) {
  return new_rd_Call(NULL, irg, block, store, callee, arity, in, tp);
}
INLINE ir_node *new_r_Add    (ir_graph *irg, ir_node *block,
                  ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_Add(NULL, irg, block, op1, op2, mode);
}
INLINE ir_node *new_r_Sub    (ir_graph *irg, ir_node *block,
                  ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_Sub(NULL, irg, block, op1, op2, mode);
}
INLINE ir_node *new_r_Minus  (ir_graph *irg, ir_node *block,
                  ir_node *op,  ir_mode *mode) {
  return new_rd_Minus(NULL, irg, block,  op, mode);
}
INLINE ir_node *new_r_Mul    (ir_graph *irg, ir_node *block,
                  ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_Mul(NULL, irg, block, op1, op2, mode);
}
INLINE ir_node *new_r_Quot   (ir_graph *irg, ir_node *block,
                  ir_node *memop, ir_node *op1, ir_node *op2) {
  return new_rd_Quot(NULL, irg, block, memop, op1, op2);
}
INLINE ir_node *new_r_DivMod (ir_graph *irg, ir_node *block,
                  ir_node *memop, ir_node *op1, ir_node *op2) {
  return new_rd_DivMod(NULL, irg, block, memop, op1, op2);
}
INLINE ir_node *new_r_Div    (ir_graph *irg, ir_node *block,
                  ir_node *memop, ir_node *op1, ir_node *op2) {
  return new_rd_Div(NULL, irg, block, memop, op1, op2);
}
INLINE ir_node *new_r_Mod    (ir_graph *irg, ir_node *block,
                  ir_node *memop, ir_node *op1, ir_node *op2) {
  return new_rd_Mod(NULL, irg, block, memop, op1, op2);
}
INLINE ir_node *new_r_Abs    (ir_graph *irg, ir_node *block,
                  ir_node *op, ir_mode *mode) {
  return new_rd_Abs(NULL, irg, block, op, mode);
}
INLINE ir_node *new_r_And    (ir_graph *irg, ir_node *block,
                  ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_And(NULL, irg, block,  op1, op2, mode);
}
INLINE ir_node *new_r_Or     (ir_graph *irg, ir_node *block,
                  ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_Or(NULL, irg, block,  op1, op2, mode);
}
INLINE ir_node *new_r_Eor    (ir_graph *irg, ir_node *block,
                  ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_Eor(NULL, irg, block,  op1, op2, mode);
}
INLINE ir_node *new_r_Not    (ir_graph *irg, ir_node *block,
               ir_node *op, ir_mode *mode) {
  return new_rd_Not(NULL, irg, block, op, mode);
}
INLINE ir_node *new_r_Cmp    (ir_graph *irg, ir_node *block,
               ir_node *op1, ir_node *op2) {
  return new_rd_Cmp(NULL, irg, block, op1, op2);
}
INLINE ir_node *new_r_Shl    (ir_graph *irg, ir_node *block,
               ir_node *op, ir_node *k, ir_mode *mode) {
  return new_rd_Shl(NULL, irg, block, op, k, mode);
}
INLINE ir_node *new_r_Shr    (ir_graph *irg, ir_node *block,
               ir_node *op, ir_node *k, ir_mode *mode) {
  return new_rd_Shr(NULL, irg, block, op, k, mode);
}
INLINE ir_node *new_r_Shrs   (ir_graph *irg, ir_node *block,
               ir_node *op, ir_node *k, ir_mode *mode) {
  return new_rd_Shrs(NULL, irg, block, op, k, mode);
}
INLINE ir_node *new_r_Rot    (ir_graph *irg, ir_node *block,
               ir_node *op, ir_node *k, ir_mode *mode) {
  return new_rd_Rot(NULL, irg, block, op, k, mode);
}
INLINE ir_node *new_r_Conv   (ir_graph *irg, ir_node *block,
               ir_node *op, ir_mode *mode) {
  return new_rd_Conv(NULL, irg, block, op, mode);
}
INLINE ir_node *new_r_Cast   (ir_graph *irg, ir_node *block, ir_node *op, type *to_tp) {
  return new_rd_Cast(NULL, irg, block, op, to_tp);
}
INLINE ir_node *new_r_Phi    (ir_graph *irg, ir_node *block, int arity,
               ir_node **in, ir_mode *mode) {
  return new_rd_Phi(NULL, irg, block, arity, in, mode);
}
INLINE ir_node *new_r_Load   (ir_graph *irg, ir_node *block,
               ir_node *store, ir_node *adr, ir_mode *mode) {
  return new_rd_Load(NULL, irg, block, store, adr, mode);
}
INLINE ir_node *new_r_Store  (ir_graph *irg, ir_node *block,
               ir_node *store, ir_node *adr, ir_node *val) {
  return new_rd_Store(NULL, irg, block, store, adr, val);
}
INLINE ir_node *new_r_Alloc  (ir_graph *irg, ir_node *block, ir_node *store,
               ir_node *size, type *alloc_type, where_alloc where) {
  return new_rd_Alloc(NULL, irg, block, store, size, alloc_type, where);
}
INLINE ir_node *new_r_Free   (ir_graph *irg, ir_node *block, ir_node *store,
               ir_node *ptr, ir_node *size, type *free_type) {
  return new_rd_Free(NULL, irg, block, store, ptr, size, free_type);
}
INLINE ir_node *new_r_Sync   (ir_graph *irg, ir_node *block, int arity, ir_node **in) {
  return new_rd_Sync(NULL, irg, block, arity, in);
}
INLINE ir_node *new_r_Proj   (ir_graph *irg, ir_node *block, ir_node *arg,
               ir_mode *mode, long proj) {
  return new_rd_Proj(NULL, irg, block, arg, mode, proj);
}
INLINE ir_node *new_r_defaultProj (ir_graph *irg, ir_node *block, ir_node *arg,
                long max_proj) {
  return new_rd_defaultProj(NULL, irg, block, arg, max_proj);
}
INLINE ir_node *new_r_Tuple  (ir_graph *irg, ir_node *block,
               int arity, ir_node **in) {
  return new_rd_Tuple(NULL, irg, block, arity, in );
}
INLINE ir_node *new_r_Id     (ir_graph *irg, ir_node *block,
               ir_node *val, ir_mode *mode) {
  return new_rd_Id(NULL, irg, block, val, mode);
}
INLINE ir_node *new_r_Bad    (ir_graph *irg) {
  return new_rd_Bad(irg);
}
INLINE ir_node *new_r_Confirm (ir_graph *irg, ir_node *block, ir_node *val, ir_node *bound, pn_Cmp cmp) {
  return new_rd_Confirm (NULL, irg, block, val, bound, cmp);
}
INLINE ir_node *new_r_Unknown (ir_graph *irg, ir_mode *m) {
  return new_rd_Unknown(irg, m);
}
INLINE ir_node *new_r_CallBegin (ir_graph *irg, ir_node *block, ir_node *callee) {
  return new_rd_CallBegin(NULL, irg, block, callee);
}
INLINE ir_node *new_r_EndReg (ir_graph *irg, ir_node *block) {
  return new_rd_EndReg(NULL, irg, block);
}
INLINE ir_node *new_r_EndExcept (ir_graph *irg, ir_node *block) {
  return new_rd_EndExcept(NULL, irg, block);
}
INLINE ir_node *new_r_Break  (ir_graph *irg, ir_node *block) {
  return new_rd_Break(NULL, irg, block);
}
INLINE ir_node *new_r_Filter (ir_graph *irg, ir_node *block, ir_node *arg,
               ir_mode *mode, long proj) {
  return new_rd_Filter(NULL, irg, block, arg, mode, proj);
}
INLINE ir_node *new_r_NoMem  (ir_graph *irg) {
  return new_rd_NoMem(irg);
}


/** ********************/
/** public interfaces  */
/** construction tools */

/**
 *
 *   - create a new Start node in the current block
 *
 *   @return s - pointer to the created Start node
 *
 *
 */
ir_node *
new_d_Start (dbg_info* db)
{
  ir_node *res;

  res = new_ir_node (db, current_ir_graph, current_ir_graph->current_block,
             op_Start, mode_T, 0, NULL);
  /* res->attr.start.irg = current_ir_graph; */

  res = optimize_node(res);
  IRN_VRFY_IRG(res, current_ir_graph);
  return res;
}

ir_node *
new_d_End (dbg_info* db)
{
  ir_node *res;
  res = new_ir_node(db, current_ir_graph,  current_ir_graph->current_block,
             op_End, mode_X, -1, NULL);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, current_ir_graph);

  return res;
}

/* Constructs a Block with a fixed number of predecessors.
   Does set current_block.  Can be used with automatic Phi
   node construction. */
ir_node *
new_d_Block (dbg_info* db, int arity, ir_node **in)
{
  ir_node *res;
  int i;
  bool has_unknown = false;

  res = new_rd_Block(db, current_ir_graph, arity, in);

  /* Create and initialize array for Phi-node construction. */
  if (get_irg_phase_state(current_ir_graph) == phase_building) {
    res->attr.block.graph_arr = NEW_ARR_D(ir_node *, current_ir_graph->obst,
					  current_ir_graph->n_loc);
    memset(res->attr.block.graph_arr, 0, sizeof(ir_node *)*current_ir_graph->n_loc);
  }

  for (i = arity-1; i >= 0; i--)
    if (get_irn_op(in[i]) == op_Unknown) {
      has_unknown = true;
      break;
    }

  if (!has_unknown) res = optimize_node(res);
  current_ir_graph->current_block = res;

  IRN_VRFY_IRG(res, current_ir_graph);

  return res;
}

/* ***********************************************************************/
/* Methods necessary for automatic Phi node creation                     */
/*
  ir_node *phi_merge            (ir_node *block, int pos, ir_mode *mode, ir_node **nin, int ins)
  ir_node *get_r_value_internal (ir_node *block, int pos, ir_mode *mode);
  ir_node *new_rd_Phi0           (ir_graph *irg, ir_node *block, ir_mode *mode)
  ir_node *new_rd_Phi_in         (ir_graph *irg, ir_node *block, ir_mode *mode,  ir_node **in, int ins)

  Call Graph:   ( A ---> B == A "calls" B)

       get_value         mature_immBlock
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
        new_rd_Phi0          new_rd_Phi_in

* *************************************************************************** */

/** Creates a Phi node with 0 predecessors */
static INLINE ir_node *
new_rd_Phi0 (ir_graph *irg, ir_node *block, ir_mode *mode)
{
  ir_node *res;

  res = new_ir_node(NULL, irg, block, op_Phi, mode, 0, NULL);
  IRN_VRFY_IRG(res, irg);
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
   new_rd_Phi_in.  The original implementation used the obstack
   to model this stack, now it is explicit.  This reduces side effects.
*/
#if USE_EXPLICIT_PHI_IN_STACK
INLINE Phi_in_stack *
new_Phi_in_stack(void) {
  Phi_in_stack *res;

  res = (Phi_in_stack *) malloc ( sizeof (Phi_in_stack));

  res->stack = NEW_ARR_F (ir_node *, 0);
  res->pos = 0;

  return res;
}

INLINE void
free_Phi_in_stack(Phi_in_stack *s) {
  DEL_ARR_F(s->stack);
  free(s);
}
static INLINE void
free_to_Phi_in_stack(ir_node *phi) {
  if (ARR_LEN(current_ir_graph->Phi_in_stack->stack) ==
      current_ir_graph->Phi_in_stack->pos)
    ARR_APP1 (ir_node *, current_ir_graph->Phi_in_stack->stack, phi);
  else
    current_ir_graph->Phi_in_stack->stack[current_ir_graph->Phi_in_stack->pos] = phi;

  (current_ir_graph->Phi_in_stack->pos)++;
}

static INLINE ir_node *
alloc_or_pop_from_Phi_in_stack(ir_graph *irg, ir_node *block, ir_mode *mode,
         int arity, ir_node **in) {
  ir_node *res;
  ir_node **stack = current_ir_graph->Phi_in_stack->stack;
  int pos = current_ir_graph->Phi_in_stack->pos;


  if (pos == 0) {
    /* We need to allocate a new node */
    res = new_ir_node (db, irg, block, op_Phi, mode, arity, in);
    res->attr.phi_backedge = new_backedge_arr(irg->obst, arity);
  } else {
    /* reuse the old node and initialize it again. */
    res = stack[pos-1];

    assert (res->kind == k_ir_node);
    assert (res->op == op_Phi);
    res->mode = mode;
    res->visited = 0;
    res->link = NULL;
    assert (arity >= 0);
    /* ???!!! How to free the old in array??  Not at all: on obstack ?!! */
    res->in = NEW_ARR_D (ir_node *, irg->obst, (arity+1));
    res->in[0] = block;
    memcpy (&res->in[1], in, sizeof (ir_node *) * arity);

    (current_ir_graph->Phi_in_stack->pos)--;
  }
  return res;
}
#endif /* USE_EXPLICIT_PHI_IN_STACK */

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
static INLINE ir_node *
new_rd_Phi_in (ir_graph *irg, ir_node *block, ir_mode *mode, ir_node **in, int ins)
{
  int i;
  ir_node *res, *known;

  /* Allocate a new node on the obstack.  This can return a node to
     which some of the pointers in the in-array already point.
     Attention: the constructor copies the in array, i.e., the later
     changes to the array in this routine do not affect the
     constructed node!  If the in array contains NULLs, there will be
     missing predecessors in the returned node.  Is this a possible
     internal state of the Phi node generation? */
#if USE_EXPLICIT_PHI_IN_STACK
  res = known = alloc_or_pop_from_Phi_in_stack(irg, block, mode, ins, in);
#else
  res = known = new_ir_node (NULL, irg, block, op_Phi, mode, ins, in);
  res->attr.phi_backedge = new_backedge_arr(irg->obst, ins);
#endif

  /* The in-array can contain NULLs.  These were returned by
     get_r_value_internal if it reached the same block/definition on a
     second path.  The NULLs are replaced by the node itself to
     simplify the test in the next loop. */
  for (i = 0;  i < ins;  ++i) {
    if (in[i] == NULL)
      in[i] = res;
  }

  /* This loop checks whether the Phi has more than one predecessor.
     If so, it is a real Phi node and we break the loop.  Else the Phi
     node merges the same definition on several paths and therefore is
     not needed. */
  for (i = 0;  i < ins;  ++i)
  {
    if (in[i] == res || in[i] == known) continue;

    if (known == res)
      known = in[i];
    else
      break;
  }

  /* i==ins: there is at most one predecessor, we don't need a phi node. */
  if (i==ins) {
#if USE_EXPLICIT_PHI_IN_STACK
    free_to_Phi_in_stack(res);
#else
    obstack_free (current_ir_graph->obst, res);
#endif
    res = known;
  } else {
    res = optimize_node (res);
    IRN_VRFY_IRG(res, irg);
  }

  /* return the pointer to the Phi node.  This node might be deallocated! */
  return res;
}

static ir_node *
get_r_value_internal (ir_node *block, int pos, ir_mode *mode);

/**
    allocates and returns this node.  The routine called to allocate the
    node might optimize it away and return a real value, or even a pointer
    to a deallocated Phi node on top of the obstack!
    This function is called with an in-array of proper size. **/
static ir_node *
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
  res = new_rd_Phi_in (current_ir_graph, block, mode, nin, ins);

  /* Now we now the value for "pos" and can enter it in the array with
     all known local variables.  Attention: this might be a pointer to
     a node, that later will be allocated!!! See new_rd_Phi_in.
     If this is called in mature, after some set_value in the same block,
     the proper value must not be overwritten:
     The call order
       get_value    (makes Phi0, put's it into graph_arr)
       set_value    (overwrites Phi0 in graph_arr)
       mature_immBlock (upgrades Phi0, puts it again into graph_arr, overwriting
                     the proper value.)
     fails. */
  if (!block->attr.block.graph_arr[pos]) {
    block->attr.block.graph_arr[pos] = res;
  } else {
    /*  printf(" value already computed by %s\n",
        get_id_str(block->attr.block.graph_arr[pos]->op->name));  */
  }

  return res;
}

/* This function returns the last definition of a variable.  In case
   this variable was last defined in a previous block, Phi nodes are
   inserted.  If the part of the firm graph containing the definition
   is not yet constructed, a dummy Phi node is returned. */
static ir_node *
get_r_value_internal (ir_node *block, int pos, ir_mode *mode)
{
  ir_node *res;
  /* There are 4 cases to treat.

     1. The block is not mature and we visit it the first time.  We can not
        create a proper Phi node, therefore a Phi0, i.e., a Phi without
        predecessors is returned.  This node is added to the linked list (field
        "link") of the containing block to be completed when this block is
        matured. (Completion will add a new Phi and turn the Phi0 into an Id
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
        Look also at phi_merge and new_rd_phi_in to understand this.
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
  if (res) return res;

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
       to the list of Phi0 nodes in this block to be matured by mature_immBlock
       later.
       The Phi0 has to remember the pos of it's internal value.  If the real
       Phi is computed, pos is used to update the array with the local
       values. */

    res = new_rd_Phi0 (current_ir_graph, block, mode);
    res->attr.phi0_pos = pos;
    res->link = block->link;
    block->link = res;
  }

  /* If we get here, the frontend missed a use-before-definition error */
  if (!res) {
    /* Error Message */
    printf("Error: no value set.  Use of undefined variable.  Initializing to zero.\n");
    assert (mode->code >= irm_F && mode->code <= irm_P);
    res = new_rd_Const (NULL, current_ir_graph, block, mode,
               tarval_mode_null[mode->code]);
  }

  /* The local valid value is available now. */
  block->attr.block.graph_arr[pos] = res;

  return res;
}

#else /* if 0 */

/**
    it starts the recursion.  This causes an Id at the entry of
    every block that has no definition of the value! **/

#if USE_EXPLICIT_PHI_IN_STACK
/* Just dummies */
INLINE Phi_in_stack * new_Phi_in_stack() {  return NULL; }
INLINE void free_Phi_in_stack(Phi_in_stack *s) { }
#endif

static INLINE ir_node *
new_rd_Phi_in (ir_graph *irg, ir_node *block, ir_mode *mode,
	       ir_node **in, int ins, ir_node *phi0)
{
  int i;
  ir_node *res, *known;

  /* Allocate a new node on the obstack.  The allocation copies the in
     array. */
  res = new_ir_node (NULL, irg, block, op_Phi, mode, ins, in);
  res->attr.phi_backedge = new_backedge_arr(irg->obst, ins);

  /* This loop checks whether the Phi has more than one predecessor.
     If so, it is a real Phi node and we break the loop.  Else the
     Phi node merges the same definition on several paths and therefore
     is not needed. Don't consider Bad nodes! */
  known = res;
  for (i=0;  i < ins;  ++i)
  {
    assert(in[i]);

    in[i] = skip_Id(in[i]);  /* increasses the number of freed Phis. */

    /* Optimize self referencing Phis:  We can't detect them yet properly, as
       they still refer to the Phi0 they will replace.  So replace right now. */
    if (phi0 && in[i] == phi0) in[i] = res;

    if (in[i]==res || in[i]==known || is_Bad(in[i])) continue;

    if (known==res)
      known = in[i];
    else
      break;
  }

  /* i==ins: there is at most one predecessor, we don't need a phi node. */
  if (i == ins) {
    if (res != known) {
      obstack_free (current_ir_graph->obst, res);
      if (is_Phi(known)) {
	/* If pred is a phi node we want to optmize it: If loops are matured in a bad
	   order, an enclosing Phi know may get superfluous. */
	res = optimize_in_place_2(known);
	if (res != known) { exchange(known, res); }
      } else {
	res = known;
      }
    } else {
      /* A undefined value, e.g., in unreachable code. */
      res = new_Bad();
    }
  } else {
    res = optimize_node (res);  /* This is necessary to add the node to the hash table for cse. */
    IRN_VRFY_IRG(res, irg);
    /* Memory Phis in endless loops must be kept alive.
       As we can't distinguish these easily we keep all of them alive. */
    if ((res->op == op_Phi) && (mode == mode_M))
      add_End_keepalive(irg->end, res);
  }

  return res;
}

static ir_node *
get_r_value_internal (ir_node *block, int pos, ir_mode *mode);

#if PRECISE_EXC_CONTEXT
static ir_node *
phi_merge (ir_node *block, int pos, ir_mode *mode, ir_node **nin, int ins);

/* Construct a new frag_array for node n.
   Copy the content from the current graph_arr of the corresponding block:
   this is the current state.
   Set ProjM(n) as current memory state.
   Further the last entry in frag_arr of current block points to n.  This
   constructs a chain block->last_frag_op-> ... first_frag_op of all frag ops in the block.
 */
static INLINE ir_node ** new_frag_arr (ir_node *n)
{
  ir_node **arr;
  int opt;

  arr = NEW_ARR_D (ir_node *, current_ir_graph->obst, current_ir_graph->n_loc);
  memcpy(arr, current_ir_graph->current_block->attr.block.graph_arr,
     sizeof(ir_node *)*current_ir_graph->n_loc);

  /* turn off optimization before allocating Proj nodes, as res isn't
     finished yet. */
  opt = get_opt_optimize(); set_optimize(0);
  /* Here we rely on the fact that all frag ops have Memory as first result! */
  if (get_irn_op(n) == op_Call)
    arr[0] = new_Proj(n, mode_M, pn_Call_M_except);
  else {
    assert((pn_Quot_M == pn_DivMod_M) &&
	   (pn_Quot_M == pn_Div_M)    &&
	   (pn_Quot_M == pn_Mod_M)    &&
	   (pn_Quot_M == pn_Load_M)   &&
	   (pn_Quot_M == pn_Store_M)  &&
	   (pn_Quot_M == pn_Alloc_M)    );
    arr[0] = new_Proj(n, mode_M, pn_Alloc_M);
  }
  set_optimize(opt);

  current_ir_graph->current_block->attr.block.graph_arr[current_ir_graph->n_loc-1] = n;
  return arr;
}

/**
 * returns the frag_arr from a node
 */
static INLINE ir_node **
get_frag_arr (ir_node *n) {
  switch (get_irn_opcode(n)) {
  case iro_Call:
    return n->attr.call.exc.frag_arr;
  case iro_Alloc:
    return n->attr.a.exc.frag_arr;
  case iro_Load:
    return n->attr.load.exc.frag_arr;
  case iro_Store:
    return n->attr.store.exc.frag_arr;
  default:
    return n->attr.except.frag_arr;
  }
}

static void
set_frag_value(ir_node **frag_arr, int pos, ir_node *val) {
#if 0
  if (!frag_arr[pos]) frag_arr[pos] = val;
  if (frag_arr[current_ir_graph->n_loc - 1]) {
    ir_node **arr = get_frag_arr(frag_arr[current_ir_graph->n_loc - 1]);
    assert(arr != frag_arr && "Endless recursion detected");
    set_frag_value(arr, pos, val);
  }
#else
  int i;

  for (i = 0; i < 1000; ++i) {
    if (!frag_arr[pos]) {
      frag_arr[pos] = val;
    }
    if (frag_arr[current_ir_graph->n_loc - 1]) {
      ir_node **arr = get_frag_arr(frag_arr[current_ir_graph->n_loc - 1]);
      frag_arr = arr;
    }
    else
      return;
  }
  assert(0 && "potential endless recursion");
#endif
}

static ir_node *
get_r_frag_value_internal (ir_node *block, ir_node *cfOp, int pos, ir_mode *mode) {
  ir_node *res;
  ir_node **frag_arr;

  assert(is_fragile_op(cfOp) && (get_irn_op(cfOp) != op_Bad));

  frag_arr = get_frag_arr(cfOp);
  res = frag_arr[pos];
  if (!res) {
    if (block->attr.block.graph_arr[pos]) {
      /* There was a set_value after the cfOp and no get_value before that
         set_value.  We must build a Phi node now. */
      if (block->attr.block.matured) {
        int ins = get_irn_arity(block);
        ir_node **nin;
        NEW_ARR_A (ir_node *, nin, ins);
        res = phi_merge(block, pos, mode, nin, ins);
      } else {
        res = new_rd_Phi0 (current_ir_graph, block, mode);
        res->attr.phi0_pos = pos;
        res->link = block->link;
        block->link = res;
      }
      assert(res);
      /* @@@ tested by Flo: set_frag_value(frag_arr, pos, res);
         but this should be better: (remove comment if this works) */
      /* It's a Phi, we can write this into all graph_arrs with NULL */
      set_frag_value(block->attr.block.graph_arr, pos, res);
    } else {
      res = get_r_value_internal(block, pos, mode);
      set_frag_value(block->attr.block.graph_arr, pos, res);
    }
  }
  return res;
}
#endif

/**
    computes the predecessors for the real phi node, and then
    allocates and returns this node.  The routine called to allocate the
    node might optimize it away and return a real value.
    This function must be called with an in-array of proper size. **/
static ir_node *
phi_merge (ir_node *block, int pos, ir_mode *mode, ir_node **nin, int ins)
{
  ir_node *prevBlock, *prevCfOp, *res, *phi0, *phi0_all;
  int i;

  /* If this block has no value at pos create a Phi0 and remember it
     in graph_arr to break recursions.
     Else we may not set graph_arr as there a later value is remembered. */
  phi0 = NULL;
  if (!block->attr.block.graph_arr[pos]) {
    if (block == get_irg_start_block(current_ir_graph)) {
      /* Collapsing to Bad tarvals is no good idea.
         So we call a user-supplied routine here that deals with this case as
         appropriate for the given language. Sorryly the only help we can give
         here is the position.

         Even if all variables are defined before use, it can happen that
         we get to the start block, if a cond has been replaced by a tuple
         (bad, jmp).  In this case we call the function needlessly, eventually
         generating an non existant error.
         However, this SHOULD NOT HAPPEN, as bad control flow nodes are intercepted
         before recuring.
      */
      if (default_initialize_local_variable)
        block->attr.block.graph_arr[pos] = default_initialize_local_variable(mode, pos - 1);
      else
        block->attr.block.graph_arr[pos] = new_Const(mode, tarval_bad);
      /* We don't need to care about exception ops in the start block.
	 There are none by definition. */
      return block->attr.block.graph_arr[pos];
    } else {
      phi0 = new_rd_Phi0(current_ir_graph, block, mode);
      block->attr.block.graph_arr[pos] = phi0;
#if PRECISE_EXC_CONTEXT
      if (get_opt_precise_exc_context()) {
	/* Set graph_arr for fragile ops.  Also here we should break recursion.
	   We could choose a cyclic path through an cfop.  But the recursion would
	   break at some point. */
	set_frag_value(block->attr.block.graph_arr, pos, phi0);
      }
#endif
    }
  }

  /* This loop goes to all predecessor blocks of the block the Phi node
     is in and there finds the operands of the Phi node by calling
     get_r_value_internal.  */
  for (i = 1;  i <= ins;  ++i) {
    prevCfOp = skip_Proj(block->in[i]);
    assert (prevCfOp);
    if (is_Bad(prevCfOp)) {
      /* In case a Cond has been optimized we would get right to the start block
     with an invalid definition. */
      nin[i-1] = new_Bad();
      continue;
    }
    prevBlock = block->in[i]->in[0]; /* go past control flow op to prev block */
    assert (prevBlock);
    if (!is_Bad(prevBlock)) {
#if PRECISE_EXC_CONTEXT
      if (get_opt_precise_exc_context() &&
	  is_fragile_op(prevCfOp) && (get_irn_op (prevCfOp) != op_Bad)) {
	assert(get_r_frag_value_internal (prevBlock, prevCfOp, pos, mode));
	nin[i-1] = get_r_frag_value_internal (prevBlock, prevCfOp, pos, mode);
      } else
#endif
      nin[i-1] = get_r_value_internal (prevBlock, pos, mode);
    } else {
      nin[i-1] = new_Bad();
    }
  }

  /* We want to pass the Phi0 node to the constructor: this finds additional
     optimization possibilities.
     The Phi0 node either is allocated in this function, or it comes from
     a former call to get_r_value_internal. In this case we may not yet
     exchange phi0, as this is done in mature_immBlock. */
  if (!phi0) {
    phi0_all = block->attr.block.graph_arr[pos];
    if (!((get_irn_op(phi0_all) == op_Phi) &&
	  (get_irn_arity(phi0_all) == 0)   &&
	  (get_nodes_block(phi0_all) == block)))
      phi0_all = NULL;
  } else {
    phi0_all = phi0;
  }

  /* After collecting all predecessors into the array nin a new Phi node
     with these predecessors is created.  This constructor contains an
     optimization: If all predecessors of the Phi node are identical it
     returns the only operand instead of a new Phi node.  */
  res = new_rd_Phi_in (current_ir_graph, block, mode, nin, ins, phi0_all);

  /* In case we allocated a Phi0 node at the beginning of this procedure,
     we need to exchange this Phi0 with the real Phi. */
  if (phi0) {
    exchange(phi0, res);
    block->attr.block.graph_arr[pos] = res;
    /* Don't set_frag_value as it does not overwrite.  Doesn't matter, is
       only an optimization. */
  }

  return res;
}

/* This function returns the last definition of a variable.  In case
   this variable was last defined in a previous block, Phi nodes are
   inserted.  If the part of the firm graph containing the definition
   is not yet constructed, a dummy Phi node is returned. */
static ir_node *
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
       to be matured by mature_immBlock later.
       The Phi0 has to remember the pos of it's internal value.  If the real
       Phi is computed, pos is used to update the array with the local
       values. */
    res = new_rd_Phi0 (current_ir_graph, block, mode);
    res->attr.phi0_pos = pos;
    res->link = block->link;
    block->link = res;
  }

  /* If we get here, the frontend missed a use-before-definition error */
  if (!res) {
    /* Error Message */
    printf("Error: no value set.  Use of undefined variable.  Initializing to zero.\n");
    assert (mode->code >= irm_F && mode->code <= irm_P);
    res = new_rd_Const (NULL, current_ir_graph, block, mode,
			get_mode_null(mode));
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
mature_immBlock (ir_node *block)
{

  int ins;
  ir_node *n, **nin;
  ir_node *next;

  assert (get_irn_opcode(block) == iro_Block);
  /* @@@ should be commented in
     assert (!get_Block_matured(block) && "Block already matured"); */

  if (!get_Block_matured(block)) {
    ins = ARR_LEN (block->in)-1;
    /* Fix block parameters */
    block->attr.block.backedge = new_backedge_arr(current_ir_graph->obst, ins);

    /* An array for building the Phi nodes. */
    NEW_ARR_A (ir_node *, nin, ins);

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
       nodes refer to the unoptimized node.
       We can call _2, as global cse has no effect on blocks. */
    block = optimize_in_place_2(block);
    IRN_VRFY_IRG(block, current_ir_graph);
  }
}

ir_node *
new_d_Phi (dbg_info* db, int arity, ir_node **in, ir_mode *mode)
{
  return new_rd_Phi(db, current_ir_graph, current_ir_graph->current_block,
            arity, in, mode);
}

ir_node *
new_d_Const (dbg_info* db, ir_mode *mode, tarval *con)
{
  return new_rd_Const(db, current_ir_graph, current_ir_graph->start_block,
              mode, con);
}

ir_node *
new_d_Const_type (dbg_info* db, ir_mode *mode, tarval *con, type *tp)
{
  return new_rd_Const_type(db, current_ir_graph, current_ir_graph->start_block,
                mode, con, tp);
}


ir_node *
new_d_Id (dbg_info* db, ir_node *val, ir_mode *mode)
{
  return new_rd_Id(db, current_ir_graph, current_ir_graph->current_block,
           val, mode);
}

ir_node *
new_d_Proj (dbg_info* db, ir_node *arg, ir_mode *mode, long proj)
{
  return new_rd_Proj(db, current_ir_graph, current_ir_graph->current_block,
             arg, mode, proj);
}

ir_node *
new_d_defaultProj (dbg_info* db, ir_node *arg, long max_proj)
{
  ir_node *res;
  assert(arg->op == op_Cond);
  arg->attr.c.kind = fragmentary;
  arg->attr.c.default_proj = max_proj;
  res = new_Proj (arg, mode_X, max_proj);
  return res;
}

ir_node *
new_d_Conv (dbg_info* db, ir_node *op, ir_mode *mode)
{
  return new_rd_Conv(db, current_ir_graph, current_ir_graph->current_block,
             op, mode);
}

ir_node *
new_d_Cast (dbg_info* db, ir_node *op, type *to_tp)
{
  return new_rd_Cast(db, current_ir_graph, current_ir_graph->current_block, op, to_tp);
}

ir_node *
new_d_Tuple (dbg_info* db, int arity, ir_node **in)
{
  return new_rd_Tuple(db, current_ir_graph, current_ir_graph->current_block,
              arity, in);
}

ir_node *
new_d_Add (dbg_info* db, ir_node *op1, ir_node *op2, ir_mode *mode)
{
  return new_rd_Add(db, current_ir_graph, current_ir_graph->current_block,
            op1, op2, mode);
}

ir_node *
new_d_Sub (dbg_info* db, ir_node *op1, ir_node *op2, ir_mode *mode)
{
  return new_rd_Sub(db, current_ir_graph, current_ir_graph->current_block,
            op1, op2, mode);
}


ir_node *
new_d_Minus (dbg_info* db, ir_node *op,  ir_mode *mode)
{
  return new_rd_Minus(db, current_ir_graph, current_ir_graph->current_block,
              op, mode);
}

ir_node *
new_d_Mul (dbg_info* db, ir_node *op1, ir_node *op2, ir_mode *mode)
{
  return new_rd_Mul(db, current_ir_graph, current_ir_graph->current_block,
            op1, op2, mode);
}

/**
 * allocate the frag array
 */
static void allocate_frag_arr(ir_node *res, ir_op *op, ir_node ***frag_store) {
  if (get_opt_precise_exc_context()) {
    if ((current_ir_graph->phase_state == phase_building) &&
	(get_irn_op(res) == op) && /* Could be optimized away. */
	!*frag_store)    /* Could be a cse where the arr is already set. */ {
      *frag_store = new_frag_arr(res);
    }
  }
}


ir_node *
new_d_Quot (dbg_info* db, ir_node *memop, ir_node *op1, ir_node *op2)
{
  ir_node *res;
  res = new_rd_Quot (db, current_ir_graph, current_ir_graph->current_block,
             memop, op1, op2);
  res->attr.except.pin_state = op_pin_state_pinned;
#if PRECISE_EXC_CONTEXT
  allocate_frag_arr(res, op_Quot, &res->attr.except.frag_arr);  /* Could be optimized away. */
#endif

  return res;
}

ir_node *
new_d_DivMod (dbg_info* db, ir_node *memop, ir_node *op1, ir_node *op2)
{
  ir_node *res;
  res = new_rd_DivMod (db, current_ir_graph, current_ir_graph->current_block,
               memop, op1, op2);
  res->attr.except.pin_state = op_pin_state_pinned;
#if PRECISE_EXC_CONTEXT
  allocate_frag_arr(res, op_DivMod, &res->attr.except.frag_arr);  /* Could be optimized away. */
#endif

  return res;
}

ir_node *
new_d_Div (dbg_info* db, ir_node *memop, ir_node *op1, ir_node *op2)
{
  ir_node *res;
  res = new_rd_Div (db, current_ir_graph, current_ir_graph->current_block,
            memop, op1, op2);
  res->attr.except.pin_state = op_pin_state_pinned;
#if PRECISE_EXC_CONTEXT
  allocate_frag_arr(res, op_Div, &res->attr.except.frag_arr);  /* Could be optimized away. */
#endif

  return res;
}

ir_node *
new_d_Mod (dbg_info* db, ir_node *memop, ir_node *op1, ir_node *op2)
{
  ir_node *res;
  res = new_rd_Mod (db, current_ir_graph, current_ir_graph->current_block,
            memop, op1, op2);
  res->attr.except.pin_state = op_pin_state_pinned;
#if PRECISE_EXC_CONTEXT
  allocate_frag_arr(res, op_Mod, &res->attr.except.frag_arr);  /* Could be optimized away. */
#endif

  return res;
}

ir_node *
new_d_And (dbg_info* db, ir_node *op1, ir_node *op2, ir_mode *mode)
{
  return new_rd_And (db, current_ir_graph, current_ir_graph->current_block,
            op1, op2, mode);
}

ir_node *
new_d_Or (dbg_info* db, ir_node *op1, ir_node *op2, ir_mode *mode)
{
  return new_rd_Or (db, current_ir_graph, current_ir_graph->current_block,
           op1, op2, mode);
}

ir_node *
new_d_Eor (dbg_info* db, ir_node *op1, ir_node *op2, ir_mode *mode)
{
  return new_rd_Eor (db, current_ir_graph, current_ir_graph->current_block,
            op1, op2, mode);
}

ir_node *
new_d_Not (dbg_info* db, ir_node *op, ir_mode *mode)
{
  return new_rd_Not (db, current_ir_graph, current_ir_graph->current_block,
            op, mode);
}

ir_node *
new_d_Shl (dbg_info* db, ir_node *op, ir_node *k, ir_mode *mode)
{
  return new_rd_Shl (db, current_ir_graph, current_ir_graph->current_block,
            op, k, mode);
}

ir_node *
new_d_Shr (dbg_info* db, ir_node *op, ir_node *k, ir_mode *mode)
{
  return new_rd_Shr (db, current_ir_graph, current_ir_graph->current_block,
            op, k, mode);
}

ir_node *
new_d_Shrs (dbg_info* db, ir_node *op, ir_node *k, ir_mode *mode)
{
  return new_rd_Shrs (db, current_ir_graph, current_ir_graph->current_block,
             op, k, mode);
}

ir_node *
new_d_Rot (dbg_info* db, ir_node *op, ir_node *k, ir_mode *mode)
{
  return new_rd_Rot (db, current_ir_graph, current_ir_graph->current_block,
             op, k, mode);
}

ir_node *
new_d_Abs (dbg_info* db, ir_node *op, ir_mode *mode)
{
  return new_rd_Abs (db, current_ir_graph, current_ir_graph->current_block,
            op, mode);
}

ir_node *
new_d_Cmp (dbg_info* db, ir_node *op1, ir_node *op2)
{
  return new_rd_Cmp (db, current_ir_graph, current_ir_graph->current_block,
            op1, op2);
}

ir_node *
new_d_Jmp (dbg_info* db)
{
  return new_rd_Jmp (db, current_ir_graph, current_ir_graph->current_block);
}

ir_node *
new_d_Cond (dbg_info* db, ir_node *c)
{
  return new_rd_Cond (db, current_ir_graph, current_ir_graph->current_block, c);
}

ir_node *
new_d_Call (dbg_info* db, ir_node *store, ir_node *callee, int arity, ir_node **in,
      type *tp)
{
  ir_node *res;
  res = new_rd_Call (db, current_ir_graph, current_ir_graph->current_block,
             store, callee, arity, in, tp);
#if PRECISE_EXC_CONTEXT
  allocate_frag_arr(res, op_Call, &res->attr.call.exc.frag_arr);  /* Could be optimized away. */
#endif

  return res;
}

ir_node *
new_d_Return (dbg_info* db, ir_node* store, int arity, ir_node **in)
{
  return new_rd_Return (db, current_ir_graph, current_ir_graph->current_block,
               store, arity, in);
}

ir_node *
new_d_Raise (dbg_info* db, ir_node *store, ir_node *obj)
{
  return new_rd_Raise (db, current_ir_graph, current_ir_graph->current_block,
              store, obj);
}

ir_node *
new_d_Load (dbg_info* db, ir_node *store, ir_node *addr, ir_mode *mode)
{
  ir_node *res;
  res = new_rd_Load (db, current_ir_graph, current_ir_graph->current_block,
             store, addr, mode);
#if PRECISE_EXC_CONTEXT
  allocate_frag_arr(res, op_Load, &res->attr.load.exc.frag_arr);  /* Could be optimized away. */
#endif

  return res;
}

ir_node *
new_d_Store (dbg_info* db, ir_node *store, ir_node *addr, ir_node *val)
{
  ir_node *res;
  res = new_rd_Store (db, current_ir_graph, current_ir_graph->current_block,
              store, addr, val);
#if PRECISE_EXC_CONTEXT
  allocate_frag_arr(res, op_Store, &res->attr.store.exc.frag_arr);  /* Could be optimized away. */
#endif

  return res;
}

ir_node *
new_d_Alloc (dbg_info* db, ir_node *store, ir_node *size, type *alloc_type,
           where_alloc where)
{
  ir_node *res;
  res = new_rd_Alloc (db, current_ir_graph, current_ir_graph->current_block,
              store, size, alloc_type, where);
#if PRECISE_EXC_CONTEXT
  allocate_frag_arr(res, op_Alloc, &res->attr.a.exc.frag_arr);  /* Could be optimized away. */
#endif

  return res;
}

ir_node *
new_d_Free (dbg_info* db, ir_node *store, ir_node *ptr, ir_node *size, type *free_type)
{
  return new_rd_Free (db, current_ir_graph, current_ir_graph->current_block,
             store, ptr, size, free_type);
}

ir_node *
new_d_simpleSel (dbg_info* db, ir_node *store, ir_node *objptr, entity *ent)
/* GL: objptr was called frame before.  Frame was a bad choice for the name
   as the operand could as well be a pointer to a dynamic object. */
{
  return new_rd_Sel (db, current_ir_graph, current_ir_graph->current_block,
            store, objptr, 0, NULL, ent);
}

ir_node *
new_d_Sel (dbg_info* db, ir_node *store, ir_node *objptr, int n_index, ir_node **index, entity *sel)
{
  return new_rd_Sel (db, current_ir_graph, current_ir_graph->current_block,
            store, objptr, n_index, index, sel);
}

ir_node *
new_d_InstOf (dbg_info *db, ir_node *store, ir_node *objptr, type *ent)
{
  return (new_rd_InstOf (db, current_ir_graph, current_ir_graph->current_block,
                         store, objptr, ent));
}

ir_node *
new_d_SymConst_type (dbg_info* db, symconst_symbol value, symconst_kind kind, type *tp)
{
  return new_rd_SymConst_type (db, current_ir_graph, current_ir_graph->start_block,
                         value, kind, tp);
}

ir_node *
new_d_SymConst (dbg_info* db, symconst_symbol value, symconst_kind kind)
{
  return new_rd_SymConst (db, current_ir_graph, current_ir_graph->start_block,
                         value, kind);
}

ir_node *
new_d_Sync (dbg_info* db, int arity, ir_node** in)
{
  return new_rd_Sync (db, current_ir_graph, current_ir_graph->current_block,
              arity, in);
}


ir_node *
(new_d_Bad)(void)
{
  return __new_d_Bad();
}

ir_node *
new_d_Confirm (dbg_info *db, ir_node *val, ir_node *bound, pn_Cmp cmp)
{
  return new_rd_Confirm (db, current_ir_graph, current_ir_graph->current_block,
             val, bound, cmp);
}

ir_node *
new_d_Unknown (ir_mode *m)
{
  return new_rd_Unknown(current_ir_graph, m);
}

ir_node *
new_d_CallBegin (dbg_info *db, ir_node *call)
{
  ir_node *res;
  res = new_rd_CallBegin (db, current_ir_graph, current_ir_graph->current_block, call);
  return res;
}

ir_node *
new_d_EndReg (dbg_info *db)
{
  ir_node *res;
  res = new_rd_EndReg(db, current_ir_graph, current_ir_graph->current_block);
  return res;
}

ir_node *
new_d_EndExcept (dbg_info *db)
{
  ir_node *res;
  res = new_rd_EndExcept(db, current_ir_graph, current_ir_graph->current_block);
  return res;
}

ir_node *
new_d_Break (dbg_info *db)
{
  return new_rd_Break (db, current_ir_graph, current_ir_graph->current_block);
}

ir_node *
new_d_Filter (dbg_info *db, ir_node *arg, ir_mode *mode, long proj)
{
  return new_rd_Filter (db, current_ir_graph, current_ir_graph->current_block,
            arg, mode, proj);
}

ir_node *
(new_d_NoMem)(void)
{
  return __new_d_NoMem();
}

/* ********************************************************************* */
/* Comfortable interface with automatic Phi node construction.           */
/* (Uses also constructors of ?? interface, except new_Block.            */
/* ********************************************************************* */

/* * Block construction **/
/* immature Block without predecessors */
ir_node *new_d_immBlock (dbg_info* db) {
  ir_node *res;

  assert(get_irg_phase_state (current_ir_graph) == phase_building);
  /* creates a new dynamic in-array as length of in is -1 */
  res = new_ir_node (db, current_ir_graph, NULL, op_Block, mode_BB, -1, NULL);
  current_ir_graph->current_block = res;
  res->attr.block.matured     = 0;
  res->attr.block.dead        = 0;
  /* res->attr.block.exc = exc_normal; */
  /* res->attr.block.handler_entry = 0; */
  res->attr.block.irg         = current_ir_graph;
  res->attr.block.backedge    = NULL;
  res->attr.block.in_cg       = NULL;
  res->attr.block.cg_backedge = NULL;
  set_Block_block_visited(res, 0);

  /* Create and initialize array for Phi-node construction. */
  res->attr.block.graph_arr = NEW_ARR_D (ir_node *, current_ir_graph->obst,
                                         current_ir_graph->n_loc);
  memset(res->attr.block.graph_arr, 0, sizeof(ir_node *)*current_ir_graph->n_loc);

  /* Immature block may not be optimized! */
  IRN_VRFY_IRG(res, current_ir_graph);

  return res;
}

INLINE ir_node *
new_immBlock (void) {
  return new_d_immBlock(NULL);
}

/* add an adge to a jmp/control flow node */
void
add_immBlock_pred (ir_node *block, ir_node *jmp)
{
  if (block->attr.block.matured) {
    assert(0 && "Error: Block already matured!\n");
  }
  else {
    assert(jmp != NULL);
    ARR_APP1(ir_node *, block->in, jmp);
  }
}

/* changing the current block */
void
set_cur_block (ir_node *target)
{
  current_ir_graph->current_block = target;
}

/* ************************ */
/* parameter administration */

/* get a value from the parameter array from the current block by its index */
ir_node *
get_d_value (dbg_info* db, int pos, ir_mode *mode)
{
  assert(get_irg_phase_state (current_ir_graph) == phase_building);
  inc_irg_visited(current_ir_graph);

  return get_r_value_internal (current_ir_graph->current_block, pos + 1, mode);
}
/* get a value from the parameter array from the current block by its index */
INLINE ir_node *
get_value (int pos, ir_mode *mode)
{
  return get_d_value(NULL, pos, mode);
}

/* set a value at position pos in the parameter array from the current block */
INLINE void
set_value (int pos, ir_node *value)
{
  assert(get_irg_phase_state (current_ir_graph) == phase_building);
  assert(pos+1 < current_ir_graph->n_loc);
  current_ir_graph->current_block->attr.block.graph_arr[pos + 1] = value;
}

/* get the current store */
INLINE ir_node *
get_store (void)
{
  assert(get_irg_phase_state (current_ir_graph) == phase_building);
  /* GL: one could call get_value instead */
  inc_irg_visited(current_ir_graph);
  return get_r_value_internal (current_ir_graph->current_block, 0, mode_M);
}

/* set the current store */
INLINE void
set_store (ir_node *store)
{
  /* GL: one could call set_value instead */
  assert(get_irg_phase_state (current_ir_graph) == phase_building);
  current_ir_graph->current_block->attr.block.graph_arr[0] = store;
}

void
keep_alive (ir_node *ka)
{
  add_End_keepalive(current_ir_graph->end, ka);
}

/** Useful access routines **/
/* Returns the current block of the current graph.  To set the current
   block use set_cur_block. */
ir_node *get_cur_block() {
  return get_irg_current_block(current_ir_graph);
}

/* Returns the frame type of the current graph */
type *get_cur_frame_type() {
  return get_irg_frame_type(current_ir_graph);
}


/* ********************************************************************* */
/* initialize */

/* call once for each run of the library */
void
init_cons (default_initialize_local_variable_func_t *func)
{
  default_initialize_local_variable = func;
}

/* call for each graph */
void
finalize_cons (ir_graph *irg) {
  irg->phase_state = phase_high;
}


ir_node *new_Block(int arity, ir_node **in) {
  return new_d_Block(NULL, arity, in);
}
ir_node *new_Start  (void) {
  return new_d_Start(NULL);
}
ir_node *new_End    (void) {
  return new_d_End(NULL);
}
ir_node *new_Jmp    (void) {
  return new_d_Jmp(NULL);
}
ir_node *new_Cond   (ir_node *c) {
  return new_d_Cond(NULL, c);
}
ir_node *new_Return (ir_node *store, int arity, ir_node *in[]) {
  return new_d_Return(NULL, store, arity, in);
}
ir_node *new_Raise  (ir_node *store, ir_node *obj) {
  return new_d_Raise(NULL, store, obj);
}
ir_node *new_Const  (ir_mode *mode, tarval *con) {
  return new_d_Const(NULL, mode, con);
}

ir_node *new_Const_type(tarval *con, type *tp) {
  return new_d_Const_type(NULL, get_type_mode(tp), con, tp);
}

ir_node *new_SymConst (symconst_symbol value, symconst_kind kind) {
  return new_d_SymConst(NULL, value, kind);
}
ir_node *new_simpleSel(ir_node *store, ir_node *objptr, entity *ent) {
  return new_d_simpleSel(NULL, store, objptr, ent);
}
ir_node *new_Sel    (ir_node *store, ir_node *objptr, int arity, ir_node **in,
                     entity *ent) {
  return new_d_Sel(NULL, store, objptr, arity, in, ent);
}
ir_node *new_InstOf (ir_node *store, ir_node *objptr, type *ent) {
  return new_d_InstOf (NULL, store, objptr, ent);
}
ir_node *new_Call   (ir_node *store, ir_node *callee, int arity, ir_node **in,
             type *tp) {
  return new_d_Call(NULL, store, callee, arity, in, tp);
}
ir_node *new_Add    (ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_d_Add(NULL, op1, op2, mode);
}
ir_node *new_Sub    (ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_d_Sub(NULL, op1, op2, mode);
}
ir_node *new_Minus  (ir_node *op,  ir_mode *mode) {
  return new_d_Minus(NULL, op, mode);
}
ir_node *new_Mul    (ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_d_Mul(NULL, op1, op2, mode);
}
ir_node *new_Quot   (ir_node *memop, ir_node *op1, ir_node *op2) {
  return new_d_Quot(NULL, memop, op1, op2);
}
ir_node *new_DivMod (ir_node *memop, ir_node *op1, ir_node *op2) {
  return new_d_DivMod(NULL, memop, op1, op2);
}
ir_node *new_Div    (ir_node *memop, ir_node *op1, ir_node *op2) {
  return new_d_Div(NULL, memop, op1, op2);
}
ir_node *new_Mod    (ir_node *memop, ir_node *op1, ir_node *op2) {
  return new_d_Mod(NULL, memop, op1, op2);
}
ir_node *new_Abs    (ir_node *op, ir_mode *mode) {
  return new_d_Abs(NULL, op, mode);
}
ir_node *new_And    (ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_d_And(NULL, op1, op2, mode);
}
ir_node *new_Or     (ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_d_Or(NULL, op1, op2, mode);
}
ir_node *new_Eor    (ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_d_Eor(NULL, op1, op2, mode);
}
ir_node *new_Not    (ir_node *op,                ir_mode *mode) {
  return new_d_Not(NULL, op, mode);
}
ir_node *new_Shl    (ir_node *op,  ir_node *k,   ir_mode *mode) {
  return new_d_Shl(NULL, op, k, mode);
}
ir_node *new_Shr    (ir_node *op,  ir_node *k,   ir_mode *mode) {
  return new_d_Shr(NULL, op, k, mode);
}
ir_node *new_Shrs   (ir_node *op,  ir_node *k,   ir_mode *mode) {
  return new_d_Shrs(NULL, op, k, mode);
}
#define new_Rotate new_Rot
ir_node *new_Rot    (ir_node *op,  ir_node *k,   ir_mode *mode) {
  return new_d_Rot(NULL, op, k, mode);
}
ir_node *new_Cmp    (ir_node *op1, ir_node *op2) {
  return new_d_Cmp(NULL, op1, op2);
}
ir_node *new_Conv   (ir_node *op, ir_mode *mode) {
  return new_d_Conv(NULL, op, mode);
}
ir_node *new_Cast   (ir_node *op, type *to_tp) {
  return new_d_Cast(NULL, op, to_tp);
}
ir_node *new_Phi    (int arity, ir_node **in, ir_mode *mode) {
  return new_d_Phi(NULL, arity, in, mode);
}
ir_node *new_Load   (ir_node *store, ir_node *addr, ir_mode *mode) {
  return new_d_Load(NULL, store, addr, mode);
}
ir_node *new_Store  (ir_node *store, ir_node *addr, ir_node *val) {
  return new_d_Store(NULL, store, addr, val);
}
ir_node *new_Alloc  (ir_node *store, ir_node *size, type *alloc_type,
                     where_alloc where) {
  return new_d_Alloc(NULL, store, size, alloc_type, where);
}
ir_node *new_Free   (ir_node *store, ir_node *ptr, ir_node *size,
             type *free_type) {
  return new_d_Free(NULL, store, ptr, size, free_type);
}
ir_node *new_Sync   (int arity, ir_node **in) {
  return new_d_Sync(NULL, arity, in);
}
ir_node *new_Proj   (ir_node *arg, ir_mode *mode, long proj) {
  return new_d_Proj(NULL, arg, mode, proj);
}
ir_node *new_defaultProj (ir_node *arg, long max_proj) {
  return new_d_defaultProj(NULL, arg, max_proj);
}
ir_node *new_Tuple  (int arity, ir_node **in) {
  return new_d_Tuple(NULL, arity, in);
}
ir_node *new_Id     (ir_node *val, ir_mode *mode) {
  return new_d_Id(NULL, val, mode);
}
ir_node *new_Bad    (void) {
  return new_d_Bad();
}
ir_node *new_Confirm (ir_node *val, ir_node *bound, pn_Cmp cmp) {
  return new_d_Confirm (NULL, val, bound, cmp);
}
ir_node *new_Unknown(ir_mode *m) {
  return new_d_Unknown(m);
}
ir_node *new_CallBegin (ir_node *callee) {
  return new_d_CallBegin(NULL, callee);
}
ir_node *new_EndReg (void) {
  return new_d_EndReg(NULL);
}
ir_node *new_EndExcept (void) {
  return new_d_EndExcept(NULL);
}
ir_node *new_Break  (void) {
  return new_d_Break(NULL);
}
ir_node *new_Filter (ir_node *arg, ir_mode *mode, long proj) {
  return new_d_Filter(NULL, arg, mode, proj);
}
ir_node *new_NoMem  (void) {
  return new_d_NoMem();
}
