/*
 * Project:     libFIRM
 * File name:   ir/ir/iropt.c
 * Purpose:     iropt --- optimizations intertwined with IR construction.
 * Author:      Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irnode_t.h"
# include "irgraph_t.h"
# include "irmode_t.h"
# include "iropt_t.h"
# include "ircons_t.h"
# include "irgmod.h"
# include "irvrfy.h"
# include "tv_t.h"
# include "dbginfo_t.h"
# include "iropt_dbg.h"
# include "irflag_t.h"
# include "firmstat.h"
# include "irarch.h"

/* Make types visible to allow most efficient access */
# include "entity_t.h"

/**
 * Trivial INLINEable routine for copy propagation.
 * Does follow Ids, needed to optimize INLINEd code.
 */
static INLINE ir_node *
follow_Id (ir_node *n)
{
  while (get_irn_op (n) == op_Id) n = get_Id_pred (n);
  return n;
}

/**
 * return the value of a Constant
 */
static tarval *computed_value_Const(ir_node *n)
{
    return get_Const_tarval(n);
}

/**
 * return the value of a 'sizeof' SymConst
 */
static tarval *computed_value_SymConst(ir_node *n)
{
  if ((get_SymConst_kind(n) == symconst_size) &&
      (get_type_state(get_SymConst_type(n))) == layout_fixed)
    return new_tarval_from_long(get_type_size_bytes(get_SymConst_type(n)), get_irn_mode(n));
  return tarval_bad;
}

/**
 * return the value of an Add
 */
static tarval *computed_value_Add(ir_node *n)
{
  ir_node *a = get_Add_left(n);
  ir_node *b = get_Add_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad) && (get_irn_mode(a) == get_irn_mode(b)))
    return tarval_add(ta, tb);

  return tarval_bad;
}

/**
 * return the value of a Sub
 * Special case: a - a
 */
static tarval *computed_value_Sub(ir_node *n)
{
  ir_node *a = get_Sub_left(n);
  ir_node *b = get_Sub_right(n);
  tarval *ta;
  tarval *tb;

  /* a - a */
  if (a == b)
    return get_tarval_null(get_irn_mode(n));

  ta = value_of(a);
  tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad) && (get_irn_mode(a) == get_irn_mode(b)))
    return tarval_sub(ta, tb);

  return tarval_bad;
}

/**
 * return the value of an unary Minus
 */
static tarval *computed_value_Minus(ir_node *n)
{
  ir_node *a = get_Minus_op(n);
  tarval *ta = value_of(a);

  if ((ta != tarval_bad) && mode_is_signed(get_irn_mode(a)))
    return tarval_neg(ta);

  return tarval_bad;
}

/**
 * return the value of a Mul
 */
static tarval *computed_value_Mul(ir_node *n)
{
  ir_node *a = get_Mul_left(n);
  ir_node *b = get_Mul_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad) && (get_irn_mode(a) == get_irn_mode(b))) {
    return tarval_mul(ta, tb);
  } else {
    /* a*0 = 0 or 0*b = 0:
       calls computed_value recursive and returns the 0 with proper
       mode. */
    if ((ta != tarval_bad) && (ta == get_mode_null(get_tarval_mode(ta))))
      return ta;
    if ((tb != tarval_bad) && (tb == get_mode_null(get_tarval_mode(tb))))
      return tb;
  }
  return tarval_bad;
}

/**
 * return the value of a floating point Quot
 */
static tarval *computed_value_Quot(ir_node *n)
{
  ir_node *a = get_Quot_left(n);
  ir_node *b = get_Quot_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  /* This was missing in original implementation. Why? */
  if ((ta != tarval_bad) && (tb != tarval_bad) && (get_irn_mode(a) == get_irn_mode(b))) {
    if (tb != get_mode_null(get_tarval_mode(tb)))   /* div by zero: return tarval_bad */
      return tarval_quo(ta, tb);
  }
  return tarval_bad;
}

/**
 * calculate the value of an integer Div of two nodes
 * Special case: 0 / b
 */
static tarval *do_computed_value_Div(ir_node *a, ir_node *b)
{
  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  /* Compute c1 / c2 or 0 / a, a != 0 */
  if (ta != tarval_bad) {
    if ((tb != tarval_bad) && (tb != get_mode_null(get_irn_mode(b))))   /* div by zero: return tarval_bad */
      return tarval_div(ta, tb);
    else if (ta == get_mode_null(get_tarval_mode(ta)))  /* 0 / b == 0 */
      return ta;
  }
  return tarval_bad;
}

/**
 * return the value of an integer Div
 */
static tarval *computed_value_Div(ir_node *n)
{
  return do_computed_value_Div(get_Div_left(n), get_Div_right(n));
}

/**
 * calculate the value of an integer Mod of two nodes
 * Special case: a % 1
 */
static tarval *do_computed_value_Mod(ir_node *a, ir_node *b)
{
  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  /* Compute c1 % c2 or a % 1 */
  if (tb != tarval_bad) {
    if ((ta != tarval_bad) && (tb != get_mode_null(get_tarval_mode(tb))))   /* div by zero: return tarval_bad */
      return tarval_mod(ta, tb);
    else if (tb == get_mode_one(get_tarval_mode(tb)))    /* x mod 1 == 0 */
      return get_mode_null(get_irn_mode(a));
  }

  return tarval_bad;
}

/**
 * return the value of an integer Mod
 */
static tarval *computed_value_Mod(ir_node *n)
{
  return do_computed_value_Mod(get_Mod_left(n), get_Mod_right(n));
}

/**
 * return the value of an Abs
 */
static tarval *computed_value_Abs(ir_node *n)
{
  ir_node *a = get_Abs_op(n);
  tarval *ta = value_of(a);

  if (ta != tarval_bad)
    return tarval_abs(ta);

  return tarval_bad;
}

/**
 * return the value of an And
 * Special case: a & 0, 0 & b
 */
static tarval *computed_value_And(ir_node *n)
{
  ir_node *a = get_And_left(n);
  ir_node *b = get_And_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad)) {
    return tarval_and (ta, tb);
  } else {
    tarval *v;

    if (   (classify_tarval ((v = ta)) == TV_CLASSIFY_NULL)
        || (classify_tarval ((v = tb)) == TV_CLASSIFY_NULL)) {
      return v;
    }
  }
  return tarval_bad;
}

/**
 * return the value of an Or
 * Special case: a | 1...1, 1...1 | b
 */
static tarval *computed_value_Or(ir_node *n)
{
  ir_node *a = get_Or_left(n);
  ir_node *b = get_Or_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad)) {
    return tarval_or (ta, tb);
  } else {
    tarval *v;
    if (   (classify_tarval ((v = ta)) == TV_CLASSIFY_ALL_ONE)
        || (classify_tarval ((v = tb)) == TV_CLASSIFY_ALL_ONE)) {
      return v;
    }
  }
  return tarval_bad;
}

/**
 * return the value of an Eor
 */
static tarval *computed_value_Eor(ir_node *n)
{
  ir_node *a = get_Eor_left(n);
  ir_node *b = get_Eor_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad)) {
    return tarval_eor (ta, tb);
  }
  return tarval_bad;
}

/**
 * return the value of a Not
 */
static tarval *computed_value_Not(ir_node *n)
{
  ir_node *a = get_Not_op(n);
  tarval *ta = value_of(a);

  if (ta != tarval_bad)
    return tarval_not(ta);

  return tarval_bad;
}

/**
 * return the value of a Shl
 */
static tarval *computed_value_Shl(ir_node *n)
{
  ir_node *a = get_Shl_left(n);
  ir_node *b = get_Shl_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad)) {
    return tarval_shl (ta, tb);
  }
  return tarval_bad;
}

/**
 * return the value of a Shr
 */
static tarval *computed_value_Shr(ir_node *n)
{
  ir_node *a = get_Shr_left(n);
  ir_node *b = get_Shr_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad)) {
    return tarval_shr (ta, tb);
  }
  return tarval_bad;
}

/**
 * return the value of a Shrs
 */
static tarval *computed_value_Shrs(ir_node *n)
{
  ir_node *a = get_Shrs_left(n);
  ir_node *b = get_Shrs_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad)) {
    return tarval_shrs (ta, tb);
  }
  return tarval_bad;
}

/**
 * return the value of a Rot
 */
static tarval *computed_value_Rot(ir_node *n)
{
  ir_node *a = get_Rot_left(n);
  ir_node *b = get_Rot_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad)) {
    return tarval_rot (ta, tb);
  }
  return tarval_bad;
}

/**
 * return the value of a Conv
 */
static tarval *computed_value_Conv(ir_node *n)
{
  ir_node *a = get_Conv_op(n);
  tarval *ta = value_of(a);

  if (ta != tarval_bad)
    return tarval_convert_to(ta, get_irn_mode(n));

  return tarval_bad;
}

/**
 * return the value of a Proj, handle Proj(Cmp), Proj(Div), Proj(Mod), Proj(DivMod)
 */
static tarval *computed_value_Proj(ir_node *n)
{
  ir_node *a = get_Proj_pred(n);
  ir_node *aa, *ab;
  long proj_nr;

  /* Optimize Cmp nodes.
     This performs a first step of unreachable code elimination.
     Proj can not be computed, but folding a Cmp above the Proj here is
     not as wasteful as folding a Cmp into a Tuple of 16 Consts of which
     only 1 is used.
     There are several case where we can evaluate a Cmp node:
     1. The nodes compared are both the same.  If we compare for
        equal, greater equal, ... this will return true, else it
        will return false.  This step relies on cse.
     2. The predecessors of Cmp are target values.  We can evaluate
        the Cmp.
     3. The predecessors are Allocs or void* constants.  Allocs never
        return NULL, they raise an exception.   Therefore we can predict
        the Cmp result. */
  switch (get_irn_opcode(a)) {
  case iro_Cmp:
    aa = get_Cmp_left(a);
    ab = get_Cmp_right(a);
    proj_nr = get_Proj_proj(n);

    if (aa == ab) { /* 1.: */
      /* This is a trick with the bits used for encoding the Cmp
         Proj numbers, the following statement is not the same:
      return new_tarval_from_long (proj_nr == Eq, mode_b) */
      return new_tarval_from_long (proj_nr & Eq, mode_b);
    } else {
      tarval *taa = value_of(aa);
      tarval *tab = value_of(ab);

      if ((taa != tarval_bad) && (tab != tarval_bad)) { /* 2.: */
        /* strange checks... */
        pnc_number flags = tarval_cmp (taa, tab);
        if (flags != False) {
          return new_tarval_from_long (proj_nr & flags, mode_b);
        }
      } else {  /* check for 3.: */
        ir_node *aaa = skip_Id(skip_Proj(aa));
        ir_node *aba = skip_Id(skip_Proj(ab));

        if (   (   (/* aa is ProjP and aaa is Alloc */
                       (get_irn_op(aa) == op_Proj)
                    && (mode_is_reference(get_irn_mode(aa)))
                    && (get_irn_op(aaa) == op_Alloc))
                && (   (/* ab is constant void */
                           (get_irn_op(ab) == op_Const)
                        && (mode_is_reference(get_irn_mode(ab)))
                        && (get_Const_tarval(ab) == get_mode_null(get_irn_mode(ab))))
                    || (/* ab is other Alloc */
                           (get_irn_op(ab) == op_Proj)
                        && (mode_is_reference(get_irn_mode(ab)))
                        && (get_irn_op(aba) == op_Alloc)
                        && (aaa != aba))))
            || (/* aa is void and aba is Alloc */
                   (get_irn_op(aa) == op_Const)
                && (mode_is_reference(get_irn_mode(aa)))
                && (get_Const_tarval(aa) == get_mode_null(get_irn_mode(aa)))
                && (get_irn_op(ab) == op_Proj)
                && (mode_is_reference(get_irn_mode(ab)))
                && (get_irn_op(aba) == op_Alloc)))
          /* 3.: */
          return new_tarval_from_long (proj_nr & Ne, mode_b);
      }
    }
    break;

  case iro_DivMod:
    /* compute either the Div or the Mod part */
    proj_nr = get_Proj_proj(n);
    if (proj_nr == pn_DivMod_res_div)
      return do_computed_value_Div(get_DivMod_left(a), get_DivMod_right(a));
    else if (proj_nr == pn_DivMod_res_mod)
      return do_computed_value_Mod(get_DivMod_left(a), get_DivMod_right(a));
    break;

  case iro_Div:
    if (get_Proj_proj(n) == pn_Div_res)
      return computed_value(a);
    break;

  case iro_Mod:
    if (get_Proj_proj(n) == pn_Mod_res)
      return computed_value(a);
    break;

  default:
    return tarval_bad;
  }
  return tarval_bad;
}

/**
 * If the parameter n can be computed, return its value, else tarval_bad.
 * Performs constant folding.
 *
 * @param n  The node this should be evaluated
 */
tarval *computed_value(ir_node *n)
{
  if (n->op->computed_value)
    return n->op->computed_value(n);
  return tarval_bad;
}

/**
 * set the default computed_value evaluator
 */
static ir_op *firm_set_default_computed_value(ir_op *op)
{
#define CASE(a)                               \
  case iro_##a:                               \
    op->computed_value  = computed_value_##a; \
    break

  switch (op->code) {
  CASE(Const);
  CASE(SymConst);
  CASE(Add);
  CASE(Sub);
  CASE(Minus);
  CASE(Mul);
  CASE(Quot);
  CASE(Div);
  CASE(Mod);
  CASE(Abs);
  CASE(And);
  CASE(Or);
  CASE(Eor);
  CASE(Not);
  CASE(Shl);
  CASE(Shr);
  CASE(Shrs);
  CASE(Rot);
  CASE(Conv);
  CASE(Proj);
  default:
    op->computed_value  = NULL;
  }

  return op;
#undef CASE
}

#if 0
/* returns 1 if the a and b are pointers to different locations. */
static bool
different_identity (ir_node *a, ir_node *b)
{
  assert (mode_is_reference(get_irn_mode (a))
          && mode_is_reference(get_irn_mode (b)));

  if (get_irn_op (a) == op_Proj && get_irn_op(b) == op_Proj) {
    ir_node *a1 = get_Proj_pred (a);
    ir_node *b1 = get_Proj_pred (b);
    if (a1 != b1 && get_irn_op (a1) == op_Alloc
                && get_irn_op (b1) == op_Alloc)
      return 1;
  }
  return 0;
}
#endif

static ir_node *equivalent_node_Block(ir_node *n)
{
  ir_node *oldn = n;

  /* The Block constructor does not call optimize, but mature_immBlock
     calls the optimization. */
  assert(get_Block_matured(n));

  /* Straightening: a single entry Block following a single exit Block
     can be merged, if it is not the Start block. */
  /* !!! Beware, all Phi-nodes of n must have been optimized away.
     This should be true, as the block is matured before optimize is called.
     But what about Phi-cycles with the Phi0/Id that could not be resolved?
     Remaining Phi nodes are just Ids. */
   if ((get_Block_n_cfgpreds(n) == 1) &&
       (get_irn_op(get_Block_cfgpred(n, 0)) == op_Jmp)) {
     ir_node *predblock = get_nodes_block(get_Block_cfgpred(n, 0));
     if (predblock == oldn) {
       /* Jmp jumps into the block it is in -- deal self cycle. */
       n = new_Bad();
       DBG_OPT_DEAD(oldn, n);
     } else if (get_opt_control_flow_straightening()) {
       n = predblock;
       DBG_OPT_STG(oldn, n);
     }
   }
   else if ((get_Block_n_cfgpreds(n) == 1) &&
            (get_irn_op(skip_Proj(get_Block_cfgpred(n, 0))) == op_Cond)) {
     ir_node *predblock = get_nodes_block(get_Block_cfgpred(n, 0));
     if (predblock == oldn) {
       /* Jmp jumps into the block it is in -- deal self cycle. */
       n = new_Bad();
       DBG_OPT_DEAD(oldn, n);
     }
   }
   else if ((get_Block_n_cfgpreds(n) == 2) &&
            (get_opt_control_flow_weak_simplification())) {
    /* Test whether Cond jumps twice to this block
       @@@ we could do this also with two loops finding two preds from several ones. */
    ir_node *a = get_Block_cfgpred(n, 0);
    ir_node *b = get_Block_cfgpred(n, 1);

    if ((get_irn_op(a) == op_Proj) &&
        (get_irn_op(b) == op_Proj) &&
        (get_Proj_pred(a) == get_Proj_pred(b)) &&
        (get_irn_op(get_Proj_pred(a)) == op_Cond) &&
        (get_irn_mode(get_Cond_selector(get_Proj_pred(a))) == mode_b)) {
      /* Also a single entry Block following a single exit Block.  Phis have
         twice the same operand and will be optimized away. */
      n = get_nodes_block(a);
      DBG_OPT_IFSIM(oldn, a, b, n);
    }
  } else if (get_opt_unreachable_code() &&
             (n != current_ir_graph->start_block) &&
             (n != current_ir_graph->end_block)     ) {
    int i;
    /* If all inputs are dead, this block is dead too, except if it is
       the start or end block.  This is a step of unreachable code
       elimination */
    for (i = 0; i < get_Block_n_cfgpreds(n); i++) {
      if (!is_Bad(get_Block_cfgpred(n, i))) break;
    }
    if (i == get_Block_n_cfgpreds(n))
      n = new_Bad();
  }

  return n;
}

/**
 * Returns a equivalent node for a Jmp, a Bad :-)
 * Of course this only happens if the Block of the Jmp is Bad.
 */
static ir_node *equivalent_node_Jmp(ir_node *n)
{
  /* GL: Why not same for op_Raise?? */
  /* unreachable code elimination */
  if (is_Bad(get_nodes_block(n)))
    n = new_Bad();

  return n;
}

static ir_node *equivalent_node_Cond(ir_node *n)
{
  /* We do not evaluate Cond here as we replace it by a new node, a Jmp.
     See cases for iro_Cond and iro_Proj in transform_node. */
  return n;
}

/**
 * Use algebraic simplification a v a = a.
 */
static ir_node *equivalent_node_Or(ir_node *n)
{
  ir_node *oldn = n;

  ir_node *a = get_Or_left(n);
  ir_node *b = get_Or_right(n);

  /* remove a v a */
  if (a == b) {
    n = a;

    DBG_OPT_ALGSIM1(oldn, a, b, n);
  }

  return n;
}

/**
 * optimize operations that are commutative and have neutral 0,
 * so a op 0 = 0 op a = a.
 */
static ir_node *equivalent_node_neutral_zero(ir_node *n)
{
  ir_node *oldn = n;

  ir_node *a = get_binop_left(n);
  ir_node *b = get_binop_right(n);

  tarval *tv;
  ir_node *on;

  /* After running compute_node there is only one constant predecessor.
     Find this predecessors value and remember the other node: */
  if ((tv = value_of(a)) != tarval_bad) {
    on = b;
  } else if ((tv = value_of(b)) != tarval_bad) {
    on = a;
  } else
    return n;

  /* If this predecessors constant value is zero, the operation is
     unnecessary. Remove it: */
  if (classify_tarval (tv) == TV_CLASSIFY_NULL) {
    n = on;

    DBG_OPT_ALGSIM1(oldn, a, b, n);
  }

  return n;
}

#define equivalent_node_Add  equivalent_node_neutral_zero
#define equivalent_node_Eor  equivalent_node_neutral_zero

/**
 * optimize operations that are not commutative but have neutral 0 on left,
 * so a op 0 = a.
 */
static ir_node *equivalent_node_left_zero(ir_node *n)
{
  ir_node *oldn = n;

  ir_node *a = get_binop_left(n);
  ir_node *b = get_binop_right(n);

  if (classify_tarval(value_of(b)) == TV_CLASSIFY_NULL) {
    n = a;

    DBG_OPT_ALGSIM1(oldn, a, b, n);
  }

  return n;
}

#define equivalent_node_Sub   equivalent_node_left_zero
#define equivalent_node_Shl   equivalent_node_left_zero
#define equivalent_node_Shr   equivalent_node_left_zero
#define equivalent_node_Shrs  equivalent_node_left_zero
#define equivalent_node_Rot   equivalent_node_left_zero

/**
 * Er, a "symmetic unop", ie op(op(n)) = n.
 */
static ir_node *equivalent_node_symmetric_unop(ir_node *n)
{
  ir_node *oldn = n;
  ir_node *pred = get_unop_op(n);

  /* optimize symmetric unop */
  if (get_irn_op(pred) == get_irn_op(n)) {
    n = get_unop_op(pred);
    DBG_OPT_ALGSIM2(oldn, pred, n);
  }
  return n;
}

/* NotNot x == x */
#define equivalent_node_Not    equivalent_node_symmetric_unop

/* --x == x */  /* ??? Is this possible or can --x raise an
                       out of bounds exception if min =! max? */
#define equivalent_node_Minus  equivalent_node_symmetric_unop

/**
 * Optimize a * 1 = 1 * a = a.
 */
static ir_node *equivalent_node_Mul(ir_node *n)
{
  ir_node *oldn = n;

  ir_node *a = get_Mul_left(n);
  ir_node *b = get_Mul_right(n);

  /* Mul is commutative and has again an other neutral element. */
  if (classify_tarval(value_of(a)) == TV_CLASSIFY_ONE) {
    n = b;
    DBG_OPT_ALGSIM1(oldn, a, b, n);
  } else if (classify_tarval(value_of(b)) == TV_CLASSIFY_ONE) {
    n = a;
    DBG_OPT_ALGSIM1(oldn, a, b, n);
  }
  return n;
}

/**
 * Optimize a / 1 = a.
 */
static ir_node *equivalent_node_Div(ir_node *n)
{
  ir_node *a = get_Div_left(n);
  ir_node *b = get_Div_right(n);

  /* Div is not commutative. */
  if (classify_tarval(value_of(b)) == TV_CLASSIFY_ONE) { /* div(x, 1) == x */
    /* Turn Div into a tuple (mem, bad, a) */
    ir_node *mem = get_Div_mem(n);
    turn_into_tuple(n, 3);
    set_Tuple_pred(n, pn_Div_M,        mem);
    set_Tuple_pred(n, pn_Div_X_except, new_Bad());        /* no exception */
    set_Tuple_pred(n, pn_Div_res,      a);
  }
  return n;
}

/**
 * Optimize a / 1 = a.
 */
static ir_node *equivalent_node_DivMod(ir_node *n)
{
  ir_node *a = get_DivMod_left(n);
  ir_node *b = get_DivMod_right(n);

  /* Div is not commutative. */
  if (classify_tarval(value_of(b)) == TV_CLASSIFY_ONE) { /* div(x, 1) == x */
    /* Turn DivMod into a tuple (mem, bad, a, 0) */
    ir_node *mem = get_Div_mem(n);
    ir_mode *mode = get_irn_mode(b);

    turn_into_tuple(n, 4);
    set_Tuple_pred(n, pn_DivMod_M,        mem);
    set_Tuple_pred(n, pn_DivMod_X_except, new_Bad());        /* no exception */
    set_Tuple_pred(n, pn_DivMod_res_div,  a);
    set_Tuple_pred(n, pn_DivMod_res_mod,  new_Const(mode, get_mode_null(mode)));
  }
  return n;
}

/**
 * Optimize a & 0b1...1 = 0b1...1 & a =  a & a = a.
 */
static ir_node *equivalent_node_And(ir_node *n)
{
  ir_node *oldn = n;

  ir_node *a = get_And_left(n);
  ir_node *b = get_And_right(n);

  if (a == b) {
    n = a;    /* And has it's own neutral element */
  } else if (classify_tarval(value_of(a)) == TV_CLASSIFY_ALL_ONE) {
    n = b;
    DBG_OPT_ALGSIM1(oldn, a, b, n);
  } else if (classify_tarval(value_of(b)) == TV_CLASSIFY_ALL_ONE) {
    n = a;
    DBG_OPT_ALGSIM1(oldn, a, b, n);
  }
  return n;
}

/**
 * Try to remove useless conv's:
 */
static ir_node *equivalent_node_Conv(ir_node *n)
{
  ir_node *oldn = n;
  ir_node *a = get_Conv_op(n);
  ir_node *b;

  ir_mode *n_mode = get_irn_mode(n);
  ir_mode *a_mode = get_irn_mode(a);

  if (n_mode == a_mode) { /* No Conv necessary */
    n = a;
    DBG_OPT_ALGSIM3(oldn, a, n);
  } else if (get_irn_op(a) == op_Conv) { /* Conv(Conv(b)) */
    ir_mode *b_mode;

    b = get_Conv_op(a);
    n_mode = get_irn_mode(n);
    b_mode = get_irn_mode(b);

    if (n_mode == b_mode) {
      if (n_mode == mode_b) {
        n = b; /* Convb(Conv*(xxxb(...))) == xxxb(...) */
	DBG_OPT_ALGSIM1(oldn, a, b, n);
      }
      else if (mode_is_int(n_mode) || mode_is_character(n_mode)) {
        if (smaller_mode(b_mode, a_mode)){
          n = b;        /* ConvS(ConvL(xxxS(...))) == xxxS(...) */
	  DBG_OPT_ALGSIM1(oldn, a, b, n);
        }
      }
    }
  }
  return n;
}

static ir_node *equivalent_node_Cast(ir_node *n) {
  ir_node *pred = get_Cast_op(n);
  if (get_irn_type(pred) == get_Cast_type(n))
    n = pred;
  return n;
}

static ir_node *equivalent_node_Phi(ir_node *n)
{
  /* Several optimizations:
     - no Phi in start block.
     - remove Id operators that are inputs to Phi
     - fold Phi-nodes, iff they have only one predecessor except
             themselves.
  */
  int i, n_preds;

  ir_node *oldn = n;
  ir_node *block = NULL;     /* to shutup gcc */
  ir_node *first_val = NULL; /* to shutup gcc */
  ir_node *scnd_val = NULL;  /* to shutup gcc */

  if (!get_opt_normalize()) return n;

  n_preds = get_Phi_n_preds(n);

  block = get_nodes_block(n);
  /* @@@ fliegt 'raus, sollte aber doch immer wahr sein!!!
     assert(get_irn_arity(block) == n_preds && "phi in wrong block!"); */
  if ((is_Bad(block)) ||                         /* Control dead */
      (block == current_ir_graph->start_block))  /* There should be no Phi nodes */
    return new_Bad();                            /* in the Start Block. */

  if (n_preds == 0) return n;           /* Phi of dead Region without predecessors. */

#if 0
  /* first we test for a special case: */
  /* Confirm is a special node fixing additional information for a
     value that is known at a certain point.  This is useful for
     dataflow analysis. */
  if (n_preds == 2) {
    ir_node *a = get_Phi_pred(n, 0);
    ir_node *b = get_Phi_pred(n, 1);
    if (   (get_irn_op(a) == op_Confirm)
        && (get_irn_op(b) == op_Confirm)
        && follow_Id (get_irn_n(a, 0) == get_irn_n(b, 0))
        && (get_irn_n(a, 1) == get_irn_n (b, 1))
        && (a->data.num == (~b->data.num & irpn_True) )) {
      return get_irn_n(a, 0);
    }
  }
#endif

  /* If the Block has a Bad pred, we also have one. */
  for (i = 0;  i < n_preds;  ++i)
    if (is_Bad (get_Block_cfgpred(block, i)))
      set_Phi_pred(n, i, new_Bad());

  /* Find first non-self-referencing input */
  for (i = 0;  i < n_preds;  ++i) {
    first_val = get_Phi_pred(n, i);
    if (   (first_val != n)                            /* not self pointer */
#if 1
        && (get_irn_op(first_val) != op_Bad)
#endif
           ) {        /* value not dead */
      break;          /* then found first value. */
    }
  }

  /* A totally Bad or self-referencing Phi (we didn't break the above loop) */
  if (i >= n_preds) { return new_Bad(); }

  scnd_val = NULL;

  /* follow_Id () for rest of inputs, determine if any of these
     are non-self-referencing */
  while (++i < n_preds) {
    scnd_val = get_Phi_pred(n, i);
    if (   (scnd_val != n)
        && (scnd_val != first_val)
#if 1
        && (get_irn_op(scnd_val) != op_Bad)
#endif
           ) {
      break;
    }
  }

  /* Fold, if no multiple distinct non-self-referencing inputs */
  if (i >= n_preds) {
    n = first_val;
    DBG_OPT_PHI(oldn, first_val, n);
  } else {
    /* skip the remaining Ids (done in get_Phi_pred). */
    /* superfluous, since we walk all to propagate Block's Bads.
       while (++i < n_preds) get_Phi_pred(n, i);     */
  }
  return n;
}

/**
 * optimize Proj(Tuple) and gigo for ProjX in Bad block
 */
static ir_node *equivalent_node_Proj(ir_node *n)
{
  ir_node *oldn = n;

  ir_node *a = get_Proj_pred(n);

  if ( get_irn_op(a) == op_Tuple) {
    /* Remove the Tuple/Proj combination. */
    if ( get_Proj_proj(n) <= get_Tuple_n_preds(a) ) {
      n = get_Tuple_pred(a, get_Proj_proj(n));
      DBG_OPT_TUPLE(oldn, a, n);
    } else {
      assert(0); /* This should not happen! */
      n = new_Bad();
    }
  } else if (get_irn_mode(n) == mode_X &&
             is_Bad(get_nodes_block(n))) {
    /* Remove dead control flow -- early gigo. */
    n = new_Bad();
  }
  return n;
}

/**
 * Remove Id's.
 */
static ir_node *equivalent_node_Id(ir_node *n)
{
  ir_node *oldn = n;

  n = follow_Id(n);
  DBG_OPT_ID(oldn, n);
  return n;
}

/**
 * equivalent_node() returns a node equivalent to input n. It skips all nodes that
 * perform no actual computation, as, e.g., the Id nodes.  It does not create
 * new nodes.  It is therefore safe to free n if the node returned is not n.
 * If a node returns a Tuple we can not just skip it.  If the size of the
 * in array fits, we transform n into a tuple (e.g., Div).
 */
ir_node *
equivalent_node(ir_node *n)
{
  if (n->op->equivalent_node)
    return n->op->equivalent_node(n);
  return n;
}

/**
 * set the default equivalent node operation
 */
static ir_op *firm_set_default_equivalent_node(ir_op *op)
{
#define CASE(a)                                 \
  case iro_##a:                                 \
    op->equivalent_node  = equivalent_node_##a; \
    break

  switch (op->code) {
  CASE(Block);
  CASE(Jmp);
  CASE(Cond);
  CASE(Or);
  CASE(Add);
  CASE(Eor);
  CASE(Sub);
  CASE(Shl);
  CASE(Shr);
  CASE(Shrs);
  CASE(Rot);
  CASE(Not);
  CASE(Minus);
  CASE(Mul);
  CASE(Div);
  CASE(DivMod);
  CASE(And);
  CASE(Conv);
  CASE(Cast);
  CASE(Phi);
  CASE(Proj);
  CASE(Id);
  default:
    op->equivalent_node  = NULL;
  }

  return op;
#undef CASE
}

/**
 * Do node specific optimizations of nodes predecessors.
 */
static void
optimize_preds(ir_node *n) {
  ir_node *a = NULL, *b = NULL;

  /* get the operands we will work on for simple cases. */
  if (is_binop(n)) {
    a = get_binop_left(n);
    b = get_binop_right(n);
  } else if (is_unop(n)) {
    a = get_unop_op(n);
  }

  switch (get_irn_opcode(n)) {

  case iro_Cmp:
    /* We don't want Cast as input to Cmp. */
    if (get_irn_op(a) == op_Cast) {
      a = get_Cast_op(a);
      set_Cmp_left(n, a);
    }
    if (get_irn_op(b) == op_Cast) {
      b = get_Cast_op(b);
      set_Cmp_right(n, b);
    }
    break;

  default: break;
  } /* end switch */
}

/** Do architecture dependend optimizations on Mul nodes */
static ir_node *transform_node_Mul(ir_node *n) {
  return arch_dep_replace_mul_with_shifts(n);
}

static ir_node *transform_node_Div(ir_node *n)
{
  tarval *tv = value_of(n);
  ir_node *value = n;

  /* BEWARE: it is NOT possible to optimize a/a to 1, as this may cause a exception */

  if (tv != tarval_bad)
    value = new_Const(get_tarval_mode(tv), tv);
  else /* Try architecture dependand optimization */
    value = arch_dep_replace_div_by_const(n);

  if (value != n) {
    /* Turn Div into a tuple (mem, bad, value) */
    ir_node *mem = get_Div_mem(n);

    turn_into_tuple(n, 3);
    set_Tuple_pred(n, pn_Div_M, mem);
    set_Tuple_pred(n, pn_Div_X_except, new_Bad());
    set_Tuple_pred(n, pn_Div_res, value);
  }
  return n;
}

static ir_node *transform_node_Mod(ir_node *n)
{
  tarval *tv = value_of(n);
  ir_node *value = n;

  /* BEWARE: it is NOT possible to optimize a%a to 0, as this may cause a exception */

  if (tv != tarval_bad)
    value = new_Const(get_tarval_mode(tv), tv);
  else /* Try architecture dependand optimization */
    value = arch_dep_replace_mod_by_const(n);

  if (value != n) {
    /* Turn Mod into a tuple (mem, bad, value) */
    ir_node *mem = get_Mod_mem(n);

    turn_into_tuple(n, 3);
    set_Tuple_pred(n, pn_Mod_M, mem);
    set_Tuple_pred(n, pn_Mod_X_except, new_Bad());
    set_Tuple_pred(n, pn_Mod_res, value);
  }
  return n;
}

static ir_node *transform_node_DivMod(ir_node *n)
{
  int evaluated = 0;

  ir_node *a = get_DivMod_left(n);
  ir_node *b = get_DivMod_right(n);
  ir_mode *mode = get_irn_mode(a);
  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if (!(mode_is_int(mode) && mode_is_int(get_irn_mode(b))))
    return n;

  /* BEWARE: it is NOT possible to optimize a/a to 1, as this may cause a exception */

  if (tb != tarval_bad) {
    if (tb == get_mode_one(get_tarval_mode(tb))) {
      b = new_Const (mode, get_mode_null(mode));
      evaluated = 1;
    } else if (ta != tarval_bad) {
      tarval *resa, *resb;
      resa = tarval_div (ta, tb);
      if (resa == tarval_bad) return n; /* Causes exception!!! Model by replacing through
                                        Jmp for X result!? */
      resb = tarval_mod (ta, tb);
      if (resb == tarval_bad) return n; /* Causes exception! */
      a = new_Const (mode, resa);
      b = new_Const (mode, resb);
      evaluated = 1;
    }
    else { /* Try architecture dependand optimization */
      arch_dep_replace_divmod_by_const(&a, &b, n);
      evaluated = a != NULL;
    }
  } else if (ta == get_mode_null(mode)) {
    /* 0 / non-Const = 0 */
    b = a;
    evaluated = 1;
  }

  if (evaluated) { /* replace by tuple */
    ir_node *mem = get_DivMod_mem(n);
    turn_into_tuple(n, 4);
    set_Tuple_pred(n, pn_DivMod_M,        mem);
    set_Tuple_pred(n, pn_DivMod_X_except, new_Bad());  /* no exception */
    set_Tuple_pred(n, pn_DivMod_res_div,  a);
    set_Tuple_pred(n, pn_DivMod_res_mod,  b);
    assert(get_nodes_block(n));
  }

  return n;
}

static ir_node *transform_node_Cond(ir_node *n)
{
  /* Replace the Cond by a Jmp if it branches on a constant
     condition. */
  ir_node *jmp;
  ir_node *a = get_Cond_selector(n);
  tarval *ta = value_of(a);

  if ((ta != tarval_bad) &&
      (get_irn_mode(a) == mode_b) &&
      (get_opt_unreachable_code())) {
    /* It's a boolean Cond, branching on a boolean constant.
               Replace it by a tuple (Bad, Jmp) or (Jmp, Bad) */
    jmp = new_r_Jmp(current_ir_graph, get_nodes_block(n));
    turn_into_tuple(n, 2);
    if (ta == tarval_b_true) {
      set_Tuple_pred(n, pn_Cond_false, new_Bad());
      set_Tuple_pred(n, pn_Cond_true, jmp);
    } else {
      set_Tuple_pred(n, pn_Cond_false, jmp);
      set_Tuple_pred(n, pn_Cond_true, new_Bad());
    }
    /* We might generate an endless loop, so keep it alive. */
    add_End_keepalive(get_irg_end(current_ir_graph), get_nodes_block(n));
  } else if ((ta != tarval_bad) &&
             (get_irn_mode(a) == mode_Iu) &&
             (get_Cond_kind(n) == dense) &&
             (get_opt_unreachable_code())) {
    /* I don't want to allow Tuples smaller than the biggest Proj.
       Also this tuple might get really big...
       I generate the Jmp here, and remember it in link.  Link is used
       when optimizing Proj. */
    set_irn_link(n, new_r_Jmp(current_ir_graph, get_nodes_block(n)));
    /* We might generate an endless loop, so keep it alive. */
    add_End_keepalive(get_irg_end(current_ir_graph), get_nodes_block(n));
  } else if ((get_irn_op(a) == op_Eor)
             && (get_irn_mode(a) == mode_b)
             && (classify_tarval(value_of(get_Eor_right(a))) == TV_CLASSIFY_ONE)) {
    /* The Eor is a negate.  Generate a new Cond without the negate,
       simulate the negate by exchanging the results. */
    set_irn_link(n, new_r_Cond(current_ir_graph, get_nodes_block(n),
                               get_Eor_left(a)));
  } else if ((get_irn_op(a) == op_Not)
             && (get_irn_mode(a) == mode_b)) {
    /* A Not before the Cond.  Generate a new Cond without the Not,
       simulate the Not by exchanging the results. */
    set_irn_link(n, new_r_Cond(current_ir_graph, get_nodes_block(n),
                               get_Not_op(a)));
  }
  return n;
}

static ir_node *transform_node_Eor(ir_node *n)
{
  ir_node *a = get_Eor_left(n);
  ir_node *b = get_Eor_right(n);

  if ((get_irn_mode(n) == mode_b)
      && (get_irn_op(a) == op_Proj)
      && (get_irn_mode(a) == mode_b)
      && (classify_tarval (value_of(b)) == TV_CLASSIFY_ONE)
      && (get_irn_op(get_Proj_pred(a)) == op_Cmp))
    /* The Eor negates a Cmp. The Cmp has the negated result anyways! */
    n = new_r_Proj(current_ir_graph, get_nodes_block(n), get_Proj_pred(a),
                   mode_b, get_negated_pnc(get_Proj_proj(a)));
  else if ((get_irn_mode(n) == mode_b)
           && (classify_tarval (value_of(b)) == TV_CLASSIFY_ONE))
    /* The Eor is a Not. Replace it by a Not. */
    /*   ????!!!Extend to bitfield 1111111. */
    n = new_r_Not(current_ir_graph, get_nodes_block(n), a, mode_b);

  return n;
}

/**
 * Transform a boolean Not.
 */
static ir_node *transform_node_Not(ir_node *n)
{
  ir_node *a = get_Not_op(n);

  if (   (get_irn_mode(n) == mode_b)
      && (get_irn_op(a) == op_Proj)
      && (get_irn_mode(a) == mode_b)
      && (get_irn_op(get_Proj_pred(a)) == op_Cmp))
    /* We negate a Cmp. The Cmp has the negated result anyways! */
    n = new_r_Proj(current_ir_graph, get_nodes_block(n), get_Proj_pred(a),
                   mode_b, get_negated_pnc(get_Proj_proj(a)));

  return n;
}

static ir_node *transform_node_Cast(ir_node *n) {
  ir_node *pred = get_Cast_op(n);
  type *tp = get_irn_type(pred);
  if (get_irn_op(pred) == op_Const && get_Const_type(pred) != tp) {
    n = new_rd_Const_type(NULL, current_ir_graph, get_nodes_block(pred), get_irn_mode(pred),
			  get_Const_tarval(pred), tp);
  } else if ((get_irn_op(pred) == op_SymConst) && (get_SymConst_value_type(pred) != tp)) {
    n = new_rd_SymConst_type(NULL, current_ir_graph, get_nodes_block(pred), get_SymConst_symbol(pred),
			     get_SymConst_kind(pred), tp);
  }
  return n;
}

/**
 * Transform a Div/Mod/DivMod with a non-zero constant. Must be
 * done here instead of equivalent node because it creates new
 * nodes.
 * Removes the exceptions and routes the memory to the initial mem.
 *
 * Further, it optimizes jump tables by removing all impossible cases.
 */
static ir_node *transform_node_Proj(ir_node *proj)
{
  ir_node *n = get_Proj_pred(proj);
  ir_node *b;
  tarval *tb;
  long proj_nr;

  switch (get_irn_opcode(n)) {
  case iro_Div:
    b  = get_Div_right(n);
    tb = value_of(b);

    if (tb != tarval_bad && classify_tarval(tb) != TV_CLASSIFY_NULL) { /* div(x, c) && c != 0 */
      proj_nr = get_Proj_proj(proj);

      /* this node may float */
      set_irn_pinned(n, op_pin_state_floats);

      if (proj_nr == pn_Div_X_except) {
        /* we found an exception handler, remove it */
        return new_Bad();
      }
      else {
	/* the memory Proj can be removed */
        ir_node *res = get_Div_mem(n);
        set_Div_mem(n, get_irg_initial_mem(current_ir_graph));

	if (proj_nr == pn_Div_M)
          return res;
      }
    }
    break;
  case iro_Mod:
    b  = get_Mod_right(n);
    tb = value_of(b);

    if (tb != tarval_bad && classify_tarval(tb) != TV_CLASSIFY_NULL) { /* mod(x, c) && c != 0 */
      proj_nr = get_Proj_proj(proj);

      /* this node may float */
      set_irn_pinned(n, op_pin_state_floats);

      if (proj_nr == pn_Mod_X_except) {
        /* we found an exception handler, remove it */
        return new_Bad();
      }
      else {
	/* the memory Proj can be removed */
        ir_node *res = get_Mod_mem(n);
        set_Mod_mem(n, get_irg_initial_mem(current_ir_graph));
        if (proj_nr == pn_Mod_M)
	  return res;
      }
    }
    break;
  case iro_DivMod:
    b  = get_DivMod_right(n);
    tb = value_of(b);

    if (tb != tarval_bad && classify_tarval(tb) != TV_CLASSIFY_NULL) { /* DivMod(x, c) && c != 0 */
      proj_nr = get_Proj_proj(proj);

      /* this node may float */
      set_irn_pinned(n, op_pin_state_floats);

      if (proj_nr == pn_DivMod_X_except) {
        /* we found an exception handler, remove it */
        return new_Bad();
      }
      else {
	/* the memory Proj can be removed */
        ir_node *res = get_DivMod_mem(n);
        set_DivMod_mem(n, get_irg_initial_mem(current_ir_graph));
        if (proj_nr == pn_DivMod_M)
	  return res;
      }
    }
    break;

  case iro_Cond:
    if (get_opt_unreachable_code()) {
      b = get_Cond_selector(n);
      tb = value_of(b);

      if (tb != tarval_bad && mode_is_int(get_tarval_mode(tb))) {
        /* we have a constant switch */
        long num = get_Proj_proj(proj);

        if (num != get_Cond_defaultProj(n)) { /* we cannot optimize default Proj's yet */
          if (get_tarval_long(tb) == num) {
            /* Do NOT create a jump here, or we will have 2 control flow ops
             * in a block. This case is optimized away in optimize_cf(). */
            return proj;
          }
          else
            return new_Bad();
        }
      }
    }
    return proj;

  case iro_Tuple:
    /* should not happen, but if it does will be optimized away */
    break;

  default:
    /* do nothing */
    return proj;
  }

  /* we have added a Tuple, optimize it for the current Proj away */
  return equivalent_node_Proj(proj);
}

/**
 * returns the operands of a commutative bin-op, if one operand is
 * a const, it is returned as the second one.
 */
static void get_comm_Binop_Ops(ir_node *binop, ir_node **a, ir_node **c)
{
  ir_node *op_a = get_binop_left(binop);
  ir_node *op_b = get_binop_right(binop);

  assert(is_op_commutative(get_irn_op(binop)));

  if (get_irn_op(op_a) == op_Const) {
    *a = op_b;
    *c = op_a;
  }
  else {
    *a = op_a;
    *c = op_b;
  }
}

/**
 * Optimize a Or(And(Or(And(v,c4),c3),c2),c1) pattern if possible.
 * Such pattern may arise in bitfield stores.
 *
 * value  c4                  value      c4 & c2
 *    AND     c3                    AND           c1 | c3
 *        OR     c2      ===>               OR
 *           AND    c1
 *               OR
 */
static ir_node *transform_node_Or(ir_node *or)
{
  ir_node *and, *c1;
  ir_node *or_l, *c2;
  ir_node *and_l, *c3;
  ir_node *value, *c4;
  ir_node *new_and, *new_const, *block;
  ir_mode *mode = get_irn_mode(or);

  tarval *tv1, *tv2, *tv3, *tv4, *tv, *n_tv4, *n_tv2;

  get_comm_Binop_Ops(or, &and, &c1);
  if ((get_irn_op(c1) != op_Const) || (get_irn_op(and) != op_And))
    return or;

  get_comm_Binop_Ops(and, &or_l, &c2);
  if ((get_irn_op(c2) != op_Const) || (get_irn_op(or_l) != op_Or))
    return or;

  get_comm_Binop_Ops(or_l, &and_l, &c3);
  if ((get_irn_op(c3) != op_Const) || (get_irn_op(and_l) != op_And))
    return or;

  get_comm_Binop_Ops(and_l, &value, &c4);
  if (get_irn_op(c4) != op_Const)
    return or;

  /* ok, found the pattern, check for conditions */
  assert(mode == get_irn_mode(and));
  assert(mode == get_irn_mode(or_l));
  assert(mode == get_irn_mode(and_l));

  tv1 = get_Const_tarval(c1);
  tv2 = get_Const_tarval(c2);
  tv3 = get_Const_tarval(c3);
  tv4 = get_Const_tarval(c4);

  tv = tarval_or(tv4, tv2);
  if (classify_tarval(tv) != TV_CLASSIFY_ALL_ONE) {
    /* have at least one 0 at the same bit position */
    return or;
  }

  n_tv4 = tarval_not(tv4);
  if (tv3 != tarval_and(tv3, n_tv4)) {
    /* bit in the or_mask is outside the and_mask */
    return or;
  }

  n_tv2 = tarval_not(tv2);
  if (tv1 != tarval_and(tv1, n_tv2)) {
    /* bit in the or_mask is outside the and_mask */
    return or;
  }

  /* ok, all conditions met */
  block = get_nodes_block(or);

  new_and = new_r_And(current_ir_graph, block,
      value, new_r_Const(current_ir_graph, block, mode, tarval_and(tv4, tv2)), mode);

  new_const = new_r_Const(current_ir_graph, block, mode, tarval_or(tv3, tv1));

  set_Or_left(or, new_and);
  set_Or_right(or, new_const);

  /* check for more */
  return transform_node_Or(or);
}

/* forward */
static ir_node *transform_node(ir_node *n);

/**
 * Optimize (a >> c1) >> c2), works for Shr, Shrs, Shl
 */
static ir_node * transform_node_shift(ir_node *n)
{
  ir_node *left;
  tarval *tv1, *tv2, *res;
  ir_mode *mode;
  int modulo_shf, flag;

  left = get_binop_left(n);

  /* different operations */
  if (get_irn_op(left) != get_irn_op(n))
    return n;

  tv1 = value_of(get_binop_right(n));
  if (tv1 == tarval_bad)
    return n;

  tv2 = value_of(get_binop_right(left));
  if (tv2 == tarval_bad)
    return n;

  res = tarval_add(tv1, tv2);

  /* beware: a simple replacement works only, if res < modulo shift */
  mode = get_irn_mode(n);

  flag = 0;

  modulo_shf = get_mode_modulo_shift(mode);
  if (modulo_shf > 0) {
    tarval *modulo = new_tarval_from_long(modulo_shf, get_tarval_mode(res));

    if (tarval_cmp(res, modulo) & Lt)
      flag = 1;
  }
  else
    flag = 1;

  if (flag) {
    /* ok, we can replace it */
    ir_node *in[2], *irn, *block = get_nodes_block(n);

    in[0] = get_binop_left(left);
    in[1] = new_r_Const(current_ir_graph, block, get_tarval_mode(res), res);

    irn = new_ir_node(NULL, current_ir_graph, block, get_irn_op(n), mode, 2, in);

    return transform_node(irn);
  }
  return n;
}


/**
 * Tries several [inplace] [optimizing] transformations and returns an
 * equivalent node.  The difference to equivalent_node() is that these
 * transformations _do_ generate new nodes, and thus the old node must
 * not be freed even if the equivalent node isn't the old one.
 */
static ir_node *transform_node(ir_node *n)
{
  if (n->op->transform_node)
    n = n->op->transform_node(n);
  return n;
}

/**
 * set the default transform node operation
 */
static ir_op *firm_set_default_transform_node(ir_op *op)
{
#define CASE(a)                                 \
  case iro_##a:                                 \
    op->transform_node  = transform_node_##a;   \
    break

  switch (op->code) {
  CASE(Mul);
  CASE(Div);
  CASE(Mod);
  CASE(DivMod);
  CASE(Cond);
  CASE(Eor);
  CASE(Not);
  CASE(Cast);
  CASE(Proj);
  CASE(Or);
  case iro_Shr:
  case iro_Shrs:
  case iro_Shl:
    op->transform_node  = transform_node_shift;
    break;
  default:
    op->transform_node  = NULL;
  }

  return op;
#undef CASE
}


/* **************** Common Subexpression Elimination **************** */

/** The size of the hash table used, should estimate the number of nodes
    in a graph. */
#define N_IR_NODES 512

/** Compares the attributes of two Const nodes. */
static int node_cmp_attr_Const(ir_node *a, ir_node *b)
{
  return (get_Const_tarval(a) != get_Const_tarval(b))
      || (get_Const_type(a) != get_Const_type(b));
}

/** Compares the attributes of two Proj nodes. */
static int node_cmp_attr_Proj(ir_node *a, ir_node *b)
{
    return get_irn_proj_attr (a) != get_irn_proj_attr (b);
}

/** Compares the attributes of two Filter nodes. */
static int node_cmp_attr_Filter(ir_node *a, ir_node *b)
{
    return get_Filter_proj(a) != get_Filter_proj(b);
}

/** Compares the attributes of two Alloc nodes. */
static int node_cmp_attr_Alloc(ir_node *a, ir_node *b)
{
    return (get_irn_alloc_attr(a).where != get_irn_alloc_attr(b).where)
        || (get_irn_alloc_attr(a).type != get_irn_alloc_attr(b).type);
}

/** Compares the attributes of two Free nodes. */
static int node_cmp_attr_Free(ir_node *a, ir_node *b)
{
    return (get_irn_free_attr(a) != get_irn_free_attr(b));
}

/** Compares the attributes of two SymConst nodes. */
static int node_cmp_attr_SymConst(ir_node *a, ir_node *b)
{
    return (get_irn_symconst_attr(a).num != get_irn_symconst_attr(b).num)
      || (get_irn_symconst_attr(a).sym.type_p != get_irn_symconst_attr(b).sym.type_p)
      || (get_irn_symconst_attr(a).tp != get_irn_symconst_attr(b).tp);
}

/** Compares the attributes of two Call nodes. */
static int node_cmp_attr_Call(ir_node *a, ir_node *b)
{
    return (get_irn_call_attr(a) != get_irn_call_attr(b));
}

/** Compares the attributes of two FuncCall nodes. */
static int node_cmp_attr_FuncCall(ir_node *a, ir_node *b)
{
    return (get_irn_funccall_attr(a) != get_irn_funccall_attr(b));
}

/** Compares the attributes of two Sel nodes. */
static int node_cmp_attr_Sel(ir_node *a, ir_node *b)
{
    return (get_irn_sel_attr(a).ent->kind  != get_irn_sel_attr(b).ent->kind)
      || (get_irn_sel_attr(a).ent->name    != get_irn_sel_attr(b).ent->name)
      || (get_irn_sel_attr(a).ent->owner   != get_irn_sel_attr(b).ent->owner)
      || (get_irn_sel_attr(a).ent->ld_name != get_irn_sel_attr(b).ent->ld_name)
      || (get_irn_sel_attr(a).ent->type    != get_irn_sel_attr(b).ent->type);
}

/** Compares the attributes of two Phi nodes. */
static int node_cmp_attr_Phi(ir_node *a, ir_node *b)
{
    return get_irn_phi_attr (a) != get_irn_phi_attr (b);
}

/** Compares the attributes of two Cast nodes. */
static int node_cmp_attr_Cast(ir_node *a, ir_node *b)
{
    return get_Cast_type(a) != get_Cast_type(b);
}

/** Compares the attributes of two Load nodes. */
static int node_cmp_attr_Load(ir_node *a, ir_node *b)
{
  if (get_Load_volatility(a) == volatility_is_volatile ||
      get_Load_volatility(b) == volatility_is_volatile)
    /* NEVER do CSE on volatile Loads */
    return 1;

  return get_Load_mode(a) != get_Load_mode(b);
}

/** Compares the attributes of two Store nodes. */
static int node_cmp_attr_Store(ir_node *a, ir_node *b)
{
  /* NEVER do CSE on volatile Stores */
  return (get_Store_volatility(a) == volatility_is_volatile ||
      get_Store_volatility(b) == volatility_is_volatile);
}

/**
 * set the default node attribute compare operation
 */
static ir_op *firm_set_default_node_cmp_attr(ir_op *op)
{
#define CASE(a)                             \
  case iro_##a:                             \
    op->node_cmp_attr  = node_cmp_attr_##a; \
    break

  switch (op->code) {
  CASE(Const);
  CASE(Proj);
  CASE(Filter);
  CASE(Alloc);
  CASE(Free);
  CASE(SymConst);
  CASE(Call);
  CASE(FuncCall);
  CASE(Sel);
  CASE(Phi);
  CASE(Cast);
  CASE(Load);
  CASE(Store);
  default:
    op->node_cmp_attr  = NULL;
  }

  return op;
#undef CASE
}

/**
 * Compare function for two nodes in the hash table. Gets two
 * nodes as parameters.  Returns 0 if the nodes are a cse.
 */
static int
vt_cmp (const void *elt, const void *key)
{
  ir_node *a, *b;
  int i, irn_arity_a;

  a = (void *)elt;
  b = (void *)key;

  if (a == b) return 0;

  if ((get_irn_op(a) != get_irn_op(b)) ||
      (get_irn_mode(a) != get_irn_mode(b))) return 1;

  /* compare if a's in and b's in are of equal length */
  irn_arity_a = get_irn_intra_arity (a);
  if (irn_arity_a != get_irn_intra_arity(b))
    return 1;

  /* for block-local cse and op_pin_state_pinned nodes: */
  if (!get_opt_global_cse() || (get_irn_pinned(a) == op_pin_state_pinned)) {
    if (get_irn_intra_n(a, -1) != get_irn_intra_n(b, -1))
      return 1;
  }

  /* compare a->in[0..ins] with b->in[0..ins] */
  for (i = 0; i < irn_arity_a; i++)
    if (get_irn_intra_n(a, i) != get_irn_intra_n(b, i))
      return 1;

  /*
   * here, we already now that the nodes are identical except their
   * attributes
   */
  if (a->op->node_cmp_attr)
    return a->op->node_cmp_attr(a, b);

  return 0;
}

#define ADDR_TO_VAL(p)  (((unsigned)(p)) >> 3)

/*
 * Calculate a hash value of a node.
 */
unsigned
ir_node_hash (ir_node *node)
{
  unsigned h;
  int i, irn_arity;

  if (node->op == op_Const) {
    /* special value for const, as they only differ in their tarval. */
    h = ADDR_TO_VAL(node->attr.con.tv);
    h = 9*h + ADDR_TO_VAL(get_irn_mode(node));
  } else if (node->op == op_SymConst) {
    /* special value for const, as they only differ in their symbol. */
    h = ADDR_TO_VAL(node->attr.i.sym.type_p);
    h = 9*h + ADDR_TO_VAL(get_irn_mode(node));
  } else {

    /* hash table value = 9*(9*(9*(9*(9*arity+in[0])+in[1])+ ...)+mode)+code */
    h = irn_arity = get_irn_intra_arity(node);

    /* consider all in nodes... except the block if not a control flow. */
    for (i =  is_cfop(node) ? -1 : 0;  i < irn_arity;  i++) {
      h = 9*h + ADDR_TO_VAL(get_irn_intra_n(node, i));
    }

    /* ...mode,... */
    h = 9*h + ADDR_TO_VAL(get_irn_mode(node));
    /* ...and code */
    h = 9*h + ADDR_TO_VAL(get_irn_op(node));
  }

  return h;
}

pset *
new_identities(void) {
  return new_pset(vt_cmp, N_IR_NODES);
}

void
del_identities(pset *value_table) {
  del_pset(value_table);
}

/**
 * Return the canonical node computing the same value as n.
 * Looks up the node in a hash table.
 *
 * For Const nodes this is performed in the constructor, too.  Const
 * nodes are extremely time critical because of their frequent use in
 * constant string arrays.
 */
static INLINE ir_node *
identify (pset *value_table, ir_node *n)
{
  ir_node *o = NULL;

  if (!value_table) return n;

  if (get_opt_reassociation()) {
    if (is_op_commutative(get_irn_op(n))) {
      ir_node *l = get_binop_left(n);
      ir_node *r = get_binop_right(n);

      /* for commutative operators perform  a OP b == b OP a */
      if (l > r) {
        set_binop_left(n, r);
        set_binop_right(n, l);
      }
    }
  }

  o = pset_find (value_table, n, ir_node_hash (n));
  if (!o) return n;

  DBG_OPT_CSE(n, o);

  return o;
}

/**
 * During construction we set the op_pin_state_pinned flag in the graph right when the
 * optimization is performed.  The flag turning on procedure global cse could
 * be changed between two allocations.  This way we are safe.
 */
static INLINE ir_node *
identify_cons (pset *value_table, ir_node *n) {
  ir_node *old = n;

  n = identify(value_table, n);
  if (get_irn_n(old, -1) != get_irn_n(n, -1))
    set_irg_pinned(current_ir_graph, op_pin_state_floats);
  return n;
}

/**
 * Return the canonical node computing the same value as n.
 * Looks up the node in a hash table, enters it in the table
 * if it isn't there yet.
 */
static ir_node *
identify_remember (pset *value_table, ir_node *n)
{
  ir_node *o = NULL;

  if (!value_table) return n;

  if (get_opt_reassociation()) {
    if (is_op_commutative(get_irn_op(n))) {
      ir_node *l = get_binop_left(n);
      ir_node *r = get_binop_right(n);

      /* for commutative operators perform  a OP b == b OP a */
      if (l > r) {
        set_binop_left(n, r);
        set_binop_right(n, l);
      }
    }
  }

  /* lookup or insert in hash table with given hash key. */
  o = pset_insert (value_table, n, ir_node_hash (n));

  if (o != n) {
    DBG_OPT_CSE(n, o);
  }

  return o;
}

void
add_identities (pset *value_table, ir_node *node) {
  if (get_opt_cse() && (get_irn_opcode(node) != iro_Block))
    identify_remember (value_table, node);
}

/**
 * garbage in, garbage out. If a node has a dead input, i.e., the
 * Bad node is input to the node, return the Bad node.
 */
static INLINE ir_node *
gigo (ir_node *node)
{
  int i, irn_arity;
  ir_op* op = get_irn_op(node);

  /* remove garbage blocks by looking at control flow that leaves the block
     and replacing the control flow by Bad. */
  if (get_irn_mode(node) == mode_X) {
    ir_node *block = get_nodes_block(node);
    if (op == op_End) return node;     /* Don't optimize End, may have Bads. */
    if (get_irn_op(block) == op_Block && get_Block_matured(block)) {
      irn_arity = get_irn_arity(block);
      for (i = 0; i < irn_arity; i++) {
        if (!is_Bad(get_irn_n(block, i))) break;
      }
      if (i == irn_arity) return new_Bad();
    }
  }

  /* Blocks, Phis and Tuples may have dead inputs, e.g., if one of the
     blocks predecessors is dead. */
  if ( op != op_Block && op != op_Phi && op != op_Tuple) {
    irn_arity = get_irn_arity(node);
    for (i = -1; i < irn_arity; i++) {
      if (is_Bad(get_irn_n(node, i))) {
        return new_Bad();
      }
    }
  }
#if 0
  /* With this code we violate the agreement that local_optimize
     only leaves Bads in Block, Phi and Tuple nodes. */
  /* If Block has only Bads as predecessors it's garbage. */
  /* If Phi has only Bads as predecessors it's garbage. */
  if ((op == op_Block && get_Block_matured(node)) || op == op_Phi)  {
    irn_arity = get_irn_arity(node);
    for (i = 0; i < irn_arity; i++) {
      if (!is_Bad(get_irn_n(node, i))) break;
    }
    if (i == irn_arity) node = new_Bad();
  }
#endif
  return node;
}


/**
 * These optimizations deallocate nodes from the obstack.
 * It can only be called if it is guaranteed that no other nodes
 * reference this one, i.e., right after construction of a node.
 */
ir_node *
optimize_node (ir_node *n)
{
  tarval *tv;
  ir_node *oldn = n;
  opcode iro = get_irn_opcode(n);

  type *old_tp = get_irn_type(n);
  {
    int i, arity = get_irn_arity(n);
    for (i = 0; i < arity && !old_tp; ++i)
      old_tp = get_irn_type(get_irn_n(n, i));
  }

  /* Allways optimize Phi nodes: part of the construction. */
  if ((!get_opt_optimize()) && (iro != iro_Phi)) return n;

  /* constant expression evaluation / constant folding */
  if (get_opt_constant_folding()) {
    /* constants can not be evaluated */
    if (iro != iro_Const) {
      /* try to evaluate */
      tv = computed_value(n);
      if ((get_irn_mode(n) != mode_T) && (tv != tarval_bad)) {
        /*
         * we MUST copy the node here temporary, because it's still needed
         * for DBG_OPT_ALGSIM0
         */
        int node_size = offsetof(ir_node, attr) +  n->op->attr_size;
        oldn = alloca(node_size);

        memcpy(oldn, n, node_size);
	CLONE_ARR_A(ir_node *, oldn->in, n->in);

	/* ARG, copy the in array, we need it for statistics */
	memcpy(oldn->in, n->in, ARR_LEN(n->in) * sizeof(n->in[0]));

        /* evaluation was successful -- replace the node. */
        obstack_free (current_ir_graph->obst, n);
        n = new_Const (get_tarval_mode (tv), tv);
	if (old_tp && get_type_mode(old_tp) == get_tarval_mode (tv))
	  set_Const_type(n, old_tp);
                                                 DBG_OPT_ALGSIM0(oldn, n);
        return n;
      }
    }
  }

  /* remove unnecessary nodes */
  if (get_opt_constant_folding() ||
      (iro == iro_Phi)  ||   /* always optimize these nodes. */
      (iro == iro_Id)   ||
      (iro == iro_Proj) ||
      (iro == iro_Block)  )  /* Flags tested local. */
    n = equivalent_node (n);

  optimize_preds(n);                  /* do node specific optimizations of nodes predecessors. */

  /** common subexpression elimination **/
  /* Checks whether n is already available. */
  /* The block input is used to distinguish different subexpressions. Right
     now all nodes are op_pin_state_pinned to blocks, i.e., the cse only finds common
     subexpressions within a block. */
  if (get_opt_cse())
    n = identify_cons (current_ir_graph->value_table, n);

  if (n != oldn) {
    /* We found an existing, better node, so we can deallocate the old node. */
    obstack_free (current_ir_graph->obst, oldn);

    return n;
  }

  /* Some more constant expression evaluation that does not allow to
     free the node. */
  iro = get_irn_opcode(n);
  if (get_opt_constant_folding() ||
      (iro == iro_Cond) ||
      (iro == iro_Proj))     /* Flags tested local. */
    n = transform_node (n);

  /* Remove nodes with dead (Bad) input.
     Run always for transformation induced Bads. */
  n = gigo (n);

  /* Now we have a legal, useful node. Enter it in hash table for cse */
  if (get_opt_cse() && (get_irn_opcode(n) != iro_Block)) {
    n = identify_remember (current_ir_graph->value_table, n);
  }

  return n;
}


/**
 * These optimizations never deallocate nodes.  This can cause dead
 * nodes lying on the obstack.  Remove these by a dead node elimination,
 * i.e., a copying garbage collection.
 */
ir_node *
optimize_in_place_2 (ir_node *n)
{
  tarval *tv;
  ir_node *oldn = n;
  opcode iro = get_irn_opcode(n);

  type *old_tp = get_irn_type(n);
  {
    int i, arity = get_irn_arity(n);
    for (i = 0; i < arity && !old_tp; ++i)
      old_tp = get_irn_type(get_irn_n(n, i));
  }

  if (!get_opt_optimize() && (get_irn_op(n) != op_Phi)) return n;

  /* if not optimize return n */
  if (n == NULL) {
    assert(0);
    /* Here this is possible.  Why? */
    return n;
  }

  /* constant expression evaluation / constant folding */
  if (get_opt_constant_folding()) {
    /* constants can not be evaluated */
    if (iro != iro_Const) {
      /* try to evaluate */
      tv = computed_value(n);
      if ((get_irn_mode(n) != mode_T) && (tv != tarval_bad)) {
        /* evaluation was successful -- replace the node. */
        n = new_Const (get_tarval_mode (tv), tv);

	if (old_tp && get_type_mode(old_tp) == get_tarval_mode (tv))
	  set_Const_type(n, old_tp);

        DBG_OPT_ALGSIM0(oldn, n);
        return n;
      }
    }
  }

  /* remove unnecessary nodes */
  if (get_opt_constant_folding() ||
      (iro == iro_Phi)  ||   /* always optimize these nodes. */
      (iro == iro_Id)   ||   /* ... */
      (iro == iro_Proj) ||   /* ... */
      (iro == iro_Block)  )  /* Flags tested local. */
    n = equivalent_node (n);

  optimize_preds(n);                  /* do node specific optimizations of nodes predecessors. */

  /** common subexpression elimination **/
  /* Checks whether n is already available. */
  /* The block input is used to distinguish different subexpressions.  Right
     now all nodes are op_pin_state_pinned to blocks, i.e., the cse only finds common
     subexpressions within a block. */
  if (get_opt_cse()) {
    n = identify (current_ir_graph->value_table, n);
  }

  /* Some more constant expression evaluation. */
  iro = get_irn_opcode(n);
  if (get_opt_constant_folding() ||
      (iro == iro_Cond) ||
      (iro == iro_Proj))     /* Flags tested local. */
    n = transform_node (n);

  /* Remove nodes with dead (Bad) input.
     Run always for transformation induced Bads.  */
  n = gigo (n);

  /* Now we can verify the node, as it has no dead inputs any more. */
  irn_vrfy(n);

  /* Now we have a legal, useful node. Enter it in hash table for cse.
     Blocks should be unique anyways.  (Except the successor of start:
     is cse with the start block!) */
  if (get_opt_cse() && (get_irn_opcode(n) != iro_Block))
    n = identify_remember (current_ir_graph->value_table, n);

  return n;
}

/**
 * Wrapper for external use, set proper status bits after optimization.
 */
ir_node *
optimize_in_place (ir_node *n)
{
  /* Handle graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);

  if (get_opt_global_cse())
    set_irg_pinned(current_ir_graph, op_pin_state_floats);
  if (get_irg_outs_state(current_ir_graph) == outs_consistent)
    set_irg_outs_inconsistent(current_ir_graph);

  /* Maybe we could also test whether optimizing the node can
     change the control graph. */
  if (get_irg_dom_state(current_ir_graph) == dom_consistent)
    set_irg_dom_inconsistent(current_ir_graph);
  return optimize_in_place_2 (n);
}

/**
 * set the default ir op operations
 */
ir_op *firm_set_default_operations(ir_op *op)
{
  op = firm_set_default_computed_value(op);
  op = firm_set_default_equivalent_node(op);
  op = firm_set_default_transform_node(op);
  op = firm_set_default_node_cmp_attr(op);

  return op;
}
