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
# include "ircons.h"
# include "irgmod.h"
# include "irvrfy.h"
# include "tv.h"
# include "dbginfo_t.h"
# include "iropt_dbg.h"
# include "irflag_t.h"
# include "firmstat.h"

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
 * Returns the tarval of a Const node or tarval_bad for all other nodes.
 */
static INLINE tarval *
value_of (ir_node *n)
{
  if ((n != NULL) && (get_irn_op(n) == op_Const))
    return get_Const_tarval(n); /* might return tarval_bad */
  else
    return tarval_bad;
}

static tarval *computed_value_Const(ir_node *n)
{
    return get_Const_tarval(n);
}

static tarval *computed_value_SymConst(ir_node *n)
{
  if ((get_SymConst_kind(n) == size) &&
      (get_type_state(get_SymConst_type(n))) == layout_fixed)
    return new_tarval_from_long (get_type_size(get_SymConst_type(n)), mode_Is);
  return tarval_bad;
}

static tarval *computed_value_Add(ir_node *n)
{
  ir_node *a = get_Add_left(n);
  ir_node *b = get_Add_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad)
        && (get_irn_mode(a) == get_irn_mode(b))
        && !(get_mode_sort(get_irn_mode(a)) == irms_reference)) {
    return tarval_add(ta, tb);
  }
  return tarval_bad;
}

static tarval *computed_value_Sub(ir_node *n)
{
  ir_node *a = get_Sub_left(n);
  ir_node *b = get_Sub_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad)
        && (get_irn_mode(a) == get_irn_mode(b))
        && !(get_mode_sort(get_irn_mode(a)) == irms_reference)) {
    return tarval_sub(ta, tb);
  }
  return tarval_bad;
}

static tarval *computed_value_Minus(ir_node *n)
{
  ir_node *a = get_Minus_op(n);
  tarval *ta = value_of(a);

  if ((ta != tarval_bad) && mode_is_signed(get_irn_mode(a)))
    return tarval_neg(ta);

  return tarval_bad;
}

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
    tarval *v;

    if ( ( ((v = ta) != tarval_bad)
             && (v == get_mode_null(get_tarval_mode(v))) )
      || ( ((v = tb) != tarval_bad)
             && (v == get_mode_null(get_tarval_mode(v))) )) {
	return v;
    }
  }
  return tarval_bad;
}

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

static tarval *computed_value_Div(ir_node *n)
{
  ir_node *a = get_Div_left(n);
  ir_node *b = get_Div_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  /* This was missing in original implementation. Why? */
  if ((ta != tarval_bad) && (tb != tarval_bad) && (get_irn_mode(a) == get_irn_mode(b))) {
    if (tb != get_mode_null(get_tarval_mode(tb)))   /* div by zero: return tarval_bad */
      return tarval_div(ta, tb);
  }
  return tarval_bad;
}

static tarval *computed_value_Mod(ir_node *n)
{
  ir_node *a = get_Mod_left(n);
  ir_node *b = get_Mod_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  /* This was missing in original implementation. Why? */
  if ((ta != tarval_bad) && (tb != tarval_bad) && (get_irn_mode(a) == get_irn_mode(b))) {
    if (tb != get_mode_null(get_tarval_mode(tb)))   /* div by zero: return tarval_bad */
      return tarval_mod(ta, tb);
  }
  return tarval_bad;
}

static tarval *computed_value_Abs(ir_node *n)
{
  ir_node *a = get_Abs_op(n);
  tarval *ta = value_of(a);

  if (ta != tarval_bad)
    return tarval_abs(ta);

  return tarval_bad;
}

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

    if (   (tarval_classify ((v = computed_value (a))) == TV_CLASSIFY_NULL)
	|| (tarval_classify ((v = computed_value (b))) == TV_CLASSIFY_NULL)) {
      return v;
    }
  }
  return tarval_bad;
}

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
    if (   (tarval_classify ((v = computed_value (a))) == TV_CLASSIFY_ALL_ONE)
	|| (tarval_classify ((v = computed_value (b))) == TV_CLASSIFY_ALL_ONE)) {
      return v;
    }
  }
  return tarval_bad;
}

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

static tarval *computed_value_Not(ir_node *n)
{
  ir_node *a = get_Not_op(n);
  tarval *ta = value_of(a);

  if (ta != tarval_bad)
    return tarval_not(ta);

  return tarval_bad;
}

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

static tarval *computed_value_Rot(ir_node *n)
{
  ir_node *a = get_Rot_left(n);
  ir_node *b = get_Rot_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad)) {
    /* return tarval_rot (ta, tb); */
  }
  return tarval_bad;
}

static tarval *computed_value_Conv(ir_node *n)
{
  ir_node *a = get_Conv_op(n);
  tarval *ta = value_of(a);

  if (ta != tarval_bad)
    return tarval_convert_to(ta, get_irn_mode(n));

  return tarval_bad;
}

static tarval *computed_value_Proj(ir_node *n)
{
  ir_node *a = get_Proj_pred(n), *b;
  ir_node *aa, *ab;

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
  if (get_irn_op(a) == op_Cmp) {
    aa = get_Cmp_left(a);
    ab = get_Cmp_right(a);

    if (aa == ab) { /* 1.: */
      /* This is a tric with the bits used for encoding the Cmp
	 Proj numbers, the following statement is not the same:
      return new_tarval_from_long ((get_Proj_proj(n) == Eq), mode_b) */
      return new_tarval_from_long ((get_Proj_proj(n) & Eq), mode_b);
    } else {
      tarval *taa = computed_value (aa);
      tarval *tab = computed_value (ab);

      if ((taa != tarval_bad) && (tab != tarval_bad)) { /* 2.: */
	/* strange checks... */
	pnc_number flags = tarval_cmp (taa, tab);
	if (flags != False) {
	  return new_tarval_from_long (get_Proj_proj(n) & flags, mode_b);
	}
      } else {  /* check for 3.: */
	ir_node *aaa = skip_nop(skip_Proj(aa));
	ir_node *aba = skip_nop(skip_Proj(ab));

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
	  return new_tarval_from_long (get_Proj_proj(n) & Ne, mode_b);
      }
    }
  } else if (get_irn_op(a) == op_DivMod) {
    tarval *tb = value_of(b = get_DivMod_right(a));
    tarval *ta = value_of(a = get_DivMod_left(a));

    if ((ta != tarval_bad)  && (tb != tarval_bad) && (get_irn_mode(a) == get_irn_mode(b))) {
      if (tb == get_mode_null(get_tarval_mode(tb)))  /* div by zero: return tarval_bad */
	return tarval_bad;
      if (get_Proj_proj(n)== 0) /* Div */
	return tarval_div(ta, tb);
      else /* Mod */
	return tarval_mod(ta, tb);
    }
  }
  return tarval_bad;
}

/**
 * If the parameter n can be computed, return its value, else tarval_bad.
 * Performs constant folding.
 *
 * GL: Only if n is arithmetic operator?
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
#define CASE(a)					\
  case iro_##a:					\
    op->computed_value  = computed_value_##a;	\
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

  /* The Block constructor does not call optimize, but mature_block
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
     ir_node *predblock = get_nodes_Block(get_Block_cfgpred(n, 0));
     if (predblock == oldn) {
       /* Jmp jumps into the block it is in -- deal self cycle. */
       n = new_Bad();                                      DBG_OPT_DEAD;
     } else if (get_opt_control_flow_straightening()) {
       n = predblock;                                      DBG_OPT_STG;
     }
   }
   else if ((get_Block_n_cfgpreds(n) == 1) &&
	    (get_irn_op(skip_Proj(get_Block_cfgpred(n, 0))) == op_Cond)) {
     ir_node *predblock = get_nodes_Block(get_Block_cfgpred(n, 0));
     if (predblock == oldn) {
       /* Jmp jumps into the block it is in -- deal self cycle. */
       n = new_Bad();                                      DBG_OPT_DEAD;
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
      n = get_nodes_Block(a);                                         DBG_OPT_IFSIM;
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

static ir_node *equivalent_node_Jmp(ir_node *n)
{
  /* GL: Why not same for op_Raise?? */
  /* unreachable code elimination */
  if (is_Bad(get_nodes_Block(n)))
    n = new_Bad();

  return n;
}

static ir_node *equivalent_node_Cond(ir_node *n)
{
  /* We do not evaluate Cond here as we replace it by a new node, a Jmp.
     See cases for iro_Cond and iro_Proj in transform_node. */
  return n;
}

static ir_node *equivalent_node_Or(ir_node *n)
{
  ir_node *oldn = n;

  ir_node *a = get_Or_left(n);
  ir_node *b = get_Or_right(n);

  /* remove a v a */
  if (a == b) {
    n = a;                                                             DBG_OPT_ALGSIM1;
  }

  return n;
}

/**
 * optimize operations that are commutative and have neutral 0.
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
  if ((tv = computed_value (a)) != tarval_bad) {
    on = b;
  } else if ((tv = computed_value (b)) != tarval_bad) {
    on = a;
  } else
    return n;

  /* If this predecessors constant value is zero, the operation is
     unnecessary. Remove it: */
  if (tarval_classify (tv) == TV_CLASSIFY_NULL) {
    n = on;                                                             DBG_OPT_ALGSIM1;
  }

  return n;
}

static ir_node *equivalent_node_Add(ir_node *n)
{
  return equivalent_node_neutral_zero(n);
}

static ir_node *equivalent_node_Eor(ir_node *n)
{
  return equivalent_node_neutral_zero(n);
}

/**
 * optimize operations that are not commutative but have neutral 0 on left.
 * Test only one predecessor.
 */
static ir_node *equivalent_node_left_zero(ir_node *n)
{
  ir_node *oldn = n;

  ir_node *a = get_binop_left(n);
  ir_node *b = get_binop_right(n);

  if (tarval_classify (computed_value (b)) == TV_CLASSIFY_NULL) {
    n = a;                                                              DBG_OPT_ALGSIM1;
  }

  return n;
}

static ir_node *equivalent_node_Sub(ir_node *n)
{
  return equivalent_node_left_zero(n);
}

static ir_node *equivalent_node_Shl(ir_node *n)
{
  return equivalent_node_left_zero(n);
}

static ir_node *equivalent_node_Shr(ir_node *n)
{
  return equivalent_node_left_zero(n);
}

static ir_node *equivalent_node_Shrs(ir_node *n)
{
  return equivalent_node_left_zero(n);
}

static ir_node *equivalent_node_Rot(ir_node *n)
{
  return equivalent_node_left_zero(n);
}

static ir_node *equivalent_node_symmetric_unop(ir_node *n)
{
  ir_node *oldn = n;

  /* optimize symmetric unop */
  if (get_irn_op(get_unop_op(n)) == get_irn_op(n)) {
    n = get_unop_op(get_unop_op(n));                                    DBG_OPT_ALGSIM2;
  }
  return n;
}

static ir_node *equivalent_node_Not(ir_node *n)
{
  /* NotNot x == x */
  return equivalent_node_symmetric_unop(n);
}

static ir_node *equivalent_node_Minus(ir_node *n)
{
  /* --x == x */  /* ??? Is this possible or can --x raise an
			 out of bounds exception if min =! max? */
  return equivalent_node_symmetric_unop(n);
}

static ir_node *equivalent_node_Mul(ir_node *n)
{
  ir_node *oldn = n;

  ir_node *a = get_Mul_left(n);
  ir_node *b = get_Mul_right(n);

  /* Mul is commutative and has again an other neutral element. */
  if (tarval_classify (computed_value (a)) == TV_CLASSIFY_ONE) {
    n = b;                                                              DBG_OPT_ALGSIM1;
  } else if (tarval_classify (computed_value (b)) == TV_CLASSIFY_ONE) {
    n = a;                                                              DBG_OPT_ALGSIM1;
  }
  return n;
}

static ir_node *equivalent_node_Div(ir_node *n)
{
  ir_node *a = get_Div_left(n);
  ir_node *b = get_Div_right(n);

  /* Div is not commutative. */
  if (tarval_classify (computed_value (b)) == TV_CLASSIFY_ONE) { /* div(x, 1) == x */
    /* Turn Div into a tuple (mem, bad, a) */
    ir_node *mem = get_Div_mem(n);
    turn_into_tuple(n, 3);
    set_Tuple_pred(n, pn_Div_M,        mem);
    set_Tuple_pred(n, pn_Div_X_except, new_Bad());	/* no exception */
    set_Tuple_pred(n, pn_Div_res,      a);
  }
  return n;
}

static ir_node *equivalent_node_And(ir_node *n)
{
  ir_node *oldn = n;

  ir_node *a = get_And_left(n);
  ir_node *b = get_And_right(n);

  if (a == b) {
    n = a;    /* And has it's own neutral element */
  } else if (tarval_classify (computed_value (a)) == TV_CLASSIFY_ALL_ONE) {
    n = b;
  } else if (tarval_classify (computed_value (b)) == TV_CLASSIFY_ALL_ONE) {
    n = a;
  }
  if (n != oldn)                                                        DBG_OPT_ALGSIM1;
  return n;
}

static ir_node *equivalent_node_Conv(ir_node *n)
{
  ir_node *oldn = n;
  ir_node *a = get_Conv_op(n);
  ir_node *b;

  ir_mode *n_mode = get_irn_mode(n);
  ir_mode *a_mode = get_irn_mode(a);

  if (n_mode == a_mode) { /* No Conv necessary */
    n = a;                                                              DBG_OPT_ALGSIM3;
  } else if (get_irn_op(a) == op_Conv) { /* Conv(Conv(b)) */
    ir_mode *b_mode;

    b = get_Conv_op(a);
    n_mode = get_irn_mode(n);
    b_mode = get_irn_mode(b);

    if (n_mode == b_mode) {
      if (n_mode == mode_b) {
	n = b;	/* Convb(Conv*(xxxb(...))) == xxxb(...) */        	DBG_OPT_ALGSIM1;
      }
      else if (mode_is_int(n_mode) || mode_is_character(n_mode)) {
	if (smaller_mode(b_mode, a_mode)){
	  n = b;	/* ConvS(ConvL(xxxS(...))) == xxxS(...) */      DBG_OPT_ALGSIM1;
	}
      }
    }
  }
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
    return new_Bad();			     /*	in the Start Block. */

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
    n = first_val;                                     DBG_OPT_PHI;
  } else {
    /* skip the remaining Ids (done in get_Phi_pred). */
    /* superfluous, since we walk all to propagate Block's Bads.
       while (++i < n_preds) get_Phi_pred(n, i);     */
  }
  return n;
}

static ir_node *equivalent_node_Load(ir_node *n)
{
#if 0  /* Is an illegal transformation: different nodes can
	  represent the same pointer value!! */
 ir_node *a = skip_Proj(get_Load_mem(n));
 ir_node *b = get_Load_ptr(n);

 if (get_irn_op(a) == op_Store) {
   if ( different_identity (b, get_Store_ptr(a))) {
	 /* load and store use different pointers, therefore load
		needs not take store's memory but the state before. */
	 set_Load_mem (n, get_Store_mem(a));
   } else if (( 0 /* ???didn't get cryptic test that returns 0 */ )) {
   }
 }
#endif
 return n;
}

static ir_node *equivalent_node_Store(ir_node *n)
{
  ir_node *oldn = n;

  /* remove unnecessary store. */
  ir_node *a = skip_Proj(get_Store_mem(n));
  ir_node *b = get_Store_ptr(n);
  ir_node *c = skip_Proj(get_Store_value(n));

  if (get_irn_op(a) == op_Store
      && get_Store_ptr(a) == b
      && skip_Proj(get_Store_value(a)) == c) {
    /* We have twice exactly the same store -- a write after write. */
    n = a;                                                         DBG_OPT_WAW;
  } else if (get_irn_op(c) == op_Load
	     && (a == c || skip_Proj(get_Load_mem(c)) == a)
	     && get_Load_ptr(c) == b ) {
    /* We just loaded the value from the same memory, i.e., the store
       doesn't change the memory -- a write after read. */
    a = get_Store_mem(n);
    turn_into_tuple(n, 2);
    set_Tuple_pred(n, pn_Store_M,        a);
    set_Tuple_pred(n, pn_Store_X_except, new_Bad());               DBG_OPT_WAR;
  }
  return n;
}

static ir_node *equivalent_node_Proj(ir_node *n)
{
  ir_node *oldn = n;

  ir_node *a = get_Proj_pred(n);

  if ( get_irn_op(a) == op_Tuple) {
    /* Remove the Tuple/Proj combination. */
    if ( get_Proj_proj(n) <= get_Tuple_n_preds(a) ) {
      n = get_Tuple_pred(a, get_Proj_proj(n));                     DBG_OPT_TUPLE;
    } else {
      assert(0); /* This should not happen! */
      n = new_Bad();
    }
  } else if (get_irn_mode(n) == mode_X &&
	     is_Bad(get_nodes_Block(n))) {
    /* Remove dead control flow -- early gigo. */
    n = new_Bad();
  }
  return n;
}

static ir_node *equivalent_node_Id(ir_node *n)
{
  ir_node *oldn = n;

  n = follow_Id (n);                                                 DBG_OPT_ID;
  return n;
}

/*
case iro_Mod, Quot, DivMod
  DivMod allocates new nodes --> it's treated in transform node.
  What about Quot, DivMod?
*/

/**
 * equivalent_node() returns a node equivalent to input n. It skips all nodes that
 * perform no actual computation, as, e.g., the Id nodes.  It does not create
 * new nodes.  It is therefore safe to free n if the node returned is not n.
 * If a node returns a Tuple we can not just skip it.  If the size of the
 * in array fits, we transform n into a tuple (e.g., Div).
 */
ir_node *
equivalent_node (ir_node *n)
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
#define CASE(a)					\
  case iro_##a:					\
    op->equivalent_node  = equivalent_node_##a;	\
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
  CASE(And);
  CASE(Conv);
  CASE(Phi);
  CASE(Load);
  CASE(Store);
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

static ir_node *transform_node_Div(ir_node *n)
{
  tarval *ta = computed_value(n);

  if (ta != tarval_bad) {
    /* Turn Div into a tuple (mem, bad, value) */
    ir_node *mem = get_Div_mem(n);

    turn_into_tuple(n, 3);
    set_Tuple_pred(n, pn_Div_M, mem);
    set_Tuple_pred(n, pn_Div_X_except, new_Bad());
    set_Tuple_pred(n, pn_Div_res, new_Const(get_tarval_mode(ta), ta));
  }
  return n;
}

static ir_node *transform_node_Mod(ir_node *n)
{
  tarval *ta = computed_value(n);

  if (ta != tarval_bad) {
    /* Turn Mod into a tuple (mem, bad, value) */
    ir_node *mem = get_Mod_mem(n);
    turn_into_tuple(n, 3);
    set_Tuple_pred(n, pn_Mod_M, mem);
    set_Tuple_pred(n, pn_Mod_X_except, new_Bad());
    set_Tuple_pred(n, pn_Mod_res, new_Const(get_tarval_mode(ta), ta));
  }
  return n;
}

static ir_node *transform_node_DivMod(ir_node *n)
{
  int evaluated = 0;

  ir_node *a = get_DivMod_left(n);
  ir_node *b = get_DivMod_right(n);
  ir_mode *mode = get_irn_mode(a);

  if (!(mode_is_int(mode) && mode_is_int(get_irn_mode(b))))
    return n;

  if (a == b) {
    a = new_Const(mode, get_mode_one(mode));
    b = new_Const(mode, get_mode_null(mode));
    evaluated = 1;
  } else {
    tarval *ta = value_of(a);
    tarval *tb = value_of(b);

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
    } else if (ta == get_mode_null(mode)) {
      b = a;
      evaluated = 1;
    }
  }
  if (evaluated) { /* replace by tuple */
    ir_node *mem = get_DivMod_mem(n);
    turn_into_tuple(n, 4);
    set_Tuple_pred(n, pn_DivMod_M,        mem);
    set_Tuple_pred(n, pn_DivMod_X_except, new_Bad());  /* no exception */
    set_Tuple_pred(n, pn_DivMod_res_div,  a);
    set_Tuple_pred(n, pn_DivMod_res_mod,  b);
    assert(get_nodes_Block(n));
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
    jmp = new_r_Jmp(current_ir_graph, get_nodes_Block(n));
    turn_into_tuple(n, 2);
    if (ta == tarval_b_true) {
      set_Tuple_pred(n, pn_Cond_false, new_Bad());
      set_Tuple_pred(n, pn_Cond_true, jmp);
    } else {
      set_Tuple_pred(n, pn_Cond_false, jmp);
      set_Tuple_pred(n, pn_Cond_true, new_Bad());
    }
    /* We might generate an endless loop, so keep it alive. */
    add_End_keepalive(get_irg_end(current_ir_graph), get_nodes_Block(n));
  } else if ((ta != tarval_bad) &&
	     (get_irn_mode(a) == mode_Iu) &&
	     (get_Cond_kind(n) == dense) &&
	     (get_opt_unreachable_code())) {
    /* I don't want to allow Tuples smaller than the biggest Proj.
       Also this tuple might get really big...
       I generate the Jmp here, and remember it in link.  Link is used
       when optimizing Proj. */
    set_irn_link(n, new_r_Jmp(current_ir_graph, get_nodes_Block(n)));
    /* We might generate an endless loop, so keep it alive. */
    add_End_keepalive(get_irg_end(current_ir_graph), get_nodes_Block(n));
  } else if ((get_irn_op(a) == op_Eor)
	     && (get_irn_mode(a) == mode_b)
	     && (tarval_classify(computed_value(get_Eor_right(a))) == TV_CLASSIFY_ONE)) {
    /* The Eor is a negate.  Generate a new Cond without the negate,
       simulate the negate by exchanging the results. */
    set_irn_link(n, new_r_Cond(current_ir_graph, get_nodes_Block(n),
			       get_Eor_left(a)));
  } else if ((get_irn_op(a) == op_Not)
	     && (get_irn_mode(a) == mode_b)) {
    /* A Not before the Cond.  Generate a new Cond without the Not,
       simulate the Not by exchanging the results. */
    set_irn_link(n, new_r_Cond(current_ir_graph, get_nodes_Block(n),
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
      && (tarval_classify (computed_value (b)) == TV_CLASSIFY_ONE)
      && (get_irn_op(get_Proj_pred(a)) == op_Cmp))
    /* The Eor negates a Cmp. The Cmp has the negated result anyways! */
    n = new_r_Proj(current_ir_graph, get_nodes_Block(n), get_Proj_pred(a),
		   mode_b, get_negated_pnc(get_Proj_proj(a)));
  else if ((get_irn_mode(n) == mode_b)
	   && (tarval_classify (computed_value (b)) == TV_CLASSIFY_ONE))
    /* The Eor is a Not. Replace it by a Not. */
    /*   ????!!!Extend to bitfield 1111111. */
    n = new_r_Not(current_ir_graph, get_nodes_Block(n), a, mode_b);

  return n;
}

static ir_node *transform_node_Not(ir_node *n)
{
  ir_node *a = get_Not_op(n);

  if (   (get_irn_mode(n) == mode_b)
      && (get_irn_op(a) == op_Proj)
      && (get_irn_mode(a) == mode_b)
      && (get_irn_op(get_Proj_pred(a)) == op_Cmp))
    /* We negate a Cmp. The Cmp has the negated result anyways! */
    n = new_r_Proj(current_ir_graph, get_nodes_Block(n), get_Proj_pred(a),
		   mode_b, get_negated_pnc(get_Proj_proj(a)));

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
#define CASE(a)					\
  case iro_##a:					\
    op->transform_node  = transform_node_##a;	\
    break

  switch (op->code) {
  CASE(Div);
  CASE(Mod);
  CASE(DivMod);
  CASE(Cond);
  CASE(Eor);
  CASE(Not);
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

static int node_cmp_attr_Const(ir_node *a, ir_node *b)
{
  return (get_Const_tarval(a) != get_Const_tarval(b))
      || (get_Const_type(a) != get_Const_type(b));
}

static int node_cmp_attr_Proj(ir_node *a, ir_node *b)
{
    return get_irn_proj_attr (a) != get_irn_proj_attr (b);
}

static int node_cmp_attr_Filter(ir_node *a, ir_node *b)
{
    return get_Filter_proj(a) != get_Filter_proj(b);
}

static int node_cmp_attr_Alloc(ir_node *a, ir_node *b)
{
    return (get_irn_alloc_attr(a).where != get_irn_alloc_attr(b).where)
      || (get_irn_alloc_attr(a).type != get_irn_alloc_attr(b).type);
}

static int node_cmp_attr_Free(ir_node *a, ir_node *b)
{
    return (get_irn_free_attr(a) != get_irn_free_attr(b));
}

static int node_cmp_attr_SymConst(ir_node *a, ir_node *b)
{
    return (get_irn_symconst_attr(a).num != get_irn_symconst_attr(b).num)
      || (get_irn_symconst_attr(a).tori.typ != get_irn_symconst_attr(b).tori.typ);
}

static int node_cmp_attr_Call(ir_node *a, ir_node *b)
{
    return (get_irn_call_attr(a) != get_irn_call_attr(b));
}

static int node_cmp_attr_FuncCall(ir_node *a, ir_node *b)
{
    return (get_irn_funccall_attr(a) != get_irn_funccall_attr(b));
}

static int node_cmp_attr_Sel(ir_node *a, ir_node *b)
{
    return (get_irn_sel_attr(a).ent->kind != get_irn_sel_attr(b).ent->kind)
      || (get_irn_sel_attr(a).ent->name != get_irn_sel_attr(b).ent->name)
      || (get_irn_sel_attr(a).ent->owner != get_irn_sel_attr(b).ent->owner)
      || (get_irn_sel_attr(a).ent->ld_name != get_irn_sel_attr(b).ent->ld_name)
      || (get_irn_sel_attr(a).ent->type != get_irn_sel_attr(b).ent->type);
}

static int node_cmp_attr_Phi(ir_node *a, ir_node *b)
{
    return get_irn_phi_attr (a) != get_irn_phi_attr (b);
}

static int node_cmp_attr_Cast(ir_node *a, ir_node *b)
{
    return get_Cast_type(a) != get_Cast_type(b);
}

/**
 * set the default node attribute compare operation
 */
static ir_op *firm_set_default_node_cmp_attr(ir_op *op)
{
#define CASE(a)					\
  case iro_##a:					\
    op->node_cmp_attr  = node_cmp_attr_##a;	\
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

  /* compare if a's in and b's in are equal */
  irn_arity_a = get_irn_arity (a);
  if (irn_arity_a != get_irn_arity(b))
    return 1;

  /* for block-local cse and pinned nodes: */
  if (!get_opt_global_cse() || (get_op_pinned(get_irn_op(a)) == pinned)) {
    if (get_irn_n(a, -1) != get_irn_n(b, -1))
      return 1;
  }

  /* compare a->in[0..ins] with b->in[0..ins] */
  for (i = 0; i < irn_arity_a; i++)
    if (get_irn_n(a, i) != get_irn_n(b, i))
      return 1;

  /*
   * here, we already now that the nodes are identical except their
   * attributes
   */
  if (a->op->node_cmp_attr)
    return a->op->node_cmp_attr(a, b);

  return 0;
}

/**
 * Calculate a hash value of a node.
 */
static unsigned
ir_node_hash (ir_node *node)
{
  unsigned h;
  int i, irn_arity;

  /* hash table value = 9*(9*(9*(9*(9*arity+in[0])+in[1])+ ...)+mode)+code */
  h = irn_arity = get_irn_arity(node);

  /* consider all in nodes... except the block. */
  for (i = 0;  i < irn_arity;  i++) {
    h = 9*h + (unsigned long)get_irn_n(node, i);
  }

  /* ...mode,... */
  h = 9*h + (unsigned long) get_irn_mode (node);
  /* ...and code */
  h = 9*h + (unsigned long) get_irn_op (node);

  return h;
}

pset *
new_identities (void)
{
  return new_pset (vt_cmp, N_IR_NODES);
}

void
del_identities (pset *value_table)
{
  del_pset (value_table);
}

/**
 * Return the canonical node computing the same value as n.
 * Looks up the node in a hash table.
 */
static INLINE ir_node *
identify (pset *value_table, ir_node *n)
{
  ir_node *o = NULL;

  if (!value_table) return n;

  /* TODO: use a generic commutative attribute */
  if (get_opt_reassociation()) {
    if (is_op_commutative(get_irn_op(n))) {
      /* for commutative operators perform  a OP b == b OP a */
      if (get_binop_left(n) > get_binop_right(n)) {
	ir_node *h = get_binop_left(n);
	set_binop_left(n, get_binop_right(n));
	set_binop_right(n, h);
      }
    }
  }

  o = pset_find (value_table, n, ir_node_hash (n));
  if (!o) return n;

  return o;
}

/**
 * During construction we set the pinned flag in the graph right when the
 * optimizatin is performed.  The flag turning on procedure global cse could
 * be changed between two allocations.  This way we are safe.
 */
static INLINE ir_node *
identify_cons (pset *value_table, ir_node *n) {
  ir_node *old = n;
  n = identify(value_table, n);
  if (get_irn_n(old, -1) != get_irn_n(n, -1))
    set_irg_pinned(current_ir_graph, floats);
  return n;
}

/**
 * Return the canonical node computing the same value as n.
 * Looks up the node in a hash table, enters it in the table
 * if it isn't there yet.
 */
static ir_node *
identify_remember (pset *value_table, ir_node *node)
{
  ir_node *o = NULL;

  if (!value_table) return node;

  /* lookup or insert in hash table with given hash key. */
  o = pset_insert (value_table, node, ir_node_hash (node));

  if (o == node) return node;

  return o;
}

void
add_identities (pset *value_table, ir_node *node) {
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

  /* Allways optimize Phi nodes: part of the construction. */
  if ((!get_opt_optimize()) && (iro != iro_Phi)) return n;

  /* constant expression evaluation / constant folding */
  if (get_opt_constant_folding()) {
    /* constants can not be evaluated */
    if (iro != iro_Const) {
      /* try to evaluate */
      tv = computed_value (n);
      if ((get_irn_mode(n) != mode_T) && (tv != tarval_bad)) {
	/*
	 * we MUST copy the node here temparary, because it's still needed
	 * for DBG_OPT_ALGSIM0
	 */
	ir_node x = *n;
	oldn = &x;
        /* evaluation was successful -- replace the node. */
	obstack_free (current_ir_graph->obst, n);
	n = new_Const (get_tarval_mode (tv), tv);
							DBG_OPT_ALGSIM0;
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
     now all nodes are pinned to blocks, i.e., the cse only finds common
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
      tv = computed_value (n);
      if ((get_irn_mode(n) != mode_T) && (tv != tarval_bad)) {
        /* evaluation was successful -- replace the node. */
	n = new_Const (get_tarval_mode (tv), tv);
						DBG_OPT_ALGSIM0;
	return n;
      }
    }
  }

  /* remove unnecessary nodes */
  /*if (get_opt_constant_folding()) */
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
     now all nodes are pinned to blocks, i.e., the cse only finds common
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
    set_irg_pinned(current_ir_graph, floats);
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
