/*
 * Project:     libFIRM
 * File name:   ir/arch/archop.c
 * Purpose:     Architecture dependand IR operations
 * Author:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "firm_common_t.h"
#include "irvrfy.h"
#include "iropt_dbg.h"
#include "archop.h"

/* when we need verifying */
#ifdef NDEBUG
# define IRN_VRFY_IRG(res, irg)
#else
# define IRN_VRFY_IRG(res, irg)  irn_vrfy_irg(res, irg)
#endif

/** current settings */
static arch_ops_info settings;

/** default settings */
static const arch_ops_info default_settings = {
  ARCH_OPS_NONE,
  0
};

/** The Min operation */
ir_op *op_Min = NULL;

/** The Max operation */
ir_op *op_Max = NULL;

ir_op *get_op_Min(void)  { return op_Min; }
ir_op *get_op_Max(void)  { return op_Max; }

/*
 * construct a Min: Min(a,b) = a < b ? a : b
 */
ir_node *
new_rd_Min(dbg_info *db, ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  if (! op_Min) {
    assert(0);
    return NULL;
  }

  in[0] = op1;
  in[1] = op2;
  res = new_ir_node(db, irg, block, op_Min, mode, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

/*
 * construct a Max: Max(a,b) = a > b ? a : b
 */
ir_node *
new_rd_Max(dbg_info *db, ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  if (! op_Max) {
    assert(0);
    return NULL;
  }

  in[0] = op1;
  in[1] = op2;
  res = new_ir_node(db, irg, block, op_Max, mode, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

ir_node *
new_r_Min(ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_Min(NULL, irg, block, op1, op2, mode);
}

ir_node *
new_r_Max(ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_Max(NULL, irg, block, op1, op2, mode);
}

ir_node *
new_d_Min(dbg_info *db, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_Min(db, current_ir_graph, current_ir_graph->current_block,
               op1, op2, mode);
}

ir_node *
new_d_Max(dbg_info *db, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_Max(db, current_ir_graph, current_ir_graph->current_block,
               op1, op2, mode);
}

ir_node *
new_Min(ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_d_Min(NULL, op1, op2, mode);
}

ir_node *
new_Max(ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_d_Max(NULL, op1, op2, mode);
}

/* optimizations */

/**
 * return the value of a Min
 */
static tarval *computed_value_Min(ir_node *n)
{
  ir_node *a = get_binop_left(n);
  ir_node *b = get_binop_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad) && (get_irn_mode(a) == get_irn_mode(b))) {
    pn_Cmp res = tarval_cmp(ta, tb);

    /* beware: there might be Unordered tarvals here, in that
     * case let the backend decide, do NOT optimize */
    if (res == pn_Cmp_Lt)
      return ta;
    if (res == pn_Cmp_Gt || res == pn_Cmp_Eq)
      return tb;
  }
  return tarval_bad;
}

/**
 * return the value of a Max
 */
static tarval *computed_value_Max(ir_node *n)
{
  ir_node *a = get_binop_left(n);
  ir_node *b = get_binop_right(n);

  tarval *ta = value_of(a);
  tarval *tb = value_of(b);

  if ((ta != tarval_bad) && (tb != tarval_bad) && (get_irn_mode(a) == get_irn_mode(b))) {
    pn_Cmp res = tarval_cmp(ta, tb);

    /* beware: there might be Unordered tarvals here, in that
     * case let the backend decide, do NOT optimize */
    if (res == pn_Cmp_Gt)
      return ta;
    if (res == pn_Cmp_Lt || res == pn_Cmp_Eq)
      return tb;
  }
  return tarval_bad;
}

/**
 * Returns an equivalent node for a Min/Max node.
 * We do not allow Exeptions in our Min/Max, so there will be always
 * an result.
 * The problem is Min(NaN, NaN) == NaN ???.
 */
static ir_node *equivalent_node_MinMax(ir_node *n)
{
  ir_node *a, *b;

  if (settings.minmax_handle_NaN == 0 && mode_is_float(get_irn_mode(n)))
    return n;

  a = get_binop_left(n);
  b = get_binop_right(n);

  if (a == b) {
    DBG_OPT_ALGSIM0(n, a);
    return a;
  }

  return n;
}

#define equivalent_node_Min equivalent_node_MinMax
#define equivalent_node_Max equivalent_node_MinMax

/*
 * Create Min and Mux from Mux nodes
 */
ir_node *arch_transform_node_Mux(ir_node *n)
{
  if (settings.enabled_ops & ARCH_OPS_MINMAX) {
    ir_node *oldn = n, *cmp, *proj = get_Mux_sel(n);
    long proj_nr;

    if (get_irn_op(proj) != op_Proj)
      return n;

    cmp = get_Proj_pred(proj);
    if (get_irn_op(cmp) == op_Cmp) {
      ir_node *a = get_Cmp_left(cmp);
      ir_node *b = get_Cmp_right(cmp);
      ir_node *t = get_Mux_true(n);
      ir_node *f = get_Mux_false(n);

      proj_nr = get_Proj_proj(proj);

      if (proj_nr == pn_Cmp_Lt || proj_nr == pn_Cmp_Le) {
        if (a == t && b == f) {
          /* a </<= b ? a : b  ==>  Min(a,b) */
          n = new_rd_Min(get_irn_dbg_info(n),
                current_ir_graph,
                get_nodes_block(n),
                a, b,
                get_irn_mode(n));

          DBG_OPT_ALGSIM1(oldn, cmp, proj, n);
          return n;
        }
        else if (a == f && b == t) {
          /* a </<= b ? b : a  ==>  Max(a,b) */
          n = new_rd_Max(get_irn_dbg_info(n),
                current_ir_graph,
                get_nodes_block(n),
                a, b,
                get_irn_mode(n));

          DBG_OPT_ALGSIM1(oldn, cmp, proj, n);
          return n;
        }
      }
      else if (proj_nr == pn_Cmp_Gt || proj_nr == pn_Cmp_Ge) {
        if (a == t && b == f) {
          /* a >/>= b ? a : b  ==>  Max(a,b) */
          n = new_rd_Max(get_irn_dbg_info(n),
                current_ir_graph,
                get_nodes_block(n),
                a, b,
                get_irn_mode(n));

          DBG_OPT_ALGSIM1(oldn, cmp, proj, n);
          return n;
        }
        else if (a == f && b == t) {
          /* a >/>= b ? b : a  ==>  Min(a,b) */
          n = new_rd_Min(get_irn_dbg_info(n),
                current_ir_graph,
                get_nodes_block(n),
                a, b,
                get_irn_mode(n));

          DBG_OPT_ALGSIM1(oldn, cmp, proj, n);
          return n;
        }
      }
    }
  }
  return n;
}

/*
 * initialize the ops.
 */
void firm_archops_init(const arch_ops_info *info)
{
  if (! info)
    info = &default_settings;

  memcpy(&settings, info, sizeof(settings));

  if (info->enabled_ops & ARCH_OPS_MINMAX) {
    op_Min = new_ir_op(get_next_ir_opcode(), "Min",  op_pin_state_floats, irop_flag_commutative, oparity_binary, 0, 0);
    op_Min->computed_value  = computed_value_Min;
    op_Min->equivalent_node = equivalent_node_Min;

    op_Max = new_ir_op(get_next_ir_opcode(), "Max",  op_pin_state_floats, irop_flag_commutative, oparity_binary, 0, 0);
    op_Max->computed_value  = computed_value_Max;
    op_Max->equivalent_node = equivalent_node_Max;
  }
}
