/*
 * Project:     libFIRM
 * File name:   ir/arch/modeconv.c
 * Purpose:     integer mode conversion
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file modeconv.c
 *
 * Contains the mode conversion for architectures that did not support lesser
 * integer modes. Converts all Op(mode_l) into Op(mode) operations by adding
 * conversions were needed. These Conv operations must be implemented in the backend
 * as bit-reducing ops.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

# include "irnode_t.h"
# include "irgraph_t.h"
# include "irmode_t.h"
# include "iropt_t.h"
# include "ircons_t.h"
# include "irgmod.h"
# include "dbginfo.h"
# include "iropt_dbg.h"
# include "irflag_t.h"
# include "irgwalk.h"
# include "modeconv.h"
# include "firmstat.h"

typedef struct _walker_t {
  int bits;
  ir_mode *s_mode;              /**< signed mode */
  ir_mode *u_mode;              /**< unsigned mode */
  int changes;
} walker_t;

/**
 * Add a Conv node where needed on output
 */
static ir_node *fix_irn_output(ir_node *node, ir_mode *mode)
{
  switch (get_irn_opcode(node)) {
    case iro_Proj:
    case iro_Conv:
    case iro_Rot: {
      ir_node *block = get_nodes_block(node);

      if (get_irn_mode(node) != mode)
        return new_r_Conv(current_ir_graph, block, node, mode);
      else
        return node;
    }
    default:
      return node;
  }
}

/**
 * Add a Conv node where needed on input
 */
static ir_node *fix_irn_input(opcode code, ir_node *block, ir_node *pred, ir_mode *mode)
{
  switch (code) {
    case iro_DivMod:
    case iro_Div:
    case iro_Mod:
    case iro_Shr:
    case iro_Shrs:
    case iro_Rot:
    case iro_Call:
    case iro_Return: {
      ir_mode *p_mode = get_irn_mode(pred);

      if (p_mode != mode && mode_is_int(p_mode))
        return new_r_Conv(current_ir_graph, block, pred, mode);
      return pred;
    }
    default:
      return pred;
  }
}

/**
 * fix the mode of the operations if possible
 */
static void fix_mode(ir_node *n, ir_mode *mode)
{
  opcode code = get_irn_opcode(n);

  if (code == iro_Proj) {
    code = get_irn_opcode(get_Proj_pred(n));
  }

  switch (code){
  case iro_Return:
  case iro_Load:
  case iro_Proj:
  case iro_Call:
  case iro_Conv:
    return;
  default:
    set_irn_mode(n, mode);
  }
}

/**
 * walker for the mode conversion
 */
static void do_mode_conv(ir_node *n, void *env)
{
  walker_t *wenv = env;
  ir_mode *mode  = get_irn_mode(n);
  ir_node *block;
  opcode code;

  /* save the old mode, we need this info later */
  set_irn_link(n, mode);

  /* special case: fix the Return */
  if (get_irn_op(n) == op_Return) {
    entity *ent = get_irg_entity(current_ir_graph);
    type *mt = get_entity_type(ent);
    int i, n_ress = get_method_n_ress(mt);

    mode  = mode_is_signed(mode) ? wenv->s_mode : wenv->u_mode;
    block = get_nodes_block(n);
    for (i = 0; i < n_ress; ++i) {
      ir_node *pred = get_irn_n(n, i + 1);
      type *rt = get_method_res_type(mt, i);

      if (is_atomic_type(rt)) {
        mode = get_type_mode(rt);

        if (mode != get_irn_mode(pred)) {
          pred = fix_irn_input(iro_Return, block, pred, mode);

          set_irn_n(n, i + 1, pred);
        }
      }
    }

    return;
  }

  /* convert only integer modes with less than 'bits' bits */
  if (mode_is_int(mode) && get_mode_size_bits(mode) < wenv->bits && get_irn_op(n) != op_Conv) {
    mode  = mode_is_signed(mode) ? wenv->s_mode : wenv->u_mode;

    code = get_irn_opcode(n);

    if (code == iro_Conv) {
      /* formally, the was a convert from modeA to modeB here.
       * So, the expression before the conv must have modeA. */
      ir_node *pred = get_Conv_op(n);
      ir_mode *modeA = get_irn_link(pred);

      if (get_irn_node_nr(n) == 171)
        printf("HAllo\n");

      if (modeA != get_irn_mode(pred)) {
        pred = new_r_Conv(current_ir_graph, get_nodes_block(pred), pred, modeA);
        set_Conv_op(n, pred);
      }
    }
    else if (code == iro_Proj) {
      /* special case for Proj: we must fix it's pred */
      ir_node *oper = get_Proj_pred(n);
      int i, arity = get_irn_arity(oper);

      code = get_irn_opcode(oper);
      block = get_nodes_block(oper);

      for (i = 0; i < arity; ++i) {
        ir_node *pred = get_irn_n(oper, i);

        pred = fix_irn_input(code, block, pred, mode);

        set_irn_n(oper, i, pred);
      }

    }
    else {
      int i, arity = get_irn_arity(n);

      block = get_nodes_block(n);
      for (i = 0; i < arity; ++i) {
        ir_node *pred = get_irn_n(n, i);

        pred = fix_irn_output(pred, mode);
        pred = fix_irn_input(code, block, pred, mode);

        set_irn_n(n, i, pred);
      }
    }

    fix_mode(n, mode);
    wenv->changes = 1;
  }

  if (get_irn_op(n) == op_Conv) {
    /* formally, the was a convert from modeA to modeB here.
     * So, the expression before the conv must have modeA. */
    ir_node *pred = get_Conv_op(n);
    ir_mode *modeA = get_irn_link(pred);

    if (modeA != get_irn_mode(pred)) {
      pred = new_r_Conv(current_ir_graph, get_nodes_block(pred), pred, modeA);
      set_Conv_op(n, pred);
    }

    /* old conv may now get useless */
    if (get_irn_mode(n) == get_irn_mode(pred)) {
      exchange(n, get_Conv_op(n));
      wenv->changes = 1;
    }
  }
}

/*
 * do the integer mode conversion
 */
void arch_mode_conversion(ir_graph *irg, ir_mode *mode)
{
  walker_t env;

  assert(get_irg_phase_state(irg) != phase_building);

  assert(mode_is_int(mode));

  env.s_mode  = mode_is_signed(mode) ? mode: find_signed_mode(mode) ;
  env.u_mode  = mode_is_signed(mode) ? find_unsigned_mode(mode) : mode;
  env.bits    = get_mode_size_bits(mode);
  env.changes = 0;

  assert(env.s_mode && env.u_mode && "Cpould not find modes");

  irg_walk_graph(irg, NULL, do_mode_conv, &env);

  /* Handle graph state */
  if (env.changes) {
    if (get_irg_outs_state(current_ir_graph) == outs_consistent)
      set_irg_outs_inconsistent(current_ir_graph);
  }
}
