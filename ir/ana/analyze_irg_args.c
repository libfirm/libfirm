/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief      read/write analyze of graph argument, which have mode reference.
 * @author     Beyhan Veliev
 * @version    $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "irouts.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "array.h"
#include "irprog.h"
#include "entity_t.h"
#include "xmalloc.h"

#include "analyze_irg_args.h"

static char v;
static void *VISITED = &v;

/**
 * Walk recursive the successors of a graph argument
 * with mode reference and mark if it will be read,
 * written or stored.
 *
 * @param arg   The graph argument with mode reference,
 *             that must be checked.
 */
static unsigned analyze_arg(ir_node *arg, unsigned bits)
{
  int i, p;
  ir_node *succ;

  /* We must visit a node once to avoid endless recursion.*/
  set_irn_link(arg, VISITED);

  for (i = get_irn_n_outs(arg) - 1; i >= 0; --i) {
    succ = get_irn_out(arg, i);

    /* We was here.*/
    if (get_irn_link(succ) == VISITED)
      continue;

    /* We should not walk over the memory edge.*/
    if (get_irn_mode(succ) == mode_M)
      continue;

    /* If we reach with the recursion a Call node and our reference
       isn't the address of this Call we accept that the reference will
       be read and written if the graph of the method represented by
       "Call" isn't computed else we analyze that graph. If our
       reference is the address of this
       Call node that mean the reference will be read.*/
    switch (get_irn_opcode(succ)) {

    case iro_Call: {
      ir_node *ptr  = get_Call_ptr(succ);

      if (ptr == arg) {
        /* Hmm: not sure what this is, most likely a read */
        bits |= ptr_access_read;
      }
      else {
        ir_op *op = get_irn_op(ptr);
        ir_entity *meth_ent;

        if (op == op_SymConst && get_SymConst_kind(ptr) == symconst_addr_ent) {
          meth_ent = get_SymConst_entity(ptr);

          for (p = get_Call_n_params(succ) - 1; p >= 0; --p) {
            if (get_Call_param(succ, p) == arg) {
              /* an arg can be used more than once ! */
              bits |= get_method_param_access(meth_ent, p);
            }
          }
        }
        else if (op == op_Sel && get_irp_callee_info_state() == irg_callee_info_consistent) {
          /* is be a polymorphic call but callee information is available */
          int i, n_params = get_Call_n_params(succ);

          /* simply look into ALL possible callees */
          for (i = get_Call_n_callees(succ) - 1; i >= 0; --i) {
            meth_ent = get_Call_callee(succ, i);

            /* unknown_entity is used to signal that we don't know what is called */
            if (meth_ent == unknown_entity) {
              bits |= ptr_access_all;
              break;
            }

            for (p = n_params - 1; p >= 0; --p) {
              if (get_Call_param(succ, p) == arg) {
                /* an arg can be used more than once ! */
                bits |= get_method_param_access(meth_ent, p);
              }
            }
          }
        }
        else /* can do anything */
          bits |= ptr_access_all;
      }

      /* search stops here anyway */
      continue;
    }
    case iro_Store:
      /* We have reached a Store node => the reference is written or stored. */
      if (get_Store_ptr(succ) == arg) {
        /* written to */
        bits |= ptr_access_write;
      }
      else {
        /* stored itself */
        bits |= ptr_access_store;
      }

      /* search stops here anyway */
      continue;

    case iro_Load:
      /* We have reached a Load node => the reference is read. */
      bits |= ptr_access_read;

      /* search stops here anyway */
      continue;

    case iro_Conv:
      /* our address is casted into something unknown. Break our search. */
      bits = ptr_access_all;
      break;

    default:
      break;
    }

    /* If we know that, the argument will be read, write and stored, we
       can break the recursion.*/
    if (bits == ptr_access_all) {
      bits = ptr_access_all;
      break;
    }

    /*
     * A calculation that do not lead to a reference mode ends our search.
     * This is dangerous: It would allow to cast into integer and that cast back ...
     * so, when we detect a Conv we go mad, see the Conv case above.
     */
    if (!mode_is_reference(get_irn_mode(succ)))
      continue;

    /* follow further the address calculation */
    bits = analyze_arg(succ, bits);
  }
  set_irn_link(arg, NULL);
  return bits;
}

/**
 * Check if a argument of the ir graph with mode
 * reference is read, write or both.
 *
 * @param irg   The ir graph to analyze.
 */
static void analyze_ent_args(ir_entity *ent)
{
  ir_graph *irg;
  ir_node *irg_args, *arg;
  ir_mode *arg_mode;
  int nparams, i;
  long proj_nr;
  ir_type *mtp;
  ptr_access_kind *rw_info;

  mtp     = get_entity_type(ent);
  nparams = get_method_n_params(mtp);

  ent->attr.mtd_attr.param_access = NEW_ARR_F(ptr_access_kind, nparams);

  /* If the method haven't parameters we have
   * nothing to do.
   */
  if (nparams <= 0)
    return;

  irg = get_entity_irg(ent);

  /* we have not yet analyzed the graph, set ALL access for pointer args */
  for (i = nparams - 1; i >= 0; --i)
    ent->attr.mtd_attr.param_access[i] =
      is_Pointer_type(get_method_param_type(mtp, i)) ? ptr_access_all : ptr_access_none;

  if (! irg) {
    /* no graph, no better info */
    return;
  }

  assure_irg_outs(irg);

  irg_args = get_irg_args(irg);

  /* A array to save the information for each argument with
     mode reference.*/
  NEW_ARR_A(ptr_access_kind, rw_info, nparams);

  /* We initialize the element with none state. */
  for (i = nparams - 1; i >= 0; --i)
    rw_info[i] = ptr_access_none;

  /* search for arguments with mode reference
     to analyze them.*/
  for (i = get_irn_n_outs(irg_args) - 1; i >= 0; --i) {
    arg      = get_irn_out(irg_args, i);
    arg_mode = get_irn_mode(arg);
    proj_nr  = get_Proj_proj(arg);

    if (mode_is_reference(arg_mode))
      rw_info[proj_nr] |= analyze_arg(arg, rw_info[proj_nr]);
  }

  /* copy the temporary info */
  memcpy(ent->attr.mtd_attr.param_access, rw_info,
    nparams * sizeof(ent->attr.mtd_attr.param_access[0]));

#if 0
  printf("\n%s:\n", get_entity_name(ent));
  for (i = 0; i < nparams; ++i) {
    if (is_Pointer_type(get_method_param_type(mtp, i)))
      if (ent->attr.mtd_attr.param_access[i] != ptr_access_none) {
        printf("  Pointer Arg %d access: ", i);
        if (ent->attr.mtd_attr.param_access[i] & ptr_access_read)
          printf("READ ");
        if (ent->attr.mtd_attr.param_access[i] & ptr_access_write)
          printf("WRITE ");
        if (ent->attr.mtd_attr.param_access[i] & ptr_access_store)
          printf("STORE ");
        printf("\n");
      }
  }
#endif
}

/**
 * Analyze how pointer arguments of a given
 * ir graph are accessed.
 *
 * @param irg   The ir graph to analyze.
 */
void analyze_irg_args(ir_graph *irg)
{
  ir_entity *ent;

  if (irg == get_const_code_irg())
    return;

  ent = get_irg_entity(irg);
  if (! ent)
    return;

  if (! ent->attr.mtd_attr.param_access)
    analyze_ent_args(ent);
}

/*
 * Compute for a method with pointer parameter(s)
 * if they will be read or written.
 */
ptr_access_kind get_method_param_access(ir_entity *ent, int pos)
{
  ir_type *mtp = get_entity_type(ent);
#ifndef NDEBUG
  int is_variadic = get_method_variadicity(mtp) == variadicity_variadic;

  assert(0 <= pos && (is_variadic || pos < get_method_n_params(mtp)));
#endif

  if (ent->attr.mtd_attr.param_access) {
    if (pos < ARR_LEN(ent->attr.mtd_attr.param_access))
      return ent->attr.mtd_attr.param_access[pos];
    else
      return ptr_access_all;
  }

  analyze_ent_args(ent);

  if (pos < ARR_LEN(ent->attr.mtd_attr.param_access))
    return ent->attr.mtd_attr.param_access[pos];
  else
    return ptr_access_all;
}

enum args_weight {
  null_weight        = 0,  /**< If can't be anything optimized. */
  binop_weight       = 1,  /**< If the argument have mode_weight and take part in binop. */
  const_binop_weight = 1,  /**< If the argument have mode_weight and take part in binop with a constant.*/
  cmp_weight         = 4,  /**< If the argument take part in cmp. */
  const_cmp_weight   = 10  /**< If the argument take part in cmp with a constant. */
};

/**
 * Compute the weight of a method parameter
 *
 * @param arg  The parameter them weight muss be computed.
 */
static float calc_method_param_weight(ir_node *arg)
{
  int i;
  ir_node *succ, *op;
  float weight = null_weight;

  /* We mark the nodes to avoid endless recursion */
  set_irn_link(arg, VISITED);

  for (i = get_irn_n_outs(arg) - 1; i >= 0; i--) {
    succ = get_irn_out(arg, i);

    /* We was here.*/
    if (get_irn_link(succ) == VISITED)
      continue;

    /* We should not walk over the memory edge.*/
    if (get_irn_mode(succ) == mode_M)
      continue;

    /* We have reached a cmp and we must increase the
       weight with the cmp_weight.*/
    if (get_irn_op(succ) == op_Cmp) {

      if (get_Cmp_left(succ) == arg)
        op = get_Cmp_right(succ);
      else
        op = get_Cmp_left(succ);

      if (is_irn_constlike(op)) {
        weight += const_cmp_weight;
      }
      else
        weight += cmp_weight;
    }
    else if (is_binop(succ)) {
      /* We have reached a binop and we must increase the
	       weight with the binop_weight. If the other operand of the
	       binop is a constant we increase the weight with const_binop_weight
	       and call the function recursive.
      */
      if (get_binop_left(succ) == arg)
	      op = get_binop_right(succ);
      else
	      op = get_binop_left(succ);

      if (is_irn_constlike(op)) {
	      weight += const_binop_weight;
	      weight += calc_method_param_weight(succ);
      }
      else
      	weight += binop_weight;
    } else if (is_unop(succ)) {
      /* We have reached a binop and we must increase the
	       weight with the const_binop_weight and call the function recursive.*/
      weight += const_binop_weight;
      weight += calc_method_param_weight(succ);
    }
  }
  set_irn_link(arg, NULL);
  return weight;
}

/**
 * Set a weight for each argument of a ir_graph.
 * The args with a greater weight are good for optimize.
 *
 * @param ent  The entity of the ir_graph.
 */
static void analyze_method_params_weight(ir_entity *ent)
{
  ir_type *mtp;
  ir_graph *irg;
  int nparams, i, proj_nr;
  ir_node *irg_args, *arg;

  mtp      = get_entity_type(ent);
  nparams  = get_method_n_params(mtp);

  /* allocate a new array. currently used as 'analysed' flag */
  ent->attr.mtd_attr.param_weight = NEW_ARR_F(float, nparams);

  /* If the method haven't parameters we have
   * nothing to do.
   */
  if (nparams <= 0)
    return;

  irg = get_entity_irg(ent);

  /* First we initialize the parameter weight with 0. */
  for (i = nparams - 1; i >= 0; i--)
    ent->attr.mtd_attr.param_weight[i] = null_weight;

  if (! irg) {
    /* no graph, no better info */
    return;
  }

  /* Call algorithm that computes the out edges */
  assure_irg_outs(irg);

  irg_args = get_irg_args(irg);

  for (i = get_irn_n_outs(irg_args) - 1; i >= 0; --i) {
    arg     = get_irn_out(irg_args, i);
    proj_nr = get_Proj_proj(arg);
    ent->attr.mtd_attr.param_weight[proj_nr]  += calc_method_param_weight(arg);
  }

#if 0
  printf("\n%s:\n", get_entity_name(ent));
  for (i = nparams - 1; i >= 0; --i)
    printf("The weight of argument %i is %f \n", i, ent->param_weight[i]);
#endif
}

/*
 * Compute for a method with pointer parameter(s)
 * if they will be read or written.
 */
float get_method_param_weight(ir_entity *ent, int pos)
{
  ir_type *mtp = get_entity_type(ent);

#ifndef NDEBUG
  int     is_variadic = get_method_variadicity(mtp) == variadicity_variadic;
  assert(0 <= pos && (is_variadic || pos < get_method_n_params(mtp)));
#endif

  if (ent->attr.mtd_attr.param_weight) {
    if (pos < ARR_LEN(ent->attr.mtd_attr.param_weight))
      return ent->attr.mtd_attr.param_weight[pos];
    else
      return 0.0f;
  }

  analyze_method_params_weight(ent);

  if (pos < ARR_LEN(ent->attr.mtd_attr.param_weight))
    return ent->attr.mtd_attr.param_weight[pos];
  else
    return 0.0f;
}


/**
 * Analyze argument's weight of a given
 * ir graph.
 *
 * @param irg The ir graph to analyze.
 */
void analyze_irg_args_weight(ir_graph *irg)
{
  ir_entity *ent;

  ent = get_irg_entity(irg);
  if (! ent)
    return;

  if (! ent->attr.mtd_attr.param_weight)
    analyze_method_params_weight(ent);
}
