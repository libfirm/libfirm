/*
 * Project:     libFIRM
 * File name:   ir/lower/lower_calls.c
 * Purpose:     lowering of Calls with compound parameters
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "irprog_t.h"
#include "irnode_t.h"
#include "type_t.h"
#include "irmode_t.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "lower_calls.h"
#include "return.h"
#include "array.h"

/**
 * Creates a new lowered type for a method type with compound
 * arguments. The new type is associated to the old one and returned.
 */
static ir_type *create_modified_mtd_type(const lower_params_t *lp, ir_type *mtp)
{
  ir_type *lowered;
  ir_type **params, **results, *res_tp;
  int n_ress, n_params, nn_ress, nn_params, i, first_variadic;
  ident *id;
  add_hidden hidden_params;
  variadicity var;

  if (is_lowered_type(mtp)) {
    /* the type is already lowered. Not handled yet. */
    assert(0 && "lowered types NYI");
  }

  lowered = get_associated_type(mtp);
  if (lowered)
    return lowered;

  n_ress   = get_method_n_ress(mtp);
  NEW_ARR_A(ir_type *, results, n_ress);

  n_params = get_method_n_params(mtp);
  NEW_ARR_A(ir_type *, params, n_params + n_ress);

  first_variadic = get_method_first_variadic_param_index(mtp);

  hidden_params = lp->hidden_params;
  if (hidden_params == ADD_HIDDEN_SMART &&
    get_method_variadicity(mtp) == variadicity_variadic)
        hidden_params = ADD_HIDDEN_ALWAYS_IN_FRONT;

  if (hidden_params == ADD_HIDDEN_ALWAYS_IN_FRONT) {
    /* add hidden in front */
    for (nn_ress = nn_params = i = 0; i < n_ress; ++i) {
      res_tp = get_method_res_type(mtp, i);

      if (is_compound_type(res_tp))
        params[nn_params++] = lp->find_pointer_type(res_tp, get_modeP_data(), lp->def_ptr_alignment);
      else
        results[nn_ress++] = res_tp;
    }

    /* move the index of the first variadic parameter */
    first_variadic += nn_params;

    for (i = 0; i < n_params; ++i)
      params[nn_params++] = get_method_param_type(mtp, i);
  }
  else {
    /* add hidden parameters last */
    assert(get_method_variadicity(mtp) == variadicity_non_variadic &&
      "Cannot add hidden parameters at end of variadic function");

    for (nn_params = 0; nn_params < n_params; ++nn_params)
      params[nn_params] = get_method_param_type(mtp, nn_params);

    for (nn_ress = i = 0; i < n_ress; ++i) {
      res_tp = get_method_res_type(mtp, i);

      if (is_compound_type(res_tp))
        params[nn_params++] = lp->find_pointer_type(res_tp, get_modeP_data(), lp->def_ptr_alignment);
      else
        results[nn_ress++] = res_tp;
    }
  }

  /* create the new type */
  id = mangle_u(new_id_from_chars("L", 1), get_type_ident(mtp));
  lowered = new_d_type_method(id, nn_params, nn_ress, get_type_dbg_info(mtp));

  /* fill it */
  for (i = 0; i < nn_params; ++i)
    set_method_param_type(lowered, i, params[i]);
  for (i = 0; i < nn_ress; ++i)
    set_method_res_type(lowered, i, results[i]);

  var = get_method_variadicity(mtp);
  set_method_variadicity(lowered, var);
  if (var == variadicity_variadic)
    set_method_first_variadic_param_index(lowered, first_variadic);

  /* associate the lowered type with the original one for easier access */
  set_lowered_type(mtp, lowered);

  return lowered;
}

/**
 * Walker environment for fix_value_res_access().
 */
typedef struct _wlk_env_t {
  int arg_shift;      /**< argument index shift for parameters. */
  int first_hidden;   /**< index of the first hidden argument. */
} wlk_env;

/**
 * Post walker: shift all parameter indeces and remove
 * Sel(Proj(P_frame_base())
 */
static void fix_value_res_access(ir_node *n, void *ctx) {
  wlk_env *env = ctx;

  if (env->arg_shift > 0 && is_Proj(n)) {
    ir_node *pred = get_Proj_pred(n);

    /* Fix the argument numbers */
    if (pred == get_irg_args(current_ir_graph)) {
      long pnr = get_Proj_proj(n);
      set_Proj_proj(n, pnr + env->arg_shift);
    }
  }
}

/**
 * returns non-zero if adr is a compound address
 * of a frame-type entity
 *
 * @param ft   the frame type
 * @param adr  the node
 */
static int is_compound_address(ir_type *ft, ir_node *adr)
{
  entity *ent;

  if (! is_Sel(adr))
    return 0;
  if (get_Sel_n_indexs(adr) != 0)
    return 0;
  ent = get_Sel_entity(adr);
  return get_entity_owner(ent) == ft;
}

/** A pair for the copy-return-optimization */
typedef struct cr_pair {
  entity *ent;    /**< the entity than can be removed from the frame */
  ir_node *arg;   /**< the argument that replaces the entities address */
} cr_pair;

/**
 * Post walker: fixes all entities addresses for the copy-return
 * optimization.
 *
 * Note: We expect the length of the cr array to be 1 (C, C++),
 * so ignore the linear search.
 */
static void do_copy_return_opt(ir_node *n, void *ctx) {
  cr_pair *arr = ctx;
  int i;

  if (is_Sel(n)) {
    entity *ent = get_Sel_entity(n);

    for (i = ARR_LEN(arr) - 1; i >= 0; --i) {
      if (ent == arr[i].ent) {
        exchange(n, arr[i].arg);
        break;
      }
    }
  }
}

/**
 * Transform all graphs with compound parameter return:
 * Remove the return and use the hidden parameter instead.
 *
 * @param irg  the graph to transform
 */
static void transform_irg(const lower_params_t *lp, ir_graph *irg)
{
  entity *ent = get_irg_entity(irg);
  ir_type *mtp, *lowered_mtp, *res_tp, *ft;
  int i, j, k, n_ress, n_com, n_cr_opt;
  ir_node **new_in, *ret, *endbl, *bl, *mem, *copy;
  cr_pair *cr_opt;
  wlk_env env;
  add_hidden hidden_params;

  assert(ent && "Cannot tranform graph without an entity");
  assert(get_irg_phase_state(irg) != phase_high && "call lowering must be done in phase high");

  mtp    = get_entity_type(ent);
  n_ress = get_method_n_ress(mtp);
  for (n_com = i = 0; i < n_ress; ++i) {
    res_tp = get_method_res_type(mtp, i);

    if (is_compound_type(res_tp))
      ++n_com;
  }
  if (! n_com)
    return;

  /* much easier if we have only one return */
  normalize_one_return(irg);

  /* This graph has a compound argument. Create a new type */
  lowered_mtp = create_modified_mtd_type(lp, mtp);
  set_entity_type(ent, lowered_mtp);

  hidden_params = lp->hidden_params;
  if (hidden_params == ADD_HIDDEN_SMART &&
    get_method_variadicity(mtp) == variadicity_variadic)
    hidden_params = ADD_HIDDEN_ALWAYS_IN_FRONT;

  if (hidden_params == ADD_HIDDEN_ALWAYS_IN_FRONT) {
    /* hidden arguments are added first */
    env.arg_shift    = n_com;
    env.first_hidden = 0;
  }
  else {
    /* hidden arguments are added last */
    env.arg_shift    = 0;
    env.first_hidden = get_method_n_params(mtp);
  }

  if (env.arg_shift > 0) {
    /* scan the code and argument numbers. */
    irg_walk_graph(irg, NULL, fix_value_res_access, &env);
  }

  /*
   * Now fix the return.
   */

  /* STEP 1: find the return */
  endbl = get_irg_end_block(irg);
  ret = NULL;
  for (i = get_Block_n_cfgpreds(endbl) - 1; i >= 0; --i) {
    ir_node *pred = get_Block_cfgpred(endbl, i);

    if (is_Return(pred)) {
      ret = pred;
      break;
    }
  }
  /* there should always be a return */
  assert(ret);

  /*
   * STEP 2: fix it. For all compound return value add a CopyB,
   * all others are copied.
   */
  NEW_ARR_A(ir_node *, new_in, n_ress - n_com + 1);

  bl  = get_nodes_block(ret);
  mem = get_Return_mem(ret);

  ft = get_irg_frame_type(irg);
  NEW_ARR_A(cr_pair, cr_opt, n_com);
  n_cr_opt = 0;
  for (j = 1, i = k = 0; i < n_ress; ++i) {
    ir_node *pred = get_Return_res(ret, i);
    res_tp = get_method_res_type(mtp, i);

    if (is_compound_type(res_tp)) {
      ir_node *arg = get_irg_args(irg);
      arg = new_r_Proj(irg, get_nodes_block(arg), arg, mode_P_data, env.first_hidden + k);
      ++k;

      if (is_compound_address(ft, pred)) {
        /* we can do the copy-return optimization here */
        cr_opt[n_cr_opt].ent = get_Sel_entity(pred);
        cr_opt[n_cr_opt].arg = arg;
        ++n_cr_opt;
      }
      else {
        /* we cannot do copy-return optimization */
        copy = new_r_CopyB(
                irg, bl,
                mem,
                arg,
                pred,
                res_tp
               );
        mem = new_r_Proj(irg, bl, copy, mode_M, pn_CopyB_M_regular);
      }
    }
    else {
      new_in[j] = pred;
      ++j;
    }
  }
  new_in[0] = mem;
  set_irn_in(ret, j, new_in);

  if (n_cr_opt > 0) {
    irg_walk_graph(irg, NULL, do_copy_return_opt, cr_opt);

    for (i = ARR_LEN(cr_opt) - 1; i >= 0; --i) {
      remove_class_member(ft, cr_opt[i].ent);
    }
  }

  dump_ir_block_graph(irg, "-lc");
}

/*
 * Lower calls with compound return types.
 * This function does the following transformations:
 *
 * - Adds a new (hidden) pointer parameter for
 *   any return compound type.
 *
 * - Use of the hidden parameters in the function code.
 *
 * - Change all calls to functions with compound return
 *   by providing space for the hidden parameter on the callers
 *   stack.
 *
 * - Replace a possible block copy after the function call.
 */
void lower_compound_ret_calls(const lower_params_t *params)
{
  int i;
  ir_graph *irg;

  /* second step: Transform all graphs */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    irg = get_irp_irg(i);

    if (irg == get_const_code_irg())
      continue;

    transform_irg(params, irg);
  }

  /*
   * Last step: transform any other method types.
   */
}
