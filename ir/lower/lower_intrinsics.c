/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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

/*
 * Project:     libFIRM
 * File name:   ir/lower/lower_intrinsics.c
 * Purpose:     lowering of Calls of intrinsic functions
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irop_t.h"
#include "irprog_t.h"
#include "irnode_t.h"
#include "irprog_t.h"
#include "irgwalk.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "trouts.h"
#include "lower_intrinsics.h"
#include "pmap.h"
#include "xmalloc.h"

/** Walker environment */
typedef struct _walker_env {
  pmap     *c_map;              /**< The intrinsic call map. */
  unsigned nr_of_intrinsics;    /**< statistics */
  i_instr_record **i_map;       /**< The intrinsic instruction map. */
} walker_env_t;

/**
 * walker: call all mapper functions
 */
static void call_mapper(ir_node *node, void *env) {
  walker_env_t *wenv = env;
  ir_op *op = get_irn_op(node);

  if (op == op_Call) {
    ir_node *symconst;
    pmap_entry *p;
    const i_call_record *r;
    ir_entity *ent;

    symconst = get_Call_ptr(node);
    if (get_irn_op(symconst) != op_SymConst ||
        get_SymConst_kind(symconst) != symconst_addr_ent)
      return;

    ent = get_SymConst_entity(symconst);
    p   = pmap_find(wenv->c_map, ent);

    if (p) {
      r = p->value;
      wenv->nr_of_intrinsics += r->i_mapper(node, r->ctx) ? 1 : 0;
    }
  }
  else {
    if (0 <= op->code && op->code < ARR_LEN(wenv->i_map)) {
      const i_instr_record *r = wenv->i_map[op->code];
      /* run all possible mapper */
      while (r) {
        if (r->i_mapper(node, r->ctx)) {
          ++wenv->nr_of_intrinsics;
          break;
        }
        r = r->link;
      }
    }
  }
}

/* Go through all graphs and map calls to intrinsic functions. */
unsigned lower_intrinsics(i_record *list, int length) {
  int            i, n_ops = get_irp_n_opcodes();
  ir_graph       *irg;
  pmap           *c_map = pmap_create_ex(length);
  i_instr_record **i_map;
  unsigned       nr_of_intrinsics = 0;
  walker_env_t   wenv;

  /* we use the ir_op generic pointers here */
  NEW_ARR_A(const i_instr_record *, i_map, n_ops);
  memset((void *)i_map, 0, sizeof(*i_map) * n_ops);

  /* fill a map for faster search */
  for (i = length - 1; i >= 0; --i) {
    if (list[i].i_call.kind == INTRINSIC_CALL) {
      pmap_insert(c_map, list[i].i_call.i_ent, (void *)&list[i].i_call);
    }
    else {
      ir_op *op = list[i].i_instr.op;
      assert(0 <= op->code && op->code < ARR_LEN(i_map));

      list[i].i_instr.link = i_map[op->code];
      i_map[op->code] = &list[i].i_instr;
    }
  }

  wenv.c_map = c_map;
  wenv.i_map = i_map;

  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    irg = get_irp_irg(i);

    wenv.nr_of_intrinsics = 0;
    irg_walk_graph(irg, NULL, call_mapper, &wenv);

    if (wenv.nr_of_intrinsics) {
      /* changes detected */
      set_irg_outs_inconsistent(irg);
      set_irg_callee_info_state(irg, irg_callee_info_inconsistent);

      /* exception control flow might have changed */
      set_irg_doms_inconsistent(irg);
      set_irg_extblk_inconsistent(irg);
      set_irg_loopinfo_inconsistent(irg);

      /* calls might be removed/added */
      set_trouts_inconsistent();

      /* optimize it, tuple might be created */
      local_optimize_graph(irg);

      nr_of_intrinsics += wenv.nr_of_intrinsics;
    }
  }
  pmap_destroy(c_map);

  return nr_of_intrinsics;
}

/* A mapper for the integer abs. */
int i_mapper_Abs(ir_node *call, void *ctx) {
  ir_node *mem   = get_Call_mem(call);
  ir_node *block = get_nodes_block(call);
  ir_node *op    = get_Call_param(call, 0);
  ir_node *irn;
  dbg_info *dbg  = get_irn_dbg_info(call);

  irn = new_rd_Abs(dbg, current_ir_graph, block, op, get_irn_mode(op));
  irn = new_Tuple(1, &irn);

  turn_into_tuple(call, pn_Call_max);
  set_Tuple_pred(call, pn_Call_M_regular, mem);
  set_Tuple_pred(call, pn_Call_X_except, new_Bad());
  set_Tuple_pred(call, pn_Call_T_result, irn);
  set_Tuple_pred(call, pn_Call_M_except, mem);
  set_Tuple_pred(call, pn_Call_P_value_res_base, new_Bad());

  return 1;
}

/* A mapper for the alloca() function. */
int i_mapper_Alloca(ir_node *call, void *ctx) {
  ir_node *mem   = get_Call_mem(call);
  ir_node *block = get_nodes_block(call);
  ir_node *op    = get_Call_param(call, 0);
  ir_node *irn, *exc;
  dbg_info *dbg  = get_irn_dbg_info(call);

  irn = new_rd_Alloc(dbg, current_ir_graph, block, mem, op, firm_unknown_type, stack_alloc);
  mem = new_Proj(irn, mode_M, pn_Alloc_M);
  exc = new_Proj(irn, mode_X, pn_Alloc_X_except);
  irn = new_Proj(irn, get_modeP_data(), pn_Alloc_res);
  irn = new_Tuple(1, &irn);

  turn_into_tuple(call, pn_Call_max);
  set_Tuple_pred(call, pn_Call_M_regular, mem);
  set_Tuple_pred(call, pn_Call_X_except, exc);
  set_Tuple_pred(call, pn_Call_T_result, irn);
  set_Tuple_pred(call, pn_Call_M_except, mem);
  set_Tuple_pred(call, pn_Call_P_value_res_base, new_Bad());

  return 1;
}

#define LMAX(a, b) ((a) > (b) ? (a) : (b))

/* A mapper for mapping unsupported instructions to runtime calls. */
int i_mapper_RuntimeCall(ir_node *node, runtime_rt *rt) {
  int i, j, arity, first, n_param, n_res;
  long n_proj;
  ir_type *mtp;
  ir_node *mem, *bl, *call, *addr, *res_proj;
  ir_node **in;
  ir_graph *irg;
  symconst_symbol sym;

  /* check if the result modes match */
  if (get_irn_mode(node) != rt->mode)
    return 0;

  arity = get_irn_arity(node);
  if (arity <= 0)
    return 0;

  mtp     = get_entity_type(rt->ent);
  n_param = get_method_n_params(mtp);
  irg     = current_ir_graph;

  mem = get_irn_n(node, 0);
  if (get_irn_mode(mem) != mode_M) {
    mem = new_r_NoMem(irg);
    first = 0;
  }
  else
    first = 1;

  /* check if the modes of the predecessors match the parameter modes */
  if (arity - first != n_param)
    return 0;

  for (i = first, j = 0; i < arity; ++i, ++j) {
    ir_type *param_tp = get_method_param_type(mtp, j);
    ir_node *pred = get_irn_n(node, i);

    if (get_type_mode(param_tp) != get_irn_mode(pred))
      return 0;
  }

  n_res = get_method_n_ress(mtp);

  /* step 0: calculate the number of needed Proj's */
  n_proj = 0;
  n_proj = LMAX(n_proj, rt->mem_proj_nr + 1);
  n_proj = LMAX(n_proj, rt->exc_proj_nr + 1);
  n_proj = LMAX(n_proj, rt->exc_mem_proj_nr + 1);
  n_proj = LMAX(n_proj, rt->res_proj_nr + 1);

  if (n_proj > 0) {
    if (rt->mode != mode_T) /* must be mode_T */
      return 0;
  }
  else {
    if (n_res > 0)
      /* must match */
      if (get_type_mode(get_method_res_type(mtp, 0)) != rt->mode)
        return 0;
  }

  /* ok, when we are here, the number of predecessors match as well as the parameter modes */
  bl = get_nodes_block(node);

  in = NULL;
  if (n_param > 0) {
    NEW_ARR_A(ir_node *, in, n_param);
    for (i = 0; i < n_param; ++i)
      in[i] = get_irn_n(node, first + i);
  }

  /* step 1: create the call */
  sym.entity_p = rt->ent;
  addr = new_r_SymConst(irg, bl, sym, symconst_addr_ent);
  call = new_rd_Call(get_irn_dbg_info(node), irg, bl, mem, addr, n_param, in, mtp);
  set_irn_pinned(call, get_irn_pinned(node));

  if (n_res > 0)
    res_proj = new_r_Proj(irg, bl, call, mode_T, pn_Call_T_result);
  else
    res_proj = NULL;

  if (n_proj > 0) {
    n_proj += n_res - 1;

    /* we are ready */
    turn_into_tuple(node, n_proj);

    for (i = 0; i < n_proj; ++i)
      set_Tuple_pred(node, i, new_r_Bad(irg));
    if (rt->mem_proj_nr >= 0)
      set_Tuple_pred(node, rt->mem_proj_nr, new_r_Proj(irg, bl, call, mode_M, pn_Call_M_regular));
    if (get_irn_op(mem) != op_NoMem) {
      /* Exceptions can only be handled with real memory */
      if (rt->exc_proj_nr >= 0)
        set_Tuple_pred(node, rt->mem_proj_nr, new_r_Proj(irg, bl, call, mode_X, pn_Call_X_except));
      if (rt->exc_mem_proj_nr >= 0)
        set_Tuple_pred(node, rt->mem_proj_nr, new_r_Proj(irg, bl, call, mode_M, pn_Call_M_except));
    }

    if (rt->res_proj_nr >= 0)
      for (i = 0; i < n_res; ++i)
        set_Tuple_pred(node, rt->res_proj_nr + i,
          new_r_Proj(irg, bl, res_proj, get_type_mode(get_method_res_type(mtp, i)), i));
    return 1;
  }
  else {
    /* only one return value supported */
    if (n_res > 0) {
      ir_mode *mode = get_type_mode(get_method_res_type(mtp, 0));

      res_proj = new_r_Proj(irg, bl, call, mode_T, pn_Call_T_result);
      res_proj = new_r_Proj(irg, bl, res_proj, mode, 0);

      exchange(node, res_proj);
      return 1;
    }
  }
  /* should not happen */
  return 0;
}
