/*
 * Project:     libFIRM
 * File name:   ir/lower/lower_intrinsics.c
 * Purpose:     lowering of Calls of intrinsic functions
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
#include "irgwalk.h"
#include "ircons.h"
#include "irgmod.h"
#include "lower_intrinsics.h"
#include "pmap.h"

/** Walker environment */
typedef struct _walker_env {
  pmap     *map;              /**< the intrinsic map. */
  unsigned nr_of_intrinsics;  /**< statistics */
} walker_env_t;

/**
 * walker: do the call mapping
 */
static void call_mapper(ir_node *call, void *env) {
  walker_env_t *wenv = env;
  ir_node *symconst;
  pmap_entry *p;
  const i_record *r;
  entity *ent;

  if (! is_Call(call))
    return;

  symconst = get_Call_ptr(call);
  if (get_irn_op(symconst) != op_SymConst ||
      get_SymConst_kind(symconst) != symconst_addr_ent)
    return;

  ent = get_SymConst_entity(symconst);
  p   = pmap_find(wenv->map, ent);

  if (p) {
    r = p->value;
    wenv->nr_of_intrinsics += r->i_mapper(call, r->ctx) ? 1 : 0;
  }
}

/* Go through all graphs and map calls to intrinsic functions. */
unsigned lower_intrinsic_calls(const i_record *list, int length) {
  int          i;
  ir_graph     *irg;
  pmap         *map = pmap_create_ex(length);
  walker_env_t wenv;
  unsigned     nr_of_intrinsics = 0;

  /* fill a map for faster search */
  for (i = length - 1; i >= 0; --i)
    pmap_insert(map, list[i].i_ent, (void *)&list[i]);

  wenv.map = map;

  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    irg = get_irp_irg(i);

    wenv.nr_of_intrinsics = 0;
    irg_walk_graph(irg, NULL, call_mapper, map);

    if (wenv.nr_of_intrinsics) {
      /* changes detected */
      set_irg_outs_inconsistent(irg);
      set_irg_callee_info_state(irg, irg_callee_info_inconsistent);

      /* exception control flow might have changed */
      set_irg_doms_inconsistent(irg);
      set_irg_loopinfo_inconsistent(irg);

      nr_of_intrinsics += wenv.nr_of_intrinsics;
    }
  }
  pmap_destroy(map);

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
