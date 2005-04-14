/*
 * Project:     libFIRM
 * File name:   ir/opt/ldstopt.c
 * Purpose:     optimization of function calls
 * Author:      Michael M. Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irvrfy.h"
#include "dbginfo_t.h"
#include "irflag_t.h"
#include "ircons.h"
#include "eset.h"
#include "funccall.h"
#include "irhooks.h"

/**
 * The walker environment for rem_mem_from_real_fkt_calls
 */
typedef struct _env_t {
  eset *pure_fkt;   /**< set containing all real functions */
  int  changed;     /**< flag, is set if a graph was changed */
  int  n_calls_removed_SymConst;
  int  n_calls_removed_Sel;
} env_t;

/**
 * remove memory from real function calls by rerouting
 * it's ProjM and connection the call with a NoMem node.
 *
 * Note: By "real function" we understand a function that did neither
 * read nor write memory.
 */
static void rem_mem_from_real_fkt_calls(ir_node *node, void *env)
{
  env_t *ctx = env;
  ir_node *call, *ptr, *mem;
  entity *ent;

  if (get_irn_op(node) == op_Call) {
    call = node;

    set_irn_link(call, NULL);

    ptr = get_Call_ptr(call);
    if (get_irn_op(ptr) == op_SymConst && get_SymConst_kind(ptr) == symconst_addr_ent) {

      ent = get_SymConst_entity(ptr);

      if (! eset_contains(ctx->pure_fkt, ent))
	return;
      ctx->n_calls_removed_SymConst++;
    }
    else if (get_irn_op(ptr) == op_Sel &&
	     get_irg_callee_info_state(current_ir_graph) == irg_callee_info_consistent) {
      /* If all possible callees are real functions, we can remove the memory edge. */
      int i, n_callees = get_Call_n_callees(call);
      if (n_callees == 0)
	/* This is kind of strange:  dying code or a Call that will raise an exception
	   when executed as there is no implementation to call.  So better not
	   optimize. */
	return;
      for (i = 0; i < n_callees; ++i) {
	if (! eset_contains(ctx->pure_fkt, get_Call_callee(call, i)))
	  return;
      }
      ctx->n_calls_removed_Sel++;
    }
    else return;

    /* ok, if we get here we found a call to a pure function,
     * route the NoMem node to the call */
    mem   = get_Call_mem(call);

    set_irn_link(call, mem);
    set_Call_mem(call, new_r_NoMem(current_ir_graph));

    /* finally, this call can float */
    set_irn_pinned(call, op_pin_state_floats);

    hook_func_call(current_ir_graph, call);

    ctx->changed = 1;
  } else if (get_irn_op(node) == op_Proj) {
    call = get_Proj_pred(node);
    if ((get_irn_op(call) != op_Call) ||
	(get_irn_op(get_Call_mem(call)) != op_NoMem))
      return;

    switch(get_Proj_proj(node)) {
    case pn_Call_M_regular: {
      ir_node *old_mem = get_irn_link(call);
      if (old_mem) exchange(node, old_mem);
    } break;
    case pn_Call_X_except:
    case pn_Call_M_except:
      exchange(node, new_Bad());
      break;
    default: ;
    }
  }
}

/*
 * optimize function calls by handling real functions
 */
void optimize_funccalls(void)
{
  int i, n, j, k;
  int change;
  unsigned num_pure = 0;
  eset *pure_fkt = eset_create();

  if (! get_opt_real_func_call())
    return;

  /* first step: detect, which functions are pure, i.e. do NOT change any memory */
  for (i = 0, n = get_irp_n_irgs(); i < n; ++i) {
    ir_graph *irg  = get_irp_irg(i);
    ir_node *end   = get_irg_end(irg);
    ir_node *endbl = get_nodes_block(end);

    change = 0;

    /* visit every Return */
    for (j = 0, k = get_Block_n_cfgpreds(endbl); j < k; ++j) {
      ir_node *node = get_Block_cfgpred(endbl, j);
      ir_op   *op   = get_irn_op(node);
      ir_node *mem;

      /* Bad nodes usually do NOT produce anything, so it's ok */
      if (op == op_Bad)
        continue;

      if (op == op_Return) {
        mem = get_Return_mem(node);

        /* Bad nodes usually do NOT produce anything, so it's ok */
        if (is_Bad(mem))
          continue;

        change = mem != get_irg_initial_mem(irg);
        if (change)
          break;
      }
      else {
        /* exception found */
        change = 1;
        break;
      }
    }

    if (! change) {
      /* check, if a keep-alive exists */
      for (j = 0, k = get_End_n_keepalives(end); j < k; ++j) {
        ir_node *mem = get_End_keepalive(end, j);

        if (mode_M != get_irn_mode(mem))
          continue;

        change = mem != get_irg_initial_mem(irg);
        if (change)
          break;
      }
    }

    if (! change) {
      eset_insert(pure_fkt, get_irg_entity(irg));
      ++num_pure;
    }
  }

  if (num_pure > 0) {
    env_t ctx;
    entity *ent;

    ctx.pure_fkt = pure_fkt;
    ctx.n_calls_removed_SymConst = 0;
    ctx.n_calls_removed_Sel = 0;

    /* all calls of pure functions can be transformed into FuncCalls */
    for (i = 0, n = get_irp_n_irgs(); i < n; ++i) {
      ir_graph *irg  = get_irp_irg(i);

      ctx.changed = 0;
      irg_walk_graph(irg, NULL, rem_mem_from_real_fkt_calls, &ctx);

      if (ctx.changed) {
        /* changes were done */
        set_irg_outs_inconsistent(irg);
        set_irg_loopinfo_state(current_ir_graph, loopinfo_cf_inconsistent);
      }
    }

    if (get_firm_verbosity()) {
      printf("Detected %d functions without side effects.\n", num_pure);
      printf("Optimizes %d(SymConst) + %d(Sel) calls to these functions.\n",
	     ctx.n_calls_removed_SymConst, ctx.n_calls_removed_Sel);
      if (get_firm_verbosity() > 1) {
	for (ent = eset_first(pure_fkt); ent; ent = eset_next(pure_fkt)) {
	  printf("  "); DDMEO(ent);
	}
      }
    }

  } else {
    if (get_firm_verbosity()) {
      printf("No functions without side effects detected\n");
    }
  }

  eset_destroy(pure_fkt);
}
