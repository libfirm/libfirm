/*
 * Project:     libFIRM
 * File name:   ir/opt/tailrec.c
 * Purpose:     tail-recursion optimization
 * Author:      Michael Beck
 * Modified by:
 * Created:     08.06.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>
#include "tailrec.h"
#include "array.h"
#include "irprog.h"
#include "irgwalk.h"
#include "irgmod.h"
#include "irop.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "ircons.h"
#include "irflag.h"
#include "tv.h"
#include "firmstat.h"

/**
 * the environment for colelcting data
 */
typedef struct _collect_t {
  ir_node *proj_X;		/**< initial exec proj */
  ir_node *block;		/**< old first block */
  int     blk_idx;		/**< cfgpred index of the initial exec in block */
  ir_node *proj_m;		/**< linked list of memory from start proj's */
  ir_node *proj_data;		/**< linked list of all parameter access proj's */
} collect_t;

/**
 * walker for collecting data
 */
static void collect_data(ir_node *node, void *env)
{
  collect_t *data = env;
  ir_node *pred;
  ir_op *op;

  switch (get_irn_opcode(node)) {
  case iro_Proj:
    pred = get_Proj_pred(node);

    op = get_irn_op(pred);
    if (op == op_Proj) {
      ir_node *start = get_Proj_pred(pred);

      if (get_irn_op(start) == op_Start) {
	if (get_Proj_proj(pred) == pn_Start_T_args) {
	  /* found Proj(ProjT(Start)) */
	  set_irn_link(node, data->proj_data);
	  data->proj_data = node;
	}
      }
    }
    else if (op == op_Start) {
      switch (get_Proj_proj(node)) {
        case pn_Start_M:
          /* found ProjM(Start) */
	  set_irn_link(node, data->proj_m);
	  data->proj_m = node;
	  break;
	case pn_Start_X_initial_exec:
	  /* found ProjX(Start) */
	  data->proj_X = node;
	  break;
	default:
	  break;
      }
    }
    break;
  case iro_Block: {
    int i, n_pred = get_Block_n_cfgpreds(node);

    /*
     * the first block has the initial exec as cfg predecessor
     */
    if (node != current_ir_graph->start_block) {
      for (i = 0; i < n_pred; ++i) {
	if (get_Block_cfgpred(node, i) == data->proj_X) {
	  data->block   = node;
	  data->blk_idx = i;
	  break;
	}
      }
    }
    break;
  }
  default:
    break;
  }
}

/**
 * do the graph reconstruction for tail-recursion elimination
 *
 * @param irg           the graph that will reconstructed
 * @param rets          linked list of all rets
 * @param n_tail_calls  number of tail-recursion calls
 */
static void do_opt_tail_rec(ir_graph *irg, ir_node *rets, int n_tail_calls)
{
  ir_graph *rem_irg = current_ir_graph;
  ir_node *end_block = irg->end_block;
  ir_node *block, *jmp, *call, *calls;
  ir_node **in;
  ir_node **phis;
  ir_node ***call_params;
  ir_node *p;
  int i, j, n_params;
  collect_t data;
  int rem = get_optimize();

  assert(n_tail_calls);

  /* we add nwe nodes, so the outs are inconsistant */
  set_irg_outs_inconsistent(irg);

  /* we add new blocks and change the control flow */
  set_irg_dom_inconsistent(irg);

  /* we add a new loop */
  set_irg_loopinfo_inconsistent(irg);

  set_optimize(0);

  /* collect needed data */
  data.proj_X    = NULL;
  data.block     = NULL;
  data.blk_idx   = -1;
  data.proj_m    = NULL;
  data.proj_data = NULL;
  irg_walk_graph(irg, NULL, collect_data, &data);

  /* check number of arguments */
  call = get_irn_link(end_block);
  n_params = get_Call_n_params(call);

  assert(data.proj_X && "Could not find initial exec from Start");
  assert(data.block  && "Could not find first block");
  assert(data.proj_m && "Could not find ProjM(Start)");
  assert((data.proj_data || n_params == 0) && "Could not find Proj(ProjT(Start)) of non-void function");

  current_ir_graph = irg;

  /* allocate in's for phi and block construction */
  NEW_ARR_A(ir_node *, in, n_tail_calls + 1);

  in[0] = data.proj_X;

  /* turn Return's into Jmp's */
  for (i = 1, p = rets; p; p = get_irn_link(p)) {
    ir_node *jmp;

    switch_block(get_nodes_Block(p));
    jmp = new_Jmp();

    exchange(p, new_Bad());
    in[i++] = jmp;

    add_End_keepalive(get_irg_end(irg), jmp);
  }

  /* create a new block at start */
  block = new_Block(n_tail_calls + 1, in);
  jmp   = new_Jmp();

  /* the old first block is now the second one */
  set_Block_cfgpred(data.block, data.blk_idx, jmp);

  /* allocate phi's, position 0 contains the memory phi */
  NEW_ARR_A(ir_node *, phis, n_params + 1);

  /* build the memory phi */
  i = 0;
  in[i] = new_rd_Proj(NULL, irg, irg->start_block, irg->start, mode_M, pn_Start_M);
  ++i;

  for (calls = call; calls; calls = get_irn_link(calls)) {
    in[i] = get_Call_mem(calls);
    ++i;
  }
  phis[0] = new_rd_Phi(NULL, irg, block, n_tail_calls + 1, in, mode_M);

  /* build the data phi's */
  if (n_params > 0) {
    ir_node *calls;

    NEW_ARR_A(ir_node **, call_params, n_params);

    /* collect all parameters */
    for (i = 0, calls = call; calls; calls = get_irn_link(calls)) {
      call_params[i] = get_Call_param_arr(calls);
      ++i;
    }

    /* build new projs and Phi's */
    for (i = 0; i < n_params; ++i) {
      ir_mode *mode = get_irn_mode(call_params[0][i]);

      in[0] = new_rd_Proj(NULL, irg, block, irg->args, mode, i);
      for (j = 0; j < n_tail_calls; ++j)
	in[j + 1] = call_params[j][i];

      phis[i + 1] = new_rd_Phi(NULL, irg, block, n_tail_calls + 1, in, mode);
    }
  }

  /*
   * ok, we are here, so we have build and collected all needed Phi's
   * now exchange all Projs into links to Phi
   */
  for (p = data.proj_m; p; p = get_irn_link(p)) {
    exchange(p, phis[0]);
  }
  for (p = data.proj_data; p; p = get_irn_link(p)) {
    long proj = get_Proj_proj(p);

    assert(0 <= proj && proj < n_params);
    exchange(p, phis[proj + 1]);
  }

  current_ir_graph = rem_irg;
  set_optimize(rem);
}

/*
 * convert simple tail-calls into loops
 */
void opt_tail_rec_irg(ir_graph *irg)
{
  ir_node *end_block = irg->end_block;
  int n_preds;
  int i, n_tail_calls = 0;
  ir_node *rets = NULL;

  if (! get_opt_tail_recursion() || ! get_opt_optimize())
    return;

  set_irn_link(end_block, NULL);

  n_preds = get_Block_n_cfgpreds(end_block);
  for (i = 0; i < n_preds; ++i) {
    ir_node *ret = get_Block_cfgpred(end_block, i);
    ir_node *proj_m, *call, *call_ptr;
    tarval *tv;
    entity *ent;
    int j, n_ress;
    ir_node **ress;

    /* search all returns of a block */
    if (get_irn_op(ret) != op_Return)
      continue;

    /* check, if it's a Return self() */
    proj_m = get_Return_mem(ret);

    if (get_irn_op(proj_m) != op_Proj)
      continue;

    call = get_Proj_pred(proj_m);
    if (get_irn_op(call) != op_Call)
      continue;

    /* check if it's a recursive call */
    call_ptr = get_Call_ptr(call);

    if (get_irn_op(call_ptr) != op_Const)
      continue;

    tv = get_Const_tarval(call_ptr);
    if (! tarval_is_entity(tv))
      continue;

    ent = tarval_to_entity(tv);
    if (!ent || get_entity_irg(ent) != irg)
      continue;

    /* ok, mem is routed to a recursive call, check return args */
    n_ress = get_Return_n_ress(ret);
    ress = get_Return_res_arr(ret);

    for (j = 0; j < n_ress; ++j) {
      ir_node *proj = ress[j];
      ir_node *proj_proj;
      ir_node *irn;

      if (get_irn_op(proj) != op_Proj) {
	/* not routed to a call */
	break;
      }

      proj_proj = get_Proj_pred(proj);

      if (get_irn_op(proj) != op_Proj) {
	/* not routed to a call */
	break;
      }

      irn = get_Proj_pred(proj_proj);

      if (irn != call) {
	/* not routed to a call */
	break;
      }
    }
    if (j < n_ress)
      continue;

    /* here, we have found a call */
    set_irn_link(call, get_irn_link(end_block));
    set_irn_link(end_block, call);
    ++n_tail_calls;

    /* link all returns, we will need this */
    set_irn_link(ret, rets);
    rets = ret;
  }

  /* now, end_block->link contains the list of all tail calls */
  if (! n_tail_calls)
    return;

  do_opt_tail_rec(irg, rets, n_tail_calls);
}

/*
 * optimize tail recursion away
 */
void opt_tail_recursion(void)
{
  int i;

  if (! get_opt_tail_recursion() || ! get_opt_optimize())
    return;

  for (i = 0; i < get_irp_n_irgs(); i++) {
    current_ir_graph = get_irp_irg(i);

    opt_tail_rec_irg(current_ir_graph);
  }
}
