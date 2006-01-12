/*
 * Project:     libFIRM
 * File name:   ir/opt/gvn_pre.c
 * Purpose:     Global Value Numbering Partial Redundancy Elimination
 *              (VanDrunen Hosking 2004)
 * Author:      Michael Beck, Rubino Geiss
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>

#include "irgraph_t.h"
#include "irgwalk.h"
#include "irdom.h"
#include "irouts.h"
#include "pset.h"
#include "irgopt.h"
#include "iropt_t.h"
#include "irprintf.h"
#include "irnode_t.h"

/* */
typedef struct block_info {
  pset *nodes;        /**< the set of nodes per block */
  pset *avail_out;    /**< the Avail_out set for a block */
  pset *antic_in;     /**< the Antic_in set for a block */
  struct block_info *next;
} block_info;

typedef struct avail_env {
  struct obstack *obst;   /**< the obstack to allocate on */
  ir_node *start_block;
  ir_node *end_block;
  block_info *list;       /**< links all block info entires for easier recovery */
  int changes;            /**< non-zero, if calculation of Antic_in has changed */
} avail_env;

/**
 * returns non-zero if a node is movable.
 */
static int is_nice_value(ir_node *n) {
  ir_mode *mode = get_irn_mode(n);

  if (mode == mode_M || mode == mode_X)
    return 0;
  return (get_irn_pinned(n) != op_pin_state_pinned);
}

/** computes dst = dst \/ src */
static void pset_union(pset *dst, pset *src, unsigned (*hash)(void *))
{
  void *entry;

  for (entry = pset_first(src); entry; entry = pset_next(src)) {
    pset_insert(dst, entry, ir_node_hash(entry));
  }
}

/**
 * computes Avail_out(block):
 *
 * Avail_in(block)  = Avail_out(dom(block))
 * Avail_out(block) = Avail_in(block) \/ Nodes(block)
 *
 * Precondition:
 *  This function must be called in the top-down dominance order:
 *  Then, it computes Leader(Nodes(block)) instead of Nodes(block) !
 */
static void compute_avail_top_down(ir_node *block, void *ctx)
{
  avail_env *env = ctx;
  block_info *dom_info;
  block_info *info = get_irn_link(block);
  ir_node *dom_blk;
  int i;

  /* the root has no dominator */
  if (block != env->start_block) {
    dom_blk = get_Block_idom(block);
    assert(is_Block(dom_blk));

    dom_info = get_irn_link(dom_blk);
    assert(dom_info);

    pset_union(info->avail_out, dom_info->avail_out, ir_node_hash);
    pset_union(info->avail_out, info->nodes, ir_node_hash);
  }
#ifdef _DEBUG
  {
    ir_node *n;

    ir_printf("Avail_out(%+F) = {\n", block);
    for (i = 0, n = pset_first(info->avail_out); n; ++i, n = pset_next(info->avail_out)) {
      if ((i & 3) == 3)
        printf("\n");
      ir_printf(" %+F,", n);
    }
    printf("\n}\n");
  }
#endif
}

/*
 * Implement phi_translate
 */
static ir_node *phi_translate(ir_node *node, ir_node *block, int pos, avail_env *env)
{
  ir_node *pred_block;
  ir_node *res;
  int i, arity = get_irn_intra_arity(node);
  struct obstack *old;

  if (is_Phi(node)) {
    if (get_nodes_block(node) == block)
      return get_Phi_pred(node, pos);
    return node;
  }

  /* check if the node has at least one Phi predecessor */
  for (i = 0; i < arity; ++i) {
    ir_node *phi = get_irn_intra_n(node, i);
    if (is_Phi(phi) && get_nodes_block(phi) == block)
      break;
  }
  if (i >= arity) {
    /* no Phi in the predecessors */
    return node;
  }

  pred_block = get_Block_cfgpred_block(block, pos);

  /* Create a copy of the node in the pos'th predecessor block.
     Use our environmental obstack, as these nodes are always
     temporary. */
  old = current_ir_graph->obst;
  current_ir_graph->obst = env->obst;
  res   = new_ir_node(
            get_irn_dbg_info(node),
            current_ir_graph,
            pred_block,
            get_irn_op(node),
            get_irn_mode(node),
            arity,
            get_irn_in(node));
  /* We need the attribute copy here, because the Hash value of a
     node might depend on that. */
  copy_node_attr(node, res);
  current_ir_graph->obst = old;

  for (i = -1; i < arity; ++i) {
    ir_node *pred = get_irn_intra_n(node, i);

    if (! is_Phi(pred))
      set_irn_n(res, i, pred);
    else
      set_irn_n(res, i, get_Phi_pred(pred, pos));
  }
  set_irn_link(res, node);
  return res;
}

/**
 * Retranslate a Phi-translated node back
 */
static ir_node *phi_retrans(ir_node *n, avail_env *env)
{
  if (node_is_in_irgs_storage(current_ir_graph, n))
    return n;
  return get_irn_link(n);
}

/**
 * computes Antic_in(block):
 *
 */
static void compute_antic(ir_node *block, void *ctx)
{
  avail_env *env = ctx;
  block_info *succ_info;
  block_info *info = get_irn_link(block);
  ir_node *succ;
  int i, size;

  size = pset_count(info->antic_in);

  /* the root has no dominator */
  if (block != env->end_block) {
    int n_succ = get_Block_n_cfg_outs(block);

    if (n_succ == 1) {
      ir_node *node;
      int i, pos = -1;
      pset *nodes = new_pset(identities_cmp, 8);

      pset_union(nodes, info->nodes, ir_node_hash);

      /* find blocks position in succ's block predecessors */
      succ = get_Block_cfg_out(block, 0);
      for (i = get_Block_n_cfgpreds(succ) - 1; i >= 0; --i) {
        if (get_Block_cfgpred_block(succ, i) == block) {
          pos = i;
          break;
        }
      }
      assert(pos >= 0);

      succ_info = get_irn_link(succ);
      for (node = pset_first(succ_info->antic_in);
           node;
           node = pset_next(succ_info->antic_in)) {
        ir_node *trans = phi_translate(node, succ, pos, env);

        identify_remember(nodes, trans);

        /* add all predecessors of node */
        for (i = get_irn_arity(node) - 1; i >= 0; --i) {
          ir_node *pred = get_irn_n(node, i);
          ir_node *trans = phi_translate(pred, succ, pos, env);

          if (is_nice_value(trans))
            identify_remember(nodes, trans);
        }
      }
     /* this step calculates Antic_in(b) = Antic_out(b) \/ Nodes(b) */
     pset_union(info->antic_in, nodes, ir_node_hash);
     del_pset(nodes);
   }
    else {
      ir_node *n, *succ0;
      block_info *succ0_info;
      int i;

      assert(n_succ > 1);

      /* Select a successor to compute the disjoint of all Nodes
         sets, it might be useful to select the block with the
         smallest number of nodes.  For simplicity we choose the
         first one. */
      succ0 = get_Block_cfg_out(block, 0);
      succ0_info = get_irn_link(succ0);
      for (n = pset_first(succ0_info->antic_in);
           n;
           n = pset_next(succ0_info->antic_in)) {
        /* we need the disjoint */
        for (i = 1; i < n_succ; ++i) {
          ir_node *succ = get_Block_cfg_out(block, i);
          block_info *succ_info = get_irn_link(succ);
          if (pset_find(succ_info->antic_in, n, ir_node_hash(n)) == NULL)
            break;
        }
        if (i >= n_succ) {
          /* we found a node that is common in all Antic_in(succ(b)),
             put it in Antic_in(b) */
          identify_remember(info->antic_in, n);
        }
      }
      /* this step calculates Antic_in(b) = Antic_out(b) \/ Nodes(b) */
      pset_union(info->antic_in, info->nodes, ir_node_hash);
    }
  }

  if (size != pset_count(info->antic_in))
    /* the Antic_in set has changed */
    env->changes |= 1;

#ifdef _DEBUG
  {
    ir_node *n;

    ir_printf("Antic_in(%+F) = {\n", block);
    for (i = 0, n = pset_first(info->antic_in); n; ++i, n = pset_next(info->antic_in)) {
      ir_node *orig = phi_retrans(n, env);
      if ((i & 3) == 3)
        printf("\n");
      ir_printf(" %+F%", n);
      if (orig != n)
        ir_printf("{%+F}", orig);
      printf(", ");
    }
    printf("\n}\n");
  }
#endif
}

/**
 * allocate a block info
 */
static void alloc_blk_info(ir_node *block, void *ctx)
{
  int i;
  avail_env *env = ctx;
  block_info *info = obstack_alloc(env->obst, sizeof(block_info));

  set_irn_link(block, info);
  info->nodes     = new_pset(identities_cmp, 8);
  info->antic_in  = new_pset(identities_cmp, 8);
  info->avail_out = new_pset(identities_cmp, 8);
  info->next      = env->list;
  env->list       = info->next;

  /* fill the nodes set, we will need it later */
  for (i = get_irn_n_outs(block) - 1; i >= 0; --i) {
    ir_node *n = get_irn_out(block, i);

    /* we cannot optimize pinned nodes, so do not remember them */
    if (is_nice_value(n))
      identify_remember(info->nodes, n);
    else if (is_Phi(n) && get_irn_mode(n) != mode_M) {
      /*
       * Phis are "temporaries" and must be handled special:
       * They are avail, but are not in Antic_in
       */
      identify_remember(info->avail_out, n);
    }
  }
}

/**
 * Insert the nodes.
 */
static void insert_nodes(ir_node *block, void *ctx)
{
  avail_env *env = ctx;
  ir_node *v, *idom, *first_s;
  block_info *info, *idom_info;
  int pos, arity = get_irn_intra_arity(block);
  int all_same, by_some;

  if (arity <= 1)
    return;

  info = get_irn_link(block);

  idom = get_Block_idom(block);
  idom_info = get_irn_link(idom);
  for (v = pset_first(info->antic_in);
       v;
       v = pset_next(info->antic_in)) {
    /* If the value was already computed in the dominator, then
       it is totally redundant.  Hence we have nothing to insert. */
    if (pset_find(idom_info->avail_out, v, ir_node_hash(v))) {
//      ir_printf("Found %+F from block %+F avail in dom %+F\n", v, block, idom);
      continue;
    }

    all_same = 1;
    by_some  = 0;
    first_s  = NULL;

    for (pos = 0; pos < arity; ++pos) {
      block_info *pred_info;
      ir_node *pred = get_Block_cfgpred_block(block, pos);
      ir_node *trans, *found;

      if (is_Bad(pred))
        continue;

      trans = phi_translate(v, block, pos, env);

      pred_info = get_irn_link(pred);
      found = pset_find(pred_info->avail_out, trans, ir_node_hash(trans));

      if (found == NULL) {
        all_same = 0;
      }
      else {
        by_some = 1;
        if (first_s == NULL)
          first_s = found;
        else if (first_s != found)
          all_same = 0;

        ir_printf("Found %+F from block %+F as %+F in pred %+F\n", v, block, found, pred);
      }
    }

    if (! all_same && by_some) {
      ir_printf("Partial redundant %+F from block %+F found\n", v, block);
    }
  }
}

void do_gvn_pre(ir_graph *irg)
{
  struct obstack obst;
  avail_env a_env;
  optimization_state_t state;
  block_info *p;
  int iter = 0;

  obstack_init(&obst);
  a_env.obst        = &obst;
  a_env.list        = NULL;
  a_env.start_block = get_irg_start_block(irg);
  a_env.end_block   = get_irg_end_block(irg);

  remove_critical_cf_edges(irg);

  /* we need dominator AND post dominator information */
  if (get_irg_dom_state(irg) != dom_consistent)
    compute_doms(irg);
  if (get_irg_postdom_state(irg) != dom_consistent)
    compute_postdoms(irg);
  if (get_irg_outs_state(irg) != outs_consistent)
    compute_irg_outs(irg);

  save_optimization_state(&state);
  set_opt_global_cse(1);

  /* allocate block info for all blocks */
  irg_block_walk_graph(irg, NULL, alloc_blk_info, &a_env);

  /* compute the available value sets for all blocks */
  dom_tree_walk_irg(irg, compute_avail_top_down, NULL, &a_env);

  /* compute the anticipated value sets for all blocks */
  do {
#ifdef _DEBUG
    printf("Antic_in Iteration %d starts ...\n", ++iter);
#endif /* _DEBUG */
    a_env.changes = 0;
    irg_block_walk_graph(irg, compute_antic, NULL, &a_env);
//    postdom_tree_walk_irg(irg, compute_antic, NULL, &a_env);
#ifdef _DEBUG
    printf("------------------------\n");
#endif /* _DEBUG */
  } while (a_env.changes != 0);

  iter = 0;
  do {
#ifdef _DEBUG
    printf("Insert Iteration %d starts ...\n", ++iter);
#endif /* _DEBUG */
    a_env.changes = 0;
    irg_block_walk_graph(irg, insert_nodes, NULL, &a_env);
//    dom_tree_walk_irg(irg, insert_nodes, NULL, &a_env);
#ifdef _DEBUG
    printf("------------------------\n");
#endif /* _DEBUG */
  } while (a_env.changes != 0);

  restore_optimization_state(&state);

  for (p = a_env.list; p != NULL; p = p->next) {
    if (p->antic_in)
      del_pset(p->antic_in);
    if (p->avail_out)
      del_pset(p->avail_out);
  }
  obstack_free(&obst, NULL);
}
