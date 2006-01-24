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
#include "ircons.h"
#include "irgmod.h"
#include "debug.h"
#include "gvn_pre.h"

/* */
typedef struct block_info {
  pset *nodes;        /**< the set of nodes per block */
  pset *avail_out;    /**< the Avail_out set for a block */
  pset *antic_in;     /**< the Antic_in set for a block */
  pset *new_set;      /**< the set of all new values for a block */
  ir_node *avail;     /**< the get_map(avail, block) result */
  int not_found;      /**< non-zero, if avail was not found in this block */
  struct block_info *next;
} block_info;

typedef struct avail_env {
  struct obstack *obst;   /**< the obstack to allocate on */
  ir_node *start_block;   /**< the start block of the current graph */
  ir_node *end_block;     /**< the end block of the current graph */
  block_info *list;       /**< links all block info entires for easier recovery */
  int changes;            /**< non-zero, if calculation of Antic_in has changed */
} avail_env;

/** The debug module handle. */
static firm_dbg_module_t *dbg;

/**
 * returns non-zero if a node is movable.
 */
static int is_nice_value(ir_node *n) {
  ir_mode *mode = get_irn_mode(n);

  if (!mode_is_data(mode))
    return 0;
  if (is_irn_constlike(n))
    return 0;
  return (get_irn_pinned(n) != op_pin_state_pinned);
}

#define pset_foreach(v, s)  for ((v) = pset_first(s); (v); (v) = pset_next(s))

/** computes dst = dst \/ src */
static void pset_union(pset *dst, pset *src, unsigned (*hash)(void *))
{
  void *entry;

  pset_foreach(entry, src) {
    pset_insert(dst, entry, ir_node_hash(entry));
  }
}

#ifdef DEBUG_libfirm
/**
 * Dump the Avail or Antic sets
 */
static void dump_set(pset *set, char *txt, ir_node *block)
{
  ir_node *n;
  int i;

  DBG((dbg, LEVEL_2, "%s(%+F) = {\n", txt, block));
  i = 0;
  pset_foreach(n, set) {
    if ((i & 3) == 3)
      DBG((dbg, LEVEL_2, "\n"));
    DBG((dbg, LEVEL_2, " %+F,", n));
    ++i;
  }
  DBG((dbg, LEVEL_2, "\n}\n"));
}  /* dump_set */

#else
#define dump_set(set, txt, block)
#endif /* DEBUG_libfirm */


/**
 * Return the block info of a block
 */
static block_info *get_block_info(ir_node *block) {
  return get_irn_link(block);
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
  block_info *info = get_block_info(block);
  ir_node *dom_blk;

  /* we don't need the end block Avail */
  if (block == env->end_block)
    return;

  pset_union(info->avail_out, info->nodes, ir_node_hash);

  /* the root has no dominator */
  if (block != env->start_block) {
    dom_blk = get_Block_idom(block);
    assert(is_Block(dom_blk));

    dom_info = get_block_info(dom_blk);
    assert(dom_info);

    pset_union(info->avail_out, dom_info->avail_out, ir_node_hash);
  }
  dump_set(info->avail_out, "Avail_out", block);
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
  set_irn_link(res, NULL);
  return res;
}

/**
 * computes Antic_in(block):
 *
 */
static void compute_antic(ir_node *block, void *ctx)
{
  avail_env *env = ctx;
  block_info *succ_info;
  block_info *info = get_block_info(block);
  ir_node *succ;
  int size;

  /* no need for computations in start block */
  if (block == env->start_block)
    return;

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

      succ_info = get_block_info(succ);
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
      succ0_info = get_block_info(succ0);
      for (n = pset_first(succ0_info->antic_in);
           n;
           n = pset_next(succ0_info->antic_in)) {
        /* we need the disjoint */
        for (i = 1; i < n_succ; ++i) {
          ir_node *succ = get_Block_cfg_out(block, i);
          block_info *succ_info = get_block_info(succ);
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

  dump_set(info->antic_in, "Antic_in", block);
}  /* compute_antic */

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
  info->avail     = NULL;
  info->not_found = 0;
  info->new_set   = NULL;
  info->next      = env->list;
  env->list       = info->next;

  /* fill the nodes set, we will need it later */
  for (i = get_irn_n_outs(block) - 1; i >= 0; --i) {
    ir_node *n = get_irn_out(block, i);

    /* clear the link field here, we need it later */
    set_irn_link(n, NULL);

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
 * Compare two nodes for equal operands.
 */
static int operands_equal(ir_node *n1, ir_node *n2)
{
  int i, arity = get_irn_arity(n1);
  assert(n1->op == n2->op && arity == get_irn_arity(n2));
  for (i = 0; i < arity; ++i)
    if (get_irn_n(n1, i) != get_irn_n(n2, i))
      return 0;
  return 1;
}

/**
 * Get the leader of an expression. In Firm, only
 * Phi nodes can be leaders, all other 'leader' are
 * handled by the identify_remember mechanism right.
 */
static ir_node *get_leader(ir_node *n)
{
  ir_node *l = get_irn_link(n);

  if (l) {
    assert(is_Phi(l));
    return l;
  }
  return n;
}

static ir_node *has_leader(ir_node *n)
{
  ir_node *l = get_irn_link(n);

  if (l) {
    assert(is_Phi(l));
    return l;
  }
  return NULL;
}

/**
 * Perform insertion of partially redundant values.
 * For every Block node, do the following:
 * 1.  Propagate the NEW_SETS of the dominator into the current block.
 * If the block has multiple predecessors,
 *     2a. Iterate over the ANTIC expressions for the block to see if
 *         any of them are partially redundant.
 *     2b. If so, insert them into the necessary predecessors to make
 *         the expression fully redundant.
 *     2c. Insert a new Phi merging the values of the predecessors.
 *     2d. Insert the new Phi, and the new expressions, into the
 *         NEW_SETS set.
 */
static void insert_nodes(ir_node *block, void *ctx)
{
  avail_env *env = ctx;
  ir_node *v, *idom, *first_s;
  block_info *curr_info, *idom_info;
  int pos, arity = get_irn_intra_arity(block);
  int all_same, by_some;

  curr_info = get_block_info(block);
  curr_info->new_set = new_pset(identities_cmp, 8);

  if (block == env->start_block)
    return;

  idom = get_Block_idom(block);
  idom_info = get_block_info(idom);

  /* update the new_sets */
  pset_union(curr_info->new_set, idom_info->new_set, ir_node_hash);
  pset_foreach(v, idom_info->new_set) {
    ir_node *old = identify_remember(idom_info->new_set, v);

    if (old != v) {
      /* v must dominate old here */
      assert(block_dominates(get_nodes_block(v), get_nodes_block(old)));

      pset_remove(curr_info->avail_out, old, ir_node_hash(old));
      identify_remember(curr_info->avail_out, v);
    }
  }
  dump_set(curr_info->avail_out, "[Avail_out]", block);

  if (arity <= 1)
    return;

  pset_foreach(v, curr_info->antic_in) {
    /*
     * If we already have a leader for this node,
     * it is totally redundant.
     */
    if (has_leader(v))
      continue;

    /* If the value was already computed in the dominator, then
       it is totally redundant.  Hence we have nothing to insert. */
    if (pset_find(idom_info->avail_out, v, ir_node_hash(v))) {
//      DBG((dbg, LEVEL_2, "Found %+F from block %+F avail in dom %+F\n", v, block, idom));
      continue;
    }

    all_same = 1;
    by_some  = 0;
    first_s  = NULL;

    /* for all predecessor blocks */
    for (pos = 0; pos < arity; ++pos) {
      block_info *pred_info;
      ir_node *pred_blk = get_Block_cfgpred_block(block, pos);
      ir_node *trans, *found;

      /* ignore bad blocks. */
      if (is_Bad(pred_blk))
        continue;

      trans = phi_translate(v, block, pos, env);

      pred_info = get_block_info(pred_blk);
      found = pset_find(pred_info->avail_out, trans, ir_node_hash(trans));

      if (found == NULL) {
        all_same = 0;
        pred_info->avail = trans;
        pred_info->not_found = 1;
      }
      else {
        found = get_leader(found);
        pred_info->avail = found;
        pred_info->not_found = 0;
        by_some = 1;
        if (first_s == NULL)
          first_s = found;
        else if (first_s != found)
          all_same = 0;

        DBG((dbg, LEVEL_2, "Found %+F from block %+F as %+F in pred %+F\n", v, block, found, pred_blk));
      }  /* if */
    }  /* for */

    /* If it's not the same value already existing along every predecessor, and
       it's defined by some predecessor, it is partially redundant. */
    if (! all_same && by_some) {
      ir_node *phi, **in;
      ir_mode *mode = NULL;
      DBG((dbg, LEVEL_1, "Partial redundant %+F from block %+F found\n", v, block));

      in = xmalloc(arity * sizeof(*in));
      /* for all predecessor blocks */
      for (pos = 0; pos < arity; ++pos) {
        ir_node *pred_blk = get_Block_cfgpred_block(block, pos);
        block_info *pred_info = get_block_info(pred_blk);

        /* ignore bad blocks. */
        if (is_Bad(pred_blk)) {
          in[pos] = new_Bad();
          continue;
        }

        /* ignore blocks that already have the expression */
        if (pred_info->not_found) {
          ir_node *avail = pred_info->avail;
          ir_node *nn;
          assert(! is_Phi(avail));

          mode = get_irn_mode(avail);
          nn = new_ir_node(
            get_irn_dbg_info(avail),
            current_ir_graph, pred_blk,
            get_irn_op(avail),
            mode,
            get_irn_arity(avail),
            get_irn_in(avail) + 1);

          pred_info->avail = identify_remember(pred_info->avail_out, nn);
        }
        in[pos] = pred_info->avail;
      }  /* for */
      phi = new_r_Phi(current_ir_graph, block, arity, in, mode);
      free(in);
      identify_remember(curr_info->avail_out, phi);
      identify_remember(curr_info->new_set, phi);
      /* v might be translated, so add it here */
      identify_remember(curr_info->avail_out, v);
      identify_remember(curr_info->new_set, v);
      set_irn_link(v, phi);
      DBG((dbg, LEVEL_2, "New %+F for redundant %+F created\n", phi, v));
      env->changes |= 1;
    }  /* if */
  }  /* pset_foreach */
}  /* insert_nodes */

/**
 * Do the elimination step
 */
static void eliminate_nodes(ir_node *block, void *ctx)
{
  avail_env *env = ctx;
  block_info *curr_info = get_block_info(block);
  ir_node *v;

  pset_foreach(v, curr_info->nodes) {
    ir_node *l = identify_remember(curr_info->avail_out, v);

    l = get_leader(l);
    if (l != v) {
      DBG((dbg, LEVEL_2, "Replacing %+F by %+F\n", v, l));
      exchange(v, l);
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

  /* register a debug mask */
  dbg = firm_dbg_register("firm.opt.gvn_pre");

  obstack_init(&obst);
  a_env.obst        = &obst;
  a_env.list        = NULL;
  a_env.start_block = get_irg_start_block(irg);
  a_env.end_block   = get_irg_end_block(irg);

  /* Move Proj's into the same block as their args,
     else we would assign the result to wrong blocks */
  normalize_proj_nodes(irg);

  /* critical edges MUST be removed */
  remove_critical_cf_edges(irg);

  /* we need dominator AND post dominator information */
  if (get_irg_dom_state(irg) != dom_consistent)
    compute_doms(irg);
  if (get_irg_postdom_state(irg) != dom_consistent)
    compute_postdoms(irg);
  if (get_irg_outs_state(irg) != outs_consistent)
    compute_irg_outs(irg);

  /*
   * Switch on GCSE. We need it to correctly compute
   * the leader of a node by hashing.
   */
  save_optimization_state(&state);
  set_opt_global_cse(1);

  /* allocate block info for all blocks */
  irg_block_walk_graph(irg, NULL, alloc_blk_info, &a_env);

  /* compute the available value sets for all blocks */
  dom_tree_walk_irg(irg, compute_avail_top_down, NULL, &a_env);

  /* compute the anticipated value sets for all blocks */
  do {
    DBG((dbg, LEVEL_1, "Antic_in Iteration %d starts ...\n", ++iter));
    a_env.changes = 0;
    irg_block_walk_graph(irg, compute_antic, NULL, &a_env);
//    postdom_tree_walk_irg(irg, compute_antic, NULL, &a_env);
    DBG((dbg, LEVEL_1, "------------------------\n"));
  } while (a_env.changes != 0);

  /* compute redundant expressions */
  iter = 0;
  do {
    DBG((dbg, LEVEL_1, "Insert Iteration %d starts ...\n", ++iter));
    a_env.changes = 0;
    dom_tree_walk_irg(irg, insert_nodes, NULL, &a_env);
    DBG((dbg, LEVEL_1, "------------------------\n"));
  } while (a_env.changes != 0);

  /* last step: eliminate nodes */
  dom_tree_walk_irg(irg, eliminate_nodes, NULL, &a_env);

  restore_optimization_state(&state);

  /* clean up: delete all sets */
  for (p = a_env.list; p != NULL; p = p->next) {
    if (p->antic_in)
      del_pset(p->antic_in);
    if (p->avail_out)
      del_pset(p->avail_out);
    if (p->nodes)
      del_pset(p->nodes);
  }
  obstack_free(&obst, NULL);
}  /* do_gvn_pre */
