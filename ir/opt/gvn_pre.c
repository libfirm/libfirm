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

/** Additional info we need for every block. */
typedef struct block_info {
  pset *nodes;        /**< The set of nodes per block. */
  pset *avail_out;    /**< The Avail_out set for a block. */
  pset *antic_in;     /**< The Antic_in set for a block. */
  pset *new_set;      /**< The set of all new values for a block. */
  ir_node *avail;     /**< The get_map(avail, block) result. */
  int not_found;      /**< Non-zero, if avail was not found in this block. */
  struct block_info *next;  /**< Links all entries, so we can recover the sets easily. */
} block_info;

/**
 * A pair of nodes that must be exchanged.
 * We must defer the exchange because our hash-sets cannot
 * find an already replace node else.
 */
typedef struct elim_pair {
  ir_node *old_node;      /**< The old node that will be replaced. */
  ir_node *new_node;      /**< The new node. */
  struct elim_pair *next; /**< Links all entries in a list. */
} elim_pair;

/** The environment for the GVN-PRE algorithm */
typedef struct pre_env {
  struct obstack *obst;   /**< The obstack to allocate on. */
  pset *trans_set;        /**< The set of all translated values. */
  ir_node *start_block;   /**< The start block of the current graph. */
  ir_node *end_block;     /**< The end block of the current graph */
  block_info *list;       /**< Links all block info entires for easier recovery. */
  elim_pair *pairs;       /**< A list of node pairs that must be eliminated. */
  int changes;            /**< Non-zero, if calculation of Antic_in has changed. */
} pre_env;

/** The debug module handle. */
static firm_dbg_module_t *dbg;

/**
 * Returns non-zero if a node is movable.
 */
static int is_nice_value(ir_node *n) {
  ir_mode *mode;

  while (is_Proj(n))
    n = get_Proj_pred(n);
  mode = get_irn_mode(n);
  /*
   * FIXME: For now, we cannot handle Div/even if it's movable.
   * That should be fixed.
   */
  if (!mode_is_data(mode))
    return 0;
  if (is_irn_constlike(n))
    return 0;
  return (get_irn_pinned(n) != op_pin_state_pinned);
}

#define pset_foreach(v, s)  for ((v) = pset_first(s); (v); (v) = pset_next(s))

#ifdef DEBUG_libfirm
/**
 * Dump a set.
 */
static void dump_set(pset *set, char *txt, ir_node *block)
{
  ir_node *n;
  int i;

  DB((dbg, LEVEL_2, "%s(%+F) = {\n", txt, block));
  i = 0;
  pset_foreach(n, set) {
    if ((i & 3) == 3)
      DB((dbg, LEVEL_2, "\n"));
    DB((dbg, LEVEL_2, " %+F,", n));
    ++i;
  }
  DB((dbg, LEVEL_2, "\n}\n"));
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

#define new_value_set()     new_pset(identities_cmp, 8)
#define del_value_set(set)  del_pset(set)

/**
 * Add a value to a value set
 */
static ir_node *value_add(pset *value_set, ir_node *n)
{
  return identify_remember(value_set, n);
}

/**
 * Lookup a value in a value set
 */
static ir_node *lookup(pset *value_set, ir_node *n)
{
  return pset_find(value_set, n, ir_node_hash(n));
}

/** computes dst = dst \/ src for value sets */
static void value_union(pset *dst, pset *src)
{
  void *entry;
  pset_foreach(entry, src)
    value_add(dst, entry);
}

/**
 * Computes Avail_out(block):
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
  pre_env *env = ctx;
  block_info *dom_info;
  block_info *info = get_block_info(block);
  ir_node *dom_blk;

  /* we don't need the end block Avail */
  if (block == env->end_block)
    return;

  /*
   * First add all nodes from the dominator.
   * This must be done to ensure that Antic_out contains the leader
   * for every node. The root has no dominator.
   */
  if (block != env->start_block) {
    dom_blk = get_Block_idom(block);
    assert(is_Block(dom_blk));

    dom_info = get_block_info(dom_blk);
    assert(dom_info);

    value_union(info->avail_out, dom_info->avail_out);
  }
  value_union(info->avail_out, info->nodes);

  dump_set(info->avail_out, "Avail_out", block);
}

/**
 * Returns the Phi-leader if one exists, else NULL.
 */
static ir_node *has_Phi_leader(ir_node *n)
{
  ir_node *l = get_irn_link(n);

  if (l) {
    assert(is_Phi(l));
    return l;
  }
  return NULL;
}  /* has_Phi_leader */

/**
 * Returns the Phi-leader if one exists, else n.
 */
static ir_node *get_Phi_leader(ir_node *n)
{
  ir_node *l = get_irn_link(n);

  if (l) {
    assert(is_Phi(l));
    return l;
  }
  return n;
}  /* get_Phi_leader */

/**
 * Get the leader of an expression.
 */
static ir_node *find_leader(pset *value_set, ir_node *n)
{
  ir_node *l = has_Phi_leader(n);
  if (l != NULL)
    return l;
  l = lookup(value_set, n);
  return l ? get_Phi_leader(l) : l;
}

/**
 * Implements phi_translate.
 */
static ir_node *phi_translate(ir_node *node, ir_node *block, int pos, pre_env *env)
{
  ir_node *nn, *res;
  int i, arity;
  struct obstack *old;
  ir_node *pred_block = get_Block_cfgpred_block(block, pos);
  block_info *pred_info = get_block_info(pred_block);

  if (is_Phi(node)) {
    if (get_nodes_block(node) == block) {
      ir_node *leader, *pred;
      pred   = get_Phi_pred(node, pos);
      leader = find_leader(pred_info->avail_out, pred);
      assert(!leader || is_Phi(pred) || is_nice_value(pred));
      node = leader != NULL ? leader : pred;
    }
    return node;
  }

  arity = get_irn_intra_arity(node);

  /* check if the node has at least one Phi predecessor */
  for (i = 0; i < arity; ++i) {
    ir_node *pred     = get_irn_intra_n(node, i);
    ir_node *local_bl = get_irn_intra_n(pred, -1);
    ir_node *leader   = find_leader(get_block_info(local_bl)->avail_out, pred);

    assert(!leader || is_Phi(pred) || is_nice_value(pred));
    leader = leader != NULL ? leader : pred;
    if (is_Phi(leader) && get_nodes_block(leader) == block)
      break;
  }
  if (i >= arity) {
    /* no Phi in the predecessors */
    return node;
  }

  /* Create a copy of the node in the pos'th predecessor block.
     Use our environmental obstack, as these nodes are always
     temporary. */
  old = current_ir_graph->obst;
  current_ir_graph->obst = env->obst;
  nn = new_ir_node(
          get_irn_dbg_info(node),
          current_ir_graph,
          pred_block,
          get_irn_op(node),
          get_irn_mode(node),
          arity,
          get_irn_in(node));
  /* We need the attribute copy here, because the Hash value of a
     node might depend on that. */
  copy_node_attr(node, nn);

  set_irn_n(nn, -1, get_irn_intra_n(node, -1));
  for (i = 0; i < arity; ++i) {
    ir_node *pred     = get_irn_intra_n(node, i);
    ir_node *local_bl = get_irn_intra_n(pred, -1);
    ir_node *leader   = find_leader(get_block_info(local_bl)->avail_out, pred);

    leader = leader != NULL ? leader : pred;
    if (is_Phi(leader) && get_nodes_block(leader) == block)
      set_irn_n(nn, i, get_Phi_pred(leader, pos));
    else
      set_irn_n(nn, i, leader);
  }
  set_irn_link(nn, NULL);

  res = value_add(env->trans_set, nn);
  current_ir_graph->obst = old;

  if (nn != res)
    obstack_free(env->obst, nn);
  else {
    DB((dbg, LEVEL_2, "Translate %+F into %+F\n", node, res));
  }
  return res;
}  /* phi_translate */

static int _is_clean(ir_node *n, ir_node *block)
{
  int i;

  if (get_nodes_block(n) != block)
    return 1;
  if (is_Phi(n))
    return 1;

  if (irn_visited(n))
    return 0;

  if (! is_nice_value(n))
    goto bad;
  for (i = get_irn_arity(n) - 1; i >= 0; --i) {
    ir_node *pred = get_irn_n(n, i);
    if (! _is_clean(pred, block))
      goto bad;
  }
  return 1;
bad:
  mark_irn_visited(n);
  return 0;
}

static int is_clean(ir_node *n)
{
  int res = _is_clean(n, get_nodes_block(n));
  return res;
}

/**
 * computes Antic_in(block):
 */
static void compute_antic(ir_node *block, void *ctx)
{
  pre_env *env = ctx;
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
      pset *nodes = new_value_set();

      pset_foreach(node, info->nodes) {
        if (is_clean(node))
          value_add(nodes, node);
      }

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

        if (is_clean(trans))
          value_add(nodes, trans);

        /* add all predecessors of node */
        for (i = get_irn_arity(node) - 1; i >= 0; --i) {
          ir_node *pred = get_irn_n(node, i);
          ir_node *trans = phi_translate(pred, succ, pos, env);

          if (is_clean(trans))
            value_add(nodes, trans);
        }
      }
      /* this step calculates Antic_in(b) = Antic_out(b) \/ Nodes(b) */
      value_union(info->antic_in, nodes);
      del_value_set(nodes);
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
          if (lookup(succ_info->antic_in, n) == NULL)
            break;
        }
        if (i >= n_succ) {
          /* we found a node that is common in all Antic_in(succ(b)),
             put it in Antic_in(b) */
          value_add(info->antic_in, n);
        }
      }
      /* this step calculates Antic_in(b) = Antic_out(b) \/ Nodes(b) */
      pset_foreach(n, info->nodes) {
        if (is_clean(n))
          value_add(info->antic_in, n);
      }
    }
  }

  dump_set(info->antic_in, "Antic_in", block);
  if (size != pset_count(info->antic_in)) {
    /* the Antic_in set has changed */
    env->changes |= 1;
  }
}  /* compute_antic */

/**
 * allocate a block info
 */
static void alloc_blk_info(ir_node *block, void *ctx)
{
  int i;
  pre_env *env = ctx;
  block_info *info = obstack_alloc(env->obst, sizeof(block_info));

  set_irn_link(block, info);
  info->nodes     = new_value_set();
  info->antic_in  = new_value_set();
  info->avail_out = new_value_set();
  info->avail     = NULL;
  info->not_found = 0;
  info->new_set   = NULL;
  info->next      = env->list;
  env->list       = info;

  /* fill the nodes set, we will need it later */
  for (i = get_irn_n_outs(block) - 1; i >= 0; --i) {
    ir_node *n = get_irn_out(block, i);

    /* clear the link field here, we need it later */
    set_irn_link(n, NULL);

    /* we cannot optimize pinned nodes, so do not remember them */
    if (is_nice_value(n))
      value_add(info->nodes, n);
    else if (is_Phi(n) && get_irn_mode(n) != mode_M) {
      /*
       * Phis are "temporaries" and must be handled special:
       * They are avail, but are not in Antic_in
       */
      value_add(info->avail_out, n);
    }
  }
}

/**
 * Compare two nodes for equal operands.
 */
static int operands_equal(ir_node *n1, ir_node *n2)
{
  int i, arity;

  if (n1 == n2)
    return 1;

  arity = get_irn_arity(n1);
  assert(n1->op == n2->op && arity == get_irn_arity(n2));
  for (i = 0; i < arity; ++i)
    if (! operands_equal(get_irn_n(n1, i), get_irn_n(n2, i)))
      return 0;
  return 1;
}

/**
 * Replace a value in a set by an node computing the same
 * value in a dominator block.
 *
 * @return non-zero if a replacement took place
 */
static int value_replace(pset *set, ir_node *e)
{
  ir_node *old = value_add(set, e);

  if (old != e) {
    /* e must dominate old here */
    assert(block_dominates(get_nodes_block(e), get_nodes_block(old)));

    pset_remove(set, old, ir_node_hash(old));
    value_add(set, e);
    return 1;
  }
  return 0;
}

static ir_node *create_shadow(ir_node *n, ir_node *block)
{
  ir_mode *mode = get_irn_mode(n);
  ir_node *nn;
  nn = new_ir_node(
              get_irn_dbg_info(n),
              current_ir_graph, block,
              get_irn_op(n),
              mode,
              get_irn_arity(n),
              get_irn_in(n) + 1);
  copy_node_attr(n, nn);
  return nn;
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
  pre_env *env = ctx;
  ir_node *e, *idom, *first_s;
  block_info *curr_info, *idom_info;
  int pos, arity = get_irn_intra_arity(block);
  int all_same, by_some, updated;

  /* ensure that even the start block has a new_set */
  curr_info = get_block_info(block);
  if (curr_info->new_set)
    del_value_set(curr_info->new_set);
  curr_info->new_set = new_value_set();

  if (block == env->start_block)
    return;

  idom = get_Block_idom(block);
  idom_info = get_block_info(idom);

  /* update the new_sets */
  updated = 0;
  dump_set(idom_info->new_set, "[New Set]", idom);
  pset_foreach(e, idom_info->new_set) {
    value_add(curr_info->new_set, e);
    updated |= value_replace(curr_info->avail_out, e);
  }
  if (updated)
    dump_set(curr_info->avail_out, "Updated [Avail_out]", block);

  if (arity <= 1)
    return;

  pset_foreach(e, curr_info->antic_in) {
    ir_mode *mode;
    /*
     * If we already have a leader for this node,
     * it is totally redundant.
     */
    if (has_Phi_leader(e))
      continue;

    /* If the value was already computed in the dominator, then
       it is totally redundant.  Hence we have nothing to insert. */
    if (lookup(idom_info->avail_out, e)) {
//      DB((dbg, LEVEL_2, "Found %+F from block %+F avail in dom %+F\n", v, block, idom));
      continue;
    }

    by_some  = 0;
    all_same = 1;
    first_s  = NULL;
    mode     = NULL;

    /* for all predecessor blocks */
    for (pos = 0; pos < arity; ++pos) {
      block_info *pred_info;
      ir_node *pred_blk = get_Block_cfgpred_block(block, pos);
      ir_node *e_prime, *v_prime, *e_dprime;

      /* ignore bad blocks. */
      if (is_Bad(pred_blk))
        continue;

      e_prime = phi_translate(e, block, pos, env);
      v_prime = e_prime;

      pred_info = get_block_info(pred_blk);
      e_dprime = find_leader(pred_info->avail_out, v_prime);

      if (e_dprime == NULL) {
        all_same = 0;
        pred_info->avail = e_prime;
        pred_info->not_found = 1;
      }
      else {
        mode     = get_irn_mode(e_dprime);
        e_dprime = e_dprime;
        pred_info->avail = e_dprime;
        pred_info->not_found = 0;
        by_some = 1;
        if (first_s == NULL)
          first_s = e_dprime;
        else if (first_s != e_dprime)
          all_same = 0;

        DB((dbg, LEVEL_2, "Found %+F from block %+F as %+F in pred %+F\n", e, block, e_dprime, pred_blk));
      }  /* if */
    }  /* for */

    /* If it's not the same value already existing along every predecessor, and
       it's defined by some predecessor, it is partially redundant. */
    if (! all_same && by_some) {
      ir_node *phi, **in;
      DB((dbg, LEVEL_1, "Partial redundant %+F from block %+F found\n", e, block));

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
          ir_node *e_prime = pred_info->avail;
          ir_node *nn;
          if (!is_Phi(e_prime)) {
            mode = get_irn_mode(e_prime);
            nn = new_ir_node(
              get_irn_dbg_info(e_prime),
              current_ir_graph, pred_blk,
              get_irn_op(e_prime),
              mode,
              get_irn_arity(e_prime),
              get_irn_in(e_prime) + 1);
            copy_node_attr(e_prime, nn);

            DB((dbg, LEVEL_2, "New node %+F in block %+F created\n", nn, pred_blk));
            pred_info->avail = value_add(pred_info->avail_out, nn);
          }
        }
        in[pos] = pred_info->avail;
      }  /* for */
      phi = new_r_Phi(current_ir_graph, block, arity, in, mode);
      free(in);
      value_add(curr_info->avail_out, phi);
      value_add(curr_info->new_set, phi);
      DB((dbg, LEVEL_2, "New %+F(%+F) for redundant %+F(%+F) created\n",
        phi, get_nodes_block(phi), e, get_nodes_block((e))));

      /* the good case: we really replace an instruction */
      if (get_nodes_block(e) == block) {
        set_irn_link(e, phi);
      }
      else {
        ir_node *shadow = create_shadow(e, block);
        set_irn_link(shadow, phi);
        value_add(curr_info->avail_out, shadow);
        value_add(curr_info->new_set, shadow);
      }

      env->changes |= 1;
    }  /* if */
  }  /* pset_foreach */
}  /* insert_nodes */

/**
 * Do the elimination step: collect all changes
 * We cannot do the changes right here, as this would change
 * the hash values of the nodes in the avail_out set!
 */
static void collect_elim_pairs(ir_node *block, void *ctx)
{
  pre_env *env = ctx;
  block_info *curr_info = get_block_info(block);
  ir_node *v;

  dump_set(curr_info->nodes, "Updating nodes", block);
  pset_foreach(v, curr_info->nodes) {
    ir_node *l = find_leader(curr_info->avail_out, v);

    assert(l);
    if (l != v) {
      elim_pair *p = obstack_alloc(env->obst, sizeof(*p));

      p->old_node = v;
      p->new_node = l;
      p->next     = env->pairs;
      env->pairs  = p;
    }
  }
}

/**
 * Do all the recorded changes.
 */
static void eliminate_nodes(elim_pair *pairs)
{
  elim_pair *p;

  for (p = pairs; p != NULL; p = p->next) {
    DB((dbg, LEVEL_2, "Replacing %+F by %+F\n", p->old_node, p->new_node));
    exchange(p->old_node, p->new_node);
  }
}

void do_gvn_pre(ir_graph *irg)
{
  struct obstack obst;
  pre_env a_env;
  optimization_state_t state;
  block_info *p;
  int iter = 0;

  /* register a debug mask */
  dbg = firm_dbg_register("firm.opt.gvn_pre");
  //firm_dbg_set_mask(dbg, SET_LEVEL_2);

  obstack_init(&obst);
  a_env.obst        = &obst;
  a_env.trans_set   = new_value_set();
  a_env.list        = NULL;
  a_env.start_block = get_irg_start_block(irg);
  a_env.end_block   = get_irg_end_block(irg);
  a_env.pairs       = NULL;

  /* Move Proj's into the same block as their args,
     else we would assign the result to wrong blocks */
  normalize_proj_nodes(irg);

  /* critical edges MUST be removed */
  remove_critical_cf_edges(irg);

  /* we need dominator for Antic_out calculation */
  if (get_irg_dom_state(irg) != dom_consistent)
    compute_doms(irg);
  /* we get all nodes of a block by following outs */
  if (get_irg_outs_state(irg) != outs_consistent)
    compute_irg_outs(irg);

  /*
   * Switch on GCSE. We need it to correctly compute
   * the leader of a node by hashing.
   */
  save_optimization_state(&state);
  set_opt_global_cse(1);

  DB((dbg, LEVEL_1, "Doing GVN-PRE for %e\n", get_irg_entity(irg)));
  printf("Doing GVN-PRE for %s\n", get_entity_name(get_irg_entity(irg)));

  /* allocate block info for all blocks */
  irg_block_walk_graph(irg, NULL, alloc_blk_info, &a_env);

  /* compute the available value sets for all blocks */
  dom_tree_walk_irg(irg, compute_avail_top_down, NULL, &a_env);

  /* compute the anticipated value sets for all blocks */
  inc_irg_visited(irg);
  do {
    DB((dbg, LEVEL_1, "Antic_in Iteration %d starts ...\n", ++iter));
    a_env.changes = 0;
    irg_block_walk_graph(irg, compute_antic, NULL, &a_env);
    DB((dbg, LEVEL_1, "------------------------\n"));
  } while (a_env.changes != 0);

  /* compute redundant expressions */
  iter = 0;
  do {
    DB((dbg, LEVEL_1, "Insert Iteration %d starts ...\n", ++iter));
    a_env.changes = 0;
    dom_tree_walk_irg(irg, insert_nodes, NULL, &a_env);
    DB((dbg, LEVEL_1, "------------------------\n"));
  } while (a_env.changes != 0);

  /* last step: eliminate nodes */
  dom_tree_walk_irg(irg, collect_elim_pairs, NULL, &a_env);
  eliminate_nodes(a_env.pairs);

  restore_optimization_state(&state);

  /* clean up: delete all sets */
  for (p = a_env.list; p != NULL; p = p->next) {
    if (p->antic_in)
      del_value_set(p->antic_in);
    if (p->avail_out)
      del_value_set(p->avail_out);
    if (p->nodes)
      del_value_set(p->nodes);
    if (p->new_set)
      del_value_set(p->new_set);
  }
  del_value_set(a_env.trans_set);
  obstack_free(&obst, NULL);
  set_irg_pinned(irg, op_pin_state_pinned);

  if (a_env.pairs) {
    set_irg_outs_inconsistent(irg);
    set_irg_loopinfo_inconsistent(irg);
  }
}  /* do_gvn_pre */
