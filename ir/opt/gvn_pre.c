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

/**
 * @file
 * @brief   Global Value Numbering Partial Redundancy Elimination
 *          (VanDrunen Hosking 2004)
 * @author  Michael Beck, Rubino Geiss
 * @version $Id$
 * @summary
 *
 * Currently completely broken because our sets do NOT preserve
 * the topological sort!
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "iroptimize.h"

#include <assert.h>

#include "irgraph_t.h"
#include "irgwalk.h"
#include "irdom.h"
#include "irouts.h"
#include "pset.h"
#include "set.h"
#include "irgopt.h"
#include "iropt_t.h"
#include "irprintf.h"
#include "irnode_t.h"
#include "ircons.h"
#include "irgmod.h"
#include "debug.h"
#include "xmalloc.h"

/** The debug module handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)


/** A value set. */
typedef struct set value_set;

/** A node set. */
typedef struct pset node_set;

/** An entry in the value set. */
typedef struct value_entry {
  ir_node *node;  /**< the node */
  ir_node *value; /**< the value of the node */
} value_entry;

/** Additional info we need for every block. */
typedef struct block_info {
  node_set *nodes;      /**< The set of nodes per block. */
  value_set *avail_out; /**< The Avail_out set for a block. */
  node_set *antic_in;   /**< The Antic_in set for a block. */
  value_set *new_set;   /**< The set of all new values for a block. */
  ir_node *avail;       /**< The get_map(avail, block) result. */
  int not_found;        /**< Non-zero, if avail was not found in this block. */
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
  node_set *trans_set;    /**< The set of all translated values. */
  ir_node *start_block;   /**< The start block of the current graph. */
  ir_node *end_block;     /**< The end block of the current graph */
  block_info *list;       /**< Links all block info entires for easier recovery. */
  elim_pair *pairs;       /**< A list of node pairs that must be eliminated. */
  char changes;           /**< Non-zero, if calculation of Antic_in has changed. */
  char first_iter;        /**< non-zero for first iteration */
} pre_env;

/* ----------  Functions for Node sets ---------- */

#define node_set_first(s)       pset_first(s)
#define node_set_next(s)        pset_next(s)
#define node_set_break(s)       pset_break(s)
#define node_set_foreach(v, s)  for ((v) = node_set_first(s); (v); (v) = node_set_next(s))

/**
 * Creates a new node set.
 */
static node_set *new_node_set(void) {
  return new_pset(identities_cmp, 8);
}

/**
 * Deletes a node set.
 */
static void del_node_set(node_set *set) {
  del_pset(set);
}

/**
 * Add a node to the set.
 */
static ir_node *node_add(node_set *set, ir_node *node) {
  return identify_remember(set, node);
}

/**
 * Remove a node from a node set.
 */
static void node_set_remove(node_set *set, ir_node *node) {
  pset_remove(set, node, ir_node_hash(node));
}

/**
 * Return the number of entries in a node set.
 */
static int node_set_count(node_set *set) {
  return pset_count(set);
}

#if 0
/** computes dst = dst \/ src for node sets */
static void node_union(node_set *dst, node_set *src)
{
  ir_node *entry;
  node_set_foreach(entry, src) {
    node_add(dst, entry);
  }
}
#endif

/**
 * Lookup a node in a node set.
 */
static ir_node *node_lookup(node_set *set, ir_node *n)
{
  return pset_find(set, n, ir_node_hash(n));
}


/* ----------  Functions for Value sets ---------- */

#define value_set_foreach(v, s) for ((v) = set_first(s); (v); (v) = set_next(s))

/**
 * calculate a hash value for a value represented by a node
 */
static unsigned value_hash(ir_node *value) {
  return ir_node_hash(value);
}

/**
 * Compare two value entries.
 */
static int value_cmp(const void *elt, const void *key, size_t size)
{
  const value_entry *e1 = elt;
  const value_entry *e2 = key;
  (void) size;

  return identities_cmp(e1->value, e2->value);
}

/** Create a new value set. */
static value_set *new_value_set(void) {
  return new_set(value_cmp, 8);
}

/** Deletes a value set. */
static void del_value_set(value_set *set) {
  del_set(set);
}

/**
 * Add a node node representing the value value to the set.
 */
static value_entry *value_add(value_set *set, ir_node *node, ir_node *value)
{
  value_entry key;
  key.node  = node;
  key.value = value;
  return set_insert(set, &key, sizeof(key), value_hash(value));
}

/** computes dst = dst \/ src for value sets */
static void value_union(value_set *dst, value_set *src)
{
  value_entry *entry;
  value_set_foreach(entry, src)
    value_add(dst, entry->node, entry->value);
}

/** computes dst = dst \/ (value_set)src for value sets */
static void value_union_nodes(value_set *dst, node_set *src)
{
  ir_node *n;
  node_set_foreach(n, src)
    value_add(dst, n, n);
}

/**
 * Lookup a value in a value set.
 */
static ir_node *value_lookup(value_set *value_set, ir_node *n)
{
  value_entry key, *e;

  key.value = n;
  e = set_find(value_set, &key, sizeof(key), value_hash(n));
  return e ? e->node : NULL;
}

/**
 * Add or replace a value in a set by an node computing the same
 * value in a dominator block.
 *
 * @return non-zero if a replacement took place
 */
static int value_add_or_replace(value_set *set, ir_node *node, ir_node *value)
{
  value_entry *e = value_add(set, node, value);

  if (e->node != node) {
    /* node must dominate old one here */
    assert(block_dominates(get_nodes_block(node), get_nodes_block(e->node)));

    e->node = node;
    return 1;
  }
  return 0;
}

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

#ifdef DEBUG_libfirm
/**
 * Dump a set.
 */
static void dump_node_set(node_set *set, char *txt, ir_node *block)
{
  ir_node *n;
  int i;

  DB((dbg, LEVEL_2, "%s(%+F) = {\n", txt, block));
  i = 0;
  node_set_foreach(n, set) {
    if ((i & 3) == 3)
      DB((dbg, LEVEL_2, "\n"));
    DB((dbg, LEVEL_2, " %+F,", n));
    ++i;
  }
  DB((dbg, LEVEL_2, "\n}\n"));
}  /* dump_set */

/**
 * Dump a value set.
 */
static void dump_value_set(value_set *set, char *txt, ir_node *block)
{
  value_entry *e;
  int i;

  DB((dbg, LEVEL_2, "%s(%+F) = {\n", txt, block));
  i = 0;
  value_set_foreach(e, set) {
    if ((i & 3) == 3)
      DB((dbg, LEVEL_2, "\n"));
    if (e->node != e->value)
      DB((dbg, LEVEL_2, " %+F(%+F),", e->node, e->value));
    else
      DB((dbg, LEVEL_2, " %+F,", e->node));
    ++i;
  }
  DB((dbg, LEVEL_2, "\n}\n"));
}  /* dump_set */

#else
#define dump_node_set(set, txt, block)
#define dump_value_set(set, txt, block)
#endif /* DEBUG_libfirm */


/**
 * Return the block info of a block
 */
static block_info *get_block_info(ir_node *block) {
  return get_irn_link(block);
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
  value_union_nodes(info->avail_out, info->nodes);

  dump_value_set(info->avail_out, "Avail_out", block);
}

/**
 * returns non-zero if a tree node must be copied because of
 * a phi_translate.
 */
static int need_copy(ir_node *node, ir_node *block)
{
  int i, arity;

  /* Phi always stop the recursion */
  if (is_Phi(node))
    return get_irn_intra_n(node, -1) == block;

  if (! is_nice_value(node))
    return 0;

  /* check predecessor */
  arity = get_irn_intra_arity(node);
  for (i = 0; i < arity; ++i) {
    ir_node *pred     = get_irn_intra_n(node, i);
    ir_node *local_bl = get_irn_intra_n(pred, -1);
    ir_node *leader   = value_lookup(get_block_info(local_bl)->avail_out, pred);

    pred = leader != NULL ? leader : pred;
    if (need_copy(pred, block))
      return 1;
  }
  return 0;
}

/**
 * Translate a node
 */
static ir_node *translate(ir_node *node, ir_node *block, int pos, pre_env *env)
{
  int i, arity, need_new;
  ir_node *res, *nn, **in;

  /* Phi always stop the recursion */
  if (is_Phi(node)) {
    if (get_irn_intra_n(node, -1) == block)
      return get_Phi_pred(node, pos);
    return node;
  }

  if (! is_nice_value(node))
    return node;

  arity = get_irn_intra_arity(node);
  if (arity > 0) {
    NEW_ARR_A(ir_node *, in, arity);
    i = arity - 1;
    need_new = 0;
    do {
      ir_node *pred = get_irn_intra_n(node, i);
      ir_node *pred_blk = get_irn_intra_n(pred, -1);
      ir_node *leader = value_lookup(get_block_info(pred_blk)->avail_out, pred);
      in[i] = translate(leader ? leader : pred, block, pos, env);
      need_new |= (in[i] != pred);
      --i;
    } while(i >= 0);
    if (! need_new)
      return node;

    /* create a copy */
    nn = new_ir_node(
          get_irn_dbg_info(node),
          current_ir_graph,
          get_Block_cfgpred_block(block, pos),
          get_irn_op(node),
          get_irn_mode(node),
          arity,
          in);
    /* We need the attribute copy here, because the Hash value of a
       node might depend on that. */
    copy_node_attr(node, nn);
    res = node_add(env->trans_set, nn);
    if (nn != res)
      obstack_free(env->obst, nn);
    else
      DB((dbg, LEVEL_2, "--> Translate %+F in <%+F,%d> into %+F\n", node, block, pos, res));
    return res;
  }
  return node;
}

#if 0
/**
 * Implements phi_translate.
 */
static ir_node *deep_phi_translate(ir_node *node, ir_node *block, int pos, pre_env *env)
{
  struct obstack *old;
  ir_node *res;

  if (! need_copy(node, block))
    return node;

  /* Create a copy of the node in the pos'th predecessor block.
     Use our environmental obstack, as these nodes are always
     temporary. */
  old = current_ir_graph->obst;
  current_ir_graph->obst = env->obst;
  res = translate(node, block, pos, env);
  current_ir_graph->obst = old;

  return res;
}  /* phi_translate */
#endif

/**
 * Implements phi_translate.
 */
static ir_node *phi_translate(ir_node *node, ir_node *block, int pos, pre_env *env)
{
  ir_node *nn, *res;
  int i, arity;
  struct obstack *old;

  if (is_Phi(node)) {
    if (get_irn_intra_n(node, -1) == block)
      return get_Phi_pred(node, pos);
    return node;
  }

  arity = get_irn_intra_arity(node);

  /* check if the node has at least one Phi predecessor */
  for (i = 0; i < arity; ++i) {
    ir_node *pred    = get_irn_intra_n(node, i);
    ir_node *pred_bl = get_irn_intra_n(pred, -1);
    ir_node *leader  = value_lookup(get_block_info(pred_bl)->avail_out, pred);

    leader = leader != NULL ? leader : pred;
    if (is_Phi(leader) && get_irn_intra_n(pred, -1) == block)
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
          NULL,
          get_irn_op(node),
          get_irn_mode(node),
          arity,
          get_irn_in(node));
  /* We need the attribute copy here, because the Hash value of a
     node might depend on that. */
  copy_node_attr(node, nn);

  set_irn_n(nn, -1, get_irn_intra_n(node, -1));
  for (i = 0; i < arity; ++i) {
    ir_node *pred    = get_irn_intra_n(node, i);
    ir_node *pred_bl = get_irn_intra_n(pred, -1);
    ir_node *leader  = value_lookup(get_block_info(pred_bl)->avail_out, pred);

    leader = leader != NULL ? leader : pred;
    if (is_Phi(leader) && get_irn_intra_n(pred, -1) == block)
      set_irn_n(nn, i, get_Phi_pred(leader, pos));
    else
      set_irn_n(nn, i, leader);
  }
  res = node_add(env->trans_set, nn);
  current_ir_graph->obst = old;

  if (nn != res)
    obstack_free(env->obst, nn);
  else {
    DB((dbg, LEVEL_2, "--> Translate %+F in <%+F,%d> into %+F\n", node, block, pos, res));
  }
  return res;
}  /* phi_translate */

/**
 * check if a node n is clean in block block.
 */
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

/**
 * check if a node n is clean.
 */
static int is_clean(ir_node *n)
{
  int res = _is_clean(n, get_nodes_block(n));
  return res;
}

/**
 * Clean a node set.
 * This function is called for node sets with is_clean
 * nodes only, so we must just remove nodes that don't
 * have available inputs
 */
static void clean_node_set(node_set *set, ir_node *blk)
{
  ir_node *n, *pred, *pred_blk;
  int i;

restart:
  for (n = node_set_first(set); n; n = node_set_next(set)) {
    for (i = get_irn_intra_arity(n) - 1; i >= 0; --i) {
      pred = get_irn_intra_n(n, i);

      pred_blk = get_irn_intra_n(pred, -1);
      if (block_dominates(pred_blk, blk))
        continue;
      /* pred do not dominate it, but may be in the set */
      if (node_lookup(set, pred) != NULL)
        continue;
      /* we found a node that must be removed */
      node_set_break(set);
      node_set_remove(set, n);
      DB((dbg, LEVEL_2, "<-- Cleaning %+F\n", n));
      goto restart;
    }
  }
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

  size = node_set_count(info->antic_in);

  /* the end block has no successor */
  if (block != env->end_block) {
    int n_succ = get_Block_n_cfg_outs(block);

    if (n_succ == 1) {
      ir_node *node, *list;
      int i, pos = -1;

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
      /* translate into list: we cannot insert into a set we iterate
       * and succ might be equal to block for endless loops */
      list = NULL;
      node_set_foreach(node, succ_info->antic_in) {
        set_irn_link(node, list);
        list = node;
      }
      for (node = list; node; node = get_irn_link(node)) {
        ir_node *trans = phi_translate(node, succ, pos, env);

        if (is_clean(trans))
          node_add(info->antic_in, trans);
      }
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
      node_set_foreach(n, succ0_info->antic_in) {
        /* we need the disjoint */
        for (i = 1; i < n_succ; ++i) {
          ir_node *succ = get_Block_cfg_out(block, i);
          block_info *succ_info = get_block_info(succ);
          if (node_lookup(succ_info->antic_in, n) == NULL)
            break;
        }
        if (i >= n_succ) {
          /* we found a node that is common in all Antic_in(succ(b)),
             put it in Antic_in(b) */
          node_add(info->antic_in, n);
        }
      }
    }

    /*
     * This step calculates Antic_in(b) = Antic_out(b) \/ Nodes(b).
     * It is enough to do this in the first iteration, because
     * the set info->nodes is not changed anymore.
     */
    if (env->first_iter) {
      ir_node *n;
      node_set_foreach(n, info->nodes) {
        if (is_clean(n))
          node_add(info->antic_in, n);
      }
    }
  }

//  clean_node_set(info->antic_in, block);
  (void) clean_node_set;

  dump_node_set(info->antic_in, "Antic_in", block);
  if (size != node_set_count(info->antic_in)) {
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
  block_info *info = obstack_alloc(env->obst, sizeof(*info));

  set_irn_link(block, info);
  info->nodes     = new_node_set();
  info->antic_in  = new_node_set();
  info->avail_out = new_value_set();
  info->avail     = NULL;
  info->not_found = 0;
  info->new_set   = NULL;
  info->next      = env->list;
  env->list       = info;

  /* fill the nodes set, we will need it later */
  for (i = get_irn_n_outs(block) - 1; i >= 0; --i) {
    ir_node *n = get_irn_out(block, i);

    set_irn_link(n, NULL);

    /* we cannot optimize pinned nodes, so do not remember them */
    if (is_nice_value(n))
      node_add(info->nodes, n);
  }
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
  value_entry *entry;
  ir_node *e, *idom, *first_s, *worklist;
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
  dump_value_set(idom_info->new_set, "[New Set]", idom);
  value_set_foreach(entry, idom_info->new_set) {
    updated |= value_add_or_replace(curr_info->avail_out, entry->node, entry->value);
  }
  if (updated)
    dump_value_set(curr_info->avail_out, "Updated [Avail_out]", block);

  if (arity <= 1)
    return;

  /* convert the set into a list. This allows the removal of
   * elements from the set */
  worklist = NULL;
  node_set_foreach(e, curr_info->antic_in) {
    set_irn_link(e, worklist);
    worklist = e;
  }

  for (e = worklist; e != NULL; e = get_irn_link(e)) {
    ir_mode *mode;

    /* If the value was already computed in the dominator, then
       it is totally redundant.  Hence we have nothing to insert. */
    if (value_lookup(idom_info->avail_out, e)) {
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
      e_dprime = value_lookup(pred_info->avail_out, v_prime);

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
            pred_info->avail = value_add(pred_info->avail_out, nn, e_prime)->node;
          }
        }
        in[pos] = pred_info->avail;
      }  /* for */
      phi = new_r_Phi(current_ir_graph, block, arity, in, mode);
      free(in);
      value_add_or_replace(curr_info->avail_out, phi, e);
      value_add(curr_info->new_set, phi, e);
      DB((dbg, LEVEL_2, "New %+F for redundant %+F created\n", phi, e));

      /* the good case: we really replace an instruction */
      node_set_remove(curr_info->antic_in, e);

      env->changes |= 1;
    }  /* if */
  }  /* node_set_foreach */
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

  dump_node_set(curr_info->nodes, "Updating nodes", block);
  node_set_foreach(v, curr_info->nodes) {
    ir_node *l = value_lookup(curr_info->avail_out, v);

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
 * Do all the recorded changes and optimize
 * newly created Phi's.
 */
static void eliminate_nodes(elim_pair *pairs)
{
  elim_pair *p;

  for (p = pairs; p != NULL; p = p->next) {
    DB((dbg, LEVEL_2, "Replacing %+F by %+F\n", p->old_node, p->new_node));
    /*
     * PRE tends to create Phi(self, self, ... , x, self, self, ...)
     * which we can optimize here
     */
    if (is_Phi(p->new_node)) {
      int i;
      ir_node *res = NULL;

      for (i = get_irn_intra_arity(p->new_node) - 1; i >= 0; --i) {
        ir_node *pred = get_irn_n(p->new_node, i);

        if (pred != p->old_node) {
          if (res) {
            res = NULL;
            break;
          }
          res = pred;
        }
      }
      if (res)
        p->new_node = res;
    }
    exchange(p->old_node, p->new_node);
  }
}

/*
 * Argh: Endless loops cause problems, because the
 * insert algorithm did not terminate. We get translated nodes that
 * references the origin. These nodes are translated again and again...
 *
 * The current fix is to use post-dominance. This simple ignores
 * endless loops, ie we cannot optimize them.
 */
void do_gvn_pre(ir_graph *irg)
{
  struct obstack obst;
  pre_env a_env;
  optimization_state_t state;
  block_info *p;
  unsigned antic_iter, insert_iter;

  assert(!"COMPLETELY BROKEN YET, DO NOT USE");

  /* register a debug mask */
  FIRM_DBG_REGISTER(dbg, "firm.opt.gvn_pre");
  firm_dbg_set_mask(dbg, SET_LEVEL_2);

  obstack_init(&obst);
  a_env.obst        = &obst;
  a_env.trans_set   = new_node_set();
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
  if (get_irg_postdom_state(irg) != dom_consistent)
    compute_postdoms(irg);
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
  antic_iter = 0;
  a_env.first_iter = 1;
  do {
    DB((dbg, LEVEL_1, "Antic_in Iteration %d starts ...\n", ++antic_iter));
    a_env.changes = 0;
    irg_block_walk_graph(irg, compute_antic, NULL, &a_env);
//    postdom_tree_walk_irg(irg, compute_antic, NULL, &a_env);
    a_env.first_iter = 0;
    DB((dbg, LEVEL_1, "------------------------\n"));
  } while (a_env.changes != 0);

  /* compute redundant expressions */
  insert_iter = 0;
  do {
    DB((dbg, LEVEL_1, "Insert Iteration %d starts ...\n", ++insert_iter));
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
      del_node_set(p->antic_in);
    if (p->avail_out)
      del_value_set(p->avail_out);
    if (p->nodes)
      del_node_set(p->nodes);
    if (p->new_set)
      del_value_set(p->new_set);
  }
  del_node_set(a_env.trans_set);
  obstack_free(&obst, NULL);
  set_irg_pinned(irg, op_pin_state_pinned);

  if (a_env.pairs) {
    set_irg_outs_inconsistent(irg);
    set_irg_loopinfo_inconsistent(irg);
  }
}  /* do_gvn_pre */
