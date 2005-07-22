#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "hashptr.h"
#include "pdeq.h"
#include "pset.h"
#include "pmap.h"
#include "util.h"
#include "debug.h"

#include "irflag_t.h"
#include "ircons_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "irgopt.h"

#include "be_t.h"
#include "bearch.h"
#include "besched_t.h"
#include "belive_t.h"
#include "benode_t.h"

#include "beirgmod.h"

#define DBG_MODULE firm_dbg_register("firm.be.irgmod")

struct _dom_front_info_t {
  pmap *df_map;
};

static void compute_df_local(ir_node *bl, void *data)
{
  pmap *df_map = ((dom_front_info_t *) data)->df_map;
  ir_node *idom = get_Block_idom(bl);
  pset *df = pmap_get(df_map, bl);
  int i, n;

  /*
   * In the case that the immediate dominator is NULL, the
   * block is the start block and its immediate dominator
   * must be itself, else it is inserted into its own
   * dominance frontier.
   */
  if(idom == NULL)
  	idom = bl;

  /*
   * Create a new dom frot set for this node,
   * if none exists.
   */
  if(!df)
  	pmap_insert(df_map, bl, pset_new_ptr(16));

  for(i = 0, n = get_irn_arity(bl); i < n; ++i) {

    /* The predecessor block */
    ir_node *pred = get_nodes_block(get_irn_n(bl, i));

    /* The dominance frontier set of the predecessor. */
    pset *df = pmap_get(df_map, pred);
	  if(!df) {
	  	df = pset_new_ptr(16);
	  	pmap_insert(df_map, pred, df);
	  }

    assert(df && "dom front set must have been created for this node");

    if(pred != idom && bl)
      pset_insert_ptr(df, bl);
  }
}

static void compute_df_up(ir_node *bl, void *data)
{
  pmap *df_map = ((dom_front_info_t *) data)->df_map;
  ir_node *y;

  for(y = get_Block_dominated_first(bl); y; y = get_Block_dominated_next(y)) {
    ir_node *w;
    pset *df = pmap_get(df_map, y);

    for(w = pset_first(df); w; w = pset_next(df))
      if(!block_dominates(bl, w) || bl == w)
        pset_insert_ptr(df, w);
  }
}

dom_front_info_t *be_compute_dominance_frontiers(ir_graph *irg)
{
  dom_front_info_t *info = malloc(sizeof(*info));

  info->df_map = pmap_create();

  /*
   * This must be called as a post walker, since the dom front sets
   * of all predecessors must be created when a block is reached.
   */
  dom_tree_walk_irg(irg, NULL, compute_df_local, info);
  dom_tree_walk_irg(irg, NULL, compute_df_up, info);
  return info;
}

void be_free_dominance_frontiers(dom_front_info_t *info)
{
  pmap_entry *ent;

  for(ent = pmap_first(info->df_map); ent; ent = pmap_next(info->df_map))
    del_pset(ent->value);

  pmap_destroy(info->df_map);
  free(info);
}

pset *be_get_dominance_frontier(dom_front_info_t *info, ir_node *block)
{
  return pmap_get(info->df_map, block);
}

/**
 * Algorithm to place the Phi-Functions.
 * @see Appel, Modern Compiler Implementation in Java, 2nd ed., p. 399ff
 *
 * This function takes an original node and a set of already placed
 * copies of that node called @p copies. It places phi nodes at the
 * iterated dominance frontiers of these copies and puts these phi nodes
 * in the @p copies set, since they are another form of copies of the
 * original value.
 *
 * The rename phase (see below) is responsible for fixing up the usages
 * of the original node.
 *
 * @param orig The original node.
 * @param copies A set contianing nodes representing a copy of the
 * original node. Each node must be inserted into the block's schedule.
 * @param copy_blocks A set in which the blocks are recorded which
 * contain a copy. This is just for efficiency in later phases (see
 * rename).
 */
static void place_phi_functions(ir_node *orig, pset *copies,
    pset *copy_blocks, dom_front_info_t *df_info)
{
  int i;
  ir_node *orig_block = get_nodes_block(orig);
  ir_graph *irg = get_irn_irg(orig);
  ir_mode *mode = get_irn_mode(orig);
  pdeq *worklist = new_pdeq();
  pset *phi_blocks = pset_new_ptr(8);
  ir_node **ins = NULL;
  void *it;
  firm_dbg_module_t *dbg = DBG_MODULE;

  /*
   * Allocate an array for all blocks where the copies and the original
   * value were defined.
   */
  int n_orig_blocks = pset_count(copy_blocks);
  ir_node **orig_blocks = malloc(n_orig_blocks * sizeof(orig_blocks[0]));

  /*
   * Fill the worklist queue and the rest of the orig blocks array.
   */
  for(it = pset_first(copies), i = 0; it; it = pset_next(copies)) {
    ir_node *copy_block = get_nodes_block(it);

	/* FIXED_DANIEL
	 * Not true anymore, since phis can be copies of a value.
	 * Not all Phis are dominated by their args
	 *
    if(!block_dominates(orig_block, copy_block)) {
    	assert(block_dominates(orig_block, copy_block)
        	&& "The block of the copy must be dominated by the block of the value");
    }
    */

    pdeq_putr(worklist, copy_block);
    orig_blocks[i++] = copy_block;
  }

  while(!pdeq_empty(worklist)) {
    ir_node *bl = pdeq_getl(worklist);
    ir_node *y;
    pset *df = be_get_dominance_frontier(df_info, bl);

    DBG((dbg, LEVEL_3, "dom front of %+F\n", bl));
    for(y = pset_first(df); y; y = pset_next(df))
	    DBG((dbg, LEVEL_3, "\t%+F\n", y));

    for(y = pset_first(df); y; y = pset_next(df)) {
      int n_preds = get_irn_arity(y);

      if(!pset_find_ptr(phi_blocks, y)) {
        ir_node *phi;
        int insert = 1;

        /*
         * Set the orig node as the only operand of the
         * phi node.
         */
        ins = realloc(ins, n_preds * sizeof(ins[0]));
        for(i = 0; i < n_preds; ++i)
          ins[i] = orig;

        /* Insert phi node */
        phi = new_r_Phi(irg, y, n_preds, ins, mode);
        DBG((dbg, LEVEL_2, "    inserting phi %+F with %d args in block %+F\n",
              phi, n_preds, y));

        /*
         * The phi node itself is also a copy of the original
         * value. So put it in the copies set also, so that
         * the rename phase can treat them right.
         */
        pset_insert_ptr(copies, phi);
        pset_insert_ptr(copy_blocks, y);

        /*
         * Insert the phi node into the schedule if it
         * can occur there (PhiM's are not to put into a schedule.
         */
        if(to_appear_in_schedule(phi))
          sched_add_before(sched_first(y), phi);

        /* Insert the phi node in the phi blocks set. */
        pset_insert_ptr(phi_blocks, y);

        /*
         * If orig or a copy of it were not defined in y,
         * add y to the worklist.
         */
        for(i = 0; i < n_orig_blocks; ++i)
          if(orig_blocks[i] == y) {
            insert = 0;
            break;
          }

        if(insert)
          pdeq_putr(worklist, y);

      }
    }
  }

  del_pset(phi_blocks);
  del_pdeq(worklist);

  free(orig_blocks);

  if(ins)
    free(ins);
}

/**
 * Find the copy of the given original node whose value is 'active'
 * at a usage.
 *
 * The usage is given as a node and a position. Initially, the given operand
 * points to a node for which copies were introduced. We have to find
 * the valid copy for this usage. This is done by travering the
 * dominance tree upwards. If the usage is a phi function, we start
 * traversing from the predecessor block which corresponds to the phi
 * usage.
 *
 * @param usage The node which uses the original node.
 * @param pos The number of the argument which corresponds to the
 * original node.
 * @param copy_blocks A set containing all basic block in which copies
 * of the original node are located.
 * @param copies A set containing all node which are copies from the
 * original node.
 * @return The valid copy for usage.
 */
static ir_node *search_def(ir_node *usage, int pos, pset *copies, pset *copy_blocks)
{
  ir_node *curr_bl;
  ir_node *start_irn;
  firm_dbg_module_t *dbg = DBG_MODULE;
  firm_dbg_set_mask(dbg, -1);

  curr_bl = get_nodes_block(usage);


  DBG((dbg, LEVEL_1, "Searching valid def for use %+F at pos %d\n", usage, pos));
  /*
   * If the usage is in a phi node, search the copy in the
   * predecessor denoted by pos.
   */
  if(is_Phi(usage)) {
    curr_bl = get_Block_cfgpred_block(curr_bl, pos);
    start_irn = sched_last(curr_bl);
  } else {
    start_irn = sched_prev(usage);
  }

  /*
   * Traverse the dominance tree upwards from the
   * predecessor block of the usage.
   */
  while(curr_bl != NULL) {

    /*
     * If this block contains a copy, search the block
     * instruction by instruction.
     */
    if(pset_find_ptr(copy_blocks, curr_bl)) {
      ir_node *irn;

      /* Look at each instruction from last to first. */
      for(irn = start_irn; !is_Block(irn); irn = sched_prev(irn)) {

        /* Take the first copy we find. */

        DBG((dbg, LEVEL_1, "Is %F a copy?\n", irn));
        if(pset_find_ptr(copies, irn))
          return irn;
      }
    }

    /* If were not done yet, look in the immediate dominator */
    curr_bl = get_Block_idom(curr_bl);
    if(curr_bl)
      start_irn = sched_last(curr_bl);
  }

  assert(0 && "Did not find a valid def");
  return NULL;
}

static void fix_usages(ir_node *orig, pset *copies, pset *copy_blocks)
{
  int i = 0;
  int n_outs = 0;
  const ir_edge_t *edge;
  firm_dbg_module_t *dbg = DBG_MODULE;

  struct {
    ir_node *irn;
    int pos;
  } *outs;

  /* Count the number of outs. */
  foreach_out_edge(orig, edge)
    n_outs++;

  /*
   * Put all outs into an array.
   * This is neccessary, since the outs would be modified while
   * interating on them what could bring the outs module in trouble.
   */
  DBG((dbg, LEVEL_2, "  Users of %+F\n", orig));
  outs = malloc(n_outs * sizeof(outs[0]));
  foreach_out_edge(orig, edge) {
    outs[i].irn = get_edge_src_irn(edge);
    outs[i].pos = get_edge_src_pos(edge);
    i += 1;
  }

  /*
   * Search the valid def for each out and set it.
   */
  for(i = 0; i < n_outs; ++i) {
    ir_node *def;
    ir_node *irn = outs[i].irn;
    int pos = outs[i].pos;

    DBG((dbg, LEVEL_2, "    %+F(%d) -> ???\n", irn, pos));
    def = search_def(irn, pos, copies, copy_blocks);
    DBG((dbg, LEVEL_2, "    %+F(%d) -> %+F\n", irn, pos, def));

    if(def != NULL)
      set_irn_n(irn, pos, def);
  }

  free(outs);
}

struct phi_collect_info {
  const ir_node *orig;
  pset *copies;
  pset *copy_blocks;
};

static void add_all_phis_walker(ir_node *irn, void *data)
{
  if(is_Phi(irn)) {
    int i, o, n;
    struct phi_collect_info *info = data;

    /*
     * Look at all operands of the phi. If one of them is the original
     * node, insert the phi into the copies and copy_blocks set.
     */
    for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
      if(get_irn_n(irn, i) == info->orig) {
        pset_insert_ptr(info->copies, irn);
        pset_insert_ptr(info->copy_blocks, get_nodes_block(irn));

		/* FIXED_DANIEL
		 * If a phi is a copy insert all its args as copies too.
		 * Else setting the phi-args will fail in serach_def */
        for(o=0; o<n; ++o) {
        	const ir_node *arg = get_irn_n(irn, o);
	        pset_insert_ptr(info->copies, arg);
	        pset_insert_ptr(info->copy_blocks, get_nodes_block(arg));
        }

        break;
      }
    }


  }
}

/**
 * Add all phis using a node to a set.
 * @param orig        The node the phis shall use.
 * @param copies      The set where the phis shall be put into.
 * @param copy_blocks The set the blocks of the phis shall be put into.
 */
static void add_all_phis_using(const ir_node *orig, pset *copies, pset *copy_blocks)
{
  struct phi_collect_info info;

  info.copies      = copies;
  info.copy_blocks = copy_blocks;
  info.orig        = orig;
  irg_walk_graph(get_irn_irg(orig), add_all_phis_walker, NULL, &info);
}

void be_introduce_copies(dom_front_info_t *info, ir_node *orig, int n, ir_node *copy_nodes[])
{
  pset *copies = pset_new_ptr(2 * n);
  pset *copy_blocks = pset_new_ptr(2 * n);
  int save_optimize = get_optimize();
  int save_normalize = get_opt_normalize();
  firm_dbg_module_t *dbg = DBG_MODULE;
  int i;

  firm_dbg_set_mask(dbg, -1);
  DBG((dbg, LEVEL_1, "Introducing following copies of %+F\n", orig));

  /* Fill the sets. */
  pset_insert_ptr(copies, orig);
  pset_insert_ptr(copy_blocks, get_nodes_block(orig));

  /*
   * All phis using the original value are also copies of it
   * and must be present in the copies set.
   */
  add_all_phis_using(orig, copies, copy_blocks);

  for(i = 0; i < n; ++i) {
    DBG((dbg, LEVEL_1,
          "  %+F in block %+F\n", copy_nodes[i], get_nodes_block(copy_nodes[i])));
    pset_insert_ptr(copies, copy_nodes[i]);
    pset_insert_ptr(copy_blocks, get_nodes_block(copy_nodes[i]));
  }

  /*
   * Disable optimization so that the phi functions do not
   * disappear.
   */
  set_optimize(0);
  set_opt_normalize(0);

  /*
   * Place the phi functions and reroute the usages.
   */
  place_phi_functions(orig, copies, copy_blocks, info);
  fix_usages(orig, copies, copy_blocks);

  /* reset the optimizations */
  set_optimize(save_optimize);
  set_opt_normalize(save_normalize);

  del_pset(copies);
  del_pset(copy_blocks);
}
