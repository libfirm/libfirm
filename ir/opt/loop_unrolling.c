/**
 *
 * @file loop_unrolling.c
 *
 * Project:     libFIRM
 * File name:   ir/opt/loop_unrolling.c
 * Purpose:     Make loop unrolling.
 * Author:      Beyhan Veliev
 * Modified by:
 * Created:     16.11.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# include "loop_unrolling.h"

# include "irgwalk.h"
# include "ircons.h"
# include "irgmod.h"
# include "irloop_t.h"
# include "irgopt_t.h"
# include "irnode_t.h"
# include "irouts.h"
# include "hashptr.h"
# include "pset.h"
# include "strength_red.h"

#define MAX_UNROLL 4

/*We will know if the head of the loop to be copy.
 * Default don't copy.*/
static int copy_loop_head = 0;

typedef struct {
  ir_node *irn ;                /* Node of the loop to be unrolling*/
  ir_node *copy[MAX_UNROLL] ;   /* The copy of the node */
} copies_t;

/**
 * compare two elements of the copies_t set
 */
static int set_cmp(const void *elt, const void *key, size_t size)
{
  const copies_t *c1 = elt;
  const copies_t *c2 = key;

  return c1->irn != c2->irn;
}

static INLINE int * new_backedge_arr(struct obstack *obst, int size)
{
  int *res = NEW_ARR_D (int, obst, size);
  memset(res, 0, sizeof(int) * size);
  return res;
}

static INLINE void new_backedge_info(ir_node *n) {
  switch(get_irn_opcode(n)) {
  case iro_Block:
    n->attr.block.cg_backedge = NULL;
    n->attr.block.backedge = new_backedge_arr(current_ir_graph->obst, get_irn_arity(n));
    break;
  case iro_Phi:
    n->attr.phi_backedge = new_backedge_arr(current_ir_graph->obst, get_irn_arity(n));
    break;
  case iro_Filter:
    n->attr.filter.backedge = new_backedge_arr(current_ir_graph->obst, get_irn_arity(n));
    break;
  default: ;
  }
}

/**
 * Remember the new node in the old node by using a field all nodes have.
 */

static INLINE void
set_new_node (ir_node *old, ir_node *new)
{
  old->link = new;
}

/**
 * Copies the node to the new obstack. The Ins of the new node point to
 * the predecessors on the old obstack.  For block/phi nodes not all
 * predecessors might be copied.  n->link points to the new node.
 * For Phi and Block nodes the function allocates in-arrays with an arity
 * only for useful predecessors.  The arity is determined by counting
 * the non-bad predecessors of the block.
 *
 * @param n    The node to be copied
 * @param env  if non-NULL, the node number attribute will be copied to the new node
 */

static void
copy_node (ir_node *n, void *env)
{
  ir_node *nn, *block;
  int new_arity;
  opcode op = get_irn_opcode(n);
  int copy_node_nr = env != NULL;

  /* The end node looses it's flexible in array.  This doesn't matter,
     as dead node elimination builds End by hand, inlineing doesn't use
     the End node. */
  /* assert(n->op == op_End ||  ((_ARR_DESCR(n->in))->cookie != ARR_F_MAGIC)); */

  if (op == iro_Bad)
    /* node copied already */
    return;

  new_arity = get_irn_arity(n);

  nn = new_ir_node(get_irn_dbg_info(n),
           current_ir_graph,
           block,
           get_irn_op(n),
           get_irn_mode(n),
           new_arity,
           get_irn_in(n));


  /* Copy the attributes.  These might point to additional data.  If this
     was allocated on the old obstack the pointers now are dangling.  This
     frees e.g. the memory of the graph_arr allocated in new_immBlock. */
  copy_node_attr(n, nn);
  new_backedge_info(nn);
  set_new_node(n, nn);

#if DEBUG_libfirm
  if (copy_node_nr) {
    /* for easier debugging, we want to copy the node numbers too */
    nn->node_nr = n->node_nr;
  }
#endif

}

static int is_Phi_in_loop_head(ir_node *phi, ir_node *loop_head) {
  assert(is_Block(loop_head));
  return is_Phi(phi) && (get_nodes_block(phi) == loop_head);
}

/**
 * Copies predecessors of node to copies of node.
 * If the predecessor is a loop invariant, then the copy get it
 * as predecessor, else the copy of the predecessor.
 *
 * @param *l_n                A set, where the node of the loop are saved.
 * @param *value              A element of the set.
 * @param *info               Contains information about the induction variable.
 * @param *unroll_factor      A integer power of 2.
 * @param *env                Free environment pointer.
 */
static void
set_preds (set *l_n, copies_t *value, induct_var_info *info, int unroll_factor, void *env)
{
  int i, irn_arity, p;
  copies_t *value_pred;

  ir_node *loop_head = get_loop_node(info->l_itervar_phi, 0);

  irn_arity = get_irn_arity(value->irn);

  for (i = 0; i < irn_arity; i++) {
    ir_node *pred = get_irn_n(value->irn, i);

    copies_t key;

    key.irn = pred;
    value_pred = set_find(l_n, &key, sizeof(key), HASH_PTR(pred));

    if (value->irn != loop_head && !is_Phi_in_loop_head(value->irn, loop_head)) {
      if (value_pred == NULL) {
      /* Is loop invariant. */
    for(p = 0; p < unroll_factor -1; p++)
      set_irn_n (value->copy[p], i, pred);

      } else
    for(p = 0; p < unroll_factor -1; p++)
      set_irn_n (value->copy[p], i, value_pred->copy[p]);
    }
  }
}

/* Set the backedge of phi in the loop head.The backedge of phis in the loop head
 * must now point to the value defined
 * in the last copie of the loop body.
 *
 * @param *l_n                A set, where the node of the loop are saved.
 * @param *value              A phi in the loop head.
 * @param *info               Contains information about the induction variable.
 * @param *unroll_factor      A integer power of 2.
 */
static void
set_phi_backedge(set *l_n, copies_t *value, induct_var_info *info, int unroll_factor)
{
  copies_t key, *value_pred;
  key.irn = get_irn_n(value->irn, info->op_pred_pos);
  value_pred = set_find(l_n, &key, sizeof(key), HASH_PTR(key.irn));

  set_Phi_pred(value->irn, info->op_pred_pos, value_pred->copy[unroll_factor - 2]);
}

/** Test for a loop head.
 *
 *  Returns true if the node has predecessors in the loop _and_ out of
 *  the loop.  Then it is a loop head: The loop can be entered through
 *  this node.
 *
 *  @param *n      The node to be tested.
 *  @param *info   Contains the loop information.
 */
static int
is_loop_head(ir_node *n, induct_var_info *info)
{
  int i, arity;
  int some_outof_loop = 0, some_in_loop = 0;

  assert(get_irn_op(n) == op_Block);
  arity = get_Block_n_cfgpreds(n);

  for (i = 0; i < arity; i++) {
    ir_node *pred = get_Block_cfgpred(n, i);
    assert(pred);
    if (is_loop_invariant(pred, get_loop_node(info->l_itervar_phi, 0))) {
      some_outof_loop = 1;
    } else
      some_in_loop = 1;
  }

  return some_outof_loop && some_in_loop;
}


/** Test wether the passed loop is a natural loop.
 *
 * Returns true if the loop has only one loop header and only a single
 * back edge.
 *
 * @param *info  Contains the loop information.
 */
static int
is_natural_loop ( induct_var_info *info)
{
  ir_node *l_node;
  int i, l_n_node = 0;
  l_n_node = get_loop_n_nodes (info->l_itervar_phi);

  for (i = 1; i < (l_n_node); i ++) {
    l_node = get_loop_node (info->l_itervar_phi, i);
    if (is_loop_head(l_node, info)) return 0;

    if (has_backedges(l_node) && i != l_n_node-1) return 0;
  }

  return 1;
}

/** Serch for all nodes of a loop.
 *
 * @param  *node       The induction variable of the loop.
 * @param  *loop_head  The head of the loop.
 * @param  *l_n        A set, where the node of the loop are saved.
 */
static void
find_loop_nodes(ir_node *node, ir_node *loop_head, set *l_n)
{
  int i;
  copies_t key, *value;

  /* Add this node to the list. */
  key.irn  = node;
  for(i = 0; i < 4 ;i++)
    key.copy[i] = NULL;
  value = set_insert(l_n, &key, sizeof(key), HASH_PTR(key.irn));

  /* Add all outs of this node to the list, if they are within the loop. */
  for (i = get_irn_n_outs(node) - 1; i >= 0; i--) {
    ir_node *pred = get_irn_out(node, i);
    key.irn = pred;
    if (!is_loop_invariant(pred, loop_head)         &&
    set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn)) == NULL ) {
      find_loop_nodes(pred, loop_head, l_n);
    }
  }

  /* Add all ins if they are within the loop. */
  for(i = get_irn_arity(node) -1; i >=0; i--) {
    ir_node *pred = get_irn_n(node, i);
    key.irn = pred;
    if (!is_loop_invariant(pred, loop_head)         &&
    set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn)) == NULL ){
      find_loop_nodes(pred, loop_head, l_n);
    }
  }
}

/* Make a new loop head if copy_loop_head = 1.
 *
 * @param *l_n              A set, where the node of the loop are saved.
 * @param *info             Contains the loop information.
 * @param *value            A element of the set.
 * @param *unroll_factor    A integer power of 2.
 *
 */
static void
new_loop_head (set *l_n, induct_var_info *info, copies_t *value, int unroll_factor)
{
  copies_t block, *value_backedge_jmp, *backedge_jmp_block;
  int i;

  ir_node *backedge_jmp = get_Block_cfgpred(value->irn, info->op_pred_pos);
  block.irn = backedge_jmp;

  value_backedge_jmp = set_find( l_n, &block, sizeof(block), HASH_PTR(block.irn));
  if(copy_loop_head){
    ir_node *new_loop_head = new_Block(1, &backedge_jmp);
    value->copy[0] = new_loop_head;

    for(i = 1; i<unroll_factor - 1; i++){
      ir_node *new_loop_head1 = new_Block(1, &value_backedge_jmp->copy[i-1]);
      value->copy[i] = new_loop_head1;
    }
  }else{

    block.irn = get_nodes_block(backedge_jmp);
    backedge_jmp_block =  set_find( l_n, &block, sizeof(block), HASH_PTR(block.irn));

    set_irn_n(backedge_jmp_block->copy[0], 0, value_backedge_jmp->irn) ;

    for(i = 1; i<unroll_factor - 1; i++)
      set_irn_n(backedge_jmp_block->copy[i], 0, value_backedge_jmp->copy[i - 1]);
  }

}

/* Set all copies of the induction variable.
 *
 * @param *phi             A phi node in the loop head block.
 * @param *phi_pred        The predecessor of the phi along the backedge.
 * @param *unroll_factor   A integer power of 2.
 *
 */
static void
set_Phi_copies(copies_t *phi, copies_t *phi_pred, int unroll_factor)
{
  int p;
  phi->copy[0] = phi_pred->irn;
  for(p = 1; p < unroll_factor -1; p++)
    phi->copy[p] =  phi_pred->copy[p -1];
}

/* Decide if the loop head to be copy. A head with important nodes
 * mus be copy.
 *
 * @param *l_n                A set, where the node of the loop are saved.
 * @param *info               Contains information about the induction variable.
 */
static void
loop_head_nodes(set *l_n, induct_var_info *info)
{
  copies_t *value;
  ir_node *loop_head = get_loop_node(info->l_itervar_phi, 0);

  for (value = set_first(l_n); value != NULL; value = set_next(l_n))
    if(value->irn->op != op_Block &&
       get_nodes_block(value->irn) == loop_head)
      switch(get_irn_opcode(value->irn)) {
      case iro_Cond:
    break;
      case iro_Phi:
    break;
      case iro_Proj:
    break;
      case iro_Const:
    break;
      case iro_Cmp:
    break;
      default:
    copy_loop_head = 1;
      }
}

/** Copy all loop nodes.
 *
 * @param *l_n    Contains all nodes of the loop.
 * @param *info   Contains the loop information.
 * @param *unroll_factor   A integer power of 2.
 */
static void
copy_loop_body(set *l_n, induct_var_info *info, int unroll_factor)
{
  copies_t *value, *info_op, *phi, *loop_h, key, *value1;
  int i;
  ir_node *loop_head = get_loop_node(info->l_itervar_phi, 0);


  for (value = set_first(l_n); value != NULL; value = set_next(l_n)) {
    if(value->irn == loop_head)
      loop_h = value;
    else if (is_Phi_in_loop_head(value->irn, loop_head))
      phi = value;
    else if(copy_loop_head){
      for (i = 0; i<unroll_factor -1; i++){
    copy_node(value->irn, NULL);
    value->copy[i] = get_irn_link(value->irn);
      }
    } else {
      if((value->irn->op == op_Block            &&
      value->irn != loop_head)              ||
     (value->irn->op != op_Block             &&
      get_nodes_block(value->irn) != loop_head))
    for (i = 0; i<unroll_factor -1; i++){
      copy_node(value->irn, NULL);
      value->copy[i] = get_irn_link(value->irn);
    }
    }
  }
  /* Treat the loop head block */
  new_loop_head (l_n, info, loop_h, unroll_factor);

  /* Similarily treat the Phis in the loop head block. */
  key.irn = info->op;
  info_op = set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn));
  assert(info_op->irn == get_Phi_pred(info->itervar_phi, info->op_pred_pos));
  for (i = 0; i < get_irn_n_outs(loop_head); ++i) {
    ir_node *phi = get_irn_out(loop_head, i);

    if (is_Phi(phi)) {
      key.irn = get_Phi_pred(phi, info->op_pred_pos); // info->op;
      copies_t *phi_pred = set_find(l_n, &key, sizeof(key), HASH_PTR(key.irn));
      key.irn = phi;
      copies_t *phi_op = set_find(l_n, &key, sizeof(key), HASH_PTR(key.irn));
      set_Phi_copies(phi_op, phi_pred, unroll_factor);
    }
  }


  for (value = set_first(l_n); value != NULL; value = set_next(l_n)) {
    int p;

    if(copy_loop_head)
      set_preds(l_n, value, info, unroll_factor, NULL);
    else if((value->irn->op != op_Block) && get_nodes_block(value->irn) != loop_head)
      set_preds(l_n, value, info, unroll_factor, NULL);

    if (is_Phi_in_loop_head(value->irn, loop_head))
      set_phi_backedge(l_n, value, info, unroll_factor);

    if ((value->irn->op != op_Block) && !is_Phi_in_loop_head(value->irn, loop_head)) {
      ir_node *nodes_block = get_nodes_block(value->irn);

      if(!copy_loop_head && nodes_block == loop_head)
    continue;

      key.irn = nodes_block;
      value1 = set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn));

      for(p = 0; p < unroll_factor - 1; p++){
    set_nodes_block(value->copy[p], value1->copy[p]);
    //add_End_keepalive(get_irg_end(current_ir_graph), value->copy[p]);
      }
    }
  }
}

static void
set_loop_outs(set *l_n, induct_var_info *info, int unroll_factor)
{
  copies_t *value, key;
  int p, i;
  ir_node *loop_head = get_loop_node(info->l_itervar_phi, 0);

  value = set_first(l_n);
  for( ; value != NULL; value = set_next(l_n))
    if(value->irn != info->op && !is_Phi_in_loop_head(value->irn, loop_head) &&
       get_irn_opcode(value->irn) != iro_Proj)
    for(i = 0; i < get_irn_n_outs(value->irn); i++){
      key.irn = get_irn_out(value->irn, i);
    if(set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn)) == NULL)
      for(p = 0; p < get_irn_arity(key.irn); p++)
        if(value->irn == get_irn_n(key.irn, p))
          set_irn_n (key.irn, p, value->copy[unroll_factor-2]);
    }
}


/** Unroll the loop boby with a factor that must be power of two.
 *
 *  @param *n        A ir node.
 *  @param *env      Free environment pointer.
 */
static void do_loop_unroll(ir_node *n, void *env){

  induct_var_info info;
  info.itervar_phi = n;
  int l_sons = 0, unroll_factor = 0;

  /* The ir node must be a induction varible. */

  if (get_irn_op (n) == op_Phi) {
    if (is_induction_variable (&info) == NULL) return;
  } else return;

  /* Brute force limiting of loop body size. */
  if (get_loop_n_nodes(info.l_itervar_phi) > 2 ) return;

  /* Only unroll loops that compare against a constant for exiting. */
  if (info.cmp == NULL) return;

  /* We only want to unroll innermost loops. */
  l_sons = get_loop_n_sons (info.l_itervar_phi);
  if ( l_sons != 0)
    return;

  ir_node* cmp_out = get_irn_out(info.cmp, 0);

  if(!is_Proj(cmp_out)) return;
  if(get_irn_op(info.increment) != op_Const) return;

  int cmp_typ =  get_Proj_proj(cmp_out);
  int init = get_tarval_long(get_Const_tarval
                  (get_Phi_pred(info.itervar_phi, info.init_pred_pos)));
  int iter_end = get_tarval_long(get_Const_tarval(info.cmp_const));
  int iter_increment = get_tarval_long(get_Const_tarval(info.increment));
  int diff, iter_number;

  if(iter_end < init){
    int p = iter_end;
    iter_end = init;
    init = p;
  }

  iter_increment = iter_increment < 0 ? -iter_increment : iter_increment;
  diff = iter_end - init;

  if (diff == 0 || iter_increment == 0) return;

  iter_number = diff/iter_increment;
  if((cmp_typ == 3 || cmp_typ == 5) && (iter_end % iter_increment == 0))
    iter_number ++;

  if(iter_number % 4 == 0)
    unroll_factor = 4;
  else if(iter_number % 3 == 0)
    unroll_factor = 3;
  else if(iter_number % 2 == 0)
    unroll_factor = 2;
  else return;


  printf("\ninit %d,\n iter_end %d, \n diff %d, cmp_typ\n %d, \n unroll_factor %d", init, iter_end, diff, cmp_typ, unroll_factor);

  // int unroll_factor = 4;  /* Must be power of 2. */
  assert(unroll_factor <= MAX_UNROLL);

  ir_node *loop_head;

  loop_head = (is_natural_loop(&info)) ? get_loop_node(info.l_itervar_phi, 0) : NULL;

  assert(loop_head != NULL && is_Block(loop_head));

 /* We assume, the loop head has exactly one backedge.  The position of
    the backedge is in the following variable: */
  int backedge_pos ;
  backedge_pos = (is_backedge(loop_head, 0)) ? 0:1;

  /* A set with the nodes to copy. */
  set *loop_nodes;
  loop_nodes = new_set(set_cmp, 8);

  ir_node *backedge_jmp = get_Block_cfgpred(loop_head, backedge_pos);

  find_loop_nodes(info.itervar_phi, get_loop_node(info.l_itervar_phi, 0), loop_nodes);
  loop_head_nodes(loop_nodes, &info);
  copy_loop_body(loop_nodes, &info, unroll_factor);

  copies_t *value;
  for (value = set_first(loop_nodes); value != NULL; value = set_next(loop_nodes)) {
    if(value->irn == backedge_jmp)
      set_Block_cfgpred(loop_head, backedge_pos, value->copy[unroll_factor-2]);
  }

  set_loop_outs(loop_nodes, &info, unroll_factor);

  /*
  if (needs_preloop(unroll_factor)) {
    return;    for now ...
    make_preloop(unroll_factor);
  }
*/


  // adapt_result_usage();

}

/* Performs loop unrolling for the passed graph. */
void optimize_loop_unrolling(ir_graph *irg /* unroll factor, max body size */) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  if ( !get_opt_loop_unrolling()) return;

  /* -- Precompute some information -- */
  /* Call algorithm that computes the backedges */
  construct_cf_backedges(irg);
 /* Call algorithm that computes the dominator trees. */
  compute_doms(irg);
  /* Call algorithm that computes the out edges */
  compute_outs(irg);
  collect_phiprojs(irg);

  /* -- Search expressions that can be optimized -- */
  irg_walk_graph(irg, NULL, do_loop_unroll, NULL);

  current_ir_graph = rem;
}
