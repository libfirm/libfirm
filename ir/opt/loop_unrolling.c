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

#include <string.h>

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

/* We will unroll maximal 4-times.  */
#define MAX_UNROLL 4

/*We will know if the head of the loop to be copy.
 * Default "0"  don't copy.*/
static int copy_loop_head;

typedef struct {
  ir_node *irn ;                /* Node of the loop to be unrolling*/
  ir_node *copy[MAX_UNROLL] ;   /* The copy of the node */
} copies_t;

/**
 * Compare two elements of the copies_t set
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
  int copy_node_nr = false;

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
/*Check if phi in the loop head is.
 *
 * @param *phi               Muss to be a phi node from the loop.
 * @param *loop_head         The loop head .
 */
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
 * @param *unroll_factor      A integer 2 <= unroll_factor <= 4.
 * @param *env                Free environment pointer.
 */
static void
set_preds (set *l_n, copies_t *value, induct_var_info *info, int unroll_factor, void *env)
{
  int i, p, irn_arity;
  copies_t key, *value_pred;
  // The head of the unrolling loop.
  ir_node *loop_head = get_loop_node(info->l_itervar_phi, 0);

  irn_arity = get_irn_arity(value->irn);

  for (i = 0; i < irn_arity; i++) {
    ir_node *pred = get_irn_n(value->irn, i);

    key.irn = pred;
    value_pred = set_find(l_n, &key, sizeof(key), HASH_PTR(pred));

    if (value->irn != loop_head && !is_Phi_in_loop_head(value->irn, loop_head)) {
      if (value_pred == NULL) {
	/* Is loop invariant. */
	for(p = 0; p < unroll_factor -1; p++)
	  set_irn_n (value->copy[p], i, pred);
	// pred is a loop invariant. The copies of the successors get it as predecessor.

      } else
	for(p = 0; p < unroll_factor -1; p++)
	  set_irn_n (value->copy[p], i, value_pred->copy[p]);
      /* value_pred->irn is a node from the unrolling loop. The copies of the successors get the
	 copies of value_pred as predecessor.*/
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
 * @param *unroll_factor      A integer 2 <= unroll_factor <= 4.
 */
static void
set_phi_backedge(set *l_n, copies_t *value, induct_var_info *info, int unroll_factor)
{
  copies_t key, *value_pred;
  /*info->op_pred_pos is the backedge position. */
  key.irn = get_irn_n(value->irn, info->op_pred_pos);
  value_pred = set_find(l_n, &key, sizeof(key), HASH_PTR(key.irn));

  /*value->copy[unroll_factor - 2] is the last copie. */
  set_Phi_pred(value->irn, info->op_pred_pos, value_pred->copy[unroll_factor - 2]);
}

/** Test for a loop head.
 *
 *  Returns true if the node has predecessors in the loop _and_ out of
 *  the loop.  Then it is a loop head: The loop can be entered through
 *  this node.
 *
 *  @param *n      The node to be tested. Muss be a block.
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
  int l_n_node = 0, i;
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
    /* Initialize all copies of the added node with NULL.*/
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
 * @param *unroll_factor    A integer 2 <= unroll_factor <= 4.
 *
 */
static void
new_loop_head (set *l_n, induct_var_info *info, copies_t *value, int unroll_factor)
{
  int i;
  copies_t block, *value_backedge_jmp, *backedge_jmp_block;
  /* The  predecessor of the loop head in the  backedge position*/
  ir_node *backedge_jmp = get_Block_cfgpred(value->irn, info->op_pred_pos);
  block.irn = backedge_jmp;

  value_backedge_jmp = set_find( l_n, &block, sizeof(block), HASH_PTR(block.irn));

  if(copy_loop_head){
    /* The first copy of the loop head muss point to the loop head.*/
    ir_node *new_loop_head = new_Block(1, &backedge_jmp);
    value->copy[0] = new_loop_head;

    for(i = 1; i<unroll_factor - 1; i++){
      /* The another copies muss point to the copy befor it in the array. */
      ir_node *new_loop_head = new_Block(1, &value_backedge_jmp->copy[i-1]);
      value->copy[i] = new_loop_head;
    }
  }else{
    /* If the loop haed muss't be copy. block.irn is the successor of the loop head.*/
    block.irn = get_nodes_block(backedge_jmp);
    backedge_jmp_block =  set_find( l_n, &block, sizeof(block), HASH_PTR(block.irn));
    /*The first copy of block.irn point to it.
      The another copies muss point to the copy befor it in the array.*/
    set_irn_n(backedge_jmp_block->copy[0], 0, value_backedge_jmp->irn) ;

    for(i = 1; i<unroll_factor - 1; i++)
      set_irn_n(backedge_jmp_block->copy[i], 0, value_backedge_jmp->copy[i - 1]);
  }

}

/* Set all copies of the induction variable.
 *
 * @param *phi             A phi node in the loop head block.
 * @param *phi_pred        The predecessor of the phi along the backedge.
 * @param *unroll_factor   A integer 2 <= unroll_factor <= 4.
 *
 */
static void
set_Phi_copies(copies_t *phi, copies_t *phi_pred, int unroll_factor)
{
  int p;
  /* The first copy of Phi node get the node along the backedge as predecessor. The next copies
     the copies of this node.*/
  phi->copy[0] = phi_pred->irn;
  for(p = 1; p < unroll_factor -1; p++){
    // If two phi nodes are in cycle.
    if(phi_pred->copy[p - 1] == NULL && get_irn_op(phi_pred->irn) == op_Phi){
      if(p % 2 != 0)
	phi->copy[p] =  phi->irn;
      else
	phi->copy[p] =  phi_pred->irn;
    }else
      phi->copy[p] =  phi_pred->copy[p -1];
  }
}

/* Decide if the loop head to be copy. A head with important nodes
 * muss be copy.
 *
 * @param *l_n         A set, where the node of the loop are saved.
 * @param *info        Contains information about the induction variable.
 */
static void
loop_head_nodes(set *l_n, induct_var_info *info)
{
  copies_t *value;
  ir_node *loop_head = get_loop_node(info->l_itervar_phi, 0);

  for (value = set_first(l_n); value != NULL; value = set_next(l_n))
    if(value->irn->op != op_Block &&
       get_nodes_block(value->irn) == loop_head)
      /* If the loop head contains just this nodes, than muss't be copy. */
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
 * @param *l_n             Contains all nodes of the loop.
 * @param *info            Contains the loop information.
 * @param *unroll_factor   A integer 2 <= unroll_factor <= 4.
 */
static void
copy_loop_body(set *l_n, induct_var_info *info, int unroll_factor)
{
  int i;
  copies_t *value, *info_op, *phi, *loop_h, key, *value_block;

  ir_node *loop_head = get_loop_node(info->l_itervar_phi, 0);


  for (value = set_first(l_n); value != NULL; value = set_next(l_n)) {
    if(value->irn == loop_head)
      loop_h = value;
    else if (is_Phi_in_loop_head(value->irn, loop_head))
      phi = value;
    else if(copy_loop_head){
      /* If the loop head muss be copy. */
      for (i = 0; i < unroll_factor - 1; i++){
	copy_node(value->irn, NULL);
	value->copy[i] = get_irn_link(value->irn);
      }
    } else {
      /* If the loop head and its nodes muss't be copy. */
      if((value->irn->op == op_Block             &&
	  value->irn != loop_head)               ||
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

  /* Similarily treat the Phis in the loop head block. info->op is the node
     along the backadge.*/
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
    /* Set the predecessors of the copies. */
    if(copy_loop_head)
      set_preds(l_n, value, info, unroll_factor, NULL);
    else if((value->irn->op != op_Block) && get_nodes_block(value->irn) != loop_head)
      set_preds(l_n, value, info, unroll_factor, NULL);

    if (is_Phi_in_loop_head(value->irn, loop_head))
      /* Set the backedge of phis in the loop head. */
      set_phi_backedge(l_n, value, info, unroll_factor);

    if ((value->irn->op != op_Block) && !is_Phi_in_loop_head(value->irn, loop_head)) {
      ir_node *nodes_block = get_nodes_block(value->irn);

      if(!copy_loop_head && nodes_block == loop_head)
	continue;

      key.irn = nodes_block;
      value_block = set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn));
      /* Set the copy of the node in the accordant copy of its block. */
      for(i = 0; i < unroll_factor - 1; i++){
	set_nodes_block(value->copy[i], value_block->copy[i]);
	//add_End_keepalive(get_irg_end(current_ir_graph), value->copy[p]);
      }
    }
  }
}
/** is_exception_possible
 *
 * If a Proj node from the loop have as predecessor a Call, Div or Load node, then is a
 * exception possible.
 *
 * @param *node  A Proj node form the loop.
 */
static int
is_exception_possible(ir_node *node)
{
  int possible = 1;
  ir_node *pred = get_Proj_pred(node);

  switch(get_irn_opcode(pred)){
  case  iro_Call:
    break;
  case iro_Div:
    break;
  case iro_Load:
    break;
  default:
    possible = 0;
  }

  return possible;
}


/** new_end_block
 *
 *  If a node from the loop is predecessor of the end block,then muss  have the end block all copies
 *  of this node as predecessors.This is possible with funktions calls in the unrolling loop.
 *
 * @param *end_block          The end block.
 * @param *loop_head          The head of the unrolling loop.
 * @param *l_n                Contains all nodes of the loop.
 * @param *loop_endblock_outs The set loop_endblock_outs contains all predecessors
 *                            of the end block from the unrolling loop.
 * @param *unroll_factor      A integer 2 <= unroll_factor <= 4.
 */

static void
new_end_block (ir_node* end_block,ir_node *loop_head, set *l_n, set *loop_endblock_outs, int unroll_factor)
{

  copies_t key, *value;
  int set_el, new_preds, all_new_preds, i, q;
  int old_preds = get_Block_n_cfgpreds(end_block);   // All old predecessors of the end block.

  for(int i = 0; i < old_preds; i++){
    ir_node *pred = get_Block_cfgpred(end_block, i);
     key.irn = pred;
     value = NULL;
     value = set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn));
     // If a predecessor of the end block is a Proj from the unrolling loop (for funktion calls) .
     if( get_irn_op(pred) == op_Proj && is_exception_possible(pred) && value != NULL &&
	 !(!copy_loop_head && get_nodes_block(pred) == loop_head))
      value =  set_insert(loop_endblock_outs, &key, sizeof(key), HASH_PTR(key.irn));
  }
  /* The set loop_endblock_outs contains all predecessors of the end block from the unrolling loop.
     set_el their number.*/
  set_el =  set_count (loop_endblock_outs);
  // If the end block haven't such predecessors.Nothing muss be do.
  if (!set_el) return;

  new_preds = (unroll_factor - 1) * set_el;          //All new predecessors of the end block.
  all_new_preds = old_preds + new_preds;             //All predecessors of this block.
  ir_node *all_in[all_new_preds];                    //A array with size for all predecessors of this block.

  for (i = 0; i < old_preds; i++)
    all_in[i] = get_Block_cfgpred(end_block, i);    // The old predecessors.

  value = set_first(loop_endblock_outs);

  for( ; value != NULL; value = set_next(loop_endblock_outs)){
    key.irn = value->irn;
    value = set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn));
    for(q = 0; q < unroll_factor - 1 ; q++){
      all_in[i] = value->copy[q];
      i++;                                    // The new predecessors.
    }

  }
  /* Replace the old predecessors of the end block whit the new one. */
  set_irn_in(end_block, all_new_preds, all_in);
}

/** new_after_loop_block
 *
 *  This after loop block muss examine the possible exceptions in the loop.If a (Proj) node from  the loop
 *  is predecessor of this block,then muss  have the after loop block as well all copies
 *  of this node as predecessors.
 *
 * @param *l_n             Contains all nodes of the loop.
 * @block *block           A block afte the loop.
 * @param *loop_in         A node from the loop, that is predecessor of the end block.
 * @param *unroll_factor   A integer 2 <= unroll_factor <= 4.
 */

static void
new_after_loop_block (set *l_n, ir_node* block, copies_t *loop_in, int unroll_factor)
{
  copies_t key, *value;
  int i, p, q, s, old_preds, new_preds, all_new_preds ;

  // The node from the unrolling loop muss be a Proj.
  if(loop_in->irn->op != op_Proj) return;

  old_preds = get_Block_n_cfgpreds(block);     // All old predecessors of this block.
  new_preds = old_preds * (unroll_factor - 1); //All new predecessors of this block.
  all_new_preds = old_preds + new_preds;       //All predecessors of this block.

  ir_node *all_in[all_new_preds];                  //A array with size for all predecessors of this block.

  for (i = 0 ; i < old_preds; i++)
    all_in[i] = get_Block_cfgpred(block, i);      //The old predecessors.

  q = old_preds;
  for(i = 0; i < old_preds; i++){
    key.irn = all_in[i];
    value = set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn));
    p = 0;
    for(s = 0; s < (unroll_factor - 1); s++){
      all_in[q] = value->copy[p];               //The new predecessors.
      p++;
      q++;
    }

  }
  /* Replace the old predecessors of the end block whit the new one. */
  set_irn_in(block, all_new_preds, all_in);
}

/** new_after_loop_node
 *
 *  This after loop node (phi or call) muss examine the possible exceptions in the loop.If a (Proj) node
 *  from the loop is predecessor of this node,then muss  have the after loop node as well all copies
 *  of this node as predecessors.
 *
 * @param *l_n             Contains all nodes of the loop.
 * @param *loop_outs       Contains nodes after the loop,that have as predecessor a node from the loop.
 * @block *node            A node afte the loop.
 * @param *loop_in         A node (Proj) from the loop, that is predecessor of *node.
 * @param *unroll_factor   A integer 2 <= unroll_factor <= 4.
 */

static void
new_after_loop_node(set *l_n, set *loop_outs, ir_node *node, copies_t *loop_in, int unroll_factor)
{
  ir_node *pred, *block_pred, *node_block, *new_phi;
  int phi = 0, old_preds, new_preds, all_new_preds, p, q, i, s;
  copies_t key, *value = NULL;

  old_preds = get_irn_arity(node);            // All old predecessors of this node.
  new_preds =old_preds * (unroll_factor - 1); //All new predecessors of this block.
  all_new_preds = old_preds + new_preds;      //All predecessors of this block.
  ir_node *all_in [all_new_preds];            //A array with size for all predecessors of this block.


  // Verification Predecessors, successors and operation of node and loop_in.
  //loop_in muss be a Proj node.
  if(loop_in->irn->op != op_Proj) return;
  // Node muss be operation Phi with mode memory or a Call node.
  if(get_irn_op(node) == op_Phi  &&
     get_irn_mode(node) == mode_M){
    // If node is a Phi node,then muss  have a Call node as successor.
    for (i = 0; i < get_irn_n_outs(node); i++)
      if(get_irn_opcode(get_irn_out(node, i)) == iro_Call)
	phi = 1;

    if (!phi) return;
  }
  // The predecessor of loop_in muss't be a loop invariant.
  pred = get_Proj_pred(loop_in->irn);
  key.irn = pred;
  value = set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn));
  if(value == NULL)return;

  node_block = get_nodes_block(node);

  // The block of node muss have too a (Proj) predecessor from the unrolling loop.
  for(i = 0; i < get_Block_n_cfgpreds(node_block); i++){
    block_pred = get_Block_cfgpred( node_block, i);

    if(get_irn_op(block_pred) == op_Proj){
      if(get_Proj_pred(block_pred) == pred)
	break;
    }else
      block_pred = NULL;
  }

  if(block_pred == NULL) return;

  key.irn = block_pred;
  value = set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn));

  if (value == NULL)
    return;
  else{
    new_after_loop_block(l_n, node_block, value, unroll_factor);
  }

  i = 0, p = 0;
  for ( ; i < old_preds; i++)
    all_in[i] = get_irn_n(node, i);  //The old predecessors.

  i = 0;
  q = old_preds;
  for(; i < old_preds; i++){
    key.irn = all_in[i];
    value = set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn));
    p = 0;
    for(s = 0; s < (unroll_factor - 1); s++){
      all_in[q] = value->copy[p];  //The new predecessors.
      p++;
      q++;
    }
  }
  // A new phi node with the new predecessors.
  new_phi =  new_r_Phi(current_ir_graph, get_nodes_block(node), all_new_preds,all_in,
		       get_irn_mode(node));

  if(phi){
    exchange(node, new_phi);
  }else{
    i = 0;
    for( ; i < get_irn_arity(node) ; i++)
      if (get_irn_n(node, i) == pred)
	set_irn_n(node, i, new_phi);
  }
  // The set loop_outs contains the visited nodes and their blocks.
  key.irn = node;
  value = set_insert(loop_outs, &key, sizeof(key), HASH_PTR(key.irn));
  key.irn = get_nodes_block(node);
  value = set_insert(loop_outs, &key, sizeof(key), HASH_PTR(key.irn));
}


/* Set the outs of the unrolling loop. All loop outs of a node muss now
 * point to the last copy of it. Just phi nodes in the loop head and proj
 * nodes save it outs. The all copies of some Projs  have too outs.
 *
 * @param *l_n                Contains all nodes of the loop.
 * @param *loop_outs          The set contains the visited and changed loop outs.
 * @param *loop_endblock_outs The set loop_endblock_outs contains all predecessors
 *                            of the end block from the unrolling loop.
 * @param *info               Contains the loop information.
 * @param *unroll_factor      A integer 2 <= unroll_factor <= 4.
 */
static void
set_loop_outs(set *l_n,set * loop_outs, set *loop_endblock_outs,induct_var_info *info, int unroll_factor)
{
  copies_t *value, key;
  int i, p;
  ir_node *loop_head = get_loop_node(info->l_itervar_phi, 0);
  ir_node *end_block = get_irg_end_block(current_ir_graph);
  value = set_first(l_n);

  for( ; value != NULL; value = set_next(l_n))
    if(!is_Phi_in_loop_head(value->irn, loop_head) &&
       (get_irn_opcode(value->irn) == iro_Proj && value->copy[0] != NULL))
      for(i = 0; i < get_irn_n_outs(value->irn); i++){
	key.irn = get_irn_out(value->irn, i);
	// Search for loop outs.
	if(set_find( l_n, &key, sizeof(key), HASH_PTR(key.irn)) == NULL)
	  if((key.irn->op == op_Block && get_Block_dom_depth(key.irn)  >
	      get_Block_dom_depth(loop_head))                          ||
	     (key.irn->op != op_Block && get_Block_dom_depth(get_nodes_block(key.irn)) >
	      get_Block_dom_depth(loop_head))){

	    for(p = 0; p < get_irn_arity(key.irn); p++)
	      if(value->irn == get_irn_n(key.irn, p)){
		if(get_irn_opcode(value->irn) == iro_Proj && is_exception_possible(value->irn)){
		  if(set_find( loop_outs, &key, sizeof(key), HASH_PTR(key.irn)) == NULL){
		    // If the loop out is for exceptions in the loop.
		    if((key.irn->op == op_Phi && get_irn_mode(key.irn) == mode_M) ||
		       get_irn_opcode(key.irn) == iro_Call)
		      new_after_loop_node(l_n,loop_outs, key.irn, value, unroll_factor);
		    else
		      continue;
		      }else
			continue;
		}else
		  set_irn_n (key.irn, p, value->copy[unroll_factor-2]);
	      }
	  }
      }
  // The funktion search for loop outs associated with funktion call in the unrolling loop.
  new_end_block (end_block, loop_head, l_n, loop_endblock_outs, unroll_factor);
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
  copy_loop_head = 0;

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
  ir_node *phi_init =  get_Phi_pred(info.itervar_phi, info.init_pred_pos);

  if(!is_Proj(cmp_out)) return;
  if(get_irn_op(info.increment) != op_Const   ||
     get_irn_op(phi_init) != op_Const         ||
     get_irn_op(info.cmp_const) != op_Const) return;

  int cmp_typ =  get_Proj_proj(cmp_out);
  int init = get_tarval_long(get_Const_tarval(phi_init));
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
  /*Test for the value of unroll factor. */
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


  printf("\nloop unrolling with factor %d \n", unroll_factor);

  DDMG(get_irn_irg(n));
  // The unrollfactor muss less than 4.
  assert(unroll_factor <= MAX_UNROLL);

  ir_node *loop_head;

  loop_head = (is_natural_loop(&info)) ? get_loop_node(info.l_itervar_phi, 0) : NULL;

  assert(loop_head != NULL && is_Block(loop_head));

 /* We assume, the loop head has exactly one backedge.  The position of
    the backedge is in the following variable: */
  int backedge_pos ;
  backedge_pos = (is_backedge(loop_head, 0)) ? 0:1;


  set *loop_nodes, *loop_outs, *loop_endblock_outs;
  /* A set with the nodes to copy. */
  loop_nodes = new_set(set_cmp, 8);
  // A set with the loop outs for exceptions.
  loop_outs =  new_set(set_cmp, 8);
  /* The set contains all predecessors
     of the end block from the unrolling loop.*/
  loop_endblock_outs = new_set(set_cmp, 8);

  /* Find all nodes of the unrolling loop. */
  find_loop_nodes(info.itervar_phi, get_loop_node(info.l_itervar_phi, 0), loop_nodes);
  /* Decide if the loop head to be copy.*/
  loop_head_nodes(loop_nodes, &info);
  /* Copy all nodes of the unrolling loop, that muss be copy. */
  copy_loop_body(loop_nodes, &info, unroll_factor);

  ir_node *backedge_jmp = get_Block_cfgpred(loop_head, backedge_pos);
  copies_t *value;
  /* Set the backedge of the loop head. */
  for (value = set_first(loop_nodes); value != NULL; value = set_next(loop_nodes)) {
    if(value->irn == backedge_jmp){

      set_Block_cfgpred(loop_head, backedge_pos, value->copy[unroll_factor-2]);
    }
  }
  set_loop_outs(loop_nodes, loop_outs, loop_endblock_outs, &info, unroll_factor);

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
