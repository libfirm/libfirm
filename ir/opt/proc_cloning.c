/*
 * Project:     libFIRM
 * File name:   ir/opt/proc_cloning.c
 * Purpose:     procedure cloning
 * Author:      Beyhan Veliev
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file proc_cloning.c
 *
 * The purpose is first to find and analyze functions, that are called
 * with constant parameter(s).
 * The second step is to optimize the function that are found from our
 * analyze. Optimize mean to make a new function with parameters, that
 * aren't be constant. The constant parameters of the function are placed
 * in the function graph. They aren't be passed as parameters.
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

//#ifdef HAVE_MALLOC_H
# include <malloc.h>
//#endif

#include <stdarg.h>
#include <string.h>

#include "tv.h"
#include "set.h"
#include "entity.h"
#include "irprog_t.h"
#include "hashptr.h"
#include "irgwalk.h"
#include "proc_cloning.h"
#include "analyze_irg_args.h"
#include "irprintf.h"
#include "old_fctnames.h"
#include "ircons.h"
#include "loop_unrolling.h"
#include "irouts.h"
#include "mangle.h"
#include "irnode_t.h"
/* A macro to iterate sets.*/
#define ITERATE_SET(set_entrys, entry) for(entry = set_first(set_entrys); entry; entry = set_next(set_entrys))

/**
 * This struct contains the information quartuple for a Call, which we need to
 * decide if this function must be cloned.
 */
typedef struct _entry {
  struct triple {
    entity        *ent;   /**< The entity of our Call. */
    int           pos;    /**< Position of a constant argument of our Call. */
    tarval        *tv;    /**< The tarval of this constant argument. */
    ir_node       *call;  /**< The Call itself. */
  } t;                    /**< The heuristic triple. */

  unsigned      num_calls;  /**< number of calls */
  float         weight;     /**< The estimated weight of this triple. */
  struct _entry *next;      /**< used for linking and sorting */
} entry_t;

/**
 * Compare two triples.
 *
 * @return 0 if they are identically
 */
static int entry_cmp(const void *elt, const void *key, size_t size)
{
  const entry_t *c1 = elt;
  const entry_t *c2 = key;

  return (c1->t.ent != c2->t.ent) || (c1->t.pos != c2->t.pos) || (c1->t.tv != c2->t.tv);
}

/**
 * Hash a element of typ entry_t
 *
 * @param entry The element to be heshed.
 */
static int hash_entry(const entry_t *entry)
{
  return HASH_PTR(entry->t.ent) ^ HASH_PTR(entry->t.tv) ^ (entry->t.pos * 9);
}

/**
 * Collect all calls in a ir_graph
 * to a set.
 *
 * @param call   A ir_node to be checked.
 * @param env    The set where we will collect the calls.
 */
static void collect_irg_calls(ir_node *call, void *env)
{
  set *entrys = env;
  ir_node *call_ptr;
  entity *call_ent;
  type *mtp;
  entry_t key, *entry;
  ir_node *call_param;
  int i, n_params;

  /* We collect just "Call" nodes*/
  if (get_irn_op(call) != op_Call)
    return;

  call_ptr = get_Call_ptr(call);

  /* Call pointer must be a symconst*/
  if (op_SymConst != get_irn_op(call_ptr))
    return;
  /* Call pointer must be the address of an entity.*/
  if (get_SymConst_kind(call_ptr) != symconst_addr_ent)
    return;

  call_ent = get_SymConst_entity(call_ptr);

  /* we can only clone calls to existing entities */
  if (get_entity_visibility(call_ent) == visibility_external_allocated)
    return;

  n_params = get_Call_n_params(call);

  /* beware: we cannot clone variadic parameters */
  mtp = get_Call_type(call);
  if (get_method_variadicity(mtp) != variadicity_non_variadic) {
    n_params = get_method_first_variadic_param_index(mtp);
  }

  /* In this for loop we collect the calls, that have
     an constant parameter. */
  for (i = n_params - 1; i >= 0; --i) {
    call_param = get_Call_param(call, i);
    if (is_Const(call_param)) {
      /* we have found a Call to collect and we save the informations,
         which we need.*/

      key.t.ent     = call_ent;
      key.t.pos     = i;
      key.t.tv      = get_Const_tarval(call_param);
      key.t.call    = call;
      key.num_calls = 0;

      /* We insert our information in the set, where we collect the calls.*/
      entry     = set_insert(entrys, &key, sizeof(key), hash_entry(&key));

      /* we found one more */
      ++entry->num_calls;
      break;
    }
  }
}
/**
 * Make a name for the clone. The clone name is
 * the name of the original method advanced with "_cl_pos_nr".
 * pos is the pos from our quartuple und  nr is a counter.
 *
 * @param id  The ident of the cloned function.
 * @param pos The "pos" from our quartuple.
 * @param nr  A counter for the clones.
 */
static ident *get_clone_ident(ident *id, int pos, unsigned nr)
{
  char clone_postfix[32];

  snprintf(clone_postfix, sizeof(clone_postfix), "_cl_%d_%u", pos, nr);

  return mangle(id, new_id_from_str(clone_postfix));
}

/**
 * The function fill the bloks and nodes, that muss be in
 * the clone graph, from the original method graph. The cloned method
 * have one argument few, why it is replaced with a constan.
 *
 * @param irn  A node from the original method graph.
 * @param env  The clone graph.
 */
static void fill_clone_irg(ir_node *irn, void *env)
{
  ir_node *arg, *irg_args, *irn_copy, *link;
  int proj_nr;
  ir_graph *clone_irg;

  clone_irg = env;
  arg       = get_irg_link(clone_irg);
  irg_args  = get_Proj_pred(arg);

  if(get_irn_op(irn) == op_Call)
    link = get_irn_link(irn);

  /* We must copied all node outside the argument,
     that we wont to replace with a constant,
     the end node and block.*/
  if(!(irn == arg || get_irg_end_block(current_ir_graph) == irn ||
       get_irg_end(current_ir_graph) == irn))
    copy_irn(irn, clone_irg);

  irn_copy = get_irn_link(irn);

  if(get_irn_op(irn) == op_Call)
    irn_copy->link = link;

  /* I repair the ir graph of the copy block, why it
     is set wrong from "copy_irn()".*/
  if(is_Block(irn))
    irn_copy->attr.block.irg = clone_irg;
  /* If the original function have arguments with a bigger number
     of the argument's number, that we want to replace, we muss
     decrement them with one.*/
  if(get_irn_op(irn) == op_Proj &&  get_Proj_pred(irn) == irg_args){
    proj_nr  = get_Proj_proj(irn);
    if(get_Proj_proj(arg) < proj_nr)
      set_Proj_proj(irn_copy, proj_nr - 1);
  }
}
/**
 * Set the predecessors of the copied nodes.
 * The copied nodes are set as link of their original nodes. The links of
 * "irn" predecessors are the predecessors of copied node.
 */
static void set_preds(ir_node *irn, void *env)
{
  int i;
  ir_node *irn_copy, *pred, *arg;
  ir_graph *clone_irg;

  clone_irg = env;
  irn_copy  = get_irn_link(irn);

  /* First we set the block our copy if it is not a block.*/
  if(!is_Block(irn))
    set_nodes_block(irn_copy, get_irn_link(get_nodes_block(irn)));

  arg = get_irg_link(clone_irg);
  /* Arg is the method argument, that wi have replaced with a constant.*/
  if(arg == irn)
    return;

  if(get_irn_op(irn) == op_Block){
    for(i = get_Block_n_cfgpreds(irn) - 1; i >= 0; i--){
      pred = get_Block_cfgpred(irn, i);
      /* "End" block muss be covered extra, why it is not matured.*/
      if(get_irg_end_block(current_ir_graph) == irn)
	ARR_APP1 (ir_node *, get_irg_end_block(clone_irg)->in, get_irn_link(pred));
      else
	set_Block_cfgpred(irn_copy, i, get_irn_link(pred));
    }
  }else
    for(i = get_irn_arity(irn) - 1; i >= 0; i--){
      pred = get_irn_n(irn, i);
      set_irn_n(irn_copy, i, get_irn_link(pred));
    }
}
/**
 * Get the method argument at the position "pos".
 *
 * @param ent The entity of the function, that muss be cloned.
 * @param pos The position of the orgument.
 */
static ir_node *get_method_arg(entity *ent, int pos)
{
  ir_graph *irg;
  ir_node *irg_args, *arg, *start;
  int i;

  irg      = get_entity_irg(ent);

  /* Call algorithm that computes the out edges */
  if (get_irg_outs_state(irg) != outs_consistent)
    compute_outs(irg);
  start = get_irg_start(irg);
  for(i = get_irn_n_outs(start) - 1; i >= 0; i--){
    irg_args = get_irn_out(start, i);
    if(is_Proj(get_irn_out(irg_args,0))){
      set_irg_args(irg, irg_args);
      break;
    }
  }
  /* Search the argument whit the numer pos.*/
  for (i = get_irn_n_outs(irg_args) - 1; i >= 0; --i) {
    arg = get_irn_out(irg_args, i);
    if(pos == get_Proj_proj(arg))
      break;
  }
  return arg;
}
/**
 * Create a new graph for the clone of the procedur,
 * that we wont to clone.
 *
 * @param ent The entity of the function, that muss be cloned.
 * @param t   Our quartuple.
 */
static void create_clone_proc_irg(entity *ent, struct triple *t)
{
  ir_graph *method_irg, *clone_irg;
  ir_node *arg, *const_arg;
  int loc_n;

  method_irg = get_entity_irg(ent);

  /* The ir graph of the cloned procedur have one local few,
     why one of the arguments is replaced with a constant.*/
  loc_n      = get_irg_n_loc(method_irg) - 1;

  /* We create the skeleton of the clone irg.*/
  clone_irg  = new_ir_graph(ent, loc_n);

  arg        = get_method_arg(t->ent, t->pos);
  /*This is the constante , with that we will replace the argument in position "t->pos".*/
  const_arg  = new_r_Const(clone_irg, get_nodes_block(arg), get_irn_mode(arg), t->tv);
  /* We have this nodes in the new ir_graph, and they muss not be
     copied.*/
  set_irn_link(arg, const_arg);
  set_irn_link(get_irg_end(method_irg), get_irg_end(clone_irg));
  set_irn_link(get_irg_end_block(method_irg), get_irg_end_block(clone_irg));

  /* I need this, why "irg_walk_graph" change "current_ir_graph" to passed irg.*/
  set_irg_link(clone_irg, arg);
  /* We fill the bloks and nodes, that muss be in
     the clone graph and set their preds.*/
  irg_walk_graph(method_irg, fill_clone_irg, set_preds, clone_irg);

  /* The "cloned" ir_graph muss be corrected.*/
  set_irg_start_block( clone_irg, get_irn_link(get_irg_start_block(method_irg)));
  set_irg_start( clone_irg, get_irn_link(get_irg_start(method_irg)));
  mature_block(get_irg_end_block(clone_irg));
  irg_finalize_cons(clone_irg);
}
/**
 * The function create a new entity type
 * for our clone and set it to clone entity.
 *
 * @param t   Contains information
 *            for the method to clone.
 * @param ent The entity of the clone.
 * @param nr  A pointer to the counter of clones.
 **/
static void change_entity_type(struct triple *t, entity *ent, unsigned *nr)
{
  type *mtp, *new_mtp, *tp;
  ident *tp_name;
  int i, n_params, n_ress, pos = 0;

  mtp      = get_entity_type(t->ent);
  tp_name  = get_clone_ident(get_type_ident(mtp), t->pos, (*nr)++);
  n_params = get_method_n_params(mtp);
  n_ress   = get_method_n_ress(mtp);
  /* Create the new type for our clone. It muss have 1 parameter
     few then the original.*/
  new_mtp  = new_type_method(tp_name, n_params - 1, n_ress);

  /* We muss set the type of the methods parameters.*/
  for( i = 0; pos < (n_params - 1); i++){

    if( i == t->pos - 1)
      /* This is the position of the argument, that we have
         replaced, t. m. "i" muss be incremented, but "pos" not
         and nothing else muss be done.*/
      continue;

    tp = get_method_param_type(mtp, i);
    set_method_param_type(new_mtp, pos, tp);
    pos++;
  }
  /* We muss set the type of the methods results.*/
  for( i = 0; i < n_ress; i++){
    tp = get_method_res_type(mtp, i);
    set_method_res_type(new_mtp, i, tp);
  }

  set_entity_type(ent, new_mtp);
}

/**
 * Make a clone of a method.
 *
 * @param t   Contains information
 *            for the method to clone.
 */
static entity *clone_method(struct triple *t)
{
  entity *new_entity;
  ident *clone_ident;
  ir_node *irn;
  ir_graph *rem;
  symconst_symbol sym;
  /* A counter for the clones.*/
  static unsigned nr = 0;

  /* We get a new ident for our clone method.*/
  clone_ident = get_clone_ident(get_entity_ident(t->ent), t->pos, nr);
  /* We get our entity for the clone method.*/
  new_entity  = copy_entity_name (t->ent, clone_ident);

  /* a cloned entity is always local */
  set_entity_visibility(new_entity, visibility_local);

  /* set a ld name here: Should we mangle this ? */
  set_entity_ld_ident(new_entity, get_entity_ident(new_entity));

  /* set a new type here.*/
  change_entity_type(t, new_entity, &nr);

  /* We need naw a new ir_graph for our clone procedure.
     This will we make with create_clone_proc_irg.*/
  create_clone_proc_irg(new_entity, t);
  /* We muss set the atomic value of our "new_entity". */
  sym.entity_p = new_entity;
  rem = current_ir_graph;
  current_ir_graph =  get_const_code_irg();
  new_entity->value = new_SymConst(sym, symconst_addr_ent);
  current_ir_graph = rem;
  /* The "new_entity" have not this information.*/
  new_entity->param_access = NULL;
  new_entity->param_weight = NULL;

  return new_entity;
}
/** The functin make a new "Call" node and return it.
 *
 * @param call        The call, that muss be exchanged.
 * @param new_entity  The entity of the cloned function.
 * @param pos         The position of the replaced parameter of this call.
 **/
static ir_node *new_cl_Call(ir_node *call, entity *new_entity, int pos)
{
  ir_node **in;
  type *mtp;
  int i, n_params, new_params = 0;
  ir_node *callee;
  symconst_symbol sym;

  sym.entity_p = new_entity;
  callee = new_r_SymConst(get_irn_irg(call), get_nodes_block(call), sym, symconst_addr_ent);

  mtp      = get_entity_type(new_entity);
  n_params = get_Call_n_params(call);
  in       = malloc(sizeof(ir_node*) * (n_params - 1));

  /* we save the parameters of the new call in the array "in" without the
   * parameter in posiotn "pos", that is replaced with a constant.*/
  for(i = 0; i < n_params; i++){
    if(pos == i)
      continue;
    in[new_params] = get_Call_param(call, i);
    new_params++;
  }
  /* We make and return the new call.*/
  return new_r_Call(get_irn_irg(call), get_nodes_block(call), get_Call_mem(call),
		    callee, n_params - 1, in, get_entity_type(new_entity));
}
/** A call node in the graph is the head of a list, that contains all
 *  clons lf this graph. If a call muss be exchanged in a graph, this muss
 *  be made in all cloned graph too. "wchange_calls" make this.
 *
 * @param call          The call, that muss be exchanged.
 * @param new_entity    The entity of the new function, that must be called from the new call.
 * @param pos           The position of the replaced parameter of "call".
 */
static void exchange_calls(ir_node *call, entity *new_entity, int pos)
{
  ir_node *copy, *new_call;
  int n_params;

  n_params = get_Call_n_params(call);
  copy     = get_irn_link(call);

  /* We iterate the list of the "call".*/
  for( ; copy; copy = get_irn_link(copy)){
    if(!is_ir_node(copy) ||
       get_irn_irg(copy) == get_irn_irg(call))
      break;
    /* A clone exist and the copy of "call" in this
     * clon graph must be exchanged with new one.*/
    new_call = new_cl_Call(copy, new_entity, pos);
    exchange(copy, new_call);
    /* The list muss be updatet too.*/
    set_irn_link(call, new_call);
    call = new_call;
  }
}
/*
 * Do the procedure cloning. Evaluate a heuristic weight for every
 * call(..., Const, ...). If the weight is bigger than threshold,
 * clone the entity and fix the calls.
 */
void proc_cloning(float threshold)
{
  ir_node *new_call, *link;
  set *set_entrys, *new_entrys;
  entry_t *entry,*p, *heavy_uses = NULL, key;
  ir_graph *irg;
  int i, count = 0;
  /* "set_entrys" contains the Calls to cloning, after
      the walk over the graph. */
  set_entrys  = new_set(entry_cmp, 8);

  entry       = NULL;

  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    irg = get_irp_irg(i);
    irg_walk_graph(irg, collect_irg_calls, NULL, set_entrys);
  }
  /* We have the "Call" nodes to optimize in set "set_entrys". Our algorithm
     replace one constant parameter and make a new "Call" node for all found "Calls". It exchange the
     old one with the new one and the algorithm is called with the new "Call".
     */
  while(set_count(set_entrys)){
    /* We iterate the set and arrange the element of the set in a list.
       The elements are arranged dependent of their value descending.*/
    ITERATE_SET(set_entrys, entry) {
      entry->weight = entry->num_calls *
	(get_method_param_weight(entry->t.ent, entry->t.pos) + 1);

      /*
       * Do not put entry with a weight < threshold in the list
       */
      if (entry->weight < threshold)
	continue;

      /* put entry in the heavy uses list */
      entry->next = NULL;
      if (! heavy_uses)
	heavy_uses = entry;
      else {
	if (entry->weight >= heavy_uses->weight) {
	  entry->next = heavy_uses;
	  heavy_uses  = entry;
	}
	else {
	  for (p = heavy_uses; p->next; p = p->next) {
	    if (entry->weight >= p->next->weight) {
	      entry->next = p->next;
	      p->next     = entry;
	      break;
	    }
	  }
	  if (! p->next)
	    p->next = entry;
	}
      }
    }

    /* Print some informations about the list. */
    for (entry = heavy_uses; entry; entry = entry->next) {
    printf("\nweight: is %f\n", entry->weight);
    ir_printf("Call for Method %E\n", entry->t.ent);
    printf("Position %i\n", entry->t.pos);
    ir_printf("Value %T\n", entry->t.tv);
    }

   /* "new_entrys" contain already optimized Calls, that muss
      be optimized again, why we can optimize just one constan
      parameter at once and when a Call have to constant parameters
      the algorithm muss be repeated, but we don't need to walk over
      the graph again.*/
    new_entrys  = new_set(entry_cmp, 8);

    for (entry = heavy_uses; entry; entry = entry->next) {
      count = set_count(new_entrys);
      /* The new cloned method shoul be made.*/
      entity *ent = clone_method(&entry->t);

      /* The new Call for the new methode should be made.*/
      new_call = new_cl_Call(entry->t.call, ent, entry->t.pos);

      /* A call node in the graph is the head of a list, that contains all
       *  clons lf this graph. The "new_call" must be inherits this list.*/
      set_irn_link(new_call, get_irn_link(entry->t.call));

      exchange(entry->t.call, new_call);
      /* We set the new Call in the set "new_entrys" if it
	 have constant parameter.*/
      collect_irg_calls(new_call, new_entrys);
      /* We muss exchange the copies ot this call in all clones too.*/
      exchange_calls(new_call, ent, entry->t.pos);
    }
    /* The "Calls" in the set "set_entrys" are optimized. */
    del_set(set_entrys);
    set_entrys  = new_set(entry_cmp, 8);
    /* The set "set_entrys" must contain the new "Calls" to optimize t.m.
       we must copy all entys of "new_entrys" to "set_entrys"*/
    ITERATE_SET(new_entrys, entry) {
      key.t.ent     = entry->t.ent;
      key.t.pos     = entry->t.pos;
      key.t.tv      = entry->t.tv;
      key.t.call    = entry->t.call;
      key.num_calls = 0;

      set_insert(set_entrys, &key, sizeof(key), hash_entry(&key));
    }
    del_set(new_entrys);
    entry = NULL;
    heavy_uses = NULL;
  }
}
