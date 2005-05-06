/*
 * Project:     libFIRM
 * File name:   ir/opt/scalar_replace.c
 * Purpose:     scalar replacement of arrays and compounds
 * Author:      Beyhan Veliev
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "scalar_replace.h"

/**
 * A path element entry: it is either an entity
 * or a tarval, because the evaluate only constant array
 * accesses like a.b.c[8].d
 */
typedef union {
  entity *ent;
  tarval *tv;
} path_elem_t;

/**
 * An access path, used to assign value numbers
 * to variables that will be skalar replaced
 */
typedef struct _path_t {
  unsigned    vnum;      /**< the value number */
  unsigned    path_len;  /**< the length of the access path */
  path_elem_t path[1];   /**< the path */
} path_t;

typedef struct {
  ir_node *irn;                 /**< Phi or unknown node from the graph to be repairing. */
  ir_node **link;               /**< Array of Stores's volue, that have been scalar replaced. */
} repairs_t;

typedef struct _scalars_t {
  entity *ent;                 /**< A entity for scalar replacement. */
  type *ent_owner;             /**< The owner of this entity. */
} scalars_t;


/**
 * Compare two pathes.
 *
 * @return 0 if they are identically
 */
static int path_cmp(const void *elt, const void *key, size_t size)
{
  const path_t *p1 = elt;
  const path_t *p2 = key;

  return memcmp(p1->path, p2->path, p1->path_len * sizeof(p1->path[0]));
}

/**
 * Compare two elements of the repairs_t set.
 *
 * @return 0 if they are identically
 */
static int set_cmp(const void *elt, const void *key, size_t size)
{
  const repairs_t *c1 = elt;
  const repairs_t *c2 = key;

  return c1->irn != c2->irn;
}

/**
 * Compare two elements of the scalars_t set.
 *
 * @return 0 if they are identically
 */
static int ent_cmp(const void *elt, const void *key, size_t size)
{
  const scalars_t *c1 = elt;
  const scalars_t *c2 = key;

  return c1->ent != c2->ent;
}

/**
 * Calculate a hash value for a path.
 */
static unsigned path_hash(const path_t *path)
{
  unsigned hash = 0;
  unsigned i;

  for (i = 0; i < path->path_len; ++i)
    hash ^= (unsigned)path->path[i].ent;

  return hash >> 4;
}

/**
 * Returns non-zero, if all indeces of a Sel node are constants.
 *
 * @param sel  the Sel node that will be checked
 */
static int is_const_sel(ir_node *sel) {
  int i, n = get_Sel_n_indexs(sel);

  for (i = 0; i < n; ++i) {
    ir_node *idx = get_Sel_index(sel, i);

    if (get_irn_op(idx) != op_Const)
      return 0;
  }
  return 1;
}

/**
 * Returns non-zero, if the address of a Sel node (or it's succsessor Sels) is taken.
 *
 * @param sel  the Sel node
 */
static int is_address_taken(type *ent_type, ir_node *sel)
{
  int i, n;

  if (! is_const_sel(sel))
    return 1;

  n = get_irn_n_outs(sel);
  for (i = 0; i < n; ++i) {
    ir_node *succ = get_irn_out(sel, i);

    switch (get_irn_opcode(succ)) {
    case iro_Load:
      break;

    case iro_Store:
      /* check that Sel is not the store's value */
      if (get_Store_value(succ) == sel)
	return 1;
      break;

    case iro_Sel: {
      /* Check the Sel successor of Sel */
      int res = is_address_taken(ent_type, succ);

      if (res)
        return 1;
      break;
    }
    default:
      /* another op, the address is taken */
      return 1;
    }
  }
  return 0;
}

/**
 * Link all leave Sels with the entity.
 *
 * @param ent  the entity that will be scalar replaced
 * @param sel  a Sel node that selects some fields of this entity
 */
static void link_all_leave_sels(entity *ent, ir_node *sel)
{
  int i, n, flag = 1;

  n = get_irn_n_outs(sel);
  for (i = 0; i < n; ++i) {
    ir_node *succ = get_irn_out(sel, i);

    if (get_irn_op(succ) == op_Sel) {
      link_all_leave_sels(ent, succ);
      flag = 0;
    }
  }
  if (flag) {
    /* we know we are at a leave, because this function is only
     * called if the address is NOT taken, so succ must be a Load
     * or a Store node
     */
    set_irn_link(sel, get_entity_link(ent));
    set_entity_link(ent, sel);
  }
}

/* we need a special address that serves as an address taken marker */
static char _x;
static void *ADDRESS_TAKEN = &_x;

/**
 * Find possible scalar replacements.
 *
 * @param irg  an IR graph
 *
 * This function finds variables on the (members of the) frame type
 * that can be scalar replaced, because their address is never taken.
 * If such a variable is found, it's entity link will hold a list of all
 * Sel nodes, that selects the atomar fields of this entity.
 * Otherwise, the link will be NULL.
 */
static void do_find_scalar_replacements(ir_graph *irg)
{
  ir_node *irg_frame = get_irg_frame(irg);
  int i, n;

  n = get_irn_n_outs(irg_frame);

  /* first, clear the link field of all interesting entities */
  for (i = 0 ; i < n; i++) {
    ir_node *succ = get_irn_out(irg_frame, i);

    if (get_irn_op(succ) == op_Sel) {
      entity *ent = get_Sel_entity(succ);
      set_entity_link(ent, NULL);
    }
  }

  /* Check the ir_graph for Sel nodes. If the entity of Sel isn't a scalar replacement
   set the link of this entity equal NULL. */
  for (i = 0 ; i < n; i++) {
    ir_node *succ = get_irn_out(irg_frame, i);

    if (get_irn_op(succ) == op_Sel) {
      entity *ent = get_Sel_entity(succ);
      type *ent_type = get_entity_type(ent);

      /* we can handle arrays, structs and atomic types yet */
      if (is_Array_type(ent_type) || is_Struct_type(ent_type) || is_atomic_type(ent_type)) {
	if (is_address_taken(ent_type, succ)) {
          set_entity_link(ent, ADDRESS_TAKEN);
        }
	else {
          if (get_entity_link(ent) != ADDRESS_TAKEN)
            link_all_leave_sels(ent, succ);
        }
      }
    }
  }
}

/**
 * Return a path from the Sel node sel to it's root.
 */
static path_t *find_path(ir_node *sel, unsigned len)
{
  int pos, i, n;
  path_t *res;
  ir_node *pred = get_Sel_ptr(sel);
  /* the current Sel node will add some path elements */
  n    = get_Sel_n_indexs(sel);
  len += n + 1;

  if (get_irn_op(pred) != op_Sel) {
    /* we found the root */

    res = xmalloc(sizeof(*res) + (len - 1) * sizeof(res->path));
    res->path_len = len;
  }
  else
    res = find_path(pred, len);

  pos = res->path_len - len;

  res->path[pos++].ent = get_Sel_entity(sel);
  for (i = 0; i < n; ++i) {
    ir_node *index = get_Sel_index(sel, i);

    res->path[pos++].tv = get_Const_tarval(index);
  }
  return res;
}


/**
 * Allocate value numbers for the leaves
 * in our found entities.
 *
 * @param ent   the entity that will be scalar replaced
 * @param vnum  the first value number we can assign
 * @param modes a flexible array, containing all the modes of
 *              the value numbers.
 *
 * @return the next free value number
 */
static unsigned allocate_value_numbers(entity *ent, unsigned vnum, ir_mode ***modes)
{
  ir_node *sel, *next;
  path_t *key, *path;
  set *pathes = new_set(path_cmp, 8);

  /* visit all Sel nodes in the chain of the entity */
  for (sel = get_entity_link(ent); sel; sel = next) {
    next = get_irn_link(sel);

    key  = find_path(sel, 0);
    path = set_find(pathes, key, sizeof(*key) + sizeof(key->path[0]) * key->path_len, path_hash(key));

    if (path)
      set_irn_link(sel, (void *)path->vnum);
    else {
      unsigned i;

      key->vnum = vnum++;

      set_insert(pathes, key, sizeof(*key) + sizeof(key->path[0]) * key->path_len, path_hash(key));

      set_irn_link(sel, (void *)key->vnum);
      ARR_EXTO(ir_mode *, *modes, (key->vnum + 15) & ~15);

      (*modes)[key->vnum] = get_type_mode(get_entity_type(get_Sel_entity(sel)));

      printf("  %s", get_entity_name(ent));
      for (i = 1; i < key->path_len; ++i) {
        if (is_entity(key->path[i].ent))
          printf(".%s", get_entity_name(key->path[i].ent));
        else
          printf("[%ld]", get_tarval_long(key->path[i].tv));
      }
      printf(" = %u (%s)\n", (int)get_irn_link(sel), get_mode_name((*modes)[key->vnum]));
    }
    free(key);
  }

  del_set(pathes);
  set_entity_link(ent, NULL);
  return vnum;
}

static char p;
static void *NODE_VISITED = &p;
static char t;
static void *LOOP_WALK = &t;
static char s;
static void *LOOP_HEAD_PHI = &s;

/**
 * environment for memory walker
 */
typedef struct _env_t {
  struct obstack obst;	/**< a obstack for the value blocks */
  int     nvals;        /**< number of values */
  ir_mode **modes;      /**< the modes of the values */
} env_t;

static char q;
static void *PRED_SEARCH = &q;

/**
 * Recursive  walk over the blocks, that are predecessors of "node".
 *
 * @param node     A Load or Phi node. The predecessor of this node muss be faund.
 * @param value    A struct pointer, that contains the block of node and their link.
 * @parm  repairs  A set, that contains all blocks, that have a array as link, and all Phis, that
 *                 have copies to repair.
 * @param pos      The position, that muss contain the  predecessor of node, in the array, that have got the bloks
 *                 from the repairs set.
 * @param phi_pred If node is a Phi, phi_pred is the predecessor number of this Phi, that muss be repair.
 */

static void pred_search(ir_node *node, repairs_t *value, set *repairs, int pos, int phi_pred)
{
  ir_node *nodes_block;
  repairs_t key, *value_pred;
  int i, n;

  DDMN(node);
  nodes_block = get_nodes_block(node);

  /* If the predecessor is found.  */
  if ((pos == -2 && value->link[pos] !=  NULL))
    return;

  n = get_Block_n_cfgpreds(nodes_block);

  for (i = n - 1; i >= 0; --i) {
    ir_node *pred = get_Block_cfgpred(nodes_block, i);

    key.irn = nodes_block;
    value_pred = set_find(repairs, &key, sizeof(key), HASH_PTR(key.irn));
    /* If nodes_block don't have the necessary information and the predecessor of it isn't
       visited "pred_search" was called recursive. Else the necessary information is found
       and the recursion stops. */
    if (value_pred == NULL || value_pred->link[pos] == NULL) {
      if (get_irn_link(pred) != PRED_SEARCH) {
	set_irn_link(node, PRED_SEARCH);
	pred_search(pred, value, repairs, pos, phi_pred);
      }
    }
    else {
      if (value->link[pos] == NULL && pos != -2) {
	if (get_Block_dom_depth(value->irn) >=
	    get_Block_dom_depth(get_nodes_block(value_pred->link[pos]))) {
	  value->link[pos] = value_pred->link[pos];
	  pos = -2;
	  break;
	}
      }
      else {
	if (get_irn_op(value->link[pos]) == op_Phi && pos != -2 &&
	    value->link[pos] != value_pred->link[pos]) {
	  set_Phi_pred(value->link[pos], phi_pred, value_pred->link[pos]);
	  pos = -2;
	  break;
	}
      }
    }
  }

  if (get_irn_link(node) == PRED_SEARCH)
    set_irn_link(node, NULL);
}

/**
 * Create a link for blocks, that have Phi, Load or Store nodes for scalar_replacement.
 *
 * @param node    A Phi, Load or Store node, that block muss get a  link.
 * @param env     Contains information about scalars number and mode.
 * @parm  repairs A set, that contains all blocks, that have a link, and all Phis, that
 *                 have copies to repair.
 */
static void block_link(ir_node *node, env_t *env, set *repairs)
{
  repairs_t key, *value;
  ir_node *nods_block ;
  int i;
  nods_block = get_nodes_block(node);
  /* If the block of the node have no link. */
  if (get_irn_link(nods_block) == NULL) {
    DDMN(nods_block);
    key.irn = nods_block;
    value = set_insert(repairs, &key, sizeof(key), HASH_PTR(key.irn));
    value->link = obstack_alloc(&env->obst, sizeof(ir_node *) * env->nvals);
     /* The block of the node be inserted in the set repairs, because it have yet a link.
	The link is a array with size equal to number of scalars.*/
    for (i = 0; i < env->nvals; i++)
      /* First  all links member be set to NULL. */
      value->link[i] = NULL;
    set_irn_link(nods_block, value->link);
  }
}

/**
 * Handle Phis, that get the memory edge of Loads or Stors, that were been scalar replaced or
 * will be scalar replaced.
 *
 * @param phi     A Phi node, that muss have mode_M.
 * @param env     Contains information about scalars number and mode.
 * @parm  repairs A set that contains all blocks having a link and all Phis that
 *                 have copies to repair.
 */
static void phi_handling(ir_node *phi, env_t *env, set *repairs)
{
  ir_node *phi_block, **link;
  repairs_t key, *value;
  int rem;
  int p = 0, i, phi_preds;
  ir_node **in;

  key.irn = phi;
  value = set_find(repairs, &key, sizeof(key), HASH_PTR(key.irn));

  /* Test if the phi have been handled. */
  if (value != NULL)
    return;

  rem = get_optimize();

  phi_block = get_nodes_block(phi);
  phi_preds = get_Phi_n_preds(phi);

  /* Test if the Phi have predecessors, that were been or will be scalar replaced. */
  for (i = 0; i < phi_preds; i++){
    ir_node *pred = get_Phi_pred(phi, i);
    ir_node *pred_block = get_nodes_block(pred);

    key.irn = pred_block;
    value = set_find(repairs, &key, sizeof(key), HASH_PTR(key.irn));

    if (value != NULL)
      p++;
  }
  /* Phis from the loop head muss be handled.*/
  if (get_irn_link(phi) == LOOP_HEAD_PHI)
    p++;

  /* If the Phi node have such predecessor(s), be inserted in the repairs set, else is nothing to do
     and exit "phi_handling".*/
  if (p) {
    key.irn = phi;
    value = set_insert(repairs, &key, sizeof(key), HASH_PTR(key.irn));
  }
  else
    return;

  //DDMN(phi);
  if (get_irn_link(phi_block) == NULL)
    block_link(phi, env, repairs);

  link = get_irn_link(phi_block);

  key.irn = phi_block;
  value = set_find(repairs, &key, sizeof(key), HASH_PTR(phi_block));

  in = alloca(phi_preds * sizeof(*in));

  /* We will build some Phi nodes. Beware, as they have all Unknown predecessors, CSE
   * would combine them into one. To prevent this, we must deactivate Optimizations here
   */
  for (p = 0; p < env->nvals; ++p) {

    for (i = 0; i < phi_preds; ++i)
      in[i] = new_Unknown(env->modes[p]);

    set_optimize(0);
    value->link[p] = new_r_Phi(current_ir_graph, get_nodes_block(phi), phi_preds, in, env->modes[p]);
    set_optimize(rem);
  }
}

/**
 * Handle Loads, that were been scalar replaced or
 * will be scalar replaced.
 *
 * @param load    A load node.
 * @param env     Contains information about scalars number and mode.
 * @parm  repairs A set, that contains all blocks, that have a  link, and all Phis, that
 *                 have copies to repair.
 */
static void load_handling(ir_node *load, env_t *env, set *repairs)
{
  repairs_t key, *value;
  ir_node *load_ptr, *load_mem, *nods_block;
  unsigned i;

  nods_block = get_nodes_block(load);
  load_ptr = get_Load_ptr(load);
  load_mem = get_Load_mem(load);
  // DDMN(load);
  /* The pointer predecessor of Load muss be a Sel node. */
  if (get_irn_op(load_ptr) == op_Sel) {
    /* If the link field of sel's entity is set to "ADDRESS_TAKEN", that means this value
       can't be scalar replaced. It is nothing to do and load_handling() must be exited. */
    if (get_entity_link(get_Sel_entity(load_ptr)) == ADDRESS_TAKEN)
      return;

    key.irn = nods_block;
    value = set_find(repairs, &key, sizeof(key), HASH_PTR(nods_block));

    /* Load's pointer predecessor's link field contains the position in the block's link, where
       must be searched the predecessor of this Load. */
    i = (unsigned)get_irn_link(load_ptr);

    /* If the link of Load's block doesn't contains at position "i" a node or isn't calculated,
       than pred_search() must be called .*/
    if (value == NULL) {
      block_link(load, env, repairs);
      key.irn = nods_block;
      value = set_find(repairs, &key, sizeof(key), HASH_PTR(nods_block));
      pred_search(load, value, repairs, i, 0);
    } else if (value->link[i] == NULL)
      pred_search(load, value, repairs, i, 0);

    /* If after the pred_search() call the link of Load's block at position "i" is equal to NULL,
       that means the loaded value wasn't initialized and the load predecessor is set to Unknown */
    if (value->link[i] == NULL)
      value->link[i] = new_Unknown(env->modes[i]);

    /* The Load node can be turned into a tuple now. This tuple will be optimized later. */
    turn_into_tuple(load, pn_Load_max);
    set_Tuple_pred(load, pn_Load_M, load_mem);
    set_Tuple_pred(load, pn_Load_res, value->link[i]);
    set_Tuple_pred(load, pn_Load_X_except, new_Bad());
  }
}

/**
 * A walker along the memory edge. Load and Phi nodes muss be found for optimization
 *
 * @param node    A node from the graph.
 * @param env     Contains information about scalars number and mode.
 * @parm  repairs A set, that contains all blocks, that have a  link, and all Phis, that
 *                have copies to repair.
*/
static void memory_edge_walk2(ir_node *node, env_t *env, set *repairs)
{
  int i, p, n = get_irn_arity(node);
  repairs_t key, *value, *value_block;
  ir_node *phi_pred;
  DDMN(node);

  for (i = 0; i < n; i++) {
    ir_node *pred = get_irn_n(node, i);

    if((get_irn_op(pred) == op_Proj    &&
	get_irn_mode(pred) == mode_M)  ||
       get_irn_mode(pred) == mode_T    ||
       is_memop(pred)                  ||
       get_irn_op(pred) == op_Call     ||
       get_irn_op(pred) == op_Alloc)
      memory_edge_walk2(pred, env, repairs);

    if(get_irn_op(pred) == op_Phi    &&
       get_irn_mode(pred) == mode_M &&
       get_irn_link(pred) != NODE_VISITED){
      set_irn_link(pred, NODE_VISITED);
      memory_edge_walk2(pred, env, repairs);
    }
  }

   if (get_irn_op(node) == op_Load)
     load_handling(node, env, repairs);

   if (get_irn_op(node) == op_Phi && get_irn_mode(node) == mode_M){
     key.irn = node;
     value = set_find(repairs, &key, sizeof(key), HASH_PTR(key.irn));
     /* If the phi is in the set " repairs ", then must be handled.*/
     if (value != NULL){
       //  DDMN(node);
       key.irn = get_nodes_block(node);
       value_block = set_find(repairs, &key, sizeof(key), HASH_PTR(key.irn));
       n = get_irn_arity(node);
       /* All predecessors of a Phi node muss be found.*/
       for(i = 0; i < env->nvals; i ++)
	 for(p = 0; p < n; p ++){
	   phi_pred = get_Phi_pred(value->irn, p);
	   pred_search(phi_pred, value_block, repairs, i, p);
	 }
     }
   }
   /* Reset the links, that have been used by the walk.*/
   if (get_irn_link(node) == NODE_VISITED)
     set_irn_link(node, NULL);
}

/**
 * A walker along the memory edge in a loop.The walker walk from the node to the loop head.
 * Load and Phi nodes muss be found for optimisation
 *
 * @param *node    A node from the graph.
 * @param *env     Contains information about scalars number and mode.
 * @parm  *repairs A set, that contains all blocks, that have a  link, and all Phis, that
 *                 have copies to repair.
*/
static void loop_walk(ir_node *node, env_t *env, set *repairs)
{
  int i, p, n = get_irn_arity(node);
  repairs_t key, *value, *value_block;
  ir_node *phi_pred;
  DDMN(node);

  /* Test if the loop head have been achieved. */
  if (has_backedges(get_nodes_block(node)))
    return;

  for (i = 0; i < n; i++) {
    ir_node *pred = get_irn_n(node, i);

    if((get_irn_op(pred) == op_Proj    &&
	get_irn_mode(pred) == mode_M)  ||
       get_irn_mode(pred) == mode_T    ||
       is_memop(pred)                  ||
       get_irn_op(pred) == op_Call     ||
       get_irn_op(pred) == op_Alloc)
      loop_walk(pred, env, repairs);

    if(get_irn_op(pred) == op_Phi    &&
       get_irn_mode(pred) == mode_M &&
       get_irn_link(pred) !=  LOOP_WALK) {
      set_irn_link(pred, LOOP_WALK);
      loop_walk(pred, env, repairs);
    }
  }

  if (get_irn_op(node) == op_Load)
    load_handling(node, env, repairs);

  if (get_irn_op(node) == op_Phi && get_irn_mode(node) == mode_M){
    key.irn = node;
    value = set_find(repairs, &key, sizeof(key), HASH_PTR(key.irn));
    /* If the phi is in the set " repairs ", then muss be handled.*/
    if(value != NULL){
      DDMN(node);
      key.irn = get_nodes_block(node);
      value_block = set_find(repairs, &key, sizeof(key), HASH_PTR(key.irn));
      n = get_irn_arity(node);
      /* All predecessors of a Phi node muss be found.*/
      for(i = 0; i < env->nvals; i ++)
        for(p = 0; p < n; p ++){
          phi_pred = get_Phi_pred(value->irn, p);
          pred_search(phi_pred, value_block, repairs, i, p);
        }
    }
  }
  /* Reset the links, that have beeb used by the walk.*/
  if(get_irn_link(node) == LOOP_WALK)
    set_irn_link(node, NULL);
}

/**
 * Handle Stores that were been scalar replaced or
 * will be scalar replaced.
 *
 * @param load    A store node.
 * @param env     Contains information about scalars number and mode.
 * @parm  repairs A set, that contains all blocks, that have a  link, and all Phis, that
 *                have copies to repair.
 */
static void store_handling(ir_node *store, env_t *env, set *repairs)
{
  repairs_t key, *value;
  ir_node *nods_block, *store_mem, *store_ptr, *store_value, **link, *phi;
  ir_loop *store_l;

  store_ptr = get_Store_ptr(store);

  /* The pointer predecessor of Store must be a Sel node.*/
  if (get_irn_op(store_ptr) == op_Sel) {
    /* If the link field of a Store's is set to "ADDRESS_TAKEN", that mean this value
       can't be scalar replaced. It's nothing to do and store_handling() must be exit.*/
    if (get_entity_link(get_Sel_entity(store_ptr)) == ADDRESS_TAKEN)
      return;

    /* If the Store node is in a loop, than the loop head of the Store
     * must be handled, if this was not already done. */
    store_l = get_irn_loop(store);
    if (store_l != NULL) {
      phi = get_loop_node(store_l, 0);
      if (get_irn_op(phi) != op_Block) {
        key.irn = get_nodes_block(phi);
        value = set_find(repairs, &key, sizeof(key), HASH_PTR(key.irn));
        if (value == NULL) {
          set_irn_link(phi, LOOP_HEAD_PHI);
          block_link(phi, env, repairs);
          phi_handling(phi, env, repairs);
        }
      }
    }

    DDMN(store);
    nods_block = get_nodes_block(store);

    key.irn = nods_block;
    value = set_find(repairs, &key, sizeof(key), HASH_PTR(nods_block));

    store_mem   = get_Store_mem(store);
    store_value = get_Store_value(store);

    if (store_l != NULL)
      loop_walk(store, env, repairs);
    else
      memory_edge_walk2(store, env, repairs);
    /* Store's pointer predecessor's link field contains the position in the block's link, where
     must be saved the value predecessor of this store. */
    value->link[(unsigned)get_irn_link(store_ptr)] = store_value;

    /* The store node can be turned into a tuple now. This tuple will be optimized later. */
    turn_into_tuple(store, pn_Store_max);
    set_Tuple_pred(store, pn_Store_M, store_mem);
    set_Tuple_pred(store, pn_Store_X_except, new_Bad());

    /* FIXME: link is uninitialized here */
    set_irn_link(nods_block, link);
  }
}

/**
 * A walker along the memory edge. Stores, Phis and their blocks as well Load's blocks must be found for optimization
 *
 * @param node    A node from the graph.
 * @param env     Contains information about scalars number and mode.
 * @parm  repairs A set, that contains all blocks, that have a link, and all Phis, that
 *                have copies to repair.
*/
static void memory_edge_walk(ir_node *node, env_t *env, set *repairs)
{
  int i, n = get_irn_arity(node);
  ir_op *op;

  DDMN(node);

  set_irn_link(get_nodes_block(node), NULL);

  for (i = n - 1; i >= 0; --i) {
    ir_node *pred = get_irn_n(node, i);
    ir_op *op = get_irn_op(pred);

    if ((op == op_Proj    &&
         get_irn_mode(pred) == mode_M) ||
       op == op_Load                   ||
       op == op_Store                  ||
       op == op_Call                   ||
       op == op_Alloc)
      memory_edge_walk(pred, env, repairs);

    if (op == op_Phi                 &&
        get_irn_mode(pred) == mode_M &&
        get_irn_link(pred) != NODE_VISITED) {
      set_irn_link(pred, NODE_VISITED);
      memory_edge_walk(pred, env, repairs);
    }
  }

  op =  get_irn_op(node);
  if (op == op_Store || op == op_Load)
    block_link(node, env, repairs);

  if (op == op_Phi && get_irn_mode(node) == mode_M)
    phi_handling(node, env, repairs);

  if (op == op_Store)
    store_handling(node, env, repairs);

  /* Reset the links, that have been used by the walk.*/
  if (get_irn_link(node) == NODE_VISITED)
    set_irn_link(node, NULL);
}

/**
 *  Make scalar replacement.
 *
 * @param envals  The number of scalars.
 * @parm  repairs A set, that contains all blocks, that have a  link, and all Phis, that
 *                have copies to repair.
 * @param modes   A flexible array, containing all the modes of
 *                the value numbers.
 */
static void do_scalar_replacements(int nvals, set * repairs, ir_mode **modes)
{
  ir_node *end_block = get_irg_end_block(current_ir_graph);
  int i;
  int n = get_Block_n_cfgpreds(end_block);
  env_t env;

  obstack_init(&env.obst);
  env.nvals = nvals;
  env.modes = modes;

  /* Search for scalars.*/
  for (i = 0; i < n; ++i) {
    ir_node *pred = get_Block_cfgpred(end_block, i);
    if (get_irn_op(pred) != op_Block)
      memory_edge_walk(pred, &env, repairs);
  }

  /* Search for predecessors of scalars.*/
  for (i = 0; i < n; ++i) {
    ir_node *pred = get_Block_cfgpred(end_block, i);
    if (get_irn_op(pred) != op_Block)
      memory_edge_walk2(pred, &env, repairs);
  }
  obstack_free(&env.obst, NULL);
}

/*
 * Find possible scalar replacements
 *
 * @param irg  The current ir graph.
 */
void find_scalar_replacements(ir_graph *irg)
{
  unsigned nvals;
  int i;
  scalars_t key, *value;
  ir_node *irg_frame;
  ir_mode **modes;
  set *set_ent, *repairs;
  type *ent_type;

  /* Call algorithm that computes the out edges */
  compute_outs(irg);
  /* Call algorithm that computes the loop information */
  construct_backedges(irg);

  /* Call algorithm that computes the backedges */
  construct_cf_backedges(irg);

  /* Call algorithm that computes the dominator trees. */
  compute_doms(irg);

  /* Find possible scalar replacements */
  do_find_scalar_replacements(irg);

  printf("Called for %s\n", get_entity_name(get_irg_entity(irg)));

  /* Insert in set the scalar replacemnts. */
  irg_frame = get_irg_frame(irg);
  nvals = 0;
  modes = NEW_ARR_F(ir_mode *, 16);
  set_ent = new_set(ent_cmp, 8);

  for (i = 0 ; i < get_irn_n_outs(irg_frame); i++) {
    ir_node *succ = get_irn_out(irg_frame, i);

    if (get_irn_op(succ) == op_Sel) {
      entity *ent = get_Sel_entity(succ);

      if (get_entity_link(ent) == NULL || get_entity_link(ent) == ADDRESS_TAKEN)
        continue;

      ent_type = get_entity_type(ent);

      key.ent = ent;
      key.ent_owner = get_entity_owner (ent);
      set_insert(set_ent, &key, sizeof(key), HASH_PTR(key.ent));

      if (is_Array_type(ent_type)) {
        printf("<<<<<<<found array %s\n", get_entity_name(ent));
      }
      else if (is_Struct_type(ent_type)) {
        printf("<<<<<<<found struct %s\n", get_entity_name(ent));
      }
      else if(is_atomic_type(ent_type))
        printf("<<<<<<<found atomic value %s\n", get_entity_name(ent));
      else {
        assert(0 && "\nDon't a array, struct or atomic value");
      }

      nvals = allocate_value_numbers(ent, nvals, &modes);
    }
  }

  printf("  %u values will be needed\n", nvals);

  /* A set of nodes that must be repaired. These are Phi or Unknown nodes. */
  repairs = new_set(set_cmp, 8);

  /* If scalars ware found. */
  if (nvals)
    do_scalar_replacements(nvals, repairs, modes);

  value = set_first(set_ent);
  for (; value != NULL; value = set_next(set_ent))
    remove_class_member(value->ent_owner, value->ent);

  del_set(set_ent);
  del_set(repairs);
  DEL_ARR_F(modes);
}
