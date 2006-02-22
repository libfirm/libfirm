/*
 * Project:     libFIRM
 * File name:   ir/opt/scalar_replace.c
 * Purpose:     scalar replacement of arrays and compounds
 * Author:      Beyhan Veliev
 * Modified by: Michael Beck
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "data_flow_scalar_replace.h"
#include "irflag_t.h"
#include "irouts.h"
#include "pset.h"
#include "ircons_t.h"
#include "hashptr.h"
#include "irgwalk.h"
#include "irgmod.h"
#include "irnode_t.h"
#include "irtools.h"
#include "irdump.h"
#include "irloop.h"
#include "analyze_irg_args.h"
#include "irprintf.h"

#define SET_ENT_VNUM(ent, vnum) set_entity_link(ent, INT_TO_PTR(vnum))
#define GET_ENT_VNUM(ent)       (unsigned)PTR_TO_INT(get_entity_link(ent))
#define SET_IRN_VNUM(irn, vnum) set_irn_link(irn, INT_TO_PTR(vnum))
#define GET_IRN_VNUM(irn)       (unsigned)PTR_TO_INT(get_irn_link(irn))


/* To save for an entity all leaves for that the memory
 * edge was split.*/
typedef struct _ent_leaves_t{
  entity  *ent;
  pset *leaves;
} ent_leaves_t;

typedef struct _sels_t {
  ir_node *sel;
  entity  *ent;
}sels_t;

typedef struct _call_access_t {
  ir_node *call;
  unsigned int access_type;
}call_access_t;

typedef struct _fixlist_entry_t {
  ir_node *irn;
  unsigned int vnum;
}fixlist_entry_t;

typedef struct _syncs_fixlist_entry_t {
  ir_node *irn;
  int *accessed_vnum;
}syncs_fixlist_entry_t;

/* A entry, that save the memory
 * edge state and the access state for this leave
 * int the array,that is created for every block.*/
typedef struct _leave_t {
  ir_node *mem_edge_state;  /**< memory state for this scalar.*/
  unsigned int access_type; /**< access state for this scalar.*/
  set *calls;              /**< call nodes,that change this scalar.*/
}value_arr_entry_t;

/**
 * environment for memory walker
 */
typedef struct _env_t {
  struct obstack obst;                   /**< a obstack for the memory edge */
  set                   *set_sels;       /**< a set with all sels, that are reachable from an entity with a scalar.*/
  set                   *set_ent;        /**< a set with all entities that have one or more scalars.*/
  fixlist_entry_t       *fix_phis;       /**< list of all Phi nodes that must be fixed */
  fixlist_entry_t       *fix_ls;         /**< list of all Load or Store nodes that must be fixed */
  syncs_fixlist_entry_t *fix_syncs;      /**< list of all Sync nodes that must be fixed */
  unsigned int          nvals;           /**< to save the number of scalars.*/
  unsigned int          gl_mem_vnum;     /**< indicate the position of the globule memory edge state in var_arr.*/
  unsigned int          vnum_state;      /**< indicate the position of the value number state in var_arr.*/
  unsigned int          changes;         /**< to save if by anlyse_calls is changed anything.*/
} env_t;

/**
 * A path element entry: it is either an entity
 * or a tarval, because we evaluate only constant array
 * accesses like a.b.c[8].d
 */
typedef union {
  entity *ent;
  tarval *tv;
} path_elem_t;

/**
 * An access path, used to assign value numbers
 * to variables that will be scalar replaced
 */
typedef struct _path_t {
  unsigned    vnum;      /**< the value number */
  unsigned    path_len;  /**< the length of the access path */
  path_elem_t path[1];   /**< the path */
} path_t;

/* we need a special address that serves as an address stored. */
static char _s;
static void *ADDRESS_STORED = &_s;


/**
 * Compare two elements of the ent_leaves_t set.
 *
 * @return 0 if they are identically
 */
static int ent_leaves_t_cmp(const void *elt, const void *key, size_t size)
{
  const ent_leaves_t *c1 = elt;
  const ent_leaves_t *c2 = key;

  return c1->ent != c2->ent;
}

/**
 * Compare two elements of the ent_access_t set.
 *
 * @return 0 if they are identically
 */
static int ent_cmp(const void *elt, const void *key, size_t size)
{
  const entity *c1 = elt;
  const entity *c2 = key;

  return c1 != c2;
}

/**
 * Compare two elements of the sels_t set.
 *
 * @return 0 if they are identically
 */
static int sels_cmp(const void *elt, const void *key, size_t size)
{
  const sels_t *c1 = elt;
  const sels_t *c2 = key;

  return c1->sel != c2->sel;
}

/**
 * Compare two elements of the leave_t set.
 *
 * @return 0 if they are identically
 */
static int leave_cmp(const void *elt, const void *key, size_t size)
{
  const ir_node *c1 = elt;
  const ir_node *c2 = key;

  return c1 != c2;
}

/**
 * Compare two elements of the call_access_t set.
 *
 * @return 0 if they are identically
 */
static int call_cmp(const void *elt, const void *key, size_t size)
{
  const call_access_t *c1 = elt;
  const call_access_t *c2 = key;

  return c1->call != c2->call;
}

/**
 * Compare two paths.
 *
 * @return 0 if they are identically
 */
static int path_cmp(const void *elt, const void *key, size_t size)
{
  const path_t *p1 = elt;
  const path_t *p2 = key;

  /* we can use memcmp here, because identical tarvals should have identical addresses */
  return memcmp(p1->path, p2->path, p1->path_len * sizeof(p1->path[0]));
}

/**
 * Calculate a hash value for a path.
 */
static unsigned path_hash(const path_t *path)
{
  unsigned hash = 0;
  unsigned i;

  for (i = 0; i < path->path_len; ++i)
    hash ^= (unsigned)PTR_TO_INT(path->path[i].ent);

  return hash >> 4;
}

/**
 * Returns non-zero, if all induces of a Sel node are constants.
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

/*
 * Returns non-zero, if the address of an entity
 * represented by a Sel node (or it's successor Sels) is taken.
 */
static int is_address_taken(ir_node *sel)
{
  int i;

  if (! is_const_sel(sel))
    return 1;

  for (i = get_irn_n_outs(sel) - 1; i >= 0; --i) {
    ir_node *succ = get_irn_out(sel, i);

    switch (get_irn_opcode(succ)) {
    case iro_Load:
      /* ok, we just load from that entity */
      break;

    case iro_Store:
      /* check that Sel is not the Store's value */
      if (get_Store_value(succ) == sel)
        return 1;
      break;

    case iro_Sel: {
      /* Check the Sel successor of Sel */
      int res = is_address_taken(succ);

      if (res)
        return 1;
      break;
    }

    case iro_Call:
      /* The address of an entity is given as a parameter.
       * We analyzes that later and optimizes this scalar
       * if possible.
       */
      return 0;

    default:
      /* another op, the address is taken */
      return 1;
    }
  }
  return 0;
}

/**
 * Link all Sels with the entity.
 *
 * @param ent  the entity that will be scalar replaced
 * @param sel  a Sel node that selects some fields of this entity
 */
static void link_all_leave_sels(entity *ent, ir_node *sel)
{
  int i, n;

  n = get_irn_n_outs(sel);
  for (i = 0; i < n; ++i) {
    ir_node *succ = get_irn_out(sel, i);

    if (get_irn_op(succ) == op_Sel)
      link_all_leave_sels(ent, succ);

  }

   /* if Sel nodes with memory inputs are used, a entity can be
    * visited more than once causing a ring here, so we use the
    * node flag to mark linked nodes
    */
   if (irn_visited(sel))
    return;

  /*
   * we link the sels to the entity.
   */
  set_irn_link(sel, get_entity_link(ent));
  set_entity_link(ent, sel);

  mark_irn_visited(sel);
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
 * Sel nodes, that selects anythings of this entity.
 * Otherwise, the link will be ADDRESS_TAKEN or NULL.
 *
 * @return  non-zero if at least one entity could be replaced
 *          potentially
 */
static int find_possible_replacements(ir_graph *irg)
{
  ir_node *irg_frame = get_irg_frame(irg);
  int i, n;
  int res = 0;

  inc_irg_visited(irg);

  n = get_irn_n_outs(irg_frame);

  /*
   * First, clear the link field of all interestingentities.
   * Note that we did not rely on the fact that there is only
   * one Sel node per entity, so we might access one entity
   * more than once here.
   * That's why we have need two loops.
   */
  for (i = 0; i < n; ++i) {
    ir_node *succ = get_irn_out(irg_frame, i);

    if (get_irn_op(succ) == op_Sel) {
      entity *ent = get_Sel_entity(succ);
      set_entity_link(ent, NULL);
    }
  }

  /*
   * Check the ir_graph for Sel nodes. If the entity of Sel
   * isn't a scalar replacement set the link of this entity
   * equal ADDRESS_TAKEN.
   */
  for (i = 0; i < n; ++i) {
    ir_node *succ = get_irn_out(irg_frame, i);

    if (get_irn_op(succ) == op_Sel) {
      entity *ent = get_Sel_entity(succ);
      ir_type *ent_type;

      if (get_entity_link(ent) == ADDRESS_TAKEN)
        continue;

      ent_type = get_entity_type(ent);

      /* we can handle arrays, structs and atomic types yet */
      if (is_Array_type(ent_type) || is_Struct_type(ent_type) || is_atomic_type(ent_type)) {
        if (is_address_taken(succ)) {
          if (get_entity_link(ent)) /* killing one */
            --res;
          set_entity_link(ent, ADDRESS_TAKEN);
        }
        else {
          /* possible found one */
          if (get_entity_link(ent) == NULL)
            ++res;
          link_all_leave_sels(ent, succ);
        }
      }
    }
  }

  return res;
}

static int is_leave_sel(ir_node *sel) {
  int i;
  ir_node *succ;

  for(i = get_irn_n_outs(sel) - 1; i >= 0; i--) {
    succ = get_irn_out(sel, i);
    if(get_irn_op(succ) == op_Sel)
      return 0;
  }

  return 1;
}

/**
 * Return a path from the Sel node sel to it's root.
 *
 * @param sel  the Sel node
 * @param len  the length of the path so far
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
 * @param sels  a set that will contain all Sels that have a value number
 * @param ent   the entity that will be scalar replaced
 * @param vnum  the first value number we can assign
 * @param modes a flexible array, containing all the modes of
 *              the value numbers.
 *
 * @return the next free value number
 */
static unsigned allocate_value_numbers(set *set_sels, pset *leaves, entity *ent, unsigned vnum)
{
  ir_node *sel, *next;
  path_t *key, *path;
  sels_t       key_sels;
  set *pathes = new_set(path_cmp, 8);

  /* visit all Sel nodes in the chain of the entity */
  for (sel = get_entity_link(ent); sel; sel = next) {
    next = get_irn_link(sel);

    /* we save for every sel it root entity, why
     * we need this information, when we split the memory edge,
     * and we must mark this sel for later. */
     key_sels.ent = ent;
     key_sels.sel = sel;
     set_insert(set_sels, &key_sels, sizeof(key_sels), HASH_PTR(sel));

    if(! is_leave_sel(sel))
      continue;

    /* We have found a leave and we add it to the pset of this entity.*/
    pset_insert_ptr(leaves, sel);

    key  = find_path(sel, 0);
    path = set_find(pathes, key, sizeof(*key) + sizeof(key->path[0]) * key->path_len, path_hash(key));

    if (path)
      SET_IRN_VNUM(sel, path->vnum);
    else {

      key->vnum = vnum++;

      set_insert(pathes, key, sizeof(*key) + sizeof(key->path[0]) * key->path_len, path_hash(key));

      SET_IRN_VNUM(sel, key->vnum);
    }
    free(key);
  }

  del_set(pathes);
  set_entity_link(ent, NULL);
  return vnum;
}
/* Analyze if this scalar was stored in this block
   or earlier.*/
static int is_scalar_stored(ir_node *call_blk, int ent_vnum) {

  value_arr_entry_t *val_arr, *val_arr_pred;
  ir_node *pred;
  int n;

  val_arr = get_irn_link(call_blk);

  if(val_arr[ent_vnum].access_type == 0)
    return 0;

  n = get_Block_n_cfgpreds(call_blk) - 1;

  for( ; n >= 0; n--) {
    pred = get_Block_cfgpred(call_blk, n);
    pred = get_nodes_block(pred);
    val_arr_pred = get_irn_link(pred);

    if(val_arr_pred[ent_vnum].access_type != 0)
      /* This scalar wasn't stored in this block.*/
      return 1;
  }
  return 0;
}


/* The function handles the call nodes, that have
 * as parameter a scalar*/
static void handle_call(env_t *env, ir_node *call, pset *accessed_entities) {

  ent_leaves_t  key_ent, *value_ent;
  value_arr_entry_t *val_arr;
  call_access_t key_call, *value_call;
  ir_node *call_blk, *new_mem_state, *leave;
  ir_node *sync, **in;
  entity *ent;
  fixlist_entry_t *l;
  syncs_fixlist_entry_t *s;
  int fix_irn = 0, *accessed_leaves_vnum = NULL;
  unsigned ent_vnum;

  call_blk = get_nodes_block(call);
  val_arr  = get_irn_link(call_blk);
  /* To save the memory edges, that must be
   * synced.*/
  in       = NEW_ARR_F(ir_node *, 1);
  /* To save the value numbers of the memory
   * edges that must be repaired.*/
  accessed_leaves_vnum = NEW_ARR_F(int, 0);

  /* We get the memory successor of the call node.
   * It is the new memory state for all synced memory
   * edges.*/
  new_mem_state = get_irn_out(call, 0);
  if(get_irn_mode(new_mem_state) != mode_M)
    new_mem_state = get_irn_out(call,1);

  if(val_arr[env->gl_mem_vnum].mem_edge_state == NULL) {
    in[0] = new_Unknown(mode_M);
    fix_irn = 1;
  } else
    in[0] = val_arr[env->gl_mem_vnum].mem_edge_state;

  for(ent = pset_first(accessed_entities); ent; ent = pset_next(accessed_entities)) {
    /* Whit this loop we iterate all accessed entities an collect
     * all memory edges, that we must sync.*/
    ent_vnum = GET_ENT_VNUM(ent);

    key_call.call = call;
    value_call = set_find(val_arr[ent_vnum].calls, &key_call, sizeof(key_call), HASH_PTR(key_call.call));

    key_ent.ent = ent;
    value_ent = set_find(env->set_ent, &key_ent, sizeof(key_ent), HASH_PTR(key_ent.ent));

    if(!is_scalar_stored(call_blk, ent_vnum)) {
      /* This scalar's address wasn't stored in this block.*/
      switch(value_call->access_type) {

      case ptr_access_none :
        /* In this case we have nothing to do.*/
      break;

      case ptr_access_read:
      case ptr_access_write:
      case ptr_access_rw:
      case 5:
      case ptr_access_all:
        /* All this cases must be traded equal.*/

        for(leave = pset_first(value_ent->leaves); leave; leave = pset_next(value_ent->leaves)){
          /* All this memory edges must be synced.*/
          if(val_arr[GET_IRN_VNUM(leave)].mem_edge_state != NULL)
            ARR_APP1(ir_node *, in, val_arr[GET_IRN_VNUM(leave)].mem_edge_state);
          else {
            ARR_APP1(int, accessed_leaves_vnum, GET_IRN_VNUM(leave));
            ARR_APP1(ir_node *, in, new_Unknown(mode_M));
            fix_irn = 1;
          }
          /* We update the memory state of this leave.*/
          if(value_call->access_type != ptr_access_read)
            val_arr[GET_IRN_VNUM(leave)].mem_edge_state = new_mem_state;
        }

      /* We are ready.*/
      break;
      }
    }
  }

  /* We must update the global memory state.*/
  val_arr[env->gl_mem_vnum].mem_edge_state = new_mem_state;

  if(ARR_LEN(in) == 1) {
    /* we must set the call memory to gobale momory*/
    set_Call_mem(call,in[0]);

    if(fix_irn) {
      /* We add this call node to the call fix list..*/
      l = obstack_alloc(&env->obst, sizeof(*l));
      l->irn = call;
      l->vnum = env->gl_mem_vnum;
      set_irn_link(l->irn, env->fix_ls);
      env->fix_ls = l;
    }
  } else {
   /* We create the sync and set it as memory predecessor of the call node.*/
      sync = new_r_Sync(current_ir_graph, call_blk, ARR_LEN(in), in);
      set_Call_mem(call, sync);

      if(fix_irn) {
        /* We add this sync node to the sync's fix list.*/
        s = obstack_alloc(&env->obst, sizeof(*s));
        s->irn  = sync;
        s->accessed_vnum = accessed_leaves_vnum;
        set_irn_link(sync, env->fix_syncs);
        env->fix_syncs = s;
      }
  }
  DEL_ARR_F(in);
}

/* The function split the memory edge.*/
static void split_memory_edge(ir_node *irn, void *ctx) {

   env_t   *env = ctx;
   ir_node *new_mem_state, *mem_state, *unk;
   ir_node *leave, *sel, *irn_blk, **in;
   ir_op   *op;
   sels_t  key_sels, *value_sels;
   ent_leaves_t key_ent, *value_ent;
   value_arr_entry_t *val_arr;
   fixlist_entry_t *l;
   pset *accessed_entities;
   unsigned ent_vnum, sel_vnum;
   int i, j, n;


   op = get_irn_op(irn);

   if(op == op_Load || op == op_Store) {

     if(op == op_Load)
       key_sels.sel = get_Load_ptr(irn);
     else
       key_sels.sel = get_Store_ptr(irn);

     value_sels = set_find(env->set_sels, &key_sels, sizeof(key_sels), HASH_PTR(key_sels.sel));

     if(value_sels != NULL) {
     /* we have found a load or store, that use a sel of our set
      * and we must split or extend, if the memory edge have been
      * split for this sel, the memory edge.*/

       key_ent.ent = value_sels->ent;
       value_ent = set_find(env->set_ent, &key_ent, sizeof(key_ent), HASH_PTR(key_ent.ent));
       assert(value_ent && " This sel's entity isn't int the entity set.");

       leave = pset_find_ptr(value_ent->leaves, key_sels.sel);

       assert(leave && "Anything in data_flow_scalar_replacment algorithm is wrong.");

       ent_vnum = GET_ENT_VNUM(value_ent->ent);
       sel_vnum = GET_IRN_VNUM(leave);

       irn_blk = get_nodes_block(irn);
       val_arr   = get_irn_link(irn_blk);

       if(val_arr[ent_vnum].access_type == 0)
         /* We have found a scalar, that address i not stored as jet.*/
         i = sel_vnum;
       else
         /* This scalar have been stored.*/
         i = env->gl_mem_vnum;

       if(val_arr[i].mem_edge_state == NULL) {
          /* We split now for this sel the memory edge in this block.*/
          mem_state = new_Unknown(mode_M);
          /* We must mark this node to fix later*/
          l = obstack_alloc(&env->obst, sizeof(*l));
          l->irn  = irn;
          l->vnum = i;

          set_irn_link(irn, env->fix_ls);
          env->fix_ls = l;
       }else
        /* We have split the memory edge and the current state is saved.*/
        mem_state = val_arr[i].mem_edge_state;

        /* We set this Load or Store to the memory edge of this
         * sel.*/
        if(op == op_Load)
          set_Load_mem(irn, mem_state);
        else
          set_Store_mem(irn, mem_state);

         /* When we have split or extended the memory edge we must
          * update the memory_edge_state of this sel*/
         new_mem_state = get_irn_out(irn, 0);
         if(get_irn_mode(new_mem_state) == mode_M)
          val_arr[i].mem_edge_state = new_mem_state;
         else
          val_arr[i].mem_edge_state = get_irn_out(irn, 1);
        }
      }
    else
      if (op == op_Phi && get_irn_mode(irn) == mode_M) {
        /*
         * found a memory Phi: Here, we must create new Phi nodes
         */
        irn_blk = get_nodes_block(irn);
        val_arr = get_irn_link(irn_blk);

        n = get_Block_n_cfgpreds(irn_blk);

        in = alloca(sizeof(*in) * n);

        for (i = val_arr[env->gl_mem_vnum].access_type - 1; i >= 0; --i) {
        unk = new_Unknown(mode_M);
        for (j = n - 1; j >= 0; --j)
          in[j] = unk;

        val_arr[i].mem_edge_state = new_r_Phi(current_ir_graph, irn_blk, n, in, mode_M);

        l = obstack_alloc(&env->obst, sizeof(*l));
        l->irn = val_arr[i].mem_edge_state;
        l->vnum = i;

        set_irn_link(val_arr[i].mem_edge_state, env->fix_phis);
        env->fix_phis = l;
    }
   }
   if(op == op_Call) {
      /* We save in this set all entities,
       * that are accessed from this call node.*/
      accessed_entities = new_pset(ent_cmp, 8);

       for ( i = get_Call_n_params(irn) - 1; i >= 0; i--) {
         sel = get_Call_param(irn, i);
         if(get_irn_op(sel) == op_Sel) {
           key_sels.sel = sel;
           value_sels   = set_find(env->set_sels, &key_sels, sizeof(key_sels), HASH_PTR(key_sels.sel));
         }

         if(value_sels != NULL)
           /* We save in this set all accessed entities from this call node.*/
          pset_insert(accessed_entities, value_sels->ent, HASH_PTR(value_sels->ent));
      }
      if(pset_count(accessed_entities))
       handle_call(env, irn, accessed_entities);
  }
}

/**
 * searches through blocks beginning from block for value
 * vnum and return it.
 */
static ir_node *find_value(ir_node *block, unsigned vnum)
{
  value_arr_entry_t *val_arr;
  int i;
  ir_node *res;

  if (Block_not_block_visited(block)) {
    mark_Block_block_visited(block);

    val_arr = get_irn_link(block);

    if (val_arr[vnum].mem_edge_state)
      return val_arr[vnum].mem_edge_state;

    for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
      ir_node *pred = get_Block_cfgpred(block, i);

      res = find_value(get_nodes_block(pred), vnum);
      if (res)
        return res;
    }
  }
  return NULL;
}

/**
 * fix the Load/Store or Call list
 */
static void fix_ls(env_t *env)
{
  fixlist_entry_t *l;
  ir_node      *irn, *block, *pred, *val;
  ir_op        *op;
  int          i;

  for (l = env->fix_ls; l; l = get_irn_link(irn)) {
    irn = l->irn;

    op     = get_irn_op(irn);
    block  = get_nodes_block(irn);
    for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
      pred = get_Block_cfgpred(block, i);
      pred = get_nodes_block(pred);

      inc_irg_block_visited(current_ir_graph);
      val = find_value(pred, l->vnum);

      if (val)
        break;
    }

    if(op == op_Store)
      set_Store_mem(irn, val);
    else
      if(op == op_Load)
        set_Load_mem(irn, val);
      else
        set_Call_mem(irn, val);
  }
}

/**
 * fix the Phi list
 */
static void fix_phis(env_t *env)
{
  fixlist_entry_t *l;
  ir_node      *phi, *block, *pred, *val;
  int          i;

  for (l = env->fix_phis; l; l = get_irn_link(phi)) {
    phi = l->irn;

    block = get_nodes_block(phi);
    for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {

      pred = get_Block_cfgpred(block, i);
      pred = get_nodes_block(pred);

      inc_irg_block_visited(current_ir_graph);
      val = find_value(pred, l->vnum);

      if (val)
        set_irn_n(phi, i, val);
    }
  }
}


/**
 * fix the Sync list
 */
static void fix_syncs(env_t *env)
{
  syncs_fixlist_entry_t *l;
  ir_node      *sync, *block, *pred, *val;
  int i, k = 0;


  for (l = env->fix_syncs; l; l = get_irn_link(sync)) {
    sync = l->irn;

    /* The sync block must have one predecessor, when it
       have unknown nodes as predecessor.*/
    block = get_nodes_block(sync);
    pred  = get_Block_cfgpred(block, 0);
    pred  = get_nodes_block(pred);

    /* We first repair the global memory edge at the first position of sync predecessors.*/
    if(get_irn_op(get_irn_n(sync, 0)) == op_Unknown) {
      inc_irg_block_visited(current_ir_graph);
      val = find_value(pred, env->gl_mem_vnum);
      set_irn_n(sync, 0, val);
    }

    for (i = get_irn_arity(sync) - 1; i >= 0; --i) {
      /* We repair the leaves*/
      if(get_irn_op(get_irn_n(sync, i)) == op_Unknown) {
        inc_irg_block_visited(current_ir_graph);
        val = find_value(pred, l->accessed_vnum[k++]);
        set_irn_n(sync, i, val);
      }
    }
  }
}
/* For the end node we must sync all memory edges.*/
static void sync_mem_edges(env_t *env) {

  ent_leaves_t *entry_ent;
  ir_node      *leave;
  value_arr_entry_t *val_arr;
  ir_node **in, *sync, *Return, *Return_blk;
  int i = 1;

  Return     = get_Block_cfgpred(get_irg_end_block(current_ir_graph), 0);
  Return_blk = get_nodes_block(Return);
  val_arr    = get_irn_link(Return_blk);
  /* We allocate the memory, that we need for the successors of the sync.*/
  in     = malloc(sizeof(ir_node*) *val_arr[env->vnum_state].access_type);
  /* The global memory edge is the first predecessor of this sync node.*/
  if(val_arr[env->gl_mem_vnum].mem_edge_state == NULL) {
    /* We must search through blocks for this memory state.*/
    inc_irg_block_visited(current_ir_graph);
    in[0] = find_value(Return_blk, env->gl_mem_vnum);
  }
  else
    in[0] = val_arr[env->gl_mem_vnum].mem_edge_state;


  for(entry_ent = set_first(env->set_ent); entry_ent; entry_ent = set_next(env->set_ent)) {
    if(val_arr[GET_ENT_VNUM(entry_ent->ent)].access_type ==0)
      /* The address of this entity was not stored. We must sync the memory edges of it's scalars.*/
      for(leave = pset_first(entry_ent->leaves); leave; leave = pset_next(entry_ent->leaves)) {
        if(val_arr[GET_IRN_VNUM(leave)].mem_edge_state == NULL) {
          /* We must search through blocks for this memory state.*/
          inc_irg_block_visited(current_ir_graph);
          in[i] = find_value(Return_blk, GET_IRN_VNUM(leave));
        }
        else
          in[i] = val_arr[GET_IRN_VNUM(leave)].mem_edge_state;
        i++;
    }
  }

  //assert(i < env->nvals && "Our nvals algorithm is baggy." );

  sync = new_r_Sync(current_ir_graph, Return_blk, val_arr[env->vnum_state].access_type, in);
  set_Return_mem(Return, sync);

  free(in);
}

/**
 * Walker: allocate the value array for every block.
 */
static void alloc_value_arr(ir_node *block, void *ctx)
{
  env_t *env = ctx;
  int i;
  value_arr_entry_t *var_arr = obstack_alloc(&env->obst, sizeof(value_arr_entry_t) *(env->nvals + set_count(env->set_ent) + 1));

  /* the value array is empty at start */
  memset(var_arr, 0, sizeof(value_arr_entry_t) * (env->nvals + set_count(env->set_ent) + 1));
  set_irn_link(block, var_arr);
 /* We set the block value number state to optimal and later we update this.*/
  var_arr[env->vnum_state].access_type = env->nvals;

  if(get_irg_start_block(current_ir_graph) == block)
    for(i = (env->nvals + set_count(env->set_ent)) ; i >=0; i--)
      var_arr[i].mem_edge_state = get_irg_initial_mem(current_ir_graph);

}

/* Analyze call nodes to get information, if they store the address of a scalar. */
static void analyse_calls(ir_node *irn, void *ctx) {

  int           i, vnum;
  unsigned int  acces_type;
  env_t         *env = ctx;
  ir_node       *param, *call_ptr, *blk;
  ir_op         *op;
  entity        *meth_ent;
  sels_t        key_sels, *value_sels;
  call_access_t key_call, *value_call;
  value_arr_entry_t *val_arr;
  ent_leaves_t key_ent, *value_ent;

  if(get_irn_op(irn) != op_Call)
    return;

    for ( i = get_Call_n_params(irn) - 1; i >= 0; i--) {
      param = get_Call_param(irn, i);
      if(get_irn_op(param) == op_Sel) {

         key_sels.sel = param;
         value_sels   = set_find(env->set_sels, &key_sels, sizeof(key_sels), HASH_PTR(key_sels.sel));
          if(value_sels != NULL ) {
            /* We have found a call, that have as parameter a sel from our set_sels.*/
            call_ptr = get_Call_ptr(irn);
            op = get_irn_op(call_ptr);

            if(op == op_SymConst && get_SymConst_kind(call_ptr) == symconst_addr_ent)
              meth_ent = get_SymConst_entity(call_ptr);
            else
              continue;
            /* we get the access type for our sel.*/
            acces_type = get_method_param_access(meth_ent, i);
            /* we save the access type and this call in the array allocated for this block.
             * The value number of this entity get us the position in the array to save this
             * information. Why we expect more calls as one we allocate a set.*/
            vnum    = GET_ENT_VNUM(value_sels->ent);
            blk     = get_nodes_block(irn);
            val_arr = get_irn_link(blk);

            if(val_arr[vnum].access_type > 3)
              /* The address of this entity have been stored.*/
              continue;

            if(val_arr[vnum].calls == NULL)
              /* for this entity i have found the firs call in this block and we must allocate the set.*/
              val_arr[vnum].calls = new_set(call_cmp, 8);

            /* This call performs anything with the scalar and we must mark it.*/
            key_call.call = irn;
            key_call.access_type = acces_type;
            value_call = set_insert(val_arr[vnum].calls, &key_call, sizeof(key_call), HASH_PTR(key_call.call));

            if(value_call->access_type < acces_type)
              /* this case tread, when a call access an entity more at once.
               * Than we must save the highest access type.*/
               value_call->access_type = acces_type;

            if(acces_type > 3) {
              /* This call save the address of our scalar and we can't
               * use the scalars of this entity for optimization as from now.
               * we mark this.*/
              val_arr[vnum].access_type = acces_type;
              /* We must update our scalars number.*/
              key_ent.ent = value_sels->ent;
              value_ent = set_find(env->set_ent, &key_ent, sizeof(key_ent), HASH_PTR(key_ent.ent));
              val_arr[env->vnum_state].access_type -= pset_count(value_ent->leaves);
            }
       }
     }
    }
}
/* Update the access information of a block if a predecessor of
 * this black have a store access for an entity.*/
static void set_block_access(ir_node *irn, void *ctx){

  value_arr_entry_t *val_arr, *val_arr_pred;
  ent_leaves_t      *value_leaves;
  env_t             *env = ctx;
  ir_node           *pred, *pred_blk;
  int               i, vnum;

  val_arr = get_irn_link(irn);

  for( i = get_Block_n_cfgpreds(irn) - 1; i >= 0; i--) {
    /* We analyze the predecessors of this block to see if this block must
     * be updated.*/
    pred = get_Block_cfgpred(irn, i);
    pred_blk = get_nodes_block(pred);
    val_arr_pred = get_irn_link(pred_blk);

    for(value_leaves = set_first(env->set_ent); value_leaves; value_leaves = set_next(env->set_ent)) {
      vnum = GET_ENT_VNUM(value_leaves->ent);

      if((val_arr_pred[vnum].access_type > 3) && (val_arr[vnum].access_type < 3)) {
        /* We have found a block for update it access and value number information.*/
            val_arr[vnum].access_type = val_arr_pred[vnum].access_type;
            val_arr[env->vnum_state].access_type = val_arr_pred[env->vnum_state].access_type;
            env->changes++;
          }
      }
  }
}

static void print_block_state(ir_node *irn, void *ctx) {

  env_t *env = ctx;
  value_arr_entry_t *val_arr;
  ent_leaves_t *value_leaves;
  call_access_t *value_calls;
  int vnum;

  val_arr = get_irn_link(irn);

  ir_printf("\n\nThe actual value number state of this block is: %i \n", val_arr[env->vnum_state].access_type - 1);

  for(value_leaves = set_first(env->set_ent); value_leaves; value_leaves = set_next(env->set_ent)) {
    vnum = GET_ENT_VNUM(value_leaves->ent);
    ir_printf("The entity %F access type in the block with nr %u is %i \n", value_leaves->ent, get_irn_node_nr(irn),
               val_arr[vnum].access_type);
    if(val_arr[vnum].calls != NULL)
      for(value_calls = set_first(val_arr[vnum].calls); value_calls; value_calls = set_next(val_arr[vnum].calls))
        ir_printf("A call with nr %i acess a element of this entity with access %u \n",
                  get_irn_node_nr(value_calls->call), value_calls->access_type);
  }

}

/** Optimize the found scalar replacements.
*
* @param sels  A pset with all sels nodes,
*              that belong to our scalars.
*/
static void do_data_flow_scalar_replacement(set *set_ent, set *set_sels, int vnum) {

  env_t env;

  obstack_init(&env.obst);
  env.set_ent     = set_ent;
  env.set_sels    = set_sels;
  env.fix_ls      = NULL;
  env.fix_phis    = NULL;
  env.fix_syncs   = NULL;
  env.gl_mem_vnum = vnum - 2;
  env.vnum_state  = vnum - 1;
  /* nvals are vnum - 1, why we indicate with nvals the number
   * of memory edges we will produce. For vnum_state we don't
   * need to produce a memory edge.*/
  env.nvals       = vnum - 1;
  env.changes     = 1;

  /* first step: allocate the value arrays for every block */
  irg_block_walk_graph(current_ir_graph, NULL, alloc_value_arr, &env);

  /* second step: we analyze all calls, that have as parameter scalar(s)
   * and we mark this calls. If the call save the address of a scalar we
   * mark the entity owner of this scalar as not optimizeble by now.*/
  irg_walk_graph(current_ir_graph, NULL, analyse_calls, &env);

  while(env.changes) {


    env.changes  = 0;
    /*
    * third step: walk over the blocks of a graph and update
    * the information for the access of our scalars.
    */
    irg_block_walk_graph(current_ir_graph, NULL, set_block_access, &env);

  }
  /* Debug info to see if analyse_calls work properly.*/
  irg_block_walk_graph(current_ir_graph, NULL, print_block_state, &env);
  /*
   * fourth step: walk over the graph blockwise in topological order
   * and split the memrory edge.
   */
  irg_walk_blkwise_graph(current_ir_graph, NULL, split_memory_edge, &env);

  /* fifth step: fix all nodes, that have as predecessor Unknown.*/
  fix_ls(&env);
  fix_phis(&env);
  fix_syncs(&env);

  sync_mem_edges(&env);
}

/*
 * Find possible scalar replacements
 *
 * @param irg  The current ir graph.
 */
void data_flow_scalar_replacement_opt(ir_graph *irg)
{
  int          i, vnum = 0;
  ir_node      *irg_frame;
  set          *set_sels;
  set          *set_ent;
  ir_graph     *rem;
  ent_leaves_t key_leaves, *value_leaves;


  if (! get_opt_scalar_replacement())
    return;

  rem = current_ir_graph;
  set_sels = new_set(sels_cmp, 8);
  set_ent  = new_set(ent_leaves_t_cmp, 8);

  /* Call algorithm that computes the out edges */
  if (get_irg_outs_state(irg) != outs_consistent)
    compute_irg_outs(irg);

  /* Find possible scalar replacements */
  if (find_possible_replacements(irg)) {

    if (get_opt_scalar_replacement_verbose()) {
      printf("Scalar Replacement: %s\n", get_entity_name(get_irg_entity(irg)));
    }

    /* Insert in set the scalar replacements. */
    irg_frame = get_irg_frame(irg);

    for (i = 0 ; i < get_irn_n_outs(irg_frame); i++) {
      ir_node *succ = get_irn_out(irg_frame, i);

      if (get_irn_op(succ) == op_Sel) {
        entity *ent = get_Sel_entity(succ);

        if (get_entity_link(ent) == NULL || get_entity_link(ent) == ADDRESS_TAKEN)
          continue;
        /* we have found a entity, that have scalars and we insert it to our set_ent*/
        key_leaves.ent = ent;
        key_leaves.leaves = new_pset(leave_cmp, 8);
        value_leaves = set_insert(set_ent, &key_leaves, sizeof(key_leaves), HASH_PTR(ent));

        /* We allocate for every leave sel a vnum.*/
        vnum = allocate_value_numbers(set_sels, value_leaves->leaves, ent, vnum);
      }
    }
    printf("vnumber in data flow= %i\n", vnum);

    /* Allocate value number for the globule memory edge.
     * and a value number for the value numbers state.*/
    vnum = vnum + 2;

    /* Allocate value numbers for the entities .*/
    for(i = vnum,value_leaves = set_first(set_ent); value_leaves; i++, value_leaves = set_next(set_ent))
      SET_ENT_VNUM(value_leaves->ent, i);

    if (vnum) {
      do_data_flow_scalar_replacement(set_ent, set_sels, vnum);
    }
  }
}
