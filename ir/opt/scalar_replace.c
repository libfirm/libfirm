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
 * @brief   Scalar replacement of compounds.
 * @author  Beyhan Veliev, Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>

#include "iroptimize.h"
#include "scalar_replace.h"
#include "irflag_t.h"
#include "irouts.h"
#include "set.h"
#include "pset.h"
#include "array.h"
#include "tv.h"
#include "ircons_t.h"
#include "hashptr.h"
#include "irgwalk.h"
#include "irgmod.h"
#include "irnode_t.h"
#include "irtools.h"
#include "xmalloc.h"

#define SET_VNUM(node, vnum) set_irn_link(node, INT_TO_PTR(vnum))
#define GET_VNUM(node)       (unsigned)PTR_TO_INT(get_irn_link(node))

/**
 * A path element entry: it is either an entity
 * or a tarval, because we evaluate only constant array
 * accesses like a.b.c[8].d
 */
typedef union {
  ir_entity *ent;
  tarval *tv;
} path_elem_t;

/**
 * An access path, used to assign value numbers
 * to variables that will be scalar replaced.
 */
typedef struct _path_t {
  unsigned    vnum;      /**< The value number. */
  unsigned    path_len;  /**< The length of the access path. */
  path_elem_t path[1];   /**< The path. */
} path_t;

/** The size of a path in bytes. */
#define PATH_SIZE(p)  (sizeof(*(p)) + sizeof((p)->path[0]) * ((p)->path_len - 1))

typedef struct _scalars_t {
  ir_entity *ent;              /**< A entity for scalar replacement. */
  ir_type *ent_owner;          /**< The owner of this entity. */
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
  (void) size;

  /* we can use memcmp here, because identical tarvals should have identical addresses */
  return memcmp(p1->path, p2->path, p1->path_len * sizeof(p1->path[0]));
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
  (void) size;

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
    hash ^= (unsigned)PTR_TO_INT(path->path[i].ent);

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
 * Check the mode of a Load/Store with the mode of the entity
 * that is accessed.
 * If the mode of the entity and the Load/Store mode do not match, we
 * have the bad reinterpret case:
 *
 * int i;
 * char b = *(char *)&i;
 *
 * We do NOT count this as one value and return address_taken
 * in that case.
 * However, we support an often used case. If the mode is two-complement
 * we allow casts between signed/unsigned.
 *
 * @param mode     the mode of the Load/Store
 * @param ent_mode the mode of the accessed entity
 */
static int check_load_store_mode(ir_mode *mode, ir_mode *ent_mode) {
  if (ent_mode != mode) {
    if (ent_mode == NULL ||
        get_mode_size_bits(ent_mode) != get_mode_size_bits(mode) ||
	get_mode_sort(ent_mode) != get_mode_sort(mode) ||
        get_mode_arithmetic(ent_mode) != irma_twos_complement ||
        get_mode_arithmetic(mode) != irma_twos_complement)
      return 0;
  }
  return 1;
}

/*
 * Returns non-zero, if the address of an entity
 * represented by a Sel node (or it's successor Sels) is taken.
 */
int is_address_taken(ir_node *sel)
{
  int     i;
  ir_mode *emode, *mode;
  ir_node *value;
  ir_entity *ent;

  if (! is_const_sel(sel))
    return 1;

  for (i = get_irn_n_outs(sel) - 1; i >= 0; --i) {
    ir_node *succ = get_irn_out(sel, i);

    switch (get_irn_opcode(succ)) {
    case iro_Load:
      /* check if this load is not a hidden conversion */
      mode = get_Load_mode(succ);
      ent = get_Sel_entity(sel);
      emode = get_type_mode(get_entity_type(ent));
      if (! check_load_store_mode(mode, emode))
        return 1;
      break;

    case iro_Store:
      /* check that Sel is not the Store's value */
      value = get_Store_value(succ);
      if (value == sel)
        return 1;
      /* check if this Store is not a hidden conversion */
      mode = get_irn_mode(value);
      ent = get_Sel_entity(sel);
      emode = get_type_mode(get_entity_type(ent));
      if (! check_load_store_mode(mode, emode))
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
       * As long as we do not have analyses that can tell what
       * is done with parameters, think is taken.
       */
      return 1;

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
static void link_all_leave_sels(ir_entity *ent, ir_node *sel)
{
  int i, n, flag = 1;

  n = get_irn_n_outs(sel);
  for (i = 0; i < n; ++i) {
    ir_node *succ = get_irn_out(sel, i);

    if (is_Sel(succ)) {
      link_all_leave_sels(ent, succ);
      flag = 0;
    }
  }

  if (flag) {
    /* if Sel nodes with memory inputs are used, a entity can be
     * visited more than once causing a ring here, so we use the
     * node flag to mark linked nodes
     */
    if (irn_visited(sel))
      return;

    /* we know we are at a leave, because this function is only
     * called if the address is NOT taken, so succ must be a Load
     * or a Store node
     */
    set_irn_link(sel, get_entity_link(ent));
    set_entity_link(ent, sel);

    mark_irn_visited(sel);
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
 * Sel nodes, that selects the atomic fields of this entity.
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
   * First, clear the link field of all interesting entities.
   * Note that we did not rely on the fact that there is only
   * one Sel node per entity, so we might access one entity
   * more than once here.
   * That's why we have need two loops.
   */
  for (i = 0; i < n; ++i) {
    ir_node *succ = get_irn_out(irg_frame, i);

    if (is_Sel(succ)) {
      ir_entity *ent = get_Sel_entity(succ);
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

    if (is_Sel(succ)) {
      ir_entity *ent = get_Sel_entity(succ);
      ir_type *ent_type;

      if (get_entity_link(ent) == ADDRESS_TAKEN)
        continue;

      /*
       * Beware: in rare cases even entities on the frame might be
       * volatile. This might happen if the entity serves as a store
       * to a value that must survive a exception. Do not optimize
       * such entities away.
       */
      if (get_entity_volatility(ent) == volatility_is_volatile) {
        set_entity_link(ent, ADDRESS_TAKEN);
        continue;
      }

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

  if (! is_Sel(pred)) {
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
static unsigned allocate_value_numbers(pset *sels, ir_entity *ent, unsigned vnum, ir_mode ***modes)
{
  ir_node *sel, *next;
  path_t *key, *path;
  set *pathes = new_set(path_cmp, 8);

  /* visit all Sel nodes in the chain of the entity */
  for (sel = get_entity_link(ent); sel; sel = next) {
    next = get_irn_link(sel);

    /* we must mark this sel for later */
    pset_insert_ptr(sels, sel);

    key  = find_path(sel, 0);
    path = set_find(pathes, key, PATH_SIZE(key), path_hash(key));

    if (path)
      SET_VNUM(sel, path->vnum);
    else {
      key->vnum = vnum++;

      set_insert(pathes, key, PATH_SIZE(key), path_hash(key));

      SET_VNUM(sel, key->vnum);
      ARR_EXTO(ir_mode *, *modes, (int)((key->vnum + 15) & ~15));

      (*modes)[key->vnum] = get_type_mode(get_entity_type(get_Sel_entity(sel)));

      assert((*modes)[key->vnum] && "Value is not atomic");

#ifdef DEBUG_libfirm
      /* Debug output */
      if (get_opt_scalar_replacement_verbose() && get_firm_verbosity() > 1) {
		unsigned i;
        printf("  %s", get_entity_name(key->path[0].ent));
        for (i = 1; i < key->path_len; ++i) {
          if (is_entity(key->path[i].ent))
            printf(".%s", get_entity_name(key->path[i].ent));
          else
            printf("[%ld]", get_tarval_long(key->path[i].tv));
        }
        printf(" = %u (%s)\n", PTR_TO_INT(get_irn_link(sel)), get_mode_name((*modes)[key->vnum]));
      }
#endif /* DEBUG_libfirm */
    }
    free(key);
  }

  del_set(pathes);
  set_entity_link(ent, NULL);
  return vnum;
}

/**
 * A list entry for the fixing lists
 */
typedef struct _list_entry_t {
  ir_node  *node;   /**< the node that must be fixed */
  unsigned vnum;    /**< the value number of this node */
} list_entry_t;

/**
 * environment for memory walker
 */
typedef struct _env_t {
  struct obstack obst;      /**< a obstack for the value blocks */
  int          nvals;       /**< number of values */
  ir_mode      **modes;     /**< the modes of the values */
  list_entry_t *fix_phis;   /**< list of all Phi nodes that must be fixed */
  list_entry_t *fix_loads;  /**< list of all Load nodes that must be fixed */
  pset         *sels;       /**< A set of all Sel nodes that have a value number */
} env_t;

/**
 * topological walker.
 */
static void topologic_walker(ir_node *node, void *ctx)
{
  env_t        *env = ctx;
  ir_op        *op = get_irn_op(node);
  ir_node      *adr, *block, *mem, *unk, **value_arr, **in, *val;
  ir_mode      *mode;
  unsigned     vnum;
  int          i, j, n;
  list_entry_t *l;

  if (op == op_Load) {
    /* a load, check if we can resolve it */
    adr = get_Load_ptr(node);

    if (! is_Sel(adr))
      return;

    if (! pset_find_ptr(env->sels, adr))
      return;

    /* ok, we have a Load that will be replaced */
    vnum = GET_VNUM(adr);

    assert(vnum < (unsigned)env->nvals);

    block     = get_nodes_block(node);
    value_arr = get_irn_link(block);

    /* check, if we can replace this Load */
    if (value_arr[vnum]) {
      mem = get_Load_mem(node);

      /* Beware: A Load can contain a hidden conversion in Firm.
         This happens for instance in the following code:

         int i;
         unsigned j = *(unsigned *)&i;

         Handle this here. */
      val = value_arr[vnum];
      mode = get_Load_mode(node);
      if (mode != get_irn_mode(val))
        val = new_d_Conv(get_irn_dbg_info(node), val, mode);

      turn_into_tuple(node, pn_Load_max);
      set_Tuple_pred(node, pn_Load_M,         mem);
      set_Tuple_pred(node, pn_Load_res,       val);
      set_Tuple_pred(node, pn_Load_X_regular, new_r_Jmp(current_ir_graph, block));
      set_Tuple_pred(node, pn_Load_X_except,  new_Bad());
    } else {
      l = obstack_alloc(&env->obst, sizeof(*l));
      l->node = node;
      l->vnum = vnum;

      set_irn_link(node, env->fix_loads);
      env->fix_loads = l;
    }
  } else if (op == op_Store) {
    /* a Store always can be replaced */
    adr = get_Store_ptr(node);

    if (! is_Sel(adr))
      return;

    if (! pset_find_ptr(env->sels, adr))
      return;

    vnum = GET_VNUM(adr);

    assert(vnum < (unsigned)env->nvals);

    block     = get_nodes_block(node);
    value_arr = get_irn_link(block);

    /* Beware: A Store can contain a hidden conversion in Firm. */
    val = get_Store_value(node);
    if (get_irn_mode(val) != env->modes[vnum])
      val = new_d_Conv(get_irn_dbg_info(node), val, env->modes[vnum]);
    value_arr[vnum] = val;

    mem = get_Store_mem(node);
	block = get_nodes_block(node);

    turn_into_tuple(node, pn_Store_max);
    set_Tuple_pred(node, pn_Store_M,         mem);
    set_Tuple_pred(node, pn_Store_X_regular, new_r_Jmp(current_ir_graph, block));
    set_Tuple_pred(node, pn_Store_X_except,  new_Bad());
  } else if (op == op_Phi && get_irn_mode(node) == mode_M) {
    /*
     * found a memory Phi: Here, we must create new Phi nodes
     */
    block     = get_nodes_block(node);
    value_arr = get_irn_link(block);

    n = get_Block_n_cfgpreds(block);

    in = alloca(sizeof(*in) * n);

    for (i = env->nvals - 1; i >= 0; --i) {
      unk = new_Unknown(env->modes[i]);
      for (j = n - 1; j >= 0; --j)
        in[j] = unk;

      value_arr[i] = new_r_Phi(current_ir_graph, block, n, in, env->modes[i]);

      l = obstack_alloc(&env->obst, sizeof(*l));
      l->node = value_arr[i];
      l->vnum = i;

      set_irn_link(value_arr[i], env->fix_phis);
      env->fix_phis = l;
    }
  }
}

/**
 * Walker: allocate the value array for every block.
 */
static void alloc_value_arr(ir_node *block, void *ctx)
{
  env_t *env = ctx;
  ir_node **var_arr = obstack_alloc(&env->obst, sizeof(*var_arr) * env->nvals);

  /* the value array is empty at start */
  memset(var_arr, 0, sizeof(*var_arr) * env->nvals);
  set_irn_link(block, var_arr);
}

/**
 * searches through blocks beginning from block for value
 * vnum and return it.
 */
static ir_node *find_vnum_value(ir_node *block, unsigned vnum)
{
  ir_node **value_arr;
  int i;
  ir_node *res;

  if (Block_not_block_visited(block)) {
    mark_Block_block_visited(block);

    value_arr = get_irn_link(block);

    if (value_arr[vnum])
      return value_arr[vnum];

    for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
      ir_node *pred = get_Block_cfgpred(block, i);

      res = find_vnum_value(get_nodes_block(pred), vnum);
      if (res)
        return res;
    }
  }
  return NULL;
}

/**
 * fix the Phi list
 */
static void fix_phis(env_t *env)
{
  list_entry_t *l;
  ir_node      *phi, *block, *pred, *val;
  int          i;

  for (l = env->fix_phis; l; l = get_irn_link(phi)) {
    phi = l->node;

    block = get_nodes_block(phi);
    for (i = get_irn_arity(phi) - 1; i >= 0; --i) {
      pred = get_Block_cfgpred(block, i);
      pred = get_nodes_block(pred);

      inc_irg_block_visited(current_ir_graph);
      val = find_vnum_value(pred, l->vnum);

      if (val)
        set_irn_n(phi, i, val);
    }
  }
}

/**
 * fix the Load list
 */
static void fix_loads(env_t *env)
{
  list_entry_t *l;
  ir_node      *load, *block, *pred, *val = NULL, *mem;
  ir_mode      *mode;
  int          i;

  for (l = env->fix_loads; l; l = get_irn_link(load)) {
    load = l->node;

    block = get_nodes_block(load);
    for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
      pred = get_Block_cfgpred(block, i);
      pred = get_nodes_block(pred);

      inc_irg_block_visited(current_ir_graph);
      val = find_vnum_value(pred, l->vnum);

      if (val)
        break;
    }

    if (! val) {
      /* access of an uninitialized value */
      val = new_Unknown(env->modes[l->vnum]);
    }

    /* Beware: A Load can contain a hidden conversion in Firm.
       Handle this here. */
    mode = get_Load_mode(load);
    if (mode != get_irn_mode(val))
      val = new_d_Conv(get_irn_dbg_info(load), val, mode);

	    mem = get_Load_mem(load);

    turn_into_tuple(load, pn_Load_max);
    set_Tuple_pred(load, pn_Load_M,         mem);
    set_Tuple_pred(load, pn_Load_res,       val);
    set_Tuple_pred(load, pn_Load_X_regular, new_r_Jmp(current_ir_graph, block));
    set_Tuple_pred(load, pn_Load_X_except,  new_Bad());
  }
}

/**
 *  Make scalar replacement.
 *
 * @param sels    A set containing all Sel nodes that have a value number
 * @param nvals   The number of scalars.
 * @param modes   A flexible array, containing all the modes of
 *                the value numbers.
 */
static void do_scalar_replacements(pset *sels, int nvals, ir_mode **modes)
{
  env_t env;

  obstack_init(&env.obst);
  env.nvals     = nvals;
  env.modes     = modes;
  env.fix_phis  = NULL;
  env.fix_loads = NULL;
  env.sels      = sels;

  /* first step: allocate the value arrays for every block */
  irg_block_walk_graph(current_ir_graph, NULL, alloc_value_arr, &env);

  /*
   * second step: walk over the graph blockwise in topological order
   * and fill the array as much as possible.
   */
  irg_walk_blkwise_graph(current_ir_graph, NULL, topologic_walker, &env);

  /* third, fix the list of Phis, then the list of Loads */
  fix_phis(&env);
  fix_loads(&env);

  obstack_free(&env.obst, NULL);
}

/*
 * Find possible scalar replacements
 *
 * @param irg  The current ir graph.
 */
void scalar_replacement_opt(ir_graph *irg)
{
  unsigned  nvals;
  int       i;
  scalars_t key, *value;
  ir_node   *irg_frame;
  ir_mode   **modes;
  set       *set_ent;
  pset      *sels;
  ir_type   *ent_type;
  ir_graph  *rem;

  if (! get_opt_scalar_replacement())
    return;

  rem = current_ir_graph;

  /* Call algorithm that computes the out edges */
  assure_irg_outs(irg);

  /* Find possible scalar replacements */
  if (find_possible_replacements(irg)) {

    if (get_opt_scalar_replacement_verbose()) {
      printf("Scalar Replacement: %s\n", get_entity_name(get_irg_entity(irg)));
    }

    /* Insert in set the scalar replacements. */
    irg_frame = get_irg_frame(irg);
    nvals = 0;
    modes = NEW_ARR_F(ir_mode *, 16);
    set_ent = new_set(ent_cmp, 8);
    sels    = pset_new_ptr(8);

    for (i = 0 ; i < get_irn_n_outs(irg_frame); i++) {
      ir_node *succ = get_irn_out(irg_frame, i);

      if (is_Sel(succ)) {
        ir_entity *ent = get_Sel_entity(succ);

        if (get_entity_link(ent) == NULL || get_entity_link(ent) == ADDRESS_TAKEN)
          continue;

        ent_type = get_entity_type(ent);

        key.ent       = ent;
        key.ent_owner = get_entity_owner(ent);
        set_insert(set_ent, &key, sizeof(key), HASH_PTR(key.ent));

        if (get_opt_scalar_replacement_verbose()) {
          if (is_Array_type(ent_type)) {
            printf("  found array %s\n", get_entity_name(ent));
          }
          else if (is_Struct_type(ent_type)) {
            printf("  found struct %s\n", get_entity_name(ent));
          }
          else if (is_atomic_type(ent_type))
            printf("  found atomic value %s\n", get_entity_name(ent));
          else {
            assert(0 && "Neither an array nor a struct or atomic value");
          }
        }

        nvals = allocate_value_numbers(sels, ent, nvals, &modes);
      }
    }

    if (get_opt_scalar_replacement_verbose()) {
      printf("  %u values will be needed\n", nvals);
    }

    /* If scalars were found. */
    if (nvals) {
      do_scalar_replacements(sels, nvals, modes);

      for (value = set_first(set_ent); value; value = set_next(set_ent)) {
        remove_class_member(value->ent_owner, value->ent);
      }
    }

    del_pset(sels);
    del_set(set_ent);
    DEL_ARR_F(modes);

    if (nvals) {
      /*
       * We changed the graph, but did NOT introduce new blocks
       * neither changed control flow, cf-backedges should be still
       * consistent.
       */
      set_irg_outs_inconsistent(irg);
      set_irg_loopinfo_inconsistent(irg);
    }
  }

  current_ir_graph = rem;
}
