/*
 * Project:     libFIRM
 * File name:   ir/opt/ldstopt.c
 * Purpose:     load store optimizations
 * Author:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 UniversitÅ‰t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif

# include "irnode_t.h"
# include "irgraph_t.h"
# include "irmode_t.h"
# include "iropt_t.h"
# include "ircons_t.h"
# include "irgmod.h"
# include "irgwalk.h"
# include "irvrfy.h"
# include "tv_t.h"
# include "dbginfo_t.h"
# include "iropt_dbg.h"
# include "irflag_t.h"
# include "array.h"
# include "firmstat.h"

#undef IMAX
#define IMAX(a,b)	((a) > (b) ? (a) : (b))

#define MAX_PROJ	IMAX(pn_Load_max, pn_Store_max)

/**
 * walker environment
 */
typedef struct _walk_env_t {
  struct obstack obst;		/**< list of all stores */
  int changes;
} walk_env_t;

/**
 * flags for Load/Store
 */
enum ldst_flags_t {
  LDST_VISITED = 1              /**< if set, this Load/Store is already visited */
};

/**
 * a Load/Store info
 */
typedef struct _ldst_info_t {
  ir_node  *projs[MAX_PROJ];    /**< list of Proj's of this node */
  ir_node  *exc_block;          /**< the exception block if available */
  int      exc_idx;             /**< predecessor index in the exception block */
  unsigned flags;               /**< flags */
} ldst_info_t;

/**
 * flags for control flow
 */
enum block_flags_t {
  BLOCK_HAS_COND = 1,      /**< Block has conditional control flow */
  BLOCK_HAS_EXC  = 2       /**< Block has exceptionl control flow */
};

/**
 * a Block info
 */
typedef struct _block_info_t {
  unsigned flags;               /**< flags for the block */
} block_info_t;

/**
 * walker, clears all links first
 */
static void init_links(ir_node *n, void *env)
{
  set_irn_link(n, NULL);
}

/**
 * get the Load/Store info of a node
 */
static ldst_info_t *get_ldst_info(ir_node *node, walk_env_t *env)
{
  ldst_info_t *info = get_irn_link(node);

  if (! info) {
    info = obstack_alloc(&env->obst, sizeof(*info));

    memset(info, 0, sizeof(*info));

    set_irn_link(node, info);
  }
  return info;
}

/**
 * get the Block info of a node
 */
static block_info_t *get_block_info(ir_node *node, walk_env_t *env)
{
  block_info_t *info = get_irn_link(node);

  if (! info) {
    info = obstack_alloc(&env->obst, sizeof(*info));

    memset(info, 0, sizeof(*info));

    set_irn_link(node, info);
  }
  return info;
}

/**
 * update the projection info for a Load/Store
 */
static int update_projs(ldst_info_t *info, ir_node *proj)
{
  long nr = get_Proj_proj(proj);

  assert(0 <= nr && nr <= MAX_PROJ && "Wrong proj from LoadStore");

  if (info->projs[nr]) {
    /* there is already one, do CSE */
    exchange(proj, info->projs[nr]);
    return 1;
  }
  else {
    info->projs[nr] = proj;
    return 0;
  }
}

/**
 * update the exception block info for a Load/Store
 */
static int update_exc(ldst_info_t *info, ir_node *block, int pos)
{
  assert(info->exc_block == NULL && "more than one exception block found");

  info->exc_block = block;
  info->exc_idx   = pos;
  return 0;
}

#define get_irn_out_n(node)     (unsigned)get_irn_link(node)
#define set_irn_out_n(node, n)  set_irn_link(adr, (void *)(n))

/**
 * walker, collects all Load/Store/Proj nodes
 *
 * walks form Start -> End
 */
static void collect_nodes(ir_node *node, void *env)
{
  ir_op       *op = get_irn_op(node);
  ir_node     *pred;
  ldst_info_t *ldst_info;
  walk_env_t  *wenv = env;

  if (op == op_Proj) {
    ir_node *adr;
    ir_op *op;

    pred = get_Proj_pred(node);
    op   = get_irn_op(pred);

    if (op == op_Load) {
      ldst_info = get_ldst_info(pred, wenv);

      wenv->changes |= update_projs(ldst_info, node);

      if ((ldst_info->flags & LDST_VISITED) == 0) {
        adr = get_Load_ptr(pred);
        set_irn_out_n(adr, get_irn_out_n(adr) + 1);

        ldst_info->flags |= LDST_VISITED;
      }
    }
    else if (op == op_Store) {
      ldst_info = get_ldst_info(pred, wenv);

      wenv->changes |= update_projs(ldst_info, node);

      if ((ldst_info->flags & LDST_VISITED) == 0) {
        adr = get_Store_ptr(pred);
        set_irn_out_n(adr, get_irn_out_n(adr) + 1);

        ldst_info->flags |= LDST_VISITED;
      }
    }
  }
  else if (op == op_Block) { /* check, if it's an exception block */
    int i, n;

    for (i = 0, n = get_Block_n_cfgpreds(node); i < n; ++i) {
      ir_node      *pred_block;
      block_info_t *bl_info;

      pred = skip_Proj(get_Block_cfgpred(node, i));

      /* ignore Bad predecessors, they will be removed later */
      if (is_Bad(pred))
	continue;

      pred_block = get_nodes_block(pred);
      bl_info    = get_block_info(pred_block, wenv);

      if (is_fragile_op(pred))
	bl_info->flags |= BLOCK_HAS_EXC;
      else if (is_forking_op(pred))
	bl_info->flags |= BLOCK_HAS_COND;

      if (get_irn_op(pred) == op_Load || get_irn_op(pred) == op_Store) {
        ldst_info = get_ldst_info(pred, wenv);

        wenv->changes |= update_exc(ldst_info, node, i);
      }
    }
  }
}

/**
 * returns a entity if the address ptr points to a constant one.
 */
static entity *find_constant_entity(ir_node *ptr)
{
  for (;;) {
    ir_op *op = get_irn_op(ptr);

    if (op == op_SymConst && (get_SymConst_kind(ptr) == symconst_addr_ent)) {
      return get_SymConst_entity(ptr);
    }
    else if (op == op_Sel) {
      entity *ent = get_Sel_entity(ptr);
      type *tp    = get_entity_owner(ent);

      if (is_Array_type(tp)) {
        /* check bounds */
        int i, n;

        for (i = 0, n = get_Sel_n_indexs(ptr); i < n; ++i) {
          ir_node *bound;
          tarval *tlower, *tupper;
          ir_node *index = get_Sel_index(ptr, i);
          tarval *tv     = computed_value(index);

          /* check if the index is constant */
          if (tv == tarval_bad)
            return NULL;

          bound  = get_array_lower_bound(tp, i);
          tlower = computed_value(bound);
          bound  = get_array_upper_bound(tp, i);
          tupper = computed_value(bound);

          if (tlower == tarval_bad || tupper == tarval_bad)
            return NULL;

          if (tarval_cmp(tv, tlower) & Lt)
            return NULL;
          if (tarval_cmp(tupper, tv) & Lt)
            return NULL;

          /* ok, bounds check finished */
        }
      }

      /* try next */
      ptr = get_Sel_ptr(ptr);
    }
    else
      return NULL;
  }
}

/**
 * optimize a Load
 */
static int optimize_load(ir_node *load)
{
  ldst_info_t *info = get_irn_link(load);
  ir_mode *load_mode = get_Load_mode(load);
  ir_node *pred, *mem, *ptr;
  entity *ent;
  int res = 0;

  /* the address of the load to be optimized */
  ptr = get_Load_ptr(load);

  /*
   * Check if we can remove the exception form a Load:
   * this can be done, if the address is from an Sel(Alloc) and
   * the Sel type is a subtype of the alloc type.
   *
   * This optimizes some often used OO constructs,
   * like x = new O; x->t;
   */
  if (info->projs[pn_Load_X_except]) {
    if (get_irn_op(ptr) == op_Sel) {
      ir_node *mem = get_Sel_mem(ptr);

      if (get_irn_op(mem) == op_Alloc) {
        /* ok, check the types */
        entity *ent  = get_Sel_entity(ptr);
        type *s_type = get_entity_type(ent);
        type *a_type = get_Alloc_type(mem);

        if (is_subclass_of(s_type, a_type)) {
          /* ok, condition met: there can't be an exception because
           * alloc guarantees that enough memory was allocated */

          exchange(info->projs[pn_Load_X_except], new_Bad());
          info->projs[pn_Load_X_except] = NULL;
        }
      }
    }
    else if (get_irn_op(ptr) == op_Alloc) {
      /* simple case: a direct load after an Alloc. Firm Alloc throw
       * an exception in case of out-of-memory. So, there is no way for an
       * exception in this load.
       * This code is constructed by the "exception lowering" in the Jack compiler.
       */
       exchange(info->projs[pn_Load_X_except], new_Bad());
       info->projs[pn_Load_X_except] = NULL;
    }
  }

  /* do NOT touch volatile loads for now */
  if (get_Load_volatility(load) == volatility_is_volatile)
    return 0;

  if (! info->projs[pn_Load_res] && ! info->projs[pn_Load_X_except]) {
    /* a Load which value is neither used nor exception checked, remove it */
    mem  = get_Load_mem(load);
    exchange(info->projs[pn_Load_M], mem);

    return 1;
  }

  /* the mem of the Load. Must still be returned after optimization */
  mem  = get_Load_mem(load);

  /* check if we can determine the entity that will be loaded */
  ent = find_constant_entity(ptr);
  if (ent) {
    if ((allocation_static == get_entity_allocation(ent)) &&
        (visibility_external_allocated != get_entity_visibility(ent))) {
      /* a static allocation that is not external: there should be NO exception
       * when loading. */

      /* no exception, clear the info field as it might be checked later again */
      if (info->projs[pn_Load_X_except]) {
        exchange(info->projs[pn_Load_X_except], new_Bad());
        info->projs[pn_Load_X_except] = NULL;
      }

      if (variability_constant == get_entity_variability(ent)
	  && is_atomic_entity(ent)) {   /* Might not be atomic after
					 lowering of Sels.  In this
					 case we could also load, but
					 it's more complicated. */
        /* more simpler case: we load the content of a constant value:
         * replace it by the constant itself
         */

        /* no memory */
        if (info->projs[pn_Load_M])
          exchange(info->projs[pn_Load_M], mem);

        /* no result :-) */
        if (info->projs[pn_Load_res]) {
          if (is_atomic_entity(ent)) {
            ir_node *c = copy_const_value(get_atomic_ent_value(ent));

            DBG_OPT_RC(load, c);
            exchange(info->projs[pn_Load_res], c);

            return 1;
          }
        }
      }
      else if (variability_constant == get_entity_variability(ent)) {
        printf(">>>>>>>>>>>>> Found access to constant entity %s in function %s\n", get_entity_name(ent),
            get_entity_name(get_irg_entity(current_ir_graph)));
      }

      /* we changed the irg, but try further */
      res = 1;
    }
  }

  /* Check, if the address of this load is used more than once.
   * If not, this load cannot be removed in any case. */
  if (get_irn_out_n(ptr) <= 1)
    return 0;

  /* follow the memory chain as long as there are only Loads */
  for (pred = skip_Proj(mem); ; pred = skip_Proj(get_Load_mem(pred))) {

    /*
     * BEWARE: one might think that checking the modes is useless, because
     * if the pointers are identical, they refer to the same object.
     * This is only true in strong typed languages, not is C were the following
     * is possible a = *(type1 *)p; b = *(type2 *)p ...
     */

    if (get_irn_op(pred) == op_Store && get_Store_ptr(pred) == ptr &&
        get_irn_mode(get_Store_value(pred)) == load_mode) {
      ldst_info_t *pred_info = get_irn_link(pred);

      /*
       * a Load immediately after a Store -- a read after write.
       * We may remove the Load, if both Load & Store does not have an exception handler
       * OR they are in the same block. In the latter case the Load cannot
       * throw an exception when the previous Store was quiet.
       *
       * Why we need to check for Store Exc? If the Store cannot be executed (ROM)
       * the exception handler might simply jump into the load block :-(
       * We could make it a little bit better if we would know that the exception
       * handler of the Store jumps directly to the end...
       */
      if ((!pred_info->projs[pn_Store_X_except] && !info->projs[pn_Load_X_except]) ||
          get_nodes_block(load) == get_nodes_block(pred)) {
        DBG_OPT_RAW(load, pred);
        exchange( info->projs[pn_Load_res], get_Store_value(pred) );

        if (info->projs[pn_Load_M])
          exchange(info->projs[pn_Load_M], mem);

        /* no exception */
        if (info->projs[pn_Load_X_except])
          exchange( info->projs[pn_Load_X_except], new_Bad());
        return 1;
      }
    }
    else if (get_irn_op(pred) == op_Load && get_Load_ptr(pred) == ptr &&
             get_Load_mode(pred) == load_mode) {
      /*
       * a Load after a Load -- a read after read.
       * We may remove the second Load, if it does not have an exception handler
       * OR they are in the same block. In the later case the Load cannot
       * throw an exception when the previous Load was quiet.
       *
       * Here, there is no need to check if the previos Load has an exception hander because
       * they would have exact the same exception...
       */
      if (! info->projs[pn_Load_X_except] || get_nodes_block(load) == get_nodes_block(pred)) {
        ldst_info_t *pred_info = get_irn_link(pred);

        DBG_OPT_RAR(load, pred);

        if (pred_info->projs[pn_Load_res]) {
          /* we need a data proj from the previous load for this optimization */
          exchange( info->projs[pn_Load_res], pred_info->projs[pn_Load_res] );
          if (info->projs[pn_Load_M])
            exchange(info->projs[pn_Load_M], mem);
        }
        else {
          if (info->projs[pn_Load_res]) {
            set_Proj_pred(info->projs[pn_Load_res], pred);
            set_nodes_block(info->projs[pn_Load_res], get_nodes_block(pred));
          }
          if (info->projs[pn_Load_M]) {
            /* Actually, this if should not be necessary.  Construct the Loads
               properly!!! */
            exchange(info->projs[pn_Load_M], mem);
          }
        }

        /* no exception */
        if (info->projs[pn_Load_X_except])
          exchange(info->projs[pn_Load_X_except], new_Bad());

        return 1;
      }
    }

    /* follow only Load chains */
    if (get_irn_op(pred) != op_Load)
      break;
   }
  return res;
}

/**
 * optimize a Store
 */
static int optimize_store(ir_node *store)
{
  ldst_info_t *info = get_irn_link(store);
  ir_node *pred, *mem, *ptr, *value, *block;
  ir_mode *mode;
  int res = 0;

  if (get_Store_volatility(store) == volatility_is_volatile)
    return 0;

  /*
   * BEWARE: one might think that checking the modes is useless, because
   * if the pointers are identical, they refer to the same object.
   * This is only true in strong typed languages, not is C were the following
   * is possible *(type1 *)p = a; *(type2 *)p = b ...
   */

  ptr   = get_Store_ptr(store);

  /* Check, if the address of this load is used more than once.
   * If not, this load cannot be removed in any case. */
  if (get_irn_out_n(ptr) <= 1)
    return 0;

  block = get_nodes_block(store);
  mem   = get_Store_mem(store);
  value = get_Store_value(store);
  mode  = get_irn_mode(value);

  /* follow the memory chain as long as there are only Loads */
  for (pred = skip_Proj(mem); ; pred = skip_Proj(get_Load_mem(pred))) {
    ldst_info_t *pred_info = get_irn_link(pred);

    if (get_irn_op(pred) == op_Store && get_Store_ptr(pred) == ptr &&
        get_nodes_block(pred) == block && get_irn_mode(get_Store_value(pred)) == mode) {
      /*
       * a Store after a Store in the same block -- a write after write.
       * We may remove the first Store, if it does not have an exception handler.
       *
       * TODO: What, if both have the same exception handler ???
       */
      if (get_Store_volatility(pred) != volatility_is_volatile && !pred_info->projs[pn_Store_X_except]) {
        DBG_OPT_WAW(pred, store);
        exchange( pred_info->projs[pn_Store_M], get_Store_mem(pred) );
        return 1;
      }
    }
    else if (get_irn_op(pred) == op_Load && get_Load_ptr(pred) == ptr &&
             value == pred_info->projs[pn_Load_res]) {
      /*
       * a Store of a value after a Load -- a write after read.
       * We may remove the second Store, if it does not have an exception handler.
       */
      if (! info->projs[pn_Store_X_except]) {
        DBG_OPT_WAR(store, pred);
        exchange( info->projs[pn_Store_M], mem );
        return 1;
      }
    }

    /* follow only Load chains */
    if (get_irn_op(pred) != op_Load)
      break;
  }
  return res;
}

/**
 * walker, optimizes Phi after Stores:
 * Does the following optimization:
 *
 *   val1   val2   val3          val1  val2  val3
 *    |      |      |               \    |    /
 *   Str    Str    Str               \   |   /
 *      \    |    /                     Phi
 *       \   |   /                       |
 *        \  |  /                       Str
 *          Phi
 *
 * This removes the number of stores and allows for predicated execution.
 * Moves Stores back to the end of a function which may be bad
 *
 * Is only allowed if the predecessor blocks have only one successor.
 */
static int optimize_phi(ir_node *phi, void *env)
{
  walk_env_t *wenv = env;
  int i, n;
  ir_node *store, *ptr, *block, *phiM, *phiD, *exc, *projM;
  ir_mode *mode;
  ir_node **inM, **inD;
  int *idx;
  dbg_info *db = NULL;
  ldst_info_t *info;
  block_info_t *bl_info;

  /* Must be a memory Phi */
  if (get_irn_mode(phi) != mode_M)
    return 0;

  n = get_Phi_n_preds(phi);
  if (n <= 0)
    return 0;

  store = skip_Proj(get_Phi_pred(phi, 0));
  if (get_irn_op(store) != op_Store)
    return 0;

  /* abort on bad blocks */
  if (is_Bad(get_nodes_block(store)))
    return 0;

  /* check if the block has only one output */
  bl_info = get_irn_link(get_nodes_block(store));
  if (bl_info->flags)
    return 0;

  /* this is the address of the store */
  ptr  = get_Store_ptr(store);
  mode = get_irn_mode(get_Store_value(store));
  info = get_irn_link(store);
  exc  = info->exc_block;

  for (i = 1; i < n; ++i) {
    ir_node *pred = skip_Proj(get_Phi_pred(phi, i));

    if (get_irn_op(pred) != op_Store)
      return 0;

    if (mode != get_irn_mode(get_Store_value(pred)) || ptr != get_Store_ptr(pred))
      return 0;

    info = get_irn_link(pred);

    /* check, if all stores have the same exception flow */
    if (exc != info->exc_block)
      return 0;

    /* abort on bad blocks */
    if (is_Bad(get_nodes_block(store)))
      return 0;

    /* check if the block has only one output */
    bl_info = get_irn_link(get_nodes_block(store));
    if (bl_info->flags)
      return 0;
  }

  /*
   * ok, when we are here, we found all predecessors of a Phi that
   * are Stores to the same address. That means whatever we do before
   * we enter the block of the Phi, we do a Store.
   * So, we can move the store to the current block:
   *
   *   val1    val2    val3          val1  val2  val3
   *    |       |       |               \    |    /
   * | Str | | Str | | Str |             \   |   /
   *      \     |     /                     Phi
   *       \    |    /                       |
   *        \   |   /                       Str
   *           Phi
   *
   * Is only allowed if the predecessor blocks have only one successor.
   */

  /* first step: collect all inputs */
  NEW_ARR_A(ir_node *, inM, n);
  NEW_ARR_A(ir_node *, inD, n);
  NEW_ARR_A(int, idx, n);

  for (i = 0; i < n; ++i) {
    ir_node *pred = skip_Proj(get_Phi_pred(phi, i));
    info = get_irn_link(pred);

    inM[i] = get_Store_mem(pred);
    inD[i] = get_Store_value(pred);
    idx[i] = info->exc_idx;
  }
  block = get_nodes_block(phi);

  /* second step: create a new memory Phi */
  phiM = new_rd_Phi(get_irn_dbg_info(phi), current_ir_graph, block, n, inM, mode_M);

  /* third step: create a new data Phi */
  phiD = new_rd_Phi(get_irn_dbg_info(phi), current_ir_graph, block, n, inD, mode);

  /* fourth step: create the Store */
  store = new_rd_Store(db, current_ir_graph, block, phiM, ptr, phiD);
  projM = new_rd_Proj(NULL, current_ir_graph, block, store, mode_M, pn_Store_M);

  info = get_ldst_info(store, wenv);
  info->projs[pn_Store_M] = projM;

  /* fifths step: repair exception flow */
  if (exc) {
    ir_node *projX = new_rd_Proj(NULL, current_ir_graph, block, store, mode_X, pn_Store_X_except);

    info->projs[pn_Store_X_except] = projX;
    info->exc_block                = exc;
    info->exc_idx                  = idx[0];

    for (i = 0; i < n; ++i) {
      set_Block_cfgpred(exc, idx[i], projX);
    }

    if (n > 1) {
      /* the exception block should be optimized as some inputs are identical now */
    }
  }

  /* sixt step: replace old Phi */
  exchange(phi, projM);

  return 1;
}

/**
 * walker, collects all Load/Store/Proj nodes
 */
static void do_load_store_optimize(ir_node *n, void *env)
{
  walk_env_t *wenv = env;

  switch (get_irn_opcode(n)) {

  case iro_Load:
    wenv->changes |= optimize_load(n);
    break;

  case iro_Store:
    wenv->changes |= optimize_store(n);
    break;

  case iro_Phi:
    wenv->changes |= optimize_phi(n, env);

  default:
    ;
  }
}

/*
 * do the load store optimization
 */
void optimize_load_store(ir_graph *irg)
{
  walk_env_t env;

  assert(get_irg_phase_state(irg) != phase_building);

  if (!get_opt_redundant_LoadStore())
    return;

  obstack_init(&env.obst);
  env.changes = 0;

  /* init the links, then collect Loads/Stores/Proj's in lists */
  irg_walk_graph(irg, init_links, collect_nodes, &env);

  /* now we have collected enough information, optimize */
  irg_walk_graph(irg, NULL, do_load_store_optimize, &env);

  obstack_free(&env.obst, NULL);

  /* Handle graph state */
  if (env.changes) {
    if (get_irg_outs_state(current_ir_graph) == outs_consistent)
      set_irg_outs_inconsistent(current_ir_graph);

    /* is this really needed: Yes, as exception block may get bad but this might be tested */
    if (get_irg_dom_state(current_ir_graph) == dom_consistent)
      set_irg_dom_inconsistent(current_ir_graph);
  }
}
