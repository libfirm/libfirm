/*
 * Project:     libFIRM
 * File name:   ir/opt/ldstopt.c
 * Purpose:     load store optimizations
 * Author:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
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
 * a Load/Store info
 */
typedef struct _ldst_info_t {
  ir_node *projs[MAX_PROJ];	/**< list of Proj's of this node */
} ldst_info_t;

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
static ldst_info_t *get_info(ir_node *node, walk_env_t *env)
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
 * update the info for a Load/Store
 */
static void update_projs(ldst_info_t *info, ir_node *proj)
{
  long nr = get_Proj_proj(proj);

  assert(0 <= nr && nr <= MAX_PROJ && "Wrong proj from LoadStore");

  if (info->projs[nr]) {
    /* there is already one, do CSE */
    exchange(proj, info->projs[nr]);
  }
  else
    info->projs[nr] = proj;
}

/**
 * walker, collects all Load/Store/Proj nodes
 */
static void collect_nodes(ir_node *n, void *env)
{
  ir_node     *pred;
  ldst_info_t *info;
  walk_env_t  *wenv = env;

  if (get_irn_op(n) == op_Proj) {
    pred = get_Proj_pred(n);

    if (get_irn_op(pred) == op_Load || get_irn_op(pred) == op_Store) {
      info = get_info(pred, wenv);

      update_projs(info, n);
    }
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
  int res = 1;

  if (get_Load_volatility(load) == volatility_is_volatile)
    return 0;

  /*
   * BEWARE: one might think that checking the modes is useless, because
   * if the pointers are identical, they refer to the same object.
   * This is only true in strong typed languages, not is C were the following
   * is possible a = *(type1 *)p; b = *(type2 *)p ...
   */

  ptr  = get_Load_ptr(load);
  mem  = get_Load_mem(load);
  pred = skip_Proj(mem);

  if (! info->projs[pn_Load_res] && ! info->projs[pn_Load_X_except]) {
    /* a Load which value is neither used nor exception checked, remove it */
    exchange(info->projs[pn_Load_M], mem);
    res = 1;
  }
  else if (get_irn_op(pred) == op_Store && get_Store_ptr(pred) == ptr &&
	   get_irn_mode(get_Store_value(pred)) == load_mode) {
    /*
     * a load immediately after a store -- a read after write.
     * We may remove the Load, if it does not have an exception handler
     * OR they are in the same block. In the latter case the Load cannot
     * throw an exception when the previous Store was quiet.
     */
    if (! info->projs[pn_Load_X_except] || get_nodes_block(load) == get_nodes_block(pred)) {
      exchange( info->projs[pn_Load_res], get_Store_value(pred) );
      if (info->projs[pn_Load_M])
	exchange(info->projs[pn_Load_M], mem);

      /* no exception */
      if (info->projs[pn_Load_X_except])
	exchange( info->projs[pn_Load_X_except], new_Bad());
      res = 1;
    }
  }
  else if (get_irn_op(pred) == op_Load && get_Load_ptr(pred) == ptr &&
	   get_Load_mode(pred) == load_mode) {
    /*
     * a load immediately after a load -- a read after read.
     * We may remove the second Load, if it does not have an exception handler
     * OR they are in the same block. In the later case the Load cannot
     * throw an exception when the previous Load was quiet.
     */
    if (! info->projs[pn_Load_X_except] || get_nodes_block(load) == get_nodes_block(pred)) {
      ldst_info_t *pred_info = get_irn_link(pred);

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

      res = 1;
    }
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
  ldst_info_t *pred_info;
  int res = 0;

  if (get_Store_volatility(store) == volatility_is_volatile)
    return 0;

  /*
   * BEWARE: one might think that checking the modes is useless, because
   * if the pointers are identical, they refer to the same object.
   * This is only true in strong typed languages, not is C were the following
   * is possible *(type1 *)p = a; *(type2 *)p = b ...
   */

  block = get_nodes_block(store);
  ptr   = get_Store_ptr(store);
  mem   = get_Store_mem(store);
  value = get_Store_value(store);
  pred  = skip_Proj(mem);
  mode  = get_irn_mode(value);

  pred_info = get_irn_link(pred);

  if (get_irn_op(pred) == op_Store && get_Store_ptr(pred) == ptr &&
      get_nodes_block(pred) == block && get_irn_mode(get_Store_value(pred)) == mode) {
    /*
     * a store immediately after a store in the same block -- a write after write.
     * We may remove the first Store, if it does not have an exception handler.
     *
     * TODO: What, if both have the same exception handler ???
     */
    if (get_Store_volatility(pred) != volatility_is_volatile && !pred_info->projs[pn_Store_X_except]) {
      exchange( pred_info->projs[pn_Store_M], get_Store_mem(pred) );
      res = 1;
    }
  }
  else if (get_irn_op(pred) == op_Load && get_Load_ptr(pred) == ptr &&
	   value == pred_info->projs[pn_Load_res]) {
    /*
     * a store a value immediately after a load -- a write after read.
     * We may remove the second Store, if it does not have an exception handler.
     */
    if (! info->projs[pn_Store_X_except]) {
      exchange( info->projs[pn_Store_M], mem );
      res = 1;
    }
  }
  return res;
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
