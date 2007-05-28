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

/*
 * Project:     libFIRM
 * File name:   ir/opt/escape_ana.c
 * Purpose:     escape analysis and optimization
 * Author:      Michael Beck
 * Modified by:
 * Created:     03.11.2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2005 Universität Karlsruhe
 */

/**
 * @file escape_ana.c
 *
 * A fast and simple Escape analysis.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "irgraph_t.h"
#include "irnode_t.h"
#include "type_t.h"
#include "irgwalk.h"
#include "irouts.h"
#include "analyze_irg_args.h"
#include "irgmod.h"
#include "ircons.h"
#include "escape_ana.h"
#include "debug.h"

/**
 * walker environment
 */
typedef struct _walk_env {
  ir_node *found_allocs;            /**< list of all found non-escaped allocs */
  ir_node *dead_allocs;             /**< list of all found dead alloc */
  check_alloc_entity_func callback; /**< callback that checks a given entity for allocation */
  unsigned nr_removed;              /**< number of removed allocs (placed of frame) */
  unsigned nr_changed;              /**< number of changed allocs (allocated on stack now) */
  unsigned nr_deads;                /**< number of dead allocs */

  /* these fields are only used in the global escape analysis */
  ir_graph *irg;                    /**< the irg for this environment */
  struct _walk_env *next;           /**< for linking environments */

} walk_env_t;

/** debug handle */
DEBUG_ONLY(firm_dbg_module_t *dbgHandle;)

/**
 * checks whether a Raise leaves a method
 */
static int is_method_leaving_raise(ir_node *raise)
{
  int i;
  ir_node *proj = NULL;
  ir_node *n;

  for (i = get_irn_n_outs(raise) - 1; i >= 0; --i) {
    ir_node *succ = get_irn_out(raise, i);

    /* there should be only one ProjX node */
    if (get_Proj_proj(succ) == pn_Raise_X) {
      proj = succ;
      break;
    }
  }

  if (! proj) {
    /* Hmm: no ProjX from a Raise? This should be a verification
     * error. For now we just assert and return.
     */
    assert(! "No ProjX after Raise found");
    return 1;
  }

  if (get_irn_n_outs(proj) != 1) {
    /* Hmm: more than one user of ProjX: This is a verification
     * error.
     */
    assert(! "More than one user of ProjX");
    return 1;
  }

  n = get_irn_out(proj, 0);
  assert(is_Block(n) && "Argh: user of ProjX is no block");

  if (n == get_irg_end_block(get_irn_irg(n)))
    return 1;

  /* ok, we get here so the raise will not leave the function */
  return 0;
}

/**
 * determine if a value calculated by n "escape", ie
 * is stored somewhere we could not track
 */
static int can_escape(ir_node *n) {
  int i, j, k;

  /* should always be pointer mode or we made some mistake */
  assert(mode_is_reference(get_irn_mode(n)));

  for (i = get_irn_n_outs(n) - 1; i >= 0; --i) {
    ir_node *succ = get_irn_out(n, i);

    switch (get_irn_opcode(succ)) {
    case iro_Store:
      if (get_Store_value(succ) == n) {
        /*
         * We are storing n. As long as we do not further
         * evaluate things, the pointer 'escape' here
         */
        return 1;
      }
      break;

    case iro_Conv:
      /*
       * Should not happen, but if it does we leave the pointer
       * path and do not track further
       */
      return 1;

    case iro_Call: { /* most complicated case */
      ir_node *ptr = get_Call_ptr(succ);
      ir_entity *ent;

      if (get_irn_op(ptr) == op_SymConst &&
          get_SymConst_kind(ptr) == symconst_addr_ent) {
        ent = get_SymConst_entity(ptr);

        /* we know the called entity */
        for (j = get_Call_n_params(succ) - 1; j >= 0; --j) {
          if (get_Call_param(succ, j) == n) {
            /* n is the j'th param of the call */
            if (get_method_param_access(ent, j) & ptr_access_store)
              /* n is store in ent */
              return 1;
          }
        }
      }
      else if (get_irn_op(ptr) == op_Sel) {
        /* go through all possible callees */
        for (k = get_Call_n_callees(succ) - 1; k >= 0; --k) {
          ent = get_Call_callee(succ, k);

          if (ent == unknown_entity) {
            /* we don't know what will be called, a possible escape */
            return 1;
          }

          for (j = get_Call_n_params(succ) - 1; j >= 0; --j) {
            if (get_Call_param(succ, j) == n) {
              /* n is the j'th param of the call */
              if (get_method_param_access(ent, j) & ptr_access_store)
                /* n is store in ent */
                return 1;
            }
          }
        }
      }
      else /* we don't know want will called */
        return 1;

      break;
    }

    case iro_Return:
      /* Bad: the allocate object is returned */
      return 1;

    case iro_Raise:
      /* Hmm: if we do NOT leave the method, it's local */
      if (is_method_leaving_raise(succ))
        return 1;
      break;

    case iro_Tuple: {
      ir_node *proj;

      /* Bad: trace the tuple backwards */
      for (j = get_irn_arity(succ) - 1; j >= 0; --j)
        if (get_irn_n(succ, j) == n)
          break;

      assert(j >= 0);


      for (k = get_irn_n_outs(succ); k >= 0; --k) {
        proj = get_irn_out(succ, k);

        if (get_Proj_proj(proj) == j) {
          /* we found the right Proj */
          succ = proj;
          break;
        }
      }

      /*
       * If we haven't found the right Proj, succ is still
       * the Tuple and the search will end here.
       */
      break;
    }

    default:
      break;

    }

    if (! mode_is_reference(get_irn_mode(succ)))
      continue;

    if (can_escape(succ))
      return 1;
  }
  return 0;
}

/**
 * walker: search for Alloc nodes and follow the usages
 */
static void find_allocations(ir_node *alloc, void *ctx)
{
  int i;
  ir_node *adr;
  walk_env_t *env = ctx;

  if (! is_Alloc(alloc))
    return;

  /* we searching only for heap allocations */
  if (get_Alloc_where(alloc) != heap_alloc)
    return;

  adr = NULL;
  for (i = get_irn_n_outs(alloc) - 1; i >= 0; --i) {
    ir_node *proj = get_irn_out(alloc, i);

    if (get_Proj_proj(proj) == pn_Alloc_res) {
      adr = proj;
      break;
    }
  }

  if (! adr) {
    /*
     * bad: no-one wants the result, should NOT happen but
     * if it does we could delete it.
     */
    set_irn_link(alloc, env->dead_allocs);
    env->dead_allocs = alloc;

    return;
  }

  if (! can_escape(adr)) {
    set_irn_link(alloc, env->found_allocs);
    env->found_allocs = alloc;
  }
}

/**
 * walker: search for allocation Call nodes and follow the usages
 */
static void find_allocation_calls(ir_node *call, void *ctx)
{
  int        i;
  ir_node    *adr;
  ir_entity  *ent;
  walk_env_t *env = ctx;

  if (! is_Call(call))
    return;
  adr = get_Call_ptr(call);
  if (! is_SymConst(adr) || get_SymConst_kind(adr) != symconst_addr_ent)
    return;
  ent = get_SymConst_entity(adr);
  if (! env->callback(ent))
    return;

  adr = NULL;
  for (i = get_irn_n_outs(call) - 1; i >= 0; --i) {
    ir_node *res_proj = get_irn_out(call, i);

    if (get_Proj_proj(res_proj) == pn_Call_T_result) {
      for (i = get_irn_n_outs(res_proj) - 1; i >= 0; --i) {
        ir_node *proj = get_irn_out(res_proj, i);

        if (get_Proj_proj(proj) == 0) {
          /* found first result */
          adr = proj;
          break;
        }
      }
      break;
    }
  }

  if (! adr) {
    /*
     * bad: no-one wants the result, should NOT happen but
     * if it does we could delete it.
     */
    set_irn_link(call, env->dead_allocs);
    env->dead_allocs = call;

    return;
  }

  if (! can_escape(adr)) {
    set_irn_link(call, env->found_allocs);
    env->found_allocs = call;
  }
}

/**
 * Do the necessary graph transformations to transform
 * Alloc nodes.
 */
static void transform_allocs(ir_graph *irg, walk_env_t *env)
{
  ir_node *alloc, *next, *mem, *sel, *size, *blk;
  ir_type *ftp, *atp, *tp;
  ir_entity *ent;
  char name[128];
  unsigned nr = 0;
  dbg_info *dbg;

  /* kill all dead allocs */
  for (alloc = env->dead_allocs; alloc; alloc = next) {
    next = get_irn_link(alloc);

    DBG((dbgHandle, LEVEL_1, "%+F allocation of %+F unused, deleted.\n", irg, alloc));

    mem = get_Alloc_mem(alloc);
	blk = get_nodes_block(alloc);
    turn_into_tuple(alloc, pn_Alloc_max);
    set_Tuple_pred(alloc, pn_Alloc_M, mem);
    set_Tuple_pred(alloc, pn_Alloc_X_regular, new_r_Jmp(irg, blk));
    set_Tuple_pred(alloc, pn_Alloc_X_except, new_r_Bad(irg));

    ++env->nr_deads;
  }

  /* convert all non-escaped heap allocs into frame variables */
  ftp = get_irg_frame_type(irg);
  for (alloc = env->found_allocs; alloc; alloc = next) {
    next = get_irn_link(alloc);
    size = get_Alloc_size(alloc);
    atp  = get_Alloc_type(alloc);

    tp = NULL;
    if (is_SymConst(size) && get_SymConst_kind(size) == symconst_type_size)  {
      /* if the size is a type size and the types matched */
      assert(atp == get_SymConst_type(size));
      tp = atp;
    }
    else if (is_Const(size)) {
      tarval *tv = get_Const_tarval(size);

      if (tv != tarval_bad && tarval_is_long(tv) &&
          get_type_state(atp) == layout_fixed &&
          get_tarval_long(tv) == get_type_size_bytes(atp)) {
        /* a already lowered type size */
        tp = atp;
      }
    }

    if (tp && tp != firm_unknown_type) {
      /* we could determine the type, so we could place it on the frame */
      dbg  = get_irn_dbg_info(alloc);

      DBG((dbgHandle, LEVEL_DEFAULT, "%+F allocation of %+F type %+F placed on frame\n", irg, alloc, tp));

      snprintf(name, sizeof(name), "%s_NE_%u", get_entity_name(get_irg_entity(irg)), nr++);
      ent = new_d_entity(ftp, new_id_from_str(name), get_Alloc_type(alloc), dbg);

      sel = new_rd_simpleSel(dbg, irg, get_nodes_block(alloc),
        get_irg_no_mem(irg), get_irg_frame(irg), ent);
      mem = get_Alloc_mem(alloc);

      turn_into_tuple(alloc, pn_Alloc_max);
      set_Tuple_pred(alloc, pn_Alloc_M, mem);
	  set_Tuple_pred(alloc, pn_Alloc_X_regular, new_r_Jmp(irg, blk));
      set_Tuple_pred(alloc, pn_Alloc_X_except, new_r_Bad(irg));
      set_Tuple_pred(alloc, pn_Alloc_res, sel);

      ++env->nr_removed;
    }
    else {
      /*
       * We could not determine the type or it is variable size.
       * At least, we could place it on the stack
       */
      DBG((dbgHandle, LEVEL_DEFAULT, "%+F allocation of %+F type %+F placed on stack\n", irg, alloc));
      set_Alloc_where(alloc, stack_alloc);

      ++env->nr_changed;
    }
  }

  /* if allocs were removed somehow */
  if (env->nr_removed | env->nr_deads) {
    set_irg_outs_inconsistent(irg);

    if (env->nr_deads) {
      /* exception control flow might have been changed */
      set_irg_doms_inconsistent(irg);
    }
  }
}

/**
 * Do the necessary graph transformations to transform
 * Call nodes.
 */
static void transform_alloc_calls(ir_graph *irg, walk_env_t *env)
{
  ir_node *call, *next, *mem, *blk;
  ir_type *ftp;

  /* kill all dead allocs */
  for (call = env->dead_allocs; call; call = next) {
    next = get_irn_link(call);

    DBG((dbgHandle, LEVEL_1, "%+F allocation of %+F unused, deleted.\n", irg, call));

    mem = get_Call_mem(call);
	blk = get_nodes_block(call);
    turn_into_tuple(call, pn_Call_max);
    set_Tuple_pred(call, pn_Call_M_regular, mem);
	set_Tuple_pred(call, pn_Call_X_regular, new_r_Jmp(irg, blk));
    set_Tuple_pred(call, pn_Call_X_except, new_r_Bad(irg));
    set_Tuple_pred(call, pn_Call_T_result, new_r_Bad(irg));
    set_Tuple_pred(call, pn_Call_M_except, mem);
    set_Tuple_pred(call, pn_Call_P_value_res_base, new_r_Bad(irg));

    ++env->nr_deads;
  }

  /* convert all non-escaped heap allocs into frame variables */
  ftp = get_irg_frame_type(irg);
  for (call = env->found_allocs; call; call = next) {
    next = get_irn_link(call);
  }
}


/* Do simple and fast escape analysis for one graph. */
void escape_enalysis_irg(ir_graph *irg, check_alloc_entity_func callback)
{
  walk_env_t env;

  if (get_irg_callee_info_state(irg) != irg_callee_info_consistent) {
    /* no way yet to calculate this for one irg */
    assert(! "need callee info");
    return;
  }

  if (get_irg_outs_state(irg) != outs_consistent)
    compute_irg_outs(irg);

  env.found_allocs = NULL;
  env.dead_allocs  = NULL;
  env.callback     = callback;
  env.nr_removed   = 0;
  env.nr_changed   = 0;
  env.nr_deads     = 0;

  if (callback) {
    /* search for Calls */
    irg_walk_graph(irg, NULL, find_allocation_calls, &env);
    transform_alloc_calls(irg, &env);
  } else {
    /* search for Alloc nodes */
    irg_walk_graph(irg, NULL, find_allocations, &env);
    transform_allocs(irg, &env);
  }
}

/* Do simple and fast escape analysis for all graphs. */
void escape_analysis(int run_scalar_replace, check_alloc_entity_func callback)
{
  ir_graph *irg;
  int i;
  struct obstack obst;
  walk_env_t *env, *elist;

  if (get_irp_callee_info_state() != irg_callee_info_consistent) {
    assert(! "need callee info");
    return;
  }

  FIRM_DBG_REGISTER(dbgHandle, "firm.opt.escape_ana");

  /*
   * We treat memory for speed: we first collect all info in a
   * list of environments, than do the transformation.
   * Doing it this way, no analysis info gets invalid while we run
   * over graphs
   */
  obstack_init(&obst);
  elist = NULL;

  env = obstack_alloc(&obst, sizeof(*env));
  env->found_allocs = NULL;
  env->dead_allocs  = NULL;
  env->callback     = callback;

  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    irg = get_irp_irg(i);

    assure_irg_outs(irg);

    if (callback) {
      /* search for Calls */
      irg_walk_graph(irg, NULL, find_allocation_calls, env);
    } else {
      /* search for Alloc nodes */
      irg_walk_graph(irg, NULL, find_allocations, env);
    }

    if (env->found_allocs || env->dead_allocs) {
      env->nr_removed   = 0;
      env->nr_deads     = 0;
      env->irg          = irg;
      env->next         = elist;

      elist = env;

      env = obstack_alloc(&obst, sizeof(*env));
      env->found_allocs = NULL;
      env->dead_allocs  = NULL;
      env->callback     = callback;
    }
  }

  if (callback) {
    for (env = elist; env; env = env->next) {
      transform_alloc_calls(env->irg, env);
    }
  } else {
    for (env = elist; env; env = env->next) {
      transform_allocs(env->irg, env);
    }
  }

  obstack_free(&obst, NULL);
}
