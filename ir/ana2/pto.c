/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/pto.c
 * Purpose:     Pto
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# ifdef HAVE_CONFIG_H
#  include <config.h>
# endif

# include "pto.h"
# include "pto_util.h"
# include "pto_init.h"

# include "entity.h"

# include "irnode.h"
# include "irprog.h"

/* # include "eset.h" */
# include "irgraph.h"
# include "irgwalk.h"
# include "irgmod.h"
# include "irvrfy.h"
# include "trvrfy.h"
# include "xmalloc.h"

# include "typewalk.h"
# include "irmemwalk.h"

# define DBGPRINT(lvl, msg) if (get_pto_verbose () > lvl) { fprintf msg; }

/*
  Local Protos
*/
static void pto_node (ir_node*, void*);
static void pto_node_pre  (ir_node*, void*);
static void pto_node_post (ir_node*, void*);

/*
   Get the pto infos of a node
*/
static pto_t *get_pto (ir_node *node)
{


  return ((pto_t*) get_irn_link (node));
}

/*
  Propagate pto values up to the given node, and return the propagated
  value.  This recursion has to stop at important nodes, such as
  loads and allocs, where pto is computed differently (albeit this may
  involve more calls to this function for a different ptr value).
*/
static pto_t *compute_pto (ir_node *node, void *env)
{
  pto_t *node_pto = get_pto (node);

  if ((NULL != node_pto) && (pto_is_dummy (node_pto))) {
    /* weed out initialisation data as good as possible */

    DBGPRINT (0, (stdout, "%s: dummy pto for (%s[%li])\n",
                  __FUNCTION__,
                  get_op_name (get_irn_op (node)),
                  get_irn_node_nr (node)));

    pto_delete (node_pto);
    node_pto = NULL;
  }

  if (NULL == node_pto) {
    DBGPRINT (1, (stdout, "%s: must compute pto for %s[%li]\n",
                  __FUNCTION__,
                  get_op_name (get_irn_op (node)),
                  get_irn_node_nr (node)));

    pto_node (node, env);

    node_pto = get_pto (node);
  }

  assert (node_pto);

  return (node_pto);
}

/*
  Transfer the actual arguments to the given call's formal arguments
*/
static void set_call_args (ir_node *call, ir_graph *graph, void *env)
{
  ir_node **args = find_irg_args (graph);
  int i;

  const int n_call_args = get_irn_arity (call);

  /* call: M x meth_ptr x Arg x Arg x ... x Arg */
  /* projT(start): Arg x Arg x ... x Arg */
  /* projM(start): M */

  for (i = 2; i < n_call_args; i ++) {
    if (NULL != args [i-2]) {
      if (mode_P == get_irn_mode (args [i-2])) {
        pto_t *arg_pto = compute_pto (get_irn_n (call, i), env);
        /* off-by-two because of ProjT bd */
        set_pto (args [i-2], arg_pto);
      } else {
        /* not a pointer value */
      }
    }
  }
  free (args);
}

/*
  Transfer the return value of a call to the callR node
*/
static void get_call_ret (ir_node *call, ir_graph *graph, void *env)
{
  entity *ent = get_irg_ent (graph);
  type *ent_tp = get_entity_type (ent);

  if (NULL != get_pto (call)) {
    pto_t *old = get_pto (call);

    if (pto_is_dummy (old)) {
      DBGPRINT (2, (stdout, "%s: dummy pto (0x%08x) from call[%li]\n",
                    __FUNCTION__, (int) old, get_irn_node_nr (call)));
    }

    pto_delete (old);
  }

  if (0 != get_method_n_ress (ent_tp)) {
    type *ent_ret_tp = get_method_res_type (ent_tp, 0);

    if (mode_P == get_type_mode (ent_ret_tp)) {
      pto_t *res_pto = get_pto (get_irg_end_block (graph));

      set_pto (call, res_pto);
    }
  }
}


/*
  Take care of a single proj node.  Basically a multiplex/dispatch for the
  different node types that a proj can have as a predecessor.
*/
static void pto_node_proj (ir_node *proj, void *env)
{
  /*
    pto for proj({proj(start),load,call,alloc,raise?,...}) node
  */
  ir_node *in = get_Proj_pred (proj);
  const opcode in_op = get_irn_opcode (in);
  const long proj_proj = get_Proj_proj (proj);

  DBGPRINT (3, (stdout, "%s: --> Proj[%li] (%s)\n",
                __FUNCTION__,
                get_irn_node_nr (proj),
                get_op_name (get_irn_op (in))));

  switch (in_op) {
  case (iro_Start): {
    /* nothing (handled by proj(proj)) */
  } break;

  case (iro_Load): {
    /* copy from load */
    if (pn_Load_res == proj_proj) {
      set_pto (proj, compute_pto (in, env));
    } else {
      /* ProjM(load) or ProjX(load) - do nothing */
    }
  } break;

  case (iro_Store): {
    /* ProjM (store) or ProjX (store) - nothing */
  } break;

  case (iro_Alloc): {
    /* copy from alloc */
    if (pn_Alloc_res == proj_proj) {
      set_pto (proj, compute_pto (in, env));
    } else {
      /* ProjM(alloc) or ProjX (alloc) -- nothing */
    }
  } break;

  case (iro_Raise): {
    /* ProjX (raise), ProjM (raise) -- nothing */
  } break;

  case (iro_Call): {
    if (pn_Call_M_regular == proj_proj) {
      /* ProjM(call) -- nothing */
    } else if (pn_Call_T_result == proj_proj) {
      /* copy return value of call */
      pto_t *call_pto = get_pto (in);   /* get result from call */
      assert (call_pto);
      set_pto (proj, call_pto);
    } else if (pn_Call_P_value_res_base == proj_proj) {
      /* wtf? */
      assert (0 && "what's with pn_Call_P_value_res_base?");
    } else {
      /* ProjX (call) or ProjM_exc (call) -- nothing */
    }

  } break;

  case (iro_Proj): {
    const ir_node *in_in = get_Proj_pred (in);
    const opcode in_in_op = get_irn_opcode (in_in);

    switch (in_in_op) {
    case (iro_Call): {
      /* projP (projT (call)) */
      /* copy from projT to projP */
      pto_t *in_pto = compute_pto (in, env);
      set_pto (proj, in_pto);
    } break;
    case (iro_Start): {
      /* proj(proj(start)) - make sure the arguments are initialised */
      assert (get_pto (proj));
    } break;
    default: {
      DBGPRINT (1, (stdout, "%s:%i: proj(proj(%s)) not handled\n",
                    __FILE__, __LINE__, get_op_name (get_irn_op (in_in))));
      assert (0);
    }
    }
  } break;

  case (iro_Cast): {
    /* make sure the input has been analysed */
    pto_t *cast_pto = compute_pto (in, env);
    set_pto (proj, cast_pto);

  } break;

  default: {
    fprintf (stderr, "%s: opcode %s of Node %ld not handled\n",
             __FUNCTION__,
             get_op_name (get_irn_op (in)),
             get_irn_node_nr (in));

    assert (0 && "something not handled");
  }
  }

  DBGPRINT (2, (stdout, "%s: Proj (%s)\n",
                __FUNCTION__,
                get_op_name (get_irn_op (in))));
}

static void pto_node_obj_load (ir_node *load, ir_node *ptr,
                               entity *ent, void *env)
{
  /*
    pto for obj load node

    so far:
    load.ptr analysed
    todo:
    look up
  */
  const char *ent_name = (char*) get_entity_name (ent);
  const char *own_name = (char*) get_type_name (get_entity_owner (ent));

  DBGPRINT (1, (stdout, "%s for (%s[%li])\n",
                __FUNCTION__,
                get_op_name (get_irn_op (ptr)),
                get_irn_node_nr (ptr)));

  if (! is_pointer_type (get_entity_type (ent))) {
    return;
  }

  if (NULL != get_pto (load)) {
    pto_t *old = get_pto (load);

    if (pto_is_dummy (old)) {
      DBGPRINT (0, (stdout, "%s: dummy pto (0x%08x) from load[%li]\n",
                    __FUNCTION__, (int) old, get_irn_node_nr (load)));
    }

    pto_delete (old);
  }

  DBGPRINT (0, (stdout, "%s: obj load from ent (0x%08x) \"%s.%s\"\n",
                __FUNCTION__,
                (int) ent,
                own_name,
                ent_name));

  pto_t *ptr_objs = compute_pto (ptr, env);
  qset_t *objs = ptr_objs->objs;
  pto_t *res = pto_new_empty (load);
  obj_desc_t *obj_desc = (obj_desc_t*) qset_start (objs);

  /*   fprintf (stdout, "%s: load ptr = ", __FUNCTION__); */
  /*   qset_print (ptr_objs->objs, stdout); */

  while (NULL != obj_desc) {
    qset_t *cnts = pto_lookup (obj_desc, ent);

    pto_add_all_names (res, cnts);

    /*     fprintf (stdout, "%s: load val = ", __FUNCTION__); */
    /*     qset_print (cnts, stdout); */

    obj_desc = (obj_desc_t*) qset_next (objs);
  }

  /*   fprintf (stdout, "%s: load res = ", __FUNCTION__); */
  /*   qset_print (res->objs, stdout); */

  set_pto (load, res);
}

static void pto_node_arr_load (ir_node *load, ir_node *ptr,
                               entity *ent, void *env)
{
  /*
    pto for array load node

    so far:
    load.ptr analysed or can be computed on-the-fly
    todo:
    look up
  */
  const char *ent_name = (char*) get_entity_name (ent);
  const char *own_name = (char*) get_type_name (get_entity_owner (ent));

  /* load from array */
  DBGPRINT (1, (stdout, "%s for (%s[%li])\n",
                __FUNCTION__,
                get_op_name (get_irn_op (ptr)),
                get_irn_node_nr (ptr)));

  if (! is_pointer_type (get_entity_type (ent))) {
    return;
  }

  DBGPRINT (0, (stdout, "%s: array load from ent (0x%08x) \"%s.%s\"\n",
                __FUNCTION__,
                (int) ent,
                own_name,
                ent_name));

  pto_t *ptr_objs =  compute_pto (ptr, env);
  qset_t *objs = ptr_objs->objs;
  pto_t *res = pto_new_empty (load);

  obj_desc_t *obj_desc = (obj_desc_t*) qset_start (objs);

  while (NULL != obj_desc) {
    qset_t *cnts = pto_lookup (obj_desc, NULL);

    pto_add_all_names (res, cnts);

    obj_desc = (obj_desc_t*) qset_next (objs);
  }

  set_pto (load, res);
}

static void pto_node_load (ir_node *load, void *env)
{
  /*
    pto for load node

    so far:
    load.ptr analysed or can be computed on-the-fly
    todo:
    look up
  */
  ir_node *ptr = get_Load_ptr (load);
  const opcode op = get_irn_opcode (ptr);
  entity *ent = NULL;

  /* check the funny cases */
  if (iro_Proj == op) {
    /* then it's an unused Load(this) (or whatever) and we don't need to look at it */
    DBGPRINT (1, (stderr, "%s: %s[%li] ignored\n",
                  __FUNCTION__,
                  get_op_name (get_irn_op (ptr)),
                  get_irn_node_nr (ptr)));
    return;
  } else if (iro_Cast == op) {
    /* then it's  (whatever) and we don't know where to look at */
    DBGPRINT (1, (stderr, "%s: %s[%li] ignored\n",
                  __FUNCTION__,
                  get_op_name (get_irn_op (ptr)),
                  get_irn_node_nr (ptr)));
    return;
  }

  ent = get_ptr_ent (ptr);

  assert (ent);

  /* array load or obj load? */
  if ((iro_Sel == op) && (3 == get_irn_arity (ptr))) {
    pto_node_arr_load (load, ptr, ent,  env);
  } else {
    pto_node_obj_load (load, ptr, ent, env);
  }

}

static void pto_node_obj_store (ir_node *store,
                                ir_node *ptr,
                                entity *ent,
                                ir_node *val,
                                void *env)
{
  /*
    pto for obj store node

    so far: ptr analysed or can be computed on-the-fly
    todo:
    update
  */


  const char *ent_name = (char*) get_entity_name (ent);
  const char *own_name = (char*) get_type_name (get_entity_owner (ent));

  DBGPRINT (0, (stdout, "%s: obj store from ent (0x%08x) \"%s.%s\"\n",
                __FUNCTION__,
                (int) ent, own_name, ent_name));

  pto_t *ptr_pto = compute_pto (ptr, env);
  pto_t *val_pto = compute_pto (val, env);
  qset_t *objs = ptr_pto->objs;
  qset_t *vals = val_pto->objs;

  obj_desc_t *obj_desc = (obj_desc_t*) qset_start (objs);

  while (NULL != obj_desc) {
    qset_t *cnts = pto_lookup (obj_desc, ent);

    qset_insert_all (cnts, vals);

    obj_desc = (obj_desc_t*) qset_next (objs);
  }
}

static void pto_node_arr_store (ir_node *store,
                                ir_node *ptr,
                                entity *ent,
                                ir_node *val,
                                void *env)
{
  /*
    pto for array store node

    so far: ptr analysed or can be computed on-the-fly
    todo:
    update
  */

  const char *ent_name = (char*) get_entity_name (ent);
  const char *own_name = (char*) get_type_name (get_entity_owner (ent));

  DBGPRINT (0, (stdout, "%s: array store from ent (0x%08x) \"%s.%s\"\n",
                __FUNCTION__,
                (int) ent, own_name, ent_name));

  pto_t *ptr_pto = compute_pto (ptr, env);
  pto_t *val_pto = compute_pto (val, env);
  qset_t *objs = ptr_pto->objs;
  qset_t *vals = val_pto->objs;

  obj_desc_t *obj_desc = (obj_desc_t*) qset_start (objs);

  while (NULL != obj_desc) {
    qset_t *cnts = pto_lookup (obj_desc, NULL);

    qset_insert_all (cnts, vals);

    obj_desc = (obj_desc_t*) qset_next (objs);
  }
}

static void pto_node_store (ir_node *store, void *env)
{
  /*
    pto for store node

    so far: ptr analysed or can be computed on-the-fly
    todo:
    update
  */

  ir_node *ptr = get_Store_ptr (store);
  ir_node *val = get_Store_value (store);
  const opcode op = get_irn_opcode (ptr);
  entity *ent = get_ptr_ent (ptr);

  if (mode_P != get_irn_mode (val)) {
    return;
  }

  /* array load or obj load? */
  if ((iro_Sel == op) && (3 == get_irn_arity (ptr))) {
    pto_node_arr_store (store, ptr, ent, val, env);
  } else {
    pto_node_obj_store (store, ptr, ent, val, env);
  }

}

static void pto_node_alloc (ir_node *alloc, void *env)
{
  /*
    pto for alloc node

    so far: nothing to do
    todo:
    invent new name
 */

  type *tp = get_Alloc_type (alloc);

  pto_t *alloc_pto = pto_new_name (alloc, tp);
  set_pto (alloc, alloc_pto);
}

static void pto_node_free (ir_node *free, void *env)
{
  /*
    pto for free node

    so far: ptr analysed or can be computed on-the-fly
    todo:
    nothing, actually
 */
  /* still, copy somthing */
  ir_node *ptr = get_Free_ptr (free);

  pto_t *ptr_pto = compute_pto (ptr, env);
  set_pto (free, ptr_pto);
}

static void pto_node_raise (ir_node *raise, void *env)
{
  /*
    pto for raise node

    so far: ptr analysed or can be computed on-the-fly
    todo:

 */

  /* right now, just copy something */
  ir_node *ptr = get_Raise_exo_ptr (raise);

  pto_t *ptr_pto = compute_pto (ptr, env);

  set_pto (raise, ptr_pto);
}

static void pto_node_sel (ir_node *sel, void *env)
{
  /*
    pto for sel node

    so far: input selected or can be computed on-the-fly
    todo:
    just copy (the selected entity is used in the load/store)
  */

  ir_node *sel_in = get_Sel_ptr (sel);
  pto_t *sel_pto = compute_pto (sel_in, env);

  if (NULL == sel_pto) {
    fprintf (stdout, "%s:%i: sel.in = %s[%li]\n",
             __FUNCTION__, __LINE__,
             get_op_name (get_irn_op (sel_in)),
             get_irn_node_nr (sel_in));
  }
  assert (sel_pto);

  set_pto (sel, sel_pto);
}

static void pto_node_call (ir_node *call, void *env)
{
  ir_graph *graph = NULL;
  ir_node *ptr = get_Call_ptr (call);
  entity *ent = get_ptr_ent (ptr);

  DBGPRINT (1, (stdout, "%s (%s[%li])\n",
                __FUNCTION__,
                get_op_name (get_irn_op (call)),
                get_irn_node_nr (call)));


  const char *ent_name = (char*) get_entity_name (ent);
  const char *own_name = (char*) get_type_name (get_entity_owner (ent));

  /* Todo:  Iterate over all graphs in 'get_implementing_graphs' */
  graph = get_entity_irg (ent);
  if (NULL != graph) {
    if (! get_irg_is_mem_visited (graph)) {
      DBGPRINT (1, (stdout, " -> visit  graph (0x%08x) of \"%s.%s\"\n",
                    (int) graph, own_name, ent_name));

      /* compute call arguments */
      set_call_args (call, graph, env);
      /* traverse callEd */
      irg_walk_mem (graph, pto_node_pre, pto_node_post, NULL);
      /* maybe get result from called graph */
      get_call_ret (call, graph, env);
    }
  } else {
    DBGPRINT (0, (stdout, "%s:%i: Warning: no graph for ent \"%s.%s\" in call[%li]\n",
                  __FILE__, __LINE__, own_name, ent_name, get_irn_node_nr (call)));
  }
}

static void pto_node_ret (ir_node *ret, void *env)
{
  /*
    pto for return node

    so far:
    input should be availlable
    todo:
    just copy
  */

  if ((0 != get_Return_n_ress (ret)) &&
      (mode_P == get_irn_mode (get_Return_res (ret, 0)))) {
    ir_node *ret_in = get_Return_res (ret, 0);

    pto_t *ret_pto = compute_pto (ret_in, env);

    DBGPRINT (4, (stdout, "--> Return Node (%ld) (%s)\n",
                  get_irn_node_nr (ret),
                  get_op_name (get_irn_op (ret))));

    set_pto (ret, ret_pto);
  } else {
    /* nothing to do! */
  }
}

static void pto_node_phi (ir_node *phi, void *env)
{
  /*
    pto for phi node

    so far:
    all ins present or can be computed on-the-fly
    todo:
    collect ins
  */

  int i;
  int n_ins = get_irn_arity (phi);
  ir_node *in = NULL;

  if (mode_P != get_irn_mode (phi)) {
    return;
  }

  assert (0 && "test phi");

  pto_t *phi_pto = get_pto (phi);

  if (NULL == phi_pto) {
    phi_pto = pto_new_empty (phi);
    set_pto (phi, phi_pto);
  }

  for (i = 0; i < n_ins; i ++) {
    in = get_irn_n (phi, i);

    pto_t *in_pto = compute_pto (in, env);

    DBGPRINT (0, (stdout, "%s: IN PHI Node (%ld) (%s) (pto = 0x%08x)\n",
                  __FUNCTION__,
                  get_irn_node_nr (in),
                  get_op_name (get_irn_op (in)),
                  (int) in_pto));

    qset_insert_all (phi_pto->objs, in_pto->objs);
  }
}

static void pto_node_cnst (ir_node *cnst, void *env)
{
  /*
    pto for const node

    so far: nothing
    todo:
    if this represents something nameable, name it
  */
  type *tp = get_Const_type (cnst);
  pto_t *cnst_pto = pto_new_name (cnst, tp);


  set_pto (cnst, cnst_pto);
}

static void pto_node_sym_cnst (ir_node *sym_cnst, void *env)
{
  /*
    pto for const node

    so far: nothing
    todo:
    if this represents something nameable, name it
  */
  type *tp = get_entity_type (get_SymConst_entity (sym_cnst));

  if (is_class_type (tp)) {
  pto_t *sym_cnst_pto = pto_new_name (sym_cnst, tp);

  set_pto (sym_cnst, sym_cnst_pto);
  } else {
    /* nothing to do */
  }
}

static void pto_node_end_block (ir_node *end, void *env)
{
  /*
    pto for const node

    so far: all returns are set or can be computed on-the-fly
    todo:
    collect results, set to node.
  */
  type *tp = get_entity_type (get_irg_entity (get_irn_irg (end)));

    DBGPRINT (2, (stdout, "%s: End Block (%ld) (%s)\n",
                  __FUNCTION__,
                  get_irn_node_nr (end),
                  get_op_name (get_irn_op (end))));

  if (0 != get_method_n_ress (tp)) {
    tp = get_method_res_type (tp, 0);

    if (mode_P == get_type_mode (tp)) {
      int i;
      int n_ins = get_irn_arity (end);
      pto_t *end_pto = pto_new_name (end, get_pointer_points_to_type (tp));

      for (i = 0; i < n_ins; i ++) {
        ir_node *ret = get_irn_n (end, i);

        pto_t *ret_pto = get_pto (ret);
        assert (ret_pto);
      }

      set_pto (end, end_pto);
    }
  }
}

/*
  Take care of a single node.  Basically a multiplex/dispatch for the
  different node types.
*/
static void pto_node (ir_node *node, void *env)
{
  const opcode op = get_irn_opcode (node);

  DBGPRINT (1, (stdout, "%s (%s[%li])\n",
                __FUNCTION__,
                get_op_name (get_irn_op (node)),
                get_irn_node_nr (node)));

  switch (op) {
  case (iro_Start): {
    /* pto_node_start (node, env); */
    /* nothing to do */

  } break;
  case (iro_Load): {
    pto_node_load (node, env);

  } break;
  case (iro_Store): {
    pto_node_store (node, env);

  } break;
  case (iro_Alloc): {
    pto_node_alloc (node, env);

  } break;
  case (iro_Free): {
    pto_node_free (node, env);

  } break;
  case (iro_Raise): {
    pto_node_raise (node, env);

  } break;
  case (iro_Sel): {
    pto_node_sel (node, env);

  } break;
  case (iro_Call): {
    /* assert (0 && "calls must be handled in main loop"); */
    pto_node_call (node, env);
  } break;
  case (iro_Return): {
    if (0 < get_Return_n_ress (node)) {
      pto_node_ret (node, env);
    }
  } break;
  case (iro_Proj): {
    pto_node_proj (node, env);
  } break;
  case (iro_Phi): {
    pto_node_phi (node, env);
  } break;
  case (iro_Const): {
    pto_node_cnst (node, env);
  } break;
  case (iro_SymConst): {
    pto_node_sym_cnst (node, env);
  } break;
  case (iro_Cast): {
    pto_t *cast_pto = compute_pto (get_Cast_op (node), env);
    set_pto (node, cast_pto);
  } break;
  case (iro_Block): {
    /* End Block ONLY */
    pto_node_end_block (node, env);
  } break;
  /* all the uninteresting stuff */
  case (iro_Div):
  case (iro_Quot):
  case (iro_Mod):
  case (iro_DivMod):
    set_pto (node, NULL);
    break;
  default: {
    /* stopgap measure */
    DBGPRINT (0, (stdout, "%s: not handled: node[%li].op = %s\n",
                  __FUNCTION__,
                  get_irn_node_nr (node),
                  get_op_name (get_irn_op (node))));
    assert (0 && "something not handled");
  }
  }
}
static void pto_node_pre (ir_node *node, void *env)
{
  DBGPRINT (1, (stdout, "%s (%s[%li])\n",
                __FUNCTION__,
                get_op_name (get_irn_op (node)),
                get_irn_node_nr (node)));

  pto_init_node (node);
}

static void pto_node_post (ir_node *node, void *env)
{
  DBGPRINT (1, (stdout, "%s (%s[%li])\n",
                __FUNCTION__,
                get_op_name (get_irn_op (node)),
                get_irn_node_nr (node)));

  pto_node (node, env);
}

static int pto_verbose = 0;

/*
  Helper to pto_init --- clear the link fields of class types
*/
static void clear_type_link (type_or_ent *thing, void *__unused)
{
  if (is_type (thing)) {
    type *tp = (type*) thing;

    if (is_class_type (tp)) {
      DBGPRINT (4, (stdout, "%s (\"%s\")\n",
                    __FUNCTION__, get_type_name (tp)));

      set_type_link (tp, NULL);
    }
  }
}

/*
  Helper to pto_cleanup --- deallocate the field closure lists and clear the link fields of class types
*/
static void free_field_list (type_or_ent *thing, void *__unused)
{
  if (is_type) {
    type *tp = (type*) thing;

    if (is_class_type (tp)) {
      if (NULL != get_type_link (tp)) {
        entity **fields = (entity**) get_type_link (tp);

        free (fields);
      }

      set_type_link (tp, NULL);
    }
  }
}


void set_pto (ir_node *node, pto_t *pto)
{
  check_pto (pto);

  set_irn_link (node, (void*) pto);
}

int get_pto_verbose ()
{
  return (pto_verbose);
}

void set_pto_verbose (int verbose)
{
  pto_verbose = verbose;
}

/*
  Initialise Pto
*/
void pto_init ()
{
  type_walk (clear_type_link, NULL, NULL);
}


/*
  Run Pto
*/
void pto_run (int do_verbose)
{
  /* int i; */
  set_pto_verbose (do_verbose);

  DBGPRINT (0, (stdout, "START PTO\n"));

  ir_graph *graph = get_irp_main_irg ();

  DBGPRINT (0, (stdout, "START GRAPH (0x%08x) of \"%s.%s\"\n",
                (int) graph,
                get_type_name (get_entity_owner (get_irg_entity (graph))),
                get_entity_name (get_irg_entity (graph))));

  /* Set args for main graph */
  {
    type *meth_tp = get_entity_type (get_irg_entity (graph));
    type *param_tp = get_method_param_type (meth_tp, 1);
    param_tp = get_pointer_points_to_type (param_tp);

    const long n_args =         /* wtf? */
      get_method_n_params (meth_tp);
    ir_node **args = find_irg_args (graph);
    pto_t *main_pto = pto_new_name (args [1], param_tp);
    int i;

    for (i = 0; i < n_args; i ++) {
      DBGPRINT (2, (stdout, "arg [%i] = %s[%ld]\n",
                    i,
                    args [i] ? get_op_name (get_irn_op (args [i])) : "none",
                    args [i] ? get_irn_node_nr (args [i]) : -1));
    }
    set_pto (args [1], main_pto);

    free (args);
  }

  irg_walk_mem (graph, pto_node_pre, pto_node_post, NULL);

  DBGPRINT (0, (stdout, "END   GRAPH (0x%08x)\n", (int) graph));
  DBGPRINT (0, (stdout, "END   PTO\n"));

  obj_desc_list_all (NULL);
}

/*
  Clean Up
*/
void pto_cleanup ()
{
  type_walk (free_field_list, NULL, NULL);
}



/*
 * $Log$
 * Revision 1.5  2004/11/08 12:33:06  liekweg
 * initialisation; sanitize print levels, misc fixes
 *
 * Revision 1.4  2004/11/04 14:58:38  liekweg
 * expanded pto, added initialisation, added debugging printing
 *
 * Revision 1.3  2004/10/25 11:59:45  liekweg
 * Copy Only works
 *
 * Revision 1.2  2004/10/21 11:09:37  liekweg
 * Moved memwalk stuf into irmemwalk
 * Moved lset stuff into lset
 * Moved typalise stuff into typalise
 *
 * Revision 1.1  2004/10/20 14:59:42  liekweg
 * Added ana2, added ecg and pto
 *
 */
