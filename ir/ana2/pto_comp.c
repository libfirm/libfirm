/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto_comp.c
   Purpose:     Main Implementation of PTO
   Author:      Florian
   Modified by:
   Created:     Sat Nov 13 19:35:27 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2004 Universität Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/

# ifdef HAVE_CONFIG_H
#  include "config.h"
# endif

/*
  pto_comp: Main Implementation of PTO
*/

# include <string.h>            /* for memset */

# include "pto_comp.h"
# include "pto_util.h"
# include "pto_name.h"
# include "pto_ctx.h"
# include "pto_mod.h"

# include "irnode_t.h"
# include "irprog.h"
# include "xmalloc.h"
# include "irmemwalk.h"

# include "pto_debug.h"
# include "pto_init.h"

# include "ecg.h"

/* Local Defines: */

/* Local Data Types: */
typedef struct pto_env_str {
  struct pto_env_str *enc_env;
  ir_graph *graph;
  int ctx_idx;
  int change;
} pto_env_t;


/* Local Variables: */

/* Debug only: */
char *spaces = NULL;

/* Local Prototypes: */
static pto_t *get_pto (ir_node*, pto_env_t*);
static void pto_call (ir_graph*, ir_node*, pto_env_t*);
static void pto_raise (ir_node*, pto_env_t*);
static void pto_load (ir_node*, pto_env_t*);
static void pto_store (ir_node*, pto_env_t*);
static void pto_method (ir_node*, pto_env_t*);
static void pto_end_block (ir_node*, pto_env_t*);

/* ===================================================
   Local Implementation:
   =================================================== */
/* Add values of the actual arguments to the formal arguments */
static int add_graph_args (ir_graph *graph, ir_node *call, pto_env_t *env)
{
  assert (iro_Call == get_irn_opcode (call));

  int change = FALSE;

  type *meth = get_entity_type (get_irg_entity (graph));

  ir_node **args = get_irg_proj_args (graph);
  int n_args = get_Call_n_params (call);
  int i;

  DBGPRINT (1, (stdout, "%s: args of %s[%li] -> 0x%08x\n",
                __FUNCTION__, OPNAME (call), OPNUM (call), (void*) graph));

  for (i = 0; i < n_args; i ++) {
    if (NULL != args [i]) {
      if (mode_P == get_type_mode (get_method_param_type (meth, i))) {
        ir_node *call_arg = get_Call_param (call, i);
        pto_t *arg_pto = get_pto (call_arg, env);
        pto_t *frm_pto = get_node_pto (args [i]);

        assert (arg_pto);
        assert (frm_pto);

        change |= qset_insert_all (frm_pto->values, arg_pto->values);

        DBGPRINT (2, (stdout, "%s: arg [%i]: -> %s[%li] (%i) -> %s[%li] (%i)\n",
                      __FUNCTION__,
                      i,
                      OPNAME (call_arg), OPNUM (call_arg),
                      arg_pto->values->id,
                      OPNAME (args [i]), OPNUM (args [i]),
                      frm_pto->values->id));
      }
    }
  }

  return (change);
}

/* Transfer the actual arguments to the formal arguments */
static void set_graph_args (ir_graph *graph, ir_node *call, pto_env_t *env)
{
  assert (iro_Call == get_irn_opcode (call));

  type *meth = get_entity_type (get_irg_entity (graph));
  ir_node **args = get_irg_proj_args (graph);

  int n_args = get_Call_n_params (call);
  int i;

  for (i = 0; i < n_args; i ++) {
    if (NULL != args [i]) {
      if (mode_P == get_type_mode (get_method_param_type (meth, i))) {
        ir_node *call_arg = get_Call_param (call, i);
        pto_t *pto = get_pto (call_arg, env);
        assert (pto);
        set_node_pto (args [i], pto);

        DBGPRINT (1, (stdout, "%s: arg [%i]: %s[%li] -> %s[%li] (%i)\n",
                      __FUNCTION__,
                      i,
                      OPNAME (call_arg), OPNUM (call_arg),
                      OPNAME (args [i]), OPNUM (args [i]),
                      pto->values->id));
      }
    }
  }
}

/* Transfer the graph's result to the call */
static int set_graph_result (ir_graph *graph, ir_node *call)
{
  type *tp = get_entity_type (get_irg_entity (graph));

  if (0 == get_method_n_ress (tp)) {
    return (FALSE);
  }

  tp = get_method_res_type (tp, 0);

  if (mode_P != get_type_mode (tp)) {
    set_node_pto (call, NULL);

    return (FALSE);
  }

  ir_node *end_block = get_irg_end_block (graph);
  pto_t *ret_pto = get_node_pto (end_block);

  pto_t *call_pto = get_node_pto (call);

  assert (call_pto);

    DBGPRINT (0, (stdout, "%s: before change args\n", __FUNCTION__));
    DBGEXE (0, pto_print_pto (end_block));
    DBGEXE (0, pto_print_pto (call));

  int change = qset_insert_all (call_pto->values, ret_pto->values);

  if (change) {
    DBGPRINT (0, (stdout, "%s: after change args\n", __FUNCTION__));
    DBGEXE (0, pto_print_pto (end_block));
    DBGEXE (0, pto_print_pto (call));
    /* assert (0); */
  }

  return (change);
}

/* Propagation of PTO values */
static pto_t *get_pto_proj (ir_node *proj, pto_env_t *env)
{
  ir_node *proj_in = get_Proj_pred (proj);
  const long proj_proj = get_Proj_proj (proj);
  const opcode in_op = get_irn_opcode (proj_in);
  pto_t *in_pto = NULL;
  pto_t *proj_pto = NULL; /* get_node_pto (proj); */

  ir_node *proj_in_in = NULL;

  switch (in_op) {
  case (iro_Start):             /* ProjT (Start) */
    assert (0 && "pto from ProjT(Start) requested");

    return (NULL);
  case (iro_Proj):              /* ProjT (Start), ProjT (Call) */
    proj_in_in = get_Proj_pred (proj_in);
    const opcode in_in_op = get_irn_opcode (proj_in_in);
    const long proj_in_proj = get_Proj_proj (proj_in);

    assert ((pn_Start_T_args == proj_in_proj) ||
            (pn_Call_T_result == proj_in_proj));

    switch (in_in_op) {
    case (iro_Start):           /* ProjArg (ProjT (Start)) */
      /* then the pto value must have been set to the node */
      proj_pto = get_node_pto (proj);
      assert (proj_pto);

      return (proj_pto);
    case (iro_Call):            /* ProjV (ProjT (Call)) */
      if (NULL != proj_pto) {
        return (proj_pto);
      } else {
        in_pto = get_pto (proj_in, env);
        set_node_pto (proj, in_pto);

        assert (in_pto);

        return (in_pto);
      }
    default: assert (0 && "Proj(Proj(?))");
    }
    /* done with case (in_op == iro_Proj) */

  case (iro_Load):              /* ProjV (Load) */
    assert (pn_Load_res == proj_proj);
    /* FALLTHROUGH */
  case (iro_Call):              /* ProjT (Call) */
    /* FALLTHROUGH */
  case (iro_Alloc):             /* ProjV (Alloc) */
    if (NULL != proj_pto) {
      return (proj_pto);
    } else {
      in_pto = get_pto (proj_in, env);
      assert (in_pto);

      set_node_pto (proj, in_pto);
      return (in_pto);
    }
  default:
    fprintf (stderr, "%s: not handled: proj (%s[%li])\n",
             __FUNCTION__,
             get_op_name (get_irn_op (proj_in)),
             get_irn_node_nr (proj_in));
    assert (0 && "Proj(?)");
  }

}

static pto_t *get_pto_phi (ir_node *phi, pto_env_t *env)
{
  assert (mode_P == get_irn_mode (phi));

  pto_t *pto = get_node_pto (phi);
  int change = FALSE;

  assert (pto);                 /* must be initialised */

  int n_ins = get_irn_arity (phi);
  int i;

  for (i = 0; i < n_ins; i ++) {
    ir_node *in = get_irn_n (phi, i);
    pto_t *in_pto = get_pto (in, env);

    assert (in_pto);

    change |= qset_insert_all (pto->values, in_pto->values);
  }

  env->change |= change;

  return (pto);
}

static pto_t *get_pto_sel (ir_node *sel, pto_env_t *env)
{
  pto_t *pto = NULL; /* get_node_pto (sel); */

  if (NULL == pto) {
    ir_node *in = get_Sel_ptr (sel);

    pto = get_pto (in, env);
    set_node_pto (sel, pto);
  }

  return (pto);
}

static pto_t *get_pto_ret (ir_node *ret, pto_env_t *env)
{
  pto_t *pto = NULL; /* get_node_pto (ret); */

  if (NULL == pto) {
    ir_node *in = get_Return_res (ret, 0);

    pto = get_pto (in, env);
    set_node_pto (ret, pto);
  }

  assert (pto);

  DBGPRINT (9, (stdout, "%s: ", __FUNCTION__));
  DBGEXE (9, pto_print_pto (ret));

  return (pto);
}


/* Dispatch to propagate PTO values */
static pto_t *get_pto (ir_node *node, pto_env_t *env)
{
  const opcode op = get_irn_opcode (node);

  DBGPRINT (2, (stdout, "%s (%s[%li])\n", __FUNCTION__,
                OPNAME (node), OPNUM (node)));

  switch (op) {
  case (iro_Cast):   return (get_pto (get_Cast_op (node), env));
  case (iro_Proj):   return (get_pto_proj  (node, env));
  case (iro_Phi):    return (get_pto_phi   (node, env));
  case (iro_Sel):    return (get_pto_sel   (node, env));
  case (iro_Alloc):  return (get_alloc_pto (node));
  case (iro_Return): return (get_pto_ret   (node, env));

  case (iro_Call):              /* FALLTHROUGH */
  case (iro_Load):              /* FALLTHROUGH */
  case (iro_Const):             /* FALLTHROUGH */
  case (iro_SymConst):{
    pto_t *pto = get_node_pto (node);

    assert (pto);
    return (pto);
  }
  default:
    /* stopgap measure */
    fprintf (stderr, "%s: not handled: node[%li].op = %s\n",
             __FUNCTION__,
             get_irn_node_nr (node),
             get_op_name (get_irn_op (node)));
    assert (0 && "something not handled");

  }
}


/* Actions for the nodes: */
static void pto_load (ir_node *load, pto_env_t *pto_env)
{
  /* perform load */
  DBGPRINT (2, (stdout, "%s (%s[%li]): pto = 0x%08x\n", __FUNCTION__,
                OPNAME (load), OPNUM (load), (void*) get_node_pto (load)));

  ir_node *ptr = get_Load_ptr (load);

  if (is_dummy_load_ptr (ptr)) {
    return;
  }

  entity *ent = get_ptr_ent (ptr);

  if (mode_P == get_type_mode (get_entity_type (ent))) {
    pto_t *ptr_pto = get_pto (ptr, pto_env);

    assert (ptr_pto);

    DBGPRINT (1, (stdout, "%s (%s[%li]): ptr = 0x%08x\n", __FUNCTION__,
                  OPNAME (ptr), OPNUM (ptr), (void*) ptr_pto));

    pto_env->change |= mod_load (load, ent, ptr_pto);
  }
}

static void pto_store (ir_node *store, pto_env_t *pto_env)
{
  /* perform store */
  DBGPRINT (2, (stdout, "%s (%s[%li]) (no pto)\n", __FUNCTION__,
                OPNAME (store), OPNUM (store)));

  ir_node *ptr = get_Store_ptr (store);
  ir_node *val = get_Store_value (store);

  if (mode_P != get_irn_mode (val)) {
    return;
  }

  entity *ent = get_ptr_ent (ptr);

  pto_t *ptr_pto = get_pto (ptr, pto_env);
  pto_t *val_pto = get_pto (val, pto_env);

  assert (ptr_pto);
  assert (val_pto);

  DBGPRINT (2, (stdout, "%s (%s[%li]): ptr_pto = 0x%08x\n", __FUNCTION__,
                OPNAME (ptr), OPNUM (ptr), (void*) ptr_pto));
  DBGPRINT (2, (stdout, "%s (%s[%li]): val_pto = 0x%08x\n", __FUNCTION__,
                OPNAME (val), OPNUM (val), (void*) val_pto));

  pto_env->change |= mod_store (store, ent, ptr_pto, val_pto);
}

static void pto_method (ir_node *call, pto_env_t *pto_env)
{
  DBGPRINT (2, (stdout, "%s:%i (%s[%li]): pto = 0x%08x\n",
                __FUNCTION__, __LINE__, OPNAME (call), OPNUM (call),
                (void*) get_node_pto (call)));

  callEd_info_t *callEd_info = ecg_get_callEd_info (call);

  if (NULL == callEd_info) {
    DBGPRINT (2, (stdout, "%s:%i (%s[%li]), no graph\n",
                  __FUNCTION__, __LINE__, OPNAME (call), OPNUM (call)));
  }

  int i = 0;
  while (NULL != callEd_info) {
    DBGPRINT (2, (stdout, "%s:%i (%s[%li]), graph %i\n",
                  __FUNCTION__, __LINE__, OPNAME (call), OPNUM (call), i ++));

    pto_call (callEd_info->callEd, call, pto_env);

    callEd_info = callEd_info->prev;
  }
}

/* Perform the appropriate action on the given node */
static void pto_node_node (ir_node *node, pto_env_t *pto_env)
{
  DBGPRINT (1, (stdout, "%s (%s[%li])\n", __FUNCTION__,
                OPNAME (node), OPNUM (node)));

  const opcode op = get_irn_opcode (node);

  switch (op) {
  case (iro_Start): /* nothing */ break;
  case (iro_Load):
    pto_load (node, pto_env);
    break;

  case (iro_Store):
    pto_store (node, pto_env);
    break;

  case (iro_Call):
    pto_method (node, pto_env);
    break;

  case (iro_Raise):
    pto_raise (node, pto_env);
    break;

  case (iro_Return):
    /* nothing to do */
    break;

  case (iro_Alloc):
    /* nothing to do */
    break;

  case (iro_Block):
    pto_end_block (node, pto_env);
    break;

  case (iro_Phi):
    /* must be a PhiM */
    assert (mode_M == get_irn_mode (node));
    /* nothing to do */
    break;

    /* uninteresting stuff: */
  case (iro_Div):
  case (iro_Quot):
  case (iro_Mod):
  case (iro_DivMod): /* nothing to do */ break;

  default:
    /* stopgap measure */
    fprintf (stderr, "%s: not handled: node[%li].op = %s\n",
             __FUNCTION__,
             get_irn_node_nr (node),
             get_op_name (get_irn_op (node)));
    assert (0 && "something not handled");
  }
}


/* Callback function to execute in pre-order */
static void pto_node_pre (ir_node *node, void *env)
{
  /* nothing */
}

/* Callback function to execute in post-order */
static void pto_node_post (ir_node *node, void *env)
{
  pto_env_t *pto_env = (pto_env_t*) env;

  DBGPRINT (999, (stdout, "%s (%s[%li])\n", __FUNCTION__,
                OPNAME (node), OPNUM (node)));

  pto_node_node (node, pto_env);
}

/* Perform a single pass over the given graph */
static void pto_graph_pass (ir_graph *graph, void *pto_env)
{
  irg_walk_mem (graph, pto_node_pre, pto_node_post, pto_env);
}

/* Continue PTO for one of the graphs called at a Call */
static void pto_call (ir_graph *graph, ir_node *call, pto_env_t *pto_env)
{
  int change = FALSE;

  /* only for debugging stuff: */
  entity *ent = get_irg_entity (graph);
  const char *ent_name = (char*) get_entity_name (ent);
  const char *own_name = (char*) get_type_name (get_entity_owner (ent));

  /* perform call */
  DBGPRINT (2, (stdout, "%s (%s[%li]) to \"%s.%s\"\n",
                __FUNCTION__, OPNAME (call), OPNUM (call),
                own_name, ent_name));

  if (! get_irg_is_mem_visited (graph)) {
    /* handle direct call */
    graph_info_t *ginfo = ecg_get_info (graph);

    /* Save CTX */
    int ctx_idx = find_ctx_idx (call, ginfo, get_curr_ctx ());
    ctx_info_t *call_ctx = get_ctx (ginfo, ctx_idx);
    ctx_info_t *old_ctx = set_curr_ctx (call_ctx);

    DBGPRINT (1, (stdout, "%s>CTX: ", -- spaces));
    DBGEXE (1, ecg_print_ctx (call_ctx, stdout));

    /* Initialise Alloc Names and Node values */
    pto_reset_graph_pto (graph, ctx_idx);

    /* Compute Arguments */
    set_graph_args (graph, call, pto_env);

    /* Visit/Iterate Graph */
    pto_graph (graph, ctx_idx, pto_env);

    /* Restore CTX */
    set_curr_ctx (old_ctx);

    /* Get Return Value from Graph */
    change |= set_graph_result (graph, call);

    DBGPRINT (1, (stdout, "%s<CTX: ", spaces ++));
    DBGEXE (1, ecg_print_ctx (call_ctx, stdout));

    /* Don't need to reset alloc names unless we handle recursion here  */
  } else {
    /* handle recursion */
    DBGPRINT (0, (stdout, "%s: recursion into \"%s.%s\"\n",
                  __FUNCTION__, own_name, ent_name));
    /* Find 'right' enclosing pto_env */
    pto_env_t *enc_env = pto_env;

    while (graph != enc_env->graph) {
      enc_env = enc_env->enc_env; /* talk about naming issues here */

      /* since we're in a recursion loop, we *must* find an env for the callEd here: */
      assert (NULL != enc_env->enc_env);
    }

    /* Re-Set arguments */
    int rec_change = add_graph_args (graph, call, pto_env);

      DBGPRINT (1, (stdout, "%s: return  in:", __FUNCTION__));
      DBGEXE (1, pto_print_pto (get_irg_end_block (graph)));

    if (rec_change) {
      DBGPRINT (0, (stdout, "%s: change args", __FUNCTION__));
    }

    rec_change |= set_graph_result (graph, call);

    if (rec_change) {
      DBGPRINT (0, (stdout, "%s: return out:", __FUNCTION__));
      DBGEXE (0, pto_print_pto (get_irg_end_block (graph)));
    }

# if 0
    /* appears that we don't need this: */
    enc_env->change |= rec_change;
# endif /* 0 */
  }

  /* Todo: Set 'Unknown' Value as Return Value when the graph is not
     known */

  pto_env->change |= change;
}

static void pto_raise (ir_node *raise, pto_env_t *pto_env)
{
  /* perform raise */
  DBGPRINT (2, (stdout, "%s (%s[%li]): pto = 0x%08x\n", __FUNCTION__,
                OPNAME (raise), OPNUM (raise), (void*) get_node_pto (raise)));
}

static void pto_end_block (ir_node *end_block, pto_env_t *pto_env)
{
  /* perform end block */
  type *tp = get_entity_type (get_irg_entity (get_irn_irg (end_block)));

  if (0 == get_method_n_ress (tp)) {
    return;
  }

  tp = get_method_res_type (tp, 0);

  if (mode_P != get_type_mode (tp)) {
    return;
  }

  DBGPRINT (2, (stdout, "%s (%s[%li]): pto = 0x%08x\n", __FUNCTION__,
                OPNAME (end_block), OPNUM (end_block),
                (void*) get_node_pto (end_block)));

  pto_t *end_pto = get_node_pto (end_block);

  assert (end_pto);

  int n_ins = get_irn_arity (end_block);
  int i;
  for (i = 0; i < n_ins; i ++) {
    ir_node *in = get_irn_n (end_block, i);

    if (iro_Return == get_irn_opcode (in)) {
      pto_t *in_pto = get_pto (in, pto_env);

      pto_env->change |= qset_insert_all (end_pto->values, in_pto->values);
    }
  }
}

/* ===================================================
   Exported Implementation:
   =================================================== */
/* Main loop: Initialise and iterate over the given graph */
void pto_graph (ir_graph *graph, int ctx_idx, pto_env_t *enc_env)
{
  /* Also exported, since we need it in 'pto.c' */
  pto_env_t *pto_env = xmalloc (sizeof (pto_env_t));
  pto_env->enc_env = enc_env;
  pto_env->graph   = graph;
  pto_env->ctx_idx = ctx_idx;
  pto_env->change  = TRUE;

  /* HERE ("start"); */

  DBGPRINT (2, (stdout, "%s: start for ctx %i\n",
                __FUNCTION__,
                ctx_idx));

  /* todo (here): iterate, obey 'changed' attribute */
  int run = 0;
  while (0 != pto_env->change) {
    run ++;
    pto_env->change = FALSE;
    pto_graph_pass (graph, pto_env);
  }

  DBGPRINT (1, (stdout, "%s: %i runs on \"%s.%s\"\n",
                __FUNCTION__,
                run,
                get_type_name (get_entity_owner (get_irg_entity (graph))),
                get_entity_name (get_irg_entity (graph))));


  memset (pto_env, 0x00, sizeof (pto_env_t));
  free (pto_env);
  /* HERE ("end"); */
}

/* Set the PTO value for the given non-alloc node */
void set_node_pto (ir_node *node, pto_t *pto)
{
  assert (iro_Alloc != get_irn_opcode (node));

  set_irn_link (node, (void*) pto);
}

/*Get the PTO value for the given non-alloc node */
pto_t *get_node_pto (ir_node *node)
{
  assert (iro_Alloc != get_irn_opcode (node));

  return ((pto_t*) get_irn_link (node));
}

/* Set the PTO value for the given alloc node */
void set_alloc_pto (ir_node *alloc, alloc_pto_t *alloc_pto)
{
  assert (iro_Alloc == get_irn_opcode (alloc));

  assert (alloc_pto);

  set_irn_link (alloc, (void*) alloc_pto);
}

/*Get the current PTO value for the given alloc node */
pto_t *get_alloc_pto (ir_node *alloc)
{
  alloc_pto_t *alloc_pto = (alloc_pto_t*) get_irn_link (alloc);

  assert (iro_Alloc == get_irn_opcode (alloc));

  assert (alloc_pto -> curr_pto);

  return (alloc_pto -> curr_pto);
}


/*
  $Log$
  Revision 1.10  2004/12/20 17:34:35  liekweg
  fix recursion handling

  Revision 1.9  2004/12/15 13:31:18  liekweg
  remove debugging stuff

  Revision 1.8  2004/12/15 09:18:18  liekweg
  pto_name.c

  Revision 1.7  2004/12/06 12:55:06  liekweg
  actually iterate

  Revision 1.6  2004/12/02 16:17:51  beck
  fixed config.h include

  Revision 1.5  2004/11/30 14:47:54  liekweg
  fix initialisation; do correct iteration

  Revision 1.4  2004/11/26 15:59:40  liekweg
  verify pto_{load,store}

  Revision 1.3  2004/11/24 14:53:55  liekweg
  Bugfixes

  Revision 1.2  2004/11/20 21:21:56  liekweg
  Finalise initialisation

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
