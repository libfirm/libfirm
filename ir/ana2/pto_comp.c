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
#  include <config.h>
# endif

/*
  pto_comp: Main Implementation of PTO
*/

# include <string.h>            /* for memset */

# include "pto_comp.h"
# include "pto_util.h"
# include "pto_ctx.h"

# include "irnode.h"
# include "irprog.h"
# include "xmalloc.h"
# include "irmemwalk.h"

# include "pto_debug.h"
# include "pto_init.h"

# include "ecg.h"

/* Local Defines: */

/* Local Data Types: */
typedef struct pto_env_str {
  int dummy;
} pto_env_t;


/* Local Variables: */

/* Debug only: */
char *spaces = NULL;

/* Local Prototypes: */
static void pto_graph (ir_graph*);
static void pto_call (ir_graph*, ir_node*, pto_env_t*);

/* ===================================================
   Local Implementation:
   =================================================== */

/* Propagation of PTO values */
static pto_t *get_pto_proj (ir_node *proj)
{
  ir_node *proj_in = get_Proj_pred (proj);
  const long proj_proj = get_Proj_proj (proj);
  const opcode in_op = get_irn_opcode (proj_in);
  pto_t *in_pto = NULL;
  pto_t *proj_pto = get_node_pto (proj);

  switch (in_op) {
  case (iro_Start):             /* ProjT (Start) */
    assert (0 && "pto from ProjT(Start) requested");

    return (NULL);
  case (iro_Proj):              /* ProjT (Start), ProjT (Call) */
    assert ((pn_Start_T_args == proj_proj) ||
            (pn_Call_T_result == proj_proj));
    ir_node *proj_in_in = get_Proj_pred (proj_in);
    const opcode in_in_op = get_irn_opcode (proj_in_in);

    switch (in_in_op) {
    case (iro_Start):           /* ProjArg (ProjT (Start)) */
      /* then the pto value must have been set to the node */
      assert (proj_pto);

      return (proj_pto);
    case (iro_Call):            /* ProjV (ProjT (Call)) */
      if (NULL != proj_pto) {
        return (proj_pto);
      } else {
        in_pto = get_node_pto (proj_in);
        set_node_pto (proj, in_pto);

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
      in_pto = get_alloc_pto (proj_in);
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

static pto_t *get_pto_phi (ir_node *phi)
{
  return (NULL);
}

static pto_t *get_pto_ret (ir_node *ret)
{
  pto_t *pto = get_node_pto (ret);

  if (NULL == pto) {
    ir_node *in = get_Return_res (ret, 0);

    pto = get_node_pto (in);
    set_node_pto (ret, pto);
  }

  return (pto);
}


/* Dispatch to propagate PTO values */
static pto_t *get_pto (ir_node *node)
{
  const opcode op = get_irn_opcode (node);

  switch (op) {
  case (iro_Cast):   return (get_pto (get_Cast_op (node)));
  case (iro_Proj):   return (get_pto_proj (node));
  case (iro_Phi):    return (get_pto_phi (node));
  case (iro_Return): return (get_pto_ret (node));

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
  DBGPRINT (0, (stdout, "%s (%s[%li])\n", __FUNCTION__, OPNAME (load), OPNUM (load)));
}

static void pto_store (ir_node *store, pto_env_t *pto_env)
{
  /* perform store */
  DBGPRINT (0, (stdout, "%s (%s[%li])\n", __FUNCTION__,
                OPNAME (store), OPNUM (store)));
}

static void pto_method (ir_node *call, pto_env_t *pto_env)
{
  DBGPRINT (0, (stdout, "%s:%i (%s[%li])\n",
                __FUNCTION__, __LINE__, OPNAME (call), OPNUM (call)));

  callEd_info_t *callEd_info = ecg_get_callEd_info (call);

  int i = 0;
  while (NULL != callEd_info) {
    DBGPRINT (0, (stdout, "%s:%i (%s[%li]), graph %i\n",
                  __FUNCTION__, __LINE__, OPNAME (call), OPNUM (call), i ++));

    pto_call (callEd_info->callEd, call, pto_env);

    callEd_info = callEd_info->prev;
  }
}


static void pto_call (ir_graph *graph, ir_node *call, pto_env_t *pto_env)
{
  /* perform call */
  DBGPRINT (0, (stdout, "%s:%i (%s[%li])\n",
                __FUNCTION__, __LINE__, OPNAME (call), OPNUM (call)));

  entity *ent = get_irg_entity (graph);

  const char *ent_name = (char*) get_entity_name (ent);
  const char *own_name = (char*) get_type_name (get_entity_owner (ent));

  if (! get_irg_is_mem_visited (graph)) {
    graph_info_t *ginfo = ecg_get_info (graph);

    /* Save CTX */
    int ctx_idx = find_ctx_idx (call, ginfo, get_curr_ctx ());
    /* ctx_info_t *call_ctx = find_ctx (call, ginfo, get_curr_ctx ()); */
    ctx_info_t *call_ctx = get_ctx (ginfo, ctx_idx);
    ctx_info_t *old_ctx = set_curr_ctx (call_ctx);
    DBGPRINT (1, (stdout, "%s>CTX: ", -- spaces));
    DBGEXE (1, ecg_print_ctx (call_ctx, stdout));

    /* Todo: Compute Arguments */

    /* Visit Graph */
    pto_graph (graph);

    /* Initialise Alloc Names */
    pto_init_allocs (ginfo, ctx_idx);

    /* Restore CTX */
    set_curr_ctx (old_ctx);

    DBGPRINT (1, (stdout, "%s<CTX: ", spaces ++));
    DBGEXE (1, ecg_print_ctx (call_ctx, stdout));

    /* Don't need to reset alloc names unless we handle recursion here  */


    /* Get Return Value from Graph */
  } else {
    DBGPRINT (0, (stdout, "%s: recursion into \"%s.%s\"\n",
                  __FUNCTION__, own_name, ent_name));
  }

  /* Todo: Set 'Unknown' Value as Return Value when the graph is not
     known */
}

static void pto_raise (ir_node *raise, pto_env_t *pto_env)
{
  /* perform raise */
  DBGPRINT (0, (stdout, "%s (%s[%li])\n", __FUNCTION__,
                OPNAME (raise), OPNUM (raise)));
}

static void pto_end_block (ir_node *end_block, pto_env_t *pto_env)
{
  /* perform end block */
  DBGPRINT (0, (stdout, "%s (%s[%li])\n", __FUNCTION__,
                OPNAME (end_block), OPNUM (end_block)));
}

/* Perform the appropriate action on the given node */
static void pto_node_node (ir_node *node, pto_env_t *pto_env)
{
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

  pto_node_node (node, pto_env);
}

/* Perform a single pass over the given graph */
static void pto_graph_pass (ir_graph *graph, void *pto_env)
{
  entity *ent = get_irg_entity (graph);
  const char *ent_name = (char*) get_entity_name (ent);
  const char *own_name = (char*) get_type_name (get_entity_owner (ent));
  HERE3 ("start", own_name, ent_name);

  irg_walk_mem (graph, pto_node_pre, pto_node_post, pto_env);

  HERE3 ("end  ", own_name, ent_name);
}


/* Main loop: Initialise and iterate over the given graph */
static void pto_graph (ir_graph *graph)
{
  pto_env_t *pto_env = xmalloc (sizeof (pto_env_t));
  HERE ("start");

  pto_init_graph (graph);

  /* todo (here): iterate, obey 'changed' attribute */
  pto_graph_pass (graph, pto_env);

  memset (pto_env, 0x00, sizeof (pto_env_t));
  free (pto_env);
  HERE ("end");
}

/* "Fake" the arguments to the main method */
static void fake_main_args (ir_graph *graph)
{
  HERE ("start");
  /* todo: fake the arguments to the main method */

  HERE ("end");
}


/* ===================================================
   Exported Implementation:
   =================================================== */
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

  set_irn_link (alloc, (void*) alloc_pto);
}

/*Get the current PTO value for the given alloc node */
pto_t *get_alloc_pto (ir_node *alloc)
{
  alloc_pto_t *alloc_pto = (alloc_pto_t*) get_irn_link (alloc);

  assert (iro_Alloc == get_irn_opcode (alloc));

  return (alloc_pto -> curr_pto);
}

/* Initialise the module (should be moved to pto_init) */
void pto_init ()
{
  HERE ("start");
  ecg_init (1);

  /* todo: initialise globals etc */
  pto_init_type_names ();

  spaces = (char*) xmalloc (512 * sizeof (char));
  memset (spaces, ' ', 512);
  spaces += 511;
  *spaces = '\0';

  /* initialise for the CTX-sensitive ecg-traversal */
  set_curr_ctx (get_main_ctx ());
  HERE ("end");
}

void pto_run (int dbg_lvl)
{
  HERE ("start");
  set_dbg_lvl (dbg_lvl);

  ir_graph *graph = get_irp_main_irg ();
  fake_main_args (graph);

  /* todo: clear entity/type links */

  DBGPRINT (0, (stdout, "START PTO\n"));
  DBGPRINT (0, (stdout, "START GRAPH (0x%08x) of \"%s.%s\"\n",
                (int) graph,
                get_type_name (get_entity_owner (get_irg_entity (graph))),
                get_entity_name (get_irg_entity (graph))));

  /* do we need some kind of environment here? */
  pto_graph (graph);

  DBGPRINT (0, (stdout, "END   PTO\n"));
  HERE ("end");
}

void pto_cleanup ()
{
  HERE ("start");
  /* todo: clean up our own mess */
  /* memset (spaces, 0x00, 512); */
  /* free (spaces); */

  /* clean up ecg infos */
  ecg_cleanup ();
  HERE ("end");
}


/*
  $Log$
  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
