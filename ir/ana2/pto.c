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

# include "entity.h"

# include "irnode_t.h"
# include "irprog_t.h"

/* # include "eset.h" */
# include "irgraph.h"
# include "irgwalk.h"
# include "irgmod.h"
# include "irvrfy.h"
# include "trvrfy.h"
# include "xmalloc.h"

# include "irmemwalk.h"

# ifndef TRUE
#  define TRUE 1
#  define FALSE 0
# endif /* not defined TRUE */

typedef struct pto_str
{
  int *dummy;
} pto_t;

/*
  Local Protos
*/
static void pto_node (ir_node*, void*);

/*
   Le Flag
*/
static int verbose = TRUE;

/*
Create a new pto value
*/
static pto_t *new_pto (void)
{
  pto_t *pto = (pto_t*) xmalloc (sizeof (pto_t));

  return (pto);
}


/*
   Get the pto infos of a node
*/
static pto_t *get_pto (ir_node *node)
{
  return ((pto_t*) node->link);
}

static pto_t *compute_pto (ir_node *node, void *env)
{
  pto_t *node_pto = get_pto (node);

  if (NULL == node_pto) {
    pto_node (node, env);

    node_pto = get_pto (node);
  }

  assert (node_pto);

  return (node_pto);
}

static void set_pto (ir_node *node, pto_t *ptrs)
{
  node->link = (void*) ptrs;
}

/*
  Take care of a single proj node.  Basically a multiplex/dispatch for the
  different node types that a proj can have as a predecessor.
*/
static void pto_node_proj (ir_node *proj, void *env)
{
  /*
    pto for proj({proj(start),load,call,alloc,raise?,...}) node

    so far:
    whatever it is we have in front of us, it's analyzed.
    todo:
    we just pick up the pointer.
  */
  ir_node *in = get_Proj_pred (proj);
  const opcode in_op = get_irn_opcode (in);
  const long proj_proj = get_Proj_proj (proj);

  fprintf (stdout, "%s: --> Proj[%li] (%s)\n",
           __FUNCTION__,
           get_irn_node_nr (proj),
           get_op_name (get_irn_op (in)));

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
      set_pto (proj, compute_pto (in, env));
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

    assert (iro_Start == in_in_op && "proj (proj (X)) but not X != Start");
    /* proj(proj(start)) - make sure the arguments are initialised */
    fprintf (stdout, "pto (proj(%li)) = 0x%08x\n",
             get_irn_node_nr (proj),
             (int) get_pto (proj));
    set_pto (proj, NULL);
  } break;

  case (iro_Cast): {
    /* make sure the input has been analysed */
    pto_t *cast_pto = compute_pto (in, env);
    set_pto (proj, cast_pto);

  } break;

  default: {
    fprintf (stderr, "Opcode %s of Node %ld not handled\n",
             get_op_name (get_irn_op (in)),
             get_irn_node_nr (in));

    assert (0 && "something not handled");
  }
  }

  if (verbose) {
    fprintf (stdout, "---> Proj (%s)\n", get_op_name (get_irn_op (in)));
  }

}

static void pto_node_load (ir_node *load, void *env)
{
  /*
    pto for load node

    so far:
    load.ptr analysed
    todo:
    look up
  */

  /* right now, just copy something */
  ir_node *ptr = get_Load_ptr (load);
  const opcode op = get_irn_opcode (ptr);
  entity *ent = NULL;

  if (iro_Sel == op) {
    ent = get_Sel_entity (ptr);
  } else if (iro_SymConst == op) {
    ent = get_SymConst_entity (ptr);
  } else if (iro_Proj == op) {
    /* then it's an unused Load(this) (or whatever) and we don't need to look at it */
    ent = NULL;
  } else if (iro_Cast == op) {
    /* then it's  whatever and we don't know where to look at */
    ent = NULL;
  } else {
    fprintf (stdout, "%s: %s[%li] not handled\n",
             __FUNCTION__,
             get_op_name (get_irn_op (ptr)),
             get_irn_node_nr (ptr));
    assert (0 && "told ya");
  }

  if (NULL != ent) {
    pto_t *ptr_pto = compute_pto (ptr, env);


    /* todo: use 'ent' to look up */
    set_pto (load, ptr_pto);      /* for now */
  }
}

static void pto_node_store (ir_node *store, void *env)
{
  /*
    pto for store node

    so far:
    ptr analysed
    todo:
    update
  */

  /* right now, just copy something */
  ir_node *ptr = get_Store_ptr (store);
  const opcode op = get_irn_opcode (ptr);
  entity *ent = NULL;

  if (iro_Sel == op) {
    ent = get_Sel_entity (ptr);
  } else if (iro_SymConst == op) {
    ent = get_SymConst_entity (ptr);
  } else {
    fprintf (stdout, "%s: opcode %s not handled\n",
             __FUNCTION__, get_op_name (get_irn_op (ptr)));
    assert (0 && "told ya");
  }

  pto_t *ptr_pto = compute_pto (ptr, env);
  /* todo: use 'ent' to look up */
  fprintf (stdout, "%s: pto (store[%li]) = 0x%08d\n",
           __FUNCTION__, get_irn_node_nr (ptr), (int) ptr_pto);
}

static void pto_node_alloc (ir_node *alloc, void *env)
{
  /*
    pto for alloc node

    so far:
    nothing
    todo:
    invent new name
 */

  pto_t *alloc_pto = (pto_t*) xmalloc (sizeof (pto_t));

  set_pto (alloc, alloc_pto);
}

static void pto_node_free (ir_node *free, void *env)
{
  /*
    pto for free node

    so far:
    ptr analysed
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

    so far:
    ptr analysed
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

    so far:
    input selected
    todo:
    just copy
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

# ifdef UNSINN
static void pto_node_call (ir_node *call, void *env)
{
  int i;
  int n_ins = get_irn_arity (call);
  ir_node *in = NULL;

  for (i = 0; i < n_ins; i ++) {
    in = get_irn_n (call, i);

    if (mode_P == get_irn_mode (in)) {
      fprintf (stdout, "--> IN CALL Node (%ld) (%s)\n",
               get_irn_node_nr (in),
               get_op_name (get_irn_op (in)));
    }
  }
}
# endif /* defined UNSINN */

static void pto_node_ret (ir_node *ret, void *env)
{
  /*
    pto for return node

    so far:
    input should be availlable
    todo:
    just copy
  */

  if (mode_P == get_irn_mode (ret)) {
    ir_node *ret_in = get_Return_res_arr (ret) [0];

    pto_t *ret_pto = compute_pto (ret_in, env);

    set_pto (ret, ret_pto);
  } else {
    /* set_pto (ret, NULL); */
    /* nothing! */
  }
}

static void pto_node_phi (ir_node *phi, void *env)
{
  /*
    pto for phi node

    so far:
    all ins present (or?)
    todo:

  */

  int i;
  int n_ins = get_irn_arity (phi);
  ir_node *in = NULL;

  if (mode_P != get_irn_mode (phi)) {
    return;
  }

  for (i = 0; i < n_ins; i ++) {
    in = get_irn_n (phi, i);

    pto_t *in_pto = compute_pto (in, env);

    fprintf (stdout, "--> IN PHI Node (%ld) (%s) (pto = 0x%08x)\n",
             get_irn_node_nr (in),
             get_op_name (get_irn_op (in)),
             (int) in_pto);

    /* must 'merge', but just set something now: */
    set_pto (phi, in_pto);
  }

}

static void pto_node_cnst (ir_node *cnst, void *env)
{
  /*
    pto for const node

    so far:

    todo:
    if this represents something nameable, name it
  */
  pto_t *cnst_pto = new_pto ();


  set_pto (cnst, cnst_pto);
}

static void pto_node_sym_cnst (ir_node *sym_cnst, void *env)
{
  /*
    pto for const node

    so far:

    todo:
    if this represents something nameable, name it
  */
  pto_t *sym_cnst_pto = new_pto ();


  set_pto (sym_cnst, sym_cnst_pto);
}


/*
  Take care of a single node.  Basically a multiplex/dispatch for the
  different node types.
*/
static void pto_node (ir_node *node, void *env)
{
  const opcode op = get_irn_opcode (node);

  if (verbose) {
    fprintf (stdout, "-> Node (%ld) (%s)\n",
             get_irn_node_nr (node),
             get_op_name (get_irn_op (node)));
  }

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
    assert (0 && "calls must be handled in main loop");
    /* pto_node_call (node, env); */
    /* also, we should at best look at the ProjV hanging off a call */

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
  case (iro_Div):
  case (iro_Quot):
  case (iro_Mod):
  case (iro_DivMod):
    set_pto (node, NULL);
    break;
  default: {
    fprintf (stdout, "%s: not handled: node[%li].op = %s\n",
             __FUNCTION__,
             get_irn_node_nr (node),
             get_op_name (get_irn_op (node)));
    assert (0 && "something not handled");
  }
  }
}

static void pto_node_post (ir_node *node, void *env)
{
  const opcode op = get_irn_opcode (node);


  if (iro_Call == op) {
    ir_node *call = node;
    entity *ent = NULL;
    type *ent_tp = NULL;
    ir_graph *graph = NULL;
    ir_node *ptr = get_Call_ptr (call);

    if (verbose) {
      fprintf (stdout, "POST MEM Call Node (%ld)\n",
               get_irn_node_nr (call));
    }

    if (iro_Sel == get_irn_opcode (ptr)) {
      ent = get_Sel_entity (ptr);
    } else if (iro_SymConst == get_irn_opcode (ptr)) {
      if (get_SymConst_kind(ptr) == symconst_addr_ent) {
        ent = get_SymConst_entity (ptr);
      }
    }

    assert (NULL != ent && "No ent to call");

    /* Todo:  Iterate over all graphs in 'get_implementing_graphs' */
    graph = get_entity_irg (ent);
    if (NULL != graph) {
      const char *ent_name = (char*) get_entity_name (ent);
      const char *own_name = (char*) get_type_name (get_entity_owner (ent));

      if (! get_irg_is_mem_visited (graph)) {
        if (verbose) {
          fprintf (stdout, " -> visit  graph (0x%08x) of \"%s.%s\"\n",
                   (int) graph,
                   own_name,
                   ent_name);
        }

        if (TRUE) {
          ent_tp = get_entity_type (ent);
          const long n_args =
            get_method_n_params (ent_tp);
          ir_node **args = find_irg_args (graph);
          int i;

          const int n_call_args = get_irn_arity (call);

          assert (n_call_args == n_call_args);

          /* Set args for graph */

          for (i = 0; i < n_args; i ++) {
            if (NULL != args [i]) {
              if (mode_P == get_irn_mode (args [i])) {
                pto_t *arg_pto = compute_pto (get_irn_n (call, i+1), env);
                /* off-by-one because of ProjT bd */
                set_pto (args [i], arg_pto);
              } else {
                /* set_pto (args [i], NULL); */
                /* nothing */
              }
            }
          }
          free (args);
        }

        irg_walk_mem (graph, NULL, pto_node_post, NULL);

        /* maybe get result from called graph */
        if (0 != get_method_n_ress (ent_tp)) {
          type *ent_ret_tp = get_method_res_type (ent_tp, 0);

          if (mode_P == get_type_mode (ent_ret_tp)) {
            pto_t *res_pto = get_pto (get_irg_end (graph));
            set_pto (call, res_pto);
          }
        } else {
          fprintf (stdout, "%s:%i: no return value for \"%s.%s\"\n",
                   __FUNCTION__, __LINE__,
                   get_type_name (get_entity_owner (ent)),
                   get_entity_name (ent));
        }
      }
    } else {
      if (verbose) {
        fprintf (stdout, "%s:%i: Warning: no graph for ent \"%s.%s\"\n",
                 __FUNCTION__, __LINE__,
                 get_type_name (get_entity_owner (ent)),
                 get_entity_name (ent));
      }
    }
  } else {
    /* anyway: */
    pto_node (node, env);
  }
}

/*
  Run Pto
*/
void pto_run (int do_verbose)
{
  /* int i; */
  verbose = do_verbose;

  if (verbose) {
    fprintf (stdout, "START PTO\n");
  }

  ir_graph *graph = get_irp_main_irg ();

  if (verbose) {
    fprintf (stdout, "START GRAPH (0x%08x) of \"%s.%s\"\n",
             (int) graph,
             get_type_name (get_entity_owner (get_irg_entity (graph))),
             get_entity_name (get_irg_entity (graph)));
  }

  if (TRUE) {
    pto_t *main_pto = new_pto ();
    const long n_args =
      get_method_n_params (get_entity_type (get_irg_entity (graph)));
    ir_node **args = find_irg_args (graph);
    int i;

    /*
      Todo:  Set args for MAIN graph!!!
    */

    for (i = 0; i < n_args; i ++) {
      fprintf (stdout, "arg [%i] = %ld\n", i,
               args [i] ? get_irn_node_nr (args [i]) : -1);
    }
    set_pto (args [1], main_pto);

    free (args);
  }

  irg_walk_mem (graph, NULL, pto_node_post, NULL);

  if (verbose) {
    fprintf (stdout, "END   GRAPH (0x%08x)\n", (int) graph);
  }

  if (verbose) {
    fprintf (stdout, "END   PTO\n");
  }
}

/*
  Clean Up
*/
void pto_cleanup ()
{
  /* TODO */
}



/*
 * $Log$
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
