/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto_util.c
   Purpose:     Utilitites for PTO
   Author:      Florian
   Modified by:
   Created:     Sat Nov 13 19:35:27 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2004 Universität Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

/*
 pto_util: Utilitites for PTO
*/

# include "pto_util.h"

# include "irnode_t.h"
# include "irgwalk.h"
# include "xmalloc.h"

# include "pto_debug.h"
# include "gnu_ext.h"

/* Local Defines: */
# ifndef TRUE
#  define TRUE 1
#  define FALSE 0
# endif /* not defined TRUE */

/* Local Data Types: */
/* Environment for find_irg_args */
typedef struct find_irg_args_env {
  ir_node **args;
  ir_node *arg;
} find_irg_args_env_t;


/* Local Variables: */

/* Local Prototypes: */

/* ===================================================
   Local Implementation:
   =================================================== */
/* Helper for find_irg_args */
static void find_irg_arg (ir_node *node, void *env)
{
  find_irg_args_env_t *arg_env = (find_irg_args_env_t*) env;

  if (iro_Proj == get_irn_opcode (node)) {
    if (arg_env->arg == get_Proj_pred (node)) {
      long n = get_Proj_proj (node);

      assert (! arg_env->args [n]);

      arg_env->args [n] = node;
    }
  }
}

/* ===================================================
   Exported Implementation:
   =================================================== */
/* Find the arguments of a graph. For a method that has n args, the
  result array has 'n+1' entries, the last of which is written NULL.
  Note that not all entries in [0..n-1] will be populated all the time.
*/
ir_node **find_irg_args (ir_graph *graph)
{
  type *tp = get_entity_type (get_irg_entity (graph));
  const int n_args = get_method_n_params (tp);
  ir_node **args = xcalloc (n_args + 1, sizeof (ir_node*));
  ir_node *arg = get_irg_args (graph);
  find_irg_args_env_t *arg_env;

  arg_env = (find_irg_args_env_t*) xmalloc (sizeof (find_irg_args_env_t));

  arg_env->args = args;
  arg_env->arg  = arg;

  {
    ir_graph *save = get_current_ir_graph ();

    set_current_ir_graph (graph);
    irg_walk (get_irg_end (graph), find_irg_arg, NULL, arg_env);
    set_current_ir_graph (save);
  }

   memset (arg_env, 0x00, sizeof (find_irg_args_env_t));
   free (arg_env);

  args [n_args] = NULL;

  return (args);
}

/* Get the entity of a ptr */
entity *get_ptr_ent (ir_node *ptr)
{
  entity *ent = NULL;
  const opcode ptr_op = get_irn_opcode (ptr);
  switch (ptr_op) {
  case (iro_Cast): {
    ent = get_ptr_ent (get_Cast_op (ptr));
  } break;
  case (iro_Sel): {
    ent = get_Sel_entity (ptr);
  } break;

  case (iro_SymConst): {
    ent = get_SymConst_entity (ptr);
  } break;

  default: {
    fprintf (stderr, "%s: no ent for ptr=%s[%ld]\n",
             __FUNCTION__,
             get_op_name (get_irn_op (ptr)),
             get_irn_node_nr (ptr));
    assert (0);
  }
  }

  return (ent);
}

/* Check whether the load of the given ptr is a dummy */
int is_dummy_load_ptr (ir_node *ptr)
{
  const opcode ptr_op = get_irn_opcode (ptr);

  switch (ptr_op) {
  case (iro_Cast): {
    return (is_dummy_load_ptr (get_Cast_op (ptr)));
  } break;
  case (iro_Sel):
  case (iro_SymConst): {
    return (FALSE);
  } break;

  default: {
    return (TRUE);
  }
  }
}


/*
  $Log$
  Revision 1.16  2005/01/14 14:13:32  liekweg
  fix gnu extension

  Revision 1.15  2005/01/10 17:26:34  liekweg
  fixup printfs, don't put environments on the stack

  Revision 1.14  2004/12/23 15:47:09  beck
  removed uneeded allocations
  used new xcalloc

  Revision 1.13  2004/12/22 14:43:14  beck
  made allocations C-like

  Revision 1.12  2004/12/21 15:53:12  beck
  removed GNUC constructs

  Revision 1.11  2004/12/20 17:34:35  liekweg
  fix recursion handling

  Revision 1.10  2004/12/06 12:55:06  liekweg
  actually iterate

  Revision 1.9  2004/12/02 16:17:51  beck
  fixed config.h include

  Revision 1.8  2004/11/26 15:59:14  liekweg
  recognize dummy loads

  Revision 1.7  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.6  2004/11/18 16:37:07  liekweg
  rewrite


*/
