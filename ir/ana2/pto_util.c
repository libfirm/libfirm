/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/pto_util.c
 * Purpose:     Pto Utilities
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

# include "pto_util.h"

# include <malloc.h>

# include "irnode.h"
# include "irgwalk.h"
# include "xmalloc.h"

/*
  Environment for find_irg_args
*/
typedef struct find_irg_args_env {
  ir_node **args;
  ir_node *arg;
} find_irg_args_env_t;

/*
  Helper for find_irg_args
*/
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

/*
  Find the arguments of a graph. For a method that has n args, the
  result array has 'n+1' entries, the last of which is written NULL.
*/
ir_node **find_irg_args (ir_graph *graph)
{
  type *tp = get_entity_type (get_irg_entity (graph));
  const int n_args = get_method_n_params (tp);
  ir_node **args = (ir_node**) xmalloc (sizeof (ir_node*) * (n_args+1));
  ir_node *arg = get_irg_args (graph);
  find_irg_args_env_t *arg_env =
    (find_irg_args_env_t*) xmalloc (sizeof (find_irg_args_env_t));
  /* int i; */

  arg_env->args = args;
  arg_env->arg  = arg;

  /* or use get_irg_end ?!? */
  {
    ir_graph *save = get_current_ir_graph ();
    set_current_ir_graph (graph);
    irg_walk (get_irg_end (graph), find_irg_arg, NULL, arg_env);
    set_current_ir_graph (save);
  }

  free (arg_env);

  args [n_args] = NULL;

  return (args);
}



/*
  $Log$
  Revision 1.1  2004/10/22 15:10:51  liekweg
  moved utils to pto_util


 */
