/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto.c
   Purpose:     Entry to PTO
   Author:      Florian
   Modified by:
   Created:     Tue Nov 23 18:37:09 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2004 Universität Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/

# ifdef HAVE_CONFIG_H
#  include <config.h>
# endif

/*
 pto: Entry to PTO
*/

#  include <string.h>

# include "pto.h"

# include "irnode.h"
# include "irprog.h"
# include "xmalloc.h"

# include "pto_debug.h"
# include "pto_init.h"
# include "pto_name.h"
# include "pto_ctx.h"
# include "ecg.h"

/* Local Defines: */

/* Local Data Types: */

/* Local Variables: */
extern char *spaces;

/* Local Prototypes: */

/* ===================================================
   Local Implementation:
   =================================================== */

/* Helper to pto_init */
static void pto_init_graph_wrapper (graph_info_t *ginfo, void *__unused)
{
  ir_graph *graph = ginfo->graph;

  pto_init_graph (graph);
}

/* ===================================================
   Exported Implementation:
   =================================================== */
/* Initialise the module (not in pto_init.c because it's the entry to pto) */
void pto_init (int lvl)
{
  HERE ("start");
  set_dbg_lvl (lvl);

  ecg_init (1);

  /* Initialise the name module */
  pto_name_init ();

  /* Initialise the init module */
  pto_init_init ();

  /* allocate ctx-sens names for allocs and set ... etc etc */
  pto_init_type_names ();

  /* initialise all graphs with the static names */
  ecg_iterate_graphs (pto_init_graph_wrapper, NULL);

  /* debugging only */
  spaces = (char*) xmalloc (512 * sizeof (char));
  memset (spaces, ' ', 512);
  spaces += 511;
  *spaces = '\0';

  /* initialise for the CTX-sensitive ecg-traversal */
  set_curr_ctx (get_main_ctx ());
  HERE ("end");
}

void pto_run ()
{
  HERE ("start");

  ir_graph *graph = get_irp_main_irg ();
  fake_main_args (graph);

  DBGPRINT (0, (stdout, "START PTO\n"));
  DBGPRINT (0, (stdout, "START GRAPH (0x%08x) of \"%s.%s\"\n",
                (int) graph,
                get_type_name (get_entity_owner (get_irg_entity (graph))),
                get_entity_name (get_irg_entity (graph))));

  /* do we need some kind of environment here? */
  pto_graph (graph, 0);

  DBGPRINT (0, (stdout, "END   PTO\n"));
  HERE ("end");
}

void pto_cleanup ()
{
  HERE ("start");
  /* todo: clean up our own mess */
  spaces -= 511;                /* hope that all changes to spaces are
                                   properly nested */
  memset (spaces, 0x00, 512);
  free (spaces);

  /* Cleanup the name module */
  pto_name_cleanup ();
  /* Cleanup the Init module */
  pto_init_cleanup ();

  /* clean up ecg infos */
  ecg_cleanup ();
  HERE ("end");
}


/*
  $Log$
  Revision 1.9  2004/11/26 16:01:56  liekweg
  debugging annotations

  Revision 1.8  2004/11/24 14:54:21  liekweg
  Added pto.c as main entry point


*/
