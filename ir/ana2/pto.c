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

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

/*
 pto: Entry to PTO
*/

#ifdef HAVE_STRING_H
# include <string.h>
#endif

# include "pto.h"

# include "irnode_t.h"
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
static void pto_init_graph_wrapper (graph_info_t *ginfo, void *_unused)
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
}

void pto_run (void)
{
  ir_graph *save;
  ir_graph *graph = get_irp_main_irg ();

  pto_reset_graph_pto (graph, 0);
  fake_main_args (graph);

  DBGPRINT (1, (stdout, "START PTO\n"));
  DBGPRINT (1, (stdout, "START GRAPH (0x%08x) of \"%s.%s\"\n",
                (int) graph,
                get_type_name (get_entity_owner (get_irg_entity (graph))),
                get_entity_name (get_irg_entity (graph))));

  /* we need some kind of environment here: NULL */
  save = get_current_ir_graph ();
  pto_graph (graph, 0, NULL);
  set_current_ir_graph (save);

  DBGPRINT (1, (stdout, "END   PTO\n"));
}

/* Dump all interesting stuff to a bunch of files */
void pto_dump (void)
{
  pto_dump_names ("names.dot");
}

void pto_cleanup (void)
{
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
}


/*
  $Log$
  Revision 1.15  2004/12/21 14:26:53  beck
  removed C99 constructs

  Revision 1.14  2004/12/20 17:41:14  liekweg
  __unused -> _unused

  Revision 1.13  2004/12/20 17:34:34  liekweg
  fix recursion handling

  Revision 1.12  2004/12/02 16:17:51  beck
  fixed config.h include

  Revision 1.11  2004/11/30 15:49:27  liekweg
  include 'dump'

  Revision 1.10  2004/11/30 14:46:41  liekweg
  Correctly reset main graph; remove dbugging stuff

  Revision 1.9  2004/11/26 16:01:56  liekweg
  debugging annotations

  Revision 1.8  2004/11/24 14:54:21  liekweg
  Added pto.c as main entry point


*/
