/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto_debug.c
   Purpose:     Useful Macros for Debugging
   Author:      Florian
   Modified by:
   Created:     Sat Nov 13 19:30:21 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2004 Universität Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/

# ifdef HAVE_CONFIG_H
#  include "config.h"
# endif

/*
 pto_debug: Useful Macros for Debugging
*/

# include "pto_debug.h"
# include "pto_comp.h"
# include "gnu_ext.h"
# include "qset.h"

/* # include "xmalloc.h" */

/* Local Defines: */

/* Local Data Types: */

/* Local Variables: */
static int dbg_lvl = 0;

/* Local Prototypes: */

/* ===================================================
   Local Implementation:
   =================================================== */


/* ===================================================
   Exported Implementation:
   =================================================== */
int get_dbg_lvl ()
{
  return (dbg_lvl);
}

void set_dbg_lvl (int lvl)
{
  /* fprintf (stdout, "%s:%s (): dbg_lvl: %i -> %i\n", */
/*            __FILE__, __FUNCTION__, dbg_lvl, lvl); */
  dbg_lvl = lvl;
}

void pto_print_pto (ir_node *node)
{
  pto_t *pto = get_node_pto (node);

  fprintf (stdout, "pto (%s[%li]) = ", OPNAME (node), OPNUM (node));
  if (NULL != pto) {
    qset_print (pto->values, stdout);
  } else {
    fprintf (stdout, "NULL");
  }
}



/*
  $Log$
  Revision 1.5  2005/02/25 16:47:51  liekweg
  fix GNU stuff

  Revision 1.4  2004/12/20 17:34:35  liekweg
  fix recursion handling

  Revision 1.3  2004/12/02 16:17:51  beck
  fixed config.h include

  Revision 1.2  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
