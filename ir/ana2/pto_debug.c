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
#  include <config.h>
# endif

/*
 pto_debug: Useful Macros for Debugging
*/

# include "pto_debug.h"

# include "irnode.h"
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
  dbg_lvl = lvl;
}



/*
  $Log$
  Revision 1.2  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
