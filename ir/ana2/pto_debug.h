/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto_debug.h
   Purpose:     Useful Macros for Debugging
   Author:      Florian
   Modified by:
   Created:     Sat Nov 13 19:30:21 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2004 Universität Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/


# ifndef _PTO_DEBUG_
# define _PTO_DEBUG_

# include "irnode.h"

/* ===================================================
   Global Defines:
   =================================================== */
# define DBGPRINT(lvl, args) if (get_dbg_lvl () > lvl) { fprintf args; }
# define DBGEXE(lvl, cmd) if (get_dbg_lvl () > lvl) {cmd;}
# define OPNAME(node) get_irn_opname(node)
# define OPNUM(node) get_irn_node_nr(node)
# define HERE(msg)  fprintf (stdout, "%s:%i %s\n", __FUNCTION__, __LINE__, msg)
# define HERE2(msg1, msg2)  fprintf (stdout, "%s:%i: %s %s\n", __FUNCTION__, __LINE__, msg1, msg2)
# define HERE3(msg1, msg2, msg3)  fprintf (stdout, "%s:%i: %s %s %s\n", __FUNCTION__, __LINE__, msg1, msg2, msg3)

/* ===================================================
 Global Data Types:
 =================================================== */

/* ===================================================
 Global Prototypes:
 =================================================== */
int get_dbg_lvl (void);
void set_dbg_lvl (int);

void pto_print_pto (ir_node*);

/* ===================================================
   Global Variables:
   =================================================== */

# endif /* not defined _PTO_DEBUG_ */



/*
  $Log$
  Revision 1.5  2005/01/14 13:33:10  liekweg
  Use only public irnode interface

  Revision 1.4  2004/12/21 15:51:07  beck
  simplifyed

  Revision 1.3  2004/12/20 17:34:35  liekweg
  fix recursion handling

  Revision 1.2  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
