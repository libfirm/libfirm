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

/* ===================================================
   Global Defines:
   =================================================== */
# define DBGPRINT(lvl, args) if (get_dbg_lvl () > lvl) { fprintf args; }
# define DBGEXE(lvl, cmd) if (get_dbg_lvl () > lvl) {cmd;}
# define OPNAME(node) get_op_name (get_irn_op (node))
# define OPNUM(node) get_irn_node_nr (node)
# define HERE(msg)  fprintf (stdout, "%s:%i: %s\n", __FUNCTION__, __LINE__, msg)
# define HERE2(msg1, msg2)  fprintf (stdout, "%s:%i: %s %s\n", __FUNCTION__, __LINE__, msg1, msg2)
# define HERE3(msg1, msg2, msg3)  fprintf (stdout, "%s:%i: %s %s %s\n", __FUNCTION__, __LINE__, msg1, msg2, msg3)

/* ===================================================
 Global Data Types:
 =================================================== */

/* ===================================================
 Global Data Prototypes:
 =================================================== */
int get_dbg_lvl (void);
void set_dbg_lvl (int);

/* ===================================================
   Global Variables:
   =================================================== */

# endif /* not defined _PTO_DEBUG_ */



/*
  $Log$
  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
