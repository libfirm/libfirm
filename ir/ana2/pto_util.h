/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto_util.h
   Purpose:     Utilitites for PTO
   Author:      Florian
   Modified by:
   Created:     Sat Nov 13 19:35:27 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2004 Universität Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/


# ifndef _PTO_UTIL_
# define _PTO_UTIL_

# include "irnode.h"
# include "entity.h"

/* ===================================================
   Global Defines:
   =================================================== */

/* ===================================================
 Global Data Types:
 =================================================== */

/* ===================================================
   Global Prototypes:
   =================================================== */
/* Get the entity of a ptr */
entity *get_ptr_ent (ir_node*);

/* Find the arguments of a graph. For a method that has n args, the
  result array has 'n+1' entries, the last of which is written NULL. */
ir_node **find_irg_args (ir_graph*);

/* Check whether the load of the given ptr is a dummy */
int is_dummy_load_ptr (ir_node*);

/* ===================================================
   Global Variables:
   =================================================== */


# endif /* not defined _PTO_UTIL_ */



/*
  $Log$
  Revision 1.6  2004/11/26 15:59:14  liekweg
  recognize dummy loads

  Revision 1.5  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.4  2004/11/18 16:37:07  liekweg
  rewrite


*/
