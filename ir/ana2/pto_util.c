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

# ifdef HAVE_CONFIG_H
#  include <config.h>
# endif

/*
 pto_util: Utilitites for PTO
*/

# include "pto_util.h"

# include "irnode.h"
# include "xmalloc.h"

# include "pto_debug.h"

/* Local Defines: */

/* Local Data Types: */

/* Local Variables: */

/* Local Prototypes: */

/* ===================================================
   Local Implementation:
   =================================================== */


/* ===================================================
   Exported Implementation:
   =================================================== */
/* Get the entity of a ptr */
entity *get_ptr_ent (ir_node *ptr)
{
  entity *ent = NULL;
  const opcode ptr_op = get_irn_opcode (ptr);
  switch (ptr_op) {
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




/*
  $Log$
  Revision 1.6  2004/11/18 16:37:07  liekweg
  rewrite


*/
