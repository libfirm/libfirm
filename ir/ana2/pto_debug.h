/* -*- c -*- */

/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief     Useful Macros for Debugging
 * @author    Florian
 * @date      Sat Nov 13 19:30:21 CET 2004
 * @version   $Id$
 */
# ifndef FIRM_ANA2_PTO_DEBUG_H
# define FIRM_ANA2_PTO_DEBUG_H

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

# endif



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
