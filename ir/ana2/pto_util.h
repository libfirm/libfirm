/* -*- c -*- */

/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief    Utilitites for PTO
 * @author   Florian
 * @date     Sat Nov 13 19:35:27 CET 2004
 * @version  $Id$
 */
# ifndef FIRM_ANA2_PTO_UTIL_H
# define FIRM_ANA2_PTO_UTIL_H

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
/** Get the entity of a ptr. */
ir_entity *get_ptr_ent (ir_node *ptr);

/**
 * Find the arguments of a graph. For a method that has n args, the
 * result array has 'n+1' entries, the last of which is written NULL.
 *
 * @param irg  The IR graph
 */
ir_node **find_irg_args (ir_graph *irg);

/* Check whether the load of the given ptr is a dummy */
int is_dummy_load_ptr (ir_node*);

/* ===================================================
   Global Variables:
   =================================================== */


# endif /* not defined _PTO_UTIL_ */



/*
  $Log$
  Revision 1.8  2006/12/13 19:46:47  beck
  rename type entity into ir_entity

  Revision 1.7  2005/06/17 17:43:52  beck
  added doxygen docu

  Revision 1.6  2004/11/26 15:59:14  liekweg
  recognize dummy loads

  Revision 1.5  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.4  2004/11/18 16:37:07  liekweg
  rewrite


*/
