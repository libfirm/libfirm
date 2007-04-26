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
 * @brief    Load/Store Transfer Functions
 * @author   Florian
 * @date     Fri Nov 26 17:29:49 CET 2004
 * @version  $Id$
 */
#ifndef FIRM_ANA2_PTO_MOD_H
#define FIRM_ANA2_PTO_MOD_H

#include "irnode.h"
#include "entity.h"
#include "pto_comp.h"

/* ===================================================
   Global Defines:
   =================================================== */

/* ===================================================
 Global Data Types:
 =================================================== */

/* ===================================================
   Global Prototypes:
   =================================================== */
/* Perform the given store; return nonzero iff any involved values change */
int mod_store (ir_node*, ir_entity*, pto_t*, pto_t*);

/* Perform the given load; return nonzero iff any involved values change */
int mod_load  (ir_node*, ir_entity*, pto_t*);

/* ===================================================
   Global Variables:
   =================================================== */


#endif /* not defined _PTO_MOD_ */



/*
  $Log$
  Revision 1.2  2006/12/13 19:46:47  beck
  rename type entity into ir_entity

  Revision 1.1  2004/11/30 14:47:54  liekweg
  fix initialisation; do correct iteration


*/
