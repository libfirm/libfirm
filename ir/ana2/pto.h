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
 * @brief   Import all includes needed for PTO/Entry to PTO
 * @author  Florian
 * @date    Sat Nov 13 19:35:27 CET 2004
 * @version $Id$
 */
# ifndef FIRM_ANA2_PTO_H
# define FIRM_ANA2_PTO_H

# include "pto_comp.h"

/* ===================================================
   Global Defines:
   =================================================== */
# define N_INITIAL_OJBS     10

/* ===================================================
 Global Data Types:
 =================================================== */

/* ===================================================
   Global Prototypes:
   =================================================== */
/* Perform PTO on all visible graphs. */
void pto_init (int);
void pto_run (void);
/* Dump all interesting stuff to a bunch of files */
void pto_dump (void);
/* Clean up our mess */
void pto_cleanup (void);

/* ===================================================
   Global Variables:
   =================================================== */


# endif



/*
  $Log$
  Revision 1.7  2004/11/30 15:49:27  liekweg
  include 'dump'

  Revision 1.6  2004/11/24 14:53:55  liekweg
  Bugfixes

  Revision 1.5  2004/11/18 16:37:07  liekweg
  rewrite


*/
