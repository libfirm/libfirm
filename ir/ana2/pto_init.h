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
 * @brief     ...
 * @author    Florian
 * @date      Sat Nov 13 19:35:27 CET 2004
 * @version   $Id$
 */
# ifndef FIRM_ANA2_PTO_INIT_H
# define FIRM_ANA2_PTO_INIT_H

# include "irgraph.h"
# include "ecg.h"

/* ===================================================
   Global Defines:
   =================================================== */

/* ===================================================
 Global Data Types:
 =================================================== */

/* ===================================================
   Global Prototypes:
   =================================================== */
/* "Fake" the arguments to the main method */
void fake_main_args (ir_graph*);

/* Initialise the Init module */
void pto_init_init (void);

/* Cleanup the Init module */
void pto_init_cleanup (void);

/* Initialise the Names of the Types/Entities */
void pto_init_type_names (void);

/* Initialise the given graph */
void pto_init_graph (ir_graph*);

/* Reset the given graph for a new pass run */
void pto_reset_graph_pto (ir_graph*, int);

/* ===================================================
   Global Variables:
   =================================================== */


# endif



/*
  $Log$
  Revision 1.4  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.3  2004/11/20 21:21:56  liekweg
  Finalise initialisation

  Revision 1.2  2004/11/18 16:37:07  liekweg
  rewrite


*/
