/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto_init.h
   Purpose:     ...
   Author:      Florian
   Modified by:
   Created:     Sat Nov 13 19:35:27 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2004 Universität Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/


# ifndef _PTO_INIT_
# define _PTO_INIT_

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


# endif /* not defined _PTO_INIT_ */



/*
  $Log$
  Revision 1.4  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.3  2004/11/20 21:21:56  liekweg
  Finalise initialisation

  Revision 1.2  2004/11/18 16:37:07  liekweg
  rewrite


*/
