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
/* Initialise the Names of the Types/Entities */
void pto_init_type_names (void);

/* Initialise the given graph */
void pto_init_graph (ir_graph*);

/* Set all alloc names to the right ptos */
void pto_init_allocs (graph_info_t*, int);

/* ===================================================
   Global Variables:
   =================================================== */


# endif /* not defined _PTO_INIT_ */



/*
  $Log$
  Revision 1.2  2004/11/18 16:37:07  liekweg
  rewrite


*/
