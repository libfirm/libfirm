/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto_comp.h
   Purpose:     Main Implementation of PTO
   Author:      Florian
   Modified by:
   Created:     Sat Nov 13 19:35:27 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2004 Universität Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/


# ifndef _PTO_COMP_
# define _PTO_COMP_

# include "pto.h"
# include "irnode.h"
# include "qset.h"

/* ===================================================
   Global Defines:
   =================================================== */

/* ===================================================
 Global Data Types:
 =================================================== */
typedef struct pto_str {
  qset_t *values;
} pto_t;

typedef struct alloc_pto_str {
  int dummy;
  pto_t **ptos;                 /* all names */
  pto_t *curr_pto;              /* name for current ctx */
} alloc_pto_t;

struct pto_env_str;             /* forward decl only */

/* ===================================================
   Global Prototypes:
   =================================================== */
/* Main loop: Initialise the graph for the given ctx_idx and iterate over it */
void pto_graph (ir_graph*, int, struct pto_env_str*);

/* Set the PTO value for the given node */
void set_node_pto (ir_node*, pto_t*);
/*Get the PTO value for the given non-alloc node */
pto_t *get_node_pto (ir_node*);

/* Set the PTO value for the given alloc node */
void set_alloc_pto (ir_node*, alloc_pto_t*);

/*Get the current PTO value for the given alloc node */
pto_t *get_alloc_pto (ir_node*);


/* ===================================================
   Global Variables:
   =================================================== */


# endif /* not defined _PTO_COMP_ */



/*
  $Log$
  Revision 1.3  2004/12/20 17:34:35  liekweg
  fix recursion handling

  Revision 1.2  2004/11/24 14:53:55  liekweg
  Bugfixes

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
