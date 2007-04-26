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
 * @brief    Main Implementation of PTO
 * @author   Florian
 * @date     Sat Nov 13 19:35:27 CET 2004
 * @version  $Id$
 */
# ifndef FIRM_ANA2_PTO_COMP_H
# define FIRM_ANA2_PTO_COMP_H

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


# endif



/*
  $Log$
  Revision 1.3  2004/12/20 17:34:35  liekweg
  fix recursion handling

  Revision 1.2  2004/11/24 14:53:55  liekweg
  Bugfixes

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
