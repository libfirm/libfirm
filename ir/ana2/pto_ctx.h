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
 * @brief    Manage context-sensitivity markers
 * @author   Florian
 * @date     Sat Nov 13 19:35:27 CET 2004
 * @version  $Id$
 */
# ifndef FIRM_ANA2_PTO_CTX_H
# define FIRM_ANA2_PTO_CTX_H

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
/* Find the appropriate ctx for the given call and the given graph */
/* ctx_info_t *find_ctx (ir_node*, graph_info_t*, ctx_info_t*); */
int find_ctx_idx (ir_node*, graph_info_t*, ctx_info_t*);

/* Get the current ctx */
ctx_info_t *get_curr_ctx (void);

/* Set the current ctx to the given ctx.  Return the old value */
ctx_info_t *set_curr_ctx (ctx_info_t*);

/* Set all alloc names to the right ptos */
void pto_ctx_allocs (graph_info_t*, int);

/* ===================================================
   Global Variables:
   =================================================== */


# endif



/*
  $Log$
  Revision 1.3  2005/01/14 13:37:55  liekweg
  Insert purpose descr

  Revision 1.2  2004/11/20 21:21:35  liekweg
  Add pto_ctx_allocs

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
