/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana/pto.c
 * Purpose:     Pto
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# ifndef _PTO_H_
# define _PTO_H_

# include "entity.h"

# include "irgraph.h"
# include "irgwalk.h"

void irg_walk_mem (ir_graph*, irg_walk_func*, irg_walk_func*, void*);

int get_irg_is_mem_visited (ir_graph*);

/* ...! */
void pto_test_mem (void);

# endif /* not defined _PTO_H_ */


/*
 * $Log$
 * Revision 1.1  2004/10/20 14:59:42  liekweg
 * Added ana2, added ecg and pto
 *
 */
