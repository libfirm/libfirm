/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/pto.c
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

# include "pto_util.h"

void pto_init (void);
void pto_run (int);
void pto_cleanup (void);

# ifndef TRUE
#  define TRUE 1
#  define FALSE 0
# endif /* not defined TRUE */

void set_pto (ir_node*, pto_t*);
int get_pto_verbose (void);
void set_pto_verbose (int);

# endif /* not defined _PTO_H_ */


/*
 * $Log$
 * Revision 1.4  2004/11/04 14:58:38  liekweg
 * expanded pto, added initialisation, added debugging printing
 *
 * Revision 1.3  2004/10/25 11:59:45  liekweg
 * Copy Only works
 *
 * Revision 1.2  2004/10/21 11:09:37  liekweg
 * Moved memwalk stuf into irmemwalk
 * Moved lset stuff into lset
 * Moved typalise stuff into typalise
 *
 * Revision 1.1  2004/10/20 14:59:42  liekweg
 * Added ana2, added ecg and pto
 *
 */
