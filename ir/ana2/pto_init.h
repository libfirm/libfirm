/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/pto_init.c
 * Purpose:     Pto Initialization
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# ifndef _PTO_INIT_H_
# define _PTO_INIT_H_

void pto_init_node (ir_node*);

# endif /* not defined _PTO_INIT_H_ */


/*
 * $Log$
 * Revision 1.1  2004/11/04 14:58:59  liekweg
 * added initialisation
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
