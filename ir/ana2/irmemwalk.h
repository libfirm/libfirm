/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/irmemwalk.h
 * Purpose:     walk along memory edges
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# ifndef _IRMEMWALK_H_
# define _IRMEMWALK_H_

# include "irgraph.h"
# include "irgwalk.h"

void irg_walk_mem (ir_graph*, irg_walk_func*, irg_walk_func*, void*);
int get_irg_is_mem_visited (ir_graph*);

# endif /* not defined _IRMEMWALK_H_ */


/*
  $Log$
  Revision 1.1  2004/10/21 11:09:37  liekweg
  Moved memwalk stuf into irmemwalk
  Moved lset stuff into lset
  Moved typalise stuff into typalise


 */
