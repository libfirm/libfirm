/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/pto_util.c
 * Purpose:     Pto Utilities
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# ifndef _PTO_UTIL_H_
# define _PTO_UTIL_H_

# include "irgraph.h"

/*
  Find the arguments of a graph. For a method that has n args, the
  result array has 'n+1' entries, the last of which is written NULL.
*/
ir_node **find_irg_args (ir_graph*);

# endif /* not defined _PTO_UTIL_H_ */


/*
  $Log$
  Revision 1.1  2004/10/22 15:10:51  liekweg
  moved utils to pto_util


 */
