/*
 * Project:     libFIRM
 * File name:   ir/ana/irbackedge_t.h
 * Purpose:     Access function for backedges -- private header.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     7.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _IRBACKEDGE_T_H_
#define _IRBACKEDGE_T_H_

# include <string.h>

static INLINE int * new_backedge_arr(struct obstack *obst, int size) {
  int *res = NEW_ARR_D (int, obst, size);
  memset(res, 0, sizeof(int) * size);
  return res;
}

/* Adapts backedges array to new size.
   Must be called if in array of irnode is changed.  Else
   Segmentation faults might occur.  */
void fix_backedges(struct obstack *obst, ir_node *n);

#endif /* _IRBACKEDGE_T_H_ */
