#ifndef _IRBACKEDGE_T_H_
#define _IRBACKEDGE_T_H_

# include "string.h"

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
