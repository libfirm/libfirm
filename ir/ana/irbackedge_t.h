#ifndef _IRBACKEDGE_T_H_
#define _IRBACKEDGE_T_H_

# include "string.h"

static INLINE int * new_backedge_arr(struct obstack *obst, int size) {
  int *res = NEW_ARR_D (int, obst, size);
  memset(res, 0, sizeof(int) * size);
  return res;
}


#endif /* _IRBACKEDGE_T_H_ */
