#ifndef _IRBACKEDGE_T_H_
#define _IRBACKEDGE_T_H_

# include "string.h"

static INLINE int * new_backedge_arr(struct obstack *obst, int size) {
  int *res = NEW_ARR_D (int, obst, size);
  memset(res, 0, sizeof(int) * size);
  return res;
}


static INLINE void fix_backedges(struct obstack *obst, ir_node *n) {
  opcode opc = get_irn_opcode(n);
  int ** arr = NULL;
  if (opc == iro_Phi)    arr = &n->attr.phi_backedge;
  if ((opc == iro_Block) && !interprocedural_view)
                         arr = &n->attr.block.backedge;
  if ((opc == iro_Block) && interprocedural_view)
                         arr = &n->attr.block.cg_backedge;
  if (opc == iro_Filter) arr = &n->attr.filter.backedge;

  if (ARR_LEN(arr) != ARR_LEN(get_irn_in(n))-1)
    *arr = new_backedge_arr(obst, ARR_LEN(get_irn_in(n))-1);
}

#endif /* _IRBACKEDGE_T_H_ */
