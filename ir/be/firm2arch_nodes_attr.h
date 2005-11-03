#ifndef NODES_ATTR_H
#define NODES_ATTR_H

#include "firm_types.h"

typedef struct {
  enum { asmop_Const, asmop_SymConst } tp;
  struct {
    tarval  *tv;       /**<< tarval for immediate operations */
    tarval  *offset;   /**<< offset for LEA */
    ir_node *old_ir;   /**<< old ir node to avoid duplicating information (symconst in case of asmop_SymConst) */
  } data;

  void *reg_constraints;
} asmop_attr;

#endif
