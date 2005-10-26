#ifndef NODES_ATTR_H
#define NODES_ATTR_H

#include "firm_types.h"

typedef struct {
  tarval *tv;
  void *reg_constraints;
} asmop_attr;

#endif
