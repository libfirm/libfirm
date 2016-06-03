#ifndef FIRM_ANA_IMPORTANT_ARGS_H
#define FIRM_ANA_IMPORTANT_ARGS_H

#include "firm_types.h"
#include "bitset.h"
#include "begin.h"

FIRM_API bitset_t *local_important_args(ir_graph *proc);

#include "end.h"

#endif
