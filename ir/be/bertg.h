#ifndef FIRM_BE_RTG_H
#define FIRM_BE_RTG_H

#include "be_t.h"
#include "irnode_t.h"

void print_parcopy(unsigned *parcopy_orig, unsigned *n_used_orig,
                   const arch_register_class_t *c, firm_dbg_module_t *dbg);

void decompose_rtg(unsigned *parcopy, unsigned *n_used,
                   unsigned *restore_srcs, unsigned *restore_dsts,
                   unsigned *num_restores, const arch_register_class_t *c);

#endif
