#ifndef BEPEEPHOLE_H
#define BEPEEPHOLE_H

#include "beirg.h"

extern ir_node ***register_values;

static inline ir_node *get_value_in_reg(unsigned regclass_idx,
                                        unsigned register_idx)
{
	return register_values[regclass_idx][register_idx];
}

void be_peephole_opt(be_irg_t *birg);

#endif
