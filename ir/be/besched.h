
#ifndef _BESCHED_H
#define _BESCHED_H

#include <stdio.h>

#include "besched_t.h"

void be_sched_init(void);
void be_sched_dump(FILE *f, const ir_graph *irg);

#endif
