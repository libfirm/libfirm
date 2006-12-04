#ifndef _MIPS_SCHEDULER_H_
#define _MIPS_SCHEDULER_H_

#include "../besched_t.h"

const list_sched_selector_t *mips_get_list_sched_selector(const void *self, list_sched_selector_t *selector);
const ilp_sched_selector_t *mips_get_ilp_sched_selector(const void *self);

#endif
