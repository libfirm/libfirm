/**
 * Implements a list schedule selector for the MRIS algorithm in:
 * Govindarajan, Yang, Amaral, Zhang, Gao
 * Minimum Register Instruction Sequencing to Reduce Register Spills
 * in out-of-order issue superscalar architectures
 */

#ifndef _BESCHEDMRIS_H
#define _BESCHEDMRIS_H

#include "be.h"
#include "belistsched.h"

typedef struct _mris_env_t mris_env_t;

mris_env_t *be_sched_mris_preprocess(const be_irg_t *birg);
void be_sched_mris_free(mris_env_t *env);

#endif /* _BESCHEDMRIS_H */
