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

/**
 * Preprocess the irg with the MRIS algorithm.
 * @param birg The backend irg.
 * @return     Private data to be kept.
 */
mris_env_t *be_sched_mris_preprocess(const be_irg_t *birg);

/**
 * Cleanup the MRIS preprocessing.
 * @param env The private data as returned by be_sched_mris_preprocess().
 */
void be_sched_mris_free(mris_env_t *env);

/**
 * Dump IR graph with lineages.
 */
void dump_ir_block_graph_mris(mris_env_t *env, const char *suffix);

#endif /* _BESCHEDMRIS_H */
