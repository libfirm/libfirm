/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

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
