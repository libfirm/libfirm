/*
 * bechordal_common.h
 *
 *  Created on: Nov 11, 2009
 *      Author: bersch
 */

#ifndef BECHORDAL_COMMON_H_
#define BECHORDAL_COMMON_H_

#include "config.h"

#include "bechordal.h"
#include "beinsn_t.h"

void pressure(ir_node *block, void *env_ptr);
inline int has_reg_class(const be_chordal_env_t *env, const ir_node *irn);

ir_node *pre_process_constraints(be_chordal_env_t *_env, be_insn_t **the_insn);
be_insn_t *chordal_scan_insn(be_chordal_env_t *env, ir_node *irn);

#endif /* BECHORDAL_COMMON_H_ */
