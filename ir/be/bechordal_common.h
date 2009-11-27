/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @file
 * @brief       Common functions for chordal register allocation.
 * @author      Sebastian Hack
 * @date        08.12.2004
 * @version     $Id: bechordal.c 26750 2009-11-27 09:37:43Z bersch $
 */

#ifndef BECHORDAL_COMMON_H_
#define BECHORDAL_COMMON_H_

#include "config.h"

#include "bechordal.h"
#include "beinsn_t.h"

/**
 * Annotate the register pressure to the nodes and compute
 * the liveness intervals.
 * @param block The block to do it for.
 * @param env_ptr The environment.
 */
void create_borders(ir_node *block, void *env_ptr);

/**
 * Check, if an irn is of the register class currently under processing.
 * @param env The chordal environment.
 * @param irn The node.
 * @return 1, if the node is of that register class, 0 if not.
 */
int has_reg_class(const be_chordal_env_t *env, const ir_node *irn);

/**
 * Insert perm nodes
 * @param env The chordal environment.
 * @param the_insn The current be_insn node.
 * @return The new perm node.
 */
ir_node *pre_process_constraints(be_chordal_env_t *_env, be_insn_t **the_insn);

/**
 * Create a be_insn_t for an IR node.
 * @param env The chordal environment.
 * @param irn The node.
 * @return the be_insn for the IR node
 */
be_insn_t *chordal_scan_insn(be_chordal_env_t *env, ir_node *irn);

#endif /* BECHORDAL_COMMON_H_ */
