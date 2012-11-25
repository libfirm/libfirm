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
 * @brief       A data structure to treat nodes and node-proj collections uniformly.
 * @author      Sebastian Hack
 */
#ifndef FIRM_BE_BEINSN_T_H
#define FIRM_BE_BEINSN_T_H

#include "firm_types.h"
#include "bitset.h"
#include "obst.h"

#include "bearch.h"
#include "bechordal.h"

typedef struct be_operand_t  be_operand_t;
typedef struct be_insn_t     be_insn_t;

struct be_operand_t {
	ir_node        *carrier; /**< node representing the operand value (Proj or the node itself for defs, the used value for uses) */
	be_operand_t   *partner; /**< used in bechordal later... (TODO what does it do?) */
	unsigned const *regs;    /**< admissible register bitset */
};

struct be_insn_t {
	be_operand_t *ops;       /**< the values used and defined by the insn */
	int           n_ops;     /**< length of the ops array */
	int           use_start; /**< entries [0-use_start) in ops are defs,
	                              [use_start-n_ops) uses */
	ir_node      *irn;       /**< ir_node of the instruction */
};

/**
 * Create a be_insn_t for an IR node.
 *
 * @param env      the insn construction environment
 * @param irn      the irn for which the be_insn should be build
 *
 * @return the be_insn for the IR node
 */
be_insn_t *be_scan_insn(be_chordal_env_t const *env, ir_node *irn);

#endif /* FIRM_BE_BEINSN_T_H */
