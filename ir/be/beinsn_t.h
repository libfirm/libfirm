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
 * @file
 * @brief       A data structure to treat nodes and node-proj collections uniformly.
 * @author      Sebastian Hack
 * @version     $Id$
 */
#ifndef FIRM_BE_BEINSN_T_H
#define FIRM_BE_BEINSN_T_H

#include "irnode.h"
#include "bitset.h"
#include "obst.h"

#include "bearch.h"
#include "beirg.h"

typedef struct _be_operand_t  be_operand_t;
typedef struct _be_insn_t     be_insn_t;
typedef struct _be_insn_env_t be_insn_env_t;

struct _be_operand_t {
	ir_node *irn;                   /**< firm node of the insn this operand blongs to */
	ir_node *carrier;               /**< node representing the operand value (proj or the node itself for defs, the value itself for uses */
	be_operand_t *partner;          /**< used in bechordal later... (TODO what does it do?) */
	bitset_t *regs;                 /**< admissible register bitset */
	int pos;                        /**< pos of the operand (0 to n are inputs, -1 to -n are outputs) */
	const arch_register_req_t *req; /**< register constraints for the carrier node */
	unsigned has_constraints : 1;   /**< the carrier node has register constraints (the constraint type is limited) */
};

struct _be_insn_t {
	be_operand_t *ops;             /**< the values used and defined by the insn */
	int n_ops;                     /**< length of the ops array */
	int use_start;                 /**< entries [0-use_start) in ops are defs,
	                                    [use_start-n_ops) uses */
	ir_node *next_insn;            /**< next instruction in schedule */
	ir_node *irn;                  /**< ir_node of the instruction */
	unsigned in_constraints  : 1;  /**< instruction has input contraints */
	unsigned out_constraints : 1;  /**< instruction has output constraints */
	unsigned has_constraints : 1;  /**< in_constraints or out_constraints true */
	unsigned pre_colored     : 1;  /**< all defined values already have a register assigned */
};

struct _be_insn_env_t {
	struct obstack              *obst;
	const arch_env_t            *aenv;
	const arch_register_class_t *cls;
	bitset_t                    *ignore_colors;
};

#define be_insn_n_defs(insn) ((insn)->use_start)
#define be_insn_n_uses(insn) ((insn)->n_ops - (insn)->use_start)

be_insn_t *be_scan_insn(const be_insn_env_t *env, ir_node *irn);

be_insn_env_t *be_insn_env_init(be_insn_env_t *ie, const be_irg_t *birg, const arch_register_class_t *cls, struct obstack *obst);

#endif /* FIRM_BE_BEINSN_T_H */
