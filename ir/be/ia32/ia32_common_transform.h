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
 * @brief       This file implements the common parts of IR transformation from
 *              firm into ia32-Firm.
 * @author      Matthias Braun, Sebastian Buchwald
 * @version     $Id: ia32_common_transform.h 20999 2008-08-05 16:48:29Z beck $
 */
#ifndef FIRM_BE_IA32_IA32_COMMON_TRANSFORM_H
#define FIRM_BE_IA32_IA32_COMMON_TRANSFORM_H

#include "bearch_ia32_t.h"
#include "height.h"

/**
 * An assembler constraint.
 */
typedef struct constraint_t constraint_t;
struct constraint_t {
	const arch_register_class_t *cls;
	unsigned                     allowed_registers;
	char                         all_registers_allowed;
	char                         memory_possible;
	char                         immediate_type;
	int                          same_as;
};

extern ia32_code_gen_t *env_cg;
extern heights_t *heights;

/**
 * Get an atomic entity that is initialized with a tarval forming
 * a given constant.
 *
 * @param cnst             the node representing the constant
 */
ir_entity *create_float_const_entity(ir_node *cnst);

/**
 * Creates an immediate.
 *
 * @param symconst       if set, create a SymConst immediate
 * @param symconst_sign  sign for the symconst
 * @param val            integer value for the immediate
 */
ir_node *create_Immediate(ir_entity *symconst, int symconst_sign, long val);

/**
 * returns register by name (used for determining clobber specifications in
 * asm instructions)
 */
const arch_register_t *ia32_get_clobber_register(const char *clobber);

#ifndef NDEBUG
/**
 * Prints the old node name on cg obst and returns a pointer to it.
 */
const char *ia32_get_old_node_name(ia32_code_gen_t *cg, ir_node *irn);
#endif /* NDEBUG */

/**
 * Return true if a mode can be stored in the GP register set.
 */
int ia32_mode_needs_gp_reg(ir_mode *mode);

/**
 * generates code for a ASM node
 */
ir_node *gen_ASM(ir_node *node);

/**
 * This function just sets the register for the Unknown node
 * as this is not done during register allocation because Unknown
 * is an "ignore" node.
 */
ir_node *gen_Unknown(ir_node *node);

const arch_register_req_t *make_register_req(const constraint_t *constraint,
		int n_outs, const arch_register_req_t **out_reqs, int pos);

const arch_register_req_t *parse_clobber(const char *clobber);

ir_node *try_create_Immediate(ir_node *node, char immediate_constraint_type);

#endif /* FIRM_BE_IA32_IA32_COMMON_TRANSFORM_H */
