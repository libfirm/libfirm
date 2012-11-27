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
 */
#ifndef FIRM_BE_IA32_IA32_COMMON_TRANSFORM_H
#define FIRM_BE_IA32_IA32_COMMON_TRANSFORM_H

#include "firm_types.h"
#include "bearch_ia32_t.h"

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

extern ir_heights_t *ia32_heights;
extern int           ia32_no_pic_adjust;

ir_type *ia32_get_prim_type(const ir_mode *mode);

/**
 * Get an atomic entity that is initialized with a tarval forming
 * a given constant.
 */
ir_entity *ia32_create_float_const_entity(ia32_isa_t *isa, ir_tarval *tv,
                                          ident *name);

/**
 * Creates an immediate.
 *
 * @param symconst       if set, create a SymConst immediate
 * @param symconst_sign  sign for the symconst
 * @param val            integer value for the immediate
 */
ir_node *ia32_create_Immediate(ir_entity *symconst, int symconst_sign, long val);

/**
 * returns register by name (used for determining clobber specifications in
 * asm instructions)
 */
const arch_register_t *ia32_get_clobber_register(const char *clobber);

/**
 * Return true if a mode can be stored in the GP register set.
 */
int ia32_mode_needs_gp_reg(ir_mode *mode);

/**
 * generates code for a ASM node
 */
ir_node *ia32_gen_ASM(ir_node *node);

/**
 * Transforms a CopyB node.
 *
 * @return The transformed node.
 */
ir_node *ia32_gen_CopyB(ir_node *node);

/**
 * Transform the Thread Local Storage Proj.
 */
ir_node *ia32_gen_Proj_tls(ir_node *node);

/**
 * This function just sets the register for the Unknown node
 * as this is not done during register allocation because Unknown
 * is an "ignore" node.
 */
ir_node *ia32_gen_Unknown(ir_node *node);

const arch_register_req_t *ia32_parse_clobber(const char *clobber);

/**
 * Checks whether other node inputs depend on the am_candidate (via mem-proj).
 */
int ia32_prevents_AM(ir_node *const block, ir_node *const am_candidate,
                       ir_node *const other);

ir_node *ia32_try_create_Immediate(ir_node *node, char immediate_constraint_type);

#endif
