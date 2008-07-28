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
 * @brief       This file implements the IR transformation from firm into ia32-Firm.
 * @author      Christian Wuerdig, Matthias Braun
 * @version     $Id$
 */
#ifndef FIRM_BE_IA32_IA32_TRANSFORM_H
#define FIRM_BE_IA32_IA32_TRANSFORM_H

#include "firm_config.h"
#include "bearch_ia32_t.h"

/**
 * Transform firm nodes to x86 assembler nodes
 */
void ia32_transform_graph(ia32_code_gen_t *cg);

#ifndef NDEBUG
/**
 * Prints the old node name on cg obst and returns a pointer to it.
 */
const char *ia32_get_old_node_name(ia32_code_gen_t *cg, ir_node *irn);
#endif /* NDEBUG */

/**
 * Some constants needed for code generation.
 * Generated on demand.
 */
typedef enum {
	ia32_SSIGN,          /**< SSE2 single precision sign */
	ia32_DSIGN,          /**< SSE2 double precision sign */
	ia32_SABS,           /**< SSE2 single precision ABS mask */
	ia32_DABS,           /**< SSE2 double precision ABS mask */
	ia32_INTMAX,         /**< x87 single precision INTMAX */
	ia32_known_const_max /**< last constant */
} ia32_known_const_t;

static const arch_register_req_t no_register_req = {
	arch_register_req_type_none,
	NULL,                         /* regclass */
	NULL,                         /* limit bitset */
	0,                            /* same pos */
	0                             /* different pos */
};

/**
 * An assembler constraint.
 */
typedef struct constraint_t constraint_t;
struct constraint_t {
	int                         is_in;
	int                         n_outs;
	const arch_register_req_t **out_reqs;

	const arch_register_req_t  *req;
	unsigned                    immediate_possible;
	char                        immediate_type;
};

/**
 * Generate a known floating point constant
 */
ir_entity *ia32_gen_fp_known_const(ia32_known_const_t kct);

void ia32_add_missing_keeps(ia32_code_gen_t *cg);

/**
 * Skip all Down-Conv's on a given node and return the resulting node.
 */
ir_node *ia32_skip_downconv(ir_node *node);

/**
 * Get a primitive type for a mode.
 */
ir_type *ia32_get_prim_type(pmap *types, ir_mode *mode);

/**
 * Return true if a mode can be stored in the GP register set
 */
int ia32_mode_needs_gp_reg(ir_mode *mode);

void parse_asm_constraint(int pos, constraint_t *constraint, const char *c);
void parse_clobber(ir_node *node, int pos, constraint_t *constraint,
                          const char *clobber);

/**
 * returns register by name (used for determining clobber specifications in
 * asm instructions)
 */
const arch_register_t *ia32_get_clobber_register(const char *clobber);

#endif /* FIRM_BE_IA32_IA32_TRANSFORM_H */
