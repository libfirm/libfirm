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
 * @brief       This file implements the IR transformation from firm into ia32-Firm.
 * @author      Christian Wuerdig, Matthias Braun
 * @version     $Id$
 */
#ifndef FIRM_BE_IA32_IA32_TRANSFORM_H
#define FIRM_BE_IA32_IA32_TRANSFORM_H

#include "firm_config.h"
#include "bearch_ia32_t.h"

typedef struct {
	/** use inc, dec instead of add ,1 and add, -1 */
	unsigned use_incdec:1;
	/** use sse2 instructions */
	unsigned use_sse2:1;
	/** use ffreep instead of fpop */
	unsigned use_ffreep:1;
	/** use ftst where possible */
	unsigned use_ftst:1;
	/** use femms to pop all float registers */
	unsigned use_femms:1;
	/** use the fucomi instruction */
	unsigned use_fucomi:1;
	/** use cmovXX instructions */
	unsigned use_cmov:1;
} transform_config_t;

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

/**
 * Generate a known floating point constant
 */
ir_entity *ia32_gen_fp_known_const(ia32_known_const_t kct);

void ia32_add_missing_keeps(ia32_code_gen_t *cg);

/**
 * return true if the node is a Proj(Load) and could be used in source address
 * mode for another node. Will return only true if the @p other node is not
 * dependent on the memory of the Load (for binary operations use the other
 * input here, for unary operations use NULL).
 */
int ia32_use_source_address_mode(ir_node *block, ir_node *node, ir_node *other);

/**
 * Skip all Down-Conv's on a given node and return the resulting node.
 */
ir_node *ia32_skip_downconv(ir_node *node);

#endif /* FIRM_BE_IA32_IA32_TRANSFORM_H */
