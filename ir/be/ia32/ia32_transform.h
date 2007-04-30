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

/**
 * Transform firm nodes to x86 assembler nodes
 */
void ia32_transform_graph(ia32_code_gen_t *cg);

/**
 * The Psi selector can be a tree of compares combined with "And"s and "Or"s.
 * We create a Set node, respectively a xCmp in case the Psi is a float, for each
 * compare, which causes the compare result to be stores in a register.  The
 * "And"s and "Or"s are transformed later, we only adjust their mode.
 */
void ia32_transform_psi_cond_tree(ir_node *node, void *env);

#ifndef NDEBUG
/**
 * Prints the old node name on cg obst and returns a pointer to it.
 */
const char *ia32_get_old_node_name(ia32_code_gen_t *cg, ir_node *irn);
#endif /* NDEBUG */

typedef enum {
	ia32_SSIGN, ia32_DSIGN, ia32_SABS, ia32_DABS, ia32_known_const_max
} ia32_known_const_t;

/**
 * Generate a known floating point constant
 */
ir_entity *ia32_gen_fp_known_const(ia32_known_const_t kct);

#endif /* FIRM_BE_IA32_IA32_TRANSFORM_H */
