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
 * @brief   declarations for SPARC backend -- private header
 * @version $Id$
 */
#ifndef FIRM_BE_SPARC_BEARCH_TEMPLATE_T_H
#define FIRM_BE_SPARC_BEARCH_TEMPLATE_T_H

#include "debug.h"
#include "sparc_nodes_attr.h"
#include "be.h"
#include "../beemitter.h"
#include "set.h"

// sparc ABI requires a min stacksize to
// save registers in case of a trap etc.
// by now we assume only non-leaf procedures: 92 + 4 (padding)
#define SPARC_MIN_STACKSIZE 112

typedef struct sparc_transform_env_t  sparc_transform_env_t;
typedef struct _sparc_isa_t sparc_isa_t;

typedef struct _sparc_code_gen_t {
    const arch_code_generator_if_t *impl;           /**< implementation */
    ir_graph                       *irg;            /**< current irg */
    set                            *reg_set;        /**< set to memorize registers for FIRM nodes (e.g. phi) */
    sparc_isa_t                      *isa;            /**< the isa instance */
    be_irg_t                       *birg;           /**< The be-irg (contains additional information about the irg) */
    char                           dump;            /**< set to 1 if graphs should be dumped */
} sparc_code_gen_t;


struct _sparc_isa_t {
    arch_env_t     arch_env;      /**< must be derived from arch_env_t */
    sparc_code_gen_t *cg;           /**< current code generator */
};


/**
 * this is a struct to minimize the number of parameters
 * for transformation walker
 */
struct sparc_transform_env_t {
	dbg_info *dbg;      /**< The node debug info */
	ir_graph *irg;      /**< The irg, the node should be created in */
	ir_node  *block;    /**< The block, the node should belong to */
	ir_node  *irn;      /**< The irn, to be transformed */
	ir_mode  *mode;     /**< The mode of the irn */
};

void sparc_finish_irg(sparc_code_gen_t *cg);

#endif
