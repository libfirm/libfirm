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
#ifndef FIRM_BE_IA32_BEARCH_TEMPLATE_T_H
#define FIRM_BE_IA32_BEARCH_TEMPLATE_T_H

#include "debug.h"
#include "bearch_TEMPLATE.h"
#include "TEMPLATE_nodes_attr.h"
#include "../be.h"
#include "../beemitter.h"
#include "set.h"

typedef struct TEMPLATE_isa_t            TEMPLATE_isa_t;
typedef struct TEMPLATE_code_gen_t       TEMPLATE_code_gen_t;
typedef struct TEMPLATE_irn_ops_t        TEMPLATE_irn_ops_t;
typedef struct TEMPLATE_transform_env_t  TEMPLATE_transform_env_t;

struct TEMPLATE_code_gen_t {
	const arch_code_generator_if_t *impl;           /**< implementation */
	ir_graph                       *irg;            /**< current irg */
	const arch_env_t               *arch_env;       /**< the arch env */
	set                            *reg_set;        /**< set to memorize registers for FIRM nodes (e.g. phi) */
	TEMPLATE_isa_t                 *isa;            /**< the isa instance */
	const be_irg_t                 *birg;           /**< The be-irg (contains additional information about the irg) */
	DEBUG_ONLY(firm_dbg_module_t   *mod;)           /**< debugging module */
};

struct TEMPLATE_isa_t {
	arch_isa_t             arch_isa;      /**< must be derived from arch_isa */
	be_emit_env_t          emit;          /**< emit datastructure */
};

struct TEMPLATE_irn_ops_t {
	const arch_irn_ops_if_t *impl;
	TEMPLATE_code_gen_t     *cg;
};

/**
 * this is a struct to minimize the number of parameters
 * for transformation walker
 */
struct TEMPLATE_transform_env_t {
	dbg_info          *dbg;      /**< The node debug info */
	ir_graph          *irg;      /**< The irg, the node should be created in */
	ir_node           *block;    /**< The block, the node should belong to */
	ir_node           *irn;      /**< The irn, to be transformed */
	ir_mode           *mode;     /**< The mode of the irn */
	DEBUG_ONLY(firm_dbg_module_t *mod;)      /**< The firm debugger */
};

#endif
