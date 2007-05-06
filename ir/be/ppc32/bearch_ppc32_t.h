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
 * @brief   declarations for arm backend -- private header
 * @author  Moritz Kroll, Jens Mueller
 * @version $Id$
 */
#ifndef FIRM_BE_PPC32_BEARCH_PPC32_T_H
#define FIRM_BE_PPC32_BEARCH_PPC32_T_H

#include "debug.h"
#include "bearch_ppc32.h"
#include "ppc32_nodes_attr.h"
#include "../be.h"
#include "../beemitter.h"
#include "pset.h"
#include "set.h"

typedef struct _ppc32_isa_t ppc32_isa_t;

typedef struct _ppc32_code_gen_t {
	const arch_code_generator_if_t *impl;             /**< implementation */
	ir_graph                       *irg;              /**< current irg */
	const arch_env_t               *arch_env;         /**< the arch env */
	set                            *reg_set;          /**< set to memorize registers for FIRM nodes (e.g. phi) */
	ppc32_isa_t                    *isa;              /**< the isa instance */
	const be_irg_t                 *birg;             /**< The be-irg (contains additional information about the irg) */
	unsigned                        area_size;        /**< size of call area for the current irg */
	ir_entity                      *area;             /**< the entity representing the call area or NULL for leaf functions */
	ir_node                        *start_succ_block; /**< the block succeeding the start block in the cfg */
	ir_node                        **blk_sched;       /**< an array containing the scheduled blocks */
	DEBUG_ONLY(firm_dbg_module_t    *mod;)             /**< debugging module */
} ppc32_code_gen_t;


struct _ppc32_isa_t {
	arch_isa_t             arch_isa;      /**< must be derived from arch_isa_t */
	be_emit_env_t          emit;          /**< An emitter environment for the GAS emitter. */
	pset                   *symbol_set;   /**< A set containing the indirect symbols. */
};


typedef struct _ppc32_irn_ops_t {
	const arch_irn_ops_if_t *impl;
	ppc32_code_gen_t     *cg;
} ppc32_irn_ops_t;


/** this is a struct to minimize the number of parameters
   for transformation walker */
typedef struct _ppc32_transform_env_t {
	dbg_info          *dbg;      /**< The node debug info */
	ir_graph          *irg;      /**< The irg, the node should be created in */
	ir_node           *block;    /**< The block, the node should belong to */
	ir_node           *irn;      /**< The irn, to be transformed */
	ir_mode           *mode;     /**< The mode of the irn */
	DEBUG_ONLY(firm_dbg_module_t *mod;) /**< The firm debugger */
} ppc32_transform_env_t;

#endif
