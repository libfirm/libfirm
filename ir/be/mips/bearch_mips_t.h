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
 * @brief   datastructures and declarations for the mips backend
 * @author  Matthias Braun, Mehdi
 * @version $Id$
 */
#ifndef FIRM_BE_MIPS_BEARCH_MIPS_T_H
#define FIRM_BE_MIPS_BEARCH_MIPS_T_H

#include "debug.h"
#include "irgopt.h"
#include "bearch_mips.h"
#include "mips_nodes_attr.h"
#include "../be.h"
#include "../beemitter.h"
#include "set.h"

typedef struct mips_isa_t            mips_isa_t;
typedef struct mips_irn_ops_t        mips_irn_ops_t;
typedef struct mips_transform_env_t  mips_transform_env_t;

struct mips_code_gen_t {
	const arch_code_generator_if_t *impl;           /**< implementation */
	ir_graph                       *irg;            /**< current irg */
	const arch_env_t               *arch_env;       /**< the arch env */
	set                            *reg_set;        /**< set to memorize registers for FIRM nodes (e.g. phi) */
	int                             emit_decls;     /**< flag indicating if decls were already emitted */
	mips_isa_t                     *isa;            /**< the isa instance */
	const be_irg_t                 *birg;           /**< The be-irg (contains additional information about the irg) */
	ir_node                        **bl_list;		/**< The block schedule list. */
	survive_dce_t				   *bl_list_sdce;	/**< survive dce environment for the block schedule list */
	DEBUG_ONLY(firm_dbg_module_t   *mod;)           /**< debugging module */
};

struct mips_isa_t {
	arch_isa_t             arch_isa;    /**< must be derived from arch_isa_t */
	be_emit_env_t          emit;        /**< An emitter environment for the GAS emitter. */
};

struct mips_irn_ops_t {
	const arch_irn_ops_if_t *impl;
	mips_code_gen_t         *cg;
};


/**
 * this is a struct to minimize the number of parameters
 * for transformation walker
 */
struct mips_transform_env_t {
	dbg_info          *dbg;      /**< The node debug info */
	ir_graph          *irg;      /**< The irg, the node should be created in */
	ir_node           *block;    /**< The block, the node should belong to */
	ir_node           *irn;      /**< The irn, to be transformed */
	ir_mode           *mode;     /**< The mode of the irn */
	mips_code_gen_t   *cg;       /**< The code generator */
	DEBUG_ONLY(firm_dbg_module_t *mod;) /**< The firm debugger */
};

ir_node *mips_new_NoReg(mips_code_gen_t *cg);

#endif
