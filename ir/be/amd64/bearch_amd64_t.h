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
 * @brief   declarations for TEMPALTE backend -- private header
 * @version $Id: bearch_amd64_t.h 26542 2009-09-18 09:18:32Z matze $
 */
#ifndef FIRM_BE_amd64_BEARCH_amd64_T_H
#define FIRM_BE_amd64_BEARCH_amd64_T_H

#include "debug.h"
#include "amd64_nodes_attr.h"
#include "be.h"
#include "../beemitter.h"
#include "set.h"

typedef struct amd64_isa_t            amd64_isa_t;
typedef struct amd64_transform_env_t  amd64_transform_env_t;

typedef struct amd64_irg_data_t {
	ir_graph    *irg;            /**< current irg */
	amd64_isa_t *isa;            /**< the isa instance */
	char         dump;           /**< set to 1 if graphs should be dumped */
	ir_node     *noreg_gp;       /**< unique NoReg_GP node */
} amd64_irg_data_t;

struct amd64_isa_t {
	arch_env_t  base;      /**< must be derived from arch_isa */
};

/**
 * this is a struct to minimize the number of parameters
 * for transformation walker
 */
struct amd64_transform_env_t {
	dbg_info *dbg;      /**< The node debug info */
	ir_graph *irg;      /**< The irg, the node should be created in */
	ir_node  *block;    /**< The block, the node should belong to */
	ir_node  *irn;      /**< The irn, to be transformed */
	ir_mode  *mode;     /**< The mode of the irn */
};

static inline amd64_irg_data_t *amd64_get_irg_data(const ir_graph *irg)
{
	return (amd64_irg_data_t*) be_birg_from_irg(irg)->isa_link;
}

ir_node *amd64_new_NoReg_gp(ir_graph *irg);

#endif
