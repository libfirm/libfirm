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
 */
#ifndef FIRM_BE_amd64_BEARCH_amd64_T_H
#define FIRM_BE_amd64_BEARCH_amd64_T_H

#include "debug.h"
#include "amd64_nodes_attr.h"
#include "be.h"
#include "beemitter.h"
#include "set.h"

typedef struct amd64_isa_t            amd64_isa_t;

struct amd64_isa_t {
	arch_env_t  base;      /**< must be derived from arch_isa */
};

ir_node *amd64_new_NoReg_gp(ir_graph *irg);

#endif
