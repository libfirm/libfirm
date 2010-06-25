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
 * @brief   attributes attached to all amd64 nodes
 * @version $Id: amd64_nodes_attr.h 26317 2009-08-05 10:53:46Z matze $
 */
#ifndef FIRM_BE_amd64_amd64_NODES_ATTR_H
#define FIRM_BE_amd64_amd64_NODES_ATTR_H

#include "../bearch.h"

typedef struct amd64_attr_t            amd64_attr_t;
typedef struct amd64_SymConst_attr_t   amd64_SymConst_attr_t;

struct amd64_attr_t
{
	const arch_register_req_t **in_req;  /**< register requirements for arguments */
	const arch_register_req_t **out_req; /**< register requirements for results */
	ir_mode                    *ls_mode; /**< Stores the "input" mode */
	struct amd64_attr_data_bitfield {
		unsigned ins_permuted : 1;      /**< inputs of node have been permuted
		                                     (for commutative nodes) */
		unsigned cmp_unsigned : 1;      /**< compare should be unsigned */
	} data;
	struct amd64_attr_extended {
		pn_Cmp   pnc;                   /**< type of compare operation >*/
		unsigned imm_value;             /**< immediate value to use >*/
	} ext;
};

struct amd64_SymConst_attr_t
{
	ir_entity *entity;
};

#define CAST_AMD64_ATTR(type,ptr)        ((type *)(ptr))
#define CONST_CAST_AMD64_ATTR(type,ptr)  ((const type *)(ptr))

#endif
