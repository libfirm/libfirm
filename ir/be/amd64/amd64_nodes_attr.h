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
 */
#ifndef FIRM_BE_amd64_amd64_NODES_ATTR_H
#define FIRM_BE_amd64_amd64_NODES_ATTR_H

#include "bearch.h"

typedef struct amd64_attr_t            amd64_attr_t;
typedef struct amd64_SymConst_attr_t   amd64_SymConst_attr_t;

struct amd64_attr_t
{
	except_attr                 exc;     /**< the exception attribute. MUST be the first one. */
	ir_mode                    *ls_mode; /**< Stores the "input" mode */
	struct amd64_attr_data_bitfield {
		unsigned ins_permuted : 1;      /**< inputs of node have been permuted
		                                     (for commutative nodes) */
		unsigned cmp_unsigned : 1;      /**< compare should be unsigned */
	} data;
	struct amd64_attr_extended {
		ir_relation relation;           /**< type of compare operation >*/
		unsigned    imm_value;          /**< immediate value to use >*/
	} ext;
};

struct amd64_SymConst_attr_t
{
	amd64_attr_t  base;
	ir_entity    *entity;
	unsigned      fp_offset;
};

#define CAST_AMD64_ATTR(type,ptr)        ((type *)(ptr))
#define CONST_CAST_AMD64_ATTR(type,ptr)  ((const type *)(ptr))

#endif
