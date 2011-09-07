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
 * @brief   attributes attached to all sparc nodes
 * @author  Hannes Rapp, Matthias Braun
 * @version $Id$
 */
#ifndef FIRM_BE_SPARC_SPARC_NODES_ATTR_H
#define FIRM_BE_SPARC_SPARC_NODES_ATTR_H

#include "../bearch.h"
#include <stdint.h>

typedef struct sparc_attr_t  sparc_attr_t;

/**
 * base SPARC attribute
 */
struct sparc_attr_t
{
	except_attr                 exc;                /**< the exception attribute. MUST be the first one. */
	int32_t                     immediate_value;    /* immediate values */
	ir_entity                  *immediate_value_entity;
};

enum sparc_arch_irn_flags_t {
	sparc_arch_irn_flag_modifies_flags        = arch_irn_flags_backend << 0,
	sparc_arch_irn_flag_modifies_fp_flags     = arch_irn_flags_backend << 1,
	sparc_arch_irn_flag_needs_64bit_spillslot = arch_irn_flags_backend << 2,
	sparc_arch_irn_flag_immediate_form        = arch_irn_flags_backend << 3,
	sparc_arch_irn_flag_aggregate_return      = arch_irn_flags_backend << 4,
};

/**
 * attribute for FP immediate instruction
 */
typedef struct sparc_fp_attr_t sparc_fp_attr_t;
struct sparc_fp_attr_t {
	sparc_attr_t  base;         /**< generic attribute */
	ir_mode      *fp_mode;
};

typedef struct sparc_fp_conv_attr_t sparc_fp_conv_attr_t;
struct sparc_fp_conv_attr_t {
	sparc_attr_t  base;
	ir_mode      *src_mode;
	ir_mode      *dest_mode;
};

/**
 * attributes for load/store addressing modes
 */
typedef struct sparc_load_store_attr_t sparc_load_store_attr_t;
struct sparc_load_store_attr_t {
	sparc_attr_t  base;    /**< generic attribute */
	ir_mode      *load_store_mode;
	bool          is_frame_entity : 1;
	bool          is_reg_reg      : 1;
};

/**
 * attributes for conditional jumps
 */
typedef struct sparc_jmp_cond_attr_t sparc_jmp_cond_attr_t;
struct sparc_jmp_cond_attr_t {
	sparc_attr_t base;    /**< generic attribute */
	ir_relation  relation;
	bool         is_unsigned : 1;
};

/**
 * attributes for switch jumps
 */
typedef struct sparc_switch_jmp_attr_t sparc_switch_jmp_attr_t;
struct sparc_switch_jmp_attr_t {
	sparc_attr_t  base;    /**< generic attribute */
	long          default_proj_num;
	ir_entity    *jump_table;
};

#endif
