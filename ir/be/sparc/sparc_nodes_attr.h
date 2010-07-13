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
 * @version $Id$
 */
#ifndef FIRM_BE_SPARC_SPARC_NODES_ATTR_H
#define FIRM_BE_SPARC_SPARC_NODES_ATTR_H

#include "../bearch.h"

typedef struct sparc_attr_t  sparc_attr_t;

/**
 * base SPARC attribute
 */
struct sparc_attr_t
{
	except_attr					exc;				/**< the exception attribute. MUST be the first one. */
	const arch_register_req_t 	**in_req;  			/**< register requirements for arguments */
	int							immediate_value;	/* immediate values */
	bool						is_load_store;
};

/**
 * attribute for FP immediate instruction
 */
typedef struct sparc_fp_attr_t sparc_fp_attr_t;
struct sparc_fp_attr_t {
	sparc_attr_t  	base;    /**< generic attribute */
	double 			fp_immediate; /* the FP immediate value */
};

/**
 * attribute for save instruction
 */
typedef struct sparc_save_attr_t sparc_save_attr_t;
struct sparc_save_attr_t {
	sparc_attr_t  	base;    /**< generic attribute */
	int 			initial_stacksize; /* the min. stack size required by the sparc ABI */
};

/**
 * attributes for load/store adressing modes
 */
typedef struct sparc_load_store_attr_t sparc_load_store_attr_t;
struct sparc_load_store_attr_t {
	sparc_attr_t  	base;    /**< generic attribute */
	ir_mode 		*load_store_mode;
	ir_entity 		*entity;
	int 			entity_sign;
	long 			offset;
	bool 			is_frame_entity;
};

/**
 * attributes for SymConsts
 */
typedef struct sparc_symconst_attr_t sparc_symconst_attr_t;
struct sparc_symconst_attr_t {
	sparc_attr_t  	base;    /**< generic attribute */
	ir_entity		*entity;
	int				fp_offset;
};

/**
 * attributes for conditional jumps
 */
typedef struct sparc_jmp_cond_attr_t sparc_jmp_cond_attr_t;
struct sparc_jmp_cond_attr_t {
	sparc_attr_t  	base;    /**< generic attribute */
	int				proj_num;
};

/**
 * attributes for switch jumps
 */
typedef struct sparc_jmp_switch_attr_t sparc_jmp_switch_attr_t;
struct sparc_jmp_switch_attr_t {
	sparc_attr_t  	base;    /**< generic attribute */
	int				n_projs;
	long			default_proj_num;
};

/**
 * attributes for Cmp
 */
typedef struct sparc_cmp_attr_t sparc_cmp_attr_t;
struct sparc_cmp_attr_t {
	sparc_attr_t  	base;    /**< generic attribute */
	bool			ins_permuted : 1;
	bool			is_unsigned  : 1;
};

#endif
