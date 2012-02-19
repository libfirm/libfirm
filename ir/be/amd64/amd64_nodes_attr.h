/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   attributes attached to all amd64 nodes
 */
#ifndef FIRM_BE_AMD64_AMD64_NODES_ATTR_H
#define FIRM_BE_AMD64_AMD64_NODES_ATTR_H

#include "bearch.h"

typedef struct amd64_attr_t            amd64_attr_t;
typedef struct amd64_SymConst_attr_t   amd64_SymConst_attr_t;
typedef struct amd64_switch_jmp_attr_t amd64_switch_jmp_attr_t;

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

struct amd64_switch_jmp_attr_t
{
	amd64_attr_t           base;
	const ir_switch_table *table;
	ir_entity             *table_entity;
};

#define CAST_AMD64_ATTR(type,ptr)        ((type *)(ptr))
#define CONST_CAST_AMD64_ATTR(type,ptr)  ((const type *)(ptr))

#endif
