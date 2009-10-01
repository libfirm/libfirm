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
 * @brief   declarations for ARM node attributes
 * @author  Oliver Richter, Tobias Gneist, Michael Beck
 * @version $Id$
 */
#ifndef FIRM_BE_ARM_ARM_NODES_ATTR_H
#define FIRM_BE_ARM_ARM_NODES_ATTR_H

#include "firm_types.h"
#include "irnode_t.h"
#include "../bearch.h"

/**
 * Possible ARM "shifter operand" addressing mode types.
 */
typedef enum _arm_shift_modifier {
	ARM_SHF_INVALID,   /**< invalid shift */
	ARM_SHF_REG,       /**< simple register operand */
	ARM_SHF_IMM,       /**< immediate operand with implicit ROR */
	ARM_SHF_ASR_IMM,   /**< arithmetic shift right */
	ARM_SHF_ASR_REG,   /**< arithmetic shift right */
	ARM_SHF_LSL_IMM,   /**< logical shift left */
	ARM_SHF_LSL_REG,   /**< logical shift left */
	ARM_SHF_LSR_IMM,   /**< logical shift right */
	ARM_SHF_LSR_REG,   /**< logical shift right */
	ARM_SHF_ROR_IMM,   /**< rotate right */
	ARM_SHF_ROR_REG,   /**< rotate right */
	ARM_SHF_RRX,       /**< rotate right through carry bits */
} arm_shift_modifier;

/** fpa immediate bit */
#define ARM_FPA_IMM  (1 << 3)   /**< fpa floating point immediate */

#define ARM_GET_FPA_IMM(attr)        ((attr)->instr_fl & ARM_FPA_IMM)
#define ARM_SET_FPA_IMM(attr)        ((attr)->instr_fl |= ARM_FPA_IMM)
#define ARM_CLR_FPA_IMM(attr)        ((attr)->instr_fl &= ~ARM_FPA_IMM)

/** Encoding for fpa immediates */
enum fpa_immediates {
	fpa_null = 0,
	fpa_one,
	fpa_two,
	fpa_three,
	fpa_four,
	fpa_five,
	fpa_ten,
	fpa_half,
	fpa_max
};

/** Generic ARM node attributes. */
typedef struct _arm_attr_t {
	except_attr      exc;                /**< the exception attribute. MUST be the first one. */

	const arch_register_req_t **in_req;  /**< register requirements for arguments */

	ir_mode  *op_mode;                   /**< operation mode if different from node's mode (used for fpa nodes) */
	unsigned  instr_fl;                  /**< deprecated (was sometimes used for shift modifiers) */
	bool      is_load_store : 1;
} arm_attr_t;

/**
 * This struct holds information needed to produce the arm
 * "data processing operands" also called "shifter operand" addressing modes
 */
typedef struct arm_shifter_operand_t {
	arm_attr_t          base;
	arm_shift_modifier  shift_modifier;
	unsigned char       immediate_value;
	unsigned char       shift_immediate;
} arm_shifter_operand_t;

typedef struct arm_cmp_attr_t {
	arm_shifter_operand_t  base;
	bool                   ins_permuted : 1;
	bool                   is_unsigned  : 1;
} arm_cmp_attr_t;

/**
 * this struct holds information needed to produce the arm addressing modes
 * for "Load and Store Word or Unsigned Byte", "Miscellaneous Loads and Stores"
 * and "Load and Store Multiple" */
typedef struct arm_load_store_attr_t {
	arm_attr_t  base;
	ir_mode    *load_store_mode;
	ir_entity  *entity;
	long        offset;
	bool        is_frame_entity : 1;
	bool        entity_sign     : 1;
} arm_load_store_attr_t;

/** Attributes for a SymConst */
typedef struct _arm_SymConst_attr_t {
	arm_attr_t  base;
	ir_entity  *entity;
	int         fp_offset;
} arm_SymConst_attr_t;

/** Attributes for a CondJmp */
typedef struct _arm_CondJmp_attr_t {
	arm_attr_t  base;
	int         proj_num;
} arm_CondJmp_attr_t;

/** Attributes for a SwitchJmp */
typedef struct _arm_SwitchJmp_attr_t {
	arm_attr_t  base;
	int         n_projs;
	long        default_proj_num;
} arm_SwitchJmp_attr_t;

/** CopyB attributes */
typedef struct {
	arm_attr_t  base;
	unsigned    size;
} arm_CopyB_attr_t;

/** Attributes for a fpaConst */
typedef struct _arm_fpaConst_attr_t {
	arm_attr_t  base;
	tarval     *tv;              /**< the tarval representing the FP const */
} arm_fpaConst_attr_t;

/**
 * Return the fpa immediate from the encoding.
 */
const char *arm_get_fpa_imm_name(long imm_value);

#define CAST_ARM_ATTR(type,ptr)        ((type *)(ptr))
#define CONST_CAST_ARM_ATTR(type,ptr)  ((const type *)(ptr))

#endif
