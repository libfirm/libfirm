/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declarations for ARM node attributes
 * @author  Oliver Richter, Tobias Gneist, Michael Beck
 */
#ifndef FIRM_BE_ARM_ARM_NODES_ATTR_H
#define FIRM_BE_ARM_ARM_NODES_ATTR_H

#include "beasm.h"
#include "benode.h"
#include "firm_types.h"
#include "irnode_t.h"

/**
 * Possible ARM "shifter operand" addressing mode types.
 */
typedef enum arm_shift_modifier_t {
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
} arm_shift_modifier_t;

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
typedef struct arm_attr_t {
	except_attr exc;                /**< the exception attribute. MUST be the first one. */
	bool        is_load_store : 1;  /**< if set, this is a load or store instruction */
} arm_attr_t;

typedef struct arm_asm_operand_t {
	be_asm_operand_t op;
	int32_t          val;
	ir_entity       *ent;
} arm_asm_operand_t;

/**
 * This struct holds information needed to produce the arm
 * "data processing operands" also called "shifter operand" addressing modes
 */
typedef struct arm_shifter_operand_t {
	arm_attr_t           base;
	arm_shift_modifier_t shift_modifier;
	uint8_t              shifter_op_input;
	uint8_t              immediate_value;
	uint8_t              shift_immediate;
} arm_shifter_operand_t;

typedef struct arm_cmp_attr_t {
	arm_shifter_operand_t base;
	bool                  ins_permuted : 1;
	bool                  is_unsigned  : 1;
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

/** Attributes for an Address */
typedef struct arm_Address_attr_t {
	arm_attr_t  base;
	ir_entity  *entity;
	int         fp_offset;
} arm_Address_attr_t;

/** Attributes for a CondJmp */
typedef struct arm_CondJmp_attr_t {
	arm_attr_t  base;
	ir_relation relation;
} arm_CondJmp_attr_t;

/** Attributes for a SwitchJmp */
typedef struct arm_SwitchJmp_attr_t {
	arm_attr_t       base;
	be_switch_attr_t swtch;
} arm_SwitchJmp_attr_t;

/** CopyB attributes */
typedef struct arm_CopyB_attr_t {
	arm_attr_t base;
	unsigned   size;
} arm_CopyB_attr_t;

/** Attributes for a fConst */
typedef struct arm_fConst_attr_t {
	arm_attr_t  base;
	ir_tarval  *tv;              /**< the tarval representing the FP const */
} arm_fConst_attr_t;

/** attributes for floatingpoint arithmetic operations */
typedef struct arm_farith_attr_t {
	arm_attr_t  base;
	ir_mode    *mode; /* operation mode */
} arm_farith_attr_t;

#define CAST_ARM_ATTR(type,ptr)        ((type *)(ptr))
#define CONST_CAST_ARM_ATTR(type,ptr)  ((const type *)(ptr))

#endif
