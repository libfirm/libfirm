/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declarations for arm backend -- private header
 * @author  Oliver Richter, Tobias Gneist
 */
#ifndef FIRM_BE_ARM_ARM_BEARCH_T_H
#define FIRM_BE_ARM_ARM_BEARCH_T_H

#include <stdbool.h>

#include "beirg.h"
#include "firm_types.h"

#define ARM_PO2_STACK_ALIGNMENT 3

typedef struct arm_irg_data_t {
	bool omit_fp;
} arm_irg_data_t;

typedef struct arm_isa_t arm_isa_t;

/**
 * ARM architecture variants (not complete yet, add variants as necessary)
 */
typedef enum {
	ARM_VARIANT_4,
	ARM_VARIANT_5T,
	ARM_VARIANT_6,
	ARM_VARIANT_6T2,
	ARM_VARIANT_7,
} arm_variant_t;

/** Floating point instruction set. */
typedef enum {
	ARM_FPU_SOFTFLOAT,
	ARM_FPU_FPA,
} arm_fpu_variant_t;

typedef struct arm_codegen_config_t {
	arm_variant_t     variant;
	arm_fpu_variant_t fpu;
	bool              big_endian;
} arm_codegen_config_t;

extern arm_codegen_config_t arm_cg_config;

extern ir_mode *arm_mode_gp;
extern ir_mode *arm_mode_flags;

static inline arm_irg_data_t *arm_get_irg_data(ir_graph const *const irg)
{
	return (arm_irg_data_t*)be_birg_from_irg(irg)->isa_link;
}

void arm_finish_graph(ir_graph *irg);

void arm_lower_64bit(void);

#endif
