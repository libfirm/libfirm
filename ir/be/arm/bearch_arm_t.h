/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declarations for arm backend -- private header
 * @author  Oliver Richter, Tobias Gneist
 */
#ifndef FIRM_BE_ARM_BEARCH_ARM_T_H
#define FIRM_BE_ARM_BEARCH_ARM_T_H

#include "bearch.h"

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

void arm_finish_graph(ir_graph *irg);

void arm_lower_64bit(void);

#endif
