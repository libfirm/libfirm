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

#include <stdio.h>

#include "bearch.h"

typedef struct arm_isa_t arm_isa_t;

/** Floating point instruction set. */
enum arm_fp_architectures {
	ARM_FPU_FPA_EXT_V1     = 0x40000000, /**< Base FPA instruction set. */
	ARM_FPU_FPA_EXT_V2     = 0x20000000, /**< LFM/SFM. */
	ARM_FPU_VFP_EXT_NONE   = 0x10000000, /**< Use VFP word-ordering. */
	ARM_FPU_VFP_EXT_V1xD   = 0x08000000, /**< Base VFP instruction set. */
	ARM_FPU_VFP_EXT_V1     = 0x04000000, /**< Double-precision insns. */
	ARM_FPU_VFP_EXT_V2     = 0x02000000, /**< ARM10E VFPr1. */

	ARM_FPU_SOFTFLOAT      = 0x01000000, /**< soft float library */
	ARM_FPU_NONE           = 0,

	ARM_FPU_ARCH_FPE       = ARM_FPU_FPA_EXT_V1,
	ARM_FPU_ARCH_FPA       = ARM_FPU_ARCH_FPE | ARM_FPU_FPA_EXT_V2,

	ARM_FPU_ARCH_VFP       = ARM_FPU_VFP_EXT_NONE,
	ARM_FPU_ARCH_VFP_V1xD  = ARM_FPU_VFP_EXT_V1xD | ARM_FPU_VFP_EXT_NONE,
	ARM_FPU_ARCH_VFP_V1    = ARM_FPU_ARCH_VFP_V1xD | ARM_FPU_VFP_EXT_V1,
	ARM_FPU_ARCH_VFP_V2    = ARM_FPU_ARCH_VFP_V1 | ARM_FPU_VFP_EXT_V2,

	ARM_FPU_ARCH_SOFTFLOAT = ARM_FPU_SOFTFLOAT,

	ARM_FPU_MASK           = 0x7f000000,
};

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

struct arm_isa_t {
	arch_env_t base; /**< must be derived from arch_env_t */
};

typedef struct arm_codegen_config_t {
	arm_variant_t variant;
	bool          use_softfloat;
	bool          use_fpa;
	bool          use_vfp;
	bool          big_endian;
} arm_codegen_config_t;

extern arm_codegen_config_t arm_cg_config;

extern ir_mode *arm_mode_gp;
extern ir_mode *arm_mode_flags;

extern const arch_irn_ops_t arm_irn_ops;

void arm_finish_graph(ir_graph *irg);

void arm_lower_64bit(void);

#endif
