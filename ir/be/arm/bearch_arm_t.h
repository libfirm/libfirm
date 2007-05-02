/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief   declarations for arm backend -- private header
 * @author  Oliver Richter, Tobias Gneist
 * @version $Id$
 */
#ifndef FIRM_BE_ARM_BEARCH_ARM_T_H
#define FIRM_BE_ARM_BEARCH_ARM_T_H

#include <stdio.h>

#include "debug.h"
#include "bearch_arm.h"
#include "arm_nodes_attr.h"
#include "../be.h"
#include "set.h"

typedef struct _arm_isa_t arm_isa_t;

/** The following bitmasks control CPU extensions:  */
enum arm_cpu_extensions {
	ARM_EXT_V1    = 0x00000001,  /**< All processors (core set). */
	ARM_EXT_V2    = 0x00000002,  /**< Multiply instructions. */
	ARM_EXT_V2S   = 0x00000004,  /**< SWP instructions. */
	ARM_EXT_V3    = 0x00000008,  /**< MSR MRS. */
	ARM_EXT_V3M   = 0x00000010,  /**< Allow long multiplies. */
	ARM_EXT_V4    = 0x00000020,  /**< Allow half word loads. */
	ARM_EXT_V4T   = 0x00000040,  /**< Thumb v1. */
	ARM_EXT_V5    = 0x00000080,  /**< Allow CLZ, etc. */
	ARM_EXT_V5T   = 0x00000100,  /**< Thumb v2.´*/
	ARM_EXT_V5ExP = 0x00000200,  /**< DSP core set. */
	ARM_EXT_V5E   = 0x00000400,  /**< DSP Double transfers. */
	ARM_EXT_V5J   = 0x00000800,  /**< Jazelle extension.   */

	/* Co-processor space extensions.  */
	ARM_CEXT_XSCALE   = 0x00800000, /**< Allow MIA etc. */
	ARM_CEXT_MAVERICK = 0x00400000, /**< Use Cirrus/DSP coprocessor. */
	ARM_CEXT_IWMMXT   = 0x00200000, /**< Intel Wireless MMX technology coprocessor. */
};

/**
 * Architectures are the sum of the base and extensions.  The ARM ARM (rev E)
 * defines the following: ARMv3, ARMv3M, ARMv4xM, ARMv4, ARMv4TxM, ARMv4T,
 * ARMv5xM, ARMv5, ARMv5TxM, ARMv5T, ARMv5TExP, ARMv5TE.  To these we add
 * three more to cover cores prior to ARM6.  Finally, there are cores which
 * implement further extensions in the co-processor space.
 */
enum arm_architectiures {
	ARM_ARCH_V1     = ARM_EXT_V1,
	ARM_ARCH_V2     = ARM_ARCH_V1 | ARM_EXT_V2,
	ARM_ARCH_V2S    = ARM_ARCH_V2 | ARM_EXT_V2S,
	ARM_ARCH_V3     = ARM_ARCH_V2S | ARM_EXT_V3,
	ARM_ARCH_V3M    = ARM_ARCH_V3 | ARM_EXT_V3M,
	ARM_ARCH_V4xM   = ARM_ARCH_V3 | ARM_EXT_V4,
	ARM_ARCH_V4     = ARM_ARCH_V3M | ARM_EXT_V4,
	ARM_ARCH_V4TxM  = ARM_ARCH_V4xM | ARM_EXT_V4T,
	ARM_ARCH_V4T    = ARM_ARCH_V4 | ARM_EXT_V4T,
	ARM_ARCH_V5xM   = ARM_ARCH_V4xM| ARM_EXT_V5,
	ARM_ARCH_V5     = ARM_ARCH_V4 | ARM_EXT_V5,
	ARM_ARCH_V5TxM  = ARM_ARCH_V5xM | ARM_EXT_V4T | ARM_EXT_V5T,
	ARM_ARCH_V5T    = ARM_ARCH_V5 | ARM_EXT_V4T | ARM_EXT_V5T,
	ARM_ARCH_V5TExP = ARM_ARCH_V5T | ARM_EXT_V5ExP,
	ARM_ARCH_V5TE   = ARM_ARCH_V5TExP | ARM_EXT_V5E,
	ARM_ARCH_V5TEJ  = ARM_ARCH_V5TE | ARM_EXT_V5J,

	/* Processors with specific extensions in the co-processor space.  */
	ARM_ARCH_XSCALE = ARM_ARCH_V5TE | ARM_CEXT_XSCALE,
	ARM_ARCH_IWMMXT = ARM_ARCH_XSCALE | ARM_CEXT_IWMMXT,

	ARM_ARCH_MASK   = 0x00ffffff,
};

/** Floating point instruction set. */
enum arm_fp_architectures {
	ARM_FPU_FPA_EXT_V1     = 0x80000000, /**< Base FPA instruction set. */
	ARM_FPU_FPA_EXT_V2     = 0x40000000, /**< LFM/SFM. */
	ARM_FPU_VFP_EXT_NONE   = 0x20000000, /**< Use VFP word-ordering. */
	ARM_FPU_VFP_EXT_V1xD   = 0x10000000, /**< Base VFP instruction set. */
	ARM_FPU_VFP_EXT_V1     = 0x08000000, /**< Double-precision insns. */
	ARM_FPU_VFP_EXT_V2     = 0x04000000, /**< ARM10E VFPr1. */

	ARM_FPU_SOFTFLOAT      = 0x01000000, /**< soft float library */
	ARM_FPU_NONE           = 0,

	ARM_FPU_ARCH_FPE       = ARM_FPU_FPA_EXT_V1,
	ARM_FPU_ARCH_FPA       = ARM_FPU_ARCH_FPE | ARM_FPU_FPA_EXT_V2,

	ARM_FPU_ARCH_VFP       = ARM_FPU_VFP_EXT_NONE,
	ARM_FPU_ARCH_VFP_V1xD  = ARM_FPU_VFP_EXT_V1xD | ARM_FPU_VFP_EXT_NONE,
	ARM_FPU_ARCH_VFP_V1    = ARM_FPU_ARCH_VFP_V1xD | ARM_FPU_VFP_EXT_V1,
	ARM_FPU_ARCH_VFP_V2    = ARM_FPU_ARCH_VFP_V1 | ARM_FPU_VFP_EXT_V2,

	ARM_FPU_ARCH_SOFTFLOAT = ARM_FPU_SOFTFLOAT,

	ARM_FPU_MASK           = 0xff000000,
};

/** Returns non-zero if FPA instructions should be issued. */
#define USE_FPA(cg)	 ((cg)->fpu_arch & ARM_FPU_FPA_EXT_V1)

/** Returns non-zero if VFP instructions should be issued. */
#define USE_VFP(cg)	 ((cg)->fpu_arch & ARM_FPU_VFP_EXT_V1xD)

/** Types of processor to generate code for. */
enum arm_processor_types {
	ARM_1      = ARM_ARCH_V1,
	ARM_2      = ARM_ARCH_V2,
	ARM_3      = ARM_ARCH_V2S,
	ARM_250    = ARM_ARCH_V2S,
	ARM_6      = ARM_ARCH_V3,
	ARM_7      = ARM_ARCH_V3,
	ARM_8      = ARM_ARCH_V4,
	ARM_9      = ARM_ARCH_V4T,
	ARM_STRONG = ARM_ARCH_V4,
};

typedef struct _arm_code_gen_t {
	const arch_code_generator_if_t *impl;           /**< implementation */
	ir_graph                       *irg;            /**< current irg */
	const arch_env_t               *arch_env;       /**< the arch env */
	set                            *reg_set;        /**< set to memorize registers for FIRM nodes (e.g. phi) */
	int                             emit_decls;     /**< flag indicating if decls were already emitted */
	arm_isa_t                      *isa;            /**< the isa instance */
	const be_irg_t                 *birg;           /**< The be-irg (contains additional information about the irg) */
	ir_type                        *int_tp;         /**< the int type, needed for Call conversion */
	int                             have_fp;        /**< non-zero, if fp hardware instructions are emitted */
	DEBUG_ONLY(firm_dbg_module_t   *mod;)            /**< debugging module */
} arm_code_gen_t;


struct _arm_isa_t {
	const arch_isa_if_t   *impl;
	const arch_register_t *sp;            /**< The stack pointer register. */
	const arch_register_t *bp;            /**< The base pointer register. */
	const int              stack_dir;     /**< -1 for decreasing, 1 for increasing. */
	int                    num_codegens;
	int                    gen_reg_names; /**< use generic register names instead of SP, LR, PC */
	arm_code_gen_t        *cg;            /**< current code generator */
	FILE                  *out;           /**< output file */

	int                   fpu_arch;      /**< FPU architecture */
};


typedef struct _arm_irn_ops_t {
	const arch_irn_ops_if_t *impl;
	arm_code_gen_t     *cg;
} arm_irn_ops_t;


#endif
