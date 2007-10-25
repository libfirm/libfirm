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
 * @brief       This is the main ia32 firm backend driver.
 * @author      Christian Wuerdig
 * @version     $Id$
 */
#ifndef FIRM_BE_IA32_BEARCH_IA32_T_H
#define FIRM_BE_IA32_BEARCH_IA32_T_H

#include "firm_config.h"

#include "pmap.h"
#include "debug.h"
#include "ia32_nodes_attr.h"
#include "set.h"
#include "pdeq.h"

#include "be.h"
#include "../bemachine.h"
#include "../beemitter.h"

#ifdef NDEBUG
#define SET_IA32_ORIG_NODE(n, o)
#else  /* ! NDEBUG */
#define SET_IA32_ORIG_NODE(n, o) set_ia32_orig_node(n, o);
#endif /* NDEBUG */

/* some typedefs */
typedef enum ia32_optimize_t ia32_optimize_t;
typedef enum cpu_support     cpu_support;
typedef enum fp_support      fp_support;

/**
 * Bitmask for the backend optimization settings.
 */
enum ia32_optimize_t {
	IA32_OPT_INCDEC    = 1 << 0,   /**< optimize add/sub 1/-1 to inc/dec */
	IA32_OPT_CC        = 1 << 1,   /**< optimize calling convention of private
	                                    functions */
	IA32_OPT_UNSAFE_FLOATCONV = 1 << 2, /**< disrespect current floating
	                           point rounding mode at entry and exit of
	                           functions (this is ok for programs that don't
	                           explicitly change the rounding mode) */
};

/**
 * CPU features.
 */
enum cpu_arch_features {
	arch_feature_intel    = 0x80000000,                      /**< Intel CPU */
	arch_feature_amd      = 0x40000000,                      /**< AMD CPU */
	arch_feature_p6       = 0x20000000,                      /**< P6 instructions */
	arch_feature_mmx      = 0x10000000,                      /**< MMX instructions */
	arch_feature_sse1     = 0x08000000 | arch_feature_mmx,   /**< SSE1 instructions, include MMX */
	arch_feature_sse2     = 0x04000000 | arch_feature_sse1,  /**< SSE2 instructions, include SSE1 */
	arch_feature_sse3     = 0x02000000 | arch_feature_sse2,  /**< SSE3 instructions, include SSE2 */
	arch_feature_ssse3    = 0x01000000 | arch_feature_sse3,  /**< SSSE3 instructions, include SSE3 */
	arch_feature_3DNow    = 0x00800000,                      /**< 3DNow! instructions */
	arch_feature_3DNowE   = 0x00400000 | arch_feature_3DNow, /**< Enhanced 3DNow! instructions */
	arch_feature_netburst = 0x00200000 | arch_feature_intel, /**< Netburst architecture */
	arch_feature_64bit    = 0x00100000 | arch_feature_sse2,  /**< x86_64 support, include SSE2 */
};

/**
 * Architectures.
 */
enum cpu_support {
	/* intel CPU's */
	arch_generic     =  0,

	arch_i386        =  1,
	arch_i486        =  2,
	arch_pentium     =  3 | arch_feature_intel,
	arch_pentium_mmx =  4 | arch_feature_intel | arch_feature_mmx,
	arch_pentium_pro =  5 | arch_feature_intel | arch_feature_p6,
	arch_pentium_2   =  6 | arch_feature_intel | arch_feature_p6 | arch_feature_mmx,
	arch_pentium_3   =  7 | arch_feature_intel | arch_feature_p6 | arch_feature_sse1,
	arch_pentium_4   =  8 | arch_feature_netburst | arch_feature_p6 | arch_feature_sse2,
	arch_pentium_m   =  9 | arch_feature_intel | arch_feature_p6 | arch_feature_sse2,
	arch_core        = 10 | arch_feature_intel | arch_feature_p6 | arch_feature_sse3,
	arch_prescott    = 11 | arch_feature_netburst | arch_feature_p6 | arch_feature_sse3,
	arch_core2       = 12 | arch_feature_intel | arch_feature_p6 | arch_feature_64bit | arch_feature_ssse3,

	/* AMD CPU's */
	arch_k6          = 13 | arch_feature_amd | arch_feature_mmx,
	arch_k6_2        = 14 | arch_feature_amd | arch_feature_mmx | arch_feature_3DNow,
	arch_k6_3        = 15 | arch_feature_amd | arch_feature_mmx | arch_feature_3DNow,
	arch_athlon      = 16 | arch_feature_amd | arch_feature_mmx | arch_feature_3DNowE | arch_feature_p6,
	arch_athlon_xp   = 17 | arch_feature_amd | arch_feature_sse1 | arch_feature_3DNowE | arch_feature_p6,
	arch_opteron     = 18 | arch_feature_amd | arch_feature_64bit | arch_feature_3DNowE | arch_feature_p6,

	/* other */
	arch_winchip_c6  = 19 | arch_feature_mmx,
	arch_winchip2    = 20 | arch_feature_mmx | arch_feature_3DNow,
	arch_c3          = 21 | arch_feature_mmx | arch_feature_3DNow,
	arch_c3_2        = 22 | arch_feature_sse1,  /* really no 3DNow! */
};

/** checks for l <= x <= h */
#define _IN_RANGE(x, l, h)  ((unsigned)((x) - (l)) <= (unsigned)((h) - (l)))

/** returns true if it's Intel architecture */
#define ARCH_INTEL(x)       ((x) & arch_feature_intel)

/** returns true if it's AMD architecture */
#define ARCH_AMD(x)         ((x) & arch_feature_amd)

/** return true if it's a Athlon/Opteron */
#define ARCH_ATHLON(x)      _IN_RANGE((x), arch_athlon, arch_opteron)

/** return true if the CPU has MMX support */
#define ARCH_MMX(x)         ((x) & arch_feature_mmx)

/** return true if the CPU has 3DNow! support */
#define ARCH_3DNow(x)       ((x) & arch_feature_3DNow)

/** return true if the CPU has P6 features (CMOV) */
#define IS_P6_ARCH(x)       ((x) & arch_feature_p6)

/** floating point support */
enum fp_support {
	fp_none,  /**< no floating point instructions are used */
	fp_x87,   /**< use x87 instructions */
	fp_sse2   /**< use SSE2 instructions */
};

/** Returns non-zero if the current floating point architecture is SSE2. */
#define USE_SSE2(cg) ((cg)->fp_kind == fp_sse2)

/** Returns non-zero if the current floating point architecture is x87. */
#define USE_x87(cg)  ((cg)->fp_kind == fp_x87)

typedef struct ia32_isa_t            ia32_isa_t;
typedef struct ia32_code_gen_t       ia32_code_gen_t;
typedef struct ia32_irn_ops_t        ia32_irn_ops_t;
typedef struct ia32_intrinsic_env_t  ia32_intrinsic_env_t;

/**
 * IA32 code generator
 */
struct ia32_code_gen_t {
	const arch_code_generator_if_t *impl;          /**< implementation */
	ir_graph                       *irg;           /**< current irg */
	const arch_env_t               *arch_env;      /**< the arch env */
	set                            *reg_set;       /**< set to memorize registers for non-ia32 nodes (e.g. phi nodes) */
	ia32_isa_t                     *isa;           /**< for fast access to the isa object */
	be_irg_t                       *birg;          /**< The be-irg (contains additional information about the irg) */
	ir_node                        **blk_sched;    /**< an array containing the scheduled blocks */
	ia32_optimize_t                opt;            /**< contains optimization information */
	int                            arch;           /**< instruction architecture */
	int                            opt_arch;       /**< optimize for architecture */
	char                           fp_kind;        /**< floating point kind */
	char                           do_x87_sim;     /**< set to 1 if x87 simulation should be enforced */
	char                           dump;           /**< set to 1 if graphs should be dumped */
	ir_node                       *unknown_gp;     /**< unique Unknown_GP node */
	ir_node                       *unknown_vfp;    /**< unique Unknown_VFP node */
	ir_node                       *unknown_xmm;    /**< unique Unknown_XMM node */
	ir_node                       *noreg_gp;       /**< unique NoReg_GP node */
	ir_node                       *noreg_vfp;      /**< unique NoReg_VFP node */
	ir_node                       *noreg_xmm;      /**< unique NoReg_XMM node */

	ir_node                       *fpu_trunc_mode; /**< truncate fpu mode */

	struct obstack                *obst;
};

/**
 * IA32 ISA object
 */
struct ia32_isa_t {
	arch_isa_t            arch_isa;       /**< must be derived from arch_isa_t */
	pmap                  *regs_16bit;    /**< Contains the 16bits names of the gp registers */
	pmap                  *regs_8bit;     /**< Contains the 8bits names of the gp registers */
	pmap                  *regs_8bit_high; /**< contains the hight part of the 8 bit names of the gp registers */
	pmap                  *types;         /**< A map of modes to primitive types */
	pmap                  *tv_ent;        /**< A map of entities that store const tarvals */
	ia32_optimize_t       opt;            /**< contains optimization information */
	int                   arch;           /**< instruction architecture */
	int                   opt_arch;       /**< optimize for architecture */
	int                   fp_kind;        /**< floating point kind */
	ia32_code_gen_t       *cg;            /**< the current code generator */
	const be_machine_t    *cpu;           /**< the abstract machine */
#ifndef NDEBUG
	struct obstack        *name_obst;     /**< holds the original node names (for debugging) */
#endif /* NDEBUG */
};

struct ia32_irn_ops_t {
	const arch_irn_ops_if_t *impl;
	ia32_code_gen_t         *cg;
};

/**
 * A helper type collecting needed info for IA32 intrinsic lowering.
 */
struct ia32_intrinsic_env_t {
	ia32_isa_t *isa;          /**< the isa object */
	ir_graph   *irg;          /**< the irg, these entities belong to */
	ir_entity  *ll_div_op1;   /**< entity for first div operand (move into FPU) */
	ir_entity  *ll_div_op2;   /**< entity for second div operand (move into FPU) */
	ir_entity  *ll_d_conv;    /**< entity for converts ll -> d */
	ir_entity  *d_ll_conv;    /**< entity for converts d -> ll */
	ir_entity  *divdi3;       /**< entity for __divdi3 library call */
	ir_entity  *moddi3;       /**< entity for __moddi3 library call */
	ir_entity  *udivdi3;      /**< entity for __udivdi3 library call */
	ir_entity  *umoddi3;      /**< entity for __umoddi3 library call */
	tarval     *u64_bias;     /**< bias value for conversion from float to unsigned 64 */
};

/** The mode for the floating point control word. */
extern ir_mode *mode_fpcw;

/** The current code generator. */
extern ia32_code_gen_t *ia32_current_cg;

/**
 * Returns the unique per irg GP NoReg node.
 */
ir_node *ia32_new_NoReg_gp(ia32_code_gen_t *cg);
ir_node *ia32_new_NoReg_xmm(ia32_code_gen_t *cg);
ir_node *ia32_new_NoReg_vfp(ia32_code_gen_t *cg);

/**
 * Returns the uniqure per irg GP Unknown node.
 * (warning: cse has to be activated)
 */
ir_node *ia32_new_Unknown_gp(ia32_code_gen_t *cg);
ir_node *ia32_new_Unknown_xmm(ia32_code_gen_t *cg);
ir_node *ia32_new_Unknown_vfp(ia32_code_gen_t *cg);

/**
 * Returns the unique per irg FP NoReg node.
 */
ir_node *ia32_new_NoReg_fp(ia32_code_gen_t *cg);

/**
 * Returns the unique per irg FPU truncation mode node.
 */
ir_node *ia32_new_Fpu_truncate(ia32_code_gen_t *cg);

/**
 * Returns gp_noreg or fp_noreg, depending on input requirements.
 */
ir_node *ia32_get_admissible_noreg(ia32_code_gen_t *cg, ir_node *irn, int pos);

/**
 * Maps all intrinsic calls that the backend support
 * and map all instructions the backend did not support
 * to runtime calls.
 */
void ia32_handle_intrinsics(void);

/**
 * Ia32 implementation.
 *
 * @param method   the method type of the emulation function entity
 * @param op       the emulated ir_op
 * @param imode    the input mode of the emulated opcode
 * @param omode    the output mode of the emulated opcode
 * @param context  the context parameter
 */
ir_entity *ia32_create_intrinsic_fkt(ir_type *method, const ir_op *op,
                                     const ir_mode *imode, const ir_mode *omode,
                                     void *context);

#endif
