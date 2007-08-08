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
	IA32_OPT_INCDEC    = 1,   /**< optimize add/sub 1/-1 to inc/dec */
	IA32_OPT_DOAM      = 2,   /**< do address mode optimizations */
	IA32_OPT_LEA       = 4,   /**< optimize address calculations into LEAs */
	IA32_OPT_PLACECNST = 8,   /**< place constants in the blocks where they are used */
	IA32_OPT_IMMOPS    = 16,  /**< create operations with immediate operands */
	IA32_OPT_PUSHARGS  = 32,  /**< create pushs for function argument passing */
};

/**
 * Architectures. Clustered for easier macro implementation,
 * do not change.
 */
enum cpu_support {
	arch_i386,          /**< i386 */
	arch_i486,          /**< i486 */
	arch_pentium,       /**< Pentium */
	arch_pentium_pro,   /**< Pentium Pro */
	arch_pentium_mmx,   /**< Pentium MMX */
	arch_pentium_2,     /**< Pentium II */
	arch_pentium_3,     /**< Pentium III */
	arch_pentium_4,     /**< Pentium IV */
	arch_pentium_m,     /**< Pentium M */
	arch_core,          /**< Core */
	arch_k6,            /**< K6 */
	arch_athlon,        /**< Athlon */
	arch_athlon_64,     /**< Athlon64 */
	arch_opteron,       /**< Opteron */
};

/** checks for l <= x <= h */
#define _IN_RANGE(x, l, h)  ((unsigned)((x) - (l)) <= (unsigned)((h) - (l)))

/** returns true if it's Intel architecture */
#define ARCH_INTEL(x)       _IN_RANGE((x), arch_i386, arch_core)

/** returns true if it's AMD architecture */
#define ARCH_AMD(x)         _IN_RANGE((x), arch_k6, arch_opteron)

#define IS_P6_ARCH(x)       (_IN_RANGE((x), arch_pentium_pro, arch_core) || \
                             _IN_RANGE((x), arch_athlon, arch_opteron))

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
	be_emit_env_t          emit;
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
	ir_graph  *irg;           /**< the irg, these entities belong to */
	ir_entity *ll_div_op1;    /**< entity for first div operand (move into FPU) */
	ir_entity *ll_div_op2;    /**< entity for second div operand (move into FPU) */
	ir_entity *ll_d_conv;     /**< entity for converts ll -> d */
	ir_entity *d_ll_conv;     /**< entity for converts d -> ll */
	ir_entity *divdi3;        /**< entity for __divdi3 library call */
	ir_entity *moddi3;        /**< entity for __moddi3 library call */
	ir_entity *udivdi3;       /**< entity for __udivdi3 library call */
	ir_entity *umoddi3;       /**< entity for __umoddi3 library call */
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
 * Returns the uniqure per irg FPU truncation mode node.
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
