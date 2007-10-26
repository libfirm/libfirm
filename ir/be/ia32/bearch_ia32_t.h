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
