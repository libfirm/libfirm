/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This is the main ia32 firm backend driver.
 * @author      Christian Wuerdig
 */
#ifndef FIRM_BE_IA32_BEARCH_IA32_T_H
#define FIRM_BE_IA32_BEARCH_IA32_T_H

#include "bearch.h"
#include "beirg.h"
#include "pmap.h"
#include "x86_cconv.h"

#ifdef NDEBUG
#define SET_IA32_ORIG_NODE(n, o) ((void)(n), (void)(o), (void)0)
#else
#define SET_IA32_ORIG_NODE(n, o) set_ia32_orig_node(n, o)
#endif

#define IA32_REGISTER_SIZE 4

typedef struct ia32_isa_t            ia32_isa_t;
typedef struct ia32_intrinsic_env_t  ia32_intrinsic_env_t;

typedef struct ia32_irg_data_t {
	ir_node  **blk_sched;     /**< an array containing the scheduled blocks */
	unsigned do_x87_sim:1;    /**< set to 1 if x87 simulation should be enforced */
	ir_node  *noreg_gp;       /**< unique NoReg_GP node */
	ir_node  *noreg_fp;       /**< unique NoReg_FP node */
	ir_node  *noreg_xmm;      /**< unique NoReg_XMM node */

	ir_node  *fpu_trunc_mode; /**< truncate fpu mode */
	ir_node  *get_eip;        /**< get eip node */
} ia32_irg_data_t;

/**
 * IA32 ISA object
 */
struct ia32_isa_t {
	arch_env_t             base;     /**< must be derived from arch_env_t */
	pmap                  *tv_ent;   /**< A map of entities that store const tarvals */
	int                    fpu_arch; /**< FPU architecture */
};

/**
 * A helper type collecting needed info for IA32 intrinsic lowering.
 */
struct ia32_intrinsic_env_t {
	ir_entity  *divdi3;  /**< entity for __divdi3 library call */
	ir_entity  *moddi3;  /**< entity for __moddi3 library call */
	ir_entity  *udivdi3; /**< entity for __udivdi3 library call */
	ir_entity  *umoddi3; /**< entity for __umoddi3 library call */
};

/** The mode for the floating point control word. */
extern ir_mode *ia32_mode_fpcw;
/** extended floatingpoint mode */
extern ir_mode *ia32_mode_E;
extern ir_type *ia32_type_E;
extern ir_mode *ia32_mode_gp;
extern ir_mode *ia32_mode_float64;
extern ir_mode *ia32_mode_float32;
extern ir_mode *ia32_mode_flags;

static inline ia32_irg_data_t *ia32_get_irg_data(const ir_graph *irg)
{
	return (ia32_irg_data_t*) be_birg_from_irg(irg)->isa_link;
}

static inline void ia32_request_x87_sim(ir_graph const *const irg)
{
	ia32_irg_data_t *const d = ia32_get_irg_data(irg);
	d->do_x87_sim = true;
}

/**
 * Returns the unique per irg GP NoReg node.
 */
ir_node *ia32_new_NoReg_gp(ir_graph *irg);
ir_node *ia32_new_NoReg_xmm(ir_graph *irg);
ir_node *ia32_new_NoReg_fp(ir_graph *irg);

/**
 * Returns the unique per irg FPU truncation mode node.
 */
ir_node *ia32_new_Fpu_truncate(ir_graph *irg);

/**
 * Split instruction with source AM into Load and separate instruction.
 * @return result of the Load
 */
ir_node *ia32_turn_back_am(ir_node *node);

void ia32_lower64(void);

/**
 * Return the stack entity that contains the return address.
 */
ir_entity *ia32_get_return_address_entity(ir_graph *irg);

/**
 * Return the stack entity that contains the frame address.
 */
ir_entity *ia32_get_frame_address_entity(ir_graph *irg);

/**
 * creates global offset table (GOT) for position independent code (PIC) and
 * adjusts address calculations for it.
 */
void ia32_adjust_pic(ir_graph *irg);

ir_node *ia32_get_pic_base(ir_graph *irg);

static inline bool ia32_is_8bit_val(int32_t const v)
{
	return -128 <= v && v < 128;
}

/**
 * Determine how function parameters and return values are passed.
 * Decides what goes to register or to stack and what stack offsets/
 * datatypes are used.
 *
 * @param function_type  the type of the caller/callee function
 * @param caller         true for convention for the caller, false for callee
 */
x86_cconv_t *ia32_decide_calling_convention(ir_type *function_type,
                                             ir_graph *irg);

void ia32_cconv_init(void);

#endif
