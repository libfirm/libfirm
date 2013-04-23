/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       ia32 architecture variants
 * @author      Michael Beck, Matthias Braun
 */
#ifndef FIRM_BE_IA32_ARCHITECTURE_H
#define FIRM_BE_IA32_ARCHITECTURE_H

#include "irarch.h"

typedef struct {
	/** optimize for size */
	unsigned optimize_size:1;
	/** use leave in function epilogue */
	unsigned use_leave:1;
	/** use inc, dec instead of add $1, reg and add $-1, reg */
	unsigned use_incdec:1;
	/** use soft float library */
	unsigned use_softfloat:1;
	/** use sse2 instructions (instead of x87) */
	unsigned use_sse2:1;
	/** use ffreep instead of fpop */
	unsigned use_ffreep:1;
	/** use femms to pop all float registers */
	unsigned use_femms:1;
	/** use emms to pop all float registers */
	unsigned use_emms:1;
	/** use the fucomi instruction */
	unsigned use_fucomi:1;
	/** use cmovXX instructions */
	unsigned use_cmov:1;
	/** mode_D moves instead of 2 integer moves */
	unsigned use_modeD_moves:1;
	/** use add esp, 4 instead of pop */
	unsigned use_add_esp_4:1;
	/** use add esp, 8 instead of 2 pops */
	unsigned use_add_esp_8:1;
	/** use sub esp, 4 instead of push */
	unsigned use_sub_esp_4:1;
	/** use sub esp, 8 instead of 2 pushs */
	unsigned use_sub_esp_8:1;
	/** use imul mem, imm32 instruction (slow on some CPUs) */
	unsigned use_imul_mem_imm32:1;
	/** use pxor instead xorps/xorpd */
	unsigned use_pxor:1;
	/** use mov reg, 0 instruction */
	unsigned use_mov_0:1;
	/** use cwtl/cltd, which are shorter, to sign extend ax/eax */
	unsigned use_short_sex_eax:1;
	/** pad Ret instructions that are destination of conditional jump or directly preceded
	    by other jump instruction. */
	unsigned use_pad_return:1;
	/** use the bt instruction */
	unsigned use_bt:1;
	/** use fisttp instruction (requires SSE3) */
	unsigned use_fisttp:1;
	/** use SSE prefetch instructions */
	unsigned use_sse_prefetch:1;
	/** use 3DNow! prefetch instructions */
	unsigned use_3dnow_prefetch:1;
	/** use SSE4.2 or SSE4a popcnt instruction */
	unsigned use_popcnt:1;
	/** use i486 instructions */
	unsigned use_bswap:1;
	/** use cmpxchg */
	unsigned use_cmpxchg:1;
	/** optimize calling convention where possible */
	unsigned optimize_cc:1;
	/**
	 * disrespect current floating  point rounding mode at entry and exit of
	 * functions (this is ok for programs that don't explicitly change the
	 * rounding mode
	 */
	unsigned use_unsafe_floatconv:1;
	/** emit machine code instead of assembler */
	unsigned emit_machcode:1;

	/** function alignment (a power of two in bytes) */
	unsigned function_alignment;
	/** alignment for labels (which are expected to be frequent jump targets) */
	unsigned label_alignment;
	/** maximum skip alignment for labels (which are expected to be frequent jump targets) */
	unsigned label_alignment_max_skip;
	/** if a blocks execfreq is factor higher than its predecessor then align
	 *  the blocks label (0 switches off label alignment) */
	double label_alignment_factor;
} ia32_code_gen_config_t;

extern ia32_code_gen_config_t  ia32_cg_config;

typedef enum ia32_fp_architectures {
	IA32_FPU_ARCH_NONE      = 0,
	IA32_FPU_ARCH_X87       = 0x00000001,
	IA32_FPU_ARCH_SSE2      = 0x00000002,
	IA32_FPU_ARCH_SOFTFLOAT = 0x00000004,
}
ia32_fp_architectures;

/** Initialize the ia32 architecture module. */
void ia32_init_architecture(void);

/** Setup the ia32_cg_config structure by inspecting current user settings. */
void ia32_setup_cg_config(void);

/**
 * Evaluate the costs of an instruction. Used by the irach multiplication
 * lowerer.
 *
 * @param kind   the instruction
 * @param mode   the mode of the instruction
 * @param tv     for MUL instruction, the multiplication constant
 *
 * @return the cost
 */
int ia32_evaluate_insn(insn_kind kind, const ir_mode *mode, ir_tarval *tv);

#endif
