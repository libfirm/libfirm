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

#include <stdbool.h>

#include "firm_types.h"
#include "irarch.h"

typedef struct {
	/** optimize for size */
	bool optimize_size:1;
	/** use leave in function epilogue */
	bool use_leave:1;
	/** use inc, dec instead of add $1, reg and add $-1, reg */
	bool use_incdec:1;
	/** use soft float library */
	bool use_softfloat:1;
	/** use sse2 instructions (instead of x87) */
	bool use_sse2:1;
	/** use ffreep instead of fpop */
	bool use_ffreep:1;
	/** use femms to pop all float registers */
	bool use_femms:1;
	/** use emms to pop all float registers */
	bool use_emms:1;
	/** use the fucomi instruction */
	bool use_fucomi:1;
	/** use cmovXX instructions */
	bool use_cmov:1;
	/** mode_D moves instead of 2 integer moves */
	bool use_modeD_moves:1;
	/** use add esp, 4 instead of pop */
	bool use_add_esp_4:1;
	/** use add esp, 8 instead of 2 pops */
	bool use_add_esp_8:1;
	/** use sub esp, 4 instead of push */
	bool use_sub_esp_4:1;
	/** use sub esp, 8 instead of 2 pushs */
	bool use_sub_esp_8:1;
	/** use imul mem, imm32 instruction (slow on some CPUs) */
	bool use_imul_mem_imm32:1;
	/** use pxor instead xorps/xorpd */
	bool use_pxor:1;
	/** use mov reg, 0 instruction */
	bool use_mov_0:1;
	/** use cwtl/cltd, which are shorter, to sign extend ax/eax */
	bool use_short_sex_eax:1;
	/** pad Ret instructions that are destination of conditional jump or
	 * directly preceded by other jump instruction. */
	bool use_pad_return:1;
	/** use the bt instruction */
	bool use_bt:1;
	/** use fisttp instruction (requires SSE3) */
	bool use_fisttp:1;
	/** use SSE prefetch instructions */
	bool use_sse_prefetch:1;
	/** use 3DNow! prefetch instructions */
	bool use_3dnow_prefetch:1;
	/** use SSE4.2 or SSE4a popcnt instruction */
	bool use_popcnt:1;
	/** use i486 instructions */
	bool use_bswap:1;
	/** use cmpxchg */
	bool use_cmpxchg:1;
	/** optimize calling convention where possible */
	bool optimize_cc:1;
	/**
	 * disrespect current floating  point rounding mode at entry and exit of
	 * functions (this is ok for programs that don't explicitly change the
	 * rounding mode
	 */
	bool use_unsafe_floatconv:1;
	/** emit machine code instead of assembler */
	bool emit_machcode:1;

	/** function alignment (a power of two in bytes) */
	unsigned function_alignment;
	/** alignment for labels (which are expected to be frequent jump targets) */
	unsigned label_alignment;
	/** maximum skip alignment for labels (which are expected to be frequent
	 * jump targets) */
	unsigned label_alignment_max_skip;
	/** if a blocks execfreq is factor higher than its predecessor then align
	 *  the blocks label (0 switches off label alignment) */
	double label_alignment_factor;
} ia32_code_gen_config_t;

extern ia32_code_gen_config_t ia32_cg_config;

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
