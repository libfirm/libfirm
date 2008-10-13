/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief       ia32 architecture variants
 * @author      Michael Beck, Matthias Braun
 * @version     $Id: bearch_ia32_t.h 16363 2007-10-25 23:27:07Z beck $
 */
#ifndef FIRM_BE_IA32_ARCHITECTURE_H
#define FIRM_BE_IA32_ARCHITECTURE_H

typedef struct {
	/** optimize for size */
	unsigned optimize_size:1;
	/** use leave in function epilogue */
	unsigned use_leave:1;
	/** use inc, dec instead of add ,1 and add, -1 */
	unsigned use_incdec:1;
	/** use sse2 instructions (instead of x87) */
	unsigned use_sse2:1;
	/** use ffreep instead of fpop */
	unsigned use_ffreep:1;
	/** use ftst where possible */
	unsigned use_ftst:1;
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
	/** use add esp, 8 instead of 2 pop's */
	unsigned use_add_esp_8:1;
	/** use sub esp, 4 instead of push */
	unsigned use_sub_esp_4:1;
	/** use sub esp, 8 instead of 2 push's */
	unsigned use_sub_esp_8:1;
	/** use imul mem, imm32 instruction (slow on some cpu's */
	unsigned use_imul_mem_imm32:1;
	/** use pxor instead xorps/xorpd */
	unsigned use_pxor:1;
	/** use mov reg, 0 instruction */
	unsigned use_mov_0:1;
	/** pad Ret instructions that are destination of conditional jump or directly preceded
	    by other jump instruction. */
	unsigned use_pad_return:1;
	/** use the bt instruction */
	unsigned use_bt:1;
	/** use fisttp instruction (requieres SSE3) */
	unsigned use_fisttp:1;
	/** optimize calling convention where possible */
	unsigned optimize_cc:1;
	/**
	 * disrespect current floating  point rounding mode at entry and exit of
	 * functions (this is ok for programs that don't explicitly change the
	 * rounding mode
	 */
	unsigned use_unsafe_floatconv:1;
	/** function alignment (a power of two in bytes) */
	unsigned function_alignment;
	/** alignment for labels (which are expected to be frequent jump targets) */
	unsigned label_alignment;
	/** maximum skip alignment for labels (which are expected to be frequent jump targets) */
	unsigned label_alignment_max_skip;
	/** if a blocks execfreq is factor higher than it's predecessor then align
	 *  the blocks label (0 switches off label alignment) */
	double label_alignment_factor;
} ia32_code_gen_config_t;

extern ia32_code_gen_config_t  ia32_cg_config;

/** Initialize the ia32 architecture module. */
void ia32_init_architecture(void);

/** Setup the ia32_cg_config structure by inspecting current user settings. */
void ia32_setup_cg_config(void);

/**
 * Evaluate the costs of an instruction. Used by the irach multiplication
 * lowerer.
 *
 * @param kind   the instruction
 * @param tv     for MUL instruction, the multiplication constant
 *
 * @return the cost
 */
int ia32_evaluate_insn(insn_kind kind, tarval *tv);

#endif
