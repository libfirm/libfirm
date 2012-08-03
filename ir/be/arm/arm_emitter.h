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
 * @brief   declarations for arm emitter
 * @author  Oliver Richter, Tobias Gneis
 */
#ifndef FIRM_BE_ARM_ARM_EMITTER_H
#define FIRM_BE_ARM_ARM_EMITTER_H

#include "firm_types.h"
#include "set.h"
#include "irargs_t.h"
#include "debug.h"

#include "bearch.h"

#include "bearch_arm_t.h"

/**
 * emit assembler instructions with format string. Automatically indents
 * instructions and adds debug comments at the end (in verbose-asm mode).
 * Format specifiers:
 *
 * fmt  parameter               output
 * ---- ----------------------  ---------------------------------------------
 * %%                           %
 * %r   const arch_register_t*  register
 * %Sx  <node>                  source register x
 * %Dx  <node>                  destination register x
 * %O   <node>                  shifter operand
 * %I   <node>                  symconst immediate
 * %o   <node>                  load/store offset
 * %C   const sym_or_tv_t*      constant
 * %t   const ir_node*          controlflow target
 * %m   ir_mode*                fpa mode postfix
 * %s   const char*             string
 * %u   unsigned int            unsigned int
 * %d   signed int              signed int
 * %X   signed int              signed int (hexadecimal)
 */
void arm_emitf(const ir_node *node, const char *format, ...);

void arm_gen_routine(ir_graph *irg);

void arm_init_emitter(void);

#endif
