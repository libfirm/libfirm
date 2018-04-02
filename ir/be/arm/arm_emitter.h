/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declarations for arm emitter
 * @author  Oliver Richter, Tobias Gneis
 */
#ifndef FIRM_BE_ARM_ARM_EMITTER_H
#define FIRM_BE_ARM_ARM_EMITTER_H

#include "firm_types.h"

/**
 * emit assembler instructions with format string. Automatically indents
 * instructions and adds debug comments at the end (in verbose-asm mode).
 * Format specifiers:
 *
 * fmt  parameter               output
 * ---- ----------------------  ---------------------------------------------
 * %r   const arch_register_t*  register
 * %Sx  <node>                  source register x
 * %Dx  <node>                  destination register x
 * %O   <node>                  shifter operand
 * %I   <node>                  address immediate
 * %o   <node>                  load/store offset
 * %C   const ent_or_tv_t*      constant
 * %t   const ir_node*          controlflow target
 * %m   ir_mode*                fpa mode postfix
 */
void arm_emitf(const ir_node *node, const char *format, ...);

void arm_emit_function(ir_graph *irg);

void arm_init_emitter(void);

void arm_emit_file_prologue(void);

#endif
