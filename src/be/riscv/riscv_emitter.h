/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

/**
 * @file
 * @brief       This file implements the RISC-V node emitter.
 * @author      Christoph Mallon
 */
#ifndef FIRM_BE_RISCV_RISCV_EMITTER_H
#define FIRM_BE_RISCV_RISCV_EMITTER_H

#include "irnode.h"

/**
 * fmt  parameter               output
 * ---- ----------------------  ---------------------------------------------
 * %A   <node>                  address, %lo of offset + base register
 * %C   int (riscv_cond_t)      condition code
 * %Dx  <node>                  destination register x
 * %H   <node>                  %hi of immediate
 * %I   <node>                  %lo of immediate
 * %J   <node>                  immediate
 * %R   arch_register_t const*  register
 * %Sx  <node>                  source register x
 */
void riscv_emitf(ir_node const *node, char const *fmt, ...);

void riscv_emit_function(ir_graph *irg);

#endif
