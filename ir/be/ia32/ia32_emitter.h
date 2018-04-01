/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file implements the ia32 node emitter.
 * @author      Christian Wuerdig, Matthias Braun
 */
#ifndef FIRM_BE_IA32_IA32_EMITTER_H
#define FIRM_BE_IA32_IA32_EMITTER_H

#include "firm_types.h"
#include "ia32_encode.h"
#include "jit.h"
#include "x86_node.h"

/**
 * fmt  parameter               output
 * ---- ----------------------  ---------------------------------------------
 * %AF  <node>                  address mode or x87 register
 * %AM  <node>                  address mode of the node
 * %ASx <node>                  address mode of the node or source register x
 * %B   <node>                  operands for binary operation
 * %Dx  <node>                  destination register x
 * %E   ir_entity const*        emit entity name
 * %Fx  <node>                  x87 register x
 * %FM  <node>                  x87 mode suffix
 * %FX  <node>                  SSE mode suffix
 * %I   <node>                  immediate of the node
 * %M   <node>                  mode suffix of the node
 * %Px  <node>                  condition code
 * %PX  int                     condition code
 * %R   arch_register_t const*  register
 * %Sx  <node>                  source register x
 *
 * x starts at 0
 * # modifier for %ASx, %D, %R, and %S uses ls mode of node to alter register width
 * # modifier for %M for extend suffix
 * * modifier does not prefix immediates with $, but AM with *
 * > modifier to output high 8bit register (ah, bh)
 * < modifier to output low 8bit register (al, bl)
 * , modifier output comma after operand, leave out operand if it is 1
 *    (used for shift nodes)
 */
void ia32_emitf(ir_node const *node, char const *fmt, ...);

void ia32_emit_function(ir_graph *irg);

void ia32_emit_thunks(void);

/** Initializes the Emitter. */
void ia32_init_emitter(void);

x86_condition_code_t ia32_determine_final_cc(ir_node const *node,
                                             int flags_pos);

void ia32_emit_jumptable_target(ir_entity const *const table,
                                ir_node const *const proj_x);

bool ia32_should_align_block(ir_node const *block);

#endif
