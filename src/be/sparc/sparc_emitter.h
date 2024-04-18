/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    declarations for emit functions
 * @author   Hannes Rapp, Matthias Braun
 */
#ifndef FIRM_BE_SPARC_EMITTER_H
#define FIRM_BE_SPARC_EMITTER_H

#include "firm_types.h"

/**
 * fmt   parameter               output
 * ----  ----------------------  -------------------------------
 * %A    <node>                  emit ",a" in delay slot annul mode
 * %Dx   <node>                  destination register x
 * %E    <node>                  entity+offset
 * %FD   <node>                  floating point destination mode of conversion
 * %FM   <node>                  floating point mode
 * %FS   <node>                  floating point source mode of conversion
 * %H    <node>                  high immediate
 * %ML   <node>                  load mode
 * %MS   <node>                  store mode
 * %Ox   <node>                  memory access with offset [%reg + %reg/imm]
 * %R    arch_register_t const*  register
 * %Sx   <node>                  source register x
 * %SIx  <node>                  immediate or source register x
 * %d    signed int              signed int
 * %X    unsigned int            unsigned int in hexadecimal
 *
 * x starts at 0
 * + may be used with %d
 */
void sparc_emitf(ir_node const *node, char const *fmt, ...);

void sparc_emit_function(ir_graph *irg);

void sparc_init_emitter(void);

#endif
