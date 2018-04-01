/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    declarations for emit functions
 */
#ifndef FIRM_BE_AMD64_AMD64_EMITTER_H
#define FIRM_BE_AMD64_AMD64_EMITTER_H

#include "firm_types.h"

/**
 * fmt  parameter               output
 * ---- ----------------------  ---------------------------------------------
 * %C   <node>                  immediate value
 * %Dx  <node>                  destination register x
 * %E   ir_entity const*        entity
 * %O   <node>                  offset
 * %R   arch_register_t const*  register
 * %Sx  <node>                  source register x
 *
 * x starts at 0
 */
void amd64_emitf(ir_node const *node, char const *fmt, ...);

void amd64_emit_function(ir_graph *irg);

#endif
