/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file implements the mips node emitter.
 * @author      Christoph Mallon
 */
#ifndef FIRM_BE_MIPS_MIPS_EMITTER_H
#define FIRM_BE_MIPS_MIPS_EMITTER_H

#include "irnode.h"

/**
 * fmt  parameter               output
 * ---- ----------------------  ---------------------------------------------
 * %C   int (mips_cond_t)       condition code
 * %Dx  <node>                  destination register x
 * %I   <node>                  immediate
 * %R   arch_register_t const*  register
 * %Sx  <node>                  source register x
 *
 * x starts at 0
 */
void mips_emitf(ir_node const *node, char const *fmt, ...);

void mips_emit_function(ir_graph *irg);

#endif
