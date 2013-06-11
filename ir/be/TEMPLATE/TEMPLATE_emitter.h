/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    declarations for emit functions
 */
#ifndef FIRM_BE_TEMPLATE_TEMPLATE_EMITTER_H
#define FIRM_BE_TEMPLATE_TEMPLATE_EMITTER_H

#include "irargs_t.h"
#include "irnode.h"
#include "debug.h"

#include "bearch.h"
#include "beemitter.h"

#include "bearch_TEMPLATE_t.h"

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
 * %I   <node>                  immediate
 * %L   <node>                  target label
 */
void TEMPLATE_emitf(const ir_node *node, const char *format, ...);

void TEMPLATE_emit_function(ir_graph *irg);

#endif
