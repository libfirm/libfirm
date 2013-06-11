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

#include "irargs_t.h"
#include "irnode.h"
#include "debug.h"

#include "bearch.h"
#include "beemitter.h"

#include "bearch_sparc_t.h"

/**
 * fmt   parameter               output
 * ----  ----------------------  -------------------------------
 * %%                            %
 * %A    <node>                  emit ",a" in delay slot annul mode
 * %Dx   <node>                  destination register x
 * %E    <node>                  entity+offset
 * %F    <node>                  floating point mode
 * %H    <node>                  high immediate
 * %L    ir_node*                control flow target of the node
 * %ML   <node>                  load mode
 * %MS   <node>                  store mode
 * %R    arch_register_t const*  register
 * %Sx   <node>                  source register x
 * %SIx  <node>                  immediate or source register x
 * %d    signed int              signed int
 * %s    const char*             string
 * %u    unsigned int            unsigned int
 *
 * x starts at 0
 * %#M prints load modeu
 * + may be used with %d and %u
 */
void sparc_emitf(ir_node const *node, char const *fmt, ...);

void sparc_emit_function(ir_graph *irg);

void sparc_init_emitter(void);

#endif
