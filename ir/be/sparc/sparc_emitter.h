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
 * %Dx   <node>                  destination register x
 * %E    <node>                  entity+offset
 * %F    <node>                  floating point mode
 * %H    <node>                  high immediate
 * %L    <node>                  control flow target of the node
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

void sparc_emit_routine(ir_graph *irg);

void sparc_init_emitter(void);

#endif
