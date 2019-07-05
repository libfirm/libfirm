/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file implements the vhdl node emitter.
 */
#ifndef FIRM_BE_VHDL_VHDL_EMITTER_H
#define FIRM_BE_VHDL_VHDL_EMITTER_H

#include "irnode.h"

void vhdl_emitf(ir_node const *node, char const *fmt, ...);

void vhdl_emit_function(ir_graph *irg);

#endif
