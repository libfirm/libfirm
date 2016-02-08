/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Matthias Braun
 */

/**
 * @file
 * @brief       ia32 binary encoding/emissiong
 * @author      Matthias Braun, Christoph Mallon
 */
#ifndef FIRM_BE_IA32_IA32_ENCODE_H
#define FIRM_BE_IA32_IA32_ENCODE_H

#include "firm_types.h"
#include "jit.h"

enum {
	IA32_RELOCATION_RELJUMP = 128,
};

ir_jit_function_t *ia32_emit_jit(ir_jit_segment_t *segment, ir_graph *irg);

void ia32_emit_jit_function(char *buffer, ir_jit_function_t *function);

#endif
