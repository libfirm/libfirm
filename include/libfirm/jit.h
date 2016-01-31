/*
 * This file is part of libFirm.
 * Copyright (C) 2015 Matthias Braun
 */

/**
 * @file
 * @brief       Just in time compilation
 */
#ifndef FIRM_JIT_H
#define FIRM_JIT_H

#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup be
 * @defgroup jit   Just in Time Compilation
 *
 * Provides interface to generate code and resolve symbols in memory buffers.
 * This is often called just in time compilation.
 * @{
 */

/**
 * Just in time compilation environment. Holds memory for several \ref
 * ir_jit_function_t.
 */
typedef struct ir_jit_segment_t ir_jit_segment_t;

/**
 * Just in time compiled function with potentially unresolved relocations.
 */
typedef struct ir_jit_function_t ir_jit_function_t;

/**
 * Create a new jit segment.
 */
FIRM_API ir_jit_segment_t *be_new_jit_segment(void);

/**
 * Destroy jit segment \p segment. Invalidates references to functions created in
 * the segment.
 */
FIRM_API void be_destroy_jit_segment(ir_jit_segment_t *segment);

/**
 * Set absolute address of global entities so relocations in jit compiled
 * code an be resolved.
 */
FIRM_API void be_jit_set_entity_addr(ir_entity *entity, void const *address);

/**
 * Return previously set address. \see be_jit_set_entity_addr()
 */
FIRM_API void const *be_jit_get_entity_addr(ir_entity const *entity);

/**
 * Compile graph \p irg to a sequence of machine instructions and relocations.
 */
FIRM_API ir_jit_function_t *be_jit_compile(ir_jit_segment_t *segment,
                                           ir_graph *irg);

/**
 * Return the buffer size necessary to emit \p function with be_emit_function().
 */
FIRM_API unsigned be_get_function_size(ir_jit_function_t const *function);

/**
 * Emit \p function into \p buffer and resolve symbols and relocations.
 */
FIRM_API void be_emit_function(char *buffer, ir_jit_function_t *function);

/** @} */

#include "end.h"

#endif
