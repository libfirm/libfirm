/*
 * This file is part of libFirm.
 * Copyright (C) 2016 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Helper functions for emitting assembly from a firm graph.
 * @author      Matthias Braun
 *
 * You typically register an emission function for each node type of your
 * backend with be_set_emitter().
 */
#ifndef FIRM_BE_BEEMITHLP_H
#define FIRM_BE_BEEMITHLP_H

#include <assert.h>
#include "be.h"
#include "irop_t.h"
#include "irnode_t.h"

/**
 * Emit spaces until the comment position is reached.
 */
void be_emit_pad_comment(void);

/**
 * The type of a emitter function.
 */
typedef void emit_func(ir_node const *node);

static inline void be_set_emitter(ir_op *const op, emit_func *const func)
{
	set_generic_function_ptr(op, func);
}

void be_init_emitters(void);

void be_emit_nothing(ir_node const *node);

/**
 * Emit code for a node by calling a handler registered with be_set_emitter().
 */
void be_emit_node(ir_node const *node);

/**
 * Set irn links of blocks to point to the predecessor blocks in the given
 * blockschedule and set irn_links of mode_X nodes to the block using them.
 * This function expects that you require the IR_RESOURCE_IRN_LINK prior
 * to using it.
 */
void be_emit_init_cf_links(ir_node **block_schedule);

/**
 * Returns the target block for a control flow node.
 * Requires a prior call to be_emit_init_cf_links().
 */
static inline ir_node *be_emit_get_cfop_target(ir_node const *const irn)
{
	assert(get_irn_mode(irn) == mode_X);
	return (ir_node*)get_irn_link(irn);
}

/**
 * Returns the previous block in the block schedule.
 * Requires a prior call to be_emit_get_cfop_target().
 */
static inline ir_node *be_emit_get_prev_block(ir_node const *const block)
{
	assert(is_Block(block));
	return (ir_node*)get_irn_link(block);
}

typedef struct be_cond_branch_projs_t {
	ir_node *f;
	ir_node *t;
} be_cond_branch_projs_t;

be_cond_branch_projs_t be_get_cond_branch_projs(ir_node const *node);

/**
 * Emit the target label for a control flow node.
 */
void be_emit_cfop_target(ir_node const *jmp);

void be_emit_cfop_target_pos(ir_node const *jmp, unsigned pos);

bool be_is_fallthrough(ir_node const *jmp);

 /**
  * fmt parameter     output
  * --- ------------  -------------------
  * %%                %
  * %L  <node>        control flow target
  * %d  int           int
  * %s  char const*   string
  * %u  unsigned int  unsigned int
  */
#define BE_EMITF(node, fmt, ap, in_delay_slot) \
	va_list ap; \
	va_start(ap, fmt); \
	be_emit_char('\t'); \
	if (in_delay_slot) \
		be_emit_char(' '); \
	for (size_t node##__n;;) \
		if (node##__n = strcspn(fmt, "\n%"), be_emit_string_len(fmt, node##__n), fmt += node##__n, *fmt == '\0') { \
			be_emit_finish_line_gas(node); \
			va_end(ap); \
			break; \
		} else if (*fmt == '\n') { \
			++fmt; \
			be_emit_finish_line_gas(node); \
			be_emit_char('\t'); \
		} else if (*++fmt == '%') { \
			++fmt; \
			be_emit_char('%'); \
		} else if (*fmt == 'L') { \
			++fmt; \
			be_emit_cfop_target(va_arg(ap, ir_node const*)); \
		} else if (*fmt == 'd') { \
			++fmt; \
			int const num = va_arg(ap, int); \
			be_emit_irprintf("%d", num); \
		} else if (*fmt == 's') { \
			++fmt; \
			char const *const string = va_arg(ap, char const*); \
			be_emit_string(string); \
		} else if (*fmt == 'u') { \
			++fmt; \
			unsigned const num = va_arg(ap, unsigned); \
			be_emit_irprintf("%u", num); \
		} else

#define BE_EMIT_JMP(arch, node, name, jmp) \
	if (be_is_fallthrough(jmp)) { \
		if (be_options.verbose_asm) \
			arch##_emitf(node, "/* fallthrough to %L */", jmp); \
	} else if (arch##_emitf(node, name " %L", jmp), 0) {} else

#endif
