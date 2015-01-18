/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Backend irg - a ir_graph with additional analysis information.
 * @author      Matthias Braun
 * @date        05.05.2006
 */
#ifndef FIRM_BE_BEIRG_H
#define FIRM_BE_BEIRG_H

#include "be.h"
#include "be_types.h"
#include "be_t.h"
#include "irtypes.h"

void be_assure_live_sets(ir_graph *irg);
void be_assure_live_chk(ir_graph *irg);
/**
 * Liveness is invalid (call when nodes have been added but the control
 * flow has not been changed)
 */
void be_invalidate_live_sets(ir_graph *irg);
/**
 * Call when control flow has changed.
 * be_invalidate_live_sets() is called.
 */
void be_invalidate_live_chk(ir_graph *irg);

/**
 * frees all memory allocated by birg structures (liveness, ...).
 * The memory of the birg structure itself is not freed.
 */
void be_free_birg(ir_graph *irg);

/** The number of parts of the stack layout. */
#define N_FRAME_TYPES 3

/**
 * This type describes the stack layout.
 * The stack is divided into 3 parts:
 * - arg_type:     A struct type describing the stack arguments and its order.
 * - between_type: A struct type describing the stack layout between arguments
 *                 and frame type. In architectures that put the return address
 *                 automatically on the stack, the return address is put here.
 * - frame_type:   A class type describing the frame layout.
 */
struct be_stack_layout_t {
	ir_type *arg_type;             /**< A type describing the stack argument layout. */
	ir_type *between_type;         /**< A type describing the "between" layout. */
	ir_type *frame_type;           /**< The frame type. */

	ir_type *order[N_FRAME_TYPES]; /**< arg, between and frame types ordered. */

	int initial_offset;            /**< the initial difference between stack pointer and frame pointer */
	int initial_bias;              /**< the initial stack bias */
	bool sp_relative : 1;          /**< entities are addressed relative to
	                                    stack pointer (omit-fp mode) */
};

/**
 * An ir_graph with additional analysis data about this irg. Also includes some
 * backend structures
 */
typedef struct be_irg_t {
	be_main_env_t             *main_env;
	be_lv_t                   *lv;
	be_stack_layout_t          stack_layout;
	unsigned                  *allocatable_regs; /**< registers available for the
	                                                  allocator */
	struct obstack             obst;             /**< birg obstack (mainly used to keep
	                                                  register constraints which we can't keep
	                                                  in the irg obst, because it gets replaced
	                                                  during code selection) */
	void                      *isa_link;         /**< architecture specific per-graph data*/
} be_irg_t;

static inline be_irg_t *be_birg_from_irg(const ir_graph *irg)
{
	return (be_irg_t*) irg->be_data;
}

static inline be_main_env_t *be_get_irg_main_env(const ir_graph *irg)
{
	return be_birg_from_irg(irg)->main_env;
}

static inline be_lv_t *be_get_irg_liveness(const ir_graph *irg)
{
	return be_birg_from_irg(irg)->lv;
}

static inline struct obstack *be_get_be_obst(const ir_graph *irg)
{
	be_irg_t       *const birg = be_birg_from_irg(irg);
	struct obstack *const obst = &birg->obst;
	assert(obstack_object_size(obst) == 0);
	return obst;
}

static inline be_stack_layout_t *be_get_irg_stack_layout(const ir_graph *irg)
{
	return &be_birg_from_irg(irg)->stack_layout;
}

#endif
