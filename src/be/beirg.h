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
#include "irgraph_t.h"

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

/**
 * An ir_graph with additional analysis data about this irg. Also includes some
 * backend structures
 */
typedef struct be_irg_t {
	be_main_env_t    *main_env;
	be_lv_t          *lv;
	/** bitset of registers available for the allocator */
	unsigned         *allocatable_regs;
	/** bitset of registers for which verification errors are not reported.
	 * A typical use is for the stackpointer for which SSA form is only built
	 * once late in the compilation. May be NULL. */
	const unsigned   *non_ssa_regs;
	/** obstack (mainly used to keep register constraints which we can't keep
	 * in the irg obst, because it gets replaced during code selection) */
	struct obstack    obst;
	/** Architecture specific per-graph data */
	void             *isa_link;
	bool              has_returns_twice_call;
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

static inline void record_returns_twice(ir_graph *const irg, ir_type *const type)
{
	if (get_method_additional_properties(type) & mtp_property_returns_twice)
		be_birg_from_irg(irg)->has_returns_twice_call = true;
}

#endif
