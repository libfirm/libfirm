/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Various irnode constructors.  Automatic construction
 *            of SSA representation. Private Header
 * @author    Martin Trapp, Christian Schaefer, Michael Beck
 */
#ifndef FIRM_IR_IRCONS_T_H
#define FIRM_IR_IRCONS_T_H

#include "ircons.h"
#include "irgraph_t.h"
#include "irverify_t.h"
#include "compiler.h"

#define verify_new_node(node) verify_new_node_((node))
#define get_cur_block()       _get_cur_block()

static inline ir_node *_get_cur_block(void)
{
	return current_ir_graph->current_block;
}

static inline void verify_new_node_(ir_node *const node)
{
#ifdef DEBUG_libfirm
	if (UNLIKELY(!irn_verify(node)))
		abort();
#else
	(void)node;
#endif
}

/**
 * Creates a new Anchor node.
 */
ir_node *new_r_Anchor(ir_graph *irg);

/** create new block node without immediately optimizing it.
 * This is an internal helper function for new_ir_graph() */
ir_node *new_r_Block_noopt(ir_graph *irg, int arity, ir_node *in[]);

/**
 * Allocate a frag array for a node if the current graph state is phase_building.
 *
 * @param irn         the node for which the frag array should be allocated
 * @param op          the opcode of the (original) node, if does not match opcode of irn,
 *                    nothing is done
 * @param frag_store  the address of the frag store in irn attributes, if this
 *                    address contains a value != NULL, does nothing
 */
void firm_alloc_frag_arr(ir_node *irn, ir_op *op, ir_node ***frag_store);

/**
 * Restarts SSA construction on the given graph with n_loc
 * new values.
 *
 * @param irg    the graph on which the SSA construction is restarted
 * @param n_loc  number of new variables
 *
 * After this function is complete, the graph is in phase_building
 * again and set_value()/get_value() and mature_block() can be used
 * to construct new values.
 *
 * @note do not use get_mem()/set_mem() they will build a new memory
 *       instead of modifying the old one which might be not what you expect...
 */
void ssa_cons_start(ir_graph *irg, int n_loc);

/**
 * Finalize the (restarted) SSA construction. Matures all blocks that are
 * not matured yet and reset the graph state to phase_high.
 *
 * @param irg    the graph on which the SSA construction was restarted
 */
void ssa_cons_finish(ir_graph *irg);

ir_node *new_rd_Const_null(dbg_info *dbgi, ir_graph *irg, ir_mode *mode);

ir_node *new_r_Const_null(ir_graph *irg, ir_mode *mode);

ir_node *new_rd_Const_one(dbg_info *dbgi, ir_graph *irg, ir_mode *mode);

ir_node *new_r_Const_one(ir_graph *irg, ir_mode *mode);

#endif
