/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   restarting SSA construction for values.
 * @author  Michael Beck
 */
#include "ircons_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"

/** Note: start and finish must use the same kind of walker */
static void (*ssa_cons_walker)(ir_graph *, irg_walk_func *, irg_walk_func *, void *)
	= irg_block_walk_graph;

/**
 * Post-walker: prepare the graph nodes for new SSA construction cycle by
 * allocation new arrays.
 */
static void prepare_blocks(ir_node *block, void *env)
{
	(void)env;
	ir_graph *const irg   = get_irn_irg(block);
	unsigned  const n_loc = irg->n_loc;
	/* reset mature flag */
	if (block != get_irg_start_block(irg))
		set_Block_matured(block, 0);
	block->attr.block.graph_arr = NEW_ARR_DZ(ir_node*, get_irg_obstack(irg), n_loc);
	set_Block_phis(block, NULL);
}

void ssa_cons_start(ir_graph *irg, int n_loc)
{
	add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION);

	irg_set_nloc(irg, n_loc);

	/*
	 * Note: we could try to reuse existing frag arrays, but it does not
	 * seems worth to do this.  First, we have to check if they really exists and
	 * then clear them.  We do not expect SSA construction is used often.
	 */
	ir_reserve_resources(irg, IR_RESOURCE_PHI_LIST);
	ssa_cons_walker(irg, NULL, prepare_blocks, NULL);
}

/**
 * mature all immature Blocks.
 */
static void finish_block(ir_node *block, void *env)
{
	(void)env;
	ir_graph *irg = get_irn_irg(block);
	if (block != get_irg_start_block(irg))
		mature_immBlock(block);
}

void ssa_cons_finish(ir_graph *irg)
{
	ssa_cons_walker(irg, NULL, finish_block, NULL);
	irg_finalize_cons(irg);
	ir_free_resources(irg, IR_RESOURCE_PHI_LIST);
}
