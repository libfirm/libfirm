/**
 * Interblock liveness analysis.
 * @author Sebastian Hack
 * @date 6.12.2004
 */

#include "irouts.h"
#include "irgwalk.h"
#include "irprintf.h"

#include "beutil.h"
#include "belive_t.h"

/** The offset of the liveness information in a firm node. */
size_t live_irn_data_offset = 0;

void be_liveness_init(void)
{
	live_irn_data_offset = register_additional_node_data(sizeof(live_info_t));
}

int (is_live_in)(const ir_node *block, const ir_node *irn)
{
	return _is_live_in(block, irn);
}

int (is_live_out)(const ir_node *block, const ir_node *irn)
{
	return _is_live_in(block, irn);
}

static INLINE void mark_live_in(ir_node *block, const ir_node *irn)
{
	block_live_info_t *info = get_block_live_info(block);
	pset_insert_ptr(info->in, irn);
}

static INLINE void mark_live_out(ir_node *block, const ir_node *irn)
{
	block_live_info_t *info = get_block_live_info(block);
	pset_insert_ptr(info->out, irn);
}

/**
 * Mark a node (value) live out at a certain block. Do this also
 * transitively, i.e. if the block is not the block of the value's
 * definition, all predecessors are also marked live.
 * @param def The node (value).
 * @param block The block to mark the value live out of.
 * @param visited A set were all visited blocks are recorded.
 */
static void live_out_at_block(ir_node *def, ir_node *block, pset *visited)
{
	if(pset_find_ptr(visited, block))
		return;

	pset_insert_ptr(visited, block);
	mark_live_out(block, def);

	/*
	 * If this block is not the definition block, we have to go up
	 * further.
	 */
	if(get_nodes_block(def) != block) {
		int i, n;

		mark_live_in(block, def);

		for(i = 0, n = get_irn_arity(block); i < n; ++i)
			live_out_at_block(def, get_nodes_block(get_irn_n(block, i)), visited);
	}
}

/**
 * Liveness analysis for a value.
 * This functions is meant to be called by a firm walker, to compute the
 * set of all blocks a value is live in.
 * @param irn The node (value).
 * @param env Ignored.
 */
static void liveness_for_node(ir_node *irn, void *env)
{
	int i, n;
	ir_node *def_block;
	pset *visited;

	/* Don't compute liveness information fornon-data nodes. */
	if(!is_data_node(irn))
		return;

	visited = pset_new_ptr(512);
	def_block = get_nodes_block(irn);

	/* Go over all uses of the value */
	for(i = 0, n = get_irn_n_outs(irn); i < n; ++i) {
		ir_node *use = get_irn_out(irn, i);
		ir_node *use_block;

		/*
		 * If the usage is no data node, skip this use, since it does not
		 * affect the liveness of the node.
		 */
		if(!is_data_node(use))
			continue;

		/* Get the block where the usage is in. */
		use_block = get_nodes_block(use);

		/*
		 * If the block of the definition equals the use block, we can skip
		 * the following computations, since this use is local to a block.
		 */
		if(def_block == use_block)
			continue;

		/*
		 * If the use is a phi function, determine the corresponding block
		 * through which the value reaches the phi function and mark the
		 * value as live out of that block.
		 */
		if(is_Phi(use)) {
			int i, n;

			for(i = 0, n = get_irn_arity(use); i < n; ++i) {
				if(get_irn_n(use, i) == irn) {
					ir_node *pred_block = get_nodes_block(get_irn_n(use_block, i));
					live_out_at_block(irn, pred_block, visited);
				}
			}
		}

		/*
		 * Else, the value is live in at this block. Mark it and call live
		 * out on the predecessors.
		 */
		else {
			int i, n;

			mark_live_in(use_block, irn);

			for(i = 0, n = get_irn_arity(use_block); i < n; ++i) {
				ir_node *pred_block = get_nodes_block(get_irn_n(use_block, i));
				live_out_at_block(irn, pred_block, visited);
			}
		}
	}

	del_pset(visited);
}

static void create_sets(ir_node *block, void *env)
{
	block_live_info_t *info = get_block_live_info(block);

	info->in = pset_new_ptr(128);
	info->out = pset_new_ptr(128);
}

void be_liveness(ir_graph *irg)
{
	irg_block_walk_graph(irg, create_sets, NULL, NULL);
	irg_walk_graph(irg, liveness_for_node, NULL, NULL);
}


static void liveness_dump(ir_node *block, void *env)
{
	FILE *f = env;
	block_live_info_t *info = get_block_live_info(block);

	assert(is_Block(block) && "Need a block here");

	ir_fprintf(f, "liveness at block %n\n", block);
	ir_fprintf(f, "\tlive  in: %*n\n", pset_iterator, info->in);
	ir_fprintf(f, "\tlive out: %*n\n", pset_iterator, info->out);
}

void be_liveness_dump(FILE *f, ir_graph *irg)
{
	irg_block_walk_graph(irg, liveness_dump, NULL, f);
}
