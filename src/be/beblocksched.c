/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Block-scheduling strategies.
 * @author      Matthias Braun, Christoph Mallon
 * @date        27.09.2006
 *
 * The goals of the greedy (and ILP) algorithm here works by assuming that
 * we want to change as many jumps to fallthroughs as possible (executed jumps
 * actually, we have to look at the execution frequencies). The algorithms
 * do this by collecting execution frequencies of all branches (which is easily
 * possible when all critical edges are split) then removes critical edges where
 * possible as we don't need and want them anymore now. The algorithms then try
 * to change as many edges to fallthroughs as possible, this is done by setting
 * a next and prev pointers on blocks. The greedy algorithm sorts the edges by
 * execution frequencies and tries to transform them to fallthroughs in this order
 */
#include "beblocksched.h"

#include "bearch.h"
#include "beirg.h"
#include "bemodule.h"
#include "besched.h"
#include "debug.h"
#include "execfreq.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "pdeq.h"
#include "util.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static bool blocks_removed;

/**
 * Post-block-walker: Find blocks containing only one jump and
 * remove them.
 */
static void remove_empty_block(ir_node *block)
{
	if (irn_visited_else_mark(block))
		return;

	if (get_Block_n_cfgpreds(block) != 1)
		goto check_preds;

	ir_node *jump = NULL;
	sched_foreach(block, node) {
		if (!(arch_get_irn_flags(node) & arch_irn_flag_simple_jump))
			goto check_preds;
		/* we should never have 2 jumps in a block */
		if (jump)
			panic("found 2 jumps in a block");
		jump = node;
	}
	if (jump == NULL)
		goto check_preds;

	ir_entity *entity     = get_Block_entity(block);
	ir_node   *pred       = get_Block_cfgpred(block, 0);
	ir_node   *succ_block = NULL;
	foreach_out_edge_safe(jump, edge) {
		assert(succ_block == NULL);
		succ_block = get_edge_src_irn(edge);
		if (get_Block_entity(succ_block) != NULL && entity != NULL) {
			/* Currently we can add only one label for a block. Therefore we
			 * cannot combine them if both block already have one. :-( */
			goto check_preds;
		}

		int const pos = get_edge_src_pos(edge);
		set_irn_n(succ_block, pos, pred);
	}

	/* move the label to the successor block */
	set_Block_entity(succ_block, entity);

	/* there can be some non-scheduled Pin nodes left in the block, move them
	 * to the succ block (Pin) or pred block (Sync) */
	foreach_out_edge_safe(block, edge) {
		ir_node *const node = get_edge_src_irn(edge);

		if (node == jump) {
			continue;
		} else if (is_Pin(node)) {
			/* we simply kill Pins, because there are some strange interactions
			 * between jump threading, which produce PhiMs with Pins, we simply
			 * kill the pins here, everything is scheduled anyway */
			exchange(node, get_Pin_op(node));
		} else if (is_Sync(node)) {
			set_nodes_block(node, get_nodes_block(pred));
		} else if (is_End(node)) { /* End-keep, reroute it to the successor */
			int pos = get_edge_src_pos(edge);
			set_irn_n(node, pos, succ_block);
		} else {
			panic("unexpected node %+F in block %+F with empty schedule", node, block);
		}
	}

	ir_graph *irg = get_irn_irg(block);
	set_Block_cfgpred(block, 0, new_r_Bad(irg, mode_X));
	kill_node(jump);
	blocks_removed = true;

	/* check predecessor */
	remove_empty_block(get_nodes_block(pred));
	return;

check_preds:
	for (int i = 0, arity = get_Block_n_cfgpreds(block); i < arity; ++i) {
		ir_node *pred = get_Block_cfgpred_block(block, i);
		remove_empty_block(pred);
	}
}

/* removes basic blocks that just contain a jump instruction */
static void remove_empty_blocks(ir_graph *irg)
{
	blocks_removed = false;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	remove_empty_block(get_irg_end_block(irg));
	foreach_irn_in(get_irg_end(irg), i, pred) {
		if (is_Block(pred))
			remove_empty_block(pred);
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);

	if (blocks_removed)
		clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
}

typedef struct blocksched_entry_t blocksched_entry_t;
struct blocksched_entry_t {
	ir_node            *block;
	blocksched_entry_t *next;
	blocksched_entry_t *prev;
};

typedef struct edge_t edge_t;
struct edge_t {
	ir_node *block;                /**< source block */
	int      pos;                  /**< number of cfg predecessor (target) */
	double   execfreq;             /**< the frequency */
	/** for edges leaving the loop this is the penality when we make them a
	 * fallthrough. */
	double   outedge_penalty_freq;
	/** flag that indicates whether this edge is the edge with the highest
	 * execfreq pointing away from this block */
	int      highest_execfreq;
};

typedef struct blocksched_env_t blocksched_env_t;
struct blocksched_env_t {
	ir_graph       *irg;
	struct obstack  obst;
	edge_t         *edges;
	deq_t           worklist;
	unsigned        blockcount;
};

static blocksched_entry_t* get_blocksched_entry(const ir_node *block)
{
	return (blocksched_entry_t*)get_irn_link(block);
}

/**
 * Collect cfg frequencies of all edges between blocks.
 * Also determines edge with highest frequency.
 */
static void collect_egde_frequency(ir_node *block, void *data)
{
	blocksched_env_t *env = (blocksched_env_t*)data;

	/* Exclude the end block from the block schedule. */
	if (block == get_irg_end_block(env->irg))
		return;

	edge_t edge;
	memset(&edge, 0, sizeof(edge));

	blocksched_entry_t *entry = OALLOCZ(&env->obst, blocksched_entry_t);
	entry->block = block;
	set_irn_link(block, entry);

	int arity = get_Block_n_cfgpreds(block);
	if (arity == 0) {
		/* must be the start block, everything else is dead code and should be
		 * removed by now */
		assert(block == get_irg_start_block(env->irg));
		/* nothing to do here */
		return;
	} else if (arity == 1) {
		ir_loop *loop       = get_irn_loop(block);
		ir_node *pred_block = get_Block_cfgpred_block(block, 0);
		ir_loop *pred_loop  = get_irn_loop(pred_block);
		float    freq       = (float)get_block_execfreq(block);

		/* is it an edge leaving a loop */
		if (get_loop_depth(pred_loop) > get_loop_depth(loop)) {
			float pred_freq = (float)get_block_execfreq(pred_block);
			edge.outedge_penalty_freq = -(pred_freq - freq);
		}

		edge.block            = block;
		edge.pos              = 0;
		edge.execfreq         = freq;
		edge.highest_execfreq = 1;
		ARR_APP1(edge_t, env->edges, edge);
	} else {
		double highest_execfreq = -1.0;
		int    highest_edge_num = -1;

		edge.block = block;
		for (int i = 0; i < arity; ++i) {
			ir_node *const pred_block = get_Block_cfgpred_block(block, i);
			double   const execfreq   = get_block_execfreq(pred_block);

			edge.pos              = i;
			edge.execfreq         = execfreq;
			edge.highest_execfreq = 0;
			ARR_APP1(edge_t, env->edges, edge);

			if (highest_execfreq < execfreq) {
				highest_execfreq = execfreq;
				highest_edge_num = ARR_LEN(env->edges) - 1;
			}
		}

		if (highest_edge_num >= 0) {
			env->edges[highest_edge_num].highest_execfreq = 1;
		}
	}
}

static int cmp_edges_base(const edge_t *e1, const edge_t *e2)
{
	long nr1 = get_irn_node_nr(e1->block);
	long nr2 = get_irn_node_nr(e2->block);
	if (nr1 < nr2) {
		return 1;
	} else if (nr1 > nr2) {
		return -1;
	} else if (e1->pos < e2->pos) {
		return 1;
	} else if (e1->pos > e2->pos) {
		return -1;
	} else {
		return 0;
	}
}

static int cmp_edges(const void *d1, const void *d2)
{
	const edge_t *e1 = (const edge_t*)d1;
	const edge_t *e2 = (const edge_t*)d2;
	double        freq1 = e1->execfreq;
	double        freq2 = e2->execfreq;
	if (freq1 < freq2) {
		return 1;
	} else if (freq1 > freq2) {
		return -1;
	} else {
		return cmp_edges_base(e1, e2);
	}
}

static int cmp_edges_outedge_penalty(const void *d1, const void *d2)
{
	const edge_t *e1   = (const edge_t*)d1;
	const edge_t *e2   = (const edge_t*)d2;
	double        pen1 = e1->outedge_penalty_freq;
	double        pen2 = e2->outedge_penalty_freq;
	if (pen1 > pen2) {
		return 1;
	} else if (pen1 < pen2) {
		return -1;
	} else {
		return cmp_edges_base(e1, e2);
	}
}

static void clear_loop_links(ir_loop *loop)
{
	set_loop_link(loop, NULL);
	for (int i = 0, n = get_loop_n_elements(loop); i < n; ++i) {
		loop_element elem = get_loop_element(loop, i);
		if (*elem.kind == k_ir_loop) {
			clear_loop_links(elem.son);
		}
	}
}

static void coalesce_blocks(blocksched_env_t *env)
{
	/* sort interblock edges by execution frequency */
	edge_t *edges = env->edges;
	QSORT_ARR(edges, cmp_edges);

	/* run1: only look at jumps */
	size_t edge_count = ARR_LEN(edges);
	for (size_t i = 0; i < edge_count; ++i) {
		edge_t const *const edge = &edges[i];
		/* only check edge with highest frequency */
		if (!edge->highest_execfreq)
			continue;

		/* the block might have been removed already... */
		ir_node *const block = edge->block;
		if (is_Bad(get_Block_cfgpred(block, 0)))
			continue;

		ir_node *pred_block = get_Block_cfgpred_block(block, edge->pos);
		blocksched_entry_t *entry      = get_blocksched_entry(block);
		blocksched_entry_t *pred_entry = get_blocksched_entry(pred_block);

		/* is one of the blocks already attached to another block? */
		if (pred_entry->next != NULL || entry->prev != NULL)
			continue;

		/* only coalesce jumps */
		if (get_block_succ_next(pred_block, get_block_succ_first(pred_block)) != NULL)
			continue;

		/* schedule the two blocks behind each other */
		DB((dbg, LEVEL_1, "Coalesce (Jump) %+F -> %+F (%.3g)\n",
		           pred_entry->block, entry->block, edge->execfreq));
		pred_entry->next = entry;
		entry->prev      = pred_entry;
	}

	/* run2: pick loop fallthroughs */
	clear_loop_links(get_irg_loop(env->irg));

	QSORT_ARR(edges, cmp_edges_outedge_penalty);
	for (size_t i = 0; i < edge_count; ++i) {
		edge_t const *const edge = &edges[i];
		/* already seen all loop outedges? */
		if (edge->outedge_penalty_freq == 0)
			break;

		ir_node *const block = edge->block;
		/* the block might have been removed already... */
		ir_node *const pred = get_Block_cfgpred(block, edge->pos);
		if (is_Bad(pred))
			continue;

		ir_node            *const pred_block = get_nodes_block(pred);
		blocksched_entry_t *const entry      = get_blocksched_entry(block);
		blocksched_entry_t *const pred_entry = get_blocksched_entry(pred_block);

		if (pred_entry->next != NULL || entry->prev != NULL)
			continue;

		/* we want at most 1 outedge fallthrough per loop */
		ir_loop *loop = get_irn_loop(pred_block);
		if (get_loop_link(loop) != NULL)
			continue;

		/* schedule the 2 blocks behind each other */
		DB((dbg, LEVEL_1, "Coalesce (Loop Outedge) %+F -> %+F (%.3g)\n",
		           pred_entry->block, entry->block, edge->execfreq));
		pred_entry->next = entry;
		entry->prev      = pred_entry;

		/* all loops left have an outedge now */
		ir_loop *outer_loop = get_irn_loop(block);
		do {
			/* we set loop link to loop to mark it */
			set_loop_link(loop, loop);
			loop = get_loop_outer_loop(loop);
		} while (loop != outer_loop);
	}

	/* sort interblock edges by execution frequency */
	QSORT_ARR(edges, cmp_edges);

	/* run3: remaining edges */
	for (size_t i = 0; i < edge_count; ++i) {
		const edge_t *edge  = &edges[i];
		ir_node      *block = edge->block;

		/* the block might have been removed already... */
		ir_node *const pred = get_Block_cfgpred(block, edge->pos);
		if (is_Bad(pred))
			continue;

		ir_node            *const pred_block = get_nodes_block(pred);
		blocksched_entry_t *const entry      = get_blocksched_entry(block);
		blocksched_entry_t *const pred_entry = get_blocksched_entry(pred_block);

		/* is one of the blocks already attached to another block? */
		if (pred_entry->next != NULL || entry->prev != NULL)
			continue;

		/* schedule the two blocks behind each other */
		DB((dbg, LEVEL_1, "Coalesce (CondJump) %+F -> %+F (%.3g)\n",
		           pred_entry->block, entry->block, edge->execfreq));
		pred_entry->next = entry;
		entry->prev      = pred_entry;
	}
}

static void pick_block_successor(blocksched_entry_t *entry, blocksched_env_t *env)
{
	ir_node *const block = entry->block;
	if (irn_visited_else_mark(block))
		return;

	env->blockcount++;

	DB((dbg, LEVEL_1, "Pick succ of %+F\n", block));

	/* put all successors into the worklist */
	foreach_block_succ(block, edge) {
		ir_node *succ_block = get_edge_src_irn(edge);

		if (irn_visited(succ_block))
			continue;

		/* we only need to put the first of a series of already connected
		 * blocks into the worklist */
		blocksched_entry_t *succ_entry = get_blocksched_entry(succ_block);
		while (succ_entry->prev != NULL) {
			/* break cycles... */
			if (succ_entry->prev->block == succ_block) {
				succ_entry->prev->next = NULL;
				succ_entry->prev       = NULL;
				break;
			}
			succ_entry = succ_entry->prev;
		}

		if (irn_visited(succ_entry->block))
			continue;

		DB((dbg, LEVEL_1, "Put %+F into worklist\n", succ_entry->block));
		deq_push_pointer_right(&env->worklist, succ_entry->block);
	}

	if (entry->next != NULL) {
		pick_block_successor(entry->next, env);
		return;
	}

	DB((dbg, LEVEL_1, "deciding...\n"));
	double best_succ_execfreq = -1;

	/* no successor yet: pick the successor block with the highest execution
	 * frequency which has no predecessor yet */

	ir_node *succ = NULL;
	foreach_block_succ(block, edge) {
		ir_node *const succ_block = get_edge_src_irn(edge);
		if (irn_visited(succ_block))
			continue;

		blocksched_entry_t *const succ_entry = get_blocksched_entry(succ_block);
		if (succ_entry->prev != NULL)
			continue;

		double execfreq = get_block_execfreq(succ_block);
		if (best_succ_execfreq < execfreq) {
			best_succ_execfreq = execfreq;
			succ               = succ_block;
		}
	}

	if (succ == NULL) {
		DB((dbg, LEVEL_1, "pick from worklist\n"));

		do {
			if (deq_empty(&env->worklist)) {
				DB((dbg, LEVEL_1, "worklist empty\n"));
				return;
			}
			succ = deq_pop_pointer_left(ir_node, &env->worklist);
		} while (irn_visited(succ));
	}

	blocksched_entry_t *const succ_entry = get_blocksched_entry(succ);
	entry->next      = succ_entry;
	succ_entry->prev = entry;

	pick_block_successor(succ_entry, env);
}

static blocksched_entry_t *finish_block_schedule(blocksched_env_t *env)
{
	ir_graph *const irg = env->irg;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	/* Exclude the end block from the block schedule. */
	mark_irn_visited(get_irg_end_block(irg));

	deq_init(&env->worklist);
	ir_node            *const startblock = get_irg_start_block(irg);
	blocksched_entry_t *const entry      = get_blocksched_entry(startblock);
	pick_block_successor(entry, env);
	assert(deq_empty(&env->worklist));
	deq_free(&env->worklist);

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);

	return entry;
}

static ir_node **create_blocksched_array(blocksched_env_t *const env)
{
	DB((dbg, LEVEL_1, "Blockschedule:\n"));

	unsigned                  i          = 0;
	blocksched_entry_t *const first      = finish_block_schedule(env);
	unsigned            const count      = env->blockcount;
	struct obstack     *const obst       = be_get_be_obst(env->irg);
	ir_node           **const block_list = NEW_ARR_D(ir_node*, obst, count);
	for (blocksched_entry_t const *entry = first; entry; entry = entry->next) {
		assert(i < count);
		block_list[i++] = entry->block;
		DB((dbg, LEVEL_1, "\t%+F\n", entry->block));
	}
	assert(i == count);

	return block_list;
}

ir_node **be_create_block_schedule(ir_graph *irg)
{
	blocksched_env_t env = {
		.irg        = irg,
		.edges      = NEW_ARR_F(edge_t, 0),
		.blockcount = 0,
	};
	obstack_init(&env.obst);

	assure_loopinfo(irg);

	// collect edge execution frequencies
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_block_walk_graph(irg, collect_egde_frequency, NULL, &env);

	remove_empty_blocks(irg);

	coalesce_blocks(&env);

	ir_node **const block_list = create_blocksched_array(&env);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	DEL_ARR_F(env.edges);
	obstack_free(&env.obst, NULL);

	return block_list;
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_blocksched)
void be_init_blocksched(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.blocksched");
}
