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
 * execution frequencies and tries to transform them to fallthroughs in this order.
 *
 * The random algorithm schedules the blocks in a non-deterministic pseudorandom order.
 */

#include "beblocksched.h"

#include "bearch.h"
#include "beirg.h"
#include "bemodule.h"
#include "benode.h"
#include "besched.h"
#include "debug.h"
#include "execfreq_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irtools.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "pdeq.h"
#ifdef _WIN32
	#include <time.h>
#else
	#include <sys/time.h>
#endif
#include "target_t.h"
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
	}
	assert(i == count);

	return block_list;
}

static ir_node **be_create_normal_block_schedule(ir_graph *irg)
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


static ir_node *start_block, *end_block;

static void count_blocks(ir_node *irn, void *env)
{
	if (irn == end_block) return;
	unsigned int *counter = (unsigned int*) env;
	(*counter)++;
}

static void collect_blocks(ir_node *irn, void *env)
{
	if (irn == start_block || irn == end_block) return;
	ir_node ***next = (ir_node***) env;
	**next = irn;
	(*next)++;
}

static ir_node **be_create_random_block_schedule(ir_graph *irg)
{
	remove_empty_blocks(irg);
	start_block = get_irg_start_block(irg);
	end_block = get_irg_end_block(irg);

	// count basic blocks
	unsigned int block_count = 0;
	irg_block_walk_graph(irg, count_blocks, NULL, &block_count);

	// create list of basic blocks
	struct obstack *const obst = be_get_be_obst(irg);
	ir_node **const block_list = NEW_ARR_D(ir_node*, obst, block_count);
	ir_node **next = block_list;
	*next++ = start_block; // schedules have to begin with start_block
	irg_block_walk_graph(irg, collect_blocks, NULL, &next);

	// shuffle list
	for (size_t i=1; i < block_count-1; i++) {
		size_t j = i + rand() / (RAND_MAX / (block_count - i) + 1);
		ir_node *t = block_list[j];
		block_list[j] = block_list[i];
		block_list[i] = t;
	}
	return block_list;
}

/*
 * Estimate size of block in machine code in bytes.
 */
static int get_block_size(ir_node *block)
{
	// try to estimate block size in final binary
	int size = 0;
	sched_foreach(block, node) {
		ir_op *const op = get_irn_op(node);
		if (op == op_Phi || op == op_be_Keep || op == op_be_Start
				||op == op_be_Unknown) continue;
		unsigned int node_size = ir_target.isa->get_op_estimated_size(node);
		size += node_size;
	}
	return size;
}

/*
 * Data structures used by the ExtTSP algorithm.
 */
typedef double exttsp_score_t;
typedef struct exttsp_edge_t exttsp_edge_t;
typedef struct exttsp_block_t exttsp_block_t;
typedef struct exttsp_chain_t exttsp_chain_t;
typedef struct exttsp_env_t exttsp_env_t;

struct exttsp_edge_t {
	exttsp_block_t *src;       // cfg source of edge
	exttsp_block_t *dest;      // cfg destination of edge
	exttsp_edge_t *next;       // next edge of corresponding block
	double freq;               // frequency of edge
};

struct exttsp_block_t {
	int size;                  // machine code size in bytes
	int addr_offset;           // address of block in memory relative to chain
	exttsp_block_t *succ;      // next block in chain
	exttsp_edge_t *first_edge; // first outgoing cfg edge
	exttsp_chain_t *chain;     // chain the block is in
	ir_node *irn;              // corresponding node
};

struct exttsp_chain_t {
	exttsp_block_t *first;     // first block of chain
	exttsp_block_t *last;      // last block of chain
	int length;                // number of blocks in chain
	int bytes;                 // length of blocks in chain in byte
	int addr_base;             // address of the chain in memory
	exttsp_score_t score;      // cached score of the chain
	bool contains_start;       // if the chain begins with the start block
};

struct exttsp_env_t {          // data needed by graph walkers
	ir_graph *irg;
	exttsp_chain_t *chains;
	exttsp_block_t *blocks;
	exttsp_edge_t  *edges;
	int chain_count;
	int block_count;
	int edge_count;
	char **adjacent;
};

/*
 * Constants for the ExtTSP algorithm.
 */
const double FALLTHROUGH_WEIGHT = 1.0;
const double FORWARD_WEIGHT     = 0.1;
const double BACKWARD_WEIGHT    = 0.1;
const int FORWARD_DISTANCE      = 1024;
const int BACKWARD_DISTANCE     = 640;
const int MAXIMAL_SPLIT_LENGTH  = 128;

/*
 * Calculate the contribution of an edge to the ExtTSP score of a chain.
 */
static exttsp_score_t exttsp_rate_edge(const exttsp_edge_t *edge)
{
	const exttsp_block_t *s = edge->src;
	const exttsp_block_t *d = edge->dest;
	const int jump_addr = s->chain->addr_base + s->addr_offset + s->size;
	const int dest_addr = d->chain->addr_base + d->addr_offset;
	const double freq = edge->freq;
	double probability;
	// fallthrough edge
	if (jump_addr == dest_addr) {
		return freq * FALLTHROUGH_WEIGHT;
	}
	// forward edge
	if (jump_addr < dest_addr) {
		const int distance = dest_addr - jump_addr;
		if (distance <= FORWARD_DISTANCE) {
			probability = 1.0 - ((double)distance)/((double)FORWARD_DISTANCE);
			return freq * FORWARD_WEIGHT * probability;
		}
		return 0;
	}
	// backward edge
	const int distance = jump_addr - dest_addr;
	if (distance <= BACKWARD_DISTANCE) {
		probability = 1.0 - ((double)distance)/((double)BACKWARD_DISTANCE);
		return freq * BACKWARD_WEIGHT * probability;
	}
	return 0;
}

/*
 * Calculate the ExtTSP score for a chain.
 * Only jumps within the chain or to jump_{a,b} are considered.
 */
static exttsp_score_t exttsp_rate_chain(const exttsp_chain_t *chain,
	const exttsp_chain_t *jump_a, const exttsp_chain_t *jump_b)
{
	exttsp_block_t *block = chain->first;
	exttsp_edge_t *edge;
	exttsp_chain_t *c;
	exttsp_score_t score = 0;
	while (block != NULL) {
		edge = block->first_edge;
		while (edge != NULL) {
			c = edge->dest->chain;
			if (c == chain || c == jump_a || c == jump_b) {
				score += exttsp_rate_edge(edge);
			}
			edge = edge->next;
		}
		block = block->succ;
	}
	return score;
}

/*
 * Count blocks and edges and set irn links of blocks to NULL.
 * The end block is ignored.
 */
static void exttsp_init_counters(ir_node *block, void *env)
{
	exttsp_env_t *e = (exttsp_env_t*) env;
	ir_node *end = get_irg_end_block(e->irg);
	if (block == end) return;
	e->chain_count++;
	e->block_count++;
	foreach_block_succ(block, edge) {
		if (get_edge_src_irn(edge) == end) continue;
		e->edge_count++;
	}
	set_irn_link(block, NULL);
}

/*
 * Create initial chains consisting of one basic block each.
 * The end block is ignored.
 */
static void exttsp_create_chains(ir_node *block, void *env)
{
	exttsp_env_t *e = (exttsp_env_t*) env;
	const ir_node *end = get_irg_end_block(e->irg);
	if (block == end) return;
	// create block
	exttsp_block_t *b = (exttsp_block_t*) get_irn_link(block);
	if (b == NULL) { // no edge to this block occured yet
		b = &(e->blocks[e->block_count++]);
	}
	b->size = get_block_size(block);
	b->addr_offset = 0;
	b->succ = NULL;
	b->first_edge = NULL;
	b->irn = block;
	set_irn_link(block, (void*)b);
	// create chain containing block
	exttsp_chain_t *c = &(e->chains[e->chain_count++]);
	c->first = c->last = b;
	c->length = 1;
	c->bytes = b->size;
	c->addr_base = 0;
	c->contains_start = (block == get_irg_start_block(e->irg));
	b->chain = c;
	// create outgoing edges
	exttsp_edge_t *last = NULL;
	foreach_block_succ(block, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		if (succ == end) continue; // ignore edges to end block
		exttsp_edge_t *succ_edge = &(e->edges[e->edge_count++]);
		succ_edge->src = b;
		exttsp_block_t *succ_block = (exttsp_block_t*) get_irn_link(succ);
		if (succ_block == NULL) { // succ block does not exist yet
			succ_block = &(e->blocks[e->block_count++]);
			set_irn_link(succ, (void*)succ_block);
		}
		succ_edge->dest = succ_block;
		if (last == NULL) { // current edge is the first
			b->first_edge = succ_edge;
		} else {
			last->next = succ_edge;
		}
		last = succ_edge;
		succ_edge->next = NULL; // list of edges is NULL terminated
		succ_edge->freq = get_edge_execfreq(edge);
		// fill adjacency matrix
		if (b != succ_block) {
			int x = b - e->blocks;
			int y = succ_block - e->blocks;
			e->adjacent[x][y] = 1;
			e->adjacent[y][x] = 1;
		}
	}
}

/*
 * Move first block of chain 'from' into 'into'.
 */
#define pop_from_into(from, into) {             \
    (into).last = (from)->first;                \
    (from)->first = (from)->first->succ;        \
    (into).last->succ = NULL;                   \
    (into).last->chain = &(into);               \
    (into).bytes += (into).last->size;          \
    (from)->bytes -= (into).last->size;         \
    (from)->addr_base -= (into).last->size; /*countervail error in block addresses*/\
}

/*
 * Merge chains in given order and compare the ExtTSP score with maximum score.
 */
#define consider_merged_score(x, y, z, type) {  \
	(x)->last->succ = (y)->first;               \
	(y)->last->succ = (z)->first;               \
	(y)->addr_base += (x)->bytes;               \
	(z)->addr_base += (x)->bytes + (y)->bytes;  \
	score = exttsp_rate_chain((x), (y), (z));   \
	if (score >= max_score) {                   \
		max_score = score;                      \
		merge_type = i*5+type;                  \
	}                                           \
	(x)->last->succ = NULL;                     \
	(y)->last->succ = NULL;                     \
	(y)->addr_base -= (x)->bytes;               \
	(z)->addr_base -= (x)->bytes + (y)->bytes;  \
}

/*
 * Compute gain in ExtTSP score achieved by merging two chains.
 * Only merges which keep the start block at begin are considered.
 */
static exttsp_score_t exttsp_compute_merge_gain(exttsp_chain_t *a,
	exttsp_chain_t *b, int *merge_type_cache)
{
	exttsp_score_t score, max_score = 0;
	int merge_type = -1;

	// try simple concatenation of a and b
	if (!b->contains_start) {
		a->last->succ = b->first;
		b->addr_base = a->bytes;
		max_score = exttsp_rate_chain(a, b, NULL);
		merge_type = 0;
		a->last->succ = NULL;
		b->addr_base = 0;
	}

	// If chain a consists of 1 block all following merges are equivalent to simple
	// concatenation. If chains get to long we stop to try complex merges too.
	if (a->length <= 1 || a->length > MAXIMAL_SPLIT_LENGTH) {
		if (merge_type == -1) { // no allowed merge possible
			*merge_type_cache = -1;
			return -1;
		}
		max_score -= a->score + b->score; // compute gain
		if (max_score <= 0) max_score = 0;
		*merge_type_cache = merge_type;
		return max_score;
	}

	// split chain a into two nonempty chains head_a and a
	// to begin head_a consists of one block
	exttsp_chain_t head_a;
	head_a.first = a->first;
	head_a.bytes = head_a.addr_base = 0;
	head_a.contains_start = a->contains_start;
	if (head_a.contains_start) a->contains_start = false;
	pop_from_into(a, head_a);

	// try all ways to cut chain a into two
	for (int i=1; i<a->length; i++) {
		// try all valid ways to concatenate a, head_a and b
		if (!b->contains_start) {
			consider_merged_score(&head_a, b, a, 1);
			if (!head_a.contains_start) {
				consider_merged_score(a, &head_a, b, 2);
				consider_merged_score(a, b, &head_a, 3);
			}
		}
		if (!head_a.contains_start) {
			consider_merged_score(b, a, &head_a, 4);
		}
		// move first block of a to head_a
		head_a.last->succ = a->first;
		pop_from_into(a, head_a);
	}
	// restore a
	a->first = head_a.first;
	a->bytes = head_a.bytes;
	a->addr_base = 0;
	a->contains_start = head_a.contains_start;
	exttsp_block_t *cur = a->first;
	do {
		cur->chain = a;
	} while ((cur = cur->succ) != NULL);
	// compute actual gain
	max_score -= a->score + b->score;
	if (max_score <= 0) max_score = 0;
	*merge_type_cache = merge_type;
	return max_score;
}
#undef pop_from_into
#undef consider_merged_score

/*
 * Merge two chains according to a merge type.
 */
static void exttsp_merge_chains(exttsp_chain_t *a, exttsp_chain_t *b,
	const int merge_type)
{
	assert(merge_type >= 0 && "executing forbidden merge");
	const int concrete_merge_type = merge_type % 5;
	if (concrete_merge_type == 4) {
		assert(!a->contains_start && "start node is not kept at beginning!");
	} else {
		assert(!b->contains_start && "start node is not kept at beginning!");
	}
	if (concrete_merge_type == 0) { // simple concatenation
		a->last->succ = b->first;
		a->last = b->last;
	} else { // split a into two chains and merge them with b
		// head of a
		const int head_a_length = merge_type / 5;
		exttsp_block_t *head_a_first = a->first;
		exttsp_block_t *head_a_last  = a->first;
		for (int i=1; i<head_a_length; i++)
			head_a_last = head_a_last->succ;
		// tail of a
		exttsp_block_t *new_a_first  = head_a_last->succ;
		exttsp_block_t *new_a_last   = a->last;
		// b (needed for macro)
		exttsp_block_t *b_first  = b->first;
		exttsp_block_t *b_last   = b->last;
		// merge chains
		exttsp_block_t *result_first, *result_last;
		#define merge(X,Y,Z) {           \
			result_first    = X##_first; \
			X##_last->succ  = Y##_first; \
			Y##_last->succ  = Z##_first; \
			Z##_last->succ  = NULL;      \
			result_last     = Z##_last;  \
		}
		switch (concrete_merge_type) {
			case 1:
				merge(head_a, b, new_a);
				break;
			case 2:
				merge(new_a, head_a, b);
				break;
			case 3:
				merge(new_a, b, head_a);
				break;
			case 4:
				merge(b, new_a, head_a);
				break;
			default: panic("undefined merge type");
		}
		#undef merge
		a->first = result_first;
		a->last  = result_last;
	}
	// recalculate block address offsets and chain pointers
	exttsp_block_t *n = a->first;
	int addr = 0;
	do {
		n->addr_offset = addr;
		addr += n->size;
		n->chain = a;
	} while ((n = n->succ) != NULL);
	// set correct data for new chain
	a->length += b->length;
	a->bytes += b->bytes;
	a->contains_start = a->contains_start || b->contains_start;
	a->score = exttsp_rate_chain(a, NULL, NULL);
	// invalidate chain b
	b->length = b->bytes = b->score = 0;
	b->first = b->last = NULL;
	b->contains_start = false;
}

#define EXTTSP_DB_CHAINS {                                                           \
	DB((dbg, LEVEL_1, "\n%d(%ld) chains, %d(%ld) blocks, %d(%ld) edges\n",           \
		env.chain_count, ARR_LEN(env.chains),env.block_count,                        \
		ARR_LEN(env.blocks), env.edge_count, ARR_LEN(env.edges)));                   \
	for (size_t x=0; x<ARR_LEN(env.chains); x++) {                                   \
		if (env.chains[x].length == 0) continue;                                     \
		DB((dbg, LEVEL_1,                                                            \
			"\t chain: %zu \t length: %d \t bytes: %d  \t score: %f \t start: %d\n", \
			x, env.chains[x].length, env.chains[x].bytes, env.chains[x].score,       \
			env.chains[x].contains_start));                                          \
	}                                                                                \
}

#define EXTTSP_DB_ADJACENCY_MATRIX {                               \
DB((dbg, LEVEL_1, "adjacency matrix:\n"));                         \
	for (size_t i=0; i<ARR_LEN(env.chains); i++)                   \
		DB((dbg, LEVEL_1, " %d", i%10));                           \
	DB((dbg, LEVEL_1, "\n"));                                      \
	for (size_t i=0; i<ARR_LEN(env.chains); i++) {                 \
		DB((dbg, LEVEL_1, "%d", i%10));                            \
		for (size_t j=0; j<ARR_LEN(env.chains); j++) {             \
			DB((dbg, LEVEL_1, "%c ", env.adjacent[i][j]?'X':' ')); \
		}                                                          \
		DB((dbg, LEVEL_1, "\n"));                                  \
	}                                                              \
}

/*
 * Create block schedule using the ExtTSP algorithm.
 */
static ir_node **be_create_exttsp_block_schedule(ir_graph *irg)
{
	exttsp_env_t env;
	struct obstack obst;
	env.irg = irg;

	// remove blocks that just contain a jump instruction
	remove_empty_blocks(irg);

	// count blocks and edges
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	env.chain_count = env.block_count = env.edge_count = 0;
	irg_block_walk_graph(irg, exttsp_init_counters, NULL, &env);

	// create data structures
	obstack_init(&obst);
	env.chains = NEW_ARR_D(exttsp_chain_t, &obst, env.chain_count);
	env.blocks = NEW_ARR_D(exttsp_block_t, &obst, env.block_count);
	env.edges  = NEW_ARR_D(exttsp_edge_t,  &obst, env.edge_count);
	env.adjacent           = OALLOCN(&obst, char*,           env.chain_count);
	exttsp_score_t **gains = OALLOCN(&obst, exttsp_score_t*, env.chain_count);
	int **merge_types      = OALLOCN(&obst, int*,            env.chain_count);
	for (int i=0; i<env.chain_count; i++) {
		env.adjacent[i] = OALLOCNZ(&obst, char,           env.chain_count);
		gains[i]        = OALLOCNZ(&obst, exttsp_score_t, env.chain_count);
		merge_types[i]  = OALLOCNZ(&obst, int,            env.chain_count);
	}

	// initial chain creation
	env.chain_count = env.block_count = env.edge_count = 0;
	irg_block_walk_graph(irg, exttsp_create_chains, NULL, &env);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	for (int i=0; i<env.chain_count; i++)
		env.chains[i].score = exttsp_rate_chain(&env.chains[i], NULL, NULL);

	// compute initial merge gains
	for (int i=0; i<env.chain_count; i++) {
		for (int j=0; j<env.chain_count; j++) {
			if (env.adjacent[i][j])
				gains[i][j] = exttsp_compute_merge_gain(&env.chains[i],
				               &env.chains[j], &merge_types[i][j]);
		}
	}

	// chain merging
	int max_i, max_j;
	exttsp_score_t max;
	while (env.chain_count > 1) {
		// find maximum merge gain
		max_i = max_j = -1;
		max = 0;
		for (size_t i=0; i<ARR_LEN(env.chains); i++) {
			if (env.chains[i].length == 0) continue;
			for (size_t j=0; j<ARR_LEN(env.chains); j++) {
				if (env.adjacent[i][j] && gains[i][j] >= max) {
					max_i = i; max_j = j; max = gains[i][j];
				}
			}
		}
		assert(max_i >= 0 && max_j >= 0 && "found no chain for merging!");

		// merge chains with maximum gain
		DB((dbg, LEVEL_1, "merging chain %u and %u (type: %d, gain:%f)\n",
			max_i, max_j, merge_types[max_i][max_j], gains[max_i][max_j]));
		exttsp_merge_chains(&env.chains[max_i], &env.chains[max_j],
			merge_types[max_i][max_j]);

		// fix adjacency matrix
		for (size_t n=0; n<ARR_LEN(env.chains); n++) {
			if (env.adjacent[max_j][n]) {
				env.adjacent[max_i][n] = env.adjacent[n][max_i] = 1;
				env.adjacent[max_j][n] = env.adjacent[n][max_j] = 0;
			}
		}
		env.adjacent[max_i][max_i] = 0;

		// recalculate gains affected by new chain i
		gains[max_i][max_j] = gains[max_j][max_i] = 0;
		for (size_t n=0; n<ARR_LEN(env.chains); n++) {
			gains[max_j][n] = gains[n][max_j] = 0;
			if (!env.adjacent[max_i][n]) continue;
			gains[max_i][n] = exttsp_compute_merge_gain(&env.chains[max_i],
			                    &env.chains[n], &merge_types[max_i][n]);
			gains[n][max_i] = exttsp_compute_merge_gain(&env.chains[n],
			                    &env.chains[max_i], &merge_types[n][max_i]);
		}
		env.chain_count--;
	}
	// get remaining chain which contains the schedule
	exttsp_chain_t *final_chain = NULL;
	for (size_t i=0; i<ARR_LEN(env.chains); i++) {
		if (env.chains[i].length != 0) {
			final_chain = &env.chains[i];
			break;
		}
	}
	assert(final_chain != NULL && final_chain->length == env.block_count);
	DB((dbg, LEVEL_1, "Final chain ExtTSP score: %f\n", final_chain->score));

	// create block schedule from remaining chain
	struct obstack *const irg_obst = be_get_be_obst(irg);
	ir_node **const block_list = NEW_ARR_D(ir_node*, irg_obst, env.block_count);
	exttsp_block_t *bb = final_chain->first;
	int i = 0;
	do {
		block_list[i++] = bb->irn;
	} while ((bb = bb->succ) != NULL);
	obstack_free(&obst, NULL);
	return block_list;
}


// list of available block schedulers
static be_module_list_entry_t *block_schedulers;

// selected block scheduler
static ir_node** (*scheduler)(ir_graph*);

ir_node **be_create_block_schedule(ir_graph *irg)
{
	DB((dbg, LEVEL_1, "\nCreating block schedule for '%F'\n", irg));
	be_timer_push(T_BLOCKSCHED);
	ir_node **block_list = scheduler(irg);
	be_timer_pop(T_BLOCKSCHED);
	DB((dbg, LEVEL_1, "Created block schedule:\n"));
	for (size_t i=0; i < ARR_LEN(block_list); i++) {
		DB((dbg, LEVEL_1, "\t%+F\n", block_list[i]));
	}
	return block_list;
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_blocksched)
void be_init_blocksched(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.blocksched");
	// add block schedulers
	be_add_module_to_list(&block_schedulers, "normal",
		(void*)be_create_normal_block_schedule);
	be_add_module_to_list(&block_schedulers, "random",
		(void*)be_create_random_block_schedule);
	be_add_module_to_list(&block_schedulers, "exttsp",
		(void*)be_create_exttsp_block_schedule);
	// set standard scheduler
	scheduler = (void*)be_create_normal_block_schedule;
	// register option
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	be_add_module_list_opt(be_grp, "block-scheduler",
		"basic block scheduling algorithm", &block_schedulers,
		(void**)&scheduler);
	// seed random number generator
#ifdef _WIN32
	// use seconds
	srand((unsigned int) time(NULL));
#else
	// use milliseconds
	struct timeval time;
    gettimeofday(&time, NULL);
    srand((time.tv_sec * 1000) + (time.tv_usec / 1000));
#endif
}
