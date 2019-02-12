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
#include "besched.h"
#include "debug.h"
#include "execfreq.h"
#include "execfreq_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irtools.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include <math.h>
#include "pdeq.h"
#include "irprintf.h"
#include <sys/time.h>
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

static unsigned int get_edgefreq_int(ir_execfreq_int_factors *factors,
	ir_node *dest, const ir_edge_t *edge)
{
	int int_freq = get_block_execfreq_int(factors, dest);
	double prob = get_edge_probability(edge);
	return (unsigned int) round(int_freq * prob);
}

// TODO replace with better size estimation
static unsigned int get_block_size(ir_node *block)
{
	// try to estimate block size in final binary
	int size = 0;
	sched_foreach(block, node) {
		size += 4; // 32 bit per node
	}
	return size;
}

// ****************************************************************************
// TODO remove graphviz debug code
// ****************************************************************************
static ir_execfreq_int_factors *execfreq_factors_graphviz;
static FILE *dump_file_graphviz;

static void print_block_graphviz(ir_node *block, void *env)
{
	int int_freq = get_block_execfreq_int(execfreq_factors_graphviz, block);
	ir_fprintf(dump_file_graphviz,
		"\t\"%+F\" [label=\"%+F\\n(freq:%f, int:%d)\\n size: %d bytes %s %s\"]\n",
		block, block, get_block_execfreq(block),
		int_freq,
		get_block_size(block),
		get_irg_start_block((ir_graph*)env)==block ? "\\nSTART" : "",
		get_irg_end_block((ir_graph*)env)==block ? "\\nEND" : "");
	foreach_block_succ(block, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		ir_fprintf(dump_file_graphviz, "\t\"%+F\" -> \"%+F\"[label=\"p:%f\\n f:%f\\n%d\"]\n",
			block, succ, get_edge_probability(edge),
			get_block_execfreq(block)*get_edge_probability(edge),
			get_edgefreq_int(execfreq_factors_graphviz, block, edge));
	}
}

static void dump_cfg_graphviz(ir_graph *irg)
{
	char file[64];
	ir_snprintf(file, 64, "%F-cfg-dump.gv", irg);
	dump_file_graphviz = fopen(file, "w");
	if (dump_file_graphviz) {
		ir_fprintf(dump_file_graphviz, "digraph {\n");
		irg_block_walk_graph(irg, print_block_graphviz, NULL, (void*)irg);
		ir_fprintf(dump_file_graphviz, "}\n");
		fclose(dump_file_graphviz);
	}
}
// ****************************************************************************

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
	unsigned int freq;         // frequency of edge
};

struct exttsp_block_t {
	unsigned int size;         // code size in bytes
	unsigned int addr;         // address of block in memory relative to chain
	exttsp_block_t *succ;      // next block in chain
	exttsp_edge_t *first_edge; // first outgoing cfg edge
	exttsp_chain_t *chain;     // chain the block is in
	ir_node *irn;              // corresponding node
};

struct exttsp_chain_t {
	exttsp_block_t *first;     // first block of chain
	exttsp_block_t *last;      // last block of chain
	unsigned int length;       // number of blocks in chain
	exttsp_score_t score;      // cached score of the chain
	bool contains_start;       // if the chain begins with the start block
};

struct exttsp_env_t {          // data needed by graph walkers
	ir_graph *irg;
	struct obstack obst;
	ir_execfreq_int_factors execfreq_factors;
	exttsp_chain_t *chains;
	unsigned int chain_count;
	exttsp_block_t *blocks;
	unsigned int block_count;
	exttsp_edge_t *edges;
	unsigned int edge_count;
};


/*
 * Constants for the ExtTSP algorithm.
 */
const double FALLTHROUGH_WEIGHT      = 1.0;
const double FORWARD_WEIGHT          = 0.1;
const double BACKWARD_WEIGHT         = 0.1;
const unsigned int FORWARD_DISTANCE  = 1024;
const unsigned int BACKWARD_DISTANCE = 640;

/*
 * Calculate the contribution of an edge to the ExtTSP score of a chain.
 */
static exttsp_score_t exttsp_rate_edge(exttsp_edge_t *edge)
{
	unsigned int jump_addr = edge->src->addr + edge->src->size;
	unsigned int dest_addr = edge->dest->addr;
	unsigned int freq = edge->freq;
	// fallthrough edge
	if (jump_addr == dest_addr) {
		return freq * FALLTHROUGH_WEIGHT;
	}
	// forward edge
	else if (jump_addr < dest_addr) {
		unsigned int distance = dest_addr - jump_addr;
		if (distance <= FORWARD_DISTANCE) {
			double probability = 1.0 - ((double)distance)/((double)FORWARD_DISTANCE);
			return freq * FORWARD_WEIGHT * probability;
		}
		return 0;
	}
	// backward edge
	unsigned int distance = jump_addr - dest_addr;
	if (distance <= BACKWARD_DISTANCE) {
		double probability = 1.0 - ((double)distance)/((double)BACKWARD_DISTANCE);
		return freq * BACKWARD_WEIGHT * probability;
	}
	return 0;
}

/*
 * Calculate the ExtTSP score for a chain.
 */
static exttsp_score_t exttsp_rate_chain(exttsp_chain_t *chain)
{
	exttsp_block_t *block = chain->first;
	exttsp_edge_t *edge;
	exttsp_score_t score = 0;
	while (block != NULL) {
		edge = block->first_edge;
		while (edge != NULL) {
			if (edge->dest->chain == chain) { // ignore jumps leaving the chain
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
	ir_node *end = get_irg_end_block(e->irg);
	if (block == end) return;
	// create block
	exttsp_block_t *b = (exttsp_block_t*) get_irn_link(block);
	if (b == NULL) { // no edge to this block occured yet
		b = &(e->blocks[e->block_count++]);
	}
	b->size = get_block_size(block);
	b->addr = 0;
	b->succ = NULL;
	b->first_edge = NULL;
	b->irn = block;
	set_irn_link(block, (void*)b);
	// create chain containing block
	exttsp_chain_t *c = &(e->chains[e->chain_count++]);
	c->first = c->last = b;
	c->length = 1;
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
		succ_edge->freq = get_edgefreq_int(&(e->execfreq_factors), block, edge);
	}
	c->score = exttsp_rate_chain(c);
}

/*
 * Compute gain in ExtTSP score reached by merging two chains.
 */
static exttsp_score_t exttsp_compute_merge_gain(exttsp_chain_t *a, exttsp_chain_t *b)
{
	exttsp_score_t score = 0;
	// try simple concatination a+b
	if (!b->contains_start) {
		// save state of a
		exttsp_chain_t save = *a;
		// merge a and b
		a->last->succ = b->first;
		a->last = b->last;
		a->length += b->length;
		// recalculate block addresses and set chain of second half
		// TODO possible to avoid this?
		unsigned int addr = save.last->addr + save.last->size;
		exttsp_block_t *cur = b->first;
		while (cur != NULL) {
			cur->addr = addr;
			addr += cur->size;
			cur->chain = a;
			cur = cur->succ;
		}
		exttsp_score_t new_score = exttsp_rate_chain(a);
		// restore a
		*a = save;
		a->last->succ = NULL;
		// restore block addresses and chain of second half
		addr = 0;
		cur = b->first;
		while (cur != NULL) {
			cur->addr = addr;
			addr += cur->size;
			cur->chain = b;
			cur = cur->succ;
		}
		new_score -= a->score + b->score; // gain
		if (new_score > score) {
			score = new_score;
			// TODO save used merge type
		}
	} else {
		score = -1; // TODO needed because some merge types are missing
	}
	// ...
	// TODO implement nontrivial merges
	return score;
}

// generate debug output for each iteration of the main loop
#define EXTTSP_DEBUG_OUTPUT_CHAINS {                                                 \
	DB((dbg, LEVEL_1, "\n%d(%ld) chains, %d(%ld) blocks, %d(%ld) edges\n",           \
		env.chain_count, ARR_LEN(env.chains),env.block_count,                        \
		ARR_LEN(env.blocks), env.edge_count, ARR_LEN(env.edges)));                   \
	for (size_t x=0; x<ARR_LEN(env.chains); x++) {                                   \
		if (env.chains[x].length == 0) continue;                                     \
		DB((dbg, LEVEL_1, "\t chain: %zu \t length: %u \t score: %f \t start: %d\n", \
			x, env.chains[x].length, env.chains[x].score,                            \
			env.chains[x].contains_start));                                          \
	}                                                                                \
}

/*
 * Create block schedule using the ExtTSP algorithm.
 */
static ir_node **be_create_exttsp_block_schedule(ir_graph *irg)
{
	exttsp_env_t env;
	env.irg = irg;

	// count blocks and edges
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	env.chain_count = env.block_count = env.edge_count = 0;
	irg_block_walk_graph(irg, exttsp_init_counters, NULL, &env);

	// create data structures
	obstack_init(&env.obst);
	env.chains = NEW_ARR_D(exttsp_chain_t, &env.obst, env.chain_count);
	env.blocks = NEW_ARR_D(exttsp_block_t, &env.obst, env.block_count);
	env.edges  = NEW_ARR_D(exttsp_edge_t,  &env.obst, env.edge_count);
	exttsp_score_t **gains = OALLOCN(&env.obst, exttsp_score_t*, ARR_LEN(env.chains));
	for (size_t i=0; i<ARR_LEN(env.chains); i++)
		gains[i] = OALLOCNZ(&env.obst, exttsp_score_t, ARR_LEN(env.chains));

	// estimate execution frequencies
	ir_calculate_execfreq_int_factors(&(env.execfreq_factors), irg);
	execfreq_factors_graphviz = &(env.execfreq_factors); // TODO remove graphviz
	dump_cfg_graphviz(irg); // TODO remove graphviz

	// initial chain creation
	env.chain_count = env.block_count = env.edge_count = 0;
	irg_block_walk_graph(irg, exttsp_create_chains, NULL, &env);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	// compute initial merge gains
	for (size_t i=0; i<ARR_LEN(env.chains); i++) {
		for (size_t j=0; j<ARR_LEN(env.chains); j++) {
			if (i == j) continue;
			gains[i][j] = exttsp_compute_merge_gain(&env.chains[i], &env.chains[j]);
		}
	}

	// chain merging
	int max_i, max_j;
	exttsp_score_t max;
	while (env.chain_count > 1) {
		EXTTSP_DEBUG_OUTPUT_CHAINS;
		// find maximum merge gain
		max_i = max_j = -1;
		max = 0;
		for (size_t i=0; i<ARR_LEN(env.chains); i++) {
			if (env.chains[i].length == 0) continue;
			for (size_t j=0; j<ARR_LEN(env.chains); j++) {
				if (i == j || env.chains[j].length == 0) continue;
				if (gains[i][j] >= max) {
					max_i = i; max_j = j; max = gains[i][j];
				}
			}
		}
		assert(max_i >= 0 && max_j >= 0 && "found no chain for merging!");
		// merge chains with maximum gain    TODO implement nontrivial merges
		DB((dbg, LEVEL_1, "merging chain %u and %u in a trivial way (gain:%f)\n",
			max_i, max_j, gains[max_i][max_j]));
		exttsp_chain_t *chain_i = &env.chains[max_i];
		exttsp_chain_t *chain_j = &env.chains[max_j];
		assert(!chain_j->contains_start && "start block is not kept at begin!");
		chain_i->length += chain_j->length;
		chain_i->contains_start =
			chain_i->contains_start || chain_j->contains_start;
		chain_i->last->succ = chain_j->first;
		chain_i->last = chain_j->last;
		// recalculate block addresses of new chain i
		// TODO integrate into rate_chain to simplify compute_merge_gain?
		unsigned int addr = 0;
		exttsp_block_t *cur = chain_i->first;
		while (cur != NULL) {
			cur->addr = addr;
			addr += cur->size;
			cur->chain = chain_i; // set new chain!
			cur = cur->succ;
		}
		chain_i->score = exttsp_rate_chain(chain_i);
		// invalidate chain j
		chain_j->length = 0;
		chain_j->first = NULL;
		chain_j->last = NULL;
		chain_j->score = 0;
		chain_j->contains_start = false;
		for (size_t n=0; n<ARR_LEN(env.chains); n++) {
			gains[max_j][n] = 0;
			gains[n][max_j] = 0;
		}
		// recalculate gains affected by new chain i
		for (size_t n=0; n<ARR_LEN(env.chains); n++) {
			if (n == (unsigned int)max_i || env.chains[n].length == 0) continue;
			gains[max_i][n] = exttsp_compute_merge_gain(&env.chains[max_i],
								&env.chains[n]);
			gains[n][max_i] = exttsp_compute_merge_gain(&env.chains[n],
								&env.chains[max_i]);
		}
		env.chain_count--;
	}
	EXTTSP_DEBUG_OUTPUT_CHAINS;
	// get remaining chain which contains the schedule
	exttsp_chain_t *final_chain = NULL;
	for (size_t i=0; i<ARR_LEN(env.chains); i++) {
		if (env.chains[i].length != 0) {
			final_chain = &env.chains[i];
			break;
		}
	}
	assert(final_chain != NULL && final_chain->length == env.block_count);
	// create block schedule from remaining chain
	struct obstack *const irg_obst = be_get_be_obst(irg);
	ir_node **const block_list = NEW_ARR_D(ir_node*, irg_obst, env.block_count);
	exttsp_block_t *block = final_chain->first;
	int i = 0;
	while (block != NULL) {
		block_list[i++] = block->irn;
		block = block->succ;
	}
	obstack_free(&env.obst, NULL);
	return block_list;
}


// list of available block schedulers
static be_module_list_entry_t *block_schedulers;

// selected block scheduler
static ir_node** (*scheduler)(ir_graph*);

ir_node **be_create_block_schedule(ir_graph *irg)
{
	DB((dbg, LEVEL_1, "Creating block schedule for '%F'\n", irg));
	ir_node **block_list = scheduler(irg);
	DB((dbg, LEVEL_1, "Created blockschedule:\n"));
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
	// seed random number generator with current milliseconds
	struct timeval time;
    gettimeofday(&time, NULL);
    srand((time.tv_sec * 1000) + (time.tv_usec / 1000));
}
