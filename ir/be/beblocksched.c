/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       Block-scheduling strategies.
 * @author      Matthias Braun, Christoph Mallon
 * @date        27.09.2006
 * @version     $Id$
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
#include "config.h"

#include "beblocksched.h"

#include <stdlib.h>

#include "array.h"
#include "pdeq.h"

#include "iredges.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irloop.h"
#include "irprintf.h"
#include "execfreq.h"
#include "irdump_t.h"
#include "irtools.h"
#include "debug.h"
#include "beirgmod.h"
#include "bemodule.h"
#include "be.h"
#include "error.h"

#include "lc_opts.h"
#include "lc_opts_enum.h"

#ifdef WITH_ILP
#include <lpp/lpp.h>
#include <lpp/lpp_net.h>
#endif /* WITH_ILP */

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef enum blocksched_algos_t {
	BLOCKSCHED_NAIV, BLOCKSCHED_GREEDY, BLOCKSCHED_ILP
} blocksched_algos_t;

static int algo = BLOCKSCHED_GREEDY;

static const lc_opt_enum_int_items_t blockschedalgo_items[] = {
	{ "naiv",   BLOCKSCHED_NAIV },
	{ "greedy", BLOCKSCHED_GREEDY },
#ifdef WITH_ILP
	{ "ilp",    BLOCKSCHED_ILP },
#endif /* WITH_ILP */
	{ NULL,     0 }
};

static lc_opt_enum_int_var_t algo_var = {
	&algo, blockschedalgo_items
};

static const lc_opt_table_entry_t be_blocksched_options[] = {
	LC_OPT_ENT_ENUM_INT ("blockscheduler", "the block scheduling algorithm", &algo_var),
	LC_OPT_LAST
};

/*
 *   ____                   _
 *  / ___|_ __ ___  ___  __| |_   _
 * | |  _| '__/ _ \/ _ \/ _` | | | |
 * | |_| | | |  __/  __/ (_| | |_| |
 *  \____|_|  \___|\___|\__,_|\__, |
 *                            |___/
 */

typedef struct blocksched_entry_t blocksched_entry_t;
struct blocksched_entry_t {
	ir_node            *block;
	blocksched_entry_t *next;
	blocksched_entry_t *prev;
};

typedef struct edge_t edge_t;
struct edge_t {
	ir_node *block;             /**< source block */
	int     pos;                /**< number of cfg predecessor (target) */
	double  execfreq;           /**< the frequency */
	double  outedge_penalty_freq; /**< for edges leaving the loop this is the
	                                   penality when we make them a
	                                   fallthrough. */
	int     highest_execfreq;   /**< flag that indicates whether this edge is
	                                 the edge with the highest execfreq pointing
	                                 away from this block */
};

typedef struct blocksched_env_t blocksched_env_t;
struct blocksched_env_t {
	ir_graph       *irg;
	struct obstack *obst;
	ir_exec_freq   *execfreqs;
	edge_t         *edges;
	pdeq           *worklist;
	int            blockcount;
};

/**
 * Collect cfg frequencies of all edges between blocks.
 * Also determines edge with highest frequency.
 */
static void collect_egde_frequency(ir_node *block, void *data)
{
	blocksched_env_t   *env = (blocksched_env_t*)data;
	int                arity;
	edge_t             edge;
	blocksched_entry_t *entry;
	ir_loop            *loop;

	memset(&edge, 0, sizeof(edge));

	entry = OALLOCZ(env->obst, blocksched_entry_t);
	entry->block = block;
	set_irn_link(block, entry);

	loop = get_irn_loop(block);

	arity = get_Block_n_cfgpreds(block);

	if (arity == 0) {
		/* must be the start block (or end-block for endless loops),
		 * everything else is dead code and should be removed by now */
		assert(block == get_irg_start_block(env->irg)
				|| block == get_irg_end_block(env->irg));
		/* nothing to do here */
		return;
	} else if (arity == 1) {
		ir_node *pred_block = get_Block_cfgpred_block(block, 0);
		ir_loop *pred_loop  = get_irn_loop(pred_block);
		float    freq       = (float)get_block_execfreq(env->execfreqs, block);

		/* is it an edge leaving a loop */
		if (get_loop_depth(pred_loop) > get_loop_depth(loop)) {
			float pred_freq = (float)get_block_execfreq(env->execfreqs, pred_block);
			edge.outedge_penalty_freq = -(pred_freq - freq);
		}

		edge.block            = block;
		edge.pos              = 0;
		edge.execfreq         = freq;
		edge.highest_execfreq = 1;
		ARR_APP1(edge_t, env->edges, edge);
	} else {
		int    i;
		double highest_execfreq = -1.0;
		int    highest_edge_num = -1;

		edge.block = block;
		for (i = 0; i < arity; ++i) {
			double  execfreq;
			ir_node *pred_block = get_Block_cfgpred_block(block, i);

			execfreq = get_block_execfreq(env->execfreqs, pred_block);

			edge.pos              = i;
			edge.execfreq         = execfreq;
			edge.highest_execfreq = 0;
			ARR_APP1(edge_t, env->edges, edge);

			if (execfreq > highest_execfreq) {
				highest_execfreq = execfreq;
				highest_edge_num = ARR_LEN(env->edges) - 1;
			}
		}

		if (highest_edge_num >= 0)
			env->edges[highest_edge_num].highest_execfreq = 1;
	}
}

static int cmp_edges(const void *d1, const void *d2)
{
	const edge_t *e1 = (const edge_t*)d1;
	const edge_t *e2 = (const edge_t*)d2;

	return QSORT_CMP(e2->execfreq, e1->execfreq);
}

static int cmp_edges_outedge_penalty(const void *d1, const void *d2)
{
	const edge_t *e1 = (const edge_t*)d1;
	const edge_t *e2 = (const edge_t*)d2;
	/* reverse sorting as penalties are negative */
	return QSORT_CMP(e1->outedge_penalty_freq, e2->outedge_penalty_freq);
}

static void clear_loop_links(ir_loop *loop)
{
	int i, n;

	set_loop_link(loop, NULL);
	n = get_loop_n_elements(loop);
	for (i = 0; i < n; ++i) {
		loop_element elem = get_loop_element(loop, i);
		if (*elem.kind == k_ir_loop) {
			clear_loop_links(elem.son);
		}
	}
}

static void coalesce_blocks(blocksched_env_t *env)
{
	int i;
	int edge_count = ARR_LEN(env->edges);
	edge_t *edges = env->edges;

	/* sort interblock edges by execution frequency */
	qsort(edges, ARR_LEN(edges), sizeof(edges[0]), cmp_edges);

	/* run1: only look at jumps */
	for (i = 0; i < edge_count; ++i) {
		const edge_t *edge  = &edges[i];
		ir_node      *block = edge->block;
		int           pos   = edge->pos;
		ir_node      *pred_block;
		blocksched_entry_t *entry, *pred_entry;

		/* only check edge with highest frequency */
		if (! edge->highest_execfreq)
			continue;

		/* the block might have been removed already... */
		if (is_Bad(get_Block_cfgpred(block, 0)))
			continue;

		pred_block = get_Block_cfgpred_block(block, pos);
		entry      = (blocksched_entry_t*)get_irn_link(block);
		pred_entry = (blocksched_entry_t*)get_irn_link(pred_block);

		if (pred_entry->next != NULL || entry->prev != NULL)
			continue;

		/* only coalesce jumps */
		if (get_block_succ_next(pred_block, get_block_succ_first(pred_block)) != NULL)
			continue;

		/* schedule the 2 blocks behind each other */
		DB((dbg, LEVEL_1, "Coalesce (Jump) %+F -> %+F (%.3g)\n",
		           pred_entry->block, entry->block, edge->execfreq));
		pred_entry->next = entry;
		entry->prev      = pred_entry;
	}

	/* run2: pick loop fallthroughs */
	clear_loop_links(get_irg_loop(env->irg));

	qsort(edges, ARR_LEN(edges), sizeof(edges[0]), cmp_edges_outedge_penalty);
	for (i = 0; i < edge_count; ++i) {
		const edge_t *edge  = &edges[i];
		ir_node      *block = edge->block;
		int           pos   = edge->pos;
		ir_node      *pred_block;
		blocksched_entry_t *entry, *pred_entry;
		ir_loop      *loop;
		ir_loop      *outer_loop;

		/* already seen all loop outedges? */
		if (edge->outedge_penalty_freq == 0)
			break;

		/* the block might have been removed already... */
		if (is_Bad(get_Block_cfgpred(block, pos)))
			continue;

		pred_block = get_Block_cfgpred_block(block, pos);
		entry      = (blocksched_entry_t*)get_irn_link(block);
		pred_entry = (blocksched_entry_t*)get_irn_link(pred_block);

		if (pred_entry->next != NULL || entry->prev != NULL)
			continue;

		/* we want at most 1 outedge fallthrough per loop */
		loop = get_irn_loop(pred_block);
		if (get_loop_link(loop) != NULL)
			continue;

		/* schedule the 2 blocks behind each other */
		DB((dbg, LEVEL_1, "Coalesce (Loop Outedge) %+F -> %+F (%.3g)\n",
		           pred_entry->block, entry->block, edge->execfreq));
		pred_entry->next = entry;
		entry->prev      = pred_entry;

		/* all loops left have an outedge now */
		outer_loop = get_irn_loop(block);
		do {
			/* we set loop link to loop to mark it */
			set_loop_link(loop, loop);
			loop = get_loop_outer_loop(loop);
		} while (loop != outer_loop);
	}

	/* sort interblock edges by execution frequency */
	qsort(edges, ARR_LEN(edges), sizeof(edges[0]), cmp_edges);

	/* run3: remaining edges */
	for (i = 0; i < edge_count; ++i) {
		const edge_t *edge  = &edges[i];
		ir_node      *block = edge->block;
		int           pos   = edge->pos;
		ir_node      *pred_block;
		blocksched_entry_t *entry, *pred_entry;

		/* the block might have been removed already... */
		if (is_Bad(get_Block_cfgpred(block, pos)))
			continue;

		pred_block = get_Block_cfgpred_block(block, pos);
		entry      = (blocksched_entry_t*)get_irn_link(block);
		pred_entry = (blocksched_entry_t*)get_irn_link(pred_block);

		/* is 1 of the blocks already attached to another block? */
		if (pred_entry->next != NULL || entry->prev != NULL)
			continue;

		/* schedule the 2 blocks behind each other */
		DB((dbg, LEVEL_1, "Coalesce (CondJump) %+F -> %+F (%.3g)\n",
		           pred_entry->block, entry->block, edge->execfreq));
		pred_entry->next = entry;
		entry->prev      = pred_entry;
	}
}

static void pick_block_successor(blocksched_entry_t *entry, blocksched_env_t *env)
{
	ir_node            *block = entry->block;
	ir_node            *succ  = NULL;
	blocksched_entry_t *succ_entry;
	const ir_edge_t    *edge;
	double             best_succ_execfreq;

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
		succ_entry = (blocksched_entry_t*)get_irn_link(succ_block);
		while (succ_entry->prev != NULL) {
			/* break cycles... */
			if (succ_entry->prev->block == succ_block) {
				succ_entry->prev->next = NULL;
				succ_entry->prev       = NULL;
				break;
			}
			succ_entry = succ_entry->prev;
		};

		if (irn_visited(succ_entry->block))
			continue;

		DB((dbg, LEVEL_1, "Put %+F into worklist\n", succ_entry->block));
		pdeq_putr(env->worklist, succ_entry->block);
	}

	if (entry->next != NULL) {
		pick_block_successor(entry->next, env);
		return;
	}

	DB((dbg, LEVEL_1, "deciding...\n"));
	best_succ_execfreq = -1;

	/* no successor yet: pick the successor block with the highest execution
	 * frequency which has no predecessor yet */

	foreach_block_succ(block, edge) {
		ir_node *succ_block = get_edge_src_irn(edge);
		double  execfreq;

		if (irn_visited(succ_block))
			continue;

		succ_entry = (blocksched_entry_t*)get_irn_link(succ_block);
		if (succ_entry->prev != NULL)
			continue;

		execfreq = get_block_execfreq(env->execfreqs, succ_block);
		if (execfreq > best_succ_execfreq) {
			best_succ_execfreq = execfreq;
			succ = succ_block;
		}
	}

	if (succ == NULL) {
		DB((dbg, LEVEL_1, "pick from worklist\n"));

		do {
			if (pdeq_empty(env->worklist)) {
				DB((dbg, LEVEL_1, "worklist empty\n"));
				return;
			}
			succ = (ir_node*)pdeq_getl(env->worklist);
		} while (irn_visited(succ));
	}

	succ_entry       = (blocksched_entry_t*)get_irn_link(succ);
	entry->next      = succ_entry;
	succ_entry->prev = entry;

	pick_block_successor(succ_entry, env);
}

static blocksched_entry_t *finish_block_schedule(blocksched_env_t *env)
{
	ir_graph           *irg        = env->irg;
	ir_node            *startblock = get_irg_start_block(irg);
	blocksched_entry_t *entry      = (blocksched_entry_t*)get_irn_link(startblock);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);

	env->worklist = new_pdeq();
	pick_block_successor(entry, env);
	assert(pdeq_empty(env->worklist));
	del_pdeq(env->worklist);

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);

	return entry;
}

static ir_node **create_blocksched_array(blocksched_env_t *env, blocksched_entry_t *first,
										int count, struct obstack* obst)
{
	int                i = 0;
	ir_node            **block_list;
	blocksched_entry_t *entry;
	(void) env;

	block_list = NEW_ARR_D(ir_node *, obst, count);
	DB((dbg, LEVEL_1, "Blockschedule:\n"));

	for (entry = first; entry != NULL; entry = entry->next) {
		assert(i < count);
		block_list[i++] = entry->block;
		DB((dbg, LEVEL_1, "\t%+F\n", entry->block));
	}
	assert(i == count);

	return block_list;
}

static ir_node **create_block_schedule_greedy(ir_graph *irg, ir_exec_freq *execfreqs)
{
	blocksched_env_t   env;
	struct obstack     obst;
	blocksched_entry_t *start_entry;
	ir_node            **block_list;

	obstack_init(&obst);

	env.irg        = irg;
	env.obst       = &obst;
	env.execfreqs  = execfreqs;
	env.edges      = NEW_ARR_F(edge_t, 0);
	env.worklist   = NULL;
	env.blockcount = 0;

	/* make sure loopinfo is up-to-date */
	if (! (get_irg_loopinfo_state(irg) & loopinfo_cf_consistent)) {
		construct_cf_backedges(irg);
	}

	// collect edge execution frequencies
	irg_block_walk_graph(irg, collect_egde_frequency, NULL, &env);

	(void)be_remove_empty_blocks(irg);

	if (algo != BLOCKSCHED_NAIV)
		coalesce_blocks(&env);

	start_entry = finish_block_schedule(&env);
	block_list  = create_blocksched_array(&env, start_entry, env.blockcount,
	                                      be_get_be_obst(irg));

	DEL_ARR_F(env.edges);
	obstack_free(&obst, NULL);

	return block_list;
}

/*
 *  ___ _     ____
 * |_ _| |   |  _ \
 *  | || |   | |_) |
 *  | || |___|  __/
 * |___|_____|_|
 *
 */

#ifdef WITH_ILP
typedef struct ilp_edge_t {
	ir_node *block;   /**< source block */
	int     pos;      /**< number of cfg predecessor (target) */
	int     ilpvar;
} ilp_edge_t;

typedef struct blocksched_ilp_env_t {
	blocksched_env_t env;
	ilp_edge_t       *ilpedges;
	lpp_t            *lpp;
} blocksched_ilp_env_t;

typedef struct blocksched_ilp_entry_t {
	ir_node *block;
	struct blocksched_entry_t *next;
	struct blocksched_entry_t *prev;

	int out_cst;
} blocksched_ilp_entry_t;

static int add_ilp_edge(ir_node *block, int pos, double execfreq, blocksched_ilp_env_t *env)
{
	char       name[64];
	ilp_edge_t edge;
	int        edgeidx = ARR_LEN(env->ilpedges);

	snprintf(name, sizeof(name), "edge%d", edgeidx);

	edge.block  = block;
	edge.pos    = pos;
	edge.ilpvar = lpp_add_var_default(env->lpp, name, lpp_binary, execfreq, 1.0);

	ARR_APP1(ilp_edge_t, env->ilpedges, edge);
	return edgeidx;
}

static void collect_egde_frequency_ilp(ir_node *block, void *data)
{
	blocksched_ilp_env_t *env        = data;
	ir_graph             *irg        = env->env.irg;
	ir_node              *startblock = get_irg_start_block(irg);
	int                  arity;
	lpp_cst_t            cst;
	char                 name[64];
	int                  out_count;
	blocksched_ilp_entry_t *entry;

	snprintf(name, sizeof(name), "block_out_constr_%ld", get_irn_node_nr(block));
	out_count = get_irn_n_edges_kind(block, EDGE_KIND_BLOCK);

	entry          = OALLOC(env->env.obst, blocksched_ilp_entry_t);
	entry->block   = block;
	entry->next    = NULL;
	entry->prev    = NULL;
	entry->out_cst = lpp_add_cst_uniq(env->lpp, name, lpp_greater, out_count - 1);
	set_irn_link(block, entry);

	if (block == startblock)
		return;

	arity = get_irn_arity(block);
	if (arity == 1) {
		double execfreq = get_block_execfreq(env->env.execfreqs, block);
		add_ilp_edge(block, 0, execfreq, env);
	}
	else {
		int i;

		snprintf(name, sizeof(name), "block_in_constr_%ld", get_irn_node_nr(block));
		cst = lpp_add_cst_uniq(env->lpp, name, lpp_greater, arity - 1);

		for (i = 0; i < arity; ++i) {
			double     execfreq;
			int        edgenum;
			ilp_edge_t *edge;
			ir_node    *pred_block = get_Block_cfgpred_block(block, i);

			execfreq = get_block_execfreq(env->env.execfreqs, pred_block);
			edgenum  = add_ilp_edge(block, i, execfreq, env);
			edge     = &env->ilpedges[edgenum];
			lpp_set_factor_fast(env->lpp, cst, edge->ilpvar, 1.0);
		}
	}
}


static void coalesce_blocks_ilp(blocksched_ilp_env_t *env)
{
	int           edge_count = ARR_LEN(env->ilpedges);
	be_options_t *options    = be_get_irg_options(env->env.irg);
	int           i;

	/* complete out constraints */
	for (i = 0; i < edge_count; ++i) {
		const ilp_edge_t *edge  = &env->ilpedges[i];
		ir_node          *block = edge->block;
		ir_node          *pred;
		blocksched_ilp_entry_t *entry;

		/* the block might have been removed already... */
		if (is_Bad(get_Block_cfgpred(block, 0)))
			continue;

		pred  = get_Block_cfgpred_block(block, edge->pos);
		entry = get_irn_link(pred);

		DB((dbg, LEVEL_1, "Adding out cst to %+F from %+F,%d\n",
				  pred, block, edge->pos));
		lpp_set_factor_fast(env->lpp, entry->out_cst, edge->ilpvar, 1.0);
	}

	lpp_solve_net(env->lpp, options->ilp_server, options->ilp_solver);
	assert(lpp_is_sol_valid(env->lpp));

	/* Apply results to edges */
	for (i = 0; i < edge_count; ++i) {
		const ilp_edge_t   *edge  = &env->ilpedges[i];
		ir_node            *block = edge->block;
		ir_node            *pred;
		int                is_jump;
		blocksched_entry_t *entry;
		blocksched_entry_t *pred_entry;

		/* the block might have been removed already... */
		if (is_Bad(get_Block_cfgpred(block, 0)))
			continue;

		is_jump = (int)lpp_get_var_sol(env->lpp, edge->ilpvar);
		if (is_jump)
			continue;

		pred       = get_Block_cfgpred_block(block, edge->pos);
		entry      = get_irn_link(block);
		pred_entry = get_irn_link(pred);

		assert(entry->prev == NULL && pred_entry->next == NULL);
		entry->prev      = pred_entry;
		pred_entry->next = entry;
	}
}

static ir_node **create_block_schedule_ilp(ir_graph *irg, ir_exec_freq *execfreqs)
{
	blocksched_ilp_env_t env;
	struct obstack       obst;
	blocksched_entry_t   *start_entry;
	ir_node              **block_list;

	obstack_init(&obst);

	env.env.irg        = irg;
	env.env.obst       = &obst;
	env.env.execfreqs  = execfreqs;
	env.env.worklist   = NULL;
	env.env.blockcount = 0;
	env.ilpedges       = NEW_ARR_F(ilp_edge_t, 0);

	env.lpp = new_lpp("blockschedule", lpp_minimize);
	lpp_set_time_limit(env.lpp, 20);
	lpp_set_log(env.lpp, stdout);

	irg_block_walk_graph(irg, collect_egde_frequency_ilp, NULL, &env);

	(void)be_remove_empty_blocks(irg);
	coalesce_blocks_ilp(&env);

	start_entry = finish_block_schedule(&env.env);
	block_list  = create_blocksched_array(&env.env, start_entry,
	                                      env.env.blockcount,
	                                      be_get_be_obst(irg));

	DEL_ARR_F(env.ilpedges);
	free_lpp(env.lpp);
	obstack_free(&obst, NULL);

	return block_list;
}
#endif /* WITH_ILP */

/*
 *  __  __       _
 * |  \/  | __ _(_)_ __
 * | |\/| |/ _` | | '_ \
 * | |  | | (_| | | | | |
 * |_|  |_|\__,_|_|_| |_|
 *
 */
BE_REGISTER_MODULE_CONSTRUCTOR(be_init_blocksched)
void be_init_blocksched(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");

	lc_opt_add_table(be_grp, be_blocksched_options);

	FIRM_DBG_REGISTER(dbg, "firm.be.blocksched");
}

ir_node **be_create_block_schedule(ir_graph *irg)
{
	ir_exec_freq *execfreqs = be_get_irg_exec_freq(irg);

	switch (algo) {
	case BLOCKSCHED_GREEDY:
	case BLOCKSCHED_NAIV:
		return create_block_schedule_greedy(irg, execfreqs);
#ifdef WITH_ILP
	case BLOCKSCHED_ILP:
		return create_block_schedule_ilp(irg, execfreqs);
#endif /* WITH_ILP */
	}

	panic("unknown blocksched algo");
}
