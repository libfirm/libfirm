/*
 * Author:      Matthias Braun, Christoph Mallon
 * Date:		27.09.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * License:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-Id:      $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "beblocksched.h"

#include <stdlib.h>

#include "array.h"
#include "pdeq.h"

#include "iredges.h"
#include "irgwalk.h"
#include "irgraph_t.h"
#include "irloop.h"
#include "irprintf.h"
#include "irdump_t.h"
#include "irtools.h"
#include "debug.h"
#include "beirgmod.h"
#include "bemodule.h"
#include "be.h"

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#include <libcore/lc_timing.h>

#ifdef WITH_ILP
#include <lpp/lpp.h>
#include <lpp/lpp_net.h>
#endif /* WITH_ILP */

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef enum _blocksched_algos_t {
	BLOCKSCHED_NAIV, BLOCKSCHED_EXTBB, BLOCKSCHED_GREEDY, BLOCKSCHED_ILP
} blocksched_algos_t;

static int algo = BLOCKSCHED_GREEDY;

static const lc_opt_enum_int_items_t blockschedalgo_items[] = {
	{ "naiv",	BLOCKSCHED_NAIV },
	{ "extbb",	BLOCKSCHED_EXTBB },
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
	LC_OPT_ENT_ENUM_INT ("algo", "the block scheduling algorithm", &algo_var),
	{ NULL }
};

/*
 *   ____                   _
 *  / ___|_ __ ___  ___  __| |_   _
 * | |  _| '__/ _ \/ _ \/ _` | | | |
 * | |_| | | |  __/  __/ (_| | |_| |
 *  \____|_|  \___|\___|\__,_|\__, |
 *                            |___/
 */

typedef struct _blocksched_entry_t {
	ir_node *block;
	struct _blocksched_entry_t *next;
	struct _blocksched_entry_t *prev;
} blocksched_entry_t;

typedef struct _edge_t {
	ir_node *block;             /**< source block */
	int     pos;                /**< number of cfg predecessor (target) */
	double  execfreq;           /**< the frequency */
	int     highest_execfreq;   /**< flag that indicates wether this edge is the edge with the highest
							   	     execfreq pointing away from this block */
} edge_t;

typedef struct _blocksched_env_t {
	ir_graph       *irg;
	struct obstack *obst;
	ir_exec_freq   *execfreqs;
	edge_t         *edges;
	pdeq           *worklist;
	int            blockcount;
} blocksched_env_t;

/**
 * Collect cfg frequencies of all edges between blocks.
 * Also determines edge with highest frequency.
 */
static void collect_egde_frequency(ir_node *block, void *data)
{
	blocksched_env_t   *env = data;
	int                arity;
	edge_t             edge;
	blocksched_entry_t *entry;

	entry        = obstack_alloc(env->obst, sizeof(entry[0]));
	entry->block = block;
	entry->next  = NULL;
	entry->prev  = NULL;
	set_irn_link(block, entry);

	if (block == get_irg_start_block(env->irg))
		return;

	arity = get_irn_arity(block);

	if (arity == 1) {
		edge.block            = block;
		edge.pos              = 0;
		edge.execfreq         = get_block_execfreq(env->execfreqs, block);
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

		if(highest_edge_num >= 0)
			env->edges[highest_edge_num].highest_execfreq = 1;
	}
}

static int cmp_edges(const void *d1, const void *d2)
{
	const edge_t *e1 = d1;
	const edge_t *e2 = d2;

	return QSORT_CMP(e2->execfreq, e1->execfreq);
}

static void coalesce_blocks(blocksched_env_t *env)
{
	int i;
	int edge_count = ARR_LEN(env->edges);

	/* run1: only look at jumps */
	for (i = 0; i < edge_count; ++i) {
		const edge_t *edge  = &env->edges[i];
		ir_node      *block = edge->block;
		ir_node      *pred_block;
		blocksched_entry_t *entry, *pred_entry;

		/* the block might have been removed already... */
		if (is_Bad(get_Block_cfgpred(block, 0)))
			continue;

		/* only check edge with highest frequency */
		if (! edge->highest_execfreq)
			continue;

		pred_block = get_Block_cfgpred_block(block, edge->pos);
		entry      = get_irn_link(block);
		pred_entry = get_irn_link(pred_block);

		if (pred_entry->next != NULL || entry->prev != NULL)
			continue;

		/* only coalesce jumps */
		if (get_block_succ_next(pred_block, get_block_succ_first(pred_block)) != NULL)
			continue;

		/* schedule the 2 blocks behind each other */
		DBG((dbg, LEVEL_1, "Coalesce (Jump) %+F -> %+F (%.3g)\n",
		           pred_entry->block, entry->block, edge->execfreq));
		pred_entry->next = entry;
		entry->prev      = pred_entry;
	}

	/* run2: remaining edges */
	for (i = 0; i < edge_count; ++i) {
		const edge_t *edge  = &env->edges[i];
		ir_node      *block = edge->block;
		ir_node      *pred_block;
		blocksched_entry_t *entry, *pred_entry;

		/* the block might have been removed already... */
		if (is_Bad(get_Block_cfgpred(block, 0)))
			continue;

		pred_block = get_Block_cfgpred_block(block, edge->pos);
		entry      = get_irn_link(block);
		pred_entry = get_irn_link(pred_block);

		/* is 1 of the blocks already attached to another block? */
		if (pred_entry->next != NULL || entry->prev != NULL)
			continue;

		/* schedule the 2 blocks behind each other */
		DBG((dbg, LEVEL_1, "Coalesce (CondJump) %+F -> %+F (%.3g)\n",
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

	if (irn_visited(block))
		return;

	env->blockcount++;
	mark_irn_visited(block);

	DBG((dbg, LEVEL_1, "Pick succ of %+F\n", block));

	/* put all successors into the worklist */
	foreach_block_succ(block, edge) {
		ir_node *succ_block = get_edge_src_irn(edge);

		if (irn_visited(succ_block))
			continue;

		/* we only need to put the first of a series of already connected
		 * blocks into the worklist */
		succ_entry = get_irn_link(succ_block);
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

		DBG((dbg, LEVEL_1, "Put %+F into worklist\n", succ_entry->block));
		pdeq_putr(env->worklist, succ_entry->block);
	}

	if (entry->next != NULL) {
		pick_block_successor(entry->next, env);
		return;
	}

	DBG((dbg, LEVEL_1, "deciding...\n"));
	best_succ_execfreq = -1;

	/* no successor yet: pick the successor block with the highest execution
	 * frequency which has no predecessor yet */

	foreach_block_succ(block, edge) {
		ir_node *succ_block = get_edge_src_irn(edge);
		double  execfreq;

		if (irn_visited(succ_block))
			continue;

		succ_entry = get_irn_link(succ_block);
		if (succ_entry->prev != NULL)
			continue;

		execfreq = get_block_execfreq(env->execfreqs, succ_block);
		if (execfreq > best_succ_execfreq) {
			best_succ_execfreq = execfreq;
			succ = succ_block;
		}
	}

	if (succ == NULL) {
		DBG((dbg, LEVEL_1, "pick from worklist\n"));

		do {
			if (pdeq_empty(env->worklist)) {
				DBG((dbg, LEVEL_1, "worklist empty\n"));
				return;
			}
			succ = pdeq_getl(env->worklist);
		} while (irn_visited(succ));
	}

	succ_entry       = get_irn_link(succ);
	entry->next      = succ_entry;
	succ_entry->prev = entry;

	pick_block_successor(succ_entry, env);
}

static blocksched_entry_t *finish_block_schedule(blocksched_env_t *env)
{
	ir_graph           *irg        = env->irg;
	ir_node            *startblock = get_irg_start_block(irg);
	blocksched_entry_t *entry      = get_irn_link(startblock);

	set_using_visited(irg);
	inc_irg_visited(irg);

	env->worklist = new_pdeq();
	pick_block_successor(entry, env);
	assert(pdeq_empty(env->worklist));
	del_pdeq(env->worklist);

	clear_using_visited(irg);

	return entry;
}

static ir_node **create_blocksched_array(blocksched_env_t *env, blocksched_entry_t *first,
										int count, struct obstack* obst)
{
	int                i = 0;
	ir_node            **block_list;
	blocksched_entry_t *entry;

	block_list = NEW_ARR_D(ir_node *, obst, count);
	DBG((dbg, LEVEL_1, "Blockschedule:\n"));

	for (entry = first; entry != NULL; entry = entry->next) {
		assert(i < count);
		block_list[i++] = entry->block;
		DBG((dbg, LEVEL_1, "\t%+F\n", entry->block));
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

	// collect edge execution frequencies
	irg_block_walk_graph(irg, collect_egde_frequency, NULL, &env);

	// sort interblock edges by execution frequency
	qsort(env.edges, ARR_LEN(env.edges), sizeof(env.edges[0]), cmp_edges);

	(void)be_remove_empty_blocks(irg);

	if (algo != BLOCKSCHED_NAIV)
		coalesce_blocks(&env);

	start_entry = finish_block_schedule(&env);
	block_list  = create_blocksched_array(&env, start_entry, env.blockcount, get_irg_obstack(irg));

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
typedef struct _ilp_edge_t {
	ir_node *block;   /**< source block */
	int     pos;      /**< number of cfg predecessor (target) */
	int     ilpvar;
} ilp_edge_t;

typedef struct _blocksched_ilp_env_t {
	blocksched_env_t env;
	ilp_edge_t       *ilpedges;
	lpp_t            *lpp;
} blocksched_ilp_env_t;

typedef struct _blocksched_ilp_entry_t {
	ir_node *block;
	struct _blocksched_entry_t *next;
	struct _blocksched_entry_t *prev;

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

	entry          = obstack_alloc(env->env.obst, sizeof(entry[0]));
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
		int *edgenums = alloca(sizeof(edgenums[0]) * arity);

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
	int  i;
	int  edge_count = ARR_LEN(env->ilpedges);

	/* complete out constraints */
	for(i = 0; i < edge_count; ++i) {
		const ilp_edge_t *edge  = &env->ilpedges[i];
		ir_node          *block = edge->block;
		ir_node          *pred;
		blocksched_ilp_entry_t *entry;

		/* the block might have been removed already... */
		if (is_Bad(get_Block_cfgpred(block, 0)))
			continue;

		pred  = get_Block_cfgpred_block(block, edge->pos);
		entry = get_irn_link(pred);

		DBG((dbg, LEVEL_1, "Adding out cst to %+F from %+F,%d\n",
				  pred, block, edge->pos));
		lpp_set_factor_fast(env->lpp, entry->out_cst, edge->ilpvar, 1.0);
	}

#if 0
	{
		FILE *f;
		char fname[256];
		lpp_dump(env->lpp, "lpp.out");
		snprintf(fname, sizeof(fname), "lpp_%s.plain", get_irg_dump_name(env->env.irg));
		f = fopen(fname, "w");
		lpp_dump_plain(env->lpp, f);
		fclose(f);
	}
#endif

	//lpp_solve_net(env->lpp, main_env->options->ilp_server, main_env->options->ilp_solver);
	lpp_solve_net(env->lpp, "i44pc52", "cplex");
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
	block_list  = create_blocksched_array(&env.env, start_entry, env.env.blockcount, get_irg_obstack(irg));

	DEL_ARR_F(env.ilpedges);
	free_lpp(env.lpp);
	obstack_free(&obst, NULL);

	return block_list;
}
#endif /* WITH_ILP */

/*
 *  _____      _   ____  ____
 * | ____|_  _| |_| __ )| __ )
 * |  _| \ \/ / __|  _ \|  _ \
 * | |___ >  <| |_| |_) | |_) |
 * |_____/_/\_\\__|____/|____/
 *
 */

/** A simple forward single linked list. */
typedef struct {
	ir_node  *start;   /**< start of the list */
	ir_node  *end;     /**< last block in the list */
	unsigned n_blks;  /**< number of blocks in the list */
} anchor;

static void add_block(anchor *list, ir_node *block) {
	if (list->start == NULL) {
		list->start = block;
		list->end   = block;
	} else {
		set_irn_link(list->end, block);
		list->end = block;
	}

	list->n_blks++;
}

static void create_block_list(ir_node *leader_block, anchor *list) {
	int             i;
	const ir_edge_t *edge;
	ir_node         *block = NULL;
	ir_extblk       *extbb = get_Block_extbb(leader_block);

	if (extbb_visited(extbb))
		return;
	mark_extbb_visited(extbb);

	for (i = 0; i < get_extbb_n_blocks(extbb); ++i) {
		block = get_extbb_block(extbb, i);
		add_block(list, block);
	}

	assert(block != NULL);

	/* pick successor extbbs */
	foreach_block_succ(block, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		create_block_list(succ, list);
	}

	for (i = 0; i < get_extbb_n_blocks(extbb) - 1; ++i) {
		block = get_extbb_block(extbb, i);

		foreach_block_succ(block, edge) {
			ir_node *succ = get_edge_src_irn(edge);
			create_block_list(succ, list);
		}
	}
}

void compute_extbb_execfreqs(ir_graph *irg, ir_exec_freq *execfreqs);

/*
 * Calculates a block schedule. The schedule is stored as a linked
 * list starting at the start_block of the irg.
 */
static ir_node **create_extbb_block_schedule(ir_graph *irg, ir_exec_freq *execfreqs)
{
	anchor list;
	ir_node **blk_list, *b, *n;
	unsigned i;

	/* schedule extended basic blocks */
	compute_extbb_execfreqs(irg, execfreqs);
	//compute_extbb(irg);

	list.start  = NULL;
	list.end    = NULL;
	list.n_blks = 0;

	set_using_irn_link(irg);
	set_using_visited(irg);
	inc_irg_block_visited(irg);

	create_block_list(get_irg_start_block(irg), &list);

	/** create an array, so we can go forward and backward */
	blk_list = NEW_ARR_D(ir_node *, irg->obst,list.n_blks);

	for (i = 0, b = list.start; b; b = n, ++i) {
		n = get_irn_link(b);
		blk_list[i] = b;
	}

	clear_using_irn_link(irg);
	clear_using_visited(irg);

	return blk_list;
}

/*
 *  __  __       _
 * |  \/  | __ _(_)_ __
 * | |\/| |/ _` | | '_ \
 * | |  | | (_| | | | | |
 * |_|  |_|\__,_|_|_| |_|
 *
 */
void be_init_blocksched(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *blocksched_grp = lc_opt_get_grp(be_grp, "blocksched");

	lc_opt_add_table(blocksched_grp, be_blocksched_options);

	FIRM_DBG_REGISTER(dbg, "firm.be.blocksched");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_blocksched);

ir_node **be_create_block_schedule(ir_graph *irg, ir_exec_freq *execfreqs)
{
	switch(algo) {
	case BLOCKSCHED_GREEDY:
	case BLOCKSCHED_NAIV:
		return create_block_schedule_greedy(irg, execfreqs);
	case BLOCKSCHED_EXTBB:
		return create_extbb_block_schedule(irg, execfreqs);
#ifdef WITH_ILP
	case BLOCKSCHED_ILP:
		return create_block_schedule_ilp(irg, execfreqs);
#endif /* WITH_ILP */
	}

	assert(0 && "unknown blocksched algo");
	return NULL;
}
