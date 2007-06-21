/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       Primitive list scheduling with different node selectors.
 * @author      Sebastian Hack
 * @date        20.10.2004
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>

#include "benode_t.h"
#include "be_t.h"

#include "obst.h"
#include "list.h"
#include "iterator.h"

#include "iredges_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "irdump.h"
#include "irprintf_t.h"
#include "array.h"
#include "debug.h"
#include "irtools.h"

#include "bemodule.h"
#include "besched_t.h"
#include "beutil.h"
#include "belive_t.h"
#include "belistsched.h"
#include "beschedmris.h"
#include "beschedrss.h"
#include "bearch_t.h"
#include "bestat.h"
#include "beirg_t.h"

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL);

#define BE_SCHED_NODE(irn) (be_is_Keep(irn) || be_is_CopyKeep(irn) || be_is_RegParams(irn))

enum {
	BE_SCHED_SELECT_TRIVIAL  = 0,
	BE_SCHED_SELECT_REGPRESS = 1,
	BE_SCHED_SELECT_MUCHNIK  = 2,
	BE_SCHED_SELECT_HEUR     = 3,
	BE_SCHED_SELECT_HMUCHNIK = 4,
	BE_SCHED_SELECT_RANDOM   = 5
};

enum {
	BE_SCHED_PREP_NONE = 0,
	BE_SCHED_PREP_MRIS = 2,
	BE_SCHED_PREP_RSS  = 3
};

typedef struct _list_sched_options_t {
	int select;  /**< the node selector */
	int prep;    /**< schedule preparation */
} list_sched_options_t;

static list_sched_options_t list_sched_options = {
	BE_SCHED_SELECT_HEUR,     /* mueller heuristic selector */
	BE_SCHED_PREP_NONE,       /* no scheduling preparation */
};

/* schedule selector options. */
static const lc_opt_enum_int_items_t sched_select_items[] = {
	{ "trivial",  BE_SCHED_SELECT_TRIVIAL  },
	{ "random",   BE_SCHED_SELECT_RANDOM },
	{ "regpress", BE_SCHED_SELECT_REGPRESS },
	{ "muchnik",  BE_SCHED_SELECT_MUCHNIK  },
	{ "heur",     BE_SCHED_SELECT_HEUR     },
	{ "hmuchnik", BE_SCHED_SELECT_HMUCHNIK },
	{ NULL,       0 }
};

/* schedule preparation options. */
static const lc_opt_enum_int_items_t sched_prep_items[] = {
	{ "none", BE_SCHED_PREP_NONE },
	{ "mris", BE_SCHED_PREP_MRIS },
	{ "rss",  BE_SCHED_PREP_RSS  },
	{ NULL,   0 }
};

static lc_opt_enum_int_var_t sched_select_var = {
	&list_sched_options.select, sched_select_items
};

static lc_opt_enum_int_var_t sched_prep_var = {
	&list_sched_options.prep, sched_prep_items
};

static const lc_opt_table_entry_t list_sched_option_table[] = {
	LC_OPT_ENT_ENUM_PTR("prep",   "schedule preparation",   &sched_prep_var),
	LC_OPT_ENT_ENUM_PTR("select", "node selector",          &sched_select_var),
	LC_OPT_LAST
};

/**
 * All scheduling info needed per node.
 */
typedef struct _sched_irn_t {
	unsigned num_not_sched_user; /**< The number of not yet scheduled users of this node */
	unsigned already_sched : 1;  /**< Set if this node is already scheduled */
} sched_irn_t;

/**
 * Scheduling environment for the whole graph.
 */
typedef struct _sched_env_t {
	sched_irn_t *sched_info;                    /**< scheduling info per node */
	const list_sched_selector_t *selector;      /**< The node selector. */
	const arch_env_t *arch_env;                 /**< The architecture environment. */
	const ir_graph *irg;                        /**< The graph to schedule. */
	void *selector_env;                         /**< A pointer to give to the selector. */
} sched_env_t;

/**
 * Environment for a block scheduler.
 */
typedef struct _block_sched_env_t {
	sched_irn_t *sched_info;                    /**< scheduling info per node, copied from the global scheduler object */
	ir_nodeset_t cands;                         /**< the set of candidates */
	ir_node *block;                             /**< the current block */
	sched_env_t *sched_env;                     /**< the scheduler environment */
	ir_nodeset_t live;                          /**< simple liveness during scheduling */
	const list_sched_selector_t *selector;
	void *selector_block_env;
} block_sched_env_t;

/**
 * Returns non-zero if the node is already scheduled
 */
static INLINE int is_already_scheduled(block_sched_env_t *env, ir_node *n)
{
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	return env->sched_info[idx].already_sched;
}

/**
 * Mark a node as already scheduled
 */
static INLINE void set_already_scheduled(block_sched_env_t *env, ir_node *n)
{
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	env->sched_info[idx].already_sched = 1;
}

/**
 * Try to put a node in the ready set.
 * @param env   The block scheduler environment.
 * @param pred  The previous scheduled node.
 * @param irn   The node to make ready.
 * @return 1, if the node could be made ready, 0 else.
 */
static INLINE int make_ready(block_sched_env_t *env, ir_node *pred, ir_node *irn)
{
	int i, n;

	/* Blocks cannot be scheduled. */
	if (is_Block(irn) || get_irn_n_edges(irn) == 0)
		return 0;

	/*
	 * Check, if the given ir node is in a different block as the
	 * currently scheduled one. If that is so, don't make the node ready.
	 */
	if (env->block != get_nodes_block(irn))
		return 0;

	for (i = 0, n = get_irn_ins_or_deps(irn); i < n; ++i) {
		ir_node *op = get_irn_in_or_dep(irn, i);

		/* if irn is an End we have keep-alives and op might be a block, skip that */
		if (is_Block(op)) {
		  	assert(get_irn_op(irn) == op_End);
			continue;
		}

		/* If the operand is local to the scheduled block and not yet
		 * scheduled, this nodes cannot be made ready, so exit. */
		if (! is_already_scheduled(env, op) && get_nodes_block(op) == env->block)
			return 0;
	}

	ir_nodeset_insert(&env->cands, irn);

	/* Notify selector about the ready node. */
	if (env->selector->node_ready)
		env->selector->node_ready(env->selector_block_env, irn, pred);

    DB((dbg, LEVEL_2, "\tmaking ready: %+F\n", irn));

    return 1;
}

/**
 * Returns non-zero if a node must be placed in the schedule.
 */
static INLINE int must_appear_in_schedule(const list_sched_selector_t *sel, void *block_env, const ir_node *irn)
{
	int res = -1;

	/* if there are no uses, don't schedule */
	if (get_irn_n_edges(irn) < 1)
		return 0;

	/* else ask the scheduler */
	if (sel->to_appear_in_schedule)
		res = sel->to_appear_in_schedule(block_env, irn);

	return res >= 0 ? res : ((to_appear_in_schedule(irn) || BE_SCHED_NODE(irn)) && ! is_Unknown(irn));
}

/* forward */
static void make_users_ready(block_sched_env_t *env, ir_node *irn);

static void make_user_ready(block_sched_env_t *env, ir_node *pred, ir_node *user) {
	if (! is_Phi(user)) {
		if (! must_appear_in_schedule(env->selector, env, user)) {
			/* notify the selector about the finally selected node. */
			if (env->selector->node_selected)
				env->selector->node_selected(env->selector_block_env, user);

			/* Insert the node in the set of all available scheduled nodes. */
			set_already_scheduled(env, user);

			make_users_ready(env, user);
		} else {
			if (! ir_nodeset_contains(&env->cands, user)) {
				/* work-around: this should NEVER be true, else we have a cycle in the basic block.
				   for now it's needed to compile bzip2.c */
				if (sched_is_scheduled(user)) {
					//assert(!"make an already scheduled user ready");
				}
				else {
					make_ready(env, pred, user);
				}
			}
		}
	}
}


/**
 * Try, to make all users of a node ready.
 * In fact, a usage node can only be made ready, if all its operands
 * have already been scheduled yet. This is checked by make_ready().
 * @param env The block schedule environment.
 * @param irn The node, which usages (successors) are to be made ready.
 */
static void make_users_ready(block_sched_env_t *env, ir_node *irn)
{
	const ir_edge_t *edge;

	/* make all data users ready */
	foreach_out_edge(irn, edge) {
		ir_node *user = get_edge_src_irn(edge);

		if (get_block(user) == env->block)
			make_user_ready(env, irn, user);
	}

	/* and the dependent nodes as well */
	foreach_out_edge_kind(irn, edge, EDGE_KIND_DEP) {
		ir_node *user = get_edge_src_irn(edge);

		if (get_block(user) == env->block)
			make_user_ready(env, irn, user);
	}
}

/**
 * Returns the number of not yet schedules users.
 */
static INLINE int get_irn_not_sched_user(block_sched_env_t *env, ir_node *n) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	return env->sched_info[idx].num_not_sched_user;
}

/**
 * Sets the number of not yet schedules users.
 */
static INLINE void set_irn_not_sched_user(block_sched_env_t *env, ir_node *n, int num) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	env->sched_info[idx].num_not_sched_user = num;
}

/**
 * Add @p num to the number of not yet schedules users and returns the result.
 */
static INLINE int add_irn_not_sched_user(block_sched_env_t *env, ir_node *n, int num) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	env->sched_info[idx].num_not_sched_user += num;
	return env->sched_info[idx].num_not_sched_user;
}

/**
 * Returns the number of users of a node having mode datab.
 */
static int get_num_successors(ir_node *irn) {
	int             sum = 0;
	const ir_edge_t *edge;

	if (get_irn_mode(irn) == mode_T) {
		/* for mode_T nodes: count the users of all Projs */
		foreach_out_edge(irn, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			ir_mode *mode = get_irn_mode(proj);

			if (mode == mode_T) {
				sum += get_num_successors(proj);
			} else if (mode_is_datab(mode)) {
				sum += get_irn_n_edges(proj);
			}
		}
	}
	else {
		/* do not count keep-alive edges */
		foreach_out_edge(irn, edge) {
			if (get_irn_opcode(get_edge_src_irn(edge)) != iro_End)
				sum++;
		}
	}

	return sum;
}

/**
 * Adds irn to @p live, updates all inputs that this user is scheduled
 * and counts all of its non scheduled users.
 */
static void update_sched_liveness(block_sched_env_t *env, ir_node *irn) {
	int i;

	/* ignore Projs */
	if (is_Proj(irn))
		return;

	for (i = get_irn_ins_or_deps(irn) - 1; i >= 0; --i) {
		ir_node *in = get_irn_in_or_dep(irn, i);

		/* if in is a proj: update predecessor */
		in = skip_Proj(in);

		/* if in is still in the live set: reduce number of users by one */
		if (ir_nodeset_contains(&env->live, in)) {
			if (add_irn_not_sched_user(env, in, -1) <= 0)
				ir_nodeset_remove(&env->live, in);
		}
	}

	/*
		get_num_successors returns the number of all users. This includes
		users in different blocks as well. As the each block is scheduled separately
		the liveness info of those users will not be updated and so these
		users will keep up the register pressure as it is desired.
	*/
	i = get_num_successors(irn);
	if (i > 0) {
		set_irn_not_sched_user(env, irn, i);
		ir_nodeset_insert(&env->live, irn);
	}
}

/**
 * Append an instruction to a schedule.
 * @param env The block scheduling environment.
 * @param irn The node to add to the schedule.
 * @return    The given node.
 */
static ir_node *add_to_sched(block_sched_env_t *env, ir_node *irn)
{
    /* If the node consumes/produces data, it is appended to the schedule
     * list, otherwise, it is not put into the list */
    if (must_appear_in_schedule(env->selector, env->selector_block_env, irn)) {
		update_sched_liveness(env, irn);
        sched_add_before(env->block, irn);

        DBG((dbg, LEVEL_2, "\tadding %+F\n", irn));
    }

	/* notify the selector about the finally selected node. */
	if (env->selector->node_selected)
		env->selector->node_selected(env->selector_block_env, irn);

    /* Insert the node in the set of all available scheduled nodes. */
    set_already_scheduled(env, irn);

    /* Remove the node from the ready set */
	ir_nodeset_remove(&env->cands, irn);

    return irn;
}

#ifdef SCHEDULE_PROJS
/**
 * Add the proj nodes of a tuple-mode irn to the schedule immediately
 * after the tuple-moded irn. By pinning the projs after the irn, no
 * other nodes can create a new lifetime between the tuple-moded irn and
 * one of its projs. This should render a realistic image of a
 * tuple-moded irn, which in fact models a node which defines multiple
 * values.
 *
 * @param irn The tuple-moded irn.
 */
static void add_tuple_projs(block_sched_env_t *env, ir_node *irn)
{
	const ir_edge_t *edge;

	assert(get_irn_mode(irn) == mode_T && "Mode of node must be tuple");

	if (is_Bad(irn))
		return;


	/* non-proj nodes can have dependency edges to tuple nodes. */
	foreach_out_edge_kind(irn, edge, EDGE_KIND_DEP) {
		ir_node *out = get_edge_src_irn(edge);
		make_ready(env, irn, out);
	}

	/* schedule the normal projs */
	foreach_out_edge(irn, edge) {
		ir_node *out = get_edge_src_irn(edge);

		assert(is_Proj(out) && "successor of a modeT node must be a proj");

		if (get_irn_mode(out) == mode_T)
			add_tuple_projs(env, out);
		else {
			add_to_sched(env, out);
			make_users_ready(env, out);
		}
	}
}
#endif

/**
 * Perform list scheduling on a block.
 *
 * Note, that the caller must compute a linked list of nodes in the block
 * using the link field before calling this function.
 *
 * Also the outs must have been computed.
 *
 * @param block The block node.
 * @param env Scheduling environment.
 */
static void list_sched_block(ir_node *block, void *env_ptr)
{
	sched_env_t *env                      = env_ptr;
	const list_sched_selector_t *selector = env->selector;
	ir_node *start_node                   = get_irg_start(get_irn_irg(block));

	block_sched_env_t be;
	const ir_edge_t *edge;
	ir_node *irn;
	int j, m;

	/* Initialize the block's list head that will hold the schedule. */
	sched_init_block(block);

	/* Initialize the block scheduling environment */
	be.sched_info = env->sched_info;
	be.block      = block;
	ir_nodeset_init_size(&be.cands, get_irn_n_edges(block));
	ir_nodeset_init_size(&be.live, get_irn_n_edges(block));
	be.selector   = selector;
	be.sched_env  = env;

	DBG((dbg, LEVEL_1, "scheduling %+F\n", block));

	if (selector->init_block)
		be.selector_block_env = selector->init_block(env->selector_env, block);

	/* Then one can add all nodes are ready to the set. */
	foreach_out_edge(block, edge) {
		ir_node *irn = get_edge_src_irn(edge);

		/* Skip the end node because of keepalive edges. */
		if (get_irn_opcode(irn) == iro_End)
			continue;

		if (get_irn_n_edges(irn) == 0)
			continue;

		if (is_Phi(irn)) {
			/*
				Phi functions are scheduled immediately, since they	only
				transfer data flow from the predecessors to this block.
			*/
			add_to_sched(&be, irn);
			make_users_ready(&be, irn);
		}
		else if (irn == start_node) {
			/* The start block will be scheduled as the first node */
			add_to_sched(&be, irn);
#ifdef SCHEDULE_PROJS
			add_tuple_projs(&be, irn);
#else
			make_users_ready(&be, irn);
#endif
		}
		else {
			/* Other nodes must have all operands in other blocks to be made
			* ready */
			int ready = 1;

			/* Check, if the operands of a node are not local to this block */
			for (j = 0, m = get_irn_ins_or_deps(irn); j < m; ++j) {
				ir_node *operand = get_irn_in_or_dep(irn, j);

				if (get_nodes_block(operand) == block) {
					ready = 0;
					break;
				} else {
					/* live in values increase register pressure */
					ir_nodeset_insert(&be.live, operand);
				}
			}

			/* Make the node ready, if all operands live in a foreign block */
			if (ready) {
				DBG((dbg, LEVEL_2, "\timmediately ready: %+F\n", irn));
				make_ready(&be, NULL, irn);
			}
		}
	}

	/* Iterate over all remaining nodes */
	while (ir_nodeset_size(&be.cands) > 0) {
		ir_nodeset_iterator_t iter;
		/* collect statistics about amount of ready nodes */
		be_do_stat_sched_ready(block, &be.cands);

		/* Keeps must be scheduled immediately */
		foreach_ir_nodeset(&be.cands, irn, iter) {
			if (be_is_Keep(irn) || be_is_CopyKeep(irn) || is_Sync(irn)) {
				break;
			}
		}

		if (! irn) {
			/* Keeps must be immediately scheduled */
			irn = be.selector->select(be.selector_block_env, &be.cands, &be.live);
		}

		DB((dbg, LEVEL_2, "\tpicked node %+F\n", irn));

		/* Add the node to the schedule. */
		add_to_sched(&be, irn);

#ifdef SCHEDULE_PROJS
		if (get_irn_mode(irn) == mode_T)
			add_tuple_projs(&be, irn);
		else
#endif
		{
			make_users_ready(&be, irn);
		}

		/* remove the scheduled node from the ready list. */
		ir_nodeset_remove(&be.cands, irn);
	}

	if (selector->finish_block)
		selector->finish_block(be.selector_block_env);

	ir_nodeset_destroy(&be.cands);
	ir_nodeset_destroy(&be.live);
}

/* List schedule a graph. */
void list_sched(const be_irg_t *birg, be_options_t *be_opts)
{
	const arch_env_t *arch_env = birg->main_env->arch_env;
	ir_graph         *irg      = birg->irg;

	int num_nodes;
	sched_env_t env;
	mris_env_t *mris = NULL;
	list_sched_selector_t sel;

	(void)be_opts;

	/* Select a scheduler based on backend options */
	switch (list_sched_options.select) {
		case BE_SCHED_SELECT_TRIVIAL:
			memcpy(&sel, trivial_selector, sizeof(sel));
			break;
		case BE_SCHED_SELECT_RANDOM:
			memcpy(&sel, random_selector, sizeof(sel));
			break;
		case BE_SCHED_SELECT_REGPRESS:
			memcpy(&sel, reg_pressure_selector, sizeof(sel));
			break;
		case BE_SCHED_SELECT_MUCHNIK:
			memcpy(&sel, muchnik_selector, sizeof(sel));
			break;
		case BE_SCHED_SELECT_HEUR:
			memcpy(&sel, heuristic_selector, sizeof(sel));
			break;
		case BE_SCHED_SELECT_HMUCHNIK:
		default:
			memcpy(&sel, trivial_selector, sizeof(sel));
	}

#if 1
	/* Matze: This is very slow, we should avoid it to improve backend speed,
	 * we just have to make sure that we have no dangling out-edges at this
	 * point...
	 */

	/* Assure, that we have no dangling out-edges to deleted stuff */
	edges_deactivate(birg->irg);
	edges_activate(birg->irg);
#endif

	switch (list_sched_options.prep) {
		case BE_SCHED_PREP_MRIS:
			mris = be_sched_mris_preprocess(birg);
			break;
		case BE_SCHED_PREP_RSS:
			rss_schedule_preparation(birg);
			break;
		default:
			break;
	}

	num_nodes = get_irg_last_idx(irg);

	/* initialize environment for list scheduler */
	memset(&env, 0, sizeof(env));
	env.selector   = arch_env->isa->impl->get_list_sched_selector(arch_env->isa, &sel);
	env.arch_env   = arch_env;
	env.irg        = irg;
	env.sched_info = NEW_ARR_F(sched_irn_t, num_nodes);

	memset(env.sched_info, 0, num_nodes * sizeof(env.sched_info[0]));

	if (env.selector->init_graph)
		env.selector_env = env.selector->init_graph(env.selector, arch_env, irg);

	/* Schedule each single block. */
	irg_block_walk_graph(irg, list_sched_block, NULL, &env);

	if (env.selector->finish_graph)
		env.selector->finish_graph(env.selector_env);

	if (list_sched_options.prep == BE_SCHED_PREP_MRIS)
		be_sched_mris_free(mris);

	DEL_ARR_F(env.sched_info);
}

/* List schedule a block. */
void list_sched_single_block(const be_irg_t *birg, ir_node *block,
                             be_options_t *be_opts)
{
	const arch_env_t *arch_env = birg->main_env->arch_env;
	ir_graph         *irg      = birg->irg;

	int num_nodes;
	sched_env_t env;
	list_sched_selector_t sel;

	(void)be_opts;

	/* Select a scheduler based on backend options */
	switch (list_sched_options.select) {
		case BE_SCHED_SELECT_TRIVIAL:
			memcpy(&sel, trivial_selector, sizeof(sel));
			break;
		case BE_SCHED_SELECT_RANDOM:
			memcpy(&sel, random_selector, sizeof(sel));
			break;
		case BE_SCHED_SELECT_REGPRESS:
			memcpy(&sel, reg_pressure_selector, sizeof(sel));
			break;
		case BE_SCHED_SELECT_MUCHNIK:
			memcpy(&sel, muchnik_selector, sizeof(sel));
			break;
		case BE_SCHED_SELECT_HEUR:
			memcpy(&sel, heuristic_selector, sizeof(sel));
			break;
		case BE_SCHED_SELECT_HMUCHNIK:
		default:
			memcpy(&sel, trivial_selector, sizeof(sel));
	}

	/* Assure, that the out edges are computed */
	edges_deactivate(birg->irg);
	edges_activate(birg->irg);

	num_nodes = get_irg_last_idx(irg);

	/* initialize environment for list scheduler */
	memset(&env, 0, sizeof(env));
	env.selector   = arch_env->isa->impl->get_list_sched_selector(arch_env->isa, &sel);
	env.arch_env   = arch_env;
	env.irg        = irg;
	env.sched_info = NEW_ARR_F(sched_irn_t, num_nodes);

	memset(env.sched_info, 0, num_nodes * sizeof(env.sched_info[0]));

	if (env.selector->init_graph)
		env.selector_env = env.selector->init_graph(env.selector, arch_env, irg);

	/* Schedule block. */
	list_sched_block(block, &env);

	if (env.selector->finish_graph)
		env.selector->finish_graph(env.selector_env);

	DEL_ARR_F(env.sched_info);
}

/**
 * Register list scheduler options.
 */
void be_init_listsched(void) {
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *sched_grp = lc_opt_get_grp(be_grp, "listsched");

	lc_opt_add_table(sched_grp, list_sched_option_table);

	FIRM_DBG_REGISTER(dbg, "firm.be.sched");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_listsched);
