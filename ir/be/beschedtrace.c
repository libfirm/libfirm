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
 * @brief       Implements a trace scheduler as presented in Muchnik[TM].
 * @author      Michael Beck
 * @date        28.08.2006
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "iredges_t.h"

#include "besched_t.h"
#include "belistsched.h"
#include "benode_t.h"

/* we need a special mark */
static char _mark;
#define MARK &_mark

typedef struct _trace_irn {
	sched_timestep_t delay;      /**< The delay for this node if already calculated, else 0. */
	sched_timestep_t etime;      /**< The earliest time of this node. */
	unsigned num_user;           /**< The number real users (mode datab) of this node */
	int      reg_diff;           /**< The difference of num(out registers) - num(in registers) */
	int      preorder;           /**< The pre-order position */
	unsigned critical_path_len;  /**< The weighted length of the longest critical path */
	unsigned is_root       : 1;  /**< is a root node of a block */
} trace_irn_t;

typedef struct _trace_env {
	trace_irn_t      *sched_info;               /**< trace scheduling information about the nodes */
	const arch_env_t *arch_env;                 /**< the arch environment */
	sched_timestep_t curr_time;                 /**< current time of the scheduler */
	void             *selector_env;             /**< the backend selector environment */
	const list_sched_selector_t *selector;      /**< the actual backend selector */
	be_lv_t          *liveness;                 /**< The liveness for the irg */
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
} trace_env_t;

/**
 * Returns a random node from a nodeset
 */
static ir_node *get_nodeset_node(const ir_nodeset_t *nodeset)
{
	ir_nodeset_iterator_t iter;

	ir_nodeset_iterator_init(&iter, nodeset);
	return ir_nodeset_iterator_next(&iter);
}

/**
 * Returns non-zero if the node is a root node
 */
static INLINE unsigned is_root_node(trace_env_t *env, ir_node *n)
{
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	return env->sched_info[idx].is_root;
}

/**
 * Mark a node as root node
 */
static INLINE void mark_root_node(trace_env_t *env, ir_node *n)
{
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	env->sched_info[idx].is_root = 1;
}

/**
 * Get the current delay.
 */
static INLINE sched_timestep_t get_irn_delay(trace_env_t *env, ir_node *n) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	return env->sched_info[idx].delay;
}

/**
 * Set the current delay.
 */
static INLINE void set_irn_delay(trace_env_t *env, ir_node *n, sched_timestep_t delay) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	env->sched_info[idx].delay = delay;
}

/**
 * Get the current etime.
 */
static INLINE sched_timestep_t get_irn_etime(trace_env_t *env, ir_node *n) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	return env->sched_info[idx].etime;
}

/**
 * Set the current etime.
 */
static INLINE void set_irn_etime(trace_env_t *env, ir_node *n, sched_timestep_t etime) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	env->sched_info[idx].etime = etime;
}

/**
 * Get the number of users.
 */
static INLINE unsigned get_irn_num_user(trace_env_t *env, ir_node *n) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	return env->sched_info[idx].num_user;
}

/**
 * Set the number of users.
 */
static INLINE void set_irn_num_user(trace_env_t *env, ir_node *n, unsigned num_user) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	env->sched_info[idx].num_user = num_user;
}

/**
 * Get the register difference.
 */
static INLINE int get_irn_reg_diff(trace_env_t *env, ir_node *n) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	return env->sched_info[idx].reg_diff;
}

/**
 * Set the register difference.
 */
static INLINE void set_irn_reg_diff(trace_env_t *env, ir_node *n, int reg_diff) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	env->sched_info[idx].reg_diff = reg_diff;
}

/**
 * Get the pre-order position.
 */
static INLINE int get_irn_preorder(trace_env_t *env, ir_node *n) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	return env->sched_info[idx].preorder;
}

/**
 * Set the pre-order position.
 */
static INLINE void set_irn_preorder(trace_env_t *env, ir_node *n, int pos) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	env->sched_info[idx].preorder = pos;
}

/**
 * Get the pre-order position.
 */
static INLINE unsigned get_irn_critical_path_len(trace_env_t *env, ir_node *n) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	return env->sched_info[idx].critical_path_len;
}

/**
 * Set the pre-order position.
 */
static INLINE void set_irn_critical_path_len(trace_env_t *env, ir_node *n, unsigned len) {
	int idx = get_irn_idx(n);

	assert(idx < ARR_LEN(env->sched_info));
	env->sched_info[idx].critical_path_len = len;
}

/**
 * returns the exec-time for node n.
 */
static sched_timestep_t exectime(trace_env_t *env, ir_node *n) {
	if (be_is_Keep(n) || is_Proj(n))
		return 0;
	if (env->selector->exectime)
		return env->selector->exectime(env->selector_env, n);
	return 1;
}

/**
 * Calculates the latency for between two ops
 */
static sched_timestep_t latency(trace_env_t *env, ir_node *pred, int pred_cycle, ir_node *curr, int curr_cycle) {
	/* a Keep hides a root */
	if (be_is_Keep(curr))
		return exectime(env, pred);

	/* Proj's are executed immediately */
	if (is_Proj(curr))
		return 0;

	/* predecessors Proj's must be skipped */
	if (is_Proj(pred))
		pred = get_Proj_pred(pred);

	if (env->selector->latency)
		return env->selector->latency(env->selector_env, pred, pred_cycle, curr, curr_cycle);
	return 1;
}

/**
 * Returns the number of users of a node having mode datab.
 */
static int get_num_successors(ir_node *irn) {
	int sum = 0;
	const ir_edge_t *edge;

	if (get_irn_mode(irn) == mode_T) {
		/* for mode_T nodes: count the users of all Projs */
		foreach_out_edge(irn, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			ir_mode *mode = get_irn_mode(proj);

			if (mode == mode_T)
				sum += get_num_successors(proj);
			else if (mode_is_datab(mode))
				sum += get_irn_n_edges(proj);
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
 * Returns the difference of regs_output - regs_input;
 */
static int get_reg_difference(trace_env_t *env, ir_node *irn) {
	int num_out = 0;
	int num_in  = 0;
	int i;
	ir_node *block = get_nodes_block(irn);

	if (be_is_Call(irn)) {
		/* we want calls prefered */
		return -5;
	}

	if (get_irn_mode(irn) == mode_T) {
		/* mode_T nodes: num out regs == num Projs with mode datab */
		const ir_edge_t *edge;
		foreach_out_edge(irn, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			if (mode_is_datab(get_irn_mode(proj)))
				num_out++;
		}
	}
	else
		num_out = 1;

	/* num in regs: number of ins with mode datab and not ignore */
	for (i = get_irn_arity(irn) - 1; i >= 0; i--) {
		ir_node *in = get_irn_n(irn, i);

		if (! be_is_live_end(env->liveness, block, in) &&  /* if the value lives outside of block: do not count */
			mode_is_datab(get_irn_mode(in))             &&  /* must be data node */
			! arch_irn_is(env->arch_env, in, ignore))       /* ignore "ignore" nodes :) */
			num_in++;
	}

	return num_out - num_in;
}

/**
 * descent into a dag and create a pre-order list.
 */
static void descent(ir_node *root, ir_node *block, ir_node **list, trace_env_t *env, unsigned path_len) {
	int i;

	if (! is_Phi(root)) {
		path_len += exectime(env, root);
		if (get_irn_critical_path_len(env, root) < path_len) {
			set_irn_critical_path_len(env, root, path_len);
		}
		/* calculate number of users (needed for heuristic) */
		set_irn_num_user(env, root, get_num_successors(root));

		/* calculate register difference (needed for heuristic) */
		set_irn_reg_diff(env, root, get_reg_difference(env, root));

		/* Phi nodes always leave the block */
		for (i = get_irn_arity(root) - 1; i >= 0; --i) {
			ir_node *pred = get_irn_n(root, i);

			DBG((env->dbg, LEVEL_3, "   node %+F\n", pred));

			/* Blocks may happen as predecessors of End nodes */
			if (is_Block(pred))
				continue;

			/* already seen nodes are not marked */
			if (get_irn_link(pred) != MARK)
				continue;

			/* don't leave our block */
			if (get_nodes_block(pred) != block)
				continue;

			set_irn_link(pred, NULL);

			descent(pred, block, list, env, path_len);
		}
	}
	set_irn_link(root, *list);
	*list = root;
}

/**
 * Returns non-zero if root is a root in the block block.
 */
static int is_root(ir_node *root, ir_node *block) {
	const ir_edge_t *edge;

	foreach_out_edge(root, edge) {
		ir_node *succ = get_edge_src_irn(edge);

		if (is_Block(succ))
			continue;
		/* Phi nodes are always in "another block */
		if (is_Phi(succ))
			continue;
		if (get_nodes_block(succ) == block)
			return 0;
	}
	return 1;
}

/**
 * Performs initial block calculations for trace scheduling.
 */
static void trace_preprocess_block(trace_env_t *env, ir_node *block) {
	ir_node *root = NULL, *preord = NULL;
	ir_node *curr, *irn;
	int cur_pos;
	const ir_edge_t *edge;

	/* First step: Find the root set. */
	foreach_out_edge(block, edge) {
		ir_node *succ = get_edge_src_irn(edge);

		if (is_Anchor(succ))
			continue;
		if (is_root(succ, block)) {
			mark_root_node(env, succ);
			set_irn_link(succ, root);
			root = succ;
		}
		else
			set_irn_link(succ, MARK);
	}

	/* Second step: calculate the pre-order list. */
	preord = NULL;
	for (curr = root; curr; curr = irn) {
		irn = get_irn_link(curr);
		DBG((env->dbg, LEVEL_2, "   DAG root %+F\n", curr));
		descent(curr, block, &preord, env, 0);
	}
	root = preord;

	/* Third step: calculate the Delay. Note that our
	* list is now in pre-order, starting at root
	*/
	for (cur_pos = 0, curr = root; curr; curr = get_irn_link(curr), cur_pos++) {
		sched_timestep_t d;

		if (arch_irn_class_is(env->arch_env, curr, branch)) {
			/* assure, that branches can be executed last */
			d = 0;
		}
		else {
			if (is_root_node(env, curr))
				d = exectime(env, curr);
			else {
				d = 0;
				foreach_out_edge(curr, edge) {
					ir_node *n = get_edge_src_irn(edge);

					if (get_nodes_block(n) == block) {
						sched_timestep_t ld;

						ld = latency(env, curr, 1, n, 0) + get_irn_delay(env, n);
						d = ld > d ? ld : d;
					}
				}
			}
		}
		set_irn_delay(env, curr, d);
		DB((env->dbg, LEVEL_2, "\t%+F delay %u\n", curr, d));

		/* set the etime of all nodes to 0 */
		set_irn_etime(env, curr, 0);

		set_irn_preorder(env, curr, cur_pos);
	}
}

/**
 * This functions gets called after a node finally has been made ready.
 */
static void trace_node_ready(void *data, ir_node *irn, ir_node *pred) {
	trace_env_t *env = data;
	sched_timestep_t etime_p, etime;

	etime = env->curr_time;
	if (pred) {
		etime_p = get_irn_etime(env, pred);
		etime  += latency(env, pred, 1, irn, 0);
		etime   = etime_p > etime ? etime_p : etime;
	}

	set_irn_etime(env, irn, etime);
	DB((env->dbg, LEVEL_2, "\tset etime of %+F to %u\n", irn, etime));
}

/**
 * Update the current time after irn has been selected.
 */
static void trace_update_time(void *data, ir_node *irn) {
	trace_env_t *env = data;
	if (is_Phi(irn) || get_irn_opcode(irn) == iro_Start) {
		env->curr_time += get_irn_etime(env, irn);
	}
	else {
		env->curr_time += exectime(env, irn);
	}
}

/**
 * Allocates memory and initializes trace scheduling environment.
 * @param birg   The backend irg object
 * @return The environment
 */
static trace_env_t *trace_init(const be_irg_t *birg) {
	trace_env_t *env = xcalloc(1, sizeof(*env));
	ir_graph    *irg = be_get_birg_irg(birg);
	int         nn   = get_irg_last_idx(irg);

	env->arch_env   = be_get_birg_arch_env(birg);
	env->curr_time  = 0;
	env->sched_info = NEW_ARR_F(trace_irn_t, nn);
	env->liveness   = be_liveness(birg);
	FIRM_DBG_REGISTER(env->dbg, "firm.be.sched.trace");

	be_liveness_assure_chk(env->liveness);
	memset(env->sched_info, 0, nn * sizeof(*(env->sched_info)));

	return env;
}

/**
 * Frees all memory allocated for trace scheduling environment.
 * @param env  The environment
 */
static void trace_free(void *data) {
	trace_env_t *env = data;
	be_liveness_free(env->liveness);
	DEL_ARR_F(env->sched_info);
	free(env);
}

/**
 * Simple selector. Just assure that jumps are scheduled last.
 */
static ir_node *basic_selection(const arch_env_t *arch_env, ir_nodeset_t *ready_set) {
	ir_node *irn = NULL;
	ir_nodeset_iterator_t iter;

	/* assure that branches and constants are executed last */
	foreach_ir_nodeset(ready_set, irn, iter) {
		if (! arch_irn_class_is(arch_env, irn, branch)) {
			return irn;
		}
	}

	/* at last: schedule branches */
	irn = get_nodeset_node(ready_set);

	return irn;
}

/**
* The muchnik selector.
*/
static ir_node *muchnik_select(void *block_env, ir_nodeset_t *ready_set, ir_nodeset_t *live_set)
{
	trace_env_t *env = block_env;
	ir_nodeset_t mcands, ecands;
	ir_nodeset_iterator_t iter;
	sched_timestep_t max_delay = 0;
	ir_node *irn;
	(void) live_set;

	/* calculate the max delay of all candidates */
	foreach_ir_nodeset(ready_set, irn, iter) {
		sched_timestep_t d = get_irn_delay(env, irn);

		max_delay = d > max_delay ? d : max_delay;
	}

	ir_nodeset_init_size(&mcands, 8);
	ir_nodeset_init_size(&ecands, 8);

	/* build mcands and ecands */
	foreach_ir_nodeset(ready_set, irn, iter) {
		if (get_irn_delay(env, irn) == max_delay) {
			ir_nodeset_insert(&mcands, irn);
			if (get_irn_etime(env, irn) <= env->curr_time)
				ir_nodeset_insert(&ecands, irn);
		}
	}

	/* select a node */
	if (ir_nodeset_size(&mcands) == 1) {
		irn = get_nodeset_node(&mcands);
		DB((env->dbg, LEVEL_3, "\tirn = %+F, mcand = 1, max_delay = %u\n", irn, max_delay));
	}
	else {
		int cnt = ir_nodeset_size(&ecands);
		if (cnt == 1) {
			irn = get_nodeset_node(&ecands);

			if (arch_irn_class_is(env->arch_env, irn, branch)) {
				/* BEWARE: don't select a JUMP if others are still possible */
				goto force_mcands;
			}
			DB((env->dbg, LEVEL_3, "\tirn = %+F, ecand = 1, max_delay = %u\n", irn, max_delay));
		}
		else if (cnt > 1) {
			DB((env->dbg, LEVEL_3, "\tecand = %d, max_delay = %u\n", cnt, max_delay));
			irn = basic_selection(env->arch_env, &ecands);
		}
		else {
force_mcands:
			DB((env->dbg, LEVEL_3, "\tmcand = %d\n", ir_nodeset_size(&mcands)));
			irn = basic_selection(env->arch_env, &mcands);
		}
	}

	return irn;
}

static void *muchnik_init_graph(const list_sched_selector_t *vtab, const be_irg_t *birg)
{
	trace_env_t *env  = trace_init(birg);
	env->selector     = vtab;
	env->selector_env = (void*) be_get_birg_arch_env(birg);
	return (void *)env;
}

static void *muchnik_init_block(void *graph_env, ir_node *bl)
{
	trace_preprocess_block(graph_env, bl);
	return graph_env;
}

const list_sched_selector_t muchnik_selector = {
	muchnik_init_graph,
	muchnik_init_block,
	muchnik_select,
	NULL,                /* to_appear_in_schedule */
	trace_node_ready,    /* node_ready */
	trace_update_time,   /* node_selected */
	NULL,                /* exectime */
	NULL,                /* latency */
	NULL,                /* finish_block */
	trace_free           /* finish_graph */
};

/**
 * Execute the heuristic function.
 */
static ir_node *heuristic_select(void *block_env, ir_nodeset_t *ns, ir_nodeset_t *lv)
{
	trace_env_t *trace_env   = block_env;
	ir_node     *irn, *cand  = NULL;
	int         max_prio     = INT_MIN;
	int         cur_prio     = INT_MIN;
	int         cur_pressure = ir_nodeset_size(lv);
	int         reg_fact, cand_reg_fact;
	ir_nodeset_iterator_t iter;

	/* prefer instructions which can be scheduled early */
#define PRIO_TIME        3
	/* prefer instructions with lots of successors */
#define PRIO_NUMSUCCS    8
	/* prefer instructions with long critical path */
#define PRIO_LEVEL      12
	/* prefer instructions coming early in preorder */
#define PRIO_PREORD      8
	/* weight of current register pressure */
#define PRIO_CUR_PRESS  20
	/* weight of register pressure difference */
#define PRIO_CHG_PRESS   8

	/* priority based selection, heuristic inspired by mueller diss */
	foreach_ir_nodeset(ns, irn, iter) {
		/* make sure that branches are scheduled last */
		if (! arch_irn_class_is(trace_env->arch_env, irn, branch)) {
			int rdiff = get_irn_reg_diff(trace_env, irn);
			int sign  = rdiff < 0;
			int chg   = (rdiff < 0 ? -rdiff : rdiff) << PRIO_CHG_PRESS;

			/* reg_fact = chg << cur_pressure; */
			reg_fact = chg * cur_pressure;
			if (reg_fact < chg)
				reg_fact = INT_MAX - 2;
			reg_fact = sign ? -reg_fact : reg_fact;

			cur_prio = (get_irn_critical_path_len(trace_env, irn) << PRIO_LEVEL)
				//- (get_irn_delay(trace_env, irn) << PRIO_LEVEL)
				+ (get_irn_num_user(trace_env, irn) << PRIO_NUMSUCCS)
				- (get_irn_etime(trace_env, irn) << PRIO_TIME)
				//- ((get_irn_reg_diff(trace_env, irn) >> PRIO_CHG_PRESS) << ((cur_pressure >> PRIO_CUR_PRESS) - 3))
				- reg_fact
				+ (get_irn_preorder(trace_env, irn) << PRIO_PREORD); /* high preorder means early schedule */
			if (cur_prio > max_prio) {
				cand          = irn;
				max_prio      = cur_prio;
				cand_reg_fact = reg_fact;
			}

			DBG((trace_env->dbg, LEVEL_4, "checked NODE %+F\n", irn));
			DBG((trace_env->dbg, LEVEL_4, "\tpriority: %d\n", cur_prio));
			DBG((trace_env->dbg, LEVEL_4, "\tpath len: %d (%d)\n", get_irn_critical_path_len(trace_env, irn), get_irn_critical_path_len(trace_env, irn) << PRIO_LEVEL));
			DBG((trace_env->dbg, LEVEL_4, "\tdelay:    %d (%d)\n", get_irn_delay(trace_env, irn), get_irn_delay(trace_env, irn) << PRIO_LEVEL));
			DBG((trace_env->dbg, LEVEL_4, "\t#user:    %d (%d)\n", get_irn_num_user(trace_env, irn), get_irn_num_user(trace_env, irn) << PRIO_NUMSUCCS));
			DBG((trace_env->dbg, LEVEL_4, "\tetime:    %d (%d)\n", get_irn_etime(trace_env, irn), 0 - (get_irn_etime(trace_env, irn) << PRIO_TIME)));
			DBG((trace_env->dbg, LEVEL_4, "\tpreorder: %d (%d)\n", get_irn_preorder(trace_env, irn), get_irn_preorder(trace_env, irn) << PRIO_PREORD));
			DBG((trace_env->dbg, LEVEL_4, "\treg diff: %d (%d)\n", get_irn_reg_diff(trace_env, irn), 0 - reg_fact));
			DBG((trace_env->dbg, LEVEL_4, "\tpressure: %d\n", cur_pressure));
		}
	}

	if (cand) {
		DBG((trace_env->dbg, LEVEL_4, "heuristic selected %+F:\n", cand));
	}
	else {
		cand = basic_selection(trace_env->arch_env, ns);
	}

	return cand;
}

const list_sched_selector_t heuristic_selector = {
	muchnik_init_graph,
	muchnik_init_block,
	heuristic_select,
	NULL,                /* to_appear_in_schedule */
	trace_node_ready,    /* node_ready */
	trace_update_time,   /* node_selected */
	NULL,                /* exectime */
	NULL,                /* latency */
	NULL,                /* finish_block */
	trace_free           /* finish_graph */
};
