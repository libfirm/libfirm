/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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
 * @brief       Various verify routines that check a scheduled graph for correctness.
 * @author      Matthias Braun
 * @date        05.05.2006
 */
#include "config.h"

#include <limits.h>
#include <stdbool.h>

#include "bitset.h"
#include "set.h"
#include "array.h"

#include "irnode.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irdump_t.h"
#include "iredges.h"

#include "beverify.h"
#include "belive.h"
#include "besched.h"
#include "benode.h"
#include "beirg.h"
#include "beintlive_t.h"
#include "belistsched.h"

static int my_values_interfere(const ir_node *a, const ir_node *b);

typedef struct be_verify_register_pressure_env_t_ {
	ir_graph                    *irg;                 /**< the irg to verify */
	 be_lv_t                    *lv;                  /**< Liveness information. */
	const arch_register_class_t *cls;                 /**< the register class to check for */
	int                         registers_available;  /**< number of available registers */
	int                         problem_found;        /**< flag indicating if a problem was found */
} be_verify_register_pressure_env_t;

/**
 * Print all nodes of a pset into a file.
 */
static void print_living_values(FILE *F, const ir_nodeset_t *live_nodes)
{
	ir_fprintf(F, "\t");
	foreach_ir_nodeset(live_nodes, node, iter) {
		ir_fprintf(F, "%+F ", node);
	}
	ir_fprintf(F, "\n");
}

static const char *get_irg_name(ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);
	return get_entity_ld_name(entity);
}

/**
 * Check if number of live nodes never exceeds the number of available registers.
 */
static void verify_liveness_walker(ir_node *block, void *data)
{
	be_verify_register_pressure_env_t *env = (be_verify_register_pressure_env_t *)data;
	ir_nodeset_t live_nodes;
	int pressure;

	/* collect register pressure info, start with end of a block */
	// ir_fprintf(stderr, "liveness check %+F\n", block);
	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block(env->lv, env->cls, block,
	                         &live_nodes);

	// print_living_values(stderr, &live_nodes);
	pressure = ir_nodeset_size(&live_nodes);
	if (pressure > env->registers_available) {
		ir_fprintf(stderr, "Verify Warning: Register pressure too high at end of block %+F(%s) (%d/%d):\n",
			block, get_irg_name(env->irg), pressure, env->registers_available);
		print_living_values(stderr, &live_nodes);
		env->problem_found = 1;
	}

	sched_foreach_reverse(block, irn) {
		if (is_Phi(irn))
			break;

		// print_living_values(stderr, &live_nodes);
		be_liveness_transfer(env->cls, irn, &live_nodes);

		pressure = ir_nodeset_size(&live_nodes);

		if (pressure > env->registers_available) {
			ir_fprintf(stderr, "Verify Warning: Register pressure too high before node %+F in %+F(%s) (%d/%d):\n",
				irn, block, get_irg_name(env->irg), pressure, env->registers_available);
			print_living_values(stderr, &live_nodes);
			env->problem_found = 1;
			assert(0);
		}
	}
	ir_nodeset_destroy(&live_nodes);
}

/**
 * Start a walk over the irg and check the register pressure.
 */
int be_verify_register_pressure(ir_graph *irg, const arch_register_class_t *cls)
{
	be_verify_register_pressure_env_t env;

	env.lv                  = be_liveness_new(irg);
	env.irg                 = irg;
	env.cls                 = cls;
	env.registers_available = be_get_n_allocatable_regs(irg, cls);
	env.problem_found       = 0;

	be_liveness_compute_sets(env.lv);
	irg_block_walk_graph(irg, verify_liveness_walker, NULL, &env);
	be_liveness_free(env.lv);

	return ! env.problem_found;
}

/*--------------------------------------------------------------------------- */

typedef struct be_verify_schedule_env_t_ {
	int       problem_found; /**< flags indicating a problem */
	bitset_t *scheduled;     /**< bitset of scheduled nodes */
	ir_graph *irg;           /**< the irg to check */
} be_verify_schedule_env_t;

/**
 * Simple schedule checker.
 */
static void verify_schedule_walker(ir_node *block, void *data)
{
	be_verify_schedule_env_t *env = (be_verify_schedule_env_t*) data;
	ir_node *non_phi_found  = NULL;
	ir_node *cfchange_found = NULL;
	int last_timestep = INT_MIN;

	/*
	 * Tests for the following things:
	 *   1. Make sure that all phi nodes are scheduled at the beginning of the
	 *      block
	 *   2. No value is defined after it has been used
	 *   3. mode_T nodes have all projs scheduled behind them followed by Keeps
	 *       (except mode_X projs)
	 */
	sched_foreach(block, node) {
		int timestep;

		/* this node is scheduled */
		if (bitset_is_set(env->scheduled, get_irn_idx(node))) {
			ir_fprintf(stderr, "Verify warning: %+F appears to be schedule twice\n");
			env->problem_found = 1;
		}
		bitset_set(env->scheduled, get_irn_idx(node));

		/* Check that scheduled nodes are in the correct block */
		if (get_nodes_block(node) != block) {
			ir_fprintf(stderr, "Verify warning: %+F is in block %+F but scheduled in %+F\n", node, get_nodes_block(node), block);
			env->problem_found = 1;
		}

		/* Check that timesteps are increasing */
		timestep = sched_get_time_step(node);
		if (timestep <= last_timestep) {
			ir_fprintf(stderr, "Verify warning: Schedule timestep did not increase at node %+F\n",
			           node);
			env->problem_found = 1;
		}
		last_timestep = timestep;

		/* Check that phis come before any other node */
		if (is_Phi(node)) {
			if (non_phi_found != NULL) {
				ir_fprintf(stderr, "Verify Warning: Phi node %+F scheduled after non-Phi nodes (for example %+F) in block %+F (%s)\n",
					node, non_phi_found, block, get_irg_name(env->irg));
				env->problem_found = 1;
			}
		} else {
			non_phi_found = node;
		}

		/* Check for control flow changing nodes */
		if (is_cfop(node)) {
			/* check, that only one CF operation is scheduled */
			if (cfchange_found != NULL) {
				ir_fprintf(stderr, "Verify Warning: Additional control flow changing node %+F scheduled after %+F in block %+F (%s)\n",
					node, block, cfchange_found, get_irg_name(env->irg));
				env->problem_found = 1;
			} else {
				cfchange_found = node;
			}
		} else if (cfchange_found != NULL) {
			/* proj and keepany aren't real instructions... */
			if (!is_Proj(node) && !be_is_Keep(node)) {
				ir_fprintf(stderr, "Verify Warning: Node %+F scheduled after control flow changing node in block %+F (%s)\n",
				           node, block, get_irg_name(env->irg));
				env->problem_found = 1;
			}
		}

		/* Check that all uses come before their definitions */
		if (!is_Phi(node)) {
			int i;
			int arity;
			sched_timestep_t nodetime = sched_get_time_step(node);
			for (i = 0, arity = get_irn_arity(node); i < arity; ++i) {
				ir_node *arg = get_irn_n(node, i);
				if (get_nodes_block(arg) != block
				   || !sched_is_scheduled(arg))
					continue;

				if (sched_get_time_step(arg) >= nodetime) {
					ir_fprintf(stderr, "Verify Warning: Value %+F used by %+F before it was defined in block %+F (%s)\n",
					           arg, node, block, get_irg_name(env->irg));
					env->problem_found = 1;
				}
			}
		}

		/* Check that no dead nodes are scheduled */
		if (get_irn_n_edges(node) == 0) {
			ir_fprintf(stderr, "Verify warning: Node %+F is dead but scheduled in block %+F (%s)\n",
			           node, block, get_irg_name(env->irg));
			env->problem_found = 1;
		}

		if (be_is_Keep(node) || be_is_CopyKeep(node)) {
			/* at least 1 of the keep arguments has to be its schedule
			 * predecessor */
			int      arity   = get_irn_arity(node);
			bool     found   = false;
			ir_node *prev    = sched_prev(node);
			while (be_is_Keep(prev) || be_is_CopyKeep(prev))
				prev = sched_prev(prev);

			while (true) {
				int i;
				for (i = 0; i < arity; ++i) {
					ir_node *in = get_irn_n(node, i);
					in = skip_Proj(in);
					if (in == prev)
						found = true;
				}
				if (found)
					break;
				prev = sched_prev(prev);
				if (!is_Phi(prev))
					break;
			}
			if (!found) {
				ir_fprintf(stderr, "%+F not scheduled after its pred node in block %+F (%s)\n",
				           node, block, get_irg_name(env->irg));
				env->problem_found = 1;
			}
		}
	}
}

static void check_schedule(ir_node *node, void *data)
{
	be_verify_schedule_env_t *env = (be_verify_schedule_env_t*)data;
	bool should_be = !is_Proj(node) && !(arch_get_irn_flags(node) & arch_irn_flags_not_scheduled);
	bool scheduled = bitset_is_set(env->scheduled, get_irn_idx(node));

	if (should_be != scheduled) {
		ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) should%s be scheduled\n",
			node, get_nodes_block(node), get_irg_name(env->irg), should_be ? "" : " not");
		env->problem_found = 1;
	}
}

/**
 * Start a walk over the irg and check schedule.
 */
int be_verify_schedule(ir_graph *irg)
{
	be_verify_schedule_env_t env;

	env.problem_found = 0;
	env.irg           = irg;
	env.scheduled     = bitset_alloca(get_irg_last_idx(env.irg));

	irg_block_walk_graph(irg, verify_schedule_walker, NULL, &env);
	/* check if all nodes are scheduled */
	irg_walk_graph(irg, check_schedule, NULL, &env);

	return ! env.problem_found;
}



/*--------------------------------------------------------------------------- */



typedef struct spill_t {
	ir_node *spill;
	ir_entity *ent;
} spill_t;

typedef struct {
	ir_graph  *irg;
	set       *spills;
	ir_node  **reloads;
	int        problem_found;
} be_verify_spillslots_env_t;

static int cmp_spill(const void* d1, const void* d2, size_t size)
{
	const spill_t* s1 = (const spill_t*)d1;
	const spill_t* s2 = (const spill_t*)d2;
	(void) size;

	return s1->spill != s2->spill;
}

static spill_t *find_spill(be_verify_spillslots_env_t *env, ir_node *node)
{
	spill_t spill;

	spill.spill = node;
	return set_find(spill_t, env->spills, &spill, sizeof(spill), hash_ptr(node));
}

static spill_t *get_spill(be_verify_spillslots_env_t *env, ir_node *node, ir_entity *ent)
{
	spill_t spill, *res;
	int hash = hash_ptr(node);

	spill.spill = node;
	res = set_find(spill_t, env->spills, &spill, sizeof(spill), hash);

	if (res == NULL) {
		spill.ent = ent;
		res = set_insert(spill_t, env->spills, &spill, sizeof(spill), hash);
	}

	return res;
}

static ir_node *get_memory_edge(const ir_node *node)
{
	int i, arity;
	ir_node *result = NULL;

	arity = get_irn_arity(node);
	for (i = arity - 1; i >= 0; --i) {
		ir_node *arg = get_irn_n(node, i);
		if (get_irn_mode(arg) == mode_M) {
			assert(result == NULL);
			result = arg;
		}
	}

	return result;
}

static void collect(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity* ent);

static void be_check_entity(be_verify_spillslots_env_t *env, ir_node *node, ir_entity *ent)
{
	if (ent == NULL) {
		ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) should have an entity assigned\n",
		           node, get_nodes_block(node), get_irg_name(env->irg));
	}
}

static void collect_spill(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity* ent)
{
	ir_entity *spillent = arch_get_frame_entity(node);
	be_check_entity(env, node, spillent);
	get_spill(env, node, ent);

	if (spillent != ent) {
		ir_fprintf(stderr, "Verify warning: Spill %+F has different entity than reload %+F in block %+F(%s)\n",
			node, reload, get_nodes_block(node), get_irg_name(env->irg));
		env->problem_found = 1;
	}
}

static void collect_memperm(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity* ent)
{
	int i, arity;
	spill_t spill, *res;
	int hash = hash_ptr(node);
	int out;
	ir_node* memperm;
	ir_entity *spillent;

	assert(is_Proj(node));

	memperm = get_Proj_pred(node);
	out = get_Proj_proj(node);

	spillent = be_get_MemPerm_out_entity(memperm, out);
	be_check_entity(env, memperm, spillent);
	if (spillent != ent) {
		ir_fprintf(stderr, "Verify warning: MemPerm %+F has different entity than reload %+F in block %+F(%s)\n",
			node, reload, get_nodes_block(node), get_irg_name(env->irg));
		env->problem_found = 1;
	}

	spill.spill = node;
	res = set_find(spill_t, env->spills, &spill, sizeof(spill), hash);
	if (res != NULL) {
		return;
	}

	spill.ent = spillent;
	res = set_insert(spill_t, env->spills, &spill, sizeof(spill), hash);

	for (i = 0, arity = be_get_MemPerm_entity_arity(memperm); i < arity; ++i) {
		ir_node* arg = get_irn_n(memperm, i + 1);
		ir_entity* argent = be_get_MemPerm_in_entity(memperm, i);

		collect(env, arg, memperm, argent);
	}
}

static void collect_memphi(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity *ent)
{
	int i, arity;
	spill_t spill, *res;
	int hash = hash_ptr(node);

	assert(is_Phi(node));

	spill.spill = node;
	res = set_find(spill_t, env->spills, &spill, sizeof(spill), hash);
	if (res != NULL) {
		return;
	}

	spill.ent = ent;
	res = set_insert(spill_t, env->spills, &spill, sizeof(spill), hash);

	/* is 1 of the arguments a spill? */
	for (i = 0, arity = get_irn_arity(node); i < arity; ++i) {
		ir_node* arg = get_irn_n(node, i);
		collect(env, arg, reload, ent);
	}
}

static void collect(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity* ent)
{
	if (be_is_Spill(node)) {
		collect_spill(env, node, reload, ent);
	} else if (is_Proj(node)) {
		collect_memperm(env, node, reload, ent);
	} else if (is_Phi(node) && get_irn_mode(node) == mode_M) {
		collect_memphi(env, node, reload, ent);
	} else {
		/* Disabled for now, spills might get transformed by the backend */
#if 0
		ir_fprintf(stderr, "Verify warning: No spill, memperm or memphi attached to node %+F found from node %+F in block %+F(%s)\n",
			node, reload, get_nodes_block(node), get_irg_name(env->irg));
		env->problem_found = 1;
#endif
	}
}

/**
 * This walker function searches for reloads and collects all the spills
 * and memphis attached to them.
 */
static void collect_spills_walker(ir_node *node, void *data)
{
	be_verify_spillslots_env_t *env = (be_verify_spillslots_env_t*)data;

	if (be_is_Reload(node)) {
		ir_node *spill = get_memory_edge(node);
		ir_entity *ent;

		if (spill == NULL) {
			ir_fprintf(stderr, "Verify warning: No spill attached to reload %+F in block %+F(%s)\n",
			           node, get_nodes_block(node), get_irg_name(env->irg));
			env->problem_found = 1;
			return;
		}
		ent = arch_get_frame_entity(node);
		be_check_entity(env, node, ent);

		collect(env, spill, node, ent);
		ARR_APP1(ir_node*, env->reloads, node);
	}
}

static void check_spillslot_interference(be_verify_spillslots_env_t *env)
{
	int       spillcount = set_count(env->spills);
	spill_t **spills     = ALLOCAN(spill_t*, spillcount);
	int       i;

	i = 0;
	foreach_set(env->spills, spill_t, spill) {
		spills[i++] = spill;
	}

	for (i = 0; i < spillcount; ++i) {
		spill_t *sp1 = spills[i];
		int i2;

		for (i2 = i+1; i2 < spillcount; ++i2) {
			spill_t *sp2 = spills[i2];

			if (sp1->ent != sp2->ent)
				continue;

			if (my_values_interfere(sp1->spill, sp2->spill)) {
				ir_fprintf(stderr, "Verify warning: Spillslots for %+F in block %+F(%s) and %+F in block %+F(%s) interfere\n",
					sp1->spill, get_nodes_block(sp1->spill), get_irg_name(env->irg),
					sp2->spill, get_nodes_block(sp2->spill), get_irg_name(env->irg));
				env->problem_found = 1;
				my_values_interfere(sp1->spill, sp2->spill);
			}
		}
	}
}

static void check_lonely_spills(ir_node *node, void *data)
{
	be_verify_spillslots_env_t *env = (be_verify_spillslots_env_t*)data;

	if (be_is_Spill(node) || (is_Proj(node) && be_is_MemPerm(get_Proj_pred(node)))) {
		spill_t *spill = find_spill(env, node);
		if (be_is_Spill(node)) {
			ir_entity *ent = arch_get_frame_entity(node);
			be_check_entity(env, node, ent);
		}

		if (spill == NULL) {
			ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) not connected to a reload\n",
			           node, get_nodes_block(node), get_irg_name(env->irg));
		}
	}
}

int be_verify_spillslots(ir_graph *irg)
{
	be_verify_spillslots_env_t env;

	env.irg = irg;
	env.spills = new_set(cmp_spill, 10);
	env.reloads = NEW_ARR_F(ir_node*, 0);
	env.problem_found = 0;

	irg_walk_graph(irg, collect_spills_walker, NULL, &env);
	irg_walk_graph(irg, check_lonely_spills, NULL, &env);

	check_spillslot_interference(&env);

	DEL_ARR_F(env.reloads);
	del_set(env.spills);

	return ! env.problem_found;
}



/*--------------------------------------------------------------------------- */



/**
 * Check, if two values interfere.
 * @param a The first value.
 * @param b The second value.
 * @return 1, if a and b interfere, 0 if not.
 */
static int my_values_interfere(const ir_node *a, const ir_node *b)
{
	ir_node *bb;
	int a2b = value_dominates(a, b);
	int b2a = value_dominates(b, a);

	/* If there is no dominance relation, they do not interfere. */
	if (!a2b && !b2a)
		return 0;

	/*
	 * Adjust a and b so, that a dominates b if
	 * a dominates b or vice versa.
	 */
	if (b2a) {
		const ir_node *t = a;
		a = b;
		b = t;
	}

	bb = get_nodes_block(b);

	/*
	 * Look at all usages of a.
	 * If there's one usage of a in the block of b, then
	 * we check, if this use is dominated by b, if that's true
	 * a and b interfere. Note that b must strictly dominate the user,
	 * since if b is the last user of in the block, b and a do not
	 * interfere.
	 * Uses of a not in b's block can be disobeyed, because the
	 * check for a being live at the end of b's block is already
	 * performed.
	 */
	foreach_out_edge(a, edge) {
		const ir_node *user = get_edge_src_irn(edge);
		if (b == user)
			continue;

		if (get_irn_opcode(user) == iro_End)
			continue;

		/* in case of phi arguments we compare with the block the value comes from */
		if (is_Phi(user)) {
			ir_node *phiblock = get_nodes_block(user);
			if (phiblock == bb)
				continue;
			user = get_irn_n(phiblock, get_edge_src_pos(edge));
		}

		if (value_dominates(b, user))
			return 1;
	}

	return 0;
}



/*--------------------------------------------------------------------------- */

static const arch_env_t            *arch_env;
static ir_graph                    *irg;
static be_lv_t                     *lv;
static bool                         problem_found;
static const ir_node              **registers;

static void check_output_constraints(const ir_node *node)
{
	if (arch_get_irn_reg_class(node) == NULL)
		return;

	/* verify output register */
	const arch_register_req_t *req = arch_get_irn_register_req(node);
	const arch_register_t     *reg = arch_get_irn_register(node);
	if (reg == NULL) {
		ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) should have a register assigned\n",
				node, get_nodes_block(node), get_irg_name(irg));
		problem_found = true;
	} else if (!arch_reg_is_allocatable(req, reg)) {
		ir_fprintf(stderr, "Verify warning: Register %s assigned as output of %+F not allowed (register constraint) in block %+F(%s)\n",
				reg->name, node, get_nodes_block(node), get_irg_name(irg));
		problem_found = true;
	}
}

static void check_input_constraints(ir_node *node)
{
	const arch_register_t *reg;
	int                    i, arity;

	/* verify input register */
	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		const arch_register_req_t *req      = arch_get_irn_register_req_in(node, i);
		ir_node                   *pred     = get_irn_n(node, i);
		const arch_register_req_t *pred_req = arch_get_irn_register_req(pred);

		if (is_Bad(pred)) {
			ir_fprintf(stderr, "Verify warning: %+F in block %+F(%s) has Bad as input %d\n",
				node, get_nodes_block(node), get_irg_name(irg), i);
			problem_found = 1;
			continue;
		}
		if (req->cls == NULL)
			continue;

		if (req->width > pred_req->width) {
			ir_fprintf(stderr, "Verify warning: %+F in block %+F(%s) register width of value at input %d too small\n",
			           node, get_nodes_block(node), get_irg_name(irg), i);
			problem_found = 1;
		}

		reg = arch_get_irn_register(pred);
		if (req->type & arch_register_req_type_aligned) {
			if (reg->index % req->width != 0) {
				ir_fprintf(stderr, "Verify warning: %+F in block %+F(%s) register allignment not fulfilled\n",
				           node, get_nodes_block(node), get_irg_name(irg), i);
				problem_found = 1;
			}
		}

		if (reg == NULL) {
			ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) should have a register assigned (%+F input constraint)\n",
			           pred, get_nodes_block(pred), get_irg_name(irg), node);
			problem_found = 1;
			continue;
		} else if (!arch_reg_is_allocatable(req, reg)) {
			ir_fprintf(stderr, "Verify warning: Register %s as input %d of %+F not allowed (register constraint) in block %+F(%s)\n",
			           reg->name, i, node, get_nodes_block(node), get_irg_name(irg));
			problem_found = 1;
		}
	}

	/* phis should be NOPs at this point, which means all input regs
	 * must be the same as the output reg */
	if (is_Phi(node)) {
		reg = arch_get_irn_register(node);

		arity = get_irn_arity(node);
		for (i = 0; i < arity; ++i) {
			ir_node               *pred     = get_Phi_pred(node, i);
			const arch_register_t *pred_reg = arch_get_irn_register(pred);

			if (reg != pred_reg && !(pred_reg->type & arch_register_type_joker)) {
				const char *pred_name = pred_reg != NULL ? pred_reg->name : "(null)";
				const char *reg_name  = reg != NULL ? reg->name : "(null)";
				ir_fprintf(stderr, "Verify warning: Input %d of %+F in block %+F(%s) uses register %s instead of %s\n",
				           i, node, get_nodes_block(node),
				           get_irg_name(irg), pred_name, reg_name);
				problem_found = 1;
			}
		}
	}
}

static void value_used(const ir_node *block, const ir_node *node)
{
	const arch_register_t     *reg = arch_get_irn_register(node);
	const arch_register_req_t *req;
	unsigned                   i;
	unsigned                   idx;

	if (reg == NULL || reg->type & arch_register_type_virtual)
		return;

	req = arch_get_irn_register_req(node);
	assert(req->width > 0);
	idx = reg->global_index;
	for (i = 0; i < req->width; ++i) {
		const ir_node *reg_node = registers[idx+i];
		if (reg_node != NULL && reg_node != node) {
			const arch_register_t *realreg = &arch_env->registers[idx+i];
			ir_fprintf(stderr, "Verify warning: Register %s assigned more than once in block %+F(%s) (nodes %+F %+F)\n",
					   realreg->name, block, get_irg_name(irg),
					   node, reg_node);
			problem_found = true;
		}
		registers[idx+i] = node;
	}
}

static void value_def(const ir_node *node)
{
	const arch_register_t     *reg = arch_get_irn_register(node);
	const arch_register_req_t *req;
	unsigned                   idx;
	unsigned                   i;

	if (reg == NULL || reg->type & arch_register_type_virtual)
		return;

	req = arch_get_irn_register_req(node);
	assert(req->width > 0);
	idx = reg->global_index;
	for (i = 0; i < req->width; ++i) {
		const ir_node *reg_node = registers[idx+i];

		/* a little cheat, since its so hard to remove all outedges to dead code
		 * in the backend. This particular case should never be a problem. */
		if (reg_node == NULL && get_irn_n_edges(node) == 0)
			return;

		if (reg_node != node) {
			const arch_register_t *realreg = &arch_env->registers[idx+i];
			ir_fprintf(stderr, "Verify warning: Node %+F not registered as value for Register %s (but %+F) in block %+F(%s)\n",
			           node, realreg->name, reg_node, get_nodes_block(node),
			           get_irg_name(irg));
			problem_found = true;
		}
		registers[idx+i] = NULL;
	}
}

static void verify_block_register_allocation(ir_node *block, void *data)
{
	unsigned i;
	unsigned n_regs;

	(void) data;

	assert(lv->sets_valid && "live sets must be computed");

	n_regs    = arch_env->n_registers;
	registers = ALLOCANZ(const ir_node*, n_regs);

	be_lv_foreach(lv, block, be_lv_state_end, lv_node) {
		value_used(block, lv_node);
	}

	sched_foreach_reverse(block, node) {
		int arity;

		if (get_irn_mode(node) == mode_T) {
			foreach_out_edge(node, edge) {
				ir_node *def = get_edge_src_irn(edge);
				value_def(def);
				check_output_constraints(def);
			}
		} else {
			value_def(node);
			check_output_constraints(node);
		}

		check_input_constraints(node);

		/* process uses. (Phi inputs are no real uses) */
		if (!is_Phi(node)) {
			int in;
			arity = get_irn_arity(node);
			for (in = 0; in < arity; ++in) {
				ir_node *use = get_irn_n(node, in);
				value_used(block, use);
			}
		}
	}

	be_lv_foreach(lv, block, be_lv_state_in, lv_node) {
		value_def(lv_node);
	}

	/* set must be empty now */
	for (i = 0; i < n_regs; ++i) {
		if (registers[i] == NULL)
			continue;

		ir_fprintf(stderr, "Verify warning: Node %+F not live-in and no def found in block %+F(%s)\n",
				registers[i], block, get_irg_name(irg));
		problem_found = true;
	}
}

bool be_verify_register_allocation(ir_graph *new_irg)
{
	irg           = new_irg;
	arch_env      = be_get_irg_arch_env(irg);
	lv            = be_liveness_new(irg);
	problem_found = false;

	be_liveness_compute_sets(lv);
	irg_block_walk_graph(irg, verify_block_register_allocation, NULL, NULL);
	be_liveness_free(lv);

	return !problem_found;
}

/*--------------------------------------------------------------------------- */

/**
 * Walker: checks that every predecessors of a node dominates the node.
 */
static void dom_check(ir_node *irn, void *data)
{
	bool *problem_found = (bool*)data;

	if (!is_Block(irn) && irn != get_irg_end(get_irn_irg(irn))) {
		int i, n;
		ir_node *bl = get_nodes_block(irn);

		for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *op     = get_irn_n(irn, i);
			ir_node *def_bl = get_nodes_block(op);
			ir_node *use_bl = bl;

			if (is_Phi(irn))
				use_bl = get_Block_cfgpred_block(bl, i);

			if (get_irn_opcode(use_bl) != iro_Bad
			     && get_irn_opcode(def_bl) != iro_Bad
			     && !block_dominates(def_bl, use_bl)) {
				ir_fprintf(stderr, "Verify warning: %+F in %+F must dominate %+F for user %+F (%s)\n", op, def_bl, use_bl, irn, get_irg_name(get_irn_irg(op)));
				*problem_found = true;
			}
		}
	}
}

/* Check, if the SSA dominance property is fulfilled. */
bool be_check_dominance(ir_graph *irg)
{
	bool problem_found = false;

	assure_doms(irg);
	irg_walk_graph(irg, dom_check, NULL, &problem_found);

	return !problem_found;
}

/*--------------------------------------------------------------------------- */

typedef struct lv_walker_t {
	be_lv_t *lv;
	void *data;
} lv_walker_t;

static const char *lv_flags_to_str(unsigned flags)
{
	static const char *states[] = {
		"---",
		"i--",
		"-e-",
		"ie-",
		"--o",
		"i-o",
		"-eo",
		"ieo"
	};

	return states[flags & 7];
}

static void lv_check_walker(ir_node *bl, void *data)
{
	lv_walker_t *w = (lv_walker_t*)data;
	be_lv_t *lv    = w->lv;
	be_lv_t *fresh = (be_lv_t*)w->data;

	be_lv_info_t *curr = ir_nodehashmap_get(be_lv_info_t, &fresh->map, bl);
	be_lv_info_t *fr   = ir_nodehashmap_get(be_lv_info_t, &fresh->map, bl);

	if (!fr && curr && curr[0].head.n_members > 0) {
		unsigned i;

		ir_fprintf(stderr, "%+F liveness should be empty but current liveness contains:\n", bl);
		for (i = 0; i < curr[0].head.n_members; ++i) {
			ir_fprintf(stderr, "\t%+F\n", get_idx_irn(lv->irg, curr[1 + i].node.idx));
		}
	}

	else if (curr) {
		unsigned n_curr  = curr[0].head.n_members;
		unsigned n_fresh = fr[0].head.n_members;

		unsigned i;

		if (n_curr != n_fresh) {
			ir_fprintf(stderr, "%+F: liveness set sizes differ. curr %d, correct %d\n", bl, n_curr, n_fresh);

			ir_fprintf(stderr, "current:\n");
			for (i = 0; i < n_curr; ++i) {
				be_lv_info_node_t *n = &curr[1 + i].node;
				ir_fprintf(stderr, "%+F %u %+F %s\n", bl, i, get_idx_irn(lv->irg, n->idx), lv_flags_to_str(n->flags));
			}

			ir_fprintf(stderr, "correct:\n");
			for (i = 0; i < n_fresh; ++i) {
				be_lv_info_node_t *n = &fr[1 + i].node;
				ir_fprintf(stderr, "%+F %u %+F %s\n", bl, i, get_idx_irn(lv->irg, n->idx), lv_flags_to_str(n->flags));
			}
		}
	}
}

void be_liveness_check(be_lv_t *lv)
{
	lv_walker_t w;
	be_lv_t *fresh = be_liveness_new(lv->irg);

	w.lv   = lv;
	w.data = fresh;
	irg_block_walk_graph(lv->irg, lv_check_walker, NULL, &w);
	be_liveness_free(fresh);
}
