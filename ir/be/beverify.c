/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Various verify routines that check a scheduled graph for correctness.
 * @author      Matthias Braun
 * @date        05.05.2006
 */
#include <limits.h>
#include <stdbool.h>

#include "bitset.h"
#include "set.h"
#include "array.h"

#include "irnode.h"
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

static bool my_values_interfere(const ir_node *a, const ir_node *b);

typedef struct be_verify_register_pressure_env_t_ {
	ir_graph                    *irg;                 /**< the irg to verify */
	be_lv_t                     *lv;                  /**< Liveness information. */
	const arch_register_class_t *cls;                 /**< the register class to check for */
	unsigned                    registers_available;  /**< number of available registers */
	bool                        problem_found;        /**< flag indicating if a problem was found */
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

	/* collect register pressure info, start with end of a block */
	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block(env->lv, env->cls, block,
	                         &live_nodes);

	unsigned pressure = ir_nodeset_size(&live_nodes);
	if (pressure > env->registers_available) {
		ir_fprintf(stderr, "Verify Warning: Register pressure too high at end of block %+F(%s) (%d/%d):\n",
			block, get_irg_name(env->irg), pressure, env->registers_available);
		print_living_values(stderr, &live_nodes);
		env->problem_found = true;
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
			env->problem_found = true;
		}
	}
	ir_nodeset_destroy(&live_nodes);
}

bool be_verify_register_pressure(ir_graph *irg, const arch_register_class_t *cls)
{
	be_verify_register_pressure_env_t env;

	env.lv                  = be_liveness_new(irg);
	env.irg                 = irg;
	env.cls                 = cls;
	env.registers_available = be_get_n_allocatable_regs(irg, cls);
	env.problem_found       = false;

	be_liveness_compute_sets(env.lv);
	irg_block_walk_graph(irg, verify_liveness_walker, NULL, &env);
	be_liveness_free(env.lv);

	return ! env.problem_found;
}

/*--------------------------------------------------------------------------- */

typedef struct be_verify_schedule_env_t_ {
	bool      problem_found; /**< flags indicating a problem */
	bitset_t *scheduled;     /**< bitset of scheduled nodes */
	ir_graph *irg;           /**< the irg to check */
} be_verify_schedule_env_t;

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
		/* this node is scheduled */
		if (bitset_is_set(env->scheduled, get_irn_idx(node))) {
			ir_fprintf(stderr, "Verify warning: %+F appears to be schedule twice\n");
			env->problem_found = true;
		}
		bitset_set(env->scheduled, get_irn_idx(node));

		/* Check that scheduled nodes are in the correct block */
		if (get_nodes_block(node) != block) {
			ir_fprintf(stderr, "Verify warning: %+F is in block %+F but scheduled in %+F\n", node, get_nodes_block(node), block);
			env->problem_found = true;
		}

		/* Check that timesteps are increasing */
		int timestep = sched_get_time_step(node);
		if (timestep <= last_timestep) {
			ir_fprintf(stderr, "Verify warning: Schedule timestep did not increase at node %+F\n",
			           node);
			env->problem_found = true;
		}
		last_timestep = timestep;

		if (arch_get_irn_flags(node) & arch_irn_flag_not_scheduled) {
			ir_fprintf(stderr, "Verify warning: flag_not_scheduled node %+F scheduled anyway\n", node);
			env->problem_found = true;
		}

		/* Check that phis come before any other node */
		if (is_Phi(node)) {
			if (non_phi_found != NULL) {
				ir_fprintf(stderr, "Verify Warning: Phi node %+F scheduled after non-Phi nodes (for example %+F) in block %+F (%s)\n",
					node, non_phi_found, block, get_irg_name(env->irg));
				env->problem_found = true;
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
				env->problem_found = true;
			} else {
				cfchange_found = node;
			}
		} else if (cfchange_found != NULL) {
			/* keepany isn't a real instruction. */
			if (!be_is_Keep(node)) {
				ir_fprintf(stderr, "Verify Warning: Node %+F scheduled after control flow changing node in block %+F (%s)\n",
				           node, block, get_irg_name(env->irg));
				env->problem_found = true;
			}
		}

		/* Check that all uses come before their definitions */
		if (!is_Phi(node)) {
			sched_timestep_t nodetime = sched_get_time_step(node);
			foreach_irn_in(node, i, arg) {
				if (get_nodes_block(arg) != block || !sched_is_scheduled(arg))
					continue;

				if (sched_get_time_step(arg) >= nodetime) {
					ir_fprintf(stderr, "Verify Warning: Value %+F used by %+F before it was defined in block %+F (%s)\n",
					           arg, node, block, get_irg_name(env->irg));
					env->problem_found = true;
				}
			}
		}

		/* Check that no dead nodes are scheduled */
		if (get_irn_n_edges(node) == 0) {
			ir_fprintf(stderr, "Verify warning: Node %+F is dead but scheduled in block %+F (%s)\n",
			           node, block, get_irg_name(env->irg));
			env->problem_found = true;
		}

		if (be_is_Keep(node) || be_is_CopyKeep(node)) {
			/* at least 1 of the keep arguments has to be its schedule
			 * predecessor */
			ir_node *prev    = sched_prev(node);
			while (be_is_Keep(prev) || be_is_CopyKeep(prev))
				prev = sched_prev(prev);

			bool found = false;
			while (true) {
				foreach_irn_in(node, i, in) {
					if (skip_Proj(in) == prev)
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
				env->problem_found = true;
			}
		}
	}
}

static void check_schedule(ir_node *node, void *data)
{
	be_verify_schedule_env_t *env = (be_verify_schedule_env_t*)data;
	bool should_be = !is_Proj(node) && !(arch_get_irn_flags(node) & arch_irn_flag_not_scheduled);
	bool scheduled = bitset_is_set(env->scheduled, get_irn_idx(node));

	if (should_be != scheduled) {
		ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) should%s be scheduled\n",
			node, get_nodes_block(node), get_irg_name(env->irg), should_be ? "" : " not");
		env->problem_found = true;
	}
}

bool be_verify_schedule(ir_graph *irg)
{
	be_verify_schedule_env_t env;

	env.problem_found = false;
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
	bool       problem_found;
} be_verify_spillslots_env_t;

static int cmp_spill(const void* d1, const void* d2, size_t size)
{
	(void) size;
	const spill_t* s1 = (const spill_t*)d1;
	const spill_t* s2 = (const spill_t*)d2;
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
	int hash = hash_ptr(node);
	spill_t spill;
	spill.spill = node;
	spill_t *res = set_find(spill_t, env->spills, &spill, sizeof(spill), hash);

	if (res == NULL) {
		spill.ent = ent;
		res = set_insert(spill_t, env->spills, &spill, sizeof(spill), hash);
	}

	return res;
}

static ir_node *get_memory_edge(const ir_node *node)
{
	ir_node *result = NULL;
	foreach_irn_in_r(node, i, arg) {
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
		env->problem_found = true;
	}
}

static void collect_memperm(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity* ent)
{
	ir_node *memperm = get_Proj_pred(node);
	int      out     = get_Proj_proj(node);

	ir_entity *spillent = be_get_MemPerm_out_entity(memperm, out);
	be_check_entity(env, memperm, spillent);
	if (spillent != ent) {
		ir_fprintf(stderr, "Verify warning: MemPerm %+F has different entity than reload %+F in block %+F(%s)\n",
			node, reload, get_nodes_block(node), get_irg_name(env->irg));
		env->problem_found = true;
	}

	int hash = hash_ptr(node);
	spill_t spill;
	spill.spill = node;
	spill_t *res = set_find(spill_t, env->spills, &spill, sizeof(spill), hash);
	if (res != NULL) {
		return;
	}

	spill.ent = spillent;
	(void)set_insert(spill_t, env->spills, &spill, sizeof(spill), hash);

	int arity = be_get_MemPerm_entity_arity(memperm);
	for (int i = 0; i < arity; ++i) {
		ir_node* arg = get_irn_n(memperm, i + 1);
		ir_entity* argent = be_get_MemPerm_in_entity(memperm, i);

		collect(env, arg, memperm, argent);
	}
}

static void collect_memphi(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity *ent)
{
	assert(is_Phi(node));

	int hash = hash_ptr(node);
	spill_t spill;
	spill.spill = node;
	spill_t *res = set_find(spill_t, env->spills, &spill, sizeof(spill), hash);
	if (res != NULL) {
		return;
	}

	spill.ent = ent;
	(void)set_insert(spill_t, env->spills, &spill, sizeof(spill), hash);

	/* is 1 of the arguments a spill? */
	foreach_irn_in(node, i, arg) {
		collect(env, arg, reload, ent);
	}
}

static void collect(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity* ent)
{
	if (arch_irn_is(node, spill)) {
		collect_spill(env, node, reload, ent);
	} else if (is_Proj(node)) {
		collect_memperm(env, node, reload, ent);
	} else if (is_Phi(node) && get_irn_mode(node) == mode_M) {
		collect_memphi(env, node, reload, ent);
	}
}

/**
 * This walker function searches for reloads and collects all the spills
 * and memphis attached to them.
 */
static void collect_spills_walker(ir_node *node, void *data)
{
	be_verify_spillslots_env_t *env = (be_verify_spillslots_env_t*)data;

	if (arch_irn_is(node, reload)) {
		ir_node *spill = get_memory_edge(node);
		if (spill == NULL) {
			ir_fprintf(stderr, "Verify warning: No spill attached to reload %+F in block %+F(%s)\n",
			           node, get_nodes_block(node), get_irg_name(env->irg));
			env->problem_found = true;
			return;
		}
		ir_entity *ent = arch_get_frame_entity(node);
		be_check_entity(env, node, ent);

		collect(env, spill, node, ent);
		ARR_APP1(ir_node*, env->reloads, node);
	}
}

static void check_spillslot_interference(be_verify_spillslots_env_t *env)
{
	int       spillcount = set_count(env->spills);
	spill_t **spills     = ALLOCAN(spill_t*, spillcount);

	int s = 0;
	foreach_set(env->spills, spill_t, spill) {
		spills[s++] = spill;
	}
	assert(s == spillcount);

	for (int i = 0; i < spillcount; ++i) {
		spill_t *sp1 = spills[i];

		for (int i2 = i+1; i2 < spillcount; ++i2) {
			spill_t *sp2 = spills[i2];

			if (sp1->ent != sp2->ent)
				continue;

			if (my_values_interfere(sp1->spill, sp2->spill)) {
				ir_fprintf(stderr, "Verify warning: Spillslots for %+F in block %+F(%s) and %+F in block %+F(%s) interfere\n",
					sp1->spill, get_nodes_block(sp1->spill), get_irg_name(env->irg),
					sp2->spill, get_nodes_block(sp2->spill), get_irg_name(env->irg));
				env->problem_found = true;
				my_values_interfere(sp1->spill, sp2->spill);
			}
		}
	}
}

static void check_lonely_spills(ir_node *node, void *data)
{
	be_verify_spillslots_env_t *env = (be_verify_spillslots_env_t*)data;

	if (arch_irn_is(node, spill)
	    || (is_Proj(node) && be_is_MemPerm(get_Proj_pred(node)))) {
		spill_t *spill = find_spill(env, node);
		if (arch_irn_is(node, spill)) {
			ir_entity *ent = arch_get_frame_entity(node);
			be_check_entity(env, node, ent);
		}

		if (spill == NULL) {
			ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) not connected to a reload\n",
			           node, get_nodes_block(node), get_irg_name(env->irg));
		}
	}
}

bool be_verify_spillslots(ir_graph *irg)
{
	be_verify_spillslots_env_t env;

	env.irg           = irg;
	env.spills        = new_set(cmp_spill, 10);
	env.reloads       = NEW_ARR_F(ir_node*, 0);
	env.problem_found = false;

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
static bool my_values_interfere(const ir_node *a, const ir_node *b)
{
	int a2b = value_dominates(a, b);
	int b2a = value_dominates(b, a);

	/* If there is no dominance relation, they do not interfere. */
	if (!a2b && !b2a)
		return false;

	/*
	 * Adjust a and b so, that a dominates b if
	 * a dominates b or vice versa.
	 */
	if (b2a) {
		const ir_node *t = a;
		a = b;
		b = t;
	}

	ir_node *bb = get_nodes_block(b);

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

		if (is_End(user))
			continue;

		/* in case of phi arguments we compare with the block the value comes from */
		if (is_Phi(user)) {
			ir_node *phiblock = get_nodes_block(user);
			if (phiblock == bb)
				continue;
			user = get_irn_n(phiblock, get_edge_src_pos(edge));
		}

		if (value_dominates(b, user))
			return true;
	}

	return false;
}

/*--------------------------------------------------------------------------- */

static const arch_env_t  *arch_env;
static ir_graph          *irg;
static be_lv_t           *lv;
static bool               ignore_sp_problems;
static bool               problem_found;
static const ir_node    **registers;

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
	/* verify input register */
	foreach_irn_in(node, i, pred) {
		if (is_Bad(pred)) {
			ir_fprintf(stderr, "Verify warning: %+F in block %+F(%s) has Bad as input %d\n",
				node, get_nodes_block(node), get_irg_name(irg), i);
			problem_found = 1;
			continue;
		}

		const arch_register_req_t *req = arch_get_irn_register_req_in(node, i);
		if (req->cls == NULL)
			continue;

		const arch_register_req_t *pred_req = arch_get_irn_register_req(pred);
		if (req->width > pred_req->width) {
			ir_fprintf(stderr, "Verify warning: %+F in block %+F(%s) register width of value at input %d too small\n",
			           node, get_nodes_block(node), get_irg_name(irg), i);
			problem_found = 1;
		}

		const arch_register_t *reg = arch_get_irn_register(pred);
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
		const arch_register_t *reg = arch_get_irn_register(node);
		foreach_irn_in(node, i, pred) {
			const arch_register_t *pred_reg = arch_get_irn_register(pred);

			if (reg != pred_reg && !(pred_reg->type & arch_register_type_virtual)) {
				const char *pred_name = pred_reg != NULL ? pred_reg->name : "(null)";
				const char *reg_name  = reg != NULL ? reg->name : "(null)";
				ir_fprintf(stderr, "Verify warning: Input %d of %+F in block %+F(%s) uses register %s instead of %s\n",
				           i, node, get_nodes_block(node),
				           get_irg_name(irg), pred_name, reg_name);
				problem_found = true;
			}
		}
	}
}

static void value_used(const ir_node *block, const ir_node *node)
{
	const arch_register_t *reg = arch_get_irn_register(node);
	if (reg == NULL || reg->type & arch_register_type_virtual)
		return;

	const arch_register_req_t *req = arch_get_irn_register_req(node);
	assert(req->width > 0);
	unsigned idx = reg->global_index;
	for (unsigned i = 0; i < req->width; ++i) {
		const ir_node *reg_node = registers[idx+i];
		if (reg_node != NULL && reg_node != node
			&& (!ignore_sp_problems
			    || !(req->type & arch_register_req_type_produces_sp))) {
			ir_fprintf(stderr, "Verify warning: Register %s assigned more than once in block %+F(%s) (nodes %+F %+F)\n",
					   reg->name, block, get_irg_name(irg),
					   node, reg_node);
			problem_found = true;
		}
		registers[idx+i] = node;
	}
}

static void value_def(const ir_node *node)
{
	const arch_register_t *reg = arch_get_irn_register(node);

	if (reg == NULL || reg->type & arch_register_type_virtual)
		return;

	const arch_register_req_t *req = arch_get_irn_register_req(node);
	assert(req->width > 0);
	unsigned idx = reg->global_index;
	for (unsigned i = 0; i < req->width; ++i) {
		const ir_node *reg_node = registers[idx+i];

		/* a little cheat, since its so hard to remove all outedges to dead code
		 * in the backend. This particular case should never be a problem. */
		if (reg_node == NULL && get_irn_n_edges(node) == 0)
			return;

		if (reg_node != node
		    && (!ignore_sp_problems
		        || !(req->type & arch_register_req_type_produces_sp))) {
			ir_fprintf(stderr, "Verify warning: Node %+F not registered as value for Register %s (but %+F) in block %+F(%s)\n",
			           node, reg->name, reg_node, get_nodes_block(node),
			           get_irg_name(irg));
			problem_found = true;
		}
		registers[idx+i] = NULL;
	}
}

static void verify_block_register_allocation(ir_node *block, void *data)
{
	(void) data;

	assert(lv->sets_valid && "live sets must be computed");

	unsigned n_regs = arch_env->n_registers;
	registers = ALLOCANZ(const ir_node*, n_regs);

	be_lv_foreach(lv, block, be_lv_state_end, lv_node) {
		value_used(block, lv_node);
	}

	sched_foreach_reverse(block, node) {
		be_foreach_value(node, value,
			value_def(value);
			check_output_constraints(value);
		);

		check_input_constraints(node);

		/* process uses. (Phi inputs are no real uses) */
		if (!is_Phi(node)) {
			foreach_irn_in(node, i, use) {
				value_used(block, use);
			}
		}
	}

	be_lv_foreach(lv, block, be_lv_state_in, lv_node) {
		value_def(lv_node);
	}

	/* set must be empty now */
	for (unsigned i = 0; i < n_regs; ++i) {
		if (registers[i] == NULL)
			continue;

		ir_fprintf(stderr, "Verify warning: Node %+F not live-in and no def found in block %+F(%s)\n",
				registers[i], block, get_irg_name(irg));
		problem_found = true;
	}
}

bool be_verify_register_allocation(ir_graph *new_irg,
                                   bool new_ignore_sp_problems)
{
	irg                = new_irg;
	arch_env           = be_get_irg_arch_env(irg);
	lv                 = be_liveness_new(irg);
	ignore_sp_problems = new_ignore_sp_problems;
	problem_found      = false;

	be_liveness_compute_sets(lv);
	irg_block_walk_graph(irg, verify_block_register_allocation, NULL, NULL);
	be_liveness_free(lv);

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
	lv_walker_t *w     = (lv_walker_t*)data;
	be_lv_t     *fresh = (be_lv_t*)w->data;

	be_lv_info_t *curr = ir_nodehashmap_get(be_lv_info_t, &lv->map, bl);
	be_lv_info_t *fr   = ir_nodehashmap_get(be_lv_info_t, &fresh->map, bl);

	if (!fr && curr && curr[0].head.n_members > 0) {
		ir_fprintf(stderr, "%+F liveness should be empty but current liveness contains:\n", bl);
		for (unsigned i = 0; i < curr[0].head.n_members; ++i) {
			ir_fprintf(stderr, "\t%+F\n", curr[1 + i].node.node);
		}
	} else if (curr) {
		unsigned n_curr  = curr[0].head.n_members;
		unsigned n_fresh = fr[0].head.n_members;

		if (n_curr != n_fresh) {
			ir_fprintf(stderr, "%+F: liveness set sizes differ. curr %d, correct %d\n", bl, n_curr, n_fresh);

			ir_fprintf(stderr, "current:\n");
			for (unsigned i = 0; i < n_curr; ++i) {
				be_lv_info_node_t *n = &curr[1 + i].node;
				ir_fprintf(stderr, "%+F %u %+F %s\n", bl, i, n->node, lv_flags_to_str(n->flags));
			}

			ir_fprintf(stderr, "correct:\n");
			for (unsigned i = 0; i < n_fresh; ++i) {
				be_lv_info_node_t *n = &fr[1 + i].node;
				ir_fprintf(stderr, "%+F %u %+F %s\n", bl, i, n->node, lv_flags_to_str(n->flags));
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
