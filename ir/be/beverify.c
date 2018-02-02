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
#include "beverify.h"

#include "array.h"
#include "beirg.h"
#include "belistsched.h"
#include "belive.h"
#include "benode.h"
#include "besched.h"
#include "bitset.h"
#include "irdump_t.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irprintf.h"
#include "set.h"
#include "target_t.h"
#include <stdbool.h>
#include <stdbool.h>

typedef struct be_verify_register_pressure_env_t_ {
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

static void verify_warnf(ir_node const *const node, char const *const fmt, ...)
{
	FILE* const f = stderr;

	ir_node const *const block    = get_block_const(node);
	ir_graph      *const irg      = get_irn_irg(node);
	ir_entity     *const irg_ent  = get_irg_entity(irg);
	char    const *const irg_name = get_entity_ld_name(irg_ent);
	ir_fprintf(f, "%+F(%s): verify warning: ", block, irg_name);
	va_list ap;
	va_start(ap, fmt);
	ir_vfprintf(f, fmt, ap);
	va_end(ap);
	fputc('\n', f);
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
		verify_warnf(block, "register pressure too high at end of block (%d/%d):",
			pressure, env->registers_available);
		print_living_values(stderr, &live_nodes);
		env->problem_found = true;
	}

	sched_foreach_non_phi_reverse(block, irn) {
		// print_living_values(stderr, &live_nodes);
		be_liveness_transfer(env->cls, irn, &live_nodes);

		pressure = ir_nodeset_size(&live_nodes);

		if (pressure > env->registers_available) {
			verify_warnf(block, "register pressure too high before %+F (%d/%d):",
				irn, pressure, env->registers_available);
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
} be_verify_schedule_env_t;

static void verify_schedule_walker(ir_node *block, void *data)
{
	be_verify_schedule_env_t *env = (be_verify_schedule_env_t*) data;

	/*
	 * Tests for the following things:
	 *   1. Make sure that all phi nodes are scheduled at the beginning of the
	 *      block
	 *   2. No value is defined after it has been used
	 */
	ir_node         *non_phi_found  = NULL;
	ir_node         *cfchange_found = NULL;
	sched_timestep_t last_timestep  = 0;
	sched_foreach(block, node) {
		/* this node is scheduled */
		if (bitset_is_set(env->scheduled, get_irn_idx(node))) {
			verify_warnf(block, "%+F appears to be schedule twice");
			env->problem_found = true;
		}
		bitset_set(env->scheduled, get_irn_idx(node));

		/* Check that scheduled nodes are in the correct block */
		if (get_nodes_block(node) != block) {
			verify_warnf(block, "%+F is in wrong %+F", node, get_nodes_block(node));
			env->problem_found = true;
		}

		/* Check that timesteps are increasing */
		sched_timestep_t timestep = sched_get_time_step(node);
		if (timestep <= last_timestep) {
			verify_warnf(block, "schedule timestep did not increase at %+F", node);
			env->problem_found = true;
		}
		last_timestep = timestep;

		if (arch_get_irn_flags(node) & arch_irn_flag_not_scheduled) {
			verify_warnf(block, "flag_not_scheduled node %+F scheduled anyway", node);
			env->problem_found = true;
		}

		/* Check that phis come before any other node */
		if (!is_Phi(node)) {
			non_phi_found = node;
		} else if (non_phi_found) {
			verify_warnf(block, "%+F scheduled after non-Phi %+F", node, non_phi_found);
			env->problem_found = true;
		}

		/* Check for control flow changing nodes */
		if (is_cfop(node)) {
			/* check, that only one CF operation is scheduled */
			if (cfchange_found != NULL) {
				verify_warnf(block, "additional control flow changing node %+F scheduled after %+F", node, cfchange_found);
				env->problem_found = true;
			} else {
				cfchange_found = node;
			}
		} else if (cfchange_found != NULL) {
			/* keepany isn't a real instruction. */
			if (!be_is_Keep(node)) {
				verify_warnf(block, "%+F scheduled after control flow changing node", node);
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
					verify_warnf(block, "%+F used by %+F before it was defined", arg, node);
					env->problem_found = true;
				}
			}
		}

		/* Check that no dead nodes are scheduled */
		if (get_irn_n_edges(node) == 0) {
			verify_warnf(block, "%+F is dead but scheduled", node);
			env->problem_found = true;
		}

		if (be_is_Keep(node) || be_is_CopyKeep(node)) {
			/* at least 1 of the keep arguments has to be its schedule
			 * predecessor */
			ir_node *prev = sched_prev(node);
			while (be_is_Keep(prev) || be_is_CopyKeep(prev))
				prev = sched_prev(prev);

			do {
				foreach_irn_in(node, i, in) {
					if (skip_Proj(in) == prev)
						goto ok;
				}
				prev = sched_prev(prev);
			} while (is_Phi(prev));
			verify_warnf(block, "%+F not scheduled after its pred node", node);
			env->problem_found = true;
ok:;
		}
	}
}

static void check_schedule(ir_node *node, void *data)
{
	be_verify_schedule_env_t *env = (be_verify_schedule_env_t*)data;
	bool const should_be = !arch_is_irn_not_scheduled(node);
	bool const scheduled = bitset_is_set(env->scheduled, get_irn_idx(node));

	if (should_be != scheduled) {
		verify_warnf(node, "%+F should%s be scheduled", node, should_be ? "" : " not");
		env->problem_found = true;
	}
}

bool be_verify_schedule(ir_graph *irg)
{
	be_verify_schedule_env_t env;
	env.problem_found = false;
	env.scheduled     = bitset_alloca(get_irg_last_idx(irg));

	irg_block_walk_graph(irg, verify_schedule_walker, NULL, &env);
	/* check if all nodes are scheduled */
	irg_walk_graph(irg, check_schedule, NULL, &env);

	return ! env.problem_found;
}

/*--------------------------------------------------------------------------- */

typedef struct be_verify_reg_alloc_env_t {
	be_lv_t *lv;
	bool     problem_found;
} be_verify_reg_alloc_env_t;

static void check_output_constraints(be_verify_reg_alloc_env_t *const env, const ir_node *node)
{
	arch_register_req_t const *const req = arch_get_irn_register_req(node);
	if (!req->cls->regs)
		return;

	/* verify output register */
	arch_register_t const *const reg = arch_get_irn_register(node);
	if (reg == NULL) {
		verify_warnf(node, "%+F should have a register assigned", node);
		env->problem_found = true;
	} else if (!arch_reg_is_allocatable(req, reg)) {
		verify_warnf(node, "register %s assigned as output of %+F not allowed (register constraint)", reg->name, node);
		env->problem_found = true;
	}
}

static void check_input_constraints(be_verify_reg_alloc_env_t *const env, ir_node *const node)
{
	arch_register_req_t const **const in_reqs = arch_get_irn_register_reqs_in(node);
	if (!in_reqs && get_irn_arity(node) != 0) {
		verify_warnf(node, "%+F has no input requirements", node);
		env->problem_found = true;
		return;
	}

	/* verify input register */
	foreach_irn_in(node, i, pred) {
		if (is_Bad(pred)) {
			verify_warnf(node, "%+F has Bad as input %d", node, i);
			env->problem_found = true;
			continue;
		}

		const arch_register_req_t *req      = arch_get_irn_register_req_in(node, i);
		const arch_register_req_t *pred_req = arch_get_irn_register_req(pred);
		if (req->cls != pred_req->cls) {
			verify_warnf(node, "%+F register class of requirement at input %d and operand differ", node, i);
			env->problem_found = true;
		}

		if (!req->cls->regs)
			continue;

		if (req->width > pred_req->width) {
			verify_warnf(node, "%+F register width of value at input %d too small", node, i);
			env->problem_found = true;
		}

		const arch_register_t *reg = arch_get_irn_register(pred);
		if (reg == NULL) {
			verify_warnf(pred, "%+F should have a register assigned (%+F input constraint)", pred, node);
			env->problem_found = true;
		} else if (!arch_reg_is_allocatable(req, reg)) {
			verify_warnf(node, "register %s as input %d of %+F not allowed (register constraint)", reg->name, i, node);
			env->problem_found = true;
		}
	}

	/* phis should be NOPs at this point, which means all input regs
	 * must be the same as the output reg */
	if (is_Phi(node)) {
		const arch_register_t *reg = arch_get_irn_register(node);
		foreach_irn_in(node, i, pred) {
			const arch_register_t *pred_reg = arch_get_irn_register(pred);

			if (reg != pred_reg && !(pred_reg->is_virtual)) {
				const char *pred_name = pred_reg != NULL ? pred_reg->name : "(null)";
				const char *reg_name  = reg != NULL ? reg->name : "(null)";
				verify_warnf(node, "input %d of %+F uses register %s instead of %s", i, node, pred_name, reg_name);
				env->problem_found = true;
			}
		}
	}
}

static bool ignore_error_for_reg(ir_graph *irg, const arch_register_t *reg)
{
	be_irg_t *birg = be_birg_from_irg(irg);
	if (birg->non_ssa_regs == NULL)
		return false;
	return rbitset_is_set(birg->non_ssa_regs, reg->global_index);
}

static void value_used(be_verify_reg_alloc_env_t *const env, ir_node const **const registers, ir_node const *const block, ir_node const *const node)
{
	const arch_register_t *reg = arch_get_irn_register(node);
	if (reg == NULL || reg->is_virtual)
		return;

	const arch_register_req_t *req = arch_get_irn_register_req(node);
	assert(req->width > 0);
	unsigned idx = reg->global_index;
	for (unsigned i = 0; i < req->width; ++i) {
		ir_node const *const reg_node = registers[idx + i];
		if (reg_node != NULL && reg_node != node
			&& !ignore_error_for_reg(get_irn_irg(block), reg)) {
			verify_warnf(block, "register %s assigned more than once (nodes %+F and %+F)", arch_register_for_index(reg->cls, reg->index + i)->name, node, reg_node);
			env->problem_found = true;
		}
		registers[idx + i] = node;
	}
}

static void value_def(be_verify_reg_alloc_env_t *const env, ir_node const **const registers, ir_node const *const node)
{
	const arch_register_t *reg = arch_get_irn_register(node);

	if (reg == NULL || reg->is_virtual)
		return;

	const arch_register_req_t *req = arch_get_irn_register_req(node);
	assert(req->width > 0);
	unsigned idx = reg->global_index;
	for (unsigned i = 0; i < req->width; ++i) {
		ir_node const *const reg_node = registers[idx + i];

		/* a little cheat, since its so hard to remove all outedges to dead code
		 * in the backend. This particular case should never be a problem. */
		if (reg_node == NULL && get_irn_n_edges(node) == 0)
			return;

		if (reg_node != node && !ignore_error_for_reg(get_irn_irg(node), reg)) {
			verify_warnf(node, "%+F not registered as value for register %s (but %+F)", node, reg->name, reg_node);
			env->problem_found = true;
		}
		registers[idx + i] = NULL;
	}
}

static void verify_block_register_allocation(ir_node *block, void *data)
{
	be_verify_reg_alloc_env_t *const env = (be_verify_reg_alloc_env_t*)data;

	unsigned        const n_regs    = ir_target.isa->n_registers;
	ir_node const **const registers = ALLOCANZ(ir_node const*, n_regs);

	be_lv_foreach(env->lv, block, be_lv_state_end, lv_node) {
		value_used(env, registers, block, lv_node);
	}

	sched_foreach_reverse(block, node) {
		be_foreach_value(node, value,
			value_def(env, registers, value);
			check_output_constraints(env, value);
		);

		check_input_constraints(env, node);

		/* process uses. (Phi inputs are no real uses) */
		if (!is_Phi(node)) {
			foreach_irn_in(node, i, use) {
				value_used(env, registers, block, use);
			}
		}
	}

	be_lv_foreach(env->lv, block, be_lv_state_in, lv_node) {
		value_def(env, registers, lv_node);
	}

	/* set must be empty now */
	for (unsigned i = 0; i < n_regs; ++i) {
		if (registers[i]) {
			verify_warnf(block, "%+F not live-in and no def found", registers[i]);
			env->problem_found = true;
		}
	}
}

bool be_verify_register_allocation(ir_graph *const irg)
{
	be_verify_reg_alloc_env_t env = {
		.lv                 = be_liveness_new(irg),
		.problem_found      = false,
	};

	be_liveness_compute_sets(env.lv);
	irg_block_walk_graph(irg, verify_block_register_allocation, NULL, &env);
	be_liveness_free(env.lv);

	return !env.problem_found;
}

/*--------------------------------------------------------------------------- */

typedef struct lv_walker_t {
	be_lv_t *given;
	be_lv_t *fresh;
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
	lv_walker_t    *const w       = (lv_walker_t*)data;
	be_lv_info_t   *const curr    = ir_nodehashmap_get(be_lv_info_t, &w->given->map, bl);
	be_lv_info_t   *const fresh   = ir_nodehashmap_get(be_lv_info_t, &w->fresh->map, bl);
	unsigned const        n_curr  = curr  ? curr->n_members  : 0;
	unsigned const        n_fresh = fresh ? fresh->n_members : 0;
	if (n_curr != n_fresh) {
		ir_fprintf(stderr, "%+F: liveness set sizes differ. curr %d, correct %d\n", bl, n_curr, n_fresh);

		ir_fprintf(stderr, "current:\n");
		for (unsigned i = 0; i < n_curr; ++i) {
			be_lv_info_node_t *const n = &curr->nodes[i];
			ir_fprintf(stderr, "%+F %u %+F %s\n", bl, i, n->node, lv_flags_to_str(n->flags));
		}

		ir_fprintf(stderr, "correct:\n");
		for (unsigned i = 0; i < n_fresh; ++i) {
			be_lv_info_node_t *const n = &fresh->nodes[i];
			ir_fprintf(stderr, "%+F %u %+F %s\n", bl, i, n->node, lv_flags_to_str(n->flags));
		}
	}
}

void be_liveness_check(be_lv_t *lv)
{
	be_lv_t *const fresh = be_liveness_new(lv->irg);
	be_liveness_compute_sets(fresh);
	lv_walker_t w = {
		.given = lv,
		.fresh = fresh,
	};
	irg_block_walk_graph(lv->irg, lv_check_walker, NULL, &w);
	be_liveness_free(fresh);
}
