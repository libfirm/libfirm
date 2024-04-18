/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Provides several statistic functions for the backend.
 * @author      Christian Wuerdig, Matthias Braun
 */
#include "bestat.h"

#include "bearch.h"
#include "beirg.h"
#include "belive.h"
#include "benode.h"
#include "besched.h"
#include "execfreq.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irnode_t.h"
#include "panic.h"
#include "statev_t.h"
#include "target_t.h"
#include "util.h"
#include <time.h>

typedef struct pressure_walker_env_t pressure_walker_env_t;
struct pressure_walker_env_t {
	ir_graph                    *irg;
	be_lv_t                     *lv;
	double                       insn_count;
	double                       regpressure;
	unsigned                     max_pressure;
	const arch_register_class_t *cls;
};

static void check_reg_pressure_class(pressure_walker_env_t *env,
                                     ir_node *block,
                                     const arch_register_class_t *cls)
{
	ir_nodeset_t live_nodes;
	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block(env->lv, cls, block, &live_nodes);
	unsigned max_live = ir_nodeset_size(&live_nodes);
	env->regpressure += max_live;

	sched_foreach_non_phi_reverse(block, irn) {
		be_liveness_transfer(cls, irn, &live_nodes);
		size_t const cnt = ir_nodeset_size(&live_nodes);
		max_live = MAX(max_live, cnt);
		env->regpressure += cnt;
		env->insn_count++;
	}

	if (max_live > env->max_pressure)
		env->max_pressure = max_live;

	ir_nodeset_destroy(&live_nodes);
}

static void stat_reg_pressure_block(ir_node *block, void *data)
{
	pressure_walker_env_t *env = (pressure_walker_env_t*)data;

	check_reg_pressure_class(env, block, env->cls);
}

void be_do_stat_reg_pressure(ir_graph *irg, const arch_register_class_t *cls)
{
	be_assure_live_sets(irg);
	pressure_walker_env_t env;
	env.irg          = irg;
	env.insn_count   = 0;
	env.max_pressure = 0;
	env.regpressure  = 0;
	env.lv           = be_get_irg_liveness(irg);
	env.cls          = cls;

	/* Collect register pressure information for each block */
	irg_block_walk_graph(irg, stat_reg_pressure_block, NULL, &env);

	double average_pressure = env.regpressure / env.insn_count;
	stat_ev_dbl("bechordal_average_register_pressure", average_pressure);
	stat_ev_dbl("bechordal_maximum_register_pressure", env.max_pressure);
}

typedef struct estimate_irg_costs_env_t {
	double costs;
} estimate_irg_costs_env_t;

static void estimate_block_costs(ir_node *block, void *data)
{
	estimate_irg_costs_env_t *env = (estimate_irg_costs_env_t*)data;
	double costs = 0.0;

	sched_foreach(block, node) {
		costs += ir_target.isa->get_op_estimated_cost(node);
	}

	env->costs += costs * get_block_execfreq(block);
}

double be_estimate_irg_costs(ir_graph *irg)
{
	estimate_irg_costs_env_t env;
	env.costs = 0.0;

	irg_block_walk_graph(irg, estimate_block_costs, NULL, &env);
	return env.costs;
}

static void node_stat_walker(ir_node *irn, void *data)
{
	be_node_stats_t *const stats = (be_node_stats_t*)data;

	/* if the node is a normal phi */
	if (is_Phi(irn)) {
		if (get_irn_mode(irn) == mode_M) {
			(*stats)[BE_STAT_MEM_PHIS]++;
		} else {
			(*stats)[BE_STAT_PHIS]++;
		}
	} else if (be_is_Perm(irn)) {
		(*stats)[BE_STAT_PERMS]++;
	} else if (be_is_Copy(irn)) {
		(*stats)[BE_STAT_COPIES]++;
	}
}

void be_collect_node_stats(be_node_stats_t *new_stats, ir_graph *irg)
{
	memset(new_stats, 0, sizeof(*new_stats));
	irg_walk_graph(irg, NULL, node_stat_walker, new_stats);
}

void be_subtract_node_stats(be_node_stats_t *stats, be_node_stats_t *sub)
{
	for (be_stat_tag_t i = BE_STAT_FIRST; i < BE_STAT_COUNT; ++i) {
		(*stats)[i] -= (*sub)[i];
	}
}

void be_copy_node_stats(be_node_stats_t *dest, be_node_stats_t *src)
{
	MEMCPY(dest, src, 1);
}

static const char *get_stat_name(enum be_stat_tag_t tag)
{
	switch (tag) {
	case BE_STAT_PHIS:     return "phis";
	case BE_STAT_MEM_PHIS: return "mem_phis";
	case BE_STAT_COPIES:   return "copies";
	case BE_STAT_PERMS:    return "perms";
	default:               panic("unknown stat tag found");
	}
}

void be_emit_node_stats(be_node_stats_t *stats, const char *prefix)
{
	for (be_stat_tag_t i = BE_STAT_FIRST; i < BE_STAT_COUNT; ++i) {
		char buf[128];
		snprintf(buf, sizeof(buf), "%s%s", prefix, get_stat_name(i));
		stat_ev_dbl(buf, (*stats)[i]);
	}
}

static void insn_count_walker(ir_node *irn, void *data)
{
	unsigned long *cnt = (unsigned long*)data;

	switch (get_irn_opcode(irn)) {
	case iro_Proj:
	case iro_Phi:
	case iro_End:
		break;
	default:
		(*cnt)++;
	}
}

unsigned long be_count_insns(ir_graph *irg)
{
	unsigned long cnt = 0;
	irg_walk_graph(irg, insn_count_walker, NULL, &cnt);
	return cnt;
}

static void block_count_walker(ir_node *node, void *data)
{
	unsigned long *cnt = (unsigned long*)data;
	if (node == get_irg_end_block(get_irn_irg(node)))
		return;
	(*cnt)++;
}

unsigned long be_count_blocks(ir_graph *irg)
{
	unsigned long cnt = 0;
	irg_block_walk_graph(irg, block_count_walker, NULL, &cnt);
	return cnt;
}

typedef struct stat_t {
	unsigned long values;
	unsigned long unused_values;
	unsigned long uses;
	unsigned long should_be_sames;
	unsigned long constrained_values;
	unsigned long constrained_uses;
	unsigned long unused_constrained_values;
} stat_t;

static void block_count_values(ir_node *block, void *data)
{
	stat_t *stats = (stat_t*)data;

	sched_foreach(block, node) {
		be_foreach_value(node, value,
			arch_register_req_t const *const req
				= arch_get_irn_register_req(value);
			if (!req->cls->regs)
				continue;
			++stats->values;
			if (req->should_be_same != 0 || is_Phi(value))
				++stats->should_be_sames;
			if (req->limited != NULL)
				++stats->constrained_values;
		);
		for (int i = 0, arity = get_irn_arity(node); i < arity; ++i) {
			const arch_register_req_t *req = arch_get_irn_register_req_in(node, i);
			if (!req->cls->regs)
				continue;
			++stats->uses;
			if (be_is_Keep(node)) {
				ir_node *value = get_irn_n(node, i);
				if (get_irn_n_edges(value) <= 1) {
					++stats->unused_values;
					const arch_register_req_t *const req
						= arch_get_irn_register_req(value);
					if (req->limited != NULL)
						++stats->unused_constrained_values;
				}
			}
			if (req->limited != NULL)
				++stats->constrained_uses;
		}
	}
}

void be_stat_values(ir_graph *irg)
{
	stat_t stats;
	memset(&stats, 0, sizeof(stats));
	irg_block_walk_graph(irg, block_count_values, NULL, &stats);
	stat_ev_ull("valstat_values", stats.values);
	stat_ev_ull("valstat_unused", stats.unused_values);
	stat_ev_ull("valstat_uses", stats.uses);
	stat_ev_ull("valstat_should_be_sames", stats.should_be_sames);
	stat_ev_ull("valstat_constrained_values", stats.constrained_values);
	stat_ev_ull("valstat_constrained_uses", stats.constrained_uses);
	stat_ev_ull("valstat_unused_constrained_values",
	            stats.unused_constrained_values);
}
