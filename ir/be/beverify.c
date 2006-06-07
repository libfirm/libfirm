/*
 * Author:    Matthias Braun
 * Date:      05.05.2006
 * Copyright: (c) Universitaet Karlsruhe
 * License:   This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-Id:    $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "beverify.h"
#include "belive.h"
#include "besched.h"

#include "irnode.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irdump_t.h"

typedef struct be_verify_register_pressure_env_t_ {
	ir_graph                    *irg;                 /**< the irg to verify */
	const arch_env_t            *arch_env;            /**< an architecture environment */
	const arch_register_class_t *cls;                 /**< the register class to check for */
	int                         registers_available;  /**< number of available registers */
	int                         problem_found;        /**< flag indicating if a problem was found */
} be_verify_register_pressure_env_t;

/**
 * Print all nodes of a pset into a file.
 */
static void print_living_values(FILE *F, pset *live_nodes)
{
	ir_node *node;

	ir_fprintf(F, "\t");
	foreach_pset(live_nodes, node) {
		ir_fprintf(F, "%+F ", node);
	}
	ir_fprintf(F, "\n");
}

/**
 * Check if number of live nodes never exceeds the number of available registers.
 */
static void verify_liveness_walker(ir_node *block, void *data)
{
	be_verify_register_pressure_env_t *env = (be_verify_register_pressure_env_t *)data;
	pset    *live_nodes = pset_new_ptr_default();
	ir_node *irn;
	int pressure;

	/* collect register pressure info, start with end of a block */
	be_liveness_end_of_block(env->arch_env, env->cls, block, live_nodes);

	pressure = pset_count(live_nodes);
	if(pressure > env->registers_available) {
		ir_fprintf(stderr, "Verify Warning: Register pressure too high at end of block %+F(%s) (%d/%d):\n",
			block, get_irg_dump_name(env->irg), pressure, env->registers_available);
		print_living_values(stderr, live_nodes);
		env->problem_found = 1;
	}

	sched_foreach_reverse(block, irn) {
		if (is_Phi(irn))
			break;

		be_liveness_transfer(env->arch_env, env->cls, irn, live_nodes);

		pressure = pset_count(live_nodes);

		if(pressure > env->registers_available) {
			ir_fprintf(stderr, "Verify Warning: Register pressure too high before node %+F in %+F(%s) (%d/%d):\n",
				irn, block, get_irg_dump_name(env->irg), pressure, env->registers_available);
			print_living_values(stderr, live_nodes);
			env->problem_found = 1;
		}
	}
	del_pset(live_nodes);
}

/**
 * Start a walk over the irg and check the register pressure.
 */
int be_verify_register_pressure(const arch_env_t *arch_env, const arch_register_class_t *cls, ir_graph *irg)
{
	be_verify_register_pressure_env_t env;

	be_liveness(irg);

	env.irg                 = irg;
	env.arch_env            = arch_env;
	env.cls                 = cls;
	env.registers_available = arch_count_non_ignore_regs(arch_env, cls);
	env.problem_found       = 0;

	irg_block_walk_graph(irg, verify_liveness_walker, NULL, &env);

	return ! env.problem_found;
}

typedef struct be_verify_schedule_env_t_ {
	int      problem_found;    /**< flags indicating if there was a problem */
	ir_graph *irg;             /**< the irg to check */
} be_verify_schedule_env_t;

/**
 * Simple schedule checker.
 */
static void verify_schedule_walker(ir_node *block, void *data)
{
	be_verify_schedule_env_t *env = (be_verify_schedule_env_t*) data;
	ir_node *node;
	int non_phi_found  = 0;
	int cfchange_found = 0;
	// TODO ask arch about delay branches
	int delay_branches = 0;
	pset *uses = pset_new_ptr_default();

	/*
	 * Tests for the following things:
	 *   1. Make sure that all phi nodes are scheduled at the beginning of the block
	 *   2. There is 1 or no control flow changing node scheduled and exactly delay_branches operations after it.
	 *   3. No value is defined after it has been used
	 */
	sched_foreach(block, node) {
		int i, arity;

		// 1. Check for phis
		if (is_Phi(node)) {
			if (non_phi_found) {
				ir_fprintf(stderr, "Verify Warning: Phi node %+F scheduled after non-Phi nodes in block %+F (%s)\n",
					node, block, get_irg_dump_name(env->irg));
				env->problem_found = 1;
			}
		} else {
                    non_phi_found = 1;
                }

		// 2. Check for control flow changing nodes
		if (is_cfop(node) && get_irn_opcode(node) != iro_Start) {
			/* check, that only one CF operation is scheduled */
			if (cfchange_found == 1) {
				ir_fprintf(stderr, "Verify Warning: More than 1 control flow changing node (%+F) scheduled in block %+F (%s)\n",
					node, block, get_irg_dump_name(env->irg));
				env->problem_found = 1;
			}
			cfchange_found = 1;
		} else if (cfchange_found) {
			/* check for delay branches */
			if (delay_branches == 0) {
				ir_fprintf(stderr, "Verify Warning: Node %+F scheduled after control flow changing node (+delay branches) in block %+F (%s)\n",
					node, block, get_irg_dump_name(env->irg));
				env->problem_found = 1;
			} else {
				delay_branches--;
			}
		}

		// 3. Check for uses
		if(pset_find_ptr(uses, node)) {
			ir_fprintf(stderr, "Verify Warning: Value %+F used before it was defined in block %+F (%s)\n",
				node, block, get_irg_dump_name(env->irg));
			env->problem_found = 1;
		}
		for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
			pset_insert_ptr(uses, get_irn_n(node, i));
		}
	}
	del_pset(uses);

	/* check that all delay branches are used (at least with NOPs) */
	if (cfchange_found && delay_branches != 0) {
		ir_fprintf(stderr, "Not all delay slots filled after jump (%d/%d) in block %+F (%s)\n",
			block, get_irg_dump_name(env->irg));
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

	irg_block_walk_graph(irg, verify_schedule_walker, NULL, &env);

	return ! env.problem_found;
}
