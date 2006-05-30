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
static void verify_liveness_walker(ir_node *bl, void *data)
{
	be_verify_register_pressure_env_t *env = (be_verify_register_pressure_env_t *)data;
	pset    *live_nodes = pset_new_ptr_default();
	ir_node *irn;

	/* collect register pressure info, start with end of a block */
	be_liveness_end_of_block(env->arch_env, env->cls, bl, live_nodes);

	sched_foreach_reverse(bl, irn) {
		int pressure = pset_count(live_nodes);

		if (is_Phi(irn))
			break;

		if(pressure > env->registers_available) {
			ir_fprintf(stderr, "Verify Warning: Register pressure too high at end of block %+F(%s) (%d/%d):\n",
				bl, get_irg_dump_name(env->irg), pressure, env->registers_available);
			print_living_values(stderr, live_nodes);
			env->problem_found = 1;
		}

		be_liveness_transfer(env->arch_env, env->cls, irn, live_nodes);
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
static void verify_schedule_walker(ir_node *bl, void *data)
{
	be_verify_schedule_env_t *env = (be_verify_schedule_env_t*) data;
	ir_node *irn;
	int non_phi_found  = 0;
	int cfchange_found = 0;
	// TODO ask ABI about delay branches
	int delay_branches = 0;

	/*
		Make sure that all phi nodes are scheduled at the beginning of the block, and that there
		is 1 or no control flow changing node scheduled as last operation
	 */
	sched_foreach(bl, irn) {
		if (is_Phi(irn)) {
			if (non_phi_found) {
				ir_fprintf(stderr, "Verify Warning: Phi node %+F scheduled after non-Phi nodes in block %+F (%s)\n",
					irn, bl, get_irg_dump_name(env->irg));
				env->problem_found = 1;
			}
			continue;
		}
		non_phi_found = 1;

		if (is_cfop(irn) && get_irn_opcode(irn) != iro_Start) {
			/* check, that only one CF operation is scheduled */
			if (cfchange_found == 1) {
				ir_fprintf(stderr, "Verify Warning: More than 1 control flow changing node (%+F) scheduled in block %+F (%s)\n",
					irn, bl, get_irg_dump_name(env->irg));
				env->problem_found = 1;
			}
			cfchange_found = 1;
		} else if (cfchange_found) {
			/* check for delay branches */
			if (delay_branches == 0) {
				ir_fprintf(stderr, "Verify Warning: Node %+F scheduled after control flow changing node (+delay branches) in block %+F (%s)\n",
					irn, bl, get_irg_dump_name(env->irg));
				env->problem_found = 1;
			} else {
				delay_branches--;
			}
		}
	}

	/* check that all delay branches are used (at least with NOPs) */
	if (cfchange_found && delay_branches != 0) {
		ir_fprintf(stderr, "Not all delay slots filled after jump (%d/%d) in block %+F (%s)\n",
			bl, get_irg_dump_name(env->irg));
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
