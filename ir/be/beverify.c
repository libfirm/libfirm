/*
 * Author:      Matthias Braun
 * Date:		05.05.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * License:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
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
	const arch_env_t *arch_env;
	const arch_register_class_t *cls;
	int registers_available;
	int problem_found;
} be_verify_register_pressure_env_t;

static void verify_liveness_walker(ir_node *bl, void *data)
{
	be_verify_register_pressure_env_t *env = (be_verify_register_pressure_env_t*) data;
	int pressure;
	pset *live_nodes = pset_new_ptr_default();
	ir_node *irn;

	// collect register pressure info
	be_liveness_end_of_block(env->arch_env, env->cls, bl, live_nodes);
	pressure = pset_count(live_nodes);
	if(pressure > env->registers_available) {
		ir_printf("Verify Warning: Register pressure too high at end of block %+F (%d/%d).\n",
			bl, pressure, env->registers_available);
		env->problem_found = 1;
	}
	sched_foreach_reverse(bl, irn) {
		int pressure;

		if(is_Phi(irn))
			break;

		be_liveness_transfer(env->arch_env, env->cls, irn, live_nodes);
		pressure = pset_count(live_nodes);

		if(pressure > env->registers_available) {
			ir_printf("Verify Warning: Register pressure too high before %+F (in block %+F) (%d/%d).\n",
				irn, bl, pressure, env->registers_available);
			env->problem_found = 1;
		}
	}
	del_pset(live_nodes);
}

void be_verify_register_pressure(const arch_env_t *arch_env, const arch_register_class_t *cls, ir_graph *irg)
{
	be_verify_register_pressure_env_t env;

	be_liveness(irg);

	env.arch_env = arch_env;
	env.cls = cls;
	env.registers_available = arch_count_non_ignore_regs(arch_env, cls);
	env.problem_found = 0;

	irg_block_walk_graph(irg, verify_liveness_walker, NULL, &env);

	assert(env.problem_found == 0);
}

typedef struct be_verify_schedule_env_t_ {
	int problem_found;
	ir_graph *irg;
} be_verify_schedule_env_t;

static void verify_schedule_walker(ir_node *bl, void *data)
{
	be_verify_schedule_env_t *env = (be_verify_schedule_env_t*) data;
	ir_node *irn;
	int non_phi_found = 0;
	int first_cfchange_found = 0;

	/*
	 * Make sure that all phi nodes are scheduled at the beginning of the block, and that there
	 * are no nodes scheduled after a control flow changing node
	 */
	sched_foreach(bl, irn) {
		if(is_Phi(irn)) {
			if(non_phi_found) {
				ir_printf("Verify Warning: Phi node %+F scheduled after non-Phi nodes in block %+F (%s)\n",
					irn, bl, get_irg_dump_name(env->irg));
				env->problem_found = 1;
			}
			continue;
		}

		non_phi_found = 1;
		if(is_cfop(irn) && get_irn_opcode(irn) != iro_Start) {
			first_cfchange_found = 1;
		} else {
			if(first_cfchange_found) {
				ir_printf("Verify Warning: Node %+F scheduled after control flow changing node in block %+F (%s)\n",
					irn, bl, get_irg_dump_name(env->irg));
				env->problem_found = 1;
			}
		}
	}
}


void be_verify_schedule(ir_graph *irg)
{
	be_verify_schedule_env_t env;

	env.problem_found = 0;
	env.irg = irg;

	irg_block_walk_graph(irg, verify_schedule_walker, NULL, &env);

	assert(env.problem_found == 0);
}
