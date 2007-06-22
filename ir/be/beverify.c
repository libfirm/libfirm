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
 * @brief       Various verify routines that check a scheduled graph for correctness.
 * @author      Matthias Braun
 * @date        05.05.2006
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits.h>

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
#include "besched_t.h"
#include "benode_t.h"
#include "beirg_t.h"
#include "beintlive_t.h"

static int my_values_interfere(const ir_node *a, const ir_node *b);

typedef struct be_verify_register_pressure_env_t_ {
	ir_graph                    *irg;                 /**< the irg to verify */
	 be_lv_t                    *lv;                  /**< Liveness information. */
	const arch_env_t            *arch_env;            /**< an architecture environment */
	const arch_register_class_t *cls;                 /**< the register class to check for */
	int                         registers_available;  /**< number of available registers */
	int                         problem_found;        /**< flag indicating if a problem was found */
} be_verify_register_pressure_env_t;

/**
 * Print all nodes of a pset into a file.
 */
static void print_living_values(FILE *F, const ir_nodeset_t *live_nodes) {
	ir_nodeset_iterator_t iter;
	ir_node *node;

	ir_fprintf(F, "\t");
	foreach_ir_nodeset(live_nodes, node, iter) {
		ir_fprintf(F, "%+F ", node);
	}
	ir_fprintf(F, "\n");
}

/**
 * Check if number of live nodes never exceeds the number of available registers.
 */
static void verify_liveness_walker(ir_node *block, void *data) {
	be_verify_register_pressure_env_t *env = (be_verify_register_pressure_env_t *)data;
	ir_nodeset_t live_nodes;
	ir_node *irn;
	int pressure;

	/* collect register pressure info, start with end of a block */
	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block(env->lv, env->arch_env, env->cls, block,
	                         &live_nodes);

	pressure = ir_nodeset_size(&live_nodes);
	if(pressure > env->registers_available) {
		ir_fprintf(stderr, "Verify Warning: Register pressure too high at end of block %+F(%s) (%d/%d):\n",
			block, get_irg_dump_name(env->irg), pressure, env->registers_available);
		print_living_values(stderr, &live_nodes);
		env->problem_found = 1;
	}

	sched_foreach_reverse(block, irn) {
		if (is_Phi(irn))
			break;

		be_liveness_transfer(env->arch_env, env->cls, irn, &live_nodes);

		pressure = ir_nodeset_size(&live_nodes);

		if(pressure > env->registers_available) {
			ir_fprintf(stderr, "Verify Warning: Register pressure too high before node %+F in %+F(%s) (%d/%d):\n",
				irn, block, get_irg_dump_name(env->irg), pressure, env->registers_available);
			print_living_values(stderr, &live_nodes);
			env->problem_found = 1;
		}
	}
	ir_nodeset_destroy(&live_nodes);
}

/**
 * Start a walk over the irg and check the register pressure.
 */
int be_verify_register_pressure(const be_irg_t *birg,
                                const arch_register_class_t *cls,
                                ir_graph *irg) {
	be_verify_register_pressure_env_t env;

	env.lv                  = be_liveness(irg);
	env.irg                 = irg;
	env.arch_env            = birg->main_env->arch_env;
	env.cls                 = cls;
	env.registers_available = env.cls->n_regs - be_put_ignore_regs(birg, env.cls, NULL);
	env.problem_found       = 0;

	be_liveness_assure_sets(env.lv);
	irg_block_walk_graph(irg, verify_liveness_walker, NULL, &env);
	be_liveness_free(env.lv);

	return ! env.problem_found;
}



/*--------------------------------------------------------------------------- */



typedef struct be_verify_schedule_env_t_ {
	int      problem_found;     /**< flags indicating if there was a problem */
	bitset_t *scheduled;        /**< bitset of scheduled nodes */
	ir_graph *irg;              /**< the irg to check */
	const arch_env_t *arch_env; /**< the arch_env */
} be_verify_schedule_env_t;

/**
 * Simple schedule checker.
 */
static void verify_schedule_walker(ir_node *block, void *data) {
	be_verify_schedule_env_t *env = (be_verify_schedule_env_t*) data;
	ir_node *node;
	int non_phi_found  = 0;
	int cfchange_found = 0;
	/* TODO ask arch about delay branches */
	int delay_branches = 0;
	int last_timestep = INT_MIN;

	/*
	 * Tests for the following things:
	 *   1. Make sure that all phi nodes are scheduled at the beginning of the block
	 *   2. There is 1 or no control flow changing node scheduled and exactly delay_branches operations after it.
	 *   3. No value is defined after it has been used
	 *   4. mode_T nodes have all projs scheduled behind them followed by Keeps
	 *       (except mode_X projs)
	 */
	sched_foreach(block, node) {
		int i, arity;
		int timestep;

		/* this node is scheduled */
		if(bitset_is_set(env->scheduled, get_irn_idx(node))) {
			ir_fprintf(stderr, "Verify warning: %+F appears to be schedule twice\n");
			env->problem_found = 1;
		}
		bitset_set(env->scheduled, get_irn_idx(node));

		/* Check that scheduled nodes are in the correct block */
		if(get_nodes_block(node) != block) {
			ir_fprintf(stderr, "Verify warning: %+F is in block %+F but scheduled in %+F\n", node, get_nodes_block(node), block);
			env->problem_found = 1;
		}

		/* Check that timesteps are increasing */
		timestep = sched_get_time_step(node);
		if(timestep <= last_timestep) {
			ir_fprintf(stderr, "Verify warning: Schedule timestep did not increase at node %+F\n",
			           node);
			env->problem_found = 1;
		}
		last_timestep = timestep;

		/* Check that phis come before any other node */
		if (is_Phi(node)) {
			if (non_phi_found) {
				ir_fprintf(stderr, "Verify Warning: Phi node %+F scheduled after non-Phi nodes in block %+F (%s)\n",
					node, block, get_irg_dump_name(env->irg));
				env->problem_found = 1;
			}
		} else {
			non_phi_found = 1;
		}

		/* Check for control flow changing nodes */
		if (is_cfop(node) && get_irn_opcode(node) != iro_Start) {
			/* check, that only one CF operation is scheduled */
			if (cfchange_found == 1) {
				ir_fprintf(stderr, "Verify Warning: More than 1 control flow changing node (%+F) scheduled in block %+F (%s)\n",
					node, block, get_irg_dump_name(env->irg));
				env->problem_found = 1;
			}
			cfchange_found = 1;
		} else if (cfchange_found) {
			/* proj and keepany aren't real instructions... */
			if(!is_Proj(node) && !be_is_Keep(node)) {
				/* check for delay branches */
				if (delay_branches == 0) {
					ir_fprintf(stderr, "Verify Warning: Node %+F scheduled after control flow changing node (+delay branches) in block %+F (%s)\n",
						node, block, get_irg_dump_name(env->irg));
					env->problem_found = 1;
				} else {
					delay_branches--;
				}
			}
		}

		/* Check that all uses come before their definitions */
		if(!is_Phi(node)) {
			int nodetime = sched_get_time_step(node);
			for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
				ir_node *arg = get_irn_n(node, i);
				if(get_nodes_block(arg) != block
				   || !sched_is_scheduled(arg))
					continue;

				if(sched_get_time_step(arg) >= nodetime) {
					ir_fprintf(stderr, "Verify Warning: Value %+F used by %+F before it was defined in block %+F (%s)\n",
					           arg, node, block, get_irg_dump_name(env->irg));
					env->problem_found = 1;
				}
			}
		}

		/* Check that no dead nodes are scheduled */
		if(get_irn_n_edges(node) == 0) {
			ir_fprintf(stderr, "Verify warning: Node %+F is dead but scheduled in block %+F (%s)\n",
			           node, block, get_irg_dump_name(env->irg));
			env->problem_found = 1;
		}

#ifdef SCHEDULE_PROJS
		/* check that all projs/keeps are behind their nodes */
		if(is_Proj(node)) {
			ir_node *prev = sched_prev(node);
			while(is_Proj(prev))
				prev = sched_prev(prev);
			if(get_Proj_pred(node) != prev) {
				ir_fprintf(stderr, "%+F not scheduled after its pred node in block %+F (%s)\n",
				           node, block, get_irg_dump_name(env->irg));
				env->problem_found = 1;
			}
		}
#endif
		if(be_is_Keep(node)) {
			/* at least 1 of the keep arguments has to be it schedule
			 * predecessor */
			int      arity   = get_irn_arity(node);
			int      problem = 1;
			ir_node *prev    = sched_prev(node);
			for(i = 0; i < arity; ++i) {
				ir_node *in = get_irn_n(node, i);
				in = skip_Proj(in);
				if(in == prev)
					problem = 0;
			}
			if(problem) {
				ir_fprintf(stderr, "%+F not scheduled after its pred node in block %+F (%s)\n",
				           node, block, get_irg_dump_name(env->irg));
				env->problem_found = 1;
			}
		}
	}

	/* check that all delay branches are filled (at least with NOPs) */
	if (cfchange_found && delay_branches != 0) {
		ir_fprintf(stderr, "Verify warning: Not all delay slots filled after jump (%d/%d) in block %+F (%s)\n",
			block, get_irg_dump_name(env->irg));
		env->problem_found = 1;
	}
}

static int should_be_scheduled(be_verify_schedule_env_t *env, ir_node *node) {
	if(is_Block(node))
		return -1;

	if(get_irn_mode(node) == mode_M) {
		if(is_Proj(node))
			return 0;
		if(is_Phi(node) || is_Sync(node) || is_Pin(node))
			return 0;
	}
#ifdef SCHEDULE_PROJS
	if(is_Proj(node)) {
		if(get_irn_mode(node) == mode_X)
			return 0;
		return should_be_scheduled(env, get_Proj_pred(node));
	}
#else
	if(is_Proj(node))
		return 0;
#endif
	if(be_is_Keep(node) && get_irn_opcode(get_nodes_block(node)) == iro_Bad)
		return 0;

	switch(get_irn_opcode(node)) {
	case iro_End:
	case iro_NoMem:
	case iro_Bad:
	case iro_Unknown:
		return 0;
	default:
		break;
	}

	if(arch_irn_get_flags(env->arch_env, node) & arch_irn_flags_ignore)
		return -1;

	return 1;
}

static void check_schedule(ir_node *node, void *data) {
	be_verify_schedule_env_t *env = data;
	int should_be;
	int scheduled;

	should_be = should_be_scheduled(env, node);
	if(should_be == -1)
		return;

	scheduled = bitset_is_set(env->scheduled, get_irn_idx(node)) ? 1 : 0;
	should_be = should_be ? 1 : 0;
	if(should_be != scheduled) {
		ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) should%s be scheduled\n",
			node, get_nodes_block(node), get_irg_dump_name(env->irg), should_be ? "" : " not");
		env->problem_found = 1;
	}
}

/**
 * Start a walk over the irg and check schedule.
 */
int be_verify_schedule(const be_irg_t *birg)
{
	be_verify_schedule_env_t env;

	env.problem_found = 0;
	env.irg           = be_get_birg_irg(birg);
	env.scheduled     = bitset_alloca(get_irg_last_idx(env.irg));
	env.arch_env      = birg->main_env->arch_env;

	irg_block_walk_graph(env.irg, verify_schedule_walker, NULL, &env);
	/* check if all nodes are scheduled */
	irg_walk_graph(env.irg, check_schedule, NULL, &env);

	return ! env.problem_found;
}



/*--------------------------------------------------------------------------- */



typedef struct _spill_t {
	ir_node *spill;
	ir_entity *ent;
} spill_t;

typedef struct {
	const arch_env_t *arch_env;
	ir_graph *irg;
	set *spills;
	ir_node **reloads;
	int problem_found;
} be_verify_spillslots_env_t;

static int cmp_spill(const void* d1, const void* d2, size_t size) {
	const spill_t* s1 = d1;
	const spill_t* s2 = d2;
	(void) size;

	return s1->spill != s2->spill;
}

static spill_t *find_spill(be_verify_spillslots_env_t *env, ir_node *node) {
	spill_t spill;

	spill.spill = node;
	return set_find(env->spills, &spill, sizeof(spill), HASH_PTR(node));
}

static spill_t *get_spill(be_verify_spillslots_env_t *env, ir_node *node, ir_entity *ent) {
	spill_t spill, *res;
	int hash = HASH_PTR(node);

	spill.spill = node;
	res = set_find(env->spills, &spill, sizeof(spill), hash);

	if(res == NULL) {
		spill.ent = ent;
		res = set_insert(env->spills, &spill, sizeof(spill), hash);
	}

	return res;
}

static ir_node *get_memory_edge(const ir_node *node) {
	int i, arity;
	ir_node *result = NULL;

	arity = get_irn_arity(node);
	for(i = arity - 1; i >= 0; --i) {
		ir_node *arg = get_irn_n(node, i);
		if(get_irn_mode(arg) == mode_M) {
			assert(result == NULL);
			result = arg;
		}
	}

	return result;
}

static
void collect(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity* ent);

static
void be_check_entity(be_verify_spillslots_env_t *env, ir_node *node, ir_entity *ent) {
	if(ent == NULL) {
		ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) should have an entity assigned\n",
		           node, get_nodes_block(node), get_irg_dump_name(env->irg));
	}
}

static
void collect_spill(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity* ent) {
	ir_entity *spillent = arch_get_frame_entity(env->arch_env, node);
	be_check_entity(env, node, spillent);
	get_spill(env, node, ent);

	if(spillent != ent) {
		ir_fprintf(stderr, "Verify warning: Spill %+F has different entity than reload %+F in block %+F(%s)\n",
			node, reload, get_nodes_block(node), get_irg_dump_name(env->irg));
		env->problem_found = 1;
	}
}

static void collect_memperm(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity* ent) {
	int i, arity;
	spill_t spill, *res;
	int hash = HASH_PTR(node);
	int out;
	ir_node* memperm;
	ir_entity *spillent;

	assert(is_Proj(node));

	memperm = get_Proj_pred(node);
	out = get_Proj_proj(node);

	spillent = be_get_MemPerm_out_entity(memperm, out);
	be_check_entity(env, memperm, spillent);
	if(spillent != ent) {
		ir_fprintf(stderr, "Verify warning: MemPerm %+F has different entity than reload %+F in block %+F(%s)\n",
			node, reload, get_nodes_block(node), get_irg_dump_name(env->irg));
		env->problem_found = 1;
	}

	spill.spill = node;
	res = set_find(env->spills, &spill, sizeof(spill), hash);
	if(res != NULL) {
		return;
	}

	spill.ent = spillent;
	res = set_insert(env->spills, &spill, sizeof(spill), hash);

	for(i = 0, arity = be_get_MemPerm_entity_arity(memperm); i < arity; ++i) {
		ir_node* arg = get_irn_n(memperm, i + 1);
		ir_entity* argent = be_get_MemPerm_in_entity(memperm, i);

		collect(env, arg, memperm, argent);
	}
}

static void collect_memphi(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity *ent) {
	int i, arity;
	spill_t spill, *res;
	int hash = HASH_PTR(node);

	assert(is_Phi(node));

	spill.spill = node;
	res = set_find(env->spills, &spill, sizeof(spill), hash);
	if(res != NULL) {
		return;
	}

	spill.ent = ent;
	res = set_insert(env->spills, &spill, sizeof(spill), hash);

	/* is 1 of the arguments a spill? */
	for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
		ir_node* arg = get_irn_n(node, i);
		collect(env, arg, reload, ent);
	}
}

static void collect(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, ir_entity* ent) {
	if(be_is_Spill(node)) {
		collect_spill(env, node, reload, ent);
	} else if(is_Proj(node)) {
		collect_memperm(env, node, reload, ent);
	} else if(is_Phi(node) && get_irn_mode(node) == mode_M) {
		collect_memphi(env, node, reload, ent);
	} else {
		/* Disabled for now, spills might get transformed by the backend */
#if 0
		ir_fprintf(stderr, "Verify warning: No spill, memperm or memphi attached to node %+F found from node %+F in block %+F(%s)\n",
			node, reload, get_nodes_block(node), get_irg_dump_name(env->irg));
		env->problem_found = 1;
#endif
	}
}

/**
 * This walker function searches for reloads and collects all the spills
 * and memphis attached to them.
 */
static void collect_spills_walker(ir_node *node, void *data) {
	be_verify_spillslots_env_t *env = data;
	const arch_env_t *arch_env = env->arch_env;

	/* @@@ ia32_classify returns classification of Proj_pred :-/ */
	if(is_Proj(node))
		return;

	if(arch_irn_class_is(arch_env, node, reload)) {
		ir_node *spill = get_memory_edge(node);
		ir_entity *ent;

		if(spill == NULL) {
			ir_fprintf(stderr, "Verify warning: No spill attached to reload %+F in block %+F(%s)\n",
			           node, get_nodes_block(node), get_irg_dump_name(env->irg));
			env->problem_found = 1;
			return;
		}
		ent = arch_get_frame_entity(env->arch_env, node);
		be_check_entity(env, node, ent);

		collect(env, spill, node, ent);
		ARR_APP1(ir_node*, env->reloads, node);
	}
}

static void check_spillslot_interference(be_verify_spillslots_env_t *env) {
	int spillcount = set_count(env->spills);
	spill_t **spills = alloca(spillcount * sizeof(spills[0]));
	spill_t *spill;
	int i;

	for(spill = set_first(env->spills), i = 0; spill != NULL; spill = set_next(env->spills), ++i) {
		spills[i] = spill;
	}

	for(i = 0; i < spillcount; ++i) {
		spill_t *sp1 = spills[i];
		int i2;

		for(i2 = i+1; i2 < spillcount; ++i2) {
			spill_t *sp2 = spills[i2];

			if(sp1->ent != sp2->ent)
				continue;

			if(my_values_interfere(sp1->spill, sp2->spill)) {
				ir_fprintf(stderr, "Verify warning: Spillslots for %+F in block %+F(%s) and %+F in block %+F(%s) interfere\n",
					sp1->spill, get_nodes_block(sp1->spill), get_irg_dump_name(env->irg),
					sp2->spill, get_nodes_block(sp2->spill), get_irg_dump_name(env->irg));
				env->problem_found = 1;
				my_values_interfere(sp1->spill, sp2->spill);
			}
		}
	}
}

static void check_lonely_spills(ir_node *node, void *data) {
	be_verify_spillslots_env_t *env = data;

	if(be_is_Spill(node) || (is_Proj(node) && be_is_MemPerm(get_Proj_pred(node)))) {
		spill_t *spill = find_spill(env, node);
		if(be_is_Spill(node)) {
			ir_entity *ent = arch_get_frame_entity(env->arch_env, node);
			be_check_entity(env, node, ent);
		}

		if(spill == NULL) {
			ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) not connected to a reaload\n",
			           node, get_nodes_block(node), get_irg_dump_name(env->irg));
		}
	}
}

int be_verify_spillslots(const arch_env_t *arch_env, ir_graph *irg)
{
	be_verify_spillslots_env_t env;

	env.arch_env = arch_env;
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
static int my_values_interfere(const ir_node *a, const ir_node *b) {
	const ir_edge_t *edge;
	ir_node *bb;
	int a2b = value_dominates(a, b);
	int b2a = value_dominates(b, a);

	/* If there is no dominance relation, they do not interfere. */
	if(!a2b && !b2a)
		return 0;

	/*
	 * Adjust a and b so, that a dominates b if
	 * a dominates b or vice versa.
	 */
	if(b2a) {
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
		if(b == user)
			continue;

		if(get_irn_opcode(user) == iro_End)
			continue;

		/* in case of phi arguments we compare with the block the value comes from */
		if(is_Phi(user)) {
			ir_node *phiblock = get_nodes_block(user);
			if(phiblock == bb)
				continue;
			user = get_irn_n(phiblock, get_edge_src_pos(edge));
		}

		if(value_dominates(b, user))
			return 1;
	}

	return 0;
}



/*--------------------------------------------------------------------------- */



typedef struct _be_verify_register_allocation_env_t {
	const arch_env_t *arch_env;
	ir_graph *irg;
	be_lv_t *lv;
	int problem_found;
} be_verify_register_allocation_env_t;

static void check_register_constraints(ir_node *node,
                                       be_verify_register_allocation_env_t *env)
{
	const arch_env_t      *arch_env = env->arch_env;
	const arch_register_t *reg;
	int                   i, arity;

	/* verify output register */
	if (arch_get_irn_reg_class(arch_env, node, -1) != NULL) {
		reg = arch_get_irn_register(arch_env, node);
		if (reg == NULL) {
			ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) should have a register assigned\n",
					node, get_nodes_block(node), get_irg_dump_name(env->irg));
			env->problem_found = 1;
		}
		else if (! arch_register_type_is(reg, joker) && !arch_reg_is_allocatable(arch_env, node, -1, reg)) {
			ir_fprintf(stderr, "Verify warning: Register %s assigned as output of %+F not allowed (register constraint) in block %+F(%s)\n",
					reg->name, node, get_nodes_block(node), get_irg_dump_name(env->irg));
			env->problem_found = 1;
		}
	}

	/* verify input register */
	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);

		if (is_Unknown(pred))
			continue;

		if (is_Bad(pred)) {
			ir_fprintf(stderr, "Verify warning: %+F in block %+F(%s) has Bad as input %d\n",
				node, get_nodes_block(node), get_irg_dump_name(env->irg), i);
			env->problem_found = 1;
			continue;
		}

		if (arch_get_irn_reg_class(arch_env, node, i) == NULL)
			continue;

		reg = arch_get_irn_register(arch_env, pred);
		if (reg == NULL) {
			ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) should have a register assigned\n",
			           pred, get_nodes_block(pred), get_irg_dump_name(env->irg));
			env->problem_found = 1;
			continue;
		}
		else if (! arch_register_type_is(reg, joker) && ! arch_reg_is_allocatable(arch_env, node, i, reg)) {
			ir_fprintf(stderr, "Verify warning: Register %s as input %d of %+F not allowed (register constraint) in block %+F(%s)\n",
			           reg->name, i, node, get_nodes_block(node), get_irg_dump_name(env->irg));
			env->problem_found = 1;
		}
	}
}

static void check_register_allocation(be_verify_register_allocation_env_t *env,
                                      const arch_register_class_t *regclass,
                                      ir_nodeset_t *nodes)
{
	const arch_env_t      *arch_env  = env->arch_env;
	const arch_register_t *reg       = NULL;
	int                   fail       = 0;
	bitset_t              *registers = bitset_alloca(arch_register_class_n_regs(regclass));
	ir_node               *node;
	ir_nodeset_iterator_t  iter;

	foreach_ir_nodeset(nodes, node, iter) {
		if (arch_get_irn_reg_class(arch_env, node, -1) != regclass)
			continue;

		reg = arch_get_irn_register(arch_env, node);

		/* this problem is already reported in 'check_register_constraints' */
		if (! reg)
			continue;

		if (bitset_is_set(registers, reg->index)) {
			fail = 1;
			break;
		}
		bitset_set(registers, reg->index);
	}

	if (fail) {
		ir_fprintf(stderr, "Verify warning: Register %s assigned more than once in block %+F(%s)\n",
			       reg->name, get_nodes_block(node), get_irg_dump_name(env->irg));
		env->problem_found = 1;

		foreach_ir_nodeset(nodes, node, iter) {
			if (arch_get_irn_register(arch_env, node) == reg) {
				ir_fprintf(stderr, "  at node %+F\n", node);
			}
		}
	}
}

static void verify_block_register_allocation(ir_node *block, void *data) {
	be_verify_register_allocation_env_t *env = data;
	const arch_env_t *arch_env = env->arch_env;
	const arch_isa_t *isa = arch_env->isa;
	int i, nregclasses;

	nregclasses = arch_isa_get_n_reg_class(isa);
	for (i = 0; i < nregclasses; ++i) {
		const arch_register_class_t *regclass = arch_isa_get_reg_class(isa, i);
		ir_node *node;
		ir_nodeset_t live_nodes;

		ir_nodeset_init(&live_nodes);

		be_liveness_end_of_block(env->lv, env->arch_env, regclass, block,
		                         &live_nodes);
		check_register_allocation(env, regclass, &live_nodes);

		sched_foreach_reverse(block, node) {
			if (is_Phi(node))
				break;

			be_liveness_transfer(env->arch_env, regclass, node, &live_nodes);
			check_register_allocation(env, regclass, &live_nodes);
			check_register_constraints(node, env);
		}

		ir_nodeset_destroy(&live_nodes);
	}
}

int be_verify_register_allocation(const arch_env_t *arch_env, ir_graph *irg) {
	be_verify_register_allocation_env_t env;

	env.arch_env = arch_env;
	env.irg = irg;
	env.lv = be_liveness(irg);
	env.problem_found = 0;

	be_liveness_assure_sets(env.lv);
	irg_block_walk_graph(irg, verify_block_register_allocation, NULL, &env);

	be_liveness_free(env.lv);

	return !env.problem_found;
}



/*--------------------------------------------------------------------------- */



typedef struct _verify_out_dead_nodes_env {
	ir_graph *irg;
	bitset_t *reachable;
	int problem_found;
} verify_out_dead_nodes_env;

static void check_out_edges(ir_node *node, verify_out_dead_nodes_env *env) {
	ir_graph *irg = env->irg;
	const ir_edge_t* edge;

	if(irn_visited(node))
		return;
	mark_irn_visited(node);

	foreach_out_edge(node, edge) {
		ir_node* src = get_edge_src_irn(edge);

		if(!bitset_is_set(env->reachable, get_irn_idx(src)) && !is_Block(node)) {
			ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) only reachable through out edges from %+F\n",
			           src, get_nodes_block(src), get_irg_dump_name(irg), node);
			env->problem_found = 1;
			continue;
		}

		check_out_edges(src, env);
	}
}

static void set_reachable(ir_node *node, void* data)
{
	bitset_t* reachable = data;
	bitset_set(reachable, get_irn_idx(node));
}

int be_verify_out_edges(ir_graph *irg) {
	verify_out_dead_nodes_env env;

	env.irg           = irg;
	env.reachable     = bitset_alloca(get_irg_last_idx(irg));
	env.problem_found = edges_verify(irg);

	irg_walk_in_or_dep_graph(irg, set_reachable, NULL, env.reachable);
	irg_walk_anchors(irg, set_reachable, NULL, env.reachable);
	inc_irg_visited(irg);
	check_out_edges(get_irg_start(irg), &env);

	return ! env.problem_found;
}
