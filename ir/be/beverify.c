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
#include "besched_t.h"

#include "irnode.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irdump_t.h"
#include "iredges.h"
#include "set.h"
#include "array.h"
#include "benode_t.h"

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
	be_liveness_end_of_block(env->lv, env->arch_env, env->cls, block, live_nodes);

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

	env.lv                  = be_liveness(irg);
	env.irg                 = irg;
	env.arch_env            = arch_env;
	env.cls                 = cls;
	env.registers_available = arch_count_non_ignore_regs(arch_env, cls);
	env.problem_found       = 0;

	irg_block_walk_graph(irg, verify_liveness_walker, NULL, &env);
	be_liveness_free(env.lv);

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
			// proj and keepany aren't real instructions...
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

		// 3. Check for uses
		if(pset_find_ptr(uses, node)) {
			ir_fprintf(stderr, "Verify Warning: Value %+F used before it was defined in block %+F (%s)\n",
				node, block, get_irg_dump_name(env->irg));
			env->problem_found = 1;
		}
		if(!is_Phi(node)) {
			for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
				pset_insert_ptr(uses, get_irn_n(node, i));
			}
		}
	}
	del_pset(uses);

	/* check that all delay branches are filled (at least with NOPs) */
	if (cfchange_found && delay_branches != 0) {
		ir_fprintf(stderr, "Verify warning: Not all delay slots filled after jump (%d/%d) in block %+F (%s)\n",
			block, get_irg_dump_name(env->irg));
		env->problem_found = 1;
	}
}

static int should_be_scheduled(ir_node *node) {
	if(is_Block(node))
		return -1;

	if(get_irn_mode(node) == mode_M) {
		if(is_Proj(node))
			return -1;
		if(is_Phi(node) || is_Sync(node) || get_irn_opcode(node) == iro_Pin)
			return 0;
	}
	if(is_Proj(node) && get_irn_mode(node) == mode_X)
		return 0;
	if(be_is_Keep(node) && get_irn_opcode(get_nodes_block(node)) == iro_Bad)
		return 0;

	switch(get_irn_opcode(node)) {
	case iro_End:
	case iro_NoMem:
	case iro_Bad:
		return 0;
	default:
		break;
	}

	return 1;
}

static void check_schedule(ir_node *node, void *data) {
	be_verify_schedule_env_t *env = data;
	int should_be;

	should_be = should_be_scheduled(node);
	if(should_be == -1)
		return;

	if(should_be ? !sched_is_scheduled(node) : sched_is_scheduled(node)) {
		ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) should%s be scheduled\n",
			node, get_nodes_block(node), get_irg_dump_name(env->irg), should_be ? "" : " not");
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
	// check if all nodes are scheduled
	irg_walk_graph(irg, check_schedule, NULL, &env);

	return ! env.problem_found;
}



//---------------------------------------------------------------------------



typedef struct _spill_t {
	ir_node *spill;
	entity *ent;
} spill_t;

typedef struct {
	be_lv_t *lv;
	ir_graph *irg;
	set *spills;
	ir_node **reloads;
	int problem_found;
} be_verify_spillslots_env_t;

static int cmp_spill(const void* d1, const void* d2, size_t size) {
	const spill_t* s1 = d1;
	const spill_t* s2 = d2;
	return s1->spill != s2->spill;
}

static spill_t *find_spill(be_verify_spillslots_env_t *env, ir_node *node) {
	spill_t spill;

	spill.spill = node;
	return set_find(env->spills, &spill, sizeof(spill), HASH_PTR(node));
}

static spill_t *get_spill(be_verify_spillslots_env_t *env, ir_node *node, entity *ent) {
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

static void collect(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, entity* ent);

static void check_entity(be_verify_spillslots_env_t *env, ir_node *node, entity *ent) {
	if(ent == NULL) {
		ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) should have an entity assigned\n",
		           node, get_nodes_block(node), get_irg_dump_name(env->irg));
	}
}

static void collect_spill(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, entity* ent) {
	entity *spillent = be_get_frame_entity(node);
	check_entity(env, node, spillent);
	get_spill(env, node, ent);

	if(spillent != ent) {
		ir_fprintf(stderr, "Verify warning: Spill %+F has different entity than reload %+F in block %+F(%s)\n",
			node, reload, get_nodes_block(node), get_irg_dump_name(env->irg));
		env->problem_found = 1;
	}
}

static void collect_memperm(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, entity* ent) {
	int i, arity;
	spill_t spill, *res;
	int hash = HASH_PTR(node);
	int out;
	ir_node* memperm;
	entity *spillent;

	assert(is_Proj(node));

	memperm = get_Proj_pred(node);
	out = get_Proj_proj(node);

	spillent = be_get_MemPerm_out_entity(memperm, out);
	check_entity(env, memperm, spillent);
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
		entity* argent = be_get_MemPerm_in_entity(memperm, i);

		collect(env, arg, memperm, argent);
	}
}

static void collect_memphi(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, entity *ent) {
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

	// is 1 of the arguments a spill?
	for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
		ir_node* arg = get_irn_n(node, i);
		collect(env, arg, reload, ent);
	}
}

static void collect(be_verify_spillslots_env_t *env, ir_node *node, ir_node *reload, entity* ent) {
	if(be_is_Spill(node)) {
		collect_spill(env, node, reload, ent);
	} else if(is_Proj(node)) {
		collect_memperm(env, node, reload, ent);
	} else if(is_Phi(node) && get_irn_mode(node) == mode_M) {
		collect_memphi(env, node, reload, ent);
	} else {
		ir_fprintf(stderr, "Verify warning: No spill, memperm or memphi attached to node %+F found from node %+F in block %+F(%s)\n",
			node, reload, get_nodes_block(node), get_irg_dump_name(env->irg));
		env->problem_found = 1;
	}
}

/**
 * This walker function searches for reloads and collects all the spills
 * and memphis attached to them.
 */
static void collect_spills_walker(ir_node *node, void *data) {
	be_verify_spillslots_env_t *env = data;

	if(be_is_Reload(node)) {
		ir_node *spill = get_irn_n(node, be_pos_Reload_mem);
		entity* ent = be_get_frame_entity(node);
		check_entity(env, node, ent);

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
				printf("Intf: %d\n", values_interfere(env->lv, sp1->spill, sp2->spill));
			}
		}
	}
}

static void check_lonely_spills(ir_node *node, void *data) {
	be_verify_spillslots_env_t *env = data;

	if(be_is_Spill(node) || (is_Proj(node) && be_is_MemPerm(get_Proj_pred(node)))) {
		spill_t *spill = find_spill(env, node);
		if(be_is_Spill(node)) {
			entity *ent = be_get_frame_entity(node);
			check_entity(env, node, ent);
		}

		if(spill == NULL) {
			ir_fprintf(stderr, "Verify warning: Node %+F in block %+F(%s) not connected to a reaload\n",
			           node, get_nodes_block(node), get_irg_dump_name(env->irg));
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
	env.lv = be_liveness(irg);

	irg_walk_graph(irg, collect_spills_walker, NULL, &env);
	irg_walk_graph(irg, check_lonely_spills, NULL, &env);

	check_spillslot_interference(&env);

	be_liveness_free(env.lv);
	DEL_ARR_F(env.reloads);
	del_set(env.spills);

	return ! env.problem_found;
}



//---------------------------------------------------------------------------



/**
 * Check, if two values interfere.
 * @param a The first value.
 * @param b The second value.
 * @return 1, if a and b interfere, 0 if not.
 */
static int my_values_interfere(const ir_node *a, const ir_node *b)
{
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

		// in case of phi arguments we compare with the block the value comes from
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
