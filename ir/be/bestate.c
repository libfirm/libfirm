/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Handles state switching. This is basically the belady spill
 *              algorithm optimized for the 1-register case.
 * @author      Matthias Braun
 * @date        26.03.2007
 */
#include "bestate.h"

#include "obst.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irgwalk.h"
#include "irloop.h"
#include "iredges_t.h"
#include "ircons_t.h"
#include "irgmod.h"
#include "irnodehashmap.h"
#include "cpset.h"

#include "bearch.h"
#include "beirg.h"
#include "beuses.h"
#include "besched.h"
#include "belive_t.h"
#include "bemodule.h"
#include "benode.h"
#include "beirgmod.h"
#include "bespillutil.h"
#include "bessaconstr.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct spill_info_t {
	struct spill_info_t *next;
	ir_node *value;
	ir_node *spill;
	ir_node **reloads;
} spill_info_t;

typedef struct minibelady_env_t {
	struct obstack         obst;
	const arch_register_t *reg;
	const be_lv_t         *lv;
	void                  *func_env;
	create_reload_func     create_reload;
	create_spill_func      create_spill;
	spill_info_t          *spills;
	ir_nodehashmap_t       spill_infos;

	be_uses_t             *uses;           /**< env for the next-use magic */
} minibelady_env_t;

typedef struct block_info_t {
	ir_node *start_state;
	ir_node *end_state;
} block_info_t;

static inline block_info_t *new_block_info(struct obstack *obst, ir_node *block)
{
	block_info_t *res = OALLOCZ(obst, block_info_t);

	assert(is_Block(block));
	set_irn_link(block, res);
	mark_irn_visited(block);

	return res;
}

static inline block_info_t *get_block_info(ir_node *block)
{
	assert(irn_visited(block));
	return (block_info_t*) get_irn_link(block);
}

static inline spill_info_t *create_spill_info(minibelady_env_t *env, ir_node *state)
{
	spill_info_t *spill_info = OALLOCZ(&env->obst, spill_info_t);
	spill_info->value = state;
	spill_info->reloads = NEW_ARR_F(ir_node*, 0);

	ir_nodehashmap_insert(&env->spill_infos, state, spill_info);
	//ir_fprintf(stderr, "Insert %+F -> %p\n", state, spill_info);

	spill_info->next = env->spills;
	env->spills = spill_info;

	return spill_info;
}

static inline spill_info_t *get_spill_info(minibelady_env_t *env, const ir_node *node)
{
	spill_info_t *spill_info = ir_nodehashmap_get(spill_info_t, &env->spill_infos, node);
	//ir_fprintf(stderr, "Get %+F -> %p\n", node, spill_info);
	return spill_info;
}

static spill_info_t *create_spill(minibelady_env_t *env, ir_node *state, int force)
{
	spill_info_t *spill_info;
	ir_node *next;
	ir_node *after;

	spill_info = get_spill_info(env, state);
	if (spill_info == NULL) {
		spill_info = create_spill_info(env, state);
	} else if (spill_info->spill != NULL) {
		return spill_info;
	}

	if (sched_is_scheduled(state)) {
		next = state;
		do {
			after = next;
			next = sched_next(after);
		} while (is_Phi(next) || be_is_Keep(next));
	} else {
		after = state;
	}
	spill_info->spill = env->create_spill(env->func_env, state, force, after);

	return spill_info;
}

static void create_reload(minibelady_env_t *env, ir_node *state,
                          ir_node *before, ir_node *last_state)
{
	spill_info_t *spill_info = create_spill(env, state, 0);
	ir_node *spill = spill_info->spill;
	ir_node *reload;

	reload = env->create_reload(env->func_env, state, spill, before,
	                            last_state);
	ARR_APP1(ir_node*, spill_info->reloads, reload);
}

static void spill_phi(minibelady_env_t *env, ir_node *phi)
{
	ir_graph     *irg           = get_irn_irg(phi);
	ir_node      *block         = get_nodes_block(phi);
	int           arity         = get_irn_arity(phi);
	ir_node     **phi_in        = ALLOCAN(ir_node*, arity);
	ir_node      *dummy         = new_r_Dummy(irg, mode_M);
	ir_node      *spill_to_kill = NULL;
	spill_info_t *spill_info;
	int           i;

	/* does a spill exist for the phis value? */
	spill_info = get_spill_info(env, phi);
	if (spill_info != NULL) {
		spill_to_kill = spill_info->spill;
	} else {
		spill_info = create_spill_info(env, phi);
	}

	/* create a new phi-M with bad preds */
	for (i = 0; i < arity; ++i) {
		phi_in[i] = dummy;
	}

	DBG((dbg, LEVEL_2, "\tcreate Phi-M for %+F\n", phi));

	/* create a Phi-M */
	spill_info->spill = be_new_Phi(block, arity, phi_in, mode_M,
	                               arch_no_register_req);
	sched_add_after(block, spill_info->spill);

	if (spill_to_kill != NULL) {
		exchange(spill_to_kill, spill_info->spill);
		sched_remove(spill_to_kill);
	}

	/* create spills for the phi values */
	for (i = 0; i < arity; ++i) {
		ir_node *in = get_irn_n(phi, i);
		spill_info_t *pred_spill = create_spill(env, in, 1);
		set_irn_n(spill_info->spill, i, pred_spill->spill);
	}
}

static void belady(minibelady_env_t *env, ir_node *block);

/**
 * Collects all values live-in at block @p block and all phi results in this
 * block.
 * Then it adds the best values (at most n_regs) to the blocks start_workset.
 * The phis among the remaining values get spilled: Introduce pseudo-copies of
 * their args to break interference and make it possible to spill them to the
 * same spill slot.
 */
static block_info_t *compute_block_start_state(minibelady_env_t *env, ir_node *block)
{
	block_info_t  *block_info;
	be_next_use_t  next_use;
	ir_loop       *loop;
	ir_node       *best_starter, *first;
	int            n_cfgpreds;
	unsigned       best_time;
	int            outer_loop_allowed;

	/* Create the block info for this block. */
	block_info = new_block_info(&env->obst, block);
	n_cfgpreds = get_Block_n_cfgpreds(block);

	/* no cfgpred -> no value active */
	if (n_cfgpreds == 0) {
		block_info->start_state = NULL;
		return block_info;
	}

	/* for 1 pred only: simply take the the end-state of the pred */
	if (n_cfgpreds == 1) {
		ir_node *pred_block = get_Block_cfgpred_block(block, 0);
		block_info_t *pred_info;

		/* process pred block */
		belady(env, pred_block);

		pred_info = get_block_info(pred_block);

		DBG((dbg, LEVEL_2, "Taking end state from %+F: %+F\n", pred_block, pred_info->end_state));
		block_info->start_state = pred_info->end_state;
		return block_info;
	}

	/* Collect all values living at start of block */
	DBG((dbg, LEVEL_2, "Living at start of %+F:\n", block));
	first = sched_first(block);
	loop = get_irn_loop(block);
	best_starter = NULL;
	best_time = USES_INFINITY;
	outer_loop_allowed = 1;

	/* check all Phis first */
	sched_foreach(block, node) {
		if (!is_Phi(node))
			break;
		if (arch_get_irn_register(node) != env->reg)
			continue;

		DBG((dbg, LEVEL_2, "\t...checking %+F\n", node));
		next_use = be_get_next_use(env->uses, first, node, 0);

		if (USES_IS_INFINITE(next_use.time)) {
			DBG((dbg, LEVEL_2, "\tnot taken (dead)\n"));
			continue;
		}

		if (next_use.outermost_loop >= get_loop_depth(loop)) {
			if (outer_loop_allowed || next_use.time < best_time) {
				DBG((dbg, LEVEL_2, "\ttaken (%u, loop %d)\n", next_use.time,
				     next_use.outermost_loop));

				if (best_starter != NULL) {
					/* spill the phi as it is not used */
					spill_phi(env, best_starter);
				}
				best_starter = node;
				best_time = next_use.time;
				outer_loop_allowed = 0;
			}
		} else {
			if (outer_loop_allowed && next_use.time < best_time) {
				DBG((dbg, LEVEL_2, "\ttaken (%u, loop %d)\n", next_use.time,
				     next_use.outermost_loop));
				if (best_starter != NULL) {
					/* spill the phi as it is not used */
					spill_phi(env, best_starter);
				}
				best_starter = node;
				best_time = next_use.time;
			}
		}

		if (best_starter != node) {
			/* spill the phi as it is not used */
			spill_phi(env, best_starter);
		}
	}

	/* check all Live-Ins */
	be_lv_foreach_cls(env->lv, block, be_lv_state_in, env->reg->reg_class, node) {
		if (arch_get_irn_register(node) != env->reg)
			continue;

		DBG((dbg, LEVEL_2, "\t...checking %+F\n", node));
		next_use = be_get_next_use(env->uses, first, node, 0);

		if (USES_IS_INFINITE(next_use.time)) {
			DBG((dbg, LEVEL_2, "\tnot taken (dead)\n"));
			continue;
		}

		if (next_use.outermost_loop >= get_loop_depth(loop)) {
			if (outer_loop_allowed || next_use.time < best_time) {
				DBG((dbg, LEVEL_2, "\ttaken (%u, loop %d)\n", next_use.time,
				     next_use.outermost_loop));

				if (best_starter != NULL && is_Phi(best_starter)) {
					/* spill the phi as it is not used */
					spill_phi(env, best_starter);
				}
				best_starter = node;
				best_time = next_use.time;
				outer_loop_allowed = 0;
			}
		} else {
			if (outer_loop_allowed && next_use.time < best_time) {
				DBG((dbg, LEVEL_2, "\ttaken (%u, loop %d)\n", next_use.time,
				     next_use.outermost_loop));
				if (best_starter != NULL && is_Phi(best_starter)) {
					/* spill the phi as it is not used */
					spill_phi(env, best_starter);
				}
				best_starter = node;
				best_time = next_use.time;
			}
		}
	}

	block_info->start_state = best_starter;

	return block_info;
}

/**
 * For the given block @p block, decide for each values
 * whether it is used from a register or is reloaded
 * before the use.
 */
static void belady(minibelady_env_t *env, ir_node *block)
{
	ir_node *current_state;
	block_info_t *block_info;

	/* Don't do a block twice */
	if (irn_visited(block))
		return;

	/* compute value to start with */
	block_info = compute_block_start_state(env, block);

	/* get the starting workset for this block */
	DBG((dbg, LEVEL_3, "\n"));
	DBG((dbg, LEVEL_3, "Decide for %+F\n", block));

	current_state = block_info->start_state;
	DBG((dbg, LEVEL_3, "Start value: %+F\n", current_state));

	/* process the block from start to end */
	DBG((dbg, LEVEL_3, "Processing...\n"));

	sched_foreach(block, node) {
		int i, arity;
		ir_node *need_val = NULL;

		/* Phis are no real instr (see insert_starters()) */
		if (is_Phi(node))
			continue;

		/* check which state is desired for the node */
		arity = get_irn_arity(node);
		for (i = 0; i < arity; ++i) {
			const arch_register_t *reg;
			ir_node *in = get_irn_n(node, i);

			if (!mode_is_data(get_irn_mode(in)))
				continue;

			reg = arch_get_irn_register(in);
			if (reg == env->reg) {
				assert(need_val == NULL);
				need_val = in;
				DBG((dbg, LEVEL_3, "\t... need state %+F\n", need_val));
			}
		}
		/* create a reload to match state if necessary */
		if (need_val != NULL && need_val != current_state) {
			ir_node *before = node;
			DBG((dbg, LEVEL_3, "\t... reloading %+F\n", need_val));
			create_reload(env, need_val, before, current_state);
			current_state = need_val;
		}

		DBG((dbg, LEVEL_3, "  ...%+F\n", node));

		/* record state changes by the node */
		be_foreach_value(node, value,
			if (!mode_is_data(get_irn_mode(value)))
				continue;
			arch_register_t const *const reg = arch_get_irn_register(value);
			if (reg != env->reg)
				continue;
			current_state = value;
			DBG((dbg, LEVEL_3, "\t... current_state <- %+F\n", current_state));
		);
	}

	/* Remember end-workset for this block */
	block_info->end_state = current_state;
	DBG((dbg, LEVEL_3, "End value for %+F: %+F\n", block, current_state));
}

static void belady_walker(ir_node *block, void *data)
{
	belady((minibelady_env_t*) data, block);
}

/**
 * We must adapt the live-outs to the live-ins at each block-border.
 */
static void fix_block_borders(ir_node *block, void *data)
{
	minibelady_env_t *env = (minibelady_env_t*)data;
	int i;
	int arity;
	block_info_t *block_info;

	DBG((dbg, LEVEL_3, "\n"));

	block_info = get_block_info(block);

	DBG((dbg, LEVEL_3, "Fixing %+F (needs %+F)\n", block,
	     block_info->start_state));

	/* process all pred blocks */
	arity = get_irn_arity(block);
	for (i = 0; i < arity; ++i) {
		ir_node      *pred       = get_Block_cfgpred_block(block, i);
		block_info_t *pred_info  = get_block_info(pred);
		ir_node      *need_state = block_info->start_state;

		if (need_state == NULL)
			continue;

		if (is_Phi(need_state) && get_nodes_block(need_state) == block) {
			need_state = get_irn_n(need_state, i);
		}

		DBG((dbg, LEVEL_3, "  Pred %+F (ends in %+F, we need %+F)\n", pred,
		     pred_info->end_state, need_state));

		if (pred_info->end_state != need_state) {
			DBG((dbg, LEVEL_3, "  Creating reload for %+F\n", need_state));
			ir_node *const insert_point = be_get_end_of_block_insertion_point(pred);
			create_reload(env, need_state, insert_point, pred_info->end_state);
		}
	}
}

void be_assure_state(ir_graph *irg, const arch_register_t *reg, void *func_env,
                     create_spill_func create_spill,
                     create_reload_func create_reload)
{
	minibelady_env_t env;
	spill_info_t *info;
	be_lv_t *lv = be_get_irg_liveness(irg);

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO
	                         | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	be_assure_live_sets(irg);

	obstack_init(&env.obst);
	env.reg           = reg;
	env.func_env      = func_env;
	env.create_spill  = create_spill;
	env.create_reload = create_reload;
	env.lv            = be_get_irg_liveness(irg);
	env.uses          = be_begin_uses(irg, env.lv);
	env.spills        = NULL;
	ir_nodehashmap_init(&env.spill_infos);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED | IR_RESOURCE_IRN_LINK);
	inc_irg_visited(irg);

	/* process blocks */
	irg_block_walk_graph(irg, NULL, belady_walker, &env);

	/* fix block end_states that don't match the next blocks start_state */
	irg_block_walk_graph(irg, fix_block_borders, NULL, &env);

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED | IR_RESOURCE_IRN_LINK);

	/* reconstruct ssa-form */
	info = env.spills;
	while (info != NULL) {
		be_ssa_construction_env_t senv;
		size_t i, len;
		ir_node **phis;

		be_ssa_construction_init(&senv, irg);
		if (sched_is_scheduled(info->value))
			be_ssa_construction_add_copy(&senv, info->value);
		be_ssa_construction_add_copies(&senv,
		                               info->reloads, ARR_LEN(info->reloads));
		be_ssa_construction_fix_users(&senv, info->value);

		if (lv != NULL) {
			be_ssa_construction_update_liveness_phis(&senv, lv);

			be_liveness_update(lv, info->value);
			len = ARR_LEN(info->reloads);
			for (i = 0; i < len; ++i) {
				ir_node *reload = info->reloads[i];
				be_liveness_update(lv, reload);
			}
		}

		phis = be_ssa_construction_get_new_phis(&senv);

		/* set register requirements for phis */
		len = ARR_LEN(phis);
		for (i = 0; i < len; ++i) {
			ir_node *phi = phis[i];
			arch_set_irn_register(phi, env.reg);
		}
		be_ssa_construction_destroy(&senv);

		info = info->next;
	}

	/* some nodes might be dead now. */
	be_remove_dead_nodes_from_schedule(irg);

	ir_nodehashmap_destroy(&env.spill_infos);
	be_end_uses(env.uses);
	obstack_free(&env.obst, NULL);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_state)
void be_init_state(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.state");
}
