/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       SSA construction for a set of nodes
 * @author      Sebastian Hack, Daniel Grund, Matthias Braun, Christian Wuerdig,
 *              Sebastian Buchwald
 * @date        04.05.2005
 *
 * The problem: Given a value and a set of "copies" that are known to
 * represent the same abstract value, rewire all usages of the original value
 * to their closest copy while introducing phis as necessary.
 *
 * Algorithm: Mark all blocks in the iterated dominance frontiers of the value
 * and its copies. Link the copies ordered by dominance to the blocks.  Then
 * we search for each use all definitions in the current block, if none is
 * found, then we search one in the immediate dominator. If we are in a block
 * of the dominance frontier, create a phi and do the same search for all
 * phi arguments.
 *
 * A copy in this context means, that you want to introduce several new
 * abstract values (in Firm: nodes) for which you know, that they
 * represent the same concrete value. This is the case if you
 * - copy
 * - spill and reload
 * - re-materialize
 * a value.
 *
 * This function reroutes all uses of the original value to the copies in the
 * corresponding dominance subtrees and creates Phi functions where necessary.
 */
/* statev in this file is extensive, so only enable if needed */
#define DISABLE_STATEV

#include "bessaconstr.h"
#include "bemodule.h"
#include "besched.h"
#include "belive.h"
#include "beirg.h"
#include "be_t.h"
#include "benode.h"

#include "debug.h"
#include "array.h"
#include "irdom.h"
#include "ircons.h"
#include "iredges_t.h"
#include "statev_t.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static ir_node *search_def_end_of_block(be_ssa_construction_env_t *env,
                                        ir_node *block);

struct constr_info {
	bool is_definition     : 1;
	bool is_use            : 1;
	bool already_processed : 1;
	union {
		/** Since we only consider scheduled nodes,
		 * this points to the real definition (e.g. a Proj). */
		ir_node *definition;

		/** Last definition of a block. */
		ir_node *last_definition;
	} u;
};

typedef struct constr_info constr_info;

/**
 * @return Whether the block contains a definition.
 */
static bool has_definition(const ir_node *block)
{
	return irn_visited(block);
}

static constr_info *get_or_set_info(be_ssa_construction_env_t *env,
                                    const ir_node *node)
{
	constr_info *info = ir_nodemap_get(constr_info, &env->infos, node);
	if (info == NULL) {
		info = OALLOCZ(&env->obst, constr_info);
		ir_nodemap_insert(&env->infos, node, info);
	}
	return info;
}

static constr_info *get_info(const be_ssa_construction_env_t *env,
                             const ir_node *node)
{
	return ir_nodemap_get(constr_info, &env->infos, node);
}

/**
 * @return Whether the block contains a use.
 */
static inline bool has_use(be_ssa_construction_env_t *env, ir_node *block)
{
	constr_info *info = get_or_set_info(env, block);
	return info->is_use;
}

/**
 * @return Whether the node is a definition.
 */
static bool is_definition(be_ssa_construction_env_t *env, ir_node *node)
{
	constr_info *info = get_info(env, node);
	return info != NULL && info->is_definition;
}

/**
 * Introduces a definition at the corresponding block.
 */
static void introduce_definition(be_ssa_construction_env_t *env, ir_node *def)
{
	assert(env->phi_req->cls == arch_get_irn_register_req(def)->cls);

	ir_node     *block      = get_nodes_block(def);
	constr_info *def_info   = get_or_set_info(env, def);
	ir_node     *skip       = skip_Proj(def);
	constr_info *skip_info  = get_or_set_info(env, skip);
	constr_info *block_info = get_or_set_info(env, block);

	DBG((dbg, LEVEL_2, "\tintroducing definition %+F in %+F\n", def, block));

	def_info->is_definition = true;

	skip_info->is_definition = true;
	skip_info->u.definition  = def;

	// Set the last definition if we only introduce one definition for the block
	if (has_definition(block)) {
		assert(!block_info->already_processed);
		block_info->u.last_definition = NULL;
	} else {
		mark_irn_visited(block);
		block_info->u.last_definition = def;
	}
}

static void introduce_use(be_ssa_construction_env_t *env, ir_node *use)
{
	ir_node     *block      = get_nodes_block(use);
	constr_info *info       = get_or_set_info(env, use);
	constr_info *block_info = get_or_set_info(env, block);

	DBG((dbg, LEVEL_2, "\tintroducing use %+F in %+F\n", use, block));

	info->is_use       = true;
	block_info->is_use = true;

	deq_push_pointer_right(&env->worklist, use);
}

/**
 * Calculates the iterated dominance frontier of a set of blocks. Marks the
 * blocks as visited.
 */
static void mark_iterated_dominance_frontiers(be_ssa_construction_env_t *env)
{
	stat_ev_cnt_decl(blocks);
	DBG((dbg, LEVEL_3, "Dominance Frontier:"));
	stat_ev_tim_push();
	while (!deq_empty(&env->worklist)) {
		ir_node  *block    = deq_pop_pointer_left(ir_node, &env->worklist);
		ir_node **domfront = ir_get_dominance_frontier(block);
		for (size_t i = 0, len = ARR_LEN(domfront); i < len; ++i) {
			ir_node *y = domfront[i];
			if (Block_block_visited(y))
				continue;

			if (!irn_visited(y))
				deq_push_pointer_right(&env->worklist, y);

			DBG((dbg, LEVEL_3, " %+F", y));
			mark_Block_block_visited(y);
			stat_ev_cnt_inc(blocks);
		}
	}
	stat_ev_tim_pop("bessaconstr_idf_time");
	stat_ev_cnt_done(blocks, "bessaconstr_idf_blocks");
	DBG((dbg, LEVEL_3, "\n"));
}

/**
 * Inserts a new phi at the given block.
 *
 * The constructed phi has no operands,
 * but can be used as definition for other nodes.
 *
 * @see fix_phi_arguments
 */
static ir_node *insert_dummy_phi(be_ssa_construction_env_t *env, ir_node *block)
{
	DBG((dbg, LEVEL_3, "\t...create phi at block %+F\n", block));

	ir_node *const phi = be_new_Phi0(block, env->phi_req);
	sched_add_after(block, phi);
	ARR_APP1(ir_node*, env->new_phis, phi);

	DBG((dbg, LEVEL_2, "\tcreating phi %+F in %+F\n", phi, block));
	introduce_definition(env, phi);

	deq_push_pointer_right(&env->worklist, phi);
	return phi;
}

/**
 * @return Last definition of the immediate dominator.
 */
static ir_node *get_def_at_idom(be_ssa_construction_env_t *env, ir_node *block)
{
	ir_node *dom = get_Block_idom(block);
	assert(dom != NULL);
	DBG((dbg, LEVEL_3, "\t...continue at idom %+F\n", dom));
	return search_def_end_of_block(env, dom);
}

static ir_node *get_def_from_preds(be_ssa_construction_env_t *const env,
                                   ir_node *const block)
{
	/* Create a phi if the block is in the dominance frontier. */
	if (Block_block_visited(block)) {
		return insert_dummy_phi(env, block);
	} else {
		return get_def_at_idom(env, block);
	}
}

/**
 * Fixes all operands of a use.
 *
 * If an operand of the use is a (original) definition,
 * it will be replaced by the given definition.
 */
static void set_operands(be_ssa_construction_env_t *env, ir_node *use,
                         ir_node *def, constr_info *const use_info)
{
	foreach_irn_in(use, i, op) {
		if (is_definition(env, op)) {
			DBG((dbg, LEVEL_1, "\t...%+F(%d) -> %+F\n", use, i, def));
			set_irn_n(use, i, def);
		}
	}

	use_info->already_processed = true;
}

/**
 * Fixes all uses of the given block.
 */
static void process_block(be_ssa_construction_env_t *env, ir_node *block)
{
	ir_node     *def        = NULL;
	constr_info *block_info = get_or_set_info(env, block);

	assert(has_definition(block));
	assert(!block_info->already_processed && "Block already processed");

	DBG((dbg, LEVEL_3, "\tprocessing block  %+F\n", block));

	sched_foreach(block, node) {
		constr_info *const info = get_info(env, node);
		if (info == NULL)
			continue;

		if (info->is_use && !is_Phi(node)) {
			DBG((dbg, LEVEL_3, "\t...found use %+F\n", node));

			/* Create a phi if the block is in the dominance frontier. */
			if (def == NULL)
				def = get_def_from_preds(env, block);

			set_operands(env, node, def, info);
		}

		if (info->is_definition) {
			def = info->u.definition;
			DBG((dbg, LEVEL_3, "\t...found definition %+F\n", def));
		}
	}

	block_info->already_processed = true;
	block_info->u.last_definition = def;
}

/**
 * @return Last definition of the given block.
 */
static ir_node *search_def_end_of_block(be_ssa_construction_env_t *env,
                                        ir_node *block)
{
	constr_info *block_info      = get_or_set_info(env, block);
	ir_node     *last_definition = block_info->u.last_definition;

	if (last_definition != NULL)
		return last_definition;

	if (has_definition(block)) {
		if (has_use(env, block)) {
			if (!block_info->already_processed)
				process_block(env, block);
		} else {
			/* Search the last definition of the block. */
			sched_foreach_reverse(block, def) {
				constr_info const *const info = get_info(env, def);
				if (info && info->is_definition) {
					DBG((dbg, LEVEL_3, "\t...found definition %+F\n",
					     info->u.definition));
					block_info->u.last_definition = info->u.definition;
					break;
				}
			}

			assert(block_info->u.last_definition && "No definition found");
		}

		return block_info->u.last_definition;
	} else {
		ir_node *const def = get_def_from_preds(env, block);
		block_info->u.last_definition = def;
		return def;
	}
}

/**
 * Fixes all operands of the given use.
 */
static void search_def_at_block(be_ssa_construction_env_t *const env,
                                ir_node *const use, constr_info *const info)
{
	ir_node     *block      = get_nodes_block(use);
	constr_info *block_info = get_or_set_info(env, block);

	if (block_info->already_processed)
		return;

	if (has_definition(block)) {
		process_block(env, block);
	} else {
		ir_node *const def = get_def_from_preds(env, block);
		set_operands(env, use, def, info);
	}
}

void be_ssa_construction_init(be_ssa_construction_env_t *env, ir_graph *irg)
{
	stat_ev_ctx_push_fmt("bessaconstr", "%+F", irg);
	stat_ev_tim_push();

	stat_ev_dbl("bessaconstr_n_blocks",
	            get_Block_dom_max_subtree_pre_num(get_irg_start_block(irg)));

	memset(env, 0, sizeof(env[0]));
	env->irg       = irg;
	env->new_phis  = NEW_ARR_F(ir_node*, 0);
	deq_init(&env->worklist);
	ir_nodemap_init(&env->infos, irg);
	obstack_init(&env->obst);

	assure_irg_properties(env->irg,
	                      IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE_FRONTIERS);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED
	                        | IR_RESOURCE_BLOCK_VISITED | IR_RESOURCE_IRN_LINK);

	/* we use the visited flag to indicate blocks in the dominance frontier
	 * and blocks that already have the relevant value at the end calculated */
	inc_irg_visited(irg);
	/* We use the block visited flag to indicate blocks in the dominance
	 * frontier of some values (and this potentially needing phis) */
	inc_irg_block_visited(irg);
}

void be_ssa_construction_destroy(be_ssa_construction_env_t *env)
{
	stat_ev_int("bessaconstr_phis", ARR_LEN(env->new_phis));
	obstack_free(&env->obst, NULL);
	ir_nodemap_destroy(&env->infos);
	deq_free(&env->worklist);
	DEL_ARR_F(env->new_phis);

	ir_free_resources(env->irg, IR_RESOURCE_IRN_VISITED
	                          | IR_RESOURCE_BLOCK_VISITED | IR_RESOURCE_IRN_LINK);

	stat_ev_tim_pop("bessaconstr_total_time");
	stat_ev_ctx_pop("bessaconstr");
}

static void determine_phi_req(be_ssa_construction_env_t *env, ir_node *value)
{
	if (env->phi_req)
		return;

	const arch_register_req_t *req = arch_get_irn_register_req(value);
	if (req->width == 1) {
		env->phi_req = req->cls->class_req;
	} else {
		/* construct a new register req... */
		ir_graph *const irg = get_irn_irg(value);
		env->phi_req = be_create_cls_req(irg, req->cls, req->width);
	}
}

void be_ssa_construction_add_copy(be_ssa_construction_env_t *env,
                                  ir_node *copy)
{
	assert(!env->iterated_domfront_calculated);
	determine_phi_req(env, copy);

	ir_node *block = get_nodes_block(copy);
	if (!has_definition(block))
		deq_push_pointer_right(&env->worklist, block);
	introduce_definition(env, copy);
}

void be_ssa_construction_add_copies(be_ssa_construction_env_t *env,
                                    ir_node **copies, size_t copies_len)
{
	assert(!env->iterated_domfront_calculated);
	determine_phi_req(env, copies[0]);

	for (size_t i = 0; i < copies_len; ++i) {
		ir_node *copy  = copies[i];
		ir_node *block = get_nodes_block(copy);

		if (!has_definition(block)) {
			deq_push_pointer_right(&env->worklist, block);
		}
		introduce_definition(env, copy);
	}
}

ir_node **be_ssa_construction_get_new_phis(be_ssa_construction_env_t *env)
{
	return env->new_phis;
}

/**
 * Fixes all arguments of a newly constructed phi.
 *
 * @see insert_dummy_phi
 */
static void fix_phi_arguments(be_ssa_construction_env_t *const env, ir_node *const phi, constr_info *const info)
{
	DBG((dbg, LEVEL_3, "\tfixing phi arguments  %+F\n", phi));

	ir_node *const block = get_nodes_block(phi);
	if (get_Phi_n_preds(phi) != 0) {
		foreach_irn_in(phi, i, op) {
			if (is_definition(env, op)) {
				ir_node *const pred_block = get_Block_cfgpred_block(block, i);
				ir_node *const pred_def   = search_def_end_of_block(env, pred_block);
				DBG((dbg, LEVEL_1, "\t...%+F(%d) -> %+F\n", phi, i, pred_def));
				set_irn_n(phi, i, pred_def);
			}
		}
	} else {
		unsigned  const arity = get_Block_n_cfgpreds(block);
		ir_node **const ins   = ALLOCAN(ir_node*, arity);
		for (unsigned i = 0; i != arity; ++i) {
			ir_node *const pred_block = get_Block_cfgpred_block(block, i);
			ir_node *const pred_def   = search_def_end_of_block(env, pred_block);
			DBG((dbg, LEVEL_1, "\t...%+F(%d) -> %+F\n", phi, i, pred_def));
			ins[i] = pred_def;
		}
		be_complete_Phi(phi, arity, ins);
	}

	info->already_processed = true;
}

void be_ssa_construction_fix_users_array(be_ssa_construction_env_t *env,
                                         ir_node **nodes, size_t nodes_len)
{
	be_timer_push(T_SSA_CONSTR);

	if (!env->iterated_domfront_calculated) {
		mark_iterated_dominance_frontiers(env);
		env->iterated_domfront_calculated = true;
	}

	DBG((dbg, LEVEL_1, "\tfixing users array\n"));

	assert(deq_empty(&env->worklist));

	stat_ev_tim_push();

	for (size_t i = 0; i < nodes_len; ++i) {
		ir_node *value = nodes[i];
		DBG((dbg, LEVEL_3, "\tfixing users of %+F\n", value));
		introduce_definition(env, value);

		foreach_out_edge_safe(value, edge) {
			ir_node *const use = get_edge_src_irn(edge);
			if (is_Anchor(use) || is_End(use))
				continue;

			introduce_use(env, use);
		}
	}

	stat_ev_cnt_decl(uses);
	while (!deq_empty(&env->worklist)) {
		ir_node     *use  = deq_pop_pointer_left(ir_node, &env->worklist);
		constr_info *info = get_info(env, use);

		if (info->already_processed)
			continue;

		if (is_Phi(use)) {
			fix_phi_arguments(env, use, info);
		} else {
			DBG((dbg, LEVEL_3, "\tsearching def for %+F at %+F\n", use, get_nodes_block(use)));
			search_def_at_block(env, use, info);
		}

		stat_ev_cnt_inc(uses);
	}

	be_timer_pop(T_SSA_CONSTR);
	stat_ev_tim_pop("bessaconstr_fix_time");
	stat_ev_cnt_done(uses, "bessaconstr_uses");
}

void be_ssa_construction_fix_users(be_ssa_construction_env_t *env,
                                   ir_node *value)
{
	be_ssa_construction_fix_users_array(env, &value, 1);
}


void be_ssa_construction_update_liveness_phis(be_ssa_construction_env_t *env,
                                              be_lv_t *lv)
{
	be_timer_push(T_SSA_CONSTR);
	for (size_t i = 0, n = ARR_LEN(env->new_phis); i < n; ++i) {
		ir_node *phi = env->new_phis[i];
		be_liveness_introduce(lv, phi);
	}
	be_timer_pop(T_SSA_CONSTR);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_ssaconstr)
void be_init_ssaconstr(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ssaconstr");
}
