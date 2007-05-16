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
 * @brief       Main spill driver.
 * @author      Daniel Grund, Sebastian Hack, Matthias Braun
 * @date		29.09.2005
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "pset.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irbackedge_t.h"
#include "irprintf.h"
#include "ident_t.h"
#include "type_t.h"
#include "entity_t.h"
#include "debug.h"
#include "irgwalk.h"
#include "array.h"
#include "pdeq.h"
#include "execfreq.h"
#include "irnodeset.h"

#include "belive_t.h"
#include "besched_t.h"
#include "bespill.h"
#include "belive_t.h"
#include "benode_t.h"
#include "bechordal_t.h"
#include "bejavacoal.h"
#include "benodesets.h"
#include "bespilloptions.h"
#include "bestatevent.h"
#include "bessaconstr.h"
#include "beirg_t.h"
#include "beintlive_t.h"

/* only rematerialise when costs are less than REMAT_COST_LIMIT */
/* TODO determine a good value here... */
#define REMAT_COST_LIMIT	10

typedef struct _reloader_t reloader_t;

struct _reloader_t {
	reloader_t *next;
	ir_node    *reloader;
	ir_node    *rematted_node;
	int        allow_remat;     /**< the node may be rematted instead of reloaded if global remat option is on */
};

typedef struct _spill_info_t {
	/** the value that should get spilled */
	ir_node *spilled_node;
	/** list of places where the value should get reloaded */
	reloader_t *reloaders;

	/** the spill node, or a PhiM node */
	ir_node *spill;
	/** if we had the value of a phi spilled before but not the phi itself then
	 * this field contains the spill for the phi value */
	ir_node *old_spill;

	/** the register class in which the reload should be placed */
	const arch_register_class_t *reload_cls;
} spill_info_t;

struct _spill_env_t {
	const arch_env_t *arch_env;
	ir_graph *irg;
	struct obstack obst;
	be_irg_t *birg;
	int spill_cost;     /**< the cost of a single spill node */
	int reload_cost;    /**< the cost of a reload node */
	set *spills;        /**< all spill_info_t's, which must be placed */
	ir_nodeset_t mem_phis;     /**< set of all special spilled phis. allocated and freed separately */

	DEBUG_ONLY(firm_dbg_module_t *dbg;)
};

/**
 * Compare two spill infos.
 */
static int cmp_spillinfo(const void *x, const void *y, size_t size) {
	const spill_info_t *xx = x;
	const spill_info_t *yy = y;
	return xx->spilled_node != yy->spilled_node;
}

/**
 * Returns spill info for a specific value (returns NULL if the info doesn't
 * exist yet)
 */
static spill_info_t *find_spillinfo(const spill_env_t *env, ir_node *value) {
	spill_info_t info;
	int hash = nodeset_hash(value);

	info.spilled_node = value;

	return set_find(env->spills, &info, sizeof(info), hash);
}

/**
 * Returns spill info for a specific value (the value that is to be spilled)
 */
static spill_info_t *get_spillinfo(const spill_env_t *env, ir_node *value) {
	spill_info_t info, *res;
	int hash = nodeset_hash(value);

	info.spilled_node = value;
	res = set_find(env->spills, &info, sizeof(info), hash);

	if (res == NULL) {
		info.reloaders = NULL;
		info.spill = NULL;
		info.old_spill = NULL;
		info.reload_cls = NULL;
		res = set_insert(env->spills, &info, sizeof(info), hash);
	}

	return res;
}

#ifdef DEBUG_libfirm
/* Sets the debug module of a spill environment. */
void be_set_spill_env_dbg_module(spill_env_t *env, firm_dbg_module_t *dbg) {
	env->dbg = dbg;
}
#endif

/* Creates a new spill environment. */
spill_env_t *be_new_spill_env(be_irg_t *birg) {
	spill_env_t *env	= xmalloc(sizeof(env[0]));
	env->spills			= new_set(cmp_spillinfo, 1024);
	env->irg            = be_get_birg_irg(birg);
	env->birg           = birg;
	env->arch_env       = birg->main_env->arch_env;
	ir_nodeset_init(&env->mem_phis);
	// TODO, ask backend about costs..., or even ask backend whether we should
	// rematerialize...
	env->spill_cost     = 8;
	env->reload_cost    = 5;
	obstack_init(&env->obst);
	return env;
}

/* Deletes a spill environment. */
void be_delete_spill_env(spill_env_t *env) {
	del_set(env->spills);
	ir_nodeset_destroy(&env->mem_phis);
	obstack_free(&env->obst, NULL);
	free(env);
}

/*
 *  ____  _                  ____      _                 _
 * |  _ \| | __ _  ___ ___  |  _ \ ___| | ___   __ _  __| |___
 * | |_) | |/ _` |/ __/ _ \ | |_) / _ \ |/ _ \ / _` |/ _` / __|
 * |  __/| | (_| | (_|  __/ |  _ <  __/ | (_) | (_| | (_| \__ \
 * |_|   |_|\__,_|\___\___| |_| \_\___|_|\___/ \__,_|\__,_|___/
 *
 */

void be_add_remat(spill_env_t *env, ir_node *to_spill, ir_node *before, ir_node *rematted_node) {
	spill_info_t *spill_info;
	reloader_t *reloader;

	spill_info = get_spillinfo(env, to_spill);

	/* add the remat information */
	reloader                = obstack_alloc(&env->obst, sizeof(reloader[0]));
	reloader->next          = spill_info->reloaders;
	reloader->reloader      = before;
	reloader->rematted_node = rematted_node;
	reloader->allow_remat   = 1;

	spill_info->reloaders  = reloader;

	DBG((env->dbg, LEVEL_1, "creating spillinfo for %+F, will be rematerialized before %+F\n",
		to_spill, before));
}

void be_add_reload(spill_env_t *env, ir_node *to_spill, ir_node *before,
		const arch_register_class_t *reload_cls, int allow_remat)
{
	spill_info_t *info;
	reloader_t *rel;

	info = get_spillinfo(env, to_spill);

	if (is_Phi(to_spill)) {
		int i, arity;

		/* create spillinfos for the phi arguments */
		for (i = 0, arity = get_irn_arity(to_spill); i < arity; ++i) {
			ir_node *arg = get_irn_n(to_spill, i);
			get_spillinfo(env, arg);
		}

#if 1
		// hackery... sometimes the morgan algo spilled the value of a phi,
		// the belady algo decides later to spill the whole phi, then sees the
		// spill node and adds a reload for that spill node, problem is the
		// reload gets attach to that same spill (and is totally unnecessary)
		if (info->old_spill != NULL &&
			(before == info->old_spill || value_dominates(before, info->old_spill)))
		{
			printf("spilledphi hack was needed...\n");
			before = sched_next(info->old_spill);
		}
#endif
	}

	/* put reload into list */
	rel                = obstack_alloc(&env->obst, sizeof(rel[0]));
	rel->next          = info->reloaders;
	rel->reloader      = before;
	rel->rematted_node = NULL;
	rel->allow_remat   = allow_remat;

	info->reloaders  = rel;
	assert(info->reload_cls == NULL || info->reload_cls == reload_cls);
	info->reload_cls = reload_cls;

	DBG((env->dbg, LEVEL_1, "creating spillinfo for %+F, will be reloaded before %+F, may%s be rematerialized\n",
		to_spill, before, allow_remat ? "" : " not"));
}

/**
 * Returns the point at which you can insert a node that should be executed
 * before block @p block when coming from pred @p pos.
 */
static ir_node *get_block_insertion_point(ir_node *block, int pos) {
	ir_node *predblock, *last;

	/* simply add the reload to the beginning of the block if we only have 1
	 * predecessor. We don't need to check for phis as there can't be any in a
	 * block with only 1 pred. */
	if(get_Block_n_cfgpreds(block) == 1) {
		assert(!is_Phi(sched_first(block)));
		return sched_first(block);
	}

	/* We have to reload the value in pred-block */
	predblock = get_Block_cfgpred_block(block, pos);
	last = sched_last(predblock);

	/* we might have projs and keepanys behind the jump... */
	while(is_Proj(last) || be_is_Keep(last)) {
		last = sched_prev(last);
		assert(!sched_is_end(last));
	}

	if(!is_cfop(last)) {
		last = sched_next(last);
		// last node must be a cfop, only exception is the start block
		assert(last	== get_irg_start_block(get_irn_irg(block)));
	}

	// add the reload before the (cond-)jump
	return last;
}

void be_add_reload_on_edge(spill_env_t *env, ir_node *to_spill, ir_node *block, int pos,
		const arch_register_class_t *reload_cls, int allow_remat)
{
	ir_node *before = get_block_insertion_point(block, pos);
	be_add_reload(env, to_spill, before, reload_cls, allow_remat);
}

void be_spill_phi(spill_env_t *env, ir_node *node) {
	spill_info_t* spill;
	int i, arity;

	assert(is_Phi(node));

	ir_nodeset_insert(&env->mem_phis, node);

	// create spillinfos for the phi arguments
	spill = get_spillinfo(env, node);
	for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
		ir_node *arg = get_irn_n(node, i);
		get_spillinfo(env, arg);
	}

	// if we had a spill for the phi value before, then remove this spill from
	// schedule, as we will remove it in the insert spill/reload phase
	if(spill->spill != NULL && !is_Phi(spill->spill)) {
		assert(spill->old_spill == NULL);
		spill->old_spill = spill->spill;
		spill->spill = NULL;
	}
}

/*
 *   ____                _         ____        _ _ _
 *  / ___|_ __ ___  __ _| |_ ___  / ___| _ __ (_) | |___
 * | |   | '__/ _ \/ _` | __/ _ \ \___ \| '_ \| | | / __|
 * | |___| | |  __/ (_| | ||  __/  ___) | |_) | | | \__ \
 *  \____|_|  \___|\__,_|\__\___| |____/| .__/|_|_|_|___/
 *                                      |_|
 */

/**
 * Schedules a node after an instruction. That is the place after all projs and
 * phis that are scheduled after the instruction. This function also skips phi
 * nodes at the beginning of a block
 */
static void sched_add_after_insn(ir_node *sched_after, ir_node *node) {
	ir_node *next = sched_next(sched_after);
	while(is_Proj(next) || is_Phi(next)) {
		next = sched_next(next);
	}
	assert(next != NULL);

	if(sched_is_end(next)) {
		sched_add_after(sched_last(get_nodes_block(sched_after)), node);
	} else {
		sched_add_before(next, node);
	}
}

/**
 * Creates a spill.
 *
 * @param senv      the spill environment
 * @param irn       the node that should be spilled
 * @param ctx_irn   an user of the spilled node
 *
 * @return a be_Spill node
 */
static void spill_irn(spill_env_t *env, spill_info_t *spillinfo) {
	optimization_state_t opt;
	ir_node              *to_spill = spillinfo->spilled_node;

	DBG((env->dbg, LEVEL_1, "spilling %+F ... ", to_spill));

	/* Trying to spill an already spilled value, no need for a new spill
	 * node then, we can simply connect to the same one for this reload
	 *
	 * Normally reloads get simply rematerialized instead of spilled again; this
	 * can happen annyway when the reload is the pred of a phi to spill)
	 */
	if (be_is_Reload(to_spill)) {
		spillinfo->spill = get_irn_n(to_spill, be_pos_Reload_mem);
		DB((env->dbg, LEVEL_1, "skip reload, using existing spill %+F\n", spillinfo->spill));
		return;
	}

	assert(!(arch_irn_is(env->arch_env, to_spill, dont_spill)
				&& "Attempt to spill a node marked 'dont_spill'"));

	/* some backends have virtual noreg/unknown nodes that are not scheduled */
	if(!sched_is_scheduled(to_spill)) {
		spillinfo->spill = new_NoMem();
		return;
	}


	/*
		We switch on optimizations here to get CSE. This is needed as some
		backends have some extra spill phases and we want to make use of those
		spills instead of creating new ones.
	*/
	save_optimization_state(&opt);
	set_optimize(1);
	spillinfo->spill = be_spill(env->arch_env, to_spill);
	restore_optimization_state(&opt);
	if (! sched_is_scheduled(spillinfo->spill)) {
		DB((env->dbg, LEVEL_1, "add spill %+F after %+F\n", spillinfo->spill, to_spill));
		sched_add_after_insn(to_spill, spillinfo->spill);
	} else {
		DB((env->dbg, LEVEL_1, "re-using spill %+F after %+F\n", spillinfo->spill, to_spill));
	}
}

static void spill_node(spill_env_t *env, spill_info_t *spillinfo);

/**
 * If the first usage of a Phi result would be out of memory
 * there is no sense in allocating a register for it.
 * Thus we spill it and all its operands to the same spill slot.
 * Therefore the phi/dataB becomes a phi/Memory
 *
 * @param senv      the spill environment
 * @param phi       the Phi node that should be spilled
 * @param ctx_irn   an user of the spilled node
 */
static void spill_phi(spill_env_t *env, spill_info_t *spillinfo) {
	ir_node *phi = spillinfo->spilled_node;
	int i;
	int arity = get_irn_arity(phi);
	ir_node     *block    = get_nodes_block(phi);
	ir_node     **ins;

	assert(is_Phi(phi));

	DBG((env->dbg, LEVEL_1, "spilling Phi %+F:\n", phi));
	/* build a new PhiM */
	ins = alloca(sizeof(ir_node*) * arity);
	for(i = 0; i < arity; ++i) {
		ins[i] = get_irg_bad(env->irg);
	}
	spillinfo->spill = new_r_Phi(env->irg, block, arity, ins, mode_M);

	for(i = 0; i < arity; ++i) {
		ir_node *arg = get_irn_n(phi, i);
		spill_info_t *arg_info = get_spillinfo(env, arg);

		spill_node(env, arg_info);

		set_irn_n(spillinfo->spill, i, arg_info->spill);
	}
	DBG((env->dbg, LEVEL_1, "... done spilling Phi %+F, created PhiM %+F\n", phi, spillinfo->spill));

	// rewire reloads from old_spill to phi
	if (spillinfo->old_spill != NULL) {
		const ir_edge_t *edge, *next;
		ir_node *old_spill = spillinfo->old_spill;

		DBG((env->dbg, LEVEL_1, "old spill found, rewiring reloads:\n"));

		foreach_out_edge_safe(old_spill, edge, next) {
			ir_node *reload = get_edge_src_irn(edge);
			int     pos     = get_edge_src_pos(edge);

			DBG((env->dbg, LEVEL_1, "\tset input %d of %+F to %+F\n", pos, reload, spillinfo->spill));

			assert(be_is_Reload(reload) || is_Phi(reload));
			set_irn_n(reload, pos, spillinfo->spill);
		}
		DBG((env->dbg, LEVEL_1, "\tset input of %+F to BAD\n", old_spill));
		set_irn_n(old_spill, be_pos_Spill_val, new_Bad());
		//sched_remove(old_spill);
		spillinfo->old_spill = NULL;
	}
}

/**
 * Spill a node.
 *
 * @param senv      the spill environment
 * @param to_spill  the node that should be spilled
 */
static void spill_node(spill_env_t *env, spill_info_t *spillinfo) {
	ir_node *to_spill;

	// the node should be tagged for spilling already...
	if(spillinfo->spill != NULL)
		return;

	to_spill = spillinfo->spilled_node;

	if (is_Phi(to_spill) && ir_nodeset_contains(&env->mem_phis, to_spill)) {
		spill_phi(env, spillinfo);
	} else {
		spill_irn(env, spillinfo);
	}
}

/*
 *
 *  ____                      _            _       _ _
 * |  _ \ ___ _ __ ___   __ _| |_ ___ _ __(_) __ _| (_)_______
 * | |_) / _ \ '_ ` _ \ / _` | __/ _ \ '__| |/ _` | | |_  / _ \
 * |  _ <  __/ | | | | | (_| | ||  __/ |  | | (_| | | |/ /  __/
 * |_| \_\___|_| |_| |_|\__,_|\__\___|_|  |_|\__,_|_|_/___\___|
 *
 */

/**
 * Tests whether value @p arg is available before node @p reloader
 * @returns 1 if value is available, 0 otherwise
 */
static int is_value_available(spill_env_t *env, ir_node *arg, ir_node *reloader) {
	if(is_Unknown(arg) || arg == new_NoMem())
		return 1;

	if(be_is_Spill(arg))
		return 1;

	if(arg == get_irg_frame(env->irg))
		return 1;

	// hack for now (happens when command should be inserted at end of block)
	if(is_Block(reloader)) {
		return 0;
	}

	/*
	 * Ignore registers are always available
	 */
	if(arch_irn_is(env->arch_env, arg, ignore)) {
		return 1;
	}

 	/* the following test does not work while spilling,
	 * because the liveness info is not adapted yet to the effects of the
	 * additional spills/reloads.
	 */
#if 0
	/* we want to remat before the insn reloader
	 * thus an arguments is alive if
	 *   - it interferes with the reloaders result
	 *   - or it is (last-) used by reloader itself
	 */
	if (values_interfere(env->birg->lv, reloader, arg)) {
		return 1;
	}

	arity = get_irn_arity(reloader);
	for (i = 0; i < arity; ++i) {
		ir_node *rel_arg = get_irn_n(reloader, i);
		if (rel_arg == arg)
			return 1;
	}
#endif

 	return 0;
}

/**
 * Checks whether the node can principally be rematerialized
 */
static int is_remat_node(spill_env_t *env, ir_node *node) {
	const arch_env_t *arch_env = env->arch_env;

	assert(!be_is_Spill(node));

	if(arch_irn_is(arch_env, node, rematerializable))
		return 1;

	return 0;
}

/**
 * Check if a node is rematerializable. This tests for the following conditions:
 *
 * - The node itself is rematerializable
 * - All arguments of the node are available or also rematerialisable
 * - The costs for the rematerialisation operation is less or equal a limit
 *
 * Returns the costs needed for rematerialisation or something
 * > REMAT_COST_LIMIT if remat is not possible.
 */
static int check_remat_conditions_costs(spill_env_t *env, ir_node *spilled, ir_node *reloader, int parentcosts) {
	int i, arity;
	int argremats;
	int costs = 0;

	if(!is_remat_node(env, spilled))
		return REMAT_COST_LIMIT;

	if(be_is_Reload(spilled)) {
		costs += 2;
	} else {
		costs += arch_get_op_estimated_cost(env->arch_env, spilled);
	}
	if(parentcosts + costs >= REMAT_COST_LIMIT) {
		return REMAT_COST_LIMIT;
	}

	argremats = 0;
	for(i = 0, arity = get_irn_arity(spilled); i < arity; ++i) {
		ir_node *arg = get_irn_n(spilled, i);

		if(is_value_available(env, arg, reloader))
			continue;

		// we have to rematerialize the argument as well...
		if(argremats >= 1) {
			/* we only support rematerializing 1 argument at the moment,
			 * so that we don't have to care about register pressure
			 */
			return REMAT_COST_LIMIT;
		}
		argremats++;

		costs += check_remat_conditions_costs(env, arg, reloader, parentcosts + costs);
		if(parentcosts + costs >= REMAT_COST_LIMIT)
			return REMAT_COST_LIMIT;
	}

	return costs;
}

static int check_remat_conditions(spill_env_t *env, ir_node *spilled, ir_node *reloader) {
	int costs = check_remat_conditions_costs(env, spilled, reloader, 0);

	return costs < REMAT_COST_LIMIT;
}

/**
 * Re-materialize a node.
 *
 * @param senv      the spill environment
 * @param spilled   the node that was spilled
 * @param reloader  a irn that requires a reload
 */
static ir_node *do_remat(spill_env_t *env, ir_node *spilled, ir_node *reloader) {
	int i, arity;
	ir_node *res;
	ir_node *bl;
	ir_node **ins;

	if(is_Block(reloader)) {
		bl = reloader;
	} else {
		bl = get_nodes_block(reloader);
	}

	ins = alloca(get_irn_arity(spilled) * sizeof(ins[0]));
	for(i = 0, arity = get_irn_arity(spilled); i < arity; ++i) {
		ir_node *arg = get_irn_n(spilled, i);

		if(is_value_available(env, arg, reloader)) {
			ins[i] = arg;
		} else {
			ins[i] = do_remat(env, arg, reloader);
		}
	}

	/* create a copy of the node */
	res = new_ir_node(get_irn_dbg_info(spilled), env->irg, bl,
		get_irn_op(spilled),
		get_irn_mode(spilled),
		get_irn_arity(spilled),
		ins);
	copy_node_attr(spilled, res);
	new_backedge_info(res);
	sched_reset(res);

	DBG((env->dbg, LEVEL_1, "Insert remat %+F of %+F before reloader %+F\n", res, spilled, reloader));

	/* insert in schedule */
	sched_add_before(reloader, res);

	return res;
}

int be_get_reload_costs(spill_env_t *env, ir_node *to_spill, ir_node *before) {
	spill_info_t *spill_info;

	if(be_do_remats) {
		// is the node rematerializable?
		int costs = check_remat_conditions_costs(env, to_spill, before, 0);
		if(costs < REMAT_COST_LIMIT)
			return costs;
	}

	// do we already have a spill?
	spill_info = find_spillinfo(env, to_spill);
	if(spill_info != NULL && spill_info->spill != NULL)
		return env->reload_cost;

	return env->spill_cost + env->reload_cost;
}

int be_get_reload_costs_on_edge(spill_env_t *env, ir_node *to_spill, ir_node *block, int pos) {
	ir_node *before = get_block_insertion_point(block, pos);
	return be_get_reload_costs(env, to_spill, before);
}

/*
 *  ___                     _     ____      _                 _
 * |_ _|_ __  ___  ___ _ __| |_  |  _ \ ___| | ___   __ _  __| |___
 *  | || '_ \/ __|/ _ \ '__| __| | |_) / _ \ |/ _ \ / _` |/ _` / __|
 *  | || | | \__ \  __/ |  | |_  |  _ <  __/ | (_) | (_| | (_| \__ \
 * |___|_| |_|___/\___|_|   \__| |_| \_\___|_|\___/ \__,_|\__,_|___/
 *
 */

void be_insert_spills_reloads(spill_env_t *env) {
	const arch_env_t *arch_env = env->arch_env;
	int              remats    = 0;
	int              reloads   = 0;
	int              spills    = 0;
	spill_info_t     *si;
	ir_nodeset_iterator_t iter;
	ir_node          *node;

	/* create all phi-ms first, this is needed so, that phis, hanging on
	   spilled phis work correctly */
	foreach_ir_nodeset(&env->mem_phis, node, iter) {
		spill_info_t *info = get_spillinfo(env, node);
		spill_node(env, info);
	}

	/* process each spilled node */
	for (si = set_first(env->spills); si; si = set_next(env->spills)) {
		reloader_t *rld;
		ir_node *spilled_node = si->spilled_node;
		ir_mode         *mode = get_irn_mode(spilled_node);
		ir_node      **copies = NEW_ARR_F(ir_node*, 0);

		DBG((env->dbg, LEVEL_1, "\nhandling all reloaders of %+F:\n", spilled_node));

		/* go through all reloads for this spill */
		for (rld = si->reloaders; rld; rld = rld->next) {
			ir_node *copy; /* a reload is a "copy" of the original value */

			if (rld->rematted_node != NULL) {
				copy = rld->rematted_node;
				remats++;
				sched_add_before(rld->reloader, copy);
			} else if (be_do_remats && rld->allow_remat &&
					check_remat_conditions(env, spilled_node, rld->reloader)) {
				copy = do_remat(env, spilled_node, rld->reloader);
				remats++;
			} else {
				/* make sure we have a spill */
				if (si->spill == NULL) {
					spill_node(env, si);
					spills++;
				}

				/* create a reload */
				copy = be_reload(arch_env, si->reload_cls, rld->reloader, mode, si->spill);
				reloads++;
			}

			DBG((env->dbg, LEVEL_1, " %+F of %+F before %+F\n",
			     copy, spilled_node, rld->reloader));
			ARR_APP1(ir_node*, copies, copy);
		}

		/* if we had any reloads or remats, then we need to reconstruct the
		 * SSA form for the spilled value */
		if (ARR_LEN(copies) > 0) {
			be_ssa_construction_env_t senv;
			/* be_lv_t *lv = be_get_birg_liveness(env->birg); */

			be_ssa_construction_init(&senv, env->birg);
			be_ssa_construction_add_copy(&senv, spilled_node);
			be_ssa_construction_add_copies(&senv, copies, ARR_LEN(copies));
			be_ssa_construction_fix_users(&senv, spilled_node);

#if 0
			/* no need to enable this as long as we invalidate liveness
			   after this function... */
			be_ssa_construction_update_liveness_phis(&senv);
			be_liveness_update(spilled_node);
			len = ARR_LEN(copies);
			for(i = 0; i < len; ++i) {
				be_liveness_update(lv, copies[i]);
			}
#endif
			be_ssa_construction_destroy(&senv);
		}

		DEL_ARR_F(copies);
		si->reloaders = NULL;
	}

#ifdef FIRM_STATISTICS
	if (be_stat_ev_is_active()) {
		be_stat_ev("spill_spills", spills);
		be_stat_ev("spill_reloads", reloads);
		be_stat_ev("spill_remats", remats);
	}
#endif /* FIRM_STATISTICS */

	be_remove_dead_nodes_from_schedule(env->irg);
	/* Matze: In theory be_ssa_construction should take care of the liveness...
	 * try to disable this again in the future */
	be_invalidate_liveness(env->birg);
}
