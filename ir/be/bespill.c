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
 * @brief       implementation of the spill/reload placement abstraction layer
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
#include "error.h"

#include "bearch_t.h"
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
#include "bemodule.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define REMAT_COST_INFINITE  1000

typedef struct reloader_t reloader_t;
struct reloader_t {
	reloader_t *next;
	ir_node    *can_spill_after;
	ir_node    *reloader;
	ir_node    *rematted_node;
	int         remat_cost_delta; /** costs needed for rematerialization,
	                                   compared to placing a reload */
};

typedef struct spill_t spill_t;
struct spill_t {
	spill_t *next;
	ir_node *before;   /**< spill has to be placed before this node (or earlier) */
	ir_node *spill;
};

typedef struct spill_info_t spill_info_t;
struct spill_info_t {
	ir_node    *to_spill;  /**< the value that should get spilled */
	reloader_t *reloaders; /**< list of places where the value should get
	                            reloaded */
	spill_t    *spills;    /**< list of latest places where spill must be
	                            placed */
	double      spill_costs; /**< costs needed for spilling the value */
	const arch_register_class_t *reload_cls; /** the register class in which the
	                                             reload should be placed */
};

struct spill_env_t {
	const arch_env_t *arch_env;
	ir_graph         *irg;
	struct obstack    obst;
	be_irg_t         *birg;
	int               spill_cost;     /**< the cost of a single spill node */
	int               reload_cost;    /**< the cost of a reload node */
	set              *spills;         /**< all spill_info_t's, which must be
	                                       placed */
	ir_nodeset_t      mem_phis;       /**< set of all spilled phis. */
	ir_exec_freq     *exec_freq;

#ifdef FIRM_STATISTICS
	unsigned          spill_count;
	unsigned          reload_count;
	unsigned          remat_count;
	unsigned          spilled_phi_count;
#endif
};

/**
 * Compare two spill infos.
 */
static int cmp_spillinfo(const void *x, const void *y, size_t size)
{
	const spill_info_t *xx = x;
	const spill_info_t *yy = y;
	(void) size;

	return xx->to_spill != yy->to_spill;
}

/**
 * Returns spill info for a specific value (the value that is to be spilled)
 */
static spill_info_t *get_spillinfo(const spill_env_t *env, ir_node *value)
{
	spill_info_t info, *res;
	int hash = nodeset_hash(value);

	info.to_spill = value;
	res = set_find(env->spills, &info, sizeof(info), hash);

	if (res == NULL) {
		info.reloaders   = NULL;
		info.spills      = NULL;
		info.spill_costs = -1;
		info.reload_cls  = NULL;
		res = set_insert(env->spills, &info, sizeof(info), hash);
	}

	return res;
}

spill_env_t *be_new_spill_env(be_irg_t *birg)
{
	const arch_env_t *arch_env = birg->main_env->arch_env;

	spill_env_t *env	= xmalloc(sizeof(env[0]));
	env->spills			= new_set(cmp_spillinfo, 1024);
	env->irg            = be_get_birg_irg(birg);
	env->birg           = birg;
	env->arch_env       = arch_env;
	ir_nodeset_init(&env->mem_phis);
	env->spill_cost     = arch_env->isa->spill_cost;
	env->reload_cost    = arch_env->isa->reload_cost;
	env->exec_freq      = be_get_birg_exec_freq(birg);
	obstack_init(&env->obst);

#ifdef FIRM_STATISTICS
	env->spill_count       = 0;
	env->reload_count      = 0;
	env->remat_count       = 0;
	env->spilled_phi_count = 0;
#endif

	return env;
}

void be_delete_spill_env(spill_env_t *env)
{
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

void be_add_spill(spill_env_t *env, ir_node *to_spill, ir_node *before)
{
	spill_info_t *spill_info = get_spillinfo(env, to_spill);
	spill_t      *spill;
	spill_t      *s;
	spill_t      *last;

	DB((dbg, LEVEL_1, "Add spill of %+F before %+F\n", to_spill, before));

	/* spills that are dominated by others are not needed */
	last = NULL;
	s    = spill_info->spills;
	for( ; s != NULL; s = s->next) {
		/* no need to add this spill if it is dominated by another */
		if(value_dominates(s->before, before)) {
			DB((dbg, LEVEL_1, "...dominated by %+F, not added\n", s->before));
			return;
		}
		/* remove spills that we dominate */
		if(value_dominates(before, s->before)) {
			DB((dbg, LEVEL_1, "...remove old spill at %+F\n", s->before));
			if(last != NULL) {
				last->next         = s->next;
			} else {
				spill_info->spills = s->next;
			}
		} else {
			last = s;
		}
	}

	spill         = obstack_alloc(&env->obst, sizeof(spill[0]));
	spill->before = before;
	spill->next   = spill_info->spills;
	spill->spill  = NULL;

	spill_info->spills = spill;
}

void be_add_remat(spill_env_t *env, ir_node *to_spill, ir_node *before,
                  ir_node *rematted_node)
{
	spill_info_t *spill_info;
	reloader_t *reloader;

	spill_info = get_spillinfo(env, to_spill);

	/* add the remat information */
	reloader                   = obstack_alloc(&env->obst, sizeof(reloader[0]));
	reloader->next             = spill_info->reloaders;
	reloader->reloader         = before;
	reloader->rematted_node    = rematted_node;
	reloader->remat_cost_delta = 0; /* We will never have a cost win over a
	                                   reload since we're not even allowed to
	                                   create a reload */

	spill_info->reloaders  = reloader;

	DBG((dbg, LEVEL_1, "creating spillinfo for %+F, will be rematerialized before %+F\n",
		to_spill, before));
}

void be_add_reload2(spill_env_t *env, ir_node *to_spill, ir_node *before,
		ir_node *can_spill_after, const arch_register_class_t *reload_cls,
		int allow_remat)
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
	}

	assert(!is_Proj(before) && !be_is_Keep(before));

	/* put reload into list */
	rel                   = obstack_alloc(&env->obst, sizeof(rel[0]));
	rel->next             = info->reloaders;
	rel->reloader         = before;
	rel->rematted_node    = NULL;
	rel->can_spill_after  = can_spill_after;
	rel->remat_cost_delta = allow_remat ? 0 : REMAT_COST_INFINITE;

	info->reloaders  = rel;
	assert(info->reload_cls == NULL || info->reload_cls == reload_cls);
	info->reload_cls = reload_cls;

	DBG((dbg, LEVEL_1, "creating spillinfo for %+F, will be reloaded before %+F, may%s be rematerialized\n",
		to_spill, before, allow_remat ? "" : " not"));
}

void be_add_reload(spill_env_t *senv, ir_node *to_spill, ir_node *before,
                   const arch_register_class_t *reload_cls, int allow_remat)
{
	be_add_reload2(senv, to_spill, before, to_spill, reload_cls, allow_remat);

}

ir_node *be_get_end_of_block_insertion_point(const ir_node *block)
{
	ir_node *last = sched_last(block);

	/* we might have keeps behind the jump... */
	while(be_is_Keep(last)) {
		last = sched_prev(last);
		assert(!sched_is_end(last));
	}

	assert(is_cfop(last));

	/* add the reload before the (cond-)jump */
	return last;
}

/**
 * Returns the point at which you can insert a node that should be executed
 * before block @p block when coming from pred @p pos.
 */
static ir_node *get_block_insertion_point(ir_node *block, int pos)
{
	ir_node *predblock;

	/* simply add the reload to the beginning of the block if we only have 1
	 * predecessor. We don't need to check for phis as there can't be any in a
	 * block with only 1 pred. */
	if(get_Block_n_cfgpreds(block) == 1) {
		assert(!is_Phi(sched_first(block)));
		return sched_first(block);
	}

	/* We have to reload the value in pred-block */
	predblock = get_Block_cfgpred_block(block, pos);
	return be_get_end_of_block_insertion_point(predblock);
}

void be_add_reload_at_end(spill_env_t *env, ir_node *to_spill,
                          const ir_node *block,
                          const arch_register_class_t *reload_cls,
                          int allow_remat)
{
	ir_node *before = be_get_end_of_block_insertion_point(block);
	be_add_reload(env, to_spill, before, reload_cls, allow_remat);
}

void be_add_reload_on_edge(spill_env_t *env, ir_node *to_spill, ir_node *block,
                           int pos,	const arch_register_class_t *reload_cls,
                           int allow_remat)
{
	ir_node *before = get_block_insertion_point(block, pos);
	be_add_reload(env, to_spill, before, reload_cls, allow_remat);
}

void be_spill_phi(spill_env_t *env, ir_node *node)
{
	ir_node *block;
	spill_info_t* spill;
	int i, arity;

	assert(is_Phi(node));

	ir_nodeset_insert(&env->mem_phis, node);

	/* create spills for the phi arguments */
	block = get_nodes_block(node);
	spill = get_spillinfo(env, node);
	for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
		ir_node *arg        = get_irn_n(node, i);
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		ir_node *insert     = be_get_end_of_block_insertion_point(pred_block);
		//get_spillinfo(env, arg);
		be_add_spill(env, arg, insert);
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

static ir_node *skip_keeps_phis(ir_node *node)
{
	node = sched_next(node);
	while(is_Phi(node) || be_is_Keep(node)) {
		node = sched_next(node);
	}
	return node;
}

static void determine_spill_costs(spill_env_t *env, spill_info_t *spillinfo);

/**
 * Creates a spill.
 *
 * @param senv      the spill environment
 * @param irn       the node that should be spilled
 * @param ctx_irn   an user of the spilled node
 *
 * @return a be_Spill node
 */
static void spill_irn(spill_env_t *env, spill_info_t *spillinfo)
{
	ir_node *to_spill = spillinfo->to_spill;
	spill_t *spill;

	/* determine_spill_costs must have been run before */
	assert(spillinfo->spill_costs >= 0);

	/* some backends have virtual noreg/unknown nodes that are not scheduled
	 * and simply always available. */
	if(!sched_is_scheduled(to_spill)) {
		/* override spillinfos or create a new one */
		spillinfo->spills->spill = new_NoMem();
		DB((dbg, LEVEL_1, "don't spill %+F use NoMem\n", to_spill));
		return;
	}

	DBG((dbg, LEVEL_1, "spilling %+F ... ", to_spill));
	spill = spillinfo->spills;
	for( ; spill != NULL; spill = spill->next) {
		ir_node *block  = get_block(spill->before);
		ir_node *before = spill->before;

		/* place all spills before the reloads (as we can't guarantee the
		 * same order as the be_add_spill and be_add_reload calls */
		while(be_is_Reload(sched_prev(before))) {
			before = sched_prev(before);
		}

		spill->spill    = be_spill(env->arch_env, block, to_spill);
		sched_add_before(before, spill->spill);
		DB((dbg, LEVEL_1, "\t%+F before %+F,", spill->spill, before));
#ifdef FIRM_STATISTICS
		env->spill_count++;
#endif
	}
	DBG((dbg, LEVEL_1, "\n"));
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
static void spill_phi(spill_env_t *env, spill_info_t *spillinfo)
{
	ir_graph *irg   = env->irg;
	ir_node  *phi   = spillinfo->to_spill;
	ir_node  *block = get_nodes_block(phi);
	ir_node  *unknown;
	ir_node **ins;
	spill_t  *spill;
	int       i;
	int       arity;

	assert(is_Phi(phi));
	assert(!get_opt_cse());
	DBG((dbg, LEVEL_1, "spilling Phi %+F:\n", phi));

	/* build a new PhiM */
	arity   = get_irn_arity(phi);
	ins     = alloca(sizeof(ir_node*) * arity);
	unknown = new_r_Unknown(irg, mode_M);
	for(i = 0; i < arity; ++i) {
		ins[i] = unknown;
	}

	/* override replace spills... */
	spill         = obstack_alloc(&env->obst, sizeof(spill[0]));
	spill->before = skip_keeps_phis(phi);
	spill->spill  = new_r_Phi(irg, block, arity, ins, mode_M);
	spill->next   = NULL;

	spillinfo->spills = spill;
#ifdef FIRM_STATISTICS
	env->spilled_phi_count++;
#endif

	for(i = 0; i < arity; ++i) {
		ir_node      *arg      = get_irn_n(phi, i);
		spill_info_t *arg_info = get_spillinfo(env, arg);

		determine_spill_costs(env, arg_info);
		spill_node(env, arg_info);

		set_irn_n(spill->spill, i, arg_info->spills->spill);
	}
	DBG((dbg, LEVEL_1, "... done spilling Phi %+F, created PhiM %+F\n", phi,
	     spill->spill));
}

/**
 * Spill a node.
 *
 * @param senv      the spill environment
 * @param to_spill  the node that should be spilled
 */
static void spill_node(spill_env_t *env, spill_info_t *spillinfo)
{
	ir_node *to_spill;

	/* node is already spilled */
	if(spillinfo->spills != NULL && spillinfo->spills->spill != NULL)
		return;

	to_spill = spillinfo->to_spill;

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
static int is_value_available(spill_env_t *env, const ir_node *arg,
                              const ir_node *reloader)
{
	if(is_Unknown(arg) || arg == new_NoMem())
		return 1;

	if(be_is_Spill(arg))
		return 1;

	if(arg == get_irg_frame(env->irg))
		return 1;

	/* hack for now (happens when command should be inserted at end of block) */
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
static int is_remat_node(spill_env_t *env, const ir_node *node)
{
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
 * >= REMAT_COST_INFINITE if remat is not possible.
 */
static int check_remat_conditions_costs(spill_env_t *env,
		const ir_node *spilled, const ir_node *reloader, int parentcosts)
{
	int i, arity;
	int argremats;
	int costs = 0;

	if(!is_remat_node(env, spilled))
		return REMAT_COST_INFINITE;

	if(be_is_Reload(spilled)) {
		costs += 2;
	} else {
		costs += arch_get_op_estimated_cost(env->arch_env, spilled);
	}
	if(parentcosts + costs >= env->reload_cost + env->spill_cost) {
		return REMAT_COST_INFINITE;
	}
	if(arch_irn_is(env->arch_env, spilled, modify_flags)) {
		return REMAT_COST_INFINITE;
	}

	argremats = 0;
	for(i = 0, arity = get_irn_arity(spilled); i < arity; ++i) {
		ir_node *arg = get_irn_n(spilled, i);

		if(is_value_available(env, arg, reloader))
			continue;

		/* we have to rematerialize the argument as well */
		if(argremats >= 1) {
			/* we only support rematerializing 1 argument at the moment,
			 * so that we don't have to care about register pressure
			 */
			return REMAT_COST_INFINITE;
		}
		argremats++;

		costs += check_remat_conditions_costs(env, arg, reloader,
		                                      parentcosts + costs);
		if(parentcosts + costs >= env->reload_cost + env->spill_cost)
			return REMAT_COST_INFINITE;
	}

	return costs;
}

/**
 * Re-materialize a node.
 *
 * @param senv      the spill environment
 * @param spilled   the node that was spilled
 * @param reloader  a irn that requires a reload
 */
static ir_node *do_remat(spill_env_t *env, ir_node *spilled, ir_node *reloader)
{
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
#ifdef FIRM_STATISTICS
			/* don't count the recursive call as remat */
			env->remat_count--;
#endif
		}
	}

	/* create a copy of the node */
	res = new_ir_node(get_irn_dbg_info(spilled), env->irg, bl,
	                  get_irn_op(spilled), get_irn_mode(spilled),
	                  get_irn_arity(spilled), ins);
	copy_node_attr(spilled, res);
	new_backedge_info(res);

	DBG((dbg, LEVEL_1, "Insert remat %+F of %+F before reloader %+F\n", res, spilled, reloader));

	if (! is_Proj(res)) {
		/* insert in schedule */
		sched_reset(res);
		sched_add_before(reloader, res);
#ifdef FIRM_STATISTICS
		env->remat_count++;
#endif
	}

	return res;
}

double be_get_spill_costs(spill_env_t *env, ir_node *to_spill, ir_node *before)
{
	ir_node *block = get_nodes_block(before);
	double   freq  = get_block_execfreq(env->exec_freq, block);
	(void) to_spill;

	return env->spill_cost * freq;
}

double be_get_reload_costs(spill_env_t *env, ir_node *to_spill, ir_node *before)
{
	ir_node      *block = get_nodes_block(before);
	double        freq  = get_block_execfreq(env->exec_freq, block);

	if(be_do_remats) {
		/* is the node rematerializable? */
		int costs = check_remat_conditions_costs(env, to_spill, before, 0);
		if(costs < env->reload_cost)
			return costs * freq;
	}

	return env->reload_cost * freq;
}

int be_is_rematerializable(spill_env_t *env, const ir_node *to_remat,
                           const ir_node *before)
{
	return check_remat_conditions_costs(env, to_remat, before, 0) < REMAT_COST_INFINITE;
}

double be_get_reload_costs_on_edge(spill_env_t *env, ir_node *to_spill,
                                   ir_node *block, int pos)
{
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

/**
 * analyzes how to best spill a node and determine costs for that
 */
static void determine_spill_costs(spill_env_t *env, spill_info_t *spillinfo)
{
	ir_node *to_spill = spillinfo->to_spill;
	ir_node *spill_block;
	spill_t *spill;
	double   spill_execfreq;

	/* already calculated? */
	if(spillinfo->spill_costs >= 0)
		return;

	assert(! arch_irn_is(env->arch_env, to_spill, dont_spill));
	assert(!be_is_Reload(to_spill));

	/* some backends have virtual noreg/unknown nodes that are not scheduled
	 * and simply always available.
	 * TODO: this is kinda hairy, the NoMem is correct for an Unknown as Phi
	 * predecessor (of a PhiM) but this test might match other things too...
	 */
	if(!sched_is_scheduled(to_spill)) {
		/* override spillinfos or create a new one */
		spill_t *spill = obstack_alloc(&env->obst, sizeof(spill[0]));
		spill->before  = NULL;
		spill->next    = NULL;
		spill->spill   = new_NoMem();

		spillinfo->spills      = spill;
		spillinfo->spill_costs = 0;

		DB((dbg, LEVEL_1, "don't spill %+F use NoMem\n", to_spill));
		return;
	}

	spill_block    = get_nodes_block(to_spill);
	spill_execfreq = get_block_execfreq(env->exec_freq, spill_block);

	if (is_Phi(to_spill) && ir_nodeset_contains(&env->mem_phis, to_spill)) {
		/* TODO calculate correct costs...
		 * (though we can't remat this node anyway so no big problem) */
		spillinfo->spill_costs = env->spill_cost * spill_execfreq;
		return;
	}

	if(spillinfo->spills != NULL) {
		spill_t *s;
		double   spills_execfreq;

		/* calculate sum of executaion frequencies of individual spills */
		spills_execfreq = 0;
		s               = spillinfo->spills;
		for( ; s != NULL; s = s->next) {
			ir_node *spill_block = s->before;
			double   freq;

			if(!is_Block(spill_block)) {
				spill_block = get_nodes_block(spill_block);
			}
			freq = get_block_execfreq(env->exec_freq, spill_block);

			spills_execfreq += freq;
		}

		DB((dbg, LEVEL_1, "%+F: latespillcosts %f after def: %f\n", to_spill,
		    spills_execfreq * env->spill_cost,
		    spill_execfreq * env->spill_cost));

		/* multi-/latespill is advantageous -> return*/
		if(spills_execfreq < spill_execfreq) {
			DB((dbg, LEVEL_1, "use latespills for %+F\n", to_spill));
			spillinfo->spill_costs = spills_execfreq * env->spill_cost;
			return;
		}
	}

	/* override spillinfos or create a new one */
	spill         = obstack_alloc(&env->obst, sizeof(spill[0]));
	spill->before = skip_keeps_phis(to_spill);
	spill->next   = NULL;
	spill->spill  = NULL;

	spillinfo->spills      = spill;
	spillinfo->spill_costs = spill_execfreq * env->spill_cost;
	DB((dbg, LEVEL_1, "spill %+F after definition\n", to_spill));
}

void be_insert_spills_reloads(spill_env_t *env)
{
	const arch_env_t      *arch_env  = env->arch_env;
	const ir_exec_freq    *exec_freq = env->exec_freq;
	spill_info_t          *si;
	ir_nodeset_iterator_t  iter;
	ir_node               *node;

	/* create all phi-ms first, this is needed so, that phis, hanging on
	   spilled phis work correctly */
	foreach_ir_nodeset(&env->mem_phis, node, iter) {
		spill_info_t *info = get_spillinfo(env, node);
		spill_node(env, info);
	}

	/* process each spilled node */
	for (si = set_first(env->spills); si; si = set_next(env->spills)) {
		reloader_t *rld;
		ir_node  *to_spill        = si->to_spill;
		ir_mode  *mode            = get_irn_mode(to_spill);
		ir_node **copies          = NEW_ARR_F(ir_node*, 0);
		double    all_remat_costs = 0; /** costs when we would remat all nodes */
		int       force_remat     = 0;

		DBG((dbg, LEVEL_1, "\nhandling all reloaders of %+F:\n", to_spill));

		determine_spill_costs(env, si);

		/* determine possibility of rematerialisations */
		if(be_do_remats) {
			/* calculate cost savings for each indivial value when it would
			   be rematted instead of reloaded */
			for (rld = si->reloaders; rld != NULL; rld = rld->next) {
				double   freq;
				int      remat_cost;
				int      remat_cost_delta;
				ir_node *block;
				ir_node *reloader = rld->reloader;

				if(rld->rematted_node != NULL) {
					DBG((dbg, LEVEL_2, "\tforced remat %+F before %+F\n",
					     rld->rematted_node, reloader));
					continue;
				}
				if(rld->remat_cost_delta >= REMAT_COST_INFINITE) {
					DBG((dbg, LEVEL_2, "\treload before %+F is forbidden\n",
					     reloader));
					all_remat_costs = REMAT_COST_INFINITE;
					continue;
				}

				remat_cost  = check_remat_conditions_costs(env, to_spill,
				                                           reloader, 0);
				if(remat_cost >= REMAT_COST_INFINITE) {
					DBG((dbg, LEVEL_2, "\tremat before %+F not possible\n",
					     reloader));
					rld->remat_cost_delta = REMAT_COST_INFINITE;
					all_remat_costs       = REMAT_COST_INFINITE;
					continue;
				}

				remat_cost_delta      = remat_cost - env->reload_cost;
				rld->remat_cost_delta = remat_cost_delta;
				block                 = is_Block(reloader) ? reloader : get_nodes_block(reloader);
				freq                  = get_block_execfreq(exec_freq, block);
				all_remat_costs      += remat_cost_delta * freq;
				DBG((dbg, LEVEL_2, "\tremat costs delta before %+F: "
				     "%d (rel %f)\n", reloader, remat_cost_delta,
				     remat_cost_delta * freq));
			}
			if(all_remat_costs < REMAT_COST_INFINITE) {
				/* we don't need the costs for the spill if we can remat
				   all reloaders */
				all_remat_costs -= si->spill_costs;

				DBG((dbg, LEVEL_2, "\tspill costs %d (rel %f)\n",
				     env->spill_cost, si->spill_costs));
			}

			if(all_remat_costs < 0) {
				DBG((dbg, LEVEL_1, "\nforcing remats of all reloaders (%f)\n",
				     all_remat_costs));
				force_remat = 1;
			}
		}

		/* go through all reloads for this spill */
		for (rld = si->reloaders; rld != NULL; rld = rld->next) {
			ir_node *copy; /* a reload is a "copy" of the original value */

			if (rld->rematted_node != NULL) {
				copy = rld->rematted_node;
				sched_add_before(rld->reloader, copy);
			} else if (be_do_remats &&
					(force_remat || rld->remat_cost_delta < 0)) {
				copy = do_remat(env, to_spill, rld->reloader);
			} else {
				/* make sure we have a spill */
				spill_node(env, si);

				/* create a reload, use the first spill for now SSA
				 * reconstruction for memory comes below */
				assert(si->spills != NULL);
				copy = be_reload(arch_env, si->reload_cls, rld->reloader, mode,
				                 si->spills->spill);
#ifdef FIRM_STATISTICS
				env->reload_count++;
#endif
			}

			DBG((dbg, LEVEL_1, " %+F of %+F before %+F\n",
			     copy, to_spill, rld->reloader));
			ARR_APP1(ir_node*, copies, copy);
		}

		/* if we had any reloads or remats, then we need to reconstruct the
		 * SSA form for the spilled value */
		if (ARR_LEN(copies) > 0) {
			be_ssa_construction_env_t senv;
			/* be_lv_t *lv = be_get_birg_liveness(env->birg); */

			be_ssa_construction_init(&senv, env->birg);
			be_ssa_construction_add_copy(&senv, to_spill);
			be_ssa_construction_add_copies(&senv, copies, ARR_LEN(copies));
			be_ssa_construction_fix_users(&senv, to_spill);

#if 0
			/* no need to enable this as long as we invalidate liveness
			   after this function... */
			be_ssa_construction_update_liveness_phis(&senv);
			be_liveness_update(to_spill);
			len = ARR_LEN(copies);
			for(i = 0; i < len; ++i) {
				be_liveness_update(lv, copies[i]);
			}
#endif
			be_ssa_construction_destroy(&senv);
		}
		/* need to reconstruct SSA form if we had multiple spills */
		if (si->spills != NULL && si->spills->next != NULL) {
			spill_t *spill;
			int      spill_count = 0;

			be_ssa_construction_env_t senv;

			be_ssa_construction_init(&senv, env->birg);
			spill = si->spills;
			for( ; spill != NULL; spill = spill->next) {
				/* maybe we rematerialized the value and need no spill */
				if(spill->spill == NULL)
					continue;
				be_ssa_construction_add_copy(&senv, spill->spill);
				spill_count++;
			}
			if(spill_count > 1) {
				/* all reloads are attached to the first spill, fix them now */
				be_ssa_construction_fix_users(&senv, si->spills->spill);
			}

			be_ssa_construction_destroy(&senv);
		}

		DEL_ARR_F(copies);
		si->reloaders = NULL;
	}

	stat_ev_dbl("spill_spills", env->spill_count);
	stat_ev_dbl("spill_reloads", env->reload_count);
	stat_ev_dbl("spill_remats", env->remat_count);
	stat_ev_dbl("spill_spilled_phis", env->spilled_phi_count);

	/* Matze: In theory be_ssa_construction should take care of the liveness...
	 * try to disable this again in the future */
	be_liveness_invalidate(env->birg->lv);

	be_remove_dead_nodes_from_schedule(env->birg);
}

void be_init_spill(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.spill");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spill);
