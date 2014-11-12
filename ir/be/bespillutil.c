/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       implementation of the spill/reload placement abstraction layer
 * @author      Daniel Grund, Sebastian Hack, Matthias Braun
 * @date        29.09.2005
 */
#include <stdlib.h>
#include <stdbool.h>

#include "array.h"
#include "bearch.h"
#include "bechordal_t.h"
#include "beirg.h"
#include "belive.h"
#include "bemodule.h"
#include "benode.h"
#include "besched.h"
#include "bespill.h"
#include "bespillutil.h"
#include "bessaconstr.h"
#include "be_t.h"
#include "beutil.h"
#include "debug.h"
#include "entity_t.h"
#include "execfreq.h"
#include "ident_t.h"
#include "irbackedge_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnodehashmap.h"
#include "irnode_t.h"
#include "panic.h"
#include "statev_t.h"
#include "type_t.h"
#include "util.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)
DEBUG_ONLY(static firm_dbg_module_t *dbg_constr;)

#define REMAT_COST_INFINITE  1000

typedef struct reloader_t reloader_t;
struct reloader_t {
	reloader_t *next;
	ir_node    *reloader;
	ir_node    *rematted_node;
	int         remat_cost_delta; /** costs needed for rematerialization,
	                                  compared to placing a reload */
};

typedef struct spill_t spill_t;
struct spill_t {
	spill_t *next;
	ir_node *after;  /**< spill has to be placed after this node (or earlier) */
	ir_node *spill;
};

typedef struct spill_info_t spill_info_t;
struct spill_info_t {
	ir_node      *to_spill;  /**< the value that should get spilled */
	reloader_t   *reloaders; /**< list of places where the value should get
	                              reloaded */
	spill_t      *spills;    /**< list of latest places where spill must be
	                              placed */
	spill_info_t *next;
	spill_info_t *next_mem_phi;
	double        spill_costs; /**< costs needed for spilling the value */
	bool          spilled_phi; /**< true when the whole Phi has been spilled and
	                                will be replaced with a PhiM. false if only
	                                the value of the Phi gets spilled */
};

struct spill_env_t {
	const arch_env_t *arch_env;
	ir_graph         *irg;
	ir_nodehashmap_t  spillmap;
	spill_info_t     *spills;
	spill_info_t     *mem_phis;
	struct obstack    obst;
	int               spill_cost;  /**< the cost of a single spill node */
	int               reload_cost; /**< the cost of a reload node */
	unsigned          spill_count;
	unsigned          reload_count;
	unsigned          remat_count;
	unsigned          spilled_phi_count;
};

/**
 * Returns spill info for a specific value (the value that is to be spilled)
 */
static spill_info_t *get_spillinfo(spill_env_t *env, ir_node *value)
{
	spill_info_t *info = ir_nodehashmap_get(spill_info_t, &env->spillmap,
	                                        value);
	if (info == NULL) {
		info = OALLOCZ(&env->obst, spill_info_t);
		info->to_spill    = value;
		info->spill_costs = -1;
		ir_nodehashmap_insert(&env->spillmap, value, info);

		info->next = env->spills;
		env->spills = info;
	}

	return info;
}

spill_env_t *be_new_spill_env(ir_graph *irg)
{
	const arch_env_t *arch_env = be_get_irg_arch_env(irg);

	spill_env_t *env = XMALLOCZ(spill_env_t);
	env->irg         = irg;
	env->arch_env    = arch_env;
	env->spill_cost  = arch_env->spill_cost;
	env->reload_cost = arch_env->reload_cost;
	ir_nodehashmap_init(&env->spillmap);
	obstack_init(&env->obst);
	return env;
}

void be_delete_spill_env(spill_env_t *env)
{
	ir_nodehashmap_destroy(&env->spillmap);
	obstack_free(&env->obst, NULL);
	free(env);
}

void be_add_spill(spill_env_t *env, ir_node *to_spill, ir_node *after)
{
	assert(!arch_irn_is(skip_Proj_const(to_spill), dont_spill));
	DB((dbg, LEVEL_1, "Add spill of %+F after %+F\n", to_spill, after));

	/* Just for safety make sure that we do not insert the spill in front of a phi */
	assert(!is_Phi(sched_next(after)));

	/* spills that are dominated by others are not needed */
	spill_info_t *spill_info = get_spillinfo(env, to_spill);
	for (spill_t *s = spill_info->spills, *last = NULL ; s != NULL;
	     s = s->next) {
		/* no need to add this spill if it is dominated by another */
		if (value_strictly_dominates(s->after, after)) {
			DB((dbg, LEVEL_1, "...dominated by %+F, not added\n", s->after));
			return;
		}
		/* remove spills that we dominate */
		if (value_strictly_dominates(after, s->after)) {
			DB((dbg, LEVEL_1, "...remove old spill at %+F\n", s->after));
			if (last != NULL) {
				last->next         = s->next;
			} else {
				spill_info->spills = s->next;
			}
		} else {
			last = s;
		}
	}

	spill_t *spill = OALLOC(&env->obst, spill_t);
	spill->after = after;
	spill->next  = spill_info->spills;
	spill->spill = NULL;
	spill_info->spills = spill;
}

void be_add_reload(spill_env_t *env, ir_node *to_spill, ir_node *before)
{
	assert(!arch_irn_is(skip_Proj_const(to_spill), dont_spill));
	assert(!be_is_Keep(before));

	spill_info_t *info = get_spillinfo(env, to_spill);
	if (is_Phi(to_spill)) {
		/* create spillinfos for the phi arguments */
		foreach_irn_in(to_spill, i, arg) {
			get_spillinfo(env, arg);
		}
	}

	/* put reload into list */
	reloader_t *rel       = OALLOC(&env->obst, reloader_t);
	rel->next             = info->reloaders;
	rel->reloader         = before;
	rel->rematted_node    = NULL;
	rel->remat_cost_delta = 0;

	info->reloaders = rel;

	DBG((dbg, LEVEL_1,
	     "creating spillinfo for %+F, will be reloaded before %+F\n",
	     to_spill, before));
}

ir_node *be_get_end_of_block_insertion_point(const ir_node *block)
{
	ir_node *last = sched_last(block);

	/* we might have keeps behind the jump... */
	while (be_is_Keep(last)) {
		last = sched_prev(last);
		assert(!sched_is_end(last));
	}

	assert(is_cfop(last));

	/* add the reload before the (cond-)jump */
	return last;
}

/**
 * determine final spill position: it should be after all phis, keep nodes
 * and behind nodes marked as prolog
 */
static ir_node *determine_spill_point(ir_node *const node)
{
	ir_node *n = skip_Proj(node);
	while (true) {
		ir_node *next = sched_next(n);
		if (!is_Phi(next) && !be_is_Keep(next) && !be_is_CopyKeep(next))
			break;
		n = next;
	}
	return n;
}

/**
 * Returns the point at which you can insert a node that should be executed
 * before block @p block when coming from pred @p pos.
 */
static ir_node *get_block_insertion_point(ir_node *block, int pos)
{
	/* simply add the reload to the beginning of the block if we only have 1
	 * predecessor. We don't need to check for phis as there can't be any in a
	 * block with only 1 pred. */
	if (get_Block_n_cfgpreds(block) == 1) {
		assert(!is_Phi(sched_first(block)));
		return sched_first(block);
	}

	/* We have to reload the value in pred-block */
	ir_node *predblock = get_Block_cfgpred_block(block, pos);
	return be_get_end_of_block_insertion_point(predblock);
}

void be_add_reload_on_edge(spill_env_t *env, ir_node *to_spill, ir_node *block,
                           int pos)
{
	ir_node *before = get_block_insertion_point(block, pos);
	be_add_reload(env, to_spill, before);
}

void be_spill_phi(spill_env_t *env, ir_node *node)
{
	assert(is_Phi(node));

	spill_info_t *info = get_spillinfo(env, node);
	info->spilled_phi  = true;
	info->next_mem_phi = env->mem_phis;
	env->mem_phis      = info;

	/* create spills for the phi arguments */
	ir_node *block = get_nodes_block(node);
	foreach_irn_in(node, i, arg) {
		/* some backends have virtual noreg/unknown nodes that are not scheduled
		 * and simply always available. */
		ir_node *insert;
		if (!sched_is_scheduled(arg)) {
			ir_node *pred_block = get_Block_cfgpred_block(block, i);
			insert = be_get_end_of_block_insertion_point(pred_block);
			insert = sched_prev(insert);
		} else {
			insert = determine_spill_point(arg);
		}
		be_add_spill(env, arg, insert);
	}
}

static void determine_spill_costs(spill_env_t *env, spill_info_t *spillinfo);

/**
 * Creates a spill.
 *
 * @param senv      the spill environment
 * @param irn       the node that should be spilled
 * @param ctx_irn   an user of the spilled node
 * @return a be_Spill node
 */
static void spill_irn(spill_env_t *env, spill_info_t *spillinfo)
{
	/* determine_spill_costs must have been run before */
	assert(spillinfo->spill_costs >= 0);

	/* some backends have virtual noreg/unknown nodes that are not scheduled
	 * and simply always available. */
	ir_node       *to_spill = spillinfo->to_spill;
	const ir_node *insn     = skip_Proj_const(to_spill);
	if (!sched_is_scheduled(insn)) {
		/* override spillinfos or create a new one */
		ir_graph *irg = get_irn_irg(to_spill);
		spillinfo->spills->spill = get_irg_no_mem(irg);
		DB((dbg, LEVEL_1, "don't spill %+F use NoMem\n", to_spill));
		return;
	}

	DBG((dbg, LEVEL_1, "spilling %+F ... \n", to_spill));
	for (spill_t *spill = spillinfo->spills; spill != NULL;
	     spill = spill->next) {
		ir_node *after = spill->after;
		after = determine_spill_point(after);

		spill->spill = arch_env_new_spill(env->arch_env, to_spill, after);
		DB((dbg, LEVEL_1, "\t%+F after %+F\n", spill->spill, after));
		env->spill_count++;
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
	assert(!get_opt_cse());
	ir_node *phi = spillinfo->to_spill;
	DBG((dbg, LEVEL_1, "spilling Phi %+F:\n", phi));

	/* build a new PhiM */
	int       const arity   = get_Phi_n_preds(phi);
	ir_node **const ins     = ALLOCAN(ir_node*, arity);
	ir_graph *const irg     = env->irg;
	ir_node  *const unknown = new_r_Unknown(irg, mode_M);
	for (int i = 0; i < arity; ++i) {
		ins[i] = unknown;
	}

	/* override or replace spills list... */
	ir_node *block = get_nodes_block(phi);
	spill_t *spill = OALLOC(&env->obst, spill_t);
	spill->after   = determine_spill_point(phi);
	spill->spill   = be_new_Phi(block, arity, ins, mode_M, arch_no_register_req);
	spill->next    = NULL;
	sched_add_after(block, spill->spill);

	spillinfo->spills = spill;
	env->spilled_phi_count++;

	foreach_irn_in(phi, i, arg) {
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
	/* node is already spilled */
	if (spillinfo->spills != NULL && spillinfo->spills->spill != NULL)
		return;

	if (spillinfo->spilled_phi) {
		spill_phi(env, spillinfo);
	} else {
		spill_irn(env, spillinfo);
	}
}

/**
 * Tests whether value @p arg is available before node @p reloader
 * @returns true if value is available
 */
static bool is_value_available(spill_env_t *env, const ir_node *arg)
{
	if (is_Unknown(arg) || is_NoMem(arg))
		return true;
	if (arch_irn_is(skip_Proj_const(arg), spill))
		return true;
	if (arg == get_irg_frame(env->irg))
		return true;
	if (get_irn_mode(arg) == mode_T)
		return false;
	/* "Ignore registers" are always available */
	if (arch_irn_is_ignore(arg))
		return true;

	return false;
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
                                        const ir_node *spilled,
                                        const ir_node *reloader,
                                        int parentcosts)
{
	const ir_node *insn = skip_Proj_const(spilled);

	assert(!arch_irn_is(insn, spill));
	if (!arch_irn_is(insn, rematerializable))
		return REMAT_COST_INFINITE;

	int costs = arch_get_op_estimated_cost(insn);
	if (parentcosts + costs >= env->reload_cost + env->spill_cost)
		return REMAT_COST_INFINITE;

	/* never rematerialize a node which modifies the flags.
	 * (would be better to test whether the flags are actually live at point
	 * reloader...)
	 */
	if (arch_irn_is(insn, modify_flags))
		return REMAT_COST_INFINITE;

	int argremats = 0;
	foreach_irn_in(insn, i, arg) {
		if (is_value_available(env, arg))
			continue;

		/* we have to rematerialize the argument as well */
		++argremats;
		if (argremats > 1) {
			/* we only support rematerializing 1 argument at the moment,
			 * as multiple arguments could increase register pressure */
			return REMAT_COST_INFINITE;
		}

		costs += check_remat_conditions_costs(env, arg, reloader,
		                                      parentcosts + costs);
		if (parentcosts + costs >= env->reload_cost + env->spill_cost)
			return REMAT_COST_INFINITE;
	}

	return costs;
}

/**
 * Re-materialize a node.
 *
 * @param env       the spill environment
 * @param spilled   the node that was spilled
 * @param reloader  a irn that requires a reload
 */
static ir_node *do_remat(spill_env_t *env, ir_node *spilled, ir_node *reloader)
{
	ir_node **ins = ALLOCAN(ir_node*, get_irn_arity(spilled));
	foreach_irn_in(spilled, i, arg) {
		if (is_value_available(env, arg)) {
			ins[i] = arg;
		} else {
			ins[i] = do_remat(env, arg, reloader);
			/* don't count the argument rematerialization as an extra remat */
			--env->remat_count;
		}
	}

	/* create a copy of the node */
	ir_node *const bl  = get_nodes_block(reloader);
	ir_node *const res = new_similar_node(spilled, bl, ins);
	arch_env_mark_remat(env->arch_env, res);

	DBG((dbg, LEVEL_1, "Insert remat %+F of %+F before reloader %+F\n", res,
	     spilled, reloader));

	if (!is_Proj(res)) {
		/* insert in schedule */
		sched_reset(res);
		sched_add_before(reloader, res);
		++env->remat_count;
	}

	return res;
}

double be_get_spill_costs(spill_env_t *env, ir_node *to_spill, ir_node *before)
{
	(void)to_spill;
	ir_node *block = get_nodes_block(before);
	double   freq  = get_block_execfreq(block);
	return env->spill_cost * freq;
}

unsigned be_get_reload_costs_no_weight(spill_env_t *env,
                                       const ir_node *to_spill,
                                       const ir_node *before)
{
	if (be_do_remats) {
		/* is the node rematerializable? */
		unsigned costs = check_remat_conditions_costs(env, to_spill, before, 0);
		if (costs < (unsigned) env->reload_cost)
			return costs;
	}

	return env->reload_cost;
}

double be_get_reload_costs(spill_env_t *env, ir_node *to_spill, ir_node *before)
{
	ir_node *block = get_nodes_block(before);
	double   freq  = get_block_execfreq(block);

	if (be_do_remats) {
		/* is the node rematerializable? */
		int costs = check_remat_conditions_costs(env, to_spill, before, 0);
		if (costs < env->reload_cost)
			return costs * freq;
	}

	return env->reload_cost * freq;
}

bool be_is_rematerializable(spill_env_t *env, const ir_node *to_remat,
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

/**
 * analyzes how to best spill a node and determine costs for that
 */
static void determine_spill_costs(spill_env_t *env, spill_info_t *spillinfo)
{
	/* already calculated? */
	if (spillinfo->spill_costs >= 0)
		return;

	ir_node       *to_spill = spillinfo->to_spill;
	const ir_node *insn     = skip_Proj_const(to_spill);
	assert(!arch_irn_is(insn, dont_spill));
	assert(!arch_irn_is(insn, reload));

	/* some backends have virtual noreg/unknown nodes that are not scheduled
	 * and simply always available.
	 * TODO: this is kinda hairy, the NoMem is correct for an Unknown as Phi
	 * predecessor (of a PhiM) but this test might match other things too...
	 */
	if (!sched_is_scheduled(insn)) {
		ir_graph *irg = get_irn_irg(to_spill);
		/* override spillinfos or create a new one */
		spill_t *spill = OALLOC(&env->obst, spill_t);
		spill->after = NULL;
		spill->next  = NULL;
		spill->spill = get_irg_no_mem(irg);

		spillinfo->spills      = spill;
		spillinfo->spill_costs = 0;

		DB((dbg, LEVEL_1, "don't spill %+F use NoMem\n", to_spill));
		return;
	}

	ir_node *spill_block    = get_nodes_block(insn);
	double   spill_execfreq = get_block_execfreq(spill_block);

	if (spillinfo->spilled_phi) {
		/* TODO calculate correct costs...
		 * (though we can't remat this node anyway so no big problem) */
		spillinfo->spill_costs = env->spill_cost * spill_execfreq;
		return;
	}

	if (spillinfo->spills != NULL) {
		/* calculate sum of execution frequencies of individual spills */
		double spills_execfreq = 0;
		for (spill_t *s = spillinfo->spills; s != NULL; s = s->next) {
			ir_node *spill_block = get_block(s->after);
			double   freq        = get_block_execfreq(spill_block);

			spills_execfreq += freq;
		}

		DB((dbg, LEVEL_1, "%+F: latespillcosts %f after def: %f\n", to_spill,
		    spills_execfreq * env->spill_cost,
		    spill_execfreq * env->spill_cost));

		/* multi-/latespill is advantageous -> return*/
		if (spills_execfreq < spill_execfreq) {
			DB((dbg, LEVEL_1, "use latespills for %+F\n", to_spill));
			spillinfo->spill_costs = spills_execfreq * env->spill_cost;
			return;
		}
	}

	/* override spillinfos or create a new one */
	spill_t *spill = OALLOC(&env->obst, spill_t);
	spill->after   = determine_spill_point(to_spill);
	spill->next    = NULL;
	spill->spill   = NULL;

	spillinfo->spills      = spill;
	spillinfo->spill_costs = spill_execfreq * env->spill_cost;
	DB((dbg, LEVEL_1, "spill %+F after definition\n", to_spill));
}

void make_spill_locations_dominate_irn(spill_env_t *env, ir_node *irn)
{
	const spill_info_t *si = get_spillinfo(env, irn);
	if (si == NULL)
		return;

	/* Fill the bitset with the dominance pre-order numbers
	 * of the blocks the reloads are located in. */
	ir_node  *start_block = get_irg_start_block(get_irn_irg(irn));
	unsigned  n_blocks    = get_Block_dom_max_subtree_pre_num(start_block);
	bitset_t *reloads     = bitset_alloca(n_blocks);
	for (reloader_t *r = si->reloaders; r != NULL; r = r->next) {
		ir_node *block = get_nodes_block(r->reloader);
		bitset_set(reloads, get_Block_dom_tree_pre_num(block));
	}

	/* Now, cancel out all the blocks that are dominated by each spill.
	 * If the bitset is not empty after that, we have reloads that are
	 * not dominated by any spill. */
	for (spill_t *s = si->spills; s != NULL; s = s->next) {
		ir_node *block = get_nodes_block(s->after);
		unsigned start = get_Block_dom_tree_pre_num(block);
		unsigned end   = get_Block_dom_max_subtree_pre_num(block);
		bitset_clear_range(reloads, start, end);
	}

	if (!bitset_is_empty(reloads))
		be_add_spill(env, si->to_spill, si->to_spill);
}

void be_insert_spills_reloads(spill_env_t *env)
{
	be_timer_push(T_RA_SPILL_APPLY);

	/* create all phi-ms first, this is needed so, that phis, hanging on
	   spilled phis work correctly */
	for (spill_info_t *info = env->mem_phis; info != NULL;
	     info = info->next_mem_phi) {
		spill_node(env, info);
	}

	/* process each spilled node */
	for (spill_info_t *si = env->spills; si != NULL; si = si->next) {
		ir_node  *to_spill        = si->to_spill;
		ir_node **copies          = NEW_ARR_F(ir_node*, 0);
		double    all_remat_costs = 0; /** costs when we would remat all nodes */
		bool      force_remat     = false;

		DBG((dbg, LEVEL_1, "\nhandling all reloaders of %+F:\n", to_spill));

		determine_spill_costs(env, si);

		/* determine possibility of rematerialisations */
		if (be_do_remats) {
			/* calculate cost savings for each individual value when it would
			   be rematerialised instead of reloaded */
			for (reloader_t *rld = si->reloaders; rld != NULL;
			     rld = rld->next) {
				ir_node *reloader = rld->reloader;
				if (rld->rematted_node != NULL) {
					DBG((dbg, LEVEL_2, "\tforced remat %+F before %+F\n",
					     rld->rematted_node, reloader));
					continue;
				}
				if (rld->remat_cost_delta >= REMAT_COST_INFINITE) {
					DBG((dbg, LEVEL_2, "\treload before %+F is forbidden\n",
					     reloader));
					all_remat_costs = REMAT_COST_INFINITE;
					continue;
				}

				int remat_cost = check_remat_conditions_costs(env, to_spill,
				                                              reloader, 0);
				if (remat_cost >= REMAT_COST_INFINITE) {
					DBG((dbg, LEVEL_2, "\tremat before %+F not possible\n",
					     reloader));
					rld->remat_cost_delta = REMAT_COST_INFINITE;
					all_remat_costs       = REMAT_COST_INFINITE;
					continue;
				}

				int remat_cost_delta  = remat_cost - env->reload_cost;
				rld->remat_cost_delta = remat_cost_delta;
				ir_node *block        = get_block(reloader);
				double   freq         = get_block_execfreq(block);
				all_remat_costs      += remat_cost_delta * freq;
				DBG((dbg, LEVEL_2, "\tremat costs delta before %+F: "
				     "%d (rel %f)\n", reloader, remat_cost_delta,
				     remat_cost_delta * freq));
			}
			if (all_remat_costs < REMAT_COST_INFINITE) {
				/* we don't need the costs for the spill if we can remat
				   all reloaders */
				all_remat_costs -= si->spill_costs;
				DBG((dbg, LEVEL_2, "\tspill costs %d (rel %f)\n",
				     env->spill_cost, si->spill_costs));
			}

			if (all_remat_costs < 0) {
				force_remat = true;
				DBG((dbg, LEVEL_1, "\nforcing remats of all reloaders (%f)\n",
				     all_remat_costs));
			}
		}

		/* go through all reloads for this spill */
		for (reloader_t *rld = si->reloaders; rld != NULL; rld = rld->next) {
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
				copy = arch_env_new_reload(env->arch_env, si->to_spill,
				                           si->spills->spill, rld->reloader);
				env->reload_count++;
			}

			DBG((dbg, LEVEL_1, " %+F of %+F before %+F\n",
			     copy, to_spill, rld->reloader));
			ARR_APP1(ir_node*, copies, copy);
		}

		/* if we had any reloads or remats, then we need to reconstruct the
		 * SSA form for the spilled value */
		if (ARR_LEN(copies) > 0) {
			be_ssa_construction_env_t senv;
			be_ssa_construction_init(&senv, env->irg);
			be_ssa_construction_add_copy(&senv, to_spill);
			be_ssa_construction_add_copies(&senv, copies, ARR_LEN(copies));
			be_ssa_construction_fix_users(&senv, to_spill);
			be_ssa_construction_destroy(&senv);
		}
		/* need to reconstruct SSA form if we had multiple spills */
		if (si->spills != NULL && si->spills->next != NULL) {
			be_ssa_construction_env_t senv;
			be_ssa_construction_init(&senv, env->irg);
			unsigned spill_count = 0;
			for (spill_t *spill = si->spills ; spill != NULL;
			     spill = spill->next) {
				/* maybe we rematerialized the value and need no spill */
				if (spill->spill == NULL)
					continue;
				be_ssa_construction_add_copy(&senv, spill->spill);
				++spill_count;
			}
			if (spill_count > 1) {
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
	be_invalidate_live_sets(env->irg);

	be_remove_dead_nodes_from_schedule(env->irg);

	be_timer_pop(T_RA_SPILL_APPLY);
}

static be_irg_t      *birg;
static be_lv_t       *lv;
static unsigned long  precol_copies;
static unsigned long  multi_precol_copies;
static unsigned long  constrained_livethrough_copies;

static void prepare_constr_insn(ir_node *const node)
{
	/* Insert a copy for constraint inputs attached to a value which can't
	 * fulfill the constraint
	 * (typical example: stack pointer as input to copyb)
	 * TODO: This really just checks precolored registers at the moment and
	 *       ignores the general case of not matching in/out constraints */
	foreach_irn_in(node, i, op) {
		const arch_register_req_t *const req
			= arch_get_irn_register_req_in(node, i);
		if (req->cls == NULL)
			continue;

		const arch_register_t *const reg = arch_get_irn_register(op);
		if (reg == NULL)
			continue;

		/* Precolored with an ignore register (which is not virtual). */
		if ((reg->type & arch_register_type_virtual) ||
		    rbitset_is_set(birg->allocatable_regs, reg->global_index))
			continue;

		if (!arch_register_req_is(req, limited))
			continue;
		if (rbitset_is_set(req->limited, reg->index))
			continue;

		ir_node *block = get_nodes_block(node);
		ir_node *copy  = be_new_Copy(block, op);
		sched_add_before(node, copy);
		set_irn_n(node, i, copy);
		++precol_copies;
		DBG((dbg_constr, LEVEL_3, "inserting ignore arg copy %+F for %+F pos %d\n",
		     copy, node, i));
	}

	/* insert copies for nodes that occur constrained more than once. */
	for (int i = 0, arity = get_irn_arity(node); i < arity; ++i) {
		const arch_register_req_t *const req
			= arch_get_irn_register_req_in(node, i);
		const arch_register_class_t *const cls = req->cls;
		if (cls == NULL)
			continue;
		if (!arch_register_req_is(req, limited))
			continue;

		ir_node *in = get_irn_n(node, i);
		const arch_register_req_t *const in_req
			= arch_get_irn_register_req(in);
		if (arch_register_req_is(in_req, ignore))
			continue;
		for (int i2 = i + 1; i2 < arity; ++i2) {
			const arch_register_req_t *const req2
				= arch_get_irn_register_req_in(node, i2);
			if (req2->cls != cls)
				continue;
			if (!arch_register_req_is(req2, limited))
				continue;

			ir_node *in2 = get_irn_n(node, i2);
			if (in2 != in)
				continue;

			/* if the constraint is the same, no copy is necessary
			 * TODO generalise to unequal but overlapping constraints */
			if (rbitsets_equal(req->limited, req2->limited, cls->n_regs))
				continue;

			ir_node *block = get_nodes_block(node);
			ir_node *copy  = be_new_Copy(block, in);
			sched_add_before(node, copy);
			set_irn_n(node, i2, copy);
			++multi_precol_copies;
			DBG((dbg_constr, LEVEL_3,
			     "inserting multiple constr copy %+F for %+F pos %d\n",
			     copy, node, i2));
		}
	}

	/* collect all registers occurring in out constraints. */
	unsigned *def_constr = NULL;
	be_foreach_value(node, value,
		const arch_register_req_t *const req = arch_get_irn_register_req(value);
		const arch_register_class_t *const cls = req->cls;
		if (cls == NULL)
			continue;
		if (!arch_register_req_is(req, limited))
			continue;
		if (def_constr == NULL) {
			const arch_env_t *const arch_env = birg->main_env->arch_env;
			def_constr = rbitset_alloca(arch_env->n_registers);
		}
		rbitset_foreach(req->limited, cls->n_regs, e) {
			const arch_register_t *reg = arch_register_for_index(cls, e);
			rbitset_set(def_constr, reg->global_index);
		}
	);
	/* no output constraints => we're good */
	if (def_constr == NULL)
		return;

	/* Insert copies for all constrained arguments living through the node and
	 * being constrained to a register which also occurs in out constraints. */
	for (int i = 0, arity = get_irn_arity(node); i < arity; ++i) {
		/* Check, if
		 * 1) the operand is constrained.
		 * 2) lives through the node.
		 * 3) is constrained to a register occurring in out constraints. */
		const arch_register_req_t *const req
			= arch_get_irn_register_req_in(node, i);
		const arch_register_class_t *const cls = req->cls;
		if (cls == NULL)
			continue;
		if (!arch_register_req_is(req, limited))
			continue;
		ir_node *in = get_irn_n(node, i);
		const arch_register_req_t *const in_req
			= arch_get_irn_register_req(in);
		if (arch_register_req_is(in_req, ignore))
			continue;
		/* Only create the copy if the operand is no copy.
		 * this is necessary since the assure constraints phase inserts
		 * Copies and Keeps for operands which must be different from the
		 * results. Additional copies here would destroy this. */
		if (be_is_Copy(in))
			continue;
		if (!be_value_live_after(in, node))
			continue;

		bool common_limits = false;
		rbitset_foreach(req->limited, cls->n_regs, e) {
			const arch_register_t *reg = arch_register_for_index(cls, e);
			if (rbitset_is_set(def_constr, reg->global_index)) {
				common_limits = true;
				break;
			}
		}
		if (!common_limits)
			continue;

		ir_node *block = get_nodes_block(node);
		ir_node *copy  = be_new_Copy(block, in);
		sched_add_before(node, copy);
		set_irn_n(node, i, copy);
		++constrained_livethrough_copies;
		DBG((dbg_constr, LEVEL_3, "inserting constr copy %+F for %+F pos %d\n",
		     copy, node, i));
		be_liveness_update(lv, in);
	}
}

static void add_missing_copies_in_block(ir_node *block, void *data)
{
	(void)data;
	sched_foreach(block, node) {
		prepare_constr_insn(node);
	}
}

static bool has_irn_users(const ir_node *irn)
{
	return get_irn_out_edge_first_kind(irn, EDGE_KIND_NORMAL) != 0;
}

static ir_node *find_copy(ir_node *irn, ir_node *op)
{
	for (ir_node *cur_node = irn;;) {
		cur_node = sched_prev(cur_node);
		if (!be_is_Copy(cur_node))
			return NULL;
		if (be_get_Copy_op(cur_node) == op && arch_irn_is(cur_node, dont_spill))
			return cur_node;
	}
}

/** Environment for constraints. */
typedef struct {
	ir_graph        *irg;
	ir_nodehashmap_t op_set;
	struct obstack   obst;
} constraint_env_t;

/** Associates an ir_node with its copy and CopyKeep. */
typedef struct {
	ir_nodeset_t copies; /**< all non-spillable copies of this irn */
	const arch_register_class_t *cls;
} op_copy_assoc_t;

static void gen_assure_different_pattern(ir_node *irn, ir_node *other_different, constraint_env_t *env)
{
	arch_register_req_t const *const req = arch_get_irn_register_req(other_different);
	if (arch_register_req_is(req, ignore) ||
			!mode_is_data(get_irn_mode(other_different))) {
		DB((dbg_constr, LEVEL_1, "ignore constraint for %+F because other_irn is ignore or not a data node\n", irn));
		return;
	}

	ir_nodehashmap_t            *op_set = &env->op_set;
	ir_node                     *block  = get_nodes_block(irn);
	const arch_register_class_t *cls    = req->cls;

	/* Make a not spillable copy of the different node   */
	/* this is needed because the different irn could be */
	/* in block far far away                             */
	/* The copy is optimized later if not needed         */

	/* check if already exists such a copy in the schedule immediately before */
	ir_node *cpy = find_copy(skip_Proj(irn), other_different);
	if (cpy == NULL) {
		cpy = be_new_Copy(block, other_different);
		arch_set_irn_flags(cpy, arch_irn_flag_dont_spill);
		DB((dbg_constr, LEVEL_1, "created non-spillable %+F for value %+F\n", cpy, other_different));
	} else {
		DB((dbg_constr, LEVEL_1, "using already existing %+F for value %+F\n", cpy, other_different));
	}

	/* Add the Keep resp. CopyKeep and reroute the users */
	/* of the other_different irn in case of CopyKeep.   */
	ir_node *keep;
	if (has_irn_users(other_different)) {
		keep = be_new_CopyKeep_single(block, cpy, irn);
		be_node_set_reg_class_in(keep, 1, cls);
	} else {
		ir_node *in[] = { irn, cpy };
		keep = be_new_Keep(block, ARRAY_SIZE(in), in);
	}

	DB((dbg_constr, LEVEL_1, "created %+F(%+F, %+F)\n\n", keep, irn, cpy));

	/* insert copy and keep into schedule */
	assert(sched_is_scheduled(irn) && "need schedule to assure constraints");
	if (!sched_is_scheduled(cpy))
		sched_add_before(skip_Proj(irn), cpy);
	sched_add_after(skip_Proj(irn), keep);

	/* insert the other different and its copies into the map */
	op_copy_assoc_t *entry
		= ir_nodehashmap_get(op_copy_assoc_t, op_set, other_different);
	if (entry == NULL) {
		entry      = OALLOC(&env->obst, op_copy_assoc_t);
		entry->cls = cls;
		ir_nodeset_init(&entry->copies);

		ir_nodehashmap_insert(op_set, other_different, entry);
	}

	/* insert copy */
	ir_nodeset_insert(&entry->copies, cpy);

	/* insert keep in case of CopyKeep */
	if (be_is_CopyKeep(keep))
		ir_nodeset_insert(&entry->copies, keep);
}

/**
 * Checks if node has a must_be_different constraint in output and adds a Keep
 * then to assure the constraint.
 *
 * @param irn          the node to check
 * @param skipped_irn  if irn is a Proj node, its predecessor, else irn
 * @param env          the constraint environment
 */
static void assure_different_constraints(ir_node *irn, ir_node *skipped_irn, constraint_env_t *env)
{
	const arch_register_req_t *req = arch_get_irn_register_req(irn);

	if (arch_register_req_is(req, must_be_different)) {
		const unsigned other = req->other_different;

		if (arch_register_req_is(req, should_be_same)) {
			const unsigned same = req->other_same;

			if (is_po2(other) && is_po2(same)) {
				int idx_other = ntz(other);
				int idx_same  = ntz(same);

				/*
				 * We can safely ignore a should_be_same x must_be_different y
				 * IFF both inputs are equal!
				 */
				if (get_irn_n(skipped_irn, idx_other) == get_irn_n(skipped_irn, idx_same)) {
					return;
				}
			}
		}
		for (unsigned i = 0; 1U << i <= other; ++i) {
			if (other & (1U << i)) {
				ir_node *different_from = get_irn_n(skipped_irn, i);
				gen_assure_different_pattern(irn, different_from, env);
			}
		}
	}
}

/**
 * Calls the functions to assure register constraints.
 *
 * @param block    The block to be checked
 * @param walk_env The walker environment
 */
static void assure_constraints_walker(ir_node *block, void *walk_env)
{
	constraint_env_t *env = (constraint_env_t*)walk_env;

	sched_foreach_reverse(block, irn) {
		be_foreach_value(irn, value,
			if (mode_is_data(get_irn_mode(value)))
				assure_different_constraints(value, irn, env);
		);
	}
}

/**
 * Melt all copykeeps pointing to the same node
 * (or Projs of the same node), copying the same operand.
 */
static void melt_copykeeps(constraint_env_t *cenv)
{
	struct obstack obst;
	obstack_init(&obst);

	/* for all */
	ir_nodehashmap_entry_t    map_entry;
	ir_nodehashmap_iterator_t map_iter;
	foreach_ir_nodehashmap(&cenv->op_set, map_entry, map_iter) {
		op_copy_assoc_t *entry = (op_copy_assoc_t*)map_entry.data;

		/* collect all copykeeps */
		unsigned num_ck = 0;
		foreach_ir_nodeset(&entry->copies, cp, iter) {
			if (be_is_CopyKeep(cp)) {
				obstack_grow(&obst, &cp, sizeof(cp));
				++num_ck;
			}
		}

		/* compare each copykeep with all other copykeeps */
		ir_node **ck_arr = (ir_node **)obstack_finish(&obst);
		for (unsigned idx = 0; idx < num_ck; ++idx) {
			if (ck_arr[idx] == NULL)
				continue;
			unsigned n_melt     = 1;
			ir_node *ref        = ck_arr[idx];
			ir_node *ref_mode_T = skip_Proj(get_irn_n(ref, 1));
			obstack_grow(&obst, &ref, sizeof(ref));

			DB((dbg_constr, LEVEL_1, "Trying to melt %+F:\n", ref));

			/* check for copykeeps pointing to the same mode_T node as the reference copykeep */
			for (unsigned j = 0; j < num_ck; ++j) {
				if (j == idx)
					continue;
				ir_node *cur_ck = ck_arr[j];
				if (cur_ck == NULL || skip_Proj(get_irn_n(cur_ck, 1)) != ref_mode_T)
					continue;

				obstack_grow(&obst, &cur_ck, sizeof(cur_ck));
				ir_nodeset_remove(&entry->copies, cur_ck);
				DB((dbg_constr, LEVEL_1, "\t%+F\n", cur_ck));
				ck_arr[j] = NULL;
				++n_melt;
				sched_remove(cur_ck);
			}
			ck_arr[idx] = NULL;

			/* check, if we found some candidates for melting */
			if (n_melt == 1) {
				DB((dbg_constr, LEVEL_1, "\tno candidate found\n"));
				continue;
			}

			ir_nodeset_remove(&entry->copies, ref);
			sched_remove(ref);

			ir_node **melt_arr = (ir_node **)obstack_finish(&obst);
			/* melt all found copykeeps */
			ir_node **new_ck_in = ALLOCAN(ir_node*,n_melt);
			for (unsigned j = 0; j < n_melt; ++j) {
				new_ck_in[j] = get_irn_n(melt_arr[j], 1);

				/* now, we can kill the melted keep, except the */
				/* ref one, we still need some information      */
				if (melt_arr[j] != ref)
					kill_node(melt_arr[j]);
			}

			ir_node *const new_ck = be_new_CopyKeep(get_nodes_block(ref), be_get_CopyKeep_op(ref), n_melt, new_ck_in);

			/* set register class for all kept inputs */
			for (unsigned j = 1; j <= n_melt; ++j) {
				be_node_set_reg_class_in(new_ck, j, entry->cls);
			}

			ir_nodeset_insert(&entry->copies, new_ck);

			/* find scheduling point */
			ir_node *sched_pt = ref_mode_T;
			do {
				/* just walk along the schedule until a non-Keep/CopyKeep node is found */
				sched_pt = sched_next(sched_pt);
			} while (be_is_Keep(sched_pt) || be_is_CopyKeep(sched_pt));

			sched_add_before(sched_pt, new_ck);
			DB((dbg_constr, LEVEL_1, "created %+F, scheduled before %+F\n", new_ck, sched_pt));

			/* finally: kill the reference copykeep */
			kill_node(ref);
		}
		obstack_free(&obst, ck_arr);
	}
	obstack_free(&obst, NULL);
}

static ir_node *add_to_keep(ir_node *last_keep,
                            const arch_register_class_t *cls, ir_node *node)
{
	if (last_keep != NULL) {
		be_Keep_add_node(last_keep, cls, node);
	} else {
		ir_node *in[1] = { node };
		ir_node *block = get_nodes_block(node);
		ir_node *schedpoint;
		last_keep = be_new_Keep(block, 1, in);

		schedpoint = skip_Proj(node);
		if (sched_is_scheduled(schedpoint)) {
			sched_add_after(schedpoint, last_keep);
		}
	}
	return last_keep;
}

/**
 * Tests whether a node has a real user and is not just kept by the End or
 * Anchor node
 */
static bool has_real_user(const ir_node *node)
{
	foreach_out_edge(node, edge) {
		ir_node *user = get_edge_src_irn(edge);
		if (!is_End(user) && !is_Anchor(user))
			return true;
	}
	return false;
}

static void add_missing_keep_walker(ir_node *node, void *data)
{
	(void)data;
	ir_mode *mode = get_irn_mode(node);
	ir_node *last_keep;

	if (mode != mode_T) {
		if (!has_real_user(node)) {
			const arch_register_req_t   *req = arch_get_irn_register_req(node);
			const arch_register_class_t *cls = req->cls;
			if (cls == NULL
					|| (cls->flags & arch_register_class_flag_manual_ra)) {
				return;
			}

			add_to_keep(NULL, cls, node);
		}
		return;
	}

	unsigned n_outs = arch_get_irn_n_outs(node);
	if (n_outs <= 0)
		return;

	unsigned *const found_projs    = rbitset_alloca(n_outs);
	ir_node **const existing_projs = ALLOCANZ(ir_node*, n_outs);
	foreach_out_edge(node, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		ir_mode *mode = get_irn_mode(succ);

		/* The node could be kept */
		if (is_End(succ) || is_Anchor(succ))
			continue;
		if (mode == mode_M || mode == mode_X)
			continue;
		unsigned pn = get_Proj_num(succ);
		existing_projs[pn] = succ;
		if (!has_real_user(succ))
			continue;

		assert(pn < n_outs);
		rbitset_set(found_projs, pn);
	}

	/* are keeps missing? */
	last_keep = NULL;
	for (unsigned i = 0; i < n_outs; ++i) {
		ir_node                     *value;
		const arch_register_req_t   *req;
		const arch_register_class_t *cls;

		if (rbitset_is_set(found_projs, i)) {
			continue;
		}

		req = arch_get_irn_register_req_out(node, i);
		cls = req->cls;
		if (cls == NULL || (cls->flags & arch_register_class_flag_manual_ra)) {
			continue;
		}

		value = existing_projs[i];
		if (value == NULL)
			value = new_r_Proj(node, cls->mode, i);
		last_keep = add_to_keep(last_keep, cls, value);
	}
}

void be_spill_prepare_for_constraints(ir_graph *irg)
{
	FIRM_DBG_REGISTER(dbg_constr, "firm.be.lower.constr");

	irg_walk_graph(irg, add_missing_keep_walker, NULL, NULL);

	constraint_env_t cenv;
	cenv.irg = irg;
	ir_nodehashmap_init(&cenv.op_set);
	obstack_init(&cenv.obst);

	irg_block_walk_graph(irg, NULL, assure_constraints_walker, &cenv);

	/* melt copykeeps, pointing to projs of */
	/* the same mode_T node and keeping the */
	/* same operand                         */
	melt_copykeeps(&cenv);

	/* for all */
	ir_nodehashmap_iterator_t map_iter;
	ir_nodehashmap_entry_t    map_entry;
	foreach_ir_nodehashmap(&cenv.op_set, map_entry, map_iter) {
		op_copy_assoc_t *entry    = (op_copy_assoc_t*)map_entry.data;
		size_t           n_copies = ir_nodeset_size(&entry->copies);
		ir_node        **nodes    = ALLOCAN(ir_node*, n_copies);

		/* put the node in an array */
		DBG((dbg_constr, LEVEL_1, "introduce copies for %+F ", map_entry.node));

		/* collect all copies */
		size_t n = 0;
		foreach_ir_nodeset(&entry->copies, cp, iter) {
			nodes[n++] = cp;
			DB((dbg_constr, LEVEL_1, ", %+F ", cp));
		}

		DB((dbg_constr, LEVEL_1, "\n"));

		/* introduce the copies for the operand and its copies */
		be_ssa_construction_env_t senv;
		be_ssa_construction_init(&senv, irg);
		be_ssa_construction_add_copy(&senv, map_entry.node);
		be_ssa_construction_add_copies(&senv, nodes, n);
		be_ssa_construction_fix_users(&senv, map_entry.node);
		be_ssa_construction_destroy(&senv);

		/* Could be that not all CopyKeeps are really needed, */
		/* so we transform unnecessary ones into Keeps.       */
		foreach_ir_nodeset(&entry->copies, cp, iter) {
			if (be_is_CopyKeep(cp) && get_irn_n_edges(cp) < 1) {
				int      arity = get_irn_arity(cp);
				ir_node *block = get_nodes_block(cp);
				ir_node *keep  = be_new_Keep(block, arity, get_irn_in(cp) + 1);
				sched_replace(cp, keep);

				/* Set all ins (including the block) of the CopyKeep BAD to keep the verifier happy. */
				kill_node(cp);
			}
		}

		ir_nodeset_destroy(&entry->copies);
	}

	ir_nodehashmap_destroy(&cenv.op_set);
	obstack_free(&cenv.obst, NULL);
	be_invalidate_live_sets(irg);

	/* part2: add missing copies */
	precol_copies                  = 0;
	multi_precol_copies            = 0;
	constrained_livethrough_copies = 0;
	be_assure_live_sets(irg);
	birg = be_birg_from_irg(irg);
	lv   = be_get_irg_liveness(irg);
	irg_block_walk_graph(irg, add_missing_copies_in_block, NULL, NULL);

	stat_ev_ull("ra_precol_copies", precol_copies);
	stat_ev_ull("ra_multi_precol_copies", multi_precol_copies);
	stat_ev_ull("ra_constrained_livethrough_copies",
	            constrained_livethrough_copies);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spill)
void be_init_spill(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.spill");
}
