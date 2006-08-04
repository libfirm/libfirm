/*
 * Author:      Daniel Grund, Sebastian Hack, Matthias Braun
 * Date:		29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "pset.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "ident_t.h"
#include "type_t.h"
#include "entity_t.h"
#include "debug.h"
#include "irgwalk.h"
#include "array.h"
#include "pdeq.h"
#include "unionfind.h"
#include "execfreq.h"

#include "belive_t.h"
#include "besched_t.h"
#include "bespill.h"
#include "belive_t.h"
#include "benode_t.h"
#include "bechordal_t.h"
#include "bejavacoal.h"

/* This enables re-computation of values. Current state: Unfinished and buggy. */
#undef BUGGY_REMAT

typedef struct _reloader_t reloader_t;

struct _reloader_t {
	reloader_t *next;
	ir_node *reloader;
};

typedef struct _spill_info_t {
	ir_node *spilled_node;
	reloader_t *reloaders;

	ir_node *spill;
} spill_info_t;

struct _spill_env_t {
	const arch_register_class_t *cls;
	const be_chordal_env_t *chordal_env;
	struct obstack obst;
	set *spills;				/**< all spill_info_t's, which must be placed */
	pset *mem_phis;				/**< set of all special spilled phis. allocated and freed separately */

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
 * Returns spill info for a specific value (the value that is to be spilled)
 */
static spill_info_t *get_spillinfo(const spill_env_t *env, ir_node *value) {
	spill_info_t info, *res;
	int hash = HASH_PTR(value);

	info.spilled_node = value;
	res = set_find(env->spills, &info, sizeof(info), hash);

	if (res == NULL) {
		info.reloaders = NULL;
		info.spill = NULL;
		res = set_insert(env->spills, &info, sizeof(info), hash);
	}

	return res;
}

DEBUG_ONLY(
/* Sets the debug module of a spill environment. */
void be_set_spill_env_dbg_module(spill_env_t *env, firm_dbg_module_t *dbg) {
	env->dbg = dbg;
}
)

/* Creates a new spill environment. */
spill_env_t *be_new_spill_env(const be_chordal_env_t *chordal_env) {
	spill_env_t *env	= xmalloc(sizeof(env[0]));
	env->spills			= new_set(cmp_spillinfo, 1024);
	env->cls			= chordal_env->cls;
	env->chordal_env	= chordal_env;
	env->mem_phis		= pset_new_ptr_default();
	obstack_init(&env->obst);
	return env;
}

/* Deletes a spill environment. */
void be_delete_spill_env(spill_env_t *env) {
	del_set(env->spills);
	del_pset(env->mem_phis);
	obstack_free(&env->obst, NULL);
	free(env);
}

/**
 * Schedules a node after an instruction. (That is the place after all projs and phis
 * that are scheduled after the instruction)
 */
static void sched_add_after_insn(ir_node *sched_after, ir_node *node) {
	ir_node *next = sched_next(sched_after);
	while(!sched_is_end(next)) {
		if(!is_Proj(next) && !is_Phi(next))
			break;
		next = sched_next(next);
	}

	if(sched_is_end(next)) {
		next = sched_last(get_nodes_block(sched_after));
		sched_add_after(next, node);
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
	const be_main_env_t *mainenv = env->chordal_env->birg->main_env;
	ir_node *to_spill = spillinfo->spilled_node;

	DBG((env->dbg, LEVEL_1, "%+F\n", to_spill));

	/* Trying to spill an already spilled value, no need for a new spill
	 * node then, we can simply connect to the same one for this reload
	 */
	if(be_is_Reload(to_spill)) {
		spillinfo->spill = get_irn_n(to_spill, be_pos_Reload_mem);
		return;
	}

	spillinfo->spill = be_spill(mainenv->arch_env, to_spill);
	sched_add_after_insn(to_spill, spillinfo->spill);
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

	/* build a new PhiM */
	ins = alloca(sizeof(ir_node*) * arity);
	for(i = 0; i < arity; ++i) {
		ins[i] = get_irg_bad(env->chordal_env->irg);
	}
	spillinfo->spill = new_r_Phi(env->chordal_env->irg, block, arity, ins, mode_M);

	for(i = 0; i < arity; ++i) {
		ir_node *arg = get_irn_n(phi, i);
		spill_info_t *arg_info = get_spillinfo(env, arg);

		spill_node(env, arg_info);

		set_irn_n(spillinfo->spill, i, arg_info->spill);
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
	if (is_Phi(to_spill) && pset_find_ptr(env->mem_phis, spillinfo->spilled_node)) {
		spill_phi(env, spillinfo);
	} else {
		spill_irn(env, spillinfo);
	}
}

static INLINE ir_node *skip_projs(ir_node *node) {
	while(is_Proj(node)) {
		node = sched_next(node);
		assert(!sched_is_end(node));
	}

	return node;
}

#if 0
/**
 * Searchs the schedule backwards until we reach the first use or def of a
 * value or a phi.
 * Returns the node after this node (so that you can do sched_add_before)
 */
static ir_node *find_last_use_def(spill_env_t *env, ir_node *block, ir_node *value) {
	ir_node *node, *last;

	last = NULL;
	sched_foreach_reverse(block, node) {
		int i, arity;

		if(is_Phi(node)) {
			return last;
		}
		if(value == node) {
			return skip_projs(last);
		}
		for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
			ir_node *arg = get_irn_n(node, i);
			if(arg == value) {
				return skip_projs(last);
			}
		}
		last = node;
	}

	// simply return first node if no def or use found
	return sched_first(block);
}
#endif

#ifdef BUGGY_REMAT

/**
 * Check if a spilled node could be rematerialized.
 *
 * @param senv      the spill environment
 * @param spill     the Spill node
 * @param spilled   the node that was spilled
 * @param reloader  a irn that requires a reload
 */
static int check_remat_conditions(spill_env_t *senv, ir_node *spilled, ir_node *reloader) {
	int pos, max;

	/* check for 'normal' spill and general remat condition */
	if (!arch_irn_is(senv->chordal_env->birg->main_env->arch_env, spilled, rematerializable))
		return 0;

	/* check availability of original arguments */
	if (is_Block(reloader)) {

		/* we want to remat at the end of a block.
		 * thus all arguments must be alive at the end of the block
		 */
		for (pos=0, max=get_irn_arity(spilled); pos<max; ++pos) {
			ir_node *arg = get_irn_n(spilled, pos);
			if (!is_live_end(reloader, arg))
				return 0;
		}

	} else {

		/* we want to remat before the insn reloader
		 * thus an arguments is alive if
		 *   - it interferes with the reloaders result
		 * or
		 *   - or it is (last-) used by reloader itself
		 */
		for (pos=0, max=get_irn_arity(spilled); pos<max; ++pos) {
			ir_node *arg = get_irn_n(spilled, pos);
			int i, m;

			if (values_interfere(reloader, arg))
				goto is_alive;

			for (i=0, m=get_irn_arity(reloader); i<m; ++i) {
				ir_node *rel_arg = get_irn_n(reloader, i);
				if (rel_arg == arg)
					goto is_alive;
			}

			/* arg is not alive before reloader */
			return 0;

is_alive:	;

		}

	}

	return 1;
}

#else /* BUGGY_REMAT */

/**
 * A very simple rematerialization checker.
 *
 * @param senv      the spill environment
 * @param spill     the Spill node
 * @param spilled   the node that was spilled
 * @param reloader  a irn that requires a reload
 */
static int check_remat_conditions(spill_env_t *senv, ir_node *spilled, ir_node *reloader) {
	const arch_env_t *aenv = senv->chordal_env->birg->main_env->arch_env;

	return get_irn_arity(spilled) == 0 &&
		   arch_irn_is(aenv, spilled, rematerializable);
}

#endif /* BUGGY_REMAT */

/**
 * Re-materialize a node.
 *
 * @param senv      the spill environment
 * @param spilled   the node that was spilled
 * @param reloader  a irn that requires a reload
 */
static ir_node *do_remat(spill_env_t *senv, ir_node *spilled, ir_node *reloader) {
	ir_node *res;
	ir_node *bl = (is_Block(reloader)) ? reloader : get_nodes_block(reloader);

	/* recompute the value */
	res = new_ir_node(get_irn_dbg_info(spilled), senv->chordal_env->irg, bl,
		get_irn_op(spilled),
		get_irn_mode(spilled),
		get_irn_arity(spilled),
		get_irn_in(spilled) + 1);
	copy_node_attr(spilled, res);

	DBG((senv->dbg, LEVEL_1, "Insert remat %+F before reloader %+F\n", res, reloader));

	/* insert in schedule */
	if (is_Block(reloader)) {
		ir_node *insert = sched_skip(reloader, 0, sched_skip_cf_predicator, (void *) senv->chordal_env->birg->main_env->arch_env);
		sched_add_after(insert, res);
	} else {
		sched_add_before(reloader, res);
	}

	return res;
}

void be_spill_phi(spill_env_t *env, ir_node *node) {
	int i, arity;

	assert(is_Phi(node));

	pset_insert_ptr(env->mem_phis, node);

	// create spillinfos for the phi arguments
	get_spillinfo(env, node);
	for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
		ir_node *arg = get_irn_n(node, i);
		get_spillinfo(env, arg);
	}
}

void be_insert_spills_reloads(spill_env_t *env) {
	const arch_env_t *arch_env = env->chordal_env->birg->main_env->arch_env;
	spill_info_t *si;

	/* process each spilled node */
	DBG((env->dbg, LEVEL_1, "Insert spills and reloads:\n"));
	for(si = set_first(env->spills); si; si = set_next(env->spills)) {
		reloader_t *rld;
		ir_mode *mode = get_irn_mode(si->spilled_node);
		pset *values = pset_new_ptr(16);

		/* go through all reloads for this spill */
		for(rld = si->reloaders; rld; rld = rld->next) {
			ir_node *new_val;

			if (check_remat_conditions(env, si->spilled_node, rld->reloader)) {
				new_val = do_remat(env, si->spilled_node, rld->reloader);
			} else {
				/* make sure we have a spill */
				spill_node(env, si);

				/* do a reload */
				new_val = be_reload(arch_env, env->cls, rld->reloader, mode, si->spill);
			}

			DBG((env->dbg, LEVEL_1, " %+F of %+F before %+F\n", new_val, si->spilled_node, rld->reloader));
			pset_insert_ptr(values, new_val);
		}

		if(pset_count(values) > 0) {
			/* introduce copies, rewire the uses */
			pset_insert_ptr(values, si->spilled_node);
			be_ssa_constr_set_ignore(env->chordal_env->dom_front, env->chordal_env->lv, values, env->mem_phis);
		}

		del_pset(values);
	}

	// reloads are placed now, but we might reuse the spill environment for further spilling decisions
	del_set(env->spills);
	env->spills = new_set(cmp_spillinfo, 1024);
}

void be_add_reload(spill_env_t *env, ir_node *to_spill, ir_node *before) {
	spill_info_t *info;
	reloader_t *rel;

	assert(sched_is_scheduled(before));
	assert(arch_irn_consider_in_reg_alloc(env->chordal_env->birg->main_env->arch_env, env->cls, to_spill));

	info = get_spillinfo(env, to_spill);

	if(is_Phi(to_spill)) {
		int i, arity;
		// create spillinfos for the phi arguments
		for(i = 0, arity = get_irn_arity(to_spill); i < arity; ++i) {
			ir_node *arg = get_irn_n(to_spill, i);
			get_spillinfo(env, arg);
		}
	}

	rel           = obstack_alloc(&env->obst, sizeof(rel[0]));
	rel->reloader = before;
	rel->next     = info->reloaders;
	info->reloaders = rel;
	be_liveness_add_missing(env->chordal_env->lv);
}

void be_add_reload_on_edge(spill_env_t *env, ir_node *to_spill, ir_node *block, int pos) {
	ir_node *predblock, *last;

	/* simply add the reload to the beginning of the block if we only have 1 predecessor
	 * (we don't need to check for phis as there can't be any in a block with only 1 pred)
	 */
	if(get_Block_n_cfgpreds(block) == 1) {
		assert(!is_Phi(sched_first(block)));
		be_add_reload(env, to_spill, sched_first(block));
		return;
	}

	/* We have to reload the value in pred-block */
	predblock = get_Block_cfgpred_block(block, pos);
	last = sched_last(predblock);

	/* we might have projs and keepanys behind the jump... */
	while(is_Proj(last) || be_is_Keep(last)) {
		last = sched_prev(last);
		assert(!sched_is_end(last));
	}
	assert(is_cfop(last));

	// add the reload before the (cond-)jump
	be_add_reload(env, to_spill, last);
}
