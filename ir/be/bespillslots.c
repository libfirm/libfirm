/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Spillslot coalescer.
 * @author      Matthias Braun
 * @date        26.07.2006
 */
#include <stdlib.h>

#include "set.h"
#include "array.h"
#include "irgwalk.h"
#include "ircons.h"
#include "execfreq.h"
#include "unionfind.h"
#include "irdump_t.h"

#include "benode.h"
#include "besched.h"
#include "bespill.h"
#include "bespillslots.h"
#include "bechordal_t.h"
#include "statev_t.h"
#include "bemodule.h"
#include "beintlive_t.h"
#include "beirg.h"
#include "bearch.h"
#include "bespillutil.h"

#define DBG_COALESCING      1
#define DBG_INTERFERENCES   2

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct spillweb_t spillweb_t;

typedef struct spill_t {
	ir_node    *spill;
	spillweb_t *web;
	int         spillslot;
} spill_t;

/**
 * A spillweb specifies the type of the values that a set of spills has to
 * prodcue. All spills that are joined through Phis form a spillweb.
 */
struct spillweb_t {
	spillweb_t    *merged_with;
	const ir_type *type;
};

typedef struct affinity_edge_t {
	double affinity;
	int    slot1;
	int    slot2;
} affinity_edge_t;

struct be_fec_env_t {
	struct obstack         obst;
	ir_graph              *irg;
	spill_t              **spills;
	unsigned              *spills_set;
	ir_node              **reloads;
	affinity_edge_t      **affinity_edges;
	set                   *memperms;
	set_frame_entity_func  set_frame_entity;
	bool                   at_begin;  /**< frame entities should be allocate at
	                                       the beginning of the stackframe */
	bool                   coalescing_forbidden;
};

/** Compare 2 affinity edges (used in quicksort) */
static int cmp_affinity(const void *d1, const void *d2)
{
	const affinity_edge_t *const *e1   = (const affinity_edge_t**)d1;
	const affinity_edge_t *const *e2   = (const affinity_edge_t**)d2;
	double                        aff1 = (*e1)->affinity;
	double                        aff2 = (*e2)->affinity;

	/* sort in descending order */
	if (aff1 < aff2) {
		return 1;
	} else if (aff1 > aff2) {
		return -1;
	} else {
		int slot11 = (*e1)->slot1;
		int slot21 = (*e2)->slot1;
		if (slot11 < slot21) {
			return 1;
		} else if (slot11 > slot21) {
			return -1;
		} else {
			int slot12 = (*e1)->slot2;
			int slot22 = (*e2)->slot2;
			return (slot12<slot22) - (slot12>slot22);
		}
	}
}

static spill_t *get_spill(be_fec_env_t *env, ir_node *node)
{
	(void)env;
	assert(rbitset_is_set(env->spills_set, get_irn_idx(node)));
	return (spill_t*)get_irn_link(node);
}

static inline ir_node *get_memory_edge(const ir_node *node)
{
	foreach_irn_in_r(node, i, arg) {
		if (get_irn_mode(arg) == mode_M)
			return arg;
	}

	return NULL;
}

#ifndef NDEBUG
static bool modes_compatible(const ir_mode *mode0, const ir_mode *mode1)
{
	ir_mode_arithmetic arith0 = get_mode_arithmetic(mode0);
	ir_mode_arithmetic arith1 = get_mode_arithmetic(mode1);
	return arith0 == arith1
	   || (arith0 == irma_ieee754 && arith1 == irma_x86_extended_float)
	   || (arith1 == irma_ieee754 && arith0 == irma_x86_extended_float);
}
#endif

static void merge_spilltypes(spillweb_t *web, const ir_type *type1)
{
	assert(web->merged_with == NULL);
	assert(type1 != NULL);
	const ir_type *type0 = web->type;
	if (type0 == NULL) {
		web->type = type1;
		return;
	}
	assert(modes_compatible(get_type_mode(type0), get_type_mode(type1)));
	web->type
		= get_type_size_bytes(type1) > get_type_size_bytes(type0)
		? type1 : type0;
}

static spillweb_t *get_spill_web(spillweb_t *begin)
{
	spillweb_t *result = begin;
	while (result->merged_with != NULL) {
		result = result->merged_with;
	}
	/* path compression */
	for (spillweb_t *web = begin, *next; web != result;
	     web = next) {
		next = web->merged_with;
		web->merged_with = result;
	}
	return result;
}

static spillweb_t *merge_spillwebs(spillweb_t *web0, spillweb_t *web1)
{
	assert(web0 != web1);
	assert(web0->merged_with == NULL);
	assert(web1->merged_with == NULL);
	const ir_type *type1 = web1->type;
	if (type1 != NULL)
		merge_spilltypes(web0, type1);
	web1->merged_with = web0;
	return web0;
}

static spill_t *collect_spill(be_fec_env_t *env, ir_node *node, spillweb_t *web)
{
	assert(web == NULL || web->merged_with == NULL);

	/* already in spill set? */
	unsigned idx = get_irn_idx(node);
	if (rbitset_is_set(env->spills_set, idx)) {
		spill_t *spill = get_spill(env, node);
		/* create a new web if necesary */
		spillweb_t *new_web = spill->web;
		if (new_web == NULL) {
			new_web = web;
			if (new_web == NULL)
				new_web = OALLOCZ(&env->obst, spillweb_t);
		} else {
			new_web = get_spill_web(new_web);
			if (web != NULL && new_web != web)
				new_web = merge_spillwebs(new_web, web);
		}
		spill->web = new_web;
		return spill;
	}
	rbitset_set(env->spills_set, idx);

	spill_t *spill = OALLOC(&env->obst, spill_t);
	/* insert into set of spills if not already there */
	spill->spill     = node;
	spill->spillslot = (int)ARR_LEN(env->spills);
	spill->web       = web;
	ARR_APP1(spill_t*, env->spills, spill);
	set_irn_link(node, spill);
	DB((dbg, DBG_COALESCING, "Slot %d: %+F (%+F)\n", spill->spillslot,
	    skip_Proj(node), node));

	if (is_Phi(node)) {
		foreach_irn_in(node, i, arg) {
			/* ignore obvious self-loops */
			if (arg == node)
				continue;
			spillweb_t *old_web = web;
			spill_t *arg_spill = collect_spill(env, arg, web);
			ir_node *block     = get_nodes_block(arg);

			/* add an affinity edge */
			affinity_edge_t *affinty_edge = OALLOC(&env->obst, affinity_edge_t);
			affinty_edge->affinity = get_block_execfreq(block);
			affinty_edge->slot1    = spill->spillslot;
			affinty_edge->slot2    = arg_spill->spillslot;
			ARR_APP1(affinity_edge_t*, env->affinity_edges, affinty_edge);
			web = arg_spill->web;
			assert(web->merged_with == NULL);
			assert(web == old_web || old_web == NULL
			       || old_web->merged_with != NULL);
			spill->web = web;
		}
	} else if (web == NULL) {
		/* create new spillweb if necessary */
		web = OALLOCZ(&env->obst, spillweb_t);
		spill->web = web;
	}

	return spill;
}

void be_load_needs_frame_entity(be_fec_env_t *env, ir_node *node,
                                const ir_type *type)
{
	ir_node *mem   = get_memory_edge(node);
	spill_t *spill = collect_spill(env, mem, NULL);
	DB((dbg, DBG_COALESCING, "Slot %d: Reload: %+F Type %+F\n",
	    spill->spillslot, node, type));
	ARR_APP1(ir_node*, env->reloads, node);
	merge_spilltypes(spill->web, type);
}

static int merge_interferences(be_fec_env_t *env, bitset_t** interferences,
                               int* spillslot_unionfind, int s1, int s2)
{
	/* merge spillslots and interferences */
	int res = uf_union(spillslot_unionfind, s1, s2);
	/* we assume that we always merge s2 to s1 so swap s1, s2 if necessary */
	if (res != s1) {
		int t = s1;
		s1 = s2;
		s2 = t;
	}

	bitset_or(interferences[s1], interferences[s2]);

	/* update other interferences */
	for (size_t i = 0, n = ARR_LEN(env->spills); i < n; ++i) {
		bitset_t *intfs = interferences[i];
		if (bitset_is_set(intfs, s2))
			bitset_set(intfs, s1);
	}

	return res;
}

static bool my_values_interfere2(ir_graph *const irg, ir_node const *a,
                                 ir_node const *b)
{
	if (value_dominates(b, a)) {
		/* Adjust a and b so, that a dominates b if
		 * a dominates b or vice versa. */
		ir_node const *const t = a;
		a = b;
		b = t;
	} else if (!value_dominates(a, b)) {
		/* If there is no dominance relation, they do not interfere. */
		return 0;
	}

	/* If a is live end in b's block it is
	 * live at b's definition (a dominates b) */
	ir_node *const bb = get_nodes_block(b);
	be_lv_t *const lv = be_get_irg_liveness(irg);
	if (be_is_live_end(lv, bb, a))
		return true;

	/* Look at all usages of a.
	 * If there's one usage of a in the block of b, then
	 * we check, if this use is dominated by b, if that's true
	 * a and b interfere. Note that b must strictly dominate the user,
	 * since if b is the last user of in the block, b and a do not
	 * interfere.
	 * Uses of a not in b's block can be disobeyed, because the
	 * check for a being live at the end of b's block is already
	 * performed. */
	foreach_out_edge(a, edge) {
		ir_node const *const user = get_edge_src_irn(edge);
		if (is_Sync(user)) {
			foreach_out_edge(user, edge2) {
				ir_node const *const user2 = get_edge_src_irn(edge2);
				assert(!is_Sync(user2));
				if (get_nodes_block(user2) == bb && !is_Phi(user2) &&
				    _value_strictly_dominates_intrablock(b, user2))
					return true;
			}
		} else {
			if (get_nodes_block(user) == bb && !is_Phi(user) &&
			    _value_strictly_dominates_intrablock(b, user))
				return true;
		}
	}

	return false;
}

/**
 * same as values_interfere but with special handling for Syncs
 */
static int my_values_interfere(ir_graph *irg, ir_node *a, ir_node *b)
{
	if (is_Sync(a)) {
		foreach_irn_in(a, i, in) {
			if (my_values_interfere(irg, in, b))
				return 1;
		}
		return 0;
	} else if (is_Sync(b)) {
		foreach_irn_in(b, i, in) {
			/* a is not a sync, so no need for my_values_interfere */
			if (my_values_interfere2(irg, a, in))
				return 1;
		}
		return 0;
	}

	return my_values_interfere2(irg, a, b);
}

/**
 * A greedy coalescing algorithm for spillslots:
 *  1. Sort the list of affinity edges
 *  2. Try to merge slots with affinity edges (most expensive slots first)
 *  3. Try to merge everything else that is possible
 */
static void do_greedy_coalescing(be_fec_env_t *env)
{
	spill_t **spills     = env->spills;
	size_t    spillcount = ARR_LEN(spills);
	if (spillcount == 0)
		return;

	DB((dbg, DBG_COALESCING, "Coalescing %d spillslots\n", spillcount));

	struct obstack data;
	obstack_init(&data);

	bitset_t **interferences       = OALLOCN(&data, bitset_t*, spillcount);
	int       *spillslot_unionfind = OALLOCN(&data, int,       spillcount);

	uf_init(spillslot_unionfind, spillcount);

	for (size_t i = 0; i < spillcount; ++i) {
		interferences[i] = bitset_obstack_alloc(&data, spillcount);
	}

	/* construct interferences */
	for (size_t i = 0; i < spillcount; ++i) {
		ir_node *spill1 = spills[i]->spill;
		if (is_NoMem(spill1))
			continue;

		for (size_t i2 = i+1; i2 < spillcount; ++i2) {
			ir_node *spill2 = spills[i2]->spill;
			if (is_NoMem(spill2))
				continue;

			if (my_values_interfere(env->irg, spill1, spill2)) {
				DB((dbg, DBG_INTERFERENCES,
				     "Slot %d and %d interfere\n", i, i2));

				bitset_set(interferences[i], i2);
				bitset_set(interferences[i2], i);
			}
		}
	}

	/* sort affinity edges */
	size_t affinity_edge_count = ARR_LEN(env->affinity_edges);
	qsort(env->affinity_edges, affinity_edge_count,
	      sizeof(env->affinity_edges[0]), cmp_affinity);

	/* try to merge affine nodes */
	for (size_t i = 0; i < affinity_edge_count; ++i) {
		const affinity_edge_t *edge = env->affinity_edges[i];
		int s1 = uf_find(spillslot_unionfind, edge->slot1);
		int s2 = uf_find(spillslot_unionfind, edge->slot2);

		/* test if values interfere */
		if (bitset_is_set(interferences[s1], s2)) {
			assert(bitset_is_set(interferences[s2], s1));
			continue;
		}

		DB((dbg, DBG_COALESCING,
		    "Merging %d and %d because of affinity edge\n", s1, s2));

		merge_interferences(env, interferences, spillslot_unionfind, s1, s2);
	}

	/* try to merge as much remaining spillslots as possible */
	for (size_t i = 0; i < spillcount; ++i) {
		int s1 = uf_find(spillslot_unionfind, i);
		if (s1 != (int)i)
			continue;

		for (size_t i2 = i+1; i2 < spillcount; ++i2) {
			int s2 = uf_find(spillslot_unionfind, i2);
			if (s2 != (int)i2)
				continue;

			/* test if values interfere
			 * we have to test n1-n2 and n2-n1, because only 1 side gets updated
			 * when node merging occurs
			 */
			if (bitset_is_set(interferences[s1], s2)) {
				assert(bitset_is_set(interferences[s2], s1));
				continue;
			}

			DB((dbg, DBG_COALESCING,
			     "Merging %d and %d because it is possible\n", s1, s2));

			if (merge_interferences(env, interferences, spillslot_unionfind, s1, s2) != 0) {
				/* we can break the loop here, because s2 is the new supernode
				 * now and we'll test s2 again later anyway */
				break;
			}
		}
	}

	/* assign spillslots to spills */
	for (size_t i = 0; i < spillcount; ++i) {
		spills[i]->spillslot = uf_find(spillslot_unionfind, i);
	}

	obstack_free(&data, 0);
}

typedef struct spill_slot_t {
	int        size;
	int        align;
	ir_entity *entity;
} spill_slot_t;

typedef struct memperm_entry_t memperm_entry_t;
struct memperm_entry_t {
	ir_node         *node;
	int              pos;
	ir_entity       *in;
	ir_entity       *out;
	memperm_entry_t *next;
};

typedef struct memperm_t {
	ir_node         *block;
	int              entrycount;
	memperm_entry_t *entries;
} memperm_t;

static int cmp_memperm(const void* d1, const void* d2, size_t size)
{
	(void)size;
	const memperm_t* e1 = (const memperm_t*)d1;
	const memperm_t* e2 = (const memperm_t*)d2;
	return e1->block != e2->block;
}

static memperm_t *get_memperm(be_fec_env_t *env, ir_node *block)
{
	memperm_t entry;
	entry.block = block;
	unsigned hash = hash_irn(block);
	memperm_t *res
		= set_find(memperm_t, env->memperms, &entry, sizeof(entry), hash);

	if (res == NULL) {
		entry.entrycount = 0;
		entry.entries = NULL;
		res = set_insert(memperm_t, env->memperms, &entry, sizeof(entry), hash);
	}
	return res;
}

static ir_entity* create_stack_entity(be_fec_env_t *env, spill_slot_t *slot)
{
	ir_graph  *irg   = env->irg;
	ir_type   *frame = get_irg_frame_type(irg);
	ir_entity *res   = frame_alloc_area(frame, slot->size, slot->align,
	                                    env->at_begin);
	slot->entity = res;

	return res;
}

/**
 * Enlarges a spillslot (if necessary) so that it can carry a value of size
 * @p othersize and alignment @p otheralign.
 */
static void enlarge_spillslot(spill_slot_t *slot, int otheralign, int othersize)
{
	if (othersize > slot->size) {
		slot->size = othersize;
	}
	if (otheralign > slot->align) {
		if (otheralign % slot->align != 0)
			slot->align *= otheralign;
		else
			slot->align = otheralign;
	} else if (slot->align % otheralign != 0) {
		slot->align *= otheralign;
	}
}

static void assign_spill_entity(be_fec_env_t *env, ir_node *node,
                                ir_entity *entity, const ir_type *type)
{
	if (is_NoMem(node))
		return;
	if (is_Sync(node)) {
		foreach_irn_in(node, i, in) {
			assert(!is_Phi(in));
			assign_spill_entity(env, in, entity, type);
		}
		return;
	}

	node = skip_Proj(node);
	assert(arch_get_frame_entity(node) == NULL);
	env->set_frame_entity(node, entity, type);
}

/**
 * Create stack entities for the spillslots and assign them to the spill and
 * reload nodes.
 */
static void assign_spillslots(be_fec_env_t *env)
{
	spill_t     **spills     = env->spills;
	size_t        spillcount = ARR_LEN(spills);
	spill_slot_t *spillslots = ALLOCANZ(spill_slot_t, spillcount);

	/* construct spillslots */
	for (size_t s = 0; s < spillcount; ++s) {
		const spill_t    *spill  = spills[s];
		int               slotid = spill->spillslot;
		const spillweb_t *web    = get_spill_web(spill->web);
		const ir_type    *type   = web->type;
		const ir_mode    *mode   = get_type_mode(type);
		spill_slot_t     *slot   = &spillslots[slotid];
		int               size   = get_mode_size_bytes(mode);
		int               align  = get_type_alignment_bytes(type);

		if (slot->align == 0 && slot->size == 0) {
			slot->align = align;
			slot->size = size;
		} else {
			enlarge_spillslot(slot, align, size);
		}
	}

	for (size_t s = 0; s < spillcount; ++s) {
		const spill_t *spill  = spills[s];
		ir_node       *node   = spill->spill;
		int            slotid = spill->spillslot;
		spill_slot_t  *slot   = &spillslots[slotid];

		if (slot->entity == NULL) {
			create_stack_entity(env, slot);
		}

		if (is_Phi(node)) {
			ir_node *block = get_nodes_block(node);

			/* should be a PhiM */
			assert(get_irn_mode(node) == mode_M);

			foreach_irn_in(node, i, arg) {
				ir_node *predblock = get_Block_cfgpred_block(block, i);
				spill_t *argspill  = get_spill(env, arg);
				int      argslotid = argspill->spillslot;

				if (slotid != argslotid) {
					memperm_t       *memperm;
					memperm_entry_t *entry;
					spill_slot_t    *argslot = &spillslots[argslotid];
					if (argslot->entity == NULL) {
						create_stack_entity(env, argslot);
					}

					memperm = get_memperm(env, predblock);

					entry = OALLOC(&env->obst, memperm_entry_t);
					entry->node = node;
					entry->pos = i;
					entry->in = argslot->entity;
					entry->out = slot->entity;
					entry->next = memperm->entries;
					memperm->entrycount++;
					memperm->entries = entry;
				}
			}
		} else {
			const spillweb_t *web = get_spill_web(spill->web);
			assign_spill_entity(env, node, slot->entity, web->type);
		}
	}

	for (size_t s = 0; s < ARR_LEN(env->reloads); ++s) {
		ir_node            *reload    = env->reloads[s];
		ir_node            *spillnode = get_memory_edge(reload);
		const spill_t      *spill     = get_spill(env, spillnode);
		const spill_slot_t *slot      = &spillslots[spill->spillslot];
		const spillweb_t   *web       = get_spill_web(spill->web);

		assert(slot->entity != NULL);

		env->set_frame_entity(reload, slot->entity, web->type);
	}
}

static void create_memperms(be_fec_env_t *env)
{
	foreach_set(env->memperms, memperm_t, memperm) {
		assert(memperm->entrycount > 0);

		ir_node **nodes = ALLOCAN(ir_node*, memperm->entrycount);
		int       i     = 0;
		for (memperm_entry_t *entry = memperm->entries; entry != NULL;
		     entry = entry->next, ++i) {
			ir_node* arg = get_irn_n(entry->node, entry->pos);
			nodes[i] = arg;
		}

		ir_node *mempermnode
			= be_new_MemPerm(memperm->block, memperm->entrycount, nodes);

		/* insert node into schedule */
		ir_node *const blockend
			= be_get_end_of_block_insertion_point(memperm->block);
		sched_add_before(blockend, mempermnode);
		stat_ev_dbl("mem_perm", memperm->entrycount);

		i = 0;
		for (memperm_entry_t *entry = memperm->entries; entry != NULL;
		     entry = entry->next, ++i) {
			ir_node* arg = get_irn_n(entry->node, entry->pos);

			be_set_MemPerm_in_entity(mempermnode, i, entry->in);
			be_set_MemPerm_out_entity(mempermnode, i, entry->out);
			ir_node *proj = new_r_Proj(mempermnode, get_irn_mode(arg), i);

			set_irn_n(entry->node, entry->pos, proj);
		}
	}
}

static unsigned count_spillslots(const be_fec_env_t *env)
{
	size_t          spillcount = ARR_LEN(env->spills);
	unsigned        slotcount  = 0;
	unsigned *const counted    = rbitset_alloca(spillcount);
	for (size_t s = 0; s < spillcount; ++s) {
		spill_t *spill     = env->spills[s];
		int      spillslot = spill->spillslot;
		if (!rbitset_is_set(counted, spillslot)) {
			++slotcount;
			rbitset_set(counted, spillslot);
		}
	}

	return slotcount;
}

be_fec_env_t *be_new_frame_entity_coalescer(ir_graph *irg)
{
	be_fec_env_t *env = XMALLOCZ(be_fec_env_t);

	be_assure_live_chk(irg);

	obstack_init(&env->obst);
	env->irg            = irg;
	env->spills         = NEW_ARR_F(spill_t*, 0);
	env->spills_set     = rbitset_malloc(get_irg_last_idx(irg));
	env->reloads        = NEW_ARR_F(ir_node*, 0);
	env->affinity_edges = NEW_ARR_F(affinity_edge_t*, 0);
	env->memperms       = new_set(cmp_memperm, 10);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	return env;
}

void be_free_frame_entity_coalescer(be_fec_env_t *env)
{
	ir_free_resources(env->irg, IR_RESOURCE_IRN_LINK);

	del_set(env->memperms);
	DEL_ARR_F(env->reloads);
	DEL_ARR_F(env->affinity_edges);
	DEL_ARR_F(env->spills);
	free(env->spills_set);
	obstack_free(&env->obst, NULL);

	free(env);
}

void be_forbid_coalescing(be_fec_env_t *env)
{
	env->coalescing_forbidden = true;
}

void be_assign_entities(be_fec_env_t *env,
                        set_frame_entity_func set_frame_entity,
                        bool alloc_entities_at_begin)
{
	env->set_frame_entity = set_frame_entity;
	env->at_begin         = alloc_entities_at_begin;

	if (stat_ev_enabled) {
		stat_ev_dbl("spillslots", ARR_LEN(env->spills));
	}

	if (be_coalesce_spill_slots && !env->coalescing_forbidden) {
		do_greedy_coalescing(env);
	}

	if (stat_ev_enabled) {
		stat_ev_dbl("spillslots_after_coalescing", count_spillslots(env));
	}

	assign_spillslots(env);

	create_memperms(env);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spillslots)
void be_init_spillslots(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.spillslots");
}
