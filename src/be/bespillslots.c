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
#include "bespillslots.h"

#include "array.h"
#include "bechordal_t.h"
#include "beirg.h"
#include "belive.h"
#include "bemodule.h"
#include "benode.h"
#include "besched.h"
#include "bespill.h"
#include "bespillutil.h"
#include "debug.h"
#include "execfreq.h"
#include "ircons.h"
#include "irdump_t.h"
#include "irgwalk.h"
#include "set.h"
#include "statev_t.h"
#include "unionfind.h"
#include "util.h"
#include <stdlib.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct spillweb_t spillweb_t;

typedef struct spill_t {
	ir_node    *spill;
	spillweb_t *web;
	int         spillslot;
} spill_t;

/**
 * A spillweb specifies the type of the values that a set of spills has to
 * produce. All spills that are joined through Phis form a spillweb.
 */
struct spillweb_t {
	spillweb_t *merged_with;
	unsigned    slot_size;
	unsigned    slot_po2align;
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

static void merge_slotsizes(spillweb_t *web, unsigned size, unsigned po2align)
{
	assert(web->merged_with == NULL);
	web->slot_size     = MAX(size, web->slot_size);
	web->slot_po2align = MAX(po2align, web->slot_po2align);
}

static spillweb_t *get_spill_web(spillweb_t *begin)
{
	spillweb_t *result = begin;
	while (result->merged_with != NULL) {
		result = result->merged_with;
	}
	/* path compression */
	for (spillweb_t *web = begin, *next; web != result; web = next) {
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
	merge_slotsizes(web0, web1->slot_size, web1->slot_po2align);
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
	DB((dbg, LEVEL_1, "Slot %d: %+F (%+F)\n", spill->spillslot,
	    skip_Proj(node), node));

	if (is_Phi(node)) {
		foreach_irn_in(node, i, arg) {
			/* ignore obvious self-loops */
			if (arg == node)
				continue;
			spill_t *arg_spill = collect_spill(env, arg, web);
			ir_node *block     = get_nodes_block(arg);

			/* add an affinity edge */
			affinity_edge_t *affinity_edge = OALLOC(&env->obst, affinity_edge_t);
			affinity_edge->affinity = get_block_execfreq(block);
			affinity_edge->slot1    = spill->spillslot;
			affinity_edge->slot2    = arg_spill->spillslot;
			ARR_APP1(affinity_edge_t*, env->affinity_edges, affinity_edge);
#ifndef NDEBUG
			spillweb_t *old_web = web;
#endif
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

void be_load_needs_frame_entity(be_fec_env_t *const env, ir_node *const node,
                                unsigned const slot_size,
                                unsigned const slot_po2align)
{
	ir_node *const mem   = get_memory_edge(node);
	spill_t *const spill = collect_spill(env, mem, NULL);
	DB((dbg, LEVEL_1, "Slot %d: Reload: %+F Size: %u Align: %u\n",
	    spill->spillslot, node, slot_size, 1u << slot_po2align));
	ARR_APP1(ir_node*, env->reloads, node);
	merge_slotsizes(spill->web, slot_size, slot_po2align);
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

	DB((dbg, LEVEL_1, "Coalescing %d spillslots\n", spillcount));

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

			if (be_memory_values_interfere(spill1, spill2)) {
				DB((dbg, LEVEL_1, "Slot %d and %d interfere\n", i, i2));

				bitset_set(interferences[i], i2);
				bitset_set(interferences[i2], i);
			}
		}
	}

	/* sort affinity edges */
	QSORT_ARR(env->affinity_edges, cmp_affinity);

	/* try to merge affine nodes */
	for (size_t i = 0, n = ARR_LEN(env->affinity_edges); i < n; ++i) {
		const affinity_edge_t *edge = env->affinity_edges[i];
		int s1 = uf_find(spillslot_unionfind, edge->slot1);
		int s2 = uf_find(spillslot_unionfind, edge->slot2);

		/* test if values interfere */
		if (bitset_is_set(interferences[s1], s2)) {
			assert(bitset_is_set(interferences[s2], s1));
			continue;
		}

		DB((dbg, LEVEL_1,
		    "Merging %d and %d because of affinity edge\n", s1, s2));

		merge_interferences(env, interferences, spillslot_unionfind, s1, s2);
	}

	/* Try to merge as much remaining spillslots as possible */
	for (size_t i = 0; i < spillcount; ++i) {
		int s1 = uf_find(spillslot_unionfind, i);
		if (s1 != (int)i)
			continue;

		for (size_t i2 = i+1; i2 < spillcount; ++i2) {
			int s2 = uf_find(spillslot_unionfind, i2);
			if (s2 != (int)i2)
				continue;

			/* Test if values interfere, we have to test n1-n2 and n2-n1,
			 * because only 1 side gets updated when node merging occurs */
			if (bitset_is_set(interferences[s1], s2)) {
				assert(bitset_is_set(interferences[s2], s1));
				continue;
			}

			DB((dbg, LEVEL_1,
			    "Merging %d and %d because it is possible\n", s1, s2));

			if (merge_interferences(env, interferences, spillslot_unionfind,
			                        s1, s2) != 0) {
				/* We can break the loop here, because s2 is the new supernode
				 * now and we'll test s2 again later anyway */
				break;
			}
		}
	}

	/* Assign spillslots to spills */
	for (size_t i = 0; i < spillcount; ++i) {
		spills[i]->spillslot = uf_find(spillslot_unionfind, i);
	}

	obstack_free(&data, 0);
}

typedef struct spill_slot_t {
	ir_entity *entity;
	unsigned   size;
	unsigned   po2align;
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
	unsigned         entrycount;
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

static void assign_spill_entity(be_fec_env_t *env, ir_node *node,
                                ir_entity *entity, unsigned size,
                                unsigned po2align)
{
	if (is_NoMem(node))
		return;
	if (is_Sync(node)) {
		foreach_irn_in(node, i, in) {
			assert(!is_Phi(in));
			assign_spill_entity(env, in, entity, size, po2align);
		}
		return;
	}

	node = skip_Proj(node);
	env->set_frame_entity(node, entity, size, po2align);
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
		const spill_t    *const spill  = spills[s];
		int               const slotid = spill->spillslot;
		spillweb_t const *const web    = get_spill_web(spill->web);
		spill_slot_t     *const slot   = &spillslots[slotid];

		slot->size     = MAX(slot->size, web->slot_size);
		slot->po2align = MAX(slot->po2align, web->slot_po2align);
	}

	ir_type *const frame = get_irg_frame_type(env->irg);
	for (size_t s = 0; s < spillcount; ++s) {
		const spill_t *spill  = spills[s];
		ir_node       *node   = spill->spill;
		int            slotid = spill->spillslot;
		spill_slot_t  *slot   = &spillslots[slotid];

		if (slot->entity == NULL)
			slot->entity = new_spillslot(frame, slot->size, slot->po2align);

		if (is_Phi(node)) {
			ir_node *block = get_nodes_block(node);

			/* should be a PhiM */
			assert(get_irn_mode(node) == mode_M);

			foreach_irn_in(node, i, arg) {
				ir_node *predblock = get_Block_cfgpred_block(block, i);
				spill_t *argspill  = get_spill(env, arg);
				int      argslotid = argspill->spillslot;

				if (slotid != argslotid) {
					spill_slot_t *argslot = &spillslots[argslotid];
					if (argslot->entity == NULL)
						argslot->entity = new_spillslot(frame, argslot->size,
						                                argslot->po2align);

					memperm_t *const memperm = get_memperm(env, predblock);
					memperm_entry_t *const entry
						= OALLOC(&env->obst, memperm_entry_t);
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
			assign_spill_entity(env, node, slot->entity, web->slot_size,
			                    web->slot_po2align);
		}
	}

	for (size_t s = 0; s < ARR_LEN(env->reloads); ++s) {
		ir_node            *reload    = env->reloads[s];
		ir_node            *spillnode = get_memory_edge(reload);
		const spill_t      *spill     = get_spill(env, spillnode);
		const spill_slot_t *slot      = &spillslots[spill->spillslot];
		const spillweb_t   *web       = get_spill_web(spill->web);

		assert(slot->entity != NULL);
		env->set_frame_entity(reload, slot->entity, web->slot_size,
		                      web->slot_po2align);
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
			be_set_MemPerm_in_entity(mempermnode, i, entry->in);
			be_set_MemPerm_out_entity(mempermnode, i, entry->out);
			ir_node *const proj = be_new_Proj(mempermnode, i);
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

void be_assign_entities(be_fec_env_t *env,
                        set_frame_entity_func set_frame_entity,
                        bool alloc_entities_at_begin)
{
	env->set_frame_entity = set_frame_entity;
	env->at_begin         = alloc_entities_at_begin;

	if (stat_ev_enabled)
		stat_ev_dbl("spillslots", ARR_LEN(env->spills));

	/* Disable coalescing for "returns twice" calls: In case of setjmp/longjmp
	 * our control flow graph isn't completely correct: There are no backedges
	 * from longjmp to the setjmp => coalescing would produce wrong results. */
	if (be_coalesce_spill_slots && !be_birg_from_irg(env->irg)->has_returns_twice_call)
		do_greedy_coalescing(env);

	if (stat_ev_enabled)
		stat_ev_dbl("spillslots_after_coalescing", count_spillslots(env));

	assign_spillslots(env);
	create_memperms(env);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spillslots)
void be_init_spillslots(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.spillslots");
}
