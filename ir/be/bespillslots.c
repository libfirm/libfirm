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

typedef struct spill_t {
	ir_node       *spill;
	const ir_mode *mode;      /**< mode of the spilled value */
	int            alignment; /**< alignment for the spilled value */
	int            spillslot;
} spill_t;

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
	const affinity_edge_t * const *e1   = (const affinity_edge_t**)d1;
	const affinity_edge_t * const *e2   = (const affinity_edge_t**)d2;
	double                         aff1 = (*e1)->affinity;
	double                         aff2 = (*e2)->affinity;

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
			return (slot12<slot22) - (slot12<slot22);
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

static spill_t *collect_spill(be_fec_env_t *env, ir_node *node,
		                      const ir_mode *mode, int align)
{
	spill_t *spill;

	/* already in spill set? */
	unsigned idx = get_irn_idx(node);
	if (rbitset_is_set(env->spills_set, idx)) {
		spill_t *spill = get_spill(env, node);
		assert(spill->mode == mode);
		assert(spill->alignment == align);
		return spill;
	}
	rbitset_set(env->spills_set, idx);

	spill = OALLOC(&env->obst, spill_t);
	/* insert into set of spills if not already there */
	spill->spill     = node;
	spill->mode      = mode;
	spill->alignment = align;
	spill->spillslot = (int)ARR_LEN(env->spills);
	ARR_APP1(spill_t*, env->spills, spill);
	set_irn_link(node, spill);
	DB((dbg, DBG_COALESCING, "Slot %d: %+F\n", spill->spillslot, node));

	if (is_Phi(node)) {
		foreach_irn_in(node, i, arg) {
			affinity_edge_t *affinty_edge;
			spill_t         *arg_spill = collect_spill(env, arg, mode, align);
			ir_node         *block     = get_nodes_block(arg);

			/* add an affinity edge */
			affinty_edge           = OALLOC(&env->obst, affinity_edge_t);
			affinty_edge->affinity = get_block_execfreq(block);
			affinty_edge->slot1    = spill->spillslot;
			affinty_edge->slot2    = arg_spill->spillslot;
			ARR_APP1(affinity_edge_t*, env->affinity_edges, affinty_edge);
		}
	}

	return spill;
}

void be_node_needs_frame_entity(be_fec_env_t *env, ir_node *node,
                                const ir_mode *mode, int align)
{
	ir_node *spillnode = get_memory_edge(node);
	assert(spillnode != NULL);

	/* walk upwards and collect all phis and spills on this way */
	collect_spill(env, spillnode, mode, align);

	ARR_APP1(ir_node *, env->reloads, node);
}

static int merge_interferences(be_fec_env_t *env, bitset_t** interferences,
                               int* spillslot_unionfind, int s1, int s2)
{
	int res;
	size_t spillcount;
	size_t i;

	/* merge spillslots and interferences */
	res = uf_union(spillslot_unionfind, s1, s2);
	/* we assume that we always merge s2 to s1 so swap s1, s2 if necessary */
	if (res != s1) {
		int t = s1;
		s1 = s2;
		s2 = t;
	}

	bitset_or(interferences[s1], interferences[s2]);

	/* update other interferences */
	spillcount = ARR_LEN(env->spills);
	for (i = 0; i < spillcount; ++i) {
		bitset_t *intfs = interferences[i];
		if (bitset_is_set(intfs, s2))
			bitset_set(intfs, s1);
	}

	return res;
}

static bool my_values_interfere2(ir_graph *const irg, ir_node const *a, ir_node const *b)
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

	ir_node *const bb = get_nodes_block(b);

	/* If a is live end in b's block it is
	 * live at b's definition (a dominates b) */
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
	size_t    i;
	size_t    affinity_edge_count;
	bitset_t **interferences;
	int* spillslot_unionfind;
	struct obstack data;

	if (spillcount == 0)
		return;

	obstack_init(&data);

	DB((dbg, DBG_COALESCING, "Coalescing %d spillslots\n", spillcount));

	interferences       = OALLOCN(&data, bitset_t*, spillcount);
	spillslot_unionfind = OALLOCN(&data, int,       spillcount);

	uf_init(spillslot_unionfind, spillcount);

	for (i = 0; i < spillcount; ++i) {
		interferences[i] = bitset_obstack_alloc(&data, spillcount);
	}

	/* construct interferences */
	for (i = 0; i < spillcount; ++i) {
		size_t   i2;
		ir_node *spill1 = spills[i]->spill;
		if (is_NoMem(spill1))
			continue;

		for (i2 = i+1; i2 < spillcount; ++i2) {
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
	affinity_edge_count = ARR_LEN(env->affinity_edges);
	qsort(env->affinity_edges, affinity_edge_count,
	      sizeof(env->affinity_edges[0]), cmp_affinity);

	/* try to merge affine nodes */
	for (i = 0; i < affinity_edge_count; ++i) {
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
	for (i = 0; i < spillcount; ++i) {
		size_t i2;
		int    s1 = uf_find(spillslot_unionfind, i);
		if (s1 != (int)i)
			continue;

		for (i2 = i+1; i2 < spillcount; ++i2) {
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
	for (i = 0; i < spillcount; ++i) {
		spills[i]->spillslot = uf_find(spillslot_unionfind, i);
	}

	obstack_free(&data, 0);
}

typedef struct spill_slot_t {
	int size;
	int align;
	ir_entity *entity;
} spill_slot_t;

typedef struct memperm_entry_t {
	ir_node* node;
	int pos;
	ir_entity *in;
	ir_entity *out;
	struct memperm_entry_t *next;
} memperm_entry_t;

typedef struct memperm_t {
	ir_node *block;
	int entrycount;
	memperm_entry_t *entries;
} memperm_t;

static int cmp_memperm(const void* d1, const void* d2, size_t size)
{
	const memperm_t* e1 = (const memperm_t*)d1;
	const memperm_t* e2 = (const memperm_t*)d2;
	(void) size;

	return e1->block != e2->block;
}

static memperm_t *get_memperm(be_fec_env_t *env, ir_node *block)
{
	memperm_t entry, *res;
	int hash;

	entry.block = block;
	hash        = hash_irn(block);

	res = set_find(memperm_t, env->memperms, &entry, sizeof(entry), hash);

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

static void assign_spill_entity(be_fec_env_t *env,
                                ir_node *node, ir_entity *entity)
{
	if (is_NoMem(node))
		return;
	if (is_Sync(node)) {
		foreach_irn_in(node, i, in) {
			assert(!is_Phi(in));
			assign_spill_entity(env, in, entity);
		}
		return;
	}

	/* beware: we might have Stores with Memory Proj's, ia32 fisttp for
	   instance */
	node = skip_Proj(node);
	assert(arch_get_frame_entity(node) == NULL);
	env->set_frame_entity(node, entity);
}

/**
 * Create stack entities for the spillslots and assign them to the spill and
 * reload nodes.
 */
static void assign_spillslots(be_fec_env_t *env)
{
	spill_t      **spills     = env->spills;
	size_t         spillcount = ARR_LEN(spills);
	spill_slot_t  *spillslots = ALLOCANZ(spill_slot_t, spillcount);
	size_t         s;

	/* construct spillslots */
	for (s = 0; s < spillcount; ++s) {
		const spill_t *spill  = spills[s];
		int            slotid = spill->spillslot;
		const ir_mode *mode   = spill->mode;
		spill_slot_t  *slot   = & (spillslots[slotid]);
		int            size   = get_mode_size_bytes(mode);
		int            align  = spill->alignment;

		if (slot->align == 0 && slot->size == 0) {
			slot->align = align;
			slot->size = size;
		} else {
			enlarge_spillslot(slot, align, size);
		}
	}

	for (s = 0; s < spillcount; ++s) {
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
			assign_spill_entity(env, node, slot->entity);
		}
	}

	for (s = 0; s < ARR_LEN(env->reloads); ++s) {
		ir_node            *reload    = env->reloads[s];
		ir_node            *spillnode = get_memory_edge(reload);
		const spill_t      *spill     = get_spill(env, spillnode);
		const spill_slot_t *slot      = &spillslots[spill->spillslot];

		assert(slot->entity != NULL);

		env->set_frame_entity(reload, slot->entity);
	}
}

static void create_memperms(be_fec_env_t *env)
{
	foreach_set(env->memperms, memperm_t, memperm) {
		ir_node         **nodes = ALLOCAN(ir_node*, memperm->entrycount);
		memperm_entry_t  *entry;
		ir_node          *mempermnode;
		int               i;

		assert(memperm->entrycount > 0);

		for (entry = memperm->entries, i = 0; entry != NULL; entry = entry->next, ++i) {
			ir_node* arg = get_irn_n(entry->node, entry->pos);
			nodes[i] = arg;
		}

		mempermnode = be_new_MemPerm(memperm->block, memperm->entrycount,
		                             nodes);

		/* insert node into schedule */
		ir_node *const blockend = be_get_end_of_block_insertion_point(memperm->block);
		sched_add_before(blockend, mempermnode);
		stat_ev_dbl("mem_perm", memperm->entrycount);

		i = 0;
		for (entry = memperm->entries; entry != NULL; entry = entry->next, ++i) {
			ir_node *proj;
			ir_node* arg = get_irn_n(entry->node, entry->pos);

			be_set_MemPerm_in_entity(mempermnode, i, entry->in);
			be_set_MemPerm_out_entity(mempermnode, i, entry->out);
			proj = new_r_Proj(mempermnode, get_irn_mode(arg), i);

			set_irn_n(entry->node, entry->pos, proj);
		}
	}
}

static unsigned count_spillslots(const be_fec_env_t *env)
{
	size_t         spillcount = ARR_LEN(env->spills);
	unsigned       slotcount  = 0;
	size_t         s;

	unsigned *const counted = rbitset_alloca(spillcount);
	for (s = 0; s < spillcount; ++s) {
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
