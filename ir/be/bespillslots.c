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
 * @brief       Spillslot coalescer.
 * @author      Matthias Braun
 * @date        26.07.2006
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "set.h"
#include "array.h"
#include "irgwalk.h"
#include "ircons.h"
#include "irprintf.h"
#include "execfreq.h"
#include "unionfind.h"
#include "irdump_t.h"

#include "benode_t.h"
#include "besched.h"
#include "bespillslots.h"
#include "bechordal_t.h"
#include "bejavacoal.h"
#include "benodesets.h"
#include "bestatevent.h"
#include "bespilloptions.h"
#include "bemodule.h"
#include "beintlive_t.h"
#include "beirg_t.h"
#include "bearch_t.h"

#define DBG_COALESCING		1
#define DBG_INTERFERENCES	2

DEBUG_ONLY(
static firm_dbg_module_t *dbg = NULL;
)

typedef struct _spill_t {
	ir_node *spill;
	/** mode of the spilled value */
	const ir_mode *mode;
	/** alignment for the spilled value */
	int alignment;
	/** index into spillslot_unionfind unionfind structure */
	int spillslot;
} spill_t;

typedef struct _affinity_edge_t {
	double affinity;
	int slot1, slot2;
} affinity_edge_t;

struct _be_fec_env_t {
	struct obstack obst;
	const arch_env_t *arch_env;
	be_irg_t *birg;
	set *spills;
	ir_node **reloads;
	affinity_edge_t **affinity_edges;
	set *memperms;
};

/** Compare 2 affinity edges (used in quicksort) */
static int cmp_affinity(const void *d1, const void *d2)
{
	const affinity_edge_t * const *e1 = d1;
	const affinity_edge_t * const *e2 = d2;

	// sort in descending order
	return (*e1)->affinity < (*e2)->affinity ? 1 : -1;
}

static int cmp_spill(const void* d1, const void* d2, size_t size)
{
	const spill_t* s1 = d1;
	const spill_t* s2 = d2;
	return s1->spill != s2->spill;
}

static spill_t *get_spill(be_fec_env_t *env, ir_node *node)
{
	spill_t spill, *res;
	int hash = nodeset_hash(node);

	spill.spill = node;
	res = set_find(env->spills, &spill, sizeof(spill), hash);

	return res;
}

/*
 *   ____      _ _           _     ____        _ _ _
 *  / ___|___ | | | ___  ___| |_  / ___| _ __ (_) | |___
 * | |   / _ \| | |/ _ \/ __| __| \___ \| '_ \| | | / __|
 * | |__| (_) | | |  __/ (__| |_   ___) | |_) | | | \__ \
 *  \____\___/|_|_|\___|\___|\__| |____/| .__/|_|_|_|___/
 *                                      |_|
 */

static INLINE ir_node *get_memory_edge(const ir_node *node)
{
	int i, arity;

	arity = get_irn_arity(node);
	for(i = arity - 1; i >= 0; --i) {
		ir_node *arg = get_irn_n(node, i);
		if(get_irn_mode(arg) == mode_M)
			return arg;
	}

	return NULL;
}

static spill_t *collect_spill(be_fec_env_t *env, ir_node *node,
		                      const ir_mode *mode, int align)
{
	spill_t spill, *res;
	int hash = nodeset_hash(node);

	/* insert into set of spills if not already there */
	spill.spill = node;
	res = set_find(env->spills, &spill, sizeof(spill), hash);

	if(res == NULL) {
		spill.spillslot = set_count(env->spills);
		spill.mode = mode;
		spill.alignment = align;
		res = set_insert(env->spills, &spill, sizeof(spill), hash);
	} else {
		assert(res->mode == mode);
		assert(res->alignment == align);
	}

	return res;
}

static spill_t *collect_memphi(be_fec_env_t *env, ir_node *node,
                               const ir_mode *mode, int align)
{
	int i, arity;
	spill_t spill, *res;
	int hash = nodeset_hash(node);
	const ir_exec_freq *exec_freq = be_get_birg_exec_freq(env->birg);

	assert(is_Phi(node));

	spill.spill = node;
	res = set_find(env->spills, &spill, sizeof(spill), hash);
	if(res != NULL) {
		assert(res->mode == mode);
		assert(res->alignment == align);
		return res;
	}

	spill.spillslot = set_count(env->spills);
	spill.mode = mode;
	spill.alignment = align;
	res = set_insert(env->spills, &spill, sizeof(spill), hash);

	// collect attached spills and mem-phis
	arity = get_irn_arity(node);
	for(i = 0; i < arity; ++i) {
		affinity_edge_t *affinty_edge;
		ir_node *arg = get_irn_n(node, i);
		spill_t *arg_spill;

		if(is_Phi(arg)) {
			arg_spill = collect_memphi(env, arg, mode, align);
		} else {
			arg_spill = collect_spill(env, arg, mode, align);
		}

		// add an affinity edge
		affinty_edge = obstack_alloc(&env->obst, sizeof(affinty_edge[0]));
		affinty_edge->affinity = get_block_execfreq(exec_freq, get_nodes_block(arg));
		affinty_edge->slot1 = res->spillslot;
		affinty_edge->slot2 = arg_spill->spillslot;
		ARR_APP1(affinity_edge_t*, env->affinity_edges, affinty_edge);
	}

	return res;
}

void be_node_needs_frame_entity(be_fec_env_t *env, ir_node *node,
                                const ir_mode *mode, int align)
{
	ir_node *spillnode = get_memory_edge(node);
	spill_t *spill;

	assert(spillnode != NULL);

	if (is_Phi(spillnode)) {
		spill = collect_memphi(env, spillnode, mode, align);
	} else {
		spill = collect_spill(env, spillnode, mode, align);
	}

	ARR_APP1(ir_node *, env->reloads, node);
}

/*
 *   ____            _                      ____  _       _
 *  / ___|___   __ _| | ___  ___  ___ ___  / ___|| | ___ | |_ ___
 * | |   / _ \ / _` | |/ _ \/ __|/ __/ _ \ \___ \| |/ _ \| __/ __|
 * | |__| (_) | (_| | |  __/\__ \ (_|  __/  ___) | | (_) | |_\__ \
 *  \____\___/ \__,_|_|\___||___/\___\___| |____/|_|\___/ \__|___/
 */

static int merge_interferences(be_fec_env_t *env, bitset_t** interferences,
                               int* spillslot_unionfind, int s1, int s2)
{
	int res;
	int i;
	int spillcount;

	// merge spillslots and interferences
	res = uf_union(spillslot_unionfind, s1, s2);
	// we assume that we always merge s2 to s1 so swap s1, s2 if necessary
	if(res != 0) {
		int t = s1;
		s1 = s2;
		s2 = t;
	}

	bitset_or(interferences[s1], interferences[s2]);

	// update other interferences
	spillcount = set_count(env->spills);
	for(i = 0; i < spillcount; ++i) {
		bitset_t *intfs = interferences[i];
		if(bitset_is_set(intfs, s2))
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
	int spillcount;
	spill_t **spilllist;
	spill_t *spill;
	int i, i2;
	int affinity_edge_count;
	bitset_t **interferences;
	int* spillslot_unionfind;

	spillcount = set_count(env->spills);
	if(spillcount == 0)
		return;

	DBG((dbg, DBG_COALESCING, "Coalescing %d spillslots\n", spillcount));

	interferences = alloca(spillcount * sizeof(interferences[0]));
	spillslot_unionfind = alloca(spillcount * sizeof(spillslot_unionfind[0]));
	spilllist = alloca(spillcount * sizeof(spilllist[0]));

	uf_init(spillslot_unionfind, 0, spillcount);

	DEBUG_ONLY(
		memset(spilllist, 0, spillcount * sizeof(spilllist[0]));
	);

	for(spill = set_first(env->spills), i = 0; spill != NULL; spill = set_next(env->spills), ++i) {
		assert(spill->spillslot < spillcount);
		spilllist[spill->spillslot] = spill;
	}

	for(i = 0; i < spillcount; ++i) {
		interferences[i] = bitset_alloca(spillcount);
	}

	/* construct interferences */
	for (i = 0; i < spillcount; ++i) {
		ir_node *spill1 = spilllist[i]->spill;

		if (is_NoMem(spill1))
			continue;

		for(i2 = i+1; i2 < spillcount; ++i2) {
			ir_node *spill2 = spilllist[i2]->spill;

			if (is_NoMem(spill2))
				continue;

			if (values_interfere(env->birg, spill1, spill2)) {
				DBG((dbg, DBG_INTERFERENCES, "Slot %d and %d interfere\n", i, i2));
				bitset_set(interferences[i], i2);
				bitset_set(interferences[i2], i);
			}
		}
	}

	/* sort affinity edges */
	affinity_edge_count = ARR_LEN(env->affinity_edges);
	qsort(env->affinity_edges, affinity_edge_count, sizeof(env->affinity_edges[0]), cmp_affinity);

	//dump_interference_graph(env, interferences, "before");

	/* try to merge affine nodes */
	for(i = 0; i < affinity_edge_count; ++i) {
		const affinity_edge_t *edge = env->affinity_edges[i];
		int s1 = uf_find(spillslot_unionfind, edge->slot1);
		int s2 = uf_find(spillslot_unionfind, edge->slot2);

		/* test if values interfere */
		if (bitset_is_set(interferences[s1], s2)) {
			assert(bitset_is_set(interferences[s2], s1));
			continue;
		}

		DBG((dbg, DBG_COALESCING, "Merging %d and %d because of affinity edge\n", s1, s2));

		merge_interferences(env, interferences, spillslot_unionfind, s1, s2);
	}

	// try to merge as much remaining spillslots as possible
	for(i = 0; i < spillcount; ++i) {
		int s1 = uf_find(spillslot_unionfind, i);
		if(s1 != i)
			continue;

		for(i2 = i+1; i2 < spillcount; ++i2) {
			int s2 = uf_find(spillslot_unionfind, i2);
			if(s2 != i2)
				continue;

			/* test if values interfere
			 * we have to test n1-n2 and n2-n1, because only 1 side gets updated
			 * when node merging occurs
			 */
			if(bitset_is_set(interferences[s1], s2)) {
				assert(bitset_is_set(interferences[s2], s1));
				continue;
			}

			DBG((dbg, DBG_COALESCING, "Merging %d and %d because it is possible\n", s1, s2));

			if(merge_interferences(env, interferences, spillslot_unionfind, s1, s2) != 0) {
				// we can break the loop here, because s2 is the new supernode now
				// and we'll test s2 again later anyway
				break;
			}
		}
	}

	// assign spillslots to spills
	for(i = 0; i < spillcount; ++i) {
		spill_t *spill = spilllist[i];

		spill->spillslot = uf_find(spillslot_unionfind, i);
	}

	//dump_interference_graph(env, interferences, "after");
}

/*
 *     _            _               _____       _   _ _   _
 *    / \   ___ ___(_) __ _ _ __   | ____|_ __ | |_(_) |_(_) ___  ___
 *   / _ \ / __/ __| |/ _` | '_ \  |  _| | '_ \| __| | __| |/ _ \/ __|
 *  / ___ \\__ \__ \ | (_| | | | | | |___| | | | |_| | |_| |  __/\__ \
 * /_/   \_\___/___/_|\__, |_| |_| |_____|_| |_|\__|_|\__|_|\___||___/
 *                    |___/
 */

typedef struct _spill_slot_t {
	int size;
	int align;
	ir_entity *entity;
} spill_slot_t;

typedef struct _memperm_entry_t {
	ir_node* node;
	int pos;
	ir_entity *in;
	ir_entity *out;
	struct _memperm_entry_t *next;
} memperm_entry_t;

typedef struct _memperm_t {
	ir_node *block;
	int entrycount;
	memperm_entry_t *entries;
} memperm_t;

static int cmp_memperm(const void* d1, const void* d2, size_t size)
{
	const memperm_t* e1 = d1;
	const memperm_t* e2 = d2;
	return e1->block != e2->block;
}

static memperm_t *get_memperm(be_fec_env_t *env, ir_node *block)
{
	memperm_t entry, *res;
	int hash;

	entry.block = block;
	hash = nodeset_hash(block);

	res = set_find(env->memperms, &entry, sizeof(entry), hash);

	if(res == NULL) {
		entry.entrycount = 0;
		entry.entries = NULL;
		res = set_insert(env->memperms, &entry, sizeof(entry), hash);
	}

	return res;
}

static ir_entity* create_stack_entity(be_fec_env_t *env, spill_slot_t *slot)
{
	ir_graph *irg = be_get_birg_irg(env->birg);
	ir_type *frame = get_irg_frame_type(irg);
	ir_entity *res = frame_alloc_area(frame, slot->size, slot->align, 0);

	/* adjust size of the entity type... */
	ir_type *enttype = get_entity_type(res);
	set_type_size_bytes(enttype, slot->size);

	slot->entity = res;

	return res;
}

/**
 * Enlarges a spillslot (if necessary) so that it can carry a value of size
 * @p othersize and alignment @p otheralign.
 */
static void enlarge_spillslot(spill_slot_t *slot, int otheralign, int othersize)
{
	if(othersize > slot->size) {
		slot->size = othersize;
	}
	if(otheralign > slot->align) {
		if(otheralign % slot->align != 0)
			slot->align *= otheralign;
		else
			slot->align = otheralign;
	} else if(slot->align % otheralign != 0) {
		slot->align *= otheralign;
	}
}

/**
 * Create stack entities for the spillslots and assign them to the spill and
 * reload nodes.
 */
static void assign_spillslots(be_fec_env_t *env)
{
	const arch_env_t *arch_env = env->arch_env;
	int i;
	int spillcount;
	spill_t *spill;
	spill_slot_t* spillslots;

	spillcount = set_count(env->spills);
	spillslots = alloca(spillcount * sizeof(spillslots[0]));

	memset(spillslots, 0, spillcount * sizeof(spillslots[0]));

	// construct spillslots
	for(spill = set_first(env->spills); spill != NULL; spill = set_next(env->spills)) {
		int slotid = spill->spillslot;
		const ir_mode *mode = spill->mode;
		spill_slot_t *slot = & (spillslots[slotid]);
		int size = get_mode_size_bytes(mode);
		int align = spill->alignment;

		if(slot->align == 0 && slot->size == 0) {
			slot->align = align;
			slot->size = size;
		} else {
			enlarge_spillslot(slot, align, size);
		}
	}

	for(spill = set_first(env->spills); spill != NULL; spill = set_next(env->spills)) {
		spill_slot_t *slot;
		ir_node *node = spill->spill;
		int slotid = spill->spillslot;

		slot = &spillslots[slotid];
		if(slot->entity == NULL) {
			create_stack_entity(env, slot);
		}

		if(is_Phi(node)) {
			int i, arity;
			ir_node *block = get_nodes_block(node);

			// should be a PhiM
			assert(is_Phi(node));

			for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
				ir_node *arg = get_irn_n(node, i);
				ir_node *predblock = get_Block_cfgpred_block(block, i);
				spill_t *argspill;
				int argslotid;

				argspill = get_spill(env, arg);
				assert(argspill != NULL);

				argslotid = argspill->spillslot;
				if(slotid != argslotid) {
					memperm_t *memperm;
					memperm_entry_t *entry;
					spill_slot_t *argslot = &spillslots[argslotid];
					if(argslot->entity == NULL) {
						create_stack_entity(env, argslot);
					}

					memperm = get_memperm(env, predblock);

					entry = obstack_alloc(&env->obst, sizeof(entry[0]));
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
			if(!is_NoMem(node))
				arch_set_frame_entity(arch_env, node, slot->entity);
		}
	}

	for(i = 0; i < ARR_LEN(env->reloads); ++i) {
		ir_node* reload = env->reloads[i];
		ir_node* spillnode = get_memory_edge(reload);
		spill_t *spill = get_spill(env, spillnode);
		const spill_slot_t *slot = & spillslots[spill->spillslot];

		assert(slot->entity != NULL);

		arch_set_frame_entity(arch_env, reload, slot->entity);
	}
}

/**
 * Returns the last node in a block which is no control flow changing node
 */
static ir_node *get_end_of_block_insertion_point(ir_node* block)
{
	ir_node* ins = sched_last(block);
	while(is_Proj(ins) && get_irn_mode(ins) == mode_X) {
		ins = sched_prev(ins);
		assert(ins != NULL);
	}

	if(is_cfop(ins)) {
		while(1) {
			ir_node *prev = sched_prev(ins);
			if(!is_cfop(prev))
				break;
			ins = prev;
		}
	}

	return ins;
}

static void create_memperms(be_fec_env_t *env)
{
	const arch_env_t *arch_env = env->arch_env;
	ir_graph *irg = be_get_birg_irg(env->birg);
	memperm_t *memperm;

	for(memperm = set_first(env->memperms); memperm != NULL; memperm = set_next(env->memperms)) {
		int i;
		memperm_entry_t *entry;
		ir_node *blockend;
		ir_node** nodes = alloca(memperm->entrycount * sizeof(nodes[0]));
		ir_node* mempermnode;

		assert(memperm->entrycount > 0);

		for(entry = memperm->entries, i = 0; entry != NULL; entry = entry->next, ++i) {
			ir_node* arg = get_irn_n(entry->node, entry->pos);
			nodes[i] = arg;
		}

		mempermnode = be_new_MemPerm(arch_env, irg, memperm->block,
		                             memperm->entrycount, nodes);

		// insert node into schedule
		blockend = get_end_of_block_insertion_point(memperm->block);
		sched_add_before(blockend, mempermnode);
		be_stat_ev("mem_perm", memperm->entrycount);

		i = 0;
		for(entry = memperm->entries; entry != NULL; entry = entry->next, ++i) {
			ir_node *proj;
			ir_node* arg = get_irn_n(entry->node, entry->pos);

			be_set_MemPerm_in_entity(mempermnode, i, entry->in);
			be_set_MemPerm_out_entity(mempermnode, i, entry->out);
			set_irg_current_block(irg, memperm->block);
			proj = new_Proj(mempermnode, get_irn_mode(arg), i);
			sched_add_before(blockend, proj);

			set_irn_n(entry->node, entry->pos, proj);
		}
	}
}

static int count_spillslots(const be_fec_env_t *env)
{
	const spill_t *spill;
	int spillcount = set_count(env->spills);
	bitset_t *counted = bitset_alloca(spillcount);
	int slotcount;

	slotcount = 0;
	for(spill = set_first(env->spills); spill != NULL;
	    spill = set_next(env->spills)) {
		int spillslot = spill->spillslot;
		if(!bitset_is_set(counted, spillslot)) {
			slotcount++;
			bitset_set(counted, spillslot);
		}
	}

	return slotcount;
}

be_fec_env_t *be_new_frame_entity_coalescer(be_irg_t *birg)
{
	const arch_env_t *arch_env = birg->main_env->arch_env;
	be_fec_env_t     *env      = xmalloc(sizeof(env[0]));

	be_assure_liveness(birg);

	obstack_init(&env->obst);
	env->arch_env       = arch_env;
	env->birg           = birg;
	env->spills         = new_set(cmp_spill, 10);
	env->reloads        = NEW_ARR_F(ir_node*, 0);
	env->affinity_edges = NEW_ARR_F(affinity_edge_t*, 0);
	env->memperms       = new_set(cmp_memperm, 10);

	return env;
}

void be_free_frame_entity_coalescer(be_fec_env_t *env)
{
	del_set(env->memperms);
	DEL_ARR_F(env->reloads);
	DEL_ARR_F(env->affinity_edges);
	del_set(env->spills);
	obstack_free(&env->obst, NULL);

	free(env);
}

void be_assign_entities(be_fec_env_t *env)
{
	if(be_stat_ev_is_active()) {
		int count = set_count(env->spills);
		be_stat_ev("spillslots", count);
	}

	if(be_coalesce_spill_slots) {
		do_greedy_coalescing(env);
	}

	if(be_stat_ev_is_active()) {
		int count = count_spillslots(env);
		be_stat_ev("spillslots_after_coalescing", count);
	}

	assign_spillslots(env);

	create_memperms(env);
}

/**
 * This walker function searches for reloads and collects all the spills
 * and memphis attached to them.
 */
static void collect_spills_walker(ir_node *node, void *data)
{
	be_fec_env_t *env = data;
	const arch_env_t *arch_env = env->arch_env;
	const ir_mode *mode;
	const arch_register_class_t *cls;
	int align;

	/* classify returns classification of the irn the proj is attached to */
	if (is_Proj(node))
		return;

	if (!arch_irn_class_is(arch_env, node, reload))
		return;

	mode  = get_irn_mode(node);
	cls   = arch_get_irn_reg_class(arch_env, node, -1);
	align = arch_isa_get_reg_class_alignment(arch_env_get_isa(arch_env), cls);

	be_node_needs_frame_entity(env, node, mode, align);
}

void be_coalesce_spillslots(be_irg_t *birg)
{
	be_fec_env_t *env = be_new_frame_entity_coalescer(birg);

	/* collect reloads */
	irg_walk_graph(birg->irg, NULL, collect_spills_walker, env);

	be_assign_entities(env);

	be_free_frame_entity_coalescer(env);
}

void be_init_spillslots(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.spillslots");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spillslots);
