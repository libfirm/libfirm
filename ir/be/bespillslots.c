/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
#include "config.h"

#include <stdlib.h>

#include "set.h"
#include "array.h"
#include "irgwalk.h"
#include "ircons.h"
#include "irprintf.h"
#include "execfreq.h"
#include "unionfind.h"
#include "irdump_t.h"

#include "benode.h"
#include "besched.h"
#include "bespill.h"
#include "bespillslots.h"
#include "bechordal_t.h"
#include "bestatevent.h"
#include "bemodule.h"
#include "beintlive_t.h"
#include "beirg.h"
#include "bearch.h"

#define DBG_COALESCING      1
#define DBG_INTERFERENCES   2

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct spill_t {
	ir_node       *spill;
	const ir_mode *mode;      /**< mode of the spilled value */
	int            alignment; /**< alignment for the spilled value */
	int            spillslot; /**< index into spillslot_unionfind structure */
} spill_t;

typedef struct affinity_edge_t {
	double affinity;
	int    slot1;
	int    slot2;
} affinity_edge_t;

struct be_fec_env_t {
	struct obstack         obst;
	ir_graph              *irg;
	set                   *spills;
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
	const affinity_edge_t * const *e1 = (const affinity_edge_t**)d1;
	const affinity_edge_t * const *e2 = (const affinity_edge_t**)d2;

	/* sort in descending order */
	return (*e1)->affinity < (*e2)->affinity ? 1 : -1;
}

static int cmp_spill(const void* d1, const void* d2, size_t size)
{
	const spill_t* s1 = (const spill_t*)d1;
	const spill_t* s2 = (const spill_t*)d2;
	(void) size;

	return s1->spill != s2->spill;
}

static spill_t *get_spill(be_fec_env_t *env, ir_node *node)
{
	spill_t spill, *res;
	int hash = hash_irn(node);

	spill.spill = node;
	res = (spill_t*)set_find(env->spills, &spill, sizeof(spill), hash);

	return res;
}


static inline ir_node *get_memory_edge(const ir_node *node)
{
	int i, arity;

	arity = get_irn_arity(node);
	for (i = arity - 1; i >= 0; --i) {
		ir_node *arg = get_irn_n(node, i);
		if (get_irn_mode(arg) == mode_M)
			return arg;
	}

	return NULL;
}

static spill_t *collect_spill(be_fec_env_t *env, ir_node *node,
		                      const ir_mode *mode, int align)
{
	spill_t spill, *res;
	int     hash = hash_irn(node);

	/* insert into set of spills if not already there */
	spill.spill = node;
	res         = (spill_t*)set_find(env->spills, &spill, sizeof(spill), hash);

	if (res == NULL) {
		spill.spillslot = set_count(env->spills);
		spill.mode      = mode;
		spill.alignment = align;
		res             = (spill_t*)set_insert(env->spills, &spill, sizeof(spill), hash);
		DB((dbg, DBG_COALESCING, "Slot %d: %+F\n", spill.spillslot, node));
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
	int hash = hash_irn(node);
	const ir_exec_freq *exec_freq = be_get_irg_exec_freq(env->irg);

	assert(is_Phi(node));

	spill.spill = node;
	res = (spill_t*)set_find(env->spills, &spill, sizeof(spill), hash);
	if (res != NULL) {
		assert(res->mode == mode);
		assert(res->alignment == align);
		return res;
	}

	spill.spillslot = set_count(env->spills);
	spill.mode      = mode;
	spill.alignment = align;
	DB((dbg, DBG_COALESCING, "Slot %d: %+F\n", spill.spillslot, node));
	res             = (spill_t*)set_insert(env->spills, &spill, sizeof(spill), hash);

	/* collect attached spills and mem-phis */
	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		affinity_edge_t *affinty_edge;
		ir_node *arg = get_irn_n(node, i);
		spill_t *arg_spill;

		if (is_Phi(arg)) {
			arg_spill = collect_memphi(env, arg, mode, align);
		} else {
			arg_spill = collect_spill(env, arg, mode, align);
		}

		/* add an affinity edge */
		affinty_edge           = OALLOC(&env->obst, affinity_edge_t);
		affinty_edge->affinity = get_block_execfreq(exec_freq, get_nodes_block(arg));
		affinty_edge->slot1    = res->spillslot;
		affinty_edge->slot2    = arg_spill->spillslot;
		ARR_APP1(affinity_edge_t*, env->affinity_edges, affinty_edge);
	}

	return res;
}

void be_node_needs_frame_entity(be_fec_env_t *env, ir_node *node,
                                const ir_mode *mode, int align)
{
	ir_node *spillnode = get_memory_edge(node);

	assert(spillnode != NULL);

	/* walk upwards and collect all phis and spills on this way */
	if (is_Phi(spillnode)) {
		collect_memphi(env, spillnode, mode, align);
	} else {
		collect_spill(env, spillnode, mode, align);
	}

	ARR_APP1(ir_node *, env->reloads, node);
}



static int merge_interferences(be_fec_env_t *env, bitset_t** interferences,
                               int* spillslot_unionfind, int s1, int s2)
{
	int res;
	int i;
	int spillcount;

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
	spillcount = set_count(env->spills);
	for (i = 0; i < spillcount; ++i) {
		bitset_t *intfs = interferences[i];
		if (bitset_is_set(intfs, s2))
			bitset_set(intfs, s1);
	}

	return res;
}

static int my_values_interfere2(ir_graph *irg, const ir_node *a,
                                const ir_node *b)
{
	be_lv_t *lv = be_get_irg_liveness(irg);

    int a2b = _value_dominates(a, b);
    int b2a = _value_dominates(b, a);

    /* If there is no dominance relation, they do not interfere. */
    if ((a2b | b2a) > 0) {
        const ir_edge_t *edge;
        ir_node *bb;

        /*
         * Adjust a and b so, that a dominates b if
         * a dominates b or vice versa.
         */
        if (b2a) {
            const ir_node *t = a;
            a = b;
            b = t;
        }

        bb = get_nodes_block(b);

        /*
         * If a is live end in b's block it is
         * live at b's definition (a dominates b)
         */
        if (be_is_live_end(lv, bb, a))
            return 1;

        /*
         * Look at all usages of a.
         * If there's one usage of a in the block of b, then
         * we check, if this use is dominated by b, if that's true
         * a and b interfere. Note that b must strictly dominate the user,
         * since if b is the last user of in the block, b and a do not
         * interfere.
         * Uses of a not in b's block can be disobeyed, because the
         * check for a being live at the end of b's block is already
         * performed.
         */
        foreach_out_edge(a, edge) {
            const ir_node *user = get_edge_src_irn(edge);
			if (is_Sync(user)) {
				const ir_edge_t *edge2;
				foreach_out_edge(user, edge2) {
					const ir_node *user2 = get_edge_src_irn(edge2);
					assert(!is_Sync(user2));
					if (get_nodes_block(user2) == bb && !is_Phi(user2) &&
					   _value_strictly_dominates(b, user2))
						return 1;
				}
			} else {
				if (get_nodes_block(user) == bb && !is_Phi(user) &&
						_value_strictly_dominates(b, user))
                return 1;
			}
        }
    }

	return 0;
}

/**
 * same as values_interfere but with special handling for Syncs
 */
static int my_values_interfere(ir_graph *irg, ir_node *a, ir_node *b)
{
	if (is_Sync(a)) {
		int i, arity = get_irn_arity(a);
		for (i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(a, i);
			if (my_values_interfere(irg, in, b))
				return 1;
		}
		return 0;
	} else if (is_Sync(b)) {
		int i, arity = get_irn_arity(b);
		for (i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(b, i);
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
	int spillcount;
	spill_t **spilllist;
	spill_t *spill;
	int i, i2;
	int affinity_edge_count;
	bitset_t **interferences;
	int* spillslot_unionfind;
	struct obstack data;

	spillcount = set_count(env->spills);
	if (spillcount == 0)
		return;

	obstack_init(&data);

	DB((dbg, DBG_COALESCING, "Coalescing %d spillslots\n", spillcount));

	interferences       = OALLOCN(&data, bitset_t*, spillcount);
	spillslot_unionfind = OALLOCN(&data, int,       spillcount);
	spilllist           = OALLOCN(&data, spill_t*,  spillcount);

	uf_init(spillslot_unionfind, spillcount);

	DEBUG_ONLY(
		memset(spilllist, 0, spillcount * sizeof(spilllist[0]));
	);

	i = 0;
	foreach_set(env->spills, spill_t*, spill) {
		assert(spill->spillslot < spillcount);
		spilllist[spill->spillslot] = spill;
		++i;
	}

	for (i = 0; i < spillcount; ++i) {
		interferences[i] = bitset_obstack_alloc(&data, spillcount);
	}

	/* construct interferences */
	for (i = 0; i < spillcount; ++i) {
		ir_node *spill1 = spilllist[i]->spill;

		if (is_NoMem(spill1))
			continue;

		for (i2 = i+1; i2 < spillcount; ++i2) {
			ir_node *spill2 = spilllist[i2]->spill;

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

	/*dump_interference_graph(env, interferences, "before"); */

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
		int s1 = uf_find(spillslot_unionfind, i);
		if (s1 != i)
			continue;

		for (i2 = i+1; i2 < spillcount; ++i2) {
			int s2 = uf_find(spillslot_unionfind, i2);
			if (s2 != i2)
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
		spill_t *spill = spilllist[i];

		spill->spillslot = uf_find(spillslot_unionfind, i);
	}

	/*dump_interference_graph(env, interferences, "after");*/
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

	res = (memperm_t*)set_find(env->memperms, &entry, sizeof(entry), hash);

	if (res == NULL) {
		entry.entrycount = 0;
		entry.entries = NULL;
		res = (memperm_t*)set_insert(env->memperms, &entry, sizeof(entry), hash);
	}

	return res;
}

static ir_entity* create_stack_entity(be_fec_env_t *env, spill_slot_t *slot)
{
	ir_graph  *irg   = env->irg;
	ir_type   *frame = get_irg_frame_type(irg);
	ir_entity *res   = frame_alloc_area(frame, slot->size, slot->align,
	                                    env->at_begin);

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
		int i, arity;

		arity = get_irn_arity(node);
		for (i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(node, i);
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
	int           spillcount = set_count(env->spills);
	spill_slot_t *spillslots = ALLOCANZ(spill_slot_t, spillcount);
	spill_t      *spill;
	size_t        i;

	/* construct spillslots */
	foreach_set(env->spills, spill_t*, spill) {
		int slotid = spill->spillslot;
		const ir_mode *mode = spill->mode;
		spill_slot_t *slot = & (spillslots[slotid]);
		int size = get_mode_size_bytes(mode);
		int align = spill->alignment;

		if (slot->align == 0 && slot->size == 0) {
			slot->align = align;
			slot->size = size;
		} else {
			enlarge_spillslot(slot, align, size);
		}
	}

	foreach_set(env->spills, spill_t*, spill) {
		ir_node      *node   = spill->spill;
		int           slotid = spill->spillslot;
		spill_slot_t *slot;

		slot = &spillslots[slotid];
		if (slot->entity == NULL) {
			create_stack_entity(env, slot);
		}

		if (is_Phi(node)) {
			int i, arity;
			ir_node *block = get_nodes_block(node);

			/* should be a PhiM */
			assert(is_Phi(node));

			for (i = 0, arity = get_irn_arity(node); i < arity; ++i) {
				ir_node *arg = get_irn_n(node, i);
				ir_node *predblock = get_Block_cfgpred_block(block, i);
				spill_t *argspill;
				int argslotid;

				argspill = get_spill(env, arg);
				assert(argspill != NULL);

				argslotid = argspill->spillslot;
				if (slotid != argslotid) {
					memperm_t *memperm;
					memperm_entry_t *entry;
					spill_slot_t *argslot = &spillslots[argslotid];
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

	for (i = 0; i < ARR_LEN(env->reloads); ++i) {
		ir_node            *reload    = env->reloads[i];
		ir_node            *spillnode = get_memory_edge(reload);
		spill_t            *spill     = get_spill(env, spillnode);
		const spill_slot_t *slot      = & spillslots[spill->spillslot];

		assert(slot->entity != NULL);

		env->set_frame_entity(reload, slot->entity);
	}
}

/**
 * Returns the last node in a block which is no control flow changing node
 */
static ir_node *get_end_of_block_insertion_point(ir_node* block)
{
	ir_node* ins = sched_last(block);
	while (is_Proj(ins) && get_irn_mode(ins) == mode_X) {
		ins = sched_prev(ins);
		assert(ins != NULL);
	}

	if (is_cfop(ins)) {
		for (;;) {
			ir_node *prev = sched_prev(ins);
			if (!is_cfop(prev))
				break;
			ins = prev;
		}
	}

	return ins;
}

static void create_memperms(be_fec_env_t *env)
{
	memperm_t *memperm;

	foreach_set(env->memperms, memperm_t*, memperm) {
		ir_node         **nodes = ALLOCAN(ir_node*, memperm->entrycount);
		memperm_entry_t  *entry;
		ir_node          *blockend;
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
		blockend = get_end_of_block_insertion_point(memperm->block);
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

static int count_spillslots(const be_fec_env_t *env)
{
	const spill_t *spill;
	int spillcount = set_count(env->spills);
	bitset_t *counted = bitset_alloca(spillcount);
	int slotcount;

	slotcount = 0;
	foreach_set(env->spills, spill_t*, spill) {
		int spillslot = spill->spillslot;
		if (!bitset_is_set(counted, spillslot)) {
			slotcount++;
			bitset_set(counted, spillslot);
		}
	}

	return slotcount;
}

be_fec_env_t *be_new_frame_entity_coalescer(ir_graph *irg)
{
	be_fec_env_t *env = XMALLOCZ(be_fec_env_t);

	be_liveness_assure_chk(be_assure_liveness(irg));

	obstack_init(&env->obst);
	env->irg            = irg;
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

void be_assign_entities(be_fec_env_t *env,
                        set_frame_entity_func set_frame_entity,
                        bool alloc_entities_at_begin)
{
	env->set_frame_entity = set_frame_entity;
	env->at_begin         = alloc_entities_at_begin;

	stat_ev_dbl("spillslots", set_count(env->spills));

	if (be_coalesce_spill_slots) {
		do_greedy_coalescing(env);
	}

	stat_ev_dbl("spillslots_after_coalescing", count_spillslots(env));

	assign_spillslots(env);

	create_memperms(env);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spillslots)
void be_init_spillslots(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.spillslots");
}
