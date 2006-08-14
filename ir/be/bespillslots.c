/*
 * Author:      Matthias Braun
 * Date:		26.7.06
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "set.h"

#include "irgwalk.h"
#include "ircons.h"
#include "irprintf.h"
#include "execfreq.h"
#include "unionfind.h"
#include "type.h"
#include "irdump_t.h"

#include "benode_t.h"
#include "besched.h"
#include "bespillslots.h"
#include "bechordal_t.h"
#include "bejavacoal.h"


#define DBG_COALESCING		1
#define DBG_INTERFERENCES	2

DEBUG_ONLY(
static firm_dbg_module_t *dbg = NULL;
)

typedef struct _spill_t {
	ir_node *spill;
	/** regclass of the spilled value */
	const arch_register_class_t *cls;
	/** index into spillslot_unionfind unionfind structure */
	int spillslot;
} spill_t;

typedef struct _affinity_edge_t {
	double affinity;
	int slot1, slot2;
} affinity_edge_t;

typedef struct _ss_env_t {
	struct obstack obst;
	const arch_env_t *arch_env;
	const be_chordal_env_t *chordal_env;
	set *spills;
	ir_node **reloads;
	affinity_edge_t **affinity_edges;
	set *memperms;
} ss_env_t;

/** Compare 2 affinity edges (used in quicksort) */
static int cmp_affinity(const void *d1, const void *d2) {
	const affinity_edge_t *e1 = d1;
	const affinity_edge_t *e2 = d2;

	return e1->affinity < e2->affinity ? -1 : 1;
}

static int cmp_spill(const void* d1, const void* d2, size_t size) {
	const spill_t* s1 = d1;
	const spill_t* s2 = d2;
	return s1->spill != s2->spill;
}

static spill_t *get_spill(ss_env_t *env, ir_node *node) {
	spill_t spill, *res;
	int hash = HASH_PTR(node);

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

static ir_node *get_memory_edge(const ir_node *node) {
	int i, arity;

	arity = get_irn_arity(node);
	for(i = arity - 1; i >= 0; --i) {
		ir_node *arg = get_irn_n(node, i);
		if(get_irn_mode(arg) == mode_M)
			return arg;
	}

	return NULL;
}

static spill_t *collect_spill(ss_env_t *env, ir_node *node) {
	const arch_env_t *arch_env = env->arch_env;
	const arch_register_class_t *cls;
	spill_t spill, *res;
	int hash = HASH_PTR(node);

	assert(arch_irn_class_is(arch_env, node, spill));

	if(be_is_Spill(node)) {
		cls = arch_get_irn_reg_class(arch_env, node, be_pos_Spill_val);
	} else {
		// TODO add a way to detect the type of the spilled value
		assert(0);
	}

	spill.spill = node;
	res = set_find(env->spills, &spill, sizeof(spill), hash);

	if(res == NULL) {
		spill.spillslot = set_count(env->spills);
		spill.cls = cls;
		res = set_insert(env->spills, &spill, sizeof(spill), hash);
	}

	return res;
}

static spill_t *collect_memphi(ss_env_t *env, ir_node *node) {
	int i, arity;
	spill_t spill, *res;
	int hash = HASH_PTR(node);

	assert(is_Phi(node));

	spill.spill = node;
	res = set_find(env->spills, &spill, sizeof(spill), hash);
	if(res != NULL) {
		return res;
	}

	spill.spillslot = set_count(env->spills);
	spill.cls = NULL;
	res = set_insert(env->spills, &spill, sizeof(spill), hash);

	// is 1 of the arguments a spill?
	for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
		affinity_edge_t *affinty_edge;
		ir_node* arg = get_irn_n(node, i);
		spill_t* arg_spill;

		if(be_is_Spill(arg)) {
			arg_spill = collect_spill(env, arg);
		} else {
			// if it wasn't a spill then it must be a Mem-Phi
			assert(is_Phi(arg));
			arg_spill = collect_memphi(env, arg);
		}

		if(res->cls == NULL) {
			res->cls = arg_spill->cls;
		} else {
			assert(arg_spill->cls == NULL || res->cls == arg_spill->cls);
		}

		// add an affinity edge
		affinty_edge = obstack_alloc(&env->obst, sizeof(affinty_edge[0]));
		affinty_edge->affinity = get_block_execfreq(env->chordal_env->exec_freq, get_nodes_block(arg));
		affinty_edge->slot1 = res->spillslot;
		affinty_edge->slot2 = arg_spill->spillslot;
		ARR_APP1(affinity_edge_t*, env->affinity_edges, affinty_edge);
	}

	return res;
}

/**
 * This walker function searches for reloads and collects all the spills
 * and memphis attached to them.
 */
static void collect_spills_walker(ir_node *node, void *data) {
	ss_env_t *env = data;
	const arch_env_t *arch_env = env->arch_env;

	// @@@ ia32 classify returns classification of the irn the proj is attached
	// too, why oh why?...
	if(is_Proj(node))
		return;

	if(arch_irn_class_is(arch_env, node, reload)) {
		ir_node *spillnode = get_memory_edge(node);
		spill_t *spill;

		assert(spillnode != NULL);

		if(is_Phi(spillnode)) {
			spill = collect_memphi(env, spillnode);
		} else {
			spill = collect_spill(env, spillnode);
		}

		assert(!be_is_Reload(node) || spill->cls == arch_get_irn_reg_class(arch_env, node, -1));
		ARR_APP1(ir_node*, env->reloads, node);
	}
}

/*
 *   ____            _                      ____  _       _
 *  / ___|___   __ _| | ___  ___  ___ ___  / ___|| | ___ | |_ ___
 * | |   / _ \ / _` | |/ _ \/ __|/ __/ _ \ \___ \| |/ _ \| __/ __|
 * | |__| (_) | (_| | |  __/\__ \ (_|  __/  ___) | | (_) | |_\__ \
 *  \____\___/ \__,_|_|\___||___/\___\___| |____/|_|\___/ \__|___/
 */

static int merge_interferences(ss_env_t *env, bitset_t** interferences, int* spillslot_unionfind, int s1, int s2)
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

#if 0

static void dump_interference_graph(ss_env_t *env, bitset_t **interferences, const char* suffix) {
	char name[256];
	int i;
	int spillcount;
	spill_t *spill;
	FILE *f;
	static int cnt = 0;

	snprintf(name, sizeof(name), "%d-%s-spillslots-%s.vcg", cnt++, get_irg_dump_name(env->chordal_env->birg->irg), suffix);

	f = fopen(name, "w");
	assert(f != NULL);

	fprintf(f, "graph: {\n");

	spillcount = set_count(env->spills);
	for(spill = set_first(env->spills), i = 0; spill != NULL; spill = set_next(env->spills), ++i) {
		int slotid = spill->spillslot;
		fprintf(f, "\tnode: { title: \"n%d\" label: \"%d\" }\n", i, slotid);
	}

	for(i = 0; i < ARR_LEN(env->affinity_edges); ++i) {
		affinity_edge_t *edge = env->affinity_edges[i];
		fprintf(f, "\tedge: { sourcename: \"n%d\" targetname: \"n%d\" color: green }\n", edge->slot1, edge->slot2);
	}

	for(i = 0; i < spillcount; ++i) {
		int i2;
		for(i2 = 0; i2 < spillcount; ++i2) {
			if(bitset_is_set(interferences[i], i2)) {
				fprintf(f, "\tedge: { sourcename: \"n%d\" targetname: \"n%d\" color: red }\n", i, i2);
			}
		}
	}

	fprintf(f, "}\n");
	fclose(f);
}

static void show_stats(ss_env_t *env) {
	int spillcount;
	int slotcount;
	int *slotused;
	spill_t *spill;

	spillcount = set_count(env->spills);
	fprintf(stderr, "%s: Collected %d spills\n", get_irg_dump_name(env->chordal_env->birg->irg), spillcount);

	slotused = alloca(spillcount * sizeof(slotused[0]));
	memset(slotused, 0, spillcount * sizeof(slotused[0]));

	slotcount = 0;
	for(spill = set_first(env->spills); spill != NULL; spill = set_next(env->spills)) {
		int slot = spill->spillslot;
		if(slotused[slot] == 0) {
			slotused[slot] = 1;
			slotcount++;
		}
	}

	fprintf(stderr, "%s: Coalesced to %d spillslots\n", get_irg_dump_name(env->chordal_env->birg->irg), slotcount);
}

#endif

static void assign_spillslots(ss_env_t *env);

/**
 * A greedy coalescing algorithm for spillslots:
 *  1. Sort the list of affinity edges
 *  2. Try to merge slots with affinity edges (most expensive slots first)
 *  3. Try to merge everything else that is possible
 */
static void do_greedy_coalescing(ss_env_t *env)
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

	// construct interferences
	for(i = 0; i < spillcount; ++i) {
		for(i2 = i+1; i2 < spillcount; ++i2) {
			if(values_interfere(env->chordal_env->lv, spilllist[i]->spill, spilllist[i2]->spill)) {
				DBG((dbg, DBG_INTERFERENCES, "Slot %d and %d interfere\n", i, i2));
				bitset_set(interferences[i], i2);
				bitset_set(interferences[i2], i);
			}
		}
	}

	// sort affinity edges
	affinity_edge_count = ARR_LEN(env->affinity_edges);
	qsort(env->affinity_edges, affinity_edge_count, sizeof(env->affinity_edges[0]), cmp_affinity);

	//dump_interference_graph(env, interferences, "before");

	// try to merge affine nodes
	for(i = 0; i < affinity_edge_count; ++i) {
		const affinity_edge_t *edge = env->affinity_edges[i];
		int s1 = uf_find(spillslot_unionfind, edge->slot1);
		int s2 = uf_find(spillslot_unionfind, edge->slot2);

		/* test if values interfere */
		if(bitset_is_set(interferences[s1], s2)) {
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

#if 0
static void do_java_coalescing(ss_env_t *env)
{
	int spillcount;
	spill_t **spilllist;
	spill_t *spill;
	int i, i2;
	be_java_coal_t *coal;

	spillcount = set_count(env->spills);
	if(spillcount == 0)
		return;

	spilllist = alloca(spillcount * sizeof(spilllist[0]));

	DEBUG_ONLY(
		memset(spilllist, 0, spillcount * sizeof(spilllist[0]));
	);

	coal = be_java_coal_init("spillslot coalescing", spillcount, spillcount, 1);

	for(spill = set_first(env->spills), i = 0; spill != NULL; spill = set_next(env->spills), ++i) {
		assert(spill->spillslot < spillcount);
		DEBUG_ONLY(assert(spilllist[spill->spillslot] == NULL));
		spilllist[spill->spillslot] = spill;

		be_java_coal_set_color(coal, spill->spillslot, spill->spillslot);
	}

	// construct interferences
	for(i = 0; i < spillcount; ++i) {
		for(i2 = i+1; i2 < spillcount; ++i2) {
			if(values_interfere(env->chordal_env->lv, spilllist[i]->spill, spilllist[i2]->spill)) {
				be_java_coal_add_int_edge(coal, i, i2);
			}
		}
	}

	for(i = 0; i < ARR_LEN(env->affinity_edges); ++i) {
		const affinity_edge_t *edge = env->affinity_edges[i];
		int n = edge->slot1;
		int m = edge->slot2;
		int costs = (int) (edge->affinity * 10000);
		be_java_coal_add_aff_edge(coal, n, m, costs);
	}

	be_java_coal_coalesce(coal);

	// construct spillslots
	for(i = 0; i < spillcount; ++i) {
		spill_t *spill = spilllist[i];
		spill->spillslot = be_java_coal_get_color(coal, i);
	}
	be_java_coal_destroy(coal);
}
#endif

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
	entity   *entity;
} spill_slot_t;

typedef struct _memperm_entry_t {
	ir_node* node;
	int pos;
	entity *in;
	entity *out;
	struct _memperm_entry_t *next;
} memperm_entry_t;

typedef struct _memperm_t {
	ir_node *block;
	int entrycount;
	memperm_entry_t *entries;
} memperm_t;

static int cmp_memperm(const void* d1, const void* d2, size_t size) {
	const memperm_t* e1 = d1;
	const memperm_t* e2 = d2;
	return e1->block != e2->block;
}

static memperm_t *get_memperm(ss_env_t *env, ir_node *block) {
	memperm_t entry, *res;
	int hash;

	entry.block = block;
	hash = HASH_PTR(block);

	res = set_find(env->memperms, &entry, sizeof(entry), hash);

	if(res == NULL) {
		entry.entrycount = 0;
		entry.entries = NULL;
		res = set_insert(env->memperms, &entry, sizeof(entry), hash);
	}

	return res;
}

static entity* create_stack_entity(ss_env_t *env, spill_slot_t *slot) {
	ir_type* frame = get_irg_frame_type(env->chordal_env->irg);
	entity* res = frame_alloc_area(frame, slot->size, slot->align, 0);

	// adjust size of the entity type...
	ir_type *enttype = get_entity_type(res);
	set_type_size_bytes(enttype, slot->size);

	slot->entity = res;

	return res;
}

static int get_spillslotsize_for_spill(ss_env_t *env, spill_t *spill) {
	const ir_mode *mode = arch_register_class_mode(spill->cls);

	return get_mode_size_bytes(mode);
}

static int get_spillslotalign_for_spill(ss_env_t *env, spill_t *spill) {
	const arch_isa_t *isa = env->chordal_env->birg->main_env->arch_env->isa;

	return arch_isa_get_reg_class_alignment(isa, spill->cls);
}

/**
 * Enlarges a spillslot (if necessary) so that it can carry a value of size
 * @p othersize and alignment @p otheralign.
 */
static void enlarge_spillslot(spill_slot_t *slot, int otheralign, int othersize) {
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
static void assign_spillslots(ss_env_t *env) {
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
		spill_slot_t *slot = & (spillslots[slotid]);
		int align = get_spillslotalign_for_spill(env, spill);
		int size = get_spillslotsize_for_spill(env, spill);

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
			assert(arch_irn_class_is(arch_env, node, spill));
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

static void create_memperms(ss_env_t *env) {
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

		mempermnode = be_new_MemPerm(env->chordal_env->birg->main_env->arch_env, env->chordal_env->irg, memperm->block,
			memperm->entrycount, nodes);

		// insert node into schedule
		blockend = get_end_of_block_insertion_point(memperm->block);
		sched_add_before(blockend, mempermnode);

		for(entry = memperm->entries, i = 0; entry != NULL; entry = entry->next, ++i) {
			ir_node *proj;
			ir_node* arg = get_irn_n(entry->node, entry->pos);

			be_set_MemPerm_in_entity(mempermnode, i, entry->in);
			be_set_MemPerm_out_entity(mempermnode, i, entry->out);
			set_irg_current_block(env->chordal_env->irg, memperm->block);
			proj = new_Proj(mempermnode, get_irn_mode(arg), i);
			sched_add_before(blockend, proj);

			set_irn_n(entry->node, entry->pos, proj);
		}
	}
}

void be_coalesce_spillslots(const be_chordal_env_t *chordal_env) {
	ss_env_t env;

	obstack_init(&env.obst);
	env.arch_env = chordal_env->birg->main_env->arch_env;
	env.chordal_env = chordal_env;
	env.spills = new_set(cmp_spill, 10);
	env.reloads = NEW_ARR_F(ir_node*, 0);
	env.affinity_edges = NEW_ARR_F(affinity_edge_t*, 0);
	env.memperms = new_set(cmp_memperm, 10);
	FIRM_DBG_REGISTER(dbg, "firm.be.spillslots");
	//firm_dbg_set_mask(dbg, DBG_COALESCING);

	/* Get initial spill slots */
	irg_walk_graph(chordal_env->irg, NULL, collect_spills_walker, &env);

	do_greedy_coalescing(&env);

	assign_spillslots(&env);

	create_memperms(&env);

	//show_stats(&env);

	del_set(env.memperms);
	DEL_ARR_F(env.reloads);
	DEL_ARR_F(env.affinity_edges);
	del_set(env.spills);
	obstack_free(&env.obst, NULL);
}
