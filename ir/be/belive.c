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
 * @brief       Interblock liveness analysis.
 * @author      Sebastian Hack
 * @date        06.12.2004
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "impl.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irprintf_t.h"
#include "irbitset.h"
#include "irdump_t.h"
#include "irnodeset.h"

#include "dfs_t.h"
#include "absgraph.h"

#include "beutil.h"
#include "belive_t.h"
#include "beirg_t.h"
#include "besched_t.h"
#include "bemodule.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/* see comment in compute_liveness() */
#define LV_COMPUTE_SORTED
#define LV_STD_SIZE             64
#define LV_USE_BINARY_SEARCH
#undef  LV_INTESIVE_CHECKS

void be_live_chk_compare(be_lv_t *lv, lv_chk_t *lvc);

static INLINE int is_liveness_node(const ir_node *irn)
{
	switch(get_irn_opcode(irn)) {
	case iro_Block:
	case iro_Bad:
	case iro_End:
	case iro_Anchor:
		return 0;
	default:;
	}

	return 1;
}

int (be_lv_next_irn)(const struct _be_lv_t *lv, const ir_node *bl, unsigned flags, int i)
{
	return _be_lv_next_irn(lv, bl, flags, i);
}

const ir_node * (be_lv_get_irn)(const struct _be_lv_t *lv, const ir_node *bl, int i)
{
	return _be_lv_get_irn(lv, bl, i);
}

int (be_is_live_in)(const be_lv_t *lv, const ir_node *block, const ir_node *irn)
{
	return _be_is_live_xxx(lv, block, irn, be_lv_state_in);
}

int (be_is_live_out)(const be_lv_t *lv, const ir_node *block, const ir_node *irn)
{
	return _be_is_live_xxx(lv, block, irn, be_lv_state_out);
}

int (be_is_live_end)(const be_lv_t *lv, const ir_node *block, const ir_node *irn)
{
	return _be_is_live_xxx(lv, block, irn, be_lv_state_end);
}


#ifdef LV_USE_BINARY_SEARCH
static INLINE unsigned _be_liveness_bsearch(struct _be_lv_info_t *arr, unsigned idx)
{
	struct _be_lv_info_t *payload = arr + 1;

	unsigned n   = arr[0].u.head.n_members;
	unsigned res = 0;
	int lo       = 0;
	int hi       = n;

	if(n == 0)
		return 0;

	while(lo < hi) {
		int md          = lo + ((hi - lo) >> 1);
		unsigned md_idx = payload[md].u.node.idx;

		if(idx > md_idx)
			lo = md + 1;
		else if(idx < md_idx)
			hi = md;
		else {
			res = md;
			assert(payload[res].u.node.idx == idx);
			break;
		}

		res = lo;
	}

#ifdef LV_INTESIVE_CHECKS
	{
		unsigned i;
		for(i = res; i < n; ++i)
			assert(payload[i].u.node.idx >= idx);

		for(i = 0; i < res; ++i)
			assert(payload[i].u.node.idx < idx);
	}
#endif

	return res;
}

#else

/**
 * This function searches linearly for the node in the array.
 */
static INLINE unsigned _be_liveness_bsearch(struct _be_lv_info_t *arr, unsigned idx) {
	unsigned n  = arr[0].u.head.n_members;
	unsigned i;

	for(i = 0; i < n; ++i) {
		if(arr[i + 1].u.node.idx == idx)
			return i;
	}

	return i;
}
#endif

struct _be_lv_info_node_t *be_lv_get(const struct _be_lv_t *li, const ir_node *bl, const ir_node *irn)
{
	struct _be_lv_info_t *irn_live = phase_get_irn_data(&li->ph, bl);

	if(irn_live) {
		unsigned idx = get_irn_idx(irn);

		/* Get the position of the index in the array. */
		int pos = _be_liveness_bsearch(irn_live, idx);

		/* Get the record in question. 1 must be added, since the first record contains information about the array and must be skipped. */
		struct _be_lv_info_node_t *res = &irn_live[pos + 1].u.node;

		/* Check, if the irn is in deed in the array. */
		if(res->idx == idx)
			return res;
	}

	return NULL;
}

static struct _be_lv_info_node_t *be_lv_get_or_set(struct _be_lv_t *li, ir_node *bl, ir_node *irn)
{
	struct _be_lv_info_t *irn_live = phase_get_or_set_irn_data(&li->ph, bl);

	unsigned idx = get_irn_idx(irn);

	/* Get the position of the index in the array. */
	unsigned pos = _be_liveness_bsearch(irn_live, idx);

	/* Get the record in question. 1 must be added, since the first record contains information about the array and must be skipped. */
	struct _be_lv_info_node_t *res = &irn_live[pos + 1].u.node;

	/* Check, if the irn is in deed in the array. */
	if(res->idx != idx) {
		struct _be_lv_info_t *payload;
		unsigned n_members = irn_live[0].u.head.n_members;
		unsigned n_size    = irn_live[0].u.head.n_size;
		unsigned i;

		if(n_members + 1 >= n_size) {
			/* double the array size. Remember that the first entry is
			 * metadata about the array and not a real array element */
			unsigned old_size_bytes  = (n_size + 1) * sizeof(irn_live[0]);
			unsigned new_size        = (2 * n_size) + 1;
			size_t   new_size_bytes  = new_size * sizeof(irn_live[0]);
			struct _be_lv_info_t *nw = phase_alloc(&li->ph, new_size_bytes);
			memcpy(nw, irn_live, old_size_bytes);
			memset(((char*) nw) + old_size_bytes, 0,
			       new_size_bytes - old_size_bytes);
			nw[0].u.head.n_size = new_size - 1;
			irn_live = nw;
			phase_set_irn_data(&li->ph, bl, nw);
		}

		payload = &irn_live[1];
		for(i = n_members; i > pos; --i) {
			payload[i] = payload[i - 1];
		}

		++irn_live[0].u.head.n_members;

		res = &payload[pos].u.node;
		res->idx    = idx;
		res->flags  = 0;
	}

#ifdef LV_INTESIVE_CHECKS
	{
		unsigned i;
		unsigned n = irn_live[0].u.head.n_members;
		unsigned last = 0;
		struct _be_lv_info_t *payload = &irn_live[1];

		for(i = 0; i < n; ++i) {
			assert(payload[i].u.node.idx >= last);
			last = payload[i].u.node.idx;
		}
	}
#endif

	return res;
}

/**
 * Removes a node from the list of live variables of a block.
 * @return 1 if the node was live at that block, 0 if not.
 */
static int be_lv_remove(struct _be_lv_t *li, ir_node *bl, ir_node *irn)
{
	struct _be_lv_info_t *irn_live = phase_get_irn_data(&li->ph, bl);

	if(irn_live) {
		unsigned n   = irn_live[0].u.head.n_members;
		unsigned idx = get_irn_idx(irn);
		unsigned pos = _be_liveness_bsearch(irn_live, idx);
		struct _be_lv_info_t *payload  = irn_live + 1;
		struct _be_lv_info_node_t *res = &payload[pos].u.node;

		/* The node is in deed in the block's array. Let's remove it. */
		if(res->idx == idx) {
			unsigned i;

			for(i = pos + 1; i < n; ++i)
				payload[i - 1] = payload[i];

			payload[n - 1].u.node.idx   = 0;
			payload[n - 1].u.node.flags = 0;

			--irn_live[0].u.head.n_members;
			DBG((dbg, LEVEL_3, "\tdeleting %+F from %+F at pos %d\n", irn, bl, pos));
			return 1;
		}
	}

	return 0;
}

static void register_node(be_lv_t *lv, const ir_node *irn)
{
	unsigned idx = get_irn_idx(irn);
	if(idx >= bitset_size(lv->nodes)) {
		bitset_t *nw = bitset_malloc(2 * idx);
		bitset_copy(nw, lv->nodes);
		bitset_free(lv->nodes);
		lv->nodes = nw;
	}

	bitset_set(lv->nodes, idx);
}

/**
 * Mark a node as live-in in a block.
 */
static INLINE void mark_live_in(be_lv_t *lv, ir_node *block, ir_node *irn)
{
	struct _be_lv_info_node_t *n = be_lv_get_or_set(lv, block, irn);
	DBG((dbg, LEVEL_2, "marking %+F live in at %+F\n", irn, block));
	n->flags |= be_lv_state_in;
	register_node(lv, irn);
}

/**
 * Mark a node as live-out in a block.
 */
static INLINE void mark_live_out(be_lv_t *lv, ir_node *block, ir_node *irn)
{
	struct _be_lv_info_node_t *n = be_lv_get_or_set(lv, block, irn);
	DBG((dbg, LEVEL_2, "marking %+F live out at %+F\n", irn, block));
	n->flags |= be_lv_state_out | be_lv_state_end;
	register_node(lv, irn);
}

/**
 * Mark a node as live-end in a block.
 */
static INLINE void mark_live_end(be_lv_t *lv, ir_node *block, ir_node *irn)
{
	struct _be_lv_info_node_t *n = be_lv_get_or_set(lv, block, irn);
	DBG((dbg, LEVEL_2, "marking %+F live end at %+F\n", irn, block));
	n->flags |= be_lv_state_end;
	register_node(lv, irn);
}

/**
 * Mark a node (value) live out at a certain block. Do this also
 * transitively, i.e. if the block is not the block of the value's
 * definition, all predecessors are also marked live.
 * @param def The node (value).
 * @param block The block to mark the value live out of.
 * @param visited A set were all visited blocks are recorded.
 * @param is_true_out Is the node real out there or only live at the end
 * of the block.
 */
static void live_end_at_block(be_lv_t *lv, ir_node *def, ir_node *block, bitset_t *visited, int is_true_out)
{
	mark_live_end(lv, block, def);
	if(is_true_out)
		mark_live_out(lv, block, def);

	if(!bitset_contains_irn(visited, block)) {
		bitset_add_irn(visited, block);

		/*
		* If this block is not the definition block, we have to go up
		* further.
		*/
		if(get_nodes_block(def) != block) {
			int i, n;

			mark_live_in(lv, block, def);

			for(i = 0, n = get_Block_n_cfgpreds(block); i < n; ++i)
				live_end_at_block(lv, def, get_Block_cfgpred_block(block, i), visited, 1);
		}

	}
}

struct _lv_walker_t {
	be_lv_t *lv;
	void *data;
};

/**
 * Liveness analysis for a value.
 * This functions is meant to be called by a firm walker, to compute the
 * set of all blocks a value is live in.
 * @param irn The node (value).
 * @param env Ignored.
 */
static void liveness_for_node(ir_node *irn, void *data)
{
	struct _lv_walker_t *walker = data;
	be_lv_t *lv       = walker->lv;
	bitset_t *visited = walker->data;
	const ir_edge_t *edge;
	ir_node *def_block;

	/* Don't compute liveness information for non-data nodes. */
	if(!is_liveness_node(irn))
		return;

	bitset_clear_all(visited);
	def_block = get_nodes_block(irn);

	/* Go over all uses of the value */
	foreach_out_edge(irn, edge) {
		ir_node *use = edge->src;
		ir_node *use_block;

		DBG((dbg, LEVEL_4, "%+F: use at %+F, pos %d in %+F\n", irn, use, edge->pos, get_block(use)));
		assert(get_irn_n(use, edge->pos) == irn);

		/*
		 * If the usage is no data node, skip this use, since it does not
		 * affect the liveness of the node.
		 */
		if(!is_liveness_node(use))
			continue;

		/* Get the block where the usage is in. */
		use_block = get_nodes_block(use);

		/*
		 * If the use is a phi function, determine the corresponding block
		 * through which the value reaches the phi function and mark the
		 * value as live out of that block.
		 */
		if(is_Phi(use)) {
			ir_node *pred_block = get_Block_cfgpred_block(use_block, edge->pos);
			live_end_at_block(lv, irn, pred_block, visited, 0);
		}

		/*
		 * Else, the value is live in at this block. Mark it and call live
		 * out on the predecessors.
		 */
		else if(def_block != use_block) {
			int i, n;

			mark_live_in(lv, use_block, irn);

			for(i = 0, n = get_Block_n_cfgpreds(use_block); i < n; ++i) {
				ir_node *pred_block = get_Block_cfgpred_block(use_block, i);
				live_end_at_block(lv, irn, pred_block, visited, 1);
			}
		}
	}
}

static void lv_remove_irn_walker(ir_node *bl, void *data)
{
	struct _lv_walker_t *w = data;
	be_lv_t *lv  = w->lv;
	ir_node *irn = w->data;
	be_lv_remove(lv, bl, irn);
}

static const char *lv_flags_to_str(unsigned flags)
{
	static const char *states[] = {
		"---",
		"i--",
		"-e-",
		"ie-",
		"--o",
		"i-o",
		"-eo",
		"ieo"
	};

	return states[flags & 7];
}

static void lv_dump_block(void *context, FILE *f, const ir_node *bl)
{
	if(is_Block(bl)) {
		be_lv_t *lv = context;
		struct _be_lv_info_t *info = phase_get_irn_data(&lv->ph, bl);

		fprintf(f, "liveness:\n");
		if(info) {
			unsigned n = info[0].u.head.n_members;
			unsigned i;

			for(i = 0; i < n; ++i) {
				struct _be_lv_info_node_t *n = &info[i+1].u.node;
				ir_fprintf(f, "%s %+F\n", lv_flags_to_str(n->flags), get_idx_irn(lv->irg, n->idx));
			}
		}
	}
}

static void *lv_phase_data_init(ir_phase *phase, ir_node *irn, void *old)
{
	struct _be_lv_info_t *info = phase_alloc(phase, LV_STD_SIZE * sizeof(info[0]));
	(void) irn;
	(void) old;

	memset(info, 0, LV_STD_SIZE * sizeof(info[0]));
	info[0].u.head.n_size = LV_STD_SIZE - 1;
	return info;
}

static void collect_nodes(ir_node *irn, void *data)
{
	struct obstack *obst = data;
	if (is_liveness_node(irn))
		obstack_ptr_grow(obst, irn);
}

static int node_idx_cmp(const void *a, const void *b)
{
	const ir_node *p = *(ir_node **) a;
	const ir_node *q = *(ir_node **) b;
	int ia = get_irn_idx(p);
	int ib = get_irn_idx(q);
	return ia - ib;
}

static void compute_liveness(be_lv_t *lv)
{
	struct obstack obst;
	struct _lv_walker_t w;
	ir_node **nodes;
	int i, n;

	obstack_init(&obst);
	irg_walk_graph(lv->irg, collect_nodes, NULL, &obst);
	n      = obstack_object_size(&obst) / sizeof(nodes[0]);
	nodes  = obstack_finish(&obst);

	/*
	 * inserting the variables sorted by their ID is probably
	 * more efficient since the binary sorted set insertion
	 * will not need to move arounf the data.
	 * However, if sorting the variables a priori pays off
	 * needs to be checked, hence the define.
	 */
#ifdef LV_COMPUTE_SORTED
	qsort(nodes, n, sizeof(nodes[0]), node_idx_cmp);
#endif

	w.lv   = lv;
	w.data = bitset_obstack_alloc(&obst, get_irg_last_idx(lv->irg));

	for (i = 0; i < n; ++i)
		liveness_for_node(nodes[i], &w);

	obstack_free(&obst, NULL);
	register_hook(hook_node_info, &lv->hook_info);
}

void be_liveness_assure_sets(be_lv_t *lv)
{
	if (!lv->nodes) {
		lv->nodes = bitset_malloc(2 * get_irg_last_idx(lv->irg));
		phase_init(&lv->ph, "liveness", lv->irg, PHASE_DEFAULT_GROWTH, lv_phase_data_init, NULL);
		compute_liveness(lv);
		/* be_live_chk_compare(lv, lv->lvc); */
	}
}

void be_liveness_assure_chk(be_lv_t *lv)
{
#ifndef USE_LIVE_CHK
	be_liveness_assure_sets(lv);
#else
	(void) lv;
#endif
}

void be_liveness_invalidate(be_lv_t *lv)
{
	if (lv && lv->nodes) {
		unregister_hook(hook_node_info, &lv->hook_info);
		phase_free(&lv->ph);
		bitset_free(lv->nodes);
		lv->nodes = NULL;
	}
}

/* Compute the inter block liveness for a graph. */
be_lv_t *be_liveness(const be_irg_t *birg)
{
	be_lv_t *lv = xmalloc(sizeof(lv[0]));

	memset(lv, 0, sizeof(lv[0]));
	lv->irg  = be_get_birg_irg(birg);
	lv->birg = birg;
#ifdef USE_LIVE_CHK
	lv->dfs  = dfs_new(&absgraph_irg_cfg_succ, lv->irg);
	lv->lvc  = lv_chk_new(lv->irg, lv->dfs);
#endif
	lv->hook_info.context = lv;
	lv->hook_info.hook._hook_node_info = lv_dump_block;

	return lv;
}

void be_liveness_recompute(be_lv_t *lv)
{
	unsigned last_idx = get_irg_last_idx(lv->irg);
	if(last_idx >= bitset_size(lv->nodes)) {
		bitset_free(lv->nodes);
		lv->nodes = bitset_malloc(last_idx * 2);
	}

	else
		bitset_clear_all(lv->nodes);

	phase_free(&lv->ph);
	phase_init(&lv->ph, "liveness", lv->irg, PHASE_DEFAULT_GROWTH, lv_phase_data_init, NULL);
	compute_liveness(lv);
}


void be_liveness_free(be_lv_t *lv)
{
	be_liveness_invalidate(lv);
	free(lv);
}

void be_liveness_remove(be_lv_t *lv, ir_node *irn)
{
	if (lv->nodes) {
		unsigned idx = get_irn_idx(irn);
		struct _lv_walker_t w;

		/*
		 * Removes a single irn from the liveness information.
		 * Since an irn can only be live at blocks dominated by the block of its
		 * definition, we only have to process that dominance subtree.
		 */
		w.lv   = lv;
		w.data = irn;
		dom_tree_walk(get_nodes_block(irn), lv_remove_irn_walker, NULL, &w);
		if(idx < bitset_size(lv->nodes))
			bitset_clear(lv->nodes, idx);
	}
}

void be_liveness_introduce(be_lv_t *lv, ir_node *irn)
{
	if (lv->nodes) {
		struct _lv_walker_t w;
		w.lv   = lv;
		w.data = bitset_malloc(get_irg_last_idx(lv->irg));
		liveness_for_node(irn, &w);
		bitset_free(w.data);
	}
}

void be_liveness_update(be_lv_t *lv, ir_node *irn)
{
	be_liveness_remove(lv, irn);
	be_liveness_introduce(lv, irn);
}

static void lv_check_walker(ir_node *bl, void *data)
{
	struct _lv_walker_t *w = data;
	be_lv_t *lv    = w->lv;
	be_lv_t *fresh = w->data;

	struct _be_lv_info_t *curr = phase_get_irn_data(&lv->ph, bl);
	struct _be_lv_info_t *fr   = phase_get_irn_data(&fresh->ph, bl);

	if(!fr && curr && curr[0].u.head.n_members > 0) {
		unsigned i;

		ir_fprintf(stderr, "%+F liveness should be empty but current liveness contains:\n", bl);
		for(i = 0; i < curr[0].u.head.n_members; ++i) {
			ir_fprintf(stderr, "\t%+F\n", get_idx_irn(lv->irg, curr[1 + i].u.node.idx));
		}
	}

	else if(curr) {
		unsigned n_curr  = curr[0].u.head.n_members;
		unsigned n_fresh = fr[0].u.head.n_members;

		unsigned i;

		if(n_curr != n_fresh) {
			ir_fprintf(stderr, "%+F: liveness set sizes differ. curr %d, correct %d\n", bl, n_curr, n_fresh);

			ir_fprintf(stderr, "current:\n");
			for(i = 0; i < n_curr; ++i) {
				struct _be_lv_info_node_t *n = &curr[1 + i].u.node;
				ir_fprintf(stderr, "%+F %u %+F %s\n", bl, i, get_idx_irn(lv->irg, n->idx), lv_flags_to_str(n->flags));
			}

			ir_fprintf(stderr, "correct:\n");
			for(i = 0; i < n_fresh; ++i) {
				struct _be_lv_info_node_t *n = &fr[1 + i].u.node;
				ir_fprintf(stderr, "%+F %u %+F %s\n", bl, i, get_idx_irn(lv->irg, n->idx), lv_flags_to_str(n->flags));
			}
		}
	}
}

void be_liveness_check(be_lv_t *lv)
{
	struct _lv_walker_t w;
	be_lv_t *fresh = be_liveness(lv->birg);

	w.lv   = lv;
	w.data = fresh;
	irg_block_walk_graph(lv->irg, lv_check_walker, NULL, &w);
	be_liveness_free(fresh);
}


static void lv_dump_block_walker(ir_node *irn, void *data)
{
	struct _lv_walker_t *w = data;
	if(is_Block(irn))
		lv_dump_block(w->lv, w->data, irn);
}


/* Dump the liveness information for a graph. */
void be_liveness_dump(const be_lv_t *lv, FILE *f)
{
	struct _lv_walker_t w;

	w.lv   = (be_lv_t *) lv;
	w.data = f;
	irg_block_walk_graph(lv->irg, lv_dump_block_walker, NULL, &w);
}

/* Dump the liveness information for a graph. */
void be_liveness_dumpto(const be_lv_t *lv, const char *cls_name)
{
	FILE *f;
	char buf[128];
	ir_snprintf(buf, sizeof(buf), "%F_%s-live.txt", lv->irg, cls_name);
	if((f = fopen(buf, "wt")) != NULL) {
		be_liveness_dump(lv, f);
		fclose(f);
	}
}

/**
 * Walker: checks the every predecessors of a node dominate
 * the note.
 */
static void dom_check(ir_node *irn, void *data)
{
	int *problem_found = data;

	if(!is_Block(irn) && irn != get_irg_end(get_irn_irg(irn))) {
		int i, n;
		ir_node *bl = get_nodes_block(irn);

		for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *op     = get_irn_n(irn, i);
			ir_node *def_bl = get_nodes_block(op);
			ir_node *use_bl = bl;

			if(is_Phi(irn))
				use_bl = get_Block_cfgpred_block(bl, i);

			if(get_irn_opcode(use_bl) != iro_Bad
			     && get_irn_opcode(def_bl) != iro_Bad
			     && !block_dominates(def_bl, use_bl)) {
				ir_fprintf(stderr, "Verify warning: %+F in %+F must dominate %+F for user %+F (%s)\n", op, def_bl, use_bl, irn, get_irg_dump_name(get_irn_irg(op)));
				*problem_found = 1;
			}
		}
	}
}

/* Check, if the SSA dominance property is fulfilled. */
int be_check_dominance(ir_graph *irg)
{
	int problem_found = 0;

	assure_doms(irg);
	irg_walk_graph(irg, dom_check, NULL, &problem_found);

	return !problem_found;
}

void be_liveness_transfer(const arch_env_t *arch_env,
                          const arch_register_class_t *cls,
                          ir_node *node, ir_nodeset_t *nodeset)
{
	int i, arity;

	/* You should better break out of your loop when hitting the first phi
	 * function. */
	assert(!is_Phi(node) && "liveness_transfer produces invalid results for phi nodes");

	if (get_irn_mode(node) == mode_T) {
		const ir_edge_t *edge;

		foreach_out_edge(node, edge) {
			ir_node *proj = get_edge_src_irn(edge);

			if (arch_irn_consider_in_reg_alloc(arch_env, cls, proj)) {
				ir_nodeset_remove(nodeset, proj);
			}
		}
	}

	if (arch_irn_consider_in_reg_alloc(arch_env, cls, node)) {
		ir_nodeset_remove(nodeset, node);
	}

	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node *op = get_irn_n(node, i);

		if (arch_irn_consider_in_reg_alloc(arch_env, cls, op))
			ir_nodeset_insert(nodeset, op);
	}
}



void be_liveness_end_of_block(const be_lv_t *lv, const arch_env_t *arch_env,
                              const arch_register_class_t *cls,
                              const ir_node *block, ir_nodeset_t *live)
{
	int i;

	assert(lv->nodes && "live sets must be computed");
	be_lv_foreach(lv, block, be_lv_state_end, i) {
		ir_node *node = be_lv_get_irn(lv, block, i);
		if(!arch_irn_consider_in_reg_alloc(arch_env, cls, node))
			continue;

		ir_nodeset_insert(live, node);
	}
}



void be_liveness_nodes_live_at(const be_lv_t *lv, const arch_env_t *arch_env,
                               const arch_register_class_t *cls,
                               const ir_node *pos, ir_nodeset_t *live)
{
	const ir_node *bl = is_Block(pos) ? pos : get_nodes_block(pos);
	ir_node *irn;

	be_liveness_end_of_block(lv, arch_env, cls, bl, live);
	sched_foreach_reverse(bl, irn) {
		/*
		 * If we encounter the node we want to insert the Perm after,
		 * exit immediately, so that this node is still live
		 */
		if(irn == pos)
			return;

		be_liveness_transfer(arch_env, cls, irn, live);
	}
}

void be_liveness_nodes_live_at_input(const be_lv_t *lv,
                                     const arch_env_t *arch_env,
                                     const arch_register_class_t *cls,
                                     const ir_node *pos, ir_nodeset_t *live)
{
	const ir_node *bl = is_Block(pos) ? pos : get_nodes_block(pos);
	ir_node *irn;

	assert(lv->nodes && "live sets must be computed");
	be_liveness_end_of_block(lv, arch_env, cls, bl, live);
	sched_foreach_reverse(bl, irn) {
		be_liveness_transfer(arch_env, cls, irn, live);
		if(irn == pos)
			return;
	}
}

static void collect_node(ir_node *irn, void *data)
{
	struct obstack *obst = data;
	obstack_ptr_grow(obst, irn);
}

void be_live_chk_compare(be_lv_t *lv, lv_chk_t *lvc)
{
	ir_graph *irg    = lv->irg;

	struct obstack obst;
	ir_node **nodes;
	ir_node **blocks;
	int i, j;

	obstack_init(&obst);

	irg_block_walk_graph(irg, collect_node, NULL, &obst);
	obstack_ptr_grow(&obst, NULL);
	blocks = obstack_finish(&obst);

	irg_walk_graph(irg, collect_node, NULL, &obst);
	obstack_ptr_grow(&obst, NULL);
	nodes = obstack_finish(&obst);

	for (i = 0; blocks[i]; ++i) {
		ir_node *bl = blocks[i];

		for (j = 0; nodes[j]; ++j) {
			ir_node *irn = nodes[j];
			if (!is_Block(irn)) {
				int lvr_in  = be_is_live_in (lv, bl, irn);
				int lvr_out = be_is_live_out(lv, bl, irn);
				int lvr_end = be_is_live_end(lv, bl, irn);

				int lvc_in  = lv_chk_bl_in (lvc, bl, irn);
				int lvc_out = lv_chk_bl_out(lvc, bl, irn);
				int lvc_end = lv_chk_bl_end(lvc, bl, irn);

				if (lvr_in - lvc_in != 0)
					ir_fprintf(stderr, "live in  info for %+F at %+F differs: nml: %d, chk: %d\n", irn, bl, lvr_in, lvc_in);

				if (lvr_end - lvc_end != 0)
					ir_fprintf(stderr, "live end info for %+F at %+F differs: nml: %d, chk: %d\n", irn, bl, lvr_end, lvc_end);

				if (lvr_out - lvc_out != 0)
					ir_fprintf(stderr, "live out info for %+F at %+F differs: nml: %d, chk: %d\n", irn, bl, lvr_out, lvc_out);
			}
		}
	}


	obstack_free(&obst, NULL);
}

void be_init_live(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.liveness");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_live);
