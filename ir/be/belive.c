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
 * @brief       Interblock liveness analysis.
 * @author      Sebastian Hack
 * @date        06.12.2004
 */
#include "config.h"

/* statev is expensive here, only enable when needed */
#define DISABLE_STATEV

#include "iredges_t.h"
#include "irgwalk.h"
#include "irprintf_t.h"
#include "irdump_t.h"
#include "irnodeset.h"

#include "absgraph.h"
#include "statev_t.h"

#include "beutil.h"
#include "belive_t.h"
#include "beirg.h"
#include "besched.h"
#include "bemodule.h"
#include "bedump.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define LV_STD_SIZE             64

/**
 * Filter out some nodes for which we never need liveness.
 *
 * @param irn  the node t check
 * @return 0 if no liveness info is needed, 1 else
 */
static inline int is_liveness_node(const ir_node *irn)
{
	switch (get_irn_opcode(irn)) {
	case iro_Block:
	case iro_Bad:
	case iro_End:
	case iro_Anchor:
	case iro_NoMem:
		return 0;
	default:
		return 1;
	}
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

static inline unsigned _be_liveness_bsearch(be_lv_info_t *arr, unsigned idx)
{
	be_lv_info_t *payload = arr + 1;

	unsigned n   = arr[0].head.n_members;
	unsigned res = 0;
	int lo       = 0;
	int hi       = n;

	if (n == 0)
		return 0;

	do {
		int md          = lo + ((hi - lo) >> 1);
		unsigned md_idx = payload[md].node.idx;

		if (idx > md_idx)
			lo = md + 1;
		else if (idx < md_idx)
			hi = md;
		else {
			res = md;
			assert(payload[res].node.idx == idx);
			break;
		}

		res = lo;
	} while (lo < hi);

	return res;
}

be_lv_info_node_t *be_lv_get(const be_lv_t *li, const ir_node *bl,
                             const ir_node *irn)
{
	be_lv_info_t *irn_live;
	be_lv_info_node_t *res = NULL;

	stat_ev_tim_push();
	irn_live = ir_nodehashmap_get(be_lv_info_t, &li->map, bl);
	if (irn_live != NULL) {
		unsigned idx = get_irn_idx(irn);

		/* Get the position of the index in the array. */
		int pos = _be_liveness_bsearch(irn_live, idx);

		/* Get the record in question. 1 must be added, since the first record contains information about the array and must be skipped. */
		be_lv_info_node_t *rec = &irn_live[pos + 1].node;

		/* Check, if the irn is in deed in the array. */
		if (rec->idx == idx)
			res = rec;
	}
	stat_ev_tim_pop("be_lv_get");

	return res;
}

static be_lv_info_node_t *be_lv_get_or_set(be_lv_t *li, ir_node *bl,
                                           ir_node *irn)
{
	be_lv_info_t *irn_live = ir_nodehashmap_get(be_lv_info_t, &li->map, bl);
	if (irn_live == NULL) {
		irn_live = OALLOCNZ(&li->obst, be_lv_info_t, LV_STD_SIZE);
		irn_live[0].head.n_size = LV_STD_SIZE-1;
		ir_nodehashmap_insert(&li->map, bl, irn_live);
	}

	unsigned idx = get_irn_idx(irn);

	/* Get the position of the index in the array. */
	unsigned pos = _be_liveness_bsearch(irn_live, idx);

	/* Get the record in question. 1 must be added, since the first record contains information about the array and must be skipped. */
	be_lv_info_node_t *res = &irn_live[pos + 1].node;

	/* Check, if the irn is in deed in the array. */
	if (res->idx != idx) {
		be_lv_info_t *payload;
		unsigned n_members = irn_live[0].head.n_members;
		unsigned n_size    = irn_live[0].head.n_size;
		unsigned i;

		if (n_members + 1 >= n_size) {
			/* double the array size. Remember that the first entry is
			 * metadata about the array and not a real array element */
			unsigned old_size_bytes  = (n_size + 1) * sizeof(irn_live[0]);
			unsigned new_size        = (2 * n_size) + 1;
			size_t   new_size_bytes  = new_size * sizeof(irn_live[0]);
			be_lv_info_t *nw = OALLOCN(&li->obst, be_lv_info_t, new_size);
			memcpy(nw, irn_live, old_size_bytes);
			memset(((char*) nw) + old_size_bytes, 0,
			       new_size_bytes - old_size_bytes);
			nw[0].head.n_size = new_size - 1;
			irn_live = nw;
			ir_nodehashmap_insert(&li->map, bl, nw);
		}

		payload = &irn_live[1];
		for (i = n_members; i > pos; --i) {
			payload[i] = payload[i - 1];
		}

		++irn_live[0].head.n_members;

		res = &payload[pos].node;
		res->idx    = idx;
		res->flags  = 0;
	}

	return res;
}

/**
 * Removes a node from the list of live variables of a block.
 * @return 1 if the node was live at that block, 0 if not.
 */
static int be_lv_remove(be_lv_t *li, const ir_node *bl,
                        const ir_node *irn)
{
	be_lv_info_t *irn_live = ir_nodehashmap_get(be_lv_info_t, &li->map, bl);

	if (irn_live != NULL) {
		unsigned n   = irn_live[0].head.n_members;
		unsigned idx = get_irn_idx(irn);
		unsigned pos = _be_liveness_bsearch(irn_live, idx);
		be_lv_info_t *payload  = irn_live + 1;
		be_lv_info_node_t *res = &payload[pos].node;

		/* The node is in deed in the block's array. Let's remove it. */
		if (res->idx == idx) {
			unsigned i;

			for (i = pos + 1; i < n; ++i)
				payload[i - 1] = payload[i];

			payload[n - 1].node.idx   = 0;
			payload[n - 1].node.flags = 0;

			--irn_live[0].head.n_members;
			DBG((dbg, LEVEL_3, "\tdeleting %+F from %+F at pos %d\n", irn, bl, pos));
			return 1;
		}
	}

	return 0;
}

/**
 * Mark a node as live-in in a block.
 */
static inline void mark_live_in(be_lv_t *lv, ir_node *block, ir_node *irn)
{
	be_lv_info_node_t *n = be_lv_get_or_set(lv, block, irn);
	DBG((dbg, LEVEL_2, "marking %+F live in at %+F\n", irn, block));
	n->flags |= be_lv_state_in;
}

/**
 * Mark a node as live-out in a block.
 */
static inline void mark_live_out(be_lv_t *lv, ir_node *block, ir_node *irn)
{
	be_lv_info_node_t *n = be_lv_get_or_set(lv, block, irn);
	DBG((dbg, LEVEL_2, "marking %+F live out at %+F\n", irn, block));
	n->flags |= be_lv_state_out | be_lv_state_end;
}

/**
 * Mark a node as live-end in a block.
 */
static inline void mark_live_end(be_lv_t *lv, ir_node *block, ir_node *irn)
{
	be_lv_info_node_t *n = be_lv_get_or_set(lv, block, irn);
	DBG((dbg, LEVEL_2, "marking %+F live end at %+F\n", irn, block));
	n->flags |= be_lv_state_end;
}

static struct {
	be_lv_t  *lv;         /**< The liveness object. */
	ir_node  *def;        /**< The node (value). */
	ir_node  *def_block;  /**< The block of def. */
	bitset_t *visited;    /**< A set were all visited blocks are recorded. */
} re;

/**
 * Mark a node (value) live out at a certain block. Do this also
 * transitively, i.e. if the block is not the block of the value's
 * definition, all predecessors are also marked live.
 * @param block The block to mark the value live out of.
 * @param is_true_out Is the node real out there or only live at the end
 * of the block.
 */
static void live_end_at_block(ir_node *block, int is_true_out)
{
	be_lv_t *lv  = re.lv;
	ir_node *def = re.def;
	bitset_t *visited;

	mark_live_end(lv, block, def);
	if (is_true_out)
		mark_live_out(lv, block, def);

	visited = re.visited;
	if (!bitset_is_set(visited, get_irn_idx(block))) {
		bitset_set(visited, get_irn_idx(block));

		/*
		 * If this block is not the definition block, we have to go up
		 * further.
		 */
		if (re.def_block != block) {
			int i;

			mark_live_in(lv, block, def);

			for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i)
				live_end_at_block(get_Block_cfgpred_block(block, i), 1);
		}
	}
}

typedef struct lv_remove_walker_t {
	be_lv_t       *lv;
	const ir_node *irn;
} lv_remove_walker_t;


/**
 * Liveness analysis for a value.
 * Compute the set of all blocks a value is live in.
 * @param irn     The node (value).
 */
static void liveness_for_node(ir_node *irn)
{
	ir_node *def_block;

	bitset_clear_all(re.visited);
	def_block = get_nodes_block(irn);

	re.def       = irn;
	re.def_block = def_block;

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
		if (!is_liveness_node(use))
			continue;

		/* Get the block where the usage is in. */
		use_block = get_nodes_block(use);

		/*
		 * If the use is a phi function, determine the corresponding block
		 * through which the value reaches the phi function and mark the
		 * value as live out of that block.
		 */
		if (is_Phi(use)) {
			ir_node *pred_block = get_Block_cfgpred_block(use_block, edge->pos);
			live_end_at_block(pred_block, 0);
		}

		/*
		 * Else, the value is live in at this block. Mark it and call live
		 * out on the predecessors.
		 */
		else if (def_block != use_block) {
			int i;

			mark_live_in(re.lv, use_block, irn);

			for (i = get_Block_n_cfgpreds(use_block) - 1; i >= 0; --i) {
				ir_node *pred_block = get_Block_cfgpred_block(use_block, i);
				live_end_at_block(pred_block, 1);
			}
		}
	}
}

static void lv_remove_irn_walker(ir_node *bl, void *data)
{
	lv_remove_walker_t *w = (lv_remove_walker_t*)data;
	be_lv_remove(w->lv, bl, w->irn);
}

/**
 * Walker, collect all nodes for which we want calculate liveness info
 * on an obstack.
 */
static void collect_liveness_nodes(ir_node *irn, void *data)
{
	ir_node **nodes = (ir_node**)data;
	if (is_liveness_node(irn))
		nodes[get_irn_idx(irn)] = irn;
}

void be_liveness_compute_sets(be_lv_t *lv)
{
	ir_node **nodes;
	int       i;
	int       n;

	if (lv->sets_valid)
		return;

	be_timer_push(T_LIVE);
	ir_nodehashmap_init(&lv->map);
	obstack_init(&lv->obst);

	n = get_irg_last_idx(lv->irg);
	nodes = NEW_ARR_F(ir_node *, n);
	memset(nodes, 0, sizeof(nodes[0]) * n);

	/* inserting the variables sorted by their ID is probably
	 * more efficient since the binary sorted set insertion
	 * will not need to move around the data. */
	irg_walk_graph(lv->irg, NULL, collect_liveness_nodes, nodes);

	re.lv      = lv;
	re.visited = bitset_malloc(n);

	for (i = 0; i < n; ++i) {
		if (nodes[i] != NULL)
			liveness_for_node(nodes[i]);
	}

	DEL_ARR_F(nodes);
	free(re.visited);
	register_hook(hook_node_info, &lv->hook_info);

	be_timer_pop(T_LIVE);

	lv->sets_valid = true;
}

void be_liveness_compute_chk(be_lv_t *lv)
{
	if (lv->lvc != NULL)
		return;
	lv->lvc = lv_chk_new(lv->irg);
}

void be_liveness_invalidate_sets(be_lv_t *lv)
{
	if (!lv->sets_valid)
		return;
	unregister_hook(hook_node_info, &lv->hook_info);
	obstack_free(&lv->obst, NULL);
	ir_nodehashmap_destroy(&lv->map);
	lv->sets_valid = false;
}

void be_liveness_invalidate_chk(be_lv_t *lv)
{
	be_liveness_invalidate_sets(lv);

	if (lv->lvc == NULL)
		return;
	lv_chk_free(lv->lvc);
	lv->lvc = NULL;
}

be_lv_t *be_liveness_new(ir_graph *irg)
{
	be_lv_t *lv = XMALLOCZ(be_lv_t);

	lv->irg = irg;
	lv->hook_info.context = lv;
	lv->hook_info.hook._hook_node_info = be_dump_liveness_block;

	return lv;
}

void be_liveness_free(be_lv_t *lv)
{
	be_liveness_invalidate_sets(lv);
	be_liveness_invalidate_chk(lv);

	xfree(lv);
}

void be_liveness_remove(be_lv_t *lv, const ir_node *irn)
{
	if (lv->sets_valid) {
		lv_remove_walker_t w;

		/*
		 * Removes a single irn from the liveness information.
		 * Since an irn can only be live at blocks dominated by the block of its
		 * definition, we only have to process that dominance subtree.
		 */
		w.lv  = lv;
		w.irn = irn;
		dom_tree_walk(get_nodes_block(irn), lv_remove_irn_walker, NULL, &w);
	}
}

void be_liveness_introduce(be_lv_t *lv, ir_node *irn)
{
	/* Don't compute liveness information for non-data nodes. */
	if (lv->sets_valid && is_liveness_node(irn)) {
		re.lv      = lv;
		re.visited = bitset_malloc(get_irg_last_idx(lv->irg));
		liveness_for_node(irn);
		bitset_free(re.visited);
	}
}

void be_liveness_update(be_lv_t *lv, ir_node *irn)
{
	be_liveness_remove(lv, irn);
	be_liveness_introduce(lv, irn);
}

void be_liveness_transfer(const arch_register_class_t *cls,
                          ir_node *node, ir_nodeset_t *nodeset)
{
	/* You should better break out of your loop when hitting the first phi
	 * function. */
	assert(!is_Phi(node) && "liveness_transfer produces invalid results for phi nodes");

	ir_node *value;
	be_foreach_definition(node, cls, value,
		ir_nodeset_remove(nodeset, value);
	);

	int arity = get_irn_arity(node);
	for (int i = 0; i < arity; ++i) {
		ir_node *op = get_irn_n(node, i);
		if (!arch_irn_consider_in_reg_alloc(cls, op))
			continue;
		ir_nodeset_insert(nodeset, op);
	}
}



void be_liveness_end_of_block(const be_lv_t *lv,
                              const arch_register_class_t *cls,
                              const ir_node *block, ir_nodeset_t *live)
{
	assert(lv->sets_valid && "live sets must be computed");
	be_lv_foreach(lv, block, be_lv_state_end, node) {
		if (!arch_irn_consider_in_reg_alloc(cls, node))
			continue;

		ir_nodeset_insert(live, node);
	}
}



void be_liveness_nodes_live_at(const be_lv_t *lv,
                               const arch_register_class_t *cls,
                               const ir_node *pos, ir_nodeset_t *live)
{
	const ir_node *bl = is_Block(pos) ? pos : get_nodes_block(pos);

	be_liveness_end_of_block(lv, cls, bl, live);
	sched_foreach_reverse(bl, irn) {
		/*
		 * If we encounter the node we want to insert the Perm after,
		 * exit immediately, so that this node is still live
		 */
		if (irn == pos)
			return;

		be_liveness_transfer(cls, irn, live);
	}
}

static void collect_node(ir_node *irn, void *data)
{
	struct obstack *obst = (struct obstack*)data;
	obstack_ptr_grow(obst, irn);
}

static void be_live_chk_compare(be_lv_t *lv, lv_chk_t *lvc)
{
	ir_graph *irg    = lv->irg;

	struct obstack obst;
	ir_node **nodes;
	ir_node **blocks;
	int i, j;

	obstack_init(&obst);

	irg_block_walk_graph(irg, collect_node, NULL, &obst);
	obstack_ptr_grow(&obst, NULL);
	blocks = (ir_node**)obstack_finish(&obst);

	irg_walk_graph(irg, collect_node, NULL, &obst);
	obstack_ptr_grow(&obst, NULL);
	nodes = (ir_node**)obstack_finish(&obst);

	stat_ev_ctx_push("be_lv_chk_compare");
	for (j = 0; nodes[j]; ++j) {
		ir_node *irn = nodes[j];
		if (is_Block(irn))
			continue;

		for (i = 0; blocks[i]; ++i) {
			ir_node *bl = blocks[i];
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
	stat_ev_ctx_pop("be_lv_chk_compare");

	obstack_free(&obst, NULL);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_live)
void be_init_live(void)
{
	(void)be_live_chk_compare;
	FIRM_DBG_REGISTER(dbg, "firm.be.liveness");
}
