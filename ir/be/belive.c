/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Interblock liveness analysis.
 * @author      Sebastian Hack
 * @date        06.12.2004
 */
/* statev is expensive here, only enable when needed */
#define DISABLE_STATEV

#include "debug.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irdump_t.h"
#include "irnodeset.h"

#include "statev_t.h"
#include "be_t.h"
#include "belive.h"
#include "besched.h"
#include "bemodule.h"
#include "beirg.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define LV_STD_SIZE             63

static unsigned _be_liveness_bsearch(be_lv_info_t const *const arr, ir_node const *const node)
{
	unsigned const n = arr->n_members;
	if (n == 0)
		return 0;

	unsigned lo = 0;
	unsigned hi = n;
	unsigned res;
	do {
		unsigned md      = lo + ((hi - lo) >> 1);
		ir_node *md_node = arr->nodes[md].node;

		if (node > md_node) {
			lo = md + 1;
		} else if (node < md_node) {
			hi = md;
		} else {
			res = md;
			break;
		}

		res = lo;
	} while (lo < hi);

	return res;
}

be_lv_info_node_t *be_lv_get(const be_lv_t *li, const ir_node *bl,
                             const ir_node *irn)
{
	stat_ev_tim_push();
	be_lv_info_t      *irn_live = ir_nodehashmap_get(be_lv_info_t, &li->map, bl);
	be_lv_info_node_t *res      = NULL;
	if (irn_live != NULL) {
		/* Get the position of the index in the array. */
		unsigned pos = _be_liveness_bsearch(irn_live, irn);

		/* Get the record in question. */
		be_lv_info_node_t *const rec = &irn_live->nodes[pos];

		/* Check, if the irn is in deed in the array. */
		if (rec->node == irn)
			res = rec;
	}
	stat_ev_tim_pop("be_lv_get");

	return res;
}

static be_lv_info_node_t *be_lv_get_or_set(be_lv_t *li, ir_node *bl,
                                           ir_node *irn)
{
	assert(get_irn_mode(irn) != mode_T);

	be_lv_info_t *irn_live = ir_nodehashmap_get(be_lv_info_t, &li->map, bl);
	if (irn_live == NULL) {
		irn_live = OALLOCFZ(&li->obst, be_lv_info_t, nodes, LV_STD_SIZE);
		irn_live->n_size = LV_STD_SIZE;
		ir_nodehashmap_insert(&li->map, bl, irn_live);
	}

	/* Get the position of the index in the array. */
	unsigned pos = _be_liveness_bsearch(irn_live, irn);

	/* Get the record in question. */
	be_lv_info_node_t *res = &irn_live->nodes[pos];

	/* Check, if the irn is in deed in the array. */
	if (res->node != irn) {
		unsigned n_members = irn_live->n_members;
		unsigned n_size    = irn_live->n_size;
		if (n_members + 1 >= n_size) {
			/* double the array size. */
			unsigned      const new_size = 2 * n_size;
			be_lv_info_t *const nw       = OALLOCF(&li->obst, be_lv_info_t, nodes, new_size);
			memcpy(nw, irn_live, sizeof(*irn_live) + n_size * sizeof(*irn_live->nodes));
			memset(&nw->nodes[n_size], 0, (new_size - n_size) * sizeof(*irn_live->nodes));
			nw->n_size = new_size;
			irn_live = nw;
			ir_nodehashmap_insert(&li->map, bl, nw);
		}

		for (unsigned i = n_members; i > pos; --i) {
			irn_live->nodes[i] = irn_live->nodes[i - 1];
		}

		++irn_live->n_members;
		res        = &irn_live->nodes[pos];
		res->node  = irn;
		res->flags = 0;
	}

	return res;
}

typedef struct lv_remove_walker_t {
	be_lv_t       *lv;
	ir_node const *irn;
} lv_remove_walker_t;

/**
 * Removes a node from the list of live variables of a block.
 */
static void lv_remove_irn_walker(ir_node *const bl, void *const data)
{
	lv_remove_walker_t *const w        = (lv_remove_walker_t*)data;
	be_lv_info_t       *const irn_live = ir_nodehashmap_get(be_lv_info_t, &w->lv->map, bl);
	if (irn_live == NULL)
		return;

	unsigned           const n   = irn_live->n_members;
	ir_node     const *const irn = w->irn;
	unsigned           const pos = _be_liveness_bsearch(irn_live, irn);
	be_lv_info_node_t *const res = &irn_live->nodes[pos];
	if (res->node != irn)
		return;

	/* The node is indeed in the block's array. Let's remove it. */
	for (unsigned i = pos + 1; i < n; ++i)
		irn_live->nodes[i - 1] = irn_live->nodes[i];

	irn_live->nodes[n - 1].node  = NULL;
	irn_live->nodes[n - 1].flags = 0;

	--irn_live->n_members;
	DBG((dbg, LEVEL_3, "\tdeleting %+F from %+F at pos %d\n", irn, bl, pos));
}

static struct {
	be_lv_t *lv;         /**< The liveness object. */
	ir_node *def;        /**< The node (value). */
	ir_node *def_block;  /**< The block of def. */
} re;

/**
 * Mark a node (value) live out at a certain block. Do this also
 * transitively, i.e. if the block is not the block of the value's
 * definition, all predecessors are also marked live.
 * @param block The block to mark the value live out of.
 * @param state The liveness bits to set, either end or end+out.
 */
static void live_end_at_block(ir_node *const block, be_lv_state_t const state)
{
	be_lv_info_node_t *const n      = be_lv_get_or_set(re.lv, block, re.def);
	be_lv_state_t      const before = n->flags;

	assert(state == be_lv_state_end || state == (be_lv_state_end | be_lv_state_out));
	DBG((dbg, LEVEL_2, "marking %+F live %s at %+F\n", re.def,
	     state & be_lv_state_out ? "end+out" : "end", block));
	n->flags |= state;

	/* There is no need to recurse further, if we where here before (i.e., any
	 * live state bits were set before). */
	if (before != be_lv_state_none)
		return;

	/* Stop going up further, if this is the block of the definition. */
	if (re.def_block == block)
		return;

	DBG((dbg, LEVEL_2, "marking %+F live in at %+F\n", re.def, block));
	n->flags |= be_lv_state_in;

	for (unsigned i = get_Block_n_cfgpreds(block); i-- > 0;) {
		ir_node *const pred_block = get_Block_cfgpred_block(block, i);
		live_end_at_block(pred_block, be_lv_state_end | be_lv_state_out);
	}
}

/**
 * Liveness analysis for a value.
 * Compute the set of all blocks a value is live in.
 * @param irn     The node (value).
 */
static void liveness_for_node(ir_node *irn)
{
	ir_node *const def_block = get_nodes_block(irn);

	re.def       = irn;
	re.def_block = def_block;

	/* Go over all uses of the value */
	foreach_out_edge(irn, edge) {
		ir_node *use = edge->src;
		/* If the usage is no data node, skip this use, since it does not
		 * affect the liveness of the node. */
		if (!is_liveness_node(use))
			continue;

		DBG((dbg, LEVEL_4, "%+F: use at %+F, pos %d in %+F\n", irn, use,
		     edge->pos, get_block(use)));

		/* Get the block where the usage is in. */
		ir_node *use_block = get_nodes_block(use);

		/* If the use is a phi function, determine the corresponding block
		 * through which the value reaches the phi function and mark the
		 * value as live out of that block. */
		if (is_Phi(use)) {
			ir_node *pred_block = get_Block_cfgpred_block(use_block, edge->pos);
			live_end_at_block(pred_block, be_lv_state_end);
		} else if (def_block != use_block) {
			/* Else, the value is live in at this block. Mark it and call live
			 * out on the predecessors. */
			be_lv_info_node_t *const n = be_lv_get_or_set(re.lv, use_block, irn);
			DBG((dbg, LEVEL_2, "marking %+F live in at %+F\n", irn, use_block));
			n->flags |= be_lv_state_in;

			for (unsigned i = get_Block_n_cfgpreds(use_block); i-- > 0; ) {
				ir_node *pred_block = get_Block_cfgpred_block(use_block, i);
				live_end_at_block(pred_block, be_lv_state_end | be_lv_state_out);
			}
		}
	}
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
	if (lv->sets_valid)
		return;

	be_timer_push(T_LIVE);
	ir_nodehashmap_init(&lv->map);
	obstack_init(&lv->obst);

	ir_graph *irg = lv->irg;
	unsigned n = get_irg_last_idx(irg);
	ir_node **const nodes = NEW_ARR_FZ(ir_node*, n);

	/* inserting the variables sorted by their ID is probably
	 * more efficient since the binary sorted set insertion
	 * will not need to move around the data. */
	irg_walk_graph(irg, NULL, collect_liveness_nodes, nodes);

	re.lv = lv;

	for (unsigned i = 0; i < n; ++i) {
		if (nodes[i] != NULL)
			liveness_for_node(nodes[i]);
	}

	DEL_ARR_F(nodes);
	lv->sets_valid = true;
	be_timer_pop(T_LIVE);
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
	return lv;
}

void be_liveness_free(be_lv_t *lv)
{
	be_liveness_invalidate_sets(lv);
	be_liveness_invalidate_chk(lv);
	free(lv);
}

void be_liveness_remove(be_lv_t *lv, const ir_node *irn)
{
	assert(lv->sets_valid);

	/* Removes a single irn from the liveness information.
	 * Since an irn can only be live at blocks dominated by the block of its
	 * definition, we only have to process that dominance subtree. */
	lv_remove_walker_t w = { lv, irn };
	dom_tree_walk(get_nodes_block(irn), lv_remove_irn_walker, NULL, &w);
}

void be_liveness_introduce(be_lv_t *lv, ir_node *irn)
{
	assert(lv->sets_valid);
	/* Don't compute liveness information for non-data nodes. */
	if (is_liveness_node(irn)) {
		re.lv = lv;
		liveness_for_node(irn);
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
	/* The arguments of phi functions are not live at the beginning of the block
	 * so calling liveness_transfer on a Phi doesn't make sense. */
	assert(!is_Phi(node));

	be_foreach_definition(node, cls, value, req,
		ir_nodeset_remove(nodeset, value);
	);

	be_foreach_use(node, cls, in_req, op, op_req,
		ir_nodeset_insert(nodeset, op);
	);
}

void be_liveness_end_of_block(const be_lv_t *lv,
                              const arch_register_class_t *cls,
                              const ir_node *block, ir_nodeset_t *live)
{
	be_lv_foreach_cls(lv, block, be_lv_state_end, cls, node) {
		ir_nodeset_insert(live, node);
	}
}

void be_liveness_nodes_live_before(be_lv_t const *const lv,
                                   arch_register_class_t const *const cls,
                                   ir_node const *const pos,
                                   ir_nodeset_t *const live)
{
	ir_node *const bl = get_nodes_block(pos);
	be_liveness_end_of_block(lv, cls, bl, live);
	sched_foreach_reverse(bl, irn) {
		be_liveness_transfer(cls, irn, live);
		if (irn == pos)
			return;
	}
}

/**
 * Check if value @p value is live at definition of @p after.
 * @note assumes value dominates after
 */
static bool value_live_after(const ir_node *const value,
                             const ir_node *const after)
{
	/* If value is live end in after's block it is
	 * live at after's definition (value dominates after) */
	const ir_node  *const bb  = get_nodes_block(after);
	const ir_graph *const irg = get_irn_irg(after);
	const be_lv_t  *const lv  = be_get_irg_liveness(irg);
	if (be_is_live_end(lv, bb, value))
		return true;

	/* Look at all usages of value.
	 * If there's one usage of value in the block of after, then we check, if
	 * this use is dominated by after, if that's true value and after interfere.
	 * Note that after must strictly dominate the user, since if after is the
	 * last user of in the block, after and value do not interfere.
	 * Uses of value not in after's block can be disobeyed, because the check
	 * for value being live at the end of after's block is already performed. */
	foreach_out_edge(value, edge) {
		const ir_node *const user = get_edge_src_irn(edge);
		if (get_nodes_block(user) == bb && !is_Phi(user)
		    && sched_comes_before(after, user))
			return true;
	}

	return false;
}

bool be_value_live_after(const ir_node *value, const ir_node *after)
{
	assert(value != after);
	if (!value_strictly_dominates(value, after))
		return false;
	return value_live_after(value, after);
}

bool be_values_interfere(const ir_node *a, const ir_node *b)
{
	assert(a != b);
	if (value_strictly_dominates(b, a)) {
		return value_live_after(b, a);
	} else if (value_strictly_dominates(a, b)) {
		return value_live_after(a, b);
	}
	return false;
}

static bool live_at_user(const ir_node *const users_of,
                         const ir_node *const node, const ir_node *const bb)
{
	foreach_out_edge(users_of, edge) {
		const ir_node *const user = get_edge_src_irn(edge);
		if (is_Sync(user))
			return live_at_user(user, node, bb);
		if (get_nodes_block(user) == bb && !is_Phi(user)
		    && sched_comes_before(node, user))
			return true;
	}
	return false;
}

static const ir_node *get_highest_sync_op(const ir_node *const sync)
{
	const ir_node *best = get_irn_n(sync, 0);
	for (int i = 1, arity = get_irn_arity(sync); i < arity; ++i) {
		const ir_node *other = get_irn_n(sync, i);
		if (is_Sync(other))
			other = get_highest_sync_op(other);
		if (value_strictly_dominates(other, best))
			best = other;
	}
	return best;
}

bool be_memory_values_interfere(const ir_node *a, const ir_node *b)
{
	if (is_Sync(a))
		a = get_highest_sync_op(a);
	if (is_Sync(b))
		b = get_highest_sync_op(b);

	if (value_strictly_dominates(b, a)) {
		/* Adjust a and b so, that a dominates b if
		 * a dominates b or vice versa. */
		ir_node const *const t = a;
		a = b;
		b = t;
	} else if (!value_strictly_dominates(a, b)) {
		/* If there is no dominance relation, they do not interfere. */
		return false;
	}

	/* If a is live end in b's block it is
	 * live at b's definition (a dominates b) */
	const ir_node  *const bb  = get_nodes_block(b);
	const ir_graph *const irg = get_irn_irg(b);
	const be_lv_t  *const lv  = be_get_irg_liveness(irg);
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
	return live_at_user(a, b, bb);
}

static void collect_node(ir_node *irn, void *data)
{
	struct obstack *obst = (struct obstack*)data;
	obstack_ptr_grow(obst, irn);
}

static void be_live_chk_compare(be_lv_t *lv, lv_chk_t *lvc)
{
	struct obstack obst;
	obstack_init(&obst);

	ir_graph *irg = lv->irg;
	irg_block_walk_graph(irg, collect_node, NULL, &obst);
	obstack_ptr_grow(&obst, NULL);
	ir_node **blocks = (ir_node**)obstack_finish(&obst);

	irg_walk_graph(irg, collect_node, NULL, &obst);
	obstack_ptr_grow(&obst, NULL);
	ir_node **nodes = (ir_node**)obstack_finish(&obst);

	stat_ev_ctx_push("be_lv_chk_compare");
	for (unsigned j = 0; nodes[j] != NULL; ++j) {
		const ir_node *irn = nodes[j];
		if (is_Block(irn))
			continue;

		for (unsigned i = 0; blocks[i] != NULL; ++i) {
			const ir_node *bl = blocks[i];
			bool lvr_in  = be_is_live_in (lv, bl, irn);
			bool lvr_out = be_is_live_out(lv, bl, irn);
			bool lvr_end = be_is_live_end(lv, bl, irn);

			bool lvc_in  = lv_chk_bl_in (lvc, bl, irn);
			bool lvc_out = lv_chk_bl_out(lvc, bl, irn);
			bool lvc_end = lv_chk_bl_end(lvc, bl, irn);

			if (lvr_in != lvc_in)
				ir_fprintf(stderr, "live in  info for %+F at %+F differs: nml: %d, chk: %d\n", irn, bl, lvr_in, lvc_in);

			if (lvr_end != lvc_end)
				ir_fprintf(stderr, "live end info for %+F at %+F differs: nml: %d, chk: %d\n", irn, bl, lvr_end, lvc_end);

			if (lvr_out != lvc_out)
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
