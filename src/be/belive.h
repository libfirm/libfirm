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
#ifndef FIRM_BE_BELIVE_H
#define FIRM_BE_BELIVE_H

#include "be_types.h"
#include "irnodeset.h"
#include "irnodehashmap.h"
#include "irlivechk.h"
#include "bearch.h"

typedef enum be_lv_state_t {
	be_lv_state_none = 0,
	be_lv_state_in   = 1u << 0,
	be_lv_state_end  = 1u << 1,
	be_lv_state_out  = 1u << 2,
} be_lv_state_t;
ENUM_BITSET(be_lv_state_t)

/**
 * Compute the inter block liveness for a graph.
 * @param irg The graph.
 */
be_lv_t *be_liveness_new(ir_graph *irg);

/**
 * Free the liveness information.
 */
void be_liveness_free(be_lv_t *lv);

/**
 * (Re)compute the liveness information if necessary.
 */
void be_liveness_compute_sets(be_lv_t *lv);
void be_liveness_compute_chk(be_lv_t *lv);

/**
 * Invalidate the liveness information.
 * You must call this if you modify the program and do not
 * update the liveness with the be_liveness_{update,remove,introduce}
 * functions.
 * @note If changed the control flow then you must also call
 *       be_liveness_invalidate_chk()
 */
void be_liveness_invalidate_sets(be_lv_t *lv);
void be_liveness_invalidate_chk(be_lv_t *lv);

/**
 * Update the liveness information for a single node.
 * It is irrelevant if there is liveness information present for the node.
 * The liveness information for the node is firstly deleted and then recomputed.
 * If the node is fresh and never recorded inf the liveness information before,
 * it is more efficient to call be_liveness_introduce().
 */
void be_liveness_update(be_lv_t *lv, ir_node *irn);

/**
 * Remove a node from the liveness information.
 */
void be_liveness_remove(be_lv_t *lv, const ir_node *irn);

/**
 * Introduce a new node to the liveness information.
 * The new irn is not deleted from any block's liveness information, so it must be fresh!
 * @param lv The liveness info.
 * @param irn The node.
 */
void be_liveness_introduce(be_lv_t *lv, ir_node *irn);

/**
 * The liveness transfer function.
 * Updates a live set over a single step from a given node to its predecessor.
 * Everything defined at the node is removed from the set, the uses of the node get inserted.
 * @param cls      The register class to consider.
 * @param irn      The node at which liveness should be computed.
 * @param live     The set of nodes live before @p irn. This set gets modified by updating it to
 *                 the nodes live after irn.
 * @return live.
 */
void be_liveness_transfer(const arch_register_class_t *cls, ir_node *node,
                          ir_nodeset_t *nodeset);

/**
 * Put all node live at the end of a block into a set.
 * @param cls      The register class to consider.
 * @param bl       The block.
 * @param live     The set to put them into.
 * @return live.
 */
void be_liveness_end_of_block(const be_lv_t *lv,
                              const arch_register_class_t *cls,
                              const ir_node *bl, ir_nodeset_t *nodeset);

/**
 * Check if value @p value is live after instruction @p after.
 */
bool be_value_live_after(const ir_node *value, const ir_node *after);

/**
 * Check, if two values interfere.
 * @param lv Liveness information
 * @param a The first value.
 * @param b The second value.
 * @return true, if a and b interfere, false if not.
 */
bool be_values_interfere(const ir_node *a, const ir_node *b);

/**
 * Similar to by_values_interfere() but with special handling for Sync nodes.
 */
bool be_memory_values_interfere(const ir_node *a, const ir_node *b);

/**
 * Compute a set of nodes which are live just before the given node.
 * @param cls      The register class to consider.
 * @param pos      The node.
 * @param live     The set to put them into.
 */
void be_liveness_nodes_live_before(be_lv_t const *lv,
                                   arch_register_class_t const *cls,
                                   ir_node const *pos, ir_nodeset_t *live);

struct be_lv_t {
	ir_nodehashmap_t map;
	struct obstack   obst;
	bool             sets_valid;
	ir_graph        *irg;
	lv_chk_t        *lvc;
};

typedef struct be_lv_info_node_t be_lv_info_node_t;
struct be_lv_info_node_t {
	ir_node      *node;
	be_lv_state_t flags;
};

struct be_lv_info_t {
	unsigned          n_members;
	unsigned          n_size;
	be_lv_info_node_t nodes[];
};

be_lv_info_node_t *be_lv_get(const be_lv_t *li, const ir_node *block,
                             const ir_node *irn);

static inline be_lv_state_t be_get_live_state(be_lv_t const *const li, ir_node const *const block, ir_node const *const irn)
{
	if (li->sets_valid) {
		be_lv_info_node_t *info = be_lv_get(li, block, irn);
		return info ? info->flags : be_lv_state_none;
	} else {
		return lv_chk_bl_xxx(li->lvc, block, irn);
	}
}

/**
 * Check, if a node is live in at a block.
 * @param block The block.
 * @param irn The node to check for.
 * @return true, if @p irn is live at the entrance of @p block
 */
static inline bool be_is_live_in(const be_lv_t *li, const ir_node *block,
                                 const ir_node *node)
{
	return be_get_live_state(li, block, node) & be_lv_state_in;
}

/**
 * Check, if a node is live out at a block.
 * @param block The block.
 * @param irn The node to check for.
 * @return true, if @p irn is live at the exit of @p block
 */
static inline bool be_is_live_out(const be_lv_t *li, const ir_node *block,
                                  const ir_node *node)
{
	return be_get_live_state(li, block, node) & be_lv_state_out;
}

/**
 * Check, if a node is live at the end of a block.
 * @param block The block.
 * @param irn The node to check for.
 * @return true, if @p irn is live at the end of the block
 */
static inline bool be_is_live_end(const be_lv_t *li, const ir_node *block,
                                  const ir_node *node)
{
	return be_get_live_state(li, block, node) & be_lv_state_end;
}

typedef struct lv_iterator_t
{
	be_lv_info_t *info;
	size_t        i;
} lv_iterator_t;

static inline lv_iterator_t be_lv_iteration_begin(const be_lv_t *lv,
                                                  const ir_node *block)
{
	assert(lv->sets_valid);
	lv_iterator_t res;
	res.info  = ir_nodehashmap_get(be_lv_info_t, &lv->map, block);
	res.i     = res.info ? res.info->n_members : 0;
	return res;
}

static inline ir_node *be_lv_iteration_next(lv_iterator_t *iterator,
                                            be_lv_state_t flags)
{
	while (iterator->i != 0) {
		be_lv_info_node_t const *const node = &iterator->info->nodes[--iterator->i];
		assert(get_irn_mode(node->node) != mode_T);
		if (node->flags & flags)
			return node->node;
	}
	return NULL;
}

static inline ir_node *be_lv_iteration_cls_next(lv_iterator_t *iterator,
                                                be_lv_state_t flags,
                                                const arch_register_class_t *cls)
{
	while (iterator->i != 0) {
		be_lv_info_node_t const *const lnode = &iterator->info->nodes[--iterator->i];
		assert(get_irn_mode(lnode->node) != mode_T);
		if (!(lnode->flags & flags))
			continue;

		ir_node *const node = lnode->node;
		if (!arch_irn_consider_in_reg_alloc(cls, node))
			continue;
		return node;
	}
	return NULL;
}

#define be_lv_foreach(lv, block, flags, node) \
	for (bool once = true; once;) \
		for (lv_iterator_t iter = be_lv_iteration_begin((lv), (block)); once; once = false) \
			for (ir_node *node; (node = be_lv_iteration_next(&iter, (flags))) != NULL;)

#define be_lv_foreach_cls(lv, block, flags, cls, node) \
	for (bool once = true; once;) \
		for (lv_iterator_t iter = be_lv_iteration_begin((lv), (block)); once; once = false) \
			for (ir_node *node; (node = be_lv_iteration_cls_next(&iter, (flags), (cls))) != NULL;)

#endif
