/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Internal headers for liveness analysis.
 * @author      Sebastian Hack
 * @date        06.12.2004
 */
#ifndef FIRM_BE_BELIVE_T_H
#define FIRM_BE_BELIVE_T_H

#include <stdbool.h>
#include "irnodehashmap.h"
#include "irhooks.h"
#include "irlivechk.h"
#include "belive.h"
#include "bearch.h"

#define be_is_live_in(lv, bl, irn)    _be_is_live_xxx(lv, bl, irn, be_lv_state_in)
#define be_is_live_end(lv, bl, irn)   _be_is_live_xxx(lv, bl, irn, be_lv_state_end)
#define be_is_live_out(lv, bl, irn)   _be_is_live_xxx(lv, bl, irn, be_lv_state_out)

struct be_lv_t {
	ir_nodehashmap_t map;
	struct obstack   obst;
	bool             sets_valid;
	ir_graph        *irg;
	lv_chk_t        *lvc;
};

typedef struct be_lv_info_node_t be_lv_info_node_t;
struct be_lv_info_node_t {
	ir_node *node;
	unsigned flags;
};

struct be_lv_info_head_t {
	unsigned n_members;
	unsigned n_size;
};

union be_lv_info_t {
	struct be_lv_info_head_t head;
	struct be_lv_info_node_t node;
};

be_lv_info_node_t *be_lv_get(const be_lv_t *li, const ir_node *block,
                             const ir_node *irn);

static inline unsigned _be_is_live_xxx(const be_lv_t *li, const ir_node *block,
                                       const ir_node *irn, unsigned flags)
{
	unsigned res;

	if (li->sets_valid) {
		be_lv_info_node_t *info = be_lv_get(li, block, irn);
		res = info != NULL ? (info->flags & flags) != 0 : 0;
	} else {
		res = (lv_chk_bl_xxx(li->lvc, block, irn) & flags) != 0;
	}

	return res;
}

typedef struct lv_iterator_t
{
	be_lv_info_t *info;
	size_t        i;
} lv_iterator_t;

static inline lv_iterator_t be_lv_iteration_begin(const be_lv_t *lv,
	const ir_node *block)
{
	lv_iterator_t res;
	res.info  = ir_nodehashmap_get(be_lv_info_t, &lv->map, block);
	res.i     = res.info != NULL ? res.info[0].head.n_members : 0;
	return res;
}

static inline ir_node *be_lv_iteration_next(lv_iterator_t *iterator, be_lv_state_t flags)
{
	while (iterator->i != 0) {
		const be_lv_info_t *info = iterator->info + iterator->i--;
		if (info->node.flags & flags)
			return info->node.node;
	}
	return NULL;
}

static inline ir_node *be_lv_iteration_cls_next(lv_iterator_t *iterator, be_lv_state_t flags, const arch_register_class_t *cls)
{
	while (iterator->i != 0) {
		const be_lv_info_t *info = iterator->info + iterator->i--;
		if (!(info->node.flags & flags))
			continue;

		ir_node *node = info->node.node;
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
