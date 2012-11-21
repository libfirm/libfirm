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
 * @brief       Internal headers for liveness analysis.
 * @author      Sebastian Hack
 * @date        06.12.2004
 */
#ifndef FIRM_BE_BELIVE_T_H
#define FIRM_BE_BELIVE_T_H

#include <stdbool.h>
#include "be_types.h"
#include "irgraph_t.h"
#include "irnodehashmap.h"
#include "irhooks.h"
#include "irlivechk.h"
#include "statev_t.h"

#include "pset.h"
#include "bitset.h"

#include "belive.h"

struct be_lv_t {
	ir_nodehashmap_t map;
	struct obstack   obst;
	bool             sets_valid;
	ir_graph        *irg;
	hook_entry_t     hook_info;
	lv_chk_t        *lvc;
};

typedef struct be_lv_info_node_t be_lv_info_node_t;
struct be_lv_info_node_t {
	unsigned idx;
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
	ir_graph     *irg;
	be_lv_state_t flags;
	size_t        i;
} lv_iterator_t;

static inline lv_iterator_t be_lv_iteration_begin(const be_lv_t *lv,
	const ir_node *block, be_lv_state_t flags)
{
	lv_iterator_t res;
	res.info  = ir_nodehashmap_get(be_lv_info_t, &lv->map, block);
	res.irg   = get_Block_irg(block);
	res.flags = flags;
	res.i     = res.info != NULL ? res.info[0].head.n_members : 0;
	return res;
}

static inline ir_node *be_lv_iteration_next(lv_iterator_t *iterator)
{
	while (iterator->i != 0) {
		const be_lv_info_t *info = iterator->info + iterator->i--;
		if (info->node.flags & iterator->flags)
			return get_idx_irn(iterator->irg, info->node.idx);
	}
	return NULL;
}

#define be_lv_foreach(lv, block, flags, node) \
	for (bool once = true; once;) \
		for (lv_iterator_t iter = be_lv_iteration_begin((lv), (block), (flags)); once; once = false) \
			for (ir_node *node; (node = be_lv_iteration_next(&iter)) != NULL;)

static inline pset *_be_lv_pset_put(const be_lv_t *lv, const ir_node *block,
                                    int state, pset *s)
{
	be_lv_foreach(lv, block, state, node)
		pset_insert_ptr(s, node);
	return s;
}

#define be_lv_get_irn(lv, bl, i)      _be_lv_get_irn(lv, bl, i)
#define be_lv_pset_put_in(lv, bl, s)  _be_lv_pset_put(lv, bl, be_lv_state_in, s)
#define be_lv_pset_put_out(lv, bl, s) _be_lv_pset_put(lv, bl, be_lv_state_out, s)
#define be_lv_pset_put_end(lv, bl, s) _be_lv_pset_put(lv, bl, be_lv_state_end, s)

#define be_is_live_in(lv, bl, irn)    _be_is_live_xxx(lv, bl, irn, be_lv_state_in)
#define be_is_live_end(lv, bl, irn)   _be_is_live_xxx(lv, bl, irn, be_lv_state_end)
#define be_is_live_out(lv, bl, irn)   _be_is_live_xxx(lv, bl, irn, be_lv_state_out)

#define be_lv_has_info_about(lv, irn) bitset_is_set((lv)->nodes, get_irn_idx(irn))

#endif
