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

#include "be_types.h"
#include "irgraph_t.h"
#include "irnodehashmap.h"
#include "irhooks.h"
#include "statev.h"

#include "pset.h"
#include "bitset.h"

#include "belive.h"

#define USE_LIVE_CHK

#ifdef USE_LIVE_CHK
#include "irlivechk.h"
#endif

struct be_lv_t {
	ir_nodehashmap_t map;
	struct obstack   obst;
	ir_graph        *irg;
	bitset_t        *nodes;
	hook_entry_t     hook_info;
#ifdef USE_LIVE_CHK
	lv_chk_t        *lvc;
#endif
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

static inline int _be_lv_next_irn(const be_lv_t *lv, const ir_node *block,
                                  unsigned flags, int i)
{
	be_lv_info_t *arr = (be_lv_info_t*)ir_nodehashmap_get(&lv->map, block);
	if (arr != NULL) {
		int n_members = (int) arr[0].head.n_members;
		while(i < n_members) {
			if(arr[i + 1].node.flags & flags) {
				return i;
			}
			++i;
		}
	}

	return -1;
}

static inline ir_node *_be_lv_get_irn(const be_lv_t *lv, const ir_node *block,
                                      int i)
{
	be_lv_info_t *arr = (be_lv_info_t*)ir_nodehashmap_get(&lv->map, block);
	return get_idx_irn(lv->irg, arr[i + 1].node.idx);
}

be_lv_info_node_t *be_lv_get(const be_lv_t *li, const ir_node *block,
                             const ir_node *irn);

static inline unsigned _be_is_live_xxx(const be_lv_t *li, const ir_node *block,
                                       const ir_node *irn, unsigned flags)
{
	unsigned res;

	if (li->nodes != NULL) {
		be_lv_info_node_t *info = be_lv_get(li, block, irn);
		res = info != NULL ? (info->flags & flags) != 0 : 0;
	}

#ifdef USE_LIVE_CHK
	else
		res = (lv_chk_bl_xxx(li->lvc, block, irn) & flags) != 0;
#endif

	return res;
}

#define be_lv_foreach(lv, bl, flags, i) \
	for (i = _be_lv_next_irn(lv, bl, flags, 0); i >= 0; i = _be_lv_next_irn(lv, bl, flags, i + 1))


static inline pset *_be_lv_pset_put(const be_lv_t *lv, const ir_node *block,
                                    int state, pset *s)
{
	int i;
	be_lv_foreach(lv, block, state, i)
		pset_insert_ptr(s, _be_lv_get_irn(lv, block, i));
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
