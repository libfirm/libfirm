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
 * @brief   Lower small CopyB nodes into a series of Load/store
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "ircons.h"
#include "lowering.h"
#include "irprog_t.h"
#include "irnode_t.h"
#include "type_t.h"
#include "irtools.h"

typedef struct copyb_env {
	ir_node *next;      /**< link to the next one */
} copyb_env;


typedef struct walker_env {
	struct obstack obst;      /**< temporary space */
	int            max_size;
	ir_node        *list;     /**< list of CopyB nodes. */
} walker_env;

/**
 * Build a graph that copies an entity of type tp from src to tgt
 */
static ir_node *build_copy_graph(dbg_info *dbg, ir_node *blk, ir_node *mem, ir_node *src, ir_node *tgt, ir_type *tp) {
	int i, n;

	if (is_Array_type(tp)) {
		assert(0);
	} else {
		for (i = 0, n = get_compound_n_members(tp); i < n; ++i) {
			ir_entity *ent  = get_compound_member(tp, i);
			ir_node *s      = new_d_simpleSel(dbg, mem, src, ent);
			ir_node *d      = new_d_simpleSel(dbg, mem, tgt, ent);
			ir_type *ent_tp = get_entity_type(ent);

			if (is_atomic_type(ent_tp)) {
				ir_mode *mode = get_type_mode(ent_tp);
				ir_node *irn  = new_rd_Load(dbg, current_ir_graph, blk, mem, s, mode);
				ir_node *res  = new_r_Proj(current_ir_graph, blk, irn, mode, pn_Load_res);

				mem = new_r_Proj(current_ir_graph, blk, irn, mode_M, pn_Load_M);
				irn = new_rd_Store(dbg, current_ir_graph, blk, mem, d, res);
				mem = new_r_Proj(current_ir_graph, blk, irn, mode_M, pn_Store_M);
			} else {
				mem = build_copy_graph(dbg, blk, mem, s, d, ent_tp);
			}
		}
	}
	return mem;
}

/**
 * Walker: lower small CopyB nodes.
 */
static void lower_copyb_nodes(ir_node *irn, void *ctx) {
	ir_type    *tp;
	int        size;
	walker_env *env = ctx;

	if (! is_CopyB(irn))
		return;

	tp = get_CopyB_type(irn);
	if (get_type_state(tp) != layout_fixed)
		return;

	size = get_type_size_bytes(tp);
	if (size > env->max_size)
		return;

	/* for now, we can only handle Struct's */
	if (is_Struct_type(tp)) {
		irn = build_copy_graph(get_irn_dbg_info(irn), get_nodes_block(irn), get_CopyB_mem(irn), get_CopyB_src(irn), get_CopyB_dst(irn), tp);
	}
}

/**
 * Lower CopyB nodes of size smaller that max_size into Loads/Stores
 */
void lower_CopyB(int max_size) {
	walker_env env;
	int        i;

	obstack_init(&env.obst);
	env.max_size = max_size;
	env.list     = NULL;
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		irg_walk_graph(get_irp_irg(i), firm_clear_link, lower_copyb_nodes, NULL);
	}
}
