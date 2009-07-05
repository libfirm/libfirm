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
 * @author  Michael Beck, Matthias Braun
 * @version $Id$
 */
#include "config.h"

#include "adt/list.h"
#include "ircons.h"
#include "lowering.h"
#include "irprog_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "type_t.h"
#include "irtools.h"
#include "irgmod.h"
#include "error.h"

typedef struct entry entry_t;
struct entry {
	struct list_head list;
	ir_node *copyb;
};

typedef struct walk_env {
	unsigned         max_size;
	struct obstack   obst;              /**< the obstack where data is allocated on */
	struct list_head list;              /**< the list of copyb nodes */
} walk_env_t;

static ir_mode *get_ir_mode(unsigned bytes)
{
	switch (bytes) {
	case 1:	 return mode_Bu;
	case 2:  return mode_Hu;
	case 4:  return mode_Iu;
	case 8:  return mode_Lu;
	case 16: return mode_LLu;
	default:
		panic("unexpected mode size requested in copyb lowering");
	}
}

/**
 * lower a CopyB node.
 */
static void lower_copyb_nodes(ir_node *irn, unsigned mode_bytes) {
	ir_graph        *irg = current_ir_graph;
	unsigned         size;
	unsigned         offset;
	ir_mode         *mode;
	ir_mode         *addr_mode;
	ir_node         *mem;
	ir_node         *addr_src;
	ir_node         *addr_dst;
	ir_node         *block;
	ir_type         *tp;

	addr_src  = get_CopyB_src(irn);
	addr_dst  = get_CopyB_dst(irn);
	mem       = get_CopyB_mem(irn);
	addr_mode = get_irn_mode(addr_src);
	block     = get_nodes_block(irn);

	tp   = get_CopyB_type(irn);
	size = get_type_size_bytes(tp);

	offset     = 0;
	while (offset < size) {
		mode = get_ir_mode(mode_bytes);
		for (; offset + mode_bytes <= size; offset += mode_bytes) {
			/* construct offset */
			ir_node *addr_const;
			ir_node *add;
			ir_node *load;
			ir_node *load_res;
			ir_node *load_mem;
			ir_node *store;
			ir_node *store_mem;

			addr_const = new_r_Const_long(irg, mode_Iu, offset);
			add        = new_r_Add(block, addr_src, addr_const, addr_mode);

			load     = new_r_Load(block, mem, add, mode, 0);
			load_res = new_r_Proj(block, load, mode, pn_Load_res);
			load_mem = new_r_Proj(block, load, mode_M, pn_Load_M);

			addr_const = new_r_Const_long(irg, mode_Iu, offset);
			add        = new_r_Add(block, addr_dst, addr_const, addr_mode);

			store     = new_r_Store(block, load_mem, add, load_res, 0);
			store_mem = new_r_Proj(block, store, mode_M, pn_Store_M);

			mem = store_mem;
		}

		mode_bytes /= 2;
	}

	turn_into_tuple(irn, pn_CopyB_max);
	set_Tuple_pred(irn, pn_CopyB_M_regular, mem);
	set_Tuple_pred(irn, pn_CopyB_X_regular, get_irg_bad(irg));
	set_Tuple_pred(irn, pn_CopyB_X_except,  get_irg_bad(irg));
	set_Tuple_pred(irn, pn_CopyB_M_except,  get_irg_bad(irg));
}

/**
 * Post-Walker: find small CopyB nodes.
 */
static void find_copyb_nodes(ir_node *irn, void *ctx) {
	walk_env_t *env = ctx;
	ir_type    *tp;
	unsigned   size;
	entry_t    *entry;

	if (is_Proj(irn)) {
		ir_node *pred = get_Proj_pred(irn);

		if (is_CopyB(pred) && get_Proj_proj(irn) != pn_CopyB_M_regular) {
			/* found an exception Proj: remove it from the list again */
			entry = get_irn_link(pred);
			list_del(&entry->list);
		}
		return;
	}

	if (! is_CopyB(irn))
		return;

	tp = get_CopyB_type(irn);
	if (get_type_state(tp) != layout_fixed)
		return;

	size = get_type_size_bytes(tp);
	if (size > env->max_size)
		return;

	/* ok, link it in */
	entry = obstack_alloc(&env->obst, sizeof(*entry));
	entry->copyb = irn;
	INIT_LIST_HEAD(&entry->list);
	set_irn_link(irn, entry);
	list_add_tail(&entry->list, &env->list);
}

void lower_CopyB(ir_graph *irg, unsigned max_size, unsigned native_mode_bytes)
{
	walk_env_t env;
	entry_t  *entry;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;

	obstack_init(&env.obst);
	env.max_size = max_size;
	INIT_LIST_HEAD(&env.list);
	irg_walk_graph(irg, NULL, find_copyb_nodes, &env);

	list_for_each_entry(entry_t, entry, &env.list, list) {
		lower_copyb_nodes(entry->copyb, native_mode_bytes);
	}

	obstack_free(&env.obst, NULL);
	current_ir_graph = rem;
}
