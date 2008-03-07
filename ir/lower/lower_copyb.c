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
 * @author  Matthias Braun, Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "ircons.h"
#include "lowering.h"
#include "irprog_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "type_t.h"
#include "irtools.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "error.h"

static unsigned max_size;
static unsigned native_mode_bytes;

static ir_mode *get_ir_mode(unsigned bytes)
{
	switch(bytes) {
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
 * Walker: lower small CopyB nodes.
 */
static void lower_copyb_nodes(ir_node *irn, void *ctx) {
	ir_graph        *irg = current_ir_graph;
	ir_type         *tp;
	unsigned         size;
	unsigned         mode_bytes;
	unsigned         offset;
	ir_mode         *mode;
	ir_mode         *addr_mode;
	ir_node         *mem;
	ir_node         *addr_src;
	ir_node         *addr_dst;
	ir_node         *block;
	ir_node         *proj_M = NULL;
	const ir_edge_t *edge;
	(void) ctx;

	if (! is_CopyB(irn))
		return;

	tp = get_CopyB_type(irn);
	if (get_type_state(tp) != layout_fixed)
		return;

	size = get_type_size_bytes(tp);
	if (size > max_size)
		return;

	foreach_out_edge(irn, edge) {
		ir_node *node = get_edge_src_irn(edge);
		long     pn   = get_Proj_proj(node);

		/* we don't lower copybs with exception edges (yet) */
		if(pn == pn_CopyB_X_except)
			return;
		if(pn == pn_CopyB_M_regular) {
			assert(proj_M == NULL);
			proj_M = node;
		}
	}
	if(proj_M == NULL) {
		panic("no projM on copyb");
	}

	addr_src  = get_CopyB_src(irn);
	addr_dst  = get_CopyB_dst(irn);
	mem       = get_CopyB_mem(irn);
	addr_mode = get_irn_mode(addr_src);
	block     = get_nodes_block(irn);

	offset     = 0;
	mode_bytes = native_mode_bytes;
	while(offset < size) {
		mode = get_ir_mode(mode_bytes);
		for( ; offset + mode_bytes <= size; offset += mode_bytes) {
			/* construct offset */
			ir_node *addr_const;
			ir_node *add;
			ir_node *load;
			ir_node *load_res;
			ir_node *load_mem;
			ir_node *store;
			ir_node *store_mem;

			addr_const = new_r_Const_long(irg, block, mode_Iu, offset);
			add        = new_r_Add(irg, block, addr_src, addr_const, addr_mode);

			load     = new_r_Load(irg, block, mem, add, mode);
			load_res = new_r_Proj(irg, block, load, mode, pn_Load_res);
			load_mem = new_r_Proj(irg, block, load, mode_M, pn_Load_M);

			addr_const = new_r_Const_long(irg, block, mode_Iu, offset);
			add        = new_r_Add(irg, block, addr_dst, addr_const, addr_mode);

			store     = new_r_Store(irg, block, mem, add, load_res);
			store_mem = new_r_Proj(irg, block, store, mode_M, pn_Store_M);

			mem = store_mem;
		}

		mode_bytes /= 2;
	}

	exchange(proj_M, mem);
}

void lower_CopyB(ir_graph *irg, unsigned new_max_size,
                 unsigned new_native_mode_bytes)
{
	max_size          = new_max_size;
	native_mode_bytes = new_native_mode_bytes;

	edges_assure(irg);

	irg_walk_graph(irg, NULL, lower_copyb_nodes, NULL);

	edges_deactivate(irg);
}
