/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief   parallelizing Load/Store optimisation
 * @author  Christoph Mallon
 * @version $Id: $
 */
#include "config.h"

#include "iroptimize.h"

#include "array_t.h"
#include "debug.h"
#include "ircons.h"
#include "irgraph.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irmemory.h"
#include "irnode.h"
#include "irnodeset.h"
#include "obst.h"
#include "irdump.h"
#include "irflag_t.h"
#include "irprintf.h"
#include "irpass.h"
#include "opt_manage.h"

typedef struct parallelize_info
{
	ir_node      *origin_block;
	ir_node      *origin_ptr;
	ir_mode      *origin_mode;
	ir_nodeset_t  this_mem;
	ir_nodeset_t  user_mem;
} parallelize_info;

static void parallelize_load(parallelize_info *pi, ir_node *irn)
{
	/* There is no point in investigating the same subgraph twice */
	if (ir_nodeset_contains(&pi->user_mem, irn))
		return;

	if (get_nodes_block(irn) == pi->origin_block) {
		if (is_Proj(irn)) {
			ir_node *pred = get_Proj_pred(irn);
			if (is_Load(pred) &&
					get_Load_volatility(pred) == volatility_non_volatile) {
				ir_node *mem = get_Load_mem(pred);
				//ir_nodeset_insert(&pi->this_mem, mem);
				ir_nodeset_insert(&pi->user_mem, irn);
				parallelize_load(pi, mem);
				return;
			} else if (is_Store(pred) &&
					get_Store_volatility(pred) == volatility_non_volatile) {
				ir_mode *org_mode   = pi->origin_mode;
				ir_node *org_ptr    = pi->origin_ptr;
				ir_mode *store_mode = get_irn_mode(get_Store_value(pred));
				ir_node *store_ptr  = get_Store_ptr(pred);
				if (get_alias_relation(org_ptr, org_mode, store_ptr, store_mode) == ir_no_alias) {
					ir_node *mem = get_Store_mem(pred);
					ir_nodeset_insert(&pi->user_mem, irn);
					parallelize_load(pi, mem);
					return;
				}
			}
		} else if (is_Sync(irn)) {
			int n = get_Sync_n_preds(irn);
			int i;

			for (i = 0; i < n; ++i) {
				ir_node *sync_pred = get_Sync_pred(irn, i);
				parallelize_load(pi, sync_pred);
			}
			return;
		}
	}
	ir_nodeset_insert(&pi->this_mem, irn);
}

static void parallelize_store(parallelize_info *pi, ir_node *irn)
{
	/* There is no point in investigating the same subgraph twice */
	if (ir_nodeset_contains(&pi->user_mem, irn))
		return;

	//ir_fprintf(stderr, "considering %+F\n", irn);
	if (get_nodes_block(irn) == pi->origin_block) {
		if (is_Proj(irn)) {
			ir_node *pred = get_Proj_pred(irn);
			if (is_Load(pred) &&
					get_Load_volatility(pred) == volatility_non_volatile) {
				ir_mode *org_mode  = pi->origin_mode;
				ir_node *org_ptr   = pi->origin_ptr;
				ir_mode *load_mode = get_Load_mode(pred);
				ir_node *load_ptr  = get_Load_ptr(pred);
				if (get_alias_relation(org_ptr, org_mode, load_ptr, load_mode) == ir_no_alias) {
					ir_node *mem = get_Load_mem(pred);
					ir_nodeset_insert(&pi->user_mem, irn);
					parallelize_store(pi, mem);
					return;
				}
			} else if (is_Store(pred) &&
					get_Store_volatility(pred) == volatility_non_volatile) {
				ir_mode *org_mode   = pi->origin_mode;
				ir_node *org_ptr    = pi->origin_ptr;
				ir_mode *store_mode = get_irn_mode(get_Store_value(pred));
				ir_node *store_ptr  = get_Store_ptr(pred);
				if (get_alias_relation(org_ptr, org_mode, store_ptr, store_mode) == ir_no_alias) {
					ir_node *mem;

					ir_nodeset_insert(&pi->user_mem, irn);
					mem = get_Store_mem(pred);
					parallelize_store(pi, mem);
					return;
				}
			}
		} else if (is_Sync(irn)) {
			int n = get_Sync_n_preds(irn);
			int i;

			for (i = 0; i < n; ++i) {
				ir_node *sync_pred = get_Sync_pred(irn, i);
				parallelize_store(pi, sync_pred);
			}
			return;
		}
	}
	ir_nodeset_insert(&pi->this_mem, irn);
}

static void walker(ir_node *proj, void *env)
{
	ir_node          *mem_op;
	ir_node          *pred;
	ir_node          *block;
	size_t            n;
	parallelize_info  pi;

	(void)env;

	if (!is_Proj(proj)) return;
	if (get_irn_mode(proj) != mode_M) return;

	mem_op = get_Proj_pred(proj);
	if (is_Load(mem_op)) {
		if (get_Load_volatility(mem_op) != volatility_non_volatile) return;

		block = get_nodes_block(mem_op);
		pred  = get_Load_mem(mem_op);

		pi.origin_block = block,
		pi.origin_ptr   = get_Load_ptr(mem_op);
		pi.origin_mode  = get_Load_mode(mem_op);
		ir_nodeset_init(&pi.this_mem);
		ir_nodeset_init(&pi.user_mem);

		parallelize_load(&pi, pred);
	} else if (is_Store(mem_op)) {
		if (get_Store_volatility(mem_op) != volatility_non_volatile) return;

		block = get_nodes_block(mem_op);
		pred  = get_Store_mem(mem_op);

		pi.origin_block = block,
		pi.origin_ptr   = get_Store_ptr(mem_op);
		pi.origin_mode  = get_irn_mode(get_Store_value(mem_op));
		ir_nodeset_init(&pi.this_mem);
		ir_nodeset_init(&pi.user_mem);

		parallelize_store(&pi, pred);
	} else {
		return;
	}

	n = ir_nodeset_size(&pi.user_mem);
	if (n != 0) { /* nothing happened otherwise */
		ir_graph               *irg  = get_irn_irg(block);
		ir_node                *sync;
		ir_node               **in;
		ir_nodeset_iterator_t   iter;
		size_t                  i;

		++n;
		NEW_ARR_A(ir_node*, in, n);
		i = 0;
		in[i++] = new_r_Unknown(irg, mode_M);
		ir_nodeset_iterator_init(&iter, &pi.user_mem);
		for (;;) {
			ir_node* p = ir_nodeset_iterator_next(&iter);
			if (p == NULL) break;
			in[i++] = p;
		}
		assert(i == n);
		sync = new_r_Sync(block, n, in);
		exchange(proj, sync);

		assert((long)pn_Load_M == (long)pn_Store_M);
		proj = new_r_Proj(mem_op, mode_M, pn_Load_M);
		set_Sync_pred(sync, 0, proj);

		n = ir_nodeset_size(&pi.this_mem);
		ir_nodeset_iterator_init(&iter, &pi.this_mem);
		if (n == 1) {
			sync = ir_nodeset_iterator_next(&iter);
		} else {
			NEW_ARR_A(ir_node*, in, n);
			i = 0;
			for (;;) {
				ir_node* p = ir_nodeset_iterator_next(&iter);
				if (p == NULL) break;
				in[i++] = p;
			}
			assert(i == n);
			sync = new_r_Sync(block, n, in);
		}
		set_memop_mem(mem_op, sync);
	}

	ir_nodeset_destroy(&pi.this_mem);
	ir_nodeset_destroy(&pi.user_mem);
}

static ir_graph_state_t do_parallelize_mem(ir_graph *irg)
{
	irg_walk_graph(irg, NULL, walker, NULL);

	return 0;
}

static optdesc_t opt_parallel_mem = {
	"parallel-mem",
	0,
	do_parallelize_mem,
};

void opt_parallelize_mem(ir_graph *irg)
{
	perform_irg_optimization(irg, &opt_parallel_mem);
}

ir_graph_pass_t *opt_parallelize_mem_pass(const char *name)
{
	return def_graph_pass(name ? name : "parallelize-mem", opt_parallelize_mem);
}
