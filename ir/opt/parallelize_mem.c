/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   parallelizing Load/Store optimization
 * @author  Christoph Mallon
 */
#include "iroptimize.h"

#include "array_t.h"
#include "debug.h"
#include "ircons.h"
#include "irgraph_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irmemory.h"
#include "irnode.h"
#include "irnodeset.h"
#include "obst.h"
#include "irdump.h"
#include "irflag_t.h"
#include "iredges.h"
#include "type_t.h"

typedef struct parallelize_info
{
	ir_node      *origin_block;
	ir_node      *origin_ptr;
	ir_type      *origin_type;
	ir_nodeset_t  this_mem;
	ir_nodeset_t  user_mem;
	ir_nodeset_t  all_visited;
} parallelize_info;

static void parallelize_load(parallelize_info *pi, ir_node *irn)
{
	/* There is no point in investigating the same subgraph twice */
	if (ir_nodeset_contains(&pi->all_visited, irn))
		return;

	ir_nodeset_insert(&pi->all_visited, irn);

	if (get_nodes_block(irn) == pi->origin_block) {
		if (is_Proj(irn)) {
			ir_node *pred = get_Proj_pred(irn);
			if (is_Load(pred) &&
					get_Load_volatility(pred) == volatility_non_volatile) {
				ir_node *mem = get_Load_mem(pred);
				ir_nodeset_insert(&pi->user_mem, irn);
				parallelize_load(pi, mem);
				return;
			} else if (is_Store(pred) &&
					get_Store_volatility(pred) == volatility_non_volatile) {
				ir_type *org_type   = pi->origin_type;
				ir_node *org_ptr    = pi->origin_ptr;
				ir_type *store_type = get_type_for_mode(get_irn_mode(get_Store_value(pred)));
				ir_node *store_ptr  = get_Store_ptr(pred);
				if (get_alias_relation(org_ptr, org_type, store_ptr, store_type) == ir_no_alias) {
					ir_node *mem = get_Store_mem(pred);
					ir_nodeset_insert(&pi->user_mem, irn);
					parallelize_load(pi, mem);
					return;
				}
			}
		} else if (is_Sync(irn)) {
			int n = get_Sync_n_preds(irn);

			for (int i = 0; i < n; ++i) {
				ir_node *sync_pred = get_Sync_pred(irn, i);
				parallelize_load(pi, sync_pred);
			}
			return;
		} else if (is_CopyB(irn) &&
				get_CopyB_volatility(irn) == volatility_non_volatile) {
			ir_type *org_type   = pi->origin_type;
			ir_node *org_ptr    = pi->origin_ptr;
			ir_type *copyB_type = get_CopyB_type(irn);
			ir_node *copyB_dst  = get_CopyB_dst(irn);
			if (get_alias_relation(org_ptr, org_type, copyB_dst, copyB_type) == ir_no_alias) {
				ir_node *mem = get_CopyB_mem(irn);
				ir_nodeset_insert(&pi->user_mem, irn);
				parallelize_load(pi, mem);
				return;
			}
		}
	}
	ir_nodeset_insert(&pi->this_mem, irn);
}

static void parallelize_store(parallelize_info *pi, ir_node *irn)
{
	/* There is no point in investigating the same subgraph twice */
	if (ir_nodeset_contains(&pi->all_visited, irn))
		return;

	ir_nodeset_insert(&pi->all_visited, irn);

	if (get_nodes_block(irn) == pi->origin_block) {
		if (is_Proj(irn)) {
			ir_node *pred = get_Proj_pred(irn);
			if (is_Load(pred) &&
					get_Load_volatility(pred) == volatility_non_volatile) {
				ir_type *org_type  = pi->origin_type;
				ir_node *org_ptr   = pi->origin_ptr;
				ir_type *load_type = get_type_for_mode(get_Load_mode(pred));
				ir_node *load_ptr  = get_Load_ptr(pred);
				if (get_alias_relation(org_ptr, org_type, load_ptr, load_type) == ir_no_alias) {
					ir_node *mem = get_Load_mem(pred);
					ir_nodeset_insert(&pi->user_mem, irn);
					parallelize_store(pi, mem);
					return;
				}
			} else if (is_Store(pred) &&
					get_Store_volatility(pred) == volatility_non_volatile) {
				ir_type *org_type   = pi->origin_type;
				ir_node *org_ptr    = pi->origin_ptr;
				ir_type *store_type = get_type_for_mode(get_irn_mode(get_Store_value(pred)));
				ir_node *store_ptr  = get_Store_ptr(pred);
				if (get_alias_relation(org_ptr, org_type, store_ptr, store_type) == ir_no_alias) {
					ir_node *mem = get_Store_mem(pred);
					ir_nodeset_insert(&pi->user_mem, irn);
					parallelize_store(pi, mem);
					return;
				}
			}
		} else if (is_Sync(irn)) {
			int n = get_Sync_n_preds(irn);

			for (int i = 0; i < n; ++i) {
				ir_node *sync_pred = get_Sync_pred(irn, i);
				parallelize_store(pi, sync_pred);
			}
			return;
		} else if (is_CopyB(irn) &&
				get_CopyB_volatility(irn) == volatility_non_volatile) {
			ir_type *org_type   = pi->origin_type;
			ir_node *org_ptr    = pi->origin_ptr;
			ir_type *copyB_type = get_CopyB_type(irn);
			ir_node *copyB_src  = get_CopyB_src(irn);
			ir_node *copyB_dst  = get_CopyB_dst(irn);
			if (get_alias_relation(org_ptr, org_type, copyB_src, copyB_type) == ir_no_alias &&
			    get_alias_relation(org_ptr, org_type, copyB_dst, copyB_type) == ir_no_alias) {
				ir_node *mem = get_CopyB_mem(irn);
				ir_nodeset_insert(&pi->user_mem, irn);
				parallelize_store(pi, mem);
				return;
			}
		}
	}
	ir_nodeset_insert(&pi->this_mem, irn);
}

static void parallelize_copyB(parallelize_info *pi, ir_node *origin, ir_node *irn)
{
	/* There is no point in investigating the same subgraph twice */
	if (ir_nodeset_contains(&pi->all_visited, irn))
		return;

	ir_nodeset_insert(&pi->all_visited, irn);

	if (get_nodes_block(irn) == pi->origin_block) {
		if (is_Proj(irn)) {
			ir_node *pred = get_Proj_pred(irn);
			if (is_Load(pred) &&
					get_Load_volatility(pred) == volatility_non_volatile) {
				ir_type *org_type  = pi->origin_type;
				ir_node *org_ptr   = get_CopyB_dst(origin);
				ir_type *load_type = get_type_for_mode(get_Load_mode(pred));
				ir_node *load_ptr  = get_Load_ptr(pred);
				if (get_alias_relation(org_ptr, org_type, load_ptr, load_type) == ir_no_alias) {
					ir_node *mem = get_Load_mem(pred);
					ir_nodeset_insert(&pi->user_mem, irn);
					parallelize_copyB(pi, origin, mem);
					return;
				}
			} else if (is_Store(pred) &&
					get_Store_volatility(pred) == volatility_non_volatile) {
				ir_type *org_type   = pi->origin_type;
				ir_node *org_src    = get_CopyB_src(origin);
				ir_node *org_dst    = get_CopyB_dst(origin);
				ir_type *store_type = get_type_for_mode(get_irn_mode(get_Store_value(pred)));
				ir_node *store_ptr  = get_Store_ptr(pred);
				if (get_alias_relation(org_src, org_type, store_ptr, store_type) == ir_no_alias &&
				    get_alias_relation(org_dst, org_type, store_ptr, store_type) == ir_no_alias) {
					ir_node *mem = get_Store_mem(pred);
					ir_nodeset_insert(&pi->user_mem, irn);
					parallelize_copyB(pi, origin, mem);
					return;
				}
			}
		} else if (is_Sync(irn)) {
			int n = get_Sync_n_preds(irn);

			for (int i = 0; i < n; ++i) {
				ir_node *sync_pred = get_Sync_pred(irn, i);
				parallelize_copyB(pi, origin, sync_pred);
			}
			return;
		} else if (is_CopyB(irn) &&
				get_Store_volatility(irn) == volatility_non_volatile) {
			ir_type *org_type   = pi->origin_type;
			ir_node *org_src    = get_CopyB_src(origin);
			ir_node *org_dst    = get_CopyB_dst(origin);
			ir_type *copyB_type = get_CopyB_type(irn);
			ir_node *copyB_src  = get_CopyB_src(irn);
			ir_node *copyB_dst  = get_CopyB_dst(irn);
			if (get_alias_relation(org_src, org_type, copyB_dst, copyB_type) == ir_no_alias &&
			    get_alias_relation(org_dst, org_type, copyB_src, copyB_type) == ir_no_alias &&
			    get_alias_relation(org_dst, org_type, copyB_dst, copyB_type) == ir_no_alias) {
				ir_node *mem = get_CopyB_mem(irn);
				ir_nodeset_insert(&pi->user_mem, irn);
				parallelize_copyB(pi, origin, mem);
				return;
			}
		}
	}
	ir_nodeset_insert(&pi->this_mem, irn);
}

static void walker(ir_node *proj, void *env)
{
	(void)env;

	ir_node *mem_op;

	if (is_Proj(proj) && get_irn_mode(proj) == mode_M) {
		mem_op = get_Proj_pred(proj);
	} else if (get_irn_mode(proj) == mode_M) {
		mem_op = proj;
	} else {
		return;
	}

	ir_node          *pred;
	ir_node          *block;
	parallelize_info  pi;

	if (is_Load(mem_op)) {
		if (get_Load_volatility(mem_op) != volatility_non_volatile) return;

		block = get_nodes_block(mem_op);
		pred  = get_Load_mem(mem_op);

		pi.origin_block = block,
		pi.origin_ptr   = get_Load_ptr(mem_op);
		pi.origin_type  = get_type_for_mode(get_Load_mode(mem_op));
		ir_nodeset_init(&pi.this_mem);
		ir_nodeset_init(&pi.user_mem);
		ir_nodeset_init(&pi.all_visited);

		parallelize_load(&pi, pred);
	} else if (is_Store(mem_op)) {
		if (get_Store_volatility(mem_op) != volatility_non_volatile) return;

		block = get_nodes_block(mem_op);
		pred  = get_Store_mem(mem_op);

		pi.origin_block = block,
		pi.origin_ptr   = get_Store_ptr(mem_op);
		pi.origin_type  = get_type_for_mode(get_irn_mode(get_Store_value(mem_op)));
		ir_nodeset_init(&pi.this_mem);
		ir_nodeset_init(&pi.user_mem);
		ir_nodeset_init(&pi.all_visited);

		parallelize_store(&pi, pred);
	} else if (is_CopyB(mem_op)) {
		if (get_CopyB_volatility(mem_op) != volatility_non_volatile) return;

		block = get_nodes_block(mem_op);
		pred  = get_CopyB_mem(mem_op);

		pi.origin_block = block;
		pi.origin_type = get_CopyB_type(mem_op);
		/* parallelize_copyB uses the node itself, because the
		 * information does not fit in a parallelize_info. */
		ir_nodeset_init(&pi.this_mem);
		ir_nodeset_init(&pi.user_mem);
		ir_nodeset_init(&pi.all_visited);

		parallelize_copyB(&pi, mem_op, pred);
	} else {
		return;
	}

	size_t n = ir_nodeset_size(&pi.user_mem);
	if (n > 0) { /* nothing happened otherwise */
		ir_node **in   = XMALLOCN(ir_node*, n+1);

		size_t i = 0;
		in[i++] = proj;
		foreach_ir_nodeset(&pi.user_mem, node, iter) {
			in[i++] = node;
		}
		assert(i == n+1);
		ir_node *sync = new_r_Sync(block, i, in);
		free(in);
		edges_reroute_except(proj, sync, sync);

		n = ir_nodeset_size(&pi.this_mem);
		if (n == 1) {
			sync = ir_nodeset_first(&pi.this_mem);
		} else {
			in = XMALLOCN(ir_node*, n);
			i = 0;
			foreach_ir_nodeset(&pi.this_mem, node, iter) {
				in[i++] = node;
			}
			assert(i == n);
			sync = new_r_Sync(block, i, in);
		}
		set_memop_mem(mem_op, sync);
	}

	ir_nodeset_destroy(&pi.this_mem);
	ir_nodeset_destroy(&pi.user_mem);
	ir_nodeset_destroy(&pi.all_visited);
}

void opt_parallelize_mem(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);
	irg_walk_graph(irg, NULL, walker, NULL);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_CONTROL_FLOW);
}
