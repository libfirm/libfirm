/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   parallelizing Load/Store optimization
 * @author  Christoph Mallon
 */
#include "array.h"
#include "ircons.h"
#include "irdump.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irmemory.h"
#include "irnode_t.h"
#include "irnodeset.h"
#include "iroptimize.h"
#include "obst.h"
#include "type_t.h"

typedef struct parallelize_info
{
	ir_node      *origin_block;
	ir_node      *origin_ptr;
	ir_type      *origin_type;  /**< Type if ptr destination. */
	unsigned      origin_size;  /**< size of memory access. */
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
				unsigned org_size   = pi->origin_size;
				ir_node *org_ptr    = pi->origin_ptr;
				ir_type *store_type = get_Store_type(pred);
				unsigned store_size = get_mode_size_bytes(get_irn_mode(get_Store_value(pred)));
				ir_node *store_ptr  = get_Store_ptr(pred);
				if (get_alias_relation(org_ptr, org_type, org_size,
				                       store_ptr, store_type, store_size) == ir_no_alias) {
					ir_node *mem = get_Store_mem(pred);
					ir_nodeset_insert(&pi->user_mem, irn);
					parallelize_load(pi, mem);
					return;
				}
			}
		} else if (is_Sync(irn)) {
			for (int i = 0, n = get_Sync_n_preds(irn); i < n; ++i) {
				ir_node *sync_pred = get_Sync_pred(irn, i);
				parallelize_load(pi, sync_pred);
			}
			return;
		} else if (is_CopyB(irn) &&
		           get_CopyB_volatility(irn) == volatility_non_volatile) {
			ir_type *org_type   = pi->origin_type;
			unsigned org_size   = pi->origin_size;
			ir_node *org_ptr    = pi->origin_ptr;
			ir_type *copyB_type = get_CopyB_type(irn);
			ir_node *copyB_dst  = get_CopyB_dst(irn);
			unsigned copyB_size = get_type_size(copyB_type);
			if (get_alias_relation(org_ptr, org_type, org_size,
			                       copyB_dst, copyB_type, copyB_size) == ir_no_alias) {
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
			if (is_Load(pred)
			    && get_Load_volatility(pred) == volatility_non_volatile) {
				ir_node *org_ptr   = pi->origin_ptr;
				ir_type *org_type  = pi->origin_type;
				unsigned org_size  = pi->origin_size;
				ir_node *load_ptr  = get_Load_ptr(pred);
				ir_type *load_type = get_Load_type(pred);
				unsigned load_size = get_mode_size_bytes(get_Load_mode(pred));
				if (get_alias_relation(org_ptr, org_type, org_size,
				                       load_ptr, load_type, load_size) == ir_no_alias) {
					ir_node *mem = get_Load_mem(pred);
					ir_nodeset_insert(&pi->user_mem, irn);
					parallelize_store(pi, mem);
					return;
				}
			} else if (is_Store(pred)
			           && get_Store_volatility(pred) == volatility_non_volatile) {
				ir_node *org_ptr    = pi->origin_ptr;
				ir_type *org_type   = pi->origin_type;
				unsigned org_size   = pi->origin_size;
				ir_node *store_ptr  = get_Store_ptr(pred);
				ir_type *store_type = get_Store_type(pred);
				unsigned store_size = get_mode_size_bytes(get_irn_mode(get_Store_value(pred)));
				if (get_alias_relation(org_ptr, org_type, org_size,
				                       store_ptr, store_type, store_size) == ir_no_alias) {
					ir_node *mem = get_Store_mem(pred);
					ir_nodeset_insert(&pi->user_mem, irn);
					parallelize_store(pi, mem);
					return;
				}
			}
		} else if (is_Sync(irn)) {
			for (int i = 0, n = get_Sync_n_preds(irn); i < n; ++i) {
				ir_node *sync_pred = get_Sync_pred(irn, i);
				parallelize_store(pi, sync_pred);
			}
			return;
		} else if (is_CopyB(irn)
		           && get_CopyB_volatility(irn) == volatility_non_volatile) {
			ir_node *org_ptr    = pi->origin_ptr;
			ir_type *org_type   = pi->origin_type;
			unsigned org_size   = pi->origin_size;
			ir_node *copyB_src  = get_CopyB_src(irn);
			ir_node *copyB_dst  = get_CopyB_dst(irn);
			ir_type *copyB_type = get_CopyB_type(irn);
			unsigned copyB_size = get_type_size(copyB_type);
			if (get_alias_relation(org_ptr, org_type, org_size,
			                       copyB_src, copyB_type, copyB_size) == ir_no_alias &&
			    get_alias_relation(org_ptr, org_type, org_size,
			                       copyB_dst, copyB_type, copyB_size) == ir_no_alias) {
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
			if (is_Load(pred)
			    && get_Load_volatility(pred) == volatility_non_volatile) {
				ir_node *org_ptr   = get_CopyB_dst(origin);
				ir_type *org_type  = pi->origin_type;
				unsigned org_size  = pi->origin_size;
				ir_node *load_ptr  = get_Load_ptr(pred);
				ir_type *load_type = get_Load_type(pred);
				unsigned load_size = get_mode_size_bytes(get_Load_mode(pred));
				if (get_alias_relation(org_ptr, org_type, org_size,
				                       load_ptr, load_type, load_size) == ir_no_alias) {
					ir_node *mem = get_Load_mem(pred);
					ir_nodeset_insert(&pi->user_mem, irn);
					parallelize_copyB(pi, origin, mem);
					return;
				}
			} else if (is_Store(pred)
			           && get_Store_volatility(pred) == volatility_non_volatile) {
				ir_node *org_src    = get_CopyB_src(origin);
				ir_node *org_dst    = get_CopyB_dst(origin);
				ir_type *org_type   = pi->origin_type;
				unsigned org_size   = pi->origin_size;
				ir_node *store_ptr  = get_Store_ptr(pred);
				ir_type *store_type = get_Store_type(pred);
				unsigned store_size = get_mode_size_bytes(get_irn_mode(get_Store_value(pred)));
				if (get_alias_relation(org_src, org_type, org_size,
				                       store_ptr, store_type, store_size) == ir_no_alias &&
				    get_alias_relation(org_dst, org_type, org_size,
				                       store_ptr, store_type, store_size) == ir_no_alias) {
					ir_node *mem = get_Store_mem(pred);
					ir_nodeset_insert(&pi->user_mem, irn);
					parallelize_copyB(pi, origin, mem);
					return;
				}
			}
		} else if (is_Sync(irn)) {
			for (int i = 0, n = get_Sync_n_preds(irn); i < n; ++i) {
				ir_node *sync_pred = get_Sync_pred(irn, i);
				parallelize_copyB(pi, origin, sync_pred);
			}
			return;
		} else if (is_CopyB(irn)
		           && get_CopyB_volatility(irn) == volatility_non_volatile) {
			ir_node *org_src    = get_CopyB_src(origin);
			ir_node *org_dst    = get_CopyB_dst(origin);
			ir_type *org_type   = pi->origin_type;
			unsigned org_size   = pi->origin_size;
			ir_node *copyB_src  = get_CopyB_src(irn);
			ir_node *copyB_dst  = get_CopyB_dst(irn);
			ir_type *copyB_type = get_CopyB_type(irn);
			unsigned copyB_size = get_type_size(copyB_type);
			if (get_alias_relation(org_src, org_type, org_size,
			                       copyB_dst, copyB_type, copyB_size) == ir_no_alias &&
			    get_alias_relation(org_dst, org_type, org_size,
			                       copyB_src, copyB_type, copyB_size) == ir_no_alias &&
			    get_alias_relation(org_dst, org_type, org_size,
			                       copyB_dst, copyB_type, copyB_size) == ir_no_alias) {
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
		pi.origin_size  = get_mode_size_bytes(get_Load_mode(mem_op));
		pi.origin_type  = get_Load_type(mem_op);
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
		pi.origin_size  = get_mode_size_bytes(get_irn_mode(get_Store_value(mem_op)));
		pi.origin_type  = get_Store_type(mem_op);
		ir_nodeset_init(&pi.this_mem);
		ir_nodeset_init(&pi.user_mem);
		ir_nodeset_init(&pi.all_visited);

		parallelize_store(&pi, pred);
	} else if (is_CopyB(mem_op)) {
		if (get_CopyB_volatility(mem_op) != volatility_non_volatile) return;

		block = get_nodes_block(mem_op);
		pred  = get_CopyB_mem(mem_op);

		pi.origin_block = block;
		pi.origin_type  = get_CopyB_type(mem_op);
		pi.origin_size  = get_type_size(pi.origin_type);
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
		ir_node **in = XMALLOCN(ir_node*, n+1);

		size_t i = 0;
		in[i++] = proj;
		foreach_ir_nodeset(&pi.user_mem, node, iter) {
			in[i++] = node;
		}
		assert(i == n + 1);
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
			free(in);
		}
		set_memop_mem(mem_op, sync);
	}

	ir_nodeset_destroy(&pi.this_mem);
	ir_nodeset_destroy(&pi.user_mem);
	ir_nodeset_destroy(&pi.all_visited);
}

/*
 * Elimination of unnecessary Sync edges
 *
 * This code removes Sync inputs, where the immediate predecessor is
 * reachable by another one of the Sync's inputs.
 */

typedef struct {
	irg_walk_func *pre;
	irg_walk_func *post;
	irg_walk_func *again;
	ir_nodeset_t *visited;
	void *inner_env;
} dfs_env_t;

/* WARNING: Stops at Phi nodes for now */
static void dfs_step(ir_node *irn, dfs_env_t *env) {
	if (ir_nodeset_contains(env->visited, irn)) {
		if (env->again) env->again(irn, env->inner_env);
		return;
	} else {
		if (env->pre) env->pre(irn, env->inner_env);
		ir_nodeset_insert(env->visited, irn);
	}

	if (is_Proj(irn)) {
		dfs_step(get_Proj_pred(irn), env);
	} else {
		foreach_irn_in(irn, i, pred) {
			if (get_irn_mode(pred) == mode_M && !is_Phi(pred)) {
				dfs_step(pred, env);
			}
		}
	}

	if (env->post) env->post(irn, env->inner_env);
}

/**
 * Performs a DFS beginning at @c irn, but traversing each edge once
 * rather than visiting each node once.
 *
 * When a node is visited again through another edge, the @c again
 * callback is called.
 *
 * WARNING: Because this particular use of DFS needs it, the search
 * stops at Phi nodes.
 */
static void dfs_by_edges_from(ir_node *irn, irg_walk_func *pre,
                              irg_walk_func *post, irg_walk_func *again,
                              void *env)
{
	ir_nodeset_t visited;
	ir_nodeset_init(&visited);

	dfs_env_t dfs_env = {
		pre,
		post,
		again,
		&visited,
		env
	};

	dfs_step(irn, &dfs_env);

	ir_nodeset_destroy(&visited);
}

typedef struct {
	bool is_Sync_pred;
	int  sync_pred_count;
} sync_pred_info_t;

static void prepare_links_Sync(ir_node *irn, void *e)
{
	struct obstack *obst = (struct obstack*) e;

	sync_pred_info_t *info = obstack_alloc(obst, sizeof(*info));
	info->is_Sync_pred = false;
	info->sync_pred_count = 0;

	set_irn_link(irn, info);
}

static void visit_Sync_pred(ir_node *irn, void *e)
{
	(void)e;

	sync_pred_info_t *info = (sync_pred_info_t*) get_irn_link(irn);

	if (info->is_Sync_pred) {
		info->sync_pred_count++;
	}
}

static void simplify_Sync(ir_node *irn, void *e)
{
	struct obstack *obst = (struct obstack*) e;

	if (!is_Sync(irn))
		return;

	ir_node **preds   = get_Sync_pred_arr(irn);
	int       n_preds = get_Sync_n_preds(irn);

	/* Mark all direct predecessors */
	for (int i = 0; i < n_preds; i++) {
		sync_pred_info_t *info = (sync_pred_info_t*) get_irn_link(preds[i]);
		info->is_Sync_pred = true;
		info->sync_pred_count--;
	}

	/* Do a DFS from the Sync, stopping at Phi nodes */
	dfs_by_edges_from(irn, visit_Sync_pred, NULL, visit_Sync_pred, NULL);

	/* Find unnecessary preds, reset sync_pred_infos. */
	ir_node **necessary = NEW_ARR_F(ir_node*, 0);
	for (int i = 0; i < n_preds; i++) {
		sync_pred_info_t *info = (sync_pred_info_t*) get_irn_link(preds[i]);

		if (info->sync_pred_count <= 0) {
			ARR_APP1(ir_node*, necessary, preds[i]);
		}

		info->is_Sync_pred = false;
		info->sync_pred_count = 0;
	}

	ir_node *block = get_nodes_block(irn);
	ir_node *new_Sync = new_r_Sync(block, ARR_LEN(necessary), necessary);
	prepare_links_Sync(new_Sync, obst);

	if (irn != new_Sync)
		exchange(irn, new_Sync);

	DEL_ARR_F(necessary);
}

static void eliminate_sync_edges(ir_graph *irg)
{
	struct obstack obst;
	obstack_init(&obst);

	irg_walk_graph(irg, prepare_links_Sync, NULL, &obst);
	irg_walk_graph(irg, simplify_Sync, NULL, &obst);

	obstack_free(&obst, NULL);
}

void opt_parallelize_mem(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
	                           | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	irg_walk_blkwise_dom_top_down(irg, NULL, walker, NULL);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	eliminate_sync_edges(irg);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_CONTROL_FLOW);
}
