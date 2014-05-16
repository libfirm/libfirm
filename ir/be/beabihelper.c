/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Helper functions for handling ABI constraints in the code
 *              selection phase.
 * @author      Matthias Braun
 */

#include "../../adt/util.h"
#include "beabihelper.h"
#include "bearch.h"
#include "beirg.h"
#include "benode.h"
#include "besched.h"
#include "ircons.h"
#include "iredges.h"
#include "irgwalk.h"
#include "irnodemap.h"
#include "irtools.h"
#include "heights.h"

/**
 * Tests whether a node has a real user and is not just kept by the End or
 * Anchor node
 */
static bool has_real_user(const ir_node *node)
{
	foreach_out_edge(node, edge) {
		ir_node *user = get_edge_src_irn(edge);
		if (!is_End(user) && !is_Anchor(user))
			return true;
	}
	return false;
}

static ir_node *add_to_keep(ir_node *last_keep,
                            const arch_register_class_t *cls, ir_node *node)
{
	if (last_keep != NULL) {
		be_Keep_add_node(last_keep, cls, node);
	} else {
		ir_node *in[1] = { node };
		ir_node *block = get_nodes_block(node);
		ir_node *schedpoint;
		last_keep = be_new_Keep(block, 1, in);

		schedpoint = skip_Proj(node);
		if (sched_is_scheduled(schedpoint)) {
			sched_add_after(schedpoint, last_keep);
		}
	}
	return last_keep;
}

void be_add_missing_keeps_node(ir_node *node)
{
	int       n_outs, i;
	ir_mode  *mode = get_irn_mode(node);
	ir_node  *last_keep;

	if (mode != mode_T) {
		if (!has_real_user(node)) {
			const arch_register_req_t   *req = arch_get_irn_register_req(node);
			const arch_register_class_t *cls = req->cls;
			if (cls == NULL
					|| (cls->flags & arch_register_class_flag_manual_ra)) {
				return;
			}

			add_to_keep(NULL, cls, node);
		}
		return;
	}

	n_outs = arch_get_irn_n_outs(node);
	if (n_outs <= 0)
		return;

	unsigned *const found_projs    = rbitset_alloca(n_outs);
	ir_node **const existing_projs = ALLOCANZ(ir_node*, n_outs);
	foreach_out_edge(node, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		ir_mode *mode = get_irn_mode(succ);
		int      pn;

		/* The node could be kept */
		if (is_End(succ) || is_Anchor(succ))
			continue;
		if (mode == mode_M || mode == mode_X)
			continue;
		pn                 = get_Proj_proj(succ);
		existing_projs[pn] = succ;
		if (!has_real_user(succ))
			continue;

		assert(pn < n_outs);
		rbitset_set(found_projs, pn);
	}

	/* are keeps missing? */
	last_keep = NULL;
	for (i = 0; i < n_outs; ++i) {
		ir_node                     *value;
		const arch_register_req_t   *req;
		const arch_register_class_t *cls;

		if (rbitset_is_set(found_projs, i)) {
			continue;
		}

		req = arch_get_irn_register_req_out(node, i);
		cls = req->cls;
		if (cls == NULL || (cls->flags & arch_register_class_flag_manual_ra)) {
			continue;
		}

		value = existing_projs[i];
		if (value == NULL)
			value = new_r_Proj(node, arch_register_class_mode(cls), i);
		last_keep = add_to_keep(last_keep, cls, value);
	}
}

static void add_missing_keep_walker(ir_node *node, void *data)
{
	(void)data;
	be_add_missing_keeps_node(node);
}

void be_add_missing_keeps(ir_graph *irg)
{
	irg_walk_graph(irg, add_missing_keep_walker, NULL, NULL);
}


/**
 * Link the node into its block list as a new head.
 */
static void collect_node(ir_node *node)
{
	ir_node *block = get_nodes_block(node);
	ir_node *old   = (ir_node*)get_irn_link(block);

	set_irn_link(node, old);
	set_irn_link(block, node);
}

/**
 * Post-walker: link all nodes that probably access the stack into lists of their block.
 */
static void link_ops_in_block_walker(ir_node *node, void *data)
{
	(void) data;

	switch (get_irn_opcode(node)) {
	case iro_Return:
	case iro_Call:
		collect_node(node);
		break;
	case iro_Alloc:
		/** all non-stack alloc nodes should be lowered before the backend */
		collect_node(node);
		break;
	case iro_Free:
		collect_node(node);
		break;
	case iro_Builtin:
		if (get_Builtin_kind(node) == ir_bk_return_address) {
			ir_node   *param = get_Builtin_param(node, 0);
			ir_tarval *tv    = get_Const_tarval(param); /* must be Const */
			long       value = get_tarval_long(tv);
			if (value > 0) {
				/* not the return address of the current function:
				 * we need the stack pointer for the frame climbing */
				collect_node(node);
			}
		}
		break;
	default:
		break;
	}
}

static ir_heights_t *heights;

/**
 * Check if a node is somehow data dependent on another one.
 * both nodes must be in the same basic block.
 * @param n1 The first node.
 * @param n2 The second node.
 * @return 1, if n1 is data dependent (transitively) on n2, 0 if not.
 */
static int dependent_on(const ir_node *n1, const ir_node *n2)
{
	assert(get_nodes_block(n1) == get_nodes_block(n2));

	return heights_reachable_in_block(heights, n1, n2);
}

/**
 * Classical qsort() comparison function behavior:
 *
 * 0  if both elements are equal, no node depend on the other
 * +1 if first depends on second (first is greater)
 * -1 if second depends on first (second is greater)
*/
static int cmp_call_dependency(const void *c1, const void *c2)
{
	const ir_node *n1 = *(const ir_node **) c1;
	const ir_node *n2 = *(const ir_node **) c2;
	unsigned h1, h2;

	if (dependent_on(n1, n2))
		return 1;

	if (dependent_on(n2, n1))
		return -1;

	/* The nodes have no depth order, but we need a total order because qsort()
	 * is not stable.
	 *
	 * Additionally, we need to respect transitive dependencies. Consider a
	 * Call a depending on Call b and an independent Call c.
	 * We MUST NOT order c > a and b > c. */
	h1 = get_irn_height(heights, n1);
	h2 = get_irn_height(heights, n2);
	if (h1 < h2) return  1;
	if (h1 > h2) return -1;
	/* Same height, so use a random (but stable) order */
	return get_irn_idx(n2) - get_irn_idx(n1);
}

/**
 * Block-walker: sorts dependencies and remember them into a phase
 */
static void process_ops_in_block(ir_node *block, void *data)
{
	ir_nodemap *map = (ir_nodemap*)data;
	unsigned    n;
	unsigned    n_nodes;
	ir_node    *node;
	ir_node   **nodes;

	n_nodes = 0;
	for (node = (ir_node*)get_irn_link(block); node != NULL;
	     node = (ir_node*)get_irn_link(node)) {
		++n_nodes;
	}

	if (n_nodes == 0)
		return;

	nodes = XMALLOCN(ir_node*, n_nodes);
	n = 0;
	for (node = (ir_node*)get_irn_link(block); node != NULL;
	     node = (ir_node*)get_irn_link(node)) {
		nodes[n++] = node;
	}
	assert(n == n_nodes);

	/* order nodes according to their data dependencies */
	QSORT(nodes, n_nodes, cmp_call_dependency);

	/* remember the calculated dependency into a phase */
	for (n = n_nodes-1; n > 0; --n) {
		ir_node *node = nodes[n];
		ir_node *pred = nodes[n-1];

		ir_nodemap_insert(map, node, pred);
	}
	free(nodes);
}



struct be_stackorder_t {
	ir_nodemap stack_order; /**< a phase to handle stack dependencies. */
};

be_stackorder_t *be_collect_stacknodes(ir_graph *irg)
{
	be_stackorder_t *env = XMALLOCZ(be_stackorder_t);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	/* collect all potential^stack accessing nodes */
	irg_walk_graph(irg, firm_clear_link, link_ops_in_block_walker, NULL);

	ir_nodemap_init(&env->stack_order, irg);

	/* use heights to create a total order for those nodes: this order is stored
	 * in the created phase */
	heights = heights_new(irg);
	irg_block_walk_graph(irg, NULL, process_ops_in_block, &env->stack_order);
	heights_free(heights);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	return env;
}

ir_node *be_get_stack_pred(const be_stackorder_t *env, const ir_node *node)
{
	return ir_nodemap_get(ir_node, &env->stack_order, node);
}

void be_free_stackorder(be_stackorder_t *env)
{
	ir_nodemap_destroy(&env->stack_order);
	free(env);
}

static void create_stores_for_type(ir_graph *irg, ir_type *type)
{
	size_t   n           = get_compound_n_members(type);
	ir_node *frame       = get_irg_frame(irg);
	ir_node *initial_mem = get_irg_initial_mem(irg);
	ir_node *mem         = initial_mem;
	ir_node *first_store = NULL;
	ir_node *start_block = get_irg_start_block(irg);
	ir_node *args        = get_irg_args(irg);
	size_t   i;

	/* all parameter entities left in the frame type require stores.
	 * (The ones passed on the stack have been moved to the arg type) */
	for (i = 0; i < n; ++i) {
		ir_entity *entity = get_compound_member(type, i);
		ir_node   *addr;
		size_t     arg;
		if (!is_parameter_entity(entity))
			continue;

		arg = get_entity_parameter_number(entity);
		if (arg == IR_VA_START_PARAMETER_NUMBER)
			continue;

		addr = new_r_Member(start_block, frame, entity);
		if (entity->attr.parameter.doubleword_low_mode != NULL) {
			ir_mode *mode      = entity->attr.parameter.doubleword_low_mode;
			ir_node *val0      = new_r_Proj(args, mode, arg);
			ir_node *val1      = new_r_Proj(args, mode, arg+1);
			ir_node *store0    = new_r_Store(start_block, mem, addr, val0,
			                                 cons_none);
			ir_node *mem0      = new_r_Proj(store0, mode_M, pn_Store_M);
			size_t   offset    = get_mode_size_bits(mode)/8;
			ir_mode *mode_ref  = get_irn_mode(addr);
			ir_mode *mode_offs = get_reference_mode_unsigned_eq(mode_ref);
			ir_node *cnst      = new_r_Const_long(irg, mode_offs, offset);
			ir_node *next_addr = new_r_Add(start_block, addr, cnst, mode_ref);
			ir_node *store1    = new_r_Store(start_block, mem0, next_addr, val1,
			                                 cons_none);
			mem = new_r_Proj(store1, mode_M, pn_Store_M);
			if (first_store == NULL)
				first_store = store0;
		} else {
			ir_type *tp    = get_entity_type(entity);
			ir_mode *mode  = is_compound_type(tp) ? mode_P : get_type_mode(tp);
			ir_node *val   = new_r_Proj(args, mode, arg);
			ir_node *store = new_r_Store(start_block, mem, addr, val, cons_none);
			mem = new_r_Proj(store, mode_M, pn_Store_M);
			if (first_store == NULL)
				first_store = store;
		}
	}

	if (mem != initial_mem) {
		edges_reroute_except(initial_mem, mem, first_store);
		set_irg_initial_mem(irg, initial_mem);
	}
}

void be_add_parameter_entity_stores(ir_graph *irg)
{
	ir_type           *frame_type   = get_irg_frame_type(irg);
	be_stack_layout_t *layout       = be_get_irg_stack_layout(irg);
	ir_type           *between_type = layout->between_type;

	create_stores_for_type(irg, frame_type);
	if (between_type != NULL) {
		create_stores_for_type(irg, between_type);
	}
}
