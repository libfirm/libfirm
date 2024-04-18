/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lowering of builtins to compiler-lib calls
 * @author  Matthias Braun
 */
#include "lower_builtins.h"

#include "adt/pmap.h"
#include "deq.h"
#include "ircons_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "irprog_t.h"
#include "panic.h"
#include "target_t.h"
#include "util.h"
#include <stdbool.h>
#include <stdlib.h>

static bool dont_lower[ir_bk_last + 1];
static lower_func lower_va_arg;

static const char *get_builtin_name(ir_builtin_kind kind)
{
	switch (kind) {
	case ir_bk_ffs:      return "ffs";
	case ir_bk_clz:      return "clz";
	case ir_bk_ctz:      return "ctz";
	case ir_bk_popcount: return "popcount";
	case ir_bk_parity:   return "parity";
	case ir_bk_bswap:    return "bswap";
	case ir_bk_prefetch:
	case ir_bk_trap:
	case ir_bk_debugbreak:
	case ir_bk_return_address:
	case ir_bk_frame_address:
	case ir_bk_inport:
	case ir_bk_outport:
	case ir_bk_saturating_increment:
	case ir_bk_compare_swap:
	case ir_bk_may_alias:
	case ir_bk_va_start:
	case ir_bk_va_arg:
		break;
	}
	abort();
}

static const char *get_gcc_machmode(ir_type *type)
{
	assert(is_Primitive_type(type));
	switch (get_type_size(type)) {
	case 4: return "si";
	case 8: return "di";
	default:
		panic("couldn't determine gcc machmode for type %+F", type);
	}
}

/*
 * The 64-bit version of libgcc does not contain some builtin
 * functions for 32-bit values (__<builtin>si2) anymore.
 */
static void widen_builtin(ir_node *node)
{
	ir_type *mtp  = get_Builtin_type(node);
	ir_type *arg1 = get_method_param_type(mtp, 0);

	// Nothing to do, if argument size is at least machine size.
	if (get_type_size(arg1) >= ir_target_pointer_size())
		return;

	// Only touch builtins with no 32-bit version.
	ir_builtin_kind kind = get_Builtin_kind(node);
	if (kind != ir_bk_clz    &&
	    kind != ir_bk_ctz    &&
	    kind != ir_bk_ffs    &&
	    kind != ir_bk_parity &&
	    kind != ir_bk_popcount) {
		return;
	}

	ir_mode  *target_mode = get_reference_offset_mode(mode_P);
	dbg_info *dbgi        = get_irn_dbg_info(node);
	ir_node  *block       = get_nodes_block(node);
	ir_node  *op          = get_irn_n(node, n_Builtin_max + 1);

	ir_node *conv = new_rd_Conv(dbgi, block, op, target_mode);
	set_irn_n(node, n_Builtin_max + 1, conv);

	ir_type *new_arg1   = get_type_for_mode(target_mode);
	ir_type *new_result = get_method_res_type(mtp, 0);
	ir_type *new_type   = new_type_method(1, 1, false, cc_cdecl_set, mtp_no_property);
	set_method_param_type(new_type, 0, new_arg1);
	set_method_res_type(new_type, 0, new_result);
	set_Builtin_type(node, new_type);
}

static void replace_with_call(ir_node *node)
{
	widen_builtin(node);

	ir_type        *const mtp      = get_Builtin_type(node);
	ir_builtin_kind const kind     = get_Builtin_kind(node);
	char     const *const name     = get_builtin_name(kind);
	ir_type        *const arg1     = get_method_param_type(mtp, 0);
	char     const *const machmode = get_gcc_machmode(arg1);
	ident          *const id       = new_id_fmt("__%s%s2", name, machmode);
	ir_entity      *const entity
		= create_compilerlib_entity(get_id_str(id), mtp);

	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const block     = get_nodes_block(node);
	ir_node  *const mem       = get_Builtin_mem(node);
	ir_graph *const irg       = get_irn_irg(node);
	ir_node  *const callee    = new_r_Address(irg, entity);
	int       const n_params  = get_Builtin_n_params(node);
	ir_node **const params    = get_Builtin_param_arr(node);
	ir_node  *const call      = new_rd_Call(dbgi, block, mem, callee, n_params, params, mtp);
	ir_node  *const call_mem  = new_r_Proj(call, mode_M, pn_Call_M);
	ir_node  *const call_ress = new_r_Proj(call, mode_T, pn_Call_T_result);
	ir_type  *const res_type  = get_method_res_type(mtp, 0);
	ir_mode  *const res_mode  = get_type_mode(res_type);
	ir_node  *const call_res  = new_r_Proj(call_ress, res_mode, 0);

	ir_node *const in[] = {
		[pn_Builtin_M]       = call_mem,
		[pn_Builtin_max + 1] = call_res,
	};
	turn_into_tuple(node, ARRAY_SIZE(in), in);
}

static void replace_may_alias(ir_node *node)
{
	ir_node *in0   = get_Builtin_param(node, 0);
	ir_node *in1   = get_Builtin_param(node, 1);
	ir_type *type  = get_Builtin_type(node);
	ir_type *type0 = get_pointer_points_to_type(get_method_param_type(type, 0));
	ir_type *type1 = get_pointer_points_to_type(get_method_param_type(type, 1));
	ir_type *rtype = get_method_res_type(type, 0);
	ir_mode *rmode = get_type_mode(rtype);

	ir_alias_relation alias = get_alias_relation(in0, type0, 1, in1, type1, 1);

	ir_graph *const irg    = get_irn_irg(node);
	ir_node  *const result = (alias != ir_no_alias ? new_r_Const_one : new_r_Const_null)(irg, rmode);
	ir_node  *const in[]   = {
		[pn_Builtin_M]     = get_Builtin_mem(node),
		[pn_Builtin_max+1] = result,
	};
	turn_into_tuple(node, ARRAY_SIZE(in), in);
}

static void lower_builtin(ir_node *node, void *env)
{
	bool *changed = (bool*)env;
	if (!is_Builtin(node))
		return;

	ir_builtin_kind const kind = get_Builtin_kind(node);
	if (dont_lower[kind])
		return;

	switch (kind) {
	case ir_bk_prefetch: {
		/* just remove it */
		ir_node *mem = get_Builtin_mem(node);
		ir_node *const in[] = { mem };
		turn_into_tuple(node, ARRAY_SIZE(in), in);
		goto changed;
	}

	case ir_bk_ffs:
	case ir_bk_clz:
	case ir_bk_ctz:
	case ir_bk_popcount:
	case ir_bk_parity:
	case ir_bk_bswap:
		/* replace with a call */
		replace_with_call(node);
		goto changed;

	case ir_bk_may_alias:
		replace_may_alias(node);
changed:
		*changed = true;
		return;

	case ir_bk_va_arg:
		lower_va_arg(node);
		return;

	case ir_bk_trap:
	case ir_bk_debugbreak:
	case ir_bk_return_address:
	case ir_bk_frame_address:
	case ir_bk_inport:
	case ir_bk_outport:
	case ir_bk_saturating_increment:
	case ir_bk_compare_swap:
	case ir_bk_va_start:
		/* can't do anything about these, backend will probably fail now */
		panic("builtin kind %s not supported (for this target)",
		      get_builtin_kind_name(kind));
	}
	panic("unexpected builtin %+F", node);
}

static void collect_builtin(ir_node *node, void *env)
{
	deq_t *builtins = env;
	if (!is_Builtin(node)) {
		return;
	}
	ir_node** right_end = deq_alloc_right(builtins, sizeof(ir_node*));
	*right_end = node;
}

static void irg_walk_builtin_nodes_post(ir_graph *irg,
										irg_walk_func *post,
										void *env)
{
	// We'll use this as a queue. Enqueue at right end, dequeue at left
	deq_t builtin_nodes;
	deq_init(&builtin_nodes);
	irg_walk_graph(irg, NULL, collect_builtin, &builtin_nodes);
	while (!deq_empty(&builtin_nodes)) {
		ir_node** left_end = deq_left_end(&builtin_nodes);
		post(*left_end, env);
		deq_shrink_left(&builtin_nodes, sizeof(ir_node*));
	}
	deq_free(&builtin_nodes);
}

void lower_builtins(size_t n_exceptions,
                    ir_builtin_kind const *const exceptions,
                    lower_func new_lower_va_arg)
{
	lower_va_arg = new_lower_va_arg;
	memset(dont_lower, 0, sizeof(dont_lower));
	for (size_t i = 0; i < n_exceptions; ++i) {
		dont_lower[exceptions[i]] = true;
	}

	foreach_irp_irg(i, irg) {
		bool changed = false;
		assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);
		irg_walk_builtin_nodes_post(irg, lower_builtin, &changed);
		confirm_irg_properties(irg, changed ? IR_GRAPH_PROPERTIES_CONTROL_FLOW
		                                    : IR_GRAPH_PROPERTIES_ALL);
	}
}
