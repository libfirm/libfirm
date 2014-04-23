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
#include <stdbool.h>
#include <stdlib.h>
#include "adt/pmap.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "iroptimize.h"
#include "error.h"
#include "irprog_t.h"
#include "util.h"

static pmap *entities;
static bool dont_lower[ir_bk_last+1];

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
	case ir_bk_inner_trampoline:
	case ir_bk_saturating_increment:
	case ir_bk_compare_swap:
	case ir_bk_may_alias:
		break;
	}
	abort();
}

static const char *get_gcc_machmode(ir_type *type)
{
	assert(is_Primitive_type(type));
	switch (get_type_size_bytes(type)) {
	case 4: return "si";
	case 8: return "di";
	default:
		panic("couldn't determine gcc machmode for type %+F", type);
	}
}

static void replace_with_call(ir_node *node)
{
	ir_graph       *irg   = get_irn_irg(node);
	ir_node        *block = get_nodes_block(node);
	ir_builtin_kind kind  = get_Builtin_kind(node);
	const char     *name  = get_builtin_name(kind);
	ir_type        *mtp   = get_Builtin_type(node);
	ir_type        *arg1  = get_method_param_type(mtp, 0);
	dbg_info       *dbgi  = get_irn_dbg_info(node);
	ir_node        *mem   = get_Builtin_mem(node);
	const char     *gcc_machmode = get_gcc_machmode(arg1);
	int             n_params     = get_Builtin_n_params(node);
	ir_node       **params       = get_Builtin_param_arr(node);
	ir_type        *res_type = get_method_res_type(mtp, 0);
	ir_mode        *res_mode = get_type_mode(res_type);
	ir_node        *call_mem;
	ir_node        *call_ress;
	ir_node        *call_res;
	ir_entity      *entity;
	ir_node        *call;
	ident          *id;

	char buf[64];
	snprintf(buf, sizeof(buf), "__%s%s2", name, gcc_machmode);
	id = new_id_from_str(buf);

	entity = pmap_get(ir_entity, entities, id);
	if (entity == NULL) {
		entity = create_compilerlib_entity(id, mtp);
		pmap_insert(entities, id, entity);
	}

	ir_node *const callee = new_r_Address(irg, entity);
	call      = new_rd_Call(dbgi, block, mem, callee, n_params, params, mtp);
	call_mem  = new_r_Proj(call, mode_M, pn_Call_M);
	call_ress = new_r_Proj(call, mode_T, pn_Call_T_result);
	call_res  = new_r_Proj(call_ress, res_mode, 0);

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
	if (is_unknown_type(type0))
		type0 = get_type_for_mode(mode_P);
	if (is_unknown_type(type1))
		type1 = get_type_for_mode(mode_P);
	ir_type *rtype = get_method_res_type(type, 0);
	ir_mode *rmode = get_type_mode(rtype);

	ir_alias_relation alias = get_alias_relation(in0, type0, in1, type1);

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
	ir_builtin_kind kind;
	if (!is_Builtin(node))
		return;

	kind = get_Builtin_kind(node);
	if (dont_lower[kind])
		return;

	switch (kind) {
	case ir_bk_prefetch: {
		/* just remove it */
		ir_node *mem = get_Builtin_mem(node);
		ir_node *const in[] = { mem };
		turn_into_tuple(node, ARRAY_SIZE(in), in);
		*changed = true;
		return;
	}

	case ir_bk_ffs:
	case ir_bk_clz:
	case ir_bk_ctz:
	case ir_bk_popcount:
	case ir_bk_parity:
	case ir_bk_bswap:
		/* replace with a call */
		replace_with_call(node);
		*changed = true;
		return;

	case ir_bk_may_alias:
		replace_may_alias(node);
		*changed = true;
		return;

	case ir_bk_trap:
	case ir_bk_debugbreak:
	case ir_bk_return_address:
	case ir_bk_frame_address:
	case ir_bk_inport:
	case ir_bk_outport:
	case ir_bk_inner_trampoline:
	case ir_bk_saturating_increment:
	case ir_bk_compare_swap:
		/* can't do anything about these, backend will probably fail now */
		panic("Cannot lower Builtin node of kind %+F", node);
	}
	panic("unexpected builtin %+F", node);
}

void lower_builtins(size_t n_exceptions, ir_builtin_kind *exceptions)
{
	memset(dont_lower, 0, sizeof(dont_lower));
	for (size_t i = 0; i < n_exceptions; ++i) {
		dont_lower[exceptions[i]] = true;
	}

	entities = pmap_create();

	foreach_irp_irg(i, irg) {
		bool changed = false;
		irg_walk_graph(irg, NULL, lower_builtin, &changed);
		confirm_irg_properties(irg, changed ? IR_GRAPH_PROPERTIES_CONTROL_FLOW
		                                    : IR_GRAPH_PROPERTIES_ALL);
	}

	pmap_destroy(entities);
}
