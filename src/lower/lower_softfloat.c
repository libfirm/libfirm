/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lower floating point operations to function calls
 * @author  Sebastian Buchwald
 */
#include "lower_softfloat.h"

#include "dbginfo_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irmode_t.h"
#include "irnodeset.h"
#include "irop_t.h"
#include "iropt_dbg.h"
#include "iroptimize.h"
#include "irprog_t.h"
#include "lowering.h"
#include "panic.h"
#include "pmap.h"
#include "target_t.h"
#include "tv_t.h"
#include "type_t.h"
#include <stdbool.h>

typedef bool (*lower_softfloat_func)(ir_node *node);

static ir_type *binop_tp_d;
static ir_type *binop_tp_f;
static ir_type *cmp_tp_d;
static ir_type *cmp_tp_f;
static ir_type *unop_tp_d;
static ir_type *unop_tp_f;
static ir_type *unop_tp_d_f;
static ir_type *unop_tp_d_is;
static ir_type *unop_tp_d_iu;
static ir_type *unop_tp_d_ls;
static ir_type *unop_tp_d_lu;
static ir_type *unop_tp_f_d;
static ir_type *unop_tp_f_is;
static ir_type *unop_tp_f_iu;
static ir_type *unop_tp_f_ls;
static ir_type *unop_tp_f_lu;
static ir_type *unop_tp_is_d;
static ir_type *unop_tp_is_f;
static ir_type *unop_tp_iu_d;
static ir_type *unop_tp_iu_f;
static ir_type *unop_tp_ls_d;
static ir_type *unop_tp_ls_f;
static ir_type *unop_tp_lu_d;
static ir_type *unop_tp_lu_f;

/** A map from a method type to its lowered type. */
static pmap *lowered_type;

static ir_nodeset_t created_mux_nodes;

/**
 * @return The lowered (floating point) mode.
 */
static ir_mode *get_lowered_mode(ir_mode *mode)
{
	if (!mode_is_float(mode))
		return mode;

	if (mode == mode_F)
		return mode_Iu;
	else if (mode == mode_D)
		return mode_Lu;

	panic("unsupported floating point type");
}

/**
 * Adapts the mode of the given node.
 */
static void lower_mode(ir_node *n, void *env)
{
	(void)env;
	ir_op                *op         = get_irn_op(n);
	lower_softfloat_func  lower_func = (lower_softfloat_func) op->ops.generic;
	ir_mode              *mode       = get_irn_mode(n);
	if (lower_func != NULL) {
		lower_func(n);
		return;
	}

	set_irn_mode(n, get_lowered_mode(mode));
}

/**
 * Wrapper for specific lower function.
 */
static void lower_node(ir_node *n, void *env)
{
	ir_op                *op         = get_irn_op(n);
	lower_softfloat_func  lower_func = (lower_softfloat_func) op->ops.generic;
	if (lower_func != NULL) {
		bool *changed = (bool*)env;
		*changed |= lower_func(n);
	}
}

/**
 * @return The type of the function replacing the given node.
 */
static ir_type *get_softfloat_type(const ir_node *n)
{
	ir_node *operand      = get_irn_n(n, 0);
	ir_mode *operand_mode = get_irn_mode(operand);

	switch (get_irn_opcode(n)) {
	case iro_Div:
		operand_mode = get_irn_mode(get_Div_left(n));
		/* fall through */
	case iro_Add:
	case iro_Mul:
	case iro_Sub:
		if (operand_mode == mode_F)
			return binop_tp_f;
		else if (operand_mode == mode_D)
			return binop_tp_d;
		break;
	case iro_Cmp:
		if (operand_mode == mode_F)
			return cmp_tp_f;
		else if (operand_mode == mode_D)
			return cmp_tp_d;
		break;

	case iro_Conv: {
		ir_mode *const mode = get_irn_mode(n);
		if (operand_mode == mode_D) {
			if (mode == mode_F)
				return unop_tp_d_f;
			else if (get_mode_arithmetic(mode) == irma_twos_complement) {
				if (get_mode_size_bits(mode) <= 32)
					return mode_is_signed(mode) ? unop_tp_d_is : unop_tp_d_iu;
				else if (get_mode_size_bits(mode) == 64)
					return mode_is_signed(mode) ? unop_tp_d_ls : unop_tp_d_lu;
			}
		} else if (operand_mode == mode_F) {
			if (mode == mode_D)
				return unop_tp_f_d;
			else if (get_mode_arithmetic(mode) == irma_twos_complement) {
				if (get_mode_size_bits(mode) <= 32)
					return mode_is_signed(mode) ? unop_tp_f_is : unop_tp_f_iu;
				else if (get_mode_size_bits(mode) == 64)
					return mode_is_signed(mode) ? unop_tp_f_ls : unop_tp_f_lu;
			}
		} else if (get_mode_arithmetic(operand_mode) == irma_twos_complement) {
			if (mode_is_signed(operand_mode)) {
				if (get_mode_size_bits(operand_mode) <= 32) {
					if (mode == mode_D)
						return unop_tp_is_d;
					else if (mode == mode_F)
						return unop_tp_is_f;
				} else if (get_mode_size_bits(operand_mode) == 64) {
					if (mode == mode_D)
						return unop_tp_ls_d;
					else if (mode == mode_F)
						return unop_tp_ls_f;
				}
			} else {
				if (get_mode_size_bits(operand_mode) <= 32) {
					if (mode == mode_D)
						return unop_tp_iu_d;
					else if (mode == mode_F)
						return unop_tp_iu_f;
				} else if (get_mode_size_bits(operand_mode) == 64) {
					if (mode == mode_D)
						return unop_tp_lu_d;
					else if (mode == mode_F)
						return unop_tp_lu_f;
				}
			}
		}
		break;
	}

	case iro_Minus:
		if (operand_mode == mode_F)
			return unop_tp_f;
		else if (operand_mode == mode_D)
			return unop_tp_d;
		break;
	default: break;
	}

	panic("could not determine a suitable type");
}

/**
 * @return An Address representing the function that replaces the given node.
 */
static ir_node *create_softfloat_address(const ir_node *n, const char *name)
{
	ir_type *const method = get_softfloat_type(n);

	/* Parameter types. */
	char const *first_param  = "";
	char const *second_param = "";
	unsigned    float_types  = 0;
	unsigned    double_types = 0;
	switch (get_method_n_params(method)) {
	case 2: {
		ir_type *const param_type = get_method_param_type(method, 1);
		ir_mode *const mode       = get_type_mode(param_type);
		if (mode == mode_F) {
			second_param = "sf";
			float_types++;
		} else if (mode == mode_D) {
			second_param = "df";
			double_types++;
		} else if (mode == mode_Iu || mode == mode_Is) {
			second_param = "si";
		} else if (mode == mode_Lu || mode == mode_Ls) {
			second_param = "di";
		}
	}
		/* FALLTHROUGH */
	case 1: {
		ir_type *const param_type = get_method_param_type(method, 0);
		ir_mode *const mode       = get_type_mode(param_type);
		if (mode == mode_F) {
			first_param = float_types > 0 ? "" : "sf";
			float_types++;
		} else if (mode == mode_D) {
			first_param = double_types > 0 ? "" : "df";
			double_types++;
		} else if (mode == mode_Iu || mode == mode_Is) {
			first_param = "si";
		} else if (mode == mode_Lu || mode == mode_Ls) {
			first_param = "di";
		}
		break;
	}

	default:
		break;
	}

	/* Result type. */
	char     const *result = "";
	ir_mode *const  mode   = is_Div(n) ? get_Div_resmode(n) : get_irn_mode(n);
	if (mode == mode_F) {
		result = float_types > 0 ? "" : "sf";
		float_types++;
	} else if (mode == mode_D) {
		result = double_types > 0 ? "" : "df";
		double_types++;
	} else if (mode == mode_Iu || mode == mode_Hu || mode == mode_Bu ||
	           mode == mode_Is || mode == mode_Hs || mode == mode_Bs)
		result = "si";
	else if (mode == mode_Lu || mode == mode_Ls)
		result = "di";

	assert(float_types <= 3);
	assert(double_types <= 3);

	ident *const id = float_types + double_types > 1 ?
		new_id_fmt("__%s%s%s%s%u", name, first_param, second_param, result, float_types + double_types) :
		new_id_fmt("__%s%s%s%s",   name, first_param, second_param, result);

	ir_graph  *const irg = get_irn_irg(n);
	ir_entity *const ent = create_compilerlib_entity(get_id_str(id), method);
	return new_r_Address(irg, ent);
}

static ir_node *make_softfloat_call(ir_node *const n, char const *const name,
                                    size_t const arity,
                                    ir_node *const *const in)
{
	dbg_info *const dbgi     = get_irn_dbg_info(n);
	ir_node  *const block    = get_nodes_block(n);
	ir_graph *const irg      = get_irn_irg(n);
	ir_node  *const nomem    = get_irg_no_mem(irg);
	ir_node  *const callee   = create_softfloat_address(n, name);
	ir_type  *const type     = get_softfloat_type(n);
	ir_mode  *const res_mode = get_type_mode(get_method_res_type(type, 0));
	ir_node  *const call     = new_rd_Call(dbgi, block, nomem, callee, arity,
	                                       in, type);
	ir_node  *const results  = new_r_Proj(call, mode_T, pn_Call_T_result);
	ir_node  *const result   = new_r_Proj(results, res_mode, 0);
	return result;
}

/**
 * Transforms an Add into the appropriate soft float function.
 */
static bool lower_Add(ir_node *const n)
{
	ir_mode *const mode = get_irn_mode(n);
	if (!mode_is_float(mode))
		return false;

	ir_node *const left   = get_Add_left(n);
	ir_node *const right  = get_Add_right(n);
	ir_node *const in[]   = { left, right };
	ir_node *const result = make_softfloat_call(n, "add", ARRAY_SIZE(in), in);
	exchange(n, result);
	return true;
}

/**
 * @return The lowered (floating point) type.
 */
static ir_type *lower_type(ir_type *const tp)
{
	if (!is_Primitive_type(tp))
		return tp;
	ir_mode *mode         = get_type_mode(tp);
	ir_mode *lowered_mode = get_lowered_mode(mode);
	return get_type_for_mode(lowered_mode);
}

/**
 * @return The lowered method type.
 */
static ir_type *lower_method_type(ir_type *mtp)
{
	ir_type *res = pmap_get(ir_type, lowered_type, mtp);
	if (res != NULL)
		return res;

	size_t                    const n_param     = get_method_n_params(mtp);
	size_t                    const n_res       = get_method_n_ress(mtp);
	bool                      const is_variadic = is_method_variadic(mtp);
	unsigned                  const cc_mask     = get_method_calling_convention(mtp);
	mtp_additional_properties const props       = get_method_additional_properties(mtp);
	res = new_type_method(n_param, n_res, is_variadic, cc_mask, props);

	/* set param types and result types */
	for (size_t i = 0; i < n_param; ++i) {
		ir_type *const ptp = get_method_param_type(mtp, i);
		set_method_param_type(res, i, lower_type(ptp));
	}
	for (size_t i = 0; i < n_res; ++i) {
		ir_type *const rtp = get_method_res_type(mtp, i);
		set_method_res_type(res, i, lower_type(rtp));
	}

	pmap_insert(lowered_type, mtp, res);
	return res;
}

static ir_type *lower_type_if_needed(ir_type *tp)
{
	bool need_lower = false;

	size_t const n_params = get_method_n_params(tp);
	for (size_t p = 0; p < n_params; ++p) {
		ir_type *ptp   = get_method_param_type(tp, p);
		ir_mode *pmode = get_type_mode(ptp);
		if (pmode && mode_is_float(pmode)) {
			need_lower = true;
			break;
		}
	}

	size_t const n_res = get_method_n_ress(tp);
	for (size_t i = 0; i < n_res; ++i) {
		ir_type *rtp   = get_method_res_type(tp, i);
		ir_mode *rmode = get_type_mode(rtp);
		if (rmode && mode_is_float(rmode)) {
			need_lower = true;
			break;
		}
	}

	if (need_lower) {
		return lower_method_type(tp);
	} else {
		return tp;
	}
}

/**
 * Adapts the method type of a Call.
 */
static bool lower_Call(ir_node *node)
{
	ir_type *tp       = get_Call_type(node);
	ir_type *lower_tp = lower_type_if_needed(tp);
	if (lower_tp != tp) {
		set_Call_type(node, lower_tp);
	}
	return true;
}

/**
 * Adapts the method type of a va_arg Builtin.
 */
static bool lower_Builtin(ir_node *node)
{
	if (get_Builtin_kind(node) != ir_bk_va_arg)
		return false;

	ir_type *tp       = get_Builtin_type(node);
	ir_type *lower_tp = lower_type_if_needed(tp);
	if (lower_tp != tp) {
		set_Builtin_type(node, lower_tp);
	}
	return true;
}

/**
 * Transforms a Cmp into the appropriate soft float function.
 */
static bool lower_Cmp(ir_node *const n)
{
	ir_node *const left    = get_Cmp_left(n);
	ir_mode *const op_mode = get_irn_mode(left);
	if (!mode_is_float(op_mode))
		return false;

	dbg_info *const dbgi = get_irn_dbg_info(n);
	ir_graph *const irg  = get_irn_irg(n);
	ir_node  *const zero = new_rd_Const_null(dbgi, irg, mode_Is);

	char const  *name     = NULL;
	char const  *name2    = NULL;
	ir_node     *result   = NULL;
	ir_relation  relation = get_Cmp_relation(n);
	switch (relation) {
	case ir_relation_false:
		result = zero;
		break;
	case ir_relation_equal:
		name = "eq";
		break;
	case ir_relation_less:
		name = "lt";
		break;
	case ir_relation_greater:
		name = "gt";
		break;
	case ir_relation_unordered:
		name     = "unord";
		relation = ir_relation_less_greater;
		break;
	case ir_relation_less_equal:
		name = "le";
		break;
	case ir_relation_greater_equal:
		name = "ge";
		break;
	case ir_relation_less_greater:
		name  = "unord";
		name2 = "ne";
		break;
	case ir_relation_less_equal_greater:
		name     = "unord";
		relation = ir_relation_equal;
		break;
	case ir_relation_unordered_equal:
		name     = "unord";
		relation = ir_relation_less_greater;
		name2    = "ne";
		break;
	case ir_relation_unordered_less:
		name     = "ge";
		relation = ir_relation_less;
		break;
	case ir_relation_unordered_less_equal:
		name     = "gt";
		relation = ir_relation_less_equal;
		break;
	case ir_relation_unordered_greater:
		name     = "le";
		relation = ir_relation_greater;
		break;
	case ir_relation_unordered_greater_equal:
		name     = "lt";
		relation = ir_relation_greater_equal;
		break;
	case ir_relation_unordered_less_greater:
		name     = "eq";
		relation = ir_relation_less_greater;
		break;
	case ir_relation_true:
		result = zero;
		break;
	}

	ir_node *const block = get_nodes_block(n);
	ir_node *const right = get_Cmp_right(n);

	if (result == NULL) {
		ir_node *const in[] = { left, right };
		result = make_softfloat_call(n, name, ARRAY_SIZE(in), in);
	}

	ir_node *cmp = new_r_Cmp(block, result, zero, relation);

	/* We need two calls into the softfloat library */
	if (name2 != NULL) {
		ir_node *const in[] = { left, right };
		result   = make_softfloat_call(n, name2, ARRAY_SIZE(in), in);
		relation = get_Cmp_relation(n);

		ir_node *const mux = new_rd_Mux(dbgi, block, cmp, result, zero);

		arch_allow_ifconv_func const allow_ifconv = ir_target.allow_ifconv;
		if (!allow_ifconv(cmp, result, zero))
			ir_nodeset_insert(&created_mux_nodes, mux);

		cmp = new_r_Cmp(block, mux, zero, relation);
	}

	exchange(n, cmp);
	return true;
}

/**
 * Adapts floating point constants.
 */
static bool lower_Const(ir_node *const n)
{
	ir_mode *mode = get_irn_mode(n);
	if (!mode_is_float(mode))
		return false;

	ir_tarval *float_tv     = get_Const_tarval(n);
	ir_mode   *lowered_mode = get_lowered_mode(mode);
	ir_tarval *int_tv       = tarval_bitcast(float_tv, lowered_mode);

	set_irn_mode(n, lowered_mode);
	set_Const_tarval(n, int_tv);
	return true;
}

/**
 * Transforms a Conv into the appropriate soft float function.
 */
static bool lower_Conv(ir_node *const n)
{
	dbg_info *const dbgi    = get_irn_dbg_info(n);
	ir_node  *const block   = get_nodes_block(n);
	ir_mode  *const mode    = get_irn_mode(n);
	ir_node        *op      = get_Conv_op(n);
	ir_mode        *op_mode = get_irn_mode(op);

	char const *name;
	if (!mode_is_float(mode)) {
		if (!mode_is_float(op_mode))
			return false;
		if (mode_is_signed(mode))
			name = "fix";
		else
			name = "fixuns";
	} else if (!mode_is_float(op_mode)) {
		ir_mode *min_mode;
		if (mode_is_signed(op_mode)) {
			name     = "float";
			min_mode = mode_Is;
		} else {
			name     = "floatun";
			min_mode = mode_Iu;
		}
		if (get_mode_size_bits(op_mode) < get_mode_size_bits(min_mode)) {
			op_mode = min_mode;
			op      = new_rd_Conv(dbgi, block, op, op_mode);
		}
	} else {
		/* Remove unnecessary Convs. */
		if (op_mode == mode) {
			exchange(n, op);
			return true;
		}
		if (get_mode_size_bits(op_mode) > get_mode_size_bits(mode))
			name = "trunc";
		else
			name = "extend";
	}

	ir_node *const in[]   = { op };
	ir_node       *result = make_softfloat_call(n, name, ARRAY_SIZE(in), in);

	/* Check whether we need a Conv for the result. */
	if (get_irn_mode(result) != mode)
		result = new_rd_Conv(dbgi, block, result, mode);

	exchange(n, result);
	return true;
}

static bool lower_Bitcast(ir_node *const n)
{
	/* bitcast was casting float->int or int->float we can simply replace it
	 * with a conv (between integer modes) now. */
	ir_node *op       = get_Bitcast_op(n);
	ir_mode *src_mode = get_irn_mode(op);
	ir_mode *dst_mode = get_irn_mode(n);
	/* note that the predecessor may already be transformed, so it's
	 * possible that we don't see a float mode anymore. */
	dst_mode = get_lowered_mode(dst_mode);
	ir_node *res = op;
	if (src_mode != dst_mode) {
		dbg_info *dbgi  = get_irn_dbg_info(n);
		ir_node  *block = get_nodes_block(n);
		res = new_rd_Conv(dbgi, block, op, dst_mode);
	}
	exchange(n, res);
	return true;
}

/**
 * Transforms a Div into the appropriate soft float function.
 */
static bool lower_Div(ir_node *const n)
{
	ir_mode *const mode = get_Div_resmode(n);
	if (!mode_is_float(mode))
		return false;

	ir_node *const left   = get_Div_left(n);
	ir_node *const right  = get_Div_right(n);
	ir_node *const in[]   = { left, right };
	ir_node *const result = make_softfloat_call(n, "div", ARRAY_SIZE(in), in);
	ir_node *const call   = skip_Proj(skip_Proj(result));
	set_irn_pinned(call, get_irn_pinned(n));

	foreach_out_edge_safe(n, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;

		switch ((pn_Div)get_Proj_num(proj)) {
		case pn_Div_M:
			set_Proj_pred(proj, call);
			set_Proj_num(proj, pn_Call_M);
			continue;
		case pn_Div_X_regular:
			set_Proj_pred(proj, call);
			set_Proj_num(proj, pn_Call_X_regular);
			continue;
		case pn_Div_X_except:
			set_Proj_pred(proj, call);
			set_Proj_num(proj, pn_Call_X_except);
			continue;
		case pn_Div_res:
			exchange(proj, result);
			continue;
		}
		panic("unexpected Proj number");
	}
	return true;
}

/**
 * Adapts the resmode of a Div.
 */
static bool lower_Div_mode(ir_node *n)
{
	ir_mode *res_mode         = get_Div_resmode(n);
	ir_mode *lowered_res_mode = get_lowered_mode(res_mode);
	ir_mode *mode             = get_irn_mode(n);
	ir_mode *lowered_mode     = get_lowered_mode(mode);
	if (lowered_mode == mode && lowered_res_mode == res_mode)
		return false;

	set_irn_mode(n, lowered_mode);
	set_Div_resmode(n, lowered_res_mode);
	return true;
}

/**
 * Adapts the ls_mode of a Load.
 */
static bool lower_Load(ir_node *n)
{
	ir_mode *ls_mode         = get_Load_mode(n);
	ir_mode *lowered_ls_mode = get_lowered_mode(ls_mode);
	ir_mode *mode            = get_irn_mode(n);
	ir_mode *lowered_mode    = get_lowered_mode(mode);
	if (ls_mode == lowered_ls_mode && mode == lowered_mode)
		return false;

	set_irn_mode(n, lowered_mode);
	set_Load_mode(n, lowered_ls_mode);
	return true;
}

/**
 * Transforms a Minus into the appropriate soft float function.
 */
static bool lower_Minus(ir_node *n)
{
	ir_mode *const mode = get_irn_mode(n);
	if (!mode_is_float(mode))
		return false;

	ir_node *const op     = get_Minus_op(n);
	ir_node *const in[]   = { op };
	ir_node *const result = make_softfloat_call(n, "neg", ARRAY_SIZE(in), in);
	exchange(n, result);
	return true;
}

/**
 * Transforms a Mul into the appropriate soft float function.
 */
static bool lower_Mul(ir_node *n)
{
	ir_mode *const mode = get_irn_mode(n);
	if (!mode_is_float(mode))
		return false;

	ir_node *const left   = get_Mul_left(n);
	ir_node *const right  = get_Mul_right(n);
	ir_node *const in[]   = { left, right };
	ir_node *const result = make_softfloat_call(n, "mul", ARRAY_SIZE(in), in);
	exchange(n, result);
	return true;
}

/**
 * Transforms a Sub into the appropriate soft float function.
 */
static bool lower_Sub(ir_node *n)
{
	ir_mode *const mode = get_irn_mode(n);
	if (!mode_is_float(mode))
		return false;

	ir_node *const left   = get_Sub_left(n);
	ir_node *const right  = get_Sub_right(n);
	ir_node *const in[]   = { left, right };
	ir_node *const result = make_softfloat_call(n, "sub", ARRAY_SIZE(in), in);
	exchange(n, result);
	return true;
}

/**
 * Enter a lowering function into an ir_op.
 */
static void ir_register_softloat_lower_function(ir_op *op,
                                                lower_softfloat_func func)
{
	op->ops.generic = (op_func)func;
}

static void make_binop_type(ir_type **const memoized, ir_type *const left,
                            ir_type *const right, ir_type *const res)
{
	if (!*memoized) {
		ir_type *const type = *memoized = new_type_method(2, 1, false, cc_cdecl_set, mtp_no_property);
		set_method_param_type(type, 0, left);
		set_method_param_type(type, 1, right);
		set_method_res_type(  type, 0, res);
	}
}

static void make_unop_type(ir_type **const memoized, ir_type *const op,
                           ir_type *const res)
{
	if (!*memoized) {
		ir_type *const type = *memoized = new_type_method(1, 1, false, cc_cdecl_set, mtp_no_property);
		set_method_param_type(type, 0, op);
		set_method_res_type(  type, 0, res);
	}
}

/*
 * Initializes softfloat lowering.
 */
static void ir_prepare_softfloat_lowering(void)
{
	if (!lowered_type)
		lowered_type = pmap_create();

	ir_type *const type_D  = get_type_for_mode(mode_D);
	ir_type *const type_F  = get_type_for_mode(mode_F);
	ir_type *const type_Is = get_type_for_mode(mode_Is);
	ir_type *const type_Iu = get_type_for_mode(mode_Iu);
	ir_type *const type_Ls = get_type_for_mode(mode_Ls);
	ir_type *const type_Lu = get_type_for_mode(mode_Lu);

	make_binop_type(&binop_tp_d, type_D, type_D, type_D);
	make_binop_type(&binop_tp_f, type_F, type_F, type_F);
	make_binop_type(&cmp_tp_d,   type_D, type_D, type_Is);
	make_binop_type(&cmp_tp_f,   type_F, type_F, type_Is);

	make_unop_type(&unop_tp_d,    type_D,  type_D);
	make_unop_type(&unop_tp_f,    type_F,  type_F);
	make_unop_type(&unop_tp_d_f,  type_D,  type_F);
	make_unop_type(&unop_tp_d_is, type_D,  type_Is);
	make_unop_type(&unop_tp_d_iu, type_D,  type_Iu);
	make_unop_type(&unop_tp_d_ls, type_D,  type_Ls);
	make_unop_type(&unop_tp_d_lu, type_D,  type_Lu);
	make_unop_type(&unop_tp_f_d,  type_F,  type_D);
	make_unop_type(&unop_tp_f_is, type_F,  type_Is);
	make_unop_type(&unop_tp_f_iu, type_F,  type_Iu);
	make_unop_type(&unop_tp_f_ls, type_F,  type_Ls);
	make_unop_type(&unop_tp_f_lu, type_F,  type_Lu);
	make_unop_type(&unop_tp_is_d, type_Is, type_D);
	make_unop_type(&unop_tp_is_f, type_Is, type_F);
	make_unop_type(&unop_tp_iu_d, type_Iu, type_D);
	make_unop_type(&unop_tp_iu_f, type_Iu, type_F);
	make_unop_type(&unop_tp_ls_d, type_Ls, type_D);
	make_unop_type(&unop_tp_ls_f, type_Ls, type_F);
	make_unop_type(&unop_tp_lu_d, type_Lu, type_D);
	make_unop_type(&unop_tp_lu_f, type_Lu, type_F);
}

/**
 * Callback to lower only the Mux nodes we created.
 */
static int lower_mux_cb(ir_node *mux)
{
	return ir_nodeset_contains(&created_mux_nodes, mux);
}

void lower_floating_point(void)
{
	ir_prepare_softfloat_lowering();

	ir_clear_opcodes_generic_func();
	ir_register_softloat_lower_function(op_Add,   lower_Add);
	ir_register_softloat_lower_function(op_Cmp,   lower_Cmp);
	ir_register_softloat_lower_function(op_Conv,  lower_Conv);
	ir_register_softloat_lower_function(op_Div,   lower_Div);
	ir_register_softloat_lower_function(op_Minus, lower_Minus);
	ir_register_softloat_lower_function(op_Mul,   lower_Mul);
	ir_register_softloat_lower_function(op_Sub,   lower_Sub);

	bool *const changed_irgs = XMALLOCNZ(bool, get_irp_n_irgs());
	foreach_irp_irg(i, irg) {
		ir_nodeset_init(&created_mux_nodes);

		assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

		irg_walk_graph(irg, NULL, lower_node, &changed_irgs[i]);

		if (ir_nodeset_size(&created_mux_nodes) > 0)
			lower_mux(irg, lower_mux_cb);

		ir_nodeset_destroy(&created_mux_nodes);
	}

	ir_clear_opcodes_generic_func();
	ir_register_softloat_lower_function(op_Bitcast, lower_Bitcast);
	ir_register_softloat_lower_function(op_Call,    lower_Call);
	ir_register_softloat_lower_function(op_Builtin, lower_Builtin);
	ir_register_softloat_lower_function(op_Const,   lower_Const);
	ir_register_softloat_lower_function(op_Div,     lower_Div_mode);
	ir_register_softloat_lower_function(op_Load,    lower_Load);

	foreach_irp_irg(i, irg) {
		ir_entity *const ent         = get_irg_entity(irg);
		ir_type   *const mtp         = get_entity_type(ent);
		ir_type   *const lowered_mtp = lower_method_type(mtp);
		if (lowered_mtp != mtp)
			set_entity_type(ent, lowered_mtp);

		irg_walk_graph(irg, NULL, lower_mode, &changed_irgs[i]);

		/* fixup parameter entities */
		ir_type *const frame_tp  = get_irg_frame_type(irg);
		size_t   const n_members = get_compound_n_members(frame_tp);
		for (size_t j = 0; j < n_members; ++j) {
			ir_entity *const member  = get_compound_member(frame_tp, j);
			ir_type   *const type    = get_entity_type(member);
			ir_type   *const lowered = lower_type(type);
			set_entity_type(member, lowered);
		}

		confirm_irg_properties(irg, changed_irgs[i] ? IR_GRAPH_PROPERTIES_NONE
		                                            : IR_GRAPH_PROPERTIES_ALL);
	}
	free(changed_irgs);
}
