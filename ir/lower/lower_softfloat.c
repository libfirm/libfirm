/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lower floating point operations to function calls
 * @author  Sebastian Buchwald
 */
#include <stdbool.h>

#include "be.h"
#include "dbginfo_t.h"
#include "debug.h"
#include "error.h"
#include "ircons.h"
#include "iredges.h"
#include "irgmod.h"
#include "irnodeset.h"
#include "irgwalk.h"
#include "irmode.h"
#include "iropt_dbg.h"
#include "iroptimize.h"
#include "irprog_t.h"
#include "lower_softfloat.h"
#include "lowering.h"
#include "pmap.h"
#include "type_t.h"
#include "tv_t.h"

/** The debug handle */
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef void (*lower_softfloat_func)(ir_node *node);

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
	if (! mode_is_float(mode))
		return mode;

	if (mode == mode_F)
		return mode_Iu;
	else if (mode == mode_D)
		return mode_Lu;

	panic("Unsupported floating point type");
}

/**
 * Adapts the mode of the given node.
 */
static void lower_mode(ir_node *n, void *env)
{
	ir_op                *op         = get_irn_op(n);
	lower_softfloat_func  lower_func = (lower_softfloat_func) op->ops.generic;
	ir_mode              *mode       = get_irn_mode(n);

	(void) env;

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

	(void) env;

	if (lower_func != NULL)
		lower_func(n);
}

/**
 * @return The type of the function replacing the given node.
 */
static ir_type *get_softfloat_type(const ir_node *n)
{
	unsigned opcode       = get_irn_opcode(n);
	ir_mode *mode         = get_irn_mode(n);
	ir_node *operand      = get_irn_n(n, 0);
	ir_mode *operand_mode = get_irn_mode(operand);

	switch (opcode) {
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
	case iro_Conv:
		if (operand_mode == mode_D) {
			if (mode == mode_F)
				return unop_tp_d_f;
			else if (mode == mode_Is || mode == mode_Hs || mode == mode_Bs)
				return unop_tp_d_is;
			else if (mode == mode_Iu || mode == mode_Hu || mode == mode_Bu)
				return unop_tp_d_iu;
			else if (mode == mode_Ls)
				return unop_tp_d_ls;
			else if (mode == mode_Lu)
				return unop_tp_d_lu;
		}
		else if (operand_mode == mode_F) {
			if (mode == mode_D)
				return unop_tp_f_d;
			else if (mode == mode_Is || mode == mode_Hs || mode == mode_Bs)
				return unop_tp_f_is;
			else if (mode == mode_Iu || mode == mode_Hu || mode == mode_Bu)
				return unop_tp_f_iu;
			else if (mode == mode_Ls)
				return unop_tp_f_ls;
			else if (mode == mode_Lu)
				return unop_tp_f_lu;
		}
		else if (operand_mode == mode_Is || operand_mode == mode_Hs || operand_mode == mode_Bs) {
			if (mode == mode_D)
				return unop_tp_is_d;
			else if (mode == mode_F)
				return unop_tp_is_f;
		}
		else if (operand_mode == mode_Iu || operand_mode == mode_Hu || operand_mode == mode_Bu) {
			if (mode == mode_D)
				return unop_tp_iu_d;
			else if (mode == mode_F)
				return unop_tp_iu_f;
		}
		else if (operand_mode == mode_Ls) {
			if (mode == mode_D)
				return unop_tp_ls_d;
			else if (mode == mode_F)
				return unop_tp_ls_f;
		}
		else if (operand_mode == mode_Lu) {
			if (mode == mode_D)
				return unop_tp_lu_d;
			else if (mode == mode_F)
				return unop_tp_lu_f;
		}
		break;
	case iro_Minus:
		if (operand_mode == mode_F)
			return unop_tp_f;
		else if (operand_mode == mode_D)
			return unop_tp_d;
		break;
	default: break;
	}

	assert(0 && "Could not determine a suitable type");

	return NULL;
}

/**
 * @return A SymConst representing the function that replaces the given node.
 */
static ir_node *create_softfloat_symconst(const ir_node *n, const char *name)
{
	char             buf[16];
	const char      *first_param = "";
	const char      *second_param = "";
	const char      *result = "";
	ir_entity       *ent;
	size_t           n_params;
	ident           *id;
	symconst_symbol  sym;
	unsigned         float_types  = 0;
	unsigned         double_types  = 0;
	ir_graph        *irg    = get_irn_irg(n);
	ir_type         *method = get_softfloat_type(n);

	n_params = get_method_n_params(method);

	/* Parameter types. */
	switch (n_params) {
	case 2:
		{
			ir_type *param_type = get_method_param_type(method, 1);
			ir_mode *mode       = get_type_mode(param_type);

			if (mode == mode_F) {
				second_param = "sf";
				float_types++;
			}
			else if (mode == mode_D) {
				second_param = "df";
				double_types++;
			}
			else if (mode == mode_Iu || mode == mode_Is)
				second_param = "si";
			else if (mode == mode_Lu || mode == mode_Ls)
				second_param = "di";
		}
		/* fall through */
	case 1:
		{
			ir_type *param_type = get_method_param_type(method, 0);
			ir_mode *mode       = get_type_mode(param_type);

			if (mode == mode_F) {
				first_param = float_types > 0 ? "" : "sf";
				float_types++;
			}
			else if (mode == mode_D) {
				first_param = double_types > 0 ? "" : "df";
				double_types++;
			}
			else if (mode == mode_Iu || mode == mode_Is)
				first_param = "si";
			else if (mode == mode_Lu || mode == mode_Ls)
				first_param = "di";
		}
		break;
	default:
		break;
	}

	/* Result type. */
	{
		ir_mode *mode;

		if (is_Div(n))
			mode = get_Div_resmode(n);
		else
			mode = get_irn_mode(n);

		if (mode == mode_F) {
			result = float_types > 0 ? "" : "sf";
			float_types++;
		}
		else if (mode == mode_D) {
			result = double_types > 0 ? "" : "df";
			double_types++;
		}
		else if (mode == mode_Iu || mode == mode_Hu || mode == mode_Bu
				|| mode == mode_Is || mode == mode_Hs || mode == mode_Bs)
			result = "si";
		else if (mode == mode_Lu || mode == mode_Ls)
			result = "di";
	}

	assert(float_types <= 3);
	assert(double_types <= 3);

	if (float_types + double_types > 1)
		snprintf(buf, sizeof(buf), "__%s%s%s%s%u", name, first_param, second_param, result, float_types + double_types);
	else
		snprintf(buf, sizeof(buf), "__%s%s%s%s", name, first_param, second_param, result);

	id           = new_id_from_str(buf);
	ent          = create_compilerlib_entity(id, method);
	sym.entity_p = ent;

	return new_r_SymConst(irg, mode_P_code, sym, symconst_addr_ent);
}

/**
 * Transforms an Add into the appropriate soft float function.
 */
static void lower_Add(ir_node *n)
{
	ir_node         *symconst;
	ir_node         *block       = get_nodes_block(n);
	ir_node         *call_result = NULL;
	dbg_info        *dbgi        = get_irn_dbg_info(n);
	ir_graph        *irg         = get_irn_irg(n);
	ir_node         *left        = get_Add_left(n);
	ir_node         *right       = get_Add_right(n);
	ir_mode         *mode        = get_irn_mode(n);

	if (! mode_is_float(mode))
		return;

	symconst = create_softfloat_symconst(n, "add");

	ir_node *call;
	ir_node *call_results;
	ir_node *in[2]    = {left, right};
	ir_node *nomem    = get_irg_no_mem(irg);
	ir_type *type     = get_softfloat_type(n);
	ir_mode *res_mode = get_type_mode(get_method_res_type(type, 0));

	call         = new_rd_Call(dbgi, block, nomem, symconst, 2, in, type);
	call_results = new_r_Proj(call, mode_T, pn_Call_T_result);
	call_result  = new_r_Proj(call_results, res_mode, 0);

	exchange(n, call_result);
}

/**
 * @return The lowered (floating point) type.
 */
static ir_type *lower_type(ir_type *tp)
{
	ir_mode *mode         = get_type_mode(tp);
	ir_mode *lowered_mode = get_lowered_mode(mode);

	return get_type_for_mode(lowered_mode);
}

/**
 * @return The lowered method type.
 */
static ir_type *lower_method_type(ir_type *mtp)
{
	ir_type *res;
	size_t   i;
	size_t   n_param;
	size_t   n_res;

	res = pmap_get(ir_type, lowered_type, mtp);
	if (res != NULL)
		return res;

	n_param = get_method_n_params(mtp);
	n_res   = get_method_n_ress(mtp);

	res = new_type_method(n_param, n_res);

	/* set param types and result types */
	for (i = 0; i < n_param; ++i) {
		ir_type *ptp   = get_method_param_type(mtp, i);
		ir_mode *pmode = get_type_mode(ptp);

		if (pmode != NULL && mode_is_float(pmode)) {
			ptp = lower_type(ptp);
		}

		set_method_param_type(res, i, ptp);
	}
	for (i = 0; i < n_res; ++i) {
		ir_type *rtp   = get_method_res_type(mtp, i);
		ir_mode *rmode = get_type_mode(rtp);

		if (rmode != NULL && mode_is_float(rmode)) {
			rtp = lower_type(rtp);
		}

		set_method_res_type(res, i, rtp);
	}

	set_method_variadicity(res, get_method_variadicity(mtp));
	set_method_calling_convention(res, get_method_calling_convention(mtp));
	set_method_additional_properties(res, get_method_additional_properties(mtp));

	set_higher_type(res, mtp);

	pmap_insert(lowered_type, mtp, res);
	return res;
}

/**
 * Adapts the method type of a Call.
 */
static void lower_Call(ir_node *node)
{
	ir_type  *tp = get_Call_type(node);
	size_t   n_params;
	size_t   n_res;
	bool     need_lower = false;
	size_t   i;
	size_t   p;

	n_params = get_method_n_params(tp);

	for (p = 0; p < n_params; ++p) {
		ir_type *ptp   = get_method_param_type(tp, p);
		ir_mode *pmode = get_type_mode(ptp);

		if (pmode == NULL)
			continue;

		if (mode_is_float(pmode)) {
			need_lower = true;
			break;
		}
	}

	n_res = get_method_n_ress(tp);

	for (i = 0; i < n_res; ++i) {
		ir_type *rtp   = get_method_res_type(tp, i);
		ir_mode *rmode = get_type_mode(rtp);

		if (rmode == NULL)
			continue;

		if (mode_is_float(rmode)) {
			need_lower = true;
			break;
		}
	}

	if (! need_lower)
		return;

	tp = lower_method_type(tp);
	set_Call_type(node, tp);
}

/**
 * Transforms a Cmp into the appropriate soft float function.
 */
static void lower_Cmp(ir_node *n)
{
	ir_node         *symconst    = NULL;
	ir_node         *block       = get_nodes_block(n);
	ir_node         *call_result = NULL;
	dbg_info        *dbgi        = get_irn_dbg_info(n);
	ir_graph        *irg         = get_irn_irg(n);
	ir_node         *left        = get_Cmp_left(n);
	ir_relation      relation    = get_Cmp_relation(n);
	ir_mode         *op_mode     = get_irn_mode(left);
	ir_node         *right       = get_Cmp_right(n);
	ir_node         *symconst2   = NULL;
	ir_node         *zero        = new_rd_Const(dbgi, irg, get_mode_null(mode_Is));

	if (! mode_is_float(op_mode))
		return;

	switch (relation) {
	case ir_relation_false:
		call_result = zero;
		break;
	case ir_relation_equal:
		symconst = create_softfloat_symconst(n, "eq");
		break;
	case ir_relation_less:
		symconst = create_softfloat_symconst(n, "lt");
		break;
	case ir_relation_greater:
		symconst = create_softfloat_symconst(n, "gt");
		break;
	case ir_relation_unordered:
		symconst = create_softfloat_symconst(n, "unord");
		relation = ir_relation_less_greater;
		break;
	case ir_relation_less_equal:
		symconst = create_softfloat_symconst(n, "le");
		break;
	case ir_relation_greater_equal:
		symconst = create_softfloat_symconst(n, "ge");
		break;
	case ir_relation_less_greater:
		symconst  = create_softfloat_symconst(n, "unord");
		symconst2 = create_softfloat_symconst(n, "ne");
		break;
	case ir_relation_less_equal_greater:
		symconst = create_softfloat_symconst(n, "unord");
		relation = ir_relation_equal;
		break;
	case ir_relation_unordered_equal:
		symconst  = create_softfloat_symconst(n, "unord");
		relation  = ir_relation_less_greater;
		symconst2 = create_softfloat_symconst(n, "ne");
		break;
	case ir_relation_unordered_less:
		symconst = create_softfloat_symconst(n, "ge");
		relation = ir_relation_less;
		break;
	case ir_relation_unordered_less_equal:
		symconst = create_softfloat_symconst(n, "gt");
		relation = ir_relation_less_equal;
		break;
	case ir_relation_unordered_greater:
		symconst = create_softfloat_symconst(n, "le");
		relation = ir_relation_greater;
		break;
	case ir_relation_unordered_greater_equal:
		symconst = create_softfloat_symconst(n, "lt");
		relation = ir_relation_greater_equal;
		break;
	case ir_relation_unordered_less_greater:
		symconst = create_softfloat_symconst(n, "eq");
		relation = ir_relation_less_greater;
		break;
	case ir_relation_true:
		call_result = zero;
		break;
	}

	if (call_result == NULL) {
		ir_node *call;
		ir_node *call_results;
		ir_node *in[2] = {left, right};
		ir_node *nomem = get_irg_no_mem(irg);
		ir_type *type  = get_softfloat_type(n);

		call         = new_rd_Call(dbgi, block, nomem, symconst, 2, in, type);
		call_results = new_r_Proj(call, mode_T, pn_Call_T_result);
		call_result  = new_r_Proj(call_results, mode_Is, 0);
	}

	ir_node *cmp = new_r_Cmp(block, call_result, zero, relation);

	/* We need two calls into the softfloat library */
	if (symconst2 != NULL) {
		ir_node *call;
		ir_node *call_results;
		ir_node *mux;
		arch_allow_ifconv_func allow_ifconv = be_get_backend_param()->allow_ifconv;
		ir_node *in[2]                      = {left, right};
		ir_node *nomem                      = get_irg_no_mem(irg);
		ir_type *type                       = get_softfloat_type(n);

		call         = new_rd_Call(dbgi, block, nomem, symconst2, 2, in, type);
		call_results = new_r_Proj(call, mode_T, pn_Call_T_result);
		call_result  = new_r_Proj(call_results, mode_Is, 0);
		relation     = get_Cmp_relation(n);

		mux = new_rd_Mux(dbgi, block, cmp, call_result, zero, mode_Is);

		if (! allow_ifconv(cmp, call_result, zero))
			ir_nodeset_insert(&created_mux_nodes, mux);

		cmp = new_r_Cmp(block, mux, zero, relation);
	}

	exchange(n, cmp);
}

static const tarval_mode_info hex_output = {
	TVO_HEX,
	"0x",
	NULL,
};

/**
 * Adapts floating point constants.
 */
static void lower_Const(ir_node *n)
{
	ir_mode *mode = get_irn_mode(n);
	if (!mode_is_float(mode))
		return;

	ir_mode *lowered_mode = get_lowered_mode(mode);
	set_irn_mode(n, lowered_mode);

	set_tarval_mode_output_option(mode, &hex_output);
	char buf[100];
	tarval_snprintf(buf, sizeof(buf), get_Const_tarval(n));

	size_t     len = strlen(buf);
	ir_tarval *tv  = new_tarval_from_str(buf, len, lowered_mode);
	set_Const_tarval(n, tv);
}

/**
 * Transforms a Conv into the appropriate soft float function.
 */
static void lower_Conv(ir_node *n)
{
	ir_node         *symconst;
	ir_node         *block       = get_nodes_block(n);
	ir_node         *call_result = NULL;
	dbg_info        *dbgi        = get_irn_dbg_info(n);
	ir_graph        *irg         = get_irn_irg(n);
	ir_node         *op          = get_Conv_op(n);
	ir_mode         *mode        = get_irn_mode(n);
	ir_mode         *op_mode     = get_irn_mode(op);

	if (! mode_is_float(mode) && ! mode_is_float(op_mode))
		return;

	/* Remove unnecessary Convs. */
	if (op_mode == mode) {
		exchange(n, op);
		return;
	}
	else if (op_mode == mode_Hs || op_mode == mode_Bs) {
		op_mode = mode_Is;
		op     = new_rd_Conv(dbgi, block, op, op_mode);
	}
	else if (op_mode == mode_Hu || op_mode == mode_Bu) {
		op_mode = mode_Iu;
		op     = new_rd_Conv(dbgi, block, op, op_mode);
	}

	if (mode_is_float(op_mode) && mode_is_float(mode)) {
		if (get_mode_size_bits(op_mode) > get_mode_size_bits(mode))
			symconst = create_softfloat_symconst(n, "trunc");
		else
			symconst = create_softfloat_symconst(n, "extend");
	}
	else if (mode_is_float(op_mode)) {
		if (mode_is_signed(mode))
			symconst = create_softfloat_symconst(n, "fix");
		else
			symconst = create_softfloat_symconst(n, "fixuns");
	}
	else {
		if (mode_is_signed(op_mode))
			symconst = create_softfloat_symconst(n, "float");
		else
			symconst = create_softfloat_symconst(n, "floatun");
	}

	ir_node *call;
	ir_node *call_results;
	ir_node *in[1]    = {op};
	ir_node *nomem    = get_irg_no_mem(irg);
	ir_type *type     = get_softfloat_type(n);
	ir_mode *res_mode = get_type_mode(get_method_res_type(type, 0));

	call         = new_rd_Call(dbgi, block, nomem, symconst, 1, in, type);
	call_results = new_r_Proj(call, mode_T, pn_Call_T_result);
	call_result  = new_r_Proj(call_results, res_mode, 0);

	/* Check whether we need a Conv for the result. */
	if (res_mode != mode)
		call_result = new_rd_Conv(dbgi, block, call_result, mode);

	exchange(n, call_result);
}

/**
 * Transforms a Div into the appropriate soft float function.
 */
static void lower_Div(ir_node *n)
{
	ir_node  *symconst;
	ir_node  *block       = get_nodes_block(n);
	ir_node  *call_result = NULL;
	dbg_info *dbgi        = get_irn_dbg_info(n);
	ir_graph *irg         = get_irn_irg(n);
	ir_node  *left        = get_Div_left(n);
	ir_mode  *mode        = get_Div_resmode(n);
	ir_node  *right       = get_Div_right(n);

	if (! mode_is_float(mode))
		return;

	symconst = create_softfloat_symconst(n, "div");

	ir_node *call;
	ir_node *call_results;
	ir_node *in[2]    = {left, right};
	ir_node *nomem    = get_irg_no_mem(irg);
	ir_type *type     = get_softfloat_type(n);
	ir_mode *res_mode = get_type_mode(get_method_res_type(type, 0));

	call         = new_rd_Call(dbgi, block, nomem, symconst, 2, in, type);
	call_results = new_r_Proj(call, mode_T, pn_Call_T_result);
	call_result  = new_r_Proj(call_results, res_mode, 0);

	set_irn_pinned(call, get_irn_pinned(n));

	foreach_out_edge_safe(n, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (! is_Proj(proj))
			continue;

		switch (get_Proj_proj(proj)) {
		case pn_Div_M:
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_M);
			break;
		case pn_Div_X_regular:
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_X_regular);
			break;
		case pn_Div_X_except:
			set_Proj_pred(proj, call);
			set_Proj_proj(proj, pn_Call_X_except);
			break;
		case pn_Div_res:
			exchange(proj, call_result);
			break;
		default:
			assert(0 && "unexpected Proj number");
		}
	}

}

/**
 * Adapts the resmode of a Div.
 */
static void lower_Div_mode(ir_node *n)
{
	ir_mode *res_mode         = get_Div_resmode(n);
	ir_mode *lowered_res_mode = get_lowered_mode(res_mode);
	ir_mode *mode             = get_irn_mode(n);
	ir_mode *lowered_mode     = get_lowered_mode(mode);

	set_irn_mode(n, lowered_mode);
	set_Div_resmode(n, lowered_res_mode);
}

/**
 * Adapts the ls_mode of a Load.
 */
static void lower_Load(ir_node *n)
{
	ir_mode *ls_mode         = get_Load_mode(n);
	ir_mode *lowered_ls_mode = get_lowered_mode(ls_mode);
	ir_mode *mode            = get_irn_mode(n);
	ir_mode *lowered_mode    = get_lowered_mode(mode);

	set_irn_mode(n, lowered_mode);
	set_Load_mode(n, lowered_ls_mode);
}

/**
 * Transforms a Minus into the appropriate soft float function.
 */
static void lower_Minus(ir_node *n)
{
	ir_node         *symconst;
	ir_node         *block       = get_nodes_block(n);
	ir_node         *call_result = NULL;
	dbg_info        *dbgi        = get_irn_dbg_info(n);
	ir_graph        *irg         = get_irn_irg(n);
	ir_mode         *mode        = get_irn_mode(n);
	ir_node         *op          = get_Minus_op(n);

	if (! mode_is_float(mode))
		return;

	symconst = create_softfloat_symconst(n, "neg");

	ir_node *call;
	ir_node *call_results;
	ir_node *in[1]    = {op};
	ir_node *nomem    = get_irg_no_mem(irg);
	ir_type *type     = get_softfloat_type(n);
	ir_mode *res_mode = get_type_mode(get_method_res_type(type, 0));

	call         = new_rd_Call(dbgi, block, nomem, symconst, 1, in, type);
	call_results = new_r_Proj(call, mode_T, pn_Call_T_result);
	call_result  = new_r_Proj(call_results, res_mode, 0);

	exchange(n, call_result);
}

/**
 * Transforms a Mul into the appropriate soft float function.
 */
static void lower_Mul(ir_node *n)
{
	ir_node         *symconst;
	ir_node         *block       = get_nodes_block(n);
	ir_node         *call_result = NULL;
	dbg_info        *dbgi        = get_irn_dbg_info(n);
	ir_graph        *irg         = get_irn_irg(n);
	ir_node         *left        = get_Mul_left(n);
	ir_mode         *mode        = get_irn_mode(n);
	ir_node         *right       = get_Mul_right(n);

	if (! mode_is_float(mode))
		return;

	symconst = create_softfloat_symconst(n, "mul");

	ir_node *call;
	ir_node *call_results;
	ir_node *in[2]    = {left, right};
	ir_node *nomem    = get_irg_no_mem(irg);
	ir_type *type     = get_softfloat_type(n);
	ir_mode *res_mode = get_type_mode(get_method_res_type(type, 0));

	call         = new_rd_Call(dbgi, block, nomem, symconst, 2, in, type);
	call_results = new_r_Proj(call, mode_T, pn_Call_T_result);
	call_result  = new_r_Proj(call_results, res_mode, 0);

	exchange(n, call_result);
}

/**
 * Transforms a Sub into the appropriate soft float function.
 */
static void lower_Sub(ir_node *n)
{
	ir_node         *symconst;
	ir_node         *block       = get_nodes_block(n);
	ir_node         *call_result = NULL;
	dbg_info        *dbgi        = get_irn_dbg_info(n);
	ir_graph        *irg         = get_irn_irg(n);
	ir_node         *left        = get_Sub_left(n);
	ir_mode         *mode        = get_irn_mode(n);
	ir_node         *right       = get_Sub_right(n);

	if (! mode_is_float(mode))
		return;

	symconst = create_softfloat_symconst(n, "sub");

	ir_node *call;
	ir_node *call_results;
	ir_node *in[2]    = {left, right};
	ir_node *nomem    = get_irg_no_mem(irg);
	ir_type *type     = get_softfloat_type(n);
	ir_mode *res_mode = get_type_mode(get_method_res_type(type, 0));

	call         = new_rd_Call(dbgi, block, nomem, symconst, 2, in, type);
	call_results = new_r_Proj(call, mode_T, pn_Call_T_result);
	call_result  = new_r_Proj(call_results, res_mode, 0);

	exchange(n, call_result);
}

/**
 * Enter a lowering function into an ir_op.
 */
static void ir_register_softloat_lower_function(ir_op *op, lower_softfloat_func func)
{
	op->ops.generic = (op_func)func;
}

/*
 * Initializes softfloat lowering.
 */
static void ir_prepare_softfloat_lowering(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.lower.softfloat");

	if (! lowered_type)
		lowered_type = pmap_create();

	if (! binop_tp_d) {
		binop_tp_d = new_type_method(2, 1);
		set_method_param_type(binop_tp_d, 0, get_type_for_mode(mode_D));
		set_method_param_type(binop_tp_d, 1, get_type_for_mode(mode_D));
		set_method_res_type(binop_tp_d, 0, get_type_for_mode(mode_D));
	}

	if (! binop_tp_f) {
		binop_tp_f = new_type_method(2, 1);
		set_method_param_type(binop_tp_f, 0, get_type_for_mode(mode_F));
		set_method_param_type(binop_tp_f, 1, get_type_for_mode(mode_F));
		set_method_res_type(binop_tp_f, 0, get_type_for_mode(mode_F));
	}

	if (! cmp_tp_d) {
		cmp_tp_d = new_type_method(2, 1);
		set_method_param_type(cmp_tp_d, 0, get_type_for_mode(mode_D));
		set_method_param_type(cmp_tp_d, 1, get_type_for_mode(mode_D));
		set_method_res_type(cmp_tp_d, 0, get_type_for_mode(mode_Is));
	}

	if (! cmp_tp_f) {
		cmp_tp_f = new_type_method(2, 1);
		set_method_param_type(cmp_tp_f, 0, get_type_for_mode(mode_F));
		set_method_param_type(cmp_tp_f, 1, get_type_for_mode(mode_F));
		set_method_res_type(cmp_tp_f, 0, get_type_for_mode(mode_Is));
	}

	if (! unop_tp_d) {
		unop_tp_d = new_type_method(1, 1);
		set_method_param_type(unop_tp_d, 0, get_type_for_mode(mode_D));
		set_method_res_type(unop_tp_d, 0, get_type_for_mode(mode_D));
	}

	if (! unop_tp_f) {
		unop_tp_f = new_type_method(1, 1);
		set_method_param_type(unop_tp_f, 0, get_type_for_mode(mode_F));
		set_method_res_type(unop_tp_f, 0, get_type_for_mode(mode_F));
	}

	if (! unop_tp_d_f) {
		unop_tp_d_f = new_type_method(1, 1);
		set_method_param_type(unop_tp_d_f, 0, get_type_for_mode(mode_D));
		set_method_res_type(unop_tp_d_f, 0, get_type_for_mode(mode_F));
	}

	if (! unop_tp_d_is) {
		unop_tp_d_is = new_type_method(1, 1);
		set_method_param_type(unop_tp_d_is, 0, get_type_for_mode(mode_D));
		set_method_res_type(unop_tp_d_is, 0, get_type_for_mode(mode_Is));
	}

	if (! unop_tp_d_iu) {
		unop_tp_d_iu = new_type_method(1, 1);
		set_method_param_type(unop_tp_d_iu, 0, get_type_for_mode(mode_D));
		set_method_res_type(unop_tp_d_iu, 0, get_type_for_mode(mode_Iu));
	}

	if (! unop_tp_d_ls) {
		unop_tp_d_ls = new_type_method(1, 1);
		set_method_param_type(unop_tp_d_ls, 0, get_type_for_mode(mode_D));
		set_method_res_type(unop_tp_d_ls, 0, get_type_for_mode(mode_Ls));
	}

	if (! unop_tp_d_lu) {
		unop_tp_d_lu = new_type_method(1, 1);
		set_method_param_type(unop_tp_d_lu, 0, get_type_for_mode(mode_D));
		set_method_res_type(unop_tp_d_lu, 0, get_type_for_mode(mode_Lu));
	}

	if (! unop_tp_f_d) {
		unop_tp_f_d = new_type_method(1, 1);
		set_method_param_type(unop_tp_f_d, 0, get_type_for_mode(mode_F));
		set_method_res_type(unop_tp_f_d, 0, get_type_for_mode(mode_D));
	}

	if (! unop_tp_f_is) {
		unop_tp_f_is = new_type_method(1, 1);
		set_method_param_type(unop_tp_f_is, 0, get_type_for_mode(mode_F));
		set_method_res_type(unop_tp_f_is, 0, get_type_for_mode(mode_Is));
	}

	if (! unop_tp_f_iu) {
		unop_tp_f_iu = new_type_method(1, 1);
		set_method_param_type(unop_tp_f_iu, 0, get_type_for_mode(mode_F));
		set_method_res_type(unop_tp_f_iu, 0, get_type_for_mode(mode_Iu));
	}

	if (! unop_tp_f_ls) {
		unop_tp_f_ls = new_type_method(1, 1);
		set_method_param_type(unop_tp_f_ls, 0, get_type_for_mode(mode_F));
		set_method_res_type(unop_tp_f_ls, 0, get_type_for_mode(mode_Ls));
	}

	if (! unop_tp_f_lu) {
		unop_tp_f_lu = new_type_method(1, 1);
		set_method_param_type(unop_tp_f_lu, 0, get_type_for_mode(mode_F));
		set_method_res_type(unop_tp_f_lu, 0, get_type_for_mode(mode_Lu));
	}

	if (! unop_tp_is_d) {
		unop_tp_is_d = new_type_method(1, 1);
		set_method_param_type(unop_tp_is_d, 0, get_type_for_mode(mode_Is));
		set_method_res_type(unop_tp_is_d, 0, get_type_for_mode(mode_D));
	}

	if (! unop_tp_is_f) {
		unop_tp_is_f = new_type_method(1, 1);
		set_method_param_type(unop_tp_is_f, 0, get_type_for_mode(mode_Is));
		set_method_res_type(unop_tp_is_f, 0, get_type_for_mode(mode_F));
	}

	if (! unop_tp_iu_d) {
		unop_tp_iu_d = new_type_method(1, 1);
		set_method_param_type(unop_tp_iu_d, 0, get_type_for_mode(mode_Iu));
		set_method_res_type(unop_tp_iu_d, 0, get_type_for_mode(mode_D));
	}

	if (! unop_tp_iu_f) {
		unop_tp_iu_f = new_type_method(1, 1);
		set_method_param_type(unop_tp_iu_f, 0, get_type_for_mode(mode_Iu));
		set_method_res_type(unop_tp_iu_f, 0, get_type_for_mode(mode_F));
	}

	if (! unop_tp_ls_d) {
		unop_tp_ls_d = new_type_method(1, 1);
		set_method_param_type(unop_tp_ls_d, 0, get_type_for_mode(mode_Ls));
		set_method_res_type(unop_tp_ls_d, 0, get_type_for_mode(mode_D));
	}

	if (! unop_tp_ls_f) {
		unop_tp_ls_f = new_type_method(1, 1);
		set_method_param_type(unop_tp_ls_f, 0, get_type_for_mode(mode_Ls));
		set_method_res_type(unop_tp_ls_f, 0, get_type_for_mode(mode_F));
	}

	if (! unop_tp_lu_d) {
		unop_tp_lu_d = new_type_method(1, 1);
		set_method_param_type(unop_tp_lu_d, 0, get_type_for_mode(mode_Lu));
		set_method_res_type(unop_tp_lu_d, 0, get_type_for_mode(mode_D));
	}

	if (! unop_tp_lu_f) {
		unop_tp_lu_f = new_type_method(1, 1);
		set_method_param_type(unop_tp_lu_f, 0, get_type_for_mode(mode_Lu));
		set_method_res_type(unop_tp_lu_f, 0, get_type_for_mode(mode_F));
	}
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
	size_t i;
	size_t n_irgs = get_irp_n_irgs();

	FIRM_DBG_REGISTER(dbg, "firm.lower.softfloat");

	ir_prepare_softfloat_lowering();

	ir_clear_opcodes_generic_func();
	ir_register_softloat_lower_function(op_Add,   lower_Add);
	ir_register_softloat_lower_function(op_Cmp,   lower_Cmp);
	ir_register_softloat_lower_function(op_Conv,  lower_Conv);
	ir_register_softloat_lower_function(op_Div,   lower_Div);
	ir_register_softloat_lower_function(op_Minus, lower_Minus);
	ir_register_softloat_lower_function(op_Mul,   lower_Mul);
	ir_register_softloat_lower_function(op_Sub,   lower_Sub);

	for (i = 0; i < n_irgs; ++i) {
		ir_graph *irg = get_irp_irg(i);

		ir_nodeset_init(&created_mux_nodes);

		assure_edges(irg);

		irg_walk_graph(irg, NULL, lower_node, NULL);

		if (ir_nodeset_size(&created_mux_nodes) > 0)
			lower_mux(irg, lower_mux_cb);

		ir_nodeset_destroy(&created_mux_nodes);
	}

	ir_clear_opcodes_generic_func();
	ir_register_softloat_lower_function(op_Call,  lower_Call);
	ir_register_softloat_lower_function(op_Const, lower_Const);
	ir_register_softloat_lower_function(op_Div,   lower_Div_mode);
	ir_register_softloat_lower_function(op_Load,  lower_Load);

	for (i = 0; i < n_irgs; ++i) {
		ir_graph *irg          = get_irp_irg(i);
		ir_entity *ent         = get_irg_entity(irg);
		ir_type   *mtp         = get_entity_type(ent);
		ir_type   *lowered_mtp = lower_method_type(mtp);
		ir_type   *frame_tp    = get_irg_frame_type(irg);
		size_t     n_members;
		size_t     i;

		if (lowered_mtp != mtp)
			set_entity_type(ent, lowered_mtp);

		irg_walk_graph(irg, NULL, lower_mode, NULL);

		/* fixup parameter entities */
		n_members = get_compound_n_members(frame_tp);
		for (i = 0; i < n_members; ++i) {
			ir_entity *member = get_compound_member(frame_tp, i);
			ir_type   *type   = get_entity_type(member);
			if (is_Primitive_type(type)) {
				ir_type *lowered = lower_type(type);
				set_entity_type(member, lowered);
			}
		}
	}
}
