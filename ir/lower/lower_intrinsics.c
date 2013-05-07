/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   lowering of Calls of intrinsic functions
 * @author  Michael Beck
 */
#include <stdbool.h>

#include "lowering.h"
#include "irop_t.h"
#include "irprog_t.h"
#include "irnode_t.h"
#include "irprog_t.h"
#include "irgwalk.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "trouts.h"
#include "irverify.h"
#include "pmap.h"
#include "array_t.h"
#include "irpass_t.h"
#include "iropt_dbg.h"
#include "error.h"
#include "be.h"
#include "util.h"
#include "firmstat_t.h"

/** Walker environment. */
typedef struct walker_env {
	pmap     *c_map;              /**< The intrinsic call map. */
	size_t   nr_of_intrinsics;    /**< statistics */
	i_instr_record **i_map;       /**< The intrinsic instruction map. */
} walker_env_t;

/**
 * walker: call all mapper functions
 */
static void call_mapper(ir_node *node, void *env)
{
	walker_env_t *wenv = (walker_env_t*)env;
	ir_op *op = get_irn_op(node);

	if (op == op_Call) {
		ir_node *symconst;
		const i_call_record *r;
		ir_entity *ent;

		symconst = get_Call_ptr(node);
		if (! is_SymConst_addr_ent(symconst))
			return;

		ent = get_SymConst_entity(symconst);
		r   = pmap_get(i_call_record const, wenv->c_map, ent);

		if (r != NULL) {
			wenv->nr_of_intrinsics += r->i_mapper(node, r->ctx) ? 1 : 0;
		}
	} else {
		if (op->code < (unsigned) ARR_LEN(wenv->i_map)) {
			const i_instr_record *r = wenv->i_map[op->code];
			/* run all possible mapper */
			while (r) {
				if (r->i_mapper(node, r->ctx)) {
					++wenv->nr_of_intrinsics;
					break;
				}
				r = (const i_instr_record*)r->link;
			}
		}
	}
}

size_t lower_intrinsics(i_record *list, size_t length, int part_block_used)
{
	size_t         i, n;
	size_t         n_ops = ir_get_n_opcodes();
	ir_graph       *irg;
	pmap           *c_map = pmap_create_ex(length);
	i_instr_record **i_map;
	size_t         nr_of_intrinsics = 0;
	walker_env_t   wenv;

	/* we use the ir_op generic pointers here */
	NEW_ARR_A(i_instr_record *, i_map, n_ops);
	memset((void *)i_map, 0, sizeof(*i_map) * n_ops);

	/* fill a map for faster search */
	for (i = 0; i < length; ++i) {
		if (list[i].i_call.kind == INTRINSIC_CALL) {
			pmap_insert(c_map, list[i].i_call.i_ent, (void *)&list[i].i_call);
		} else {
			ir_op *op = list[i].i_instr.op;
			assert(op->code < (unsigned) ARR_LEN(i_map));

			list[i].i_instr.link = i_map[op->code];
			i_map[op->code] = &list[i].i_instr;
		}
	}

	wenv.c_map = c_map;
	wenv.i_map = i_map;

	for (i = 0, n = get_irp_n_irgs(); i < n; ++i) {
		irg = get_irp_irg(i);

		if (part_block_used) {
			ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);
			collect_phiprojs(irg);
		}

		wenv.nr_of_intrinsics = 0;
		irg_walk_graph(irg, NULL, call_mapper, &wenv);

		if (part_block_used)
			ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

		if (wenv.nr_of_intrinsics > 0) {
			confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
			nr_of_intrinsics += wenv.nr_of_intrinsics;
		}
	}
	pmap_destroy(c_map);

	return nr_of_intrinsics;
}

typedef struct pass_t {
	ir_prog_pass_t pass;

	int      part_block_used;
	size_t   length;
	i_record list[1];
} pass_t;

/**
 * Wrapper for running lower_intrinsics() as an ir_prog pass.
 */
static int pass_wrapper(ir_prog *irp, void *context)
{
	pass_t *pass = (pass_t*)context;
	(void) irp; /* TODO: set current irp, or remove parameter */
	lower_intrinsics(pass->list, pass->length, pass->part_block_used);
	/* probably this pass should not run again */
	return 0;
}

/**
 * Creates an ir_prog pass for lower_intrinsics.
 *
 * @param name             the name of this pass or NULL
 * @param list             an array of intrinsic map records
 * @param length           the length of the array
 * @param part_block_used  set to true if part_block() must be using during lowering
 */
ir_prog_pass_t *lower_intrinsics_pass(
	const char *name,
	i_record *list, size_t length, int part_block_used)
{
	pass_t *const pass = XMALLOCF(pass_t, list, length);
	memcpy(pass->list, list, sizeof(list[0]) * length);
	pass->length          = length;
	pass->part_block_used = part_block_used;

	return def_prog_pass_constructor(
		&pass->pass, name ? name : "lower_intrinsics", pass_wrapper);
}

/**
 * Helper function, replace the call by the given node.
 *
 * @param irn      the result node
 * @param call     the call to replace
 * @param mem      the new mem result
 * @param reg_jmp  new regular control flow, if NULL, a Jmp will be used
 * @param exc_jmp  new exception control flow, if reg_jmp == NULL, a Bad will be used
 */
static void replace_call(ir_node *irn, ir_node *call, ir_node *mem,
                         ir_node *reg_jmp, ir_node *exc_jmp)
{
	ir_node  *block = get_nodes_block(call);
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *rest  = new_r_Tuple(block, 1, &irn);

	if (ir_throws_exception(call)) {
		if (reg_jmp == NULL) {
			reg_jmp = new_r_Jmp(block);
		}
		if (exc_jmp == NULL) {
			exc_jmp = new_r_Bad(irg, mode_X);
		}
		ir_node *const in[] = {
			[pn_Call_M]         = mem,
			[pn_Call_T_result]  = rest,
			[pn_Call_X_regular] = reg_jmp,
			[pn_Call_X_except]  = exc_jmp,
		};
		turn_into_tuple(call, ARRAY_SIZE(in), in);
	} else {
		assert(reg_jmp == NULL);
		assert(exc_jmp == NULL);
		assert(pn_Call_M <= pn_Call_T_result);
		assert(pn_Call_X_regular > pn_Call_T_result);
		assert(pn_Call_X_except > pn_Call_T_result);
		ir_node *const in[] = {
			[pn_Call_M]         = mem,
			[pn_Call_T_result]  = rest,
		};
		turn_into_tuple(call, ARRAY_SIZE(in), in);
	}
}

int i_mapper_abs(ir_node *call, void *ctx)
{
	ir_node  *mem      = get_Call_mem(call);
	ir_node  *block    = get_nodes_block(call);
	ir_node  *op       = get_Call_param(call, 0);
	ir_graph *irg      = get_irn_irg(call);
	ir_mode  *mode     = get_irn_mode(op);
	dbg_info *dbg      = get_irn_dbg_info(call);
	ir_node  *zero     = new_r_Const(irg, get_mode_null(mode));
	ir_node  *cmp      = new_rd_Cmp(dbg, block, op, zero, ir_relation_less);
	ir_node  *minus_op = new_rd_Minus(dbg, block, op, mode);
	ir_node  *mux;
	arch_allow_ifconv_func allow_ifconv = be_get_backend_param()->allow_ifconv;
	(void) ctx;

	/* mux allowed by backend? */
	if (!allow_ifconv(cmp, op, minus_op))
		return 0;

	/* construct Mux */
	mux = new_rd_Mux(dbg, block, cmp, op, minus_op, mode);
	DBG_OPT_ALGSIM0(call, mux, FS_OPT_RTS_ABS);
	replace_call(mux, call, mem, NULL, NULL);
	return 1;
}

int i_mapper_bswap(ir_node *call, void *ctx)
{
	ir_node *mem   = get_Call_mem(call);
	ir_node *block = get_nodes_block(call);
	ir_node *op    = get_Call_param(call, 0);
	ir_type *tp    = get_Call_type(call);
	dbg_info *dbg  = get_irn_dbg_info(call);
	ir_node *irn;
	(void) ctx;

	ir_graph *const irg = get_Block_irg(block);
	irn = new_rd_Builtin(dbg, block, get_irg_no_mem(irg), 1, &op, ir_bk_bswap, tp);
	set_irn_pinned(irn, op_pin_state_floats);
	irn = new_r_Proj(irn, get_irn_mode(op), pn_Builtin_max+1);
	replace_call(irn, call, mem, NULL, NULL);
	return 1;
}

int i_mapper_alloca(ir_node *call, void *ctx)
{
	ir_node *mem   = get_Call_mem(call);
	ir_node *block = get_nodes_block(call);
	ir_node *op    = get_Call_param(call, 0);
	ir_node *irn;
	dbg_info *dbg  = get_irn_dbg_info(call);
	(void) ctx;

	if (mode_is_signed(get_irn_mode(op))) {
		ir_mode *mode = get_irn_mode(op);
		mode = find_unsigned_mode(mode);
		if (mode == NULL) {
			panic("Cannot find unsigned mode for %M", mode);
		}
		op = new_rd_Conv(dbg, block, op, mode);
	}

	irn    = new_rd_Alloc(dbg, block, mem, op, 1);
	mem    = new_rd_Proj(dbg, irn, mode_M, pn_Alloc_M);
	irn    = new_rd_Proj(dbg, irn, get_modeP_data(), pn_Alloc_res);
	assert(!ir_throws_exception(call));

	DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_ALLOCA);
	replace_call(irn, call, mem, NULL, NULL);
	return 1;
}

int i_mapper_sqrt(ir_node *call, void *ctx)
{
	ir_node   *mem;
	ir_tarval *tv;
	ir_node   *op = get_Call_param(call, 0);
	(void) ctx;

	if (!is_Const(op))
		return 0;

	tv = get_Const_tarval(op);
	if (! tarval_is_null(tv) && !tarval_is_one(tv))
		return 0;

	mem = get_Call_mem(call);

	/* sqrt(0) = 0, sqrt(1) = 1 */
	DBG_OPT_ALGSIM0(call, op, FS_OPT_RTS_SQRT);
	replace_call(op, call, mem, NULL, NULL);
	return 1;
}

int i_mapper_cbrt(ir_node *call, void *ctx)
{
	ir_node   *mem;
	ir_tarval *tv;
	ir_node   *op = get_Call_param(call, 0);
	(void) ctx;

	if (!is_Const(op))
		return 0;

	tv = get_Const_tarval(op);
	if (! tarval_is_null(tv) && !tarval_is_one(tv) && !tarval_is_minus_one(tv))
		return 0;

	mem = get_Call_mem(call);

	/* cbrt(0) = 0, cbrt(1) = 1, cbrt(-1) = -1 */
	DBG_OPT_ALGSIM0(call, op, FS_OPT_RTS_CBRT);
	replace_call(op, call, mem, NULL, NULL);
	return 1;
}

int i_mapper_pow(ir_node *call, void *ctx)
{
	ir_node  *left    = get_Call_param(call, 0);
	ir_node  *right   = get_Call_param(call, 1);
	ir_node  *block   = get_nodes_block(call);
	ir_graph *irg     = get_irn_irg(block);
	ir_node  *reg_jmp = NULL;
	ir_node  *exc_jmp = NULL;
	ir_node  *irn;
	dbg_info *dbg;
	ir_node  *mem;
	(void) ctx;

	if (is_Const(left) && is_Const_one(left)) {
		/* pow (1.0, x) = 1.0 */
		irn = left;
	} else if (is_Const(right)) {
		ir_tarval *tv = get_Const_tarval(right);
		if (tarval_is_null(tv)) {
			/* pow(x, 0.0) = 1.0 */
			ir_mode *mode = get_tarval_mode(tv);
			irn = new_r_Const(irg, get_mode_one(mode));
		} else if (tarval_is_one(tv)) {
			/* pow(x, 1.0) = x */
			irn = left;
		} else if (tarval_is_minus_one(tv)) {
			/* pow(x, -1.0) = 1/x */
			irn = NULL;
		} else
			return 0;
	} else {
		return 0;
	}

	mem = get_Call_mem(call);
	dbg = get_irn_dbg_info(call);

	if (irn == NULL) {
		ir_mode *result_mode = get_irn_mode(left);
		ir_node *div;

		ir_mode *mode             = result_mode;
		ir_mode *float_arithmetic = be_get_backend_param()->mode_float_arithmetic;
		if (float_arithmetic != NULL) {
			left = new_r_Conv(block, left, float_arithmetic);
			mode = float_arithmetic;
		}

		irn  = new_r_Const(irg, get_mode_one(mode));
		div  = new_rd_Div(dbg, block, mem, irn, left, mode, op_pin_state_pinned);
		mem  = new_r_Proj(div, mode_M, pn_Div_M);
		irn  = new_r_Proj(div, mode, pn_Div_res);
		if (ir_throws_exception(call)) {
			reg_jmp = new_r_Proj(div, mode_X, pn_Div_X_regular);
			exc_jmp = new_r_Proj(div, mode_X, pn_Div_X_except);
			ir_set_throws_exception(div, true);
		}
		if (result_mode != mode) {
			irn = new_r_Conv(block, irn, result_mode);
		}
	}
	DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_POW);
	replace_call(irn, call, mem, reg_jmp, exc_jmp);
	return 1;
}

int i_mapper_exp(ir_node *call, void *ctx)
{
	ir_node *val  = get_Call_param(call, 0);
	(void) ctx;

	if (is_Const(val) && is_Const_null(val)) {
		/* exp(0.0) = 1.0 */
		ir_graph *irg  = get_irn_irg(val);
		ir_mode *mode  = get_irn_mode(val);
		ir_node *irn   = new_r_Const(irg, get_mode_one(mode));
		ir_node *mem   = get_Call_mem(call);
		DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_EXP);
		replace_call(irn, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}

int i_mapper_exp2(ir_node *call, void *ctx)
{
	return i_mapper_exp(call, ctx);
}

int i_mapper_exp10(ir_node *call, void *ctx)
{
	return i_mapper_exp(call, ctx);
}

/**
 * A mapper for mapping f(0.0) to 0.0.
 */
static int i_mapper_zero_to_zero(ir_node *call, void *ctx, int reason)
{
	ir_node *val  = get_Call_param(call, 0);
	(void) ctx;

	if (is_Const(val) && is_Const_null(val)) {
		/* f(0.0) = 0.0 */
		ir_node *mem = get_Call_mem(call);
		DBG_OPT_ALGSIM0(call, val, reason);
		replace_call(val, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}

/**
 * A mapper for mapping f(1.0) to 0.0.
 */
static int i_mapper_one_to_zero(ir_node *call, void *ctx, int reason)
{
	ir_node *val  = get_Call_param(call, 0);
	(void) ctx;

	if (is_Const(val) && is_Const_one(val)) {
		/* acos(1.0) = 0.0 */
		ir_graph *irg = get_irn_irg(val);
		ir_mode *mode = get_irn_mode(val);
		ir_node *irn  = new_r_Const(irg, get_mode_null(mode));
		ir_node *mem  = get_Call_mem(call);
		DBG_OPT_ALGSIM0(call, irn, reason);
		replace_call(irn, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}

/**
 * A mapper for mapping a functions with the following characteristics:
 * f(-x)  = f(x).
 * f(0.0) = 1.0
 */
static int i_mapper_symmetric_zero_to_one(ir_node *call, void *ctx, int reason)
{
	int      changed = 0;
	ir_node *val     = get_Call_param(call, 0);
	(void) ctx;

	if (is_Conv(val)) {
		ir_node *op = get_Conv_op(val);
		if (is_Minus(op)) {
			/* f(-x) = f(x) with strictConv */
			ir_node *block = get_nodes_block(call);
			ir_mode *mode  = get_irn_mode(val);
			dbg_info *dbg  = get_irn_dbg_info(val);

			op = get_Minus_op(op);
			val = new_rd_Conv(dbg, block, op, mode);
			DBG_OPT_ALGSIM2(call, op, call, FS_OPT_RTS_SYMMETRIC);
			set_Call_param(call, 0, val);
			changed = 1;
		}
	} else if (is_Minus(val)) {
		/* f(-x) = f(x) */
		val = get_Minus_op(val);
		DBG_OPT_ALGSIM2(call, val, call, FS_OPT_RTS_SYMMETRIC);
		set_Call_param(call, 0, val);
		changed = 1;
	}

	if (is_Const(val) && is_Const_null(val)) {
		/* f(0.0) = 1.0 */
		ir_graph *irg  = get_irn_irg(val);
		ir_mode *mode  = get_irn_mode(val);
		ir_node *irn   = new_r_Const(irg, get_mode_one(mode));
		ir_node *mem   = get_Call_mem(call);
		DBG_OPT_ALGSIM0(call, irn, reason);
		replace_call(irn, call, mem, NULL, NULL);
		changed = 1;
	}
	return changed;
}

int i_mapper_log(ir_node *call, void *ctx)
{
	/* log(1.0) = 0.0 */
	return i_mapper_one_to_zero(call, ctx, FS_OPT_RTS_LOG);
}

int i_mapper_log2(ir_node *call, void *ctx)
{
	/* log2(1.0) = 0.0 */
	return i_mapper_one_to_zero(call, ctx, FS_OPT_RTS_LOG);
}

int i_mapper_log10(ir_node *call, void *ctx)
{
	/* log10(1.0) = 0.0 */
	return i_mapper_one_to_zero(call, ctx, FS_OPT_RTS_LOG);
}

int i_mapper_sin(ir_node *call, void *ctx)
{
	/* sin(0.0) = 0.0 */
	return i_mapper_zero_to_zero(call, ctx, FS_OPT_RTS_SIN);
}

int i_mapper_cos(ir_node *call, void *ctx)
{
	/* cos(0.0) = 1.0, cos(-x) = x */
	return i_mapper_symmetric_zero_to_one(call, ctx, FS_OPT_RTS_COS);
}

int i_mapper_tan(ir_node *call, void *ctx)
{
	/* tan(0.0) = 0.0 */
	return i_mapper_zero_to_zero(call, ctx, FS_OPT_RTS_TAN);
}

int i_mapper_asin(ir_node *call, void *ctx)
{
	/* asin(0.0) = 0.0 */
	return i_mapper_zero_to_zero(call, ctx, FS_OPT_RTS_ASIN);
}

int i_mapper_acos(ir_node *call, void *ctx)
{
	/* acos(1.0) = 0.0 */
	return i_mapper_one_to_zero(call, ctx, FS_OPT_RTS_ACOS);
}

int i_mapper_atan(ir_node *call, void *ctx)
{
	/* atan(0.0) = 0.0 */
	return i_mapper_zero_to_zero(call, ctx, FS_OPT_RTS_ATAN);
}

int i_mapper_sinh(ir_node *call, void *ctx)
{
	/* sinh(0.0) = 0.0 */
	return i_mapper_zero_to_zero(call, ctx, FS_OPT_RTS_SINH);
}

int i_mapper_cosh(ir_node *call, void *ctx)
{
	/* cosh(0.0) = 1.0, cosh(-x) = x */
	return i_mapper_symmetric_zero_to_one(call, ctx, FS_OPT_RTS_COSH);
}

int i_mapper_tanh(ir_node *call, void *ctx)
{
	/* tanh(0.0) = 0.0 */
	return i_mapper_zero_to_zero(call, ctx, FS_OPT_RTS_TANH);
}

/**
 * Return the const entity that is accessed through the pointer ptr or
 * NULL if there is no entity (or the entity is not constant).
 *
 * @param ptr  the pointer
 */
static ir_entity *get_const_entity(ir_node *ptr)
{
	if (is_SymConst_addr_ent(ptr)) {
		ir_entity *ent = get_SymConst_entity(ptr);

		if (get_entity_linkage(ent) & IR_LINKAGE_CONSTANT) {
			/* a constant entity */
			return ent;
		}
	}
	return NULL;
}

static ir_tarval *get_initializer_value(ir_initializer_t *const init, ir_mode *const mode)
{
	switch (get_initializer_kind(init)) {
	case IR_INITIALIZER_NULL:
		return get_mode_null(mode);

	case IR_INITIALIZER_TARVAL:
		return get_initializer_tarval_value(init);

	case IR_INITIALIZER_CONST: {
		ir_node *const irn = get_initializer_const_value(init);
		if (is_Const(irn))
			return get_Const_tarval(irn);
		break;
	}

	case IR_INITIALIZER_COMPOUND:
		break;
	}

	return get_tarval_undefined();
}

static bool initializer_val_is_null(ir_initializer_t *init)
{
	ir_tarval *tv;

	if (get_initializer_kind(init) == IR_INITIALIZER_NULL)
		return true;

	if (get_initializer_kind(init) == IR_INITIALIZER_TARVAL) {
		tv = get_initializer_tarval_value(init);
	} else if (get_initializer_kind(init) == IR_INITIALIZER_CONST) {
		ir_node *irn = get_initializer_const_value(init);
		if (!is_Const(irn))
			return false;
		tv = get_Const_tarval(irn);
	} else {
		return false;
	}

	return tarval_is_null(tv);
}

/**
 * Calculate the value of strlen if possible.
 *
 * @param ent     the entity
 * @param res_tp  the result type
 *
 * @return a Const node containing the strlen() result or NULL
 *         if the evaluation fails
 */
static ir_node *eval_strlen(ir_graph *irg, ir_entity *ent, ir_type *res_tp)
{
	ir_type *tp = get_entity_type(ent);
	ir_mode *mode;
	ir_initializer_t *initializer;
	size_t            size;
	size_t            i;

	if (! is_Array_type(tp))
		return NULL;
	tp = get_array_element_type(tp);
	if (! is_Primitive_type(tp))
		return NULL;
	mode = get_type_mode(tp);

	/* FIXME: This is too restrict, as the type char might be more the 8bits */
	if (!mode_is_int(mode) || get_mode_size_bits(mode) != 8)
		return NULL;

	initializer = get_entity_initializer(ent);
	if (get_initializer_kind(initializer) != IR_INITIALIZER_COMPOUND)
		return NULL;

	size = get_initializer_compound_n_entries(initializer);
	for (i = 0; i < size; ++i) {
		ir_initializer_t *val = get_initializer_compound_value(initializer, i);
		if (initializer_val_is_null(val)) {
			ir_tarval *tv = new_tarval_from_long(i, get_type_mode(res_tp));
			return new_r_Const(irg, tv);
		}
	}

	return NULL;
}

int i_mapper_strlen(ir_node *call, void *ctx)
{
	ir_node *s     = get_Call_param(call, 0);
	ir_entity *ent = get_const_entity(s);

	(void) ctx;

	/* FIXME: this cannot handle constant strings inside struct initializers yet */
	if (ent != NULL) {
		/* a constant entity */
		ir_type *tp = get_Call_type(call);
		ir_node *irn;

		tp  = get_method_res_type(tp, 0);
		irn = eval_strlen(get_irn_irg(call), ent, tp);

		if (irn) {
			ir_node *mem = get_Call_mem(call);
			DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_STRLEN);
			replace_call(irn, call, mem, NULL, NULL);
			return 1;
		}
	}
	return 0;
}

/**
 * Calculate the value of strlen if possible.
 *
 * @param left    the left entity
 * @param right   the right entity
 * @param res_tp  the result type
 *
 * @return a Const node containing the strcmp() result or NULL
 *         if the evaluation fails
 */
static ir_node *eval_strcmp(ir_graph *irg, ir_entity *left, ir_entity *right,
                            ir_type *res_tp)
{
	ir_type          *tp;
	ir_mode          *mode;
	ir_initializer_t *init_l;
	ir_initializer_t *init_r;
	size_t            size_l;
	size_t            size_r;
	size_t            size;
	size_t            i;

	tp = get_entity_type(left);
	if (! is_Array_type(tp))
		return NULL;
	tp = get_array_element_type(tp);
	if (! is_Primitive_type(tp))
		return NULL;
	mode = get_type_mode(tp);

	/* FIXME: This is too restrict, as the type char might be more the 8bits */
	if (!mode_is_int(mode) || get_mode_size_bits(mode) != 8)
		return NULL;

	tp = get_entity_type(right);
	if (! is_Array_type(tp))
		return NULL;
	tp = get_array_element_type(tp);
	if (! is_Primitive_type(tp))
		return NULL;
	mode = get_type_mode(tp);

	/* FIXME: This is too restrict, as the type char might be more the 8bits */
	if (!mode_is_int(mode) || get_mode_size_bits(mode) != 8)
		return NULL;

	init_l = get_entity_initializer(left);
	init_r = get_entity_initializer(right);
	if (get_initializer_kind(init_l) != IR_INITIALIZER_COMPOUND ||
	    get_initializer_kind(init_r) != IR_INITIALIZER_COMPOUND)
		return NULL;

	size_l = get_initializer_compound_n_entries(init_l);
	size_r = get_initializer_compound_n_entries(init_r);
	size   = size_l < size_r ? size_l : size_r;

	for (i = 0; i != size; ++i) {
		ir_initializer_t *const val_l = get_initializer_compound_value(init_l, i);
		ir_tarval        *const tv_l  = get_initializer_value(val_l, mode);
		ir_initializer_t *const val_r = get_initializer_compound_value(init_r, i);
		ir_tarval        *const tv_r  = get_initializer_value(val_r, mode);

		if (!tarval_is_constant(tv_l) || !tarval_is_constant(tv_r))
			return NULL;

		if (tv_l != tv_r) {
			ir_mode   *const res_mode = get_type_mode(res_tp);
			ir_tarval *const res_l    = tarval_convert_to(tv_l, res_mode);
			ir_tarval *const res_r    = tarval_convert_to(tv_r, res_mode);
			ir_tarval *const tv       = tarval_sub(res_l, res_r, res_mode);
			return new_r_Const(irg, tv);
		}

		if (tarval_is_null(tv_l)) {
			ir_tarval *const tv = get_mode_null(get_type_mode(res_tp));
			return new_r_Const(irg, tv);
		}
	}

	return NULL;
}

/**
 * Checks if an entity represents the empty string.
 *
 * @param ent     the entity
 *
 * @return non-zero if ent represents the empty string
 */
static int is_empty_string(ir_entity *ent)
{
	ir_type          *tp = get_entity_type(ent);
	ir_mode          *mode;
	ir_initializer_t *initializer;
	ir_initializer_t *init0;

	if (! is_Array_type(tp))
		return 0;
	tp = get_array_element_type(tp);
	if (! is_Primitive_type(tp))
		return 0;
	mode = get_type_mode(tp);

	/* FIXME: This is too restrict, as the type char might be more the 8bits */
	if (!mode_is_int(mode) || get_mode_size_bits(mode) != 8)
		return 0;

	initializer = get_entity_initializer(ent);
	if (get_initializer_kind(initializer) != IR_INITIALIZER_COMPOUND)
		return 0;

	if (get_initializer_compound_n_entries(initializer) < 1)
		return 0;

	init0 = get_initializer_compound_value(initializer, 0);
	return initializer_val_is_null(init0);
}

int i_mapper_strcmp(ir_node *call, void *ctx)
{
	ir_node   *left    = get_Call_param(call, 0);
	ir_node   *right   = get_Call_param(call, 1);
	ir_node   *irn     = NULL;
	ir_node   *exc     = NULL;
	ir_node   *reg     = NULL;
	ir_type   *call_tp = get_Call_type(call);
	ir_type   *res_tp  = get_method_res_type(call_tp, 0);
	ir_entity *ent_l, *ent_r;
	ir_type   *char_tp;
	ir_node   *v;

	(void) ctx;

	/* do some type checks first */
	if (! is_Primitive_type(res_tp))
		return 0;
	char_tp = get_method_param_type(call_tp, 0);
	if (char_tp != get_method_param_type(call_tp, 1))
		return 0;
	if (! is_Pointer_type(char_tp))
		return 0;
	char_tp = get_pointer_points_to_type(char_tp);

	ir_node *mem = get_Call_mem(call);
	if (left == right) {
		/* a strcmp(s, s) ==> 0 */
		ir_graph *irg = get_irn_irg(call);
		ir_mode *mode = get_type_mode(res_tp);

		irn = new_r_Const(irg, get_mode_null(mode));
		DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_STRCMP);
		replace_call(irn, call, mem, NULL, NULL);
		return 1;
	}
	ent_l = get_const_entity(left);
	ent_r = get_const_entity(right);

	if (ent_l != NULL && ent_r != NULL) {
		/* both entities are const, try to evaluate */
		irn = eval_strcmp(get_irn_irg(call), ent_l, ent_r, res_tp);
	} else if (ent_l != NULL) {
		if (is_empty_string(ent_l)) {
			/* s strcmp("", s) ==> -(*s)*/
			v = right;
			goto replace_by_call;
		}
	} else if (ent_r != NULL) {
		if (is_empty_string(ent_r)) {
			/* s strcmp(s, "") ==> (*s) */
			ir_node  *block;
			dbg_info *dbg;
			ir_mode  *mode;

			v = left;
replace_by_call:
			mem   = get_Call_mem(call);
			block = get_nodes_block(call);
			dbg   = get_irn_dbg_info(call);
			mode  = get_type_mode(char_tp);

			/* replace the strcmp by (*x) */
			irn = new_rd_Load(dbg, block, mem, v, mode, cons_none);
			mem = new_r_Proj(irn, mode_M, pn_Load_M);
			irn = new_r_Proj(irn, mode, pn_Load_res);
			if (ir_throws_exception(call)) {
				exc = new_r_Proj(irn, mode_X, pn_Load_X_except);
				reg = new_r_Proj(irn, mode_X, pn_Load_X_regular);
				ir_set_throws_exception(irn, true);
			} else {
				exc = NULL;
				reg = NULL;
			}

			/* conv to the result mode */
			mode = get_type_mode(res_tp);
			irn  = new_rd_Conv(dbg, block, irn, mode);

			if (v == right) {
				/* negate in the ("", s) case */
				irn = new_rd_Minus(dbg, block, irn, mode);
			}
		}
	}

	if (irn != NULL) {
		DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_STRCMP);
		replace_call(irn, call, mem, reg, exc);
		return 1;
	}

	return 0;
}

int i_mapper_strncmp(ir_node *call, void *ctx)
{
	ir_node *left  = get_Call_param(call, 0);
	ir_node *right = get_Call_param(call, 1);
	ir_node *len   = get_Call_param(call, 2);
	ir_node *irn;
	(void) ctx;

	if (left == right || (is_Const(len) && is_Const_null(len))) {
		/* a strncmp(s, s, len) ==> 0 OR
		   a strncmp(a, b, 0) ==> 0 */
		ir_graph  *irg     = get_irn_irg(call);
		ir_node   *mem     = get_Call_mem(call);
		ir_node   *adr     = get_Call_ptr(call);
		ir_entity *ent     = get_SymConst_entity(adr);
		ir_type   *call_tp = get_entity_type(ent);
		ir_type   *res_tp  = get_method_res_type(call_tp, 0);
		ir_mode   *mode    = get_type_mode(res_tp);

		irn = new_r_Const(irg, get_mode_null(mode));
		DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_STRNCMP);
		replace_call(irn, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}

int i_mapper_strcpy(ir_node *call, void *ctx)
{
	ir_node *dst = get_Call_param(call, 0);
	ir_node *src = get_Call_param(call, 1);
	(void) ctx;

	if (dst == src) {
		/* a strcpy(d, s) ==> d */
		ir_node *mem = get_Call_mem(call);
		ir_node *dst = get_Call_param(call, 0);

		DBG_OPT_ALGSIM0(call, dst, FS_OPT_RTS_STRCPY);
		replace_call(dst, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}

int i_mapper_memcpy(ir_node *call, void *ctx)
{
	ir_node *dst = get_Call_param(call, 0);
	ir_node *src = get_Call_param(call, 1);
	ir_node *len = get_Call_param(call, 2);
	(void) ctx;

	if (dst == src || (is_Const(len) && is_Const_null(len))) {
		/* a memcpy(d, d, len) ==> d OR
		   a memcpy(d, s, 0) ==> d */
		ir_node *mem = get_Call_mem(call);

		DBG_OPT_ALGSIM0(call, dst, FS_OPT_RTS_MEMCPY);
		replace_call(dst, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}

int i_mapper_mempcpy(ir_node *call, void *ctx)
{
	ir_node *dst = get_Call_param(call, 0);
	ir_node *src = get_Call_param(call, 1);
	ir_node *len = get_Call_param(call, 2);
	(void) ctx;

	if (dst == src || (is_Const(len) && is_Const_null(len))) {
		/* a memcpy(d, d, len) ==> d + len OR
		   a memcpy(d, s, 0) ==> d + 0 */
		dbg_info *dbg = get_irn_dbg_info(call);
		ir_node *mem  = get_Call_mem(call);
		ir_node *blk  = get_nodes_block(call);
		ir_mode *mode = get_irn_mode(dst);
		ir_node *res  = new_rd_Add(dbg, blk, dst, len, mode);

		DBG_OPT_ALGSIM0(call, res, FS_OPT_RTS_MEMPCPY);
		replace_call(res, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}

int i_mapper_memmove(ir_node *call, void *ctx)
{
	ir_node *dst = get_Call_param(call, 0);
	ir_node *src = get_Call_param(call, 1);
	ir_node *len = get_Call_param(call, 2);
	(void) ctx;

	if (dst == src || (is_Const(len) && is_Const_null(len))) {
		/* a memmove(d, d, len) ==> d OR
		   a memmove(d, s, 0) ==> d */
		ir_node *mem = get_Call_mem(call);

		DBG_OPT_ALGSIM0(call, dst, FS_OPT_RTS_MEMMOVE);
		replace_call(dst, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}

int i_mapper_memset(ir_node *call, void *ctx)
{
	ir_node *len = get_Call_param(call, 2);
	(void) ctx;

	if (is_Const(len) && is_Const_null(len)) {
		/* a memset(d, C, 0) ==> d */
		ir_node *mem = get_Call_mem(call);
		ir_node *dst = get_Call_param(call, 0);

		DBG_OPT_ALGSIM0(call, dst, FS_OPT_RTS_MEMSET);
		replace_call(dst, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}

int i_mapper_memcmp(ir_node *call, void *ctx)
{
	ir_node *left  = get_Call_param(call, 0);
	ir_node *right = get_Call_param(call, 1);
	ir_node *len   = get_Call_param(call, 2);
	ir_node *irn;
	(void) ctx;

	if (left == right || (is_Const(len) && is_Const_null(len))) {
		/* a memcmp(s, s, len) ==> 0 OR
		   a memcmp(a, b, 0) ==> 0 */
		ir_graph  *irg     = get_irn_irg(call);
		ir_node   *mem     = get_Call_mem(call);
		ir_node   *adr     = get_Call_ptr(call);
		ir_entity *ent     = get_SymConst_entity(adr);
		ir_type   *call_tp = get_entity_type(ent);
		ir_type   *res_tp  = get_method_res_type(call_tp, 0);
		ir_mode   *mode    = get_type_mode(res_tp);

		irn = new_r_Const(irg, get_mode_null(mode));
		DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_STRNCMP);
		replace_call(irn, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}

/**
 * Returns the result mode of a node.
 */
static ir_mode *get_irn_res_mode(ir_node *node)
{
	switch (get_irn_opcode(node)) {
	case iro_Load:   return get_Load_mode(node);
	case iro_Div:    return get_Div_resmode(node);
	case iro_Mod:    return get_Mod_resmode(node);
	default: return NULL;
	}
}

int i_mapper_RuntimeCall(ir_node *node, runtime_rt *rt)
{
	int i, j, arity, first, n_param, n_res;
	long n_proj;
	ir_type *mtp;
	ir_node *mem, *call, *addr, *res_proj;
	ir_node **in;
	bool     throws_exception;
	ir_op   *op;
	symconst_symbol sym;
	ir_mode *mode = get_irn_mode(node);

	if (mode != rt->mode)
		return 0;
	/* check if the result modes match */
	if (mode == mode_T && rt->res_mode != NULL) {
		mode = get_irn_res_mode(node);
		if (mode != rt->res_mode)
			return 0;
	}

	arity = get_irn_arity(node);
	if (arity <= 0)
		return 0;

	mtp     = get_entity_type(rt->ent);
	n_param = get_method_n_params(mtp);
	ir_node  *const bl  = get_nodes_block(node);
	ir_graph *const irg = get_Block_irg(bl);

	mem = get_irn_n(node, 0);
	if (get_irn_mode(mem) != mode_M) {
		mem = new_r_NoMem(irg);
		first = 0;
	} else
		first = 1;

	/* check if the modes of the predecessors match the parameter modes */
	if (arity - first != n_param)
		return 0;

	for (i = first, j = 0; i < arity; ++i, ++j) {
		ir_type *param_tp = get_method_param_type(mtp, j);
		ir_node *pred = get_irn_n(node, i);

		if (get_type_mode(param_tp) != get_irn_mode(pred))
			return 0;
	}

	n_res            = get_method_n_ress(mtp);
	throws_exception = ir_throws_exception(node);

	/* step 0: calculate the number of needed Proj's */
	n_proj = 0;
	n_proj = MAX(n_proj, rt->mem_proj_nr + 1);
	n_proj = MAX(n_proj, rt->res_proj_nr + 1);
	if (throws_exception) {
		n_proj = MAX(n_proj, rt->regular_proj_nr + 1);
		n_proj = MAX(n_proj, rt->exc_proj_nr + 1);
	}

	if (n_proj > 0) {
		if (rt->mode != mode_T) /* must be mode_T */
			return 0;
	} else {
		if (n_res > 0)
			/* must match */
			if (get_type_mode(get_method_res_type(mtp, 0)) != rt->mode)
				return 0;
	}

	/* ok, when we are here, the number of predecessors match as well as the parameter modes */
	op = get_irn_op(node);

	in = NULL;
	if (n_param > 0) {
		NEW_ARR_A(ir_node *, in, n_param);
		for (i = 0; i < n_param; ++i)
			in[i] = get_irn_n(node, first + i);
	}

	/* step 1: create the call */
	sym.entity_p = rt->ent;
	addr = new_r_SymConst(irg, mode_P_code, sym, symconst_addr_ent);
	call = new_rd_Call(get_irn_dbg_info(node), bl, mem, addr, n_param, in, mtp);
	set_irn_pinned(call, get_irn_pinned(node));

	if (n_res > 0)
		res_proj = new_r_Proj(call, mode_T, pn_Call_T_result);
	else
		res_proj = NULL;

	if (n_proj > 0) {
		n_proj += n_res - 1;

		ir_node **const in  = ALLOCAN(ir_node*, n_proj);
		ir_node  *const bad = new_r_Bad(irg, mode_ANY);
		for (i = 0; i != n_proj; ++i) {
			in[i] = bad;
		}

		if (rt->mem_proj_nr >= 0)
			in[rt->mem_proj_nr] = new_r_Proj(call, mode_M, pn_Call_M);
		if (throws_exception) {
			in[op->pn_x_regular] = new_r_Proj(call, mode_X, pn_Call_X_regular);
			in[op->pn_x_except]  = new_r_Proj(call, mode_X, pn_Call_X_except);
		}

		if (rt->res_proj_nr >= 0) {
			for (i = 0; i < n_res; ++i) {
				ir_mode *mode = get_type_mode(get_method_res_type(mtp, i));
				ir_node *proj = new_r_Proj(res_proj, mode, i);
				in[rt->res_proj_nr + i] = proj;
			}
		}

		turn_into_tuple(node, n_proj, in);
		return 1;
	} else {
		/* only one return value supported */
		if (n_res > 0) {
			ir_mode *mode = get_type_mode(get_method_res_type(mtp, 0));

			res_proj = new_r_Proj(call, mode_T, pn_Call_T_result);
			res_proj = new_r_Proj(res_proj, mode, 0);

			exchange(node, res_proj);
			return 1;
		}
	}
	/* should not happen */
	return 0;
}
