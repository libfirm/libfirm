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
 * @brief   lowering of Calls of intrinsic functions
 * @author  Michael Beck
 * @version $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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
#include "pmap.h"
#include "xmalloc.h"
#include "iropt_dbg.h"

/** Walker environment */
typedef struct _walker_env {
	pmap     *c_map;              /**< The intrinsic call map. */
	unsigned nr_of_intrinsics;    /**< statistics */
	i_instr_record **i_map;       /**< The intrinsic instruction map. */
} walker_env_t;

/**
 * walker: call all mapper functions
 */
static void call_mapper(ir_node *node, void *env) {
	walker_env_t *wenv = env;
	ir_op *op = get_irn_op(node);

	if (op == op_Call) {
		ir_node *symconst;
		pmap_entry *p;
		const i_call_record *r;
		ir_entity *ent;

		symconst = get_Call_ptr(node);
		if (get_irn_op(symconst) != op_SymConst ||
			get_SymConst_kind(symconst) != symconst_addr_ent)
			return;

		ent = get_SymConst_entity(symconst);
		p   = pmap_find(wenv->c_map, ent);

		if (p) {
			r = p->value;
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
				r = r->link;
			}
		}
	}
}  /* call_mapper */

/* Go through all graphs and map calls to intrinsic functions. */
unsigned lower_intrinsics(i_record *list, int length, int part_block_used) {
	int            i, n_ops = get_irp_n_opcodes();
	ir_graph       *irg;
	pmap           *c_map = pmap_create_ex(length);
	i_instr_record **i_map;
	unsigned       nr_of_intrinsics = 0;
	walker_env_t   wenv;

	/* we use the ir_op generic pointers here */
	NEW_ARR_A(const i_instr_record *, i_map, n_ops);
	memset((void *)i_map, 0, sizeof(*i_map) * n_ops);

	/* fill a map for faster search */
	for (i = length - 1; i >= 0; --i) {
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

	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		irg = get_irp_irg(i);

		if (part_block_used)
			collect_phiprojs(irg);

		wenv.nr_of_intrinsics = 0;
		irg_walk_graph(irg, NULL, call_mapper, &wenv);

		if (wenv.nr_of_intrinsics) {
			/* changes detected */
			set_irg_outs_inconsistent(irg);
			set_irg_callee_info_state(irg, irg_callee_info_inconsistent);

			/* exception control flow might have changed */
			set_irg_doms_inconsistent(irg);
			set_irg_extblk_inconsistent(irg);
			set_irg_loopinfo_inconsistent(irg);

			/* calls might be removed/added */
			set_trouts_inconsistent();

			/* optimize it, tuple might be created */
			local_optimize_graph(irg);

			nr_of_intrinsics += wenv.nr_of_intrinsics;
		}
	}
	pmap_destroy(c_map);

	return nr_of_intrinsics;
}  /* lower_intrinsics */

/**
 * Helper function, replace the call by the given node.
 *
 * @param irn      the result node
 * @param call     the call to replace
 * @param mem      the new mem result
 * @param reg_jmp  new regular control flow, if NULL, a Jmp will be used
 * @param exc_jmp  new exception control flow, if reg_jmp == NULL, a Bad will be used
 */
static void replace_call(ir_node *irn, ir_node *call, ir_node *mem, ir_node *reg_jmp, ir_node *exc_jmp) {
	if (reg_jmp == NULL) {
		ir_node *block = get_nodes_block(call);

		/* Beware: do we need here a protection against CSE? Better we do it. */
		int old_cse = get_opt_cse();
		set_opt_cse(0);
		reg_jmp = new_r_Jmp(current_ir_graph, block);
		set_opt_cse(old_cse);
		exc_jmp = new_Bad();
	}
	irn = new_Tuple(1, &irn);

	turn_into_tuple(call, pn_Call_max);
	set_Tuple_pred(call, pn_Call_M_regular, mem);
	set_Tuple_pred(call, pn_Call_X_regular, reg_jmp);
	set_Tuple_pred(call, pn_Call_X_except, exc_jmp);
	set_Tuple_pred(call, pn_Call_T_result, irn);
	set_Tuple_pred(call, pn_Call_M_except, mem);
	set_Tuple_pred(call, pn_Call_P_value_res_base, new_Bad());
}  /* replace_call */

/* A mapper for the integer abs. */
int i_mapper_abs(ir_node *call, void *ctx) {
	ir_node *mem   = get_Call_mem(call);
	ir_node *block = get_nodes_block(call);
	ir_node *op    = get_Call_param(call, 0);
	ir_node *irn;
	dbg_info *dbg  = get_irn_dbg_info(call);
	(void) ctx;

	irn = new_rd_Abs(dbg, current_ir_graph, block, op, get_irn_mode(op));
	DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_ABS);
	replace_call(irn, call, mem, NULL, NULL);
	return 1;
}  /* i_mapper_abs */

/* A mapper for the alloca() function. */
int i_mapper_alloca(ir_node *call, void *ctx) {
	ir_node *mem   = get_Call_mem(call);
	ir_node *block = get_nodes_block(call);
	ir_node *op    = get_Call_param(call, 0);
	ir_node *irn, *exc, *no_exc;
	dbg_info *dbg  = get_irn_dbg_info(call);
	(void) ctx;

	irn    = new_rd_Alloc(dbg, current_ir_graph, block, mem, op, firm_unknown_type, stack_alloc);
	mem    = new_Proj(irn, mode_M, pn_Alloc_M);
	no_exc = new_Proj(irn, mode_X, pn_Alloc_X_regular);
	exc    = new_Proj(irn, mode_X, pn_Alloc_X_except);
	irn    = new_Proj(irn, get_modeP_data(), pn_Alloc_res);

	DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_ALLOCA);
	replace_call(irn, call, mem, no_exc, exc);
	return 1;
}  /* i_mapper_alloca */

/* A mapper for the floating point sqrt. */
int i_mapper_sqrt(ir_node *call, void *ctx) {
	ir_node *mem;
	tarval *tv;
	ir_node *op = get_Call_param(call, 0);
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
}  /* i_mapper_sqrt */

/* A mapper for the floating point cbrt. */
int i_mapper_cbrt(ir_node *call, void *ctx) {
	ir_node *mem;
	tarval *tv;
	ir_node *op = get_Call_param(call, 0);
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
}  /* i_mapper_cbrt */

/* A mapper for the floating point pow. */
int i_mapper_pow(ir_node *call, void *ctx) {
	dbg_info *dbg;
	ir_node *mem;
	ir_node *left  = get_Call_param(call, 0);
	ir_node *right = get_Call_param(call, 1);
	ir_node *block = get_nodes_block(call);
	ir_node *irn, *reg_jmp = NULL, *exc_jmp = NULL;
	(void) ctx;

	if (is_Const(left) && is_Const_one(left)) {
		/* pow (1.0, x) = 1.0 */
		irn = left;
	} else if (is_Const(right)) {
		tarval *tv = get_Const_tarval(right);
		if (tarval_is_null(tv)) {
			/* pow(x, 0.0) = 1.0 */
			ir_mode *mode = get_tarval_mode(tv);
			irn = new_r_Const(current_ir_graph, block, mode, get_mode_one(mode));
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
		ir_mode *mode = get_irn_mode(left);
		ir_node *quot;

		irn  = new_r_Const(current_ir_graph, block, mode, get_mode_one(mode));
		quot = new_rd_Quot(dbg, current_ir_graph, block, mem, irn, left, mode, op_pin_state_pinned);
		mem  = new_r_Proj(current_ir_graph, block, quot, mode_M, pn_Quot_M);
		irn  = new_r_Proj(current_ir_graph, block, quot, mode, pn_Quot_res);
		reg_jmp = new_r_Proj(current_ir_graph, block, quot, mode_X, pn_Quot_X_regular);
		exc_jmp = new_r_Proj(current_ir_graph, block, quot, mode_X, pn_Quot_X_except);
	}
	DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_POW);
	replace_call(irn, call, mem, reg_jmp, exc_jmp);
	return 1;
}  /* i_mapper_pow */

/* A mapper for the floating point exp. */
int i_mapper_exp(ir_node *call, void *ctx) {
	ir_node *val  = get_Call_param(call, 0);
	(void) ctx;

	if (is_Const(val) && is_Const_null(val)) {
		/* exp(0.0) = 1.0 */
		ir_node *block = get_nodes_block(call);
		ir_mode *mode  = get_irn_mode(val);
		ir_node *irn   = new_r_Const(current_ir_graph, block, mode, get_mode_one(mode));
		ir_node *mem   = get_Call_mem(call);
		DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_EXP);
		replace_call(irn, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}  /* i_mapper_exp */

/* A mapper for the floating point log. */
int i_mapper_log(ir_node *call, void *ctx) {
	ir_node *val  = get_Call_param(call, 0);
	(void) ctx;

	if (is_Const(val) && is_Const_one(val)) {
		/* log(1.0) = 0.0 */
		ir_node *block = get_nodes_block(call);
		ir_mode *mode  = get_irn_mode(val);
		ir_node *irn   = new_r_Const(current_ir_graph, block, mode, get_mode_null(mode));
		ir_node *mem   = get_Call_mem(call);
		DBG_OPT_ALGSIM0(call, val, FS_OPT_RTS_LOG);
		replace_call(val, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}  /* i_mapper_log */

/**
 * A mapper for mapping f(0.0) to 0.0.
 */
static int i_mapper_zero_to_zero(ir_node *call, void *ctx, int reason) {
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
}  /* i_mapper_zero_to_zero */

/**
 * A mapper for mapping f(1.0) to 0.0.
 */
static int i_mapper_one_to_zero(ir_node *call, void *ctx, int reason) {
	ir_node *val  = get_Call_param(call, 0);
	(void) ctx;

	if (is_Const(val) && is_Const_one(val)) {
		/* acos(1.0) = 0.0 */
		ir_node *block = get_nodes_block(call);
		ir_mode *mode  = get_irn_mode(val);
		ir_node *irn   = new_r_Const(current_ir_graph, block, mode, get_mode_null(mode));
		ir_node *mem   = get_Call_mem(call);
		DBG_OPT_ALGSIM0(call, irn, reason);
		replace_call(irn, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}  /* i_mapper_one_to_zero */

/**
 * A mapper for mapping a functions with the following characteristics:
 * f(-x)  = f(x).
 * f(0.0) = 1.0
 */
static int i_mapper_symmetric_zero_to_one(ir_node *call, void *ctx, int reason) {
	ir_node *val  = get_Call_param(call, 0);
	(void) ctx;

	if (is_strictConv(val)) {
		ir_node *op = get_Conv_op(val);
		if (is_Minus(op)) {
			/* f(-x) = f(x) with strictConv */
			ir_node *block = get_nodes_block(call);
			ir_mode *mode  = get_irn_mode(val);
			dbg_info *dbg  = get_irn_dbg_info(val);

			op = get_Minus_op(op);
			val = new_rd_Conv(dbg, current_ir_graph, block, op, mode);
			if (is_Conv(val)) {
				/* still a Conv ? */
				set_Conv_strict(val, 1);
			}
			DBG_OPT_ALGSIM2(call, op, call, FS_OPT_RTS_SYMMETRIC);
			set_Call_param(call, 0, val);
		}
	} else if (is_Minus(val)) {
		/* f(-x) = f(x) */
		val = get_Minus_op(val);
		DBG_OPT_ALGSIM2(call, val, call, FS_OPT_RTS_SYMMETRIC);
		set_Call_param(call, 0, val);
	}

	if (is_Const(val) && is_Const_null(val)) {
		/* f(0.0) = 1.0 */
		ir_node *block = get_nodes_block(call);
		ir_mode *mode  = get_irn_mode(val);
		ir_node *irn   = new_r_Const(current_ir_graph, block, mode, get_mode_one(mode));
		ir_node *mem   = get_Call_mem(call);
		DBG_OPT_ALGSIM0(call, irn, reason);
		replace_call(irn, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}  /* i_mapper_symmetric_zero_to_one */

/* A mapper for the floating point sin. */
int i_mapper_sin(ir_node *call, void *ctx) {
	/* sin(0.0) = 0.0 */
	return i_mapper_zero_to_zero(call, ctx, FS_OPT_RTS_SIN);
}  /* i_mapper_sin */

/* A mapper for the floating point cos. */
int i_mapper_cos(ir_node *call, void *ctx) {
	/* cos(0.0) = 1.0, cos(-x) = x */
	return i_mapper_symmetric_zero_to_one(call, ctx, FS_OPT_RTS_COS);
}  /* i_mapper_cos */

/* A mapper for the floating point tan. */
int i_mapper_tan(ir_node *call, void *ctx) {
	/* tan(0.0) = 0.0 */
	return i_mapper_zero_to_zero(call, ctx, FS_OPT_RTS_TAN);
}  /* i_mapper_tan */

/* A mapper for the floating point asin. */
int i_mapper_asin(ir_node *call, void *ctx) {
	/* asin(0.0) = 0.0 */
	return i_mapper_zero_to_zero(call, ctx, FS_OPT_RTS_ASIN);
}  /* i_mapper_asin */

/* A mapper for the floating point acos. */
int i_mapper_acos(ir_node *call, void *ctx) {
	/* acos(1.0) = 0.0 */
	return i_mapper_one_to_zero(call, ctx, FS_OPT_RTS_ACOS);
}  /* i_mapper_acos */

/* A mapper for the floating point atan. */
int i_mapper_atan(ir_node *call, void *ctx) {
	/* atan(0.0) = 0.0 */
	return i_mapper_zero_to_zero(call, ctx, FS_OPT_RTS_ATAN);
}  /* i_mapper_atan */

/* A mapper for the floating point sinh. */
int i_mapper_sinh(ir_node *call, void *ctx) {
	/* sinh(0.0) = 0.0 */
	return i_mapper_zero_to_zero(call, ctx, FS_OPT_RTS_SINH);
}  /* i_mapper_sinh */

/* A mapper for the floating point cosh. */
int i_mapper_cosh(ir_node *call, void *ctx) {
	/* cosh(0.0) = 1.0, cosh(-x) = x */
	return i_mapper_symmetric_zero_to_one(call, ctx, FS_OPT_RTS_COSH);
}  /* i_mapper_cosh */

/* A mapper for the floating point tanh. */
int i_mapper_tanh(ir_node *call, void *ctx) {
	/* tanh(0.0) = 0.0 */
	return i_mapper_zero_to_zero(call, ctx, FS_OPT_RTS_TANH);
}  /* i_mapper_tanh */

/**
 * Return the const entity that is accessed through the pointer ptr or
 * NULL if there is no entity (or the entity is not constant).
 *
 * @param ptr  the pointer
 */
static ir_entity *get_const_entity(ir_node *ptr) {
	/* FIXME: this cannot handle constant strings inside struct initializers yet */
	if (is_SymConst(ptr) && get_SymConst_kind(ptr) == symconst_addr_ent) {
		ir_entity *ent = get_SymConst_entity(ptr);

		if (get_entity_variability(ent) == variability_constant) {
			/* a constant entity */
			return ent;
		}
	}
	return NULL;
}  /* get_const_entity */

/**
 * Calculate the value of strlen if possible.
 *
 * @param ent     the entity
 * @param res_tp  the result type
 *
 * @return a Const node containing the strlen() result or NULL
 *         if the evaluation fails
 */
static ir_node *eval_strlen(ir_entity *ent, ir_type *res_tp) {
	ir_type *tp = get_entity_type(ent);
	ir_mode *mode;
	int     i, n, len = -1;

	if (! is_Array_type(tp))
		return NULL;
	tp = get_array_element_type(tp);
	if (! is_Primitive_type(tp))
		return NULL;
	mode = get_type_mode(tp);

	/* FIXME: This is too restrict, as the type char might be more the 8bits */
	if (!mode_is_int(mode) || get_mode_size_bits(mode) != get_mode_size_bits(mode_Bs))
		return NULL;

	n = get_compound_ent_n_values(ent);
	for (i = 0; i < n; ++i) {
		ir_node *irn = get_compound_ent_value(ent, i);

		if (! is_Const(irn))
			return NULL;

		if (is_Const_null(irn)) {
			/* found the length */
			len = i;
			break;
		}
	}
	if (len >= 0) {
		tarval *tv = new_tarval_from_long(len, get_type_mode(res_tp));
		return new_Const_type(tv, res_tp);
	}
	return NULL;
}  /* eval_strlen */

/* A mapper for strlen */
int i_mapper_strlen(ir_node *call, void *ctx) {
	ir_node *s     = get_Call_param(call, 0);
	ir_entity *ent = get_const_entity(s);

	/* FIXME: this cannot handle constant strings inside struct initializers yet */
	if (ent != NULL) {
		/* a constant entity */
		ir_type *tp = get_Call_type(call);
		ir_node *irn;

		tp  = get_method_res_type(tp, 0);
		irn = eval_strlen(ent, tp);

		if (irn) {
			ir_node *mem = get_Call_mem(call);
			DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_STRLEN);
			replace_call(irn, call, mem, NULL, NULL);
			return 1;
		}
	}
	return 0;
}  /* i_mapper_strlen */

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
static ir_node *eval_strcmp(ir_entity *left, ir_entity *right, ir_type *res_tp) {
	ir_type *tp;
	ir_mode *mode;
	int     i, n, n_r, res;

	tp = get_entity_type(left);
	if (! is_Array_type(tp))
		return NULL;
	tp = get_array_element_type(tp);
	if (! is_Primitive_type(tp))
		return NULL;
	mode = get_type_mode(tp);

	/* FIXME: This is too restrict, as the type char might be more the 8bits */
	if (!mode_is_int(mode) || get_mode_size_bits(mode) != get_mode_size_bits(mode_Bs))
		return NULL;

	tp = get_entity_type(right);
	if (! is_Array_type(tp))
		return NULL;
	tp = get_array_element_type(tp);
	if (! is_Primitive_type(tp))
		return NULL;
	mode = get_type_mode(tp);

	/* FIXME: This is too restrict, as the type char might be more the 8bits */
	if (!mode_is_int(mode) || get_mode_size_bits(mode) != get_mode_size_bits(mode_Bs))
		return NULL;

	n   = get_compound_ent_n_values(left);
	n_r = get_compound_ent_n_values(right);
	if (n_r < n)
		n = n_r;
	for (i = 0; i < n; ++i) {
		ir_node *irn;
		long v_l, v_r;
		tarval *tv;

		irn = get_compound_ent_value(left, i);
		if (! is_Const(irn))
			return NULL;
		tv = get_Const_tarval(irn);
		v_l = get_tarval_long(tv);

		irn = get_compound_ent_value(right, i);
		if (! is_Const(irn))
			return NULL;
		tv = get_Const_tarval(irn);
		v_r = get_tarval_long(tv);

		if (v_l < v_r) {
			res = -1;
			break;
		}
		if (v_l > v_r) {
			res = +1;
			break;
		}

		if (v_l == 0) {
			res = 0;
			break;
		}
	}
	if (i < n) {
		/* we found an end */
		tarval *tv = new_tarval_from_long(res, get_type_mode(res_tp));
		return new_Const_type(tv, res_tp);
	}
	return NULL;
}  /* eval_strcmp */

/**
 * Checks if an entity represents the empty string.
 *
 * @param ent     the entity
 *
 * @return non-zero if ent represents the empty string
 */
static int is_empty_string(ir_entity *ent) {
	ir_type *tp = get_entity_type(ent);
	ir_mode *mode;
	int     n;
	ir_node *irn;

	if (! is_Array_type(tp))
		return 0;
	tp = get_array_element_type(tp);
	if (! is_Primitive_type(tp))
		return 0;
	mode = get_type_mode(tp);

	/* FIXME: This is too restrict, as the type char might be more the 8bits */
	if (!mode_is_int(mode) || get_mode_size_bits(mode) != get_mode_size_bits(mode_Bs))
		return 0;

	n = get_compound_ent_n_values(ent);
	if (n < 1)
		return 0;
	irn = get_compound_ent_value(ent, 0);

	return is_Const(irn) && is_Const_null(irn);
}  /* is_empty_string */

/* A mapper for strcmp */
int i_mapper_strcmp(ir_node *call, void *ctx) {
	ir_node   *left    = get_Call_param(call, 0);
	ir_node   *right   = get_Call_param(call, 1);
	ir_node   *irn     = NULL;
	ir_node   *exc     = NULL;
	ir_node   *reg     = NULL;
	ir_node   *adr     = get_Call_ptr(call);
	ir_entity *ent     = get_SymConst_entity(adr);
	ir_type   *call_tp = get_entity_type(ent);
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

	if (left == right) {
		/* a strcmp(s, s) ==> 0 */
		ir_node *mem   = get_Call_mem(call);
		ir_mode *mode  = get_type_mode(res_tp);
		ir_node *block = get_nodes_block(call);

		irn = new_r_Const(current_ir_graph, block, mode, get_mode_null(mode));
		DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_STRCMP);
		replace_call(irn, call, mem, NULL, NULL);
		return 1;
	}
	ent_l = get_const_entity(left);
	ent_r = get_const_entity(right);

	if (ent_l != NULL && ent_r != NULL) {
		/* both entities are const, try to evaluate */
		irn = eval_strcmp(ent_l, ent_r, res_tp);
	} else if (ent_l != NULL) {
		if (is_empty_string(ent_l)) {
			/* s strcmp("", s) ==> -(*s)*/
			v = right;
			goto replace_by_call;
		}
	} else if (ent_r != NULL) {
		if (is_empty_string(ent_r)) {
			/* s strcmp(s, "") ==> (*s) */
			ir_node  *mem, *block;
			dbg_info *dbg;
			ir_mode  *mode;

			v = left;
replace_by_call:
			mem   = get_Call_mem(call);
			block = get_nodes_block(call);
			dbg   = get_irn_dbg_info(call);
			mode  = get_type_mode(char_tp);

			/* replace the strcmp by (*x) */
			irn = new_rd_Load(dbg, current_ir_graph, block, mem, v, mode);
			mem = new_r_Proj(current_ir_graph, block, irn, mode_M, pn_Load_M);
			exc = new_r_Proj(current_ir_graph, block, irn, mode_X, pn_Load_X_except);
			reg = new_r_Proj(current_ir_graph, block, irn, mode_X, pn_Load_X_regular);
			irn = new_r_Proj(current_ir_graph, block, irn, mode, pn_Load_res);

			/* conv to the result mode */
			mode = get_type_mode(res_tp);
			irn  = new_rd_Conv(dbg, current_ir_graph, block, irn, mode);

			if (v == right) {
				/* negate in the ("", s) case */
				irn = new_rd_Minus(dbg, current_ir_graph, block, irn, mode);
			}
		}
	}

	if (irn != NULL) {
		ir_node *mem = get_Call_mem(call);
		DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_STRCMP);
		replace_call(irn, call, mem, reg, exc);
		return 1;
	}

	return 0;
}  /* i_mapper_strcmp */

/* A mapper for strncmp */
int i_mapper_strncmp(ir_node *call, void *ctx) {
	ir_node *left  = get_Call_param(call, 0);
	ir_node *right = get_Call_param(call, 1);
	ir_node *len   = get_Call_param(call, 2);
	ir_node *irn;
	(void) ctx;

	if (left == right || (is_Const(len) && is_Const_null(len))) {
		/* a strncmp(s, s, len) ==> 0 OR
		   a strncmp(a, b, 0) ==> 0 */
		ir_node   *mem     = get_Call_mem(call);
		ir_node   *adr     = get_Call_ptr(call);
		ir_entity *ent     = get_SymConst_entity(adr);
		ir_type   *call_tp = get_entity_type(ent);
		ir_type   *res_tp  = get_method_res_type(call_tp, 0);
		ir_mode   *mode    = get_type_mode(res_tp);
		ir_node   *block   = get_nodes_block(call);

		irn = new_r_Const(current_ir_graph, block, mode, get_mode_null(mode));
		DBG_OPT_ALGSIM0(call, irn, FS_OPT_RTS_STRNCMP);
		replace_call(irn, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}  /* i_mapper_strncmp */

/* A mapper for memcpy */
int i_mapper_memcpy(ir_node *call, void *ctx) {
	ir_node *len = get_Call_param(call, 2);
	(void) ctx;

	if (is_Const(len) && is_Const_null(len)) {
		/* a memcpy(d, s, 0) ==> d */
		ir_node *mem = get_Call_mem(call);
		ir_node *dst = get_Call_param(call, 0);

		DBG_OPT_ALGSIM0(call, dst, FS_OPT_RTS_MEMCPY);
		replace_call(dst, call, mem, NULL, NULL);
		return 1;
	}
	return 0;
}  /* i_mapper_memcpy */

/* A mapper for memset */
int i_mapper_memset(ir_node *call, void *ctx) {
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
}  /* i_mapper_memset */

/**
 * Returns the result mode of a node.
 */
static ir_mode *get_irn_res_mode(ir_node *node) {
	switch (get_irn_opcode(node)) {
	case iro_Load:   return get_Load_mode(node);
	case iro_Quot:   return get_Quot_resmode(node);
	case iro_Div:    return get_Div_resmode(node);
	case iro_Mod:    return get_Mod_resmode(node);
	case iro_DivMod: return get_DivMod_resmode(node);
	default: return NULL;
	}
}  /* get_irn_res_mode */

#define LMAX(a, b) ((a) > (b) ? (a) : (b))

/* A mapper for mapping unsupported instructions to runtime calls. */
int i_mapper_RuntimeCall(ir_node *node, runtime_rt *rt) {
	int i, j, arity, first, n_param, n_res;
	long n_proj;
	ir_type *mtp;
	ir_node *mem, *bl, *call, *addr, *res_proj;
	ir_node **in;
	ir_graph *irg;
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
	irg     = current_ir_graph;

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

	n_res = get_method_n_ress(mtp);

	/* step 0: calculate the number of needed Proj's */
	n_proj = 0;
	n_proj = LMAX(n_proj, rt->mem_proj_nr + 1);
	n_proj = LMAX(n_proj, rt->regular_proj_nr + 1);
	n_proj = LMAX(n_proj, rt->exc_proj_nr + 1);
	n_proj = LMAX(n_proj, rt->exc_mem_proj_nr + 1);
	n_proj = LMAX(n_proj, rt->res_proj_nr + 1);

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
	bl = get_nodes_block(node);

	in = NULL;
	if (n_param > 0) {
		NEW_ARR_A(ir_node *, in, n_param);
		for (i = 0; i < n_param; ++i)
			in[i] = get_irn_n(node, first + i);
	}

	/* step 1: create the call */
	sym.entity_p = rt->ent;
	addr = new_r_SymConst(irg, bl, sym, symconst_addr_ent);
	call = new_rd_Call(get_irn_dbg_info(node), irg, bl, mem, addr, n_param, in, mtp);
	set_irn_pinned(call, get_irn_pinned(node));

	if (n_res > 0)
		res_proj = new_r_Proj(irg, bl, call, mode_T, pn_Call_T_result);
	else
		res_proj = NULL;

	if (n_proj > 0) {
		n_proj += n_res - 1;

		/* we are ready */
		turn_into_tuple(node, n_proj);

		for (i = 0; i < n_proj; ++i)
			set_Tuple_pred(node, i, new_r_Bad(irg));
		if (rt->mem_proj_nr >= 0)
			set_Tuple_pred(node, rt->mem_proj_nr, new_r_Proj(irg, bl, call, mode_M, pn_Call_M_regular));
		if (get_irn_op(mem) != op_NoMem) {
			/* Exceptions can only be handled with real memory */
			if (rt->regular_proj_nr >= 0)
				set_Tuple_pred(node, rt->regular_proj_nr, new_r_Proj(irg, bl, call, mode_X, pn_Call_X_regular));
			if (rt->exc_proj_nr >= 0)
				set_Tuple_pred(node, rt->exc_proj_nr, new_r_Proj(irg, bl, call, mode_X, pn_Call_X_except));
			if (rt->exc_mem_proj_nr >= 0)
				set_Tuple_pred(node, rt->mem_proj_nr, new_r_Proj(irg, bl, call, mode_M, pn_Call_M_except));
		}

		if (rt->res_proj_nr >= 0)
			for (i = 0; i < n_res; ++i)
				set_Tuple_pred(node, rt->res_proj_nr + i,
				new_r_Proj(irg, bl, res_proj, get_type_mode(get_method_res_type(mtp, i)), i));
			return 1;
	} else {
		/* only one return value supported */
		if (n_res > 0) {
			ir_mode *mode = get_type_mode(get_method_res_type(mtp, 0));

			res_proj = new_r_Proj(irg, bl, call, mode_T, pn_Call_T_result);
			res_proj = new_r_Proj(irg, bl, res_proj, mode, 0);

			exchange(node, res_proj);
			return 1;
		}
	}
	/* should not happen */
	return 0;
}  /* i_mapper_RuntimeCall */
