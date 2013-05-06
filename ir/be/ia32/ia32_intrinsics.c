/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file implements the mapping of 64Bit intrinsic
 *              functions to code or library calls.
 * @author      Michael Beck
 */
#include "iredges.h"
#include "irgmod.h"
#include "irop.h"
#include "irnode_t.h"
#include "ircons.h"
#include "irprog_t.h"
#include "iroptimize.h"
#include "lower_dw.h"
#include "array.h"
#include "error.h"
#include "util.h"

#include "ia32_new_nodes.h"
#include "bearch_ia32_t.h"
#include "gen_ia32_regalloc_if.h"
#include "begnuas.h"

/** The array of all intrinsics that must be mapped. */
static i_record *intrinsics;

/** An array to cache all entities. */
static ir_entity *i_ents[iro_last + 1];

/*
 * Maps all intrinsic calls that the backend support
 * and map all instructions the backend did not support
 * to runtime calls.
 */
void ia32_handle_intrinsics(void)
{
	if (intrinsics && ARR_LEN(intrinsics) > 0) {
		lower_intrinsics(intrinsics, ARR_LEN(intrinsics), /*part_block_used=*/1);
	}
}

#define BINOP_Left_Low   0
#define BINOP_Left_High  1
#define BINOP_Right_Low  2
#define BINOP_Right_High 3

/**
 * Reroute edges from the pn_Call_T_result proj of a call.
 *
 * @param resproj  the pn_Call_T_result Proj
 * @param l_res    the lower 32 bit result
 * @param h_res    the upper 32 bit result or NULL
 */
static void reroute_result(ir_node *resproj, ir_node *l_res, ir_node *h_res)
{
	foreach_out_edge_safe(resproj, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		long    pn    = get_Proj_proj(proj);

		if (pn == 0) {
			edges_reroute(proj, l_res);
		} else if (pn == 1 && h_res != NULL) {
			edges_reroute(proj, h_res);
		} else {
			panic("Unsupported Result-Proj from Call found");
		}
	}
}

/**
 * Replace a call be a tuple of l_res, h_res.
 *
 * @param call   the call node to replace
 * @param l_res  the lower 32 bit result
 * @param h_res  the upper 32 bit result or NULL
 * @param irg    the graph to replace on
 * @param block  the block to replace on (always the call block)
 */
static void resolve_call(ir_node *call, ir_node *l_res, ir_node *h_res, ir_graph *irg, ir_node *block)
{
	ir_node *jmp, *res, *in[2];
	ir_node *nomem = get_irg_no_mem(irg);
	int     old_cse;

	if (edges_activated(irg)) {
		/* use rerouting to prevent some warning in the backend */
		foreach_out_edge_safe(call, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			pn_Call pn    = (pn_Call)get_Proj_proj(proj);

			switch (pn) {
			case pn_Call_X_regular:
				/* Beware:
				 * We do not check here if this call really has exception and regular Proj's.
				 * new_r_Jmp might than be CSEd with the real exit jmp and then bad things happen
				 * (in movgen.c from 186.crafty for example).
				 * So be sure the newly created Jmp cannot CSE.
				 */
				old_cse = get_opt_cse();
				set_opt_cse(0);
				jmp = new_r_Jmp(block);
				set_opt_cse(old_cse);
				edges_reroute(proj, jmp);
				break;

			case pn_Call_X_except:
				/* should not happen here */
				edges_reroute(proj, new_r_Bad(irg, mode_X));
				break;
			case pn_Call_M:
				/* should not happen here */
				edges_reroute(proj, nomem);
				break;
			case pn_Call_T_result:
				reroute_result(proj, l_res, h_res);
				break;
			default:
				panic("Wrong Proj from Call");
			}
			kill_node(proj);
		}
		kill_node(call);
	} else {
		/* no edges, build Tuple */
		if (h_res == NULL)
			res = l_res;
		else {
			in[0] = l_res;
			in[1] = h_res;
			res = new_r_Tuple(block, 2, in);
		}

		/*
		 * Beware:
		 * We do not check here if this call really has exception and regular Proj's.
		 * new_r_Jmp might than be CSEd with the real exit jmp and then bad things happen
		 * (in movgen.c from 186.crafty for example).
		 * So be sure the newly created Jmp cannot CSE.
		 */
		old_cse = get_opt_cse();
		set_opt_cse(0);
		jmp = new_r_Jmp(block);
		set_opt_cse(old_cse);

		ir_node *const in[] = {
			[pn_Call_M]         = nomem,
			[pn_Call_T_result]  = res,
			[pn_Call_X_regular] = jmp,
			[pn_Call_X_except]  = new_r_Bad(irg, mode_X),
		};
		turn_into_tuple(call, ARRAY_SIZE(in), in);
	}
}

/**
 * Map an Add (a_l, a_h, b_l, b_h)
 */
static int map_Add(ir_node *call, void *ctx)
{
	dbg_info *dbg        = get_irn_dbg_info(call);
	ir_node  *block      = get_nodes_block(call);
	ir_node  **params    = get_Call_param_arr(call);
	ir_type  *method     = get_Call_type(call);
	ir_node  *a_l        = params[BINOP_Left_Low];
	ir_node  *a_h        = params[BINOP_Left_High];
	ir_node  *b_l        = params[BINOP_Right_Low];
	ir_node  *b_h        = params[BINOP_Right_High];
	ir_mode  *l_mode     = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_mode     = get_type_mode(get_method_res_type(method, 1));
	ir_mode  *mode_flags = ia32_reg_classes[CLASS_ia32_flags].mode;
	ir_node  *add_low, *add_high, *flags;
	ir_node  *l_res, *h_res;
	(void) ctx;

	/* l_res = a_l + b_l */
	/* h_res = a_h + b_h + carry */

	add_low  = new_bd_ia32_l_Add(dbg, block, a_l, b_l, mode_T);
	flags    = new_r_Proj(add_low, mode_flags, pn_ia32_flags);
	add_high = new_bd_ia32_l_Adc(dbg, block, a_h, b_h, flags, h_mode);

	l_res = new_r_Proj(add_low, l_mode, pn_ia32_res);
	h_res = add_high;

	resolve_call(call, l_res, h_res, current_ir_graph, block);
	return 1;
}

/**
 * Map a Sub (a_l, a_h, b_l, b_h)
 */
static int map_Sub(ir_node *call, void *ctx)
{
	dbg_info *dbg        = get_irn_dbg_info(call);
	ir_node  *block      = get_nodes_block(call);
	ir_node  **params    = get_Call_param_arr(call);
	ir_type  *method     = get_Call_type(call);
	ir_node  *a_l        = params[BINOP_Left_Low];
	ir_node  *a_h        = params[BINOP_Left_High];
	ir_node  *b_l        = params[BINOP_Right_Low];
	ir_node  *b_h        = params[BINOP_Right_High];
	ir_mode  *l_mode     = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_mode     = get_type_mode(get_method_res_type(method, 1));
	ir_mode  *mode_flags = ia32_reg_classes[CLASS_ia32_flags].mode;
	ir_node  *sub_low, *sub_high, *flags;
	ir_node  *l_res, *h_res;
	(void) ctx;

	/* l_res = a_l - b_l */
	/* h_res = a_h - b_h - carry */

	sub_low  = new_bd_ia32_l_Sub(dbg, block, a_l, b_l, mode_T);
	flags    = new_r_Proj(sub_low, mode_flags, pn_ia32_flags);
	sub_high = new_bd_ia32_l_Sbb(dbg, block, a_h, b_h, flags, h_mode);

	l_res = new_r_Proj(sub_low, l_mode, pn_ia32_res);
	h_res = sub_high;

	resolve_call(call, l_res, h_res, current_ir_graph, block);
	return 1;
}

/**
 * Checks where node high is a sign extension of low.
 */
static int is_sign_extend(ir_node *low, ir_node *high)
{
	if (is_Shrs(high)) {
		ir_node   *high_l;
		ir_node   *high_r;
		ir_tarval *shift_count;

		high_r = get_Shrs_right(high);
		if (!is_Const(high_r)) return 0;

		shift_count = get_Const_tarval(high_r);
		if (!tarval_is_long(shift_count))       return 0;
		if (get_tarval_long(shift_count) != 31) return 0;

		high_l = get_Shrs_left(high);

		if (is_Conv(low)    && get_Conv_op(low)    == high_l) return 1;
		if (is_Conv(high_l) && get_Conv_op(high_l) == low)    return 1;
	} else if (is_Const(low) && is_Const(high)) {
		ir_tarval *tl = get_Const_tarval(low);
		ir_tarval *th = get_Const_tarval(high);

		if (tarval_is_long(th) && tarval_is_long(tl)) {
			long l = get_tarval_long(tl);
			long h = get_tarval_long(th);

			return (h == 0  && l >= 0) || (h == -1 && l <  0);
		}
	}

	return 0;
}

/**
 * Map a Mul (a_l, a_h, b_l, b_h)
 */
static int map_Mul(ir_node *call, void *ctx)
{
	dbg_info *dbg     = get_irn_dbg_info(call);
	ir_node  *block   = get_nodes_block(call);
	ir_node  **params = get_Call_param_arr(call);
	ir_type  *method  = get_Call_type(call);
	ir_node  *a_l     = params[BINOP_Left_Low];
	ir_node  *a_h     = params[BINOP_Left_High];
	ir_node  *b_l     = params[BINOP_Right_Low];
	ir_node  *b_h     = params[BINOP_Right_High];
	ir_mode  *l_mode  = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_mode  = get_type_mode(get_method_res_type(method, 1));
	ir_node  *l_res, *h_res, *mul, *pEDX, *add;
	(void) ctx;

	/*
		EDX:EAX = a_l * b_l
		l_res   = EAX

		t1 = b_l * a_h
		t2 = t1 + EDX
		t3 = a_l * b_h
		h_res = t2 + t3
	*/

	/* handle the often used case of 32x32=64 mul */
	if (is_sign_extend(a_l, a_h) && is_sign_extend(b_l, b_h)) {
		mul   = new_bd_ia32_l_IMul(dbg, block, a_l, b_l);
		h_res = new_rd_Proj(dbg, mul, h_mode, pn_ia32_l_IMul_res_high);
		l_res = new_rd_Proj(dbg, mul, l_mode, pn_ia32_l_IMul_res_low);
	} else {
		/* note that zero extension is handled hare efficiently */
		mul   = new_bd_ia32_l_Mul(dbg, block, a_l, b_l);
		pEDX  = new_rd_Proj(dbg, mul, h_mode, pn_ia32_l_Mul_res_high);
		l_res = new_rd_Proj(dbg, mul, l_mode, pn_ia32_l_Mul_res_low);

		b_l   = new_rd_Conv(dbg, block, b_l, h_mode);
		mul   = new_rd_Mul( dbg, block, a_h, b_l, h_mode);
		add   = new_rd_Add( dbg, block, mul, pEDX, h_mode);
		a_l   = new_rd_Conv(dbg, block, a_l, h_mode);
		mul   = new_rd_Mul( dbg, block, a_l, b_h, h_mode);
		h_res = new_rd_Add( dbg, block, add, mul, h_mode);
	}
	resolve_call(call, l_res, h_res, current_ir_graph, block);

	return 1;
}

/**
 * Map a Minus (a_l, a_h)
 */
static int map_Minus(ir_node *call, void *ctx)
{
	dbg_info *dbg     = get_irn_dbg_info(call);
	ir_node  *block   = get_nodes_block(call);
	ir_node  **params = get_Call_param_arr(call);
	ir_type  *method  = get_Call_type(call);
	ir_node  *a_l     = params[BINOP_Left_Low];
	ir_node  *a_h     = params[BINOP_Left_High];
	ir_mode  *l_mode  = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_mode  = get_type_mode(get_method_res_type(method, 1));
	ir_node  *l_res, *h_res, *res;
	(void) ctx;

	res   = new_bd_ia32_Minus64Bit(dbg, block, a_l, a_h);
	l_res = new_r_Proj(res, l_mode, pn_ia32_Minus64Bit_low_res);
	h_res = new_r_Proj(res, h_mode, pn_ia32_Minus64Bit_high_res);

	resolve_call(call, l_res, h_res, current_ir_graph, block);

	return 1;
}

#define ID(x) new_id_from_chars(x, sizeof(x)-1)

/**
 * Maps a Div. Change into a library call.
 */
static int map_Div(ir_node *call, void *ctx)
{
	ia32_intrinsic_env_t *env = (ia32_intrinsic_env_t*)ctx;
	ir_type   *method    = get_Call_type(call);
	ir_mode   *h_mode    = get_type_mode(get_method_res_type(method, 1));
	ir_node   *ptr;
	ir_entity *ent;
	ir_graph  *irg = get_irn_irg(call);
	symconst_symbol sym;

	if (mode_is_signed(h_mode)) {
		/* 64bit signed Division */
		ent = env->divdi3;
		if (ent == NULL) {
			/* create library entity */
			ident *id = ID("__divdi3");
			ent = env->divdi3 = create_compilerlib_entity(id, method);
		}
	} else {
		/* 64bit unsigned Division */
		ent = env->udivdi3;
		if (ent == NULL) {
			/* create library entity */
			ident *id = ID("__udivdi3");
			ent = env->udivdi3 = create_compilerlib_entity(id, method);
		}
	}

	ptr = get_Call_ptr(call);
	sym.entity_p = ent;
	ptr = new_r_SymConst(irg, get_irn_mode(ptr), sym, symconst_addr_ent);
	set_Call_ptr(call, ptr);

	return 1;
}

/**
 * Maps a Mod. Change into a library call
 */
static int map_Mod(ir_node *call, void *ctx)
{
	ia32_intrinsic_env_t *env = (ia32_intrinsic_env_t*)ctx;
	ir_type   *method    = get_Call_type(call);
	ir_mode   *h_mode    = get_type_mode(get_method_res_type(method, 1));
	ir_node   *ptr;
	ir_entity *ent;
	ir_graph  *irg = get_irn_irg(call);
	symconst_symbol sym;

	if (mode_is_signed(h_mode)) {
		/* 64bit signed Modulo */
		ent = env->moddi3;
		if (ent == NULL) {
			/* create library entity */
			ident *id = ID("__moddi3");
			ent = env->moddi3 = create_compilerlib_entity(id, method);
		}
	} else {
		/* 64bit signed Modulo */
		ent = env->umoddi3;
		if (ent == NULL) {
			/* create library entity */
			ident *id = ID("__umoddi3");
			ent = env->umoddi3 = create_compilerlib_entity(id, method);
		}
	}

	ptr = get_Call_ptr(call);
	sym.entity_p = ent;
	ptr = new_r_SymConst(irg, get_irn_mode(ptr), sym, symconst_addr_ent);
	set_Call_ptr(call, ptr);

	return 1;
}

/**
 * Maps a Conv.
 */
static int map_Conv(ir_node *call, void *ctx)
{
	ir_graph  *irg     = current_ir_graph;
	dbg_info  *dbg     = get_irn_dbg_info(call);
	ir_node   *block   = get_nodes_block(call);
	ir_node   **params = get_Call_param_arr(call);
	ir_type   *method  = get_Call_type(call);
	int       n        = get_Call_n_params(call);
	ir_node   *l_res, *h_res;
	(void) ctx;

	if (n == 1) {
		ir_node *float_to_ll;

		/* We have a Conv float -> long long here */
		ir_node *a_f        = params[0];
		ir_mode *l_res_mode = get_type_mode(get_method_res_type(method, 0));
		ir_mode *h_res_mode = get_type_mode(get_method_res_type(method, 1));

		assert(mode_is_float(get_irn_mode(a_f)) && "unexpected Conv call");

		if (mode_is_signed(h_res_mode)) {
			/* convert from float to signed 64bit */
			float_to_ll = new_bd_ia32_l_FloattoLL(dbg, block, a_f);

			l_res = new_r_Proj(float_to_ll, l_res_mode,
			                   pn_ia32_l_FloattoLL_res_low);
			h_res = new_r_Proj(float_to_ll, h_res_mode,
							   pn_ia32_l_FloattoLL_res_high);
		} else {
			/* Convert from float to unsigned 64bit. */
			ir_tarval *flt_tv   = new_tarval_from_str("9223372036854775808", 19, ia32_mode_E);
			ir_node   *flt_corr = new_r_Const(irg, flt_tv);
			ir_node   *lower_blk = block;
			ir_node   *upper_blk;
			ir_node   *cmp, *proj, *cond, *blk, *int_phi, *flt_phi;
			ir_node   *in[2];

			part_block(call);
			upper_blk = get_nodes_block(call);

			a_f   = new_rd_Conv(dbg, upper_blk, a_f, ia32_mode_E);
			cmp   = new_rd_Cmp(dbg, upper_blk, a_f, flt_corr, ir_relation_less);
			cond  = new_rd_Cond(dbg, upper_blk, cmp);
			in[0] = new_r_Proj(cond, mode_X, pn_Cond_true);
			in[1] = new_r_Proj(cond, mode_X, pn_Cond_false);
			blk   = new_r_Block(irg, 1, &in[1]);
			in[1] = new_r_Jmp(blk);

			set_irn_in(lower_blk, 2, in);

			/* create to Phis */
			in[0] = new_r_Const(irg, get_mode_null(h_res_mode));
			in[1] = new_r_Const_long(irg, h_res_mode, 0x80000000);

			int_phi = new_r_Phi(lower_blk, 2, in, h_res_mode);

			in[0] = a_f;
			in[1] = new_rd_Sub(dbg, upper_blk, a_f, flt_corr, ia32_mode_E);

			flt_phi = new_r_Phi(lower_blk, 2, in, ia32_mode_E);

			/* fix Phi links for next part_block() */
			if (is_Phi(int_phi))
				add_Block_phi(lower_blk, int_phi);
			if (is_Phi(flt_phi))
				add_Block_phi(lower_blk, flt_phi);

			float_to_ll = new_bd_ia32_l_FloattoLL(dbg, lower_blk, flt_phi);

			l_res = new_r_Proj(float_to_ll, l_res_mode,
							   pn_ia32_l_FloattoLL_res_low);
			h_res = new_r_Proj(float_to_ll, h_res_mode,
							   pn_ia32_l_FloattoLL_res_high);

			h_res = new_rd_Add(dbg, lower_blk, h_res, int_phi, h_res_mode);

			/* move the call and its Proj's to the lower block */
			set_nodes_block(call, lower_blk);

			for (proj = (ir_node*)get_irn_link(call); proj != NULL;
			     proj = (ir_node*)get_irn_link(proj)) {
				set_nodes_block(proj, lower_blk);
			}
			block = lower_blk;
		}
		/* lower the call */
		resolve_call(call, l_res, h_res, irg, block);
	} else if (n == 2) {
		ir_node *ll_to_float;

		/* We have a Conv long long -> float here */
		ir_node *a_l       = params[BINOP_Left_Low];
		ir_node *a_h       = params[BINOP_Left_High];
		ir_mode *fres_mode = get_type_mode(get_method_res_type(method, 0));

		assert(! mode_is_float(get_irn_mode(a_l))
				&& ! mode_is_float(get_irn_mode(a_h)));

		ll_to_float = new_bd_ia32_l_LLtoFloat(dbg, block, a_h, a_l, fres_mode);

		/* lower the call */
		resolve_call(call, ll_to_float, NULL, irg, block);
	} else {
		panic("unexpected Conv call %+F", call);
	}

	return 1;
}

/* Ia32 implementation of intrinsic mapping. */
ir_entity *ia32_create_intrinsic_fkt(ir_type *method, const ir_op *op,
                                     const ir_mode *imode, const ir_mode *omode,
                                     void *context)
{
	i_record      elt;
	ir_entity     **ent = NULL;
	i_mapper_func mapper;

	if (! intrinsics)
		intrinsics = NEW_ARR_F(i_record, 0);

	switch (get_op_code(op)) {
	case iro_Add:
		ent    = &i_ents[iro_Add];
		mapper = map_Add;
		break;
	case iro_Sub:
		ent    = &i_ents[iro_Sub];
		mapper = map_Sub;
		break;
	case iro_Mul:
		ent    = &i_ents[iro_Mul];
		mapper = map_Mul;
		break;
	case iro_Minus:
		ent    = &i_ents[iro_Minus];
		mapper = map_Minus;
		break;
	case iro_Div:
		ent    = &i_ents[iro_Div];
		mapper = map_Div;
		break;
	case iro_Mod:
		ent    = &i_ents[iro_Mod];
		mapper = map_Mod;
		break;
	case iro_Conv:
		ent    = &i_ents[iro_Conv];
		mapper = map_Conv;
		break;
	default:
		fprintf(stderr, "FIXME: unhandled op for ia32 intrinsic function %s\n", get_id_str(op->name));
		return def_create_intrinsic_fkt(method, op, imode, omode, context);
	}

	if (ent && ! *ent) {
		ident *id = id_mangle(ID("L"), get_op_ident(op));
		*ent = new_entity(get_glob_type(), id, method);
		set_entity_visibility(*ent, ir_visibility_private);
	}

	elt.i_call.kind     = INTRINSIC_CALL;
	elt.i_call.i_ent    = *ent;
	elt.i_call.i_mapper = mapper;
	elt.i_call.ctx      = context;
	elt.i_call.link     = NULL;

	ARR_APP1(i_record, intrinsics, elt);
	return *ent;
}
