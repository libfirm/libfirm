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
#include "iredges_t.h"
#include "irgmod.h"
#include "irop_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irprog_t.h"
#include "iroptimize.h"
#include "lower_dw.h"
#include "array.h"
#include "panic.h"
#include "util.h"
#include "constbits.h"
#include "tv.h"

#include "ia32_new_nodes.h"
#include "bearch_ia32_t.h"
#include "gen_ia32_regalloc_if.h"
#include "begnuas.h"

typedef enum {
	no_carry = 0,
	can_carry,
	must_carry
} carry_result;

static ir_tarval *bitinfo_max(bitinfo *info)
{
	ir_tarval *z    = info->z;
	ir_tarval *o    = info->o;
	ir_mode   *mode = get_tarval_mode(z);
	ir_tarval *min  = get_mode_min(mode);

	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	return tarval_and(z, tarval_ornot(o, min));
}

static ir_tarval *bitinfo_min(bitinfo *info)
{
	ir_tarval *z    = info->z;
	ir_tarval *o    = info->o;
	ir_mode   *mode = get_tarval_mode(z);
	ir_tarval *min  = get_mode_min(mode);

	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	return tarval_or(o, tarval_and(z, min));
}

static carry_result lower_add_carry(ir_node *left, ir_node *right, ir_mode *mode)
{
	assert(!mode_is_signed(mode));

	bitinfo *bi_left = get_bitinfo(left);
	if (!bi_left) {
		return can_carry;
	}
	bitinfo *bi_right = get_bitinfo(right);
	// If we have bitinfo for one node, we should also have it for
	// the other
	assert(bi_right);

	ir_tarval    *lmin   = tarval_convert_to(bitinfo_min(bi_left),  mode);
	ir_tarval    *rmin   = tarval_convert_to(bitinfo_min(bi_right), mode);
	ir_tarval    *lmax   = tarval_convert_to(bitinfo_max(bi_left),  mode);
	ir_tarval    *rmax   = tarval_convert_to(bitinfo_max(bi_right), mode);
	carry_result  result = no_carry;

	int old_wrap_on_overflow = tarval_get_wrap_on_overflow();
	tarval_set_wrap_on_overflow(false);

	if (tarval_add(lmax, rmax) == tarval_bad) {
		result = can_carry;
		if (tarval_add(lmin, rmin) == tarval_bad) {
			result = must_carry;
		}
	}

	tarval_set_wrap_on_overflow(old_wrap_on_overflow);

	return result;
}

static carry_result lower_sub_borrow(ir_node *left, ir_node *right, ir_mode *mode)
{
	assert(!mode_is_signed(mode));

	bitinfo *bi_left = get_bitinfo(left);
	if (!bi_left) {
		return can_carry;
	}
	bitinfo *bi_right = get_bitinfo(right);
	// If we have bitinfo for one node, we should also have it for
	// the other
	assert(bi_right);

	ir_tarval    *lmin   = tarval_convert_to(bitinfo_min(bi_left),  mode);
	ir_tarval    *rmin   = tarval_convert_to(bitinfo_min(bi_right), mode);
	ir_tarval    *lmax   = tarval_convert_to(bitinfo_max(bi_left),  mode);
	ir_tarval    *rmax   = tarval_convert_to(bitinfo_max(bi_right), mode);
	carry_result  result = no_carry;

	int old_wrap_on_overflow = tarval_get_wrap_on_overflow();
	tarval_set_wrap_on_overflow(false);

	if (tarval_sub(lmin, rmax, NULL) == tarval_bad) {
		result = can_carry;
		if (tarval_sub(lmax, rmin, NULL) == tarval_bad) {
			result = must_carry;
		}
	}

	tarval_set_wrap_on_overflow(old_wrap_on_overflow);

	return result;
}

/**
 * lower 64bit addition: an 32bit add for the lower parts, an add with
 * carry for the higher parts. If the carry's value is known, fold it
 * into the upper add.
 */
static void ia32_lower_add64(ir_node *node, ir_mode *mode)
{
	dbg_info     *dbg        = get_irn_dbg_info(node);
	ir_node      *block      = get_nodes_block(node);
	ir_node      *left       = get_Add_left(node);
	ir_node      *right      = get_Add_right(node);
	ir_node      *left_low   = get_lowered_low(left);
	ir_node      *left_high  = get_lowered_high(left);
	ir_node      *right_low  = get_lowered_low(right);
	ir_node      *right_high = get_lowered_high(right);
	ir_mode      *low_mode   = get_irn_mode(left_low);
	ir_mode      *high_mode  = get_irn_mode(left_high);
	carry_result  cr         = lower_add_carry(left, right, low_mode);

	assert(get_irn_mode(left_low)  == get_irn_mode(right_low));
	assert(get_irn_mode(left_high) == get_irn_mode(right_high));

	if (cr == no_carry) {
		ir_node *add_low  = new_rd_Add(dbg, block, left_low,  right_low, low_mode);
		ir_node *add_high = new_rd_Add(dbg, block, left_high, right_high, high_mode);
		ir_set_dw_lowered(node, add_low, add_high);
	} else if (cr == must_carry && (is_Const(left_high) || is_Const(right_high))) {
		// We cannot assume that left_high and right_high form a normalized Add.
		ir_node *constant;
		ir_node *other;

		if (is_Const(left_high)) {
			constant = left_high;
			other    = right_high;
		} else {
			constant = right_high;
			other    = left_high;
		}

		ir_graph *irg            = get_irn_irg(right_high);
		ir_node  *one            = new_rd_Const(dbg, irg, get_mode_one(high_mode));
		ir_node  *const_plus_one = new_rd_Add(dbg, block, constant, one, high_mode);
		ir_node  *add_high       = new_rd_Add(dbg, block, other, const_plus_one, high_mode);
		ir_node  *add_low        = new_rd_Add(dbg, block, left_low, right_low, low_mode);
		ir_set_dw_lowered(node, add_low, add_high);
	} else {
		/* l_res = a_l + b_l */
		ir_node  *add_low    = new_bd_ia32_l_Add(dbg, block, left_low, right_low);
		ir_mode  *mode_flags = ia32_reg_classes[CLASS_ia32_flags].mode;
		ir_node  *res_low    = new_r_Proj(add_low, ia32_mode_gp, pn_ia32_l_Add_res);
		ir_node  *flags      = new_r_Proj(add_low, mode_flags, pn_ia32_l_Add_flags);

		/* h_res = a_h + b_h + carry */
		ir_node  *add_high
			= new_bd_ia32_l_Adc(dbg, block, left_high, right_high, flags, mode);
		ir_set_dw_lowered(node, res_low, add_high);
	}
}

/**
 * lower 64bit subtraction: a 32bit sub for the lower parts, a sub
 * with borrow for the higher parts. If the borrow's value is known,
 * fold it into the upper sub.
 */
static void ia32_lower_sub64(ir_node *node, ir_mode *mode)
{
	dbg_info     *dbg        = get_irn_dbg_info(node);
	ir_node      *block      = get_nodes_block(node);
	ir_node      *left       = get_Sub_left(node);
	ir_node      *right      = get_Sub_right(node);
	ir_node      *left_low   = get_lowered_low(left);
	ir_node      *left_high  = get_lowered_high(left);
	ir_node      *right_low  = get_lowered_low(right);
	ir_node      *right_high = get_lowered_high(right);
	ir_mode      *low_mode   = get_irn_mode(left_low);
	ir_mode      *high_mode  = get_irn_mode(left_high);
	carry_result  cr         = lower_sub_borrow(left, right, low_mode);

	assert(get_irn_mode(left_low)  == get_irn_mode(right_low));
	assert(get_irn_mode(left_high) == get_irn_mode(right_high));

	if (cr == no_carry) {
		ir_node *sub_low  = new_rd_Sub(dbg, block, left_low,  right_low, low_mode);
		ir_node *sub_high = new_rd_Sub(dbg, block, left_high, right_high, high_mode);
		ir_set_dw_lowered(node, sub_low, sub_high);
	} else if (cr == must_carry && (is_Const(left_high) || is_Const(right_high))) {
		ir_node  *sub_high;
		ir_graph *irg        = get_irn_irg(right_high);
		ir_node  *one        = new_rd_Const(dbg, irg, get_mode_one(high_mode));

		if (is_Const(right_high)) {
			ir_node *new_const = new_rd_Add(dbg, block, right_high, one, high_mode);
			sub_high = new_rd_Sub(dbg, block, left_high, new_const, high_mode);
		} else if (is_Const(left_high)) {
			ir_node *new_const = new_rd_Sub(dbg, block, left_high, one, high_mode);
			sub_high = new_rd_Sub(dbg, block, new_const, right_high, high_mode);
		} else {
			panic("logic error");
		}

		ir_node  *sub_low  = new_rd_Sub(dbg, block, left_low, right_low, low_mode);
		ir_set_dw_lowered(node, sub_low, sub_high);
	} else {
		/* l_res = a_l - b_l */
		ir_node  *sub_low    = new_bd_ia32_l_Sub(dbg, block, left_low, right_low);
		ir_mode  *mode_flags = ia32_reg_classes[CLASS_ia32_flags].mode;
		ir_node  *res_low    = new_r_Proj(sub_low, ia32_mode_gp, pn_ia32_l_Sub_res);
		ir_node  *flags      = new_r_Proj(sub_low, mode_flags, pn_ia32_l_Sub_flags);

		/* h_res = a_h - b_h - carry */
		ir_node  *sub_high
			= new_bd_ia32_l_Sbb(dbg, block, left_high, right_high, flags, mode);
		ir_set_dw_lowered(node, res_low, sub_high);
	}
}

/**
 * Checks whether node high is a sign extension of low.
 */
static bool is_sign_extend(ir_node *low, ir_node *high)
{
	if (is_Shrs(high)) {
		ir_node *high_r = get_Shrs_right(high);
		if (!is_Const(high_r)) return false;

		ir_tarval *shift_count = get_Const_tarval(high_r);
		if (!tarval_is_long(shift_count))       return false;
		if (get_tarval_long(shift_count) != 31) return false;

		ir_node *high_l = get_Shrs_left(high);

		if (is_Conv(low)    && get_Conv_op(low)    == high_l) return true;
		if (is_Conv(high_l) && get_Conv_op(high_l) == low)    return true;
	} else if (is_Const(low) && is_Const(high)) {
		ir_tarval *tl = get_Const_tarval(low);
		ir_tarval *th = get_Const_tarval(high);

		if (tarval_is_long(th) && tarval_is_long(tl)) {
			long l = get_tarval_long(tl);
			long h = get_tarval_long(th);

			return (h == 0  && l >= 0) || (h == -1 && l <  0);
		}
	}

	return false;
}

/**
 * lower 64bit Mul operation.
 */
static void ia32_lower_mul64(ir_node *node, ir_mode *mode)
{
	dbg_info *dbg        = get_irn_dbg_info(node);
	ir_node  *block      = get_nodes_block(node);
	ir_node  *left       = get_Mul_left(node);
	ir_node  *right      = get_Mul_right(node);
	ir_node  *left_low   = get_lowered_low(left);
	ir_node  *left_high  = get_lowered_high(left);
	ir_node  *right_low  = get_lowered_low(right);
	ir_node  *right_high = get_lowered_high(right);

	/*
		EDX:EAX = left_low * right_low
		l_res   = EAX

		t1 = right_low * left_high
		t2 = t1 + EDX
		t3 = left_low * right_high
		h_res = t2 + t3
	*/

	/* handle the often used case of 32x32=64 mul */
	ir_node *h_res;
	ir_node *l_res;
	if (is_sign_extend(left_low, left_high)
	    && is_sign_extend(right_low, right_high)) {
		ir_node *mul = new_bd_ia32_l_IMul(dbg, block, left_low, right_low);
		h_res = new_rd_Proj(dbg, mul, mode, pn_ia32_l_IMul_res_high);
		l_res = new_rd_Proj(dbg, mul, ia32_mode_gp, pn_ia32_l_IMul_res_low);
	} else {
		/* note that zero extension is handled hare efficiently */
		ir_node *mul  = new_bd_ia32_l_Mul(dbg, block, left_low, right_low);
		ir_node *pEDX = new_rd_Proj(dbg, mul, mode, pn_ia32_l_Mul_res_high);
		l_res = new_rd_Proj(dbg, mul, ia32_mode_gp, pn_ia32_l_Mul_res_low);

		ir_node *right_lowc = new_rd_Conv(dbg, block, right_low, mode);
		ir_node *mul1 = new_rd_Mul(dbg, block, left_high, right_lowc, mode);
		ir_node *add        = new_rd_Add(dbg, block, mul1, pEDX, mode);
		ir_node *left_lowc  = new_rd_Conv(dbg, block, left_low, mode);
		ir_node *mul2 = new_rd_Mul(dbg, block, left_lowc, right_high, mode);
		h_res = new_rd_Add(dbg, block, add, mul2, mode);
	}
	ir_set_dw_lowered(node, l_res, h_res);
}

/**
 * lower 64bit minus operation
 */
static void ia32_lower_minus64(ir_node *node, ir_mode *mode)
{
	dbg_info *dbg     = get_irn_dbg_info(node);
	ir_node  *block   = get_nodes_block(node);
	ir_node  *op      = get_Minus_op(node);
	ir_node  *op_low  = get_lowered_low(op);
	ir_node  *op_high = get_lowered_high(op);
	ir_node  *minus   = new_bd_ia32_l_Minus64(dbg, block, op_low, op_high);
	ir_node  *l_res   = new_r_Proj(minus, ia32_mode_gp, pn_ia32_Minus64_res_low);
	ir_node  *h_res   = new_r_Proj(minus, mode, pn_ia32_Minus64_res_high);
	ir_set_dw_lowered(node, l_res, h_res);
}

/**
 * lower 64bit conversions
 */
static void ia32_lower_conv64(ir_node *node, ir_mode *mode)
{
	dbg_info  *dbg       = get_irn_dbg_info(node);
	ir_node   *op        = get_Conv_op(node);
	ir_mode   *mode_from = get_irn_mode(op);
	ir_mode   *mode_to   = get_irn_mode(node);

	if (mode_is_float(mode_from) && get_mode_size_bits(mode_to) == 64
	    && get_mode_arithmetic(mode_to) == irma_twos_complement) {
		/* We have a Conv float -> long long here */
		ir_node *float_to_ll;
		ir_node *l_res;
		ir_node *h_res;
		if (mode_is_signed(mode)) {
			/* convert from float to signed 64bit */
			ir_node *block = get_nodes_block(node);
			float_to_ll = new_bd_ia32_l_FloattoLL(dbg, block, op);
			l_res = new_r_Proj(float_to_ll, ia32_mode_gp,
			                   pn_ia32_l_FloattoLL_res_low);
			h_res = new_r_Proj(float_to_ll, mode,
							   pn_ia32_l_FloattoLL_res_high);
		} else {
			/* Convert from float to unsigned 64bit. */
			ir_graph  *irg = get_irn_irg(node);
			ir_tarval *flt_tv
				= new_tarval_from_str("9223372036854775808", 19, ia32_mode_E);
			ir_node   *flt_corr  = new_r_Const(irg, flt_tv);

			ir_node *lower_blk = part_block_dw(node);
			ir_node *upper_blk = get_nodes_block(node);
			set_dw_control_flow_changed();

			ir_node *opc  = new_rd_Conv(dbg, upper_blk, op, ia32_mode_E);
			ir_node *cmp  = new_rd_Cmp(dbg, upper_blk, opc, flt_corr,
			                           ir_relation_less);
			ir_node *cond = new_rd_Cond(dbg, upper_blk, cmp);
			ir_node *in[] = {
				new_r_Proj(cond, mode_X, pn_Cond_true),
				new_r_Proj(cond, mode_X, pn_Cond_false)
			};
			ir_node *blk   = new_r_Block(irg, 1, &in[1]);
			in[1] = new_r_Jmp(blk);

			set_irn_in(lower_blk, 2, in);

			/* create to Phis */
			ir_node *phi_in[] = {
				new_r_Const_null(irg, mode),
				new_r_Const_long(irg, mode, 0x80000000)
			};
			ir_node *int_phi
				= new_r_Phi(lower_blk, ARRAY_SIZE(phi_in), phi_in, mode);

			ir_node *fphi_in[] = {
				opc,
				new_rd_Sub(dbg, upper_blk, opc, flt_corr, ia32_mode_E)
			};
			ir_node *flt_phi
				= new_r_Phi(lower_blk, ARRAY_SIZE(fphi_in), fphi_in,
				            ia32_mode_E);

			/* fix Phi links for next part_block() */
			if (is_Phi(int_phi))
				add_Block_phi(lower_blk, int_phi);
			if (is_Phi(flt_phi))
				add_Block_phi(lower_blk, flt_phi);

			float_to_ll = new_bd_ia32_l_FloattoLL(dbg, lower_blk, flt_phi);
			l_res = new_r_Proj(float_to_ll, ia32_mode_gp,
							   pn_ia32_l_FloattoLL_res_low);
			h_res = new_r_Proj(float_to_ll, mode,
							   pn_ia32_l_FloattoLL_res_high);
			h_res = new_rd_Add(dbg, lower_blk, h_res, int_phi, mode);

			/* move the call and its Proj's to the lower block */
			set_nodes_block(node, lower_blk);
			for (ir_node *proj = (ir_node*)get_irn_link(node); proj != NULL;
			     proj = (ir_node*)get_irn_link(proj)) {
				set_nodes_block(proj, lower_blk);
			}
		}
		ir_set_dw_lowered(node, l_res, h_res);
	} else if (get_mode_size_bits(mode_from) == 64
	           && get_mode_arithmetic(mode_from) == irma_twos_complement
	           && mode_is_float(mode_to)) {
		/* We have a Conv long long -> float here */
		ir_node *op_low  = get_lowered_low(op);
		ir_node *op_high = get_lowered_high(op);
		ir_node *block   = get_nodes_block(node);
		ir_node *ll_to_float
			= new_bd_ia32_l_LLtoFloat(dbg, block, op_high, op_low, mode_to);

		exchange(node, ll_to_float);
	} else {
		ir_default_lower_dw_Conv(node, mode);
	}
}

static ir_entity *ia32_create_intrinsic_fkt(ir_type *method, const ir_op *op,
                                            const ir_mode *imode,
                                            const ir_mode *omode, void *context)
{
	(void)omode;
	(void)context;

	const char *name;
	if (op == op_Div) {
		name = mode_is_signed(imode) ? "__divdi3" : "__udivdi3";
	} else if (op == op_Mod) {
		name = mode_is_signed(imode) ? "__moddi3" : "__umoddi3";
	} else {
		panic("ia32: Unexpected lowering of 64bit op %s", get_op_name(op));
	}
	return create_compilerlib_entity(new_id_from_str(name), method);
}

void ia32_lower64(void)
{
	/* perform doubleword lowering */
	ir_mode *word_unsigned = ia32_reg_classes[CLASS_ia32_gp].mode;
	ir_mode *word_signed   = find_signed_mode(word_unsigned);
	lwrdw_param_t lower_dw_params = {
		ia32_create_intrinsic_fkt,
		NULL,
		word_unsigned,
		word_signed,
		64,    /* doubleword size */
		be_is_big_endian(),
	};

	ir_prepare_dw_lowering(&lower_dw_params);
	ir_register_dw_lower_function(op_Add,   ia32_lower_add64);
	ir_register_dw_lower_function(op_Sub,   ia32_lower_sub64);
	ir_register_dw_lower_function(op_Mul,   ia32_lower_mul64);
	ir_register_dw_lower_function(op_Minus, ia32_lower_minus64);
	ir_register_dw_lower_function(op_Conv,  ia32_lower_conv64);
	ir_lower_dw_ops();
}
