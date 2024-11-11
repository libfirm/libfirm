/*
 * This file is part of libFirm.
 * Copyright (C) 2013 University of Karlsruhe.
 */

/**
 * @file
 * @author  Andreas Seltenreich
 * @brief   Compute don't care bits.
 *
 * This analysis computes a conservative minimum fixpoint of tarvals
 * determining whether bits in integer mode nodes are relevant(1) or
 * irrelevant(0) for the program's computation.
 *
 * In combination with the VRP bitinfo, it ought to become the basis
 * for an improved Conv optimization.  It also allows finding
 * additional constants (vrp->z ^ vrp->o & dc == 0).
 */
#include "dca.h"

#include "constbits.h"
#include "debug.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "pdeq.h"
#include "tv.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static deq_t worklist;

/**
 * Set cared for bits in irn, possibly putting it on the worklist.
 * care == 0 is short for unqualified caring.
 */
static void care_for(ir_node *irn, ir_tarval *care)
{
	if (care == NULL)
		care = tarval_b_true;

	/* Assume worst case if modes don't match and care has bits set. */
	ir_tarval *old_care = (ir_tarval *)get_irn_link(irn);
	ir_mode   *mode     = get_tarval_mode(old_care);
	if (mode != get_tarval_mode(care))
		care = tarval_is_null(care) ? get_mode_null(mode)
		                            : get_mode_all_one(mode);

	if (mode_is_int(mode) || mode == mode_b)
		care = tarval_or(care, old_care);

	if (care != old_care) {
		DBG((dbg, LEVEL_3, "queueing %+F: %T->%T\n", irn, old_care, care));
		assert(old_care != tarval_b_true || care == tarval_b_true);
		set_irn_link(irn, (void *)care);
		deq_push_pointer_right(&worklist, irn);
	} else {
		DBG((dbg, LEVEL_3, "no change on %+F: %T\n", irn, old_care, care));
	}
}

/** Creates a bit mask that have the lsb and all more significant bits set. */
static ir_tarval *create_lsb_mask(ir_tarval *tv)
{
	return tarval_or(tv, tarval_neg(tv));
}

/** Creates a bit mask that have the msb and all less significant bits set. */
static ir_tarval *create_msb_mask(ir_tarval *tv)
{
	unsigned shift_amount = 1;
	for (int msb = get_tarval_highest_bit(tv); msb != 0; msb /= 2) {
		tv            = tarval_or(tv, tarval_shr_unsigned(tv, shift_amount));
		shift_amount *= 2;
	}

	return tv;
}

/** Create a mask with the bits that are relevant for shifting the given mode. */
static ir_tarval *create_modulo_shift_mask(ir_mode *mode, ir_mode *dest_mode)
{
	long modulo_shift = get_mode_modulo_shift(mode);
	if (modulo_shift == 0)
		return NULL;

	assert(is_po2_or_zero(modulo_shift));
	return new_tarval_from_long(modulo_shift - 1, dest_mode);
}

/** Compute cared for bits in predecessors of irn. */
static void dca_transfer(ir_node *irn)
{
	DBG((dbg, LEVEL_2, "analysing %+F\n", irn));

	ir_tarval *care = get_irn_link(irn);
	if (is_Block(irn)) {
		for (int i = 0; i < get_Block_n_cfgpreds(irn); i++)
			care_for(get_Block_cfgpred(irn, i), care);
		return;
	}

	ir_mode *mode = get_irn_mode(irn);
	if (mode == mode_X) {
		care_for(get_nodes_block(irn), NULL);
		switch (get_irn_opcode(irn)) {
		case iro_Return:
			for (int i = 0; i < get_Return_n_ress(irn); i++)
				care_for(get_Return_res(irn, i), care);
			care_for(get_Return_mem(irn), care);
			return;
		case iro_Jmp:
		default:
			foreach_irn_in(irn, i, pred) {
				care_for(pred, NULL);
			}

			care_for(get_nodes_block(irn), NULL);
			return;
		}
	}

	if (is_Phi(irn)) {
		int npreds = get_Phi_n_preds(irn);
		for (int i = 0; i < npreds; i++)
			care_for(get_Phi_pred(irn, i), care);

		care_for(get_nodes_block(irn), NULL);

		return;
	}

	if (mode_is_int(mode) || mode==mode_b) {
		switch (get_irn_opcode(irn)) {
		case iro_Cmp: {
			ir_node *const left  = get_Cmp_left(irn);
			ir_node *const right = get_Cmp_right(irn);
			bitinfo *const bl    = get_bitinfo(left);
			bitinfo *const br    = get_bitinfo(right);
			if (bl != NULL && br != NULL) {
				/* If we know that one bit of the operands differ
				 * all less significant bits are irrelevant. */
				assert(get_mode_arithmetic(get_irn_mode(left)) == irma_twos_complement);
				ir_tarval *const diff = tarval_or(tarval_andnot(bl->z, br->o),
				                                  tarval_andnot(br->z, bl->o));
				if (!tarval_is_null(diff)) {
					/* Create mask with msb and more significant bits. */
					ir_mode        *const cmp_mode     = get_irn_mode(left);
					const unsigned        bits         = get_mode_size_bits(cmp_mode);
					unsigned              shift_amount = 1;
					const int             highest_bit  = get_tarval_highest_bit(diff);
					ir_tarval      *      care         = tarval_shl_unsigned(get_mode_one(cmp_mode), highest_bit);
					for (int msb = bits - 1 - highest_bit; msb != 0; msb /= 2) {
						care          = tarval_or(care, tarval_shl_unsigned(care, shift_amount));
						shift_amount *= 2;
					}
					care_for(left, care);
					care_for(right, care);
				}
			}
			break;
		}
		case iro_Conv: {
			ir_node *pred = get_Conv_op(irn);
			ir_mode *pred_mode = get_irn_mode(pred);

			unsigned pred_bits = get_mode_size_bits(pred_mode);
			unsigned bits = get_mode_size_bits(mode);

			if (pred_bits < bits && mode_is_signed(pred_mode)) {
				/* Bits still care about the sign bit even if they
				 * don't fit into the smaller mode. */
				if (get_tarval_highest_bit(care) >= (int)pred_bits)
					care = tarval_or(care,
					                 tarval_shl_unsigned(get_mode_one(mode),
					                 pred_bits - 1));
			} else {
				/* Thwart sign extension as it doesn't make sense on
				 * our abstract tarvals. */
				/* TODO: ugly */
				care = tarval_convert_to(care, find_unsigned_mode(get_tarval_mode(care)));
			}

			care = tarval_convert_to(care, pred_mode);
			care_for(pred, care);
			return;
		}
		case iro_And: {
			ir_node *left  = get_And_left(irn);
			ir_node *right = get_And_right(irn);

			if (is_Const(right)) {
				care_for(left, tarval_and(care, get_Const_tarval(right)));
				care_for(right, care);
			} else {
				bitinfo *bl = get_bitinfo(left);
				bitinfo *br = get_bitinfo(right);

				if (bl != NULL && br != NULL) {
					care_for(left, tarval_and(care, tarval_ornot(br->z, bl->z)));
					care_for(right, tarval_and(care, tarval_ornot(bl->z, br->z)));
				} else {
					care_for(left, care);
					care_for(right, care);
				}
			}
			return;
		}
		case iro_Mux: {
			care_for(get_Mux_true(irn), care);
			care_for(get_Mux_false(irn), care);
			care_for(get_Mux_sel(irn), NULL);
			return;
		}
		case iro_Or: {
			ir_node *left  = get_binop_left(irn);
			ir_node *right = get_binop_right(irn);

			if (is_Const(right)) {
				care_for(left, tarval_andnot(care, get_Const_tarval(right)));
				care_for(right, care);
			} else {
				bitinfo *bl = get_bitinfo(left);
				bitinfo *br = get_bitinfo(right);

				if (bl != NULL && br != NULL) {
					care_for(left, tarval_and(care, tarval_ornot(bl->o, br->o)));
					care_for(right, tarval_and(care, tarval_ornot(br->o, bl->o)));
				} else {
					care_for(left, care);
					care_for(right, care);
				}
			}
			return;
		}
		case iro_Eor:
		case iro_Confirm:
			care_for(get_irn_n(irn, 0), care);
			care_for(get_irn_n(irn, 1), care);
			return;
		case iro_Add: {
			ir_node   *left      = get_Add_left(irn);
			ir_node   *right     = get_Add_right(irn);
			bitinfo   *bl        = get_bitinfo(left);
			bitinfo   *br        = get_bitinfo(right);
			ir_tarval *care_mask = create_msb_mask(care);

			if (bl != NULL && br != NULL && tarval_is_null(tarval_and(care_mask, tarval_and(bl->z, br->z)))) {
				care_for(left, tarval_or(tarval_andnot(care_mask, bl->z),
				                         tarval_and(care, tarval_ornot(bl->o, br->o))));
				care_for(right, tarval_or(tarval_andnot(care_mask, br->z),
				                          tarval_and(care, tarval_ornot(br->o, bl->o))));
			} else {
				care_for(right, care_mask);
				care_for(left, care_mask);
			}
			return;
		}
		case iro_Sub: {
			ir_node   *left      = get_binop_left(irn);
			ir_node   *right     = get_binop_right(irn);
			ir_tarval *care_mask = create_msb_mask(care);
			care_for(right, care_mask);
			care_for(left, care_mask);
			return;
		}
		case iro_Minus:
			care_for(get_Minus_op(irn), create_msb_mask(care));
			return;
		case iro_Not:
			care_for(get_Not_op(irn), care);
			return;
		case iro_Shrs:
		case iro_Shr: {
			ir_node *left  = get_binop_left(irn);
			ir_node *right = get_binop_right(irn);

			ir_tarval *      care_left;
			ir_tarval *      shift_amount = NULL;
			bitinfo   *const br           = get_bitinfo(right);
			if (is_Const(right)) {
				shift_amount = get_Const_tarval(right);
				care_left    = tarval_shl(care, shift_amount);
			} else if (br != NULL) {
				const long        size_bits     = get_mode_size_bits(mode);
				const long        modulo_shift  = get_mode_modulo_shift(mode);
				ir_mode    *const rmode         = get_irn_mode(right);
				ir_tarval  *const rone          = get_mode_one(rmode);
				ir_tarval  *const size_mask     = tarval_sub(new_tarval_from_long(size_bits, rmode), rone);
				ir_tarval  *const modulo_mask   = tarval_sub(new_tarval_from_long(modulo_shift, rmode), rone);
				ir_tarval  *const oversize_mask = tarval_andnot(modulo_mask, size_mask);
				ir_tarval  *const ro            = br->o;
				ir_tarval  *const rz            = br->z;
				if (tarval_is_null(tarval_and(ro, oversize_mask))) {
					ir_tarval *const rmask     = tarval_and(size_mask, modulo_mask);
					ir_tarval *const rsure     = tarval_and(tarval_not(tarval_eor(ro, rz)), rmask);
					ir_tarval *const rbound    = tarval_add(rmask, rone);
					ir_tarval *const rzero     = get_mode_null(rmode);
					care_left = get_mode_null(mode);
					for (ir_tarval *shift_amount = rzero; shift_amount != rbound; shift_amount = tarval_add(shift_amount, rone)) {
						if (tarval_is_null(tarval_and(rsure, tarval_eor(shift_amount, rz)))) {
							care_left = tarval_or(care_left, tarval_shl(care, shift_amount));
						}
					}
				} else {
					care_left = get_mode_null(mode);
				}
			} else {
				care_left = create_lsb_mask(care);
			}

			if (is_Shrs(irn) && shift_amount != NULL) {
				unsigned   bits = get_mode_size_bits(mode);
				ir_tarval *one  = get_mode_one(mode);
				ir_tarval *msb  = tarval_shl_unsigned(one, bits - 1);
				if (!tarval_is_null(tarval_and(tarval_shrs(msb, shift_amount), care))) {
					/* Care bits that disappeared still care about the sign bit. */
					care_left = tarval_or(care_left, msb);
				}
			}

			care_for(left, care_left);
			care_for(right, create_modulo_shift_mask(mode, get_irn_mode(right)));

			return;
		}
		case iro_Shl: {
			ir_node *left  = get_Shl_left(irn);
			ir_node *right = get_Shl_right(irn);

			bitinfo *const br = get_bitinfo(right);
			if (is_Const(right)) {
				care_for(left, tarval_shr(care, get_Const_tarval(right)));
			} else if (br != NULL) {
				const long        size_bits     = get_mode_size_bits(mode);
				const long        modulo_shift  = get_mode_modulo_shift(mode);
				ir_mode    *const rmode         = get_irn_mode(right);
				ir_tarval  *const rone          = get_mode_one(rmode);
				ir_tarval  *const size_mask     = tarval_sub(new_tarval_from_long(size_bits, rmode), rone);
				ir_tarval  *const modulo_mask   = tarval_sub(new_tarval_from_long(modulo_shift, rmode), rone);
				ir_tarval  *const oversize_mask = tarval_andnot(modulo_mask, size_mask);
				ir_tarval  *const ro            = br->o;
				ir_tarval  *const rz            = br->z;
				if (tarval_is_null(tarval_and(ro, oversize_mask))) {
					ir_tarval *const rmask     = tarval_and(size_mask, modulo_mask);
					ir_tarval *const rsure     = tarval_and(tarval_not(tarval_eor(ro, rz)), rmask);
					ir_tarval *const rbound    = tarval_add(rmask, rone);
					ir_tarval *const rzero     = get_mode_null(rmode);
					ir_tarval *      care_left = get_mode_null(mode);
					for (ir_tarval *shift_amount = rzero; shift_amount != rbound; shift_amount = tarval_add(shift_amount, rone)) {
						if (tarval_is_null(tarval_and(rsure, tarval_eor(shift_amount, rz)))) {
							care_left = tarval_or(care_left, tarval_shr(care, shift_amount));
						}
					}
					care_for(left, care_left);
				} else {
					care_for(left, get_mode_null(mode));
				}
			} else {
				care_for(left, create_msb_mask(care));
			}

			care_for(right, create_modulo_shift_mask(mode, get_irn_mode(right)));

			return;
		}
		case iro_Mul: {
			ir_node   *left      = get_Mul_left(irn);
			ir_node   *right     = get_Mul_right(irn);
			ir_tarval *care_mask = create_msb_mask(care);

			if (is_Const(right))
				care_for(
					left,
					tarval_shr_unsigned(care_mask,
					                    get_tarval_lowest_bit(
					                       get_Const_tarval(right))));
			else
				care_for(left, care_mask);

			care_for(right, care_mask);
			return;
		}
		}
	}

	if (mode == mode_M || mode == mode_T) {
		foreach_irn_in(irn, i, pred) {
			care_for(pred, care);
		}
		return;
	}

	/* Assume worst case on other nodes */
	foreach_irn_in(irn, i, pred) {
		care_for(pred, NULL);
	}
}

static void dca_init_node(ir_node *n, void *data)
{
	(void)data;

	ir_mode *m = get_irn_mode(n);
	set_irn_link(n, (void *) (mode_is_int(m) ?
	             get_mode_null(m) : tarval_b_false));
}

void dca_analyze(ir_graph *irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.ana.dca");

	DB((dbg, LEVEL_1, "===> Performing don't care bit analysis on %+F\n", irg));

	assert(tarval_get_wrap_on_overflow());

	assert(((ir_resources_reserved(irg) & IR_RESOURCE_IRN_LINK) != 0) &&
	       "user of dc analysis must reserve links");

	irg_walk_graph(irg, dca_init_node, NULL, 0);

	deq_init(&worklist);

	care_for(get_irg_end(irg), 0);

	while (!deq_empty(&worklist)) {
		ir_node *n = deq_pop_pointer_left(ir_node, &worklist);
		dca_transfer(n);
	}
	deq_free(&worklist);
}
