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
 *
 * There is a commented-out walker at the end of this file that might
 * be useful when revising this code.
 */
#include "constbits.h"
#include "debug.h"
#include "irnode_t.h"
#include "tv.h"
#include "irtypes.h"
#include "pdeq.h"
#include "irgwalk.h"
#include "dca.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg);

/* Set cared for bits in irn, possibly putting it on the worklist.
   care == 0 is short for unqualified caring. */
static void care_for(ir_node *irn, ir_tarval *care, pdeq *q)
{
	ir_mode *mode = get_tarval_mode(get_irn_link(irn));

	if (!care)
		care = tarval_b_true;

	/* Assume worst case if modes don't match and care has bits set. */
	if (mode != get_tarval_mode(care))
		care = tarval_is_null(care) ?
			get_tarval_null(mode) : get_tarval_all_one(mode);

	if (mode_is_int(mode)) {
		care = tarval_or(care, get_irn_link(irn));
	}

	if (care != get_irn_link(irn)) {
		DBG((dbg, LEVEL_3, "queueing %+F: %T->%T\n", irn, get_irn_link(irn), care));
		assert(get_irn_link(irn) != tarval_b_true || care == tarval_b_true);
		set_irn_link(irn, (void *)care);
		pdeq_putr(q, irn);
	} else {
		DBG((dbg, LEVEL_3, "no change on %+F: %T\n", irn, get_irn_link(irn), care));
	}
}

/* Creates a bit mask that have the lsb and all more significant bits set. */
static ir_tarval *create_lsb_mask(ir_tarval *tv)
{
	return tarval_or(tv, tarval_neg(tv));
}

/* Creates a bit mask that have the msb and all less significant bits set. */
static ir_tarval *create_msb_mask(ir_tarval *tv)
{
	unsigned shift_amount = 1;
	for (int msb = get_tarval_highest_bit(tv); msb != 0; msb /= 2) {
		tv            = tarval_or(tv, tarval_shr_unsigned(tv, shift_amount));
		shift_amount *= 2;
	}

	return tv;
}

/* Compute cared for bits in predecessors of irn. */
static void dca_transfer(ir_node *irn, pdeq *q)
{
	ir_mode *mode = get_irn_mode(irn);
	ir_tarval *care = get_irn_link(irn);

	DBG((dbg, LEVEL_2, "analysing %+F\n", irn));

	if (is_Block(irn)) {
		for (int i = 0; i < get_Block_n_cfgpreds(irn); i++)
			care_for(get_Block_cfgpred(irn, i), care, q);
		return;
	}

	if (mode == mode_X) {
		care_for(get_nodes_block(irn), 0, q);
		switch (get_irn_opcode(irn)) {
		case iro_Return:
			for (int i = 0; i < get_Return_n_ress(irn); i++)
				care_for(get_Return_res(irn, i), care, q);
			care_for(get_Return_mem(irn), care, q);
			return;
		case iro_Jmp:
		default:
			foreach_irn_in(irn, i, pred) {
				care_for(pred, 0, q);
			}

			care_for(get_nodes_block(irn), 0, q);
			return;
		}
	}

	if (is_Phi(irn)) {
		int npreds = get_Phi_n_preds(irn);
		for (int i = 0; i < npreds; i++)
			care_for(get_Phi_pred(irn, i), care, q);

		care_for(get_nodes_block(irn), 0, q);

		return;
	}

	if (mode_is_int(mode) || mode==mode_b) {
		switch (get_irn_opcode(irn)) {
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
									 tarval_shl_unsigned(get_tarval_one(mode),
												         pred_bits - 1));
			} else {
				/* Thwart sign extension as it doesn't make sense on
				 * our abstract tarvals. */
				/* TODO: ugly */
				care = tarval_convert_to(care, find_unsigned_mode(get_tarval_mode(care)));
			}

			care = tarval_convert_to(care, pred_mode);
			care_for(pred, care, q);
			return;
		}
		case iro_And: {
			ir_node *left  = get_And_left(irn);
			ir_node *right = get_And_right(irn);

			if (is_Const(left)) {
				care_for(right, tarval_and(care, get_Const_tarval(left)), q);
				care_for(left, care, q);
			} else if (is_Const(right)) {
				care_for(left, tarval_and(care, get_Const_tarval(right)), q);
				care_for(right, care, q);
			} else {
				bitinfo *bl = get_bitinfo(left);
				bitinfo *br = get_bitinfo(right);

				if (bl != NULL && br != NULL) {
					care_for(left, tarval_and(care, br->z), q);
					care_for(right, tarval_and(care, bl->z), q);
				} else {
					care_for(left, care, q);
					care_for(right, care, q);
				}
			}
			return;
		}
		case iro_Mux: {
			care_for(get_Mux_true(irn), care, q);
			care_for(get_Mux_false(irn), care, q);
			care_for(get_Mux_sel(irn), 0, q);
			return;
		}
		case iro_Or: {
			ir_node *left  = get_binop_left(irn);
			ir_node *right = get_binop_right(irn);

			if (is_Const(left)) {
				care_for(right, tarval_andnot(care, get_Const_tarval(left)), q);
				care_for(left, care, q);
			} else if (is_Const(right)) {
				care_for(left, tarval_andnot(care, get_Const_tarval(right)), q);
				care_for(right, care, q);
			} else {
				bitinfo *bl = get_bitinfo(left);
				bitinfo *br = get_bitinfo(right);

				if (bl != NULL && br != NULL) {
					care_for(left, tarval_andnot(care, br->o), q);
					care_for(right, tarval_andnot(care, bl->o), q);
				} else {
					care_for(left, care, q);
					care_for(right, care, q);
				}
			}
			return;
		}
		case iro_Eor:
		case iro_Confirm:
			care_for(get_irn_n(irn, 0), care, q);
			care_for(get_irn_n(irn, 1), care, q);
			return;
		case iro_Add: {
			ir_node *left  = get_Add_left(irn);
			ir_node *right = get_Add_right(irn);
			bitinfo *bl    = get_bitinfo(left);
			bitinfo *br    = get_bitinfo(right);

			if (bl != NULL && br != NULL && tarval_is_null(tarval_and(bl->z, br->z))) {
				care_for(left, tarval_andnot(care, br->o), q);
				care_for(right, tarval_andnot(care, bl->o), q);
			} else {
				ir_tarval *care_mask = create_msb_mask(care);
				care_for(right, care_mask, q);
				care_for(left, care_mask, q);
			}
			return;
		}
		case iro_Sub: {
			ir_node   *left      = get_binop_left(irn);
			ir_node   *right     = get_binop_right(irn);
			ir_tarval *care_mask = create_msb_mask(care);
			care_for(right, care_mask, q);
			care_for(left, care_mask, q);

			return;
		}
		case iro_Minus:
			care_for(get_Minus_op(irn), create_msb_mask(care), q);
			return;
		case iro_Not:
			care_for(get_Not_op(irn), care, q);
			return;
		case iro_Shrs:
		case iro_Shr: {
			ir_node *left  = get_binop_left(irn);
			ir_node *right = get_binop_right(irn);

			if (is_Const(right)) {
				ir_tarval *right_tv = get_Const_tarval(right);
				care_for(left, tarval_shl(care, right_tv), q);
				if (iro_Shrs == get_irn_opcode(irn)
					&& !tarval_is_null(tarval_and(tarval_shrs(get_tarval_min(mode), right_tv),
					                              tarval_convert_to(care, mode))))
					/* Care bits that disappeared still care about the sign bit. */
					care_for(left, get_tarval_min(mode), q);
			}
			else
				care_for(left, create_lsb_mask(care), q);

			// TODO Consider modulo shift
			care_for(right, 0, q);

			return;
		}
		case iro_Shl: {
			ir_node *left  = get_Shl_left(irn);
			ir_node *right = get_Shl_right(irn);

			if (is_Const(right))
				care_for(left, tarval_shr(care, get_Const_tarval(right)), q);
			else
				care_for(left, create_msb_mask(care), q);

			// TODO Consider modulo shift
			care_for(right, 0, q);

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
					                       get_Const_tarval(right))), q);
			else
				care_for(left, care_mask, q);

			care_for(right, care_mask, q);
			return;
		}
		}
	}

	if (mode == mode_M || mode == mode_T) {
		foreach_irn_in(irn, i, pred) {
			care_for(pred, care, q);
		}
		return;
	}

	/* Assume worst case on other nodes */
	foreach_irn_in(irn, i, pred) {
		care_for(pred, 0, q);
	}
}

static void dca_init_node(ir_node *n, void *data)
{
	ir_mode *m = get_irn_mode(n);
	(void) data;

	set_irn_link(n, (void *) (mode_is_int(m) ?
				  get_tarval_null(m) : get_tarval_b_false()));
}

/* Compute don't care bits.
   The result is available via links to tarvals. */
void dca_analyze(ir_graph *irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.ana.dca");

	DB((dbg, LEVEL_1, "===> Performing don't care bit analysis on %+F\n", irg));

	assert(tarval_get_integer_overflow_mode() == TV_OVERFLOW_WRAP);

	assert(((ir_resources_reserved(irg) & IR_RESOURCE_IRN_LINK) != 0) &&
			"user of dc analysis must reserve links");

	irg_walk_graph(irg, dca_init_node, NULL, 0);

	{
		pdeq *q = new_pdeq();

		care_for(get_irg_end(irg), 0, q);

		while (!pdeq_empty(q)) {
			ir_node *n = (ir_node*)pdeq_getl(q);
			dca_transfer(n, q);
		}
		del_pdeq(q);
	}

	return;
}

#if 0
/* Walker to "test" the fixpoint.
 Insert Eor nodes that toggle don't care bits. */
void dca_add_fuzz(ir_node *node, void *data)
{
	(void) data;
	ir_graph *irg = get_irn_irg(node);

	if (is_Eor(node)) return;

	foreach_irn_in(node, i, pred) {
		ir_mode *pred_mode = get_irn_mode(pred);
		ir_tarval *dc = get_irn_link(pred);

		if (is_Eor(pred)) continue;

		if (mode_is_int(pred_mode)
			&& dc
			&& ! tarval_is_all_one(dc))
		{
			ir_node *block = get_nodes_block(pred);
			ir_node *eor, *constnode;

			constnode = new_r_Const(irg, tarval_not(dc));
			eor = new_r_Eor(block, constnode, pred, pred_mode);

			set_irn_n(node, i, eor);
		}
	}
}
#endif
