/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Data-flow driven minimal fixpoint value range analysis
 * @author  Christoph Mallon
 */
#include "constbits.h"

#include "debug.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irnodemap.h"
#include "iropt.h"
#include <assert.h>

#ifndef VERIFY_CONSTBITS
#	ifdef DEBUG_libfirm
#		define VERIFY_CONSTBITS 1
#	else
#		define VERIFY_CONSTBITS 0
#	endif
#endif

#if VERIFY_CONSTBITS
#include "irdump.h"
#include "irprintf.h"
#include "panic.h"
#endif

/* TODO:
 * - Implement cleared/set bit calculation for Div, Mod
 * - Implement min/max calculation for And, Eor, Or, Not, Conv, Shl, Shr, Shrs, Mux
 * - Implement min/max calculation for Add, Sub, Minus, Mul, Div, Mod, Conv, Shl, Shr, Shrs, Mux
 */

/* Tables of the cleared/set bit lattice
 *
 * Encoding of the lattice
 * zo
 * 00 0 zero
 * 01 - impossible state, is zero /and/ one
 * 10 T top, may be either zero or one
 * 11 1 one
 *
 * S = Sum
 * c = Carry
 * D = Difference
 * b = Borrow
 *
 * Not
 * A ~
 * 0 1
 * 1 0
 * T T
 *
 * Half adder, half subtractor, and, xor, or, Mux
 * AB  Sc  Db  &  ^  |  M
 * 00  00  00  0  0  0  0
 * 01  10  11  0  1  1  T
 * 0T  T0  TT  0  T  T  T
 * 10  10  10  0  1  1  T
 * 11  01  00  1  0  1  1
 * 1T  TT  T0  T  T  1  T
 * T0  T0  T0  0  T  T  T
 * T1  TT  TT  T  T  1  T
 * TT  TT  TT  T  T  T  T
 *
 * Full adder, full subtractor
 * ABc-1  Sc  Db
 * 000    00  00
 * 001    10  11
 * 00T    T0  TT
 * 010    10  11
 * 011    01  01
 * 01T    TT  T1
 * 0T0    T0  TT
 * 0T1    TT  T1
 * 0TT    TT  TT
 * 100    10  10
 * 101    01  00
 * 10T    TT  T0
 * 110    01  00
 * 111    11  11
 * 11T    T1  TT
 * 1T0    TT  T0
 * 1T1    T1  TT
 * 1TT    TT  TT
 * T00    T0  T0
 * T01    TT  TT
 * T0T    TT  TT
 * T10    TT  TT
 * T11    T1  T1
 * T1T    TT  TT
 * TT0    TT  TT
 * TT1    TT  TT
 * TTT    TT  TT
 *
 *
 * Assume: Xmin <= Xmax and no overflow
 * A + B = (Amin + Bmin, Amax + Bmax)
 *    -A = (-Amax, -Amin)
 * A - B = A + -B = (Amin (-B)min, Amax + (-B)max) = (Amin - Bmax, Amax - Bmin)
 */

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static bool is_undefined(bitinfo const *const b)
{
	return tarval_is_null(b->z) && tarval_is_all_one(b->o);
}

/** Set analysis information for node @p irn. */
static bool set_bitinfo(ir_node const *const irn, ir_tarval *const z, ir_tarval *const o)
{
	ir_graph   *const irg  = get_irn_irg(irn);
	ir_nodemap *const map  = &irg->bitinfo.map;
	bitinfo          *b    = ir_nodemap_get(bitinfo, map, irn);
	if (b == NULL) {
		struct obstack *const obst = &irg->bitinfo.obst;
		b = OALLOCZ(obst, bitinfo);
		ir_nodemap_insert(map, irn, b);
	} else if (z == b->z && o == b->o) {
		return false;
	} else {
		/* Assert ascending chain. */
		assert(tarval_is_null(tarval_andnot(b->z, z)));
		assert(tarval_is_null(tarval_andnot(o, b->o)));
	}
	b->z = z;
	b->o = o;
	DB((dbg, LEVEL_3, "Set %+F: 0:%T 1:%T%s\n", irn, z, o, is_undefined(b) ? " (bottom)" : tarval_is_all_one(z) && tarval_is_null(o) ? " (top)" : ""));
	return true;
}

static bool mode_is_intb(ir_mode const *const m)
{
	return mode_is_int(m) || m == mode_b;
}

bitinfo const *try_get_bitinfo(ir_node const *const irn)
{
	ir_graph   *const irg = get_irn_irg(irn);
	ir_nodemap *const map = &irg->bitinfo.map;
	return map->data ? ir_nodemap_get(bitinfo, map, irn) : NULL;
}

static bitinfo *get_bitinfo_null(ir_node const *const irn)
{
	(void)irn;
	return NULL;
}

static bitinfo *get_bitinfo_direct(ir_node const *const irn)
{
	ir_graph   *const irg = get_irn_irg(irn);
	ir_nodemap *const map = &irg->bitinfo.map;
	bitinfo          *b   = ir_nodemap_get(bitinfo, map, irn);
	if (!b && is_Const(irn) && mode_is_intb(get_irn_mode(irn))) {
		ir_tarval *const tv = get_Const_tarval(irn);
		set_bitinfo(irn, tv, tv);
		b = ir_nodemap_get(bitinfo, map, irn);
	}
	return b;
}

static void calc_bitinfo(ir_node const *irn, bitinfo *b);

/**
 * Get analysis information for node @p irn.
 *
 * If it is not available, calculate it.
 */
static bitinfo *get_bitinfo_recursive(ir_node const *const irn)
{
	ir_graph   *const irg = get_irn_irg(irn);
	ir_nodemap *const map = &irg->bitinfo.map;
	bitinfo          *b   = ir_nodemap_get(bitinfo, map, irn);
	if (!b || b->state == BITINFO_INVALID || b->state == BITINFO_UNSTABLE) {
		ir_mode *mode = get_irn_mode(irn);
		if (mode == mode_BB || mode == mode_X) {
			/* Blocks and jumps use a boolean domain. */
			mode = mode_b;
		} else if (!mode_is_intb(mode)) {
			return NULL;
		}

		/* Insert bottom to break cycles. */
		struct obstack *const obst = &irg->bitinfo.obst;
		if (!b)
			b = OALLOCZ(obst, bitinfo);
		if (b->state == BITINFO_INVALID) {
			b->z = get_mode_null(mode);
			b->o = get_mode_all_one(mode);
		}
		ir_nodemap_insert(map, irn, b);

		calc_bitinfo(irn, b);
	}
	return b;
}

static bitinfo *(*get_bitinfo_func)(ir_node const*) = &get_bitinfo_null;

bitinfo *get_bitinfo(ir_node const *const irn)
{
	return get_bitinfo_func(irn);
}

static bool transfer(ir_node const *const irn)
{
	ir_tarval *const f = tarval_b_false;
	ir_tarval *const t = tarval_b_true;
	ir_mode   *const m = get_irn_mode(irn);
	ir_tarval       *z;
	ir_tarval       *o;

	if (m == mode_X) {
		DB((dbg, LEVEL_3, "transfer %+F\n", irn));

		bitinfo *const b = get_bitinfo_recursive(get_nodes_block(irn));
		if (b->z == f) {
unreachable_X:
			z = f;
			o = t;
		} else switch (get_irn_opcode(irn)) {
			case iro_Bad:
				goto unreachable_X;

			case iro_Proj: {
				ir_node *const pred = get_Proj_pred(irn);
				if (is_Start(pred)) {
					goto result_unknown_X;
				} else if (is_Cond(pred)) {
					ir_node   *const selector = get_Cond_selector(pred);
					bitinfo   *const b        = get_bitinfo_recursive(selector);
					if (is_undefined(b))
						goto unreachable_X;
					if (b->z == b->o) {
						if ((b->z == t) == get_Proj_num(irn)) {
							z = o = t;
						} else {
							z = o = f;
						}
					} else {
						goto result_unknown_X;
					}
				} else if (is_Switch(pred)) {
					ir_node *const selector = get_Switch_selector(pred);
					bitinfo *const b        = get_bitinfo_recursive(selector);
					if (is_undefined(b))
						goto unreachable_X;
					/* TODO */
					goto cannot_analyse_X;
				} else {
					goto cannot_analyse_X;
				}
				break;
			}

			case iro_Jmp:
				goto result_unknown_X;

			default:
cannot_analyse_X:
				DB((dbg, LEVEL_4, "cannot analyse %+F\n", irn));
result_unknown_X:
				z = t;
				o = f;
				break;
		}
	} else if (is_Block(irn)) {
		DB((dbg, LEVEL_3, "transfer %+F\n", irn));
		bool reachable = false;
		foreach_irn_in(irn, i, pred_block) {
			bitinfo *const b = get_bitinfo_recursive(pred_block);
			if (b->z == t) {
				reachable = true;
				/* We need to iterate all operands to reach a global fix point.
				 * Thus, do not use a break here. */
			}
		}

		if (!reachable) {
			ir_graph *const irg = get_irn_irg(irn);
			reachable =
				irn == get_irg_start_block(irg) ||
				irn == get_irg_end_block(irg);
		}

		if (reachable) {
			z = t;
			o = f;
		} else {
			z = f;
			o = t;
		}
	} else if (mode_is_intb(m)) {
		DB((dbg, LEVEL_3, "transfer %+F\n", irn));

		if (is_Phi(irn)) {
			ir_node *const block = get_nodes_block(irn);

repeatphi:
			z = get_mode_null(m);
			o = get_mode_all_one(m);
			foreach_irn_in(block, i, pred_block) {
				bitinfo *const b_cfg = get_bitinfo_recursive(pred_block);
				if (b_cfg->z != f) {
					bitinfo *const b = get_bitinfo_recursive(get_Phi_pred(irn, i));
					z = tarval_or( z, b->z);
					o = tarval_and(o, b->o);
				}
			}
			/* Computing bitinfo for operand 1 might render operand 0 unstable.
			 * Thus, evaluate the operands until all of them are stable. */
			foreach_irn_in(block, i, pred_block) {
				bitinfo *const b_cfg = get_bitinfo_recursive(pred_block);
				if (b_cfg->z != f) {
					bitinfo *const b = get_bitinfo_direct(get_Phi_pred(irn, i));
					if (b->state == BITINFO_UNSTABLE) {
						goto repeatphi;
					}
				}
			}
		} else {
			/* Undefined if any input is undefined. */
			foreach_irn_in(irn, i, pred) {
				bitinfo *const pred_b = get_bitinfo_recursive(pred);
				if (pred_b != NULL && is_undefined(pred_b))
					goto undefined;
			}

			switch (get_irn_opcode(irn)) {
				case iro_Bad:
undefined:
					z = get_mode_null(m);
					o = get_mode_all_one(m);
					break;

				case iro_Const: {
					z = o = get_Const_tarval(irn);
					break;
				}

				case iro_Confirm: {
					ir_node *const v = get_Confirm_value(irn);
					bitinfo *const b = get_bitinfo_recursive(v);
					/* TODO Use bound and relation. */
					z = b->z;
					o = b->o;
					if ((get_Confirm_relation(irn) & ~ir_relation_unordered) == ir_relation_equal) {
						bitinfo *const bound_b = get_bitinfo_recursive(get_Confirm_bound(irn));
						z = tarval_and(z, bound_b->z);
						o = tarval_or( o, bound_b->o);
					}
					break;
				}

				case iro_Shl: {
					ir_node   *const right = get_Shl_right(irn);
					bitinfo   *const l     = get_bitinfo_recursive(get_Shl_left(irn));
					bitinfo   *const r     = get_bitinfo_recursive(right);
					ir_tarval *const lo    = l->o;
					ir_tarval *const lz    = l->z;
					ir_tarval *const ro    = r->o;
					ir_tarval *const rz    = r->z;
					if (rz == ro) {
						z = tarval_shl(lz, rz);
						o = tarval_shl(lo, rz);
					} else {
						const long        size_bits     = get_mode_size_bits(m);
						const long        modulo_shift  = get_mode_modulo_shift(m);
						ir_mode    *const rmode         = get_irn_mode(right);
						ir_tarval  *const rone          = get_mode_one(rmode);
						ir_tarval  *const size_mask     = tarval_sub(new_tarval_from_long(size_bits, rmode), rone);
						ir_tarval  *const modulo_mask   = tarval_sub(new_tarval_from_long(modulo_shift, rmode), rone);
						ir_tarval  *const zero          = get_mode_null(m);
						ir_tarval  *const all_one       = get_mode_all_one(m);
						ir_tarval  *const oversize_mask = tarval_andnot(modulo_mask, size_mask);

						z = zero;
						o = tarval_is_null(tarval_and(rz, oversize_mask)) ? all_one : zero;

						if (tarval_is_null(tarval_and(ro, oversize_mask))) {
							ir_tarval *const rmask  = tarval_and(size_mask, modulo_mask);
							ir_tarval *const rsure  = tarval_and(tarval_not(tarval_eor(ro, rz)), rmask);
							ir_tarval *const rbound = tarval_add(rmask, rone);
							ir_tarval *const rzero  = get_mode_null(rmode);
							for (ir_tarval *shift_amount = rzero; shift_amount != rbound; shift_amount = tarval_add(shift_amount, rone)) {
								if (tarval_is_null(tarval_and(rsure, tarval_eor(shift_amount, rz)))) {
									z = tarval_or(z, tarval_shl(lz, shift_amount));
									o = tarval_and(o, tarval_shl(lo, shift_amount));
								}
							}
						}

						/* Ensure that we do not create undefined bit information. */
						assert(!tarval_is_null(z) || !tarval_is_all_one(o));
					}
					break;
				}

				case iro_Shr: {
					ir_node   *const right = get_Shr_right(irn);
					bitinfo   *const l     = get_bitinfo_recursive(get_Shr_left(irn));
					bitinfo   *const r     = get_bitinfo_recursive(right);
					ir_tarval *const lz    = l->z;
					ir_tarval *const lo    = l->o;
					ir_tarval *const rz    = r->z;
					ir_tarval *const ro    = r->o;
					if (rz == r->o) {
						z = tarval_shr(lz, rz);
						o = tarval_shr(lo, rz);
					} else {
						const long        size_bits     = get_mode_size_bits(m);
						const long        modulo_shift  = get_mode_modulo_shift(m);
						ir_mode    *const rmode         = get_irn_mode(right);
						ir_tarval  *const rone          = get_mode_one(rmode);
						ir_tarval  *const size_mask     = tarval_sub(new_tarval_from_long(size_bits, rmode), rone);
						ir_tarval  *const modulo_mask   = tarval_sub(new_tarval_from_long(modulo_shift, rmode), rone);
						ir_tarval  *const zero          = get_mode_null(m);
						ir_tarval  *const all_one       = get_mode_all_one(m);
						ir_tarval  *const oversize_mask = tarval_andnot(modulo_mask, size_mask);

						z = zero;
						o = tarval_is_null(tarval_and(rz, oversize_mask)) ? all_one : zero;

						if (tarval_is_null(tarval_and(ro, oversize_mask))) {
							ir_tarval *const rmask  = tarval_and(size_mask, modulo_mask);
							ir_tarval *const rsure  = tarval_and(tarval_not(tarval_eor(ro, rz)), rmask);
							ir_tarval *const rbound = tarval_add(rmask, rone);
							ir_tarval *const rzero  = get_mode_null(rmode);
							for (ir_tarval *shift_amount = rzero; shift_amount != rbound; shift_amount = tarval_add(shift_amount, rone)) {
								if (tarval_is_null(tarval_and(rsure, tarval_eor(shift_amount, rz)))) {
									z = tarval_or(z, tarval_shr(lz, shift_amount));
									o = tarval_and(o, tarval_shr(lo, shift_amount));
								}
							}
						}

						/* Ensure that we do not create undefined bit information. */
						assert(!tarval_is_null(z) || !tarval_is_all_one(o));
					}
					break;
				}

				case iro_Shrs: {
					ir_node   *const right = get_Shrs_right(irn);
					bitinfo   *const l     = get_bitinfo_recursive(get_Shrs_left(irn));
					bitinfo   *const r     = get_bitinfo_recursive(right);
					ir_tarval *const lz    = l->z;
					ir_tarval *const lo    = l->o;
					ir_tarval *const rz    = r->z;
					ir_tarval *const ro    = r->o;
					if (rz == r->o) {
						z = tarval_shrs(lz, rz);
						o = tarval_shrs(lo, rz);
					} else {
						const long        size_bits     = get_mode_size_bits(m);
						const long        modulo_shift  = get_mode_modulo_shift(m);
						ir_mode    *const rmode         = get_irn_mode(right);
						ir_tarval  *const rone          = get_mode_one(rmode);
						ir_tarval  *const size_mask     = tarval_sub(new_tarval_from_long(size_bits, rmode), rone);
						ir_tarval  *const modulo_mask   = tarval_sub(new_tarval_from_long(modulo_shift, rmode), rone);
						ir_tarval  *const zero          = get_mode_null(m);
						ir_tarval  *const all_one       = get_mode_all_one(m);
						ir_tarval  *const oversize_mask = tarval_andnot(modulo_mask, size_mask);

						z = zero;
						o = tarval_is_null(tarval_and(rz, oversize_mask)) ? all_one : zero;

						if (tarval_is_null(tarval_and(ro, oversize_mask))) {
							ir_tarval *const rmask  = tarval_and(size_mask, modulo_mask);
							ir_tarval *const rsure  = tarval_and(tarval_not(tarval_eor(ro, rz)), rmask);
							ir_tarval *const rbound = tarval_add(rmask, rone);
							ir_tarval *const rzero  = get_mode_null(rmode);
							for (ir_tarval *shift_amount = rzero; shift_amount != rbound; shift_amount = tarval_add(shift_amount, rone)) {
								if (tarval_is_null(tarval_and(rsure, tarval_eor(shift_amount, rz)))) {
									z = tarval_or(z, tarval_shrs(lz, shift_amount));
									o = tarval_and(o, tarval_shrs(lo, shift_amount));
								}
							}
						}

						/* Ensure that we do not create undefined bit information. */
						assert(!tarval_is_null(z) || !tarval_is_all_one(o));
					}
					break;
				}

				case iro_Add: {
					bitinfo   *const l   = get_bitinfo_recursive(get_Add_left(irn));
					bitinfo   *const r   = get_bitinfo_recursive(get_Add_right(irn));
					ir_tarval *const lz  = l->z;
					ir_tarval *const lo  = l->o;
					ir_tarval *const rz  = r->z;
					ir_tarval *const ro  = r->o;
					ir_tarval *const vz  = tarval_add(lz, rz);
					ir_tarval *const vo  = tarval_add(lo, ro);
					ir_tarval *const lnc = tarval_eor(lz, lo);
					ir_tarval *const rnc = tarval_eor(rz, ro);
					ir_tarval *const vnc = tarval_eor(vz, vo);
					ir_tarval *const nc  = tarval_or(tarval_or(lnc, rnc), vnc);
					z = tarval_or(vz, nc);
					o = tarval_andnot(vz, nc);
					break;
				}

				case iro_Sub: {
					bitinfo *const l = get_bitinfo_recursive(get_Sub_left(irn));
					bitinfo *const r = get_bitinfo_recursive(get_Sub_right(irn));
					// might subtract pointers
					if (l == NULL || r == NULL)
						goto cannot_analyse;

					ir_tarval *const lz  = l->z;
					ir_tarval *const lo  = l->o;
					ir_tarval *const rz  = r->z;
					ir_tarval *const ro  = r->o;
					ir_tarval *const vz  = tarval_sub(lo, rz);
					ir_tarval *const vo  = tarval_sub(lz, ro);
					ir_tarval *const lnc = tarval_eor(lz, lo);
					ir_tarval *const rnc = tarval_eor(rz, ro);
					ir_tarval *const vnc = tarval_eor(vz, vo);
					ir_tarval *const nc  = tarval_or(tarval_or(lnc, rnc), vnc);
					z = tarval_or(vz, nc);
					o = tarval_andnot(vz, nc);
					break;
				}

				case iro_Mul: {
					bitinfo   *const l  = get_bitinfo_recursive(get_Mul_left(irn));
					bitinfo   *const r  = get_bitinfo_recursive(get_Mul_right(irn));
					ir_tarval *      lz = l->z;
					ir_tarval *      lo = l->o;
					ir_tarval *      rz = r->z;
					ir_tarval *      ro = r->o;
					if (lz == lo && rz == ro) {
						z = o = tarval_mul(lz, rz);
					} else {
						ir_tarval *one = get_mode_one(m);
						z = o = get_mode_null(m);
						while (!tarval_is_null(rz)) {
							if (!tarval_is_null(tarval_and(rz, one))) {
								ir_tarval *const vz  = tarval_add(lz, z);
								ir_tarval *const vo  = tarval_add(lo, o);
								ir_tarval *const lnc = tarval_eor(lz, lo);
								ir_tarval *const rnc = tarval_eor(z, o);
								ir_tarval *const vnc = tarval_eor(vz, vo);
								ir_tarval *const nc  = tarval_or(tarval_or(lnc, rnc), vnc);
								ir_tarval *const az  = tarval_or(vz, nc);
								ir_tarval *const ao  = tarval_andnot(vz, nc);

								if (tarval_is_null(tarval_andnot(one, ro))) {
									z = az;
									o = ao;
								} else {
									z = tarval_or(z, az);
									o = tarval_and(o, ao);
								}
							}
							lz = tarval_shl(lz, one);
							lo = tarval_shl(lo, one);
							rz = tarval_shr(rz, one);
							ro = tarval_shr(ro, one);
						}
					}
					break;
				}

				case iro_Minus: {
					/* -a = 0 - a */
					bitinfo   *const b   = get_bitinfo_recursive(get_Minus_op(irn));
					ir_tarval *const bz  = b->z;
					ir_tarval *const bo  = b->o;
					ir_tarval *const vz  = tarval_neg(bz);
					ir_tarval *const vo  = tarval_neg(bo);
					ir_tarval *const bnc = tarval_eor(bz, bo);
					ir_tarval *const vnc = tarval_eor(vz, vo);
					ir_tarval *const nc  = tarval_or(bnc, vnc);
					z = tarval_or(vz, nc);
					o = tarval_andnot(vz, nc);
					break;

				}

				case iro_And: {
					bitinfo *const l = get_bitinfo_recursive(get_And_left(irn));
					bitinfo *const r = get_bitinfo_recursive(get_And_right(irn));
					z = tarval_and(l->z, r->z);
					o = tarval_and(l->o, r->o);
					break;
				}

				case iro_Or: {
					bitinfo *const l = get_bitinfo_recursive(get_Or_left(irn));
					bitinfo *const r = get_bitinfo_recursive(get_Or_right(irn));
					z = tarval_or(l->z, r->z);
					o = tarval_or(l->o, r->o);
					break;
				}

				case iro_Eor: {
					bitinfo   *const l  = get_bitinfo_recursive(get_Eor_left(irn));
					bitinfo   *const r  = get_bitinfo_recursive(get_Eor_right(irn));
					ir_tarval *const lz = l->z;
					ir_tarval *const lo = l->o;
					ir_tarval *const rz = r->z;
					ir_tarval *const ro = r->o;
					z = tarval_or(tarval_andnot(lz, ro), tarval_andnot(rz, lo));
					o = tarval_or(tarval_andnot(ro, lz), tarval_andnot(lo, rz));
					break;
				}

				case iro_Not: {
					bitinfo *const b = get_bitinfo_recursive(get_Not_op(irn));
					z = tarval_not(b->o);
					o = tarval_not(b->z);
					break;
				}

				case iro_Conv: {
					bitinfo *const b = get_bitinfo_recursive(get_Conv_op(irn));
					if (b == NULL) // Happens when converting from float values.
						goto result_unknown;
					z = tarval_convert_to(b->z, m);
					o = tarval_convert_to(b->o, m);
					break;
				}

				case iro_Mux: {
					bitinfo *const bf = get_bitinfo_recursive(get_Mux_false(irn));
					bitinfo *const bt = get_bitinfo_recursive(get_Mux_true(irn));
					bitinfo *const c  = get_bitinfo_recursive(get_Mux_sel(irn));
					if (c->o == t) {
						z = bt->z;
						o = bt->o;
					} else if (c->z == f) {
						z = bf->z;
						o = bf->o;
					} else {
						z = tarval_or( bf->z, bt->z);
						o = tarval_and(bf->o, bt->o);
					}
					break;
				}

				case iro_Cmp: {
					bitinfo *const l = get_bitinfo_recursive(get_Cmp_left(irn));
					bitinfo *const r = get_bitinfo_recursive(get_Cmp_right(irn));
					if (l == NULL || r == NULL)
						goto result_unknown; // Cmp compares something we cannot evaluate.
					ir_tarval  *const lz       = l->z;
					ir_tarval  *const lo       = l->o;
					ir_tarval  *const rz       = r->z;
					ir_tarval  *const ro       = r->o;
					ir_relation const relation = get_Cmp_relation(irn);
					switch (relation) {
						case ir_relation_less_greater:
							if (!tarval_is_null(tarval_andnot(ro, lz)) ||
							    !tarval_is_null(tarval_andnot(lo, rz))) {
								// At least one bit differs.
								z = o = t;
							} else if (lz == lo && rz == ro && lz == rz) {
								z = o = f;
							} else {
								goto result_unknown;
							}
							break;

						case ir_relation_equal:
							if (!tarval_is_null(tarval_andnot(ro, lz)) ||
							    !tarval_is_null(tarval_andnot(lo, rz))) {
								// At least one bit differs.
								z = o = f;
							} else if (lz == lo && rz == ro && lz == rz) {
								z = o = t;
							} else {
								goto result_unknown;
							}
							break;

						case ir_relation_less_equal:
						case ir_relation_less:
							/* TODO handle negative values */
							if (tarval_is_negative(lz) || tarval_is_negative(lo) ||
							    tarval_is_negative(rz) || tarval_is_negative(ro))
								goto result_unknown;

							if (tarval_cmp(lz, ro) & relation) {
								/* Left upper bound is smaller(/equal) than right lower bound. */
								z = o = t;
							} else if (!(tarval_cmp(lo, rz) & relation)) {
								/* Left lower bound is not smaller(/equal) than right upper bound. */
								z = o = f;
							} else {
								goto result_unknown;
							}
							break;

						case ir_relation_greater_equal:
						case ir_relation_greater:
							/* TODO handle negative values */
							if (tarval_is_negative(lz) || tarval_is_negative(lo) ||
							    tarval_is_negative(rz) || tarval_is_negative(ro))
								goto result_unknown;

							if (!(tarval_cmp(lz, ro) & relation)) {
								/* Left upper bound is not greater(/equal) than right lower bound. */
								z = o = f;
							} else if (tarval_cmp(lo, rz) & relation) {
								/* Left lower bound is greater(/equal) than right upper bound. */
								z = o = t;
							} else {
								goto result_unknown;
							}
							break;

						default:
							goto cannot_analyse;
					}
					break;
				}

				case iro_Proj: {
					ir_node *const pred = get_Proj_pred(irn);
					if (is_Tuple(pred)) {
						unsigned       pn = get_Proj_num(irn);
						ir_node *const op = get_Tuple_pred(pred, pn);
						bitinfo *const b  = get_bitinfo_recursive(op);
						z = b->z;
						o = b->o;
						goto set_info;
					}
					goto cannot_analyse;
				}

				default: {
cannot_analyse:
					DB((dbg, LEVEL_4, "cannot analyse %+F\n", irn));
result_unknown:
					z = get_mode_all_one(m);
					o = get_mode_null(m);
					break;
				}
			}
		}
	} else {
		return false;
	}

set_info:;
	bool changed = set_bitinfo(irn, z, o);
	DB((dbg, LEVEL_4, "finish transfer %+F\n", irn));
	return changed;
}

static void trigger_users(ir_node const *irn);

static void trigger(ir_node const *const irn, ir_node const *const operand)
{
	(void)operand;

	bitinfo *const b = get_bitinfo_direct(irn);
	if (b && b->state == BITINFO_VALID) {
		DB((dbg, LEVEL_5, "%+F triggers %+F\n", operand, irn));
		b->state = BITINFO_UNSTABLE;
		trigger_users(irn);
	} else {
		DB((dbg, LEVEL_5, "%+F does not trigger %+F\n", operand, irn));
	}
}

static void trigger_users(ir_node const *const irn)
{
	if (is_Bad(irn))
		return;

	if (is_Block(irn)) {
		/* Blocks just trigger the jump nodes inside.  The value of all other nodes
		 * should not depend on the reachability of the block. */
		foreach_out_edge(irn, e) {
			ir_node *const src = get_edge_src_irn(e);
			if (get_irn_mode(src) == mode_X)
				trigger(src, irn);
		}
	} else if (get_irn_mode(irn) == mode_X) {
		if (!is_End(irn)) {
			/* When the state of a control flow node changes, not only trigger its
			 * successor blocks, but also the Phis in these blocks, because the Phis
			 * must reconsider this input path. */
			foreach_out_edge(irn, e) {
				ir_node *const src = get_edge_src_irn(e);
				if (is_Block(src)) {
					trigger(src, irn);
					foreach_out_edge(src, f) {
						ir_node *const phi = get_edge_src_irn(f);
						if (is_Phi(phi))
							trigger(phi, irn);
					}
				} else {
					assert(is_Tuple(src) && get_nodes_block(src) == get_nodes_block(irn));
				}
			}
		}
	} else {
		foreach_out_edge(irn, e) {
			ir_node* const src = get_edge_src_irn(e);
			if (get_irn_mode(src) == mode_T) {
				/* Trigger Projs of tuple nodes.  They might contain analysis information,
				 * but the tuple node does not. */
				trigger_users(src);
			} else {
				trigger(src, irn);
			}
		}
	}
}

static void calc_bitinfo(ir_node const *const irn, bitinfo *const b)
{
	do {
		b->state = BITINFO_IN_FLIGHT;
		bool const changed = transfer(irn);
		b->state = BITINFO_VALID;
		if (changed)
			trigger_users(irn);
	} while (b->state != BITINFO_VALID);
}

static void calc_bitinfo_walker(ir_node *const n, void *const env)
{
	(void)env;

	ir_mode *const mode = get_irn_mode(n);
	if (mode_is_intb(mode) || mode == mode_BB || mode == mode_X)
		get_bitinfo_recursive(n);
}

#if VERIFY_CONSTBITS
static void verify_constbits_walker(ir_node *const n, void *const env)
{
	bool *const failed = (bool*)env;

	bitinfo *const bi = get_bitinfo_direct(n);
	if (bi) {
		bitinfo const old = *bi;
		if (transfer(n)) {
			ir_fprintf(stderr, "---> no fixpoint for %+F\n", n);
			*bi     = old;
			*failed = true;
		}
	}
}

static void verify_constbits(ir_graph *const irg)
{
	bool failed = false;
	DB((dbg, LEVEL_1, "---> verifying constbits for %+F\n", irg));
	irg_walk_graph(irg, NULL, verify_constbits_walker, &failed);
	if (failed) {
		dump_ir_graph(irg, "verify-constbits");
		panic("verify constbits failed");
	}
}
#endif

void constbits_analyze(ir_graph *const irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.ana.constbits");
	DB((dbg, LEVEL_1, "---> activating constbits for %+F\n", irg));

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	obstack_init(&irg->bitinfo.obst);
	ir_nodemap_init(&irg->bitinfo.map, irg);
	get_bitinfo_func = &get_bitinfo_recursive;
	irg_walk_graph(irg, NULL, calc_bitinfo_walker, NULL);
	get_bitinfo_func = &get_bitinfo_direct;

#if VERIFY_CONSTBITS
	verify_constbits(irg);
#endif
}

void constbits_clear(ir_graph *const irg)
{
	get_bitinfo_func = &get_bitinfo_null;
	ir_nodemap_destroy(&irg->bitinfo.map);
	obstack_free(&irg->bitinfo.obst, NULL);
}
