/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Data-flow driven minimal fixpoint value range analysis
 * @author  Christoph Mallon
 */
#include <assert.h>
#include <stdbool.h>

#include "adt/pdeq.h"
#include "adt/obst.h"
#include "adt/xmalloc.h"
#include "debug.h"
#include "ircons.h"
#include "irdom.h"
#include "iredges.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irnodemap.h"
#include "iroptimize.h"
#include "iropt.h"
#include "tv.h"
#include "irmemory.h"
#include "constbits.h"

/* TODO:
 * - Implement cleared/set bit calculation for Div, Mod, Shr, Shrs
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

bitinfo *get_bitinfo(ir_node const *const irn)
{
	ir_graph   *const irg = get_irn_irg(irn);
	ir_nodemap *const map = &irg->bitinfo.map;
	if (map->data == NULL)
		return NULL;

	return ir_nodemap_get(bitinfo, map, irn);
}

bool join_bitinfo(ir_node *const irn, ir_tarval *const z, ir_tarval *const o)
{
	if (tarval_is_null(z) && tarval_is_all_one(o)) {
		/*
		 * Reject undefined bit information.
		 *
		 * This occurs if we optimize within unreachable code.
		 * Due to CSE, the node might also be used within reachable code,
		 * so we cannot set undefined bit information in this case.
		 */
		return false;
	}

	bitinfo *b = get_bitinfo(irn);
	if (b == NULL) {
		ir_graph       *const irg  = get_irn_irg(irn);
		ir_nodemap     *const map  = &irg->bitinfo.map;
		struct obstack *const obst = &irg->bitinfo.obst;
		b = OALLOCZ(obst, bitinfo);
		ir_nodemap_insert(map, irn, b);
		b->z = z;
		b->o = o;
	} else if (z == b->z && o == b->o) {
		return false;
	} else {
		b->z = tarval_and(b->z, z);
		b->o = tarval_or(b->o, o);
	}
	DB((dbg, LEVEL_3, "Join %+F: 0:%T 1:%T\n", irn, b->z, b->o));
	return true;
}

bool set_bitinfo(ir_node *const irn, ir_tarval *const z, ir_tarval *const o)
{
	bitinfo *b = get_bitinfo(irn);
	if (b == NULL) {
		ir_graph       *const irg  = get_irn_irg(irn);
		ir_nodemap     *const map  = &irg->bitinfo.map;
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
	DB((dbg, LEVEL_3, "Set %+F: 0:%T 1:%T\n", irn, z, o));
	return true;
}

static bool mode_is_intb(ir_mode const *const m)
{
	return mode_is_int(m) || m == mode_b;
}

static bool transfer(ir_node *const irn)
{
	ir_tarval *const f = tarval_b_false;
	ir_tarval *const t = tarval_b_true;
	ir_mode   *const m = get_irn_mode(irn);
	ir_tarval       *z;
	ir_tarval       *o;

	if (m == mode_X) {
		bitinfo *const b = get_bitinfo(get_nodes_block(irn));

		DB((dbg, LEVEL_3, "transfer %+F\n", irn));

		/* Unreachble blocks might have no bitinfo. */
		if (b == NULL || b->z == f) {
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
					bitinfo   *const b        = get_bitinfo(selector);
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
					bitinfo *const b        = get_bitinfo(selector);
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
			bitinfo *const b = get_bitinfo(pred_block);
			if (b != NULL && b->z == t) {
				reachable = true;
				break;
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
		bitinfo *const b = get_bitinfo(get_nodes_block(irn));

		DB((dbg, LEVEL_3, "transfer %+F\n", irn));

		if (b == NULL || b->z == f) {
undefined:
			z = get_mode_null(m);
			o = get_mode_all_one(m);
		} else if (is_Phi(irn)) {
			ir_node *const block = get_nodes_block(irn);

			z = get_mode_null(m);
			o = get_mode_all_one(m);
			foreach_irn_in(block, i, pred_block) {
				bitinfo *const b_cfg = get_bitinfo(pred_block);
				if (b_cfg != NULL && b_cfg->z != f) {
					bitinfo *const b = get_bitinfo(get_Phi_pred(irn, i));
					/* Only use input if it's not undefined. */
					if (!is_undefined(b)) {
						z = tarval_or( z, b->z);
						o = tarval_and(o, b->o);
					}
				}
			}
		} else {
			/* Undefined if any input is undefined. */
			foreach_irn_in(irn, i, pred) {
				bitinfo *const pred_b = get_bitinfo(pred);
				if (pred_b != NULL && is_undefined(pred_b))
					goto undefined;
			}

			/* leverage compute_value_node_XXX knowledge */
			ir_tarval *computed = computed_value(irn);
			if (tarval_is_constant(computed)) {
				z = o = computed;
				goto set_info;
			}

			switch (get_irn_opcode(irn)) {
				case iro_Bad:
					goto undefined;

				case iro_Const: {
					z = o = get_Const_tarval(irn);
					break;
				}

				case iro_Confirm: {
					ir_node *const v = get_Confirm_value(irn);
					bitinfo *const b = get_bitinfo(v);
					/* TODO Use bound and relation. */
					z = b->z;
					o = b->o;
					if ((get_Confirm_relation(irn) & ~ir_relation_unordered) == ir_relation_equal) {
						bitinfo *const bound_b = get_bitinfo(get_Confirm_bound(irn));
						z = tarval_and(z, bound_b->z);
						o = tarval_or( o, bound_b->o);
					}
					break;
				}

				case iro_Shl: {
					ir_node   *const right = get_Shl_right(irn);
					bitinfo   *const l     = get_bitinfo(get_Shl_left(irn));
					bitinfo   *const r     = get_bitinfo(right);
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
						ir_tarval  *const size_mask     = tarval_sub(new_tarval_from_long(size_bits, rmode), rone, NULL);
						ir_tarval  *const modulo_mask   = tarval_sub(new_tarval_from_long(modulo_shift, rmode), rone, NULL);
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
					bitinfo   *const l  = get_bitinfo(get_Shr_left(irn));
					bitinfo   *const r  = get_bitinfo(get_Shr_right(irn));
					ir_tarval *const lz = l->z;
					ir_tarval *const lo = l->o;
					ir_tarval *const rz = r->z;
					if (rz == r->o) {
						z = tarval_shr(lz, rz);
						o = tarval_shr(lo, rz);
					} else {
						int high = get_tarval_highest_bit(lz);
						if (high >= 0) {
							ir_tarval *one  = get_mode_one(m);
							ir_tarval *shl  = tarval_shl_unsigned(one, high);
							ir_tarval *mask = tarval_or(shl, tarval_sub(shl, one, m));
							z = tarval_or(lz, mask);
							o = tarval_andnot(lo, mask);
						} else {
							z = lz;
							o = lo;
						}
					}
					break;
				}

				case iro_Shrs: {
					bitinfo   *const l  = get_bitinfo(get_Shrs_left(irn));
					bitinfo   *const r  = get_bitinfo(get_Shrs_right(irn));
					ir_tarval *const lz = l->z;
					ir_tarval *const lo = l->o;
					ir_tarval *const rz = r->z;
					if (rz == r->o) {
						z = tarval_shrs(lz, rz);
						o = tarval_shrs(lo, rz);
					} else {
						int high = get_tarval_highest_bit(lz);
						if (high >= 0) {
							ir_tarval *one  = get_mode_one(m);
							ir_tarval *shl  = tarval_shl_unsigned(one, high);
							ir_tarval *mask = tarval_or(shl, tarval_sub(shl, one, m));
							z = tarval_or(lz, mask);
							o = tarval_andnot(lo, mask);
						} else {
							z = lz;
							o = lo;
						}
					}
					break;
				}

				case iro_Add: {
					bitinfo   *const l   = get_bitinfo(get_Add_left(irn));
					bitinfo   *const r   = get_bitinfo(get_Add_right(irn));
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
					bitinfo *const l = get_bitinfo(get_Sub_left(irn));
					bitinfo *const r = get_bitinfo(get_Sub_right(irn));
					// might subtract pointers
					if (l == NULL || r == NULL)
						goto cannot_analyse;

					ir_tarval *const lz  = l->z;
					ir_tarval *const lo  = l->o;
					ir_tarval *const rz  = r->z;
					ir_tarval *const ro  = r->o;
					ir_tarval *const vz  = tarval_sub(lo, rz, m);
					ir_tarval *const vo  = tarval_sub(lz, ro, m);
					ir_tarval *const lnc = tarval_eor(lz, lo);
					ir_tarval *const rnc = tarval_eor(rz, ro);
					ir_tarval *const vnc = tarval_eor(vz, vo);
					ir_tarval *const nc  = tarval_or(tarval_or(lnc, rnc), vnc);
					z = tarval_or(vz, nc);
					o = tarval_andnot(vz, nc);
					break;
				}

				case iro_Mul: {
					bitinfo   *const l  = get_bitinfo(get_Mul_left(irn));
					bitinfo   *const r  = get_bitinfo(get_Mul_right(irn));
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
					bitinfo   *const b   = get_bitinfo(get_Minus_op(irn));
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
					bitinfo *const l = get_bitinfo(get_And_left(irn));
					bitinfo *const r = get_bitinfo(get_And_right(irn));
					z = tarval_and(l->z, r->z);
					o = tarval_and(l->o, r->o);
					break;
				}

				case iro_Or: {
					bitinfo *const l = get_bitinfo(get_Or_left(irn));
					bitinfo *const r = get_bitinfo(get_Or_right(irn));
					z = tarval_or(l->z, r->z);
					o = tarval_or(l->o, r->o);
					break;
				}

				case iro_Eor: {
					bitinfo   *const l  = get_bitinfo(get_Eor_left(irn));
					bitinfo   *const r  = get_bitinfo(get_Eor_right(irn));
					ir_tarval *const lz = l->z;
					ir_tarval *const lo = l->o;
					ir_tarval *const rz = r->z;
					ir_tarval *const ro = r->o;
					z = tarval_or(tarval_andnot(lz, ro), tarval_andnot(rz, lo));
					o = tarval_or(tarval_andnot(ro, lz), tarval_andnot(lo, rz));
					break;
				}

				case iro_Not: {
					bitinfo *const b = get_bitinfo(get_Not_op(irn));
					z = tarval_not(b->o);
					o = tarval_not(b->z);
					break;
				}

				case iro_Conv: {
					bitinfo *const b = get_bitinfo(get_Conv_op(irn));
					if (b == NULL) // Happens when converting from float values.
						goto result_unknown;
					z = tarval_convert_to(b->z, m);
					o = tarval_convert_to(b->o, m);
					break;
				}

				case iro_Mux: {
					bitinfo *const bf = get_bitinfo(get_Mux_false(irn));
					bitinfo *const bt = get_bitinfo(get_Mux_true(irn));
					bitinfo *const c  = get_bitinfo(get_Mux_sel(irn));
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
					bitinfo *const l = get_bitinfo(get_Cmp_left(irn));
					bitinfo *const r = get_bitinfo(get_Cmp_right(irn));
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
						bitinfo *const b  = get_bitinfo(op);
						z = b->z;
						o = b->o;
						goto set_info;
					}
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

set_info:
	return set_bitinfo(irn, z, o);
}

static void first_round(ir_node *const irn, void *const env)
{
	pdeq *const worklist = (pdeq*)env;

	transfer(irn);
	if (is_Phi(irn) || is_Block(irn)) {
		/* Only Phis (and their users) need another round, if we did not have
		 * information about all their inputs in the first round, i.e. in loops. */
		/* TODO inserts all Phis, should only insert Phis, which did no have all
		 * predecessors available */
		pdeq_putr(worklist, irn);
	}
}

static void queue_users(pdeq *const worklist, ir_node *const n)
{
	if (get_irn_mode(n) == mode_X) {
		/* When the state of a control flow node changes, not only queue its
		 * successor blocks, but also the Phis in these blocks, because the Phis
		 * must reconsider this input path. */
		foreach_out_edge(n, e) {
			ir_node *const src = get_edge_src_irn(e);
			pdeq_putr(worklist, src);
			/* should always be a block */
			if (is_Block(src)) {
				ir_node *phi;
				for (phi = get_Block_phis(src); phi; phi = get_Phi_next(phi))
					pdeq_putr(worklist, phi);
			}
		}
	} else {
		foreach_out_edge(n, e) {
			ir_node *const src = get_edge_src_irn(e);
			if (get_irn_mode(src) == mode_T) {
				queue_users(worklist, src);
			} else {
				pdeq_putr(worklist, src);
			}
		}
	}
}

static void clear_phi_lists(ir_node *irn, void *env)
{
	(void)env;
	if (is_Block(irn))
		set_Block_phis(irn, NULL);
}

static void build_phi_lists(ir_node *irn, void *env)
{
	(void)env;
	if (is_Phi(irn))
		add_Block_phi(get_nodes_block(irn), irn);
}

void constbits_analyze(ir_graph *const irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.ana.fp-vrp");
	DB((dbg, LEVEL_1,
	    "===> Performing constant propagation on %+F (analysis)\n", irg));

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
	                         | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	ir_reserve_resources(irg, IR_RESOURCE_PHI_LIST);

	obstack_init(&irg->bitinfo.obst);
	ir_nodemap_init(&irg->bitinfo.map, irg);

	/* We need this extra step because the dom tree does not contain
	 * unreachable blocks in Firm. Moreover build phi list. */
	irg_walk_anchors(irg, clear_phi_lists, build_phi_lists, NULL);

	ir_tarval *const f = get_tarval_b_false();
	ir_tarval *const t = get_tarval_b_true();
	set_bitinfo(get_irg_end_block(irg), t, f); /* Reachable. */

	/* TODO Improve iteration order. Best is reverse postorder in data flow
	 * direction and respecting loop nesting for fastest convergence. */
	pdeq *const worklist = new_pdeq();
	irg_walk_blkwise_dom_top_down(irg, NULL, first_round, worklist);

	while (!pdeq_empty(worklist)) {
		ir_node *const n = (ir_node*)pdeq_getl(worklist);
		if (transfer(n))
			queue_users(worklist, n);
	}
	del_pdeq(worklist);

	ir_free_resources(irg, IR_RESOURCE_PHI_LIST);
}

void constbits_clear(ir_graph *const irg)
{
	ir_nodemap_destroy(&irg->bitinfo.map);
	obstack_free(&irg->bitinfo.obst, NULL);
}
