/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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
 * @brief   Data-flow driven minimal fixpoint value range propagation
 * @author  Christoph Mallon
 */
#include "config.h"

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
#include "iroptimize.h"
#include "irtools.h"
#include "tv.h"
#include "irpass.h"
#include "irmemory.h"

/* TODO:
 * - Implement cleared/set bit calculation for Add, Sub, Minus, Mul, Div, Mod, Shl, Shr, Shrs, Rotl
 * - Implement min/max calculation for And, Eor, Or, Not, Conv, Shl, Shr, Shrs, Rotl, Mux
 * - Implement min/max calculation for Add, Sub, Minus, Mul, Div, Mod, Conv, Shl, Shr, Shrs, Rotl, Mux
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

static struct obstack obst;

typedef struct bitinfo
{
	ir_tarval* z; // safe zeroes, 0 = bit is zero,       1 = bit maybe is 1
	ir_tarval* o; // safe ones,   0 = bit maybe is zero, 1 = bit is 1
} bitinfo;

typedef struct environment_t {
	unsigned modified:1;     /**< Set, if the graph was modified. */
} environment_t;

static bool is_undefined(bitinfo const* const b)
{
	return tarval_is_null(b->z) && tarval_is_all_one(b->o);
}

static inline bitinfo* get_bitinfo(ir_node const* const irn)
{
	return (bitinfo*)get_irn_link(irn);
}

static int set_bitinfo(ir_node* const irn, ir_tarval* const z, ir_tarval* const o)
{
	bitinfo* b = get_bitinfo(irn);
	if (b == NULL) {
		b = OALLOCZ(&obst, bitinfo);
		set_irn_link(irn, b);
	} else if (z == b->z && o == b->o) {
		return 0;
	} else {
		/* Assert monotonicity. */
		assert(tarval_is_null(tarval_andnot(b->z, z)));
		assert(tarval_is_null(tarval_andnot(o, b->o)));
	}
	b->z = z;
	b->o = o;
	DB((dbg, LEVEL_3, "%+F: 0:%T 1:%T\n", irn, z, o));
	return 1;
}

static int mode_is_intb(ir_mode const* const m)
{
	return mode_is_int(m) || m == mode_b;
}

static int transfer(ir_node* const irn)
{
	ir_tarval* const f = get_tarval_b_false();
	ir_tarval* const t = get_tarval_b_true();
	ir_mode*   const m = get_irn_mode(irn);
	ir_tarval*       z;
	ir_tarval*       o;

	if (is_Bad(irn)) return 0;

	if (m == mode_X) {
		bitinfo* const b = get_bitinfo(get_nodes_block(irn));

		DB((dbg, LEVEL_3, "transfer %+F\n", irn));

		/* Unreachble blocks might have no bitinfo. */
		if (b == NULL || b->z == f) {
unreachable_X:
			z = f;
			o = t;
		} else switch (get_irn_opcode(irn)) {
			case iro_Proj: {
				ir_node* const pred = get_Proj_pred(irn);
				if (is_Start(pred)) {
					goto result_unknown_X;
				} else if (is_Cond(pred)) {
					ir_node*   const selector = get_Cond_selector(pred);
					bitinfo*   const b        = get_bitinfo(selector);
					if (is_undefined(b))
						goto unreachable_X;
					if (b->z == b->o) {
						if ((b->z == t) == get_Proj_proj(irn)) {
							z = o = t;
						} else {
							z = o = f;
						}
					} else {
						goto result_unknown_X;
					}
				} else if (is_Switch(pred)) {
					ir_node* const selector = get_Switch_selector(pred);
					bitinfo* const b        = get_bitinfo(selector);
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
		int       reachable = 0;
		int const arity     = get_Block_n_cfgpreds(irn);
		int       i;

		DB((dbg, LEVEL_3, "transfer %+F\n", irn));
		for (i = 0; i != arity; ++i) {
			bitinfo* const b = get_bitinfo(get_Block_cfgpred(irn, i));
			if (b != NULL && b->z == t) {
				reachable = 1;
				break;
			}
		}

		if (!reachable) {
			ir_graph *const irg = get_Block_irg(irn);
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
		bitinfo* const b = get_bitinfo(get_nodes_block(irn));

		DB((dbg, LEVEL_3, "transfer %+F\n", irn));

		if (b == NULL || b->z == f) {
undefined:
			z = get_tarval_null(m);
			o = get_tarval_all_one(m);
		} else if (is_Phi(irn)) {
			ir_node* const block = get_nodes_block(irn);
			int      const arity = get_Phi_n_preds(irn);
			int            i;

			z = get_tarval_null(m);
			o = get_tarval_all_one(m);
			for (i = 0; i != arity; ++i) {
				bitinfo* const b_cfg = get_bitinfo(get_Block_cfgpred(block, i));
				if (b_cfg != NULL && b_cfg->z != f) {
					bitinfo* const b = get_bitinfo(get_Phi_pred(irn, i));
					/* Only use input if it's not undefined. */
					if (!is_undefined(b)) {
						z = tarval_or( z, b->z);
						o = tarval_and(o, b->o);
					}
				}
			}
		} else {
			int const arity = get_irn_arity(irn);
			int       i;

			/* Undefined if any input is undefined. */
			for (i = 0; i != arity; ++i) {
				ir_node* const pred   = get_irn_n(irn, i);
				bitinfo* const pred_b = get_bitinfo(pred);
				if (pred_b != NULL && is_undefined(pred_b))
					goto undefined;
			}

			switch (get_irn_opcode(irn)) {
				case iro_Const: {
					z = o = get_Const_tarval(irn);
					break;
				}

				case iro_Confirm: {
					ir_node* const v = get_Confirm_value(irn);
					bitinfo* const b = get_bitinfo(v);
					/* TODO Use bound and relation. */
					z = b->z;
					o = b->o;
					if ((get_Confirm_relation(irn) & ~ir_relation_unordered) == ir_relation_equal) {
						bitinfo* const bound_b = get_bitinfo(get_Confirm_bound(irn));
						z = tarval_and(z, bound_b->z);
						o = tarval_or( o, bound_b->o);
					}
					break;
				}

				case iro_Shl: {
					bitinfo*   const l  = get_bitinfo(get_Shl_left(irn));
					bitinfo*   const r  = get_bitinfo(get_Shl_right(irn));
					ir_tarval* const rz = r->z;
					if (rz == r->o) {
						z = tarval_shl(l->z, rz);
						o = tarval_shl(l->o, rz);
					} else {
						goto cannot_analyse;
					}
					break;
				}

				case iro_Shr: {
					bitinfo*   const l  = get_bitinfo(get_Shr_left(irn));
					bitinfo*   const r  = get_bitinfo(get_Shr_right(irn));
					ir_tarval* const rz = r->z;
					if (rz == r->o) {
						z = tarval_shr(l->z, rz);
						o = tarval_shr(l->o, rz);
					} else {
						goto cannot_analyse;
					}
					break;
				}

				case iro_Shrs: {
					bitinfo*   const l  = get_bitinfo(get_Shrs_left(irn));
					bitinfo*   const r  = get_bitinfo(get_Shrs_right(irn));
					ir_tarval* const rz = r->z;
					if (rz == r->o) {
						z = tarval_shrs(l->z, rz);
						o = tarval_shrs(l->o, rz);
					} else {
						goto cannot_analyse;
					}
					break;
				}

				case iro_Rotl: {
					bitinfo*   const l  = get_bitinfo(get_Rotl_left(irn));
					bitinfo*   const r  = get_bitinfo(get_Rotl_right(irn));
					ir_tarval* const rz = r->z;
					if (rz == r->o) {
						z = tarval_rotl(l->z, rz);
						o = tarval_rotl(l->o, rz);
					} else {
						goto cannot_analyse;
					}
					break;
				}

				case iro_Add: {
					bitinfo*   const l  = get_bitinfo(get_Add_left(irn));
					bitinfo*   const r  = get_bitinfo(get_Add_right(irn));
					ir_tarval* const lz = l->z;
					ir_tarval* const lo = l->o;
					ir_tarval* const rz = r->z;
					ir_tarval* const ro = r->o;
					if (lz == lo && rz == ro) {
						z = o = tarval_add(lz, rz);
					} else {
						// TODO improve: can only do lower disjoint bits
						/* Determine where any of the operands has zero bits, i.e. where no
						 * carry out is generated if there is not carry in */
						ir_tarval* const no_c_in_no_c_out = tarval_and(lz, rz);
						/* Generate a mask of the lower consecutive zeroes: x | -x.  In this
						 * range the addition is disjoint and therefore Add behaves like Or.
						 */
						ir_tarval* const low_zero_mask = tarval_or(no_c_in_no_c_out, tarval_neg(no_c_in_no_c_out));
						ir_tarval* const low_one_mask  = tarval_not(low_zero_mask);
						z = tarval_or( tarval_or(lz, rz), low_zero_mask);
						o = tarval_and(tarval_or(lo, ro), low_one_mask);
					}
					break;
				}

				case iro_Sub: {
					bitinfo* const l = get_bitinfo(get_Sub_left(irn));
					bitinfo* const r = get_bitinfo(get_Sub_right(irn));
					if (l != NULL && r != NULL) { // Sub might subtract pointers.
						ir_tarval* const lz = l->z;
						ir_tarval* const lo = l->o;
						ir_tarval* const rz = r->z;
						ir_tarval* const ro = r->o;
						if (lz == lo && rz == ro) {
							z = o = tarval_sub(lz, rz, NULL);
						} else if (tarval_is_null(tarval_andnot(rz, lo))) {
							/* Every possible one of the subtrahend is backed by a safe one of the
							 * minuend, i.e. there are no borrows. */
							// TODO extend no-borrow like carry for Add above
							z = tarval_andnot(lz, ro);
							o = tarval_andnot(lo, rz);
						} else {
							goto cannot_analyse;
						}
					} else {
						goto cannot_analyse;
					}
					break;
				}

				case iro_Mul: {
					bitinfo*   const l  = get_bitinfo(get_Mul_left(irn));
					bitinfo*   const r  = get_bitinfo(get_Mul_right(irn));
					ir_tarval* const lz = l->z;
					ir_tarval* const lo = l->o;
					ir_tarval* const rz = r->z;
					ir_tarval* const ro = r->o;
					if (lz == lo && rz == ro) {
						z = o = tarval_mul(lz, rz);
					} else {
						// TODO improve
						// Determine safe lower zeroes: x | -x.
						ir_tarval* const lzn = tarval_or(lz, tarval_neg(lz));
						ir_tarval* const rzn = tarval_or(rz, tarval_neg(rz));
						// Concatenate safe lower zeroes.
						if (tarval_cmp(lzn, rzn) == ir_relation_less) {
							z = tarval_mul(tarval_eor(lzn, tarval_shl(lzn, get_tarval_one(m))), rzn);
						} else {
							z = tarval_mul(tarval_eor(rzn, tarval_shl(rzn, get_tarval_one(m))), lzn);
						}
						o = get_tarval_null(m);
					}
					break;
				}

				case iro_Minus: {
					bitinfo* const b = get_bitinfo(get_Minus_op(irn));
					if (b->z == b->o) {
						z = o = tarval_neg(b->z);
					} else {
						goto cannot_analyse;
					}
					break;
				}

				case iro_And: {
					bitinfo* const l = get_bitinfo(get_And_left(irn));
					bitinfo* const r = get_bitinfo(get_And_right(irn));
					z = tarval_and(l->z, r->z);
					o = tarval_and(l->o, r->o);
					break;
				}

				case iro_Or: {
					bitinfo* const l = get_bitinfo(get_Or_left(irn));
					bitinfo* const r = get_bitinfo(get_Or_right(irn));
					z = tarval_or(l->z, r->z);
					o = tarval_or(l->o, r->o);
					break;
				}

				case iro_Eor: {
					bitinfo*   const l  = get_bitinfo(get_Eor_left(irn));
					bitinfo*   const r  = get_bitinfo(get_Eor_right(irn));
					ir_tarval* const lz = l->z;
					ir_tarval* const lo = l->o;
					ir_tarval* const rz = r->z;
					ir_tarval* const ro = r->o;
					z = tarval_or(tarval_andnot(lz, ro), tarval_andnot(rz, lo));
					o = tarval_or(tarval_andnot(ro, lz), tarval_andnot(lo, rz));
					break;
				}

				case iro_Not: {
					bitinfo* const b = get_bitinfo(get_Not_op(irn));
					z = tarval_not(b->o);
					o = tarval_not(b->z);
					break;
				}

				case iro_Conv: {
					bitinfo* const b = get_bitinfo(get_Conv_op(irn));
					if (b == NULL) // Happens when converting from float values.
						goto result_unknown;
					z = tarval_convert_to(b->z, m);
					o = tarval_convert_to(b->o, m);
					break;
				}

				case iro_Mux: {
					bitinfo* const bf = get_bitinfo(get_Mux_false(irn));
					bitinfo* const bt = get_bitinfo(get_Mux_true(irn));
					bitinfo* const c  = get_bitinfo(get_Mux_sel(irn));
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
					bitinfo* const l = get_bitinfo(get_Cmp_left(irn));
					bitinfo* const r = get_bitinfo(get_Cmp_right(irn));
					if (l == NULL || r == NULL) {
						goto result_unknown; // Cmp compares something we cannot evaluate.
					} else {
						ir_tarval*  const lz       = l->z;
						ir_tarval*  const lo       = l->o;
						ir_tarval*  const rz       = r->z;
						ir_tarval*  const ro       = r->o;
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
					}
					break;
				}

				default: {
cannot_analyse:
					DB((dbg, LEVEL_4, "cannot analyse %+F\n", irn));
result_unknown:
					z = get_tarval_all_one(m);
					o = get_tarval_null(m);
					break;
				}
			}
		}
	} else {
		return 0;
	}

	return set_bitinfo(irn, z, o);
}

static void first_round(ir_node* const irn, void* const env)
{
	pdeq* const q = (pdeq*)env;

	transfer(irn);
	if (is_Phi(irn) || is_Block(irn)) {
		/* Only Phis (and their users) need another round, if we did not have
		 * information about all their inputs in the first round, i.e. in loops. */
		/* TODO inserts all Phis, should only insert Phis, which did no have all
		 * predecessors available */
		pdeq_putr(q, irn);
	}
}

static ir_node *make_bad_block(ir_graph *irg)
{
	ir_node *bad = new_r_Bad(irg, mode_BB);
	bitinfo *bb  = get_bitinfo(bad);
	if (bb == NULL) {
		ir_tarval* const f = get_tarval_b_false();
		ir_tarval* const t = get_tarval_b_true();
		set_bitinfo(bad, f, t); /* Undefined. */
	}
	return bad;
}

static void apply_result(ir_node* const irn, void* ctx)
{
	environment_t* env = (environment_t*)ctx;
	ir_node*       block;
	bitinfo*       block_b;
	bitinfo*       b;
	ir_tarval*     z;
	ir_tarval*     o;

	if (is_Block(irn)) {
		block_b = get_bitinfo(irn);
		/* Trivially unreachable blocks have no info. */
		if (block_b == NULL || block_b->z == get_tarval_b_false()) {
			ir_node  *bad = make_bad_block(get_irn_irg(irn));
			exchange(irn, bad);
			env->modified = 1;
		}
		return;
	}

	block   = get_nodes_block(irn);
	block_b = get_bitinfo(block);
	/* Trivially unreachable blocks have no info. */
	if (block_b == NULL || block_b->z == get_tarval_b_false()) {
		/* Unreachable blocks might be replaced before the nodes in them. */
		ir_mode  *mode = get_irn_mode(irn);
		ir_graph *irg  = get_irn_irg(irn);
		ir_node  *bad  = new_r_Bad(irg, mode);
		exchange(irn, bad);
		env->modified = 1;
		return;
	}

	b = get_bitinfo(irn);
	if (!b) return;
	if (is_Const(irn)) return; // It cannot get any better than a Const.

	z = b->z;
	o = b->o;
	// Only display information if we could find out anything about the value.
	DEBUG_ONLY(if (!tarval_is_all_one(z) || !tarval_is_null(o)))
		DB((dbg, LEVEL_2, "%+F: 0:%T 1:%T%s\n", irn, z, o, z == o ? " --- constant" : ""));

	// Replace node with constant value by Const.
	if (z == o) {
		ir_mode* const m = get_irn_mode(irn);
		ir_node*       n;
		if (mode_is_intb(m)) {
			ir_graph *irg = get_irn_irg(irn);
			n = new_r_Const(irg, z);
		} else if (m == mode_X) {
			ir_graph* const irg = get_Block_irg(block);
			if (z == get_tarval_b_true()) {
				// Might produce an endless loop, so keep the block.
				add_End_keepalive(get_irg_end(irg), block);
				n = new_r_Jmp(block);
			} else {
				n = new_r_Bad(irg, mode_X);
				/* Transferring analysis information to the bad node makes it a
				 * candidate for replacement. */
				goto exchange_only;
			}
		} else {
			return;
		}
		set_irn_link(n, b);
exchange_only:
		exchange(irn, n);
		env->modified = 1;
	}

	switch (get_irn_opcode(irn)) {
		case iro_And: {
			ir_node*       const l  = get_And_left(irn);
			ir_node*       const r  = get_And_right(irn);
			bitinfo const* const bl = get_bitinfo(l);
			bitinfo const* const br = get_bitinfo(r);
			if (tarval_is_null(tarval_andnot(br->z, bl->o))) {
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) is superfluous\n", irn, l, r));
				exchange(irn, r);
				env->modified = 1;
			} else if (tarval_is_null(tarval_andnot(bl->z, br->o))) {
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) is superfluous\n", irn, l, r));
				exchange(irn, l);
				env->modified = 1;
			}
			break;
		}

		case iro_Eor: {
			ir_node*       const l  = get_Eor_left(irn);
			ir_node*       const r  = get_Eor_right(irn);
			bitinfo const* const bl = get_bitinfo(l);
			bitinfo const* const br = get_bitinfo(r);
			/* if each bit is guaranteed to be zero on either the left or right
			 * then an Add will have the same effect as the Eor. Change it for
			 * normalisation */
			if (tarval_is_null(tarval_and(bl->z, br->z))) {
				dbg_info      *dbgi     = get_irn_dbg_info(irn);
				ir_node       *block    = get_nodes_block(irn);
				ir_mode       *mode     = get_irn_mode(irn);
				ir_node       *new_node = new_rd_Add(dbgi, block, l, r, mode);
				bitinfo const *bi       = get_bitinfo(irn);
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) normalised to Add\n", irn, l, r));
				set_bitinfo(new_node, bi->z, bi->o);
				exchange(irn, new_node);
				env->modified = 1;
			}
			break;
		}

		case iro_Or: {
			ir_node*       const l  = get_Or_left(irn);
			ir_node*       const r  = get_Or_right(irn);
			bitinfo const* const bl = get_bitinfo(l);
			bitinfo const* const br = get_bitinfo(r);
			if (tarval_is_null(tarval_andnot(bl->z, br->o))) {
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) is superfluous\n", irn, l, r));
				exchange(irn, r);
				env->modified = 1;
			} else if (tarval_is_null(tarval_andnot(br->z, bl->o))) {
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) is superfluous\n", irn, l, r));
				exchange(irn, l);
				env->modified = 1;
			}

			/* if each bit is guaranteed to be zero on either the left or right
			 * then an Add will have the same effect as the Or. Change it for
			 * normalisation */
			if (tarval_is_null(tarval_and(bl->z, br->z))) {
				dbg_info      *dbgi     = get_irn_dbg_info(irn);
				ir_node       *block    = get_nodes_block(irn);
				ir_mode       *mode     = get_irn_mode(irn);
				ir_node       *new_node = new_rd_Add(dbgi, block, l, r, mode);
				bitinfo const *bi       = get_bitinfo(irn);
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) normalised to Add\n", irn, l, r));
				set_bitinfo(new_node, bi->z, bi->o);
				exchange(irn, new_node);
				env->modified = 1;
			}

			break;
		}
	}
}

static void queue_users(pdeq* const q, ir_node* const n)
{
	if (get_irn_mode(n) == mode_X) {
		/* When the state of a control flow node changes, not only queue its
		 * successor blocks, but also the Phis in these blocks, because the Phis
		 * must reconsider this input path. */
		ir_edge_t const* e;
		foreach_out_edge(n, e) {
			ir_node*  const  src = get_edge_src_irn(e);
			pdeq_putr(q, src);
			/* should always be a block */
			if (is_Block(src)) {
				ir_node *phi;
				for (phi = get_Block_phis(src); phi; phi = get_Phi_next(phi))
					pdeq_putr(q, phi);
			}
		}
	} else {
		ir_edge_t const* e;
		foreach_out_edge(n, e) {
			ir_node* const src = get_edge_src_irn(e);
			if (get_irn_mode(src) == mode_T) {
				queue_users(q, src);
			} else {
				pdeq_putr(q, src);
			}
		}
	}
}

static void clear_links(ir_node *irn, void *env)
{
	(void) env;
	set_irn_link(irn, NULL);
	if (is_Block(irn))
		set_Block_phis(irn, NULL);
}

static void build_phi_lists(ir_node *irn, void *env)
{
	(void) env;
	if (is_Phi(irn))
		add_Block_phi(get_nodes_block(irn), irn);
}

void fixpoint_vrp(ir_graph* const irg)
{
	environment_t env;

	FIRM_DBG_REGISTER(dbg, "firm.opt.fp-vrp");
	DB((dbg, LEVEL_1, "===> Performing constant propagation on %+F\n", irg));

	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
		| IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		| IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	obstack_init(&obst);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	{
		pdeq* const q = new_pdeq();

		/* We need this extra step because the dom tree does not contain
		 * unreachable blocks in Firm. Moreover build phi list. */
		irg_walk_anchors(irg, clear_links, build_phi_lists, NULL);

		{
			ir_tarval* const f = get_tarval_b_false();
			ir_tarval* const t = get_tarval_b_true();
			set_bitinfo(get_irg_end_block(irg), t, f); /* Reachable. */
		}

		/* TODO Improve iteration order. Best is reverse postorder in data flow
		 * direction and respecting loop nesting for fastest convergence. */
		irg_walk_blkwise_dom_top_down(irg, NULL, first_round, q);

		while (!pdeq_empty(q)) {
			ir_node* const n = (ir_node*)pdeq_getl(q);
			if (transfer(n))
				queue_users(q, n);
		}

		del_pdeq(q);
	}

	DB((dbg, LEVEL_2, "---> Applying analysis results\n"));
	env.modified = 0;
	irg_walk_graph(irg, NULL, apply_result, &env);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	obstack_free(&obst, NULL);
	confirm_irg_properties(irg,
		env.modified ? IR_GRAPH_PROPERTIES_NONE : IR_GRAPH_PROPERTIES_ALL);
}

ir_graph_pass_t *fixpoint_vrp_irg_pass(const char *name)
{
	return def_graph_pass(name ? name : "fixpoint_vrp", fixpoint_vrp);
}
