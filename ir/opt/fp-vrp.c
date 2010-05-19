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
 * @version $Id$
 */
#include "config.h"

#include "adt/pdeq.h"
#include "adt/obst.h"
#include "adt/xmalloc.h"
#include "debug.h"
#include "ircons.h"
#include "irdom.h"
#include "iredges.h"
#include "irgmod.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irnode.h"
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
	tarval* z; // safe zeroes, 0 = bit is zero,       1 = bit maybe is 1
	tarval* o; // safe ones,   0 = bit maybe is zero, 1 = bit is 1
} bitinfo;

typedef struct environment_t {
	unsigned modified:1;     /**< Set, if the graph was modified. */
} environment_t;

static inline bitinfo* get_bitinfo(ir_node const* const irn)
{
	return get_irn_link(irn);
}

static int set_bitinfo(ir_node* const irn, tarval* const z, tarval* const o)
{
	bitinfo* b = get_bitinfo(irn);
	if (b == NULL) {
		b = OALLOCZ(&obst, bitinfo);
		set_irn_link(irn, b);
	} else if (z == b->z && o == b->o) {
		return 0;
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
	ir_mode* const m = get_irn_mode(irn);
	tarval*        z;
	tarval*        o;

	if (m == mode_X) {
		DB((dbg, LEVEL_3, "transfer %+F\n", irn));
		switch (get_irn_opcode(irn)) {
			case iro_Proj: {
				ir_node* const pred = get_Proj_pred(irn);
				if (is_Start(pred)) {
					z = get_tarval_b_true();
					o = get_tarval_b_false();
				} else if (is_Cond(pred)) {
					ir_node* const selector = get_Cond_selector(pred);
					bitinfo* const b        = get_bitinfo(selector);
					tarval*  const bz       = b->z;
					tarval*  const bo       = b->o;
					if (get_irn_mode(selector) == mode_b) {
						if (bz == bo) {
							if ((bz == get_tarval_b_true()) == get_Proj_proj(irn)) {
								z = o = get_tarval_b_true();
							} else {
								z = o = get_tarval_b_false();
							}
						} else {
							goto result_unknown_X;
						}
					} else {
						long const val = get_Proj_proj(irn);
						if (val != get_Cond_default_proj(pred)) {
							tarval* const tv = new_tarval_from_long(val, get_irn_mode(selector));
							if (!tarval_is_null(tarval_andnot(tv, bz)) ||
									!tarval_is_null(tarval_andnot(bo, tv))) {
								// At least one bit differs.
								z = o = get_tarval_b_false();
#if 0 // TODO must handle default Proj
							} else if (bz == bo && bz == tv) {
								z = o = get_tarval_b_true();
#endif
							} else {
								goto result_unknown_X;
							}
						} else {
							goto cannot_analyse_X;
						}
					}
				} else {
					goto cannot_analyse_X;
				}
				break;
			}

			case iro_Jmp: {
				bitinfo* const b = get_bitinfo(get_nodes_block(irn));
				z = b->z;
				o = b->o;
				break;
			}

			default:
cannot_analyse_X:
				DB((dbg, LEVEL_4, "cannot analyse %+F\n", irn));
result_unknown_X:
				z = get_tarval_b_true();
				o = get_tarval_b_false();
				break;
		}
	} else if (is_Block(irn)) {
		int       reachable = 0;
		int const arity     = get_Block_n_cfgpreds(irn);
		int       i;

		DB((dbg, LEVEL_3, "transfer %+F\n", irn));
		for (i = 0; i != arity; ++i) {
			bitinfo* const b = get_bitinfo(get_Block_cfgpred(irn, i));
			if (b != NULL && b->z == get_tarval_b_true()) {
				reachable = 1;
				break;
			}
		}

		o = get_tarval_b_false();
		z = reachable || irn == get_irg_start_block(get_irn_irg(irn)) ? get_tarval_b_true() : o;
	} else if (mode_is_intb(m)) {
		DB((dbg, LEVEL_3, "transfer %+F\n", irn));
		switch (get_irn_opcode(irn)) {
			case iro_Const: {
				z = o = get_Const_tarval(irn);
				break;
			}

			case iro_Shl: {
				bitinfo* const l  = get_bitinfo(get_Shl_left(irn));
				bitinfo* const r  = get_bitinfo(get_Shl_right(irn));
				tarval*  const rz = r->z;
				if (rz == r->o) {
					z = tarval_shl(l->z, rz);
					o = tarval_shl(l->o, rz);
				} else {
					goto cannot_analyse;
				}
				break;
			}

			case iro_Shr: {
				bitinfo* const l  = get_bitinfo(get_Shr_left(irn));
				bitinfo* const r  = get_bitinfo(get_Shr_right(irn));
				tarval*  const rz = r->z;
				if (rz == r->o) {
					z = tarval_shr(l->z, rz);
					o = tarval_shr(l->o, rz);
				} else {
					goto cannot_analyse;
				}
				break;
			}

			case iro_Shrs: {
				bitinfo* const l  = get_bitinfo(get_Shrs_left(irn));
				bitinfo* const r  = get_bitinfo(get_Shrs_right(irn));
				tarval*  const rz = r->z;
				if (rz == r->o) {
					z = tarval_shrs(l->z, rz);
					o = tarval_shrs(l->o, rz);
				} else {
					goto cannot_analyse;
				}
				break;
			}

			case iro_Rotl: {
				bitinfo* const l  = get_bitinfo(get_Rotl_left(irn));
				bitinfo* const r  = get_bitinfo(get_Rotl_right(irn));
				tarval*  const rz = r->z;
				if (rz == r->o) {
					z = tarval_rotl(l->z, rz);
					o = tarval_rotl(l->o, rz);
				} else {
					goto cannot_analyse;
				}
				break;
			}

			case iro_Add: {
				bitinfo* const l  = get_bitinfo(get_Add_left(irn));
				bitinfo* const r  = get_bitinfo(get_Add_right(irn));
				tarval*  const lz = l->z;
				tarval*  const lo = l->o;
				tarval*  const rz = r->z;
				tarval*  const ro = r->o;
				if (lz == lo && rz == ro) {
					z = o = tarval_add(lz, rz);
				} else {
					// TODO improve: can only do lower disjoint bits
					/* Determine where any of the operands has zero bits, i.e. where no
					 * carry out is generated if there is not carry in */
					tarval* const no_c_in_no_c_out = tarval_and(lz, rz);
					/* Generate a mask of the lower consecutive zeroes: x | -x.  In this
					 * range the addition is disjoint and therefore Add behaves like Or.
					 */
					tarval* const low_zero_mask = tarval_or(no_c_in_no_c_out, tarval_neg(no_c_in_no_c_out));
					tarval* const low_one_mask  = tarval_not(low_zero_mask);
					z = tarval_or( tarval_or(lz, rz), low_zero_mask);
					o = tarval_and(tarval_or(lo, ro), low_one_mask);
				}
				break;
			}

			case iro_Sub: {
				bitinfo* const l = get_bitinfo(get_Sub_left(irn));
				bitinfo* const r = get_bitinfo(get_Sub_right(irn));
				if (l != NULL && r != NULL) { // Sub might subtract pointers.
					tarval* const lz = l->z;
					tarval* const lo = l->o;
					tarval* const rz = r->z;
					tarval* const ro = r->o;
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
				bitinfo* const l  = get_bitinfo(get_Mul_left(irn));
				bitinfo* const r  = get_bitinfo(get_Mul_right(irn));
				tarval*  const lz = l->z;
				tarval*  const lo = l->o;
				tarval*  const rz = r->z;
				tarval*  const ro = r->o;
				if (lz == lo && rz == ro) {
					z = o = tarval_mul(lz, rz);
				} else {
					// TODO improve
					// Determine safe lower zeroes: x | -x.
					tarval* const lzn = tarval_or(lz, tarval_neg(lz));
					tarval* const rzn = tarval_or(rz, tarval_neg(rz));
					// Concatenate safe lower zeroes.
					if (tarval_cmp(lzn, rzn) == pn_Cmp_Lt) {
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
				bitinfo* const l  = get_bitinfo(get_Eor_left(irn));
				bitinfo* const r  = get_bitinfo(get_Eor_right(irn));
				tarval*  const lz = l->z;
				tarval*  const lo = l->o;
				tarval*  const rz = r->z;
				tarval*  const ro = r->o;
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
				bitinfo* const f = get_bitinfo(get_Mux_false(irn));
				bitinfo* const t = get_bitinfo(get_Mux_true(irn));
				bitinfo* const c = get_bitinfo(get_Mux_sel(irn));
				if (c->o == get_tarval_b_true()) {
					z = t->z;
					o = t->o;
				} else if (c->z == get_tarval_b_false()) {
					z = f->z;
					o = f->o;
				} else {
					z = tarval_or( f->z, t->z);
					o = tarval_and(f->o, t->o);
				}
				break;
			}

			case iro_Phi: {
				ir_node* const block = get_nodes_block(irn);
				int      const arity = get_Phi_n_preds(irn);
				int            i;

				z = get_tarval_null(m);
				o = get_tarval_all_one(m);
				for (i = 0; i != arity; ++i) {
					bitinfo* const b_cfg = get_bitinfo(get_Block_cfgpred(block, i));
					if (b_cfg != NULL && b_cfg->z != get_tarval_b_false()) {
						bitinfo* const b = get_bitinfo(get_Phi_pred(irn, i));
						z = tarval_or( z, b->z);
						o = tarval_and(o, b->o);
					}
				}
				break;
			}

			case iro_Proj: {
				ir_node* const pred = get_Proj_pred(irn);
				if (is_Cmp(pred)) { // TODO generalize
					bitinfo* const l = get_bitinfo(get_Cmp_left(pred));
					bitinfo* const r = get_bitinfo(get_Cmp_right(pred));
					if (l == NULL || r == NULL)
						goto result_unknown; // Cmp compares something we cannot evaluate.
					switch (get_Proj_proj(irn)) {
						case pn_Cmp_Lg: {
							tarval* const lz = l->z;
							tarval* const lo = l->o;
							tarval* const rz = r->z;
							tarval* const ro = r->o;
							if (!tarval_is_null(tarval_andnot(ro, lz)) ||
									!tarval_is_null(tarval_andnot(lo, rz))) {
								// At least one bit differs.
								z = o = get_tarval_b_true();
							} else if (lz == lo && rz == ro && lz == rz) {
								z = o = get_tarval_b_false();
							} else {
								goto result_unknown;
							}
							break;
						}

						case pn_Cmp_Eq: {
							tarval* const lz = l->z;
							tarval* const lo = l->o;
							tarval* const rz = r->z;
							tarval* const ro = r->o;
							if (!tarval_is_null(tarval_andnot(ro, lz)) ||
									!tarval_is_null(tarval_andnot(lo, rz))) {
								// At least one bit differs.
								z = o = get_tarval_b_false();
							} else if (lz == lo && rz == ro && lz == rz) {
								z = o = get_tarval_b_true();
							} else {
								goto result_unknown;
							}
							break;
						}

						default:
							goto cannot_analyse;
					}
				} else {
					goto cannot_analyse;
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
	} else {
		return 0;
	}

	return set_bitinfo(irn, z, o);
}

static void first_round(ir_node* const irn, void* const env)
{
	pdeq* const q = env;

	transfer(irn);
	if (is_Phi(irn) || is_Block(irn)) {
		/* Only Phis (and their users) need another round, if we did not have
		 * information about all their inputs in the first round, i.e. in loops. */
		/* TODO inserts all Phis, should only insert Phis, which did no have all
		 * predecessors available */
		pdeq_putr(q, irn);
	}
}

static void apply_result(ir_node* const irn, void* ctx)
{
	bitinfo* const b = get_bitinfo(irn);
	tarval*        z;
	tarval*        o;
	environment_t* env = ctx;

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
			n = new_Const(z);
		} else if (m == mode_X) {
			ir_node*  const block = get_nodes_block(irn);
			ir_graph* const irg   = get_Block_irg(block);
			if (z == get_tarval_b_true()) {
				// Might produce an endless loop, so keep the block.
				add_End_keepalive(get_irg_end(irg), block);
				n = new_r_Jmp(block);
			} else {
				n = new_r_Bad(irg);
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
			if (bl->z == bl->o) {
				if (tarval_is_null(tarval_andnot(br->z, bl->z))) {
					DB((dbg, LEVEL_2, "%+F(%+F, %+F) is superfluous\n", irn, l, r));
					exchange(irn, r);
					env->modified = 1;
				}
			} else if (br->z == br->o) {
				if (tarval_is_null(tarval_andnot(bl->z, br->z))) {
					DB((dbg, LEVEL_2, "%+F(%+F, %+F) is superfluous\n", irn, l, r));
					exchange(irn, l);
					env->modified = 1;
				}
			}
			break;
		}

		case iro_Or: {
			ir_node*       const l  = get_Or_left(irn);
			ir_node*       const r  = get_Or_right(irn);
			bitinfo const* const bl = get_bitinfo(l);
			bitinfo const* const br = get_bitinfo(r);
			if (bl->z == bl->o) {
				if (tarval_is_null(tarval_andnot(bl->o, br->o))) {
					DB((dbg, LEVEL_2, "%+F(%+F, %+F) is superfluous\n", irn, l, r));
					exchange(irn, r);
					env->modified = 1;
				}
			} else if (br->z == br->o) {
				if (tarval_is_null(tarval_andnot(br->o, bl->o))) {
					DB((dbg, LEVEL_2, "%+F(%+F, %+F) is superfluous\n", irn, l, r));
					exchange(irn, l);
					env->modified = 1;
				}
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

	obstack_init(&obst);

	edges_assure(irg);
	assure_doms(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	{
		pdeq* const q = new_pdeq();

		/* We need this extra step because the dom tree does not contain unreachable
		   blocks in Firm. Moreover build phi list. */
		irg_walk_graph(irg, clear_links, build_phi_lists, NULL);

		/* TODO Improve iteration order. Best is reverse postorder in data flow
		 * direction and respecting loop nesting for fastest convergence. */
		irg_walk_blkwise_dom_top_down(irg, firm_clear_link, first_round, q);

		while (!pdeq_empty(q)) {
			ir_node* const n = pdeq_getl(q);
			if (transfer(n))
				queue_users(q, n);
		}

		del_pdeq(q);
	}

	DB((dbg, LEVEL_2, "---> Applying analysis results\n"));
	env.modified = 0;
	irg_walk_graph(irg, NULL, apply_result, &env);

	if (env.modified) {
		/* control flow might changed */
		set_irg_outs_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_doms_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
		set_irg_entity_usage_state(irg, ir_entity_usage_not_computed);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	obstack_free(&obst, NULL);
}

ir_graph_pass_t *fixpoint_vrp_irg_pass(const char *name)
{
	return def_graph_pass(name ? name : "fixpoint_vrp", fixpoint_vrp);
}
