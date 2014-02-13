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
#include "tv.h"
#include "irmemory.h"
#include "constbits.h"

/* TODO:
 * - Implement cleared/set bit calculation for Add, Sub, Minus, Mul, Div, Mod, Shl, Shr, Shrs
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

static struct obstack *obst;

static bool is_undefined(bitinfo const* const b)
{
	return tarval_is_null(b->z) && tarval_is_all_one(b->o);
}

bitinfo* get_bitinfo(ir_node const* const irn)
{
	ir_graph   *const irg = get_irn_irg(irn);
	ir_nodemap *const map = &irg->bitinfo.map;
	if (map->data == NULL)
		return NULL;

	return ir_nodemap_get(bitinfo, map, irn);
}

int set_bitinfo(ir_node* const irn, ir_tarval* const z, ir_tarval* const o)
{
	bitinfo* b = get_bitinfo(irn);
	if (b == NULL) {
		ir_graph   *const irg = get_irn_irg(irn);
		ir_nodemap *const map = &irg->bitinfo.map;
		b = OALLOCZ(obst, bitinfo);
		ir_nodemap_insert(map, irn, b);
	} else if (z == b->z && o == b->o) {
		return 0;
	} else {
		/* Assert ascending chain. */
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
			/* Undefined if any input is undefined. */
			foreach_irn_in(irn, i, pred) {
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
							z = tarval_mul(tarval_eor(lzn, tarval_shl_unsigned(lzn, 1)), rzn);
						} else {
							z = tarval_mul(tarval_eor(rzn, tarval_shl_unsigned(rzn, 1)), lzn);
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

static void queue_users(pdeq* const q, ir_node* const n)
{
	if (get_irn_mode(n) == mode_X) {
		/* When the state of a control flow node changes, not only queue its
		 * successor blocks, but also the Phis in these blocks, because the Phis
		 * must reconsider this input path. */
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

static void clear_phi_lists(ir_node *irn, void *env)
{
	(void) env;
	if (is_Block(irn))
		set_Block_phis(irn, NULL);
}

static void build_phi_lists(ir_node *irn, void *env)
{
	(void) env;
	if (is_Phi(irn))
		add_Block_phi(get_nodes_block(irn), irn);
}

void constbits_analyze(ir_graph* const irg, struct obstack *client_obst)
{
	obst = client_obst;
	ir_nodemap_init(&irg->bitinfo.map, irg);

	FIRM_DBG_REGISTER(dbg, "firm.ana.fp-vrp");
	DB((dbg, LEVEL_1, "===> Performing constant propagation on %+F (analysis)\n", irg));

	assert(((ir_resources_reserved(irg) & IR_RESOURCE_PHI_LIST) != 0) &&
			"user of fp-vrp analysis must reserve phi list");

	{
		pdeq* const q = new_pdeq();

		/* We need this extra step because the dom tree does not contain
		 * unreachable blocks in Firm. Moreover build phi list. */
		irg_walk_anchors(irg, clear_phi_lists, build_phi_lists, NULL);

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
}

void constbits_clear(ir_graph* const irg)
{
	ir_nodemap_destroy(&irg->bitinfo.map);
}
