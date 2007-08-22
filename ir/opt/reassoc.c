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
 * @brief   Reassociation
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "iropt_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "irgmod.h"
#include "iropt_dbg.h"
#include "irflag_t.h"
#include "irgwalk.h"
#include "reassoc_t.h"
#include "irhooks.h"
#include "irloop.h"
#include "pdeq.h"
#include "debug.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

typedef struct _walker_t {
	int   changes;        /**< set, if a reassociation take place */
	waitq *wq;            /**< a wait queue */
} walker_t;

typedef enum {
	NO_CONSTANT   = 0,    /**< node is not constant */
	REAL_CONSTANT = 1,    /**< node is a Const that is suitable for constant folding */
	REGION_CONST  = 4     /**< node is a constant expression in the current context,
	                           use 4 here to simplify implementation of get_comm_Binop_ops() */
} const_class_t;

/**
 * returns whether a node is constant ie is a constant or
 * is loop invariant (called region constant)
 *
 * @param n     the node to be checked for constant
 * @param block a block that might be in a loop
 */
static const_class_t get_const_class(ir_node *n, ir_node *block)
{
	ir_op *op = get_irn_op(n);

	if (op == op_Const)
		return REAL_CONSTANT;

	/* although SymConst's are of course real constant, we cannot
	   fold them, so handle them like region constants */
	if (op == op_SymConst)
		return REGION_CONST;

	/*
	 * Beware: Bad nodes are always loop-invariant, but
	 * cannot handled in later code, so filter them here.
	 */
	if (! is_Bad(n) && is_loop_invariant(n, block))
		return REGION_CONST;

	return NO_CONSTANT;
}  /* get_const_class */

/**
 * returns the operands of a commutative bin-op, if one operand is
 * a region constant, it is returned as the second one.
 *
 * Beware: Real constants must be returned with higher priority than
 * region constants, because they might be folded.
 */
static void get_comm_Binop_ops(ir_node *binop, ir_node **a, ir_node **c)
{
	ir_node *op_a = get_binop_left(binop);
	ir_node *op_b = get_binop_right(binop);
	ir_node *block = get_nodes_block(binop);
	int class_a = get_const_class(op_a, block);
	int class_b = get_const_class(op_b, block);

	assert(is_op_commutative(get_irn_op(binop)));

	switch (class_a + 2*class_b) {
	case REAL_CONSTANT + 2*REAL_CONSTANT:
		/* if both are constants, one might be a
		 * pointer constant like NULL, return the other
		 */
		if (mode_is_reference(get_irn_mode(op_a))) {
			*a = op_a;
			*c = op_b;
		} else {
			*a = op_b;
			*c = op_a;
		}
		break;
	case REAL_CONSTANT + 2*NO_CONSTANT:
	case REAL_CONSTANT + 2*REGION_CONST:
	case REGION_CONST  + 2*NO_CONSTANT:
		*a = op_b;
		*c = op_a;
		break;
	default:
		*a = op_a;
		*c = op_b;
		break;
	}
}  /* get_comm_Binop_ops */

/**
 * reassociate a Sub: x - c = x + (-c)
 */
static int reassoc_Sub(ir_node **in)
{
	ir_node *n = *in;
	ir_node *right = get_Sub_right(n);
	ir_mode *rmode = get_irn_mode(right);
	ir_node *block;

	/* cannot handle SubIs(P, P) */
	if (mode_is_reference(rmode))
		return 0;

	block = get_nodes_block(n);

	/* handles rule R6:
	 * convert x - c => x + (-c)
	 */
	if (get_const_class(right, block) == REAL_CONSTANT) {
		ir_node *left  = get_Sub_left(n);
		ir_mode *mode;
		dbg_info *dbi;
		ir_node *irn;

		switch (get_const_class(left, block)) {
		case REAL_CONSTANT:
			irn = optimize_in_place(n);
			if (irn != n) {
				exchange(n, irn);
				*in = irn;
				return 1;
			}
			return 0;
		case NO_CONSTANT:
			break;
		default:
			/* already constant, nothing to do */
			return 0;
		}
		mode = get_irn_mode(n);
		dbi  = get_irn_dbg_info(n);

		/* Beware of SubP(P, Is) */
		irn = new_rd_Minus(dbi, current_ir_graph, block, right, rmode);
		irn = new_rd_Add(dbi, current_ir_graph, block, left, irn, get_irn_mode(n));

		DBG((dbg, LEVEL_5, "Applied: %n - %n => %n + (-%n)\n",
			get_Sub_left(n), right, get_Sub_left(n), right));

		if (n == irn)
			return 0;

		exchange(n, irn);
		*in = irn;

		return 1;
	}
	return 0;
}  /* reassoc_Sub */

/** Retrieve a mode from the operands. We need this, because
 * Add and Sub are allowed to operate on (P, Is)
 */
static ir_mode *get_mode_from_ops(ir_node *op1, ir_node *op2)
{
	ir_mode *m1, *m2;

	m1 = get_irn_mode(op1);
	if (mode_is_reference(m1))
		return m1;

	m2 = get_irn_mode(op2);
	if (mode_is_reference(m2))
		return m2;

	assert(m1 == m2);

	return m1;
}  /* get_mode_from_ops */

/**
 * reassociate a commutative Binop
 *
 * BEWARE: this rule leads to a potential loop, if
 * two operands are region constants and the third is a
 * constant, so avoid this situation.
 */
static int reassoc_commutative(ir_node **node)
{
	ir_node *n     = *node;
	ir_op *op      = get_irn_op(n);
	ir_node *block = get_nodes_block(n);
	ir_node *t1, *c1;

	get_comm_Binop_ops(n, &t1, &c1);

	if (get_irn_op(t1) == op) {
		ir_node *t2, *c2;
		const_class_t c_c1, c_c2, c_t2;

		get_comm_Binop_ops(t1, &t2, &c2);

		/* do not optimize Bad nodes, will fail later */
		if (is_Bad(t2))
			return 0;

		c_c1 = get_const_class(c1, block);
		c_c2 = get_const_class(c2, block);
		c_t2 = get_const_class(t2, block);

		if ( ((c_c1 > NO_CONSTANT) & (c_t2 > NO_CONSTANT)) &&
		     ((((c_c1 ^ c_c2 ^ c_t2) & REGION_CONST) == 0) || ((c_c1 & c_c2 & c_t2) == REGION_CONST)) ) {
			/* All three are constant and either all are constant expressions or two of them are:
			 * then applying this rule would lead into a cycle
			 *
			 * Note that if t2 is a constant so is c2 hence we save one test.
			 */
			return 0;
		}

		if ((c_c1 != NO_CONSTANT) /* & (c_c2 != NO_CONSTANT) */) {
			/* handles rules R7, R8, R9, R10:
			 * convert c1 .OP. (c2 .OP. x) => x .OP. (c1 .OP. c2)
			 */
			ir_node *irn, *in[2];
			ir_mode *mode, *mode_c1 = get_irn_mode(c1), *mode_c2 = get_irn_mode(c2);

			/* It might happen, that c1 and c2 have different modes, for instance Is and Iu.
			 * Handle this here.
			 */
			if (mode_c1 != mode_c2) {
				if (mode_is_int(mode_c1) && mode_is_int(mode_c2)) {
					/* get the bigger one */
					if (get_mode_size_bits(mode_c1) > get_mode_size_bits(mode_c2))
						c2 = new_r_Conv(current_ir_graph, block, c2, mode_c1);
					else if (get_mode_size_bits(mode_c1) < get_mode_size_bits(mode_c2))
						c1 = new_r_Conv(current_ir_graph, block, c1, mode_c2);
					else {
						/* Try to cast the real const */
						if (c_c1 == REAL_CONSTANT)
							c1 = new_r_Conv(current_ir_graph, block, c1, mode_c2);
						else
							c2 = new_r_Conv(current_ir_graph, block, c2, mode_c1);
					}
				}
			}

			in[0] = c1;
			in[1] = c2;

			mode  = get_mode_from_ops(in[0], in[1]);
			in[1] = optimize_node(new_ir_node(NULL, current_ir_graph, block, op, mode, 2, in));
			in[0] = t2;

			mode = get_mode_from_ops(in[0], in[1]);
			irn   = optimize_node(new_ir_node(NULL, current_ir_graph, block, op, mode, 2, in));

			DBG((dbg, LEVEL_5, "Applied: %n .%s. (%n .%s. %n) => %n .%s. (%n .%s. %n)\n",
			     c1, get_irn_opname(n), c2, get_irn_opname(n), t2,
			     t2, get_irn_opname(n), c1, get_irn_opname(n), c2));
			/*
			 * In some rare cases it can really happen that we get the same node back.
			 * This might be happen in dead loops, were the Phi nodes are already gone away.
			 * So check this.
			 */
			if (n != irn) {
				exchange(n, irn);
				*node = irn;
				return 1;
			}
		}
	}
	return 0;
}  /* reassoc_commutative */

#define reassoc_Add  reassoc_commutative
#define reassoc_And  reassoc_commutative
#define reassoc_Or   reassoc_commutative
#define reassoc_Eor  reassoc_commutative

/**
 * Reassociate using commutative law for Mul and distributive law for Mul and Add/Sub:
 */
static int reassoc_Mul(ir_node **node)
{
	ir_node *n = *node;
	ir_node *add_sub, *c;
	ir_op *op;

	if (reassoc_commutative(&n))
		return 1;

	get_comm_Binop_ops(n, &add_sub, &c);
	op = get_irn_op(add_sub);

	/* handles rules R11, R12, R13, R14, R15, R16, R17, R18, R19, R20 */
	if (op == op_Add || op == op_Sub) {
		ir_mode *mode = get_irn_mode(n);
		ir_node *irn, *block, *t1, *t2, *in[2];

		block = get_nodes_block(n);
		t1 = get_binop_left(add_sub);
		t2 = get_binop_right(add_sub);

		/* we can only multiplication rules on integer arithmetic */
		if (mode_is_int(get_irn_mode(t1)) && mode_is_int(get_irn_mode(t2))) {
			in[0] = new_rd_Mul(NULL, current_ir_graph, block, c, t1, mode);
			in[1] = new_rd_Mul(NULL, current_ir_graph, block, c, t2, mode);

			mode  = get_mode_from_ops(in[0], in[1]);
			irn   = optimize_node(new_ir_node(NULL, current_ir_graph, block, op, mode, 2, in));

			/* In some cases it might happen that the new irn is equal the old one, for
			 * instance in:
			 * (x - 1) * y == x * y - y
			 * will be transformed back by simpler optimization
			 * We could switch simple optimizations off, but this only happens iff y
			 * is a loop-invariant expression and that it is not clear if the new form
			 * is better.
			 * So, we let the old one.
			 */
			if (irn != n) {
				DBG((dbg, LEVEL_5, "Applied: (%n .%s. %n) %n %n => (%n %n %n) .%s. (%n %n %n)\n",
					t1, get_op_name(op), t2, n, c, t1, n, c, get_op_name(op), t2, n, c));
				exchange(n, irn);
				*node = irn;

				return 1;
			}
		}
	}
	return 0;
}  /* reassoc_Mul */

/**
 * The walker for the reassociation.
 */
static void wq_walker(ir_node *n, void *env)
{
	walker_t *wenv = env;

	set_irn_link(n, NULL);
	if (is_no_Block(n)) {
		ir_node *blk = get_nodes_block(n);

		if (is_Block_dead(blk) || get_Block_dom_depth(blk) < 0) {
			/* We are in a dead block, do not optimize or we may fall into an endless
			   loop. We check this here instead of requiring that all dead blocks are removed
			   which or cf_opt do not guarantee yet. */
			return;
		}
		waitq_put(wenv->wq, n);
		set_irn_link(n, wenv->wq);
	}
}  /* wq_walker */

/**
 * The walker for the reassociation.
 */
static void do_reassociation(walker_t *wenv)
{
	int i, res, changed;
	ir_node *n, *blk;


	while (! waitq_empty(wenv->wq)) {
		n = waitq_get(wenv->wq);
		set_irn_link(n, NULL);

		blk = get_nodes_block(n);
		if (is_Block_dead(blk) || get_Block_dom_depth(blk) < 0) {
			/* We are in a dead block, do not optimize or we may fall into an endless
			   loop. We check this here instead of requiring that all dead blocks are removed
			   which or cf_opt do not guarantee yet. */
			continue;
		}


		hook_reassociate(1);

		/* reassociation must run until a fixpoint is reached. */
		changed = 0;
		do {
			ir_op   *op    = get_irn_op(n);
			ir_mode *mode  = get_irn_mode(n);

			res = 0;

			/* for FP these optimizations are only allowed if fp_strict_algebraic is disabled */
			if (mode_is_float(mode) && get_irg_fp_model(current_ir_graph) & fp_strict_algebraic)
				break;

			if (op->ops.reassociate) {
				res = op->ops.reassociate(&n);

				changed |= res;
			}
		} while (res == 1);
		hook_reassociate(0);

		wenv->changes |= changed;

		if (changed) {
			for (i = get_irn_arity(n) - 1; i >= 0; --i) {
				ir_node *pred = get_irn_n(n, i);

				if (get_irn_link(pred) != wenv->wq) {
					waitq_put(wenv->wq, pred);
					set_irn_link(pred, wenv->wq);
				}
			}
		}
	}
}  /* do_reassociation */

/**
 * Returns the earliest were a,b are available.
 * Note that we know that we know that a, b both dominate
 * the block of the previous operation, so one must dominate the other.
 */
static ir_node *earliest_block(ir_node *a, ir_node *b) {
	ir_node *blk_a = get_nodes_block(a);
	ir_node *blk_b = get_nodes_block(b);

	/* if blk_a != blk_b, one must dominate the other */
	if (block_dominates(blk_a, blk_b))
		return blk_b;
	else
		return blk_a;
}

/**
 * Apply distributive Law for Mul and Add/Sub
 */
static int reverse_rule_distributive(ir_node **node) {
	ir_node *n = *node;
	ir_node *left  = get_binop_left(n);
	ir_node *right = get_binop_right(n);
	ir_node *x, *blk;
	ir_node *a, *b, *irn;
	ir_mode *mode;
	dbg_info *dbg;

	if (! is_Mul(left) || !is_Mul(right))
		return 0;

	x = get_Mul_left(left);

	if (x == get_Mul_left(right)) {
		/* (x * a) +/- (x * b) */
		a = get_Mul_right(left);
		b = get_Mul_right(right);
		goto transform;
	} else if (x == get_Mul_right(right)) {
		/* (x * a) +/- (b * x) */
		a = get_Mul_right(left);
		b = get_Mul_left(right);
		goto transform;
	}

	x = get_Mul_right(left);

	if (x == get_Mul_right(right)) {
		/* (a * x) +/- (b * x) */
		a = get_Mul_left(left);
		b = get_Mul_left(right);
		goto transform;
	} else if (x == get_Mul_left(right)) {
		/* (a * x) +/- (x * b) */
		a = get_Mul_left(left);
		b = get_Mul_right(right);
		goto transform;
	}
	return 0;

transform:
	blk = earliest_block(a, b);

	dbg  = get_irn_dbg_info(n);
	mode = get_irn_mode(n);

	if (is_Add(n))
		irn = new_rd_Add(dbg, current_ir_graph, blk, a, b, mode);
	else
		irn = new_rd_Sub(dbg, current_ir_graph, blk, a, b, mode);

	blk  = earliest_block(irn, x);
	irn = new_rd_Mul(dbg, current_ir_graph, blk, irn, x, mode);

	exchange(n, irn);
	*node = irn;
	return 1;
}  /* reverse_rule_distributive */


/**
 * Apply the rules in reverse order, removing code that was not collapsed
 */
static void reverse_rules(ir_node *node, void *env) {
	walker_t *wenv = env;
	ir_mode *mode = get_irn_mode(node);
	int res;

	/* for FP these optimizations are only allowed if fp_strict_algebraic is disabled */
	if (mode_is_float(mode) && get_irg_fp_model(current_ir_graph) & fp_strict_algebraic)
		return;

	do {
		res = 0;
		if (is_Add(node) || is_Sub(node)) {
			wenv->changes |= res = reverse_rule_distributive(&node);
		}
	} while (res);
}

/*
 * do the reassociation
 */
void optimize_reassociation(ir_graph *irg)
{
	walker_t env;
	irg_loopinfo_state state;

	assert(get_irg_phase_state(irg) != phase_building);
	assert(get_irg_pinned(irg) != op_pin_state_floats &&
		"Reassociation needs pinned graph to work properly");

	/* reassociation needs constant folding */
	if (!get_opt_reassociation() || !get_opt_constant_folding())
		return;

	/* we use dominance to detect dead blocks */
	assure_doms(irg);

	/*
	 * Calculate loop info, so we could identify loop-invariant
	 * code and threat it like a constant.
	 * We only need control flow loops here but can handle generic
	 * INTRA info as well.
	 */
	state = get_irg_loopinfo_state(irg);
	if ((state & loopinfo_inter) ||
		(state & (loopinfo_constructed | loopinfo_valid)) != (loopinfo_constructed | loopinfo_valid))
		construct_cf_backedges(irg);

	env.changes = 0;
	env.wq      = new_waitq();

	/* now we have collected enough information, optimize */
	irg_walk_graph(irg, NULL, wq_walker, &env);
	do_reassociation(&env);

	/* reverse those rules that do not result in collapsed constants */
	irg_walk_graph(irg, NULL, reverse_rules, &env);

	/* Handle graph state */
	if (env.changes) {
		set_irg_outs_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	}

	del_waitq(env.wq);
}  /* optimize_reassociation */

/* Sets the default reassociation operation for an ir_op_ops. */
ir_op_ops *firm_set_default_reassoc(ir_opcode code, ir_op_ops *ops)
{
#define CASE(a) case iro_##a: ops->reassociate  = reassoc_##a; break

	switch (code) {
	CASE(Mul);
	CASE(Add);
	CASE(Sub);
	CASE(And);
	CASE(Or);
	CASE(Eor);
	default:
		/* leave NULL */;
	}

	return ops;
#undef CASE
}  /* firm_set_default_reassoc */

/* initialize the reassociation by adding operations to some opcodes */
void firm_init_reassociation(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.reassoc");
}  /* firm_init_reassociation */
