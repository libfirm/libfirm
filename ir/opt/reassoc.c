/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Reassociation
 * @author  Michael Beck
 */
#include "iroptimize.h"
#include "iropt_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "iropt_dbg.h"
#include "irflag_t.h"
#include "irgwalk.h"
#include "irouts.h"
#include "reassoc_t.h"
#include "opt_init.h"
#include "irhooks.h"
#include "irloop.h"
#include "pdeq.h"
#include "debug.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

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
static const_class_t get_const_class(const ir_node *n, const ir_node *block)
{
	if (is_Const(n))
		return REAL_CONSTANT;

	/* constant nodes which can't be folded are region constants */
	if (is_irn_constlike(n))
		return REGION_CONST;

	/*
	 * Beware: Bad nodes are always loop-invariant, but
	 * cannot handled in later code, so filter them here.
	 */
	if (! is_Bad(n) && is_loop_invariant(n, block))
		return REGION_CONST;

	return NO_CONSTANT;
}

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
}

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
		irn = new_rd_Minus(dbi, block, right, rmode);
		irn = new_rd_Add(dbi, block, left, irn, mode);

		DBG((dbg, LEVEL_5, "Applied: %n - %n => %n + (-%n)\n",
			get_Sub_left(n), right, get_Sub_left(n), right));

		if (n == irn)
			return 0;

		exchange(n, irn);
		*in = irn;

		return 1;
	}
	return 0;
}

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
}

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
	ir_op   *op    = get_irn_op(n);
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
			/* All three are constant and either all are constant expressions
			 * or two of them are:
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
			ir_graph *irg = get_irn_irg(c1);

			/* It might happen, that c1 and c2 have different modes, for
			 * instance Is and Iu.
			 * Handle this here.
			 */
			if (mode_c1 != mode_c2) {
				if (mode_is_int(mode_c1) && mode_is_int(mode_c2)) {
					/* get the bigger one */
					if (get_mode_size_bits(mode_c1) > get_mode_size_bits(mode_c2))
						c2 = new_r_Conv(block, c2, mode_c1);
					else if (get_mode_size_bits(mode_c1) < get_mode_size_bits(mode_c2))
						c1 = new_r_Conv(block, c1, mode_c2);
					else {
						/* Try to cast the real const */
						if (c_c1 == REAL_CONSTANT)
							c1 = new_r_Conv(block, c1, mode_c2);
						else
							c2 = new_r_Conv(block, c2, mode_c1);
					}
				}
			}

			in[0] = c1;
			in[1] = c2;

			mode  = get_mode_from_ops(in[0], in[1]);
			in[1] = optimize_node(new_ir_node(NULL, irg, block, op, mode, 2, in));
			in[0] = t2;

			mode = get_mode_from_ops(in[0], in[1]);
			irn   = optimize_node(new_ir_node(NULL, irg, block, op, mode, 2, in));

			DBG((dbg, LEVEL_5, "Applied: %n .%s. (%n .%s. %n) => %n .%s. (%n .%s. %n)\n",
			     c1, get_irn_opname(n), c2, get_irn_opname(n), t2,
			     t2, get_irn_opname(n), c1, get_irn_opname(n), c2));
			/*
			 * In some rare cases it can really happen that we get the same
			 * node back. This might be happen in dead loops, were the Phi
			 * nodes are already gone away. So check this.
			 */
			if (n != irn) {
				exchange(n, irn);
				*node = irn;
				return 1;
			}
		}
	}
	if (get_irn_op(c1) == op) {
		ir_node *t = c1;
		c1 = t1;
		t1 = t;
	}
	if (get_irn_op(t1) == op) {
		ir_node *l = get_binop_left(t1);
		ir_node *r = get_binop_right(t1);
		const_class_t c_r;

		if (r == c1) {
			ir_node *t = r;
			r = l;
			l = t;
		}
		c_r = get_const_class(r, block);
		if (c_r != NO_CONSTANT) {
			/*
			 * Beware: don't do the following op if a constant was
			 * placed below, else we will fall into a loop.
			 */
			return 0;
		}

		if (l == c1) {
			/* convert x .OP. (x .OP. y) => y .OP. (x .OP. x) */
			ir_mode *mode_res = get_irn_mode(n);
			ir_mode *mode_c1  = get_irn_mode(c1);
			ir_graph *irg     = get_irn_irg(c1);
			ir_node *irn, *in[2];

			in[0] = c1;
			in[1] = c1;

			in[1] = optimize_node(new_ir_node(NULL, irg, block, op, mode_c1, 2, in));
			in[0] = r;

			irn   = optimize_node(new_ir_node(NULL, irg, block, op, mode_res, 2, in));

			DBG((dbg, LEVEL_5, "Applied: %n .%s. (%n .%s. %n) => %n .%s. (%n .%s. %n)\n",
				c1, get_irn_opname(n), l, get_irn_opname(n), r,
				r, get_irn_opname(n), c1, get_irn_opname(n), c1));

			if (n != irn) {
				exchange(n, irn);
				*node = irn;
				return 1;
			}
		}
	}
	return 0;
}

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
			ir_graph *irg = get_irn_irg(t1);
			in[0] = new_r_Mul(block, c, t1, mode);
			in[1] = new_r_Mul(block, c, t2, mode);

			irn   = optimize_node(new_ir_node(NULL, irg, block, op, mode, 2, in));

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
}

/**
 * The walker for the reassociation.
 */
static void wq_walker(ir_node *n, void *env)
{
	waitq *const wq = (waitq*)env;

	set_irn_link(n, NULL);
	if (!is_Block(n)) {
		waitq_put(wq, n);
		set_irn_link(n, wq);
	}
}

/**
 * The walker for the reassociation.
 */
static void do_reassociation(waitq *const wq)
{
	int i, res, changed;
	ir_node *n;

	while (! waitq_empty(wq)) {
		n = (ir_node*)waitq_get(wq);
		set_irn_link(n, NULL);

		hook_reassociate(1);

		/* reassociation must run until a fixpoint is reached. */
		changed = 0;
		do {
			ir_op    *op   = get_irn_op(n);
			ir_mode  *mode = get_irn_mode(n);

			res = 0;

			/* reassociating floatingpoint ops is imprecise */
			if (mode_is_float(mode) && !ir_imprecise_float_transforms_allowed())
				break;

			if (op->ops.reassociate) {
				res = op->ops.reassociate(&n);

				changed |= res;
			}
		} while (res == 1);
		hook_reassociate(0);

		if (changed) {
			for (i = get_irn_arity(n) - 1; i >= 0; --i) {
				ir_node *pred = get_irn_n(n, i);

				if (get_irn_link(pred) != wq) {
					waitq_put(wq, pred);
					set_irn_link(pred, wq);
				}
			}
		}
	}
}

/**
 * Returns the earliest were a,b are available.
 * Note that we know that a, b both dominate
 * the block of the previous operation, so one must dominate the other.
 *
 * If the earliest block is the start block, return curr_blk instead
 */
static ir_node *earliest_block(ir_node *a, ir_node *b, ir_node *curr_blk)
{
	ir_node *blk_a = get_nodes_block(a);
	ir_node *blk_b = get_nodes_block(b);
	ir_node *res;

	/* if blk_a != blk_b, one must dominate the other */
	if (block_dominates(blk_a, blk_b))
		res = blk_b;
	else
		res = blk_a;
	if (res == get_irg_start_block(get_irn_irg(curr_blk)))
		return curr_blk;
	return res;
}

static bool is_const(ir_node *const node)
{
	switch (get_irn_opcode(node)) {
	case iro_Address:
	case iro_Align:
	case iro_Const:
	case iro_Offset:
	case iro_Size:
		return true;
	default:
		return false;
	}
}

/**
 * Checks whether a node is a Constant expression.
 * The following trees are constant expressions:
 *
 * Address, Const, Offset, TypeConst, Const + Address/Offset/TypeConst
 *
 * Handling Address/Offset/TypeConsts as const might be not a good idea for all
 * architectures ...
 */
static int is_constant_expr(ir_node *irn)
{
	switch (get_irn_opcode(irn)) {
	case iro_Address:
	case iro_Align:
	case iro_Const:
	case iro_Offset:
	case iro_Size:
		return 1;

	case iro_Add:
		return is_const(get_Add_left(irn)) && is_const(get_Add_right(irn));

	default:
		return 0;
	}
}

/**
 * Apply distributive Law for Mul and Add/Sub
 */
static int reverse_rule_distributive(ir_node **node)
{
	ir_node *n = *node;
	ir_node *left  = get_binop_left(n);
	ir_node *right = get_binop_right(n);
	ir_node *x, *blk, *curr_blk;
	ir_node *a, *b, *irn;
	ir_op *op;
	ir_mode *mode;
	dbg_info *dbg;

	op = get_irn_op(left);
	if (op != get_irn_op(right))
		return 0;

	if (op == op_Shl) {
		x = get_Shl_right(left);

		if (x == get_Shl_right(right)) {
			/* (a << x) +/- (b << x) ==> (a +/- b) << x */
			a = get_Shl_left(left);
			b = get_Shl_left(right);
			goto transform;
		}
	} else if (op == op_Mul) {
		x = get_Mul_left(left);

		if (x == get_Mul_left(right)) {
			/* (x * a) +/- (x * b) ==> (a +/- b) * x */
			a = get_Mul_right(left);
			b = get_Mul_right(right);
			goto transform;
		} else if (x == get_Mul_right(right)) {
			/* (x * a) +/- (b * x) ==> (a +/- b) * x */
			a = get_Mul_right(left);
			b = get_Mul_left(right);
			goto transform;
		}

		x = get_Mul_right(left);

		if (x == get_Mul_right(right)) {
			/* (a * x) +/- (b * x) ==> (a +/- b) * x */
			a = get_Mul_left(left);
			b = get_Mul_left(right);
			goto transform;
		} else if (x == get_Mul_left(right)) {
			/* (a * x) +/- (x * b) ==> (a +/- b) * x */
			a = get_Mul_left(left);
			b = get_Mul_right(right);
			goto transform;
		}
	}
	return 0;

transform:
	curr_blk = get_nodes_block(n);

	blk = earliest_block(a, b, curr_blk);

	dbg  = get_irn_dbg_info(n);
	mode = get_irn_mode(n);

	if (is_Add(n))
		irn = new_rd_Add(dbg, blk, a, b, mode);
	else
		irn = new_rd_Sub(dbg, blk, a, b, mode);

	blk  = earliest_block(irn, x, curr_blk);

	if (op == op_Mul)
		irn = new_rd_Mul(dbg, blk, irn, x, mode);
	else
		irn = new_rd_Shl(dbg, blk, irn, x, mode);

	exchange(n, irn);
	*node = irn;
	return 1;
}

/**
 * Move Constants towards the root.
 */
static int move_consts_up(ir_node **node)
{
	ir_node *n = *node;
	ir_op *op;
	ir_node *l, *r, *a, *b, *c, *blk, *irn, *in[2];
	ir_mode *mode, *ma, *mb;
	dbg_info *dbg;
	ir_graph *irg;

	l = get_binop_left(n);
	r = get_binop_right(n);

	/* check if one is already a constant expression */
	if (is_constant_expr(l) || is_constant_expr(r))
		return 0;

	dbg = get_irn_dbg_info(n);
	op = get_irn_op(n);
	if (get_irn_op(l) == op) {
		/* (a .op. b) .op. r */
		a = get_binop_left(l);
		b = get_binop_right(l);

		if (is_constant_expr(a)) {
			/* (C .op. b) .op. r ==> (r .op. b) .op. C */
			c = a;
			a = r;
			blk = get_nodes_block(l);
			dbg = dbg == get_irn_dbg_info(l) ? dbg : NULL;
			goto transform;
		} else if (is_constant_expr(b)) {
			/* (a .op. C) .op. r ==> (a .op. r) .op. C */
			c = b;
			b = r;
			blk = get_nodes_block(l);
			dbg = dbg == get_irn_dbg_info(l) ? dbg : NULL;
			goto transform;
		}
	}
	if (get_irn_op(r) == op) {
		/* l .op. (a .op. b) */
		a = get_binop_left(r);
		b = get_binop_right(r);

		if (is_constant_expr(a)) {
			/* l .op. (C .op. b) ==> (l .op. b) .op. C */
			c = a;
			a = l;
			blk = get_nodes_block(r);
			dbg = dbg == get_irn_dbg_info(r) ? dbg : NULL;
			goto transform;
		} else if (is_constant_expr(b)) {
			/* l .op. (a .op. C) ==> (a .op. l) .op. C */
			c = b;
			b = l;
			blk = get_nodes_block(r);
			dbg = dbg == get_irn_dbg_info(r) ? dbg : NULL;
			goto transform;
		}
	}
	return 0;

transform:
	/* In some cases a and b might be both of different integer mode, and c a
	 * Address/Offset/TypeConst.
	 * in that case we could either
	 * 1.) cast into unsigned mode
	 * 2.) ignore
	 * we implement the second here
	 */
	ma = get_irn_mode(a);
	mb = get_irn_mode(b);
	if (ma != mb && mode_is_int(ma) && mode_is_int(mb))
		return 0;

	/* check if (a .op. b) can be calculated in the same block is the old instruction */
	if (! block_dominates(get_nodes_block(a), blk))
		return 0;
	if (! block_dominates(get_nodes_block(b), blk))
		return 0;
	/* ok */
	in[0] = a;
	in[1] = b;

	mode = get_mode_from_ops(a, b);
	irg  = get_irn_irg(blk);
	in[0] = irn = optimize_node(new_ir_node(dbg, irg, blk, op, mode, 2, in));

	/* beware: optimize_node might have changed the opcode, check again */
	if (is_Add(irn) || is_Sub(irn)) {
		reverse_rule_distributive(&in[0]);
	}
	in[1] = c;

	mode = get_mode_from_ops(in[0], in[1]);
	irn = optimize_node(new_ir_node(dbg, irg, blk, op, mode, 2, in));

	exchange(n, irn);
	*node = irn;
	return 1;
}

/**
 * Apply the rules in reverse order, removing code that was not collapsed
 */
static void reverse_rules(ir_node *node, void *env)
{
	(void)env;

	ir_mode  *mode = get_irn_mode(node);
	int res;

	/* reassociating floatingpoint ops is imprecise */
	if (mode_is_float(mode) && !ir_imprecise_float_transforms_allowed())
		return;

	do {
		ir_op *op = get_irn_op(node);

		res = 0;
		if (is_op_commutative(op)) {
			res = move_consts_up(&node);
		}
		/* beware: move_consts_up might have changed the opcode, check again */
		if (is_Add(node) || is_Sub(node)) {
			res = reverse_rule_distributive(&node);
		}
	} while (res);
}

/*
 * do the reassociation
 */
void optimize_reassociation(ir_graph *irg)
{
	assert(get_irg_pinned(irg) != op_pin_state_floats &&
		"Reassociation needs pinned graph to work properly");

	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		| IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);

	waitq *const wq = new_waitq();

	/* disable some optimizations while reassoc is running to prevent endless loops */
	set_reassoc_running(1);
	{
		/* now we have collected enough information, optimize */
		irg_walk_graph(irg, NULL, wq_walker, wq);
		do_reassociation(wq);

		/* reverse those rules that do not result in collapsed constants */
		irg_walk_graph(irg, NULL, reverse_rules, NULL);
	}
	set_reassoc_running(0);

	del_waitq(wq);

	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_CONTROL_FLOW);
}

static void register_node_reassoc_func(ir_op *op, reassociate_func func)
{
	op->ops.reassociate = func;
}

void ir_register_reassoc_node_ops(void)
{
	register_node_reassoc_func(op_Add, reassoc_commutative);
	register_node_reassoc_func(op_And, reassoc_commutative);
	register_node_reassoc_func(op_Eor, reassoc_commutative);
	register_node_reassoc_func(op_Mul, reassoc_Mul);
	register_node_reassoc_func(op_Or,  reassoc_commutative);
	register_node_reassoc_func(op_Sub, reassoc_Sub);
}

/* initialize the reassociation by adding operations to some opcodes */
void firm_init_reassociation(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.reassoc");
}
