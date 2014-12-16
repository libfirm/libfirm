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
	NO_CONSTANT,   /**< node is not constant */
	REAL_CONSTANT, /**< a Const that is suitable for constant folding */
	REGION_CONST   /**< a constant expression in the current context */
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
	if (!is_Bad(n) && is_loop_invariant(n, block))
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
	assert(is_op_commutative(get_irn_op(binop)));
	ir_node       *op_a    = get_binop_left(binop);
	ir_node       *op_b    = get_binop_right(binop);
	ir_node       *block   = get_nodes_block(binop);
	const_class_t  class_a = get_const_class(op_a, block);
	const_class_t  class_b = get_const_class(op_b, block);

	if (class_a == REAL_CONSTANT && class_b == REAL_CONSTANT) {
		/* if both are constants, one might be a
		 * pointer constant like NULL, return the other */
		if (mode_is_reference(get_irn_mode(op_a))) {
			*a = op_a;
			*c = op_b;
		} else {
			*a = op_b;
			*c = op_a;
		}
	} else if (class_a == REAL_CONSTANT
	           || (class_a == REGION_CONST && class_b == NO_CONSTANT)) {
		*a = op_b;
		*c = op_a;
	} else {
		*a = op_a;
		*c = op_b;
	}
}

/**
 * Retrieve a mode from the operands. We need this, because
 * Add and Sub are allowed to operate on (P, Is)
 */
static ir_mode *get_mode_from_ops(ir_node *op1, ir_node *op2)
{
	ir_mode *m1 = get_irn_mode(op1);
	if (mode_is_reference(m1))
		return m1;

	ir_mode *m2 = get_irn_mode(op2);
	if (mode_is_reference(m2))
		return m2;

	assert(m1 == m2);
	return m1;
}

static ir_node *create_node(dbg_info *dbgi, ir_node *block, ir_op *op,
                            ir_mode *mode, int n_in, ir_node **in)
{
	ir_graph *irg = get_irn_irg(block);
	ir_node  *new = new_ir_node(dbgi, irg, block, op, mode, n_in, in);
	verify_new_node(irg, new);
	return optimize_node(new);
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
	ir_node *t1;
	ir_node *c1;

	get_comm_Binop_ops(n, &t1, &c1);

	if (get_irn_op(t1) == op) {
		ir_node *t2;
		ir_node *c2;
		get_comm_Binop_ops(t1, &t2, &c2);
		const_class_t c_c1 = get_const_class(c1, block);
		const_class_t c_c2 = get_const_class(c2, block);
		const_class_t c_t2 = get_const_class(t2, block);
		if ( (c_c1 != NO_CONSTANT && c_t2 >= NO_CONSTANT) &&
		     ((((c_c1 ^ c_c2 ^ c_t2) & REGION_CONST) == 0)
		         || ((c_c1 & c_c2 & c_t2) == REGION_CONST)) ) {
			/* All three are constant and either all are constant expressions
			 * or two of them are:
			 * then applying this rule would lead into a cycle
			 *
			 * Note that if t2 is a constant so is c2 hence we save one test.
			 */
			return 0;
		}

		if (c_c1 != NO_CONSTANT /* && c_c2 != NO_CONSTANT */) {
			/* handles rules R7, R8, R9, R10:
			 * convert c1 .OP. (c2 .OP. x) => x .OP. (c1 .OP. c2)
			 */
			ir_mode *mode_c1 = get_irn_mode(c1);
			ir_mode *mode_c2 = get_irn_mode(c2);

			/* It might happen, that c1 and c2 have different modes, for
			 * instance Is and Iu.
			 * Handle this here.
			 */
			if (mode_c1 != mode_c2) {
				if (mode_is_int(mode_c1) && mode_is_int(mode_c2)) {
					/* get the bigger one */
					if (get_mode_size_bits(mode_c1) > get_mode_size_bits(mode_c2)) {
						c2 = new_r_Conv(block, c2, mode_c1);
					} else if (get_mode_size_bits(mode_c1) < get_mode_size_bits(mode_c2)) {
						c1 = new_r_Conv(block, c1, mode_c2);
					} else {
						/* Try to cast the real const */
						if (c_c1 == REAL_CONSTANT)
							c1 = new_r_Conv(block, c1, mode_c2);
						else
							c2 = new_r_Conv(block, c2, mode_c1);
					}
				}
			}

			ir_node *in0[] = { c1, c2};
			ir_mode *mode0 = get_mode_from_ops(c1, c2);
			ir_node *irn0  = create_node(NULL, block, op, mode0,
			                             ARRAY_SIZE(in0), in0);

			ir_node *in1[] = { t2, irn0 };
			ir_mode *mode1 = get_mode_from_ops(t2, irn0);
			ir_node *irn1  = create_node(NULL, block, op, mode1,
			                             ARRAY_SIZE(in1), in1);

			DBG((dbg, LEVEL_5, "Applied: %n .%s. (%n .%s. %n) => %n .%s. (%n .%s. %n)\n",
			     c1, get_irn_opname(n), c2, get_irn_opname(n), t2,
			     t2, get_irn_opname(n), c1, get_irn_opname(n), c2));
			/*
			 * In some rare cases it can really happen that we get the same
			 * node back. This might be happen in dead loops, were the Phi
			 * nodes are already gone away. So check this.
			 */
			if (n != irn1) {
				exchange(n, irn1);
				*node = irn1;
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
		if (r == c1) {
			ir_node *t = r;
			r = l;
			l = t;
		}

		const_class_t c_r = get_const_class(r, block);
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
			ir_node *in[]     = { c1, c1 };
			ir_node *irn0     = create_node(NULL, block, op, mode_c1,
			                                ARRAY_SIZE(in), in);

			ir_node *in1[] = { r, irn0 };
			ir_node *irn1  = create_node(NULL, block, op, mode_res,
			                             ARRAY_SIZE(in1), in1);

			DBG((dbg, LEVEL_5, "Applied: %n .%s. (%n .%s. %n) => %n .%s. (%n .%s. %n)\n",
				c1, get_irn_opname(n), l, get_irn_opname(n), r,
				r, get_irn_opname(n), c1, get_irn_opname(n), c1));

			if (n != irn1) {
				exchange(n, irn1);
				*node = irn1;
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
	while (!waitq_empty(wq)) {
		ir_node *n = (ir_node*)waitq_get(wq);
		set_irn_link(n, NULL);

		hook_reassociate(1);

		/* reassociation must run until a fixpoint is reached. */
		bool changed = false;
		bool res;
		do {
			res = false;
			ir_op   *op   = get_irn_op(n);
			ir_mode *mode = get_irn_mode(n);

			/* reassociating floatingpoint ops is imprecise */
			if (mode_is_float(mode) && !ir_imprecise_float_transforms_allowed())
				break;

			if (op->ops.reassociate) {
				res = op->ops.reassociate(&n);

				changed |= res;
			}
		} while (res);
		hook_reassociate(0);

		if (changed) {
			foreach_irn_in_r(n, i, pred) {
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
	/* if blk_a != blk_b, one must dominate the other */
	ir_node *blk_a = get_nodes_block(a);
	ir_node *blk_b = get_nodes_block(b);
	ir_node *res   = block_dominates(blk_a, blk_b) ? blk_b : blk_a;
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
	ir_node *n     = *node;
	ir_node *left  = get_binop_left(n);
	ir_node *right = get_binop_right(n);
	ir_op   *op    = get_irn_op(left);
	if (op != get_irn_op(right))
		return 0;

	ir_node *x;
	ir_node *a;
	ir_node *b;
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

transform:;
	ir_node  *curr_blk = get_nodes_block(n);
	ir_node  *blk      = earliest_block(a, b, curr_blk);
	dbg_info *dbg      = get_irn_dbg_info(n);
	ir_mode  *mode     = get_irn_mode(n);

	ir_node *irn;
	if (is_Add(n))
		irn = new_rd_Add(dbg, blk, a, b, mode);
	else
		irn = new_rd_Sub(dbg, blk, a, b, mode);

	blk = earliest_block(irn, x, curr_blk);
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
	ir_node *l = get_binop_left(n);
	ir_node *r = get_binop_right(n);

	/* check if one is already a constant expression */
	if (is_constant_expr(l) || is_constant_expr(r))
		return 0;

	dbg_info *dbg = get_irn_dbg_info(n);
	ir_op    *op  = get_irn_op(n);
	ir_node  *blk;
	ir_node  *a;
	ir_node  *b;
	ir_node  *c;
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

transform:;
	/* In some cases a and b might be both of different integer mode, and c a
	 * Address/Offset/TypeConst.
	 * in that case we could either
	 * 1.) cast into unsigned mode
	 * 2.) ignore
	 * we implement the second here
	 */
	ir_mode *ma = get_irn_mode(a);
	ir_mode *mb = get_irn_mode(b);
	if (ma != mb && mode_is_int(ma) && mode_is_int(mb))
		return 0;

	/* check if (a .op. b) can be calculated in the same block is the old
	 * instruction */
	if (! block_dominates(get_nodes_block(a), blk))
		return 0;
	if (! block_dominates(get_nodes_block(b), blk))
		return 0;
	/* ok */
	ir_mode *mode  = get_mode_from_ops(a, b);
	ir_node *in0[] = { a, b };
	ir_node *irn   = create_node(dbg, blk, op, mode, ARRAY_SIZE(in0), in0);

	/* beware: optimize_node might have changed the opcode, check again */
	if (is_Add(irn) || is_Sub(irn)) {
		reverse_rule_distributive(&irn);
	}

	ir_node *in1[] = { irn, c };
	ir_mode *mode1 = get_mode_from_ops(irn, c);
	ir_node *irn1  = create_node(dbg, blk, op, mode1, ARRAY_SIZE(in1), in1);

	exchange(n, irn1);
	*node = irn1;
	return 1;
}

/**
 * Apply the rules in reverse order, removing code that was not collapsed
 */
static void reverse_rules(ir_node *node, void *env)
{
	(void)env;
	/* reassociating floatingpoint ops is imprecise */
	ir_mode *mode = get_irn_mode(node);
	if (mode_is_float(mode) && !ir_imprecise_float_transforms_allowed())
		return;

	bool res;
	do {
		res = false;

		ir_op *op = get_irn_op(node);
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

	/* now we have collected enough information, optimize */
	irg_walk_graph(irg, NULL, wq_walker, wq);
	do_reassociation(wq);

	/* reverse those rules that do not result in collapsed constants */
	irg_walk_graph(irg, NULL, reverse_rules, NULL);

	del_waitq(wq);

	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_CONTROL_FLOW);
}

void ir_register_reassoc_node_ops(void)
{
	set_op_reassociate(op_Add, reassoc_commutative);
	set_op_reassociate(op_And, reassoc_commutative);
	set_op_reassociate(op_Eor, reassoc_commutative);
	set_op_reassociate(op_Mul, reassoc_commutative);
	set_op_reassociate(op_Or,  reassoc_commutative);
}

/* initialize the reassociation by adding operations to some opcodes */
void firm_init_reassociation(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.reassoc");
}
