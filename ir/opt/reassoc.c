/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Reassociation
 * @author  Michael Beck
 */
#include "reassoc_t.h"

#include "debug.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irloop.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iropt_dbg.h"
#include "iropt_t.h"
#include "iroptimize.h"
#include "irouts.h"
#include "opt_init.h"
#include "panic.h"
#include "pdeq.h"
#include "unionfind.h"

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
	verify_new_node(new);
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
		if (c_c1 != NO_CONSTANT) {
			if (((c_c1 ^ c_c2 ^ c_t2) & REGION_CONST) == 0
			    || (c_c1 & c_c2 & c_t2) == REGION_CONST) {
				/* All three are constant and either all are constant expressions
				 * or two of them are:
				 * then applying this rule would lead into a cycle
				 *
				 * Note that if t2 is a constant so is c2 hence we save one test.
				 */
				return 0;
			}

			/* handles rules R7, R8, R9, R10:
			 * convert c1 .OP. (c2 .OP. x) => x .OP. (c1 .OP. c2)
			 */
			ir_mode *mode_c1 = get_irn_mode(c1);
			ir_mode *mode_c2 = get_irn_mode(c2);

			/* It might happen, that c1 and c2 have different modes, for
			 * instance Is and Iu.
			 * Handle this here.
			 */
			if (mode_c1 != mode_c2 && mode_is_int(mode_c1) && mode_is_int(mode_c2)) {
				unsigned bits_c1 = get_mode_size_bits(mode_c1);
				unsigned bits_c2 = get_mode_size_bits(mode_c2);
				/* get the bigger one */
				if (bits_c1 > bits_c2) {
					c2 = new_r_Conv(block, c2, mode_c1);
				} else if (bits_c1 < bits_c2) {
					c1 = new_r_Conv(block, c1, mode_c2);
				} else {
					/* Try to cast the real const */
					if (c_c1 == REAL_CONSTANT)
						c1 = new_r_Conv(block, c1, mode_c2);
					else
						c2 = new_r_Conv(block, c2, mode_c1);
				}
			}

			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *in0[] = { c1, c2};
			ir_mode  *mode0 = get_mode_from_ops(c1, c2);
			ir_node  *irn0  = create_node(dbgi, block, op, mode0, ARRAY_SIZE(in0), in0);

			ir_node *in1[] = { t2, irn0 };
			ir_mode *mode1 = get_mode_from_ops(t2, irn0);
			ir_node *irn1  = create_node(dbgi, block, op, mode1, ARRAY_SIZE(in1), in1);

			DBG((dbg, LEVEL_5, "Applied: %n .%s. (%n .%s. %n) => %n .%s. (%n .%s. %n)\n",
			     c1, get_irn_opname(n), c2, get_irn_opname(n), t2,
			     t2, get_irn_opname(n), c1, get_irn_opname(n), c2));

			exchange(n, irn1);
			*node = irn1;
			return 1;
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

		if (l == c1 && r != c1) {
			/* convert x .OP. (x .OP. y) => y .OP. (x .OP. x) */
			dbg_info *dbgi     = get_irn_dbg_info(n);
			ir_mode  *mode_res = get_irn_mode(n);
			ir_mode  *mode_c1  = get_irn_mode(c1);
			ir_node  *in[]     = { c1, c1 };
			ir_node  *irn0     = create_node(dbgi, block, op, mode_c1,
			                                 ARRAY_SIZE(in), in);

			ir_node *in1[] = { r, irn0 };
			ir_node *irn1  = create_node(dbgi, block, op, mode_res,
			                             ARRAY_SIZE(in1), in1);

			DBG((dbg, LEVEL_5, "Applied: %n .%s. (%n .%s. %n) => %n .%s. (%n .%s. %n)\n",
			     c1, get_irn_opname(n), l, get_irn_opname(n), r,
			     r, get_irn_opname(n), c1, get_irn_opname(n), c1));

			exchange(n, irn1);
			*node = irn1;
			return 1;
		}
	}
	return 0;
}

/**
 * The walker for the reassociation.
 */
static void wq_walker(ir_node *n, void *env)
{
	deq_t *const wq = (deq_t*)env;

	set_irn_link(n, NULL);
	if (!is_Block(n)) {
		deq_push_pointer_right(wq, n);
		set_irn_link(n, wq);
	}
}

/**
 * The walker for the reassociation.
 */
static void do_reassociation(deq_t *const wq)
{
	while (!deq_empty(wq)) {
		ir_node *n = deq_pop_pointer_left(ir_node, wq);
		set_irn_link(n, NULL);

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

		if (changed) {
			foreach_irn_in_r(n, i, pred) {
				if (get_irn_link(pred) != wq) {
					deq_push_pointer_right(wq, pred);
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

	ir_node *irn;
	if (is_Add(n))
		irn = new_rd_Add(dbg, blk, a, b);
	else
		irn = new_rd_Sub(dbg, blk, a, b);

	blk = earliest_block(irn, x, curr_blk);
	if (op == op_Mul)
		irn = new_rd_Mul(dbg, blk, irn, x);
	else
		irn = new_rd_Shl(dbg, blk, irn, x);

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

	ir_op   *op  = get_irn_op(n);
	ir_node *blk;
	ir_node *a;
	ir_node *b;
	ir_node *c;
	if (get_irn_op(l) == op) {
		/* (a .op. b) .op. r */
		a = get_binop_left(l);
		b = get_binop_right(l);

		if (is_constant_expr(a)) {
			/* (C .op. b) .op. r ==> (r .op. b) .op. C */
			c   = a;
			a   = r;
			blk = get_nodes_block(l);
			goto transform;
		} else if (is_constant_expr(b)) {
			/* (a .op. C) .op. r ==> (a .op. r) .op. C */
			c   = b;
			b   = r;
			blk = get_nodes_block(l);
			goto transform;
		}
	}
	if (get_irn_op(r) == op) {
		/* l .op. (a .op. b) */
		a = get_binop_left(r);
		b = get_binop_right(r);

		if (is_constant_expr(a)) {
			/* l .op. (C .op. b) ==> (l .op. b) .op. C */
			c   = a;
			a   = l;
			blk = get_nodes_block(r);
			goto transform;
		} else if (is_constant_expr(b)) {
			/* l .op. (a .op. C) ==> (a .op. l) .op. C */
			c   = b;
			b   = l;
			blk = get_nodes_block(r);
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
	dbg_info *dbgi  = get_irn_dbg_info(n);
	ir_mode  *mode  = get_mode_from_ops(a, b);
	ir_node  *in0[] = { a, b };
	ir_node  *irn   = create_node(dbgi, blk, op, mode, ARRAY_SIZE(in0), in0);

	/* beware: optimize_node might have changed the opcode, check again */
	if (is_Add(irn) || is_Sub(irn)) {
		reverse_rule_distributive(&irn);
	}

	ir_node *in1[] = { irn, c };
	ir_mode *mode1 = get_mode_from_ops(irn, c);
	ir_node *irn1  = create_node(dbgi, blk, op, mode1, ARRAY_SIZE(in1), in1);

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

/**
 * Returns true iff node is a bitwise function.
 */
static bool is_bitop(ir_node *node)
{
	return is_And(node) || is_Eor(node) || is_Or(node) || is_Not(node);
}

typedef struct {
	ir_graph *irg;
	deq_t     optimizations;

	pmap        *walk_counter;
	unsigned int walk_base;
	unsigned int walk_max;
} shannon_data;


typedef struct {
	ir_node *base_node;
	ir_node *middle_node;
	ir_node *top_node;
	ir_node *other_node;
} optimization_t;

/**
 * Returns true if we can be sure that @p node only has a single read user.
 */
static bool only_one_user(const ir_node *node)
{
	return get_irn_n_edges(node) <= 1;
}

/**
 * Try to find middle_node or top_node, from base_node over a non-direct path.
 *
 *              top_node
 *              ^      ^
 *              |      |
 *          +---+      +------+
 *          |                 |
 *     other_node       middle_node (optional)
 *          ^                 ^
 *          |                 |
 *          |                 |
 *          .                 |
 *          .                 |
 *          .                 |
 *          |                 |
 *          |                 |
 *          +-------+   +-----+
 *                  |   |
 *               base_node
 *
 * @param current      Current node of the search
 * @param other_node   Previous visited node of the search
 * @param base_node    Common root node
 * @param middle_node  Not node,  Eor node with constant operand, or NULL
 * @param top_node     Non-constant Operand of middle_node
 * @param shdata       Shannon data
 */
static void find_path_to_top_node(ir_node *current, ir_node *other_node, ir_node *base_node, ir_node *middle_node, ir_node *top_node, shannon_data *shdata)
{
	ir_node *top_node2;
	ir_node *middle_node2;
	if (current == middle_node) {
		top_node2    = middle_node;
		middle_node2 = NULL;
	} else {
		top_node2    = top_node;
		middle_node2 = middle_node;
	}

	if (current == top_node2 && ((middle_node && !only_one_user(middle_node)) || base_node != other_node)) {
		optimization_t *optimization = XMALLOC(optimization_t);
		optimization->base_node      = base_node;
		optimization->middle_node    = middle_node2;
		optimization->top_node       = top_node2;
		optimization->other_node     = other_node;

		deq_push_pointer_right(&shdata->optimizations, optimization);

		return;
	}

	uintptr_t counter = (uintptr_t)pmap_get(void, shdata->walk_counter, current);
	if (counter < shdata->walk_base) {
		counter = shdata->walk_base;
	}
	counter++;
	if (counter > shdata->walk_max) {
		shdata->walk_max = counter;
	}
	pmap_insert(shdata->walk_counter, current, (void *)counter);

	if ((counter - shdata->walk_base) == (unsigned)get_irn_n_edges(current) && is_bitop(current)) {
		foreach_irn_in(current, i, n) {
			find_path_to_top_node(n, current, base_node, middle_node, top_node, shdata);
		}
	}
}

/**
 * If given node is a middle_node, return the top_node. Else return the node itself.
 */
static ir_node *get_topnode_from_middlenode(ir_node *node)
{
	if (is_Not(node))
		return get_Not_op(node);

	if (is_Eor(node)) {
		ir_node *l = get_Eor_left(node);
		ir_node *r = get_Eor_right(node);
		if (is_Const(r)) {
			return l;
		}
		if (is_Const(l)) {
			return r;
		}
	}

	return node;
}

/**
 * Walker function that tries to find a top_node to given base_node.
 */
static void try_basenode(ir_node *base_node, void *env)
{
	if (!is_And(base_node) && !is_Or(base_node)) {
		return;
	}

	shannon_data *shdata = (shannon_data *)env;
	ir_node      *l      = get_binop_left(base_node);
	ir_node      *r      = get_binop_right(base_node);

	for (int i = 0; i < 2; i++) {
		ir_node *top_node    = get_topnode_from_middlenode(l);
		ir_node *middle_node = NULL;
		if (top_node != l) {
			middle_node = l;
		}

		shdata->walk_base = shdata->walk_max;
		find_path_to_top_node(r, base_node, base_node, middle_node, top_node, shdata);

		ir_node *t = l;
		l = r;
		r = t;
	}
}

/**
 * Replace top_node from given other_node by constant. base_node could be And or Or and is used to
 * decide if the constant will be (replacement Eor -1) or (replacement Eor 0).
 */
static void replace_node(ir_node *top_node, ir_node *base_node, ir_node *other_node, ir_tarval *replacement)
{
	assert(is_And(base_node) || is_Or(base_node));

	/* find index of top_node from other_node */
	int pos = -1;
	foreach_irn_in(other_node, i, n) {
		if (n == top_node) {
			pos = i;
			break;
		}
	}
	assert(pos >= 0);

	ir_mode   *other_mode = get_irn_mode(other_node);
	ir_tarval *base_val   = is_And(base_node) ? get_mode_all_one(other_mode) : get_mode_null(other_mode);
	dbg_info  *dbgi       = get_irn_dbg_info(other_node);
	ir_graph  *irg        = get_irn_irg(top_node);
	ir_tarval *tv         = tarval_eor(base_val, replacement);
	ir_node   *c          = new_rd_Const(dbgi, irg, tv);
	set_irn_n(other_node, pos, c);
}

/**
 * Returns the tarval of the const operator of the node.
 */
static ir_tarval *get_Eor_tarval(ir_node *node)
{
	assert(is_Eor(node));
	ir_node *l = get_Eor_left(node);
	ir_node *r = get_Eor_right(node);

	if (is_Const(l))
		return get_Const_tarval(l);

	assert(is_Const(r));
	return get_Const_tarval(r);
}

/**
 * Returns true iff operand is a operand of node.
 */
static bool has_operand(ir_node *node, ir_node *operand)
{
	foreach_irn_in(node, i, n) {
		if (n == operand) {
			return true;
		}
	}
	return false;
}

/**
 * Applies Shannon to given irg.
 */
static void do_shannon(ir_graph *irg)
{
	shannon_data shdata = {
		.irg          = irg,
		.walk_counter = pmap_create(),
		.walk_base    = 0,
		.walk_max     = 0
	};
	deq_init(&shdata.optimizations);

	/* walk and get optimization data */
	irg_walk_edges(get_irg_start_block(irg), NULL, try_basenode, &shdata);

	/* optimize */
	DBG((dbg, LEVEL_4, "optimizations:\n"));
	deq_foreach_pointer(&shdata.optimizations, optimization_t, optimization) {
		ir_node *const middle_node = optimization->middle_node;
		ir_node *const top_node    = optimization->top_node;
		ir_node *const base_node   = optimization->base_node;
		ir_node *const other_node  = optimization->other_node;

		DBG((dbg, LEVEL_4, "base_node: %+F, middle_node: %+F, top_node: %+F, other_node: %+F\n",
		     base_node, middle_node, top_node, other_node));

		/* This case can be handled by local optimizations.
		 * We skip the case, because we would otherwise
		 * simplify a & a to -1 & -1, or need to track edges
		 * instead of nodes in the optimization environment. */
		if (base_node == other_node)
			continue;

		/* check if optimization is still valid */
		if (middle_node) {
			if (!has_operand(middle_node, top_node) || !has_operand(base_node, middle_node)) {
				continue;
			}
		} else if (!has_operand(base_node, top_node)) {
			continue;
		}

		if (!has_operand(other_node, top_node)) {
			continue;
		}

		/* calculate replacement */
		ir_mode   *mode        = get_irn_mode(top_node);
		ir_tarval *replacement;
		if (!middle_node) {
			replacement = get_mode_null(mode);
		} else if (is_Not(middle_node)) {
			replacement = get_mode_all_one(mode);
		} else {
			assert(is_Eor(middle_node));
			replacement = get_Eor_tarval(middle_node);
		}

		/* replace */
		replace_node(top_node, base_node, other_node, replacement);
		DBG((dbg, LEVEL_4, "replaced\n"));

		free(optimization);
	}

	pmap_destroy(shdata.walk_counter);
	deq_free(&shdata.optimizations);
}

typedef enum {
    NOT_FOUND,    /**< no match found */
    FOUND_FIRST,  /**< found a match for the first node */
    FOUND_SECOND  /**< found a match for the second node */
} match_result_t;

/**
 * Recursively replaces a with b/~b, or b with a/~a, according to need_not.
 *
 * We only want to replace something if our subgraph contains a and b.
 * So we set the @p replace flag when we found the first node and perform
 * the actual replacement if we found the second one.
 *
 * @param node      The currently visited node
 * @param a         The first node we want to match
 * @param b         The second node we want to match
 * @param need_not  Whether we should replace a with ~b (instead of b), or vice versa.
 * @param replace   Whether we already found one node and thus can replace the other.
 *
 * @return match result that indicates whether we found a or b first, or none of them.
 */
static match_result_t replace_until_other_user(ir_node *node, ir_node *a, ir_node *b, bool need_not, bool replace)
{
	match_result_t ret = replace;
	if (!is_bitop(node) || !only_one_user(node)) {
		return ret;
	}

	foreach_irn_in(node, i, t) {
		if (t == a) {
			if (replace) {
				ir_node *op = b;
				if (need_not) {
					op = new_rd_Not(get_irn_dbg_info(op), get_nodes_block(op), op);
				}
				DBG((dbg, LEVEL_4, "replace %+F @ %+F with %+F\n", t, node, op));
				set_irn_n(node, i, op);
			} else {
				replace = true;
				ir_node *t  = a;
				a   = b;
				b   = t;
				ret = FOUND_SECOND;
			}
		} else if (t == b) {
			if (!replace) {
				replace = true;
				ret     = FOUND_FIRST;
			}
		} else {
			match_result_t res = replace_until_other_user(t, a, b, need_not, replace);
			switch (res) {
			case FOUND_SECOND: {
				ir_node *t  = a;
				a = b;
				b = t;
			} /* fall through */
			case FOUND_FIRST:
				if (!replace)
					ret = res;
				replace = true;
				break;
			default:
				break;
			}
		}
	}

	return ret;
}

/**
 * If one of the following cases is matched, replace_until_other_user will be called:
 *  (a ^ b) & f(a, b) ->  (a ^ b) & f(a, ~a)
 * ~(a ^ b) & f(a, b) -> ~(a ^ b) & f(a,  a)
 *  (a ^ b) | f(a, b) ->  (a ^ b) | f(a,  a)
 * ~(a ^ b) | f(a, b) -> ~(a ^ b) | f(a, ~a)
 */
static void walk_equality(ir_node *node)
{
	if (!is_And(node) && !is_Or(node)) {
		return;
	}

	ir_node *l        = get_binop_left(node);
	ir_node *r        = get_binop_right(node);
	bool     need_not = is_And(node);

	if (is_Not(l) && is_Eor(get_Not_op(l))) {
		l        = get_Not_op(l);
		need_not = !need_not;
	} else if (is_Not(r) && is_Eor(get_Not_op(r))) {
		ir_node *t = get_Not_op(r);
		r          = l;
		l          = t;
		need_not   = !need_not;
	} else if (is_Eor(r)) {
		ir_node *t = r;
		r          = l;
		l          = t;
	} else if (!is_Eor(l)) {
		return;
	}

	ir_node *a = get_binop_left(l);
	ir_node *b = get_binop_right(l);
	if (a == b) {
		return;
	}

	if (is_Const(b)) {
		replace_until_other_user(r, a, b, need_not, true);
	} else if (is_Const(a)) {
		replace_until_other_user(r, b, a, need_not, true);
	} else {
		if (is_Not(a)) {
			need_not = !need_not;
			a        = get_Not_op(a);
		}

		if (is_Not(b)) {
			need_not = !need_not;
			b        = get_Not_op(b);
		}

		replace_until_other_user(r, a, b, need_not, false);
	}
}

typedef struct {
	ir_node *base_node;      /**< Root node of the multiset */
	pset    *operands;       /**< Non-constant operands of the multiop */
	pset    *nodes;          /**< Nodes that belong to the multiop */
	pmap    *multiplier;     /**< Maps each node to its multiplier,
	                              e.g. x - y + x has the mapping x -> 2 and y -> -1 */
	pmap    *edge_value;     /**< Edge-based multiplier, used to compute the multiplier above */
	pset    *multi_operands; /**< Multiops of all operands */
	pset    *multi_users;    /**< Multiops of all users */
	bool     other_op;       /**< Whether a node has a user that does not belong to any multiop in multi_users
	                              (because the operation is not supported by multiops) */
	bool     changed;        /**< Whether we reassociate the nodes in the multiop */
} multi_op;

typedef struct {
	pmap *set_map;     /**< Maps each node to its multi_op */
	pset *sets;        /**< Set of all multi_ops */
	pset *walkhistory; /**< Visited nodes during computation of multi_ops */
} multi_op_env;

/**
 * Creates a new multi_op. This is a set of operations, with same type and mode.
 * Sub and scalar Mul is seen as Add.
 */
static multi_op *new_multi_op(multi_op_env *multi_env, ir_node *base_node)
{
	multi_op *data       = XMALLOC(multi_op);
	data->operands       = pset_new_ptr_default();
	data->nodes          = pset_new_ptr_default();
	data->multi_operands = pset_new_ptr_default();
	data->multi_users    = pset_new_ptr_default();
	data->base_node      = base_node;
	data->other_op       = false;
	data->changed        = false;
	data->multiplier     = pmap_create();
	data->edge_value     = pmap_create();

	if (is_Add(base_node) || is_Sub(base_node)) {
		ir_mode *mode = get_irn_mode(base_node);
		if (mode_is_reference(mode)) {
			ir_node *right      = get_binop_right(base_node);
			ir_mode *right_mode = get_irn_mode(right);
			if (mode_is_reference(right_mode)) {
				ir_node *left = get_binop_left(base_node);
				mode = get_irn_mode(left);
			} else {
				mode = right_mode;
			}
		}
		pmap_insert(data->edge_value, base_node, get_mode_one(mode));
	} else if (is_Mul(base_node)) {
		ir_node *l = get_Mul_left(base_node);
		ir_node *r = get_Mul_right(base_node);
		if (is_Const(l)) {
			pmap_insert(data->edge_value, base_node, get_Const_tarval(l));
			if (tarval_is_negative(get_Const_tarval(l))) {
				data->changed = true;
			}
		} else {
			assert(is_Const(r));
			pmap_insert(data->edge_value, base_node, get_Const_tarval(r));
			if (tarval_is_negative(get_Const_tarval(r))) {
				data->changed = true;
			}
		}
	}

	pset_insert_ptr(data->nodes, base_node);
	pmap_insert(multi_env->set_map, base_node, data);
	pset_insert_ptr(multi_env->sets, data);

	return data;
}

static ir_op *get_multi_op_op(multi_op *multi_op)
{
	ir_node *base_node = multi_op->base_node;
	return is_Sub(base_node) || is_Mul(base_node) ? op_Add : get_irn_op(base_node);
}

/**
 * Destroys a multi_op and frees its memory.
 */
static void destroy_multi_op(multi_op *o)
{
	del_pset(o->operands);
	del_pset(o->nodes);
	del_pset(o->multi_operands);
	del_pset(o->multi_users);
	pmap_destroy(o->multiplier);
	pmap_destroy(o->edge_value);

	free(o);
}

/**
 * Returns true if the given node is a scalar multiplication.
 */
static bool is_scalar_Mul(ir_node *node)
{
	return is_Mul(node) && (is_Const(get_Mul_left(node)) || is_Const(get_Mul_right(node)));
}

/**
 * Adds a node to a Eor set.
 */
static void add_to_Eor_set(multi_op_env *multi_env, ir_node *node, multi_op *set)
{
	if (get_irn_op(node) == get_multi_op_op(set)) {
		assert(!pset_find_ptr(set->nodes, node));

		pset_insert_ptr(set->nodes, node);
		pmap_insert(multi_env->set_map, node, set);
	} else if (pset_find_ptr(set->operands, node)) {
		pset_remove_ptr(set->operands, node);
	} else {
		pset_insert_ptr(set->operands, node);
	}
}

/**
 * Adds a node to an And or Or set.
 */
static void add_to_And_Or_set(multi_op_env *multi_env, ir_node *node, multi_op *set)
{
	if (get_irn_op(node) == get_multi_op_op(set)) {
		pset_insert_ptr(set->nodes, node);
		pmap_insert(multi_env->set_map, node, set);
	} else {
		pset_insert_ptr(set->operands, node);
	}
}

/**
 * Adds a node to a Add set, with given entry. The entry is the user of the node and indicates it's value.
 * Example: In 3*(x + y) is 3 the value of (x+y) and * is the entry.
 */
static void add_to_Add_set(multi_op_env *multi_env, ir_node *node, multi_op *set, ir_node *entry)
{
	ir_mode *mode      = get_irn_mode(node);
	ir_op   *op        = get_irn_op(node);
	ir_node *base_node = set->base_node;
	ir_mode *base_mode = get_irn_mode(base_node);
	if (is_scalar_Mul(node) && mode == base_mode) {
		/* add scalar Mul to set */
		assert(!entry);
		assert(!pset_find_ptr(set->nodes, node));

		ir_tarval *val = get_mode_null(mode);

		foreach_out_edge(node, edge) {
			ir_node   *src = get_edge_src_irn(edge);
			ir_tarval *t   = pmap_get(ir_tarval, set->edge_value, src);
			assert(t);
			if (is_Sub(src) && get_Sub_right(src) == node) {
				t = tarval_neg(t);
			}
			val = tarval_add(val, t);
		}

		if (is_Const(get_Mul_left(node))) {
			val = tarval_mul(val, get_Const_tarval(get_Mul_left(node)));
		} else {
			val = tarval_mul(val, get_Const_tarval(get_Mul_right(node)));
		}
		assert(tarval_is_constant(val));

		pset_insert_ptr(set->nodes, node);
		pmap_insert(multi_env->set_map, node, set);
		pmap_insert(set->edge_value, node, val);
	} else if ((op == op_Add || (op == op_Sub && !mode_is_reference(get_irn_mode(get_Sub_right(node)))
		       && !mode_is_reference(get_irn_mode(get_Sub_left(node))))) && mode == base_mode) {
		/* add Add or Sub to set */
		assert(!entry);
		assert(!pset_find_ptr(set->nodes, node));

		if (mode_is_reference(mode)) {
			if (mode_is_reference(get_irn_mode(get_binop_right(node)))) {
				mode = get_irn_mode(get_binop_left(node));
			} else {
				mode = get_irn_mode(get_binop_right(node));
			}
		}
		ir_tarval *val = get_mode_null(mode);
		foreach_out_edge(node, edge) {
			ir_node *src = get_edge_src_irn(edge);

			ir_tarval *t = pmap_get(ir_tarval, set->edge_value, src);
			if (tarval_is_one(t) && get_tarval_mode(t) != mode) {
				t = get_mode_one(get_tarval_mode(val));
			}
			assert(t);
			if (is_Sub(src) && get_Sub_right(src) == node) {
				t = tarval_neg(t);
			}
			val = tarval_add(val, t);
		}
		assert(tarval_is_constant(val));

		pset_insert_ptr(set->nodes, node);
		pmap_insert(multi_env->set_map, node, set);
		pmap_insert(set->edge_value, node, val);
	} else {
		assert(entry);

		ir_tarval *new_val = pmap_get(ir_tarval, set->edge_value, entry);
		assert(new_val);

		if (is_Sub(entry) && get_Sub_right(entry) == node) {
			new_val = tarval_neg(new_val);
		}

		if (is_Const(node) && is_Mul(entry)) {
			DBG((dbg, LEVEL_5, "Const skipped\n"));
			return;
		}

		if (pset_find_ptr(set->operands, node)) {
			ir_tarval *old_val = pmap_get(ir_tarval, set->multiplier, node);
			new_val            = tarval_add(old_val, new_val);
			set->changed       = true;
		} else {
			pset_insert_ptr(set->operands, node);
		}
		pmap_insert(set->multiplier, node, new_val);
	}
}

/**
 * Adds a node to a set, with given entry.
 */
static void add_to_set(multi_op_env *multi_env, ir_node *node, multi_op *set, ir_node *entry)
{
	switch (get_op_code(get_multi_op_op(set))) {
	case iro_Eor:
		add_to_Eor_set(multi_env, node, set);
		break;
	case iro_And:
	case iro_Or:
		add_to_And_Or_set(multi_env, node, set);
		break;
	case iro_Add:
		add_to_Add_set(multi_env, node, set, entry);
		break;

	default:
		panic("Operation not supported");
	}
}

/**
 * Connects an operand multi_op to a user multi_op.
 * If operand is not Eor it would be added to operands.
 */
static void connect_Eor(multi_op *operand, multi_op *user)
{
	assert(get_multi_op_op(user) == op_Eor);

	if (pset_find_ptr(user->multi_operands, operand)) {
		assert(pset_find_ptr(operand->multi_users, user));

		pset_remove_ptr(user->multi_operands, operand);
		pset_remove_ptr(operand->multi_users, user);

		user->changed = true;
	} else {
		pset_insert_ptr(operand->multi_users, user);
		pset_insert_ptr(user->multi_operands, operand);
	}
}

/**
 * Connects an operand multi_op to a user multi_op.
 * If operand is not And it would be added to operands.
 */
static void connect_And_Or(multi_op *operand, multi_op *user)
{
	assert(get_multi_op_op(user) == op_And || get_multi_op_op(user) == op_Or);

	if (pset_find_ptr(user->multi_operands, operand)) {
		assert(pset_find_ptr(operand->multi_users, user));

		user->changed = true;
	} else {
		pset_insert_ptr(operand->multi_users, user);
		pset_insert_ptr(user->multi_operands, operand);
	}
}

/**
 * Connects an operand multi_op to a user multi_op.
 * Entry gives the value of the operand, like in add_to_Add_set.
 */
static void connect_Add(multi_op *operand, multi_op *user, ir_node *op_node, ir_node *entry)
{
	assert(entry);

	ir_mode *mode = get_irn_mode(operand->base_node);
	if (mode_is_reference(mode)) {
		if (mode_is_reference(get_irn_mode(get_binop_right(operand->base_node)))) {
			mode = get_irn_mode(get_binop_left(operand->base_node));
		} else {
			mode = get_irn_mode(get_binop_right(operand->base_node));
		}
	}
	ir_tarval *new_val = get_mode_null(mode);
	ir_tarval *t = pmap_get(ir_tarval, user->edge_value, entry);

	if (tarval_is_one(t) && get_tarval_mode(t) != mode) {
		t = get_mode_one(mode);
	}

	new_val = tarval_add(new_val, t);
	assert(new_val);

	if (is_Sub(entry) && get_Sub_right(entry) == op_node) {
		new_val = tarval_neg(new_val);
	}

	assert(get_multi_op_op(user) == op_Add);

	if (pset_find_ptr(user->multi_operands, operand)) {
		assert(pset_find_ptr(operand->multi_users, user));

		ir_tarval *val = pmap_get(ir_tarval, user->multiplier, operand);
		new_val        = tarval_add(val, new_val);

		user->changed = true;
	} else {
		pset_insert_ptr(operand->multi_users, user);
		pset_insert_ptr(user->multi_operands, operand);
	}
	pmap_insert(user->multiplier, operand, new_val);
}

/**
 * Connects a operand multi_op to a user multi_op. Connect means operand is a whole operand of user.
 */
static void connect_sets(multi_op *operand, multi_op *user, ir_node *op_node, ir_node *user_node)
{
	switch (get_op_code(get_multi_op_op(user))) {
	case iro_Eor:
		connect_Eor(operand, user);
		break;
	case iro_And:
	case iro_Or:
		connect_And_Or(operand, user);
		break;
	case iro_Add:
		connect_Add(operand, user, op_node, user_node);
		break;
	default:
		panic("Operation not supported");
	}

}

/**
 * Returns the first user of node.
 */
static ir_node *get_first_user(ir_node *node)
{
	return get_edge_src_irn(get_irn_out_edge_first(node));
}


/**
 * Returns true iff the given node is supported by setsort, else it would be handled as a variable.
 */
static bool is_supported_node(ir_node *node)
{
	if (mode_is_float(get_irn_mode(node))) {
		return false;
	}

	switch (get_op_code(get_irn_op(node))) {
	case iro_Eor:
	case iro_And:
	case iro_Or:
	case iro_Add:
		return true;
	case iro_Sub:
		return !mode_is_reference(get_irn_mode(get_Sub_left(node)))
		    && !mode_is_reference(get_irn_mode(get_Sub_right(node)));
	case iro_Mul:
		return is_scalar_Mul(node);
	default:
		return false;
	}
}

/**
 * Rewalk a node if it is called before its user.
 *
 * @param multi_env The multiop environment
 * @param node      Node that was visited before its user @p user
 * @param user      The user that was visited after @p node
 */
static void second_walk(multi_op_env *multi_env, ir_node *node, ir_node *user)
{
	assert(user);

	multi_op *user_set = pmap_get(multi_op, multi_env->set_map, user);
	if (is_supported_node(node)) {
		multi_op *operand_set = pmap_get(multi_op, multi_env->set_map, node);
		assert(operand_set);

		connect_sets(operand_set, user_set, node, user);
	} else if (get_multi_op_op(user_set) == op_Add) {
		ir_tarval *new_val = pmap_get(ir_tarval, user_set->edge_value, user);
		assert(new_val);

		if (is_Sub(user) && get_Sub_right(user) == node) {
			new_val = tarval_neg(new_val);
		}

		if (is_Mul(user) && is_Const(node)) {
			DBG((dbg, LEVEL_5, "Const skipped\n"));
			return;
		}

		if (pset_find_ptr(user_set->operands, node)) {
			ir_tarval *old_val = pmap_get(ir_tarval, user_set->multiplier, node);
			new_val            = tarval_add(old_val, new_val);

			pmap_insert(user_set->multiplier, node, new_val);

			user_set->changed = true;
		} else {
			pmap_insert(user_set->multiplier, node, new_val);
			pset_insert_ptr(user_set->operands, node);
		}
	} else {
		pset_insert_ptr(user_set->operands, node);
	}
}

/**
 * Walk on the graph and add nodes to sets.
 */
static void walk_sets(ir_node *node, void *env)
{
	if (is_Block(node)) {
		return;
	}

	DBG((dbg, LEVEL_5, "%+F\n", node));

	multi_op_env *multi_env = (multi_op_env *)env;
	pmap         *set_map   = multi_env->set_map;

	if (is_supported_node(node)) {
		bool      is_same_set = true;
		multi_op *user_set    = pmap_get(multi_op, set_map, get_first_user(node));
		if (!user_set || get_irn_mode(user_set->base_node) != get_irn_mode(node)) {
			is_same_set = false;
		} else {
			foreach_out_edge(node, edge) {
				ir_node *user = get_edge_src_irn(edge);
				if (!pset_find_ptr(user_set->nodes, user)) {
					is_same_set = false;
					break;
				}
			}
		}

		ir_op *op = get_irn_op(node);
		if (is_same_set && (get_multi_op_op(user_set) == op || (get_multi_op_op(user_set) == op_Add && (op == op_Sub || is_scalar_Mul(node))))) {
			add_to_set(multi_env, node, user_set, NULL);
		} else {
			/* create new Set and add to user sets */
			multi_op *new_set = new_multi_op(multi_env, node);

			/* connect to all multi users */
			foreach_out_edge(node, edge) {
				ir_node  *user = get_edge_src_irn(edge);
				multi_op *t    = pmap_get(multi_op, set_map, user);
				if (t) {
					connect_sets(new_set, t, node, user);
				}

				if (!is_supported_node(user)) {
					new_set->other_op = true;
				}
			}
		}

		foreach_irn_in(node, i, n) {
			if (pset_find_ptr(multi_env->walkhistory, n)) {
				DBG((dbg, LEVEL_5, "%+F already visited!\n", n));
				second_walk(multi_env, n, node);
			}
		}
	} else {
		/* add unsupported operations to multiops */
		foreach_out_edge(node, edge) {
			ir_node  *user = get_edge_src_irn(edge);
			multi_op *t    = pmap_get(multi_op, set_map, user);
			if (t) {
				add_to_set(multi_env, node, t, user);
			}
		}
	}

	pset_insert_ptr(multi_env->walkhistory, node);
}

/**
 * insert elements from a into b. a will be freed.
 */
static void merge(multi_op_env *multi_env, multi_op *a, multi_op *b)
{
	assert(get_multi_op_op(a) == get_multi_op_op(b));
	assert(!a->other_op);
	assert(pset_count(a->multi_users) == 1);

	DBG((dbg, LEVEL_4, "merge %+F in %+F...\n", a->base_node, b->base_node));

	if (get_multi_op_op(b) == op_Add) {
		ir_tarval *a_val = pmap_get(ir_tarval, b->multiplier, a);

		foreach_pset(a->operands, ir_node, node) {
			ir_tarval *val;
			if (!tarval_is_one(a_val)) {
				val = tarval_mul(a_val, pmap_get(ir_tarval, a->multiplier, node));
			} else {
				val = pmap_get(ir_tarval, a->multiplier, node);
			}

			if (pset_find_ptr(b->operands, node)) {
				val = tarval_add(val, pmap_get(ir_tarval, b->multiplier, node));
				pmap_insert(b->multiplier, node, val);
			} else {
				pmap_insert(b->multiplier, node, val);
				pset_insert_ptr(b->operands, node);
			}
		}

		pset_remove_ptr(b->multi_operands, a);

		foreach_pset(a->multi_operands, multi_op, o) {
			ir_tarval *val;
			if (!tarval_is_one(a_val)) {
				val = tarval_mul(a_val, pmap_get(ir_tarval, a->multiplier, o));
			} else {
				val = pmap_get(ir_tarval, a->multiplier, o);
			}

			if (pset_find_ptr(b->multi_operands, o)) {
				val = tarval_add(val, pmap_get(ir_tarval, b->multiplier, o));
				pmap_insert(b->multiplier, o, val);
			} else {
				pmap_insert(b->multiplier, o, val);
				pset_insert_ptr(b->multi_operands, o);
			}

			pset_remove_ptr(o->multi_users, a);
			pset_insert_ptr(o->multi_users, b);
		}
	} else {
		foreach_pset(a->operands, ir_node, n) {
			add_to_set(multi_env, n, b, NULL);
		}

		pset_remove_ptr(b->multi_operands, a);

		foreach_pset(a->multi_operands, multi_op, o) {
			connect_sets(o, b, NULL, NULL);

			pset_remove_ptr(o->multi_users, a);
		}
	}

	pset_remove_ptr(multi_env->sets, a);
	destroy_multi_op(a);
	b->changed = true;
}

/**
 * Returns whether the given multi_op has only one operand.
 * Example:
 * 1*x -> true
 * 2*x, x+y, x^y -> false
 */
static bool is_trivial_multi_op(multi_op *o)
{
	if (pset_count(o->operands) + pset_count(o->multi_operands) != 1) {
		return false;
	}

	if (get_multi_op_op(o) == op_Add) {
		if (pset_count(o->operands) == 1) {
			foreach_pset(o->operands, ir_node, n) {
				assert(pmap_get(ir_tarval, o->multiplier, n));
				if (!tarval_is_one(pmap_get(ir_tarval, o->multiplier, n))) {
					pset_break(o->operands);
					return false;
				}
			}
		} else {
			assert(pset_count(o->multi_operands) == 1);
			foreach_pset(o->multi_operands, multi_op, m) {
				assert(pmap_get(ir_tarval, o->multiplier, m));
				if (!tarval_is_one(pmap_get(ir_tarval, o->multiplier, m))) {
					pset_break(o->multi_operands);
					return false;
				}
			}
		}
	}

	return true;
}

/**
 * Inserts elements from the trivial multiop a into b.
 */
static void merge_trivial_multi_op(multi_op_env *multi_env, multi_op *a, multi_op *b)
{
	assert(is_trivial_multi_op(a));

	DBG((dbg, LEVEL_4, "merge trivial multiop %+F in %+F...\n", a->base_node, b->base_node));

	foreach_pset(a->operands, ir_node, n) {
		if (get_multi_op_op(b) == op_Add) {
			ir_tarval *val = pmap_get(ir_tarval, b->multiplier, a);
			if (pset_find_ptr(b->operands, n)) {
				val = tarval_add(val, pmap_get(ir_tarval, b->multiplier, n));
			} else {
				pset_insert_ptr(b->operands, n);
			}
			pmap_insert(b->multiplier, n, val);
		} else {
			add_to_set(multi_env, n, b, NULL);
		}
	}

	pset_remove_ptr(b->multi_operands, a);

	foreach_pset(a->multi_operands, multi_op, o) {
		if (get_multi_op_op(b) == op_Add) {
			ir_tarval *val = pmap_get(ir_tarval, b->multiplier, a);
			if (pset_find_ptr(b->multi_operands, o)) {
				val = tarval_add(val, pmap_get(ir_tarval, b->multiplier, o));
			} else {
				pset_insert_ptr(b->multi_operands, o);
			}
			pmap_insert(b->multiplier, o, val);
			pset_insert_ptr(o->multi_users, b);
		} else {
			connect_sets(o, b, NULL, NULL);
		}

		pset_remove_ptr(o->multi_users, a);
	}

	pset_remove_ptr(multi_env->sets, a);
	destroy_multi_op(a);
	b->changed = true;
}

/**
 * Returns true iff all multi_ops in set are from type op.
 */
static bool is_same_op(pset *set, ir_op *op)
{
	foreach_pset(set, multi_op, o) {
		if (get_multi_op_op(o) != op) {
			pset_break(set);
			return false;
		}
	}
	return true;
}

/**
 * Returns the user of a multi_op with only one user.
 */
static multi_op *get_user(multi_op *o)
{
	assert(pset_count(o->multi_users) == 1);
	multi_op *user = NULL;
	foreach_pset(o->multi_users, multi_op, n) {
		user = n;
	}
	assert(user && "Found no multi op user");
	return user;
}

static ir_node *rebuild_node(multi_op *o, ir_node *curr, ir_node *node)
{
	ir_node  *block = get_nodes_block(o->base_node);
	dbg_info *dbgi  = get_irn_dbg_info(o->base_node);

	if (!curr)
		return node;

	switch (get_op_code(get_multi_op_op(o))) {
	case iro_Eor:
		return new_rd_Eor(dbgi, block, node, curr);
	case iro_And:
		return new_rd_And(dbgi, block, node, curr);
	case iro_Or:
		return new_rd_Or(dbgi, block, node, curr);
	default:
		panic("Operation not supported");
	}
}

/**
 * Rebuild the graph according to the multi_ops. Only sets with change flag set, will bereplaced.
 */
static void rebuild(multi_op_env *multi_env)
{
	DBG((dbg, LEVEL_5, "rebuilding...\n"));

	foreach_pset(multi_env->sets, multi_op, o) {
		if (!o->changed) {
			continue;
		}

		DBG((dbg, LEVEL_5, "rebuild %+F\n", o->base_node));

		ir_node  *block = get_nodes_block(o->base_node);
		dbg_info *dbgi  = get_irn_dbg_info(o->base_node);
		ir_node  *curr  = NULL;


		if (get_multi_op_op(o) == op_Add) {
			/* rebuild polynoms */
			pmap    *dic     = pmap_create();
			ir_node *pointer = NULL;

			foreach_pset(o->multi_operands, multi_op, operand) {
				ir_node *node = operand->base_node;

				/* In each Add-Set there should be at most one pointer.
				 * If that's the case it have to be topmost. */
				if (mode_is_reference(get_irn_mode(node))) {
					assert(pointer == NULL);
					pointer = node;
					continue;
				}

				ir_tarval *val = pmap_get(ir_tarval, o->multiplier, operand);
				assert(val);
				if (!tarval_is_null(val)) {
					pset *dic_set = pmap_get(pset, dic, val);
					if (!dic_set) {
						dic_set = pset_new_ptr_default();
						pmap_insert(dic, val, dic_set);
					}

					pset_insert_ptr(dic_set, node);
				}
			}
			foreach_pset(o->operands, ir_node, node) {
				/* In each Add-Set there should be at most one pointer.
				 * If that's the case it have to be topmost. */
				if (mode_is_reference(get_irn_mode(node))) {
					assert(pointer == NULL);
					pointer = node;
					continue;
				}

				ir_tarval *val = pmap_get(ir_tarval, o->multiplier, node);
				assert(val);
				if (!tarval_is_null(val)) {
					pset *dic_set = pmap_get(pset, dic, val);
					if (!dic_set) {
						dic_set = pset_new_ptr_default();
						pmap_insert(dic, val, dic_set);
					}

					pset_insert_ptr(dic_set, node);
				}
			}

			curr = pointer;
			pset    *done      = pset_new_ptr_default();
			deq_t negatives;
			deq_init(&negatives);

			foreach_pmap(dic, entry) {
				ir_tarval *curr_val = (ir_tarval *)entry->key;

				if (pset_find_ptr(done, curr_val)) {
					continue;
				}

				ir_tarval *neg_val    = tarval_neg(curr_val);
				bool       is_max_val = neg_val == curr_val;

				if (is_max_val || pmap_contains(dic, neg_val) || !tarval_is_negative(curr_val)) {
					assert(!(mode_is_signed(get_irn_mode(o->base_node)) && pset_find_ptr(done, neg_val)));

					ir_tarval *negativ_val;
					ir_tarval *positiv_val;

					if (is_max_val || !tarval_is_negative(curr_val)) {
						negativ_val = neg_val;
						positiv_val = curr_val;
					} else {
						negativ_val = curr_val;
						positiv_val = neg_val;
					}

					pset *negativ_set = NULL;
					if (!is_max_val && mode_is_signed(get_irn_mode(o->base_node))) {
						negativ_set = pmap_get(pset, dic, negativ_val);
					}
					pset *positiv_set = pmap_get(pset, dic, positiv_val);
					assert(positiv_set);

					ir_node *inner = NULL;
					bool is_one = tarval_is_one(positiv_val);
					if (curr && is_one) {
						inner = curr;
					}
					foreach_pset(positiv_set, ir_node, node) {
						if (!inner) {
							inner = node;
						} else {
							inner = new_rd_Add(dbgi, block, inner, node);
						}
					}
					assert(inner);
					if (negativ_set) {
						foreach_pset(negativ_set, ir_node, node) {
							inner = new_rd_Sub(dbgi, block, inner, node);
						}
					}

					if (!is_one) {
						ir_node *c = new_rd_Const(dbgi, get_irn_irg(inner), positiv_val);
						inner      = new_rd_Mul(dbgi, block, c, inner);
					}

					if (!curr || is_one) {
						curr = inner;
					} else {
						curr = new_rd_Add(dbgi, block, curr, inner);
					}

					pset_insert_ptr(done, positiv_val);
					if (negativ_set) {
						pset_insert_ptr(done, negativ_val);
					}
				} else {
					deq_push_pointer_right(&negatives, curr_val);
				}

			}

			deq_foreach_pointer(&negatives, ir_tarval, negativ_val) {
				assert(tarval_is_negative(negativ_val));

				pset *negativ_set = pmap_get(pset, dic, negativ_val);
				assert(negativ_set);

				ir_node *inner = NULL;
				foreach_pset(negativ_set, ir_node, node) {
					if (!inner) {
						inner = node;
					} else {
						inner = new_rd_Add(dbgi, block, inner, node);
					}
				}

				ir_tarval *positiv_val = tarval_neg(negativ_val);
				if (!tarval_is_one(positiv_val)) {
					ir_mode *node_mode = get_irn_mode(inner);
					ir_node *c         = new_rd_Const(dbgi, get_irn_irg(inner), tarval_convert_to(positiv_val, node_mode));
					inner              = new_rd_Mul(dbgi, block, c, inner);
				}

				if (!curr) {
					curr = new_rd_Minus(dbgi, block, inner);
				} else {
					curr = new_rd_Sub(dbgi, block, curr, inner);
				}
			}

			deq_free(&negatives);
			del_pset(done);
			foreach_pmap(dic, entry) {
				pset *dic_set = (pset *)entry->value;
				del_pset(dic_set);
			}
			pmap_destroy(dic);
		} else {
			/* rebuild other sets */
			foreach_pset(o->multi_operands, multi_op, operand) {
				ir_node *node = operand->base_node;
				curr = rebuild_node(o, curr, node);
			}
			foreach_pset(o->operands, ir_node, node) {
				curr = rebuild_node(o, curr, node);
			}
		}

		if (!curr) {
			ir_node  *base      = o->base_node;
			ir_graph *irg       = get_irn_irg(base);
			ir_mode  *base_mode = get_irn_mode(base);
			curr = new_rd_Const_null(dbgi, irg, base_mode);
		}

		if (o->base_node != curr) {
			DBG((dbg, LEVEL_4, "exchanging... %+F with %+F\n", o->base_node, curr));

			exchange(o->base_node, curr);
			assert(get_irn_mode(o->base_node) == get_irn_mode(curr));
			o->base_node = curr;
		}
	}
}

/**
 * Applies setsort to a given irg.
 */
static void do_Setsort(ir_graph *irg)
{
	multi_op_env *multi_env = XMALLOC(multi_op_env);
	multi_env->set_map     = pmap_create();
	multi_env->sets        = pset_new_ptr_default();
	multi_env->walkhistory = pset_new_ptr_default();

	/* walk upwards */
	irg_walk_edges(get_irg_start_block(irg), NULL, walk_sets, multi_env);

	/* from this point multi_ops->ops and multi_env->set_map are irrelevant */

	DBG((dbg, LEVEL_5, "analysing...\n"));

	deq_t queue;
	deq_init(&queue);
	foreach_pset(multi_env->sets, multi_op, o) {
		assert(is_supported_node(o->base_node));
		deq_push_pointer_left(&queue, o);
	}

	deq_foreach_pointer(&queue, multi_op, o) {
		/* Merge sets with only one user */
		if (!o->other_op && pset_count(o->multi_users) == 1) {
			multi_op *user = get_user(o);
			if (get_irn_mode(user->base_node) == get_irn_mode(o->base_node)) {
				if (is_same_op(o->multi_users, get_multi_op_op(o))) {
					merge(multi_env, o, user);
					continue;
				}
				if (is_trivial_multi_op(o)) {
					merge_trivial_multi_op(multi_env, o, get_user(o));
					continue;
				}
			}
		}
	}
	deq_free(&queue);

	rebuild(multi_env);

	pmap_destroy(multi_env->set_map);
	foreach_pset(multi_env->sets, multi_op, o) {
		destroy_multi_op(o);
	}
	del_pset(multi_env->sets);
	del_pset(multi_env->walkhistory);
	free(multi_env);
}

/**
 * This function checks if chaining could be applied and swaps nodes if possible.
 */
static bool walk_chains(ir_node *node)
{
	ir_op *node_op = get_irn_op(node);
	bool   changed = false;

	DBG((dbg, LEVEL_5, "%+F\n", node));

	if (node_op == op_Or || node_op == op_And || node_op == op_Eor) {
		ir_node *a = get_binop_left(node);
		ir_node *b = get_binop_right(node);

		if (get_irn_op(a) == node_op && is_bitop(b) && only_one_user(a)) {
			ir_node *l = get_binop_left(a);
			ir_node *r = get_binop_right(a);

			if (!is_bitop(l)) {
				/* (l .op. r) .op. b -> (b .op. r) .op. l */
				DBG((dbg, LEVEL_4, "(%+F .op. %+F) .op. %+F -> (%+F .op. %+F) .op. %+F\n",
				     l, r, b, b, r, l));

				if (get_nodes_block(l) == get_nodes_block(b)) {
					set_irn_n(node, 1, l);
					set_irn_n(a, 0, b);
					changed = true;
				}
			} else if (!is_bitop(r)) {
				/* (l .op. r) .op. b -> (l .op. b) .op. r */
				DBG((dbg, LEVEL_4, "(%+F .op. %+F) .op. %+F -> (%+F .op. %+F) .op. %+F\n",
				     l, r, b, l, b, r));

				if (get_nodes_block(r) == get_nodes_block(b)) {
					set_irn_n(node, 1, r);
					set_irn_n(a, 1, b);
					changed = true;
				}
			}
		}

		if (!changed && get_irn_op(b) == node_op && is_bitop(a) && only_one_user(b)) {
			ir_node *l = get_binop_left(b);
			ir_node *r = get_binop_right(b);

			if (!is_bitop(l)) {
				/* a .op. (l .op. r) -> l .op. (a .op. r) */
				DBG((dbg, LEVEL_4, "%+F .op. (%+F .op. %+F) -> %+F .op. (%+F .op. %+F)\n",
				     a, l, r, l, a, r));

				if (get_nodes_block(l) == get_nodes_block(a)) {
					set_irn_n(node, 0, l);
					set_irn_n(b, 0, a);
					changed = true;
				}
			} else if (!is_bitop(r)) {
				/* a .op. (l .op. r) -> r .op. (l .op. a) */
				DBG((dbg, LEVEL_4, "%+F .op. (%+F .op. %+F) -> %+F .op. (%+F .op. %+F)\n",
				     a, l, r, r, l, a));

				if (get_nodes_block(r) == get_nodes_block(a)) {
					set_irn_n(node, 0, r);
					set_irn_n(b, 1, a);
					changed = true;
				}
			}
		}
	}

	return changed;
}

/**
 * Applies Chaining to a given irg.
 */
static void do_chaining(ir_graph *irg)
{
	deq_t wq;
	deq_init(&wq);

	/* now we have collected enough information, optimize */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_walk_graph(irg, NULL, wq_walker, &wq);

	while (!deq_empty(&wq)) {
		ir_node *n = deq_pop_pointer_left(ir_node, &wq);
		set_irn_link(n, NULL);

		/* reassociation must run until a fixpoint is reached. */
		bool changed = false;
		bool res;
		do {
			/* reassociating floatingpoint ops is imprecise */
			ir_mode *mode = get_irn_mode(n);
			if (mode_is_float(mode) && !ir_imprecise_float_transforms_allowed())
				break;

			res      = walk_chains(n);
			changed |= res;

		} while (res);

		if (changed) {
			foreach_irn_in_r(n, i, pred) {
				if (get_irn_link(pred) != &wq) {
					deq_push_pointer_right(&wq, pred);
					set_irn_link(pred, &wq);
				}
			}
		}
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	deq_free(&wq);
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
	                      | IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO
	                      | IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	DBG((dbg, LEVEL_5, "chaining start...\n"));
	do_chaining(irg);

	DBG((dbg, LEVEL_5, "shannon start...\n"));
	do_shannon(irg);

	DBG((dbg, LEVEL_5, "Eor equality start...\n"));

	deq_t wq;
	deq_init(&wq);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_walk_edges(get_irg_start_block(irg), wq_walker, NULL, &wq);
	while (!deq_empty(&wq)) {
		ir_node *n = deq_pop_pointer_left(ir_node, &wq);
		set_irn_link(n, NULL);

		walk_equality(n);
	}

	DBG((dbg, LEVEL_5, "setsort start...\n"));
	do_Setsort(irg);

	/* now we have collected enough information, optimize */
	irg_walk_graph(irg, NULL, wq_walker, &wq);
	do_reassociation(&wq);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	/* reverse those rules that do not result in collapsed constants */
	irg_walk_graph(irg, NULL, reverse_rules, NULL);

	deq_free(&wq);

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
