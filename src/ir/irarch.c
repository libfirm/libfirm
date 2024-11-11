/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Machine dependent Firm optimizations.
 * @date    28.9.2004
 * @author  Sebastian Hack, Michael Beck
 *
 * Implements "Strength Reduction of Multiplications by Integer Constants"
 * by Youfeng Wu.
 * Implements Division and Modulo by Consts from "Hackers Delight",
 */
#include "irarch.h"

#include "dbginfo_t.h"
#include "ircons.h"
#include "ircons_t.h"
#include "irflag.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgraph_t.h"
#include "irhooks.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iropt_dbg.h"
#include "iropt_t.h"
#include "irprog_t.h"
#include "irverify.h"
#include "panic.h"
#include "tv_t.h"
#include <assert.h>
#include <stdlib.h>

static ir_settings_arch_dep_t settings;

void ir_arch_lower(ir_settings_arch_dep_t const *const new_settings)
{
	settings = *new_settings;
	foreach_irp_irg(i, irg) {
		optimize_graph_df(irg);
	}
}

/** check, whether a mode allows a Mulh instruction. */
static bool allow_Mulh(ir_mode *mode)
{
	if (get_mode_size_bits(mode) > settings.max_bits_for_mulh)
		return false;
	return mode_is_signed(mode) ? settings.allow_mulhs : settings.allow_mulhu;
}

/**
 * An instruction,
 */
typedef struct instruction instruction;
struct instruction {
	insn_kind    kind;        /**< the instruction kind */
	instruction *in[2];       /**< the ins */
	unsigned     shift_count; /**< shift count for LEA and SHIFT */
	ir_node     *irn;         /**< the generated node for this instruction */
	int          costs;       /**< the costs for this instruction */
};

/**
 * The environment for the strength reduction of multiplications.
 */
typedef struct mul_env {
	struct obstack                obst;
	ir_mode     *mode;     /**< the mode of the multiplication constant */
	unsigned     bits;     /**< number of bits in the mode */
	unsigned     max_S;    /**< the maximum LEA shift value. */
	instruction *root;     /**< the root of the instruction tree */
	ir_node     *op;       /**< the operand that is multiplied */
	ir_node     *blk;      /**< the block where the new graph is built */
	ir_graph    *irg;
	dbg_info    *dbg;      /**< the debug info for the new graph. */
	ir_mode     *shf_mode; /**< the (unsigned) mode for the shift constants */
	bool         fail;     /**< set if the insn sequence fails constraints */
	int          n_shift;  /**< maximum number of allowed shift instructions */

	evaluate_costs_func evaluate;  /**< the evaluate callback */
} mul_env;

/**
 * Some kind of default evaluator. Return the cost of
 * instructions.
 */
static int default_evaluate(insn_kind kind, const ir_mode *mode, ir_tarval *tv)
{
	(void)mode;
	(void)tv;
	if (kind == MUL)
		return 13;
	return 1;
}

/**
 * emit a LEA (or an Add) instruction
 */
static instruction *emit_LEA(mul_env *env, instruction *a, instruction *b,
                             unsigned shift)
{
	instruction *res = OALLOC(&env->obst, instruction);
	res->kind = shift > 0 ? LEA : ADD;
	res->in[0] = a;
	res->in[1] = b;
	res->shift_count = shift;
	res->irn = NULL;
	res->costs = -1;
	return res;
}

/**
 * emit a SHIFT (or an Add or a Zero) instruction
 */
static instruction *emit_SHIFT(mul_env *env, instruction *a, unsigned shift)
{
	instruction *res = OALLOC(&env->obst, instruction);
	if (shift == env->bits) {
		/* a 2^bits with bits resolution is a zero */
		res->kind = ZERO;
		res->in[0] = NULL;
		res->in[1] = NULL;
		res->shift_count = 0;
	} else if (shift != 1) {
		res->kind = SHIFT;
		res->in[0] = a;
		res->in[1] = NULL;
		res->shift_count = shift;
	} else {
		res->kind = ADD;
		res->in[0] = a;
		res->in[1] = a;
		res->shift_count = 0;
	}
	res->irn = NULL;
	res->costs = -1;
	return res;
}

/**
 * emit a SUB instruction
 */
static instruction *emit_SUB(mul_env *env, instruction *a, instruction *b)
{
	instruction *res = OALLOC(&env->obst, instruction);
	res->kind = SUB;
	res->in[0] = a;
	res->in[1] = b;
	res->shift_count = 0;
	res->irn = NULL;
	res->costs = -1;
	return res;
}

/**
 * emit the ROOT instruction
 */
static instruction *emit_ROOT(mul_env *env, ir_node *root_op)
{
	instruction *res = OALLOC(&env->obst, instruction);
	res->kind = ROOT;
	res->in[0] = NULL;
	res->in[1] = NULL;
	res->shift_count = 0;
	res->irn = root_op;
	res->costs = 0;
	return res;
}

/**
 * Returns the condensed representation of the tarval tv
 */
static unsigned char *value_to_condensed(mul_env *env, ir_tarval *tv, int *pr)
{
	ir_mode       *mode = get_tarval_mode(tv);
	unsigned       bits = get_mode_size_bits(mode);
	unsigned char *R    = OALLOCN(&env->obst, unsigned char, bits);

	int r = 0;
	for (unsigned i = 0, l = 0; i < bits; ++i) {
		if (tarval_get_bit(tv, i)) {
			R[r] = i - l;
			l = i;
			++r;
		}
	}

	*pr = r;
	return R;
}

/**
 * Calculate the gain when using the generalized complementary technique
 */
static int calculate_gain(const unsigned char *R, int r)
{
	int max_gain = 0;
	int gain     = 2 - 3 - R[0];
	int idx      = -1;
	for (int i = 2; i < r; ++i) {
		/* calculate the gain for r from the gain for r-1 */
		gain += 2 - R[i - 1];

		if (gain > max_gain) {
			max_gain = gain;
			idx = i;
		}
	}
	return idx;
}

/**
 * Calculates the condensed complement of a given (R,r) tuple
 */
static unsigned char *complement_condensed(mul_env *env, const unsigned char *R,
                                           int gain, int *prs)
{
	unsigned char *value = OALLOCNZ(&env->obst, unsigned char, env->bits);

	int j = 0;
	for (int i = 0; i < gain; ++i) {
		j += R[i];
		value[j] = 1;
	}

	/* negate and propagate 1 */
	unsigned char c = 1;
	for (int i = 0; i <= j; ++i) {
		unsigned char v = !value[i];

		value[i] = v ^ c;
		c = v & c;
	}

	/* condense it again */
	int l = 0;
	int r = 0;
	for (int i = 0; i <= j; ++i) {
		if (value[i] == 1) {
			value[r] = i - l;
			l = i;
			++r;
		}
	}

	*prs = r;
	return value;
}

/**
 * creates a tarval from a condensed representation.
 */
static ir_tarval *condensed_to_value(mul_env *env, const unsigned char *R, int r)
{
	ir_tarval *tv  = get_mode_one(env->mode);
	ir_tarval *res = NULL;
	for (int i = 0; i < r; ++i) {
		int j = R[i];
		if (j != 0)
			tv = tarval_shl_unsigned(tv, j);
		res = res ? tarval_add(res, tv) : tv;
	}
	return res;
}

static instruction *basic_decompose_mul(mul_env *env, unsigned char *R, int r,
                                        ir_tarval *N);

/*
 * handle simple cases with up-to 2 bits set
 */
static instruction *decompose_simple_cases(mul_env *env, unsigned char *R,
                                           int r)
{
	if (r == 1) {
		return emit_SHIFT(env, env->root, R[0]);
	} else {
		assert(r == 2);
		instruction *ins = env->root;
		if (R[1] <= env->max_S) {
			ins = emit_LEA(env, ins, ins, R[1]);
			if (R[0] != 0) {
				ins = emit_SHIFT(env, ins, R[0]);
			}
			return ins;
		}
		if (R[0] != 0) {
			ins = emit_SHIFT(env, ins, R[0]);
		}

		instruction *ins2 = emit_SHIFT(env, env->root, R[0] + R[1]);
		return emit_LEA(env, ins, ins2, 0);
	}
}

/**
 * Main decompose driver.
 */
static instruction *decompose_mul(mul_env *env, unsigned char *R, int r,
                                  ir_tarval *N)
{
	if (r <= 2)
		return decompose_simple_cases(env, R, r);

	if (settings.also_use_subs) {
		int gain = calculate_gain(R, r);
		if (gain > 0) {
			int            r1;
			unsigned char *R1 = complement_condensed(env, R, gain, &r1);
			int            r2 = r - gain + 1;
			unsigned char *R2 = OALLOCN(&env->obst, unsigned char, r2);

			int k = 1;
			for (int i = 0; i < gain; ++i) {
				k += R[i];
			}
			R2[0] = k;
			R2[1] = R[gain] - 1;
			int j = 2;
			if (R2[1] == 0) {
				/* Two identical bits: normalize */
				++R2[0];
				--j;
				--r2;
			}
			for (int i = gain + 1; i < r; ++i) {
				R2[j++] = R[i];
			}

			instruction *instr1 = decompose_mul(env, R1, r1, NULL);
			instruction *instr2 = decompose_mul(env, R2, r2, NULL);
			return emit_SUB(env, instr2, instr1);
		}
	}

	if (N == NULL)
		N = condensed_to_value(env, R, r);

	for (unsigned i = env->max_S; i > 0; --i) {
		ir_tarval *div_res, *mod_res;
		ir_tarval *tv = new_tarval_from_long((1 << i) + 1, env->mode);

		div_res = tarval_divmod(N, tv, &mod_res);
		if (mod_res == get_mode_null(env->mode)) {
			unsigned char *Rs;
			int rs;

			Rs = value_to_condensed(env, div_res, &rs);
			if (rs < r) {
				instruction *N1 = decompose_mul(env, Rs, rs, div_res);
				return emit_LEA(env, N1, N1, i);
			}
		}
	}
	return basic_decompose_mul(env, R, r, N);
}

/**
 * basic decomposition routine
 */
static instruction *basic_decompose_mul(mul_env *env, unsigned char *R, int r,
                                        ir_tarval *N)
{
	if (R[0] == 0) {                    /* Case 1 */
		unsigned t = R[1] > MAX(env->max_S, R[1]);
		R[1] -= t;
		instruction *ns = decompose_mul(env, &R[1], r - 1, N);
		return emit_LEA(env, env->root, ns, t);
	} else if (R[0] <= env->max_S) {    /* Case 2 */
		unsigned t = R[0];
		R[1] += t;
		instruction *ns = decompose_mul(env, &R[1], r - 1, N);
		return emit_LEA(env, ns, env->root, t);
	} else {
		unsigned t = R[0];
		R[0] = 0;
		instruction *ns = decompose_mul(env, R, r, N);
		return emit_SHIFT(env, ns, t);
	}
}

/**
 * Recursively build the graph for the instructions.
 *
 * @param env   the environment
 * @param inst  the instruction
 */
static ir_node *build_graph(mul_env *env, instruction *inst)
{
	if (inst->irn != NULL)
		return inst->irn;

	ir_graph *irg = env->irg;
	switch (inst->kind) {
	case LEA: {
		ir_node *l = build_graph(env, inst->in[0]);
		ir_node *r = build_graph(env, inst->in[1]);
		ir_node *c = new_r_Const_long(irg, env->shf_mode, inst->shift_count);
		ir_node *s = new_rd_Shl(env->dbg, env->blk, r, c);
		return inst->irn = new_rd_Add(env->dbg, env->blk, l, s);
	}
	case SHIFT: {
		ir_node *l = build_graph(env, inst->in[0]);
		ir_node *c = new_r_Const_long(irg, env->shf_mode, inst->shift_count);
		return inst->irn = new_rd_Shl(env->dbg, env->blk, l, c);
	}
	case SUB: {
		ir_node *l = build_graph(env, inst->in[0]);
		ir_node *r = build_graph(env, inst->in[1]);
		return inst->irn = new_rd_Sub(env->dbg, env->blk, l, r);
	}
	case ADD: {
		ir_node *l = build_graph(env, inst->in[0]);
		ir_node *r = build_graph(env, inst->in[1]);
		return inst->irn = new_rd_Add(env->dbg, env->blk, l, r);
	}
	case ZERO:
		return inst->irn = new_r_Const_null(irg, env->mode);
	case ROOT:
	case MUL:
		break;
	}
	panic("unsupported instruction kind");
}

/**
 * Calculate the costs for the given instruction sequence.
 * Note that additional costs due to higher register pressure are NOT
 * evaluated yet
 */
static int evaluate_insn(mul_env *env, instruction *inst)
{
	if (inst->costs >= 0) {
		/* was already evaluated */
		return 0;
	}

	switch (inst->kind) {
	case LEA:
	case SUB:
	case ADD: {
		int costs = evaluate_insn(env, inst->in[0])
		          + evaluate_insn(env, inst->in[1])
		          + env->evaluate(inst->kind, env->mode, NULL);
		inst->costs = costs;
		return costs;
	}
	case SHIFT:
		if (inst->shift_count > settings.highest_shift_amount)
			env->fail = true;
		if (env->n_shift <= 0)
			env->fail = true;
		else
			--env->n_shift;
		int costs = evaluate_insn(env, inst->in[0])
		          + env->evaluate(inst->kind, env->mode, NULL);
		inst->costs = costs;
		return costs;
	case ZERO: {
		int costs = env->evaluate(inst->kind, env->mode, NULL);
		inst->costs = costs;
		return costs;
	}
	case MUL:
	case ROOT:
		break;
	}
	panic("unsupported instruction kind");
}

/**
 * Evaluate the replacement instructions and build a new graph
 * if faster than the Mul.
 * Returns the root of the new graph then or irn otherwise.
 *
 * @param irn      the Mul operation
 * @param operand  the multiplication operand
 * @param tv       the multiplication constant
 *
 * @return the new graph
 */
static ir_node *do_decomposition(ir_node *irn, ir_node *operand, ir_tarval *tv)
{
	mul_env env;
	obstack_init(&env.obst);
	env.mode     = get_tarval_mode(tv);
	env.bits     = (unsigned)get_mode_size_bits(env.mode);
	env.max_S    = 3;
	env.root     = emit_ROOT(&env, operand);
	env.fail     = false;
	env.n_shift  = settings.maximum_shifts;
	env.evaluate = settings.evaluate != NULL ? settings.evaluate
	                                         : default_evaluate;
	env.irg      = get_irn_irg(irn);

	int            r;
	unsigned char *R    = value_to_condensed(&env, tv, &r);
	instruction   *inst = decompose_mul(&env, R, r, tv);

	/* the paper suggests 70% here */
	int      mul_costs = (env.evaluate(MUL, env.mode, tv) * 7 + 5) / 10;
	ir_node *res       = irn;
	if (evaluate_insn(&env, inst) <= mul_costs && !env.fail) {
		env.op       = operand;
		env.blk      = get_nodes_block(irn);
		env.dbg      = get_irn_dbg_info(irn);
		env.shf_mode = find_unsigned_mode(env.mode);
		if (env.shf_mode == NULL)
			env.shf_mode = mode_Iu;

		res = build_graph(&env, inst);
	}
	obstack_free(&env.obst, NULL);
	return res;
}

/* Replace Muls with Shifts and Add/Subs. */
ir_node *arch_dep_replace_mul_with_shifts(ir_node *irn)
{
	/* If the architecture dependent optimizations were not initialized
	   or this optimization was not enabled. */
	if (!settings.replace_muls)
		return irn;

	assert(is_Mul(irn));
	ir_mode *mode = get_irn_mode(irn);
	if (!mode_is_int(mode))
		return irn;

	/* we should never do the reverse transformations again
	   (like x+x -> 2*x) */
	ir_graph *irg = get_irn_irg(irn);
	add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_ARCH_DEP);

	/* Look, if one operand is a constant. */
	ir_node   *left    = get_binop_left(irn);
	ir_node   *right   = get_binop_right(irn);
	ir_tarval *tv      = NULL;
	ir_node   *operand = NULL;
	if (is_Const(left)) {
		tv = get_Const_tarval(left);
		operand = right;
	} else if (is_Const(right)) {
		tv = get_Const_tarval(right);
		operand = left;
	}

	/* multiplications with 0 are a special case which we leave for
	 * equivalent_node_Mul because the code here can't handle them */
	if (tv == get_mode_null(mode))
		return irn;

	ir_node *res = irn;
	if (tv != NULL) {
		res = do_decomposition(irn, operand, tv);
		if (res != irn) {
			exchange(irn, res);
		}
	}

	return res;
}

/**
 * calculated the ld2 of a tarval if tarval is 2^n, else returns -1.
 */
static int tv_ld2(ir_tarval *tv, int bits)
{
	int num = 0;
	int k   = 0;
	for (int i = 0; i < bits; ++i) {
		unsigned char v = get_tarval_sub_bits(tv, i);
		if (v == 0)
			continue;
		for (int j = 0; j < 8; ++j) {
			if ((1 << j) & v) {
				++num;
				k = 8 * i + j;
			}
		}
	}
	if (num == 1)
		return k;
	return -1;
}


/* for shorter lines */
#define ABS(a)    tarval_abs(a)
#define NEG(a)    tarval_neg(a)
#define NOT(a)    tarval_not(a)
#define SHL(a, b) tarval_shl_unsigned(a, b)
#define SHR(a, b) tarval_shr_unsigned(a, b)
#define ADD(a, b) tarval_add(a, b)
#define SUB(a, b) tarval_sub(a, b)
#define MUL(a, b) tarval_mul(a, b)
#define DIV(a, b) tarval_div(a, b)
#define MOD(a, b) tarval_mod(a, b)
#define CMP(a, b) tarval_cmp(a, b)
#define CNV(a, m) tarval_convert_to(a, m)
#define ONE(m)    get_mode_one(m)
#define ZERO(m)   get_mode_null(m)
#define AND(a, b) tarval_and(a, b)

/** The result of a the magic() function. */
struct ms {
	ir_tarval *M;        /**< magic number */
	int        s;        /**< shift amount */
	bool       need_add; /**< an additional add is needed */
	bool       need_sub; /**< an additional sub is needed */
};

/**
 * Signed division by constant d: calculate the Magic multiplier M and the
 * shift amount s
 *
 * see Hacker's Delight: 10-6 Integer Division by Constants: Incorporation
 * into a Compiler
 */
static struct ms magic(ir_tarval *d)
{
	/* we need overflow mode to work correctly */
	assert(tarval_get_wrap_on_overflow());

	ir_mode *mode   = get_tarval_mode(d);
	ir_mode *u_mode = find_unsigned_mode(mode);
	unsigned bits   = get_mode_size_bits(u_mode);

	/* 2^(bits-1) */
	ir_tarval *two_bits_1 = SHL(get_mode_one(u_mode), bits-1);

	ir_tarval *ad  = CNV(ABS(d), u_mode);
	ir_tarval *t   = ADD(two_bits_1, SHR(CNV(d, u_mode), bits-1));
	ir_tarval *anc = SUB(SUB(t, ONE(u_mode)), MOD(t, ad)); /* abs(nc) */
	int        p   = bits - 1;
	ir_tarval *q1  = DIV(two_bits_1, anc);          /* q1 = 2^p/|nc| */
	ir_tarval *r1  = SUB(two_bits_1, MUL(q1, anc)); /* r1 = rem(2^p, |nc|) */
	ir_tarval *q2  = DIV(two_bits_1, ad);           /* q2 = 2^p/|d| */
	ir_tarval *r2  = SUB(two_bits_1, MUL(q2, ad));  /* r2 = rem(2^p, |d|) */

	ir_tarval *delta;
	do {
		++p;
		q1 = ADD(q1, q1);                      /* Update q1 = 2^p/|nc| */
		r1 = ADD(r1, r1);                      /* Update r1 = rem(2^p, |nc|) */

		if (CMP(r1, anc) & ir_relation_greater_equal) {
			q1 = ADD(q1, ONE(u_mode));
			r1 = SUB(r1, anc);
		}

		q2 = ADD(q2, q2);                      /* Update q2 = 2^p/|d| */
		r2 = ADD(r2, r2);                      /* Update r2 = rem(2^p, |d|) */

		if (CMP(r2, ad) & ir_relation_greater_equal) {
			q2 = ADD(q2, ONE(u_mode));
			r2 = SUB(r2, ad);
		}

		delta = SUB(ad, r2);
	} while (CMP(q1, delta) & ir_relation_less
	         || (CMP(q1, delta) & ir_relation_equal
	             && CMP(r1, ZERO(u_mode)) & ir_relation_equal));

	ir_relation d_cmp = CMP(d, ZERO(mode));

	struct ms mag;
	if (d_cmp & ir_relation_greater_equal)
		mag.M = ADD(CNV(q2, mode), ONE(mode));
	else
		mag.M = SUB(ZERO(mode), ADD(CNV(q2, mode), ONE(mode)));

	ir_relation M_cmp = CMP(mag.M, ZERO(mode));

	mag.s = p - bits;

	/* need an add if d > 0 && M < 0 */
	mag.need_add = d_cmp & ir_relation_greater && M_cmp & ir_relation_less;

	/* need a sub if d < 0 && M > 0 */
	mag.need_sub = d_cmp & ir_relation_less && M_cmp & ir_relation_greater;

	return mag;
}

/**
 * Unsigned division by constant d: calculate the Magic multiplier M and the
 * shift amount s
 *
 * see Faster Unsigned Division by Constants
 *  (http://ridiculousfish.com/blog/posts/labor-of-division-episode-iii.html)
 */
struct magicu_info
{
	ir_tarval *multiplier; /* the "magic number" multiplier */
	unsigned   pre_shift;  /* shift for the dividend before multiplying */
	unsigned   post_shift; /* shift for the dividend after multiplying */
	unsigned   increment;  /* 0 or 1; if set then increment the numerator,
	                          using one of the two strategies */
};

static struct magicu_info compute_unsigned_magic_info(ir_tarval *divisor,
                                                      unsigned num_bits)
{
	ir_mode *mode = get_tarval_mode(divisor);

	/* divisor must be larger than zero and not a power of 2
	 * D & (D-1) > 0 */
	assert(get_tarval_long(AND(divisor, SUB(divisor, ONE(mode)))));

	/* Bits in ir_tarval */
	const unsigned UINT_BITS = get_mode_size_bits(mode);

	/* The extra shift implicit in the difference between UINT_BITS and num_bits */
	const unsigned extra_shift = UINT_BITS - num_bits;

	/* The initial power of 2 is one less than the first one that can possibly work */
	ir_tarval *initial_power_of_2 = SHL(ONE(mode), UINT_BITS - 1);

	/* The remainder and quotient of our power of 2 divided by divisor */
	ir_tarval *quotient  = DIV(initial_power_of_2, divisor);
	ir_tarval *remainder = MOD(initial_power_of_2, divisor);

	/* The magic info for the variant "round down" algorithm */
	ir_tarval *down_multiplier = ZERO(mode);
	unsigned   down_exponent   = 0;
	int        has_magic_down  = 0;

	/* Compute ceil(log_2 D) */
	unsigned ceil_log_2_D = 0;
	for (ir_tarval *tmp = divisor; CMP(tmp, ZERO(mode)) & ir_relation_greater; tmp = SHR(tmp, 1))
		ceil_log_2_D++;

	/* Begin a loop that increments the exponent, until we find a power of 2
	 * that works. */
	unsigned exponent = 0;
	for (;; ++exponent) {
		/* Quotient and remainder is from previous exponent; compute it for
		 * this exponent. */
		ir_tarval *two = new_tarval_from_long(2, mode);
		if (CMP(remainder, SUB(divisor, remainder)) & ir_relation_greater_equal) {
			/* Doubling remainder will wrap around divisor */
			quotient  = ADD(MUL(quotient, two), ONE(mode));
			remainder = SUB(MUL(remainder, two), divisor);
		} else {
			/* Remainder will not wrap */
			quotient  = MUL(quotient, two);
			remainder = MUL(remainder, two);
		}

		/* We are done if this exponent works for the round_up algorithm.
		 * Note that exponent may be larger than the maximum shift supported,
		 * so the check for >= ceil_log_2_D is critical. */
		if ((exponent + extra_shift >= ceil_log_2_D) ||
		    /* (divisor - remainder) <= (1 << exponent + extra_shift) */
		    (CMP(SUB(divisor, remainder), SHL(ONE(mode), exponent + extra_shift)) & ir_relation_less_equal))
			break;

		/* Set magic_down if we have not set it yet and this exponent works for
		 * the round_down algorithm */
		if (!has_magic_down &&
		    (CMP(remainder, SHL(ONE(mode), exponent + extra_shift)) &
		    ir_relation_less_equal)) {
			has_magic_down  = 1;
			down_multiplier = quotient;
			down_exponent   = exponent;
		}
	}

	struct magicu_info result;
	if (exponent < ceil_log_2_D) {
		/* magic_up is efficient */
		result.multiplier = ADD(quotient, ONE(mode));
		result.pre_shift  = 0;
		result.post_shift = exponent;
		result.increment  = 0;
	} else if (CMP(AND(divisor, ONE(mode)), ZERO(mode)) & ir_relation_greater) {
		/* Odd divisor, so use magic_down, which must have been set */
		assert(has_magic_down);
		result.multiplier = down_multiplier;
		result.pre_shift  = 0;
		result.post_shift = down_exponent;
		result.increment  = 1;
	} else {
		/* Even divisor, so use a prefix-shifted dividend */
		unsigned pre_shift   = 0;
		ir_tarval *shifted_D = divisor;
		while (CMP(AND(shifted_D, ONE(mode)), ZERO(mode)) & ir_relation_equal) {
			shifted_D  = SHR(shifted_D, 1);
			pre_shift += 1;
		}
		result = compute_unsigned_magic_info(shifted_D, num_bits - pre_shift);
		/* expect no increment or pre_shift in this path */
		assert(result.increment == 0 && result.pre_shift == 0);
		result.pre_shift = pre_shift;
	}
	return result;
}

static struct magicu_info get_magic_info(ir_tarval *d)
{
	unsigned num_bits = get_mode_size_bits(get_tarval_mode(d));
	return compute_unsigned_magic_info(d, num_bits);
}

/**
 * Build the Mulh replacement code for n / tv.
 *
 * Note that 'div' might be a Mod operation as well
 */
static ir_node *replace_div_by_mulh(ir_node *div, ir_tarval *tv)
{
	/* Beware: do not transform bad code */
	ir_node *n     = get_binop_left(div);
	ir_node *block = get_nodes_block(div);
	if (is_Bad(n) || is_Bad(block))
		return div;

	dbg_info *dbg   = get_irn_dbg_info(div);
	ir_mode  *mode  = get_irn_mode(n);
	int       bits  = get_mode_size_bits(mode);
	ir_node  *res;
	if (mode_is_signed(mode)) {
		ir_graph *irg = get_irn_irg(div);
		struct ms mag = magic(tv);

		/* generate the Mulh instruction */
		ir_node *c = new_r_Const(irg, mag.M);
		ir_node *q = new_rd_Mulh(dbg, block, n, c);

		/* do we need an Add or Sub */
		if (mag.need_add)
			q = new_rd_Add(dbg, block, q, n);
		else if (mag.need_sub)
			q = new_rd_Sub(dbg, block, q, n);

		/* Do we need the shift */
		if (mag.s > 0) {
			c = new_r_Const_long(irg, mode_Iu, mag.s);
			q = new_rd_Shrs(dbg, block, q, c);
		}

		/* final */
		ir_node *c2 = new_r_Const_long(irg, mode_Iu, bits - 1);
		ir_node *t  = new_rd_Shr(dbg, block, q, c2);
		res = new_rd_Add(dbg, block, q, t);
	} else {
		struct magicu_info mafo = get_magic_info(tv);
		ir_graph          *irg  = get_irn_irg(div);

		if (mafo.pre_shift > 0) {
			ir_node *c = new_r_Const_long(irg, mode_Iu, mafo.pre_shift);
			n = new_rd_Shr(dbg, block, n, c);
		}

		if (mafo.increment) {
			ir_node *no_mem    = new_rd_NoMem(dbg, irg);
			ir_node *in[1]     = {n};
			ir_type *utype     = get_unknown_type();
			ir_node *increment = new_rd_Builtin(dbg, block, no_mem, 1, in,
			                                    ir_bk_saturating_increment, utype);

			n = new_r_Proj(increment, mode_Iu, pn_Builtin_max + 1);
		}

		/* generate the Mulh instruction */
		ir_node *c = new_r_Const(irg, mafo.multiplier);
		ir_node *q = new_rd_Mulh(dbg, block, n, c);
		c = new_r_Const_long(irg, mode_Iu, get_mode_size_bits(get_tarval_mode(tv)));
		res = new_rd_Shr(dbg, block, q, c);
		if (mafo.post_shift > 0) {
			c = new_r_Const_long(irg, mode_Iu, mafo.post_shift);
			res = new_rd_Shr(dbg, block, res, c);
		}
	}
	return res;
}

/* Replace Divs with Shifts and Add/Subs and Mulh. */
ir_node *arch_dep_replace_div_by_const(ir_node *irn)
{
	/* If the architecture dependent optimizations were not initialized
		or this optimization was not enabled. */
	if (!settings.replace_divs)
		return irn;
	if (!is_Div(irn))
		return irn;

	ir_node *c = get_Div_right(irn);
	if (!is_Const(c))
		return irn;

	/* check for division by zero */
	ir_tarval *tv = get_Const_tarval(c);
	if (tarval_is_null(tv))
		return irn;

	/* can only handle integer Div's */
	ir_node *left = get_Div_left(irn);
	ir_mode *mode = get_irn_mode(left);
	if (!mode_is_int(mode))
		return irn;

	ir_node  *block  = get_nodes_block(irn);
	dbg_info *dbg    = get_irn_dbg_info(irn);
	int       bits   = get_mode_size_bits(mode);
	int       n      = (bits + 7) / 8;
	int       k      = -1;
	bool      n_flag = false;
	if (mode_is_signed(mode)) {
		/* for signed divisions, the algorithm works for a / -2^k by negating
		 * the result */
		ir_tarval *ntv = tarval_neg(tv);
		n_flag = true;
		k = tv_ld2(ntv, n);
	}
	if (k < 0) {
		n_flag = false;
		k = tv_ld2(tv, n);
	}

	ir_node *res = irn;
	if (k > 0) { /* division by 2^k or -2^k */
		ir_graph *irg = get_irn_irg(irn);
		if (mode_is_signed(mode)) {
			ir_node *curr = left;

			/* create the correction code for signed values only if there might be a remainder */
			if (!get_Div_no_remainder(irn)) {
				if (k != 1) {
					ir_node *k_node = new_r_Const_long(irg, mode_Iu, k - 1);
					curr   = new_rd_Shrs(dbg, block, left, k_node);
				}

				ir_node *k_node = new_r_Const_long(irg, mode_Iu, bits - k);
				curr = new_rd_Shr(dbg, block, curr, k_node);
				/* curr is now 2^(k-1) in case left <  0
				 *          or       0 in case left >= 0
				 *
				 * For an example, where this fixup is necessary consider -3/2,
				 * which should compute to -1,
				 * but simply shifting right by one computes -2.
				 */

				curr = new_rd_Add(dbg, block, left, curr);
			}

			ir_node *k_node = new_r_Const_long(irg, mode_Iu, k);
			res = new_rd_Shrs(dbg, block, curr, k_node);

			if (n_flag) { /* negate the result */
				ir_node *k_node = new_r_Const_null(irg, mode);
				res = new_rd_Sub(dbg, block, k_node, res);
			}
		} else {      /* unsigned case */
			ir_node *k_node = new_r_Const_long(irg, mode_Iu, k);
			res    = new_rd_Shr(dbg, block, left, k_node);
		}
	} else if (k != 0) {
		/* other constant */
		if (allow_Mulh(mode))
			res = replace_div_by_mulh(irn, tv);
	} else { /* k == 0  i.e. division by 1 */
		res = left;
	}

	return res;
}

/* Replace Mods with Shifts and Add/Subs and Mulh. */
ir_node *arch_dep_replace_mod_by_const(ir_node *irn)
{
	/* If the architecture dependent optimizations were not initialized
	   or this optimization was not enabled. */
	if (!settings.replace_mods)
		return irn;
	if (!is_Mod(irn))
		return irn;

	ir_node *c = get_Mod_right(irn);
	if (!is_Const(c))
		return irn;

	/* check for division by zero */
	ir_tarval *tv = get_Const_tarval(c);
	if (tarval_is_null(tv))
		return irn;

	ir_node  *left  = get_Mod_left(irn);
	ir_mode  *mode  = get_irn_mode(left);
	ir_node  *block = get_nodes_block(irn);
	dbg_info *dbg   = get_irn_dbg_info(irn);
	int       bits  = get_mode_size_bits(mode);
	int       n     = (bits + 7) / 8;
	int       k     = -1;
	if (mode_is_signed(mode)) {
		/* for signed divisions, the algorithm works for a / -2^k by
		 * negating the result */
		ir_tarval *ntv = tarval_neg(tv);
		k = tv_ld2(ntv, n);
	}

	if (k < 0) {
		k = tv_ld2(tv, n);
	}

	/* k == 0  i.e. modulo by 1 */
	ir_node *res = irn;
	if (k == 0) {
		ir_graph *irg = get_irn_irg(irn);
		res = new_r_Const_null(irg, mode);
	} else if (k > 0) {
		ir_graph *irg = get_irn_irg(irn);
		/* division by 2^k or -2^k:
		 * we use "modulus" here, so x % y == x % -y that's why is no
		 * difference between the case 2^k and -2^k */
		if (mode_is_signed(mode)) {
			ir_node *curr = left;
			if (k != 1) {
				ir_node *c = new_r_Const_long(irg, mode_Iu, k - 1);
				curr = new_rd_Shrs(dbg, block, left, c);
			}

			ir_node *k_node = new_r_Const_long(irg, mode_Iu, bits - k);
			curr = new_rd_Shr(dbg, block, curr, k_node);
			curr = new_rd_Add(dbg, block, left, curr);

			ir_tarval *k_val
				= tarval_shl_unsigned(get_mode_all_one(mode), k);
			k_node = new_r_Const(irg, k_val);
			curr   = new_rd_And(dbg, block, curr, k_node);
			res    = new_rd_Sub(dbg, block, left, curr);
		} else {      /* unsigned case */
			ir_tarval *k_val
				= tarval_shr_unsigned(get_mode_all_one(mode),
				                      get_mode_size_bits(mode) - k);
			ir_node *k_node = new_r_Const(irg, k_val);
			res = new_rd_And(dbg, block, left, k_node);
		}
	/* other constant */
	} else if (allow_Mulh(mode)) {
		res = replace_div_by_mulh(irn, tv);
		res = new_rd_Mul(dbg, block, res, c);
		res = new_rd_Sub(dbg, block, left, res);
	}

	return res;
}
