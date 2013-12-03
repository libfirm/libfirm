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
#include <stdlib.h>
#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "iropt_t.h"
#include "ircons_t.h"
#include "irgmod.h"
#include "irverify.h"
#include "tv_t.h"
#include "dbginfo_t.h"
#include "iropt_dbg.h"
#include "irflag_t.h"
#include "irhooks.h"
#include "ircons.h"
#include "irarch.h"
#include "irflag.h"
#include "be.h"
#include "error.h"

/** The bit mask, which optimizations to apply. */
static arch_dep_opts_t opts;

void arch_dep_set_opts(arch_dep_opts_t the_opts)
{
	opts = the_opts;
}

/** check, whether a mode allows a Mulh instruction. */
static int allow_Mulh(const ir_settings_arch_dep_t *params, ir_mode *mode)
{
	if (get_mode_size_bits(mode) > params->max_bits_for_mulh)
		return 0;
	return (mode_is_signed(mode) && params->allow_mulhs) || (!mode_is_signed(mode) && params->allow_mulhu);
}

/**
 * An instruction,
 */
typedef struct instruction instruction;
struct instruction {
	insn_kind   kind;        /**< the instruction kind */
	instruction *in[2];      /**< the ins */
	unsigned    shift_count; /**< shift count for LEA and SHIFT */
	ir_node     *irn;        /**< the generated node for this instruction if any. */
	int         costs;       /**< the costs for this instruction */
};

/**
 * The environment for the strength reduction of multiplications.
 */
typedef struct mul_env {
	struct obstack obst;       /**< an obstack for local space. */
	const ir_settings_arch_dep_t *params;
	ir_mode        *mode;      /**< the mode of the multiplication constant */
	unsigned       bits;       /**< number of bits in the mode */
	unsigned       max_S;      /**< the maximum LEA shift value. */
	instruction    *root;      /**< the root of the instruction tree */
	ir_node        *op;        /**< the operand that is multiplied */
	ir_node        *blk;       /**< the block where the new graph is built */
	ir_graph       *irg;
	dbg_info       *dbg;       /**< the debug info for the new graph. */
	ir_mode        *shf_mode;  /**< the (unsigned) mode for the shift constants */
	int            fail;       /**< set to 1 if the instruction sequence fails the constraints */
	int            n_shift;    /**< maximum number of allowed shift instructions */

	evaluate_costs_func evaluate;  /**< the evaluate callback */
} mul_env;

/**
 * Some kind of default evaluator. Return the cost of
 * instructions.
 */
static int default_evaluate(insn_kind kind, const ir_mode *mode, ir_tarval *tv)
{
	(void) mode;
	(void) tv;

	if (kind == MUL)
		return 13;
	return 1;
}

/**
 * emit a LEA (or an Add) instruction
 */
static instruction *emit_LEA(mul_env *env, instruction *a, instruction *b, unsigned shift)
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
	ir_mode *mode = get_tarval_mode(tv);
	int     bits = get_mode_size_bits(mode);
	char    *bitstr = get_tarval_bitpattern(tv);
	int     i, l, r;
	unsigned char *R = (unsigned char*)obstack_alloc(&env->obst, bits);

	l = r = 0;
	for (i = 0; bitstr[i] != '\0'; ++i) {
		if (bitstr[i] == '1') {
			R[r] = i - l;
			l = i;
			++r;
		}
	}
	free(bitstr);

	*pr = r;
	return R;
}

/**
 * Calculate the gain when using the generalized complementary technique
 */
static int calculate_gain(unsigned char *R, int r)
{
	int max_gain = 0;
	int idx = -1, i;
	int gain;

	/* the gain for r == 1 */
	gain = 2 - 3 - R[0];
	for (i = 2; i < r; ++i) {
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
static unsigned char *complement_condensed(mul_env *env, unsigned char *R, int r, int gain, int *prs)
{
	unsigned char *value = (unsigned char*)obstack_alloc(&env->obst, env->bits);
	int i, l, j;
	unsigned char c;

	memset(value, 0, env->bits);

	j = 0;
	for (i = 0; i < gain; ++i) {
		j += R[i];
		value[j] = 1;
	}

	/* negate and propagate 1 */
	c = 1;
	for (i = 0; i <= j; ++i) {
		unsigned char v = !value[i];

		value[i] = v ^ c;
		c = v & c;
	}

	/* condense it again */
	l = r = 0;
	R = value;
	for (i = 0; i <= j; ++i) {
		if (value[i] == 1) {
			R[r] = i - l;
			l = i;
			++r;
		}
	}

	*prs = r;
	return R;
}

/**
 * creates a tarval from a condensed representation.
 */
static ir_tarval *condensed_to_value(mul_env *env, unsigned char *R, int r)
{
	ir_tarval *tv  = get_mode_one(env->mode);
	ir_tarval *res = NULL;
	for (int i = 0; i < r; ++i) {
		int j = R[i];
		if (j)
			tv = tarval_shl_unsigned(tv, j);
		res = res ? tarval_add(res, tv) : tv;
	}
	return res;
}

/* forward */
static instruction *basic_decompose_mul(mul_env *env, unsigned char *R, int r, ir_tarval *N);

/*
 * handle simple cases with up-to 2 bits set
 */
static instruction *decompose_simple_cases(mul_env *env, unsigned char *R, int r, ir_tarval *N)
{
	instruction *ins, *ins2;

	(void) N;
	if (r == 1) {
		return emit_SHIFT(env, env->root, R[0]);
	} else {
		assert(r == 2);

		ins = env->root;
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

		ins2 = emit_SHIFT(env, env->root, R[0] + R[1]);
		return emit_LEA(env, ins, ins2, 0);
	}
}

/**
 * Main decompose driver.
 */
static instruction *decompose_mul(mul_env *env, unsigned char *R, int r, ir_tarval *N)
{
	unsigned i;
	int gain;

	if (r <= 2)
		return decompose_simple_cases(env, R, r, N);

	if (env->params->also_use_subs) {
		gain = calculate_gain(R, r);
		if (gain > 0) {
			instruction *instr1, *instr2;
			unsigned char *R1, *R2;
			int r1, r2, i, k, j;

			R1 = complement_condensed(env, R, r, gain, &r1);
			r2 = r - gain + 1;
			R2 = (unsigned char*)obstack_alloc(&env->obst, r2);

			k = 1;
			for (i = 0; i < gain; ++i) {
				k += R[i];
			}
			R2[0] = k;
			R2[1] = R[gain] - 1;
			j = 2;
			if (R2[1] == 0) {
				/* Two identical bits: normalize */
				++R2[0];
				--j;
				--r2;
			}
			for (i = gain + 1; i < r; ++i) {
				R2[j++] = R[i];
			}

			instr1 = decompose_mul(env, R1, r1, NULL);
			instr2 = decompose_mul(env, R2, r2, NULL);
			return emit_SUB(env, instr2, instr1);
		}
	}

	if (N == NULL)
		N = condensed_to_value(env, R, r);

	for (i = env->max_S; i > 0; --i) {
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

#define IMAX(a,b) ((a) > (b) ? (a) : (b))

/**
 * basic decomposition routine
 */
static instruction *basic_decompose_mul(mul_env *env, unsigned char *R, int r, ir_tarval *N)
{
	instruction *Ns;
	unsigned t;

	if (R[0] == 0) {                    /* Case 1 */
		t = R[1] > IMAX(env->max_S, R[1]);
		R[1] -= t;
		Ns = decompose_mul(env, &R[1], r - 1, N);
		return emit_LEA(env, env->root, Ns, t);
	} else if (R[0] <= env->max_S) {    /* Case 2 */
		t = R[0];
		R[1] += t;
		Ns = decompose_mul(env, &R[1], r - 1, N);
		return emit_LEA(env, Ns, env->root, t);
	} else {
		t = R[0];
		R[0] = 0;
		Ns = decompose_mul(env, R, r, N);
		return emit_SHIFT(env, Ns, t);
	}
}

/**
 * recursive build the graph form the instructions.
 *
 * @param env   the environment
 * @param inst  the instruction
 */
static ir_node *build_graph(mul_env *env, instruction *inst)
{
	ir_node *l, *r, *c;
	ir_graph *irg = env->irg;

	if (inst->irn)
		return inst->irn;

	switch (inst->kind) {
	case LEA:
		l = build_graph(env, inst->in[0]);
		r = build_graph(env, inst->in[1]);
		c = new_r_Const_long(irg, env->shf_mode, inst->shift_count);
		r = new_rd_Shl(env->dbg, env->blk, r, c, env->mode);
		return inst->irn = new_rd_Add(env->dbg, env->blk, l, r, env->mode);
	case SHIFT:
		l = build_graph(env, inst->in[0]);
		c = new_r_Const_long(irg, env->shf_mode, inst->shift_count);
		return inst->irn = new_rd_Shl(env->dbg, env->blk, l, c, env->mode);
	case SUB:
		l = build_graph(env, inst->in[0]);
		r = build_graph(env, inst->in[1]);
		return inst->irn = new_rd_Sub(env->dbg, env->blk, l, r, env->mode);
	case ADD:
		l = build_graph(env, inst->in[0]);
		r = build_graph(env, inst->in[1]);
		return inst->irn = new_rd_Add(env->dbg, env->blk, l, r, env->mode);
	case ZERO:
		return inst->irn = new_r_Const(irg, get_mode_null(env->mode));
	default:
		panic("Unsupported instruction kind");
	}
}

/**
 * Calculate the costs for the given instruction sequence.
 * Note that additional costs due to higher register pressure are NOT evaluated yet
 */
static int evaluate_insn(mul_env *env, instruction *inst)
{
	int costs;

	if (inst->costs >= 0) {
		/* was already evaluated */
		return 0;
	}

	switch (inst->kind) {
	case LEA:
	case SUB:
	case ADD:
		costs  = evaluate_insn(env, inst->in[0]);
		costs += evaluate_insn(env, inst->in[1]);
		costs += env->evaluate(inst->kind, env->mode, NULL);
		inst->costs = costs;
		return costs;
	case SHIFT:
		if (inst->shift_count > env->params->highest_shift_amount)
			env->fail = 1;
		if (env->n_shift <= 0)
			env->fail = 1;
		else
			--env->n_shift;
		costs  = evaluate_insn(env, inst->in[0]);
		costs += env->evaluate(inst->kind, env->mode, NULL);
		inst->costs = costs;
		return costs;
	case ZERO:
		inst->costs = costs = env->evaluate(inst->kind, env->mode, NULL);
		return costs;
	case MUL:
	case ROOT:
		break;
	}
	panic("Unsupported instruction kind");
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
	mul_env       env;
	instruction   *inst;
	unsigned char *R;
	int           r;
	ir_node       *res = irn;
	int           mul_costs;

	obstack_init(&env.obst);
	env.params   = be_get_backend_param()->dep_param;
	env.mode     = get_tarval_mode(tv);
	env.bits     = (unsigned)get_mode_size_bits(env.mode);
	env.max_S    = 3;
	env.root     = emit_ROOT(&env, operand);
	env.fail     = 0;
	env.n_shift  = env.params->maximum_shifts;
	env.evaluate = env.params->evaluate != NULL ? env.params->evaluate : default_evaluate;
	env.irg      = get_irn_irg(irn);

	R = value_to_condensed(&env, tv, &r);
	inst = decompose_mul(&env, R, r, tv);

	/* the paper suggests 70% here */
	mul_costs = (env.evaluate(MUL, env.mode, tv) * 7 + 5) / 10;
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
	ir_node   *res  = irn;
	ir_mode   *mode = get_irn_mode(irn);
	ir_graph  *irg;
	ir_node   *left;
	ir_node   *right;
	ir_node   *operand;
	ir_tarval *tv;
	const ir_settings_arch_dep_t *params = be_get_backend_param()->dep_param;

	/* If the architecture dependent optimizations were not initialized
	   or this optimization was not enabled. */
	if (params == NULL || (opts & arch_dep_mul_to_shift) == 0)
		return res;

	assert(is_Mul(irn));
	if (!mode_is_int(mode))
		return res;

	/* we should never do the reverse transformations again
	   (like x+x -> 2*x) */
	irg = get_irn_irg(irn);
	add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_ARCH_DEP);

	left    = get_binop_left(irn);
	right   = get_binop_right(irn);
	tv      = NULL;
	operand = NULL;

	/* Look, if one operand is a constant. */
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
		return res;

	if (tv != NULL) {
		res = do_decomposition(irn, operand, tv);

		if (res != irn) {
			hook_arch_dep_replace_mul_with_shifts(irn);
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
	int i, k = 0, num;

	for (num = i = 0; i < bits; ++i) {
		unsigned char v = get_tarval_sub_bits(tv, i);

		if (v) {
			int j;

			for (j = 0; j < 8; ++j)
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
#define SUB(a, b) tarval_sub(a, b, NULL)
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
	ir_tarval *M;     /**< magic number */
	int s;            /**< shift amount */
	int need_add;     /**< an additional add is needed */
	int need_sub;     /**< an additional sub is needed */
};

/**
 * Signed division by constant d: calculate the Magic multiplier M and the shift amount s
 *
 * see Hacker's Delight: 10-6 Integer Division by Constants: Incorporation into a Compiler
 */
static struct ms magic(ir_tarval *d)
{
	ir_mode *mode   = get_tarval_mode(d);
	ir_mode *u_mode = find_unsigned_mode(mode);
	int bits        = get_mode_size_bits(u_mode);
	int p;
	ir_tarval *ad, *anc, *delta, *q1, *r1, *q2, *r2, *t;     /* unsigned */
	ir_relation d_cmp, M_cmp;

	ir_tarval *two_bits_1;

	struct ms mag;

	tarval_int_overflow_mode_t rem = tarval_get_integer_overflow_mode();

	/* we need overflow mode to work correctly */
	tarval_set_integer_overflow_mode(TV_OVERFLOW_WRAP);

	/* 2^(bits-1) */
	two_bits_1 = SHL(get_mode_one(u_mode), bits-1);

	ad  = CNV(ABS(d), u_mode);
	t   = ADD(two_bits_1, SHR(CNV(d, u_mode), bits-1));
	anc = SUB(SUB(t, ONE(u_mode)), MOD(t, ad));   /* Absolute value of nc */
	p   = bits - 1;                               /* Init: p */
	q1  = DIV(two_bits_1, anc);                   /* Init: q1 = 2^p/|nc| */
	r1  = SUB(two_bits_1, MUL(q1, anc));          /* Init: r1 = rem(2^p, |nc|) */
	q2  = DIV(two_bits_1, ad);                    /* Init: q2 = 2^p/|d| */
	r2  = SUB(two_bits_1, MUL(q2, ad));           /* Init: r2 = rem(2^p, |d|) */

	do {
		++p;
		q1 = ADD(q1, q1);                           /* Update q1 = 2^p/|nc| */
		r1 = ADD(r1, r1);                           /* Update r1 = rem(2^p, |nc|) */

		if (CMP(r1, anc) & ir_relation_greater_equal) {
			q1 = ADD(q1, ONE(u_mode));
			r1 = SUB(r1, anc);
		}

		q2 = ADD(q2, q2);                           /* Update q2 = 2^p/|d| */
		r2 = ADD(r2, r2);                           /* Update r2 = rem(2^p, |d|) */

		if (CMP(r2, ad) & ir_relation_greater_equal) {
			q2 = ADD(q2, ONE(u_mode));
			r2 = SUB(r2, ad);
		}

		delta = SUB(ad, r2);
	} while (CMP(q1, delta) & ir_relation_less || (CMP(q1, delta) & ir_relation_equal && CMP(r1, ZERO(u_mode)) & ir_relation_equal));

	d_cmp = CMP(d, ZERO(mode));

	if (d_cmp & ir_relation_greater_equal)
		mag.M = ADD(CNV(q2, mode), ONE(mode));
	else
		mag.M = SUB(ZERO(mode), ADD(CNV(q2, mode), ONE(mode)));

	M_cmp = CMP(mag.M, ZERO(mode));

	mag.s = p - bits;

	/* need an add if d > 0 && M < 0 */
	mag.need_add = d_cmp & ir_relation_greater && M_cmp & ir_relation_less;

	/* need a sub if d < 0 && M > 0 */
	mag.need_sub = d_cmp & ir_relation_less && M_cmp & ir_relation_greater;

	tarval_set_integer_overflow_mode(rem);

	return mag;
}

/**
 * Unsigned division by constant d: calculate the Magic multiplier M and the shift amount s
 *
 * see Faster Unsigned Division by Constants (http://ridiculousfish.com/blog/posts/labor-of-division-episode-iii.html)
 */
struct magicu_info
{
	ir_tarval *multiplier; /* the "magic number" multiplier */
	unsigned   pre_shift;  /* shift for the dividend before multiplying */
	unsigned   post_shift; /* shift for the dividend after multiplying */
	int        increment;  /* 0 or 1; if set then increment the numerator, using one of the two strategies */
};

static struct magicu_info compute_unsigned_magic_info(ir_tarval *divisor, unsigned num_bits)
{
	ir_mode *mode = get_tarval_mode(divisor);

	/* divisor must be larger than zero and not a power of 2
	 * D & (D-1) > 0 */
	assert(get_tarval_long(AND(divisor, SUB(divisor, ONE(mode)))));

	/* The eventual result */
	struct magicu_info result;

	/* Bits in ir_tarval */
	const unsigned UINT_BITS = get_mode_size_bits(mode);

	/* The extra shift implicit in the difference between UINT_BITS and num_bits */
	const unsigned extra_shift = UINT_BITS - num_bits;

	/* The initial power of 2 is one less than the first one that can possibly work */
	ir_tarval *initial_power_of_2 = SHL(ONE(mode), UINT_BITS - 1);

	/* The remainder and quotient of our power of 2 divided by divisor */
	ir_tarval *quotient  = DIV(initial_power_of_2, divisor);
	ir_tarval *remainder = MOD(initial_power_of_2, divisor);

	/* ceil(log_2 D) */
	unsigned ceil_log_2_D;

	/* The magic info for the variant "round down" algorithm */
	ir_tarval *down_multiplier = ZERO(mode);
	unsigned   down_exponent   = 0;
	int        has_magic_down  = 0;

	/* Compute ceil(log_2 D) */
	ceil_log_2_D = 0;
	for (ir_tarval *tmp = divisor; CMP(tmp, ZERO(mode)) & ir_relation_greater; tmp = SHR(tmp, 1))
		ceil_log_2_D++;

	/* Begin a loop that increments the exponent, until we find a power of 2 that works. */
	unsigned exponent;
	for (exponent = 0; ; exponent++) {
		/* Quotient and remainder is from previous exponent; compute it for this exponent. */
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

		/* Set magic_down if we have not set it yet and this exponent works for the round_down algorithm */
		if (! has_magic_down &&
				(CMP(remainder, SHL(ONE(mode), exponent + extra_shift)) &
				ir_relation_less_equal)) {
			has_magic_down  = 1;
			down_multiplier = quotient;
			down_exponent   = exponent;
		}
	}

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
	dbg_info *dbg  = get_irn_dbg_info(div);
	ir_node *n     = get_binop_left(div);
	ir_node *block = get_nodes_block(div);
	ir_mode *mode  = get_irn_mode(n);
	int bits       = get_mode_size_bits(mode);
	ir_node *q;

	/* Beware: do not transform bad code */
	if (is_Bad(n) || is_Bad(block))
		return div;

	if (mode_is_signed(mode)) {
		ir_graph *irg = get_irn_irg(div);
		struct ms mag = magic(tv);

		/* generate the Mulh instruction */
		ir_node *c = new_r_Const(irg, mag.M);
		ir_node *t;
		q = new_rd_Mulh(dbg, block, n, c, mode);

		/* do we need an Add or Sub */
		if (mag.need_add)
			q = new_rd_Add(dbg, block, q, n, mode);
		else if (mag.need_sub)
			q = new_rd_Sub(dbg, block, q, n, mode);

		/* Do we need the shift */
		if (mag.s > 0) {
			c = new_r_Const_long(irg, mode_Iu, mag.s);
			q = new_rd_Shrs(dbg, block, q, c, mode);
		}

		/* final */
		c = new_r_Const_long(irg, mode_Iu, bits - 1);
		t = new_rd_Shr(dbg, block, q, c, mode);

		q = new_rd_Add(dbg, block, q, t, mode);
	} else {
		struct magicu_info mafo = get_magic_info(tv);
		ir_graph *irg = get_irn_irg(div);
		ir_node *c;

		if (mafo.pre_shift > 0) {
			c = new_r_Const_long(irg, mode_Iu, mafo.pre_shift);
			n = new_rd_Shr(dbg, block, n, c, mode);
		}

		if (mafo.increment) {
			ir_node *no_mem    = new_rd_NoMem(dbg, irg);
			ir_node *in[1]     = {n};
			ir_type *utype     = get_unknown_type();
			ir_node *increment = new_rd_Builtin(dbg, block, no_mem, 1, in, ir_bk_saturating_increment, utype);

			n = new_r_Proj(increment, mode_Iu, pn_Builtin_max + 1);
		}

		/* generate the Mulh instruction */
		c = new_r_Const(irg, mafo.multiplier);
		q = new_rd_Mulh(dbg, block, n, c, mode);
		c = new_r_Const_long(irg, mode_Iu, get_mode_size_bits(get_tarval_mode(tv)));
		q = new_rd_Shr(dbg, block, q, c, mode);

		if (mafo.post_shift > 0) {
			c = new_r_Const_long(irg, mode_Iu, mafo.post_shift);
			q = new_rd_Shr(dbg, block, q, c, mode);
		}
	}
	return q;
}

/* Replace Divs with Shifts and Add/Subs and Mulh. */
ir_node *arch_dep_replace_div_by_const(ir_node *irn)
{
	const ir_settings_arch_dep_t *params = be_get_backend_param()->dep_param;
	ir_node *res  = irn;

	/* If the architecture dependent optimizations were not initialized
		or this optimization was not enabled. */
	if (params == NULL || (opts & arch_dep_div_by_const) == 0)
		return irn;

	if (!is_Div(irn))
		return irn;

	ir_node *c = get_Div_right(irn);
	ir_node *block, *left;
	ir_mode *mode;
	ir_tarval *tv, *ntv;
	dbg_info *dbg;
	int n, bits;
	int k;
	int n_flag = 0;

	if (! is_Const(c))
		return irn;

	tv = get_Const_tarval(c);

	/* check for division by zero */
	if (tarval_is_null(tv))
		return irn;

	left  = get_Div_left(irn);
	mode  = get_irn_mode(left);

	/* can only handle integer Div's */
	if (!mode_is_int(mode))
		return irn;

	block = get_nodes_block(irn);
	dbg   = get_irn_dbg_info(irn);

	bits = get_mode_size_bits(mode);
	n    = (bits + 7) / 8;

	k = -1;
	if (mode_is_signed(mode)) {
		/* for signed divisions, the algorithm works for a / -2^k by negating the result */
		ntv = tarval_neg(tv);
		n_flag = 1;
		k = tv_ld2(ntv, n);
	}

	if (k < 0) {
		n_flag = 0;
		k = tv_ld2(tv, n);
	}

	if (k > 0) { /* division by 2^k or -2^k */
		ir_graph *irg = get_irn_irg(irn);
		if (mode_is_signed(mode)) {
			ir_node *k_node;
			ir_node *curr = left;

			/* create the correction code for signed values only if there might be a remainder */
			if (! get_Div_no_remainder(irn)) {
				if (k != 1) {
					k_node = new_r_Const_long(irg, mode_Iu, k - 1);
					curr   = new_rd_Shrs(dbg, block, left, k_node, mode);
				}

				k_node = new_r_Const_long(irg, mode_Iu, bits - k);
				curr   = new_rd_Shr(dbg, block, curr, k_node, mode);
				/* curr is now 2^(k-1) in case left <  0
				 *          or       0 in case left >= 0
				 *
				 * For an example, where this fixup is necessary consider -3 / 2,
				 * which should compute to -1,
				 * but simply shifting right by one computes -2.
				 */

				curr   = new_rd_Add(dbg, block, left, curr, mode);
			}

			k_node = new_r_Const_long(irg, mode_Iu, k);
			res    = new_rd_Shrs(dbg, block, curr, k_node, mode);

			if (n_flag) { /* negate the result */
				k_node = new_r_Const(irg, get_mode_null(mode));
				res = new_rd_Sub(dbg, block, k_node, res, mode);
			}
		} else {      /* unsigned case */
			ir_node *k_node;

			k_node = new_r_Const_long(irg, mode_Iu, k);
			res    = new_rd_Shr(dbg, block, left, k_node, mode);
		}
	} else if (k != 0) {
		/* other constant */
		if (allow_Mulh(params, mode))
			res = replace_div_by_mulh(irn, tv);
	} else { /* k == 0  i.e. division by 1 */
		res = left;
	}

	if (res != irn)
		hook_arch_dep_replace_division_by_const(irn);

	return res;
}

/* Replace Mods with Shifts and Add/Subs and Mulh. */
ir_node *arch_dep_replace_mod_by_const(ir_node *irn)
{
	const ir_settings_arch_dep_t *params = be_get_backend_param()->dep_param;
	ir_node *res  = irn;

	/* If the architecture dependent optimizations were not initialized
	   or this optimization was not enabled. */
	if (params == NULL || (opts & arch_dep_mod_by_const) == 0)
		return irn;

	if (is_Mod(irn)) {
		ir_node *c = get_Mod_right(irn);
		ir_node *block, *left;
		ir_mode *mode;
		ir_tarval *tv, *ntv;
		dbg_info *dbg;
		int n, bits;
		int k;

		if (! is_Const(c))
			return irn;

		tv = get_Const_tarval(c);

		/* check for division by zero */
		if (tarval_is_null(tv))
			return irn;

		left  = get_Mod_left(irn);
		mode  = get_irn_mode(left);
		block = get_nodes_block(irn);
		dbg   = get_irn_dbg_info(irn);
		bits = get_mode_size_bits(mode);
		n    = (bits + 7) / 8;

		k = -1;
		if (mode_is_signed(mode)) {
			/* for signed divisions, the algorithm works for a / -2^k by negating the result */
			ntv = tarval_neg(tv);
			k = tv_ld2(ntv, n);
		}

		if (k < 0) {
			k = tv_ld2(tv, n);
		}

		/* k == 0  i.e. modulo by 1 */
		if (k == 0) {
			ir_graph *irg = get_irn_irg(irn);

			res = new_r_Const(irg, get_mode_null(mode));
		}
		else if (k > 0) {
			ir_graph *irg = get_irn_irg(irn);
			/* division by 2^k or -2^k:
			 * we use "modulus" here, so x % y == x % -y that's why is no difference between the case 2^k and -2^k
			 */
			if (mode_is_signed(mode)) {
				ir_node *k_node;
				ir_node *curr = left;

				if (k != 1) {
					k_node = new_r_Const_long(irg, mode_Iu, k - 1);
					curr   = new_rd_Shrs(dbg, block, left, k_node, mode);
				}

				k_node = new_r_Const_long(irg, mode_Iu, bits - k);
				curr   = new_rd_Shr(dbg, block, curr, k_node, mode);

				curr   = new_rd_Add(dbg, block, left, curr, mode);

				ir_tarval *k_val
					= tarval_shl_unsigned(get_mode_all_one(mode), k);
				k_node = new_r_Const(irg, k_val);
				curr   = new_rd_And(dbg, block, curr, k_node, mode);

				res    = new_rd_Sub(dbg, block, left, curr, mode);
			} else {      /* unsigned case */
				ir_node *k_node;

				ir_tarval *k_val
					= tarval_shr_unsigned(get_mode_all_one(mode),
					                      get_mode_size_bits(mode)-k);
				k_node = new_r_Const(irg, k_val);
				res    = new_rd_And(dbg, block, left, k_node, mode);
			}
		} else {
			/* other constant */
			if (allow_Mulh(params, mode)) {
				res = replace_div_by_mulh(irn, tv);

				res = new_rd_Mul(dbg, block, res, c, mode);

				/* res = arch_dep_mul_to_shift(res); */

				res = new_rd_Sub(dbg, block, left, res, mode);
			}
		}
	}

	if (res != irn)
		hook_arch_dep_replace_division_by_const(irn);

	return res;
}
