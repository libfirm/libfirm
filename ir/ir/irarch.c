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
 * @brief   Machine dependent Firm optimizations.
 * @date    28.9.2004
 * @author  Sebastian Hack, Michael Beck
 * @version $Id$
 *
 * Implements "Strenght Reduction of Multiplications by Integer Constants" by Youfeng Wu.
 * Implements Division and Modulo by Consts from "Hackers Delight",
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "iropt_t.h"
#include "ircons_t.h"
#include "irgmod.h"
#include "irvrfy.h"
#include "tv_t.h"
#include "dbginfo_t.h"
#include "iropt_dbg.h"
#include "irflag_t.h"
#include "irhooks.h"
#include "ircons.h"
#include "irarch.h"
#include "irflag.h"

#undef DEB

#define MAX_BITSTR 64

/* when we need verifying */
#ifdef NDEBUG
# define IRN_VRFY_IRG(res, irg)
#else
# define IRN_VRFY_IRG(res, irg)  irn_vrfy_irg(res, irg)
#endif

/** The params got from the factory in arch_dep_init(...). */
static const ir_settings_arch_dep_t *params = NULL;

/** The bit mask, which optimizations to apply. */
static arch_dep_opts_t opts;

void arch_dep_init(arch_dep_params_factory_t factory) {
	opts = arch_dep_none;

	if (factory != NULL)
		params = factory();
}

void arch_dep_set_opts(arch_dep_opts_t the_opts) {
	opts = the_opts;

	if (opts & arch_dep_mul_to_shift)
		set_opt_arch_dep_running(1);
}

/** check, whether a mode allows a Mulh instruction. */
static int allow_Mulh(ir_mode *mode) {
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
typedef struct _mul_env {
	struct obstack obst;       /**< an obstack for local space. */
	ir_mode        *mode;      /**< the mode of the multiplication constant */
	unsigned       bits;       /**< number of bits in the mode */
	unsigned       max_S;      /**< the maximum LEA shift value. */
	instruction    *root;      /**< the root of the instruction tree */
	ir_node        *op;        /**< the operand that is multiplied */
	ir_node        *blk;       /**< the block where the new graph is built */
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
static int default_evaluate(insn_kind kind, tarval *tv) {
	(void) tv;

	if (kind == MUL)
		return 13;
	return 1;
}

/**
 * emit a LEA (or an Add) instruction
 */
static instruction *emit_LEA(mul_env *env, instruction *a, instruction *b, unsigned shift) {
	instruction *res = obstack_alloc(&env->obst, sizeof(*res));
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
static instruction *emit_SHIFT(mul_env *env, instruction *a, unsigned shift) {
	instruction *res = obstack_alloc(&env->obst, sizeof(*res));
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
static instruction *emit_SUB(mul_env *env, instruction *a, instruction *b) {
	instruction *res = obstack_alloc(&env->obst, sizeof(*res));
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
static instruction *emit_ROOT(mul_env *env, ir_node *root_op) {
	instruction *res = obstack_alloc(&env->obst, sizeof(*res));
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
static unsigned char *value_to_condensed(mul_env *env, tarval *tv, int *pr) {
	ir_mode *mode = get_tarval_mode(tv);
	int     bits = get_mode_size_bits(mode);
	char    *bitstr = get_tarval_bitpattern(tv);
	int     i, l, r;
	unsigned char *R = obstack_alloc(&env->obst, bits);

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
static int calculate_gain(unsigned char *R, int r) {
	int max_gain = -1;
	int idx, i;
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
	if (max_gain > 0)
		return idx;
	return -1;
}

/**
 * Calculates the condensed complement of a given (R,r) tuple
 */
static unsigned char *complement_condensed(mul_env *env, unsigned char *R, int r, int gain, int *prs) {
	unsigned char *value = obstack_alloc(&env->obst, env->bits);
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
static tarval *condensed_to_value(mul_env *env, unsigned char *R, int r) {
	tarval *res, *tv;
	int i, j;

	j = 0;
	tv = get_mode_one(env->mode);
	res = NULL;
	for (i = 0; i < r; ++i) {
		j = R[i];
		if (j) {
			tarval *t = new_tarval_from_long(j, mode_Iu);
			tv = tarval_shl(tv, t);
		}
		res = res ? tarval_add(res, tv) : tv;
	}
	return res;
}

/* forward */
static instruction *basic_decompose_mul(mul_env *env, unsigned char *R, int r, tarval *N);

/*
 * handle simple cases with up-to 2 bits set
 */
static instruction *decompose_simple_cases(mul_env *env, unsigned char *R, int r, tarval *N) {
	instruction *ins, *ins2;

	(void) N;
	if (r == 1) {
		return emit_SHIFT(env, env->root, R[0]);
	} else {
		assert(r == 2);

		ins = env->root;
		if (R[0] != 0) {
			ins = emit_SHIFT(env, ins, R[0]);
		}
		if (R[1] <= env->max_S)
			return emit_LEA(env, ins, ins, R[1]);

		ins2 = emit_SHIFT(env, env->root, R[0] + R[1]);
		return emit_LEA(env, ins, ins2, 0);
	}
}

/**
 * Main decompose driver.
 */
static instruction *decompose_mul(mul_env *env, unsigned char *R, int r, tarval *N) {
	unsigned i;
	int gain;

	if (r <= 2)
		return decompose_simple_cases(env, R, r, N);

	if (params->also_use_subs) {
		gain = calculate_gain(R, r);
		if (gain > 0) {
			instruction *instr1, *instr2;
			unsigned char *R1, *R2;
			int r1, r2, i, k, j;

			R1 = complement_condensed(env, R, r, gain, &r1);
			r2 = r - gain + 1;
			R2 = obstack_alloc(&env->obst, r2);

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
		tarval *div_res, *mod_res;
		tarval *tv = new_tarval_from_long((1 << i) + 1, env->mode);

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
static instruction *basic_decompose_mul(mul_env *env, unsigned char *R, int r, tarval *N) {
	instruction *Ns;
	unsigned t;

	if (R[0] == 0) {					/* Case 1 */
		t = R[1] > IMAX(env->max_S, R[1]);
		R[1] -= t;
		Ns = decompose_mul(env, &R[1], r - 1, N);
		return emit_LEA(env, env->root, Ns, t);
	} else if (R[0] <= env->max_S) {	/* Case 2 */
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
static ir_node *build_graph(mul_env *env, instruction *inst) {
	ir_node *l, *r, *c;

	if (inst->irn)
		return inst->irn;

	switch (inst->kind) {
	case LEA:
		l = build_graph(env, inst->in[0]);
		r = build_graph(env, inst->in[1]);
		c = new_r_Const(current_ir_graph, env->blk, env->shf_mode, new_tarval_from_long(inst->shift_count, env->shf_mode));
		r = new_rd_Shl(env->dbg, current_ir_graph, env->blk, r, c, env->mode);
		return inst->irn = new_rd_Add(env->dbg, current_ir_graph, env->blk, l, r, env->mode);
	case SHIFT:
		l = build_graph(env, inst->in[0]);
		c = new_r_Const(current_ir_graph, env->blk, env->shf_mode, new_tarval_from_long(inst->shift_count, env->shf_mode));
		return inst->irn = new_rd_Shl(env->dbg, current_ir_graph, env->blk, l, c, env->mode);
	case SUB:
		l = build_graph(env, inst->in[0]);
		r = build_graph(env, inst->in[1]);
		return inst->irn = new_rd_Sub(env->dbg, current_ir_graph, env->blk, l, r, env->mode);
	case ADD:
		l = build_graph(env, inst->in[0]);
		r = build_graph(env, inst->in[1]);
		return inst->irn = new_rd_Add(env->dbg, current_ir_graph, env->blk, l, r, env->mode);
	case ZERO:
		return inst->irn = new_r_Const(current_ir_graph, env->blk, env->mode, get_mode_null(env->mode));
	default:
		assert(0);
		return NULL;
	}
}

/**
 * Calculate the costs for the given instruction sequence.
 * Note that additional costs due to higher register pressure are NOT evaluated yet
 */
static int evaluate_insn(mul_env *env, instruction *inst) {
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
		costs += env->evaluate(inst->kind, NULL);
		inst->costs = costs;
		return costs;
	case SHIFT:
		if (inst->shift_count > params->highest_shift_amount)
			env->fail = 1;
		if (env->n_shift <= 0)
			env->fail = 1;
		else
			--env->n_shift;
		costs  = evaluate_insn(env, inst->in[0]);
		costs += env->evaluate(inst->kind, NULL);
		inst->costs = costs;
		return costs;
	case ZERO:
		inst->costs = costs = env->evaluate(inst->kind, NULL);
		return costs;
	default:
		assert(0);
		return 0;
	}
}

/**
 * Evaluate the replacement instructions and build a new graph
 * if faster than the Mul.
 * returns the root of the new graph then or irn otherwise.
 *
 * @param irn      the Mul operation
 * @param operand  the multiplication operand
 * @param tv       the multiplication constant
 *
 * @return the new graph
 */
static ir_node *do_decomposition(ir_node *irn, ir_node *operand, tarval *tv) {
	mul_env       env;
	instruction   *inst;
	unsigned char *R;
	int           r;
	ir_node       *res = irn;
	int           mul_costs;

	obstack_init(&env.obst);
	env.mode     = get_tarval_mode(tv);
	env.bits     = (unsigned)get_mode_size_bits(env.mode);
	env.max_S    = 3;
	env.root     = emit_ROOT(&env, operand);
	env.fail     = 0;
	env.n_shift  = params->maximum_shifts;
	env.evaluate = params->evaluate != NULL ? params->evaluate : default_evaluate;

	R = value_to_condensed(&env, tv, &r);
	inst = decompose_mul(&env, R, r, tv);

	/* the paper suggests 70% here */
	mul_costs = (env.evaluate(MUL, tv) * 7) / 10;
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
ir_node *arch_dep_replace_mul_with_shifts(ir_node *irn) {
	ir_node *res = irn;
	ir_mode *mode = get_irn_mode(irn);

	/* If the architecture dependent optimizations were not initialized
	   or this optimization was not enabled. */
	if (params == NULL || (opts & arch_dep_mul_to_shift) == 0)
		return irn;

	if (is_Mul(irn) && mode_is_int(mode)) {
		ir_node *left    = get_binop_left(irn);
		ir_node *right   = get_binop_right(irn);
		tarval *tv       = NULL;
		ir_node *operand = NULL;

		/* Look, if one operand is a constant. */
		if (is_Const(left)) {
			tv = get_Const_tarval(left);
			operand = right;
		} else if (is_Const(right)) {
			tv = get_Const_tarval(right);
			operand = left;
		}

		if (tv != NULL) {
			res = do_decomposition(irn, operand, tv);

			if (res != irn) {
				hook_arch_dep_replace_mul_with_shifts(irn);
				exchange(irn, res);
			}
		}
	}

	return res;
}

/**
 * calculated the ld2 of a tarval if tarval is 2^n, else returns -1.
 */
static int tv_ld2(tarval *tv, int bits) {
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
#define SHL(a, b) tarval_shl(a, b)
#define SHR(a, b) tarval_shr(a, b)
#define ADD(a, b) tarval_add(a, b)
#define SUB(a, b) tarval_sub(a, b)
#define MUL(a, b) tarval_mul(a, b)
#define DIV(a, b) tarval_div(a, b)
#define MOD(a, b) tarval_mod(a, b)
#define CMP(a, b) tarval_cmp(a, b)
#define CNV(a, m) tarval_convert_to(a, m)
#define ONE(m)    get_mode_one(m)
#define ZERO(m)   get_mode_null(m)

/** The result of a the magic() function. */
struct ms {
	tarval *M;        /**< magic number */
	int s;            /**< shift amount */
	int need_add;     /**< an additional add is needed */
	int need_sub;     /**< an additional sub is needed */
};

/**
 * Signed division by constant d: calculate the Magic multiplier M and the shift amount s
 *
 * see Hacker's Delight: 10-6 Integer Division by Constants: Incorporation into a Compiler
 */
static struct ms magic(tarval *d) {
	ir_mode *mode   = get_tarval_mode(d);
	ir_mode *u_mode = find_unsigned_mode(mode);
	int bits        = get_mode_size_bits(u_mode);
	int p;
	tarval *ad, *anc, *delta, *q1, *r1, *q2, *r2, *t;     /* unsigned */
	pn_Cmp d_cmp, M_cmp;

	tarval *bits_minus_1, *two_bits_1;

	struct ms mag;

	tarval_int_overflow_mode_t rem = tarval_get_integer_overflow_mode();

	/* we need overflow mode to work correctly */
	tarval_set_integer_overflow_mode(TV_OVERFLOW_WRAP);

	/* 2^(bits-1) */
	bits_minus_1 = new_tarval_from_long(bits - 1, u_mode);
	two_bits_1   = SHL(get_mode_one(u_mode), bits_minus_1);

	ad  = CNV(ABS(d), u_mode);
	t   = ADD(two_bits_1, SHR(CNV(d, u_mode), bits_minus_1));
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

		if (CMP(r1, anc) & pn_Cmp_Ge) {
			q1 = ADD(q1, ONE(u_mode));
			r1 = SUB(r1, anc);
		}

		q2 = ADD(q2, q2);                           /* Update q2 = 2^p/|d| */
		r2 = ADD(r2, r2);                           /* Update r2 = rem(2^p, |d|) */

		if (CMP(r2, ad) & pn_Cmp_Ge) {
			q2 = ADD(q2, ONE(u_mode));
			r2 = SUB(r2, ad);
		}

		delta = SUB(ad, r2);
	} while (CMP(q1, delta) & pn_Cmp_Lt || (CMP(q1, delta) & pn_Cmp_Eq && CMP(r1, ZERO(u_mode)) & pn_Cmp_Eq));

	d_cmp = CMP(d, ZERO(mode));

	if (d_cmp & pn_Cmp_Ge)
		mag.M = ADD(CNV(q2, mode), ONE(mode));
	else
		mag.M = SUB(ZERO(mode), ADD(CNV(q2, mode), ONE(mode)));

	M_cmp = CMP(mag.M, ZERO(mode));

	mag.s = p - bits;

	/* need an add if d > 0 && M < 0 */
	mag.need_add = d_cmp & pn_Cmp_Gt && M_cmp & pn_Cmp_Lt;

	/* need a sub if d < 0 && M > 0 */
	mag.need_sub = d_cmp & pn_Cmp_Lt && M_cmp & pn_Cmp_Gt;

	tarval_set_integer_overflow_mode(rem);

	return mag;
}

/** The result of the magicu() function. */
struct mu {
	tarval *M;        /**< magic add constant */
	int s;            /**< shift amount */
	int need_add;     /**< add indicator */
};

/**
 * Unsigned division by constant d: calculate the Magic multiplier M and the shift amount s
 *
 * see Hacker's Delight: 10-10 Integer Division by Constants: Incorporation into a Compiler (Unsigned)
 */
static struct mu magicu(tarval *d) {
	ir_mode *mode   = get_tarval_mode(d);
	int bits        = get_mode_size_bits(mode);
	int p;
	tarval *nc, *delta, *q1, *r1, *q2, *r2;
	tarval *bits_minus_1, *two_bits_1, *seven_ff;

	struct mu magu;

	tarval_int_overflow_mode_t rem = tarval_get_integer_overflow_mode();

	/* we need overflow mode to work correctly */
	tarval_set_integer_overflow_mode(TV_OVERFLOW_WRAP);

	bits_minus_1 = new_tarval_from_long(bits - 1, mode);
	two_bits_1   = SHL(get_mode_one(mode), bits_minus_1);
	seven_ff     = SUB(two_bits_1, ONE(mode));

	magu.need_add = 0;                            /* initialize the add indicator */
	nc = SUB(NEG(ONE(mode)), MOD(NEG(d), d));
	p  = bits - 1;                                /* Init: p */
	q1 = DIV(two_bits_1, nc);                     /* Init: q1 = 2^p/nc */
	r1 = SUB(two_bits_1, MUL(q1, nc));            /* Init: r1 = rem(2^p, nc) */
	q2 = DIV(seven_ff, d);                        /* Init: q2 = (2^p - 1)/d */
	r2 = SUB(seven_ff, MUL(q2, d));               /* Init: r2 = rem(2^p - 1, d) */

	do {
		++p;
		if (CMP(r1, SUB(nc, r1)) & pn_Cmp_Ge) {
			q1 = ADD(ADD(q1, q1), ONE(mode));
			r1 = SUB(ADD(r1, r1), nc);
		}
		else {
			q1 = ADD(q1, q1);
			r1 = ADD(r1, r1);
		}

		if (CMP(ADD(r2, ONE(mode)), SUB(d, r2)) & pn_Cmp_Ge) {
			if (CMP(q2, seven_ff) & pn_Cmp_Ge)
				magu.need_add = 1;

			q2 = ADD(ADD(q2, q2), ONE(mode));
			r2 = SUB(ADD(ADD(r2, r2), ONE(mode)), d);
		}
		else {
			if (CMP(q2, two_bits_1) & pn_Cmp_Ge)
				magu.need_add = 1;

			q2 = ADD(q2, q2);
			r2 = ADD(ADD(r2, r2), ONE(mode));
		}
		delta = SUB(SUB(d, ONE(mode)), r2);
	} while (p < 2*bits &&
		(CMP(q1, delta) & pn_Cmp_Lt || (CMP(q1, delta) & pn_Cmp_Eq && CMP(r1, ZERO(mode)) & pn_Cmp_Eq)));

	magu.M = ADD(q2, ONE(mode));                       /* Magic number */
	magu.s = p - bits;                                 /* and shift amount */

	tarval_set_integer_overflow_mode(rem);

	return magu;
}

/**
 * Build the Mulh replacement code for n / tv.
 *
 * Note that 'div' might be a mod or DivMod operation as well
 */
static ir_node *replace_div_by_mulh(ir_node *div, tarval *tv) {
	dbg_info *dbg  = get_irn_dbg_info(div);
	ir_node *n     = get_binop_left(div);
	ir_node *block = get_irn_n(div, -1);
	ir_mode *mode  = get_irn_mode(n);
	int bits       = get_mode_size_bits(mode);
	ir_node *q, *t, *c;

	/* Beware: do not transform bad code */
	if (is_Bad(n) || is_Bad(block))
		return div;

	if (mode_is_signed(mode)) {
		struct ms mag = magic(tv);

		/* generate the Mulh instruction */
		c = new_r_Const(current_ir_graph, block, mode, mag.M);
		q = new_rd_Mulh(dbg, current_ir_graph, block, n, c, mode);

		/* do we need an Add or Sub */
		if (mag.need_add)
			q = new_rd_Add(dbg, current_ir_graph, block, q, n, mode);
		else if (mag.need_sub)
			q = new_rd_Sub(dbg, current_ir_graph, block, q, n, mode);

		/* Do we need the shift */
		if (mag.s > 0) {
			c = new_r_Const_long(current_ir_graph, block, mode_Iu, mag.s);
			q    = new_rd_Shrs(dbg, current_ir_graph, block, q, c, mode);
		}

		/* final */
		c = new_r_Const_long(current_ir_graph, block, mode_Iu, bits-1);
		t = new_rd_Shr(dbg, current_ir_graph, block, q, c, mode);

		q = new_rd_Add(dbg, current_ir_graph, block, q, t, mode);
	} else {
		struct mu mag = magicu(tv);
		ir_node *c;

		/* generate the Mulh instruction */
		c = new_r_Const(current_ir_graph, block, mode, mag.M);
		q    = new_rd_Mulh(dbg, current_ir_graph, block, n, c, mode);

		if (mag.need_add) {
			if (mag.s > 0) {
				/* use the GM scheme */
				t = new_rd_Sub(dbg, current_ir_graph, block, n, q, mode);

				c = new_r_Const(current_ir_graph, block, mode_Iu, get_mode_one(mode_Iu));
				t = new_rd_Shr(dbg, current_ir_graph, block, t, c, mode);

				t = new_rd_Add(dbg, current_ir_graph, block, t, q, mode);

				c = new_r_Const_long(current_ir_graph, block, mode_Iu, mag.s-1);
				q = new_rd_Shr(dbg, current_ir_graph, block, t, c, mode);
			} else {
				/* use the default scheme */
				q = new_rd_Add(dbg, current_ir_graph, block, q, n, mode);
			}
		} else if (mag.s > 0) { /* default scheme, shift needed */
			c = new_r_Const_long(current_ir_graph, block, mode_Iu, mag.s);
			q = new_rd_Shr(dbg, current_ir_graph, block, q, c, mode);
		}
	}
	return q;
}

/* Replace Divs with Shifts and Add/Subs and Mulh. */
ir_node *arch_dep_replace_div_by_const(ir_node *irn) {
	ir_node *res  = irn;

	/* If the architecture dependent optimizations were not initialized
	or this optimization was not enabled. */
	if (params == NULL || (opts & arch_dep_div_by_const) == 0)
		return irn;

	if (get_irn_opcode(irn) == iro_Div) {
		ir_node *c = get_Div_right(irn);
		ir_node *block, *left;
		ir_mode *mode;
		tarval *tv, *ntv;
		dbg_info *dbg;
		int n, bits;
		int k, n_flag;

		if (get_irn_op(c) != op_Const)
			return irn;

		tv = get_Const_tarval(c);

		/* check for division by zero */
		if (tarval_is_null(tv))
			return irn;

		left  = get_Div_left(irn);
		mode  = get_irn_mode(left);
		block = get_irn_n(irn, -1);
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

		if (k >= 0) { /* division by 2^k or -2^k */
			if (mode_is_signed(mode)) {
				ir_node *k_node;
				ir_node *curr = left;

				if (k != 1) {
					k_node = new_r_Const_long(current_ir_graph, block, mode_Iu, k - 1);
					curr   = new_rd_Shrs(dbg, current_ir_graph, block, left, k_node, mode);
				}

				k_node = new_r_Const_long(current_ir_graph, block, mode_Iu, bits - k);
				curr   = new_rd_Shr(dbg, current_ir_graph, block, curr, k_node, mode);

				curr   = new_rd_Add(dbg, current_ir_graph, block, left, curr, mode);

				k_node = new_r_Const_long(current_ir_graph, block, mode_Iu, k);
				res    = new_rd_Shrs(dbg, current_ir_graph, block, curr, k_node, mode);

				if (n_flag) { /* negate the result */
					ir_node *k_node;

					k_node = new_r_Const(current_ir_graph, block, mode, get_mode_null(mode));
					res = new_rd_Sub(dbg, current_ir_graph, block, k_node, res, mode);
				}
			} else {      /* unsigned case */
				ir_node *k_node;

				k_node = new_r_Const_long(current_ir_graph, block, mode_Iu, k);
				res    = new_rd_Shr(dbg, current_ir_graph, block, left, k_node, mode);
			}
		} else {
			/* other constant */
			if (allow_Mulh(mode))
				res = replace_div_by_mulh(irn, tv);
		}
	}

	if (res != irn)
		hook_arch_dep_replace_division_by_const(irn);

	return res;
}

/* Replace Mods with Shifts and Add/Subs and Mulh. */
ir_node *arch_dep_replace_mod_by_const(ir_node *irn) {
	ir_node *res  = irn;

	/* If the architecture dependent optimizations were not initialized
	   or this optimization was not enabled. */
	if (params == NULL || (opts & arch_dep_mod_by_const) == 0)
		return irn;

	if (get_irn_opcode(irn) == iro_Mod) {
		ir_node *c = get_Mod_right(irn);
		ir_node *block, *left;
		ir_mode *mode;
		tarval *tv, *ntv;
		dbg_info *dbg;
		int n, bits;
		int k;

		if (get_irn_op(c) != op_Const)
			return irn;

		tv = get_Const_tarval(c);

		/* check for division by zero */
		if (tarval_is_null(tv))
			return irn;

		left  = get_Mod_left(irn);
		mode  = get_irn_mode(left);
		block = get_irn_n(irn, -1);
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

		if (k >= 0) {
			/* division by 2^k or -2^k:
			 * we use "modulus" here, so x % y == x % -y that's why is no difference between the case 2^k and -2^k
			 */
			if (mode_is_signed(mode)) {
				ir_node *k_node;
				ir_node *curr = left;

				if (k != 1) {
					k_node = new_r_Const_long(current_ir_graph, block, mode_Iu, k - 1);
					curr   = new_rd_Shrs(dbg, current_ir_graph, block, left, k_node, mode);
				}

				k_node = new_r_Const_long(current_ir_graph, block, mode_Iu, bits - k);
				curr   = new_rd_Shr(dbg, current_ir_graph, block, curr, k_node, mode);

				curr   = new_rd_Add(dbg, current_ir_graph, block, left, curr, mode);

				k_node = new_r_Const_long(current_ir_graph, block, mode, (-1) << k);
				curr   = new_rd_And(dbg, current_ir_graph, block, curr, k_node, mode);

				res    = new_rd_Sub(dbg, current_ir_graph, block, left, curr, mode);
			} else {      /* unsigned case */
				ir_node *k_node;

				k_node = new_r_Const_long(current_ir_graph, block, mode, (1 << k) - 1);
				res    = new_rd_And(dbg, current_ir_graph, block, left, k_node, mode);
			}
		} else {
			/* other constant */
			if (allow_Mulh(mode)) {
				res = replace_div_by_mulh(irn, tv);

				res = new_rd_Mul(dbg, current_ir_graph, block, res, c, mode);

				/* res = arch_dep_mul_to_shift(res); */

				res = new_rd_Sub(dbg, current_ir_graph, block, left, res, mode);
			}
		}
	}

	if (res != irn)
		hook_arch_dep_replace_division_by_const(irn);

	return res;
}

/* Replace DivMods with Shifts and Add/Subs and Mulh. */
void arch_dep_replace_divmod_by_const(ir_node **div, ir_node **mod, ir_node *irn) {
	*div = *mod = NULL;

	/* If the architecture dependent optimizations were not initialized
	   or this optimization was not enabled. */
	if (params == NULL ||
		((opts & (arch_dep_div_by_const|arch_dep_mod_by_const)) != (arch_dep_div_by_const|arch_dep_mod_by_const)))
		return;

	if (get_irn_opcode(irn) == iro_DivMod) {
		ir_node *c = get_DivMod_right(irn);
		ir_node *block, *left;
		ir_mode *mode;
		tarval *tv, *ntv;
		dbg_info *dbg;
		int n, bits;
		int k, n_flag;

		if (get_irn_op(c) != op_Const)
			return;

		tv = get_Const_tarval(c);

		/* check for division by zero */
		if (tarval_is_null(tv))
			return;

		left  = get_DivMod_left(irn);
		mode  = get_irn_mode(left);
		block = get_irn_n(irn, -1);
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

		if (k >= 0) { /* division by 2^k or -2^k */
			if (mode_is_signed(mode)) {
				ir_node *k_node, *c_k;
				ir_node *curr = left;

				if (k != 1) {
					k_node = new_r_Const_long(current_ir_graph, block, mode_Iu, k - 1);
					curr   = new_rd_Shrs(dbg, current_ir_graph, block, left, k_node, mode);
				}

				k_node = new_r_Const_long(current_ir_graph, block, mode_Iu, bits - k);
				curr   = new_rd_Shr(dbg, current_ir_graph, block, curr, k_node, mode);

				curr   = new_rd_Add(dbg, current_ir_graph, block, left, curr, mode);

				c_k    = new_r_Const_long(current_ir_graph, block, mode_Iu, k);

				*div   = new_rd_Shrs(dbg, current_ir_graph, block, curr, c_k, mode);

				if (n_flag) { /* negate the div result */
					ir_node *k_node;

					k_node = new_r_Const(current_ir_graph, block, mode, get_mode_null(mode));
					*div = new_rd_Sub(dbg, current_ir_graph, block, k_node, *div, mode);
				}

				k_node = new_r_Const_long(current_ir_graph, block, mode, (-1) << k);
				curr   = new_rd_And(dbg, current_ir_graph, block, curr, k_node, mode);

				*mod   = new_rd_Sub(dbg, current_ir_graph, block, left, curr, mode);
			} else {      /* unsigned case */
				ir_node *k_node;

				k_node = new_r_Const_long(current_ir_graph, block, mode_Iu, k);
				*div   = new_rd_Shr(dbg, current_ir_graph, block, left, k_node, mode);

				k_node = new_r_Const_long(current_ir_graph, block, mode, (1 << k) - 1);
				*mod   = new_rd_And(dbg, current_ir_graph, block, left, k_node, mode);
			}
		} else {
			/* other constant */
			if (allow_Mulh(mode)) {
				ir_node *t;

				*div = replace_div_by_mulh(irn, tv);

				t    = new_rd_Mul(dbg, current_ir_graph, block, *div, c, mode);

				/* t = arch_dep_mul_to_shift(t); */

				*mod = new_rd_Sub(dbg, current_ir_graph, block, left, t, mode);
			}
		}
	}

	if (*div)
		hook_arch_dep_replace_division_by_const(irn);
}


static const ir_settings_arch_dep_t default_params = {
	1,  /* also use subs */
	4,  /* maximum shifts */
	31, /* maximum shift amount */
	default_evaluate,  /* default evaluator */

	0,  /* allow Mulhs */
	0,  /* allow Mulus */
	32  /* Mulh allowed up to 32 bit */
};

/* A default parameter factory for testing purposes. */
const ir_settings_arch_dep_t *arch_dep_default_factory(void) {
	return &default_params;
}
