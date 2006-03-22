/**
 * This file implements the node emitter.
 *
 * $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits.h>

#include "xmalloc.h"
#include "tv.h"
#include "iredges.h"
#include "debug.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irop_t.h"
#include "irargs_t.h"
#include "irprog_t.h"
#include "iredges_t.h"

#include "../besched_t.h"
#include "../benode_t.h"

#include "ia32_emitter.h"
#include "gen_ia32_emitter.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_nodes_attr.h"
#include "ia32_new_nodes.h"
#include "ia32_map_regs.h"

#ifdef obstack_chunk_alloc
# undef obstack_chunk_alloc
# define obstack_chunk_alloc xmalloc
#else
# define obstack_chunk_alloc xmalloc
# define obstack_chunk_free free
#endif

#define BLOCK_PREFIX(x) ".L" x

extern int obstack_printf(struct obstack *obst, char *fmt, ...);

#define SNPRINTF_BUF_LEN 128

/* global arch_env for lc_printf functions */
static const arch_env_t *arch_env = NULL;

/* indicates whether blocks are scheduled or not
   (this variable is set automatically) */
static int have_block_sched       = 0;

/*************************************************************
 *             _       _    __   _          _
 *            (_)     | |  / _| | |        | |
 *  _ __  _ __ _ _ __ | |_| |_  | |__   ___| |_ __   ___ _ __
 * | '_ \| '__| | '_ \| __|  _| | '_ \ / _ \ | '_ \ / _ \ '__|
 * | |_) | |  | | | | | |_| |   | | | |  __/ | |_) |  __/ |
 * | .__/|_|  |_|_| |_|\__|_|   |_| |_|\___|_| .__/ \___|_|
 * | |                                       | |
 * |_|                                       |_|
 *************************************************************/

/* We always pass the ir_node which is a pointer. */
static int ia32_get_arg_type(const lc_arg_occ_t *occ) {
	return lc_arg_type_ptr;
}


/**
 * Returns the register at in position pos.
 */
static const arch_register_t *get_in_reg(const ir_node *irn, int pos) {
	ir_node                *op;
	const arch_register_t  *reg = NULL;

	assert(get_irn_arity(irn) > pos && "Invalid IN position");

	/* The out register of the operator at position pos is the
	   in register we need. */
	op = get_irn_n(irn, pos);

	reg = arch_get_irn_register(arch_env, op);

	assert(reg && "no in register found");
	return reg;
}

/**
 * Returns the register at out position pos.
 */
static const arch_register_t *get_out_reg(const ir_node *irn, int pos) {
	ir_node                *proj;
	const arch_register_t  *reg = NULL;

	/* 1st case: irn is not of mode_T, so it has only                 */
	/*           one OUT register -> good                             */
	/* 2nd case: irn is of mode_T -> collect all Projs and ask the    */
	/*           Proj with the corresponding projnum for the register */

	if (get_irn_mode(irn) != mode_T) {
		reg = arch_get_irn_register(arch_env, irn);
	}
	else if (is_ia32_irn(irn)) {
		reg = get_ia32_out_reg(irn, pos);
	}
	else {
		const ir_edge_t *edge;

		foreach_out_edge(irn, edge) {
			proj = get_edge_src_irn(edge);
			assert(is_Proj(proj) && "non-Proj from mode_T node");
			if (get_Proj_proj(proj) == pos) {
				reg = arch_get_irn_register(arch_env, proj);
				break;
			}
		}
	}

	assert(reg && "no out register found");
	return reg;
}

enum io_direction {
  IN_REG,
  OUT_REG
};

/**
 * Returns the name of the in register at position pos.
 */
static const char *get_ia32_reg_name(ir_node *irn, int pos, enum io_direction in_out) {
	const arch_register_t *reg;
	const char            *name;
	static char           *buf = NULL;
	int                    len;

	if (in_out == IN_REG) {
		reg = get_in_reg(irn, pos);
	}
	else {
		/* destination address mode nodes don't have outputs */
		if (is_ia32_irn(irn) && get_ia32_op_type(irn) == ia32_AddrModeD) {
			return "MEM";
		}

		reg = get_out_reg(irn, pos);
	}

	name = arch_register_get_name(reg);

	if (buf) {
		free(buf);
	}

	len = strlen(name) + 2;
	buf = xcalloc(1, len);

	snprintf(buf, len, "%%%s", name);

	return buf;
}

/**
 * Get the register name for a node.
 */
static int ia32_get_reg_name(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	const char *buf;
	ir_node    *X  = arg->v_ptr;
	int         nr = occ->width - 1;

	if (!X)
		return lc_appendable_snadd(app, "(null)", 6);

	buf = get_ia32_reg_name(X, nr, occ->conversion == 'S' ? IN_REG : OUT_REG);

	return lc_appendable_snadd(app, buf, strlen(buf));
}

/**
 * Returns the tarval, offset or scale of an ia32 as a string.
 */
static int ia32_const_to_str(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	const char *buf;
	ir_node    *X = arg->v_ptr;

	if (!X)
		return lc_arg_append(app, occ, "(null)", 6);

	if (occ->conversion == 'C') {
		buf = get_ia32_cnst(X);
	}
	else { /* 'O' */
		buf = get_ia32_am_offs(X);
	}

	return buf ? lc_appendable_snadd(app, buf, strlen(buf)) : 0;
}

/**
 * Determines the SSE suffix depending on the mode.
 */
static int ia32_get_mode_suffix(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	ir_node *X    = arg->v_ptr;
	ir_mode *mode = get_irn_mode(X);

	if (mode == mode_T) {
		mode = is_ia32_AddrModeS(X) || is_ia32_AddrModeD(X) ? get_ia32_ls_mode(X) : get_ia32_res_mode(X);
	}

	if (!X)
		return lc_arg_append(app, occ, "(null)", 6);

	if (mode_is_float(mode)) {
		return lc_appendable_chadd(app, get_mode_size_bits(mode) == 32 ? 's' : 'd');
	}
	else {
		return lc_appendable_chadd(app, mode_is_signed(mode) ? 's' : 'z');
	}
}

/**
 * Return the ia32 printf arg environment.
 * We use the firm environment with some additional handlers.
 */
const lc_arg_env_t *ia32_get_arg_env(void) {
	static lc_arg_env_t *env = NULL;

	static const lc_arg_handler_t ia32_reg_handler   = { ia32_get_arg_type, ia32_get_reg_name };
	static const lc_arg_handler_t ia32_const_handler = { ia32_get_arg_type, ia32_const_to_str };
	static const lc_arg_handler_t ia32_mode_handler  = { ia32_get_arg_type, ia32_get_mode_suffix };

	if(env == NULL) {
		/* extend the firm printer */
		env = firm_get_arg_env();

		lc_arg_register(env, "ia32:sreg", 'S', &ia32_reg_handler);
		lc_arg_register(env, "ia32:dreg", 'D', &ia32_reg_handler);
		lc_arg_register(env, "ia32:cnst", 'C', &ia32_const_handler);
		lc_arg_register(env, "ia32:offs", 'O', &ia32_const_handler);
		lc_arg_register(env, "ia32:mode", 'M', &ia32_mode_handler);
	}

	return env;
}

static char *ia32_get_reg_name_for_mode(ia32_emit_env_t *env, ir_mode *mode, const arch_register_t *reg) {
	switch(get_mode_size_bits(mode)) {
		case 8:
			return ia32_get_mapped_reg_name(env->isa->regs_8bit, reg);
		case 16:
			return ia32_get_mapped_reg_name(env->isa->regs_16bit, reg);
		case 32:
			return (char *)arch_register_get_name(reg);
		default:
			assert(0 && "unsupported mode size");
			return NULL;
	}
}

/**
 * Emits registers and/or address mode of a binary operation.
 */
char *ia32_emit_binop(const ir_node *n, ia32_emit_env_t *env) {
	static char *buf = NULL;

	/* verify that this function is never called on non-AM supporting operations */
	//assert(get_ia32_am_support(n) != ia32_am_None && "emit binop expects addressmode support");

#define PRODUCES_RESULT(n)   \
	(!(is_ia32_St(n)      || \
	is_ia32_Store8Bit(n)  || \
	is_ia32_CondJmp(n)    || \
	is_ia32_fCondJmp(n)   || \
	is_ia32_SwitchJmp(n)))

	if (! buf) {
		buf = xcalloc(1, SNPRINTF_BUF_LEN);
	}
	else {
		memset(buf, 0, SNPRINTF_BUF_LEN);
	}

	switch(get_ia32_op_type(n)) {
		case ia32_Normal:
			if (get_ia32_cnst(n)) {
				lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%3S, %s", n, get_ia32_cnst(n));
			}
			else {
				const arch_register_t *in1 = get_in_reg(n, 2);
				const arch_register_t *in2 = get_in_reg(n, 3);
				const arch_register_t *out = PRODUCES_RESULT(n) ? get_out_reg(n, 0) : NULL;
				const arch_register_t *in;

				in  = out ? (REGS_ARE_EQUAL(out, in2) ? in1 : in2) : in2;
				out = out ? out : in1;

				snprintf(buf, SNPRINTF_BUF_LEN, "%%%s, %%%s", \
					arch_register_get_name(out), arch_register_get_name(in));
			}
			break;
		case ia32_AddrModeS:
			lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%4S, %s", n, ia32_emit_am(n, env));
			break;
		case ia32_AddrModeD:
			if (get_ia32_cnst(n)) {
				lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%s,%s%s",
					ia32_emit_am(n, env),
					get_ia32_sc(n) ? " OFFSET FLAT:" : " ",    /* In case of a symconst we must add OFFSET to */
					get_ia32_cnst(n));                         /* tell the assembler to store it's address.   */
			}
			else {
				const arch_register_t *in1 = get_in_reg(n, 2);
				ir_mode              *mode = get_ia32_res_mode(n);

				mode = mode ? mode : get_ia32_ls_mode(n);
				lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%s, %%%s",
					ia32_emit_am(n, env), ia32_get_reg_name_for_mode(env, mode, in1));
			}
			break;
		default:
			assert(0 && "unsupported op type");
	}

#undef PRODUCES_RESULT

	return buf;
}

/**
 * Emits registers and/or address mode of a unary operation.
 */
char *ia32_emit_unop(const ir_node *n, ia32_emit_env_t *env) {
	static char *buf = NULL;

	if (! buf) {
		buf = xcalloc(1, SNPRINTF_BUF_LEN);
	}
	else {
		memset(buf, 0, SNPRINTF_BUF_LEN);
	}

	switch(get_ia32_op_type(n)) {
		case ia32_Normal:
			lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%1D", n);
			break;
		case ia32_am_Dest:
			snprintf(buf, SNPRINTF_BUF_LEN, ia32_emit_am(n, env));
			break;
		default:
			assert(0 && "unsupported op type");
	}

	return buf;
}

/**
 * Emits address mode.
 */
char *ia32_emit_am(const ir_node *n, ia32_emit_env_t *env) {
	ia32_am_flavour_t am_flav    = get_ia32_am_flavour(n);
	int               had_output = 0;
	char             *s;
	int               size;
	static struct obstack *obst  = NULL;
	ir_mode *mode = get_ia32_ls_mode(n);

	if (! is_ia32_Lea(n))
		assert(mode && "AM node must have ls_mode attribute set.");

	if (! obst) {
		obst = xcalloc(1, sizeof(*obst));
	}
	else {
		obstack_free(obst, NULL);
	}

	/* obstack_free with NULL results in an uninitialized obstack */
	obstack_init(obst);

	if (mode) {
		switch (get_mode_size_bits(mode)) {
			case 8:
				obstack_printf(obst, "BYTE PTR ");
				break;
			case 16:
				obstack_printf(obst, "WORD PTR ");
				break;
			case 32:
				obstack_printf(obst, "DWORD PTR ");
				break;
			default:
				break;
		}
	}

	obstack_printf(obst, "[");

	if (am_flav & ia32_B) {
		lc_eoprintf(ia32_get_arg_env(), obst, "%1S", n);
		had_output = 1;
	}

	if (am_flav & ia32_I) {
		if (had_output) {
			obstack_printf(obst, "+");
		}

		lc_eoprintf(ia32_get_arg_env(), obst, "%2S", n);

		if (am_flav & ia32_S) {
			obstack_printf(obst, "*%d", 1 << get_ia32_am_scale(n));
		}

		had_output = 1;
	}

	if (am_flav & ia32_O) {
		obstack_printf(obst, get_ia32_am_offs(n));
	}

	obstack_printf(obst, "] ");

	size        = obstack_object_size(obst);
	s           = obstack_finish(obst);
	s[size - 1] = '\0';

	return s;
}



/**
 * Formated print of commands and comments.
 */
static void ia32_fprintf_format(FILE *F, char *cmd_buf, char *cmnt_buf) {
	fprintf(F, "\t%-35s %-60s\n", cmd_buf, cmnt_buf);
}



/**
 * Add a number to a prefix. This number will not be used a second time.
 */
static char *get_unique_label(char *buf, size_t buflen, const char *prefix) {
	static unsigned long id = 0;
	snprintf(buf, buflen, "%s%lu", prefix, ++id);
	return buf;
}



/*************************************************
 *                 _ _                         _
 *                (_) |                       | |
 *   ___ _ __ ___  _| |_    ___ ___  _ __   __| |
 *  / _ \ '_ ` _ \| | __|  / __/ _ \| '_ \ / _` |
 * |  __/ | | | | | | |_  | (_| (_) | | | | (_| |
 *  \___|_| |_| |_|_|\__|  \___\___/|_| |_|\__,_|
 *
 *************************************************/

#undef IA32_DO_EMIT
#define IA32_DO_EMIT ia32_fprintf_format(F, cmd_buf, cmnt_buf)

/*
 * coding of conditions
 */
struct cmp2conditon_t {
	const char *name;
	pn_Cmp      num;
};

/*
 * positive conditions for signed compares
 */
static const struct cmp2conditon_t cmp2condition_s[] = {
  { NULL,              pn_Cmp_False },  /* always false */
  { "e",               pn_Cmp_Eq },     /* == */
  { "l",               pn_Cmp_Lt },     /* < */
  { "le",              pn_Cmp_Le },     /* <= */
  { "g",               pn_Cmp_Gt },     /* > */
  { "ge",              pn_Cmp_Ge },     /* >= */
  { "ne",              pn_Cmp_Lg },     /* != */
  { "ordered",         pn_Cmp_Leg },    /* Floating point: ordered */
  { "unordered",       pn_Cmp_Uo },     /* FLoting point: unordered */
  { "unordered or ==", pn_Cmp_Ue },     /* Floating point: unordered or == */
  { "unordered or <",  pn_Cmp_Ul },     /* Floating point: unordered or < */
  { "unordered or <=", pn_Cmp_Ule },    /* Floating point: unordered or <= */
  { "unordered or >",  pn_Cmp_Ug },     /* Floating point: unordered or > */
  { "unordered or >=", pn_Cmp_Uge },    /* Floating point: unordered or >= */
  { "unordered or !=", pn_Cmp_Ne },     /* Floating point: unordered or != */
  { NULL,              pn_Cmp_True },   /* always true */
};

/*
 * positive conditions for unsigned compares
 */
static const struct cmp2conditon_t cmp2condition_u[] = {
	{ NULL,              pn_Cmp_False },  /* always false */
	{ "e",               pn_Cmp_Eq },     /* == */
	{ "b",               pn_Cmp_Lt },     /* < */
	{	"be",              pn_Cmp_Le },     /* <= */
	{ "a",               pn_Cmp_Gt },     /* > */
	{ "ae",              pn_Cmp_Ge },     /* >= */
	{ "ne",              pn_Cmp_Lg },     /* != */
	{ "ordered",         pn_Cmp_Leg },    /* Floating point: ordered */
	{ "unordered",       pn_Cmp_Uo },     /* FLoting point: unordered */
	{ "unordered or ==", pn_Cmp_Ue },     /* Floating point: unordered or == */
	{ "unordered or <",  pn_Cmp_Ul },     /* Floating point: unordered or < */
	{ "unordered or <=", pn_Cmp_Ule },    /* Floating point: unordered or <= */
	{ "unordered or >",  pn_Cmp_Ug },     /* Floating point: unordered or > */
	{ "unordered or >=", pn_Cmp_Uge },    /* Floating point: unordered or >= */
	{ "unordered or !=", pn_Cmp_Ne },     /* Floating point: unordered or != */
	{ NULL,              pn_Cmp_True },   /* always true */
};

/*
 * returns the condition code
 */
static const char *get_cmp_suffix(int cmp_code, int unsigned_cmp)
{
	assert(cmp2condition_s[cmp_code].num == cmp_code);
	assert(cmp2condition_u[cmp_code].num == cmp_code);

	return unsigned_cmp ? cmp2condition_u[cmp_code & 7].name : cmp2condition_s[cmp_code & 7].name;
}

/**
 * Returns the target block for a control flow node.
 */
static ir_node *get_cfop_target_block(const ir_node *irn) {
	return get_irn_link(irn);
}

/**
 * Returns the target label for a control flow node.
 */
static char *get_cfop_target(const ir_node *irn, char *buf) {
	ir_node *bl = get_cfop_target_block(irn);

	snprintf(buf, SNPRINTF_BUF_LEN, BLOCK_PREFIX("%ld"), get_irn_node_nr(bl));
	return buf;
}

/** Return the next block in Block schedule */
static ir_node *next_blk_sched(const ir_node *block) {
	return have_block_sched ? get_irn_link(block) : NULL;
}

/**
 * Emits the jump sequence for a conditional jump (cmp + jmp_true + jmp_false)
 */
static void finish_CondJmp(FILE *F, const ir_node *irn, ir_mode *mode) {
	const ir_node   *proj1, *proj2 = NULL;
	const ir_node   *block, *next_bl = NULL;
	const ir_edge_t *edge;
	char buf[SNPRINTF_BUF_LEN];
	char cmd_buf[SNPRINTF_BUF_LEN];
	char cmnt_buf[SNPRINTF_BUF_LEN];

	/* get both Proj's */
	edge = get_irn_out_edge_first(irn);
	proj1 = get_edge_src_irn(edge);
	assert(is_Proj(proj1) && "CondJmp with a non-Proj");

	edge = get_irn_out_edge_next(irn, edge);
	if (edge) {
		proj2 = get_edge_src_irn(edge);
		assert(is_Proj(proj2) && "CondJmp with a non-Proj");
	}

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(irn);
	if (proj2) {
		/* we have a block schedule */
		next_bl = next_blk_sched(block);

		if (get_cfop_target_block(proj1) == next_bl) {
			/* exchange both proj's so the second one can be omitted */
			const ir_node *t = proj1;
			proj1 = proj2;
			proj2 = t;
		}
	}

	/* the first Proj must always be created */
	if (get_Proj_proj(proj1) == pn_Cond_true) {
		snprintf(cmd_buf, SNPRINTF_BUF_LEN, "j%s %s",
					get_cmp_suffix(get_ia32_pncode(irn), !mode_is_signed(get_irn_mode(get_irn_n(irn, 0)))),
					get_cfop_target(proj1, buf));
		snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* cmp(a, b) == TRUE */");
	}
	else  {
		snprintf(cmd_buf, SNPRINTF_BUF_LEN, "j%s %s",
					get_cmp_suffix(get_negated_pnc(get_ia32_pncode(irn), mode),
					!mode_is_signed(get_irn_mode(get_irn_n(irn, 0)))),
					get_cfop_target(proj1, buf));
		snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* cmp(a, b) == FALSE */");
	}
	IA32_DO_EMIT;

	/* the second Proj might be a fallthrough */
	if (proj2) {
		if (get_cfop_target_block(proj2) != next_bl) {
			snprintf(cmd_buf, SNPRINTF_BUF_LEN, "jmp %s", get_cfop_target(proj2, buf));
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* otherwise */");
		}
		else {
			cmd_buf[0] = '\0';
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* fallthrogh %s */", get_cfop_target(proj2, buf));
		}
		IA32_DO_EMIT;
	}
}

/**
 * Emits code for conditional jump.
 */
static void CondJmp_emitter(const ir_node *irn, ia32_emit_env_t *env) {
	FILE *F = env->out;
	char cmd_buf[SNPRINTF_BUF_LEN];
	char cmnt_buf[SNPRINTF_BUF_LEN];

	snprintf(cmd_buf, SNPRINTF_BUF_LEN, "cmp %s", ia32_emit_binop(irn, env));
	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F */", irn);
	IA32_DO_EMIT;
	finish_CondJmp(F, irn, get_irn_mode(get_irn_n(irn, 2)));
}

/**
 * Emits code for conditional jump with two variables.
 */
static void emit_ia32_CondJmp(const ir_node *irn, ia32_emit_env_t *env) {
	CondJmp_emitter(irn, env);
}

/**
 * Emits code for conditional jump with immediate.
 */
void emit_ia32_CondJmp_i(const ir_node *irn, ia32_emit_env_t *env) {
	CondJmp_emitter(irn, env);
}

/**
 * Emits code for conditional test and jump.
 */
static void TestJmp_emitter(const ir_node *irn, ia32_emit_env_t *env) {
	FILE       *F   = env->out;
	const char *op1 = arch_register_get_name(get_in_reg(irn, 0));
	const char *op2 = get_ia32_cnst(irn);
	char        cmd_buf[SNPRINTF_BUF_LEN];
	char        cmnt_buf[SNPRINTF_BUF_LEN];

	if (! op2)
		op2 = arch_register_get_name(get_in_reg(irn, 1));

	snprintf(cmd_buf, SNPRINTF_BUF_LEN, "test %%%s,%s%s ", op1, get_ia32_cnst(irn) ? " " : " %", op2);
	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F */", irn);
	IA32_DO_EMIT;
	finish_CondJmp(F, irn, get_irn_mode(get_irn_n(irn, 0)));
}

/**
 * Emits code for conditional test and jump with two variables.
 */
static void emit_ia32_TestJmp(const ir_node *irn, ia32_emit_env_t *env) {
	TestJmp_emitter(irn, env);
}



/*********************************************************
 *                 _ _       _
 *                (_) |     (_)
 *   ___ _ __ ___  _| |_     _ _   _ _ __ ___  _ __  ___
 *  / _ \ '_ ` _ \| | __|   | | | | | '_ ` _ \| '_ \/ __|
 * |  __/ | | | | | | |_    | | |_| | | | | | | |_) \__ \
 *  \___|_| |_| |_|_|\__|   | |\__,_|_| |_| |_| .__/|___/
 *                         _/ |               | |
 *                        |__/                |_|
 *********************************************************/

/* jump table entry (target and corresponding number) */
typedef struct _branch_t {
	ir_node *target;
	int      value;
} branch_t;

/* jump table for switch generation */
typedef struct _jmp_tbl_t {
	ir_node  *defProj;         /**< default target */
	int       min_value;       /**< smallest switch case */
	int       max_value;       /**< largest switch case */
	int       num_branches;    /**< number of jumps */
	char     *label;           /**< label of the jump table */
	branch_t *branches;        /**< jump array */
} jmp_tbl_t;

/**
 * Compare two variables of type branch_t. Used to sort all switch cases
 */
static int ia32_cmp_branch_t(const void *a, const void *b) {
	branch_t *b1 = (branch_t *)a;
	branch_t *b2 = (branch_t *)b;

	if (b1->value <= b2->value)
		return -1;
	else
		return 1;
}

/**
 * Emits code for a SwitchJmp (creates a jump table if
 * possible otherwise a cmp-jmp cascade). Port from
 * cggg ia32 backend
 */
void emit_ia32_SwitchJmp(const ir_node *irn, ia32_emit_env_t *emit_env) {
	unsigned long       interval;
	char                buf[SNPRINTF_BUF_LEN];
	int                 last_value, i, pn, do_jmp_tbl = 1;
	jmp_tbl_t           tbl;
	ir_node            *proj;
	const ir_edge_t    *edge;
	const lc_arg_env_t *env = ia32_get_arg_env();
	FILE               *F   = emit_env->out;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	/* fill the table structure */
	tbl.label        = xmalloc(SNPRINTF_BUF_LEN);
	tbl.label        = get_unique_label(tbl.label, SNPRINTF_BUF_LEN, "JMPTBL_");
	tbl.defProj      = NULL;
	tbl.num_branches = get_irn_n_edges(irn);
	tbl.branches     = xcalloc(tbl.num_branches, sizeof(tbl.branches[0]));
	tbl.min_value    = INT_MAX;
	tbl.max_value    = INT_MIN;

	i = 0;
	/* go over all proj's and collect them */
	foreach_out_edge(irn, edge) {
		proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

		pn = get_Proj_proj(proj);

		/* create branch entry */
		tbl.branches[i].target = proj;
		tbl.branches[i].value  = pn;

		tbl.min_value = pn < tbl.min_value ? pn : tbl.min_value;
		tbl.max_value = pn > tbl.max_value ? pn : tbl.max_value;

		/* check for default proj */
		if (pn == get_ia32_pncode(irn)) {
			assert(tbl.defProj == NULL && "found two defProjs at SwitchJmp");
			tbl.defProj = proj;
		}

		i++;
	}

	/* sort the branches by their number */
	qsort(tbl.branches, tbl.num_branches, sizeof(tbl.branches[0]), ia32_cmp_branch_t);

	/* two-complement's magic make this work without overflow */
	interval = tbl.max_value - tbl.min_value;

	/* check value interval */
	if (interval > 16 * 1024) {
		do_jmp_tbl = 0;
	}

	/* check ratio of value interval to number of branches */
	if ((float)(interval + 1) / (float)tbl.num_branches > 8.0) {
		do_jmp_tbl = 0;
	}

	if (do_jmp_tbl) {
		/* emit the table */
		if (tbl.min_value != 0) {
			lc_esnprintf(env, cmd_buf, SNPRINTF_BUF_LEN, "cmpl %lu, -%d(%1S)",
				interval, tbl.min_value, irn);
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* first switch value is not 0 */");

			IA32_DO_EMIT;
		}
		else {
			lc_esnprintf(env, cmd_buf, SNPRINTF_BUF_LEN, "cmpl %lu, %1S", interval, irn);
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* compare for switch */");

			IA32_DO_EMIT;
		}

		snprintf(cmd_buf, SNPRINTF_BUF_LEN, "ja %s", get_cfop_target(tbl.defProj, buf));
		snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* default jump if out of range  */");
		IA32_DO_EMIT;

		if (tbl.num_branches > 1) {
			/* create table */

			lc_esnprintf(env, cmd_buf, SNPRINTF_BUF_LEN, "jmp [%1S*4+%s]", irn, tbl.label);
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* get jump table entry as target */");
			IA32_DO_EMIT;

			fprintf(F, "\t.section\t.rodata\n");
			fprintf(F, "\t.align 4\n");

			fprintf(F, "%s:\n", tbl.label);

			snprintf(cmd_buf, SNPRINTF_BUF_LEN, ".long %s", get_cfop_target(tbl.branches[0].target, buf));
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* case %d */\n",  tbl.branches[0].value);
			IA32_DO_EMIT;

			last_value = tbl.branches[0].value;
			for (i = 1; i < tbl.num_branches; ++i) {
				while (++last_value < tbl.branches[i].value) {
					snprintf(cmd_buf, SNPRINTF_BUF_LEN, ".long %s", get_cfop_target(tbl.defProj, buf));
					snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* default case */");
					IA32_DO_EMIT;
				}
				snprintf(cmd_buf, SNPRINTF_BUF_LEN, ".long %s", get_cfop_target(tbl.branches[i].target, buf));
				snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* case %d */", last_value);
				IA32_DO_EMIT;
			}

			fprintf(F, "\t.text");
		}
		else {
			/* one jump is enough */
			snprintf(cmd_buf, SNPRINTF_BUF_LEN, "jmp %s", get_cfop_target(tbl.branches[0].target, buf));
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* only one case given */");
			IA32_DO_EMIT;
		}
	}
	else { // no jump table
		for (i = 0; i < tbl.num_branches; ++i) {
			lc_esnprintf(env, cmd_buf, SNPRINTF_BUF_LEN, "cmpl %d, %1S", tbl.branches[i].value, irn);
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* case %d */", i);
			IA32_DO_EMIT;
			fprintf(F, "\tje %s\n", get_cfop_target(tbl.branches[i].target, buf));
		}

		snprintf(cmd_buf, SNPRINTF_BUF_LEN, "jmp %s", get_cfop_target(tbl.defProj, buf));
		snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* default case */");
		IA32_DO_EMIT;
	}

	if (tbl.label)
		free(tbl.label);
	if (tbl.branches)
		free(tbl.branches);
}

/**
 * Emits code for a unconditional jump.
 */
void emit_Jmp(const ir_node *irn, ia32_emit_env_t *env) {
	ir_node *block, *next_bl;
	FILE *F = env->out;
	char buf[SNPRINTF_BUF_LEN], cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(irn);

	/* we have a block schedule */
	next_bl = next_blk_sched(block);
	if (get_cfop_target_block(irn) != next_bl) {
		snprintf(cmd_buf, SNPRINTF_BUF_LEN, "jmp %s", get_cfop_target(irn, buf));
		lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F(%+F) */", irn, get_cfop_target_block(irn));
	}
	else {
		cmd_buf[0] = '\0';
		lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* fallthrough %s */", get_cfop_target(irn, buf));
	}
	IA32_DO_EMIT;
}

/****************************
 *                  _
 *                 (_)
 *  _ __  _ __ ___  _  ___
 * | '_ \| '__/ _ \| |/ __|
 * | |_) | | | (_) | |\__ \
 * | .__/|_|  \___/| ||___/
 * | |            _/ |
 * |_|           |__/
 ****************************/

/**
 * Emits code for a proj -> node
 */
void emit_Proj(const ir_node *irn, ia32_emit_env_t *env) {
	ir_node *pred = get_Proj_pred(irn);

	if (get_irn_op(pred) == op_Start) {
		switch(get_Proj_proj(irn)) {
			case pn_Start_X_initial_exec:
				emit_Jmp(irn, env);
				break;
			default:
				break;
		}
	}
}

/**********************************
 *   _____                  ____
 *  / ____|                |  _ \
 * | |     ___  _ __  _   _| |_) |
 * | |    / _ \| '_ \| | | |  _ <
 * | |___| (_) | |_) | |_| | |_) |
 *  \_____\___/| .__/ \__, |____/
 *             | |     __/ |
 *             |_|    |___/
 **********************************/

/**
 * Emit movsb/w instructions to make mov count divideable by 4
 */
static void emit_CopyB_prolog(FILE *F, int rem, int size) {
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	fprintf(F, "\t/* memcopy %d bytes*/\n", size);

	snprintf(cmd_buf, SNPRINTF_BUF_LEN, "cld");
	snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* copy direction forward*/");
	IA32_DO_EMIT;

	switch(rem) {
		case 1:
			snprintf(cmd_buf, SNPRINTF_BUF_LEN, "movsb");
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* memcopy remainder 1 */");
			break;
		case 2:
			snprintf(cmd_buf, SNPRINTF_BUF_LEN, "movsw");
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* memcopy remainder 2 */");
			break;
		case 3:
			snprintf(cmd_buf, SNPRINTF_BUF_LEN, "movsb");
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* memcopy remainder 3 */");
			IA32_DO_EMIT;
			snprintf(cmd_buf, SNPRINTF_BUF_LEN, "movsw");
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* memcopy remainder 3 */");
			break;
	}

	IA32_DO_EMIT;
}

/**
 * Emit rep movsd instruction for memcopy.
 */
void emit_ia32_CopyB(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE   *F    = emit_env->out;
	tarval *tv   = get_ia32_Immop_tarval(irn);
	int     rem  = get_tarval_long(tv);
	int     size = get_tarval_long(get_ia32_Immop_tarval(get_irn_n(irn, 2)));
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	emit_CopyB_prolog(F, rem, size);

	snprintf(cmd_buf, SNPRINTF_BUF_LEN, "rep movsd");
	snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* memcopy */");
	IA32_DO_EMIT;
}

/**
 * Emits unrolled memcopy.
 */
void emit_ia32_CopyB_i(const ir_node *irn, ia32_emit_env_t *emit_env) {
	tarval *tv   = get_ia32_Immop_tarval(irn);
	int     size = get_tarval_long(tv);
	FILE   *F    = emit_env->out;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	emit_CopyB_prolog(F, size & 0x3, size);

	size >>= 2;
	while (size--) {
		snprintf(cmd_buf, SNPRINTF_BUF_LEN, "movsd");
		snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* memcopy unrolled */");
		IA32_DO_EMIT;
	}
}



/***************************
 *   _____
 *  / ____|
 * | |     ___  _ ____   __
 * | |    / _ \| '_ \ \ / /
 * | |___| (_) | | | \ V /
 *  \_____\___/|_| |_|\_/
 *
 ***************************/

/**
 * Emit code for conversions (I, FP), (FP, I) and (FP, FP).
 */
static void emit_ia32_Conv_with_FP(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE               *F    = emit_env->out;
	const lc_arg_env_t *env  = ia32_get_arg_env();
	char               *from, *to, buf[64];
	ir_mode *src_mode, *tgt_mode;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	src_mode = is_ia32_AddrModeS(irn) ? get_ia32_ls_mode(irn) : get_irn_mode(get_irn_n(irn, 2));
	tgt_mode = get_ia32_res_mode(irn);

	from = mode_is_float(src_mode) ? (get_mode_size_bits(src_mode) == 32 ? "ss" : "sd") : "si";
	to   = mode_is_float(tgt_mode) ? (get_mode_size_bits(tgt_mode) == 32 ? "ss" : "sd") : "si";

	switch(get_ia32_op_type(irn)) {
		case ia32_Normal:
			lc_esnprintf(env, buf, sizeof(buf), "%1D, %3S", irn, irn);
			break;
		case ia32_AddrModeS:
			lc_esnprintf(env, buf, sizeof(buf), "%1D, %s", irn, ia32_emit_am(irn, emit_env));
			break;
		default:
			assert(0 && "unsupported op type for Conv");
	}

	snprintf(cmd_buf, SNPRINTF_BUF_LEN, "cvt%s2%s %s", from, to, buf);
	lc_esnprintf(env, cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F(%+F, %+F) */", irn, src_mode, tgt_mode);
	IA32_DO_EMIT;
}

void emit_ia32_Conv_I2FP(const ir_node *irn, ia32_emit_env_t *emit_env) {
	emit_ia32_Conv_with_FP(irn, emit_env);
}

void emit_ia32_Conv_FP2I(const ir_node *irn, ia32_emit_env_t *emit_env) {
	emit_ia32_Conv_with_FP(irn, emit_env);
}

void emit_ia32_Conv_FP2FP(const ir_node *irn, ia32_emit_env_t *emit_env) {
	emit_ia32_Conv_with_FP(irn, emit_env);
}

/**
 * Emits code for an Int conversion.
 */
void emit_ia32_Conv_I2I(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE               *F    = emit_env->out;
	const lc_arg_env_t *env  = ia32_get_arg_env();
	char *move_cmd, *conv_cmd;
	ir_mode *src_mode, *tgt_mode;
	int n, m;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];
	const arch_register_t *in_reg, *out_reg;

	src_mode = is_ia32_AddrModeS(irn) ? get_ia32_ls_mode(irn) : get_irn_mode(get_irn_n(irn, 2));
	tgt_mode = get_ia32_res_mode(irn);

	n = get_mode_size_bits(src_mode);
	m = get_mode_size_bits(tgt_mode);

	if (mode_is_signed(n < m ? src_mode : tgt_mode)) {
		move_cmd = "movsx";
		if (n == 8 || m == 8)
			conv_cmd = "cbw";
		else if (n == 16 || m == 16)
			conv_cmd = "cwde";
		else
			assert(0 && "unsupported Conv_I2I");
	}
	else {
		move_cmd = "movzx";
		conv_cmd = NULL;
	}

	switch(get_ia32_op_type(irn)) {
		case ia32_Normal:
			in_reg  = get_in_reg(irn, 2);
			out_reg = get_out_reg(irn, 0);

			if (REGS_ARE_EQUAL(in_reg, &ia32_gp_regs[REG_EAX]) &&
				REGS_ARE_EQUAL(out_reg, in_reg)                &&
				mode_is_signed(n < m ? src_mode : tgt_mode))
			{
				/* argument and result are both in EAX and */
				/* signedness is ok: -> use converts       */
				lc_esnprintf(env, cmd_buf, SNPRINTF_BUF_LEN, "%s", conv_cmd);
			}
			else if (REGS_ARE_EQUAL(out_reg, in_reg) &&
				! mode_is_signed(n < m ? src_mode : tgt_mode))
			{
				/* argument and result are in the same register */
				/* and signedness is ok: -> use and with mask   */
				int mask = (1 << (n < m ? n : m)) - 1;
				lc_esnprintf(env, cmd_buf, SNPRINTF_BUF_LEN, "and %1D, 0x%x", irn, mask);
			}
			else {
				/* use move w/o sign extension */
				lc_esnprintf(env, cmd_buf, SNPRINTF_BUF_LEN, "%s %1D, %%%s",
					move_cmd, irn, ia32_get_reg_name_for_mode(emit_env, n < m ? src_mode : tgt_mode, in_reg));
			}

			break;
		case ia32_AddrModeS:
			lc_esnprintf(env, cmd_buf, SNPRINTF_BUF_LEN, "%s %1D, %s",
				move_cmd, irn, ia32_emit_am(irn, emit_env));
			break;
		default:
			assert(0 && "unsupported op type for Conv");
	}

	lc_esnprintf(env, cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F(%d Bit mode_%F -> %d Bit mode_%F) */",
		irn, n, src_mode, m, tgt_mode);

	IA32_DO_EMIT;
}

/*******************************************
 *  _                          _
 * | |                        | |
 * | |__   ___ _ __   ___   __| | ___  ___
 * | '_ \ / _ \ '_ \ / _ \ / _` |/ _ \/ __|
 * | |_) |  __/ | | | (_) | (_| |  __/\__ \
 * |_.__/ \___|_| |_|\___/ \__,_|\___||___/
 *
 *******************************************/

/**
 * Emits a backend call
 */
void emit_be_Call(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	entity *ent = be_Call_get_entity(irn);
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	if (ent) {
		snprintf(cmd_buf, SNPRINTF_BUF_LEN, "call %s", get_entity_name(ent));
	}
	else {
		lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "%1D", get_irn_n(irn, be_pos_Call_ptr));
	}

	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F (be_Call) */", irn);

	IA32_DO_EMIT;
}

/**
 * Emits code to increase stack pointer.
 */
void emit_be_IncSP(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE          *F    = emit_env->out;
	unsigned       offs = be_get_IncSP_offset(irn);
	be_stack_dir_t dir  = be_get_IncSP_direction(irn);
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	if (offs) {
		lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "add %1S,%s%u", irn,
			(dir == be_stack_dir_along) ? " -" : " ", offs);
		lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F (IncSP) */", irn);
	}
	else {
		snprintf(cmd_buf, SNPRINTF_BUF_LEN, " ");
		lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* omitted %+F (IncSP) with 0 */", irn);
	}

	IA32_DO_EMIT;
}

/**
 * Emits code to set stack pointer.
 */
void emit_be_SetSP(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "mov %1D, %3S", irn, irn);
	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F (restore SP) */", irn);
	IA32_DO_EMIT;
}

/**
 * Emits code for Copy.
 */
void emit_be_Copy(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "mov %1D, %1S", irn, irn);
	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F */", irn);
	IA32_DO_EMIT;
}

/**
 * Emits code for exchange.
 */
void emit_be_Perm(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "xchg %1S, %2S", irn, irn);
	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F(%1A, %2A) */", irn, irn, irn);
	IA32_DO_EMIT;
}

/***********************************************************************************
 *                  _          __                                             _
 *                 (_)        / _|                                           | |
 *  _ __ ___   __ _ _ _ __   | |_ _ __ __ _ _ __ ___   _____      _____  _ __| | __
 * | '_ ` _ \ / _` | | '_ \  |  _| '__/ _` | '_ ` _ \ / _ \ \ /\ / / _ \| '__| |/ /
 * | | | | | | (_| | | | | | | | | | | (_| | | | | | |  __/\ V  V / (_) | |  |   <
 * |_| |_| |_|\__,_|_|_| |_| |_| |_|  \__,_|_| |_| |_|\___| \_/\_/ \___/|_|  |_|\_\
 *
 ***********************************************************************************/

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void ia32_register_emitters(void) {

#define IA32_EMIT(a) op_ia32_##a->ops.generic = (op_func)emit_ia32_##a
#define EMIT(a)      op_##a->ops.generic = (op_func)emit_##a
#define BE_EMIT(a)   op_be_##a->ops.generic = (op_func)emit_be_##a

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	ia32_register_spec_emitters();

	/* other ia32 emitter functions */
	IA32_EMIT(CondJmp);
	IA32_EMIT(TestJmp);
	IA32_EMIT(SwitchJmp);
	IA32_EMIT(CopyB);
	IA32_EMIT(CopyB_i);
	IA32_EMIT(Conv_I2FP);
	IA32_EMIT(Conv_FP2I);
	IA32_EMIT(Conv_FP2FP);
	IA32_EMIT(Conv_I2I);

	/* benode emitter */
	BE_EMIT(Call);
	BE_EMIT(IncSP);
	BE_EMIT(SetSP);
	BE_EMIT(Copy);
	BE_EMIT(Perm);

	/* firm emitter */
	EMIT(Jmp);
	EMIT(Proj);

#undef IA32_EMIT
#undef BE_EMIT
#undef EMIT
}

/**
 * Emits code for a node.
 */
static void ia32_emit_node(const ir_node *irn, void *env) {
	ia32_emit_env_t        *emit_env = env;
	firm_dbg_module_t *mod      = emit_env->mod;
	FILE              *F        = emit_env->out;
	ir_op             *op       = get_irn_op(irn);

	DBG((mod, LEVEL_1, "emitting code for %+F\n", irn));

	if (op->ops.generic) {
		void (*emit)(const ir_node *, void *) = (void (*)(const ir_node *, void *))op->ops.generic;
		(*emit)(irn, env);
	}
	else {
		ir_fprintf(F, "\t%35s /* %+F */\n", " ", irn);
	}
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void ia32_gen_block(ir_node *block, void *env) {
	const ir_node *irn;

	if (! is_Block(block))
		return;

	fprintf(((ia32_emit_env_t *)env)->out, BLOCK_PREFIX("%ld:\n"), get_irn_node_nr(block));
	sched_foreach(block, irn) {
		ia32_emit_node(irn, env);
	}
}

/**
 * Emits code for function start.
 */
static void ia32_emit_func_prolog(FILE *F, ir_graph *irg) {
	entity     *irg_ent  = get_irg_entity(irg);
	const char *irg_name = get_entity_name(irg_ent);

	fprintf(F, "\t.text\n");
	if (get_entity_visibility(irg_ent) == visibility_external_visible) {
		fprintf(F, ".globl %s\n", irg_name);
	}
	fprintf(F, "\t.type\t%s, @function\n", irg_name);
	fprintf(F, "%s:\n", irg_name);
}

/**
 * Emits code for function end
 */
static void ia32_emit_func_epilog(FILE *F, ir_graph *irg) {
	const char *irg_name = get_entity_name(get_irg_entity(irg));

	fprintf(F, "\tret\n");
	fprintf(F, "\t.size\t%s, .-%s\n\n", irg_name, irg_name);
}

/**
 * Block-walker:
 * Sets labels for control flow nodes (jump target)
 * TODO: Jump optimization
 */
static void ia32_gen_labels(ir_node *block, void *env) {
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block);
	}
}

typedef struct {
	ir_node *start;
	ir_node *end;
} anchor;

/**
 * Ext-Block walker: create a block schedule
 */
static void create_block_list(ir_extblk *blk, void *env) {
	anchor *list = env;
	int i, n;

	for (i = 0, n = get_extbb_n_blocks(blk); i < n; ++i) {
		ir_node *block = get_extbb_block(blk, i);

		set_irn_link(block, NULL);
		if (list->start)
			set_irn_link(list->end, block);
		else
			list->start = block;

		list->end = block;
	}
}

/**
 * Main driver. Emits the code for one routine.
 */
void ia32_gen_routine(FILE *F, ir_graph *irg, const ia32_code_gen_t *cg) {
	ia32_emit_env_t emit_env;
	anchor list;
	ir_node *block;

	emit_env.mod      = firm_dbg_register("firm.be.ia32.emitter");
	emit_env.out      = F;
	emit_env.arch_env = cg->arch_env;
	emit_env.cg       = cg;
	emit_env.isa      = (ia32_isa_t *)cg->arch_env->isa;

	/* set the global arch_env (needed by print hooks) */
	arch_env = cg->arch_env;

	ia32_register_emitters();

	ia32_emit_func_prolog(F, irg);
	irg_block_walk_graph(irg, ia32_gen_labels, NULL, &emit_env);

	if (cg->opt.extbb) {
		/* schedule extended basic blocks */

		compute_extbb(irg);

		list.start = NULL;
		list.end   = NULL;
		irg_extblock_walk_graph(irg, NULL, create_block_list, &list);

		have_block_sched = 1;
		for (block = list.start; block; block = get_irn_link(block))
			ia32_gen_block(block, &emit_env);
	}
	else {
		/* "normal" block schedule */

		have_block_sched = 0;
		irg_walk_blkwise_graph(irg, NULL, ia32_gen_block, &emit_env);
	}

	ia32_emit_func_epilog(F, irg);
}
