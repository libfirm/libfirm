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
#include "bearch_ia32_t.h"

#define BLOCK_PREFIX(x) ".L" x

#define SNPRINTF_BUF_LEN 128

/* global arch_env for lc_printf functions */
static const arch_env_t *arch_env = NULL;

/** by default, we generate assembler code for the Linux gas */
asm_flavour_t asm_flavour = ASM_LINUX_GAS;

/**
 * Switch to a new section
 */
void ia32_switch_section(FILE *F, section_t sec) {
	static section_t curr_sec = NO_SECTION;
	static const char *text[ASM_MAX][SECTION_MAX] = {
		{
			".section\t.text", ".section\t.data", ".section\t.rodata", ".section\t.text"
		},
		{
			".section\t.text", ".section\t.data", ".section .rdata,\"dr\"", ".section\t.text"
		}
	};

	if (curr_sec == sec)
		return;

	curr_sec = sec;
	switch (sec) {

	case NO_SECTION:
		break;

	case SECTION_TEXT:
	case SECTION_DATA:
	case SECTION_RODATA:
	case SECTION_COMMON:
		fprintf(F, "\t%s\n", text[asm_flavour][sec]);
	}
}

static void ia32_dump_function_object(FILE *F, const char *name)
{
	switch (asm_flavour) {
	case ASM_LINUX_GAS:
		fprintf(F, "\t.type\t%s, @function\n", name);
		break;
	case ASM_MINGW_GAS:
		fprintf(F, "\t.def\t%s;\t.scl\t2;\t.type\t32;\t.endef\n", name);
		break;
	}
}

static void ia32_dump_function_size(FILE *F, const char *name)
{
	switch (asm_flavour) {
	case ASM_LINUX_GAS:
		fprintf(F, "\t.size\t%s, .-%s\n", name, name);
		break;
	}
}

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

/**
 * returns true if a node has x87 registers
 */
static int has_x87_register(const ir_node *n) {
	return is_irn_machine_user(n, 0);
}

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

	/* in case of unknown: just return a register */
	if (REGS_ARE_EQUAL(reg, &ia32_gp_regs[REG_GP_UKNWN]))
		reg = &ia32_gp_regs[REG_EAX];
	else if (REGS_ARE_EQUAL(reg, &ia32_xmm_regs[REG_XMM_UKNWN]))
		reg = &ia32_xmm_regs[REG_XMM0];
	else if (REGS_ARE_EQUAL(reg, &ia32_vfp_regs[REG_VFP_UKNWN]))
		reg = &ia32_vfp_regs[REG_VF0];

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

	if (in_out == IN_REG) {
		reg = get_in_reg(irn, pos);

		if (reg->reg_class == &ia32_reg_classes[CLASS_ia32_vfp]) {
			/* FIXME: works for binop only */
			assert(2 <= pos && pos <= 3);
			reg = get_ia32_attr(irn)->x87[pos - 2];
		}
	}
	else {
		/* destination address mode nodes don't have outputs */
		if (is_ia32_irn(irn) && get_ia32_op_type(irn) == ia32_AddrModeD) {
			return "MEM";
		}

		reg = get_out_reg(irn, pos);
		if (reg->reg_class == &ia32_reg_classes[CLASS_ia32_vfp])
			reg = get_ia32_attr(irn)->x87[pos + 2];
	}
	return arch_register_get_name(reg);
}

/**
 * Get the register name for a node.
 */
static int ia32_get_reg_name(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	const char *buf;
	ir_node    *irn = arg->v_ptr;
	int         nr = occ->width - 1;

	if (! irn)
		return lc_appendable_snadd(app, "(null)", 6);

	buf = get_ia32_reg_name(irn, nr, occ->conversion == 'S' ? IN_REG : OUT_REG);

	/* append the stupid % to register names */
	lc_appendable_chadd(app, '%');
	return lc_appendable_snadd(app, buf, strlen(buf));
}

/**
 * Get the x87 register name for a node.
 */
static int ia32_get_x87_name(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	const char *buf;
	ir_node     *irn = arg->v_ptr;
	int         nr = occ->width - 1;
	ia32_attr_t *attr;

	if (! irn)
		return lc_appendable_snadd(app, "(null)", 6);

	attr = get_ia32_attr(irn);
	buf = attr->x87[nr]->name;
	lc_appendable_chadd(app, '%');
	return lc_appendable_snadd(app, buf, strlen(buf));
}

/**
 * Returns the tarval, offset or scale of an ia32 as a string.
 */
static int ia32_const_to_str(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	const char *buf;
	ir_node    *irn = arg->v_ptr;

	if (! irn)
		return lc_arg_append(app, occ, "(null)", 6);

	if (occ->conversion == 'C') {
		buf = get_ia32_cnst(irn);
	}
	else { /* 'O' */
		buf = get_ia32_am_offs(irn);
	}

	return buf ? lc_appendable_snadd(app, buf, strlen(buf)) : 0;
}

/**
 * Determines the SSE suffix depending on the mode.
 */
static int ia32_get_mode_suffix(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	ir_node *irn  = arg->v_ptr;
	ir_mode *mode = get_irn_mode(irn);

	if (mode == mode_T) {
		mode = (is_ia32_Ld(irn) || is_ia32_St(irn)) ? get_ia32_ls_mode(irn) : get_ia32_res_mode(irn);
	}

	if (! irn)
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
	static const lc_arg_handler_t ia32_x87_handler   = { ia32_get_arg_type, ia32_get_x87_name };

	if(env == NULL) {
		/* extend the firm printer */
		env = firm_get_arg_env();

		lc_arg_register(env, "ia32:sreg", 'S', &ia32_reg_handler);
		lc_arg_register(env, "ia32:dreg", 'D', &ia32_reg_handler);
		lc_arg_register(env, "ia32:cnst", 'C', &ia32_const_handler);
		lc_arg_register(env, "ia32:offs", 'O', &ia32_const_handler);
		lc_arg_register(env, "ia32:mode", 'M', &ia32_mode_handler);
		lc_arg_register(env, "ia32:x87",  'X', &ia32_x87_handler);
	}

	return env;
}

static char *ia32_get_reg_name_for_mode(ia32_emit_env_t *env, ir_mode *mode, const arch_register_t *reg) {
	switch(get_mode_size_bits(mode)) {
		case 8:
			return ia32_get_mapped_reg_name(env->isa->regs_8bit, reg);
		case 16:
			return ia32_get_mapped_reg_name(env->isa->regs_16bit, reg);
		default:
			return (char *)arch_register_get_name(reg);
	}
}

/**
 * Emits registers and/or address mode of a binary operation.
 */
const char *ia32_emit_binop(const ir_node *n, ia32_emit_env_t *env) {
	static char *buf = NULL;

	/* verify that this function is never called on non-AM supporting operations */
	//assert(get_ia32_am_support(n) != ia32_am_None && "emit binop expects addressmode support");

#define PRODUCES_RESULT(n)   \
	(!(is_ia32_St(n)      || \
	is_ia32_Store8Bit(n)  || \
	is_ia32_CondJmp(n)    || \
	is_ia32_xCondJmp(n)   || \
	is_ia32_SwitchJmp(n)))

	if (! buf) {
		buf = xcalloc(1, SNPRINTF_BUF_LEN);
	}
	else {
		memset(buf, 0, SNPRINTF_BUF_LEN);
	}

	switch(get_ia32_op_type(n)) {
		case ia32_Normal:
			if (is_ia32_ImmConst(n) || is_ia32_ImmSymConst(n)) {
				lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%3S, %s", n, get_ia32_cnst(n));
			}
			else {
				const arch_register_t *in1 = get_in_reg(n, 2);
				const arch_register_t *in2 = get_in_reg(n, 3);
				const arch_register_t *out = PRODUCES_RESULT(n) ? get_out_reg(n, 0) : NULL;
				const arch_register_t *in;
				const char            *in_name;

				in      = out ? (REGS_ARE_EQUAL(out, in2) ? in1 : in2) : in2;
				out     = out ? out : in1;
				in_name = arch_register_get_name(in);

				if (is_ia32_emit_cl(n)) {
					assert(REGS_ARE_EQUAL(&ia32_gp_regs[REG_ECX], in) && "shift operation needs ecx");
					in_name = "cl";
				}

				snprintf(buf, SNPRINTF_BUF_LEN, "%%%s, %%%s", arch_register_get_name(out), in_name);
			}
			break;
		case ia32_AddrModeS:
			if (is_ia32_ImmConst(n) || is_ia32_ImmSymConst(n)) {
				assert(! PRODUCES_RESULT(n) && "Source AM with Const must not produce result");
				snprintf(buf, SNPRINTF_BUF_LEN, "%s, %s", get_ia32_cnst(n), ia32_emit_am(n, env));
			}
			else {
				if (PRODUCES_RESULT(n)) {
					lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%1D, %s", n, ia32_emit_am(n, env));
				}
				else {
					lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%3S, %s", n, ia32_emit_am(n, env));
				}
			}
			break;
		case ia32_AddrModeD:
			if (is_ia32_ImmConst(n) || is_ia32_ImmSymConst(n)) {
				lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%s,%s%s",
					ia32_emit_am(n, env),
					is_ia32_ImmSymConst(n) ? " OFFSET FLAT:" : " ",  /* In case of a symconst we must add OFFSET to */
					get_ia32_cnst(n));                               /* tell the assembler to store it's address.   */
			}
			else {
				const arch_register_t *in1 = get_in_reg(n, 2);
				ir_mode              *mode = get_ia32_res_mode(n);
				const char           *in_name;

				mode    = mode ? mode : get_ia32_ls_mode(n);
				in_name = ia32_get_reg_name_for_mode(env, mode, in1);

				if (is_ia32_emit_cl(n)) {
					assert(REGS_ARE_EQUAL(&ia32_gp_regs[REG_ECX], in1) && "shift operation needs ecx");
					in_name = "cl";
				}

				lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%s, %%%s", ia32_emit_am(n, env), in_name);
			}
			break;
		default:
			assert(0 && "unsupported op type");
	}

#undef PRODUCES_RESULT

	return buf;
}

/**
 * Returns the xxx PTR string for a given mode
 *
 * @param mode      the mode
 * @param x87_insn  if non-zero returns the string for a x87 instruction
 *                  else for a SSE instruction
 */
static const char *pointer_size(ir_mode *mode, int x87_insn)
{
	if (mode) {
		switch (get_mode_size_bits(mode)) {
		case 8:  return "BYTE PTR";
		case 16: return "WORD PTR";
		case 32: return "DWORD PTR";
		case 64:
			if (x87_insn)
				return "QWORD PTR";
			return NULL;
		case 80:
		case 96: return "XWORD PTR";
		default: return NULL;
		}
	}
	return NULL;
}

/**
 * Emits registers and/or address mode of a binary operation.
 */
const char *ia32_emit_x87_binop(const ir_node *n, ia32_emit_env_t *env) {
	static char *buf = NULL;

	/* verify that this function is never called on non-AM supporting operations */
	//assert(get_ia32_am_support(n) != ia32_am_None && "emit binop expects addressmode support");

	if (! buf) {
		buf = xcalloc(1, SNPRINTF_BUF_LEN);
	}
	else {
		memset(buf, 0, SNPRINTF_BUF_LEN);
	}

	switch(get_ia32_op_type(n)) {
		case ia32_Normal:
			if (is_ia32_ImmConst(n) || is_ia32_ImmSymConst(n)) {
				ir_mode *mode = get_ia32_ls_mode(n);
				const char *p = pointer_size(mode, 1);
				lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%s %s", p, get_ia32_cnst(n));
			}
			else {
				ia32_attr_t *attr = get_ia32_attr(n);
				const arch_register_t *in1 = attr->x87[0];
				const arch_register_t *in2 = attr->x87[1];
				const arch_register_t *out = attr->x87[2];
				const arch_register_t *in;
				const char            *in_name;

				in      = out ? (REGS_ARE_EQUAL(out, in2) ? in1 : in2) : in2;
				out     = out ? out : in1;
				in_name = arch_register_get_name(in);

				snprintf(buf, SNPRINTF_BUF_LEN, "%%%s, %%%s", arch_register_get_name(out), in_name);
			}
			break;
		case ia32_AddrModeS:
		case ia32_AddrModeD:
			lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%s", ia32_emit_am(n, env));
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
const char *ia32_emit_unop(const ir_node *n, ia32_emit_env_t *env) {
	static char *buf = NULL;

	if (! buf) {
		buf = xcalloc(1, SNPRINTF_BUF_LEN);
	}
	else {
		memset(buf, 0, SNPRINTF_BUF_LEN);
	}

	switch(get_ia32_op_type(n)) {
		case ia32_Normal:
			if (is_ia32_ImmConst(n) || is_ia32_ImmSymConst(n)) {
				lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%C", n);
			}
			else {
				lc_esnprintf(ia32_get_arg_env(), buf, SNPRINTF_BUF_LEN, "%1D", n);
			}
			break;
		case ia32_AddrModeD:
			snprintf(buf, SNPRINTF_BUF_LEN, "%s", ia32_emit_am(n, env));
			break;
		default:
			assert(0 && "unsupported op type");
	}

	return buf;
}

/**
 * Emits address mode.
 */
const char *ia32_emit_am(const ir_node *n, ia32_emit_env_t *env) {
	ia32_am_flavour_t am_flav    = get_ia32_am_flavour(n);
	int               had_output = 0;
	char              *s;
	const char        *p;
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

	p = pointer_size(mode, has_x87_register(n));
	if (p)
		obstack_printf(obst, "%s ", p);

	/* emit address mode symconst */
	if (get_ia32_am_sc(n)) {
		if (is_ia32_am_sc_sign(n))
			obstack_printf(obst, "-");
		obstack_printf(obst, "%s", get_id_str(get_ia32_am_sc(n)));
	}

	if (am_flav & ia32_B) {
		obstack_printf(obst, "[");
		lc_eoprintf(ia32_get_arg_env(), obst, "%1S", n);
		had_output = 1;
	}

	if (am_flav & ia32_I) {
		if (had_output) {
			obstack_printf(obst, "+");
		}
		else {
			obstack_printf(obst, "[");
		}

		lc_eoprintf(ia32_get_arg_env(), obst, "%2S", n);

		if (am_flav & ia32_S) {
			obstack_printf(obst, "*%d", 1 << get_ia32_am_scale(n));
		}

		had_output = 1;
	}

	if (am_flav & ia32_O) {
		s = get_ia32_am_offs(n);

		if (s) {
			/* omit explicit + if there was no base or index */
			if (! had_output) {
				obstack_printf(obst, "[");
				if (s[0] == '+')
					s++;
			}

			obstack_printf(obst, s);
			had_output = 1;
		}
	}

	if (had_output)
		obstack_printf(obst, "] ");

	size        = obstack_object_size(obst);
	s           = obstack_finish(obst);
	s[size - 1] = '\0';

	return s;
}

/**
 * emit an address
 */
const char *ia32_emit_adr(const ir_node *irn, ia32_emit_env_t *env)
{
	static char buf[SNPRINTF_BUF_LEN];
	ir_mode    *mode = get_ia32_ls_mode(irn);
	const char *adr  = get_ia32_cnst(irn);
	const char *pref = pointer_size(mode, has_x87_register(irn));

	snprintf(buf, SNPRINTF_BUF_LEN, "%s %s", pref ? pref : "", adr);
	return buf;
}

/**
 * Formated print of commands and comments.
 */
static void ia32_fprintf_format(FILE *F, const ir_node *irn, char *cmd_buf, char *cmnt_buf) {
	unsigned lineno;
	const char *name = irn ? be_retrieve_dbg_info(get_irn_dbg_info((ir_node *)irn), &lineno) : NULL;

	if (name)
		fprintf(F, "\t%-35s %-60s /* %s:%u */\n", cmd_buf, cmnt_buf, name, lineno);
	else
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
#define IA32_DO_EMIT(irn) ia32_fprintf_format(F, irn, cmd_buf, cmnt_buf)

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
	{ "be",              pn_Cmp_Le },     /* <= */
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
	return get_irn_link(block);
}

/**
 * Returns the Proj with projection number proj and NOT mode_M
 */
static ir_node *get_proj(const ir_node *irn, long proj) {
	const ir_edge_t *edge;
	ir_node         *src;

	assert(get_irn_mode(irn) == mode_T && "expected mode_T node");

	foreach_out_edge(irn, edge) {
		src = get_edge_src_irn(edge);

		assert(is_Proj(src) && "Proj expected");
		if (get_irn_mode(src) == mode_M)
			continue;

		if (get_Proj_proj(src) == proj)
			return src;
	}
	return NULL;
}

/**
 * Emits the jump sequence for a conditional jump (cmp + jmp_true + jmp_false)
 */
static void finish_CondJmp(FILE *F, const ir_node *irn, ir_mode *mode) {
	const ir_node   *proj1, *proj2 = NULL;
	const ir_node   *block, *next_bl = NULL;
	char buf[SNPRINTF_BUF_LEN];
	char cmd_buf[SNPRINTF_BUF_LEN];
	char cmnt_buf[SNPRINTF_BUF_LEN];

	/* get both Proj's */
	proj1 = get_proj(irn, pn_Cond_true);
	assert(proj1 && "CondJmp without true Proj");

	proj2 = get_proj(irn, pn_Cond_false);
	assert(proj2 && "CondJmp without false Proj");

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(irn);

	/* we have a block schedule */
	next_bl = next_blk_sched(block);

	if (get_cfop_target_block(proj1) == next_bl) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj1;
		proj1 = proj2;
		proj2 = t;
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
	IA32_DO_EMIT(irn);

	/* the second Proj might be a fallthrough */
	if (get_cfop_target_block(proj2) != next_bl) {
		snprintf(cmd_buf, SNPRINTF_BUF_LEN, "jmp %s", get_cfop_target(proj2, buf));
		snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* otherwise */");
	}
	else {
		cmd_buf[0] = '\0';
		snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* fallthrough %s */", get_cfop_target(proj2, buf));
	}
	IA32_DO_EMIT(irn);
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
	IA32_DO_EMIT(irn);
	finish_CondJmp(F, irn, get_ia32_res_mode(irn));
}

/**
 * Emits code for conditional jump with two variables.
 */
static void emit_ia32_CondJmp(const ir_node *irn, ia32_emit_env_t *env) {
	CondJmp_emitter(irn, env);
}

/**
 * Emits code for conditional test and jump.
 */
static void TestJmp_emitter(const ir_node *irn, ia32_emit_env_t *env) {

#define IA32_IS_IMMOP (is_ia32_ImmConst(irn) || is_ia32_ImmSymConst(irn))

	FILE       *F   = env->out;
	const char *op1 = arch_register_get_name(get_in_reg(irn, 0));
	const char *op2 = IA32_IS_IMMOP ? get_ia32_cnst(irn) : NULL;
	char        cmd_buf[SNPRINTF_BUF_LEN];
	char        cmnt_buf[SNPRINTF_BUF_LEN];

	if (! op2)
		op2 = arch_register_get_name(get_in_reg(irn, 1));

	snprintf(cmd_buf, SNPRINTF_BUF_LEN, "test %%%s,%s%s ", op1, IA32_IS_IMMOP ? " " : " %", op2);
	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F */", irn);

	IA32_DO_EMIT(irn);
	finish_CondJmp(F, irn, get_ia32_res_mode(irn));

#undef IA32_IS_IMMOP
}

/**
 * Emits code for conditional test and jump with two variables.
 */
static void emit_ia32_TestJmp(const ir_node *irn, ia32_emit_env_t *env) {
	TestJmp_emitter(irn, env);
}

static void emit_ia32_CJmp(const ir_node *irn, ia32_emit_env_t *env) {
	FILE *F = env->out;
	char cmd_buf[SNPRINTF_BUF_LEN];
	char cmnt_buf[SNPRINTF_BUF_LEN];

	snprintf(cmd_buf, SNPRINTF_BUF_LEN, " ");
	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F omitted redundant test */", irn);
	IA32_DO_EMIT(irn);
	finish_CondJmp(F, irn, get_ia32_res_mode(irn));
}

static void emit_ia32_CJmpAM(const ir_node *irn, ia32_emit_env_t *env) {
	FILE *F = env->out;
	char cmd_buf[SNPRINTF_BUF_LEN];
	char cmnt_buf[SNPRINTF_BUF_LEN];

	snprintf(cmd_buf, SNPRINTF_BUF_LEN, " ");
	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F omitted redundant test/cmp */", irn);
	IA32_DO_EMIT(irn);
	finish_CondJmp(F, irn, get_ia32_res_mode(irn));
}

/**
 * Emits code for conditional x87 floating point jump with two variables.
 */
static void emit_ia32_x87CondJmp(ir_node *irn, ia32_emit_env_t *env) {
	FILE *F = env->out;
	char cmd_buf[SNPRINTF_BUF_LEN];
	char cmnt_buf[SNPRINTF_BUF_LEN];
	ia32_attr_t *attr = get_ia32_attr(irn);
	const char *reg = attr->x87[1]->name;
	const char *instr = "fcom";
	int reverse = 0;

	switch (get_ia32_pncode(irn)) {
	case iro_ia32_fcomrJmp:
		reverse = 1;
	case iro_ia32_fcomJmp:
	default:
		instr = "fucom";
		break;
	case iro_ia32_fcomrpJmp:
		reverse = 1;
	case iro_ia32_fcompJmp:
		instr = "fucomp";
		break;
	case iro_ia32_fcomrppJmp:
		reverse = 1;
	case iro_ia32_fcomppJmp:
		instr = "fucompp";
		reg = "";
		break;
	}

	if (reverse)
		set_ia32_pncode(irn, (long)get_negated_pnc(get_ia32_pncode(irn), mode_Is));

	snprintf(cmd_buf, SNPRINTF_BUF_LEN, "%s %s", instr, reg);
	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F */", irn);
	IA32_DO_EMIT(irn);
//	lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "fnstsw %3D", irn);
	lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "fnstsw %%ax", irn);
	snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* Store x87 FPU Control Word */");
	IA32_DO_EMIT(irn);
	snprintf(cmd_buf, SNPRINTF_BUF_LEN, "sahf");
	snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* Store ah into flags */");
	IA32_DO_EMIT(irn);

	finish_CondJmp(F, irn, mode_Is);
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
static void emit_ia32_SwitchJmp(const ir_node *irn, ia32_emit_env_t *emit_env) {
	unsigned long       interval;
	char                buf[SNPRINTF_BUF_LEN];
	int                 last_value, i, pn;
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

	/* emit the table */
	lc_esnprintf(env, cmd_buf, SNPRINTF_BUF_LEN, "cmp %1S, %u", irn, interval);
	snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* compare for switch */");
	IA32_DO_EMIT(irn);

	snprintf(cmd_buf, SNPRINTF_BUF_LEN, "ja %s", get_cfop_target(tbl.defProj, buf));
	snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* default jump if out of range  */");
	IA32_DO_EMIT(irn);

	if (tbl.num_branches > 1) {
		/* create table */

		lc_esnprintf(env, cmd_buf, SNPRINTF_BUF_LEN, "jmp %s[%1S*4]", tbl.label, irn);
		snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* get jump table entry as target */");
		IA32_DO_EMIT(irn);

		ia32_switch_section(F, SECTION_RODATA);
		fprintf(F, "\t.align 4\n");

		fprintf(F, "%s:\n", tbl.label);

		snprintf(cmd_buf, SNPRINTF_BUF_LEN, ".long %s", get_cfop_target(tbl.branches[0].target, buf));
		snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* case %d */",  tbl.branches[0].value);
		IA32_DO_EMIT(irn);

		last_value = tbl.branches[0].value;
		for (i = 1; i < tbl.num_branches; ++i) {
			while (++last_value < tbl.branches[i].value) {
				snprintf(cmd_buf, SNPRINTF_BUF_LEN, ".long %s", get_cfop_target(tbl.defProj, buf));
				snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* default case */");
				IA32_DO_EMIT(irn);
			}
			snprintf(cmd_buf, SNPRINTF_BUF_LEN, ".long %s", get_cfop_target(tbl.branches[i].target, buf));
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* case %d */", last_value);
			IA32_DO_EMIT(irn);
		}
		ia32_switch_section(F, SECTION_TEXT);
	}
	else {
		/* one jump is enough */
		snprintf(cmd_buf, SNPRINTF_BUF_LEN, "jmp %s", get_cfop_target(tbl.branches[0].target, buf));
		snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* only one case given */");
		IA32_DO_EMIT(irn);
	}

	if (tbl.label)
		free(tbl.label);
	if (tbl.branches)
		free(tbl.branches);
}

/**
 * Emits code for a unconditional jump.
 */
static void emit_Jmp(const ir_node *irn, ia32_emit_env_t *env) {
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
	IA32_DO_EMIT(irn);
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
static void emit_Proj(const ir_node *irn, ia32_emit_env_t *env) {
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
	IA32_DO_EMIT(NULL);

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
			IA32_DO_EMIT(NULL);
			snprintf(cmd_buf, SNPRINTF_BUF_LEN, "movsw");
			snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* memcopy remainder 3 */");
			break;
	}

	IA32_DO_EMIT(NULL);
}

/**
 * Emit rep movsd instruction for memcopy.
 */
static void emit_ia32_CopyB(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE    *F         = emit_env->out;
	tarval  *tv        = get_ia32_Immop_tarval(irn);
	int      rem       = get_tarval_long(tv);
	ir_node *size_node = get_irn_n(irn, 2);
	int      size;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	/* beware: size_node could be a be_Copy to fulfill constraints for ecx */
	size_node = be_is_Copy(size_node) ? be_get_Copy_op(size_node) : size_node;
	size      = get_tarval_long(get_ia32_Immop_tarval(size_node));

	emit_CopyB_prolog(F, rem, size);

	snprintf(cmd_buf, SNPRINTF_BUF_LEN, "rep movsd");
	snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* memcopy */");
	IA32_DO_EMIT(irn);
}

/**
 * Emits unrolled memcopy.
 */
static void emit_ia32_CopyB_i(const ir_node *irn, ia32_emit_env_t *emit_env) {
	tarval *tv   = get_ia32_Immop_tarval(irn);
	int     size = get_tarval_long(tv);
	FILE   *F    = emit_env->out;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	emit_CopyB_prolog(F, size & 0x3, size);

	size >>= 2;
	while (size--) {
		snprintf(cmd_buf, SNPRINTF_BUF_LEN, "movsd");
		snprintf(cmnt_buf, SNPRINTF_BUF_LEN, "/* memcopy unrolled */");
		IA32_DO_EMIT(irn);
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
	FILE               *F        = emit_env->out;
	const lc_arg_env_t *env      = ia32_get_arg_env();
	ir_mode	           *src_mode = get_ia32_src_mode(irn);
	ir_mode            *tgt_mode = get_ia32_tgt_mode(irn);
	char               *from, *to, buf[64];
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

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
	IA32_DO_EMIT(irn);
}

static void emit_ia32_Conv_I2FP(const ir_node *irn, ia32_emit_env_t *emit_env) {
	emit_ia32_Conv_with_FP(irn, emit_env);
}

static void emit_ia32_Conv_FP2I(const ir_node *irn, ia32_emit_env_t *emit_env) {
	emit_ia32_Conv_with_FP(irn, emit_env);
}

static void emit_ia32_Conv_FP2FP(const ir_node *irn, ia32_emit_env_t *emit_env) {
	emit_ia32_Conv_with_FP(irn, emit_env);
}

/**
 * Emits code for an Int conversion.
 */
static void emit_ia32_Conv_I2I(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE               *F        = emit_env->out;
	const lc_arg_env_t *env      = ia32_get_arg_env();
	char               *move_cmd = "movzx";
	char               *conv_cmd = NULL;
	ir_mode	           *src_mode = get_ia32_src_mode(irn);
	ir_mode            *tgt_mode = get_ia32_tgt_mode(irn);
	int n, m;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];
	const arch_register_t *in_reg, *out_reg;

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

	IA32_DO_EMIT(irn);
}

/**
 * Emits code for an 8Bit Int conversion.
 */
void emit_ia32_Conv_I2I8Bit(const ir_node *irn, ia32_emit_env_t *emit_env) {
	emit_ia32_Conv_I2I(irn, emit_env);
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
static void emit_be_Call(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	entity *ent = be_Call_get_entity(irn);
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	if (ent) {
		snprintf(cmd_buf, SNPRINTF_BUF_LEN, "call %s", get_entity_ld_name(ent));
	}
	else {
		lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "%1D", get_irn_n(irn, be_pos_Call_ptr));
	}

	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F (be_Call) */", irn);

	IA32_DO_EMIT(irn);
}

/**
 * Emits code to increase stack pointer.
 */
static void emit_be_IncSP(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE          *F    = emit_env->out;
	unsigned       offs = be_get_IncSP_offset(irn);
	be_stack_dir_t dir  = be_get_IncSP_direction(irn);
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	if (offs) {
		if (dir == be_stack_dir_expand)
			lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "sub %1S, %u", irn, offs);
		else
			lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "add %1S, %u", irn, offs);
		lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F (IncSP) */", irn);
	}
	else {
		snprintf(cmd_buf, SNPRINTF_BUF_LEN, " ");
		lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* omitted %+F (IncSP) with 0 */", irn);
	}

	IA32_DO_EMIT(irn);
}

/**
 * Emits code to set stack pointer.
 */
static void emit_be_SetSP(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "mov %1D, %3S", irn, irn);
	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F (restore SP) */", irn);
	IA32_DO_EMIT(irn);
}

/**
 * Emits code for Copy.
 */
static void emit_be_Copy(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	const arch_env_t *aenv = emit_env->arch_env;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	if (REGS_ARE_EQUAL(arch_get_irn_register(aenv, irn), arch_get_irn_register(aenv, be_get_Copy_op(irn))))
		return;

	if (mode_is_float(get_irn_mode(irn)))
		lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "movs%M %1D, %1S", irn, irn, irn);
	else
		lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "mov %1D, %1S", irn, irn);
	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F */", irn);
	IA32_DO_EMIT(irn);
}

/**
 * Emits code for exchange.
 */
static void emit_be_Perm(const ir_node *irn, ia32_emit_env_t *emit_env) {
	FILE *F = emit_env->out;
	char cmd_buf[SNPRINTF_BUF_LEN], cmnt_buf[SNPRINTF_BUF_LEN];

	lc_esnprintf(ia32_get_arg_env(), cmd_buf, SNPRINTF_BUF_LEN, "xchg %1S, %2S", irn, irn);
	lc_esnprintf(ia32_get_arg_env(), cmnt_buf, SNPRINTF_BUF_LEN, "/* %+F(%1A, %2A) */", irn, irn, irn);
	IA32_DO_EMIT(irn);
}

/**
 * Emits code for Constant loading.
 */
static void emit_ia32_Const(const ir_node *n, ia32_emit_env_t *env) {
  FILE *F = env->out;
  char cmd_buf[256], cmnt_buf[256];
  const lc_arg_env_t *arg_env = ia32_get_arg_env();

  if (get_ia32_Immop_tarval(n) == get_tarval_null(get_irn_mode(n))) {
		const char *instr = "xor";
		if (env->isa->opt_arch == arch_pentium_4) {
			/* P4 prefers sub r, r, others xor r, r */
			instr = "sub";
		}
    lc_esnprintf(arg_env, cmd_buf, 256, "%s %1D, %1D ", instr, n, n);
    lc_esnprintf(arg_env, cmnt_buf, 256, "/* optimized mov 0 to register */");
  }
  else {
    if (get_ia32_op_type(n) == ia32_SymConst) {
      lc_esnprintf(arg_env, cmd_buf, 256, "mov %1D, OFFSET FLAT:%C ", n, n);
      lc_esnprintf(arg_env, cmnt_buf, 256, "/* Move address of SymConst into register */");
    }
		else {
				lc_esnprintf(arg_env, cmd_buf, 256, "mov %1D, %C ", n, n);
				lc_esnprintf(arg_env, cmnt_buf, 256, "/* Mov Const into register */");
		}
  }
  lc_efprintf(arg_env, F, "\t%-35s %-60s /* %+F (%+G) */\n", cmd_buf, cmnt_buf, n, n);
}

static void emit_be_Return(const ir_node *n, ia32_emit_env_t *env) {
  FILE *F = env->out;
  const lc_arg_env_t *arg_env = ia32_get_arg_env();

  lc_efprintf(arg_env, F, "\t%-35s %-60s /* %+F (%+G) */\n", "ret", "/* be_Return */", n, n);
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

#define IA32_EMIT2(a,b) op_ia32_##a->ops.generic = (op_func)emit_ia32_##b
#define IA32_EMIT(a)    IA32_EMIT2(a,a)
#define EMIT(a)         op_##a->ops.generic = (op_func)emit_##a
#define BE_EMIT(a)      op_be_##a->ops.generic = (op_func)emit_be_##a

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	ia32_register_spec_emitters();

	/* other ia32 emitter functions */
	IA32_EMIT(CondJmp);
	IA32_EMIT(TestJmp);
	IA32_EMIT(CJmp);
	IA32_EMIT(CJmpAM);
	IA32_EMIT(SwitchJmp);
	IA32_EMIT(CopyB);
	IA32_EMIT(CopyB_i);
	IA32_EMIT(Conv_I2FP);
	IA32_EMIT(Conv_FP2I);
	IA32_EMIT(Conv_FP2FP);
	IA32_EMIT(Conv_I2I);
	IA32_EMIT(Conv_I2I8Bit);
	IA32_EMIT(Const);
	IA32_EMIT2(fcomJmp, x87CondJmp);
	IA32_EMIT2(fcompJmp, x87CondJmp);
	IA32_EMIT2(fcomppJmp, x87CondJmp);
	IA32_EMIT2(fcomrJmp, x87CondJmp);
	IA32_EMIT2(fcomrpJmp, x87CondJmp);
	IA32_EMIT2(fcomrppJmp, x87CondJmp);

	/* benode emitter */
	BE_EMIT(Call);
	BE_EMIT(IncSP);
	BE_EMIT(SetSP);
	BE_EMIT(Copy);
	BE_EMIT(Perm);
	BE_EMIT(Return);

	/* firm emitter */
	EMIT(Jmp);
	EMIT(Proj);

#undef BE_EMIT
#undef EMIT
#undef IA32_EMIT2
#undef IA32_EMIT
}

/**
 * Emits code for a node.
 */
static void ia32_emit_node(const ir_node *irn, void *env) {
	ia32_emit_env_t   *emit_env = env;
	FILE              *F        = emit_env->out;
	ir_op             *op       = get_irn_op(irn);
	DEBUG_ONLY(firm_dbg_module_t *mod = emit_env->mod;)

	DBG((mod, LEVEL_1, "emitting code for %+F\n", irn));

	if (op->ops.generic) {
		void (*emit)(const ir_node *, void *) = (void (*)(const ir_node *, void *))op->ops.generic;
		(*emit)(irn, env);
	}
	else {
		ir_fprintf(F, "\t%35s /* %+F (%+G) */\n", " ", irn, irn);
	}
}

/**
 * Emits gas alignment directives
 */
static void ia32_emit_alignment(FILE *F, unsigned align, unsigned skip) {
	fprintf(F, "\t.p2align %u,,%u\n", align, skip);
}

/**
 * Emits gas alignment directives for Functions depended on cpu architecture.
 */
static void ia32_emit_align_func(FILE *F, cpu_support cpu) {
	unsigned align; unsigned maximum_skip;

	/* gcc doesn't emit alignment for p4 ?*/
    if (cpu == arch_pentium_4)
		return;

	switch (cpu) {
		case arch_i386:
			align = 2; maximum_skip = 3;
			break;
		case arch_i486:
			align = 4; maximum_skip = 15;
			break;
		case arch_k6:
			align = 5; maximum_skip = 31;
			break;
		default:
			align = 4; maximum_skip = 15;
	}
	ia32_emit_alignment(F, align, maximum_skip);
}

/**
 * Emits gas alignment directives for Labels depended on cpu architecture.
 */
static void ia32_emit_align_label(FILE *F, cpu_support cpu) {
	unsigned align; unsigned maximum_skip;

	/* gcc doesn't emit alignment for p4 ?*/
    if (cpu == arch_pentium_4)
		return;

	switch (cpu) {
		case arch_i386:
			align = 2; maximum_skip = 3;
			break;
		case arch_i486:
			align = 4; maximum_skip = 15;
			break;
		case arch_k6:
			align = 5; maximum_skip = 7;
			break;
		default:
			align = 4; maximum_skip = 7;
	}
	ia32_emit_alignment(F, align, maximum_skip);
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void ia32_gen_block(ir_node *block, void *env) {
	ia32_emit_env_t *emit_env = env;
	const ir_node *irn;
	int need_label = block != get_irg_start_block(get_irn_irg(block));

	if (! is_Block(block))
		return;

	if (need_label && (emit_env->cg->opt & IA32_OPT_EXTBB)) {
		/* if the extended block scheduler is used, only leader blocks need
		   labels. */
		need_label = (block == get_extbb_leader(get_nodes_extbb(block)));
	}

	if (need_label) {
		ia32_emit_align_label(emit_env->out, emit_env->isa->opt_arch);
		fprintf(emit_env->out, BLOCK_PREFIX("%ld:\n"), get_irn_node_nr(block));
	}

	sched_foreach(block, irn) {
		ia32_emit_node(irn, env);
	}
}

/**
 * Emits code for function start.
 */
static void ia32_emit_func_prolog(FILE *F, ir_graph *irg, cpu_support cpu) {
	entity     *irg_ent  = get_irg_entity(irg);
	const char *irg_name = get_entity_ld_name(irg_ent);

	fprintf(F, "\n");
	ia32_switch_section(F, SECTION_TEXT);
	ia32_emit_align_func(F, cpu);
	if (get_entity_visibility(irg_ent) == visibility_external_visible) {
		fprintf(F, ".globl %s\n", irg_name);
	}
	ia32_dump_function_object(F, irg_name);
	fprintf(F, "%s:\n", irg_name);
}

/**
 * Emits code for function end
 */
static void ia32_emit_func_epilog(FILE *F, ir_graph *irg) {
	const char *irg_name = get_entity_ld_name(get_irg_entity(irg));

	ia32_dump_function_size(F, irg_name);
	fprintf(F, "\n");
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

/**
 * Main driver. Emits the code for one routine.
 */
void ia32_gen_routine(FILE *F, ir_graph *irg, const ia32_code_gen_t *cg) {
	ia32_emit_env_t emit_env;
	ir_node *block;

	emit_env.out      = F;
	emit_env.arch_env = cg->arch_env;
	emit_env.cg       = cg;
	emit_env.isa      = (ia32_isa_t *)cg->arch_env->isa;
	FIRM_DBG_REGISTER(emit_env.mod, "firm.be.ia32.emitter");

	/* set the global arch_env (needed by print hooks) */
	arch_env = cg->arch_env;

	ia32_register_emitters();

	ia32_emit_func_prolog(F, irg, emit_env.isa->opt_arch);
	irg_block_walk_graph(irg, ia32_gen_labels, NULL, &emit_env);

	if ((cg->opt & IA32_OPT_EXTBB) && cg->blk_sched) {
		int i, n = ARR_LEN(cg->blk_sched);

		for (i = 0; i < n;) {
			ir_node *next_bl;

			block   = cg->blk_sched[i];
			++i;
			next_bl = i < n ? cg->blk_sched[i] : NULL;

			/* set here the link. the emitter expects to find the next block here */
			set_irn_link(block, next_bl);
			ia32_gen_block(block, &emit_env);
		}
	}
	else {
		/* "normal" block schedule: Note the get_next_block() returns the NUMBER of the block
		   in the block schedule. As this number should NEVER be equal the next block,
		   we does not need a clear block link here. */
		irg_walk_blkwise_graph(irg, NULL, ia32_gen_block, &emit_env);
	}

	ia32_emit_func_epilog(F, irg);
}
