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
 * @brief       This file implements the ia32 node emitter.
 * @author      Christian Wuerdig, Matthias Braun
 * @version     $Id$
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
#include "execfreq.h"
#include "error.h"
#include "raw_bitset.h"

#include "../besched_t.h"
#include "../benode_t.h"
#include "../beabi.h"
#include "../be_dbgout.h"
#include "../beemitter.h"
#include "../begnuas.h"
#include "../beirg_t.h"

#include "ia32_emitter.h"
#include "gen_ia32_emitter.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_nodes_attr.h"
#include "ia32_new_nodes.h"
#include "ia32_map_regs.h"
#include "bearch_ia32_t.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define BLOCK_PREFIX ".L"

#define SNPRINTF_BUF_LEN 128

/**
 * Returns the register at in position pos.
 */
static
const arch_register_t *get_in_reg(ia32_emit_env_t *env, const ir_node *irn,
                                  int pos)
{
	const arch_env_t       *arch_env = env->arch_env;
	ir_node                *op;
	const arch_register_t  *reg = NULL;

	assert(get_irn_arity(irn) > pos && "Invalid IN position");

	/* The out register of the operator at position pos is the
	   in register we need. */
	op = get_irn_n(irn, pos);

	reg = arch_get_irn_register(arch_env, op);

	assert(reg && "no in register found");

	if(reg == &ia32_gp_regs[REG_GP_NOREG])
		panic("trying to emit noreg");

	/* in case of unknown register: just return a valid register */
	if (reg == &ia32_gp_regs[REG_GP_UKNWN]) {
		const arch_register_req_t *req;

		/* ask for the requirements */
		req = arch_get_register_req(arch_env, irn, pos);

		if (arch_register_req_is(req, limited)) {
			/* in case of limited requirements: get the first allowed register */
			unsigned idx = rbitset_next(req->limited, 0, 1);
			reg = arch_register_for_index(req->cls, idx);
		} else {
			/* otherwise get first register in class */
			reg = arch_register_for_index(req->cls, 0);
		}
	}

	return reg;
}

/**
 * Returns the register at out position pos.
 */
static
const arch_register_t *get_out_reg(ia32_emit_env_t *env, const ir_node *irn,
                                   int pos)
{
	const arch_env_t      *arch_env = env->arch_env;
	ir_node               *proj;
	const arch_register_t *reg = NULL;

	/* 1st case: irn is not of mode_T, so it has only                 */
	/*           one OUT register -> good                             */
	/* 2nd case: irn is of mode_T -> collect all Projs and ask the    */
	/*           Proj with the corresponding projnum for the register */

	if (get_irn_mode(irn) != mode_T) {
		reg = arch_get_irn_register(arch_env, irn);
	} else if (is_ia32_irn(irn)) {
		reg = get_ia32_out_reg(irn, pos);
	} else {
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

/**
 * Determine the gnu assembler suffix that indicates a mode
 */
static
char get_mode_suffix(const ir_mode *mode) {
	if(mode_is_float(mode)) {
		switch(get_mode_size_bits(mode)) {
		case 32:
			return 's';
		case 64:
			return 'l';
		case 80:
		case 96:
			return 't';
		}
	} else {
		assert(mode_is_int(mode) || mode_is_reference(mode) || mode_is_character(mode));
		switch(get_mode_size_bits(mode)) {
		case 64:
			return 'q';
		case 32:
			return 'l';
		case 16:
			return 'w';
		case 8:
			return 'b';
		}
	}
	panic("Can't output mode_suffix for %+F\n", mode);
}

static
int produces_result(const ir_node *node) {
	return
		!is_ia32_CmpSet(node)    &&
		!is_ia32_CondJmp(node)   &&
		!is_ia32_St(node)        &&
		!is_ia32_SwitchJmp(node) &&
		!is_ia32_TestJmp(node)   &&
		!is_ia32_xCmpSet(node)   &&
		!is_ia32_xCondJmp(node);
}

static
const char *ia32_get_reg_name_for_mode(ia32_emit_env_t *env, ir_mode *mode,
                                       const arch_register_t *reg) {
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
 * Add a number to a prefix. This number will not be used a second time.
 */
static
char *get_unique_label(char *buf, size_t buflen, const char *prefix) {
	static unsigned long id = 0;
	snprintf(buf, buflen, "%s%lu", prefix, ++id);
	return buf;
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

// we have no C++ and can't define an implicit ia32_emit_env_t* cast to
// be_emit_env_t* so we cheat a bit...
#define be_emit_char(env,c)             be_emit_char(env->emit,c)
#define be_emit_string(env,s)           be_emit_string(env->emit,s)
#undef be_emit_cstring
#define be_emit_cstring(env,x)          { be_emit_string_len(env->emit, x, sizeof(x)-1); }
#define be_emit_ident(env,i)            be_emit_ident(env->emit,i)
#define be_emit_tarval(env,tv)          be_emit_tarval(env->emit,tv)
#define be_emit_write_line(env)         be_emit_write_line(env->emit)
#define be_emit_finish_line_gas(env,n)  be_emit_finish_line_gas(env->emit,n)
#define be_emit_pad_comment(env)        be_emit_pad_comment(env->emit)

void ia32_emit_source_register(ia32_emit_env_t *env, const ir_node *node, int pos)
{
	const arch_register_t *reg = get_in_reg(env, node, pos);
	const char *reg_name = arch_register_get_name(reg);

	assert(pos < get_irn_arity(node));

	be_emit_char(env, '%');
	be_emit_string(env, reg_name);
}

void ia32_emit_dest_register(ia32_emit_env_t *env, const ir_node *node, int pos) {
	const arch_register_t *reg = get_out_reg(env, node, pos);
	const char *reg_name = arch_register_get_name(reg);

	be_emit_char(env, '%');
	be_emit_string(env, reg_name);
}

void ia32_emit_x87_name(ia32_emit_env_t *env, const ir_node *node, int pos)
{
	const ia32_x87_attr_t *attr = get_ia32_x87_attr_const(node);

	assert(pos < 3);
	be_emit_char(env, '%');
	be_emit_string(env, attr->x87[pos]->name);
}

void ia32_emit_immediate(ia32_emit_env_t *env, const ir_node *node)
{
	tarval *tv;
	ir_entity *ent;
	ident *id;

	be_emit_char(env, '$');

	switch(get_ia32_immop_type(node)) {
	case ia32_ImmConst:
		tv = get_ia32_Immop_tarval(node);
		be_emit_tarval(env, tv);
		return;
	case ia32_ImmSymConst:
		ent = get_ia32_Immop_symconst(node);
		set_entity_backend_marked(ent, 1);
		id = get_entity_ld_ident(ent);
		be_emit_ident(env, id);
		return;
	case ia32_ImmNone:
		break;
	}

	assert(0);
	be_emit_string(env, "BAD");
	return;
}

static
void ia32_emit_mode_suffix_mode(ia32_emit_env_t *env, const ir_mode *mode)
{
	be_emit_char(env, get_mode_suffix(mode));
}

void ia32_emit_mode_suffix(ia32_emit_env_t *env, const ir_node *node)
{
	ir_mode *mode = get_ia32_ls_mode(node);
	if(mode == NULL)
		mode = mode_Iu;

	ia32_emit_mode_suffix_mode(env, mode);
}

void ia32_emit_x87_mode_suffix(ia32_emit_env_t *env, const ir_node *node)
{
	ir_mode *mode = get_ia32_ls_mode(node);
	if(mode != NULL)
		ia32_emit_mode_suffix_mode(env, mode);
}

static
char get_xmm_mode_suffix(ir_mode *mode)
{
	assert(mode_is_float(mode));
	switch(get_mode_size_bits(mode)) {
	case 32:
		return 's';
	case 64:
		return 'd';
	default:
		assert(0);
	}
	return '%';
}

void ia32_emit_xmm_mode_suffix(ia32_emit_env_t *env, const ir_node *node)
{
	ir_mode *mode = get_ia32_ls_mode(node);
	assert(mode != NULL);
	be_emit_char(env, 's');
	be_emit_char(env, get_xmm_mode_suffix(mode));
}

void ia32_emit_xmm_mode_suffix_s(ia32_emit_env_t *env, const ir_node *node)
{
	ir_mode *mode = get_ia32_ls_mode(node);
	assert(mode != NULL);
	be_emit_char(env, get_xmm_mode_suffix(mode));
}

void ia32_emit_extend_suffix(ia32_emit_env_t *env, const ir_mode *mode)
{
	if(get_mode_size_bits(mode) == 32)
		return;
	if(mode_is_signed(mode)) {
		be_emit_char(env, 's');
	} else {
		be_emit_char(env, 'z');
	}
}

static
void ia32_emit_function_object(ia32_emit_env_t *env, const char *name)
{
	switch (be_gas_flavour) {
	case GAS_FLAVOUR_NORMAL:
		be_emit_cstring(env, "\t.type\t");
		be_emit_string(env, name);
		be_emit_cstring(env, ", @function\n");
		be_emit_write_line(env);
		break;
	case GAS_FLAVOUR_MINGW:
		be_emit_cstring(env, "\t.def\t");
		be_emit_string(env, name);
		be_emit_cstring(env, ";\t.scl\t2;\t.type\t32;\t.endef\n");
		be_emit_write_line(env);
		break;
	default:
		break;
	}
}

static
void ia32_emit_function_size(ia32_emit_env_t *env, const char *name)
{
	switch (be_gas_flavour) {
	case GAS_FLAVOUR_NORMAL:
		be_emit_cstring(env, "\t.size\t");
		be_emit_string(env, name);
		be_emit_cstring(env, ", .-");
		be_emit_string(env, name);
		be_emit_char(env, '\n');
		be_emit_write_line(env);
		break;
	default:
		break;
	}
}


static
void emit_ia32_Immediate(ia32_emit_env_t *env, const ir_node *node);

/**
 * Emits registers and/or address mode of a binary operation.
 */
void ia32_emit_binop(ia32_emit_env_t *env, const ir_node *node) {
	int            right_pos;
	const ir_node *right_op;

	switch(get_ia32_op_type(node)) {
	case ia32_Normal:
		right_op = get_irn_n(node, 3);
		if(is_ia32_Immediate(right_op)) {
			emit_ia32_Immediate(env, right_op);
			be_emit_cstring(env, ", ");
			ia32_emit_source_register(env, node, 2);
			break;
		} else if (is_ia32_ImmConst(node) || is_ia32_ImmSymConst(node)) {
			ia32_emit_immediate(env, node);
			be_emit_cstring(env, ", ");
			ia32_emit_source_register(env, node, 2);
		} else {
			const arch_register_t *in1 = get_in_reg(env, node, 2);
			const arch_register_t *in2 = get_in_reg(env, node, 3);
			const arch_register_t *out = produces_result(node) ? get_out_reg(env, node, 0) : NULL;
			const arch_register_t *in;
			const char            *in_name;

			in      = out ? (REGS_ARE_EQUAL(out, in2) ? in1 : in2) : in2;
			out     = out ? out : in1;
			in_name = arch_register_get_name(in);

			if (is_ia32_emit_cl(node)) {
				assert(REGS_ARE_EQUAL(&ia32_gp_regs[REG_ECX], in) && "shift operation needs ecx");
				in_name = "cl";
			}

			be_emit_char(env, '%');
			be_emit_string(env, in_name);
			be_emit_cstring(env, ", %");
			be_emit_string(env, arch_register_get_name(out));
		}
		break;
	case ia32_AddrModeS:
		ia32_emit_am(env, node);
		be_emit_cstring(env, ", ");
		if (is_ia32_ImmConst(node) || is_ia32_ImmSymConst(node)) {
			assert(!produces_result(node) && "Source AM with Const must not produce result");
			ia32_emit_immediate(env, node);
		} else if (produces_result(node)) {
			ia32_emit_dest_register(env, node, 0);
		} else {
			ia32_emit_source_register(env, node, 2);
		}
		break;
	case ia32_AddrModeD:
		right_pos = get_irn_arity(node) == 5 ? 3 : 2;
		right_op  = get_irn_n(node, right_pos);
		if(is_ia32_Immediate(right_op)) {
			emit_ia32_Immediate(env, right_op);
			be_emit_cstring(env, ", ");
			ia32_emit_am(env, node);
			break;
		} else if (is_ia32_ImmConst(node) || is_ia32_ImmSymConst(node)) {
			ia32_emit_immediate(env, node);
			be_emit_cstring(env, ", ");
			ia32_emit_am(env, node);
		} else {
			const arch_register_t *in1 = get_in_reg(env, node, right_pos);
			ir_mode               *mode = get_ia32_ls_mode(node);
			const char            *in_name;

			in_name = ia32_get_reg_name_for_mode(env, mode, in1);

			if (is_ia32_emit_cl(node)) {
				assert(REGS_ARE_EQUAL(&ia32_gp_regs[REG_ECX], in1) && "shift operation needs ecx");
				in_name = "cl";
			}

			be_emit_char(env, '%');
			be_emit_string(env, in_name);
			be_emit_cstring(env, ", ");
			ia32_emit_am(env, node);
		}
		break;
	default:
		assert(0 && "unsupported op type");
	}
}

/**
 * Emits registers and/or address mode of a binary operation.
 */
void ia32_emit_x87_binop(ia32_emit_env_t *env, const ir_node *node) {
	switch(get_ia32_op_type(node)) {
		case ia32_Normal:
			if (is_ia32_ImmConst(node) || is_ia32_ImmSymConst(node)) {
				// should not happen...
				assert(0);
			} else {
				const ia32_x87_attr_t *x87_attr = get_ia32_x87_attr_const(node);
				const arch_register_t *in1      = x87_attr->x87[0];
				const arch_register_t *in2      = x87_attr->x87[1];
				const arch_register_t *out      = x87_attr->x87[2];
				const arch_register_t *in;

				in  = out ? (REGS_ARE_EQUAL(out, in2) ? in1 : in2) : in2;
				out = out ? out : in1;

				be_emit_char(env, '%');
				be_emit_string(env, arch_register_get_name(in));
				be_emit_cstring(env, ", %");
				be_emit_string(env, arch_register_get_name(out));
			}
			break;
		case ia32_AddrModeS:
		case ia32_AddrModeD:
			ia32_emit_am(env, node);
			break;
		default:
			assert(0 && "unsupported op type");
	}
}

void ia32_emit_am_or_dest_register(ia32_emit_env_t *env, const ir_node *node,
                                   int pos) {
	if(get_ia32_op_type(node) == ia32_Normal) {
		ia32_emit_dest_register(env, node, pos);
	} else {
		assert(get_ia32_op_type(node) == ia32_AddrModeD);
		ia32_emit_am(env, node);
	}
}

/**
 * Emits registers and/or address mode of a unary operation.
 */
void ia32_emit_unop(ia32_emit_env_t *env, const ir_node *node, int pos) {
	const ir_node *op;

	switch(get_ia32_op_type(node)) {
	case ia32_Normal:
		op = get_irn_n(node, pos);
		if (is_ia32_Immediate(op)) {
			emit_ia32_Immediate(env, op);
		} else if (is_ia32_ImmConst(node) || is_ia32_ImmSymConst(node)) {
			ia32_emit_immediate(env, node);
		} else {
			ia32_emit_source_register(env, node, pos);
		}
		break;
	case ia32_AddrModeS:
	case ia32_AddrModeD:
		ia32_emit_am(env, node);
		break;
	default:
		assert(0 && "unsupported op type");
	}
}

/**
 * Emits address mode.
 */
void ia32_emit_am(ia32_emit_env_t *env, const ir_node *node) {
	ir_entity *ent       = get_ia32_am_sc(node);
	int        offs      = get_ia32_am_offs_int(node);
	ir_node   *base      = get_irn_n(node, 0);
	int        has_base  = !is_ia32_NoReg_GP(base);
	ir_node   *index     = get_irn_n(node, 1);
	int        has_index = !is_ia32_NoReg_GP(index);

	/* just to be sure... */
	assert(!is_ia32_use_frame(node) || get_ia32_frame_ent(node) != NULL);

	/* emit offset */
	if (ent != NULL) {
		ident *id;

		set_entity_backend_marked(ent, 1);
		id = get_entity_ld_ident(ent);
		if (is_ia32_am_sc_sign(node))
			be_emit_char(env, '-');
		be_emit_ident(env, id);

		if(get_entity_owner(ent) == get_tls_type()) {
			if (get_entity_visibility(ent) == visibility_external_allocated) {
				be_emit_cstring(env, "@INDNTPOFF");
			} else {
				be_emit_cstring(env, "@NTPOFF");
			}
		}
	}

	if(offs != 0) {
		if(ent != NULL) {
			be_emit_irprintf(env->emit, "%+d", offs);
		} else {
			be_emit_irprintf(env->emit, "%d", offs);
		}
	}

	if (has_base || has_index) {
		be_emit_char(env, '(');

		/* emit base */
		if (has_base) {
			ia32_emit_source_register(env, node, 0);
		}

		/* emit index + scale */
		if (has_index) {
			int scale;
			be_emit_char(env, ',');
			ia32_emit_source_register(env, node, 1);

			scale = get_ia32_am_scale(node);
			if (scale > 0) {
				be_emit_irprintf(env->emit, ",%d", 1 << get_ia32_am_scale(node));
			}
		}
		be_emit_char(env, ')');
	}
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
static
const struct cmp2conditon_t cmp2condition_s[] = {
	{ NULL,              pn_Cmp_False },  /* always false */
	{ "e",               pn_Cmp_Eq },     /* == */
	{ "l",               pn_Cmp_Lt },     /* < */
	{ "le",              pn_Cmp_Le },     /* <= */
	{ "g",               pn_Cmp_Gt },     /* > */
	{ "ge",              pn_Cmp_Ge },     /* >= */
	{ "ne",              pn_Cmp_Lg },     /* != */
	{ NULL,              pn_Cmp_Leg},     /* Floating point: ordered */
	{ NULL,              pn_Cmp_Uo },     /* Floating point: unordered */
	{ "e",               pn_Cmp_Ue },     /* Floating point: unordered or == */
	{ "b",               pn_Cmp_Ul },     /* Floating point: unordered or < */
	{ "be",              pn_Cmp_Ule },    /* Floating point: unordered or <= */
	{ "a",               pn_Cmp_Ug },     /* Floating point: unordered or > */
	{ "ae",              pn_Cmp_Uge },    /* Floating point: unordered or >= */
	{ "ne",              pn_Cmp_Ne },     /* Floating point: unordered or != */
	{ NULL,              pn_Cmp_True },   /* always true */
};

/*
 * positive conditions for unsigned compares
 */
static
const struct cmp2conditon_t cmp2condition_u[] = {
	{ NULL,              pn_Cmp_False },  /* always false */
	{ "e",               pn_Cmp_Eq },     /* == */
	{ "b",               pn_Cmp_Lt },     /* < */
	{ "be",              pn_Cmp_Le },     /* <= */
	{ "a",               pn_Cmp_Gt },     /* > */
	{ "ae",              pn_Cmp_Ge },     /* >= */
	{ "ne",              pn_Cmp_Lg },     /* != */
	{ NULL,              pn_Cmp_True },   /* always true */
};

/*
 * returns the condition code
 */
static
const char *get_cmp_suffix(pn_Cmp cmp_code)
{
	assert( (cmp2condition_s[cmp_code & 15].num) == (cmp_code & 15));
	assert( (cmp2condition_u[cmp_code & 7].num) == (cmp_code & 7));

	if((cmp_code & ia32_pn_Cmp_Unsigned)) {
		return cmp2condition_u[cmp_code & 7].name;
	} else {
		return cmp2condition_s[cmp_code & 15].name;
	}
}

void ia32_emit_cmp_suffix(ia32_emit_env_t *env, long pnc)
{
	be_emit_string(env, get_cmp_suffix(pnc));
}


/**
 * Returns the target block for a control flow node.
 */
static
ir_node *get_cfop_target_block(const ir_node *irn) {
	return get_irn_link(irn);
}

/**
 * Emits a block label for the given block.
 */
static
void ia32_emit_block_name(ia32_emit_env_t *env, const ir_node *block)
{
	be_emit_cstring(env, BLOCK_PREFIX);
	be_emit_irprintf(env->emit, "%d", get_irn_node_nr(block));
}

/**
 * Emits the target label for a control flow node.
 */
static
void ia32_emit_cfop_target(ia32_emit_env_t * env, const ir_node *node) {
	ir_node *block = get_cfop_target_block(node);

	ia32_emit_block_name(env, block);
}

/** Return the next block in Block schedule */
static ir_node *next_blk_sched(const ir_node *block) {
	return get_irn_link(block);
}

/**
 * Returns the Proj with projection number proj and NOT mode_M
 */
static
ir_node *get_proj(const ir_node *node, long proj) {
	const ir_edge_t *edge;
	ir_node         *src;

	assert(get_irn_mode(node) == mode_T && "expected mode_T node");

	foreach_out_edge(node, edge) {
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
static
void finish_CondJmp(ia32_emit_env_t *env, const ir_node *node, ir_mode *mode,
                    long pnc) {
	const ir_node *proj_true;
	const ir_node *proj_false;
	const ir_node *block;
	const ir_node *next_block;
	int flipped = 0;

	/* get both Proj's */
	proj_true = get_proj(node, pn_Cond_true);
	assert(proj_true && "CondJmp without true Proj");

	proj_false = get_proj(node, pn_Cond_false);
	assert(proj_false && "CondJmp without false Proj");

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	/* we have a block schedule */
	next_block = next_blk_sched(block);

	if (get_cfop_target_block(proj_true) == next_block) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj_true;

		proj_true  = proj_false;
		proj_false = t;
		flipped    = 1;
		pnc        = get_negated_pnc(pnc, mode);
	}

	/* in case of unordered compare, check for parity */
	if (pnc & pn_Cmp_Uo) {
		be_emit_cstring(env, "\tjp ");
		ia32_emit_cfop_target(env, proj_true);
		be_emit_finish_line_gas(env, proj_true);
	}

	be_emit_cstring(env, "\tj");
	ia32_emit_cmp_suffix(env, pnc);
	be_emit_char(env, ' ');
	ia32_emit_cfop_target(env, proj_true);
	be_emit_finish_line_gas(env, proj_true);

	/* the second Proj might be a fallthrough */
	if (get_cfop_target_block(proj_false) != next_block) {
		be_emit_cstring(env, "\tjmp ");
		ia32_emit_cfop_target(env, proj_false);
		be_emit_finish_line_gas(env, proj_false);
	} else {
		be_emit_cstring(env, "\t/* fallthrough to ");
		ia32_emit_cfop_target(env, proj_false);
		be_emit_cstring(env, " */");
		be_emit_finish_line_gas(env, proj_false);
	}
}

/**
 * Emits code for conditional jump.
 */
static
void CondJmp_emitter(ia32_emit_env_t *env, const ir_node *node) {
	be_emit_cstring(env, "\tcmp ");
	ia32_emit_binop(env, node);
	be_emit_finish_line_gas(env, node);

	finish_CondJmp(env, node, mode_Iu, get_ia32_pncode(node));
}

/**
 * Emits code for conditional jump with two variables.
 */
static
void emit_ia32_CondJmp(ia32_emit_env_t *env, const ir_node *node) {
	CondJmp_emitter(env, node);
}

/**
 * Emits code for conditional test and jump.
 */
static
void TestJmp_emitter(ia32_emit_env_t *env, const ir_node *node) {
	be_emit_cstring(env, "\ttest ");
	ia32_emit_binop(env, node);
	be_emit_finish_line_gas(env, node);

	finish_CondJmp(env, node, mode_Iu, get_ia32_pncode(node));
}

/**
 * Emits code for conditional test and jump with two variables.
 */
static
void emit_ia32_TestJmp(ia32_emit_env_t *env, const ir_node *node) {
	TestJmp_emitter(env, node);
}

static
void emit_ia32_CJmp(ia32_emit_env_t *env, const ir_node *node) {
	be_emit_cstring(env, "/* omitted redundant test */");
	be_emit_finish_line_gas(env, node);

	finish_CondJmp(env, node, mode_Is, get_ia32_pncode(node));
}

static
void emit_ia32_CJmpAM(ia32_emit_env_t *env, const ir_node *node) {
	be_emit_cstring(env, "/* omitted redundant test/cmp */");
	be_emit_finish_line_gas(env, node);

	finish_CondJmp(env, node, mode_Is, get_ia32_pncode(node));
}

/**
 * Emits code for conditional SSE floating point jump with two variables.
 */
static
void emit_ia32_xCondJmp(ia32_emit_env_t *env, const ir_node *node) {
	be_emit_cstring(env, "\tucomi");
	ia32_emit_xmm_mode_suffix(env, node);
	be_emit_char(env, ' ');
	ia32_emit_binop(env, node);
	be_emit_finish_line_gas(env, node);

	finish_CondJmp(env, node, mode_F, get_ia32_pncode(node));
}

/**
 * Emits code for conditional x87 floating point jump with two variables.
 */
static
void emit_ia32_x87CondJmp(ia32_emit_env_t *env, const ir_node *node) {
	const ia32_x87_attr_t *x87_attr = get_ia32_x87_attr_const(node);
	const char            *reg      = x87_attr->x87[1]->name;
	long                   pnc      = get_ia32_pncode(node);

	switch (get_ia32_irn_opcode(node)) {
	case iro_ia32_fcomrJmp:
		pnc = get_inversed_pnc(pnc);
		reg = x87_attr->x87[0]->name;
	case iro_ia32_fcomJmp:
	default:
		be_emit_cstring(env, "\tfucom ");
		break;
	case iro_ia32_fcomrpJmp:
		pnc = get_inversed_pnc(pnc);
		reg = x87_attr->x87[0]->name;
	case iro_ia32_fcompJmp:
		be_emit_cstring(env, "\tfucomp ");
		break;
	case iro_ia32_fcomrppJmp:
		pnc = get_inversed_pnc(pnc);
	case iro_ia32_fcomppJmp:
		be_emit_cstring(env, "\tfucompp ");
		reg = "";
		break;
	}

	if(reg[0] != '\0') {
		be_emit_char(env, '%');
		be_emit_string(env, reg);
	}
	be_emit_finish_line_gas(env, node);

	be_emit_cstring(env, "\tfnstsw %ax");
	be_emit_finish_line_gas(env, node);
	be_emit_cstring(env, "\tsahf");
	be_emit_finish_line_gas(env, node);

	finish_CondJmp(env, node, mode_E, pnc);
}

static
void emit_register_or_immediate(ia32_emit_env_t *env, const ir_node *node,
                                int pos)
{
	ir_node *op = get_irn_n(node, pos);
	if(is_ia32_Immediate(op)) {
		emit_ia32_Immediate(env, op);
	} else {
		ia32_emit_source_register(env, node, pos);
	}
}

static
int is_ia32_Immediate_0(const ir_node *node)
{
	const ia32_immediate_attr_t *attr = get_ia32_immediate_attr_const(node);

	return attr->offset == 0 && attr->symconst == NULL;
}

static
void CMov_emitter(ia32_emit_env_t *env, const ir_node *node)
{
	long pnc = get_ia32_pncode(node);
	const arch_register_t *in1, *in2, *out;

	out = arch_get_irn_register(env->arch_env, node);
	in1 = arch_get_irn_register(env->arch_env, get_irn_n(node, 2));
	in2 = arch_get_irn_register(env->arch_env, get_irn_n(node, 3));

	/* we have to emit the cmp first, because the destination register */
	/* could be one of the compare registers                           */
	if (is_ia32_CmpCMov(node)) {
		long pncr = pnc & ~ia32_pn_Cmp_Unsigned;
		ir_node *cmp_right = get_irn_n(node, 1);

		if( (pncr == pn_Cmp_Eq || pncr == pn_Cmp_Lg)
				&& is_ia32_Immediate(cmp_right)
				&& is_ia32_Immediate_0(cmp_right)) {
			be_emit_cstring(env, "\ttest ");
			ia32_emit_source_register(env, node, 0);
			be_emit_cstring(env, ", ");
			ia32_emit_source_register(env, node, 0);
		} else {
			be_emit_cstring(env, "\tcmp ");
			emit_register_or_immediate(env, node, 1);
			be_emit_cstring(env, ", ");
			ia32_emit_source_register(env, node, 0);
		}
	} else if (is_ia32_xCmpCMov(node)) {
		be_emit_cstring(env, "\tucomis");
		ia32_emit_mode_suffix_mode(env, get_irn_mode(node));
		be_emit_char(env, ' ');
		ia32_emit_source_register(env, node, 1);
		be_emit_cstring(env, ", ");
		ia32_emit_source_register(env, node, 0);
	} else {
		assert(0 && "unsupported CMov");
	}
	be_emit_finish_line_gas(env, node);

	if (REGS_ARE_EQUAL(out, in2)) {
		/* best case: default in == out -> do nothing */
	} else if (REGS_ARE_EQUAL(out, in1)) {
		ir_node *n = (ir_node*) node;
		/* true in == out -> need complement compare and exchange true and default in */
		ir_node *t = get_irn_n(n, 2);
		set_irn_n(n, 2, get_irn_n(n, 3));
		set_irn_n(n, 3, t);

		pnc = get_negated_pnc(pnc, get_irn_mode(node));
	} else {
		/* out is different from in: need copy default -> out */
		be_emit_cstring(env, "\tmovl ");
		ia32_emit_source_register(env, node, n_ia32_CmpCMov_val_false);
		be_emit_cstring(env, ", ");
		ia32_emit_dest_register(env, node, 0);
		be_emit_finish_line_gas(env, node);
	}

	be_emit_cstring(env, "\tcmov");
	ia32_emit_cmp_suffix(env, pnc);
	be_emit_cstring(env, "l ");
	ia32_emit_source_register(env, node, n_ia32_CmpCMov_val_true);
	be_emit_cstring(env, ", ");
	ia32_emit_dest_register(env, node, 0);
	be_emit_finish_line_gas(env, node);
}

static
void emit_ia32_CmpCMov(ia32_emit_env_t *env, const ir_node *node)
{
	CMov_emitter(env, node);
}

static
void emit_ia32_xCmpCMov(ia32_emit_env_t *env, const ir_node *node)
{
	CMov_emitter(env, node);
}

static
void Set_emitter(ia32_emit_env_t *env, const ir_node *node)
{
	long pnc = get_ia32_pncode(node);
	const char *reg8bit;
	const arch_register_t *out;

	out     = arch_get_irn_register(env->arch_env, node);
	reg8bit = ia32_get_mapped_reg_name(env->isa->regs_8bit, out);

	if (is_ia32_CmpSet(node)) {
		long     pncr      = pnc & ~ia32_pn_Cmp_Unsigned;
		ir_node *cmp_right = get_irn_n(node, n_ia32_CmpSet_cmp_right);

		if( (pncr == pn_Cmp_Eq || pncr == pn_Cmp_Lg)
				&& is_ia32_Immediate(cmp_right)
				&& is_ia32_Immediate_0(cmp_right)) {
			be_emit_cstring(env, "\ttest ");
			ia32_emit_source_register(env, node, n_ia32_CmpSet_cmp_left);
			be_emit_cstring(env, ", ");
			ia32_emit_source_register(env, node, n_ia32_CmpSet_cmp_left);
		} else {
			be_emit_cstring(env, "\tcmp ");
			ia32_emit_binop(env, node);
		}
	} else if (is_ia32_xCmpSet(node)) {
		be_emit_cstring(env, "\tucomis");
		ia32_emit_mode_suffix_mode(env, get_irn_mode(get_irn_n(node, 2)));
		be_emit_char(env, ' ');
		ia32_emit_binop(env, node);
	} else {
		assert(0 && "unsupported Set");
	}
	be_emit_finish_line_gas(env, node);

	/* use mov to clear target because it doesn't affect the eflags */
	be_emit_cstring(env, "\tmovl $0, %");
	be_emit_string(env, arch_register_get_name(out));
	be_emit_finish_line_gas(env, node);

	be_emit_cstring(env, "\tset");
	ia32_emit_cmp_suffix(env, pnc);
	be_emit_cstring(env, " %");
	be_emit_string(env, reg8bit);
	be_emit_finish_line_gas(env, node);
}

static
void emit_ia32_CmpSet(ia32_emit_env_t *env, const ir_node *node) {
	Set_emitter(env, node);
}

static
void emit_ia32_xCmpSet(ia32_emit_env_t *env, const ir_node *node) {
	Set_emitter(env, node);
}

static
void emit_ia32_xCmp(ia32_emit_env_t *env, const ir_node *node) {
	int  sse_pnc  = -1;
	long pnc      = get_ia32_pncode(node);
	long unord    = pnc & pn_Cmp_Uo;

	assert( (pnc & ia32_pn_Cmp_Unsigned) == 0);

	switch (pnc) {
		case pn_Cmp_Leg: /* odered */
			sse_pnc = 7;
			break;
		case pn_Cmp_Uo:  /* unordered */
			sse_pnc = 3;
			break;
		case pn_Cmp_Ue:
		case pn_Cmp_Eq:  /* == */
			sse_pnc = 0;
			break;
		case pn_Cmp_Ul:
		case pn_Cmp_Lt:  /* < */
			sse_pnc = 1;
			break;
		case pn_Cmp_Ule:
		case pn_Cmp_Le: /* <= */
			sse_pnc = 2;
			break;
		case pn_Cmp_Ug:
		case pn_Cmp_Gt:  /* > */
			sse_pnc = 6;
			break;
		case pn_Cmp_Uge:
		case pn_Cmp_Ge: /* >= */
			sse_pnc = 5;
			break;
		case pn_Cmp_Ne:
		case pn_Cmp_Lg:  /* != */
			sse_pnc = 4;
			break;
	}

	assert(sse_pnc >= 0 && "unsupported compare");

	if (unord && sse_pnc != 3) {
		/*
			We need a separate compare against unordered.
			Quick and Dirty solution:
			- get some memory on stack
			- compare
			- store result
			- compare
			- and result and stored result
		    - cleanup stack
		*/
		be_emit_cstring(env, "\tsubl $8, %esp");
		be_emit_finish_line_gas(env, node);

		be_emit_cstring(env, "\tcmpsd $3, ");
		ia32_emit_binop(env, node);
		be_emit_finish_line_gas(env, node);

		be_emit_cstring(env, "\tmovsd ");
		ia32_emit_dest_register(env, node, 0);
		be_emit_cstring(env, ", (%esp)");
		be_emit_finish_line_gas(env, node);
	}

	be_emit_cstring(env, "\tcmpsd ");
	be_emit_irprintf(env->emit, "%d, ", sse_pnc);
	ia32_emit_binop(env, node);
	be_emit_finish_line_gas(env, node);

	if (unord && sse_pnc != 3) {
		be_emit_cstring(env, "\tandpd (%esp), ");
		ia32_emit_dest_register(env, node, 0);
		be_emit_finish_line_gas(env, node);

		be_emit_cstring(env, "\taddl $8, %esp");
		be_emit_finish_line_gas(env, node);
	}
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
	long      min_value;       /**< smallest switch case */
	long      max_value;       /**< largest switch case */
	long      num_branches;    /**< number of jumps */
	char     *label;           /**< label of the jump table */
	branch_t *branches;        /**< jump array */
} jmp_tbl_t;

/**
 * Compare two variables of type branch_t. Used to sort all switch cases
 */
static
int ia32_cmp_branch_t(const void *a, const void *b) {
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
static
void emit_ia32_SwitchJmp(ia32_emit_env_t *env, const ir_node *node) {
	unsigned long       interval;
	int                 last_value, i;
	long                pnc;
	jmp_tbl_t           tbl;
	ir_node            *proj;
	const ir_edge_t    *edge;

	/* fill the table structure */
	tbl.label        = xmalloc(SNPRINTF_BUF_LEN);
	tbl.label        = get_unique_label(tbl.label, SNPRINTF_BUF_LEN, ".TBL_");
	tbl.defProj      = NULL;
	tbl.num_branches = get_irn_n_edges(node);
	tbl.branches     = xcalloc(tbl.num_branches, sizeof(tbl.branches[0]));
	tbl.min_value    = INT_MAX;
	tbl.max_value    = INT_MIN;

	i = 0;
	/* go over all proj's and collect them */
	foreach_out_edge(node, edge) {
		proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

		pnc = get_Proj_proj(proj);

		/* create branch entry */
		tbl.branches[i].target = proj;
		tbl.branches[i].value  = pnc;

		tbl.min_value = pnc < tbl.min_value ? pnc : tbl.min_value;
		tbl.max_value = pnc > tbl.max_value ? pnc : tbl.max_value;

		/* check for default proj */
		if (pnc == get_ia32_pncode(node)) {
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
	be_emit_cstring(env, "\tcmpl $");
	be_emit_irprintf(env->emit, "%u, ", interval);
	ia32_emit_source_register(env, node, 0);
	be_emit_finish_line_gas(env, node);

	be_emit_cstring(env, "\tja ");
	ia32_emit_cfop_target(env, tbl.defProj);
	be_emit_finish_line_gas(env, node);

	if (tbl.num_branches > 1) {
		/* create table */
		be_emit_cstring(env, "\tjmp *");
		be_emit_string(env, tbl.label);
		be_emit_cstring(env, "(,");
		ia32_emit_source_register(env, node, 0);
		be_emit_cstring(env, ",4)");
		be_emit_finish_line_gas(env, node);

		be_gas_emit_switch_section(env->emit, GAS_SECTION_RODATA);
		be_emit_cstring(env, "\t.align 4\n");
		be_emit_write_line(env);

		be_emit_string(env, tbl.label);
		be_emit_cstring(env, ":\n");
		be_emit_write_line(env);

		be_emit_cstring(env, ".long ");
		ia32_emit_cfop_target(env, tbl.branches[0].target);
		be_emit_finish_line_gas(env, NULL);

		last_value = tbl.branches[0].value;
		for (i = 1; i < tbl.num_branches; ++i) {
			while (++last_value < tbl.branches[i].value) {
				be_emit_cstring(env, ".long ");
				ia32_emit_cfop_target(env, tbl.defProj);
				be_emit_finish_line_gas(env, NULL);
			}
			be_emit_cstring(env, ".long ");
			ia32_emit_cfop_target(env, tbl.branches[i].target);
			be_emit_finish_line_gas(env, NULL);
		}
		be_gas_emit_switch_section(env->emit, GAS_SECTION_TEXT);
	} else {
		/* one jump is enough */
		be_emit_cstring(env, "\tjmp ");
		ia32_emit_cfop_target(env, tbl.branches[0].target);
		be_emit_finish_line_gas(env, node);
	}

	if (tbl.label)
		free(tbl.label);
	if (tbl.branches)
		free(tbl.branches);
}

/**
 * Emits code for a unconditional jump.
 */
static
void emit_Jmp(ia32_emit_env_t *env, const ir_node *node) {
	ir_node *block, *next_block;

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	/* we have a block schedule */
	next_block = next_blk_sched(block);
	if (get_cfop_target_block(node) != next_block) {
		be_emit_cstring(env, "\tjmp ");
		ia32_emit_cfop_target(env, node);
	} else {
		be_emit_cstring(env, "\t/* fallthrough to ");
		ia32_emit_cfop_target(env, node);
		be_emit_cstring(env, " */");
	}
	be_emit_finish_line_gas(env, node);
}

static
void emit_ia32_Immediate(ia32_emit_env_t *env, const ir_node *node)
{
	const ia32_immediate_attr_t *attr = get_ia32_immediate_attr_const(node);

	be_emit_char(env, '$');
	if(attr->symconst != NULL) {
		ident *id = get_entity_ld_ident(attr->symconst);

		if(attr->attr.data.am_sc_sign)
			be_emit_char(env, '-');
		be_emit_ident(env, id);
	}
	if(attr->symconst == NULL || attr->offset != 0) {
		if(attr->symconst != NULL)
			be_emit_char(env, '+');
		be_emit_irprintf(env->emit, "%d", attr->offset);
	}
}

static
const char* emit_asm_operand(ia32_emit_env_t *env, const ir_node *node,
                             const char *s)
{
	const arch_register_t *reg;
	const char            *reg_name;
	char                   c;
	char                   modifier = 0;
	int                    num      = -1;
	const ia32_attr_t     *attr;
	int                    n_outs;
	int                    p;

	assert(*s == '%');
	c = *(++s);

	/* parse modifiers */
	switch(c) {
	case 0:
		ir_fprintf(stderr, "Warning: asm text (%+F) ends with %\n", node);
		be_emit_char(env, '%');
		return s + 1;
	case '%':
		be_emit_char(env, '%');
		return s + 1;
	case 'w':
	case 'b':
	case 'h':
		modifier = c;
		++s;
		break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		break;
	default:
		ir_fprintf(stderr, "Warning: asm text (%+F) contains unknown modifier "
		           "'%c' for asm op\n", node, c);
		++s;
		break;
	}

	/* parse number */
	sscanf(s, "%d%n", &num, &p);
	if(num < 0) {
		ir_fprintf(stderr, "Warning: Couldn't parse assembler operand (%+F)\n",
		           node);
		return s;
	} else {
		s += p;
	}

	/* get register */
	attr   = get_ia32_attr_const(node);
	n_outs = ARR_LEN(attr->slots);
	if(num < n_outs) {
		reg = get_out_reg(env, node, num);
	} else {
		ir_node *pred;
		int      in = num - n_outs;
		if(in >= get_irn_arity(node)) {
			ir_fprintf(stderr, "Warning: Invalid input %d specified in asm "
			           "op (%+F)\n", num, node);
			return s;
		}
		pred = get_irn_n(node, in);
		/* might be an immediate value */
		if(is_ia32_Immediate(pred)) {
			emit_ia32_Immediate(env, pred);
			return s;
		}
		reg = get_in_reg(env, node, in);
	}
	if(reg == NULL) {
		ir_fprintf(stderr, "Warning: no register assigned for %d asm op "
		           "(%+F)\n", num, node);
		return s;
	}

	/* emit it */
	be_emit_char(env, '%');
	switch(modifier) {
	case 0:
		reg_name = arch_register_get_name(reg);
		break;
	case 'b':
		reg_name = ia32_get_mapped_reg_name(env->isa->regs_8bit, reg);
		break;
	case 'h':
		reg_name = ia32_get_mapped_reg_name(env->isa->regs_8bit_high, reg);
		break;
	case 'w':
		reg_name = ia32_get_mapped_reg_name(env->isa->regs_16bit, reg);
		break;
	default:
		panic("Invalid asm op modifier");
	}
	be_emit_string(env, reg_name);

	return s;
}

/**
 * Emits code for an ASM pseudo op.
 */
static
void emit_ia32_Asm(ia32_emit_env_t *env, const ir_node *node)
{
	const void            *gen_attr = get_irn_generic_attr_const(node);
	const ia32_asm_attr_t *attr
		= CONST_CAST_IA32_ATTR(ia32_asm_attr_t, gen_attr);
	ident                 *asm_text = attr->asm_text;
	const char            *s        = get_id_str(asm_text);

	be_emit_cstring(env, "# Begin ASM \t");
	be_emit_finish_line_gas(env, node);

	if (s[0] != '\t')
		be_emit_char(env, '\t');

	while(*s != 0) {
		if(*s == '%') {
			s = emit_asm_operand(env, node, s);
			continue;
		} else {
			be_emit_char(env, *s);
		}
		++s;
	}

	be_emit_char(env, '\n');
	be_emit_write_line(env);

	be_emit_cstring(env, "# End ASM\n");
	be_emit_write_line(env);
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
static
void emit_CopyB_prolog(ia32_emit_env_t *env, int rem) {
	be_emit_cstring(env, "\tcld");
	be_emit_finish_line_gas(env, NULL);

	switch(rem) {
	case 1:
		be_emit_cstring(env, "\tmovsb");
		be_emit_finish_line_gas(env, NULL);
		break;
	case 2:
		be_emit_cstring(env, "\tmovsw");
		be_emit_finish_line_gas(env, NULL);
		break;
	case 3:
		be_emit_cstring(env, "\tmovsb");
		be_emit_finish_line_gas(env, NULL);
		be_emit_cstring(env, "\tmovsw");
		be_emit_finish_line_gas(env, NULL);
		break;
	}
}

/**
 * Emit rep movsd instruction for memcopy.
 */
static
void emit_ia32_CopyB(ia32_emit_env_t *env, const ir_node *node) {
	tarval *tv = get_ia32_Immop_tarval(node);
	int    rem = get_tarval_long(tv);

	emit_CopyB_prolog(env, rem);

	be_emit_cstring(env, "\trep movsd");
	be_emit_finish_line_gas(env, node);
}

/**
 * Emits unrolled memcopy.
 */
static
void emit_ia32_CopyB_i(ia32_emit_env_t *env, const ir_node *node) {
	tarval *tv   = get_ia32_Immop_tarval(node);
	int     size = get_tarval_long(tv);

	emit_CopyB_prolog(env, size & 0x3);

	size >>= 2;
	while (size--) {
		be_emit_cstring(env, "\tmovsd");
		be_emit_finish_line_gas(env, NULL);
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
static
void emit_ia32_Conv_with_FP(ia32_emit_env_t *env, const ir_node *node) {
	ir_mode            *ls_mode = get_ia32_ls_mode(node);
	int                 ls_bits = get_mode_size_bits(ls_mode);

	be_emit_cstring(env, "\tcvt");

	if(is_ia32_Conv_I2FP(node)) {
		if(ls_bits == 32) {
			be_emit_cstring(env, "si2ss");
		} else {
			be_emit_cstring(env, "si2sd");
		}
	} else if(is_ia32_Conv_FP2I(node)) {
		if(ls_bits == 32) {
			be_emit_cstring(env, "ss2si");
		} else {
			be_emit_cstring(env, "sd2si");
		}
	} else {
		assert(is_ia32_Conv_FP2FP(node));
		if(ls_bits == 32) {
			be_emit_cstring(env, "sd2ss");
		} else {
			be_emit_cstring(env, "ss2sd");
		}
	}
	be_emit_char(env, ' ');

	switch(get_ia32_op_type(node)) {
		case ia32_Normal:
			ia32_emit_source_register(env, node, 2);
			be_emit_cstring(env, ", ");
			ia32_emit_dest_register(env, node, 0);
			break;
		case ia32_AddrModeS:
			ia32_emit_dest_register(env, node, 0);
			be_emit_cstring(env, ", ");
			ia32_emit_am(env, node);
			break;
		default:
			assert(0 && "unsupported op type for Conv");
	}
	be_emit_finish_line_gas(env, node);
}

static
void emit_ia32_Conv_I2FP(ia32_emit_env_t *env, const ir_node *node) {
	emit_ia32_Conv_with_FP(env, node);
}

static
void emit_ia32_Conv_FP2I(ia32_emit_env_t *env, const ir_node *node) {
	emit_ia32_Conv_with_FP(env, node);
}

static
void emit_ia32_Conv_FP2FP(ia32_emit_env_t *env, const ir_node *node) {
	emit_ia32_Conv_with_FP(env, node);
}

/**
 * Emits code for an Int conversion.
 */
static
void emit_ia32_Conv_I2I(ia32_emit_env_t *env, const ir_node *node) {
	const char *sign_suffix;
	ir_mode *smaller_mode = get_ia32_ls_mode(node);
	int smaller_bits = get_mode_size_bits(smaller_mode);
	int signed_mode;
	const arch_register_t *in_reg, *out_reg;

	assert(!mode_is_float(smaller_mode));
	assert(smaller_bits == 8 || smaller_bits == 16 || smaller_bits == 32);

	signed_mode = mode_is_signed(smaller_mode);
	if(smaller_bits == 32) {
		// this should not happen as it's no convert
		assert(0);
		sign_suffix = "";
	} else {
		sign_suffix = signed_mode ? "s" : "z";
	}

	switch(get_ia32_op_type(node)) {
		case ia32_Normal:
			in_reg  = get_in_reg(env, node, 2);
			out_reg = get_out_reg(env, node, 0);

			if (REGS_ARE_EQUAL(in_reg, &ia32_gp_regs[REG_EAX]) &&
				REGS_ARE_EQUAL(out_reg, in_reg)                &&
				signed_mode &&
				smaller_bits == 16)
			{
				/* argument and result are both in EAX and */
				/* signedness is ok: -> use the smaller cwtl opcode */
				be_emit_cstring(env, "\tcwtl");
			} else {
				const char *sreg = ia32_get_reg_name_for_mode(env, smaller_mode, in_reg);

				be_emit_cstring(env, "\tmov");
				be_emit_string(env, sign_suffix);
				ia32_emit_mode_suffix_mode(env, smaller_mode);
				be_emit_cstring(env, "l %");
				be_emit_string(env, sreg);
				be_emit_cstring(env, ", ");
				ia32_emit_dest_register(env, node, 0);
			}
			break;
		case ia32_AddrModeS: {
			be_emit_cstring(env, "\tmov");
			be_emit_string(env, sign_suffix);
			ia32_emit_mode_suffix_mode(env, smaller_mode);
			be_emit_cstring(env, "l %");
			ia32_emit_am(env, node);
			be_emit_cstring(env, ", ");
			ia32_emit_dest_register(env, node, 0);
			break;
		}
		default:
			assert(0 && "unsupported op type for Conv");
	}
	be_emit_finish_line_gas(env, node);
}

/**
 * Emits code for an 8Bit Int conversion.
 */
void emit_ia32_Conv_I2I8Bit(ia32_emit_env_t *env, const ir_node *node) {
	emit_ia32_Conv_I2I(env, node);
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
static
void emit_be_Call(ia32_emit_env_t *env, const ir_node *node) {
	ir_entity *ent = be_Call_get_entity(node);

	be_emit_cstring(env, "\tcall ");
	if (ent) {
		set_entity_backend_marked(ent, 1);
		be_emit_string(env, get_entity_ld_name(ent));
	} else {
		be_emit_char(env, '*');
		ia32_emit_dest_register(env, get_irn_n(node, be_pos_Call_ptr), 0);
	}
	be_emit_finish_line_gas(env, node);
}

/**
 * Emits code to increase stack pointer.
 */
static
void emit_be_IncSP(ia32_emit_env_t *env, const ir_node *node) {
	int offs = be_get_IncSP_offset(node);

	if (offs == 0)
		return;

	if (offs > 0) {
		be_emit_cstring(env, "\tsubl $");
		be_emit_irprintf(env->emit, "%u, ", offs);
		ia32_emit_source_register(env, node, 0);
	} else {
		be_emit_cstring(env, "\taddl $");
		be_emit_irprintf(env->emit, "%u, ", -offs);
		ia32_emit_source_register(env, node, 0);
	}
	be_emit_finish_line_gas(env, node);
}

/**
 * Emits code to set stack pointer.
 */
static
void emit_be_SetSP(ia32_emit_env_t *env, const ir_node *node) {
	be_emit_cstring(env, "\tmovl ");
	ia32_emit_source_register(env, node, 2);
	be_emit_cstring(env, ", ");
	ia32_emit_dest_register(env, node, 0);
	be_emit_finish_line_gas(env, node);
}

/**
 * Emits code for Copy/CopyKeep.
 */
static
void Copy_emitter(ia32_emit_env_t *env, const ir_node *node, const ir_node *op)
{
	const arch_env_t *aenv = env->arch_env;
	ir_mode *mode;

	if (REGS_ARE_EQUAL(arch_get_irn_register(aenv, node), arch_get_irn_register(aenv, op)) ||
		arch_register_type_is(arch_get_irn_register(aenv, op), virtual))
		return;

	mode = get_irn_mode(node);
	if (mode == mode_E) {
		be_emit_cstring(env, "\tmovsd ");
		ia32_emit_source_register(env, node, 0);
		be_emit_cstring(env, ", ");
		ia32_emit_dest_register(env, node, 0);
	} else {
		be_emit_cstring(env, "\tmovl ");
		ia32_emit_source_register(env, node, 0);
		be_emit_cstring(env, ", ");
		ia32_emit_dest_register(env, node, 0);
	}
	be_emit_finish_line_gas(env, node);
}

static
void emit_be_Copy(ia32_emit_env_t *env, const ir_node *node) {
	Copy_emitter(env, node, be_get_Copy_op(node));
}

static
void emit_be_CopyKeep(ia32_emit_env_t *env, const ir_node *node) {
	Copy_emitter(env, node, be_get_CopyKeep_op(node));
}

/**
 * Emits code for exchange.
 */
static
void emit_be_Perm(ia32_emit_env_t *env, const ir_node *node) {
	const arch_register_t *in1, *in2;
	const arch_register_class_t *cls1, *cls2;

	in1 = arch_get_irn_register(env->arch_env, get_irn_n(node, 0));
	in2 = arch_get_irn_register(env->arch_env, get_irn_n(node, 1));

	cls1 = arch_register_get_class(in1);
	cls2 = arch_register_get_class(in2);

	assert(cls1 == cls2 && "Register class mismatch at Perm");

	if (cls1 == &ia32_reg_classes[CLASS_ia32_gp]) {
		be_emit_cstring(env, "\txchg ");
		ia32_emit_source_register(env, node, 1);
		be_emit_cstring(env, ", ");
		ia32_emit_source_register(env, node, 0);
		be_emit_finish_line_gas(env, node);
	} else if (cls1 == &ia32_reg_classes[CLASS_ia32_xmm]) {
		be_emit_cstring(env, "\txorpd ");
		ia32_emit_source_register(env, node, 1);
		be_emit_cstring(env, ", ");
		ia32_emit_source_register(env, node, 0);
		be_emit_finish_line_gas(env, NULL);

		be_emit_cstring(env, "\txorpd ");
		ia32_emit_source_register(env, node, 0);
		be_emit_cstring(env, ", ");
		ia32_emit_source_register(env, node, 1);
		be_emit_finish_line_gas(env, NULL);

		be_emit_cstring(env, "\txorpd ");
		ia32_emit_source_register(env, node, 1);
		be_emit_cstring(env, ", ");
		ia32_emit_source_register(env, node, 0);
		be_emit_finish_line_gas(env, node);
	} else if (cls1 == &ia32_reg_classes[CLASS_ia32_vfp]) {
		/* is a NOP */
	} else if (cls1 == &ia32_reg_classes[CLASS_ia32_st]) {
		/* is a NOP */
	}
}

/**
 * Emits code for Constant loading.
 */
static
void emit_ia32_Const(ia32_emit_env_t *env, const ir_node *node) {
	ia32_immop_type_t imm_tp = get_ia32_immop_type(node);

	if (imm_tp == ia32_ImmSymConst) {
		be_emit_cstring(env, "\tmovl ");
		ia32_emit_immediate(env, node);
		be_emit_cstring(env, ", ");
		ia32_emit_dest_register(env, node, 0);
	} else {
		tarval *tv = get_ia32_Immop_tarval(node);
		assert(get_irn_mode(node) == mode_Iu);
		/* beware: in some rare cases mode is mode_b which has no tarval_null() */
		if (tarval_is_null(tv)) {
			if (env->isa->opt_arch == arch_pentium_4) {
				/* P4 prefers sub r, r, others xor r, r */
				be_emit_cstring(env, "\tsubl ");
			} else {
				be_emit_cstring(env, "\txorl ");
			}
			ia32_emit_dest_register(env, node, 0);
			be_emit_cstring(env, ", ");
			ia32_emit_dest_register(env, node, 0);
		} else {
			be_emit_cstring(env, "\tmovl ");
			ia32_emit_immediate(env, node);
			be_emit_cstring(env, ", ");
			ia32_emit_dest_register(env, node, 0);
		}
	}
	be_emit_finish_line_gas(env, node);
}

/**
 * Emits code to load the TLS base
 */
static
void emit_ia32_LdTls(ia32_emit_env_t *env, const ir_node *node) {
	be_emit_cstring(env, "\tmovl %gs:0, ");
	ia32_emit_dest_register(env, node, 0);
	be_emit_finish_line_gas(env, node);
}

static
void emit_be_Return(ia32_emit_env_t *env, const ir_node *node)
{
	be_emit_cstring(env, "\tret");
	be_emit_finish_line_gas(env, node);
}

static
void emit_Nothing(ia32_emit_env_t *env, const ir_node *node)
{
	(void) env;
	(void) node;
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
static
void ia32_register_emitters(void) {

#define IA32_EMIT2(a,b) op_ia32_##a->ops.generic = (op_func)emit_ia32_##b
#define IA32_EMIT(a)    IA32_EMIT2(a,a)
#define EMIT(a)         op_##a->ops.generic = (op_func)emit_##a
#define IGN(a)			op_##a->ops.generic = (op_func)emit_Nothing
#define BE_EMIT(a)      op_be_##a->ops.generic = (op_func)emit_be_##a
#define BE_IGN(a)		op_be_##a->ops.generic = (op_func)emit_Nothing

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	ia32_register_spec_emitters();

	/* other ia32 emitter functions */
	IA32_EMIT(Asm);
	IA32_EMIT(CondJmp);
	IA32_EMIT(TestJmp);
	IA32_EMIT(CJmp);
	IA32_EMIT(CJmpAM);
	IA32_EMIT(CmpCMov);
	IA32_EMIT(CmpSet);
	IA32_EMIT(SwitchJmp);
	IA32_EMIT(CopyB);
	IA32_EMIT(CopyB_i);
	IA32_EMIT(Conv_I2FP);
	IA32_EMIT(Conv_FP2I);
	IA32_EMIT(Conv_FP2FP);
	IA32_EMIT(Conv_I2I);
	IA32_EMIT(Conv_I2I8Bit);
	IA32_EMIT(Const);
	IA32_EMIT(LdTls);
	IA32_EMIT(xCmp);
	IA32_EMIT(xCmpSet);
	IA32_EMIT(xCmpCMov);
	IA32_EMIT(xCondJmp);
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
	BE_EMIT(CopyKeep);
	BE_EMIT(Perm);
	BE_EMIT(Return);

	BE_IGN(RegParams);
	BE_IGN(Barrier);
	BE_IGN(Keep);

	/* firm emitter */
	EMIT(Jmp);
	IGN(Proj);
	IGN(Phi);
	IGN(Start);

#undef BE_EMIT
#undef EMIT
#undef IGN
#undef IA32_EMIT2
#undef IA32_EMIT
}

static const char *last_name = NULL;
static unsigned last_line = -1;
static unsigned num = -1;

/**
 * Emit the debug support for node node.
 */
static
void ia32_emit_dbg(ia32_emit_env_t *env, const ir_node *node) {
	dbg_info *db = get_irn_dbg_info(node);
	unsigned lineno;
	const char *fname = be_retrieve_dbg_info(db, &lineno);

	if (! env->cg->birg->main_env->options->stabs_debug_support)
		return;

	if (fname) {
		if (last_name != fname) {
			last_line = -1;
			be_dbg_include_begin(env->cg->birg->main_env->db_handle, fname);
			last_name = fname;
		}
		if (last_line != lineno) {
			char name[64];

			snprintf(name, sizeof(name), ".LM%u", ++num);
			last_line = lineno;
			be_dbg_line(env->cg->birg->main_env->db_handle, lineno, name);
			be_emit_string(env, name);
			be_emit_cstring(env, ":\n");
			be_emit_write_line(env);
		}
	}
}

typedef void (*emit_func_ptr) (ia32_emit_env_t *, const ir_node *);

/**
 * Emits code for a node.
 */
static
void ia32_emit_node(ia32_emit_env_t *env, const ir_node *node) {
	ir_op *op = get_irn_op(node);

	DBG((dbg, LEVEL_1, "emitting code for %+F\n", node));

	if (op->ops.generic) {
		emit_func_ptr func = (emit_func_ptr) op->ops.generic;
		ia32_emit_dbg(env, node);
		(*func) (env, node);
	} else {
		emit_Nothing(env, node);
		ir_fprintf(stderr, "Warning: No emit handler for node %+F (%+G)\n", node, node);
	}
}

/**
 * Emits gas alignment directives
 */
static
void ia32_emit_alignment(ia32_emit_env_t *env, unsigned align, unsigned skip) {
	be_emit_cstring(env, "\t.p2align ");
	be_emit_irprintf(env->emit, "%u,,%u\n", align, skip);
	be_emit_write_line(env);
}

/**
 * Emits gas alignment directives for Functions depended on cpu architecture.
 */
static
void ia32_emit_align_func(ia32_emit_env_t *env, cpu_support cpu) {
	unsigned align;
	unsigned maximum_skip;

	switch (cpu) {
		case arch_i386:
			align = 2;
			break;
		case arch_i486:
			align = 4;
			break;
		case arch_k6:
			align = 5;
			break;
		default:
			align = 4;
	}
	maximum_skip = (1 << align) - 1;
	ia32_emit_alignment(env, align, maximum_skip);
}

/**
 * Emits gas alignment directives for Labels depended on cpu architecture.
 */
static
void ia32_emit_align_label(ia32_emit_env_t *env, cpu_support cpu) {
	unsigned align; unsigned maximum_skip;

	switch (cpu) {
		case arch_i386:
			align = 2;
			break;
		case arch_i486:
			align = 4;
			break;
		case arch_k6:
			align = 5;
			break;
		default:
			align = 4;
	}
	maximum_skip = (1 << align) - 1;
	ia32_emit_alignment(env, align, maximum_skip);
}

/**
 * Test wether a block should be aligned.
 * For cpus in the P4/Athlon class it is usefull to align jump labels to
 * 16 bytes. However we should only do that if the alignment nops before the
 * label aren't executed more often than we have jumps to the label.
 */
static
int should_align_block(ia32_emit_env_t *env, ir_node *block, ir_node *prev) {
	static const double DELTA = .0001;
	ir_exec_freq *exec_freq = env->cg->birg->exec_freq;
	double        block_freq;
	double        prev_freq = 0;  /**< execfreq of the fallthrough block */
	double        jmp_freq  = 0;  /**< execfreq of all non-fallthrough blocks */
	cpu_support   cpu       = env->isa->opt_arch;
	int           i, n_cfgpreds;

	if(exec_freq == NULL)
		return 0;
	if(cpu == arch_i386 || cpu == arch_i486)
		return 0;

	block_freq = get_block_execfreq(exec_freq, block);
	if(block_freq < DELTA)
		return 0;

	n_cfgpreds = get_Block_n_cfgpreds(block);
	for(i = 0; i < n_cfgpreds; ++i) {
		ir_node *pred      = get_Block_cfgpred_block(block, i);
		double   pred_freq = get_block_execfreq(exec_freq, pred);

		if(pred == prev) {
			prev_freq += pred_freq;
		} else {
			jmp_freq  += pred_freq;
		}
	}

	if(prev_freq < DELTA && !(jmp_freq < DELTA))
		return 1;

	jmp_freq /= prev_freq;

	switch (cpu) {
		case arch_athlon:
		case arch_athlon_64:
		case arch_k6:
			return jmp_freq > 3;
		default:
			return jmp_freq > 2;
	}
}

static
void ia32_emit_block_header(ia32_emit_env_t *env, ir_node *block, ir_node *prev)
{
	int           n_cfgpreds;
	int           need_label;
	int           i, arity;
	ir_exec_freq  *exec_freq = env->cg->birg->exec_freq;

	need_label = 1;
	n_cfgpreds = get_Block_n_cfgpreds(block);
	if (n_cfgpreds == 0) {
		need_label = 0;
	} else if (n_cfgpreds == 1) {
		ir_node *pred       = get_Block_cfgpred(block, 0);
		ir_node *pred_block = get_nodes_block(pred);

		/* we don't need labels for fallthrough blocks, however switch-jmps
		 * are no fallthroughs */
		if(pred_block == prev &&
				!(is_Proj(pred) && is_ia32_SwitchJmp(get_Proj_pred(pred)))) {
			need_label = 0;
		} else {
			need_label = 1;
		}
	} else {
		need_label = 1;
	}

	if (should_align_block(env, block, prev)) {
		assert(need_label);
		ia32_emit_align_label(env, env->isa->opt_arch);
	}

	if(need_label) {
		ia32_emit_block_name(env, block);
		be_emit_char(env, ':');

		be_emit_pad_comment(env);
		be_emit_cstring(env, "   /* preds:");

		/* emit list of pred blocks in comment */
		arity = get_irn_arity(block);
		for (i = 0; i < arity; ++i) {
			ir_node *predblock = get_Block_cfgpred_block(block, i);
			be_emit_irprintf(env->emit, " %d", get_irn_node_nr(predblock));
		}
	} else {
		be_emit_cstring(env, "\t/* ");
		ia32_emit_block_name(env, block);
		be_emit_cstring(env, ": ");
	}
	if (exec_freq != NULL) {
		be_emit_irprintf(env->emit, " freq: %f",
		                 get_block_execfreq(exec_freq, block));
	}
	be_emit_cstring(env, " */\n");
	be_emit_write_line(env);
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static
void ia32_gen_block(ia32_emit_env_t *env, ir_node *block, ir_node *last_block)
{
	const ir_node *node;

	ia32_emit_block_header(env, block, last_block);

	/* emit the contents of the block */
	ia32_emit_dbg(env, block);
	sched_foreach(block, node) {
		ia32_emit_node(env, node);
	}
}

/**
 * Emits code for function start.
 */
static
void ia32_emit_func_prolog(ia32_emit_env_t *env, ir_graph *irg) {
	ir_entity  *irg_ent  = get_irg_entity(irg);
	const char *irg_name = get_entity_ld_name(irg_ent);
	cpu_support cpu      = env->isa->opt_arch;
	const be_irg_t *birg = env->cg->birg;

	be_emit_write_line(env);
	be_gas_emit_switch_section(env->emit, GAS_SECTION_TEXT);
	be_dbg_method_begin(birg->main_env->db_handle, irg_ent, be_abi_get_stack_layout(birg->abi));
	ia32_emit_align_func(env, cpu);
	if (get_entity_visibility(irg_ent) == visibility_external_visible) {
		be_emit_cstring(env, ".global ");
		be_emit_string(env, irg_name);
		be_emit_char(env, '\n');
		be_emit_write_line(env);
	}
	ia32_emit_function_object(env, irg_name);
	be_emit_string(env, irg_name);
	be_emit_cstring(env, ":\n");
	be_emit_write_line(env);
}

/**
 * Emits code for function end
 */
static
void ia32_emit_func_epilog(ia32_emit_env_t *env, ir_graph *irg) {
	const char *irg_name = get_entity_ld_name(get_irg_entity(irg));
	const be_irg_t *birg = env->cg->birg;

	ia32_emit_function_size(env, irg_name);
	be_dbg_method_end(birg->main_env->db_handle);
	be_emit_char(env, '\n');
	be_emit_write_line(env);
}

/**
 * Block-walker:
 * Sets labels for control flow nodes (jump target)
 */
static
void ia32_gen_labels(ir_node *block, void *data)
{
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);
	(void) data;

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block);
	}
}

/**
 * Emit an exception label if the current instruction can fail.
 */
void ia32_emit_exc_label(ia32_emit_env_t *env, const ir_node *node) {
	if (get_ia32_exc_label(node)) {
		be_emit_irprintf(env->emit, ".EXL%u\n", 0);
		be_emit_write_line(env);
	}
}

/**
 * Main driver. Emits the code for one routine.
 */
void ia32_gen_routine(ia32_code_gen_t *cg, ir_graph *irg) {
	ia32_emit_env_t env;
	ir_node *block;
	ir_node *last_block = NULL;
	int i, n;

	env.isa      = (ia32_isa_t *)cg->arch_env->isa;
	env.emit     = &env.isa->emit;
	env.arch_env = cg->arch_env;
	env.cg       = cg;

	ia32_register_emitters();

	ia32_emit_func_prolog(&env, irg);
	irg_block_walk_graph(irg, ia32_gen_labels, NULL, &env);

	n = ARR_LEN(cg->blk_sched);
	for (i = 0; i < n;) {
		ir_node *next_bl;

		block   = cg->blk_sched[i];
		++i;
		next_bl = i < n ? cg->blk_sched[i] : NULL;

		/* set here the link. the emitter expects to find the next block here */
		set_irn_link(block, next_bl);
		ia32_gen_block(&env, block, last_block);
		last_block = block;
	}

	ia32_emit_func_epilog(&env, irg);
}

void ia32_init_emitter(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.emitter");
}
