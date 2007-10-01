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

static const arch_env_t *arch_env;
static const ia32_isa_t *isa;
static ia32_code_gen_t  *cg;

/**
 * Returns the register at in position pos.
 */
static const arch_register_t *get_in_reg(const ir_node *irn, int pos)
{
	ir_node               *op;
	const arch_register_t *reg = NULL;

	assert(get_irn_arity(irn) > pos && "Invalid IN position");

	/* The out register of the operator at position pos is the
	   in register we need. */
	op = get_irn_n(irn, pos);

	reg = arch_get_irn_register(arch_env, op);

	assert(reg && "no in register found");

	if(reg == &ia32_gp_regs[REG_GP_NOREG])
		panic("trying to emit noreg for %+F input %d", irn, pos);

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
static const arch_register_t *get_out_reg(const ir_node *irn, int pos)
{
	ir_node               *proj;
	const arch_register_t *reg = NULL;

	/* 1st case: irn is not of mode_T, so it has only                 */
	/*           one OUT register -> good                             */
	/* 2nd case: irn is of mode_T -> collect all Projs and ask the    */
	/*           Proj with the corresponding projnum for the register */

	if (get_irn_mode(irn) != mode_T) {
		assert(pos == 0);
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
 * Add a number to a prefix. This number will not be used a second time.
 */
static char *get_unique_label(char *buf, size_t buflen, const char *prefix)
{
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

static void emit_8bit_register(const arch_register_t *reg)
{
	const char *reg_name = arch_register_get_name(reg);

	be_emit_char('%');
	be_emit_char(reg_name[1]);
	be_emit_char('l');
}

static void emit_16bit_register(const arch_register_t *reg)
{
	const char *reg_name = ia32_get_mapped_reg_name(isa->regs_16bit, reg);

	be_emit_char('%');
	be_emit_string(reg_name);
}

static void emit_register(const arch_register_t *reg, const ir_mode *mode)
{
	const char *reg_name;

	if(mode != NULL) {
		int size = get_mode_size_bits(mode);
		if(size == 8) {
			emit_8bit_register(reg);
			return;
		} else if(size == 16) {
			emit_16bit_register(reg);
			return;
		} else {
			assert(mode_is_float(mode) || size == 32);
		}
	}

	reg_name = arch_register_get_name(reg);

	be_emit_char('%');
	be_emit_string(reg_name);
}

void ia32_emit_source_register(const ir_node *node, int pos)
{
	const arch_register_t *reg      = get_in_reg(node, pos);

	emit_register(reg, NULL);
}

static void emit_ia32_Immediate(const ir_node *node);

void ia32_emit_8bit_source_register_or_immediate(const ir_node *node, int pos)
{
	const arch_register_t *reg;
	ir_node               *in = get_irn_n(node, pos);
	if(is_ia32_Immediate(in)) {
		emit_ia32_Immediate(in);
		return;
	}

	reg = get_in_reg(node, pos);
	emit_8bit_register(reg);
}

void ia32_emit_dest_register(const ir_node *node, int pos)
{
	const arch_register_t *reg  = get_out_reg(node, pos);

	emit_register(reg, NULL);
}

void ia32_emit_8bit_dest_register(const ir_node *node, int pos)
{
	const arch_register_t *reg  = get_out_reg(node, pos);

	emit_register(reg, mode_Bu);
}

void ia32_emit_x87_register(const ir_node *node, int pos)
{
	const ia32_x87_attr_t *attr = get_ia32_x87_attr_const(node);

	assert(pos < 3);
	be_emit_char('%');
	be_emit_string(attr->x87[pos]->name);
}

static void ia32_emit_mode_suffix_mode(const ir_mode *mode)
{
	if(mode_is_float(mode)) {
		switch(get_mode_size_bits(mode)) {
		case 32: be_emit_char('s'); return;
		case 64: be_emit_char('l'); return;
		case 80: be_emit_char('t'); return;
		}
	} else {
		assert(mode_is_int(mode) || mode_is_reference(mode));
		switch(get_mode_size_bits(mode)) {
		case 64: be_emit_cstring("ll"); return;
				 /* gas docu says q is the suffix but gcc, objdump and icc use
				 	ll apparently */
		case 32: be_emit_char('l'); return;
		case 16: be_emit_char('w'); return;
		case 8:  be_emit_char('b'); return;
		}
	}
	panic("Can't output mode_suffix for %+F\n", mode);
}

void ia32_emit_mode_suffix(const ir_node *node)
{
	ir_mode *mode = get_ia32_ls_mode(node);
	if(mode == NULL)
		mode = mode_Iu;

	ia32_emit_mode_suffix_mode(mode);
}

void ia32_emit_x87_mode_suffix(const ir_node *node)
{
	ir_mode *mode = get_ia32_ls_mode(node);
	if(mode != NULL)
		ia32_emit_mode_suffix_mode(mode);
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

void ia32_emit_xmm_mode_suffix(const ir_node *node)
{
	ir_mode *mode = get_ia32_ls_mode(node);
	assert(mode != NULL);
	be_emit_char('s');
	be_emit_char(get_xmm_mode_suffix(mode));
}

void ia32_emit_xmm_mode_suffix_s(const ir_node *node)
{
	ir_mode *mode = get_ia32_ls_mode(node);
	assert(mode != NULL);
	be_emit_char(get_xmm_mode_suffix(mode));
}

void ia32_emit_extend_suffix(const ir_mode *mode)
{
	if(get_mode_size_bits(mode) == 32)
		return;
	if(mode_is_signed(mode)) {
		be_emit_char('s');
	} else {
		be_emit_char('z');
	}
}

static
void ia32_emit_function_object(const char *name)
{
	switch (be_gas_flavour) {
	case GAS_FLAVOUR_NORMAL:
		be_emit_cstring("\t.type\t");
		be_emit_string(name);
		be_emit_cstring(", @function\n");
		be_emit_write_line();
		break;
	case GAS_FLAVOUR_MINGW:
		be_emit_cstring("\t.def\t");
		be_emit_string(name);
		be_emit_cstring(";\t.scl\t2;\t.type\t32;\t.endef\n");
		be_emit_write_line();
		break;
	default:
		break;
	}
}

static
void ia32_emit_function_size(const char *name)
{
	switch (be_gas_flavour) {
	case GAS_FLAVOUR_NORMAL:
		be_emit_cstring("\t.size\t");
		be_emit_string(name);
		be_emit_cstring(", .-");
		be_emit_string(name);
		be_emit_char('\n');
		be_emit_write_line();
		break;
	default:
		break;
	}
}


void ia32_emit_source_register_or_immediate(const ir_node *node, int pos)
{
	ir_node *in = get_irn_n(node, pos);
	if(is_ia32_Immediate(in)) {
		emit_ia32_Immediate(in);
	} else {
		const ir_mode         *mode = get_ia32_ls_mode(node);
		const arch_register_t *reg  = get_in_reg(node, pos);
		emit_register(reg, mode);
	}
}

/**
 * Emits registers and/or address mode of a binary operation.
 */
void ia32_emit_binop(const ir_node *node) {
	const ir_node         *right_op  = get_irn_n(node, n_ia32_binary_right);
	const ir_mode         *mode      = get_ia32_ls_mode(node);
	const arch_register_t *reg_left;

	switch(get_ia32_op_type(node)) {
	case ia32_Normal:
		reg_left = get_in_reg(node, n_ia32_binary_left);
		if(is_ia32_Immediate(right_op)) {
			emit_ia32_Immediate(right_op);
			be_emit_cstring(", ");
			emit_register(reg_left, mode);
			break;
		} else {
			const arch_register_t *reg_right
				= get_in_reg(node, n_ia32_binary_right);
			emit_register(reg_right, mode);
			be_emit_cstring(", ");
			emit_register(reg_left, mode);
		}
		break;
	case ia32_AddrModeS:
		if(is_ia32_Immediate(right_op)) {
			emit_ia32_Immediate(right_op);
			be_emit_cstring(", ");
			ia32_emit_am(node);
		} else {
			reg_left = get_in_reg(node, n_ia32_binary_left);
			ia32_emit_am(node);
			be_emit_cstring(", ");
			emit_register(reg_left, mode);
		}
		break;
	case ia32_AddrModeD:
		panic("DestMode can't be output by %%binop anymore");
		break;
	default:
		assert(0 && "unsupported op type");
	}
}

/**
 * Emits registers and/or address mode of a binary operation.
 */
void ia32_emit_x87_binop(const ir_node *node) {
	switch(get_ia32_op_type(node)) {
		case ia32_Normal:
			{
				const ia32_x87_attr_t *x87_attr = get_ia32_x87_attr_const(node);
				const arch_register_t *in1      = x87_attr->x87[0];
				const arch_register_t *in2      = x87_attr->x87[1];
				const arch_register_t *out      = x87_attr->x87[2];
				const arch_register_t *in;

				in  = out ? ((out == in2) ? in1 : in2) : in2;
				out = out ? out : in1;

				be_emit_char('%');
				be_emit_string(arch_register_get_name(in));
				be_emit_cstring(", %");
				be_emit_string(arch_register_get_name(out));
			}
			break;
		case ia32_AddrModeS:
			ia32_emit_am(node);
			break;
		case ia32_AddrModeD:
		default:
			assert(0 && "unsupported op type");
	}
}

void ia32_emit_am_or_dest_register(const ir_node *node,
                                   int pos) {
	if(get_ia32_op_type(node) == ia32_Normal) {
		ia32_emit_dest_register(node, pos);
	} else {
		assert(get_ia32_op_type(node) == ia32_AddrModeD);
		ia32_emit_am(node);
	}
}

/**
 * Emits registers and/or address mode of a unary operation.
 */
void ia32_emit_unop(const ir_node *node, int pos) {
	const ir_node *op;

	switch(get_ia32_op_type(node)) {
	case ia32_Normal:
		op = get_irn_n(node, pos);
		if (is_ia32_Immediate(op)) {
			emit_ia32_Immediate(op);
		} else {
			ia32_emit_source_register(node, pos);
		}
		break;
	case ia32_AddrModeS:
	case ia32_AddrModeD:
		ia32_emit_am(node);
		break;
	default:
		assert(0 && "unsupported op type");
	}
}

/**
 * Emits address mode.
 */
void ia32_emit_am(const ir_node *node) {
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
			be_emit_char('-');
		be_emit_ident(id);

		if(get_entity_owner(ent) == get_tls_type()) {
			if (get_entity_visibility(ent) == visibility_external_allocated) {
				be_emit_cstring("@INDNTPOFF");
			} else {
				be_emit_cstring("@NTPOFF");
			}
		}
	}

	if(offs != 0) {
		if(ent != NULL) {
			be_emit_irprintf("%+d", offs);
		} else {
			be_emit_irprintf("%d", offs);
		}
	}

	if (has_base || has_index) {
		be_emit_char('(');

		/* emit base */
		if (has_base) {
			const arch_register_t *reg = get_in_reg(node, n_ia32_base);
			emit_register(reg, NULL);
		}

		/* emit index + scale */
		if (has_index) {
			const arch_register_t *reg = get_in_reg(node, n_ia32_index);
			int scale;
			be_emit_char(',');
			emit_register(reg, NULL);

			scale = get_ia32_am_scale(node);
			if (scale > 0) {
				be_emit_irprintf(",%d", 1 << get_ia32_am_scale(node));
			}
		}
		be_emit_char(')');
	}

	/* special case if nothing is set */
	if(ent == NULL && offs == 0 && !has_base && !has_index) {
		be_emit_char('0');
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
	int         num;
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
	{ NULL,              pn_Cmp_Leg},     /* always true */
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
	{ NULL,              pn_Cmp_Leg },   /* always true  */
};

enum {
	ia32_pn_Cmp_unsigned = 0x1000,
	ia32_pn_Cmp_float    = 0x2000,
};

/**
 * walks up a tree of copies/perms/spills/reloads to find the original value
 * that is moved around
 */
static ir_node *find_original_value(ir_node *node)
{
	inc_irg_visited(current_ir_graph);
	while(1) {
		mark_irn_visited(node);
		if(be_is_Copy(node)) {
			node = be_get_Copy_op(node);
		} else if(be_is_CopyKeep(node)) {
			node = be_get_CopyKeep_op(node);
		} else if(is_Proj(node)) {
			ir_node *pred = get_Proj_pred(node);
			if(be_is_Perm(pred)) {
				node = get_irn_n(pred, get_Proj_proj(node));
			} else if(be_is_MemPerm(pred)) {
				node = get_irn_n(pred, get_Proj_proj(node) + 1);
			} else if(is_ia32_Load(pred)) {
				node = get_irn_n(pred, n_ia32_Load_mem);
			} else {
				return node;
			}
		} else if(is_ia32_Store(node)) {
			node = get_irn_n(node, n_ia32_Store_val);
		} else if(is_Phi(node)) {
			int i, arity;
			arity = get_irn_arity(node);
			for(i = 0; i < arity; ++i) {
				ir_node *in = get_irn_n(node, i);
				if(irn_visited(in))
					continue;
				node = in;
				break;
			}
			assert(i < arity);
		} else {
			return node;
		}
	}
}

static int determine_final_pnc(const ir_node *node, int flags_pos,
                               int pnc)
{
	ir_node           *flags = get_irn_n(node, flags_pos);
	const ia32_attr_t *flags_attr;
	flags = skip_Proj(flags);

	if(is_ia32_Sahf(flags)) {
		ir_node *cmp = get_irn_n(flags, n_ia32_Sahf_val);
		if(!(is_ia32_FucomFnstsw(cmp) || is_ia32_FucompFnstsw(cmp)
				|| is_ia32_FucomppFnstsw(cmp) || is_ia32_FtstFnstsw(cmp))) {
			cmp = find_original_value(cmp);
			assert(is_ia32_FucomFnstsw(cmp) || is_ia32_FucompFnstsw(cmp)
			       || is_ia32_FucomppFnstsw(cmp) || is_ia32_FtstFnstsw(cmp));
		}

		flags_attr = get_ia32_attr_const(cmp);
		if(flags_attr->data.cmp_flipped)
			pnc = get_mirrored_pnc(pnc);
		pnc |= ia32_pn_Cmp_float;
	} else if(is_ia32_Ucomi(flags) || is_ia32_Fucomi(flags)
			|| is_ia32_Fucompi(flags)) {
		flags_attr = get_ia32_attr_const(flags);

		if(flags_attr->data.cmp_flipped)
			pnc = get_mirrored_pnc(pnc);
		pnc |= ia32_pn_Cmp_float;
	} else {
		assert(is_ia32_Cmp(flags) || is_ia32_Test(flags)
				|| is_ia32_Cmp8Bit(flags) || is_ia32_Test8Bit(flags));
		flags_attr = get_ia32_attr_const(flags);

		if(flags_attr->data.cmp_flipped)
			pnc = get_mirrored_pnc(pnc);
		if(flags_attr->data.cmp_unsigned)
			pnc |= ia32_pn_Cmp_unsigned;
	}

	return pnc;
}

static void ia32_emit_cmp_suffix(int pnc)
{
	const char        *str;

	if((pnc & ia32_pn_Cmp_float) || (pnc & ia32_pn_Cmp_unsigned)) {
		pnc = pnc & 7;
		assert(cmp2condition_u[pnc].num == pnc);
		str = cmp2condition_u[pnc].name;
	} else {
		pnc = pnc & 7;
		assert(cmp2condition_s[pnc].num == pnc);
		str = cmp2condition_s[pnc].name;
	}

	be_emit_string(str);
}

void ia32_emit_cmp_suffix_node(const ir_node *node,
                               int flags_pos)
{
	pn_Cmp pnc = get_ia32_pncode(node);

	pnc = determine_final_pnc(node, flags_pos, pnc);
	ia32_emit_cmp_suffix(pnc);
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
void ia32_emit_block_name(const ir_node *block)
{
	if (has_Block_label(block)) {
		be_emit_string(be_gas_label_prefix());
		be_emit_irprintf("%u", (unsigned)get_Block_label(block));
	} else {
		be_emit_cstring(BLOCK_PREFIX);
		be_emit_irprintf("%d", get_irn_node_nr(block));
	}
}

/**
 * Emits the target label for a control flow node.
 */
static void ia32_emit_cfop_target(const ir_node *node)
{
	ir_node *block = get_cfop_target_block(node);

	ia32_emit_block_name(block);
}

/** Return the next block in Block schedule */
static ir_node *next_blk_sched(const ir_node *block)
{
	return get_irn_link(block);
}

/**
 * Returns the Proj with projection number proj and NOT mode_M
 */
static ir_node *get_proj(const ir_node *node, long proj) {
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
static void emit_ia32_Jcc(const ir_node *node)
{
	const ir_node *proj_true;
	const ir_node *proj_false;
	const ir_node *block;
	const ir_node *next_block;
	pn_Cmp         pnc = get_ia32_pncode(node);

	pnc = determine_final_pnc(node, 0, pnc);

	/* get both Projs */
	proj_true = get_proj(node, pn_ia32_Jcc_true);
	assert(proj_true && "Jcc without true Proj");

	proj_false = get_proj(node, pn_ia32_Jcc_false);
	assert(proj_false && "Jcc without false Proj");

	block      = get_nodes_block(node);
	next_block = next_blk_sched(block);

	if (get_cfop_target_block(proj_true) == next_block) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj_true;

		proj_true  = proj_false;
		proj_false = t;
		if(pnc & ia32_pn_Cmp_float) {
			pnc = get_negated_pnc(pnc, mode_F);
		} else {
			pnc = get_negated_pnc(pnc, mode_Iu);
		}
	}

	if (pnc & ia32_pn_Cmp_float) {
		/* Some floating point comparisons require a test of the parity flag,
		 * which indicates that the result is unordered */
		switch (pnc & 15) {
			case pn_Cmp_Uo:
				be_emit_cstring("\tjp ");
				ia32_emit_cfop_target(proj_true);
				be_emit_finish_line_gas(proj_true);
				break;

			case pn_Cmp_Leg:
				be_emit_cstring("\tjnp ");
				ia32_emit_cfop_target(proj_true);
				be_emit_finish_line_gas(proj_true);
				break;

			case pn_Cmp_Eq:
			case pn_Cmp_Lt:
			case pn_Cmp_Le:
				be_emit_cstring("\tjp ");
				ia32_emit_cfop_target(proj_false);
				be_emit_finish_line_gas(proj_false);
				goto emit_jcc;

			case pn_Cmp_Ug:
			case pn_Cmp_Uge:
			case pn_Cmp_Ne:
				be_emit_cstring("\tjp ");
				ia32_emit_cfop_target(proj_true);
				be_emit_finish_line_gas(proj_true);
				goto emit_jcc;

			default:
				goto emit_jcc;
		}
	} else {
emit_jcc:
		be_emit_cstring("\tj");
		ia32_emit_cmp_suffix(pnc);
		be_emit_char(' ');
		ia32_emit_cfop_target(proj_true);
		be_emit_finish_line_gas(proj_true);
	}

	/* the second Proj might be a fallthrough */
	if (get_cfop_target_block(proj_false) != next_block) {
		be_emit_cstring("\tjmp ");
		ia32_emit_cfop_target(proj_false);
		be_emit_finish_line_gas(proj_false);
	} else {
		be_emit_cstring("\t/* fallthrough to ");
		ia32_emit_cfop_target(proj_false);
		be_emit_cstring(" */");
		be_emit_finish_line_gas(proj_false);
	}
}

static void emit_ia32_CMov(const ir_node *node)
{
	const ia32_attr_t     *attr    = get_ia32_attr_const(node);
	const arch_register_t *out     = arch_get_irn_register(arch_env, node);
	pn_Cmp                 pnc     = get_ia32_pncode(node);
	int                    flipped = attr->data.cmp_flipped;
	const arch_register_t *in_true;
	const arch_register_t *in_false;

	pnc = determine_final_pnc(node, n_ia32_CMov_eflags, pnc);

	in_true  = arch_get_irn_register(arch_env,
	                                 get_irn_n(node, n_ia32_CMov_val_true));
	in_false = arch_get_irn_register(arch_env,
	                                 get_irn_n(node, n_ia32_CMov_val_false));

	/* should be same constraint fullfilled? */
	if(out == in_false) {
		/* yes -> nothing to do */
	} else if(out == in_true) {
		const arch_register_t *tmp;

		flipped = !flipped;

		tmp      = in_true;
		in_true  = in_false;
		in_false = tmp;
	} else {
		/* we need a mov */
		be_emit_cstring("\tmovl ");
		emit_register(in_false, NULL);
		be_emit_cstring(", ");
		emit_register(out, NULL);
		be_emit_finish_line_gas(node);
	}

	if(flipped) {
		if(pnc & ia32_pn_Cmp_float) {
			pnc = get_negated_pnc(pnc, mode_F);
		} else {
			pnc = get_negated_pnc(pnc, mode_Iu);
		}
	}

	/* TODO: handling of Nans isn't correct yet */

	be_emit_cstring("\tcmov");
	ia32_emit_cmp_suffix(pnc);
	be_emit_char(' ');
	if(get_ia32_op_type(node) == ia32_AddrModeS) {
		ia32_emit_am(node);
	} else {
		emit_register(in_true, get_ia32_ls_mode(node));
	}
	be_emit_cstring(", ");
	emit_register(out, get_ia32_ls_mode(node));
	be_emit_finish_line_gas(node);
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
void emit_ia32_SwitchJmp(const ir_node *node) {
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
	be_emit_cstring("\tcmpl $");
	be_emit_irprintf("%u, ", interval);
	ia32_emit_source_register(node, 0);
	be_emit_finish_line_gas(node);

	be_emit_cstring("\tja ");
	ia32_emit_cfop_target(tbl.defProj);
	be_emit_finish_line_gas(node);

	if (tbl.num_branches > 1) {
		/* create table */
		be_emit_cstring("\tjmp *");
		be_emit_string(tbl.label);
		be_emit_cstring("(,");
		ia32_emit_source_register(node, 0);
		be_emit_cstring(",4)");
		be_emit_finish_line_gas(node);

		be_gas_emit_switch_section(GAS_SECTION_RODATA);
		be_emit_cstring("\t.align 4\n");
		be_emit_write_line();

		be_emit_string(tbl.label);
		be_emit_cstring(":\n");
		be_emit_write_line();

		be_emit_cstring(".long ");
		ia32_emit_cfop_target(tbl.branches[0].target);
		be_emit_finish_line_gas(NULL);

		last_value = tbl.branches[0].value;
		for (i = 1; i < tbl.num_branches; ++i) {
			while (++last_value < tbl.branches[i].value) {
				be_emit_cstring(".long ");
				ia32_emit_cfop_target(tbl.defProj);
				be_emit_finish_line_gas(NULL);
			}
			be_emit_cstring(".long ");
			ia32_emit_cfop_target(tbl.branches[i].target);
			be_emit_finish_line_gas(NULL);
		}
		be_gas_emit_switch_section(GAS_SECTION_TEXT);
	} else {
		/* one jump is enough */
		be_emit_cstring("\tjmp ");
		ia32_emit_cfop_target(tbl.branches[0].target);
		be_emit_finish_line_gas(node);
	}

	if (tbl.label)
		free(tbl.label);
	if (tbl.branches)
		free(tbl.branches);
}

/**
 * Emits code for a unconditional jump.
 */
static void emit_Jmp(const ir_node *node)
{
	ir_node *block, *next_block;

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	/* we have a block schedule */
	next_block = next_blk_sched(block);
	if (get_cfop_target_block(node) != next_block) {
		be_emit_cstring("\tjmp ");
		ia32_emit_cfop_target(node);
	} else {
		be_emit_cstring("\t/* fallthrough to ");
		ia32_emit_cfop_target(node);
		be_emit_cstring(" */");
	}
	be_emit_finish_line_gas(node);
}

static void emit_ia32_Immediate(const ir_node *node)
{
	const ia32_immediate_attr_t *attr = get_ia32_immediate_attr_const(node);

	be_emit_char('$');
	if(attr->symconst != NULL) {
		ident *id = get_entity_ld_ident(attr->symconst);

		if(attr->attr.data.am_sc_sign)
			be_emit_char('-');
		be_emit_ident(id);
	}
	if(attr->symconst == NULL || attr->offset != 0) {
		if(attr->symconst != NULL) {
			be_emit_irprintf("%+d", attr->offset);
		} else {
			be_emit_irprintf("0x%X", attr->offset);
		}
	}
}

static const char* emit_asm_operand(const ir_node *node, const char *s)
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
		be_emit_char('%');
		return s + 1;
	case '%':
		be_emit_char('%');
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
		reg = get_out_reg(node, num);
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
			emit_ia32_Immediate(pred);
			return s;
		}
		reg = get_in_reg(node, in);
	}
	if(reg == NULL) {
		ir_fprintf(stderr, "Warning: no register assigned for %d asm op "
		           "(%+F)\n", num, node);
		return s;
	}

	/* emit it */
	be_emit_char('%');
	switch(modifier) {
	case 0:
		reg_name = arch_register_get_name(reg);
		break;
	case 'b':
		reg_name = ia32_get_mapped_reg_name(isa->regs_8bit, reg);
		break;
	case 'h':
		reg_name = ia32_get_mapped_reg_name(isa->regs_8bit_high, reg);
		break;
	case 'w':
		reg_name = ia32_get_mapped_reg_name(isa->regs_16bit, reg);
		break;
	default:
		panic("Invalid asm op modifier");
	}
	be_emit_string(reg_name);

	return s;
}

/**
 * Emits code for an ASM pseudo op.
 */
static void emit_ia32_Asm(const ir_node *node)
{
	const void            *gen_attr = get_irn_generic_attr_const(node);
	const ia32_asm_attr_t *attr
		= CONST_CAST_IA32_ATTR(ia32_asm_attr_t, gen_attr);
	ident                 *asm_text = attr->asm_text;
	const char            *s        = get_id_str(asm_text);

	be_emit_cstring("# Begin ASM \t");
	be_emit_finish_line_gas(node);

	if (s[0] != '\t')
		be_emit_char('\t');

	while(*s != 0) {
		if(*s == '%') {
			s = emit_asm_operand(node, s);
			continue;
		} else {
			be_emit_char(*s);
		}
		++s;
	}

	be_emit_char('\n');
	be_emit_write_line();

	be_emit_cstring("# End ASM\n");
	be_emit_write_line();
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
static void emit_CopyB_prolog(int rem) {
	be_emit_cstring("\tcld");
	be_emit_finish_line_gas(NULL);

	switch(rem) {
	case 1:
		be_emit_cstring("\tmovsb");
		be_emit_finish_line_gas(NULL);
		break;
	case 2:
		be_emit_cstring("\tmovsw");
		be_emit_finish_line_gas(NULL);
		break;
	case 3:
		be_emit_cstring("\tmovsb");
		be_emit_finish_line_gas(NULL);
		be_emit_cstring("\tmovsw");
		be_emit_finish_line_gas(NULL);
		break;
	}
}

/**
 * Emit rep movsd instruction for memcopy.
 */
static void emit_ia32_CopyB(const ir_node *node)
{
	int rem = get_ia32_pncode(node);

	emit_CopyB_prolog(rem);

	be_emit_cstring("\trep movsd");
	be_emit_finish_line_gas(node);
}

/**
 * Emits unrolled memcopy.
 */
static void emit_ia32_CopyB_i(const ir_node *node)
{
	int size = get_ia32_pncode(node);

	emit_CopyB_prolog(size & 0x3);

	size >>= 2;
	while (size--) {
		be_emit_cstring("\tmovsd");
		be_emit_finish_line_gas(NULL);
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
static void emit_ia32_Conv_with_FP(const ir_node *node)
{
	ir_mode            *ls_mode = get_ia32_ls_mode(node);
	int                 ls_bits = get_mode_size_bits(ls_mode);

	be_emit_cstring("\tcvt");

	if(is_ia32_Conv_I2FP(node)) {
		if(ls_bits == 32) {
			be_emit_cstring("si2ss");
		} else {
			be_emit_cstring("si2sd");
		}
	} else if(is_ia32_Conv_FP2I(node)) {
		if(ls_bits == 32) {
			be_emit_cstring("ss2si");
		} else {
			be_emit_cstring("sd2si");
		}
	} else {
		assert(is_ia32_Conv_FP2FP(node));
		if(ls_bits == 32) {
			be_emit_cstring("sd2ss");
		} else {
			be_emit_cstring("ss2sd");
		}
	}
	be_emit_char(' ');

	switch(get_ia32_op_type(node)) {
		case ia32_Normal:
			ia32_emit_source_register(node, n_ia32_unary_op);
			break;
		case ia32_AddrModeS:
			ia32_emit_am(node);
			break;
		default:
			assert(0 && "unsupported op type for Conv");
	}
	be_emit_cstring(", ");
	ia32_emit_dest_register(node, 0);
	be_emit_finish_line_gas(node);
}

static void emit_ia32_Conv_I2FP(const ir_node *node)
{
	emit_ia32_Conv_with_FP(node);
}

static void emit_ia32_Conv_FP2I(const ir_node *node)
{
	emit_ia32_Conv_with_FP(node);
}

static void emit_ia32_Conv_FP2FP(const ir_node *node)
{
	emit_ia32_Conv_with_FP(node);
}

/**
 * Emits code for an Int conversion.
 */
static void emit_ia32_Conv_I2I(const ir_node *node)
{
	const char            *sign_suffix;
	ir_mode               *smaller_mode = get_ia32_ls_mode(node);
	int                    smaller_bits = get_mode_size_bits(smaller_mode);
	int                    signed_mode;
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

	out_reg = get_out_reg(node, 0);

	switch(get_ia32_op_type(node)) {
		case ia32_Normal:
			in_reg  = get_in_reg(node, n_ia32_unary_op);

			if (in_reg  == &ia32_gp_regs[REG_EAX] &&
				out_reg == &ia32_gp_regs[REG_EAX] &&
				signed_mode &&
				smaller_bits == 16)
			{
				/* argument and result are both in EAX and */
				/* signedness is ok: -> use the smaller cwtl opcode */
				be_emit_cstring("\tcwtl");
			} else {
				be_emit_cstring("\tmov");
				be_emit_string(sign_suffix);
				ia32_emit_mode_suffix_mode(smaller_mode);
				be_emit_cstring("l ");
				emit_register(in_reg, smaller_mode);
				be_emit_cstring(", ");
				emit_register(out_reg, NULL);
			}
			break;
		case ia32_AddrModeS: {
			be_emit_cstring("\tmov");
			be_emit_string(sign_suffix);
			ia32_emit_mode_suffix_mode(smaller_mode);
			be_emit_cstring("l ");
			ia32_emit_am(node);
			be_emit_cstring(", ");
			emit_register(out_reg, NULL);
			break;
		}
		default:
			assert(0 && "unsupported op type for Conv");
	}
	be_emit_finish_line_gas(node);
}

/**
 * Emits code for an 8Bit Int conversion.
 */
static void emit_ia32_Conv_I2I8Bit(const ir_node *node)
{
	emit_ia32_Conv_I2I(node);
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
static void emit_be_Call(const ir_node *node)
{
	ir_entity *ent = be_Call_get_entity(node);

	be_emit_cstring("\tcall ");
	if (ent) {
		set_entity_backend_marked(ent, 1);
		be_emit_string(get_entity_ld_name(ent));
	} else {
		const arch_register_t *reg = get_in_reg(node, be_pos_Call_ptr);
		be_emit_char('*');
		emit_register(reg, NULL);
	}
	be_emit_finish_line_gas(node);
}

/**
 * Emits code to increase stack pointer.
 */
static void emit_be_IncSP(const ir_node *node)
{
	int                    offs = be_get_IncSP_offset(node);
	const arch_register_t *reg  = arch_get_irn_register(arch_env, node);

	if (offs == 0)
		return;

	if (offs > 0) {
		be_emit_cstring("\tsubl $");
		be_emit_irprintf("%u, ", offs);
		emit_register(reg, NULL);
	} else {
		be_emit_cstring("\taddl $");
		be_emit_irprintf("%u, ", -offs);
		emit_register(reg, NULL);
	}
	be_emit_finish_line_gas(node);
}

/**
 * Emits code for Copy/CopyKeep.
 */
static void Copy_emitter(const ir_node *node, const ir_node *op)
{
	const arch_register_t *in  = arch_get_irn_register(arch_env, op);
	const arch_register_t *out = arch_get_irn_register(arch_env, node);
	ir_mode               *mode;

	if(in == out) {
		return;
	}
	if(is_unknown_reg(in))
		return;
	/* copies of vf nodes aren't real... */
	if(arch_register_get_class(in) == &ia32_reg_classes[CLASS_ia32_vfp])
		return;

	mode = get_irn_mode(node);
	if (mode == mode_E) {
		be_emit_cstring("\tmovsd ");
		emit_register(in, NULL);
		be_emit_cstring(", ");
		emit_register(out, NULL);
	} else {
		be_emit_cstring("\tmovl ");
		emit_register(in, NULL);
		be_emit_cstring(", ");
		emit_register(out, NULL);
	}
	be_emit_finish_line_gas(node);
}

static void emit_be_Copy(const ir_node *node)
{
	Copy_emitter(node, be_get_Copy_op(node));
}

static void emit_be_CopyKeep(const ir_node *node)
{
	Copy_emitter(node, be_get_CopyKeep_op(node));
}

/**
 * Emits code for exchange.
 */
static void emit_be_Perm(const ir_node *node)
{
	const arch_register_t *in0, *in1;
	const arch_register_class_t *cls0, *cls1;

	in0 = arch_get_irn_register(arch_env, get_irn_n(node, 0));
	in1 = arch_get_irn_register(arch_env, get_irn_n(node, 1));

	cls0 = arch_register_get_class(in0);
	cls1 = arch_register_get_class(in1);

	assert(cls0 == cls1 && "Register class mismatch at Perm");

	if (cls0 == &ia32_reg_classes[CLASS_ia32_gp]) {
		be_emit_cstring("\txchg ");
		emit_register(in1, NULL);
		be_emit_cstring(", ");
		emit_register(in0, NULL);
		be_emit_finish_line_gas(node);
	} else if (cls0 == &ia32_reg_classes[CLASS_ia32_xmm]) {
		be_emit_cstring("\txorpd ");
		emit_register(in1, NULL);
		be_emit_cstring(", ");
		emit_register(in0, NULL);
		be_emit_finish_line_gas(NULL);

		be_emit_cstring("\txorpd ");
		emit_register(in0, NULL);
		be_emit_cstring(", ");
		emit_register(in1, NULL);
		be_emit_finish_line_gas(NULL);

		be_emit_cstring("\txorpd ");
		emit_register(in1, NULL);
		be_emit_cstring(", ");
		emit_register(in0, NULL);
		be_emit_finish_line_gas(node);
	} else if (cls0 == &ia32_reg_classes[CLASS_ia32_vfp]) {
		/* is a NOP */
	} else if (cls0 == &ia32_reg_classes[CLASS_ia32_st]) {
		/* is a NOP */
	} else {
		panic("unexpected register class in be_Perm (%+F)\n", node);
	}
}

/**
 * Emits code for Constant loading.
 */
static void emit_ia32_Const(const ir_node *node)
{
	be_emit_cstring("\tmovl ");
	emit_ia32_Immediate(node);
	be_emit_cstring(", ");
	ia32_emit_dest_register(node, 0);

	be_emit_finish_line_gas(node);
}

/**
 * Emits code to load the TLS base
 */
static void emit_ia32_LdTls(const ir_node *node)
{
	be_emit_cstring("\tmovl %gs:0, ");
	ia32_emit_dest_register(node, 0);
	be_emit_finish_line_gas(node);
}

/* helper function for emit_ia32_Minus64Bit */
static void emit_mov(const ir_node* node, const arch_register_t *src, const arch_register_t *dst)
{
	be_emit_cstring("\tmovl ");
	emit_register(src, NULL);
	be_emit_cstring(", ");
	emit_register(dst, NULL);
	be_emit_finish_line_gas(node);
}

/* helper function for emit_ia32_Minus64Bit */
static void emit_neg(const ir_node* node, const arch_register_t *reg)
{
	be_emit_cstring("\tnegl ");
	emit_register(reg, NULL);
	be_emit_finish_line_gas(node);
}

/* helper function for emit_ia32_Minus64Bit */
static void emit_sbb0(const ir_node* node, const arch_register_t *reg)
{
	be_emit_cstring("\tsbbl $0, ");
	emit_register(reg, NULL);
	be_emit_finish_line_gas(node);
}

/* helper function for emit_ia32_Minus64Bit */
static void emit_sbb(const ir_node* node, const arch_register_t *src, const arch_register_t *dst)
{
	be_emit_cstring("\tsbbl ");
	emit_register(src, NULL);
	be_emit_cstring(", ");
	emit_register(dst, NULL);
	be_emit_finish_line_gas(node);
}

/* helper function for emit_ia32_Minus64Bit */
static void emit_xchg(const ir_node* node, const arch_register_t *src, const arch_register_t *dst)
{
	be_emit_cstring("\txchgl ");
	emit_register(src, NULL);
	be_emit_cstring(", ");
	emit_register(dst, NULL);
	be_emit_finish_line_gas(node);
}

/* helper function for emit_ia32_Minus64Bit */
static void emit_zero(const ir_node* node, const arch_register_t *reg)
{
	be_emit_cstring("\txorl ");
	emit_register(reg, NULL);
	be_emit_cstring(", ");
	emit_register(reg, NULL);
	be_emit_finish_line_gas(node);
}

static void emit_ia32_Minus64Bit(const ir_node *node)
{
	const arch_register_t *in_lo  = get_in_reg(node, 0);
	const arch_register_t *in_hi  = get_in_reg(node, 1);
	const arch_register_t *out_lo = get_out_reg(node, 0);
	const arch_register_t *out_hi = get_out_reg(node, 1);

	if (out_lo == in_lo) {
		if (out_hi != in_hi) {
			/* a -> a, b -> d */
			goto zero_neg;
		} else {
			/* a -> a, b -> b */
			goto normal_neg;
		}
	} else if (out_lo == in_hi) {
		if (out_hi == in_lo) {
			/* a -> b, b -> a */
			emit_xchg(node, in_lo, in_hi);
			goto normal_neg;
		} else {
			/* a -> b, b -> d */
			emit_mov(node, in_hi, out_hi);
			emit_mov(node, in_lo, out_lo);
			goto normal_neg;
		}
	} else {
		if (out_hi == in_lo) {
			/* a -> c, b -> a */
			emit_mov(node, in_lo, out_lo);
			goto zero_neg;
		} else if (out_hi == in_hi) {
			/* a -> c, b -> b */
			emit_mov(node, in_lo, out_lo);
			goto normal_neg;
		} else {
			/* a -> c, b -> d */
			emit_mov(node, in_lo, out_lo);
			goto zero_neg;
		}
	}

normal_neg:
	emit_neg( node, out_hi);
	emit_neg( node, out_lo);
	emit_sbb0(node, out_hi);
	return;

zero_neg:
	emit_zero(node, out_hi);
	emit_neg( node, out_lo);
	emit_sbb( node, in_hi, out_hi);
}

static void emit_be_Return(const ir_node *node)
{
	unsigned pop;
	be_emit_cstring("\tret");

	pop = be_Return_get_pop(node);
	if(pop > 0) {
		be_emit_irprintf(" $%d", pop);
	}
	be_emit_finish_line_gas(node);
}

static void emit_Nothing(const ir_node *node)
{
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
	IA32_EMIT(CMov);
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
	IA32_EMIT(Minus64Bit);
	IA32_EMIT(Jcc);

	/* benode emitter */
	BE_EMIT(Call);
	BE_EMIT(IncSP);
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
static void ia32_emit_dbg(const ir_node *node)
{
	dbg_info *db = get_irn_dbg_info(node);
	unsigned lineno;
	const char *fname = be_retrieve_dbg_info(db, &lineno);

	if (! cg->birg->main_env->options->stabs_debug_support)
		return;

	if (fname) {
		if (last_name != fname) {
			last_line = -1;
			be_dbg_include_begin(cg->birg->main_env->db_handle, fname);
			last_name = fname;
		}
		if (last_line != lineno) {
			char name[64];

			snprintf(name, sizeof(name), ".LM%u", ++num);
			last_line = lineno;
			be_dbg_line(cg->birg->main_env->db_handle, lineno, name);
			be_emit_string(name);
			be_emit_cstring(":\n");
			be_emit_write_line();
		}
	}
}

typedef void (*emit_func_ptr) (const ir_node *);

/**
 * Emits code for a node.
 */
static void ia32_emit_node(const ir_node *node)
{
	ir_op *op = get_irn_op(node);

	DBG((dbg, LEVEL_1, "emitting code for %+F\n", node));

	if (op->ops.generic) {
		emit_func_ptr func = (emit_func_ptr) op->ops.generic;
		ia32_emit_dbg(node);
		(*func) (node);
	} else {
		emit_Nothing(node);
		ir_fprintf(stderr, "Error: No emit handler for node %+F (%+G, graph %+F)\n", node, node, current_ir_graph);
		abort();
	}
}

/**
 * Emits gas alignment directives
 */
static void ia32_emit_alignment(unsigned align, unsigned skip)
{
	be_emit_cstring("\t.p2align ");
	be_emit_irprintf("%u,,%u\n", align, skip);
	be_emit_write_line();
}

/**
 * Emits gas alignment directives for Functions depended on cpu architecture.
 */
static void ia32_emit_align_func(cpu_support cpu)
{
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
	ia32_emit_alignment(align, maximum_skip);
}

/**
 * Emits gas alignment directives for Labels depended on cpu architecture.
 */
static void ia32_emit_align_label(cpu_support cpu)
{
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
	ia32_emit_alignment(align, maximum_skip);
}

/**
 * Test wether a block should be aligned.
 * For cpus in the P4/Athlon class it is usefull to align jump labels to
 * 16 bytes. However we should only do that if the alignment nops before the
 * label aren't executed more often than we have jumps to the label.
 */
static int should_align_block(ir_node *block, ir_node *prev)
{
	static const double DELTA = .0001;
	ir_exec_freq *exec_freq   = cg->birg->exec_freq;
	double        block_freq;
	double        prev_freq = 0;  /**< execfreq of the fallthrough block */
	double        jmp_freq  = 0;  /**< execfreq of all non-fallthrough blocks */
	cpu_support   cpu       = isa->opt_arch;
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

static void ia32_emit_block_header(ir_node *block, ir_node *prev)
{
	int           n_cfgpreds;
	int           need_label;
	int           i, arity;
	ir_exec_freq  *exec_freq = cg->birg->exec_freq;

	n_cfgpreds = get_Block_n_cfgpreds(block);
	need_label = (n_cfgpreds != 0);

	if (should_align_block(block, prev)) {
		assert(need_label);
		ia32_emit_align_label(isa->opt_arch);
	}

	if(need_label) {
		ia32_emit_block_name(block);
		be_emit_char(':');

		be_emit_pad_comment();
		be_emit_cstring("   /* preds:");

		/* emit list of pred blocks in comment */
		arity = get_irn_arity(block);
		for (i = 0; i < arity; ++i) {
			ir_node *predblock = get_Block_cfgpred_block(block, i);
			be_emit_irprintf(" %d", get_irn_node_nr(predblock));
		}
	} else {
		be_emit_cstring("\t/* ");
		ia32_emit_block_name(block);
		be_emit_cstring(": ");
	}
	if (exec_freq != NULL) {
		be_emit_irprintf(" freq: %f",
		                 get_block_execfreq(exec_freq, block));
	}
	be_emit_cstring(" */\n");
	be_emit_write_line();
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void ia32_gen_block(ir_node *block, ir_node *last_block)
{
	const ir_node *node;

	ia32_emit_block_header(block, last_block);

	/* emit the contents of the block */
	ia32_emit_dbg(block);
	sched_foreach(block, node) {
		ia32_emit_node(node);
	}
}

/**
 * Emits code for function start.
 */
static void ia32_emit_func_prolog(ir_graph *irg)
{
	ir_entity  *irg_ent  = get_irg_entity(irg);
	const char *irg_name = get_entity_ld_name(irg_ent);
	cpu_support cpu      = isa->opt_arch;
	const be_irg_t *birg = cg->birg;

	be_emit_write_line();
	be_gas_emit_switch_section(GAS_SECTION_TEXT);
	be_dbg_method_begin(birg->main_env->db_handle, irg_ent, be_abi_get_stack_layout(birg->abi));
	ia32_emit_align_func(cpu);
	if (get_entity_visibility(irg_ent) == visibility_external_visible) {
		be_emit_cstring(".global ");
		be_emit_string(irg_name);
		be_emit_char('\n');
		be_emit_write_line();
	}
	ia32_emit_function_object(irg_name);
	be_emit_string(irg_name);
	be_emit_cstring(":\n");
	be_emit_write_line();
}

/**
 * Emits code for function end
 */
static void ia32_emit_func_epilog(ir_graph *irg)
{
	const char     *irg_name = get_entity_ld_name(get_irg_entity(irg));
	const be_irg_t *birg     = cg->birg;

	ia32_emit_function_size(irg_name);
	be_dbg_method_end(birg->main_env->db_handle);
	be_emit_char('\n');
	be_emit_write_line();
}

/**
 * Block-walker:
 * Sets labels for control flow nodes (jump target)
 */
static void ia32_gen_labels(ir_node *block, void *data)
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
void ia32_emit_exc_label(const ir_node *node)
{
	if (get_ia32_exc_label(node)) {
		be_emit_irprintf(".EXL%u\n", 0);
		be_emit_write_line();
	}
}

/**
 * Main driver. Emits the code for one routine.
 */
void ia32_gen_routine(ia32_code_gen_t *ia32_cg, ir_graph *irg)
{
	ir_node *block;
	ir_node *last_block = NULL;
	int i, n;

	cg       = ia32_cg;
	isa      = (const ia32_isa_t*) cg->arch_env->isa;
	arch_env = cg->arch_env;

	ia32_register_emitters();

	ia32_emit_func_prolog(irg);
	irg_block_walk_graph(irg, ia32_gen_labels, NULL, NULL);

	n = ARR_LEN(cg->blk_sched);
	for (i = 0; i < n;) {
		ir_node *next_bl;

		block   = cg->blk_sched[i];
		++i;
		next_bl = i < n ? cg->blk_sched[i] : NULL;

		/* set here the link. the emitter expects to find the next block here */
		set_irn_link(block, next_bl);
		ia32_gen_block(block, last_block);
		last_block = block;
	}

	ia32_emit_func_epilog(irg);
}

void ia32_init_emitter(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.emitter");
}
