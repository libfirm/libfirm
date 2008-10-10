/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
#include "dbginfo.h"

#include "../besched_t.h"
#include "../benode_t.h"
#include "../beabi.h"
#include "../be_dbgout.h"
#include "../beemitter.h"
#include "../begnuas.h"
#include "../beirg_t.h"
#include "../be_dbgout.h"

#include "ia32_emitter.h"
#include "gen_ia32_emitter.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_nodes_attr.h"
#include "ia32_new_nodes.h"
#include "ia32_map_regs.h"
#include "ia32_architecture.h"
#include "bearch_ia32_t.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define BLOCK_PREFIX ".L"

#define SNPRINTF_BUF_LEN 128

static const arch_env_t *arch_env;
static const ia32_isa_t *isa;
static ia32_code_gen_t  *cg;
static int               do_pic;
static char              pic_base_label[128];
static ir_label_t        exc_label_id;
static int               mark_spill_reload = 0;

/** Return the next block in Block schedule */
static ir_node *get_prev_block_sched(const ir_node *block)
{
	return get_irn_link(block);
}

static int is_fallthrough(const ir_node *cfgpred)
{
	ir_node *pred;

	if (!is_Proj(cfgpred))
		return 1;
	pred = get_Proj_pred(cfgpred);
	if (is_ia32_SwitchJmp(pred))
		return 0;

	return 1;
}

static int block_needs_label(const ir_node *block)
{
	int need_label = 1;
	int  n_cfgpreds = get_Block_n_cfgpreds(block);

	if (n_cfgpreds == 0) {
		need_label = 0;
	} else if (n_cfgpreds == 1) {
		ir_node *cfgpred       = get_Block_cfgpred(block, 0);
		ir_node *cfgpred_block = get_nodes_block(cfgpred);

		if (get_prev_block_sched(block) == cfgpred_block
				&& is_fallthrough(cfgpred)) {
			need_label = 0;
		}
	}

	return need_label;
}

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

	if (reg == &ia32_gp_regs[REG_GP_NOREG])
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

	if (mode != NULL) {
		int size = get_mode_size_bits(mode);
		switch (size) {
			case  8: emit_8bit_register(reg);  return;
			case 16: emit_16bit_register(reg); return;
		}
		assert(mode_is_float(mode) || size == 32);
	}

	reg_name = arch_register_get_name(reg);

	be_emit_char('%');
	be_emit_string(reg_name);
}

void ia32_emit_source_register(const ir_node *node, int pos)
{
	const arch_register_t *reg = get_in_reg(node, pos);

	emit_register(reg, NULL);
}

static void ia32_emit_entity(ir_entity *entity, int no_pic_adjust)
{
	ident *id;

	set_entity_backend_marked(entity, 1);
	id = get_entity_ld_ident(entity);
	be_emit_ident(id);

	if (get_entity_owner(entity) == get_tls_type()) {
		if (get_entity_visibility(entity) == visibility_external_allocated) {
			be_emit_cstring("@INDNTPOFF");
		} else {
			be_emit_cstring("@NTPOFF");
		}
	}

	if (!no_pic_adjust && do_pic) {
		/* TODO: only do this when necessary */
		be_emit_char('-');
		be_emit_string(pic_base_label);
	}
}

static void emit_ia32_Immediate_no_prefix(const ir_node *node)
{
	const ia32_immediate_attr_t *attr = get_ia32_immediate_attr_const(node);

	if (attr->symconst != NULL) {
		if (attr->sc_sign)
			be_emit_char('-');
		ia32_emit_entity(attr->symconst, 0);
	}
	if (attr->symconst == NULL || attr->offset != 0) {
		if (attr->symconst != NULL) {
			be_emit_irprintf("%+d", attr->offset);
		} else {
			be_emit_irprintf("0x%X", attr->offset);
		}
	}
}

static void emit_ia32_Immediate(const ir_node *node)
{
	be_emit_char('$');
	emit_ia32_Immediate_no_prefix(node);
}

void ia32_emit_8bit_source_register_or_immediate(const ir_node *node, int pos)
{
	const arch_register_t *reg;
	ir_node               *in = get_irn_n(node, pos);
	if (is_ia32_Immediate(in)) {
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
	assert(mode_is_int(mode) || mode_is_reference(mode));
	switch (get_mode_size_bits(mode)) {
		case 8:  be_emit_char('b');     return;
		case 16: be_emit_char('w');     return;
		case 32: be_emit_char('l');     return;
		/* gas docu says q is the suffix but gcc, objdump and icc use ll
		 * apparently */
		case 64: be_emit_cstring("ll"); return;
	}
	panic("Can't output mode_suffix for %+F", mode);
}

void ia32_emit_mode_suffix(const ir_node *node)
{
	ir_mode *mode = get_ia32_ls_mode(node);
	if (mode == NULL)
		mode = mode_Iu;

	ia32_emit_mode_suffix_mode(mode);
}

void ia32_emit_x87_mode_suffix(const ir_node *node)
{
	ir_mode *mode;

	/* we only need to emit the mode on address mode */
	if (get_ia32_op_type(node) == ia32_Normal)
		return;

	mode = get_ia32_ls_mode(node);
	assert(mode != NULL);

	if (mode_is_float(mode)) {
		switch (get_mode_size_bits(mode)) {
			case 32: be_emit_char('s'); return;
			case 64: be_emit_char('l'); return;
			case 80:
			case 96: be_emit_char('t'); return;
		}
	} else {
		assert(mode_is_int(mode));
		switch (get_mode_size_bits(mode)) {
			case 16: be_emit_char('s');     return;
			case 32: be_emit_char('l');     return;
			/* gas docu says q is the suffix but gcc, objdump and icc use ll
			 * apparently */
			case 64: be_emit_cstring("ll"); return;
		}
	}
	panic("Can't output mode_suffix for %+F", mode);
}

static char get_xmm_mode_suffix(ir_mode *mode)
{
	assert(mode_is_float(mode));
	switch(get_mode_size_bits(mode)) {
	case 32: return 's';
	case 64: return 'd';
	default: panic("Invalid XMM mode");
	}
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
	if (get_mode_size_bits(mode) == 32)
		return;
	be_emit_char(mode_is_signed(mode) ? 's' : 'z');
}

void ia32_emit_source_register_or_immediate(const ir_node *node, int pos)
{
	ir_node *in = get_irn_n(node, pos);
	if (is_ia32_Immediate(in)) {
		emit_ia32_Immediate(in);
	} else {
		const ir_mode         *mode = get_ia32_ls_mode(node);
		const arch_register_t *reg  = get_in_reg(node, pos);
		emit_register(reg, mode);
	}
}

/**
 * Returns the target block for a control flow node.
 */
static ir_node *get_cfop_target_block(const ir_node *irn)
{
	assert(get_irn_mode(irn) == mode_X);
	return get_irn_link(irn);
}

/**
 * Emits a block label for the given block.
 */
static void ia32_emit_block_name(const ir_node *block)
{
	if (has_Block_label(block)) {
		be_emit_string(be_gas_block_label_prefix());
		be_emit_irprintf("%lu", get_Block_label(block));
	} else {
		be_emit_cstring(BLOCK_PREFIX);
		be_emit_irprintf("%ld", get_irn_node_nr(block));
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

static void ia32_emit_cmp_suffix(int pnc)
{
	const char *str;

	if ((pnc & ia32_pn_Cmp_float) || (pnc & ia32_pn_Cmp_unsigned)) {
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

typedef enum ia32_emit_mod_t {
	EMIT_RESPECT_LS   = 1U << 0,
	EMIT_ALTERNATE_AM = 1U << 1
} ia32_emit_mod_t;

/**
 * fmt  parameter               output
 * ---- ----------------------  ---------------------------------------------
 * %%                           %
 * %AM  <node>                  address mode of the node
 * %AR  const arch_register_t*  address mode of the node or register
 * %ASx <node>                  address mode of the node or source register x
 * %Dx  <node>                  destination register x
 * %I   <node>                  immediate of the node
 * %L   <node>                  control flow target of the node
 * %M   <node>                  mode suffix of the node
 * %P   int                     condition code
 * %R   const arch_register_t*  register
 * %Sx  <node>                  source register x
 * %s   const char*             string
 * %u   unsigned int            unsigned int
 *
 * x starts at 0
 * # modifier for %ASx, %D and %S uses ls mode of node to alter register width
 * * modifier does not prefix immediates with $, but AM with *
 */
static void ia32_emitf(const ir_node *node, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);

	for (;;) {
		const char      *start = fmt;
		ia32_emit_mod_t  mod   = 0;

		while (*fmt != '%' && *fmt != '\n' && *fmt != '\0')
			++fmt;
		if (fmt != start) {
			be_emit_string_len(start, fmt - start);
		}

		if (*fmt == '\n') {
			be_emit_finish_line_gas(node);
			++fmt;
			if (*fmt == '\0')
				break;
			continue;
		}

		if (*fmt == '\0')
			break;

		++fmt;
		if (*fmt == '*') {
			mod |= EMIT_ALTERNATE_AM;
			++fmt;
		}

		if (*fmt == '#') {
			mod |= EMIT_RESPECT_LS;
			++fmt;
		}

		switch (*fmt++) {
			case '%':
				be_emit_char('%');
				break;

			case 'A': {
				switch (*fmt++) {
					case 'M':
						if (mod & EMIT_ALTERNATE_AM)
							be_emit_char('*');
						ia32_emit_am(node);
						break;

					case 'R': {
						const arch_register_t *reg = va_arg(ap, const arch_register_t*);
						if (mod & EMIT_ALTERNATE_AM)
							be_emit_char('*');
						if (get_ia32_op_type(node) == ia32_AddrModeS) {
							ia32_emit_am(node);
						} else {
							emit_register(reg, NULL);
						}
						break;
					}

					case 'S':
						if (get_ia32_op_type(node) == ia32_AddrModeS) {
							if (mod & EMIT_ALTERNATE_AM)
								be_emit_char('*');
							ia32_emit_am(node);
							++fmt;
						} else {
							assert(get_ia32_op_type(node) == ia32_Normal);
							goto emit_S;
						}
						break;

					default: goto unknown;
				}
				break;
			}

			case 'D': {
				unsigned               pos;
				const arch_register_t *reg;

				if (*fmt < '0' || '9' <= *fmt)
					goto unknown;

				pos = *fmt++ - '0';
				reg = get_out_reg(node, pos);
				emit_register(reg, mod & EMIT_RESPECT_LS ? get_ia32_ls_mode(node) : NULL);
				break;
			}

			case 'I':
				if (!(mod & EMIT_ALTERNATE_AM))
					be_emit_char('$');
				emit_ia32_Immediate_no_prefix(node);
				break;

			case 'L':
				ia32_emit_cfop_target(node);
				break;

			case 'M': {
				ia32_emit_mode_suffix_mode(get_ia32_ls_mode(node));
				break;
			}

			case 'P': {
				int pnc = va_arg(ap, int);
				ia32_emit_cmp_suffix(pnc);
				break;
			}

			case 'R': {
				const arch_register_t *reg = va_arg(ap, const arch_register_t*);
				emit_register(reg, NULL);
				break;
			}

emit_S:
			case 'S': {
				unsigned       pos;
				const ir_node *in;

				if (*fmt < '0' || '9' <= *fmt)
					goto unknown;

				pos = *fmt++ - '0';
				in  = get_irn_n(node, pos);
				if (is_ia32_Immediate(in)) {
					if (!(mod & EMIT_ALTERNATE_AM))
						be_emit_char('$');
					emit_ia32_Immediate_no_prefix(in);
				} else {
					if (mod & EMIT_ALTERNATE_AM)
						be_emit_char('*');
					const arch_register_t *reg = get_in_reg(node, pos);
					emit_register(reg, mod & EMIT_RESPECT_LS ? get_ia32_ls_mode(node) : NULL);
				}
				break;
			}

			case 's': {
				const char *str = va_arg(ap, const char*);
				be_emit_string(str);
				break;
			}

			case 'u': {
				unsigned num = va_arg(ap, unsigned);
				be_emit_irprintf("%u", num);
				break;
			}

			default:
unknown:
				panic("unknown conversion");
		}
	}

	va_end(ap);
}

/**
 * Emits registers and/or address mode of a binary operation.
 */
void ia32_emit_binop(const ir_node *node)
{
	if (is_ia32_Immediate(get_irn_n(node, n_ia32_binary_right))) {
		ia32_emitf(node, "%#S4, %#AS3");
	} else {
		ia32_emitf(node, "%#AS4, %#S3");
	}
}

/**
 * Emits registers and/or address mode of a binary operation.
 */
void ia32_emit_x87_binop(const ir_node *node)
{
	switch(get_ia32_op_type(node)) {
		case ia32_Normal:
			{
				const ia32_x87_attr_t *x87_attr = get_ia32_x87_attr_const(node);
				const arch_register_t *in1      = x87_attr->x87[0];
				const arch_register_t *in       = x87_attr->x87[1];
				const arch_register_t *out      = x87_attr->x87[2];

				if (out == NULL) {
					out = in1;
				} else if (out == in) {
					in = in1;
				}

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

/**
 * Emits registers and/or address mode of a unary operation.
 */
void ia32_emit_unop(const ir_node *node, int pos)
{
	char fmt[] = "%ASx";
	fmt[3] = '0' + pos;
	ia32_emitf(node, fmt);
}

/**
 * Emits address mode.
 */
void ia32_emit_am(const ir_node *node)
{
	ir_entity *ent       = get_ia32_am_sc(node);
	int        offs      = get_ia32_am_offs_int(node);
	ir_node   *base      = get_irn_n(node, n_ia32_base);
	int        has_base  = !is_ia32_NoReg_GP(base);
	ir_node   *index     = get_irn_n(node, n_ia32_index);
	int        has_index = !is_ia32_NoReg_GP(index);

	/* just to be sure... */
	assert(!is_ia32_use_frame(node) || get_ia32_frame_ent(node) != NULL);

	/* emit offset */
	if (ent != NULL) {
		if (is_ia32_am_sc_sign(node))
			be_emit_char('-');
		ia32_emit_entity(ent, 0);
	}

	/* also handle special case if nothing is set */
	if (offs != 0 || (ent == NULL && !has_base && !has_index)) {
		if (ent != NULL) {
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
				be_emit_irprintf(",%d", 1 << scale);
			}
		}
		be_emit_char(')');
	}
}

static void emit_ia32_IMul(const ir_node *node)
{
	ir_node               *left    = get_irn_n(node, n_ia32_IMul_left);
	const arch_register_t *out_reg = get_out_reg(node, pn_ia32_IMul_res);

	/* do we need the 3-address form? */
	if (is_ia32_NoReg_GP(left) ||
			get_in_reg(node, n_ia32_IMul_left) != out_reg) {
		ia32_emitf(node, "\timul%M %#S4, %#AS3, %#D0\n");
	} else {
		ia32_emitf(node, "\timul%M %#AS4, %#S3\n");
	}
}

/**
 * walks up a tree of copies/perms/spills/reloads to find the original value
 * that is moved around
 */
static ir_node *find_original_value(ir_node *node)
{
	if (irn_visited(node))
		return NULL;

	mark_irn_visited(node);
	if (be_is_Copy(node)) {
		return find_original_value(be_get_Copy_op(node));
	} else if (be_is_CopyKeep(node)) {
		return find_original_value(be_get_CopyKeep_op(node));
	} else if (is_Proj(node)) {
		ir_node *pred = get_Proj_pred(node);
		if (be_is_Perm(pred)) {
			return find_original_value(get_irn_n(pred, get_Proj_proj(node)));
		} else if (be_is_MemPerm(pred)) {
			return find_original_value(get_irn_n(pred, get_Proj_proj(node) + 1));
		} else if (is_ia32_Load(pred)) {
			return find_original_value(get_irn_n(pred, n_ia32_Load_mem));
		} else {
			return node;
		}
	} else if (is_ia32_Store(node)) {
		return find_original_value(get_irn_n(node, n_ia32_Store_val));
	} else if (is_Phi(node)) {
		int i, arity;
		arity = get_irn_arity(node);
		for (i = 0; i < arity; ++i) {
			ir_node *in  = get_irn_n(node, i);
			ir_node *res = find_original_value(in);

			if (res != NULL)
				return res;
		}
		return NULL;
	} else {
		return node;
	}
}

static int determine_final_pnc(const ir_node *node, int flags_pos,
                               int pnc)
{
	ir_node           *flags = get_irn_n(node, flags_pos);
	const ia32_attr_t *flags_attr;
	flags = skip_Proj(flags);

	if (is_ia32_Sahf(flags)) {
		ir_node *cmp = get_irn_n(flags, n_ia32_Sahf_val);
		if (!(is_ia32_FucomFnstsw(cmp) || is_ia32_FucompFnstsw(cmp)
				|| is_ia32_FucomppFnstsw(cmp) || is_ia32_FtstFnstsw(cmp))) {
			inc_irg_visited(current_ir_graph);
			cmp = find_original_value(cmp);
			assert(cmp != NULL);
			assert(is_ia32_FucomFnstsw(cmp) || is_ia32_FucompFnstsw(cmp)
			       || is_ia32_FucomppFnstsw(cmp) || is_ia32_FtstFnstsw(cmp));
		}

		flags_attr = get_ia32_attr_const(cmp);
		if (flags_attr->data.ins_permuted)
			pnc = get_mirrored_pnc(pnc);
		pnc |= ia32_pn_Cmp_float;
	} else if (is_ia32_Ucomi(flags) || is_ia32_Fucomi(flags)
			|| is_ia32_Fucompi(flags)) {
		flags_attr = get_ia32_attr_const(flags);

		if (flags_attr->data.ins_permuted)
			pnc = get_mirrored_pnc(pnc);
		pnc |= ia32_pn_Cmp_float;
	} else {
		flags_attr = get_ia32_attr_const(flags);

		if (flags_attr->data.ins_permuted)
			pnc = get_mirrored_pnc(pnc);
		if (flags_attr->data.cmp_unsigned)
			pnc |= ia32_pn_Cmp_unsigned;
	}

	return pnc;
}

void ia32_emit_cmp_suffix_node(const ir_node *node,
                               int flags_pos)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);

	pn_Cmp pnc = get_ia32_condcode(node);

	pnc = determine_final_pnc(node, flags_pos, pnc);
	if (attr->data.ins_permuted) {
		if (pnc & ia32_pn_Cmp_float) {
			pnc = get_negated_pnc(pnc, mode_F);
		} else {
			pnc = get_negated_pnc(pnc, mode_Iu);
		}
	}

	ia32_emit_cmp_suffix(pnc);
}

/**
 * Emits an exception label for a given node.
 */
static void ia32_emit_exc_label(const ir_node *node)
{
	be_emit_string(be_gas_insn_label_prefix());
	be_emit_irprintf("%lu", get_ia32_exc_label_id(node));
}

/**
 * Returns the Proj with projection number proj and NOT mode_M
 */
static ir_node *get_proj(const ir_node *node, long proj)
{
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

static int can_be_fallthrough(const ir_node *node)
{
	ir_node *target_block = get_cfop_target_block(node);
	ir_node *block        = get_nodes_block(node);
	return get_prev_block_sched(target_block) == block;
}

/**
 * Emits the jump sequence for a conditional jump (cmp + jmp_true + jmp_false)
 */
static void emit_ia32_Jcc(const ir_node *node)
{
	int            need_parity_label = 0;
	const ir_node *proj_true;
	const ir_node *proj_false;
	const ir_node *block;
	pn_Cmp         pnc = get_ia32_condcode(node);

	pnc = determine_final_pnc(node, 0, pnc);

	/* get both Projs */
	proj_true = get_proj(node, pn_ia32_Jcc_true);
	assert(proj_true && "Jcc without true Proj");

	proj_false = get_proj(node, pn_ia32_Jcc_false);
	assert(proj_false && "Jcc without false Proj");

	block      = get_nodes_block(node);

	if (can_be_fallthrough(proj_true)) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj_true;

		proj_true  = proj_false;
		proj_false = t;
		if (pnc & ia32_pn_Cmp_float) {
			pnc = get_negated_pnc(pnc, mode_F);
		} else {
			pnc = get_negated_pnc(pnc, mode_Iu);
		}
	}

	if (pnc & ia32_pn_Cmp_float) {
		/* Some floating point comparisons require a test of the parity flag,
		 * which indicates that the result is unordered */
		switch (pnc & 15) {
			case pn_Cmp_Uo: {
				ia32_emitf(proj_true, "\tjp %L\n");
				break;
			}

			case pn_Cmp_Leg:
				ia32_emitf(proj_true, "\tjnp %L\n");
				break;

			case pn_Cmp_Eq:
			case pn_Cmp_Lt:
			case pn_Cmp_Le:
				/* we need a local label if the false proj is a fallthrough
				 * as the falseblock might have no label emitted then */
				if (can_be_fallthrough(proj_false)) {
					need_parity_label = 1;
					ia32_emitf(proj_false, "\tjp 1f\n");
				} else {
					ia32_emitf(proj_false, "\tjp %L\n");
				}
				goto emit_jcc;

			case pn_Cmp_Ug:
			case pn_Cmp_Uge:
			case pn_Cmp_Ne:
				ia32_emitf(proj_true, "\tjp %L\n");
				goto emit_jcc;

			default:
				goto emit_jcc;
		}
	} else {
emit_jcc:
		ia32_emitf(proj_true, "\tj%P %L\n", pnc);
	}

	if (need_parity_label) {
		ia32_emitf(NULL, "1:\n");
	}

	/* the second Proj might be a fallthrough */
	if (can_be_fallthrough(proj_false)) {
		ia32_emitf(proj_false, "\t/* fallthrough to %L */\n");
	} else {
		ia32_emitf(proj_false, "\tjmp %L\n");
	}
}

static void emit_ia32_CMov(const ir_node *node)
{
	const ia32_attr_t     *attr         = get_ia32_attr_const(node);
	int                    ins_permuted = attr->data.ins_permuted;
	const arch_register_t *out          = arch_get_irn_register(arch_env, node);
	pn_Cmp                 pnc          = get_ia32_condcode(node);
	const arch_register_t *in_true;
	const arch_register_t *in_false;

	pnc = determine_final_pnc(node, n_ia32_CMov_eflags, pnc);

	in_true  = arch_get_irn_register(arch_env,
	                                 get_irn_n(node, n_ia32_CMov_val_true));
	in_false = arch_get_irn_register(arch_env,
	                                 get_irn_n(node, n_ia32_CMov_val_false));

	/* should be same constraint fullfilled? */
	if (out == in_false) {
		/* yes -> nothing to do */
	} else if (out == in_true) {
		const arch_register_t *tmp;

		assert(get_ia32_op_type(node) == ia32_Normal);

		ins_permuted = !ins_permuted;

		tmp      = in_true;
		in_true  = in_false;
		in_false = tmp;
	} else {
		/* we need a mov */
		ia32_emitf(node, "\tmovl %R, %R\n", in_false, out);
	}

	if (ins_permuted) {
		if (pnc & ia32_pn_Cmp_float) {
			pnc = get_negated_pnc(pnc, mode_F);
		} else {
			pnc = get_negated_pnc(pnc, mode_Iu);
		}
	}

	/* TODO: handling of Nans isn't correct yet */

	ia32_emitf(node, "\tcmov%P %AR, %#R\n", pnc, in_true, out);
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
static int ia32_cmp_branch_t(const void *a, const void *b)
{
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
static void emit_ia32_SwitchJmp(const ir_node *node)
{
	unsigned long       interval;
	int                 last_value, i;
	long                pnc;
	long                default_pn;
	jmp_tbl_t           tbl;
	ir_node            *proj;
	const ir_edge_t    *edge;

	/* fill the table structure */
	tbl.label        = XMALLOCN(char, SNPRINTF_BUF_LEN);
	tbl.label        = get_unique_label(tbl.label, SNPRINTF_BUF_LEN, ".TBL_");
	tbl.defProj      = NULL;
	tbl.num_branches = get_irn_n_edges(node) - 1;
	tbl.branches     = XMALLOCNZ(branch_t, tbl.num_branches);
	tbl.min_value    = INT_MAX;
	tbl.max_value    = INT_MIN;

	default_pn = get_ia32_condcode(node);
	i = 0;
	/* go over all proj's and collect them */
	foreach_out_edge(node, edge) {
		proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

		pnc = get_Proj_proj(proj);

		/* check for default proj */
		if (pnc == default_pn) {
			assert(tbl.defProj == NULL && "found two default Projs at SwitchJmp");
			tbl.defProj = proj;
		} else {
			tbl.min_value = pnc < tbl.min_value ? pnc : tbl.min_value;
			tbl.max_value = pnc > tbl.max_value ? pnc : tbl.max_value;

			/* create branch entry */
			tbl.branches[i].target = proj;
			tbl.branches[i].value  = pnc;
			++i;
		}

	}
	assert(i == tbl.num_branches);

	/* sort the branches by their number */
	qsort(tbl.branches, tbl.num_branches, sizeof(tbl.branches[0]), ia32_cmp_branch_t);

	/* two-complement's magic make this work without overflow */
	interval = tbl.max_value - tbl.min_value;

	/* emit the table */
	ia32_emitf(node,        "\tcmpl $%u, %S0\n", interval);
	ia32_emitf(tbl.defProj, "\tja %L\n");

	if (tbl.num_branches > 1) {
		/* create table */
		ia32_emitf(node, "\tjmp *%s(,%S0,4)\n", tbl.label);

		be_gas_emit_switch_section(GAS_SECTION_RODATA);
		ia32_emitf(NULL, "\t.align 4\n");
		ia32_emitf(NULL, "%s:\n", tbl.label);

		last_value = tbl.branches[0].value;
		for (i = 0; i != tbl.num_branches; ++i) {
			while (last_value != tbl.branches[i].value) {
				ia32_emitf(tbl.defProj, ".long %L\n");
				++last_value;
			}
			ia32_emitf(tbl.branches[i].target, ".long %L\n");
			++last_value;
		}
		be_gas_emit_switch_section(GAS_SECTION_TEXT);
	} else {
		/* one jump is enough */
		ia32_emitf(tbl.branches[0].target, "\tjmp %L\n");
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
	ir_node *block;

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	/* we have a block schedule */
	if (can_be_fallthrough(node)) {
		ia32_emitf(node, "\t/* fallthrough to %L */\n");
	} else {
		ia32_emitf(node, "\tjmp %L\n");
	}
}

/**
 * Emit an inline assembler operand.
 *
 * @param node  the ia32_ASM node
 * @param s     points to the operand (a %c)
 *
 * @return  pointer to the first char in s NOT in the current operand
 */
static const char* emit_asm_operand(const ir_node *node, const char *s)
{
	const ia32_attr_t     *ia32_attr = get_ia32_attr_const(node);
	const ia32_asm_attr_t *attr      = CONST_CAST_IA32_ATTR(ia32_asm_attr_t,
                                                            ia32_attr);
	const arch_register_t *reg;
	const ia32_asm_reg_t  *asm_regs = attr->register_map;
	const ia32_asm_reg_t  *asm_reg;
	const char            *reg_name;
	char                   c;
	char                   modifier = 0;
	int                    num      = -1;
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
	if (num < 0) {
		ir_fprintf(stderr, "Warning: Couldn't parse assembler operand (%+F)\n",
		           node);
		return s;
	} else {
		s += p;
	}

	if (num < 0 || num >= ARR_LEN(asm_regs)) {
		ir_fprintf(stderr, "Error: Custom assembler references invalid "
		           "input/output (%+F)\n", node);
		return s;
	}
	asm_reg = & asm_regs[num];
	assert(asm_reg->valid);

	/* get register */
	if (asm_reg->use_input == 0) {
		reg = get_out_reg(node, asm_reg->inout_pos);
	} else {
		ir_node *pred = get_irn_n(node, asm_reg->inout_pos);

		/* might be an immediate value */
		if (is_ia32_Immediate(pred)) {
			emit_ia32_Immediate(pred);
			return s;
		}
		reg = get_in_reg(node, asm_reg->inout_pos);
	}
	if (reg == NULL) {
		ir_fprintf(stderr, "Warning: no register assigned for %d asm op "
		           "(%+F)\n", num, node);
		return s;
	}

	if (asm_reg->memory) {
		be_emit_char('(');
	}

	/* emit it */
	if (modifier != 0) {
		be_emit_char('%');
		switch(modifier) {
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
	} else {
		emit_register(reg, asm_reg->mode);
	}

	if (asm_reg->memory) {
		be_emit_char(')');
	}

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

	ia32_emitf(node, "#APP\t\n");

	if (s[0] != '\t')
		be_emit_char('\t');

	while(*s != 0) {
		if (*s == '%') {
			s = emit_asm_operand(node, s);
		} else {
			be_emit_char(*s++);
		}
	}

	ia32_emitf(NULL, "\n#NO_APP\n");
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
static void emit_CopyB_prolog(unsigned size)
{
	if (size & 1)
		ia32_emitf(NULL, "\tmovsb\n");
	if (size & 2)
		ia32_emitf(NULL, "\tmovsw\n");
}

/**
 * Emit rep movsd instruction for memcopy.
 */
static void emit_ia32_CopyB(const ir_node *node)
{
	unsigned size = get_ia32_copyb_size(node);

	emit_CopyB_prolog(size);
	ia32_emitf(node, "\trep movsd\n");
}

/**
 * Emits unrolled memcopy.
 */
static void emit_ia32_CopyB_i(const ir_node *node)
{
	unsigned size = get_ia32_copyb_size(node);

	emit_CopyB_prolog(size);

	size >>= 2;
	while (size--) {
		ia32_emitf(NULL, "\tmovsd\n");
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
	const char         *conv;

	if (is_ia32_Conv_I2FP(node)) {
		if (ls_bits == 32) {
			conv = "si2ss";
		} else {
			conv = "si2sd";
		}
	} else if (is_ia32_Conv_FP2I(node)) {
		if (ls_bits == 32) {
			conv = "ss2si";
		} else {
			conv = "sd2si";
		}
	} else {
		assert(is_ia32_Conv_FP2FP(node));
		if (ls_bits == 32) {
			conv = "sd2ss";
		} else {
			conv = "ss2sd";
		}
	}

	ia32_emitf(node, "\tcvt%s %AS3, %D0\n", conv);
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
	ir_mode *smaller_mode = get_ia32_ls_mode(node);
	int      smaller_bits = get_mode_size_bits(smaller_mode);
	int      signed_mode  = mode_is_signed(smaller_mode);

	assert(!mode_is_float(smaller_mode));
	assert(smaller_bits == 8 || smaller_bits == 16);

	if (signed_mode                                    &&
			smaller_bits == 16                             &&
			&ia32_gp_regs[REG_EAX] == get_out_reg(node, 0) &&
			&ia32_gp_regs[REG_EAX] == arch_get_irn_register(arch_env, get_irn_n(node, n_ia32_unary_op))) {
		/* argument and result are both in EAX and signedness is ok: use the
		 * smaller cwtl opcode */
		ia32_emitf(node, "\tcwtl\n");
	} else {
		const char *sign_suffix = signed_mode ? "s" : "z";
		ia32_emitf(node, "\tmov%s%Ml %#AS3, %D0\n", sign_suffix);
	}
}

/**
 * Emits a call
 */
static void emit_ia32_Call(const ir_node *node)
{
	/* Special case: Call must not have its immediates prefixed by $, instead
	 * address mode is prefixed by *. */
	ia32_emitf(node, "\tcall %*AS3\n");
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
 * Emits code to increase stack pointer.
 */
static void emit_be_IncSP(const ir_node *node)
{
	int offs = be_get_IncSP_offset(node);

	if (offs == 0)
		return;

	if (offs > 0) {
		ia32_emitf(node, "\tsubl $%u, %D0\n", offs);
	} else {
		ia32_emitf(node, "\taddl $%u, %D0\n", -offs);
	}
}

/**
 * Emits code for Copy/CopyKeep.
 */
static void Copy_emitter(const ir_node *node, const ir_node *op)
{
	const arch_register_t *in  = arch_get_irn_register(arch_env, op);
	const arch_register_t *out = arch_get_irn_register(arch_env, node);

	if (in == out) {
		return;
	}
	if (is_unknown_reg(in))
		return;
	/* copies of vf nodes aren't real... */
	if (arch_register_get_class(in) == &ia32_reg_classes[CLASS_ia32_vfp])
		return;

	if (get_irn_mode(node) == mode_E) {
		ia32_emitf(node, "\tmovsd %R, %R\n", in, out);
	} else {
		ia32_emitf(node, "\tmovl %R, %R\n", in, out);
	}
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
		ia32_emitf(node, "\txchg %R, %R\n", in1, in0);
	} else if (cls0 == &ia32_reg_classes[CLASS_ia32_xmm]) {
		ia32_emitf(NULL, "\txorpd %R, %R\n", in1, in0);
		ia32_emitf(NULL, "\txorpd %R, %R\n", in0, in1);
		ia32_emitf(node, "\txorpd %R, %R\n", in1, in0);
	} else if (cls0 == &ia32_reg_classes[CLASS_ia32_vfp]) {
		/* is a NOP */
	} else if (cls0 == &ia32_reg_classes[CLASS_ia32_st]) {
		/* is a NOP */
	} else {
		panic("unexpected register class in be_Perm (%+F)", node);
	}
}

/**
 * Emits code for Constant loading.
 */
static void emit_ia32_Const(const ir_node *node)
{
	ia32_emitf(node, "\tmovl %I, %D0\n");
}

/**
 * Emits code to load the TLS base
 */
static void emit_ia32_LdTls(const ir_node *node)
{
	ia32_emitf(node, "\tmovl %%gs:0, %D0\n");
}

/* helper function for emit_ia32_Minus64Bit */
static void emit_mov(const ir_node* node, const arch_register_t *src, const arch_register_t *dst)
{
	ia32_emitf(node, "\tmovl %R, %R\n", src, dst);
}

/* helper function for emit_ia32_Minus64Bit */
static void emit_neg(const ir_node* node, const arch_register_t *reg)
{
	ia32_emitf(node, "\tnegl %R\n", reg);
}

/* helper function for emit_ia32_Minus64Bit */
static void emit_sbb0(const ir_node* node, const arch_register_t *reg)
{
	ia32_emitf(node, "\tsbbl $0, %R\n", reg);
}

/* helper function for emit_ia32_Minus64Bit */
static void emit_sbb(const ir_node* node, const arch_register_t *src, const arch_register_t *dst)
{
	ia32_emitf(node, "\tsbbl %R, %R\n", src, dst);
}

/* helper function for emit_ia32_Minus64Bit */
static void emit_xchg(const ir_node* node, const arch_register_t *src, const arch_register_t *dst)
{
	ia32_emitf(node, "\txchgl %R, %R\n", src, dst);
}

/* helper function for emit_ia32_Minus64Bit */
static void emit_zero(const ir_node* node, const arch_register_t *reg)
{
	ia32_emitf(node, "\txorl %R, %R\n", reg, reg);
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

static void emit_ia32_GetEIP(const ir_node *node)
{
	ia32_emitf(node, "\tcall %s\n", pic_base_label);
	ia32_emitf(NULL, "%s:\n", pic_base_label);
	ia32_emitf(node, "\tpopl %D0\n");
}

static void emit_be_Return(const ir_node *node)
{
	unsigned pop = be_Return_get_pop(node);

	if (pop > 0 || be_Return_get_emit_pop(node)) {
		ia32_emitf(node, "\tret $%u\n", pop);
	} else {
		ia32_emitf(node, "\tret\n");
	}
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
static void ia32_register_emitters(void)
{
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
	IA32_EMIT2(Conv_I2I8Bit, Conv_I2I);
	IA32_EMIT(Asm);
	IA32_EMIT(CMov);
	IA32_EMIT(Call);
	IA32_EMIT(Const);
	IA32_EMIT(Conv_FP2FP);
	IA32_EMIT(Conv_FP2I);
	IA32_EMIT(Conv_I2FP);
	IA32_EMIT(Conv_I2I);
	IA32_EMIT(CopyB);
	IA32_EMIT(CopyB_i);
	IA32_EMIT(GetEIP);
	IA32_EMIT(IMul);
	IA32_EMIT(Jcc);
	IA32_EMIT(LdTls);
	IA32_EMIT(Minus64Bit);
	IA32_EMIT(SwitchJmp);

	/* benode emitter */
	BE_EMIT(Copy);
	BE_EMIT(CopyKeep);
	BE_EMIT(IncSP);
	BE_EMIT(Perm);
	BE_EMIT(Return);

	BE_IGN(Barrier);
	BE_IGN(Keep);
	BE_IGN(RegParams);

	/* firm emitter */
	EMIT(Jmp);
	IGN(Phi);
	IGN(Start);

#undef BE_EMIT
#undef EMIT
#undef IGN
#undef IA32_EMIT2
#undef IA32_EMIT
}

typedef void (*emit_func_ptr) (const ir_node *);

/**
 * Assign and emit an exception label if the current instruction can fail.
 */
static void ia32_assign_exc_label(ir_node *node)
{
	/* assign a new ID to the instruction */
	set_ia32_exc_label_id(node, ++exc_label_id);
	/* print it */
	ia32_emit_exc_label(node);
	be_emit_char(':');
	be_emit_pad_comment();
	be_emit_cstring("/* exception to Block ");
	ia32_emit_cfop_target(node);
	be_emit_cstring(" */\n");
	be_emit_write_line();
}

/**
 * Emits code for a node.
 */
static void ia32_emit_node(ir_node *node)
{
	ir_op *op = get_irn_op(node);

	DBG((dbg, LEVEL_1, "emitting code for %+F\n", node));

	if (is_ia32_irn(node)) {
		if (get_ia32_exc_label(node)) {
			/* emit the exception label of this instruction */
			ia32_assign_exc_label(node);
		}
		if (mark_spill_reload) {
			if (is_ia32_is_spill(node)) {
				ia32_emitf(NULL, "\txchg %ebx, %ebx        /* spill mark */\n");
			}
			if (is_ia32_is_reload(node)) {
				ia32_emitf(NULL, "\txchg %edx, %edx        /* reload mark */\n");
			}
			if (is_ia32_is_remat(node)) {
				ia32_emitf(NULL, "\txchg %ecx, %ecx        /* remat mark */\n");
			}
		}
	}
	if (op->ops.generic) {
		emit_func_ptr func = (emit_func_ptr) op->ops.generic;

		be_dbg_set_dbg_info(get_irn_dbg_info(node));

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
	ia32_emitf(NULL, "\t.p2align %u,,%u\n", align, skip);
}

/**
 * Emits gas alignment directives for Labels depended on cpu architecture.
 */
static void ia32_emit_align_label(void)
{
	unsigned align        = ia32_cg_config.label_alignment;
	unsigned maximum_skip = ia32_cg_config.label_alignment_max_skip;
	ia32_emit_alignment(align, maximum_skip);
}

/**
 * Test whether a block should be aligned.
 * For cpus in the P4/Athlon class it is useful to align jump labels to
 * 16 bytes. However we should only do that if the alignment nops before the
 * label aren't executed more often than we have jumps to the label.
 */
static int should_align_block(const ir_node *block)
{
	static const double DELTA = .0001;
	ir_exec_freq *exec_freq   = cg->birg->exec_freq;
	ir_node      *prev        = get_prev_block_sched(block);
	double        block_freq;
	double        prev_freq = 0;  /**< execfreq of the fallthrough block */
	double        jmp_freq  = 0;  /**< execfreq of all non-fallthrough blocks */
	int           i, n_cfgpreds;

	if (exec_freq == NULL)
		return 0;
	if (ia32_cg_config.label_alignment_factor <= 0)
		return 0;

	block_freq = get_block_execfreq(exec_freq, block);
	if (block_freq < DELTA)
		return 0;

	n_cfgpreds = get_Block_n_cfgpreds(block);
	for(i = 0; i < n_cfgpreds; ++i) {
		const ir_node *pred      = get_Block_cfgpred_block(block, i);
		double         pred_freq = get_block_execfreq(exec_freq, pred);

		if (pred == prev) {
			prev_freq += pred_freq;
		} else {
			jmp_freq  += pred_freq;
		}
	}

	if (prev_freq < DELTA && !(jmp_freq < DELTA))
		return 1;

	jmp_freq /= prev_freq;

	return jmp_freq > ia32_cg_config.label_alignment_factor;
}

/**
 * Emit the block header for a block.
 *
 * @param block       the block
 * @param prev_block  the previous block
 */
static void ia32_emit_block_header(ir_node *block)
{
	ir_graph     *irg = current_ir_graph;
	int           need_label = block_needs_label(block);
	int           i, arity;
	ir_exec_freq *exec_freq = cg->birg->exec_freq;

	if (block == get_irg_end_block(irg) || block == get_irg_start_block(irg))
		return;

	if (ia32_cg_config.label_alignment > 0) {
		/* align the current block if:
		 * a) if should be aligned due to its execution frequency
		 * b) there is no fall-through here
		 */
		if (should_align_block(block)) {
			ia32_emit_align_label();
		} else {
			/* if the predecessor block has no fall-through,
			   we can always align the label. */
			int i;
			int has_fallthrough = 0;

			for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
				ir_node *cfg_pred = get_Block_cfgpred(block, i);
				if (can_be_fallthrough(cfg_pred)) {
					has_fallthrough = 1;
					break;
				}
			}

			if (!has_fallthrough)
				ia32_emit_align_label();
		}
	}

	if (need_label || has_Block_label(block)) {
		ia32_emit_block_name(block);
		be_emit_char(':');

		be_emit_pad_comment();
		be_emit_cstring("   /* ");
	} else {
		be_emit_cstring("\t/* ");
		ia32_emit_block_name(block);
		be_emit_cstring(": ");
	}

	be_emit_cstring("preds:");

	/* emit list of pred blocks in comment */
	arity = get_irn_arity(block);
	for (i = 0; i < arity; ++i) {
		ir_node *predblock = get_Block_cfgpred_block(block, i);
		be_emit_irprintf(" %d", get_irn_node_nr(predblock));
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
static void ia32_gen_block(ir_node *block)
{
	ir_node *node;

	ia32_emit_block_header(block);

	/* emit the contents of the block */
	be_dbg_set_dbg_info(get_irn_dbg_info(block));
	sched_foreach(block, node) {
		ia32_emit_node(node);
	}
}

typedef struct exc_entry {
	ir_node *exc_instr;  /** The instruction that can issue an exception. */
	ir_node *block;      /** The block to call then. */
} exc_entry;

/**
 * Block-walker:
 * Sets labels for control flow nodes (jump target).
 * Links control predecessors to there destination blocks.
 */
static void ia32_gen_labels(ir_node *block, void *data)
{
	exc_entry **exc_list = data;
	ir_node *pred;
	int     n;

	for (n = get_Block_n_cfgpreds(block) - 1; n >= 0; --n) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block);

		pred = skip_Proj(pred);
		if (is_ia32_irn(pred) && get_ia32_exc_label(pred)) {
			exc_entry e;

			e.exc_instr = pred;
			e.block     = block;
			ARR_APP1(exc_entry, *exc_list, e);
			set_irn_link(pred, block);
		}
	}
}

/**
 * Compare two exception_entries.
 */
static int cmp_exc_entry(const void *a, const void *b)
{
	const exc_entry *ea = a;
	const exc_entry *eb = b;

	if (get_ia32_exc_label_id(ea->exc_instr) < get_ia32_exc_label_id(eb->exc_instr))
		return -1;
	return +1;
}

/**
 * Main driver. Emits the code for one routine.
 */
void ia32_gen_routine(ia32_code_gen_t *ia32_cg, ir_graph *irg)
{
	ir_entity *entity     = get_irg_entity(irg);
	exc_entry *exc_list   = NEW_ARR_F(exc_entry, 0);
	int i, n;

	cg       = ia32_cg;
	isa      = (const ia32_isa_t*) cg->arch_env;
	arch_env = cg->arch_env;
	do_pic   = cg->birg->main_env->options->pic;

	ia32_register_emitters();

	get_unique_label(pic_base_label, sizeof(pic_base_label), ".PIC_BASE");

	be_dbg_method_begin(entity, be_abi_get_stack_layout(cg->birg->abi));
	be_gas_emit_function_prolog(entity, ia32_cg_config.function_alignment);

	/* we use links to point to target blocks */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_block_walk_graph(irg, ia32_gen_labels, NULL, &exc_list);

	/* initialize next block links */
	n = ARR_LEN(cg->blk_sched);
	for (i = 0; i < n; ++i) {
		ir_node *block = cg->blk_sched[i];
		ir_node *prev  = i > 0 ? cg->blk_sched[i-1] : NULL;

		set_irn_link(block, prev);
	}

	for (i = 0; i < n; ++i) {
		ir_node *block = cg->blk_sched[i];

		ia32_gen_block(block);
	}

	be_gas_emit_function_epilog(entity);
	be_dbg_method_end();
	be_emit_char('\n');
	be_emit_write_line();

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	/* Sort the exception table using the exception label id's.
	   Those are ascending with ascending addresses. */
	qsort(exc_list, ARR_LEN(exc_list), sizeof(exc_list[0]), cmp_exc_entry);
	{
		int i;

		for (i = 0; i < ARR_LEN(exc_list); ++i) {
			be_emit_cstring("\t.long ");
			ia32_emit_exc_label(exc_list[i].exc_instr);
			be_emit_char('\n');
			be_emit_cstring("\t.long ");
			ia32_emit_block_name(exc_list[i].block);
			be_emit_char('\n');
		}
	}
	DEL_ARR_F(exc_list);
}

static const lc_opt_table_entry_t ia32_emitter_options[] = {
	LC_OPT_ENT_BOOL("mark_spill_reload",   "mark spills and reloads with ud opcodes", &mark_spill_reload),
	LC_OPT_LAST
};

void ia32_init_emitter(void)
{
	lc_opt_entry_t *be_grp;
	lc_opt_entry_t *ia32_grp;

	be_grp   = lc_opt_get_grp(firm_opt_get_root(), "be");
	ia32_grp = lc_opt_get_grp(be_grp, "ia32");

	lc_opt_add_table(ia32_grp, ia32_emitter_options);

	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.emitter");
}
