/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file implements the ia32 node emitter.
 * @author      Christian Wuerdig, Matthias Braun
 *
 * Summary table for x86 floatingpoint compares:
 * (remember effect of unordered on x86: ZF=1, PF=1, CF=1)
 *
 *   pnc_Eq  => !P && E
 *   pnc_Lt  => !P && B
 *   pnc_Le  => !P && BE
 *   pnc_Gt  => A
 *   pnc_Ge  => AE
 *   pnc_Lg  => NE
 *   pnc_Leg => NP  (ordered)
 *   pnc_Uo  => P
 *   pnc_Ue  => E
 *   pnc_Ul  => B
 *   pnc_Ule => BE
 *   pnc_Ug  => P || A
 *   pnc_Uge => P || AE
 *   pnc_Ne  => P || NE
 */
#include <inttypes.h>

#include "be_t.h"
#include "bearch_ia32_t.h"
#include "beasm.h"
#include "beblocksched.h"
#include "bediagnostic.h"
#include "begnuas.h"
#include "besched.h"
#include "bestack.h"
#include "beutil.h"
#include "execfreq.h"
#include "gen_ia32_emitter.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_architecture.h"
#include "ia32_emitter.h"
#include "ia32_new_nodes.h"
#include "irgwalk.h"
#include "irtools.h"
#include "lc_opts.h"
#include "panic.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static char       pic_base_label[128];
static ir_label_t exc_label_id;
static bool       mark_spill_reload;

static bool       sp_relative;
static int        frame_type_size;
static int        callframe_offset;

/** Return the next block in Block schedule */
static ir_node *get_prev_block_sched(const ir_node *block)
{
	return (ir_node*)get_irn_link(block);
}

/** Checks if the current block is a fall-through target. */
static bool is_fallthrough(const ir_node *cfgpred)
{
	if (!is_Proj(cfgpred))
		return true;
	ir_node *pred = get_Proj_pred(cfgpred);
	if (is_ia32_SwitchJmp(pred))
		return false;
	return true;
}

/**
 * returns non-zero if the given block needs a label
 * because of being a jump-target (and not a fall-through)
 */
static bool block_needs_label(const ir_node *block)
{
	if (get_Block_entity(block) != NULL)
		return true;

	int  n_cfgpreds = get_Block_n_cfgpreds(block);
	bool need_label = true;
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
 * Add a number to a prefix. This number will not be used a second time.
 */
static char *get_unique_label(char *buf, size_t buflen, const char *prefix)
{
	static unsigned long id = 0;
	snprintf(buf, buflen, "%s%s%lu", be_gas_get_private_prefix(), prefix, ++id);
	return buf;
}

static const char *get_register_name_8bit_low(const arch_register_t *reg)
{
	switch (reg->global_index) {
	case REG_EAX: return "al";
	case REG_EBX: return "bl";
	case REG_ECX: return "cl";
	case REG_EDX: return "dl";
	}
	panic("unexpected register");
}

static const char *get_register_name_8bit_high(const arch_register_t *reg)
{
	switch (reg->global_index) {
	case REG_EAX: return "ah";
	case REG_EBX: return "bh";
	case REG_ECX: return "ch";
	case REG_EDX: return "dh";
	}
	panic("unexpected register");
}

static const char *get_register_name_16bit(const arch_register_t *reg)
{
	switch (reg->global_index) {
	case REG_EAX: return "ax";
	case REG_EBX: return "bx";
	case REG_ECX: return "cx";
	case REG_EDX: return "dx";
	case REG_ESI: return "si";
	case REG_EDI: return "di";
	case REG_ESP: return "sp";
	case REG_EBP: return "bp";
	}
	panic("unexpected register");
}

static const char *get_register_name_mode(const arch_register_t *reg,
                                          ir_mode *mode)
{
	if (mode == NULL)
		return reg->name;
	unsigned size = get_mode_size_bits(mode);
	if (size == 8)
		return get_register_name_8bit_low(reg);
	else if (size == 16)
		return get_register_name_16bit(reg);
	else
		return reg->name;
}

/**
 * emit a register, possible shortened by a mode
 * @param reg   the register
 * @param mode  the mode of the register or NULL for full register
 */
static void emit_register(const arch_register_t *reg, ir_mode *mode)
{
	const char *name = get_register_name_mode(reg, mode);
	be_emit_char('%');
	be_emit_string(name);
}

static void ia32_emit_entity(x86_imm32_t const *const imm, bool no_pic_adjust)
{
	ir_entity *entity = imm->entity;
	be_gas_emit_entity(entity);

	if (is_tls_entity(entity)) {
		if (!entity_has_definition(entity)) {
			be_emit_cstring("@INDNTPOFF");
		} else {
			be_emit_cstring("@NTPOFF");
		}
	}

	if (be_options.pic && !no_pic_adjust && get_entity_type(entity) != get_code_type()) {
		be_emit_char('-');
		be_emit_string(pic_base_label);
	}
}

static void emit_ia32_immediate(bool const prefix, bool const no_pic_adjust,
                                x86_imm32_t const *const imm)
{
	if (prefix)
		be_emit_char('$');
	ir_entity const *const entity = imm->entity;
	int32_t          const offset = imm->offset;
	if (entity != NULL) {
		ia32_emit_entity(imm, no_pic_adjust);
		if (offset != 0)
			be_emit_irprintf("%+"PRId32, offset);
	} else {
		be_emit_irprintf("0x%"PRIX32, (uint32_t)offset);
	}
}

static void emit_ia32_immediate_attr(bool const prefix, ir_node const *const node)
{
	ia32_immediate_attr_t const *const attr = get_ia32_immediate_attr_const(node);
	emit_ia32_immediate(prefix, attr->no_pic_adjust, &attr->imm);
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
	panic("cannot output mode_suffix for %+F", mode);
}

static void ia32_emit_x87_mode_suffix(ir_node const *const node)
{
	/* we only need to emit the mode on address mode */
	if (get_ia32_op_type(node) == ia32_Normal)
		return;

	ir_mode *mode = get_ia32_ls_mode(node);
	assert(mode != NULL);

	if (mode_is_float(mode)) {
		switch (get_mode_size_bits(mode)) {
			case  32: be_emit_char('s'); return;
			case  64: be_emit_char('l'); return;
			/* long doubles have different sizes due to alignment on different
			 * platforms. */
			case  80:
			case  96:
			case 128: be_emit_char('t'); return;
		}
	} else {
		assert(mode_is_int(mode) || mode_is_reference(mode));
		switch (get_mode_size_bits(mode)) {
			case 16: be_emit_char('s');     return;
			case 32: be_emit_char('l');     return;
			/* gas docu says q is the suffix but gcc, objdump and icc use ll
			 * apparently */
			case 64: be_emit_cstring("ll"); return;
		}
	}
	panic("cannot output mode_suffix for %+F", mode);
}

static char get_xmm_mode_suffix(ir_mode *mode)
{
	assert(mode_is_float(mode));
	switch (get_mode_size_bits(mode)) {
	case 32: return 's';
	case 64: return 'd';
	default: panic("invalid XMM mode");
	}
}

static void ia32_emit_xmm_mode_suffix(ir_node const *const node)
{
	ir_mode *mode = get_ia32_ls_mode(node);
	assert(mode != NULL);
	be_emit_char(get_xmm_mode_suffix(mode));
}

/**
 * Returns the target block for a control flow node.
 */
static ir_node *get_cfop_target_block(const ir_node *irn)
{
	assert(get_irn_mode(irn) == mode_X);
	return (ir_node*)get_irn_link(irn);
}

/**
 * Emits the target label for a control flow node.
 */
static void ia32_emit_cfop_target(const ir_node *node)
{
	ir_node *block = get_cfop_target_block(node);
	be_gas_emit_block_name(block);
}

void x86_emit_condition_code(x86_condition_code_t cc)
{
	switch (cc) {
	case x86_cc_overflow:      be_emit_cstring("o");  return;
	case x86_cc_not_overflow:  be_emit_cstring("no"); return;
	case x86_cc_float_below:
	case x86_cc_float_unordered_below:
	case x86_cc_below:         be_emit_cstring("b");  return;
	case x86_cc_float_above_equal:
	case x86_cc_float_unordered_above_equal:
	case x86_cc_above_equal:   be_emit_cstring("ae"); return;
	case x86_cc_float_equal:
	case x86_cc_equal:         be_emit_cstring("e");  return;
	case x86_cc_float_not_equal:
	case x86_cc_not_equal:     be_emit_cstring("ne"); return;
	case x86_cc_float_below_equal:
	case x86_cc_float_unordered_below_equal:
	case x86_cc_below_equal:   be_emit_cstring("be"); return;
	case x86_cc_float_above:
	case x86_cc_float_unordered_above:
	case x86_cc_above:         be_emit_cstring("a");  return;
	case x86_cc_sign:          be_emit_cstring("s");  return;
	case x86_cc_not_sign:      be_emit_cstring("ns"); return;
	case x86_cc_parity:        be_emit_cstring("p");  return;
	case x86_cc_not_parity:    be_emit_cstring("np"); return;
	case x86_cc_less:          be_emit_cstring("l");  return;
	case x86_cc_greater_equal: be_emit_cstring("ge"); return;
	case x86_cc_less_equal:    be_emit_cstring("le"); return;
	case x86_cc_greater:       be_emit_cstring("g");  return;
	case x86_cc_float_parity_cases:
	case x86_cc_additional_float_cases:
		break;
	}
	panic("invalid ia32 condition code");
}

typedef enum ia32_emit_mod_t {
	EMIT_NONE         = 0,
	EMIT_RESPECT_LS   = 1U << 0,
	EMIT_ALTERNATE_AM = 1U << 1,
	EMIT_LONG         = 1U << 2,
	EMIT_HIGH_REG     = 1U << 3,
	EMIT_LOW_REG      = 1U << 4,
	EMIT_16BIT_REG    = 1U << 5,
	EMIT_SHIFT_COMMA  = 1U << 6,
} ia32_emit_mod_t;
ENUM_BITSET(ia32_emit_mod_t)

static ir_node const *get_irn_n_reg(ir_node const *const node, int const pos)
{
	ir_node *const in = get_irn_n(node, pos);
	return is_ia32_NoReg_GP(in) ? NULL : in;
}

/**
 * Emits address mode.
 */
static void ia32_emit_am(ir_node const *const node)
{
	/* just to be sure... */
	assert(get_ia32_frame_use(node) == IA32_FRAME_USE_NONE || get_ia32_frame_ent(node));

	if (get_ia32_am_tls_segment(node))
		be_emit_cstring("%gs:");

	ir_node const *const base = get_irn_n_reg(node, n_ia32_base);
	ir_node const *const idx  = get_irn_n_reg(node, n_ia32_index);

	/* emit offset */
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	int32_t          const offset = attr->am_imm.offset;
	ir_entity const *const entity = attr->am_imm.entity;
	if (entity) {
		const ia32_attr_t *attr = get_ia32_attr_const(node);
		ia32_emit_entity(&attr->am_imm, attr->am_sc_no_pic_adjust);
		if (offset != 0)
			be_emit_irprintf("%+"PRId32, offset);
	} else if (offset != 0 || (!base && !idx)) {
		/* also handle special case if nothing is set */
		be_emit_irprintf("%"PRId32, offset);
	}

	if (base || idx) {
		be_emit_char('(');

		/* emit base */
		if (base) {
			arch_register_t const *const reg = arch_get_irn_register(base);
			emit_register(reg, NULL);
		}

		/* emit index + scale */
		if (idx) {
			be_emit_char(',');
			arch_register_t const *const reg = arch_get_irn_register(idx);
			emit_register(reg, NULL);

			int const scale = get_ia32_am_scale(node);
			if (scale > 0)
				be_emit_irprintf(",%d", 1 << scale);
		}
		be_emit_char(')');
	}
}

static x86_condition_code_t determine_final_cc(ir_node const *node, int flags_pos);

void ia32_emitf(ir_node const *const node, char const *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);

	be_emit_char('\t');
	for (;;) {
		const char      *start = fmt;
		ia32_emit_mod_t  mod   = EMIT_NONE;

		while (*fmt != '%' && *fmt != '\n' && *fmt != '\0')
			++fmt;
		if (fmt != start) {
			be_emit_string_len(start, fmt - start);
		}

		if (*fmt == '\n') {
			be_emit_char('\n');
			be_emit_write_line();
			be_emit_char('\t');
			++fmt;
			if (*fmt == '\0')
				break;
			continue;
		}

		if (*fmt == '\0')
			break;

		++fmt;
		for (;;) {
			switch (*fmt) {
			case '*': mod |= EMIT_ALTERNATE_AM; break;
			case '#': mod |= EMIT_RESPECT_LS;   break;
			case 'l': mod |= EMIT_LONG;         break;
			case '>': mod |= EMIT_HIGH_REG;     break;
			case '<': mod |= EMIT_LOW_REG;      break;
			case '^': mod |= EMIT_16BIT_REG;    break;
			case ',': mod |= EMIT_SHIFT_COMMA;  break;
			default:
				goto end_of_mods;
			}
			++fmt;
		}
end_of_mods:

		switch (*fmt++) {
			arch_register_t const *reg;
			ir_node         const *imm;

			case '%':
				be_emit_char('%');
				break;

			case 'A': {
				switch (*fmt++) {
					case 'F':
						if (get_ia32_op_type(node) == ia32_Normal) {
							ia32_x87_attr_t const *const attr = get_ia32_x87_attr_const(node);
							char            const *const fmt  = attr->res_in_reg ? "%%st, %%%s" : "%%%s, %%st";
							be_emit_irprintf(fmt, attr->reg->name);
							break;
						} else {
							goto emit_AM;
						}

emit_AM:
					case 'M':
						if (mod & EMIT_ALTERNATE_AM)
							be_emit_char('*');
						ia32_emit_am(node);
						break;

					case 'R':
						reg = va_arg(ap, const arch_register_t*);
						if (get_ia32_op_type(node) == ia32_Normal) {
							goto emit_R;
						} else {
							goto emit_AM;
						}

					case 'S':
						if (get_ia32_op_type(node) == ia32_Normal) {
							goto emit_S;
						} else {
							++fmt;
							goto emit_AM;
						}

					default: goto unknown;
				}
				break;
			}

			case 'B': {
				ir_node const *const src = get_irn_n(node, n_ia32_binary_right);
				if (is_ia32_Immediate(src)) {
					emit_ia32_immediate_attr(true, src);
					be_emit_cstring(", ");
					if (get_ia32_op_type(node) == ia32_Normal) {
						goto destination_operand;
					} else {
						ia32_emit_am(node);
					}
				} else {
					if (get_ia32_op_type(node) == ia32_Normal) {
						reg = arch_get_irn_register(src);
						emit_register(reg, get_ia32_ls_mode(node));
					} else {
						ia32_emit_am(node);
					}
					be_emit_cstring(", ");
destination_operand:
					reg = arch_get_irn_register_in(node, n_ia32_binary_left);
					emit_register(reg, get_ia32_ls_mode(node));
				}
				break;
			}

			case 'D':
				if (!is_digit(*fmt))
					goto unknown;
				reg = arch_get_irn_register_out(node, *fmt++ - '0');
				goto emit_R;

			case 'F':
				if (*fmt == 'M') {
					ia32_emit_x87_mode_suffix(node);
				} else if (*fmt == 'P') {
					ia32_x87_attr_t const *const attr = get_ia32_x87_attr_const(node);
					if (attr->pop)
						be_emit_char('p');
				} else if (*fmt == 'R') {
					/* NOTE: Work around a gas quirk for non-commutative operations if the
					 * destination register is not %st0.  In this case r/non-r is swapped.
					 * %st0 = %st0 - %st1 -> fsub  %st1, %st0 (as expected)
					 * %st0 = %st1 - %st0 -> fsubr %st1, %st0 (as expected)
					 * %st1 = %st0 - %st1 -> fsub  %st0, %st1 (expected: fsubr)
					 * %st1 = %st1 - %st0 -> fsubr %st0, %st1 (expected: fsub)
					 * In fact this corresponds to the encoding of the instruction:
					 * - The r suffix selects whether %st0 is on the left (no r) or on the
					 *   right (r) side of the executed operation.
					 * - The placement of %st0 selects whether the result is written to
					 *   %st0 (right) or the other register (left).
					 * This means that it is sufficient to test whether the operands are
					 * permuted.  In particular it is not necessary to consider wether the
					 * result is to be placed into the explicit register operand. */
					if (get_ia32_x87_attr_const(node)->attr.ins_permuted)
						be_emit_char('r');
				} else if (*fmt == 'X') {
					ia32_emit_xmm_mode_suffix(node);
				} else if (*fmt == '0') {
					be_emit_char('%');
					be_emit_string(get_ia32_x87_attr_const(node)->reg->name);
				} else {
					goto unknown;
				}
				++fmt;
				break;

			case 'I':
				imm = node;
emit_I:
				if (mod & EMIT_SHIFT_COMMA) {
					const ia32_immediate_attr_t *attr
						= get_ia32_immediate_attr_const(imm);
					if (attr->imm.entity == NULL && attr->imm.offset == 1)
						break;
				}
				emit_ia32_immediate_attr(!(mod & EMIT_ALTERNATE_AM), imm);
				if (mod & EMIT_SHIFT_COMMA) {
					be_emit_char(',');
				}
				break;

			case 'L':
				ia32_emit_cfop_target(node);
				break;

			case 'M': {
				ir_mode *mode = get_ia32_ls_mode(node);
				if (!mode)
					mode = ia32_mode_gp;
				if (mod & EMIT_RESPECT_LS) {
					if (get_mode_size_bits(mode) == 32)
						break;
					be_emit_char(mode_is_signed(mode) ? 's' : 'z');
				}
				ia32_emit_mode_suffix_mode(mode);
				break;
			}

			case 'P': {
				x86_condition_code_t cc;
				if (*fmt == 'X') {
					++fmt;
					cc = (x86_condition_code_t)va_arg(ap, int);
				} else if (is_digit(*fmt)) {
					cc = determine_final_cc(node, *fmt - '0');
					++fmt;
				} else {
					goto unknown;
				}
				x86_emit_condition_code(cc);
				break;
			}

			case 'R':
				reg = va_arg(ap, const arch_register_t*);
emit_R:
				if (mod & EMIT_ALTERNATE_AM)
					be_emit_char('*');
				const char *name;
				if (mod & EMIT_HIGH_REG) {
					name = get_register_name_8bit_high(reg);
				} else if (mod & EMIT_LOW_REG) {
					name = get_register_name_8bit_low(reg);
				} else if (mod & EMIT_16BIT_REG) {
					name = get_register_name_16bit(reg);
				} else {
					name = mod & EMIT_RESPECT_LS
						 ? get_register_name_mode(reg, get_ia32_ls_mode(node))
						 : reg->name;
				}
				be_emit_char('%');
				be_emit_string(name);
				if (mod & EMIT_SHIFT_COMMA) {
					be_emit_char(',');
				}
				break;

emit_S:
			case 'S': {
				if (!is_digit(*fmt))
					goto unknown;

				unsigned pos = *fmt++ - '0';
				ir_node const *const src = get_irn_n(node, pos);
				if (is_ia32_Immediate(src)) {
					imm = src;
					goto emit_I;
				} else {
					reg = arch_get_irn_register(src);
					goto emit_R;
				}
			}

			case 's': {
				const char *str = va_arg(ap, const char*);
				be_emit_string(str);
				break;
			}

			case 'u':
				if (mod & EMIT_LONG) {
					unsigned long num = va_arg(ap, unsigned long);
					be_emit_irprintf("%lu", num);
				} else {
					unsigned num = va_arg(ap, unsigned);
					be_emit_irprintf("%u", num);
				}
				break;

			case 'd':
				if (mod & EMIT_LONG) {
					long num = va_arg(ap, long);
					be_emit_irprintf("%ld", num);
				} else {
					int num = va_arg(ap, int);
					be_emit_irprintf("%d", num);
				}
				break;

			default:
unknown:
				panic("unknown format conversion");
		}
	}

	be_emit_finish_line_gas(node);
	va_end(ap);
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
			return find_original_value(get_irn_n(pred, get_Proj_num(node)));
		} else if (be_is_MemPerm(pred)) {
			return find_original_value(get_irn_n(pred, get_Proj_num(node)));
		} else if (is_ia32_Load(pred)) {
			return find_original_value(get_irn_n(pred, n_ia32_Load_mem));
		} else if (is_ia32_Store(pred)) {
			return find_original_value(get_irn_n(pred, n_ia32_Store_val));
		} else {
			return node;
		}
	} else if (is_Phi(node)) {
		foreach_irn_in(node, i, in) {
			ir_node *res = find_original_value(in);
			if (res != NULL)
				return res;
		}
		return NULL;
	} else {
		return node;
	}
}

static x86_condition_code_t determine_final_cc(ir_node const *const node, int const flags_pos)
{
	ia32_condcode_attr_t const *const attr = get_ia32_condcode_attr_const(node);
	x86_condition_code_t              cc   = attr->condition_code;
	if (attr->attr.ins_permuted)
		cc = x86_negate_condition_code(cc);

	ir_node *flags = skip_Proj(get_irn_n(node, flags_pos));

	/* Permuted operands of a test instruction do not change the result. */
	if (is_ia32_Test(flags))
		return cc;

	if (is_ia32_Sahf(flags)) {
		flags = get_irn_n(flags, n_ia32_Sahf_val);
		if (!is_ia32_FucomFnstsw(flags) && !is_ia32_FucomppFnstsw(flags) && !is_ia32_FtstFnstsw(flags)) {
			ir_graph *const irg = get_irn_irg(node);
			inc_irg_visited(irg);
			flags = find_original_value(flags);
			assert(is_ia32_FucomFnstsw(flags) || is_ia32_FucomppFnstsw(flags) || is_ia32_FtstFnstsw(flags));
		}
	}

	ia32_attr_t const *const flags_attr = get_ia32_attr_const(flags);
	if (flags_attr->ins_permuted)
		cc = x86_invert_condition_code(cc);
	return cc;
}

/**
 * Emits an exception label for a given node.
 */
static void ia32_emit_exc_label(const ir_node *node)
{
	be_emit_string(be_gas_insn_label_prefix());
	be_emit_irprintf("%lu", get_ia32_exc_label_id(node));
}

static bool fallthrough_possible(const ir_node *block, const ir_node *target)
{
	return get_prev_block_sched(target) == block;
}

/**
 * Emits the jump sequence for a conditional jump (cmp + jmp_true + jmp_false)
 */
static void emit_ia32_Jcc(const ir_node *node)
{
	x86_condition_code_t cc = determine_final_cc(node, n_ia32_Jcc_eflags);

	/* get both Projs */
	ir_node const *proj_true   = get_Proj_for_pn(node, pn_ia32_Jcc_true);
	ir_node const *target_true = get_cfop_target_block(proj_true);
	ir_node const *proj_false  = get_Proj_for_pn(node, pn_ia32_Jcc_false);
	ir_node const *block       = get_nodes_block(node);
	if (fallthrough_possible(block, target_true)) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj_true;

		proj_true  = proj_false;
		proj_false = t;
		cc         = x86_negate_condition_code(cc);
	}
	const ir_node *target_false = get_cfop_target_block(proj_false);
	bool           fallthrough  = fallthrough_possible(block, target_false);
	/* if we can't have a fallthrough anyway, put the more likely case first */
	if (!fallthrough) {
		/* We would need execfreq for the concrete edge, but don't have it
		 * available here, so we use the block execfreq :-( */
		double freq_true  = get_block_execfreq(target_true);
		double freq_false = get_block_execfreq(target_false);
		if (freq_false > freq_true) {
			const ir_node *t = proj_true;
			proj_true  = proj_false;
			proj_false = t;
			cc         = x86_negate_condition_code(cc);
		}
	}

	bool need_parity_label = false;
	if (cc & x86_cc_float_parity_cases) {
		/* Some floating point comparisons require a test of the parity flag,
		 * which indicates that the result is unordered */
		if (cc & x86_cc_negated) {
			ia32_emitf(proj_true, "jp %L");
		} else {
			/* we need a local label if the false proj is a fallthrough
			 * as the falseblock might have no label emitted then */
			if (fallthrough) {
				need_parity_label = true;
				ia32_emitf(proj_false, "jp 1f");
			} else {
				ia32_emitf(proj_false, "jp %L");
			}
		}
	}
	ia32_emitf(proj_true, "j%PX %L", (int)cc);
	if (need_parity_label) {
		be_emit_cstring("1:\n");
		be_emit_write_line();
	}

	/* the second Proj might be a fallthrough */
	if (fallthrough) {
		if (be_options.verbose_asm)
			ia32_emitf(proj_false, "/* fallthrough to %L */");
	} else {
		ia32_emitf(proj_false, "jmp %L");
	}
}

/**
 * Emits an ia32 Setcc. This is mostly easy but some floating point compares
 * are tricky.
 */
static void emit_ia32_Setcc(const ir_node *node)
{
	const arch_register_t *dreg = arch_get_irn_register_out(node, pn_ia32_Setcc_res);

	x86_condition_code_t const cc = determine_final_cc(node, n_ia32_Setcc_eflags);
	if (cc & x86_cc_float_parity_cases) {
		if (cc & x86_cc_negated) {
			ia32_emitf(node, "set%PX %<R", (int)cc, dreg);
			ia32_emitf(node, "setp %>R", dreg);
			ia32_emitf(node, "orb %>R, %<R", dreg, dreg);
		} else {
			ia32_emitf(node, "set%PX %<R", (int)cc, dreg);
			ia32_emitf(node, "setnp %>R", dreg);
			ia32_emitf(node, "andb %>R, %<R", dreg, dreg);
		}
	} else {
		ia32_emitf(node, "set%PX %#R", (int)cc, dreg);
	}
}

/**
 * Emits code for a SwitchJmp
 */
static void emit_ia32_SwitchJmp(const ir_node *node)
{
	ir_entity             *jump_table = get_ia32_am_ent(node);
	const ir_switch_table *table      = get_ia32_switch_table(node);

	ia32_emitf(node, "jmp %*AM");
	be_emit_jump_table(node, table, jump_table, get_cfop_target_block);
}

/**
 * Emits code for a unconditional jump.
 */
static void emit_ia32_Jmp(const ir_node *node)
{
	/* we have a block schedule */
	ir_node *block  = get_nodes_block(node);
	ir_node *target = get_cfop_target_block(node);
	if (fallthrough_possible(block, target)) {
		if (be_options.verbose_asm)
			ia32_emitf(node, "/* fallthrough to %L */");
	} else {
		ia32_emitf(node, "jmp %L");
	}
}

static void emit_ia32_asm_register(const arch_register_t *reg, char modifier,
                                   ir_mode *mode)
{
	const char *name;
	switch (modifier) {
	case '\0': name = get_register_name_mode(reg, mode); break;
	case  'b': name = get_register_name_8bit_low(reg); break;
	case  'h': name = get_register_name_8bit_high(reg); break;
	case  'w': name = get_register_name_16bit(reg); break;
	case  'k': name = reg->name; break;
	default:
		panic("invalid asm op modifier");
	}
	be_emit_char('%');
	be_emit_string(name);
}

static void emit_ia32_asm_operand(ir_node const *const node, char const modifier, unsigned const pos)
{
	switch (modifier) {
	case '\0':
	case 'b':
	case 'h':
	case 'k':
	case 'w':
		break;

	default:
		be_errorf(node, "asm contains unknown modifier '%c'", modifier);
		return;
	}

	be_asm_attr_t     const *const attr = get_be_asm_attr_const(node);
	x86_asm_operand_t const *const op   = &((x86_asm_operand_t const*)attr->operands)[pos];
	switch ((x86_asm_operand_kind_t)op->kind) {
	case ASM_OP_INVALID:
		panic("invalid asm operand");

	case ASM_OP_IN_REG: {
		arch_register_t const *const reg = arch_get_irn_register_in(node, op->inout_pos);
		emit_ia32_asm_register(reg, modifier, op->u.mode);
		return;
	}

	case ASM_OP_OUT_REG: {
		arch_register_t const *const reg = arch_get_irn_register_out(node, op->inout_pos);
		emit_ia32_asm_register(reg, modifier, op->u.mode);
		return;
	}

	case ASM_OP_MEMORY: {
		arch_register_t const *const reg = arch_get_irn_register_in(node, op->inout_pos);
		be_emit_irprintf("(%%%s)", reg->name);
		return;
	}

	case ASM_OP_IMMEDIATE:
		emit_ia32_immediate(true, true, &op->u.imm32);
		return;
	}
	panic("invalid asm operand kind");
}

/**
 * Emits code for an ASM pseudo op.
 */
static void emit_ia32_Asm(const ir_node *node)
{
	be_emit_asm(node, emit_ia32_asm_operand);
}

/**
 * Emit movsb/w instructions to make mov count divideable by 4
 */
static void emit_CopyB_prolog(unsigned size)
{
	if (size & 1)
		ia32_emitf(NULL, "movsb");
	if (size & 2)
		ia32_emitf(NULL, "movsw");
}

/**
 * Emit rep movsd instruction for memcopy.
 */
static void emit_ia32_CopyB(const ir_node *node)
{
	unsigned size = get_ia32_copyb_size(node);

	emit_CopyB_prolog(size);
	ia32_emitf(node, "rep movsd");
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
		ia32_emitf(NULL, "movsd");
	}
}


/**
 * Emit code for conversions (I, FP), (FP, I) and (FP, FP).
 */
static void emit_ia32_Conv_with_FP(const ir_node *node, const char* conv_f,
		const char* conv_d)
{
	ir_mode    *ls_mode = get_ia32_ls_mode(node);
	int         ls_bits = get_mode_size_bits(ls_mode);
	const char *conv    = ls_bits == 32 ? conv_f : conv_d;
	ia32_emitf(node, "cvt%s %AS3, %D0", conv);
}

static void emit_ia32_Conv_I2FP(const ir_node *node)
{
	emit_ia32_Conv_with_FP(node, "si2ss", "si2sd");
}

static void emit_ia32_Conv_FP2I(const ir_node *node)
{
	emit_ia32_Conv_with_FP(node, "ss2si", "sd2si");
}

static void emit_ia32_Conv_FP2FP(const ir_node *node)
{
	emit_ia32_Conv_with_FP(node, "sd2ss", "ss2sd");
}

/**
 * Emits code to increase stack pointer.
 */
static void emit_be_IncSP(const ir_node *node)
{
	int offs = be_get_IncSP_offset(node);
	if (offs == 0)
		return;

	if (offs > 0) {
		ia32_emitf(node, "subl $%u, %D0", offs);
	} else {
		ia32_emitf(node, "addl $%u, %D0", -offs);
	}
}

/**
 * Emits code for Copy/CopyKeep.
 */
static void Copy_emitter(const ir_node *node, const ir_node *op)
{
	const arch_register_t *in  = arch_get_irn_register(op);
	const arch_register_t *out = arch_get_irn_register(node);
	if (in == out)
		return;

	/* copies of fp nodes aren't real... */
	if (in->cls == &ia32_reg_classes[CLASS_ia32_fp])
		return;

	ia32_emitf(node, "movl %R, %R", in, out);
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
	arch_register_t const *const reg0 = arch_get_irn_register_out(node, 0);
	arch_register_t const *const reg1 = arch_get_irn_register_out(node, 1);

	arch_register_class_t const *const cls = reg0->cls;
	assert(cls == reg1->cls && "Register class mismatch at Perm");

	if (cls == &ia32_reg_classes[CLASS_ia32_gp]) {
		ia32_emitf(node, "xchg %R, %R", reg1, reg0);
	} else if (cls == &ia32_reg_classes[CLASS_ia32_xmm]) {
		ia32_emitf(NULL, "xorpd %R, %R", reg1, reg0);
		ia32_emitf(NULL, "xorpd %R, %R", reg0, reg1);
		ia32_emitf(node, "xorpd %R, %R", reg1, reg0);
	} else if (cls == &ia32_reg_classes[CLASS_ia32_fp]) {
		/* is a NOP */
	} else {
		panic("unexpected register class in be_Perm (%+F)", node);
	}
}

/* helper function for emit_ia32_Minus64 */
static void emit_mov(const ir_node* node, const arch_register_t *src, const arch_register_t *dst)
{
	ia32_emitf(node, "movl %R, %R", src, dst);
}

/* helper function for emit_ia32_Minus64 */
static void emit_neg(const ir_node* node, const arch_register_t *reg)
{
	ia32_emitf(node, "negl %R", reg);
}

/* helper function for emit_ia32_Minus64 */
static void emit_sbb0(const ir_node* node, const arch_register_t *reg)
{
	ia32_emitf(node, "sbbl $0, %R", reg);
}

/* helper function for emit_ia32_Minus64 */
static void emit_sbb(const ir_node* node, const arch_register_t *src, const arch_register_t *dst)
{
	ia32_emitf(node, "sbbl %R, %R", src, dst);
}

/* helper function for emit_ia32_Minus64 */
static void emit_xchg(const ir_node* node, const arch_register_t *src, const arch_register_t *dst)
{
	ia32_emitf(node, "xchgl %R, %R", src, dst);
}

/* helper function for emit_ia32_Minus64 */
static void emit_zero(const ir_node* node, const arch_register_t *reg)
{
	ia32_emitf(node, "xorl %R, %R", reg, reg);
}

static void emit_ia32_Minus64(const ir_node *node)
{
	arch_register_t const *const in_lo  = arch_get_irn_register_in(node, n_ia32_Minus64_low);
	arch_register_t const *const in_hi  = arch_get_irn_register_in(node, n_ia32_Minus64_high);
	arch_register_t const *const out_lo = arch_get_irn_register_out(node, pn_ia32_Minus64_res_low);
	arch_register_t const *const out_hi = arch_get_irn_register_out(node, pn_ia32_Minus64_res_high);

	if (out_lo == in_lo) {
		if (out_hi != in_hi) {
			if (in_lo == in_hi) {
				/* a -> a, a -> d */
				emit_neg( node, out_lo);
				emit_mov( node, out_lo, out_hi);
				emit_sbb0(node, out_hi);
				return;
			} else {
				/* a -> a, b -> d */
				goto zero_neg;
			}
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
	ia32_emitf(node, "call %s", pic_base_label);
	be_emit_irprintf("%s:\n", pic_base_label);
	be_emit_write_line();
	ia32_emitf(node, "popl %D0");
}

static void emit_ia32_ClimbFrame(const ir_node *node)
{
	const ia32_climbframe_attr_t *attr = get_ia32_climbframe_attr_const(node);

	ia32_emitf(node, "movl $%u, %D1", attr->count);
	be_emit_cstring("0:\n");
	be_emit_write_line();
	ia32_emitf(node, "movl (%D0), %D0");
	ia32_emitf(node, "dec %D1");
	ia32_emitf(node, "jnz 0b");
}

static void emit_ia32_Return(const ir_node *node)
{
	const ia32_return_attr_t *attr = get_ia32_return_attr_const(node);
	unsigned pop = attr->pop;
	if (attr->emit_pop || pop > 0) {
		ia32_emitf(node, "ret $%u", pop);
	} else {
		ia32_emitf(node, "ret");
	}
}

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void ia32_register_emitters(void)
{
	be_init_emitters();

	/* register all emitter functions defined in spec */
	ia32_register_spec_emitters();

	be_set_emitter(op_be_Asm,          emit_ia32_Asm);
	be_set_emitter(op_be_Copy,         emit_be_Copy);
	be_set_emitter(op_be_CopyKeep,     emit_be_CopyKeep);
	be_set_emitter(op_be_IncSP,        emit_be_IncSP);
	be_set_emitter(op_be_Perm,         emit_be_Perm);
	be_set_emitter(op_ia32_Return,     emit_ia32_Return);
	be_set_emitter(op_ia32_ClimbFrame, emit_ia32_ClimbFrame);
	be_set_emitter(op_ia32_Conv_FP2FP, emit_ia32_Conv_FP2FP);
	be_set_emitter(op_ia32_Conv_FP2I,  emit_ia32_Conv_FP2I);
	be_set_emitter(op_ia32_Conv_I2FP,  emit_ia32_Conv_I2FP);
	be_set_emitter(op_ia32_CopyB,      emit_ia32_CopyB);
	be_set_emitter(op_ia32_CopyB_i,    emit_ia32_CopyB_i);
	be_set_emitter(op_ia32_GetEIP,     emit_ia32_GetEIP);
	be_set_emitter(op_ia32_Jcc,        emit_ia32_Jcc);
	be_set_emitter(op_ia32_Jmp,        emit_ia32_Jmp);
	be_set_emitter(op_ia32_Minus64,    emit_ia32_Minus64);
	be_set_emitter(op_ia32_Setcc,      emit_ia32_Setcc);
	be_set_emitter(op_ia32_SwitchJmp,  emit_ia32_SwitchJmp);
}

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
	DBG((dbg, LEVEL_1, "emitting code for %+F\n", node));

	if (is_ia32_irn(node)) {
		/* emit the exception label of this instruction */
		if (get_ia32_exc_label(node))
			ia32_assign_exc_label(node);
		if (mark_spill_reload) {
			if (is_ia32_is_spill(node))
				ia32_emitf(NULL, "xchg %ebx, %ebx        /* spill mark */");
			if (is_ia32_is_reload(node))
				ia32_emitf(NULL, "xchg %edx, %edx        /* reload mark */");
			if (is_ia32_is_remat(node))
				ia32_emitf(NULL, "xchg %ecx, %ecx        /* remat mark */");
		}
	}

	be_emit_node(node);

	if (sp_relative) {
		int sp_change = ia32_get_sp_bias(node);
		if (sp_change != 0) {
			assert(sp_change != SP_BIAS_RESET);
			callframe_offset += sp_change;
			be_dwarf_callframe_offset(callframe_offset);
		}
	}
}

/**
 * Emits gas alignment directives
 */
static void ia32_emit_alignment(unsigned align, unsigned skip)
{
	ia32_emitf(NULL, ".p2align %u,,%u", align, skip);
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
static bool should_align_block(const ir_node *block)
{
	static const double DELTA = .0001;
	ir_node *prev      = get_prev_block_sched(block);
	double   prev_freq = 0;  /**< execfreq of the fallthrough block */
	double   jmp_freq  = 0;  /**< execfreq of all non-fallthrough blocks */

	if (ia32_cg_config.label_alignment_factor <= 0)
		return false;

	double block_freq = get_block_execfreq(block);
	if (block_freq < DELTA)
		return false;

	for (int i = 0, n_cfgpreds = get_Block_n_cfgpreds(block);
	     i < n_cfgpreds; ++i) {
		const ir_node *pred      = get_Block_cfgpred_block(block, i);
		double         pred_freq = get_block_execfreq(pred);

		if (pred == prev) {
			assert (pred_freq > 0.0f && "Might result in NaN below!?!");
			prev_freq += pred_freq;
		} else {
			jmp_freq  += pred_freq;
		}
	}
	if (prev_freq == 0.0)
		/* Prev-by-schedule block never falls-through => align! */
		return true;

	if (prev_freq < DELTA && !(jmp_freq < DELTA))
		return true;

	jmp_freq /= prev_freq;
	assert (jmp_freq == jmp_freq && "NaN!");

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
	ir_graph *const irg = get_irn_irg(block);
	if (block == get_irg_end_block(irg))
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
			bool has_fallthrough = false;
			for (int i = get_Block_n_cfgpreds(block); i-- > 0; ) {
				ir_node *pred_block = get_Block_cfgpred_block(block, i);
				if (fallthrough_possible(pred_block, block)) {
					has_fallthrough = true;
					break;
				}
			}

			if (!has_fallthrough)
				ia32_emit_align_label();
		}
	}

	const bool need_label = block_needs_label(block);
	be_gas_begin_block(block, need_label);
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void ia32_gen_block(ir_node *block)
{
	ia32_emit_block_header(block);

	if (sp_relative) {
		ir_graph *irg = get_irn_irg(block);
		callframe_offset = 4; /* 4 bytes for the return address */
		/* ESP guessing, TODO perform a real ESP simulation */
		if (block != get_irg_start_block(irg)) {
			callframe_offset += frame_type_size;
		}
		be_dwarf_callframe_offset(callframe_offset);
	}

	/* emit the contents of the block */
	be_dwarf_location(get_irn_dbg_info(block));
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
	exc_entry **exc_list = (exc_entry**)data;
	for (unsigned n = get_Block_n_cfgpreds(block); n-- > 0; ) {
		ir_node *pred = get_Block_cfgpred(block, n);
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
	const exc_entry *ea = (const exc_entry*)a;
	const exc_entry *eb = (const exc_entry*)b;
	if (get_ia32_exc_label_id(ea->exc_instr) < get_ia32_exc_label_id(eb->exc_instr))
		return -1;
	return +1;
}

static parameter_dbg_info_t *construct_parameter_infos(ir_graph *irg)
{
	ir_entity            *entity    = get_irg_entity(irg);
	ir_type              *type      = get_entity_type(entity);
	size_t                n_params  = get_method_n_params(type);
	be_stack_layout_t    *layout    = be_get_irg_stack_layout(irg);
	ir_type              *arg_type  = layout->arg_type;
	size_t                n_members = get_compound_n_members(arg_type);
	parameter_dbg_info_t *infos     = XMALLOCNZ(parameter_dbg_info_t, n_params);

	for (size_t i = 0; i < n_members; ++i) {
		ir_entity *member = get_compound_member(arg_type, i);
		if (!is_parameter_entity(member))
			continue;
		size_t param = get_entity_parameter_number(member);
		if (param == IR_VA_START_PARAMETER_NUMBER)
			continue;
		assert(infos[param].entity == NULL && infos[param].reg == NULL);
		infos[param].reg    = NULL;
		infos[param].entity = member;
	}

	return infos;
}

/**
 * Main driver. Emits the code for one routine.
 */
static void ia32_emit_function_text(ir_graph *const irg, ir_node **const blk_sched)
{
	ir_entity         *entity   = get_irg_entity(irg);
	exc_entry         *exc_list = NEW_ARR_F(exc_entry, 0);
	be_stack_layout_t *layout   = be_get_irg_stack_layout(irg);

	be_gas_elf_type_char = '@';

	ia32_register_emitters();

	get_unique_label(pic_base_label, sizeof(pic_base_label), "PIC_BASE");

	parameter_dbg_info_t *infos = construct_parameter_infos(irg);
	be_gas_emit_function_prolog(entity, ia32_cg_config.function_alignment,
	                            infos);
	free(infos);

	sp_relative = layout->sp_relative;
	if (layout->sp_relative) {
		ir_type *frame_type = get_irg_frame_type(irg);
		frame_type_size = get_type_size_bytes(frame_type);
		be_dwarf_callframe_register(&ia32_registers[REG_ESP]);
	} else {
		/* well not entirely correct here, we should emit this after the
		 * "movl esp, ebp" */
		be_dwarf_callframe_register(&ia32_registers[REG_EBP]);
		/* TODO: do not hardcode the following */
		be_dwarf_callframe_offset(8);
		be_dwarf_callframe_spilloffset(&ia32_registers[REG_EBP], -8);
	}

	/* we use links to point to target blocks */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_block_walk_graph(irg, ia32_gen_labels, NULL, &exc_list);

	/* initialize next block links */
	size_t n = ARR_LEN(blk_sched);
	for (size_t i = 0; i < n; ++i) {
		ir_node *block = blk_sched[i];
		ir_node *prev  = i > 0 ? blk_sched[i-1] : NULL;

		set_irn_link(block, prev);
	}

	for (size_t i = 0; i < n; ++i) {
		ir_node *block = blk_sched[i];

		ia32_gen_block(block);
	}

	be_gas_emit_function_epilog(entity);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	/* Sort the exception table using the exception label id's.
	   Those are ascending with ascending addresses. */
	QSORT_ARR(exc_list, cmp_exc_entry);
	for (size_t e = 0; e < ARR_LEN(exc_list); ++e) {
		be_emit_cstring("\t.long ");
		ia32_emit_exc_label(exc_list[e].exc_instr);
		be_emit_char('\n');
		be_emit_cstring("\t.long ");
		be_gas_emit_block_name(exc_list[e].block);
		be_emit_char('\n');
	}
	DEL_ARR_F(exc_list);
}

static const lc_opt_table_entry_t ia32_emitter_options[] = {
	LC_OPT_ENT_BOOL("mark_spill_reload",   "mark spills and reloads with ud opcodes", &mark_spill_reload),
	LC_OPT_LAST
};

/* ==== Experimental binary emitter ==== */

/** Returns the encoding for a pnc field. */
static unsigned char pnc2cc(x86_condition_code_t cc)
{
	return cc & 0xf;
}

enum OpSize {
	OP_8          = 0x00, /* 8bit operation. */
	OP_16_32      = 0x01, /* 16/32bit operation. */
	OP_MEM_SRC    = 0x02, /* The memory operand is in the source position. */
	OP_IMM8       = 0x02, /* 8bit immediate, which gets sign extended for 16/32bit operation. */
	OP_16_32_IMM8 = 0x03, /* 16/32bit operation with sign extended 8bit immediate. */
	OP_EAX        = 0x04, /* Short form of instruction with al/ax/eax as operand. */
};

/** The mod encoding of the ModR/M */
enum Mod {
	MOD_IND          = 0x00, /**< [reg1] */
	MOD_IND_BYTE_OFS = 0x40, /**< [reg1 + byte ofs] */
	MOD_IND_WORD_OFS = 0x80, /**< [reg1 + word ofs] */
	MOD_REG          = 0xC0  /**< reg1 */
};

/** create R/M encoding for ModR/M */
#define ENC_RM(x) (x)
/** create REG encoding for ModR/M */
#define ENC_REG(x) ((x) << 3)

/** create encoding for a SIB byte */
#define ENC_SIB(scale, index, base) ((scale) << 6 | (index) << 3 | (base))

/* Node: The following routines are supposed to append bytes, words, dwords
   to the output stream.
   Currently the implementation is stupid in that it still creates output
   for an "assembler" in the form of .byte, .long
   We will change this when enough infrastructure is there to create complete
   machine code in memory/object files */

static void bemit8(const uint8_t byte)
{
	be_emit_irprintf("\t.byte 0x%x\n", byte);
	be_emit_write_line();
}

static void bemit16(const uint16_t u16)
{
	be_emit_irprintf("\t.word 0x%x\n", u16);
	be_emit_write_line();
}

static void bemit32(const uint32_t u32)
{
	be_emit_irprintf("\t.long 0x%x\n", u32);
	be_emit_write_line();
}

/**
 * Emit address of an entity. If @p is_relative is true then a relative
 * offset from behind the address to the entity is created.
 */
static void bemit_entity(x86_imm32_t const *const imm, bool is_relative)
{
	ir_entity *entity = imm->entity;
	int32_t    offset = imm->offset;
	if (entity == NULL) {
		bemit32(offset);
		return;
	}

	/* the final version should remember the position in the bytestream
	   and patch it with the correct address at linktime... */
	be_emit_cstring("\t.long ");
	be_gas_emit_entity(entity);

	if (is_tls_entity(entity)) {
		if (!entity_has_definition(entity)) {
			be_emit_cstring("@INDNTPOFF");
		} else {
			be_emit_cstring("@NTPOFF");
		}
	}

	if (is_relative) {
		be_emit_cstring("-.");
		offset -= 4;
	}

	if (offset != 0) {
		be_emit_irprintf("%+"PRId32, offset);
	}
	be_emit_char('\n');
	be_emit_write_line();
}

static void bemit_jmp_destination(const ir_node *dest_block)
{
	be_emit_cstring("\t.long ");
	be_gas_emit_block_name(dest_block);
	be_emit_cstring(" - . - 4\n");
	be_emit_write_line();
}

/* end emit routines, all emitters following here should only use the functions
   above. */

typedef enum reg_modifier {
	REG_LOW  = 0,
	REG_HIGH = 4
} reg_modifier_t;

/** Create a ModR/M byte for src1,src2 registers */
static void bemit_modrr(const arch_register_t *src1,
                        const arch_register_t *src2)
{
	unsigned char modrm = MOD_REG;
	modrm |= ENC_RM(src1->encoding);
	modrm |= ENC_REG(src2->encoding);
	bemit8(modrm);
}

/** Create a ModR/M8 byte for src1,src2 registers */
static void bemit_modrr8(reg_modifier_t high_part1, const arch_register_t *src1,
						 reg_modifier_t high_part2, const arch_register_t *src2)
{
	unsigned char modrm = MOD_REG;
	modrm |= ENC_RM( high_part1 | src1->encoding);
	modrm |= ENC_REG(high_part2 | src2->encoding);
	bemit8(modrm);
}

/** Create a ModR/M byte for one register and extension */
static void bemit_modru(const arch_register_t *reg, unsigned ext)
{
	unsigned char modrm = MOD_REG;
	assert(ext <= 7);
	modrm |= ENC_RM(reg->encoding);
	modrm |= ENC_REG(ext);
	bemit8(modrm);
}

/** Create a ModR/M8 byte for one register */
static void bemit_modrm8(reg_modifier_t high_part, const arch_register_t *reg)
{
	unsigned char modrm = MOD_REG;
	assert(reg->encoding < 4);
	modrm |= ENC_RM(high_part | reg->encoding);
	modrm |= MOD_REG;
	bemit8(modrm);
}

static bool ia32_is_8bit_imm(ia32_immediate_attr_t const *const imm)
{
	return !imm->imm.entity && ia32_is_8bit_val(imm->imm.offset);
}

/**
 * Emit an address mode.
 *
 * @param reg   content of the reg field: either a register index or an opcode extension
 * @param node  the node
 */
static void bemit_mod_am(unsigned reg, const ir_node *node)
{
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	ir_entity const *const entity = attr->am_imm.entity;
	int32_t          const offset = attr->am_imm.offset;

	/* set the mod part depending on displacement */
	unsigned modrm    = 0;
	unsigned emitoffs = 0;
	if (entity) {
		modrm |= MOD_IND_WORD_OFS;
		emitoffs = 32;
	} else if (offset == 0) {
		modrm |= MOD_IND;
		emitoffs = 0;
	} else if (ia32_is_8bit_val(offset)) {
		modrm |= MOD_IND_BYTE_OFS;
		emitoffs = 8;
	} else {
		modrm |= MOD_IND_WORD_OFS;
		emitoffs = 32;
	}

	unsigned base_enc;
	ir_node const *const base = get_irn_n_reg(node, n_ia32_base);
	if (base) {
		arch_register_t const *const base_reg = arch_get_irn_register(base);
		base_enc = base_reg->encoding;
	} else {
		/* Use the EBP encoding + MOD_IND if NO base register. There is
		 * always a 32bit offset present in this case. */
		modrm    = MOD_IND;
		base_enc = 0x05;
		emitoffs = 32;
	}

	/* Determine if we need a SIB byte. */
	bool                 emitsib = false;
	unsigned             sib     = 0;
	ir_node const *const idx     = get_irn_n_reg(node, n_ia32_index);
	if (idx) {
		arch_register_t const *const reg_index = arch_get_irn_register(idx);
		unsigned               const scale     = get_ia32_am_scale(node);
		/* R/M set to ESP means SIB in 32bit mode. */
		modrm   |= ENC_RM(0x04);
		sib      = ENC_SIB(scale, reg_index->encoding, base_enc);
		emitsib = true;
	} else if (base_enc == 0x04) {
		/* for the above reason we are forced to emit a SIB when base is ESP.
		 * Only the base is used, index must be ESP too, which means no index.
		 */
		modrm   |= ENC_RM(0x04);
		sib      = ENC_SIB(0, 0x04, 0x04);
		emitsib  = true;
	} else {
		modrm |= ENC_RM(base_enc);
	}

	/* We are forced to emit an 8bit offset as EBP base without offset is a
	 * special case for SIB without base register. */
	if (base_enc == 0x05 && emitoffs == 0) {
		modrm    |= MOD_IND_BYTE_OFS;
		emitoffs  = 8;
	}

	modrm |= ENC_REG(reg);

	bemit8(modrm);
	if (emitsib)
		bemit8(sib);

	/* emit displacement */
	if (emitoffs == 8) {
		bemit8((unsigned) offset);
	} else if (emitoffs == 32) {
		bemit_entity(&attr->am_imm, false);
	}
}

/**
 * Emit an unop.
 */
static void bemit_unop(const ir_node *node, unsigned char code, unsigned char ext, int input)
{
	bemit8(code);
	if (get_ia32_op_type(node) == ia32_Normal) {
		const arch_register_t *in = arch_get_irn_register_in(node, input);
		bemit_modru(in, ext);
	} else {
		bemit_mod_am(ext, node);
	}
}

static void bemit_unop_reg(const ir_node *node, unsigned char code, int input)
{
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_res);
	bemit_unop(node, code, out->encoding, input);
}

static void bemit_unop_mem(const ir_node *node, unsigned char code, unsigned char ext)
{
	unsigned size = get_mode_size_bits(get_ia32_ls_mode(node));
	if (size == 16)
		bemit8(0x66);
	bemit8(size == 8 ? code : code | OP_16_32);
	bemit_mod_am(ext, node);
}

static void bemit_0f_unop_reg(ir_node const *const node, unsigned char const code, int const input)
{
	bemit8(0x0F);
	bemit_unop_reg(node, code, input);
}

static void bemit_imm32(ir_node const *const node, bool const relative)
{
	const ia32_immediate_attr_t *attr = get_ia32_immediate_attr_const(node);
	bemit_entity(&attr->imm, relative);
}

static void bemit_imm(ia32_immediate_attr_t const *const attr,
                      unsigned const size)
{
	switch (size) {
	case  8: bemit8(attr->imm.offset);  break;
	case 16: bemit16(attr->imm.offset); break;
	case 32: bemit_entity(&attr->imm, false); break;
	}
}

static void bemit_mov(arch_register_t const *const src, arch_register_t const *const dst)
{
	bemit8(0x88 | OP_MEM_SRC | OP_16_32); // movl %src, %dst
	bemit_modrr(src, dst);
}

static void bemit_xchg(arch_register_t const *const src, arch_register_t const *const dst)
{
	if (src->index == REG_GP_EAX) {
		bemit8(0x90 + dst->encoding); // xchgl %eax, %dst
	} else if (dst->index == REG_GP_EAX) {
		bemit8(0x90 + src->encoding); // xchgl %src, %eax
	} else {
		bemit8(0x86 | OP_16_32); // xchgl %src, %dst
		bemit_modrr(src, dst);
	}
}

static void bemit_copy(const ir_node *copy)
{
	const arch_register_t *in  = arch_get_irn_register_in(copy, 0);
	const arch_register_t *out = arch_get_irn_register_out(copy, 0);

	if (in == out)
		return;
	/* copies of fp nodes aren't real... */
	if (in->cls == &ia32_reg_classes[CLASS_ia32_fp])
		return;

	assert(in->cls == &ia32_reg_classes[CLASS_ia32_gp]);
	bemit_mov(in, out);
}

static void bemit_perm(const ir_node *node)
{
	arch_register_t       const *const reg0 = arch_get_irn_register_out(node, 0);
	arch_register_t       const *const reg1 = arch_get_irn_register_out(node, 1);
	arch_register_class_t const *const cls  = reg0->cls;

	assert(cls == reg1->cls && "Register class mismatch at Perm");

	if (cls == &ia32_reg_classes[CLASS_ia32_gp]) {
		bemit_xchg(reg0, reg1);
	} else if (cls == &ia32_reg_classes[CLASS_ia32_xmm]) {
		panic("unimplemented"); // TODO implement
		//ia32_emitf(NULL, "xorpd %R, %R", reg1, reg0);
		//ia32_emitf(NULL, "xorpd %R, %R", reg0, reg1);
		//ia32_emitf(node, "xorpd %R, %R", reg1, reg0);
	} else if (cls == &ia32_reg_classes[CLASS_ia32_fp]) {
		/* is a NOP */
	} else {
		panic("unexpected register class in be_Perm (%+F)", node);
	}
}

static void bemit_xor0(const ir_node *node)
{
	bemit8(0x31);
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_Xor0_res);
	bemit_modrr(out, out);
}

static void bemit_mov_const(const ir_node *node)
{
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_Const_res);
	bemit8(0xB8 + out->encoding);
	bemit_imm32(node, false);
}

static bool use_eax_short_form(ir_node const *const node)
{
	return
		get_ia32_op_type(node) == ia32_Normal &&
		arch_get_irn_register_in(node, n_ia32_binary_left)->index == REG_GP_EAX;
}

static void bemit_binop_reg(ir_node const *const node, unsigned char const code, ir_node const *const right)
{
	bemit8(code);
	arch_register_t const *const dst = arch_get_irn_register_in(node, n_ia32_binary_left);
	if (get_ia32_op_type(node) == ia32_Normal) {
		arch_register_t const *const src = arch_get_irn_register(right);
		bemit_modrr(src, dst);
	} else {
		bemit_mod_am(dst->encoding, node);
	}
}

/**
 * Emit a binop.
 */
static void bemit_binop(ir_node const *const node, unsigned const code)
{
	ir_mode *const ls_mode = get_ia32_ls_mode(node);
	unsigned       size    = ls_mode ? get_mode_size_bits(ls_mode) : 32;
	if (size == 16)
		bemit8(0x66);

	unsigned       op    = size == 8 ? OP_8 : OP_16_32;
	ir_node *const right = get_irn_n(node, n_ia32_binary_right);
	if (is_ia32_Immediate(right)) {
		ia32_immediate_attr_t const *const attr = get_ia32_immediate_attr_const(right);
		/* Try to use the short form with 8bit sign extended immediate. */
		if (op != OP_8 && ia32_is_8bit_imm(attr)) {
			op   = OP_16_32_IMM8;
			size = 8;
		}

		/* Emit the main opcode. */
		if (op != OP_16_32_IMM8 && use_eax_short_form(node)) {
			bemit8(code << 3 | OP_EAX | op);
		} else {
			bemit_unop(node, 0x80 | op, code, n_ia32_binary_left);
		}

		bemit_imm(attr, size);
	} else {
		bemit_binop_reg(node, code << 3 | OP_MEM_SRC | op, right);
	}
}

/**
 * Create a function for a binop.
 */
#define BINOP(op, code) \
	static void bemit_##op(ir_node const *const node) \
	{ \
		bemit_binop(node, code); \
	}

/*    insn opcode */
BINOP(add, 0)
BINOP(or,  1)
BINOP(adc, 2)
BINOP(sbb, 3)
BINOP(and, 4)
BINOP(sub, 5)
BINOP(xor, 6)
BINOP(cmp, 7)

static void bemit_binop_mem(ir_node const *const node, unsigned const code)
{
	unsigned size = get_mode_size_bits(get_ia32_ls_mode(node));
	if (size == 16)
		bemit8(0x66);

	unsigned       op  = size == 8 ? OP_8 : OP_16_32;
	ir_node *const val = get_irn_n(node, n_ia32_unary_op);
	if (is_ia32_Immediate(val)) {
		ia32_immediate_attr_t const *const attr = get_ia32_immediate_attr_const(val);
		/* Try to use the short form with 8bit sign extended immediate. */
		if (op != OP_8 && ia32_is_8bit_imm(attr)) {
			op   = OP_16_32_IMM8;
			size = 8;
		}

		/* Emit the main opcode. */
		bemit8(0x80 | op);
		bemit_mod_am(code, node);

		bemit_imm(attr, size);
	} else {
		bemit8(code << 3 | op);
		bemit_mod_am(arch_get_irn_register(val)->encoding, node);
	}
}

#define BINOPMEM(op, code) \
	static void bemit_##op(ir_node const *const node) \
	{ \
		bemit_binop_mem(node, code); \
	}

BINOPMEM(addmem,  0)
BINOPMEM(ormem,   1)
BINOPMEM(andmem,  4)
BINOPMEM(submem,  5)
BINOPMEM(xormem,  6)


/**
 * Creates a function for an Unop with code /ext encoding.
 */
#define UNOP(op, code, ext, input)              \
static void bemit_ ## op(const ir_node *node) { \
	bemit_unop(node, code, ext, input);         \
}

UNOP(not,     0xF7, 2, n_ia32_Not_val)
UNOP(neg,     0xF7, 3, n_ia32_Neg_val)
UNOP(mul,     0xF7, 4, n_ia32_Mul_right)
UNOP(imul1op, 0xF7, 5, n_ia32_IMul1OP_right)
UNOP(div,     0xF7, 6, n_ia32_Div_divisor)
UNOP(idiv,    0xF7, 7, n_ia32_IDiv_divisor)

/* TODO: am support for IJmp */
UNOP(ijmp,    0xFF, 4, n_ia32_IJmp_target)

#define SHIFT(op, ext) \
static void bemit_##op(const ir_node *node) \
{ \
	arch_register_t const *const out   = arch_get_irn_register_out(node, pn_ia32_res); \
	ir_node               *const count = get_irn_n(node, 1); \
	if (is_ia32_Immediate(count)) { \
		int32_t offset = get_ia32_immediate_attr_const(count)->imm.offset; \
		if (offset == 1) { \
			bemit8(0xD1); \
			bemit_modru(out, ext); \
		} else { \
			bemit8(0xC1); \
			bemit_modru(out, ext); \
			bemit8(offset); \
		} \
	} else { \
		bemit8(0xD3); \
		bemit_modru(out, ext); \
	} \
} \
 \
static void bemit_##op##mem(const ir_node *node) \
{ \
	ir_node *count; \
	unsigned size = get_mode_size_bits(get_ia32_ls_mode(node)); \
	if (size == 16) \
		bemit8(0x66); \
	count = get_irn_n(node, 1); \
	if (is_ia32_Immediate(count)) { \
		int32_t offset = get_ia32_immediate_attr_const(count)->imm.offset; \
		if (offset == 1) { \
			bemit8(size == 8 ? 0xD0 : 0xD1); \
			bemit_mod_am(ext, node); \
		} else { \
			bemit8(size == 8 ? 0xC0 : 0xC1); \
			bemit_mod_am(ext, node); \
			bemit8(offset); \
		} \
	} else { \
		bemit8(size == 8 ? 0xD2 : 0xD3); \
		bemit_mod_am(ext, node); \
	} \
}

SHIFT(rol, 0)
SHIFT(ror, 1)
SHIFT(shl, 4)
SHIFT(shr, 5)
SHIFT(sar, 7)

static void bemit_shld(const ir_node *node)
{
	const arch_register_t *in  = arch_get_irn_register_in(node, n_ia32_ShlD_val_low);
	const arch_register_t *out = arch_get_irn_register_out(node, pn_ia32_ShlD_res);
	ir_node *count = get_irn_n(node, n_ia32_ShlD_count);
	bemit8(0x0F);
	if (is_ia32_Immediate(count)) {
		bemit8(0xA4);
		bemit_modrr(out, in);
		bemit8(get_ia32_immediate_attr_const(count)->imm.offset);
	} else {
		bemit8(0xA5);
		bemit_modrr(out, in);
	}
}

static void bemit_shrd(const ir_node *node)
{
	const arch_register_t *in  = arch_get_irn_register_in(node, n_ia32_ShrD_val_low);
	const arch_register_t *out = arch_get_irn_register_out(node, pn_ia32_ShrD_res);
	ir_node *count = get_irn_n(node, n_ia32_ShrD_count);
	bemit8(0x0F);
	if (is_ia32_Immediate(count)) {
		bemit8(0xAC);
		bemit_modrr(out, in);
		bemit8(get_ia32_immediate_attr_const(count)->imm.offset);
	} else {
		bemit8(0xAD);
		bemit_modrr(out, in);
	}
}

static void bemit_sbb0(ir_node const *const node)
{
	bemit8(0x1B);
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_Sbb0_res);
	bemit_modrr(out, out);
}

/**
 * binary emitter for setcc.
 */
static void bemit_setcc(const ir_node *node)
{
	const arch_register_t *dreg = arch_get_irn_register_out(node, pn_ia32_Setcc_res);

	x86_condition_code_t const cc = determine_final_cc(node, n_ia32_Setcc_eflags);
	if (cc & x86_cc_float_parity_cases) {
		if (cc & x86_cc_negated) {
			/* set%PNC <dreg */
			bemit8(0x0F);
			bemit8(0x90 | pnc2cc(cc));
			bemit_modrm8(REG_LOW, dreg);

			/* setp >dreg */
			bemit8(0x0F);
			bemit8(0x9A);
			bemit_modrm8(REG_HIGH, dreg);

			/* orb %>dreg, %<dreg */
			bemit8(0x08);
			bemit_modrr8(REG_LOW, dreg, REG_HIGH, dreg);
		} else {
			 /* set%PNC <dreg */
			bemit8(0x0F);
			bemit8(0x90 | pnc2cc(cc));
			bemit_modrm8(REG_LOW, dreg);

			/* setnp >dreg */
			bemit8(0x0F);
			bemit8(0x9B);
			bemit_modrm8(REG_HIGH, dreg);

			/* andb %>dreg, %<dreg */
			bemit8(0x20);
			bemit_modrr8(REG_LOW, dreg, REG_HIGH, dreg);
		}
	} else {
		/* set%PNC <dreg */
		bemit8(0x0F);
		bemit8(0x90 | pnc2cc(cc));
		bemit_modrm8(REG_LOW, dreg);
	}
}

static void bemit_bsf(ir_node const *const node)
{
	bemit_0f_unop_reg(node, 0xBC, n_ia32_Bsf_operand);
}

static void bemit_bsr(ir_node const *const node)
{
	bemit_0f_unop_reg(node, 0xBD, n_ia32_Bsr_operand);
}

static void bemit_bswap(ir_node const *const node)
{
	bemit8(0x0F);
	bemit_modru(arch_get_irn_register_out(node, pn_ia32_Bswap_res), 1);
}

static void bemit_bt(ir_node const *const node)
{
	bemit8(0x0F);
	arch_register_t const *const lreg  = arch_get_irn_register_in(node, n_ia32_Bt_left);
	ir_node         const *const right = get_irn_n(node, n_ia32_Bt_right);
	if (is_ia32_Immediate(right)) {
		ia32_immediate_attr_t const *const attr = get_ia32_immediate_attr_const(right);
		assert(ia32_is_8bit_imm(attr));
		bemit8(0xBA);
		bemit_modru(lreg, 4);
		bemit8(attr->imm.offset);
	} else {
		bemit8(0xA3);
		bemit_modrr(lreg, arch_get_irn_register(right));
	}
}

static void bemit_cmovcc(const ir_node *node)
{
	bemit8(0x0F);
	ir_node       const *const val_true = get_irn_n(node, n_ia32_CMovcc_val_true);
	x86_condition_code_t const cc       = determine_final_cc(node, n_ia32_CMovcc_eflags);
	if (cc & x86_cc_float_parity_cases)
		panic("cmov can't handle parity float cases");
	bemit_binop_reg(node, 0x40 | pnc2cc(cc), val_true);
}

static void bemit_test(ir_node const *const node)
{
	unsigned const size = get_mode_size_bits(get_ia32_ls_mode(node));
	if (size == 16)
		bemit8(0x66);

	unsigned const op    = size == 8 ? OP_8 : OP_16_32;
	ir_node *const right = get_irn_n(node, n_ia32_Test_right);
	if (is_ia32_Immediate(right)) {
		/* Emit the main opcode. */
		if (use_eax_short_form(node)) {
			bemit8(0xA8 | op);
		} else {
			bemit_unop(node, 0xF6, 0, n_ia32_Test_left);
		}

		bemit_imm(get_ia32_immediate_attr_const(right), size);
	} else {
		bemit_binop_reg(node, 0x84 | op, right);
	}
}

static void bemit_imul(const ir_node *node)
{
	bemit_0f_unop_reg(node, 0xAF, n_ia32_IMul_right);
}

static void bemit_imulimm(const ir_node *node)
{
	ir_node               const *const right = get_irn_n(node, n_ia32_IMul_right);
	ia32_immediate_attr_t const *const attr  = get_ia32_immediate_attr_const(right);
	bool                         const imm8  = ia32_is_8bit_imm(attr);
	bemit_unop_reg(node, 0x69 | (imm8 ? OP_IMM8 : 0), n_ia32_IMul_left);
	bemit_imm(attr, imm8 ? 8 : 32);
}

static void bemit_dec(const ir_node *node)
{
	const arch_register_t *out = arch_get_irn_register_out(node, pn_ia32_Dec_res);
	bemit8(0x48 + out->encoding);
}

static void bemit_inc(const ir_node *node)
{
	const arch_register_t *out = arch_get_irn_register_out(node, pn_ia32_Inc_res);
	bemit8(0x40 + out->encoding);
}

#define UNOPMEM(op, code, ext) \
static void bemit_##op(const ir_node *node) \
{ \
	bemit_unop_mem(node, code, ext); \
}

UNOPMEM(notmem, 0xF6, 2)
UNOPMEM(negmem, 0xF6, 3)
UNOPMEM(incmem, 0xFE, 0)
UNOPMEM(decmem, 0xFE, 1)

static void bemit_ldtls(const ir_node *node)
{
	bemit8(0x65); // gs:
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_LdTls_res);
	if (out->index == REG_GP_EAX) {
		bemit8(0xA1); // movl 0, %eax
	} else {
		bemit8(0x88 | OP_MEM_SRC | OP_16_32); // movl 0, %reg
		bemit8(MOD_IND | ENC_REG(out->encoding) | ENC_RM(0x05));
	}
	bemit32(0);
}

/**
 * Emit a Lea.
 */
static void bemit_lea(const ir_node *node)
{
	bemit8(0x8D);
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_Lea_res);
	bemit_mod_am(out->encoding, node);
}

/* helper function for bemit_minus64bit */
static void bemit_helper_neg(const arch_register_t *reg)
{
	bemit8(0xF7); // negl %reg
	bemit_modru(reg, 3);
}

/* helper function for bemit_minus64bit */
static void bemit_helper_sbb0(const arch_register_t *reg)
{
	bemit8(0x80 | OP_16_32_IMM8); // sbbl $0, %reg
	bemit_modru(reg, 3);
	bemit8(0);
}

/* helper function for bemit_minus64bit */
static void bemit_helper_sbb(const arch_register_t *src, const arch_register_t *dst)
{
	bemit8(0x1B); // sbbl %src, %dst
	bemit_modrr(src, dst);
}

/* helper function for bemit_minus64bit */
static void bemit_helper_zero(const arch_register_t *reg)
{
	bemit8(0x33); // xorl %reg, %reg
	bemit_modrr(reg, reg);
}

static void bemit_minus64(const ir_node *node)
{
	arch_register_t const *const in_lo  = arch_get_irn_register_in(node, n_ia32_Minus64_low);
	arch_register_t const *const in_hi  = arch_get_irn_register_in(node, n_ia32_Minus64_high);
	arch_register_t const *const out_lo = arch_get_irn_register_out(node, pn_ia32_Minus64_res_low);
	arch_register_t const *const out_hi = arch_get_irn_register_out(node, pn_ia32_Minus64_res_high);

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
			bemit_xchg(in_lo, in_hi);
			goto normal_neg;
		} else {
			/* a -> b, b -> d */
			bemit_mov(in_hi, out_hi);
			bemit_mov(in_lo, out_lo);
			goto normal_neg;
		}
	} else {
		if (out_hi == in_lo) {
			/* a -> c, b -> a */
			bemit_mov(in_lo, out_lo);
			goto zero_neg;
		} else if (out_hi == in_hi) {
			/* a -> c, b -> b */
			bemit_mov(in_lo, out_lo);
			goto normal_neg;
		} else {
			/* a -> c, b -> d */
			bemit_mov(in_lo, out_lo);
			goto zero_neg;
		}
	}

normal_neg:
	bemit_helper_neg( out_hi);
	bemit_helper_neg( out_lo);
	bemit_helper_sbb0(out_hi);
	return;

zero_neg:
	bemit_helper_zero(out_hi);
	bemit_helper_neg( out_lo);
	bemit_helper_sbb( in_hi, out_hi);
}

/**
 * Emit a single opcode.
 */
#define EMIT_SINGLEOP(op, code)                 \
static void bemit_ ## op(const ir_node *node) { \
	(void) node;                                \
	bemit8(code);                               \
}

EMIT_SINGLEOP(cwtl,  0x98)
EMIT_SINGLEOP(cltd,  0x99)
EMIT_SINGLEOP(sahf,  0x9E)
EMIT_SINGLEOP(leave, 0xC9)
EMIT_SINGLEOP(int3,  0xCC)
EMIT_SINGLEOP(cmc,   0xF5)
EMIT_SINGLEOP(stc,   0xF9)

/**
 * Emits a MOV out, [MEM].
 */
static void bemit_load(const ir_node *node)
{
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_Load_res);

	if (out->index == REG_GP_EAX) {
		ir_node const *const base = get_irn_n_reg(node, n_ia32_base);
		ir_node const *const idx  = get_irn_n_reg(node, n_ia32_index);
		if (!base && !idx) {
			/* load from constant address to EAX can be encoded
			   as 0xA1 [offset] */
			bemit8(0xA1);
			ia32_attr_t const *const attr = get_ia32_attr_const(node);
			bemit_entity(&attr->am_imm, false);
			return;
		}
	}
	bemit8(0x88 | OP_MEM_SRC | OP_16_32);
	bemit_mod_am(out->encoding, node);
}

/**
 * Emits a MOV [mem], in.
 */
static void bemit_store(const ir_node *node)
{
	const ir_node *value = get_irn_n(node, n_ia32_Store_val);
	unsigned       size  = get_mode_size_bits(get_ia32_ls_mode(node));

	if (is_ia32_Immediate(value)) {
		if (size == 16)
			bemit8(0x66);
		bemit8(0xC6 | (size != 8 ? OP_16_32 : 0));
		bemit_mod_am(0, node);
		bemit_imm(get_ia32_immediate_attr_const(value), size);
	} else {
		arch_register_t const *const in = arch_get_irn_register(value);

		if (in->index == REG_GP_EAX) {
			ir_node const *const base = get_irn_n_reg(node, n_ia32_base);
			ir_node const *const idx  = get_irn_n_reg(node, n_ia32_index);
			if (!base && !idx) {
				/* store to constant address from EAX can be encoded as
				 * 0xA2/0xA3 [offset]*/
				if (size == 8) {
					bemit8(0xA2);
				} else {
					if (size == 16)
						bemit8(0x66);
					bemit8(0xA3);
				}
				ia32_attr_t const *const attr = get_ia32_attr_const(node);
				bemit_entity(&attr->am_imm, false);
				return;
			}
		}

		if (size == 16)
			bemit8(0x66);
		bemit8(0x88 | (size != 8 ? OP_16_32 : 0));
		bemit_mod_am(in->encoding, node);
	}
}

static void bemit_conv_i2i(const ir_node *node)
{
	/*        8 16 bit source
	 * movzx B6 B7
	 * movsx BE BF */
	ir_mode *const smaller_mode = get_ia32_ls_mode(node);
	unsigned       opcode       = 0xB6;
	if (mode_is_signed(smaller_mode))           opcode |= 0x08;
	if (get_mode_size_bits(smaller_mode) == 16) opcode |= 0x01;
	bemit_0f_unop_reg(node, opcode, n_ia32_Conv_I2I_val);
}

static void bemit_popcnt(ir_node const *const node)
{
	bemit8(0xF3);
	bemit_0f_unop_reg(node, 0xB8, n_ia32_Popcnt_operand);
}

/**
 * Emit a Push.
 */
static void bemit_push(const ir_node *node)
{
	ir_node const *const value = get_irn_n_reg(node, n_ia32_Push_val);
	if (!value) {
		bemit8(0xFF);
		bemit_mod_am(6, node);
	} else if (is_ia32_Immediate(value)) {
		ia32_immediate_attr_t const *const attr = get_ia32_immediate_attr_const(value);
		bool                         const imm8 = ia32_is_8bit_imm(attr);
		bemit8(0x68 | (imm8 ? OP_IMM8 : 0));
		bemit_imm(attr, imm8 ? 8 : 32);
	} else {
		arch_register_t const *const reg = arch_get_irn_register(value);
		bemit8(0x50 + reg->encoding);
	}
}

/**
 * Emit a Pop.
 */
static void bemit_pop(const ir_node *node)
{
	arch_register_t const *const reg = arch_get_irn_register_out(node, pn_ia32_Pop_res);
	bemit8(0x58 + reg->encoding);
}

static void bemit_popmem(const ir_node *node)
{
	bemit8(0x8F);
	bemit_mod_am(0, node);
}

static void bemit_call(const ir_node *node)
{
	(void)node;
	panic("bemit_call NIY");
#if 0
	ir_node *proc = get_irn_n(node, n_ia32_Call_addr);

	if (is_ia32_Immediate(proc)) {
		bemit8(0xE8);
		bemit_imm32(proc, true);
	} else {
		bemit_unop(node, 0xFF, 2, n_ia32_Call_addr);
	}
#endif
}

static void bemit_jmp(const ir_node *dest_block)
{
	bemit8(0xE9);
	bemit_jmp_destination(dest_block);
}

static void bemit_jump(const ir_node *node)
{
	ir_node *block  = get_nodes_block(node);
	ir_node *target = get_cfop_target_block(node);
	if (fallthrough_possible(block, target))
		return;

	bemit_jmp(get_cfop_target_block(node));
}

static void bemit_jcc(x86_condition_code_t pnc, const ir_node *dest_block)
{
	unsigned char cc = pnc2cc(pnc);
	bemit8(0x0F);
	bemit8(0x80 + cc);
	bemit_jmp_destination(dest_block);
}

static void bemit_jp(bool odd, const ir_node *dest_block)
{
	bemit8(0x0F);
	bemit8(0x8A + odd);
	bemit_jmp_destination(dest_block);
}

static void bemit_ia32_jcc(const ir_node *node)
{
	x86_condition_code_t cc = determine_final_cc(node, n_ia32_Jcc_eflags);

	/* get both Projs */
	ir_node const *proj_true    = get_Proj_for_pn(node, pn_ia32_Jcc_true);
	ir_node const *proj_false   = get_Proj_for_pn(node, pn_ia32_Jcc_false);
	ir_node const *block        = get_nodes_block(node);
	ir_node const *target_true  = get_cfop_target_block(proj_true);
	if (fallthrough_possible(block, target_true)) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj_true;

		proj_true  = proj_false;
		proj_false = t;
		cc         = x86_negate_condition_code(cc);
	}

	ir_node const *target_false = get_cfop_target_block(proj_false);
	bool const     fallthrough  = fallthrough_possible(block, target_false);
	/* if we can't have a fallthrough anyway, put the more likely case first */
	if (!fallthrough) {
		/* We would need execfreq for the concrete edge, but don't have it
		 * available here, so we use the block execfreq :-( */
		double freq_true  = get_block_execfreq(target_true);
		double freq_false = get_block_execfreq(target_false);
		if (freq_false > freq_true) {
			const ir_node *t = proj_true;
			proj_true  = proj_false;
			proj_false = t;
			cc         = x86_negate_condition_code(cc);
		}
	}

	target_true  = get_cfop_target_block(proj_true);
	target_false = get_cfop_target_block(proj_false);

	if (cc & x86_cc_float_parity_cases) {
		/* Some floating point comparisons require a test of the parity flag,
		 * which indicates that the result is unordered */
		if (cc & x86_cc_negated) {
			bemit_jp(false, target_true);
		} else {
			/* we need a local label if the false proj is a fallthrough
			 * as the falseblock might have no label emitted then */
			if (fallthrough) {
				bemit8(0x7A);
				bemit8(0x06);  // jp + 6
			} else {
				bemit_jp(false, target_false);
			}
		}
	}
	bemit_jcc(cc, target_true);

	/* the second Proj might be a fallthrough */
	if (fallthrough) {
		/* it's a fallthrough */
	} else {
		bemit_jmp(target_false);
	}
}

static void bemit_switchjmp(const ir_node *node)
{
	ir_entity             *jump_table = get_ia32_am_ent(node);
	const ir_switch_table *table      = get_ia32_switch_table(node);

	bemit8(0xFF); // jmp *tbl.label(,%in,4)
	bemit_mod_am(0x05, node);

	be_emit_jump_table(node, table, jump_table, get_cfop_target_block);
}

static void bemit_return(const ir_node *node)
{
	const ia32_return_attr_t *attr = get_ia32_return_attr_const(node);
	unsigned pop = attr->pop;
	if (attr->emit_pop || pop > 0) {
		bemit8(0xC2);
		bemit16(pop);
	} else {
		bemit8(0xC3);
	}
}

static void bemit_subsp(const ir_node *node)
{
	/* sub %in, %esp */
	bemit_sub(node);
	/* mov %esp, %out */
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_SubSP_addr);
	bemit_mov(&ia32_registers[REG_ESP], out);
}

static void bemit_incsp(const ir_node *node)
{
	int offs = be_get_IncSP_offset(node);
	if (offs == 0)
		return;

	unsigned ext;
	if (offs > 0) {
		ext = 5; /* sub */
	} else {
		ext = 0; /* add */
		offs = -offs;
	}

	bool const imm8b = ia32_is_8bit_val(offs);
	bemit8(0x80 | OP_16_32 | (imm8b ? OP_IMM8 : 0));

	const arch_register_t *reg  = arch_get_irn_register_out(node, 0);
	bemit_modru(reg, ext);

	if (imm8b) {
		bemit8(offs);
	} else {
		bemit32(offs);
	}
}

static void bemit_copybi(const ir_node *node)
{
	unsigned size = get_ia32_copyb_size(node);
	if (size & 1)
		bemit8(0xA4); // movsb
	if (size & 2) {
		bemit8(0x66);
		bemit8(0xA5); // movsw
	}
	size >>= 2;
	while (size--) {
		bemit8(0xA5); // movsl
	}
}

static void bemit_fbinop(ir_node const *const node, unsigned const op_fwd, unsigned const op_rev)
{
	ia32_x87_attr_t const *const attr = get_ia32_x87_attr_const(node);
	unsigned               const op   = attr->attr.ins_permuted ? op_rev : op_fwd;
	if (get_ia32_op_type(node) == ia32_Normal) {
		assert(!attr->pop || attr->res_in_reg);

		unsigned char op0 = 0xD8;
		if (attr->res_in_reg) op0 |= 0x04;
		if (attr->pop)        op0 |= 0x02;
		bemit8(op0);

		bemit_modru(attr->reg, op);
	} else {
		assert(!attr->reg);
		assert(!attr->pop);

		unsigned const size = get_mode_size_bits(get_ia32_ls_mode(node));
		bemit8(size == 32 ? 0xD8 : 0xDC);
		bemit_mod_am(op, node);
	}
}

static void bemit_fop_reg(ir_node const *const node, unsigned char const op0, unsigned char const op1)
{
	bemit8(op0);
	bemit8(op1 + get_ia32_x87_attr_const(node)->reg->encoding);
}

static void bemit_fabs(const ir_node *node)
{
	(void)node;
	bemit8(0xD9);
	bemit8(0xE1);
}

static void bemit_fadd(const ir_node *node)
{
	bemit_fbinop(node, 0, 0);
}

static void bemit_fchs(const ir_node *node)
{
	(void)node;
	bemit8(0xD9);
	bemit8(0xE0);
}

static void bemit_fdiv(const ir_node *node)
{
	bemit_fbinop(node, 6, 7);
}

static void bemit_ffreep(ir_node const *const node)
{
	bemit_fop_reg(node, 0xDF, 0xC0);
}

static void bemit_fild(const ir_node *node)
{
	switch (get_mode_size_bits(get_ia32_ls_mode(node))) {
	case 16:
		bemit8(0xDF); // filds
		bemit_mod_am(0, node);
		return;

	case 32:
		bemit8(0xDB); // fildl
		bemit_mod_am(0, node);
		return;

	case 64:
		bemit8(0xDF); // fildll
		bemit_mod_am(5, node);
		return;

	default:
		panic("invalid mode size");
	}
}

static void bemit_fist(const ir_node *node)
{
	unsigned       op;
	unsigned const size = get_mode_size_bits(get_ia32_ls_mode(node));
	switch (size) {
	case 16: bemit8(0xDF); op = 2; break; // fist[p]s
	case 32: bemit8(0xDB); op = 2; break; // fist[p]l
	case 64: bemit8(0xDF); op = 6; break; // fistpll
	default: panic("invalid mode size");
	}
	if (get_ia32_x87_attr_const(node)->pop)
		++op;
	// There is only a pop variant for 64 bit integer store.
	assert(size < 64 || get_ia32_x87_attr_const(node)->pop);
	bemit_mod_am(op, node);
}

static void bemit_fisttp(ir_node const *const node)
{
	switch (get_mode_size_bits(get_ia32_ls_mode(node))) {
	case 16: bemit8(0xDF); break; // fisttps
	case 32: bemit8(0xDB); break; // fisttpl
	case 64: bemit8(0xDD); break; // fisttpll
	default: panic("invalid mode size");
	}
	bemit_mod_am(1, node);
}

static void bemit_fld(const ir_node *node)
{
	switch (get_mode_size_bits(get_ia32_ls_mode(node))) {
	case 32:
		bemit8(0xD9); // flds
		bemit_mod_am(0, node);
		return;

	case 64:
		bemit8(0xDD); // fldl
		bemit_mod_am(0, node);
		return;

	case 80:
	case 96:
		bemit8(0xDB); // fldt
		bemit_mod_am(5, node);
		return;

	default:
		panic("invalid mode size");
	}
}

static void bemit_fld1(const ir_node *node)
{
	(void)node;
	bemit8(0xD9);
	bemit8(0xE8); // fld1
}

static void bemit_fldcw(const ir_node *node)
{
	bemit8(0xD9); // fldcw
	bemit_mod_am(5, node);
}

static void bemit_fldz(const ir_node *node)
{
	(void)node;
	bemit8(0xD9);
	bemit8(0xEE); // fldz
}

static void bemit_fmul(const ir_node *node)
{
	bemit_fbinop(node, 1, 1);
}

static void bemit_fpop(const ir_node *node)
{
	bemit_fop_reg(node, 0xDD, 0xD8);
}

static void bemit_fdup(const ir_node *node)
{
	bemit_fop_reg(node, 0xD9, 0xC0);
}

static void bemit_fst(const ir_node *node)
{
	unsigned       op;
	unsigned const size = get_mode_size_bits(get_ia32_ls_mode(node));
	switch (size) {
	case 32: bemit8(0xD9); op = 2; break; // fst[p]s
	case 64: bemit8(0xDD); op = 2; break; // fst[p]l
	case 80:
	case 96: bemit8(0xDB); op = 6; break; // fstpt
	default: panic("invalid mode size");
	}
	if (get_ia32_x87_attr_const(node)->pop)
		++op;
	// There is only a pop variant for long double store.
	assert(size < 80 || get_ia32_x87_attr_const(node)->pop);
	bemit_mod_am(op, node);
}

static void bemit_fsub(const ir_node *node)
{
	bemit_fbinop(node, 4, 5);
}

static void bemit_fnstcw(const ir_node *node)
{
	bemit8(0xD9); // fnstcw
	bemit_mod_am(7, node);
}

static void bemit_fnstsw(void)
{
	bemit8(0xDF); // fnstsw %ax
	bemit8(0xE0);
}

static void bemit_ftstfnstsw(const ir_node *node)
{
	(void)node;
	bemit8(0xD9); // ftst
	bemit8(0xE4);
	bemit_fnstsw();
}

static void bemit_fucomi(const ir_node *node)
{
	const ia32_x87_attr_t *attr = get_ia32_x87_attr_const(node);
	bemit8(attr->pop ? 0xDF : 0xDB); // fucom[p]i
	bemit8(0xE8 + attr->reg->encoding);
}

static void bemit_fucomfnstsw(const ir_node *node)
{
	const ia32_x87_attr_t *attr = get_ia32_x87_attr_const(node);
	bemit8(0xDD); // fucom[p]
	bemit8((attr->pop ? 0xE8 : 0xE0) + attr->reg->encoding);
	bemit_fnstsw();
}

static void bemit_fucomppfnstsw(const ir_node *node)
{
	(void)node;
	bemit8(0xDA); // fucompp
	bemit8(0xE9);
	bemit_fnstsw();
}

static void bemit_fxch(const ir_node *node)
{
	bemit_fop_reg(node, 0xD9, 0xC8);
}

static void ia32_register_binary_emitters(void)
{
	/* first clear the generic function pointer for all ops */
	ir_clear_opcodes_generic_func();

	/* benode emitter */
	be_set_emitter(op_be_Asm,             emit_ia32_Asm); // TODO implement binary emitter
	be_set_emitter(op_be_Copy,            bemit_copy);
	be_set_emitter(op_be_CopyKeep,        bemit_copy);
	be_set_emitter(op_be_IncSP,           bemit_incsp);
	be_set_emitter(op_be_Perm,            bemit_perm);
	be_set_emitter(op_ia32_Return,        bemit_return);
	be_set_emitter(op_ia32_Adc,           bemit_adc);
	be_set_emitter(op_ia32_Add,           bemit_add);
	be_set_emitter(op_ia32_AddMem,        bemit_addmem);
	be_set_emitter(op_ia32_And,           bemit_and);
	be_set_emitter(op_ia32_AndMem,        bemit_andmem);
	be_set_emitter(op_ia32_Breakpoint,    bemit_int3);
	be_set_emitter(op_ia32_Bsf,           bemit_bsf);
	be_set_emitter(op_ia32_Bsr,           bemit_bsr);
	be_set_emitter(op_ia32_Bswap,         bemit_bswap);
	be_set_emitter(op_ia32_Bt,            bemit_bt);
	be_set_emitter(op_ia32_CMovcc,        bemit_cmovcc);
	be_set_emitter(op_ia32_Call,          bemit_call);
	be_set_emitter(op_ia32_Cltd,          bemit_cltd);
	be_set_emitter(op_ia32_Cmc,           bemit_cmc);
	be_set_emitter(op_ia32_Cmp,           bemit_cmp);
	be_set_emitter(op_ia32_Const,         bemit_mov_const);
	be_set_emitter(op_ia32_Conv_I2I,      bemit_conv_i2i);
	be_set_emitter(op_ia32_CopyB_i,       bemit_copybi);
	be_set_emitter(op_ia32_Cwtl,          bemit_cwtl);
	be_set_emitter(op_ia32_Dec,           bemit_dec);
	be_set_emitter(op_ia32_DecMem,        bemit_decmem);
	be_set_emitter(op_ia32_Div,           bemit_div);
	be_set_emitter(op_ia32_FldCW,         bemit_fldcw);
	be_set_emitter(op_ia32_FnstCW,        bemit_fnstcw);
	be_set_emitter(op_ia32_FtstFnstsw,    bemit_ftstfnstsw);
	be_set_emitter(op_ia32_FucomFnstsw,   bemit_fucomfnstsw);
	be_set_emitter(op_ia32_Fucomi,        bemit_fucomi);
	be_set_emitter(op_ia32_FucomppFnstsw, bemit_fucomppfnstsw);
	be_set_emitter(op_ia32_IDiv,          bemit_idiv);
	be_set_emitter(op_ia32_IJmp,          bemit_ijmp);
	be_set_emitter(op_ia32_IMul,          bemit_imul);
	be_set_emitter(op_ia32_IMul1OP,       bemit_imul1op);
	be_set_emitter(op_ia32_IMulImm,       bemit_imulimm);
	be_set_emitter(op_ia32_Inc,           bemit_inc);
	be_set_emitter(op_ia32_IncMem,        bemit_incmem);
	be_set_emitter(op_ia32_Jcc,           bemit_ia32_jcc);
	be_set_emitter(op_ia32_Jmp,           bemit_jump);
	be_set_emitter(op_ia32_LdTls,         bemit_ldtls);
	be_set_emitter(op_ia32_Lea,           bemit_lea);
	be_set_emitter(op_ia32_Leave,         bemit_leave);
	be_set_emitter(op_ia32_Load,          bemit_load);
	be_set_emitter(op_ia32_Minus64,       bemit_minus64);
	be_set_emitter(op_ia32_Mul,           bemit_mul);
	be_set_emitter(op_ia32_Neg,           bemit_neg);
	be_set_emitter(op_ia32_NegMem,        bemit_negmem);
	be_set_emitter(op_ia32_Not,           bemit_not);
	be_set_emitter(op_ia32_NotMem,        bemit_notmem);
	be_set_emitter(op_ia32_Or,            bemit_or);
	be_set_emitter(op_ia32_OrMem,         bemit_ormem);
	be_set_emitter(op_ia32_Pop,           bemit_pop);
	be_set_emitter(op_ia32_PopMem,        bemit_popmem);
	be_set_emitter(op_ia32_Popcnt,        bemit_popcnt);
	be_set_emitter(op_ia32_Push,          bemit_push);
	be_set_emitter(op_ia32_Rol,           bemit_rol);
	be_set_emitter(op_ia32_RolMem,        bemit_rolmem);
	be_set_emitter(op_ia32_Ror,           bemit_ror);
	be_set_emitter(op_ia32_RorMem,        bemit_rormem);
	be_set_emitter(op_ia32_Sahf,          bemit_sahf);
	be_set_emitter(op_ia32_Sar,           bemit_sar);
	be_set_emitter(op_ia32_SarMem,        bemit_sarmem);
	be_set_emitter(op_ia32_Sbb,           bemit_sbb);
	be_set_emitter(op_ia32_Sbb0,          bemit_sbb0);
	be_set_emitter(op_ia32_Setcc,         bemit_setcc);
	be_set_emitter(op_ia32_Shl,           bemit_shl);
	be_set_emitter(op_ia32_ShlD,          bemit_shld);
	be_set_emitter(op_ia32_ShlMem,        bemit_shlmem);
	be_set_emitter(op_ia32_Shr,           bemit_shr);
	be_set_emitter(op_ia32_ShrD,          bemit_shrd);
	be_set_emitter(op_ia32_ShrMem,        bemit_shrmem);
	be_set_emitter(op_ia32_Stc,           bemit_stc);
	be_set_emitter(op_ia32_Store,         bemit_store);
	be_set_emitter(op_ia32_Sub,           bemit_sub);
	be_set_emitter(op_ia32_SubMem,        bemit_submem);
	be_set_emitter(op_ia32_SubSP,         bemit_subsp);
	be_set_emitter(op_ia32_SwitchJmp,     bemit_switchjmp);
	be_set_emitter(op_ia32_Test,          bemit_test);
	be_set_emitter(op_ia32_Xor,           bemit_xor);
	be_set_emitter(op_ia32_Xor0,          bemit_xor0);
	be_set_emitter(op_ia32_XorMem,        bemit_xormem);
	be_set_emitter(op_ia32_fabs,          bemit_fabs);
	be_set_emitter(op_ia32_fadd,          bemit_fadd);
	be_set_emitter(op_ia32_fchs,          bemit_fchs);
	be_set_emitter(op_ia32_fdiv,          bemit_fdiv);
	be_set_emitter(op_ia32_ffreep,        bemit_ffreep);
	be_set_emitter(op_ia32_fild,          bemit_fild);
	be_set_emitter(op_ia32_fist,          bemit_fist);
	be_set_emitter(op_ia32_fisttp,        bemit_fisttp);
	be_set_emitter(op_ia32_fld,           bemit_fld);
	be_set_emitter(op_ia32_fld1,          bemit_fld1);
	be_set_emitter(op_ia32_fldz,          bemit_fldz);
	be_set_emitter(op_ia32_fmul,          bemit_fmul);
	be_set_emitter(op_ia32_fpop,          bemit_fpop);
	be_set_emitter(op_ia32_fdup,          bemit_fdup);
	be_set_emitter(op_ia32_fst,           bemit_fst);
	be_set_emitter(op_ia32_fsub,          bemit_fsub);
	be_set_emitter(op_ia32_fxch,          bemit_fxch);
}

static void gen_binary_block(ir_node *block)
{
	ia32_emit_block_header(block);

	/* emit the contents of the block */
	sched_foreach(block, node) {
		ia32_emit_node(node);
	}
}

static void ia32_emit_function_binary(ir_graph *const irg, ir_node **const blk_sched)
{
	ir_entity *entity = get_irg_entity(irg);

	ia32_register_binary_emitters();

	parameter_dbg_info_t *infos = construct_parameter_infos(irg);
	be_gas_emit_function_prolog(entity, ia32_cg_config.function_alignment,
	                            NULL);
	free(infos);

	/* we use links to point to target blocks */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_block_walk_graph(irg, ia32_gen_labels, NULL, NULL);

	/* initialize next block links */
	size_t n = ARR_LEN(blk_sched);
	for (size_t i = 0; i < n; ++i) {
		ir_node *block = blk_sched[i];
		ir_node *prev  = i > 0 ? blk_sched[i-1] : NULL;

		set_irn_link(block, prev);
	}
	for (size_t i = 0; i < n; ++i) {
		ir_node *block = blk_sched[i];
		gen_binary_block(block);
	}

	be_gas_emit_function_epilog(entity);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
}

void ia32_emit_function(ir_graph *const irg)
{
	ir_node **const blk_sched = be_create_block_schedule(irg);

	/* emit the code */
	if (ia32_cg_config.emit_machcode) {
		ia32_emit_function_binary(irg, blk_sched);
	} else {
		ia32_emit_function_text(irg, blk_sched);
	}
}

void ia32_init_emitter(void)
{
	lc_opt_entry_t *be_grp   = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ia32_grp = lc_opt_get_grp(be_grp, "ia32");

	lc_opt_add_table(ia32_grp, ia32_emitter_options);

	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.emitter");
}
