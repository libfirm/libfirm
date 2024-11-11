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
#include "ia32_emitter.h"

#include "beasm.h"
#include "beblocksched.h"
#include "bediagnostic.h"
#include "beemithlp.h"
#include "beemitter.h"
#include "begnuas.h"
#include "bejit.h"
#include "besched.h"
#include "bestack.h"
#include "beutil.h"
#include "debug.h"
#include "execfreq.h"
#include "gen_ia32_emitter.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_architecture.h"
#include "ia32_bearch_t.h"
#include "ia32_encode.h"
#include "ia32_new_nodes.h"
#include "irgwalk.h"
#include "irnodehashmap.h"
#include "irtools.h"
#include "lc_opts.h"
#include "lc_opts_enum.h"
#include "panic.h"
#include "platform_t.h"
#include <inttypes.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static char       pic_base_label[128];
static ir_label_t exc_label_id;
static bool       mark_spill_reload;

static bool       omit_fp;
static int        frame_type_size;
static int        callframe_offset;
static ir_entity *thunks[N_ia32_gp_REGS];
static ir_type   *thunk_type;

typedef enum get_ip_style_t {
	IA32_GET_IP_POP,
	IA32_GET_IP_THUNK,
} get_ip_style_t;

static int get_ip_style = IA32_GET_IP_THUNK;

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

static const char *get_register_name_size(arch_register_t const *const reg,
                                          x86_insn_size_t const size,
                                          bool use_8bit_high)
{
	switch (size) {
	case X86_SIZE_8:
		return use_8bit_high ? get_register_name_8bit_high(reg)
		                     : get_register_name_8bit_low(reg);
	case X86_SIZE_16: return get_register_name_16bit(reg);
	case X86_SIZE_32: return reg->name;
	case X86_SIZE_64:
	case X86_SIZE_80:
	case X86_SIZE_128:
		break;
	}
	panic("Unexpected size");
}

/**
 * emit a register, possible shortened by a mode
 */
static void emit_register(arch_register_t const *const reg,
                          x86_insn_size_t const size, bool const use_8bit_high)
{
	const char *name = get_register_name_size(reg, size, use_8bit_high);
	be_emit_char('%');
	be_emit_string(name);
}

static void emit_ia32_immediate_attr(bool const prefix,
                                     ir_node const *const node)
{
	ia32_immediate_attr_t const *const attr
		= get_ia32_immediate_attr_const(node);
	if (prefix)
		be_emit_char('$');
	x86_emit_imm32(&attr->imm);
}

static void ia32_emit_mode_suffix(x86_insn_size_t const size)
{
	switch (size) {
	case X86_SIZE_8:  be_emit_char('b');     return;
	case X86_SIZE_16: be_emit_char('w');     return;
	case X86_SIZE_32: be_emit_char('l');     return;
	/* gas docu says q is the suffix but gcc, objdump and icc use ll
	 * apparently */
	case X86_SIZE_64: be_emit_cstring("ll"); return;
	case X86_SIZE_80:
	case X86_SIZE_128:
		break;
	}
	panic("Unexpected mode size");
}

static void ia32_emit_x87_mode_suffix(ir_node const *const node)
{
	/* we only need to emit the mode on address mode */
	if (get_ia32_op_type(node) == ia32_Normal)
		return;

	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	switch (attr->size) {
	case X86_SIZE_32: be_emit_char('s'); return;
	case X86_SIZE_64: be_emit_char('l'); return;
	/* long doubles have different sizes due to alignment on different
	 * platforms. */
	case X86_SIZE_80:
	case X86_SIZE_128: be_emit_char('t'); return;
	case X86_SIZE_8:
	case X86_SIZE_16:
		break;
	}
	panic("Unexpected size");
}

static void ia32_emit_x87_mode_suffix_int(ir_node const *const node)
{
	assert(get_ia32_op_type(node) != ia32_Normal);
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	switch (attr->size) {
	case X86_SIZE_16: be_emit_char('s');     return;
	case X86_SIZE_32: be_emit_char('l');     return;
	/* gas docu says q is the suffix but gcc, objdump and icc use ll
	 * apparently */
	case X86_SIZE_64: be_emit_cstring("ll"); return;
	case X86_SIZE_8:
	case X86_SIZE_80:
	case X86_SIZE_128:
		break;
	}
	panic("Unexpected size");
}

static char get_xmm_mode_suffix(x86_insn_size_t const size)
{
	switch (size) {
	case X86_SIZE_32: return 's';
	case X86_SIZE_64: return 'd';
	case X86_SIZE_8:
	case X86_SIZE_16:
	case X86_SIZE_80:
	case X86_SIZE_128:
		break;
	}
	panic("invalid XMM mode");
}

static void ia32_emit_xmm_mode_suffix(ir_node const *const node)
{
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	be_emit_char(get_xmm_mode_suffix(attr->size));
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
	EMIT_ALTERNATE_AM = 1U << 0,
	EMIT_LOW_REG      = 1U << 1,
	EMIT_HIGH_REG     = 1U << 2,
	EMIT_16BIT_REG    = 1U << 3,
	EMIT_32BIT_REG    = 1U << 4,
	EMIT_SHIFT_COMMA  = 1U << 5,
} ia32_emit_mod_t;
ENUM_BITSET(ia32_emit_mod_t)

/**
 * Emits address mode.
 */
static void ia32_emit_am(ir_node const *const node)
{
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	x86_emit_addr(node, &attr->addr);
}

void ia32_emitf(ir_node const *const node, char const *fmt, ...)
{
	BE_EMITF(node, fmt, ap, false) {
		ia32_emit_mod_t mod = EMIT_NONE;
		for (;;) {
			switch (*fmt) {
			case '*': mod |= EMIT_ALTERNATE_AM; break;
			case '<': mod |= EMIT_LOW_REG;      break;
			case '>': mod |= EMIT_HIGH_REG;     break;
			case '^': mod |= EMIT_16BIT_REG;    break;
			case '#': mod |= EMIT_32BIT_REG;    break;
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

			case 'A': {
				switch (*fmt++) {
					case 'F':
						if (get_ia32_op_type(node) == ia32_Normal) {
							ia32_x87_attr_t const *const attr = get_ia32_x87_attr_const(node);
							char            const *const fmt  = attr->x87.res_in_reg ? "%%st, %%%s" : "%%%s, %%st";
							be_emit_irprintf(fmt, attr->x87.reg->name);
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
				ia32_attr_t const *const attr = get_ia32_attr_const(node);
				ir_node const *const src = get_irn_n(node, n_ia32_binary_right);
				if (is_ia32_Immediate(src)) {
					emit_ia32_immediate_attr(true, src);
					be_emit_cstring(", ");
					if (attr->tp == ia32_Normal) {
						goto destination_operand;
					} else {
						ia32_emit_am(node);
					}
				} else {
					if (attr->tp == ia32_Normal) {
						reg = arch_get_irn_register(src);
						emit_register(reg, attr->size, attr->use_8bit_high);
					} else {
						ia32_emit_am(node);
					}
					be_emit_cstring(", ");
destination_operand:
					reg = arch_get_irn_register_in(node, n_ia32_binary_left);
					emit_register(reg, attr->size, attr->use_8bit_high);
				}
				break;
			}

			case 'D':
				if (!is_digit(*fmt))
					goto unknown;
				reg = arch_get_irn_register_out(node, *fmt++ - '0');
				goto emit_R;

			case 'E': {
				const ir_entity *const entity = va_arg(ap, const ir_entity*);
				be_gas_emit_entity(entity);
				break;
			}

			case 'F':
				if (*fmt == 'M') {
					ia32_emit_x87_mode_suffix(node);
				} else if (*fmt == 'I') {
					ia32_emit_x87_mode_suffix_int(node);
				} else if (*fmt == 'P') {
					ia32_x87_attr_t const *const attr = get_ia32_x87_attr_const(node);
					if (attr->x87.pop)
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
					 * permuted.  In particular it is not necessary to consider whether the
					 * result is to be placed into the explicit register operand. */
					if (get_ia32_x87_attr_const(node)->x87.reverse)
						be_emit_char('r');
				} else if (*fmt == 'X') {
					ia32_emit_xmm_mode_suffix(node);
				} else if (*fmt == '0') {
					be_emit_char('%');
					be_emit_string(get_ia32_x87_attr_const(node)->x87.reg->name);
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

			case 'M': {
				ia32_attr_t const *const attr = get_ia32_attr_const(node);
				if (mod & EMIT_32BIT_REG) {
					assert(is_ia32_Load(node) || is_ia32_Conv_I2I(node));
					if (attr->size == X86_SIZE_32)
						break;
					be_emit_char(attr->sign_extend ? 's' : 'z');
				}
				ia32_emit_mode_suffix(attr->size);
				break;
			}

			case 'P': {
				x86_condition_code_t cc;
				if (*fmt == 'X') {
					++fmt;
					cc = (x86_condition_code_t)va_arg(ap, int);
				} else if (is_digit(*fmt)) {
					cc = ia32_determine_final_cc(node, *fmt - '0');
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
				} else if (mod & EMIT_32BIT_REG) {
					name = reg->name;
				} else {
					ia32_attr_t const *const attr = get_ia32_attr_const(node);
					name = get_register_name_size(reg, attr->size,
					                              attr->use_8bit_high);
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

			default:
unknown:
				panic("unknown format conversion");
		}
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

x86_condition_code_t ia32_determine_final_cc(ir_node const *const node,
                                             int const flags_pos)
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

static void emit_jmp(ir_node const *const node, ir_node const *const target)
{
	BE_EMIT_JMP(ia32, node, "jmp", target) {}
}

/**
 * Emits the jump sequence for a conditional jump (cmp + jmp_true + jmp_false)
 */
static void emit_ia32_Jcc(const ir_node *node)
{
	x86_condition_code_t cc = ia32_determine_final_cc(node, n_ia32_Jcc_eflags);

	be_cond_branch_projs_t projs = be_get_cond_branch_projs(node);

	if (be_is_fallthrough(projs.t)) {
		/* exchange both proj's so the second one can be omitted */
		ir_node *const t = projs.t;
		projs.t = projs.f;
		projs.f = t;
		cc      = x86_negate_condition_code(cc);
	}
	bool const fallthrough = be_is_fallthrough(projs.f);
	/* if we can't have a fallthrough anyway, put the more likely case first */
	if (!fallthrough) {
		/* We would need execfreq for the concrete edge, but don't have it
		 * available here, so we use the block execfreq :-( */
		ir_node const *const target_true  = be_emit_get_cfop_target(projs.t);
		double         const freq_true    = get_block_execfreq(target_true);
		ir_node const *const target_false = be_emit_get_cfop_target(projs.f);
		double         const freq_false   = get_block_execfreq(target_false);
		if (freq_false > freq_true) {
			ir_node *const t = projs.t;
			projs.t = projs.f;
			projs.f = t;
			cc      = x86_negate_condition_code(cc);
		}
	}

	bool need_parity_label = false;
	if (cc & x86_cc_float_parity_cases) {
		/* Some floating point comparisons require a test of the parity flag,
		 * which indicates that the result is unordered */
		if (cc & x86_cc_negated) {
			ia32_emitf(node, "jp %L", projs.t);
		} else {
			/* we need a local label if the false proj is a fallthrough
			 * as the falseblock might have no label emitted then */
			if (fallthrough) {
				need_parity_label = true;
				ia32_emitf(node, "jp 1f");
			} else {
				ia32_emitf(node, "jp %L", projs.f);
			}
		}
	}
	ia32_emitf(node, "j%PX %L", (int)cc, projs.t);
	if (need_parity_label) {
		be_emit_cstring("1:\n");
		be_emit_write_line();
	}

	emit_jmp(node, projs.f);
}

/**
 * Emits an ia32 Setcc. This is mostly easy but some floating point compares
 * are tricky.
 */
static void emit_ia32_Setcc(const ir_node *node)
{
	const arch_register_t *dreg = arch_get_irn_register_out(node, pn_ia32_Setcc_res);

	x86_condition_code_t const cc
		= ia32_determine_final_cc(node, n_ia32_Setcc_eflags);
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
		ia32_emitf(node, "set%PX %R", (int)cc, dreg);
	}
}

void ia32_emit_jumptable_target(ir_entity const *const table,
                                ir_node const *const proj_x)
{
	(void)table;
	be_emit_cfop_target(proj_x);
	switch (ir_platform.pic_style) {
	case BE_PIC_NONE:
		break;

	case BE_PIC_ELF_NO_PLT:
	case BE_PIC_ELF_PLT:
		be_emit_cstring("@GOTOFF");
		break;

	case BE_PIC_MACH_O:
		be_emit_char('-');
		be_emit_string(pic_base_label);
		break;
	}
}

/**
 * Emits code for a SwitchJmp
 */
static void emit_ia32_SwitchJmp(const ir_node *node)
{
	ia32_switch_attr_t const *const attr = get_ia32_switch_attr_const(node);
	ia32_emitf(node, "jmp %*AS0");
	be_emit_jump_table(node, &attr->swtch, mode_P, ia32_emit_jumptable_target);
}

/**
 * Emits code for a unconditional jump.
 */
static void emit_ia32_Jmp(const ir_node *node)
{
	emit_jmp(node, node);
}

static void emit_ia32_asm_register(const arch_register_t *reg, char modifier,
                                   ir_mode *mode)
{
	const char *name;
	switch (modifier) {
	case 'b': name = get_register_name_8bit_low(reg); break;
	case 'h': name = get_register_name_8bit_high(reg); break;
	case 'w': name = get_register_name_16bit(reg); break;
	case 'k': name = reg->name; break;
	default:
		if (mode_is_float(mode)) {
			name = reg->name;
		} else {
			x86_insn_size_t const size = x86_size_from_mode(mode);
			name = get_register_name_size(reg, size, false);
		}
		break;
	}
	be_emit_char('%');
	be_emit_string(name);
}

static void emit_ia32_asm_operand(ir_node const *const node, char const modifier, unsigned const pos)
{
	be_asm_attr_t     const *const attr = get_be_asm_attr_const(node);
	x86_asm_operand_t const *const op   = &((x86_asm_operand_t const*)attr->operands)[pos];
	/* modifiers:
	 *   A: print '*' before operand (gcc doc: print an absolute memory reference)
	 *   P: like 'p' (but gcc doc says "if PIC, print an @PLT suffix")
	 *   X: no modifying effect (gcc doc: don't print any sort of PIC '@' suffix for a symbol)
	 *   b: 8 bit low name of register
	 *   c: immediate without prefix '$'
	 *   l: label without prefix '$'
	 *   h: 8 bit high name of register
	 *   k: 32 bit name of register
	 *   p: like 'c' and other operands unmodified (gcc doc: "print raw symbol")
	 *   w: 16 bit name of register */
	if (!be_is_valid_asm_operand_kind(node, modifier, pos, op->op.kind, "APXbhkpw", "c", ""))
		return;

	if (modifier == 'A')
		be_emit_char('*');

	switch ((be_asm_operand_kind_t)op->op.kind) {
	case BE_ASM_OPERAND_INVALID:
		panic("invalid asm operand");

	case BE_ASM_OPERAND_INPUT_VALUE: {
		arch_register_t const *const reg
			= arch_get_irn_register_in(node, op->op.pos);
		emit_ia32_asm_register(reg, modifier, op->u.mode);
		return;
	}

	case BE_ASM_OPERAND_OUTPUT_VALUE: {
		arch_register_t const *const reg
			= arch_get_irn_register_out(node, op->op.pos);
		emit_ia32_asm_register(reg, modifier, op->u.mode);
		return;
	}

	case BE_ASM_OPERAND_MEMORY:
		x86_emit_addr(node, &op->u.addr);
		return;

	case BE_ASM_OPERAND_IMMEDIATE:
		if (!be_has_modifier("Pcp", modifier))
			be_emit_char('$');
		x86_emit_imm32(&op->u.imm32);
		return;

	case BE_ASM_OPERAND_LABEL:
		if (modifier != 'l')
			be_emit_char('$');
		be_emit_cfop_target_pos(node, op->op.pos);
		return;
	}
	panic("invalid asm operand kind");
}

/**
 * Emits code for an ASM pseudo op.
 */
static void emit_ia32_Asm(const ir_node *node)
{
	ir_node *const fallthrough = be_emit_asm(node, emit_ia32_asm_operand);
	if (fallthrough)
		emit_jmp(node, fallthrough);
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
	x86_insn_size_t const size = get_ia32_attr_const(node)->size;
	const char *conv = size == X86_SIZE_32 ? conv_f : conv_d;
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
		ia32_emitf(node, "subl $%d, %#D0", offs);
	} else {
		ia32_emitf(node, "addl $%d, %#D0", -offs);
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

	ia32_emitf(node, "movl %#R, %#R", in, out);
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
		ia32_emitf(node, "xchg %#R, %#R", reg1, reg0);
	} else if (cls == &ia32_reg_classes[CLASS_ia32_xmm]) {
		ia32_emitf(NULL, "xorpd %#R, %#R", reg1, reg0);
		ia32_emitf(NULL, "xorpd %#R, %#R", reg0, reg1);
		ia32_emitf(node, "xorpd %#R, %#R", reg1, reg0);
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

static ir_type *get_thunk_type(void)
{
	if (!thunk_type) {
		ir_type *const tp = new_type_method(0, 1, false, cc_cdecl_set, mtp_no_property);
		set_method_res_type(tp, 0, get_type_for_mode(mode_P));
		thunk_type = tp;
	}
	return thunk_type;
}

static void emit_ia32_GetEIP(const ir_node *node)
{
	switch ((get_ip_style_t)get_ip_style) {
	case IA32_GET_IP_POP: {
		char const *const base = pic_base_label;
		ia32_emitf(node, "call %s", base);
		be_emit_irprintf("%s:\n", base);
		be_emit_write_line();
		ia32_emitf(node, "popl %D0");
		switch (ir_platform.pic_style) {
		case BE_PIC_ELF_PLT:
		case BE_PIC_ELF_NO_PLT:
			ia32_emitf(node, "addl $_GLOBAL_OFFSET_TABLE_ - (%s - .), %D0", base);
			return;

		case BE_PIC_MACH_O:
			return;

		case BE_PIC_NONE:
			break;
		}
		break;
	}

	case IA32_GET_IP_THUNK: {
		const arch_register_t *reg = arch_get_irn_register_out(node, 0);
		ir_entity *thunk = thunks[reg->index];
		if (thunk == NULL) {
			ir_type    *const glob = get_glob_type();
			char const *const name = get_register_name_16bit(reg);
			ident      *const id   = new_id_fmt("__x86.get_pc_thunk.%s", name);
			ir_type    *const tp   = get_thunk_type();
			thunk = new_global_entity(glob, id, tp, ir_visibility_external_private,
			                          IR_LINKAGE_MERGE|IR_LINKAGE_GARBAGE_COLLECT);
			/* Note that we do not create a proper method graph, but rather cheat
			 * later and emit the instructions manually. This is just necessary so
			 * firm knows we will actually output code for this entity. */
			new_ir_graph(thunk, 0);

			thunks[reg->index] = thunk;
		}

		ia32_emitf(node, "call %E", thunk);
		switch (ir_platform.pic_style) {
		case BE_PIC_MACH_O:
			be_emit_irprintf("%s:\n", pic_base_label);
			be_emit_write_line();
			return;
		case BE_PIC_ELF_PLT:
		case BE_PIC_ELF_NO_PLT:
			ia32_emitf(node, "addl $_GLOBAL_OFFSET_TABLE_, %D0");
			return;
		case BE_PIC_NONE:
			break;
		}
		break;
	}
	}

	panic("invalid pic_style");
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
	be_set_emitter(op_ia32_Ret,        emit_ia32_Return);
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
	be_emit_cfop_target(node);
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

	if (omit_fp) {
		int sp_change = -ia32_get_sp_change(node);
		if (sp_change != 0) {
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
bool ia32_should_align_block(ir_node const *const block)
{
	static const double DELTA = .0001;
	ir_node *prev      = be_emit_get_prev_block(block);
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

static bool has_fallthrough(ir_node const *const block)
{
	for (int i = get_Block_n_cfgpreds(block); i-- > 0; ) {
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		if (be_emit_get_prev_block(block) == pred_block)
			return true;
	}
	return false;
}

/**
 * Emit the block header for a block.
 *
 * @param block       the block
 * @param prev_block  the previous block
 */
static void ia32_emit_block_header(ir_node *block)
{
	if (ia32_cg_config.label_alignment > 0) {
		/* align the current block if:
		 * a) if should be aligned due to its execution frequency
		 * b) there is no fall-through here
		 */
		if (ia32_should_align_block(block) || !has_fallthrough(block))
			ia32_emit_align_label();
	}

	be_gas_begin_block(block);
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void ia32_gen_block(ir_node *block)
{
	ia32_emit_block_header(block);

	if (omit_fp) {
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

		pred = skip_Proj(pred);
		if (is_ia32_irn(pred) && get_ia32_exc_label(pred) && exc_list != NULL) {
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
	ir_entity            *entity     = get_irg_entity(irg);
	ir_type              *type       = get_entity_type(entity);
	ir_type              *frame_type = get_irg_frame_type(irg);
	size_t                n_params   = get_method_n_params(type);
	parameter_dbg_info_t *infos     = XMALLOCNZ(parameter_dbg_info_t, n_params);

	for (size_t i = 0, n_members = get_compound_n_members(frame_type);
	     i < n_members; ++i) {
		ir_entity *member = get_compound_member(frame_type, i);
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

static void emit_function_text(ir_graph *const irg, exc_entry **const exc_list)
{
	ia32_register_emitters();

	ir_node  **const blk_sched = be_create_block_schedule(irg);

	/* we use links to point to target blocks */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_block_walk_graph(irg, ia32_gen_labels, NULL, exc_list);

	be_emit_init_cf_links(blk_sched);

	for (size_t i = 0, n = ARR_LEN(blk_sched); i < n; ++i) {
		ir_node *const block = blk_sched[i];
		ia32_gen_block(block);
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
}

static const lc_opt_enum_int_items_t get_ip_syle_items[] = {
  { "pop",   IA32_GET_IP_POP   },
  { "thunk", IA32_GET_IP_THUNK },
  { NULL, 0 }
};

static lc_opt_enum_int_var_t get_ip_style_var = {
  &get_ip_style, get_ip_syle_items
};

static const lc_opt_table_entry_t ia32_emitter_options[] = {
	LC_OPT_ENT_BOOL    ("mark_spill_reload", "mark spills and reloads with ud opcodes", &mark_spill_reload),
	LC_OPT_ENT_ENUM_INT("get_ip",            "method to get IP for the pic base",       &get_ip_style_var),
	LC_OPT_LAST
};

static unsigned emit_jit_entity_relocation_asm(char *const buffer,
                                               uint8_t const be_kind,
                                               ir_entity *const entity,
                                               int32_t const offset)
{
	(void)buffer;
	assert(buffer == NULL);
	if (be_kind == IA32_RELOCATION_RELJUMP) {
		be_emit_irprintf("\t.long %"PRId32"\n", offset);
		be_emit_write_line();
		return 4;
	}

	unsigned res = 4;
	if (be_kind == X86_IMM_PCREL) {
		/* cheat... */
		be_emit_cstring("\tcall ");
		res = 5;
	} else {
		be_emit_cstring("\t.long ");
	}
	x86_emit_relocation_no_offset(be_kind, entity);
	if (offset != 0)
		be_emit_irprintf("%+"PRId32, offset);
	be_emit_char('\n');
	be_emit_write_line();
	return res;
}

void ia32_emit_function(ir_graph *const irg)
{
	exc_entry *exc_list = NEW_ARR_F(exc_entry, 0);
	be_gas_elf_type_char = '@';

	ir_entity *const entity = get_irg_entity(irg);
	parameter_dbg_info_t *infos = construct_parameter_infos(irg);
	be_gas_emit_function_prolog(entity, ia32_cg_config.function_alignment, infos);
	free(infos);

	omit_fp = ia32_get_irg_data(irg)->omit_fp;
	if (omit_fp) {
		ir_type *frame_type = get_irg_frame_type(irg);
		frame_type_size = get_type_size(frame_type);
		be_dwarf_callframe_register(&ia32_registers[REG_ESP]);
	} else {
		/* well not entirely correct here, we should emit this after the
		 * "movl esp, ebp" */
		be_dwarf_callframe_register(&ia32_registers[REG_EBP]);
		/* TODO: do not hardcode the following */
		be_dwarf_callframe_offset(8);
		be_dwarf_callframe_spilloffset(&ia32_registers[REG_EBP], -8);
	}

	get_unique_label(pic_base_label, sizeof(pic_base_label), "PIC_BASE");
	x86_pic_base_label = pic_base_label;

	if (ia32_cg_config.emit_machcode) {
		/* For debugging we can jit the code and output it embedded into a
		 * normal .s file with .byte directives etc. */
		ir_jit_segment_t *const segment = be_new_jit_segment();
		ir_jit_function_t *const function = ia32_emit_jit(segment, irg);
		be_jit_emit_as_asm(function, emit_jit_entity_relocation_asm);
		be_destroy_jit_segment(segment);
	} else {
		emit_function_text(irg, &exc_list);
	}

	be_gas_emit_function_epilog(entity);

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

void ia32_emit_thunks(void)
{
	for (unsigned i = 0; i < N_ia32_gp_REGS; ++i) {
		ir_entity *entity = thunks[i];
		if (entity == NULL)
			continue;
		const arch_register_t *reg = &ia32_reg_classes[CLASS_ia32_gp].regs[i];

		be_gas_emit_function_prolog(entity, ia32_cg_config.function_alignment,
		                            NULL);
		ia32_emitf(NULL, "movl (%%esp), %#R", reg);
		ia32_emitf(NULL, "ret");
		be_gas_emit_function_epilog(entity);
	}
}

void ia32_init_emitter(void)
{
	lc_opt_entry_t *be_grp   = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ia32_grp = lc_opt_get_grp(be_grp, "ia32");

	lc_opt_add_table(ia32_grp, ia32_emitter_options);

	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.emitter");
}
