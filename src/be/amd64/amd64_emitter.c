/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   emit assembler for a backend graph
 */
#include "amd64_emitter.h"

#include "amd64_bearch_t.h"
#include "amd64_new_nodes.h"
#include "amd64_nodes_attr.h"
#include "be_t.h"
#include "beasm.h"
#include "beblocksched.h"
#include "bediagnostic.h"
#include "beemithlp.h"
#include "beemitter.h"
#include "begnuas.h"
#include "beirg.h"
#include "benode.h"
#include "besched.h"
#include "gen_amd64_emitter.h"
#include "gen_amd64_regalloc_if.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "panic.h"
#include "platform_t.h"
#include <inttypes.h>

static bool omit_fp;
static int  frame_type_size;
static int  callframe_offset;

static char get_gp_size_suffix(x86_insn_size_t const size)
{
	switch (size) {
	case X86_SIZE_8:  return 'b';
	case X86_SIZE_16: return 'w';
	case X86_SIZE_32: return 'l';
	case X86_SIZE_64: return 'q';
	case X86_SIZE_80:
	case X86_SIZE_128:
		break;
	}
	panic("invalid insn mode");
}

static void amd64_emit_insn_size_suffix(x86_insn_size_t const size)
{
	be_emit_char(get_gp_size_suffix(size));
}

static char get_xmm_size_suffix(x86_insn_size_t const size)
{
	switch (size) {
	case X86_SIZE_32:  return 's';
	case X86_SIZE_64:  return 'd';
	case X86_SIZE_128: return 'q';
	case X86_SIZE_8:
	case X86_SIZE_16:
	case X86_SIZE_80:
		break;
	}
	panic("invalid insn mode");
}

static void amd64_emit_xmm_size_suffix(x86_insn_size_t const size)
{
	be_emit_char(get_xmm_size_suffix(size));
}

static char get_x87_size_suffix(x86_insn_size_t const size)
{
	switch (size) {
	case X86_SIZE_32: return 's';
	case X86_SIZE_64: return 'l';
	case X86_SIZE_80: return 't';
	case X86_SIZE_8:
	case X86_SIZE_16:
	case X86_SIZE_128:
		break;
	}
	panic("Invalid insn mode");
}

static void amd64_emit_x87_size_suffix(x86_insn_size_t const size)
{
	be_emit_char(get_x87_size_suffix(size));
}

static const char *get_register_name_8bit(const arch_register_t *reg)
{
	switch (reg->global_index) {
	case REG_RAX: return "al";
	case REG_RBX: return "bl";
	case REG_RCX: return "cl";
	case REG_RDX: return "dl";
	case REG_RSP: return "spl";
	case REG_RBP: return "bpl";
	case REG_RSI: return "sil";
	case REG_RDI: return "dil";
	case REG_R8:  return "r8b";
	case REG_R9:  return "r9b";
	case REG_R10: return "r10b";
	case REG_R11: return "r11b";
	case REG_R12: return "r12b";
	case REG_R13: return "r13b";
	case REG_R14: return "r14b";
	case REG_R15: return "r15b";
	}
	panic("unexpected register number");
}

static const char *get_register_name_8bit_high(const arch_register_t *reg)
{
	switch (reg->global_index) {
	case REG_RAX: return "ah";
	case REG_RBX: return "bh";
	case REG_RCX: return "ch";
	case REG_RDX: return "dh";
	}
	panic("unexpected register number");
}

static const char *get_register_name_16bit(const arch_register_t *reg)
{
	switch (reg->global_index) {
	case REG_RAX: return "ax";
	case REG_RBX: return "bx";
	case REG_RCX: return "cx";
	case REG_RDX: return "dx";
	case REG_RSP: return "sp";
	case REG_RBP: return "bp";
	case REG_RSI: return "si";
	case REG_RDI: return "di";
	case REG_R8:  return "r8w";
	case REG_R9:  return "r9w";
	case REG_R10: return "r10w";
	case REG_R11: return "r11w";
	case REG_R12: return "r12w";
	case REG_R13: return "r13w";
	case REG_R14: return "r14w";
	case REG_R15: return "r15w";
	}
	panic("unexpected register number");
}

static const char *get_register_name_32bit(const arch_register_t *reg)
{
	switch (reg->global_index) {
	case REG_RAX: return "eax";
	case REG_RBX: return "ebx";
	case REG_RCX: return "ecx";
	case REG_RDX: return "edx";
	case REG_RSP: return "esp";
	case REG_RBP: return "ebp";
	case REG_RSI: return "esi";
	case REG_RDI: return "edi";
	case REG_R8:  return "r8d";
	case REG_R9:  return "r9d";
	case REG_R10: return "r10d";
	case REG_R11: return "r11d";
	case REG_R12: return "r12d";
	case REG_R13: return "r13d";
	case REG_R14: return "r14d";
	case REG_R15: return "r15d";
	}
	panic("unexpected register number");
}

static void emit_register(const arch_register_t *reg)
{
	be_emit_char('%');
	be_emit_string(reg->name);
}

static const char *get_register_name_mode(const arch_register_t *reg,
                                          const x86_insn_size_t size)
{
	switch (size) {
	case X86_SIZE_8:  return get_register_name_8bit(reg);
	case X86_SIZE_16: return get_register_name_16bit(reg);
	case X86_SIZE_32: return get_register_name_32bit(reg);
	case X86_SIZE_64:
	case X86_SIZE_80:
	case X86_SIZE_128: return reg->name;
	}
	panic("invalid mode");
}

static void emit_register_sized(const arch_register_t *reg,
                                const x86_insn_size_t size)
{
	be_emit_char('%');
	be_emit_string(get_register_name_mode(reg, size));
}

static void emit_register_mode(const arch_register_t *reg,
                               x86_insn_size_t size)
{
	if (reg->cls == &amd64_reg_classes[CLASS_amd64_xmm]) {
		emit_register(reg);
	} else {
		emit_register_sized(reg, size);
	}
}

typedef enum amd64_emit_mod_t {
	EMIT_NONE          = 0,
	EMIT_IGNORE_MODE   = 1U << 1,
	EMIT_FORCE_32      = 1U << 2,
	EMIT_CONV_DEST     = 1U << 3,
	EMIT_INDIRECT_STAR = 1U << 4,
} amd64_emit_mod_t;
ENUM_BITSET(amd64_emit_mod_t)

static void amd64_emit_immediate64(const amd64_imm64_t *const imm)
{
	if (imm->kind == X86_IMM_VALUE) {
		assert(imm->entity == NULL);
		be_emit_irprintf("0x%" PRIX64, imm->offset);
		return;
	}
	x86_emit_relocation_no_offset(imm->kind, imm->entity);
	if (imm->offset != 0)
		be_emit_irprintf("%+" PRId64, imm->offset);
}

static void amd64_emit_am(const ir_node *const node, bool indirect_star)
{
	const amd64_addr_attr_t *const attr = get_amd64_addr_attr_const(node);

	switch ((amd64_op_mode_t)attr->base.op_mode) {
	case AMD64_OP_REG_IMM: {
		const amd64_binop_addr_attr_t *const binop_attr
			= (const amd64_binop_addr_attr_t*)attr;
		be_emit_char('$');
		x86_emit_imm32(&binop_attr->u.immediate);
		be_emit_cstring(", ");
		goto emit_addr_reg;
	}
	case AMD64_OP_REG_REG_ADDR: {
		x86_emit_addr(node, &attr->addr);
		be_emit_cstring(", ");
		const arch_register_t *reg1 = arch_get_irn_register_in(node, 1);
		emit_register_mode(reg1, attr->base.size);
		be_emit_cstring(", ");
		const arch_register_t *reg0 = arch_get_irn_register_in(node, 0);
		emit_register_mode(reg0, attr->base.size);
		return;
	}
	case AMD64_OP_REG_REG_REG: {
		const arch_register_t *reg2 = arch_get_irn_register_in(node, 2);
		emit_register_mode(reg2, attr->base.size);
		be_emit_cstring(", ");
		// fallthrough
	}
	case AMD64_OP_REG_REG: {
		const arch_register_t *reg1 = arch_get_irn_register_in(node, 1);
		emit_register_mode(reg1, attr->base.size);
		be_emit_cstring(", ");
		goto emit_addr_reg;
	}
	case AMD64_OP_REG_ADDR: {
		const amd64_binop_addr_attr_t *const binop_attr
			= (const amd64_binop_addr_attr_t*)attr;
		x86_emit_addr(node, &attr->addr);
		be_emit_cstring(", ");
		const arch_register_t *reg
			= arch_get_irn_register_in(node, binop_attr->u.reg_input);
		emit_register_mode(reg, binop_attr->base.base.size);
		return;
	}
	case AMD64_OP_ADDR_IMM: {
		const amd64_binop_addr_attr_t *const binop_attr
			= (const amd64_binop_addr_attr_t*)attr;
		be_emit_char('$');
		x86_emit_imm32(&binop_attr->u.immediate);
		be_emit_cstring(", ");
		goto emit_addr;
	}
	case AMD64_OP_ADDR:
	case AMD64_OP_X87_ADDR_REG:
		if (indirect_star)
			be_emit_char('*');
		goto emit_addr;
	case AMD64_OP_ADDR_REG: {
		amd64_binop_addr_attr_t const *const binop_attr = (amd64_binop_addr_attr_t const*)attr;
		arch_register_t const *const reg = arch_get_irn_register_in(node, binop_attr->u.reg_input);
		emit_register_mode(reg, binop_attr->base.base.size);
		be_emit_cstring(", ");
emit_addr:
		x86_emit_addr(node, &attr->addr);
		return;
	}
	case AMD64_OP_REG: {
		if (indirect_star)
			be_emit_char('*');
emit_addr_reg:
		assert(attr->addr.variant == X86_ADDR_REG);
		arch_register_t const *const reg
			= arch_get_irn_register_in(node, attr->addr.base_input);
		emit_register_mode(reg, attr->base.size);
		return;
	}
	case AMD64_OP_IMM32:
		x86_emit_imm32(&attr->addr.immediate);
		return;

	case AMD64_OP_X87:
		return;
	case AMD64_OP_IMM64:
	case AMD64_OP_NONE:
	case AMD64_OP_SHIFT_REG:
	case AMD64_OP_SHIFT_IMM:
	case AMD64_OP_CC:
		break;
	}
	panic("invalid op_mode");
}

static void emit_shiftop(const ir_node *const node)
{
	amd64_shift_attr_t const *const attr = get_amd64_shift_attr_const(node);

	switch (attr->base.op_mode) {
	case AMD64_OP_SHIFT_IMM: {
		be_emit_irprintf("$%u, ", attr->immediate);
		const arch_register_t *reg = arch_get_irn_register_in(node, 0);
		emit_register_mode(reg, attr->base.size);
		return;
	}
	case AMD64_OP_SHIFT_REG: {
		const arch_register_t *reg0 = arch_get_irn_register_in(node, 0);
		const arch_register_t *reg1 = arch_get_irn_register_in(node, 1);
		emit_register_mode(reg1, X86_SIZE_8);
		be_emit_cstring(", ");
		emit_register_mode(reg0, attr->base.size);
		return;
	}
	default:
		break;
	}
	panic("invalid op_mode for shiftop");
}

void amd64_emitf(ir_node const *const node, char const *fmt, ...)
{
	BE_EMITF(node, fmt, ap, false) {
		amd64_emit_mod_t mod = EMIT_NONE;
		for (;;) {
			switch (*fmt) {
			case '^': mod |= EMIT_IGNORE_MODE;   break;
			case '3': mod |= EMIT_FORCE_32;      break;
			case '#': mod |= EMIT_CONV_DEST;     break;
			case '*': mod |= EMIT_INDIRECT_STAR; break;
			default:
				goto end_of_mods;
			}
			++fmt;
		}
end_of_mods:

		switch (*fmt++) {
			arch_register_t const *reg;

			case 'A':
				switch (*fmt++) {
				case 'F': {
					x87_attr_t const *const attr
						= amd64_get_x87_attr_const(node);
					char const *const fmt
						= attr->res_in_reg ? "%%st, %%%s" : "%%%s, %%st";
					be_emit_irprintf(fmt, attr->reg->name);
					break;
				}
				case 'M':
					amd64_emit_am(node, mod & EMIT_INDIRECT_STAR);
					break;
				default: {
					amd64_addr_attr_t const *const attr
						= get_amd64_addr_attr_const(node);
					x86_emit_addr(node, &attr->addr);
					--fmt;
				}
				}
				break;

			case 'C': {
				amd64_movimm_attr_t const *const attr
					= get_amd64_movimm_attr_const(node);
				amd64_emit_immediate64(&attr->immediate);
				break;
			}

			case 'D':
				if (!is_digit(*fmt))
					goto unknown;
				reg = arch_get_irn_register_out(node, *fmt++ - '0');
				goto emit_R;

			case 'E': {
				ir_entity const *const ent = va_arg(ap, ir_entity const*);
				be_gas_emit_entity(ent);
				break;
			}

			case 'F': {
				if (*fmt == 'M') {
					++fmt;
					amd64_addr_attr_t const *const attr
						= get_amd64_addr_attr_const(node);
					amd64_emit_x87_size_suffix(attr->base.size);
				} else if (*fmt == 'P') {
					++fmt;
					x87_attr_t const *const attr
						= amd64_get_x87_attr_const(node);
					if (attr->pop)
						be_emit_char('p');
				} else if (*fmt == '0') {
					++fmt;
					x87_attr_t const *const attr
						= amd64_get_x87_attr_const(node);
					be_emit_char('%');
					be_emit_string(attr->reg->name);
				} else if (*fmt == 'R') {
					++fmt;
					x87_attr_t const *const attr
						= amd64_get_x87_attr_const(node);
					/** see also ia32_emitter comment */
					if (attr->reverse)
						be_emit_char('r');
				} else
					goto unknown;
				break;
			}

			case 'P': {
				x86_condition_code_t cc;
				if (*fmt == 'X') {
					// Fetch cc from varargs
					++fmt;
					cc = (x86_condition_code_t)va_arg(ap, int);
				} else if (is_digit(*fmt)) {
					// Format string is backwards compatible to IA32 backend.
					// Fetch cc from node attributes
					++fmt;
					cc = get_amd64_cc_attr_const(node)->cc;
				} else {
					panic("unknown modifier");
				}
				x86_emit_condition_code(cc);
				break;
			}

			case 'R':
				reg = va_arg(ap, arch_register_t const*);
				goto emit_R;

			case 'S': {
				if (*fmt == 'O') {
					++fmt;
					emit_shiftop(node);
					break;
				}
				if (!is_digit(*fmt))
					goto unknown;
				int const pos = *fmt++ - '0';
				reg = arch_get_irn_register_in(node, pos);
emit_R:
				if (mod & EMIT_IGNORE_MODE) {
					emit_register(reg);
				} else if (mod & EMIT_FORCE_32) {
					emit_register_mode(reg, X86_SIZE_32);
				} else if (mod & EMIT_CONV_DEST) {
					amd64_attr_t const *const attr = get_amd64_attr_const(node);
					x86_insn_size_t src_size  = attr->size;
					x86_insn_size_t dest_size = src_size == X86_SIZE_64
					                            ? X86_SIZE_64 : X86_SIZE_32;
					emit_register_mode(reg, dest_size);
				} else {
					amd64_attr_t const *const attr = get_amd64_attr_const(node);
					emit_register_mode(reg, attr->size);
				}
				break;
			}

			case 'M': {
				amd64_attr_t const *const attr = get_amd64_attr_const(node);
				if (*fmt == 'X') {
					++fmt;
					amd64_emit_xmm_size_suffix(attr->size);
				} else {
					amd64_emit_insn_size_suffix(attr->size);
				}
				break;
			}

			default:
unknown:
				panic("unknown format conversion");
		}
	}
}

static const char *get_register_name_ir_mode(const arch_register_t *reg,
                                             ir_mode *mode)
{
	if (get_mode_arithmetic(mode) != irma_twos_complement)
		return reg->name;
	switch (get_mode_size_bits(mode)) {
	case 8:  return get_register_name_8bit(reg);
	case 16: return get_register_name_16bit(reg);
	case 32: return get_register_name_32bit(reg);
	case 64: return reg->name;
	default:
		panic("unexpected mode size");
	}
}

static void emit_amd64_asm_register(const arch_register_t *reg, char modifier,
                                    ir_mode *mode)
{
	const char *name;
	switch (modifier) {
	case  'b': name = get_register_name_8bit(reg); break;
	case  'h': name = get_register_name_8bit_high(reg); break;
	case  'w': name = get_register_name_16bit(reg); break;
	case  'k': name = get_register_name_32bit(reg); break;
	case  'q': name = reg->name; break;
	// gcc also knows 'x' V4SFmode, 't' V8SFmode, 'y' "st(0)" instead of "st",
	// 'd' duplicate operand for AVX instruction
	default:   name = mode ? get_register_name_ir_mode(reg, mode) : reg->name; break;
	}
	be_emit_char('%');
	be_emit_string(name);
}

static void emit_amd64_asm_operand(ir_node const *const node, char const modifier, unsigned const pos)
{
	be_asm_attr_t     const *const attr = get_be_asm_attr_const(node);
	x86_asm_operand_t const *const op   = &((x86_asm_operand_t const*)attr->operands)[pos];
	/* modifiers:
	 *   A: print '*' before operand (gcc doc: print an absolute memory reference)
	 *   P: like 'p' and rip-relative operands without (%rip) (but gcc doc says "if PIC, print an @PLT suffix")
	 *   X: no modifying effect (gcc doc: don't print any sort of PIC '@' suffix for a symbol)
	 *   b: 8 bit low name of register
	 *   c: immediate without prefix '$'
	 *   l: label without prefix '$'
	 *   h: 8 bit high name of register
	 *   k: 32 bit name of register
	 *   p: like 'c' and other operands unmodified (gcc doc: "print raw symbol")
	 *   q: 64 bit name of register
	 *   w: 16 bit name of register */
	if (!be_is_valid_asm_operand_kind(node, modifier, pos, op->op.kind, "APXbhkpqw", "c", ""))
		return;

	if (modifier == 'A')
		be_emit_char('*');

	switch ((be_asm_operand_kind_t)op->op.kind) {
	case BE_ASM_OPERAND_INVALID:
		panic("invalid asm operand");

	case BE_ASM_OPERAND_INPUT_VALUE: {
		arch_register_t const *const reg
			= arch_get_irn_register_in(node, op->op.pos);
		emit_amd64_asm_register(reg, modifier, op->u.mode);
		return;
	}

	case BE_ASM_OPERAND_OUTPUT_VALUE: {
		arch_register_t const *const reg
			= arch_get_irn_register_out(node, op->op.pos);
		emit_amd64_asm_register(reg, modifier, op->u.mode);
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

static void emit_jmp(ir_node const *const node, ir_node const *const target)
{
	BE_EMIT_JMP(amd64, node, "jmp", target) {}
}

static void emit_amd64_asm(const ir_node *node)
{
	ir_node const *const fallthrough = be_emit_asm(node, emit_amd64_asm_operand);
	if (fallthrough)
		emit_jmp(node, fallthrough);
}

/**
 * Emit a Jmp.
 */
static void emit_amd64_jmp(const ir_node *node)
{
	emit_jmp(node, node);
}

static void emit_jumptable_target(ir_entity const *const table,
                                  ir_node const *const proj_x)
{
	be_emit_cfop_target(proj_x);
	if (ir_platform.pic_style != BE_PIC_NONE) {
		be_emit_char('-');
		be_gas_emit_entity(table);
	}
}

static void emit_amd64_jmp_switch(const ir_node *node)
{
	const amd64_switch_jmp_attr_t *attr = get_amd64_switch_jmp_attr_const(node);

	amd64_emitf(node, "jmp %*AM");
	ir_mode *entry_mode = ir_platform.pic_style != BE_PIC_NONE ? mode_Iu
	                                                           : mode_Lu;
	be_emit_jump_table(node, &attr->swtch, entry_mode, emit_jumptable_target);
}

static x86_condition_code_t determine_final_cc(ir_node const *const flags,
                                               x86_condition_code_t cc)
{
	if (is_amd64_fucomi(flags)) {
		amd64_x87_attr_t const *const attr = get_amd64_x87_attr_const(flags);
		if (attr->x87.reverse)
			cc = x86_invert_condition_code(cc);
	}
	return cc;
}

/**
 * Emit a Compare with conditional branch.
 */
static void emit_amd64_jcc(const ir_node *irn)
{
	const ir_node         *flags = get_irn_n(irn, n_amd64_jcc_flags);
	const amd64_cc_attr_t *attr  = get_amd64_cc_attr_const(irn);
	x86_condition_code_t   cc    = determine_final_cc(flags, attr->cc);

	be_cond_branch_projs_t projs = be_get_cond_branch_projs(irn);

	if (be_is_fallthrough(projs.t)) {
		/* exchange both proj's so the second one can be omitted */
		ir_node *const t = projs.t;
		projs.t = projs.f;
		projs.f = t;
		cc      = x86_negate_condition_code(cc);
	}

	if (cc & x86_cc_float_parity_cases) {
		/* Some floating point comparisons require a test of the parity flag,
		 * which indicates that the result is unordered */
		if (cc & x86_cc_negated) {
			amd64_emitf(irn, "jp %L", projs.t);
		} else {
			amd64_emitf(irn, "jp %L", projs.f);
		}
	}

	/* emit the true proj */
	amd64_emitf(irn, "j%PX %L", (int)cc, projs.t);

	emit_jmp(irn, projs.f);
}

static void emit_amd64_mov_gp(const ir_node *node)
{
	amd64_attr_t const *const attr = get_amd64_attr_const(node);
	switch (attr->size) {
	case X86_SIZE_8:  amd64_emitf(node, "movzbl %AM, %3D0"); return;
	case X86_SIZE_16: amd64_emitf(node, "movzwl %AM, %3D0"); return;
	case X86_SIZE_32: amd64_emitf(node, "movl %AM, %3D0");   return;
	case X86_SIZE_64: amd64_emitf(node, "movq %AM, %^D0");   return;
	case X86_SIZE_80:
	case X86_SIZE_128:
		break;
	}
	panic("invalid insn mode");
}

/**
 * Emit movsb/w instructions to make mov count divisible by 8.
 */
static void emit_copyB_prolog(unsigned size)
{
	if (size & 1)
		amd64_emitf(NULL, "movsb");
	if (size & 2)
		amd64_emitf(NULL, "movsw");
	if (size & 4)
		amd64_emitf(NULL, "movsd");
}

/**
 * Emit rep movsq instruction for memcopy.
 */
static void emit_amd64_copyB(const ir_node *node)
{
	unsigned size = get_amd64_copyb_attr_const(node)->size;

	emit_copyB_prolog(size);
	amd64_emitf(node, "rep movsd");
}

/**
 * Emits unrolled memcopy.
 */
static void emit_amd64_copyB_i(const ir_node *node)
{
	unsigned size = get_amd64_copyb_attr_const(node)->size;

	emit_copyB_prolog(size);

	size >>= 3;
	while (size--) {
		amd64_emitf(NULL, "movsq");
	}
}

/**
 * emit copy node
 */
static void emit_be_Copy(const ir_node *irn)
{
	arch_register_t const *const out = arch_get_irn_register_out(irn, 0);
	if (arch_get_irn_register_in(irn, 0) == out) {
		/* omitted Copy */
		return;
	}

	arch_register_class_t const *const cls = out->cls;
	if (cls == &amd64_reg_classes[CLASS_amd64_gp]) {
		amd64_emitf(irn, "mov %^S0, %^D0");
	} else if (cls == &amd64_reg_classes[CLASS_amd64_xmm]) {
		amd64_emitf(irn, "movapd %^S0, %^D0");
	} else if (cls == &amd64_reg_classes[CLASS_amd64_x87]) {
		/* nothing to do */
	} else {
		panic("move not supported for this register class");
	}
}

static void emit_be_Perm(const ir_node *node)
{
	arch_register_t const *const reg0 = arch_get_irn_register_out(node, 0);
	arch_register_t const *const reg1 = arch_get_irn_register_out(node, 1);

	arch_register_class_t const* const cls = reg0->cls;
	assert(cls == reg1->cls && "Register class mismatch at Perm");

	if (cls == &amd64_reg_classes[CLASS_amd64_gp]) {
		amd64_emitf(node, "xchg %^R, %^R", reg0, reg1);
	} else if (cls == &amd64_reg_classes[CLASS_amd64_xmm]) {
		amd64_emitf(node, "pxor %^R, %^R", reg0, reg1);
		amd64_emitf(node, "pxor %^R, %^R", reg1, reg0);
		amd64_emitf(node, "pxor %^R, %^R", reg0, reg1);
	} else {
		panic("unexpected register class in be_Perm (%+F)", node);
	}
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
		amd64_emitf(node, "subq $%d, %^D0", offs);
	} else {
		amd64_emitf(node, "addq $%d, %^D0", -offs);
	}
}

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void amd64_register_emitters(void)
{
	be_init_emitters();

	/* register all emitter functions defined in spec */
	amd64_register_spec_emitters();

	be_set_emitter(op_amd64_jcc,        emit_amd64_jcc);
	be_set_emitter(op_amd64_jmp,        emit_amd64_jmp);
	be_set_emitter(op_amd64_jmp_switch, emit_amd64_jmp_switch);
	be_set_emitter(op_amd64_mov_gp,     emit_amd64_mov_gp);
	be_set_emitter(op_amd64_copyB,      emit_amd64_copyB);
	be_set_emitter(op_amd64_copyB_i,    emit_amd64_copyB_i);
	be_set_emitter(op_be_Asm,           emit_amd64_asm);
	be_set_emitter(op_be_Copy,          emit_be_Copy);
	be_set_emitter(op_be_CopyKeep,      emit_be_Copy);
	be_set_emitter(op_be_IncSP,         emit_be_IncSP);
	be_set_emitter(op_be_Perm,          emit_be_Perm);
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void amd64_gen_block(ir_node *block)
{
	be_gas_begin_block(block);

	if (omit_fp) {
		ir_graph *irg = get_irn_irg(block);
		callframe_offset = 8; /* 8 bytes for the return address */
		/* RSP guessing, TODO perform a real RSP simulation */
		if (block != get_irg_start_block(irg)) {
			callframe_offset += frame_type_size;
		}
		be_dwarf_callframe_offset(callframe_offset);
	}

	be_dwarf_location(get_irn_dbg_info(block));
	sched_foreach(block, node) {
		be_emit_node(node);

		if (omit_fp) {
			const int sp_change = -amd64_get_sp_change(node);
			if (sp_change != 0) {
				callframe_offset += sp_change;
				be_dwarf_callframe_offset(callframe_offset);
			}
		}
	}
}

void amd64_emit_function(ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);

	/* register all emitter functions */
	amd64_register_emitters();

	ir_node **blk_sched = be_create_block_schedule(irg);

	be_gas_emit_function_prolog(entity, 4, NULL);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	be_emit_init_cf_links(blk_sched);

	amd64_irg_data_t const *const irg_data = amd64_get_irg_data(irg);
	omit_fp = irg_data->omit_fp;

	if (omit_fp) {
		ir_type *frame_type = get_irg_frame_type(irg);
		frame_type_size = get_type_size(frame_type);
		be_dwarf_callframe_register(&amd64_registers[REG_RSP]);
	} else {
		/* well not entirely correct here, we should emit this after the
		 * "movq rsp, rbp" */
		be_dwarf_callframe_register(&amd64_registers[REG_RBP]);
		/* TODO: do not hardcode the following */
		be_dwarf_callframe_offset(16);
		be_dwarf_callframe_spilloffset(&amd64_registers[REG_RBP], -16);
	}

	for (size_t i = 0, n = ARR_LEN(blk_sched); i < n; ++i) {
		ir_node *block = blk_sched[i];
		amd64_gen_block(block);
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	be_gas_emit_function_epilog(entity);
}
