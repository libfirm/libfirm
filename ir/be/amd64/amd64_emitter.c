/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   emit assembler for a backend graph
 */
#include <inttypes.h>

#include "amd64_emitter.h"
#include "amd64_new_nodes.h"
#include "amd64_nodes_attr.h"
#include "be_t.h"
#include "beasm.h"
#include "beblocksched.h"
#include "bediagnostic.h"
#include "begnuas.h"
#include "beirg.h"
#include "benode.h"
#include "besched.h"
#include "gen_amd64_emitter.h"
#include "gen_amd64_regalloc_if.h"
#include "irgwalk.h"
#include "panic.h"

static be_stack_layout_t *layout;

/**
 * Returns the target block for a control flow node.
 */
static ir_node *get_cfop_target_block(const ir_node *irn)
{
	return (ir_node*)get_irn_link(irn);
}

static char get_gp_mode_suffix(const amd64_insn_mode_t mode)
{
	switch (mode) {
	case INSN_MODE_8:  return 'b';
	case INSN_MODE_16: return 'w';
	case INSN_MODE_32: return 'l';
	case INSN_MODE_64: return 'q';
	case INSN_MODE_128:
	case INSN_MODE_INVALID:
		break;
	}
	panic("invalid insn mode");
}

static void amd64_emit_insn_mode_suffix(const amd64_insn_mode_t mode)
{
	be_emit_char(get_gp_mode_suffix(mode));
}

static char get_xmm_mode_suffix(const amd64_insn_mode_t mode)
{
	switch (mode) {
	case INSN_MODE_32:  return 's';
	case INSN_MODE_64:  return 'd';
	case INSN_MODE_128: return 'q';
	case INSN_MODE_8:
	case INSN_MODE_16:
	case INSN_MODE_INVALID:
		break;
	}
	panic("invalid insn mode");
}

static void amd64_emit_xmm_mode_suffix(const amd64_insn_mode_t mode)
{
	be_emit_char(get_xmm_mode_suffix(mode));
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
                                          const amd64_insn_mode_t mode)
{
	switch (mode) {
	case INSN_MODE_8:  return get_register_name_8bit(reg);
	case INSN_MODE_16: return get_register_name_16bit(reg);
	case INSN_MODE_32: return get_register_name_32bit(reg);
	case INSN_MODE_64:
	case INSN_MODE_128: return reg->name;
	case INSN_MODE_INVALID:
		break;
	}
	panic("invalid mode");
}

static void emit_register_insn_mode(const arch_register_t *reg,
                                    const amd64_insn_mode_t mode)
{
	be_emit_char('%');
	be_emit_string(get_register_name_mode(reg, mode));
}

static void emit_register_mode(const arch_register_t *reg,
                               amd64_insn_mode_t insn_mode)
{
	if (reg->cls == &amd64_reg_classes[CLASS_amd64_xmm]) {
		emit_register(reg);
	} else {
		emit_register_insn_mode(reg, insn_mode);
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

static void emit_relocation_no_offset(x86_immediate_kind_t const kind,
                                      ir_entity const *const entity)
{
	be_gas_emit_entity(entity);
	switch (kind) {
	case X86_IMM_ADDR:
	case X86_IMM_PCREL:
		return;
	case X86_IMM_GOTPCREL: be_emit_cstring("@GOTPCREL"); return;
	case X86_IMM_VALUE:
	case X86_IMM_TLS_IE:
	case X86_IMM_TLS_LE:
	case X86_IMM_PICBASE_REL:
	case X86_IMM_FRAMEOFFSET:
	case X86_IMM_GOT:
	case X86_IMM_GOTOFF:
	case X86_IMM_PLT:
		break;
	}
	panic("unexpected or invalid immediate kind");
}

static void amd64_emit_immediate64(const amd64_imm64_t *const imm)
{
	if (imm->kind == X86_IMM_VALUE) {
		assert(imm->entity == NULL);
		be_emit_irprintf("0x%" PRIX64, imm->offset);
		return;
	}
	emit_relocation_no_offset(imm->kind, imm->entity);
	if (imm->offset != 0)
		be_emit_irprintf("%+" PRId64, imm->offset);
}

static void amd64_emit_immediate32(bool const prefix,
                                   x86_imm32_t const *const imm)
{
	if (prefix)
		be_emit_char('$');
	if (imm->kind == X86_IMM_VALUE) {
		assert(imm->entity == NULL);
		be_emit_irprintf("%" PRId32, imm->offset);
		return;
	}
	emit_relocation_no_offset(imm->kind, imm->entity);
	if (imm->offset != 0)
		be_emit_irprintf("%+" PRId32, imm->offset);
}

static bool is_fp_relative(const ir_entity *entity)
{
	ir_type *owner = get_entity_owner(entity);
	return is_frame_type(owner) || owner == layout->arg_type;
}

static void amd64_emit_addr(const ir_node *const node,
                            const amd64_addr_t *const addr)
{
	ir_entity *entity = addr->immediate.entity;
	if (entity != NULL) {
		assert(addr->immediate.kind != X86_IMM_VALUE);
		if (is_fp_relative(entity)) {
			entity = NULL; /* only emit offset for frame entities */
		} else {
			emit_relocation_no_offset(addr->immediate.kind, entity);
		}
	}

	int32_t offset      = addr->immediate.offset;
	uint8_t base_input  = addr->base_input;
	uint8_t index_input = addr->index_input;
	if (offset != 0 || (entity == NULL && base_input == NO_INPUT
	                    && index_input == NO_INPUT)) {
		if (entity != NULL) {
			be_emit_irprintf("%+" PRId32, offset);
		} else {
			assert(addr->immediate.kind == X86_IMM_VALUE);
			be_emit_irprintf("%" PRId32, offset);
		}
	}

	if (base_input != NO_INPUT || index_input != NO_INPUT) {
		be_emit_char('(');

		if (base_input == RIP_INPUT) {
			be_emit_cstring("%rip");
		} else if (base_input != NO_INPUT) {
			const arch_register_t *reg
				= arch_get_irn_register_in(node, base_input);
			emit_register(reg);
		}

		if (index_input != NO_INPUT) {
			be_emit_char(',');
			const arch_register_t *reg
				= arch_get_irn_register_in(node, index_input);
			emit_register(reg);

			unsigned scale = addr->log_scale;
			if (scale > 0)
				be_emit_irprintf(",%u", 1 << scale);
		}
		be_emit_char(')');
	}
}

static void amd64_emit_am(const ir_node *const node, bool indirect_star)
{
	const amd64_addr_attr_t *const attr = get_amd64_addr_attr_const(node);

	switch ((amd64_op_mode_t)attr->base.op_mode) {
	case AMD64_OP_REG_IMM: {
		const amd64_binop_addr_attr_t *const binop_attr
			= (const amd64_binop_addr_attr_t*)attr;
		amd64_emit_immediate32(true, &binop_attr->u.immediate);
		be_emit_cstring(", ");
		const arch_register_t *reg = arch_get_irn_register_in(node, 0);
		emit_register_mode(reg, binop_attr->base.insn_mode);
		return;
	}
	case AMD64_OP_REG_REG: {
		const amd64_addr_attr_t *const addr_attr
			= (const amd64_addr_attr_t*)attr;
		const arch_register_t *reg0 = arch_get_irn_register_in(node, 0);
		const arch_register_t *reg1 = arch_get_irn_register_in(node, 1);
		emit_register_mode(reg1, addr_attr->insn_mode);
		be_emit_cstring(", ");
		emit_register_mode(reg0, addr_attr->insn_mode);
		return;
	}
	case AMD64_OP_REG_ADDR: {
		const amd64_binop_addr_attr_t *const binop_attr
			= (const amd64_binop_addr_attr_t*)attr;
		amd64_emit_addr(node, &attr->addr);
		be_emit_cstring(", ");
		const arch_register_t *reg
			= arch_get_irn_register_in(node, binop_attr->u.reg_input);
		emit_register_mode(reg, binop_attr->base.insn_mode);
		return;
	}
	case AMD64_OP_ADDR_IMM: {
		const amd64_binop_addr_attr_t *const binop_attr
			= (const amd64_binop_addr_attr_t*)attr;
		amd64_emit_immediate32(true, &binop_attr->u.immediate);
		be_emit_cstring(", ");
		amd64_emit_addr(node, &attr->addr);
		return;
	}
	case AMD64_OP_ADDR:
		if (indirect_star)
			be_emit_char('*');
		amd64_emit_addr(node, &attr->addr);
		return;
	case AMD64_OP_ADDR_REG: {
		amd64_binop_addr_attr_t const *const binop_attr = (amd64_binop_addr_attr_t const*)attr;
		arch_register_t const *const reg = arch_get_irn_register_in(node, binop_attr->u.reg_input);
		emit_register_mode(reg, binop_attr->base.insn_mode);
		be_emit_cstring(", ");
		amd64_emit_addr(node, &attr->addr);
		return;
	}
	case AMD64_OP_REG: {
		if (indirect_star)
			be_emit_char('*');
		const arch_register_t *reg = arch_get_irn_register_in(node, 0);
		emit_register_mode(reg, attr->insn_mode);
		return;
	}
	case AMD64_OP_UNOP_IMM32:
		amd64_emit_immediate32(false, &attr->addr.immediate);
		return;

	case AMD64_OP_IMM32:
	case AMD64_OP_IMM64:
	case AMD64_OP_NONE:
	case AMD64_OP_SHIFT_REG:
	case AMD64_OP_SHIFT_IMM:
		break;
	}
	panic("invalid op_mode");
}

static amd64_insn_mode_t get_amd64_insn_mode(const ir_node *node)
{
	if (is_amd64_mov_imm(node)) {
		const amd64_movimm_attr_t *const attr
			= get_amd64_movimm_attr_const(node);
		return attr->insn_mode;
	} else {
		amd64_addr_attr_t const *const attr = get_amd64_addr_attr_const(node);
		return attr->insn_mode;
	}
}


static void emit_shiftop(const ir_node *const node)
{
	amd64_shift_attr_t const *const attr = get_amd64_shift_attr_const(node);

	switch (attr->base.op_mode) {
	case AMD64_OP_SHIFT_IMM: {
		be_emit_irprintf("$0x%X, ", attr->immediate);
		const arch_register_t *reg = arch_get_irn_register_in(node, 0);
		emit_register_mode(reg, attr->insn_mode);
		return;
	}
	case AMD64_OP_SHIFT_REG: {
		const arch_register_t *reg0 = arch_get_irn_register_in(node, 0);
		const arch_register_t *reg1 = arch_get_irn_register_in(node, 1);
		emit_register_mode(reg1, INSN_MODE_8);
		be_emit_cstring(", ");
		emit_register_mode(reg0, attr->insn_mode);
		return;
	}
	default:
		break;
	}
	panic("invalid op_mode for shiftop");
}

void amd64_emitf(ir_node const *const node, char const *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);

	be_emit_char('\t');
	for (;;) {
		char const *start = fmt;

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
			continue;
		}

		if (*fmt == '\0')
			break;

		++fmt;
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

			case '%':
				be_emit_char('%');
				break;

			case 'A':
				switch (*fmt++) {
				case 'M':
					amd64_emit_am(node, mod & EMIT_INDIRECT_STAR);
					break;
				default: {
					amd64_addr_attr_t const *const attr
						= get_amd64_addr_attr_const(node);
					amd64_emit_addr(node, &attr->addr);
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

			case 'L': {
				ir_node *const block = get_cfop_target_block(node);
				be_gas_emit_block_name(block);
				break;
			}

			case 'P': {
				x86_condition_code_t cc;
				if (*fmt == 'X') {
					++fmt;
					cc = (x86_condition_code_t)va_arg(ap, int);
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
					emit_register_mode(reg, INSN_MODE_32);
				} else if (mod & EMIT_CONV_DEST) {
					amd64_insn_mode_t src_mode  = get_amd64_insn_mode(node);
					amd64_insn_mode_t dest_mode = src_mode == INSN_MODE_64
					                            ? INSN_MODE_64 : INSN_MODE_32;
					emit_register_mode(reg, dest_mode);
				} else {
					amd64_insn_mode_t insn_mode = get_amd64_insn_mode(node);
					emit_register_mode(reg, insn_mode);
				}
				break;
			}

			case 'M': {
				if (*fmt == 'S') {
					++fmt;
					const amd64_shift_attr_t *attr
						= get_amd64_shift_attr_const(node);
					amd64_emit_insn_mode_suffix(attr->insn_mode);
				} else if (*fmt == 'M') {
					++fmt;
					const amd64_movimm_attr_t *attr
						= get_amd64_movimm_attr_const(node);
					amd64_emit_insn_mode_suffix(attr->insn_mode);
				} else if (*fmt == 'X') {
					++fmt;
					amd64_addr_attr_t const *const attr
						= get_amd64_addr_attr_const(node);
					amd64_emit_xmm_mode_suffix(attr->insn_mode);
				} else {
					amd64_addr_attr_t const *const attr
						= get_amd64_addr_attr_const(node);
					amd64_emit_insn_mode_suffix(attr->insn_mode);
				}
				break;
			}

			case 'd': {
				int const num = va_arg(ap, int);
				be_emit_irprintf("%d", num);
				break;
			}

			case 's': {
				char const *const str = va_arg(ap, char const*);
				be_emit_string(str);
				break;
			}

			case 'u': {
				unsigned const num = va_arg(ap, unsigned);
				be_emit_irprintf("%u", num);
				break;
			}

			default:
unknown:
				panic("unknown format conversion");
		}
	}

	be_emit_finish_line_gas(node);
	va_end(ap);
}

/**
 * Returns the next block in a block schedule.
 */
static ir_node *sched_next_block(const ir_node *block)
{
    return (ir_node*)get_irn_link(block);
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
	case '\0':
		name = mode != NULL ? get_register_name_ir_mode(reg, mode) : reg->name;
		break;
	case  'b': name = get_register_name_8bit(reg); break;
	case  'h': name = get_register_name_8bit_high(reg); break;
	case  'w': name = get_register_name_16bit(reg); break;
	case  'k': name = get_register_name_32bit(reg); break;
	case  'q': name = reg->name; break;
	// gcc also knows 'x' V4SFmode, 't' V8SFmode, 'y' "st(0)" instead of "st",
	// 'd' duplicate operand for AVX instruction
	default:
		panic("invalid asm op modifier");
	}
	be_emit_char('%');
	be_emit_string(name);
}

static void emit_amd64_asm_operand(ir_node const *const node, char const modifier, unsigned const pos)
{
	switch (modifier) {
	case '\0':
	case 'b':
	case 'h':
	case 'k':
	case 'q':
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
		emit_amd64_asm_register(reg, modifier, op->u.mode);
		return;
	}

	case ASM_OP_OUT_REG: {
		arch_register_t const *const reg = arch_get_irn_register_out(node, op->inout_pos);
		emit_amd64_asm_register(reg, modifier, op->u.mode);
		return;
	}

	case ASM_OP_MEMORY: {
		arch_register_t const *const reg = arch_get_irn_register_in(node, op->inout_pos);
		be_emit_irprintf("(%%%s)", reg->name);
		return;
	}

	case ASM_OP_IMMEDIATE: {
		amd64_emit_immediate32(true, &op->u.imm32);
		return;
	}
	}
	panic("invalid asm operand kind");
}

static void emit_amd64_asm(const ir_node *node)
{
	be_emit_asm(node, emit_amd64_asm_operand);
}

/**
 * Emit a Jmp.
 */
static void emit_amd64_jmp(const ir_node *node)
{
	ir_node *block, *next_block;

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	/* we have a block schedule */
	next_block = sched_next_block(block);
	if (get_cfop_target_block(node) != next_block) {
		amd64_emitf(node, "jmp %L");
	} else if (be_options.verbose_asm) {
		amd64_emitf(node, "/* fallthrough to %L */");
	}
}

static void emit_amd64_jmp_switch(const ir_node *node)
{
	const amd64_switch_jmp_attr_t *attr = get_amd64_switch_jmp_attr_const(node);

	amd64_emitf(node, "jmp *%E(,%^S0,8)", attr->table_entity);
	be_emit_jump_table(node, attr->table, attr->table_entity,
	                   get_cfop_target_block);
}

/**
 * Emit a Compare with conditional branch.
 */
static void emit_amd64_jcc(const ir_node *irn)
{
	const ir_node         *proj_true  = NULL;
	const ir_node         *proj_false = NULL;
	const ir_node         *block;
	const ir_node         *next_block;
	const amd64_cc_attr_t *attr = get_amd64_cc_attr_const(irn);
	x86_condition_code_t   cc   = attr->cc;

	foreach_out_edge(irn, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		unsigned nr = get_Proj_num(proj);
		if (nr == pn_Cond_true) {
			proj_true = proj;
		} else {
			proj_false = proj;
		}
	}

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(irn);

	/* we have a block schedule */
	next_block = sched_next_block(block);

	if (get_cfop_target_block(proj_true) == next_block) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj_true;

		proj_true  = proj_false;
		proj_false = t;
		cc         = x86_negate_condition_code(cc);
	}

	if (cc & x86_cc_float_parity_cases) {
		/* Some floating point comparisons require a test of the parity flag,
		 * which indicates that the result is unordered */
		if (cc & x86_cc_negated) {
			amd64_emitf(proj_true, "jp %L");
		} else {
			amd64_emitf(proj_false, "jp %L");
		}
	}

	/* emit the true proj */
	amd64_emitf(proj_true, "j%PX %L", (int)cc);

	if (get_cfop_target_block(proj_false) == next_block) {
		if (be_options.verbose_asm)
			amd64_emitf(proj_false, "/* fallthrough to %L */");
	} else  {
		amd64_emitf(proj_false, "jmp %L");
	}
}

static void emit_amd64_mov_gp(const ir_node *node)
{
	const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
	switch (attr->insn_mode) {
	case INSN_MODE_8:  amd64_emitf(node, "movzbq %AM, %^D0"); return;
	case INSN_MODE_16: amd64_emitf(node, "movzwq %AM, %^D0"); return;
	case INSN_MODE_32: amd64_emitf(node, "movl %AM, %3D0");   return;
	case INSN_MODE_64: amd64_emitf(node, "movq %AM, %^D0");   return;
	case INSN_MODE_128:
	case INSN_MODE_INVALID:
		break;
	}
	panic("invalid insn mode");
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
	be_gas_begin_block(block, true);

	sched_foreach(block, node) {
		be_emit_node(node);
	}
}


/**
 * Sets labels for control flow nodes (jump target)
 * TODO: Jump optimization
 */
static void amd64_gen_labels(ir_node *block, void *env)
{
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);
	(void) env;

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block);
	}
}

void amd64_emit_function(ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);
	ir_node  **blk_sched;
	size_t i, n;

	layout = be_get_irg_stack_layout(irg);

	/* register all emitter functions */
	amd64_register_emitters();

	blk_sched = be_create_block_schedule(irg);

	be_gas_emit_function_prolog(entity, 4, NULL);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_block_walk_graph(irg, amd64_gen_labels, NULL, NULL);

	n = ARR_LEN(blk_sched);
	for (i = 0; i < n; i++) {
		ir_node *block = blk_sched[i];
		ir_node *next  = (i + 1) < n ? blk_sched[i+1] : NULL;

		set_irn_link(block, next);
	}

	for (i = 0; i < n; ++i) {
		ir_node *block = blk_sched[i];
		amd64_gen_block(block);
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	be_gas_emit_function_epilog(entity);
}
