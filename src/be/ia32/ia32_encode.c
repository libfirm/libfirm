/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Matthias Braun
 */

/**
 * @file
 * @brief       ia32 binary encoding/emissiong
 * @author      Matthias Braun, Christoph Mallon
 */
#include "ia32_encode.h"

#include "bearch.h"
#include "beblocksched.h"
#include "beemithlp.h"
#include "begnuas.h"
#include "bejit.h"
#include "besched.h"
#include "execfreq.h"
#include "gen_ia32_emitter.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_architecture.h"
#include "ia32_bearch_t.h"
#include "ia32_emitter.h"
#include "ia32_new_nodes.h"
#include "irnodehashmap.h"
#include "x86_node.h"
#include <stdint.h>

static ir_nodehashmap_t block_fragmentnum;

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

typedef enum reg_modifier {
	REG_LOW  = 0,
	REG_HIGH = 4
} reg_modifier_t;

/** create R/M encoding for ModR/M */
static uint8_t ENC_RM(unsigned const regnum, reg_modifier_t const modifier)
{
	return regnum | modifier;
}

/** create REG encoding for ModR/M */
static uint8_t ENC_REG(unsigned const regnum, reg_modifier_t const modifier)
{
	return (regnum | modifier) << 3;
}

/** create encoding for a SIB byte */
static uint8_t ENC_SIB(uint8_t scale, uint8_t index, uint8_t base)
{
	return scale << 6 | index << 3 | base;
}

/**
 * Emit address of an entity. If @p is_relative is true then a relative
 * offset from behind the address to the entity is created.
 */
static void enc_relocation(x86_imm32_t const *const imm)
{
	ir_entity *entity = imm->entity;
	int32_t    offset = imm->offset;
	if (entity == NULL) {
		be_emit32(offset);
		return;
	}

	be_emit_reloc_entity(4, imm->kind, entity, offset);
}

static void enc_jmp_destination(ir_node const *const cfop)
{
	assert(get_irn_mode(cfop) == mode_X);
	ir_node const *const dest_block = be_emit_get_cfop_target(cfop);
	unsigned const fragment_num
		= PTR_TO_INT(ir_nodehashmap_get(void, &block_fragmentnum, dest_block));
	be_emit_reloc_fragment(4, IA32_RELOCATION_RELJUMP, fragment_num, -4);
}

/* end emit routines, all emitters following here should only use the functions
   above. */

/** Create a ModR/M byte for src1,src2 registers */
static void enc_modrr(const arch_register_t *src1,
                      const arch_register_t *src2)
{
	unsigned char modrm = MOD_REG;
	modrm |= ENC_RM(src1->encoding, REG_LOW);
	modrm |= ENC_REG(src2->encoding, REG_LOW);
	be_emit8(modrm);
}

/** Create a ModR/M8 byte for src1,src2 registers */
static void enc_modrr8(reg_modifier_t high_part1, const arch_register_t *src1,
                       reg_modifier_t high_part2, const arch_register_t *src2)
{
	unsigned char modrm = MOD_REG;
	modrm |= ENC_RM(src1->encoding, high_part1);
	modrm |= ENC_REG(src2->encoding, high_part2);
	be_emit8(modrm);
}

/** Create a ModR/M byte for one register and extension */
static void enc_modru(const arch_register_t *reg, unsigned ext)
{
	unsigned char modrm = MOD_REG;
	assert(ext <= 7);
	modrm |= ENC_RM(reg->encoding, REG_LOW);
	modrm |= ENC_REG(ext, REG_LOW);
	be_emit8(modrm);
}

/** Create a ModR/M8 byte for one register */
static void enc_modrm8(reg_modifier_t high_part, const arch_register_t *reg)
{
	unsigned char modrm = MOD_REG;
	assert(reg->encoding < 4);
	modrm |= ENC_RM(reg->encoding, high_part);
	modrm |= MOD_REG;
	be_emit8(modrm);
}

static bool ia32_is_8bit_imm(ia32_immediate_attr_t const *const imm)
{
	return !imm->imm.entity && ia32_is_8bit_val(imm->imm.offset);
}

static ir_node const *get_irn_n_reg(ir_node const *const node, int const pos)
{
	ir_node *const in = get_irn_n(node, pos);
	return is_ia32_NoReg_GP(in) ? NULL : in;
}

/**
 * Emit an address mode.
 *
 * @param reg   content of the reg field: either a register index or an opcode extension
 * @param node  the node
 */
static void enc_mod_am(unsigned reg, const ir_node *node)
{
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	ir_entity const *const entity = attr->addr.immediate.entity;
	int32_t          const offset = attr->addr.immediate.offset;

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
		unsigned               const log_scale = attr->addr.log_scale;
		/* R/M set to ESP means SIB in 32bit mode. */
		modrm   |= ENC_RM(0x04, REG_LOW);
		sib      = ENC_SIB(log_scale, reg_index->encoding, base_enc);
		emitsib = true;
	} else if (base_enc == 0x04) {
		/* for the above reason we are forced to emit a SIB when base is ESP.
		 * Only the base is used, index must be ESP too, which means no index.
		 */
		modrm   |= ENC_RM(0x04, REG_LOW);
		sib      = ENC_SIB(0, 0x04, 0x04);
		emitsib  = true;
	} else {
		modrm |= ENC_RM(base_enc, REG_LOW);
	}

	/* We are forced to emit an 8bit offset as EBP base without offset is a
	 * special case for SIB without base register. */
	if (base_enc == 0x05 && emitoffs == 0) {
		modrm    |= MOD_IND_BYTE_OFS;
		emitoffs  = 8;
	}

	modrm |= ENC_REG(reg, REG_LOW);

	be_emit8(modrm);
	if (emitsib)
		be_emit8(sib);

	/* emit displacement */
	if (emitoffs == 8) {
		be_emit8((unsigned) offset);
	} else if (emitoffs == 32) {
		enc_relocation(&attr->addr.immediate);
	}
}

static void enc_imm32(ir_node const *const node)
{
	const ia32_immediate_attr_t *attr = get_ia32_immediate_attr_const(node);
	enc_relocation(&attr->imm);
}

static void enc_imm(ia32_immediate_attr_t const *const attr,
                    x86_insn_size_t size)
{
	switch (size) {
	case X86_SIZE_8:  be_emit8(attr->imm.offset);  return;
	case X86_SIZE_16: be_emit16(attr->imm.offset); return;
	case X86_SIZE_32: enc_relocation(&attr->imm);  return;
	case X86_SIZE_64:
	case X86_SIZE_80:
	case X86_SIZE_128:
		break;
	}
	panic("Invalid size");
}

static void enc_mov(arch_register_t const *const src, arch_register_t const *const dst)
{
	be_emit8(0x88 | OP_MEM_SRC | OP_16_32); // movl %src, %dst
	enc_modrr(src, dst);
}

static void enc_xchg(arch_register_t const *const src, arch_register_t const *const dst)
{
	if (src->index == REG_GP_EAX) {
		be_emit8(0x90 + dst->encoding); // xchgl %eax, %dst
	} else if (dst->index == REG_GP_EAX) {
		be_emit8(0x90 + src->encoding); // xchgl %src, %eax
	} else {
		be_emit8(0x86 | OP_16_32); // xchgl %src, %dst
		enc_modrr(src, dst);
	}
}

static void enc_copy(const ir_node *copy)
{
	const arch_register_t *in  = arch_get_irn_register_in(copy, 0);
	const arch_register_t *out = arch_get_irn_register_out(copy, 0);

	if (in == out)
		return;
	/* copies of fp nodes aren't real... */
	if (in->cls == &ia32_reg_classes[CLASS_ia32_fp])
		return;

	assert(in->cls == &ia32_reg_classes[CLASS_ia32_gp]);
	enc_mov(in, out);
}

static void enc_perm(const ir_node *node)
{
	arch_register_t       const *const reg0 = arch_get_irn_register_out(node, 0);
	arch_register_t       const *const reg1 = arch_get_irn_register_out(node, 1);
	arch_register_class_t const *const cls  = reg0->cls;

	assert(cls == reg1->cls && "Register class mismatch at Perm");

	if (cls == &ia32_reg_classes[CLASS_ia32_gp]) {
		enc_xchg(reg0, reg1);
	} else if (cls == &ia32_reg_classes[CLASS_ia32_xmm]) {
		panic("unimplemented"); // TODO implement
		//ia32_emitf(NULL, "xorpd %#R, %#R", reg1, reg0);
		//ia32_emitf(NULL, "xorpd %#R, %#R", reg0, reg1);
		//ia32_emitf(node, "xorpd %#R, %#R", reg1, reg0);
	} else if (cls == &ia32_reg_classes[CLASS_ia32_fp]) {
		/* is a NOP */
	} else {
		panic("unexpected register class in be_Perm (%+F)", node);
	}
}

static void enc_xor0(const ir_node *node)
{
	be_emit8(0x31);
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_Xor0_res);
	enc_modrr(out, out);
}

static void enc_mov_const(const ir_node *node)
{
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_Const_res);
	be_emit8(0xB8 + out->encoding);
	enc_imm32(node);
}

void ia32_enc_simple(uint8_t const opcode)
{
	be_emit8(opcode);
}

void ia32_enc_unop(ir_node const *const node, uint8_t const code,
                   uint8_t const ext, int const input)
{
	be_emit8(code);
	if (get_ia32_op_type(node) == ia32_Normal) {
		const arch_register_t *in = arch_get_irn_register_in(node, input);
		enc_modru(in, ext);
	} else {
		enc_mod_am(ext, node);
	}
}

void ia32_enc_unop_mem(ir_node const *const node, uint8_t const code,
                       uint8_t const ext)
{
	x86_insn_size_t const size = get_ia32_attr_const(node)->size;
	if (size == X86_SIZE_16)
		be_emit8(0x66);
	be_emit8(size == X86_SIZE_8 ? code : code | OP_16_32);
	enc_mod_am(ext, node);
}

static bool use_eax_short_form(ir_node const *const node)
{
	return
		get_ia32_op_type(node) == ia32_Normal &&
		arch_get_irn_register_in(node, n_ia32_binary_left)->index == REG_GP_EAX;
}

static void enc_binop_reg(ir_node const *const node, unsigned char const code, ir_node const *const right)
{
	be_emit8(code);
	arch_register_t const *const dst = arch_get_irn_register_in(node, n_ia32_binary_left);
	if (get_ia32_op_type(node) == ia32_Normal) {
		arch_register_t const *const src = arch_get_irn_register(right);
		enc_modrr(src, dst);
	} else {
		enc_mod_am(dst->encoding, node);
	}
}

void ia32_enc_binop(ir_node const *const node, unsigned const code)
{
	x86_insn_size_t size = get_ia32_attr_const(node)->size;
	if (size == X86_SIZE_16)
		be_emit8(0x66);

	unsigned       op    = size == X86_SIZE_8 ? OP_8 : OP_16_32;
	ir_node *const right = get_irn_n(node, n_ia32_binary_right);
	if (is_ia32_Immediate(right)) {
		ia32_immediate_attr_t const *const attr = get_ia32_immediate_attr_const(right);
		/* Try to use the short form with 8bit sign extended immediate. */
		if (op != OP_8 && ia32_is_8bit_imm(attr)) {
			op   = OP_16_32_IMM8;
			size = X86_SIZE_8;
		}

		/* Emit the main opcode. */
		if (op != OP_16_32_IMM8 && use_eax_short_form(node)) {
			be_emit8(code << 3 | OP_EAX | op);
		} else {
			ia32_enc_unop(node, 0x80 | op, code, n_ia32_binary_left);
		}

		enc_imm(attr, size);
	} else {
		enc_binop_reg(node, code << 3 | OP_MEM_SRC | op, right);
	}
}

void ia32_enc_binop_mem(ir_node const *const node, unsigned const code)
{
	x86_insn_size_t size = get_ia32_attr_const(node)->size;
	if (size == X86_SIZE_16)
		be_emit8(0x66);

	unsigned       op  = size == X86_SIZE_8 ? OP_8 : OP_16_32;
	ir_node *const val = get_irn_n(node, n_ia32_unary_op);
	if (is_ia32_Immediate(val)) {
		ia32_immediate_attr_t const *const attr = get_ia32_immediate_attr_const(val);
		/* Try to use the short form with 8bit sign extended immediate. */
		if (op != OP_8 && ia32_is_8bit_imm(attr)) {
			op   = OP_16_32_IMM8;
			size = X86_SIZE_8;
		}

		/* Emit the main opcode. */
		be_emit8(0x80 | op);
		enc_mod_am(code, node);

		enc_imm(attr, size);
	} else {
		be_emit8(code << 3 | op);
		enc_mod_am(arch_get_irn_register(val)->encoding, node);
	}
}

void ia32_enc_shiftop(ir_node const *const node, uint8_t const ext)
{
	arch_register_t const *const out
		= arch_get_irn_register_out(node, pn_ia32_res);
	ir_node         const *const count = get_irn_n(node, 1);
	if (is_ia32_Immediate(count)) {
		int32_t const offset = get_ia32_immediate_attr_const(count)->imm.offset;
		if (offset == 1) {
			be_emit8(0xD1);
			enc_modru(out, ext);
		} else {
			be_emit8(0xC1);
			enc_modru(out, ext);
			be_emit8(offset);
		}
	} else {
		be_emit8(0xD3);
		enc_modru(out, ext);
	}
}

void ia32_enc_shiftop_mem(ir_node const *const node, uint8_t const ext)
{
	x86_insn_size_t const size = get_ia32_attr_const(node)->size;
	if (size == X86_SIZE_16)
		be_emit8(0x66);
	ir_node const *const count = get_irn_n(node, 1);
	if (is_ia32_Immediate(count)) {
		int32_t const offset = get_ia32_immediate_attr_const(count)->imm.offset;
		if (offset == 1) {
			be_emit8(size == X86_SIZE_8 ? 0xD0 : 0xD1);
			enc_mod_am(ext, node);
		} else {
			be_emit8(size == X86_SIZE_8 ? 0xC0 : 0xC1);
			enc_mod_am(ext, node);
			be_emit8(offset);
		}
	} else {
		be_emit8(size == X86_SIZE_8 ? 0xD2 : 0xD3);
		enc_mod_am(ext, node);
	}
}

static void enc_shld(const ir_node *node)
{
	const arch_register_t *in  = arch_get_irn_register_in(node, n_ia32_ShlD_val_low);
	const arch_register_t *out = arch_get_irn_register_out(node, pn_ia32_ShlD_res);
	ir_node *count = get_irn_n(node, n_ia32_ShlD_count);
	be_emit8(0x0F);
	if (is_ia32_Immediate(count)) {
		be_emit8(0xA4);
		enc_modrr(out, in);
		be_emit8(get_ia32_immediate_attr_const(count)->imm.offset);
	} else {
		be_emit8(0xA5);
		enc_modrr(out, in);
	}
}

static void enc_shrd(const ir_node *node)
{
	const arch_register_t *in  = arch_get_irn_register_in(node, n_ia32_ShrD_val_low);
	const arch_register_t *out = arch_get_irn_register_out(node, pn_ia32_ShrD_res);
	ir_node *count = get_irn_n(node, n_ia32_ShrD_count);
	be_emit8(0x0F);
	if (is_ia32_Immediate(count)) {
		be_emit8(0xAC);
		enc_modrr(out, in);
		be_emit8(get_ia32_immediate_attr_const(count)->imm.offset);
	} else {
		be_emit8(0xAD);
		enc_modrr(out, in);
	}
}

static void enc_sbb0(ir_node const *const node)
{
	be_emit8(0x1B);
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_Sbb0_res);
	enc_modrr(out, out);
}

/**
 * binary emitter for setcc.
 */
static void enc_setcc(const ir_node *node)
{
	const arch_register_t *dreg = arch_get_irn_register_out(node, pn_ia32_Setcc_res);

	x86_condition_code_t const cc
		= ia32_determine_final_cc(node, n_ia32_Setcc_eflags);
	if (cc & x86_cc_float_parity_cases) {
		if (cc & x86_cc_negated) {
			/* set%PNC <dreg */
			be_emit8(0x0F);
			be_emit8(0x90 | pnc2cc(cc));
			enc_modrm8(REG_LOW, dreg);

			/* setp >dreg */
			be_emit8(0x0F);
			be_emit8(0x9A);
			enc_modrm8(REG_HIGH, dreg);

			/* orb %>dreg, %<dreg */
			be_emit8(0x08);
			enc_modrr8(REG_LOW, dreg, REG_HIGH, dreg);
		} else {
			 /* set%PNC <dreg */
			be_emit8(0x0F);
			be_emit8(0x90 | pnc2cc(cc));
			enc_modrm8(REG_LOW, dreg);

			/* setnp >dreg */
			be_emit8(0x0F);
			be_emit8(0x9B);
			enc_modrm8(REG_HIGH, dreg);

			/* andb %>dreg, %<dreg */
			be_emit8(0x20);
			enc_modrr8(REG_LOW, dreg, REG_HIGH, dreg);
		}
	} else {
		/* set%PNC <dreg */
		be_emit8(0x0F);
		be_emit8(0x90 | pnc2cc(cc));
		enc_modrm8(REG_LOW, dreg);
	}
}

static void enc_unop_reg(ir_node const *const node, uint8_t const code,
                         int const input)
{
	arch_register_t const *const out
		= arch_get_irn_register_out(node, pn_ia32_res);
	ia32_enc_unop(node, code, out->encoding, input);
}

void ia32_enc_0f_unop_reg(ir_node const *const node, uint8_t const code,
                          int const input)
{
	be_emit8(0x0F);
	enc_unop_reg(node, code, input);
}

static void enc_bswap(ir_node const *const node)
{
	be_emit8(0x0F);
	enc_modru(arch_get_irn_register_out(node, pn_ia32_Bswap_res), 1);
}

static void enc_bt(ir_node const *const node)
{
	be_emit8(0x0F);
	arch_register_t const *const lreg  = arch_get_irn_register_in(node, n_ia32_Bt_left);
	ir_node         const *const right = get_irn_n(node, n_ia32_Bt_right);
	if (is_ia32_Immediate(right)) {
		ia32_immediate_attr_t const *const attr = get_ia32_immediate_attr_const(right);
		assert(ia32_is_8bit_imm(attr));
		be_emit8(0xBA);
		enc_modru(lreg, 4);
		be_emit8(attr->imm.offset);
	} else {
		be_emit8(0xA3);
		enc_modrr(lreg, arch_get_irn_register(right));
	}
}

static void enc_cmovcc(const ir_node *node)
{
	be_emit8(0x0F);
	ir_node       const *const val_true = get_irn_n(node, n_ia32_CMovcc_val_true);
	x86_condition_code_t const cc
		= ia32_determine_final_cc(node, n_ia32_CMovcc_eflags);
	if (cc & x86_cc_float_parity_cases)
		panic("cmov can't handle parity float cases");
	enc_binop_reg(node, 0x40 | pnc2cc(cc), val_true);
}

static void enc_test(ir_node const *const node)
{
	x86_insn_size_t const size = get_ia32_attr_const(node)->size;
	if (size == X86_SIZE_16)
		be_emit8(0x66);

	unsigned const op    = size == X86_SIZE_8 ? OP_8 : OP_16_32;
	ir_node *const right = get_irn_n(node, n_ia32_Test_right);
	if (is_ia32_Immediate(right)) {
		/* Emit the main opcode. */
		if (use_eax_short_form(node)) {
			be_emit8(0xA8 | op);
		} else {
			ia32_enc_unop(node, 0xF6, 0, n_ia32_Test_left);
		}

		enc_imm(get_ia32_immediate_attr_const(right), size);
	} else {
		enc_binop_reg(node, 0x84 | op, right);
	}
}

static void enc_imulimm(const ir_node *node)
{
	ir_node               const *const right = get_irn_n(node, n_ia32_IMul_right);
	ia32_immediate_attr_t const *const attr  = get_ia32_immediate_attr_const(right);
	bool                         const imm8  = ia32_is_8bit_imm(attr);
	enc_unop_reg(node, 0x69 | (imm8 ? OP_IMM8 : 0), n_ia32_IMul_left);
	enc_imm(attr, imm8 ? 8 : 32);
}

static void enc_dec(const ir_node *node)
{
	const arch_register_t *out = arch_get_irn_register_out(node, pn_ia32_Dec_res);
	be_emit8(0x48 + out->encoding);
}

static void enc_inc(const ir_node *node)
{
	const arch_register_t *out = arch_get_irn_register_out(node, pn_ia32_Inc_res);
	be_emit8(0x40 + out->encoding);
}

static void enc_ldtls(const ir_node *node)
{
	be_emit8(0x65); // gs:
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_LdTls_res);
	if (out->index == REG_GP_EAX) {
		be_emit8(0xA1); // movl 0, %eax
	} else {
		be_emit8(0x88 | OP_MEM_SRC | OP_16_32); // movl 0, %reg
		be_emit8(MOD_IND | ENC_REG(out->encoding, REG_LOW) | ENC_RM(0x05, REG_LOW));
	}
	be_emit32(0);
}

/**
 * Emit a Lea.
 */
static void enc_lea(const ir_node *node)
{
	be_emit8(0x8D);
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_Lea_res);
	enc_mod_am(out->encoding, node);
}

/* helper function for enc_minus64bit */
static void enc_helper_neg(const arch_register_t *reg)
{
	be_emit8(0xF7); // negl %reg
	enc_modru(reg, 3);
}

/* helper function for enc_minus64bit */
static void enc_helper_sbb0(const arch_register_t *reg)
{
	be_emit8(0x80 | OP_16_32_IMM8); // sbbl $0, %reg
	enc_modru(reg, 3);
	be_emit8(0);
}

/* helper function for enc_minus64bit */
static void enc_helper_sbb(const arch_register_t *src, const arch_register_t *dst)
{
	be_emit8(0x1B); // sbbl %src, %dst
	enc_modrr(src, dst);
}

/* helper function for enc_minus64bit */
static void enc_helper_zero(const arch_register_t *reg)
{
	be_emit8(0x33); // xorl %reg, %reg
	enc_modrr(reg, reg);
}

static void enc_minus64(const ir_node *node)
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
			enc_xchg(in_lo, in_hi);
			goto normal_neg;
		} else {
			/* a -> b, b -> d */
			enc_mov(in_hi, out_hi);
			enc_mov(in_lo, out_lo);
			goto normal_neg;
		}
	} else {
		if (out_hi == in_lo) {
			/* a -> c, b -> a */
			enc_mov(in_lo, out_lo);
			goto zero_neg;
		} else if (out_hi == in_hi) {
			/* a -> c, b -> b */
			enc_mov(in_lo, out_lo);
			goto normal_neg;
		} else {
			/* a -> c, b -> d */
			enc_mov(in_lo, out_lo);
			goto zero_neg;
		}
	}

normal_neg:
	enc_helper_neg( out_hi);
	enc_helper_neg( out_lo);
	enc_helper_sbb0(out_hi);
	return;

zero_neg:
	enc_helper_zero(out_hi);
	enc_helper_neg( out_lo);
	enc_helper_sbb( in_hi, out_hi);
}

/**
 * Emits a MOV out, [MEM].
 */
static void enc_load(const ir_node *node)
{
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_Load_res);

	if (out->index == REG_GP_EAX) {
		ir_node const *const base = get_irn_n_reg(node, n_ia32_base);
		ir_node const *const idx  = get_irn_n_reg(node, n_ia32_index);
		if (!base && !idx) {
			/* load from constant address to EAX can be encoded
			   as 0xA1 [offset] */
			be_emit8(0xA1);
			ia32_attr_t const *const attr = get_ia32_attr_const(node);
			enc_relocation(&attr->addr.immediate);
			return;
		}
	}
	be_emit8(0x88 | OP_MEM_SRC | OP_16_32);
	enc_mod_am(out->encoding, node);
}

/**
 * Emits a MOV [mem], in.
 */
static void enc_store(const ir_node *node)
{
	ir_node  const *const value = get_irn_n(node, n_ia32_Store_val);
	x86_insn_size_t const size  = get_ia32_attr_const(node)->size;

	if (is_ia32_Immediate(value)) {
		if (size == X86_SIZE_16)
			be_emit8(0x66);
		be_emit8(0xC6 | (size != X86_SIZE_8 ? OP_16_32 : 0));
		enc_mod_am(0, node);
		enc_imm(get_ia32_immediate_attr_const(value), size);
	} else {
		arch_register_t const *const in = arch_get_irn_register(value);

		if (in->index == REG_GP_EAX) {
			ir_node const *const base = get_irn_n_reg(node, n_ia32_base);
			ir_node const *const idx  = get_irn_n_reg(node, n_ia32_index);
			if (!base && !idx) {
				/* store to constant address from EAX can be encoded as
				 * 0xA2/0xA3 [offset]*/
				if (size == X86_SIZE_8) {
					be_emit8(0xA2);
				} else {
					if (size == X86_SIZE_16)
						be_emit8(0x66);
					be_emit8(0xA3);
				}
				ia32_attr_t const *const attr = get_ia32_attr_const(node);
				enc_relocation(&attr->addr.immediate);
				return;
			}
		}

		if (size == X86_SIZE_16)
			be_emit8(0x66);
		be_emit8(0x88 | (size != X86_SIZE_8 ? OP_16_32 : 0));
		enc_mod_am(in->encoding, node);
	}
}

static void enc_conv_i2i(const ir_node *node)
{
	/*        8 16 bit source
	 * movzx B6 B7
	 * movsx BE BF */
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	unsigned opcode = 0xB6;
	if (attr->sign_extend)         opcode |= 0x08;
	if (attr->size == X86_SIZE_16) opcode |= 0x01;
	ia32_enc_0f_unop_reg(node, opcode, n_ia32_Conv_I2I_val);
}

static void enc_popcnt(ir_node const *const node)
{
	be_emit8(0xF3);
	ia32_enc_0f_unop_reg(node, 0xB8, n_ia32_Popcnt_operand);
}

/**
 * Emit a Push.
 */
static void enc_push(const ir_node *node)
{
	ir_node const *const value = get_irn_n_reg(node, n_ia32_Push_val);
	if (!value) {
		be_emit8(0xFF);
		enc_mod_am(6, node);
	} else if (is_ia32_Immediate(value)) {
		ia32_immediate_attr_t const *const attr = get_ia32_immediate_attr_const(value);
		bool                         const imm8 = ia32_is_8bit_imm(attr);
		be_emit8(0x68 | (imm8 ? OP_IMM8 : 0));
		enc_imm(attr, imm8 ? 8 : 32);
	} else {
		arch_register_t const *const reg = arch_get_irn_register(value);
		be_emit8(0x50 + reg->encoding);
	}
}

static void enc_pusheax(ir_node const *const node)
{
	(void)node;
	arch_register_t const *const reg = &ia32_registers[REG_EAX];
	be_emit8(0x50 + reg->encoding);
}

/**
 * Emit a Pop.
 */
static void enc_pop(const ir_node *node)
{
	arch_register_t const *const reg = arch_get_irn_register_out(node, pn_ia32_Pop_res);
	be_emit8(0x58 + reg->encoding);
}

static void enc_popmem(const ir_node *node)
{
	be_emit8(0x8F);
	enc_mod_am(0, node);
}

static void enc_call(ir_node const *const node)
{
	ir_node *const callee = get_irn_n(node, n_ia32_Call_callee);
	if (is_ia32_Immediate(callee)) {
		x86_imm32_t const *const imm
			= &get_ia32_immediate_attr_const(callee)->imm;
		assert(imm->kind == X86_IMM_PCREL);

		if (ia32_cg_config.emit_machcode) {
			/* Cheat because I cannot find a way to output .long ENTITY
			 * as a PC relative relocation. See emit_jit_entity_relocation_asm()
			 * for the other half of the cheat! */
			be_emit_reloc_entity(5, X86_IMM_PCREL, imm->entity, imm->offset);
		} else {
			be_emit8(0xE8);
			x86_imm32_t const call_imm = {
				.kind   = X86_IMM_PCREL,
				.entity = imm->entity,
				.offset = imm->offset - 4,
			};
			enc_relocation(&call_imm);
		}
	} else {
		ia32_enc_unop(node, 0xFF, 2, n_ia32_Call_callee);
	}
}

static void enc_jmp(ir_node const *const cfop)
{
	be_emit8(0xE9);
	enc_jmp_destination(cfop);
}

static void enc_jump(const ir_node *node)
{
	if (!be_is_fallthrough(node))
		enc_jmp(node);
}

static void enc_jcc(x86_condition_code_t pnc, ir_node const *const cfop)
{
	unsigned char cc = pnc2cc(pnc);
	be_emit8(0x0F);
	be_emit8(0x80 + cc);
	enc_jmp_destination(cfop);
}

static void enc_jp(bool odd, ir_node const *const cfop)
{
	be_emit8(0x0F);
	be_emit8(0x8A + odd);
	enc_jmp_destination(cfop);
}

static void enc_ia32_jcc(const ir_node *node)
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

	if (cc & x86_cc_float_parity_cases) {
		/* Some floating point comparisons require a test of the parity flag,
		 * which indicates that the result is unordered */
		if (cc & x86_cc_negated) {
			enc_jp(false, projs.t);
		} else {
			/* we need a local label if the false proj is a fallthrough
			 * as the falseblock might have no label emitted then */
			if (fallthrough) {
				be_emit8(0x7A);
				be_emit8(0x06);  // jp + 6
			} else {
				enc_jp(false, projs.f);
			}
		}
	}
	enc_jcc(cc, projs.t);

	/* the second Proj might be a fallthrough */
	if (fallthrough) {
		/* it's a fallthrough */
	} else {
		enc_jmp(projs.f);
	}
}

static void enc_switchjmp(const ir_node *node)
{
	be_emit8(0xFF); // jmp *tbl.label(,%in,4)
	enc_mod_am(0x05, node);

	ia32_switch_attr_t const *const attr = get_ia32_switch_attr_const(node);
	be_emit_jump_table(node, &attr->swtch, mode_P, ia32_emit_jumptable_target);
}

static void enc_return(const ir_node *node)
{
	const ia32_return_attr_t *attr = get_ia32_return_attr_const(node);
	unsigned pop = attr->pop;
	if (attr->emit_pop || pop > 0) {
		be_emit8(0xC2);
		be_emit16(pop);
	} else {
		be_emit8(0xC3);
	}
}

static void enc_subsp(const ir_node *node)
{
	/* sub %in, %esp */
	ia32_enc_binop(node, 5);
	/* mov %esp, %out */
	arch_register_t const *const out = arch_get_irn_register_out(node, pn_ia32_SubSP_addr);
	enc_mov(&ia32_registers[REG_ESP], out);
}

static void enc_incsp(const ir_node *node)
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
	be_emit8(0x80 | OP_16_32 | (imm8b ? OP_IMM8 : 0));

	const arch_register_t *reg  = arch_get_irn_register_out(node, 0);
	enc_modru(reg, ext);

	if (imm8b) {
		be_emit8(offs);
	} else {
		be_emit32(offs);
	}
}

static void enc_copybi(const ir_node *node)
{
	unsigned size = get_ia32_copyb_size(node);
	if (size & 1)
		be_emit8(0xA4); // movsb
	if (size & 2) {
		be_emit8(0x66);
		be_emit8(0xA5); // movsw
	}
	size >>= 2;
	while (size--) {
		be_emit8(0xA5); // movsl
	}
}

void ia32_enc_fsimple(uint8_t const opcode)
{
	be_emit8(0xD9);
	be_emit8(opcode);
}

void ia32_enc_fbinop(ir_node const *const node, unsigned const op_fwd,
                     unsigned const op_rev)
{
	ia32_x87_attr_t const *const attr = get_ia32_x87_attr_const(node);
	x87_attr_t      const *const x87  = &attr->x87;
	unsigned               const op   = attr->attr.ins_permuted ? op_rev : op_fwd;
	if (get_ia32_op_type(node) == ia32_Normal) {
		assert(!x87->pop || x87->res_in_reg);

		unsigned char op0 = 0xD8;
		if (x87->res_in_reg) op0 |= 0x04;
		if (x87->pop)        op0 |= 0x02;
		be_emit8(op0);

		enc_modru(attr->x87.reg, op);
	} else {
		assert(!x87->reg);
		assert(!x87->pop);

		be_emit8(attr->attr.size == X86_SIZE_32 ? 0xD8 : 0xDC);
		enc_mod_am(op, node);
	}
}

void ia32_enc_fop_reg(ir_node const *const node, uint8_t const op0,
                      uint8_t const op1)
{
	be_emit8(op0);
	be_emit8(op1 + get_ia32_x87_attr_const(node)->x87.reg->encoding);
}

static void enc_fild(const ir_node *node)
{
	x86_insn_size_t const size = get_ia32_attr_const(node)->size;
	switch (size) {
	case X86_SIZE_16:
		be_emit8(0xDF); // filds
		enc_mod_am(0, node);
		return;

	case X86_SIZE_32:
		be_emit8(0xDB); // fildl
		enc_mod_am(0, node);
		return;

	case X86_SIZE_64:
		be_emit8(0xDF); // fildll
		enc_mod_am(5, node);
		return;

	case X86_SIZE_8:
	case X86_SIZE_128:
	case X86_SIZE_80:
		break;
	}
	panic("invalid mode size");
}

static void enc_fist(const ir_node *node)
{
	x86_insn_size_t const size = get_ia32_attr_const(node)->size;
	switch (size) {
		unsigned op;
	case X86_SIZE_16: be_emit8(0xDF); op = 2; goto enc; // fist[p]s
	case X86_SIZE_32: be_emit8(0xDB); op = 2; goto enc; // fist[p]l
	case X86_SIZE_64: be_emit8(0xDF); op = 6; goto enc; // fistpll
enc:
		if (get_ia32_x87_attr_const(node)->x87.pop)
			++op;
		// There is only a pop variant for 64 bit integer store.
		assert(size < X86_SIZE_64 || get_ia32_x87_attr_const(node)->x87.pop);
		enc_mod_am(op, node);

	case X86_SIZE_8:
	case X86_SIZE_80:
	case X86_SIZE_128:
		break;
	}
	panic("invalid mode size");
}

static void enc_fisttp(ir_node const *const node)
{
	x86_insn_size_t const size = get_ia32_attr_const(node)->size;
	switch (size) {
	case X86_SIZE_16: be_emit8(0xDF); break; // fisttps
	case X86_SIZE_32: be_emit8(0xDB); break; // fisttpl
	case X86_SIZE_64: be_emit8(0xDD); break; // fisttpll
	case X86_SIZE_8:
	case X86_SIZE_80:
	case X86_SIZE_128:
		panic("unexpected mode size");
	}
	enc_mod_am(1, node);
}

static void enc_fld(const ir_node *node)
{
	x86_insn_size_t const size = get_ia32_attr_const(node)->size;
	switch (size) {
	case X86_SIZE_32:
		be_emit8(0xD9); // flds
		enc_mod_am(0, node);
		return;

	case X86_SIZE_64:
		be_emit8(0xDD); // fldl
		enc_mod_am(0, node);
		return;

	case X86_SIZE_80:
		be_emit8(0xDB); // fldt
		enc_mod_am(5, node);
		return;

	case X86_SIZE_8:
	case X86_SIZE_16:
	case X86_SIZE_128:
		break;
	}
	panic("unexpected mode size");
}

static void enc_fldcw(const ir_node *node)
{
	be_emit8(0xD9); // fldcw
	enc_mod_am(5, node);
}

static void enc_fst(const ir_node *node)
{
	x86_insn_size_t const size = get_ia32_attr_const(node)->size;
	switch (size) {
		unsigned op;
	case X86_SIZE_32: be_emit8(0xD9); op = 2; goto enc; // fst[p]s
	case X86_SIZE_64: be_emit8(0xDD); op = 2; goto enc; // fst[p]l
	case X86_SIZE_80: be_emit8(0xDB); op = 6; goto enc; // fstpt
enc:
		if (get_ia32_x87_attr_const(node)->x87.pop)
			++op;
		/* There is only a pop variant for long double store. */
		assert(size < X86_SIZE_80 || get_ia32_x87_attr_const(node)->x87.pop);
		enc_mod_am(op, node);

	case X86_SIZE_8:
	case X86_SIZE_16:
	case X86_SIZE_128:
		break;
	}
	panic("unexpected mode size");
}

static void enc_fnstcw(const ir_node *node)
{
	be_emit8(0xD9); // fnstcw
	enc_mod_am(7, node);
}

static void enc_fnstsw(void)
{
	be_emit8(0xDF); // fnstsw %ax
	be_emit8(0xE0);
}

static void enc_ftstfnstsw(const ir_node *node)
{
	(void)node;
	be_emit8(0xD9); // ftst
	be_emit8(0xE4);
	enc_fnstsw();
}

static void enc_fucomi(const ir_node *node)
{
	const ia32_x87_attr_t *attr = get_ia32_x87_attr_const(node);
	be_emit8(attr->x87.pop ? 0xDF : 0xDB); // fucom[p]i
	be_emit8(0xE8 + attr->x87.reg->encoding);
}

static void enc_fucomfnstsw(const ir_node *node)
{
	const ia32_x87_attr_t *attr = get_ia32_x87_attr_const(node);
	be_emit8(0xDD); // fucom[p]
	be_emit8((attr->x87.pop ? 0xE8 : 0xE0) + attr->x87.reg->encoding);
	enc_fnstsw();
}

static void enc_fucomppfnstsw(const ir_node *node)
{
	(void)node;
	be_emit8(0xDA); // fucompp
	be_emit8(0xE9);
	enc_fnstsw();
}

static void ia32_register_binary_emitters(void)
{
	be_init_emitters();

	ia32_register_spec_binary_emitters();

	/* benode emitter */
	be_set_emitter(op_be_Copy,            enc_copy);
	be_set_emitter(op_be_CopyKeep,        enc_copy);
	be_set_emitter(op_be_IncSP,           enc_incsp);
	be_set_emitter(op_be_Perm,            enc_perm);
	be_set_emitter(op_ia32_Ret,           enc_return);
	be_set_emitter(op_ia32_Bswap,         enc_bswap);
	be_set_emitter(op_ia32_Bt,            enc_bt);
	be_set_emitter(op_ia32_CMovcc,        enc_cmovcc);
	be_set_emitter(op_ia32_Call,          enc_call);
	be_set_emitter(op_ia32_Const,         enc_mov_const);
	be_set_emitter(op_ia32_Conv_I2I,      enc_conv_i2i);
	be_set_emitter(op_ia32_CopyB_i,       enc_copybi);
	be_set_emitter(op_ia32_Dec,           enc_dec);
	be_set_emitter(op_ia32_FldCW,         enc_fldcw);
	be_set_emitter(op_ia32_FnstCW,        enc_fnstcw);
	be_set_emitter(op_ia32_FtstFnstsw,    enc_ftstfnstsw);
	be_set_emitter(op_ia32_FucomFnstsw,   enc_fucomfnstsw);
	be_set_emitter(op_ia32_Fucomi,        enc_fucomi);
	be_set_emitter(op_ia32_FucomppFnstsw, enc_fucomppfnstsw);
	be_set_emitter(op_ia32_IMulImm,       enc_imulimm);
	be_set_emitter(op_ia32_Inc,           enc_inc);
	be_set_emitter(op_ia32_Jcc,           enc_ia32_jcc);
	be_set_emitter(op_ia32_Jmp,           enc_jump);
	be_set_emitter(op_ia32_LdTls,         enc_ldtls);
	be_set_emitter(op_ia32_Lea,           enc_lea);
	be_set_emitter(op_ia32_Load,          enc_load);
	be_set_emitter(op_ia32_Minus64,       enc_minus64);
	be_set_emitter(op_ia32_Pop,           enc_pop);
	be_set_emitter(op_ia32_PopMem,        enc_popmem);
	be_set_emitter(op_ia32_Popcnt,        enc_popcnt);
	be_set_emitter(op_ia32_Push,          enc_push);
	be_set_emitter(op_ia32_PushEax,       enc_pusheax);
	be_set_emitter(op_ia32_Sbb0,          enc_sbb0);
	be_set_emitter(op_ia32_Setcc,         enc_setcc);
	be_set_emitter(op_ia32_ShlD,          enc_shld);
	be_set_emitter(op_ia32_ShrD,          enc_shrd);
	be_set_emitter(op_ia32_Store,         enc_store);
	be_set_emitter(op_ia32_SubSP,         enc_subsp);
	be_set_emitter(op_ia32_SwitchJmp,     enc_switchjmp);
	be_set_emitter(op_ia32_Test,          enc_test);
	be_set_emitter(op_ia32_Xor0,          enc_xor0);
	be_set_emitter(op_ia32_fild,          enc_fild);
	be_set_emitter(op_ia32_fist,          enc_fist);
	be_set_emitter(op_ia32_fisttp,        enc_fisttp);
	be_set_emitter(op_ia32_fld,           enc_fld);
	be_set_emitter(op_ia32_fst,           enc_fst);
}

static void assign_block_fragment_num(ir_node *const block, unsigned const num)
{
	assert(ir_nodehashmap_get(void, &block_fragmentnum, block) == NULL);
	ir_nodehashmap_insert(&block_fragmentnum, block, INT_TO_PTR(num));
}

static void gen_binary_block(ir_node *const block)
{
	ir_graph *const irg = get_irn_irg(block);

	uint8_t p2align  = 0;
	uint8_t max_skip = 0;
	if (block != get_irg_end_block(irg)
	 && ia32_cg_config.label_alignment > 0 && ia32_should_align_block(block)) {
		p2align  = ia32_cg_config.label_alignment;
		max_skip = ia32_cg_config.label_alignment_max_skip;
	}

	unsigned fragment_num = be_begin_fragment(p2align, max_skip);
	assert(fragment_num
	       == (unsigned)PTR_TO_INT(ir_nodehashmap_get(void, &block_fragmentnum, block)));
	(void)fragment_num;

	/* emit the contents of the block */
	sched_foreach(block, node) {
		be_emit_node(node);
	}

	be_finish_fragment();
}

ir_jit_function_t *ia32_emit_jit(ir_jit_segment_t *const segment,
                                 ir_graph *const irg)
{
	ia32_register_binary_emitters();

	ir_node **const blk_sched = be_create_block_schedule(irg);

	be_jit_begin_function(segment);

	/* we use links to point to target blocks */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	be_emit_init_cf_links(blk_sched);

	ir_nodehashmap_init(&block_fragmentnum);
	size_t n = ARR_LEN(blk_sched);
	for (size_t i = 0; i < n; ++i) {
		ir_node *block = blk_sched[i];
		assign_block_fragment_num(block, (unsigned)i);
	}
	for (size_t i = 0; i < n; ++i) {
		ir_node *block = blk_sched[i];
		gen_binary_block(block);
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	ir_nodehashmap_destroy(&block_fragmentnum);

	return be_jit_finish_function();
}

static void enc_nop_callback(char *buffer, unsigned size)
{
	memset(buffer, 0, size);
	while (size > 0) {
		switch (size) {
		case 1: buffer[0] = 0x90; return;
		case 2:
			buffer[0] = 0x66;
			++buffer;
			--size;
			continue;
		case 3:
		sequence_0f1f:
			buffer[0] = 0x0F;
			buffer[1] = 0x1F;
			return;
		case 4: buffer[2] = 0x40; goto sequence_0f1f;
		case 5: buffer[2] = 0x44; goto sequence_0f1f;
		case 6:
			buffer[0] = 0x66;
			++buffer;
			--size;
			continue;
		case 7: buffer[2] = 0x80; goto sequence_0f1f;
		case 8: buffer[2] = 0x84; goto sequence_0f1f;
		default:
			buffer[0] = 0x66;
			buffer[1] = 0x0F;
			buffer[2] = 0x1F;
			buffer[3] = 0x84;
			buffer += 9;
			size   -= 9;
			continue;
		}
	}
}

static unsigned enc_relocation_callback(char *const buffer,
                                        uint8_t const be_kind,
                                        ir_entity *const entity,
                                        int32_t const offset)
{
	uint32_t value;
	if (entity == NULL) {
		assert(be_kind == IA32_RELOCATION_RELJUMP);
		value = (uint32_t)offset;
	} else {
		intptr_t const entity_addr = (intptr_t)be_jit_get_entity_addr(entity);
		if (entity_addr == (intptr_t)-1)
			panic("Could not resolve address of entity %+F", entity);
		intptr_t addr = entity_addr + offset;
		if (be_kind == X86_IMM_PCREL)
			addr -= (intptr_t)buffer;
		value = (uint32_t)addr;
		if ((intptr_t)value != addr)
			panic("Overflow in relocation");
	}

	memcpy(buffer, &value, 4);
	return 4;
}

void ia32_emit_jit_function(char *buffer, ir_jit_function_t *const function)
{
	static const be_jit_emit_interface_t jit_emit_interface = {
		.nops       = enc_nop_callback,
		.relocation = enc_relocation_callback,
	};
	be_jit_emit_memory(buffer, function, &jit_emit_interface);
}
