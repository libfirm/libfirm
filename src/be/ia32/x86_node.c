/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Helper functions for immediates / relocations.
 * @author Matthias Braun
 */
#include "x86_node.h"

#include "bediagnostic.h"
#include "beemitter.h"
#include "begnuas.h"
#include "betranshlp.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "irprintf.h"
#include "panic.h"
#include "tv_t.h"
#include <inttypes.h>

char const *x86_pic_base_label;

static bool check_immediate_constraint(long val, char immediate_constraint_type)
{
	/* Workaround for warning about always true comparison when long has 32 bits. */
	long long const v31 = 2147483648;
	switch (immediate_constraint_type) {
	case 'g':
	case 'i':
	case 'n': return true;

	case 'e': return -v31 <= val && val < v31;
	case 'I': return    0 <= val && val <  32;
	case 'J': return    0 <= val && val <  64;
	case 'K': return -128 <= val && val < 128;
	case 'L': return val == 0xff || val == 0xffff;
	case 'M': return    0 <= val && val <   4;
	case 'N': return    0 <= val && val < 256;
	case 'O': return    0 <= val && val < 128;
	}
	panic("invalid immediate constraint found");
}

bool x86_match_immediate(x86_imm32_t *immediate, const ir_node *node,
                         char const constraint)
{
	ir_mode *const mode = get_irn_mode(node);
	if (get_mode_arithmetic(mode) != irma_twos_complement)
		return false;

	ir_tarval *offset;
	ir_entity *entity;
	unsigned   reloc_kind;
	if (!be_match_immediate(node, &offset, &entity, &reloc_kind))
		return false;

	long val = 0;
	if (offset) {
		if (!tarval_is_long(offset)) {
			be_warningf(node, "tarval is not long");
			return false;
		}

		val = get_tarval_long(offset);
		if (!check_immediate_constraint(val, constraint))
			return false;
	}

	x86_immediate_kind_t kind = (x86_immediate_kind_t)reloc_kind;
	if (entity != NULL) {
		/* We need an immediate, which can hold all bits of an address. */
		switch (constraint) {
		case 'e': if (get_mode_size_bits(mode_P) > 32) return false;
		case 'g': break;
		case 'i': break;
		default:  return false;
		}
		if (kind == X86_IMM_VALUE)
			kind = X86_IMM_ADDR;
	}

	/* we are fine */
	immediate->entity = entity;
	immediate->offset = (int32_t)val;
	immediate->kind   = kind;
	return true;
}

char const *x86_get_immediate_kind_str(x86_immediate_kind_t const kind)
{
	switch (kind) {
	case X86_IMM_VALUE:       return "value";
	case X86_IMM_ADDR:        return "addr";
	case X86_IMM_PCREL:       return "pcrel";
	case X86_IMM_PICBASE_REL: return "picbase_rel";
	case X86_IMM_TLS_IE:      return "tls_ie";
	case X86_IMM_TLS_LE:      return "tls_le";
	case X86_IMM_FRAMEENT:    return "frameent";
	case X86_IMM_FRAMEOFFSET: return "frameoffset";
	case X86_IMM_GOTPCREL:    return "gotpcrel";
	case X86_IMM_GOTOFF:      return "gotoff";
	case X86_IMM_GOT:         return "got";
	case X86_IMM_PLT:         return "plt";
	}
	return "invalid";
}

void x86_dump_imm32(x86_imm32_t const *const imm, FILE *const F)
{
	if (imm->entity != NULL)
		ir_fprintf(F, "%+F", imm->entity);
	if (imm->offset != 0 || imm->entity == NULL)
		fprintf(F, imm->entity != NULL ? "%+"PRId32 : "%"PRId32, imm->offset);
	if (imm->kind != X86_IMM_VALUE && imm->kind != X86_IMM_ADDR)
		fprintf(F, " [%s]", x86_get_immediate_kind_str(imm->kind));
}

void x86_emit_relocation_no_offset(x86_immediate_kind_t const kind,
                                   ir_entity const *const entity)
{
	be_gas_emit_entity(entity);
	switch (kind) {
	case X86_IMM_ADDR:
	case X86_IMM_PCREL:
		return;
	case X86_IMM_PICBASE_REL:
		be_emit_char('-');
		be_emit_string(x86_pic_base_label);
		return;
	char const *rstr;
	case X86_IMM_GOTPCREL: rstr = "@GOTPCREL";  goto emit_str;
	case X86_IMM_PLT:      rstr = "@PLT";       goto emit_str;
	case X86_IMM_TLS_IE:   rstr = "@INDNTPOFF"; goto emit_str;
	case X86_IMM_TLS_LE:   rstr = "@NTPOFF";    goto emit_str;
	case X86_IMM_GOT:      rstr = "@GOT";       goto emit_str;
	case X86_IMM_GOTOFF:   rstr = "@GOTOFF";    goto emit_str;
	emit_str:
		be_emit_string(rstr);
		return;

	case X86_IMM_VALUE:
	case X86_IMM_FRAMEENT:
	case X86_IMM_FRAMEOFFSET:
		break;
	}
	panic("unexpected or invalid immediate kind");
}

void x86_emit_imm32(x86_imm32_t const *const imm)
{
	x86_immediate_kind_t const kind   = imm->kind;
	int32_t              const offset = imm->offset;
	if (kind == X86_IMM_VALUE) {
		assert(imm->entity == NULL);
		be_emit_irprintf("%"PRId32, (uint32_t)offset);
	} else {
		x86_emit_relocation_no_offset(kind, imm->entity);
		if (offset != 0)
			be_emit_irprintf("%+"PRId32, offset);
	}
}
