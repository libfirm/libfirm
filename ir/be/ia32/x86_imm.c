/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Helper functions for immediates / relocations.
 * @author Matthias Braun
 */
#include "x86_imm.h"

#include "bediagnostic.h"
#include "betranshlp.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "panic.h"
#include "tv_t.h"

static bool check_immediate_constraint(long val, char immediate_constraint_type)
{
	switch (immediate_constraint_type) {
	case 'g':
	case 'i':
	case 'n': return true;

	case 'I': return 0 <= val && val <=  31;
	case 'J': return 0 <= val && val <=  63;
	case 'K': return -128 <= val && val < 128;
	case 'L': return val == 0xff || val == 0xffff;
	case 'M': return 0 <= val && val <=   3;
	case 'N': return 0 <= val && val <= 255;
	case 'O': return 0 <= val && val <= 127;
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
	if (!be_match_immediate(node, &offset, &entity))
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

	x86_immediate_kind_t kind = X86_IMM_VALUE;
	if (entity != NULL) {
		/* we need full 32bits for entities */
		if (constraint != 'i' && constraint != 'g')
			return false;
		kind = X86_IMM_ADDR;
	}

	/* we are fine */
	immediate->entity = entity;
	immediate->offset = (int32_t)val;
	immediate->kind   = kind;
	return true;
}
