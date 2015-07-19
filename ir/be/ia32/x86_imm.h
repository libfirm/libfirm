/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Data structures for immediate values and relocations.
 * @author Matthias Braun
 */
#ifndef FIRM_BE_IA32_IMM_H
#define FIRM_BE_IA32_IMM_H

#include <stdbool.h>
#include <stdint.h>
#include "compiler.h"
#include "firm_types.h"

typedef struct x86_imm32_t {
	ir_entity *entity;
	int32_t    offset;
} x86_imm32_t;

bool x86_match_immediate(x86_imm32_t *immediate, const ir_node *node,
                         char constraint);

static inline bool x86_imm32_equal(x86_imm32_t const *const imm0,
								   x86_imm32_t const *const imm1)
{
	return imm0->entity == imm1->entity && imm0->offset == imm1->offset;
}

#endif
