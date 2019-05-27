/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

#ifndef FIRM_BE_RISCV_RISCV_CCONV_H
#define FIRM_BE_RISCV_RISCV_CCONV_H

#include <stdbool.h>
#include "be_types.h"

typedef struct riscv_reg_or_slot_t {
	arch_register_t const *reg;
	unsigned               offset;
	ir_entity             *entity;
} riscv_reg_or_slot_t;

typedef struct riscv_calling_convention_t {
	bool                 omit_fp;          /**< do not use frame pointer (and no
	                                           save/restore) */
	unsigned             param_stack_size;
	unsigned             n_mem_param;
	riscv_reg_or_slot_t *parameters;
	riscv_reg_or_slot_t *results;
} riscv_calling_convention_t;

void riscv_determine_calling_convention(riscv_calling_convention_t *cconv, ir_type *fun_type, ir_graph *irg);

void riscv_layout_parameter_entities(riscv_calling_convention_t *cconv, ir_graph *irg);

void riscv_free_calling_convention(riscv_calling_convention_t *cconv);

#endif
