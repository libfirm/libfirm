/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

#ifndef FIRM_BE_RISCV_RISCV_CCONV_H
#define FIRM_BE_RISCV_RISCV_CCONV_H

#include "be_types.h"

typedef struct riscv_reg_or_slot_t {
	arch_register_t const *reg;
	unsigned               offset;
	ir_entity             *entity;
} riscv_reg_or_slot_t;

typedef struct riscv_calling_convention_t {
	unsigned             param_stack_size;
	unsigned             n_mem_param;
	unsigned             n_parameters; 
	riscv_reg_or_slot_t *parameters;
	riscv_reg_or_slot_t *results;
    ir_entity          *va_start_addr; 
} riscv_calling_convention_t;

void riscv_determine_calling_convention(riscv_calling_convention_t *cconv, ir_type *fun_type);

void riscv_layout_parameter_entities(riscv_calling_convention_t *cconv, ir_graph *irg);

void riscv_free_calling_convention(riscv_calling_convention_t *cconv);

#endif
