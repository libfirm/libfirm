/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

/**
 * @file
 * @brief   Internal declarations used by gen_new_nodes.c
 */
#ifndef FIRM_BE_RISCV_RISCV_NEW_NODES_T_H
#define FIRM_BE_RISCV_RISCV_NEW_NODES_T_H

#include <stdio.h>
#include <inttypes.h>

#include "firm_types.h"
#include "irop.h"

void riscv_dump_node(FILE *F, const ir_node *n, dump_reason_t reason);

int riscv_attrs_equal(ir_node const *a, ir_node const *b);
int riscv_immediate_attrs_equal(ir_node const *a, ir_node const *b);
int riscv_cond_attrs_equal(ir_node const *a, ir_node const *b);
int riscv_switch_attrs_equal(ir_node const *a, ir_node const *b);

void riscv_set_attr_imm(ir_node *res, ir_entity *entity, int32_t immediate_value);

#endif
