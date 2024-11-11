/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Matthias Braun
 */

/**
 * @file
 * @brief   Internal declarations used by gen_new_nodes.c
 */
#ifndef FIRM_BE_IA32_IA32_NEW_NODES_T_H
#define FIRM_BE_IA32_IA32_NEW_NODES_T_H

#include "ia32_new_nodes.h"

void ia32_dump_node(FILE *F, const ir_node *n, dump_reason_t reason);

void init_ia32_attributes(ir_node *node, x86_insn_size_t size);
void init_ia32_x87_attributes(ir_node *res);
void init_ia32_immediate_attributes(ir_node *res, x86_imm32_t const *const imm);
void init_ia32_call_attributes(ir_node* res, uint8_t pop, uint8_t n_reg_res);
void init_ia32_copyb_attributes(ir_node *res, unsigned size);
void init_ia32_condcode_attributes(ir_node *res, x86_condition_code_t cc);
void init_ia32_climbframe_attributes(ir_node *res, unsigned count);
void init_ia32_switch_attributes(ir_node *node,
                                 ir_switch_table const *const table,
                                 ir_entity const *const table_entity);
void init_ia32_return_attributes(ir_node *node, uint16_t pop);

unsigned ia32_hash_Immediate(const ir_node *irn);

int ia32_attrs_equal(const ir_node *a, const ir_node *b);
int ia32_condcode_attrs_equal(const ir_node *a, const ir_node *b);
int ia32_call_attrs_equal(const ir_node *a, const ir_node *b);
int ia32_copyb_attrs_equal(const ir_node *a, const ir_node *b);
int ia32_immediate_attrs_equal(const ir_node *a, const ir_node *b);
int ia32_x87_attrs_equal(const ir_node *a, const ir_node *b);
int ia32_climbframe_attrs_equal(const ir_node *a, const ir_node *b);
int ia32_switch_attrs_equal(const ir_node *a, const ir_node *b);
int ia32_return_attrs_equal(const ir_node *a, const ir_node *b);

void ia32_init_op(ir_op *op, unsigned latency);

#endif
