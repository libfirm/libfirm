/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Matthias Braun
 */

/**
 * @file
 * @brief   Internal declarations used by gen_new_nodes.c
 */
#ifndef FIRM_BE_AMD64_AMD64_NEW_NODES_T_H
#define FIRM_BE_AMD64_AMD64_NEW_NODES_T_H

#include "amd64_nodes_attr.h"

void amd64_dump_node(FILE *F, const ir_node *n, dump_reason_t reason);

void init_amd64_attributes(ir_node *node, amd64_op_mode_t op_mode,
                           x86_insn_size_t size);

void init_amd64_cc_attributes(ir_node *node, x86_condition_code_t cc);

void init_amd64_movimm_attributes(ir_node *node, const amd64_imm64_t *imm);

void init_amd64_copyb_attributes(ir_node *node, unsigned size);

int amd64_attrs_equal(const ir_node *a, const ir_node *b);
int amd64_addr_attrs_equal(const ir_node *a, const ir_node *b);
int amd64_binop_addr_attrs_equal(const ir_node *a, const ir_node *b);
int amd64_movimm_attrs_equal(const ir_node *a, const ir_node *b);
int amd64_copyb_attrs_equal(const ir_node *const a, const ir_node *const b);
int amd64_shift_attrs_equal(const ir_node *a, const ir_node *b);
int amd64_cc_attrs_equal(const ir_node *a, const ir_node *b);
int amd64_switch_jmp_attrs_equal(const ir_node *a, const ir_node *b);
int amd64_call_addr_attrs_equal(const ir_node *a, const ir_node *b);
int amd64_x87_attrs_equal(const ir_node *a, const ir_node *b);
int amd64_x87_addr_attrs_equal(const ir_node *a, const ir_node *b);
int amd64_x87_binop_addr_attrs_equal(const ir_node *a, const ir_node *b);

#endif
