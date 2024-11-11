/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Matthias Braun
 */

/**
 * @file
 * @brief   Internal declarations used by gen_new_nodes.c
 */
#ifndef FIRM_BE_ARM_ARM_NEW_NODES_T_H
#define FIRM_BE_ARM_ARM_NEW_NODES_T_H

#include "arm_new_nodes.h"

void init_arm_attributes(ir_node *node);

void init_arm_load_store_attributes(ir_node *res, ir_mode *ls_mode,
                                    ir_entity *entity, int entity_sign,
                                    long offset, bool is_frame_entity);

void init_arm_shifter_operand(ir_node *res, unsigned shifter_op_input,
                              unsigned immediate_value,
                              arm_shift_modifier_t shift_modifier,
                              unsigned shift_immediate);

void init_arm_cmp_attr(ir_node *res, bool ins_permuted, bool is_unsigned);
void init_arm_Address_attributes(ir_node *res, ir_entity *entity, int offset);
void init_arm_farith_attributes(ir_node *res, ir_mode *mode);

int arm_Address_attrs_equal(const ir_node *a, const ir_node *b);
int arm_CondJmp_attrs_equal(const ir_node *a, const ir_node *b);
int arm_SwitchJmp_attrs_equal(const ir_node *a, const ir_node *b);
int arm_attrs_equal(const ir_node *a, const ir_node *b);
int arm_cmp_attrs_equal(const ir_node *a, const ir_node *b);
int arm_fConst_attrs_equal(const ir_node *a, const ir_node *b);
int arm_farith_attrs_equal(const ir_node *a, const ir_node *b);
int arm_load_store_attrs_equal(const ir_node *a, const ir_node *b);
int arm_shifter_operands_equal(const ir_node *a, const ir_node *b);

void arm_dump_node(FILE *F, const ir_node *n, dump_reason_t reason);

#endif
