/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Matthias Braun
 */

/**
 * @file
 * @brief   Internal declarations used by gen_new_nodes.c
 */
#ifndef FIRM_BE_SPARC_SPARC_NEW_NODES_T_H
#define FIRM_BE_SPARC_SPARC_NEW_NODES_T_H

#include "sparc_new_nodes.h"

void sparc_dump_node(FILE *F, const ir_node *n, dump_reason_t reason);

void sparc_set_attr_imm(ir_node *res, ir_entity *entity,
                        int32_t immediate_value);
void init_sparc_jmp_cond_attr(ir_node *node, ir_relation relation,
                              bool is_unsigned);
void init_sparc_load_store_attributes(ir_node *res, ir_mode *ls_mode,
                                      ir_entity *entity, int32_t offset,
                                      bool is_frame_entity, bool is_reg_reg);
void init_sparc_fp_attributes(ir_node *res, ir_mode *fp_mode);
void init_sparc_fp_conv_attributes(ir_node *res, ir_mode *src_mode,
                                   ir_mode *dest_mode);
void init_sparc_call_attributes(ir_node *node, bool aggregate_return);

int sparc_attrs_equal(const ir_node *a, const ir_node *b);
int sparc_load_store_attrs_equal(const ir_node *a, const ir_node *b);
int sparc_jmp_cond_attrs_equal(const ir_node *a, const ir_node *b);
int sparc_fp_attrs_equal(const ir_node *a, const ir_node *b);
int sparc_fp_conv_attrs_equal(const ir_node *a, const ir_node *b);
int sparc_switch_jmp_attrs_equal(const ir_node *a, const ir_node *b);

#endif
