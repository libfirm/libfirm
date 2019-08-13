/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Internal declarations used by gen_new_nodes.c
 */
#ifndef FIRM_BE_VHDL_VHDL_NEW_NODES_T_H
#define FIRM_BE_VHDL_VHDL_NEW_NODES_T_H

#include <stdio.h>

#include "firm_types.h"
#include "irop.h"
#include "vhdl_nodes_attr.h"

void vhdl_dump_node(FILE *F, const ir_node *n, dump_reason_t reason);

int vhdl_attrs_equal(ir_node const *a, ir_node const *b);
int vhdl_cmp_attrs_equal(ir_node const *a, ir_node const *b);
int vhdl_immediate_attrs_equal(ir_node const *a, ir_node const *b);
int vhdl_varsig_attrs_equal(ir_node const *a, ir_node const *b);
int vhdl_varsig_attrs_equal_(vhdl_varsig_attr_t const *a, vhdl_varsig_attr_t const *b);
int vhdl_start_attrs_equal(ir_node const *a, ir_node const *b);

#endif
