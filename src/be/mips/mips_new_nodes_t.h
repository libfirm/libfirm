/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Internal declarations used by gen_new_nodes.c
 */
#ifndef FIRM_BE_MIPS_MIPS_NEW_NODES_T_H
#define FIRM_BE_MIPS_MIPS_NEW_NODES_T_H

#include <stdio.h>

#include "firm_types.h"
#include "irop.h"

void mips_dump_node(FILE *F, const ir_node *n, dump_reason_t reason);

int mips_attrs_equal(ir_node const *a, ir_node const *b);
int mips_cond_attrs_equal(ir_node const *a, ir_node const *b);
int mips_immediate_attrs_equal(ir_node const *a, ir_node const *b);
int mips_switch_attrs_equal(ir_node const *a, ir_node const *b);

#endif
