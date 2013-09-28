/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Function prototypes for the assembler ir node constructors.
 */
#ifndef FIRM_BE_AMD64_AMD64_NEW_NODES_H
#define FIRM_BE_AMD64_AMD64_NEW_NODES_H

#include <stdint.h>
#include "amd64_nodes_attr.h"

/**
 * Sets the input mode of the node.
 */
void set_amd64_ls_mode(ir_node *n, ir_mode *mode);

/**
 * Returns the attributes of an amd64 node.
 */
amd64_attr_t *get_amd64_attr(ir_node *node);
const amd64_attr_t *get_amd64_attr_const(const ir_node *node);

const amd64_SymConst_attr_t *get_amd64_SymConst_attr_const(const ir_node *node);
amd64_SymConst_attr_t *get_amd64_SymConst_attr(ir_node *node);

const amd64_switch_jmp_attr_t *get_amd64_switch_jmp_attr_const(const ir_node *node);
amd64_switch_jmp_attr_t *get_amd64_switch_jmp_attr(ir_node *node);

/* Include the generated headers */
#include "gen_amd64_new_nodes.h"

#endif
