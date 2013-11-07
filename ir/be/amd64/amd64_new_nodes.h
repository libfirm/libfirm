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
 * Returns the attributes of an amd64 node.
 */
amd64_attr_t *get_amd64_attr(ir_node *node);
const amd64_attr_t *get_amd64_attr_const(const ir_node *node);

const amd64_switch_jmp_attr_t *get_amd64_switch_jmp_attr_const(const ir_node *node);
amd64_switch_jmp_attr_t *get_amd64_switch_jmp_attr(ir_node *node);

const amd64_cc_attr_t *get_amd64_cc_attr_const(const ir_node *node);
amd64_cc_attr_t *get_amd64_cc_attr(ir_node *node);

const amd64_movimm_attr_t *get_amd64_movimm_attr_const(const ir_node *node);
amd64_movimm_attr_t *get_amd64_movimm_attr(ir_node *node);

/* Include the generated headers */
#include "gen_amd64_new_nodes.h"

#endif
