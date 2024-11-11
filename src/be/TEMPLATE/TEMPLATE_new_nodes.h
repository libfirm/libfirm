/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Function prototypes for the assembler ir node constructors.
 */
#ifndef FIRM_BE_TEMPLATE_TEMPLATE_NEW_NODES_H
#define FIRM_BE_TEMPLATE_TEMPLATE_NEW_NODES_H

#include "TEMPLATE_nodes_attr.h"

/**
 * Returns the attributes of an TEMPLATE node.
 */
TEMPLATE_attr_t *get_TEMPLATE_attr(ir_node *node);

const TEMPLATE_attr_t *get_TEMPLATE_attr_const(const ir_node *node);

/* Include the generated headers */
#include "gen_TEMPLATE_new_nodes.h"

#endif
