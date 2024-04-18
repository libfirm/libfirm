/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Christoph Mallon
 */

/**
 * @file
 * @brief   Helper for handling 2-address code instructions
 * @author  Christoph Mallon
 */
#ifndef FIRM_BE_BE2ADDR_H
#define FIRM_BE_BE2ADDR_H

#include <stdbool.h>

#include "be_types.h"

/**
 * Type of the 2-address code handler callback.
 *
 * @param node  The node with the violated constraint
 * @param req   The violated out constraint
 * @param reg   The register assigned to the out
 *
 * @return Whether the handler resolved the constraint violation
 */
typedef bool be_handle_2addr_callback_t(ir_node *node, arch_register_req_t const *req, arch_register_t const *reg);

/**
 * Generic handler to detect and handle 2-address code constraint violations.
 *
 * @param irg       The graph to process
 * @param callback  Optional backend-specific callback to handle violated
 *                  2-address code constraints
 */
void be_handle_2addr(ir_graph *irg, be_handle_2addr_callback_t *callback);

#endif
