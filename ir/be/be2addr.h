/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Christoph Mallon
 */

/**
 * @file
 * @brief   Helpers for handling 2-address code instructions
 * @author  Christoph Mallon
 */
#ifndef FIRM_BE_BE2ADDR_H
#define FIRM_BE_BE2ADDR_H

#include <stdbool.h>

#include "be_types.h"

typedef bool be_handle_2addr_callback_t(ir_node *node, arch_register_req_t const *req, arch_register_t const *reg);

void be_handle_2addr(ir_graph *const irg, be_handle_2addr_callback_t *const handle_2addr_callback);

#endif
