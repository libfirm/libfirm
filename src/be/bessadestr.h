/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Performs SSA destruction.
 * @author      Daniel Grund
 * @date        25.05.2005
 */
#ifndef FIRM_BE_BESSADESTR_H
#define FIRM_BE_BESSADESTR_H

#include <stdbool.h>
#include "be_types.h"
#include "firm_types.h"

/**
 * Performs SSA destruction. Arguments get adjusted, phi nodes just stay.
 */
void be_ssa_destruction(ir_graph *irg, const arch_register_class_t *cls);

#endif
