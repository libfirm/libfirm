/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Scalar replacement of compounds.
 * @author  Beyhan Veliev, Michael Beck
 */
#ifndef FIRM_OPT_SCALAR_REPLACE_H
#define FIRM_OPT_SCALAR_REPLACE_H

#include "firm_types.h"
#include <stdbool.h>

/**
 * Returns non-zero, if the address of an entity
 * represented by a Sel node (or its successor Sels) is taken.
 *
 * @param sel  the Sel node
 */
bool is_address_taken(ir_node *sel);

#endif
