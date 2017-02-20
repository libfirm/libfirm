/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declarations for transform functions (code selection)
 * @author  Oliver Richter, Tobias Gneist
 */
#ifndef FIRM_BE_ARM_ARM_TRANSFORM_H
#define FIRM_BE_ARM_ARM_TRANSFORM_H

#include "firm_types.h"

/**
 * Transform a Firm graph into an ARM graph.
 */
void arm_transform_graph(ir_graph *irg);

void arm_init_transform(void);

#endif
