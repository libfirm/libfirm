/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Backend diagnostic output.
 */
#ifndef FIRM_BE_BEDIAGNOSTIC_H
#define FIRM_BE_BEDIAGNOSTIC_H

#include "firm_types.h"

void be_errorf(ir_node const *node, char const *fmt, ...);
void be_warningf(ir_node const *node, char const *fmt, ...);

#endif
