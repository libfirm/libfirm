/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief   This file implements functions to finalize the irg for emit.
 */
#ifndef FIRM_BE_AMD64_AMD64_FINISH_H
#define FIRM_BE_AMD64_AMD64_FINISH_H

#include "firm_types.h"

/**
 * Check 2-Addresscode constraints.
 * @param irg  The irg to finish
 */
void amd64_finish_irg(ir_graph *irg);

#endif
