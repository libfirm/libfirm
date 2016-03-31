/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   This file implements functions to finalize the irg for emit.
 * @author  Christian Wuerdig
 */
#ifndef FIRM_BE_IA32_IA32_FINISH_H
#define FIRM_BE_IA32_IA32_FINISH_H

#include "firm_types.h"

/**
 * Check 2-Addresscode constraints and call peephole optimizations.
 * @param irg  The irg to finish
 */
void ia32_finish_irg(ir_graph *irg);

#endif
