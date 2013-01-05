/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Handles fpu rounding modes
 * @author  Matthias Braun
 */
#ifndef FIRM_BE_IA32_IA32_FPU_H
#define FIRM_BE_IA32_IA32_FPU_H

#include "firm_types.h"

/**
 * Handle switching of fpu mode
 */
void ia32_setup_fpu_mode(ir_graph *irg);

#endif
