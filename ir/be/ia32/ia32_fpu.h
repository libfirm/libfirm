/**
 * @file
 * @brief   Handles fpu rounding modes
 * @author  Matthias Braun
 * @version $Id$
 */
#ifndef FIRM_BE_IA32_FPU_H
#define FIRM_BE_IA32_FPU_H

#include "bearch_ia32_t.h"

/**
 * Handle switching of fpu mode
 */
void ia32_setup_fpu_mode(ia32_code_gen_t *cg);

#endif
