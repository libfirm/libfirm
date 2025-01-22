/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   irargs private declarations
 */
#ifndef FIRM_IR_IRARGS_H
#define FIRM_IR_IRARGS_H

#include "lc_printf.h"

/**
 * Get the firm printf arg environment.
 * @return The environment.
 */
lc_arg_env_t *firm_get_arg_env(void);

#endif
