/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Some machine dependent optimizations.
 * @date   1.10.2004
 * @author Sebastian Hack
 */
#ifndef FIRM_IR_IRARCH_H
#define FIRM_IR_IRARCH_H

#include "firm_types.h"
#include "begin.h"

/**
 * @addtogroup iroptimize
 * @{
 */

/**
 * Optimization flags.
 */
typedef enum arch_dep_opts_t {
	arch_dep_none         = 0,
	arch_dep_mul_to_shift = 1u << 0,  /**< optimize Mul into Shift/Add/Sub */
	arch_dep_div_by_const = 1u << 1,  /**< optimize Div into Shift/Add/Mulh */
	arch_dep_mod_by_const = 1u << 2   /**< optimize Mod into Shift/Add/Mulh */
} arch_dep_opts_t;
ENUM_BITSET(arch_dep_opts_t)

typedef struct ir_settings_arch_dep_t ir_settings_arch_dep_t;

/**
 * Sets the optimizations that shall be applied.
 * @param opts  An optimization bit mask.
 */
FIRM_API void arch_dep_set_opts(arch_dep_opts_t opts);

/** @} */

#include "end.h"

#endif
