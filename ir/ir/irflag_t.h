/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Flags to control optimizations, inline implementation.
 * @author   Michael Beck, Sebastian Hack
 */
#ifndef FIRM_IR_IRFLAG_T_H
#define FIRM_IR_IRFLAG_T_H

#include "irflag.h"

/**
 * libFIRM optimizations flags
 */
typedef enum {
#define E_FLAG(name, value, def)    irf_##name = (1 << value),
#define I_FLAG(name, value, def)    irf_##name = (1 << value),

#include "irflag_t.def"
	irf_last
#undef I_FLAG
#undef E_FLAG
} libfirm_opts_t;

/**
 * libFIRM running flags
 */
typedef enum {
#define E_FLAG(name, value, def)
#define I_FLAG(name, value, def)

#include "irflag_t.def"
	ir_rf_last
#undef I_FLAG
#undef E_FLAG
} libfirm_running_t;

extern optimization_state_t libFIRM_opt, libFIRM_running, libFIRM_verb;

/** initialises the flags */
void firm_init_flags(void);

/* generate the getter functions for external access */
#define E_FLAG(name, value, def)                    \
static inline int get_opt_##name##_(void) {         \
  return libFIRM_opt & irf_##name;                  \
}

/* generate the getter functions for internal access */
#define I_FLAG(name, value, def)                   \
static inline int get_opt_##name(void) {           \
  return libFIRM_opt & irf_##name;                 \
}

#include "irflag_t.def"

#undef I_FLAG
#undef E_FLAG

static inline int get_optimize_(void)
{
	return get_opt_optimize();
}

#define get_optimize()                           get_optimize_()
#define get_opt_cse()                            get_opt_cse_()

#endif
