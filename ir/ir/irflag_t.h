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

#define get_opt_cse()                      get_opt_cse_()
#define get_optimize()                     get_optimize_()
#define get_opt_constant_folding()         get_opt_constant_folding_()
#define get_opt_algebraic_simplification() get_opt_algebraic_simplification_()

/**
 * libFIRM optimizations flags
 */
typedef enum {
#define FLAG(name, value, def)    irf_##name = (1 << value),

#include "irflag_t.def"
	irf_last
#undef FLAG
} libfirm_opts_t;

extern optimization_state_t libFIRM_opt;

/** initialises the flags */
void firm_init_flags(void);

static inline int get_opt_cse_(void)
{
	return (libFIRM_opt & irf_cse) != 0;
}

static inline int get_opt_constant_folding_(void)
{
	return (libFIRM_opt & irf_constant_folding) != 0;
}

static inline int get_opt_algebraic_simplification_(void)
{
	return (libFIRM_opt & irf_algebraic_simplification) != 0;
}

static inline int get_optimize_(void)
{
	return (libFIRM_opt & irf_optimize) != 0;
}

#endif
