/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Representation of and static computations on target machine
 *           values -- private header.
 * @date     2003
 * @author   Mathias Heil
 */
#ifndef FIRM_TV_TV_T_H
#define FIRM_TV_TV_T_H

#include "firm_common.h"
#include "irmode.h"
#include "tv.h"

#define get_tarval_mode(tv)      _get_tarval_mode(tv)
#define get_tarval_bad()         _get_tarval_bad()
#define get_tarval_undefined()   _get_tarval_undefined()
#define get_tarval_b_false()     _get_tarval_b_false()
#define get_tarval_b_true()      _get_tarval_b_true()
#define get_tarval_unreachable() _get_tarval_unreachable()
#define get_tarval_reachable()   _get_tarval_reachable()
#define is_tarval(thing)         _is_tarval(thing)

/**
 * Initialization of the tarval module.
 *
 * Call before init_mode().
 *
 * @param null_value
 *            The reference mode NULL value, typical 0l
 * @param support_quad_precision
 *            If non-zero, activate support for quad precision
 */
void init_tarval_1(long null_value, int support_quad_precision);

/**
 * Initialization of the tarval module.
 *
 * Call after init_mode().
 */
void init_tarval_2(void);

/**
 * Free all memory occupied by the tarval module.
 */
void finish_tarval(void);

/**
 * This struct represents the afore mentioned tarvals.
 *
 * A tarval struct consists of an internal representation of the
 * value and some additional fields further describing the value.
 *
 * ATTRIBUTES:
 *   - ir_mode *mode     The mode of the stored value
 *   - void *value       The internal representation
 *
 * @sa
 *   irmode.h for predefined modes
 */
struct ir_tarval {
	firm_kind  kind;    /**< must be k_tarval */
	ir_mode    *mode;   /**< the mode of the stored value */
	const void *value;  /**< the value stored in an internal way... */
	size_t     length;  /**< the length of the stored value */
};

/* inline functions */
/*
 * Access routines for tarval fields ========================================
 */
static inline ir_mode *_get_tarval_mode(const ir_tarval *tv)
{
	return tv->mode;
}

static inline ir_tarval *_get_tarval_bad(void)
{
	return tarval_bad;
}

static inline ir_tarval *_get_tarval_undefined(void)
{
	return tarval_undefined;
}

static inline ir_tarval *_get_tarval_b_false(void)
{
	return tarval_b_false;
}

static inline ir_tarval *_get_tarval_b_true(void)
{
	return tarval_b_true;
}

static inline ir_tarval *_get_tarval_reachable(void)
{
	return tarval_reachable;
}

static inline ir_tarval *_get_tarval_unreachable(void)
{
	return tarval_unreachable;
}

static inline int _is_tarval(const void *thing)
{
	return get_kind(thing) == k_tarval;
}

#endif /* FIRM_TV_TV_T_H */
