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

#include <assert.h>
#include "firm_common.h"
#include "irmode_t.h"
#include "tv.h"
#include "strcalc.h"
#include <stdint.h>
#include <stdbool.h>

#define get_tarval_mode(tv)      get_tarval_mode_(tv)
#define get_tarval_bad()         get_tarval_bad_()
#define get_tarval_unknown()     get_tarval_unknown_()
#define get_tarval_b_false()     get_tarval_b_false_()
#define get_tarval_b_true()      get_tarval_b_true_()
#define tarval_is_constant(tv)   tarval_is_constant_(tv)

/**
 * Initialization of the tarval module.
 *
 * Call before init_mode().
 */
void init_tarval_1(void);

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
 * This struct represents the aforementioned tarvals.
 *
 * A tarval struct consists of an internal representation of the
 * value and some additional fields further describing the value.
 *
 * @sa
 *   irmode.h for predefined modes
 */
struct ir_tarval {
	firm_kind     kind;    /**< must be k_tarval */
	uint16_t      length;  /**< the length of the stored value */
	ir_mode      *mode;    /**< the mode of the stored value */
	unsigned char value[]; /**< the value stored in an internal way */
};

/* inline functions */
/*
 * Access routines for tarval fields ========================================
 */
static inline ir_mode *get_tarval_mode_(ir_tarval const *tv)
{
	return tv->mode;
}

static inline ir_tarval *get_tarval_bad_(void)
{
	return tarval_bad;
}

static inline ir_tarval *get_tarval_unknown_(void)
{
	return tarval_unknown;
}

static inline ir_tarval *get_tarval_b_false_(void)
{
	return tarval_b_false;
}

static inline ir_tarval *get_tarval_b_true_(void)
{
	return tarval_b_true;
}

static inline int tarval_is_constant_(ir_tarval const *tv)
{
	return tv != tarval_bad && tv != tarval_unknown;
}


/**
 *   Checks whether a pointer points to a tarval.
 *
 *   @param thing     an arbitrary pointer
 *
 *   @return
 *       true if the thing is a tarval, else false
 */
static inline bool is_tarval(const void *thing)
{
	return get_kind(thing) == k_tarval;
}

/**
 * Converts tarval to ascii representation (in contrast to tarval_snprintf()
 * this is meant to be machine readble).
 * If the output is bigger than buf_len the behaviour is undefined. The
 * final value may be near the end of the buffer, use the return value!
 */
const char *ir_tarval_to_ascii(char *buf, size_t buf_len, ir_tarval const *tv);

/**
 * Converts ascii representation to tarval with specified mode. Compatible with
 * ir_tarval_to_ascii().
 */
ir_tarval *ir_tarval_from_ascii(const char *buf, ir_mode *mode);

uint64_t get_tarval_uint64(ir_tarval const *tv);

bool tarval_is_uint64(ir_tarval const *tv);

bool tarval_is_minus_null(ir_tarval const *tv);

bool tarval_is_minus_one(ir_tarval const *tv);

ir_tarval *get_tarval_small(ir_mode *mode);

ir_tarval *get_tarval_epsilon(ir_mode *mode);

/**
 * Get the number of bits required to reconstruct this tarval by sign extension.
 */
unsigned get_tarval_magnitude(ir_tarval const *tv);

/**
 * Get the @p idx'th bit of the internal representation of the given tarval
 * @p tv.
 */
static inline unsigned tarval_get_bit(ir_tarval const *const tv,
                                      unsigned const idx)
{
#ifndef NDEBUG
	ir_mode *mode = get_tarval_mode(tv);
	assert(mode_is_data(mode));
	assert(idx < get_mode_size_bits(mode));
#endif
	return sc_get_bit_at(tv->value, idx);
}

bool tarval_in_range(ir_tarval const *min, ir_tarval const *val, ir_tarval const *max);

void init_mode_values(ir_mode *mode);

#endif
