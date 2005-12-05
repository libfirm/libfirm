/*
 * Project:     libFIRM
 * File name:   ir/tv/tv_t.h
 * Purpose:     Representation of and static computations on target machine
 *              values -- private header.
 * Author:      Mathias Heil
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _TV_T_H_
#define _TV_T_H_

#include "firm_config.h"

#include <assert.h>
#include "entity.h"
#include "irmode.h"
#include "tv.h"

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
struct tarval {
  firm_kind kind;         /**< must be k_tarval */
  ir_mode   *mode;        /**< the mode of the stored value */
  const void *value;      /**< the value stored in an internal way... */
  unsigned int length;    /**< the length of the stored value */
};

/** remove tarval representing an entity that is about to be destroyed */
void free_tarval_entity(entity *ent);

/* inline functions */
/*
 * Access routines for tarval fields ========================================
 */
static INLINE ir_mode *
_get_tarval_mode(const tarval *tv) {
  assert(tv);
  return tv->mode;
}

static INLINE tarval *
_get_tarval_bad(void) {
  return tarval_bad;
}

static INLINE tarval *
_get_tarval_undefined(void) {
  return tarval_undefined;
}

static INLINE tarval *
_get_tarval_b_false(void) {
  return tarval_b_false;
}

static INLINE tarval *
_get_tarval_b_true(void) {
  return tarval_b_true;
}

static INLINE tarval *
_get_tarval_P_void(void) {
  return tarval_P_void;
}

#define get_tarval_mode(tv)     _get_tarval_mode(tv)
#define get_tarval_bad()        _get_tarval_bad()
#define get_tarval_undefined()  _get_tarval_undefined()
#define get_tarval_b_false()    _get_tarval_b_false()
#define get_tarval_b_true()     _get_tarval_b_true()
#define get_tarval_P_void()     _get_tarval_P_void()

#endif /* _TV_T_H_ */
