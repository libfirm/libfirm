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
    ir_mode *mode;      /**< the mode of the stored value */
    const void *value;      /**< the value stored in an internal way... */
    unsigned int length;    /**< the length of the stored value */
  /* void *link; */                   /**< some opaque information */
};

/** remove tarval representing an entity that is about to be destroyed */
void free_tarval_entity(entity *ent);

#endif /* _TV_T_H_ */
