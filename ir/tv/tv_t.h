/* Declarations for Target Values.
   Copyright (C) 1995, 1996 Christian von Roques */

/**
* @file tv_t.h
*
* @author Christian von Roques
*/

/* $Id$ */

#ifndef _TV_T_H_
#define _TV_T_H_

#include "tv.h"

/**
 * This struct represents the aforementioned tarvals.
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
    ir_mode *mode; 		/**< the mode of the stored value */
    const void *value; 		/**< the value stored in an internal way... */
    unsigned int length; 	/**< the length of the stored value */
};

/** remove tarval representing an entity that is about to be destroyed */
void free_tarval_entity(entity *ent);

#endif /* _TV_T_H_ */
