/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*/

/**
 * @file firm_common.h
 *
 * common firm declarations
 *
 * @author Martin Trapp, Christian Schaefer & Goetz Lindenmaier
 */

/* $Id$ */

# ifndef _FIRM_COMMON_H_
# define _FIRM_COMMON_H_

#ifndef INLINE
#ifdef USE_GCC_INLINE
#define INLINE inline
#else
#define INLINE
#endif
#endif

/** a list of firm kinds */
typedef enum {
  k_entity,     /**< an entity */
  k_type,       /**< a type */
  k_ir_node,    /**< an ir node */
  k_ir_loop
} firm_kind;

/**
 * Returns the kind of a thing.
 *
 * @param firm_thing  pointer repraesenting a firm object
 */
firm_kind get_kind(void *firm_thing);

/** Returns the kind of a thing as a string. */
const char* print_firm_kind(void *firm_thing);

# endif /*_FIRM_COMMON_H_ */
