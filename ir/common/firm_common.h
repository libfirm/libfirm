/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Martin Trapp, Christian Schaefer &
*          Goetz Lindenmaier
*
* firm_common.h: common firm declarations
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

/* a list of firm kinds */
typedef enum {
  k_entity,
  k_type,
  k_ir_node,
  k_ir_loop
} firm_kind;

/* returns the kind of the thing */
firm_kind get_kind(void *firm_thing);

/* returns a string. */
const char* print_firm_kind(void *firm_thing);

# endif /*_FIRM_COMMON_H_ */
