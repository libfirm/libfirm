/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer &
**          Goetz Lindenmaier
**
** common.h: common firm declarations
*/

/* $Id$ */

# ifndef _COMMON_H_
# define _COMMON_H_

#define INLINE inline

/* a list of firm kinds */
typedef enum {
  k_entity,
  k_type,
  k_ir_node
} firm_kind;

/* returns the kind of the thing */
firm_kind get_kind(void *firm_thing);

/* returns a string. */
const char* print_firm_kind(void *firm_thing);

# endif /*_COMMON_H_ */
