/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer &
**          Goetz Lindenmaier
**
** common.h: common firm declarations
*/

# ifndef _COMMON_H_
# define _COMMON_H_


/* set to compile with extensions for compiler constructon lab. */
# define UEBPRAKT 1

/* If this flag is set, new_r_Phi_in uses an explicit stack for
   allocating and deallocating Phi nodes.  Else it uses the obstack
   as a stack! */
#define USE_EXPICIT_PHI_IN_STACK 1

/* a list of firm kinds */
typedef enum {
  k_entity,
  k_type_class,
  k_type_strct,
  k_type_method,
  k_type_union,
  k_type_array,
  k_type_enumeration,
  k_type_pointer,
  k_type_primitive,
  k_ir_node
} firm_kind;

/* returns the kind of the thing */
firm_kind get_kind(void *firm_thing);


# endif /*_COMMON_H_ */
