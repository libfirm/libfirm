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

/** Global flags.  Set these by autoconf?? **/

/* There are two implementations of the Phi node construction.  The first
   is faster, but does not work for blocks with more than 2 predecessors.
   The second works always but is slower and causes more unnecessary Phi
   nodes.
   Select the implementations by the following preprocessor flag: */
#define USE_FAST_PHI_CONSTRUCTION 0

/* Further there are two versions of the fast Phi node construction.
   If the following flag is set, new_r_Phi_in uses an explicit stack for
   allocating and deallocating Phi nodes.  Else it uses the obstack
   as a stack! */
#define USE_EXPICIT_PHI_IN_STACK 1

/* If this is defined debuging aids are created, e.g. a field in
   ir_node uniquely numbering the nodes. */
#define DEBUG_libfirm 1

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

/* returns a string. */
const char* print_firm_kind(void *firm_thing);


# endif /*_COMMON_H_ */
