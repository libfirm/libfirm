/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer &
**          Goetz Lindenmaier
**
** common.c:
*/

#include "common.h"
#include "irgraph.h"

/* returns the kind of the thing */
firm_kind
get_kind (void *firm_thing) {
  return *(firm_kind *)firm_thing;
}
