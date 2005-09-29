
/**
 * @file   beuses_t.h
 * @date   27.06.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _BEUSES_T_H
#define _BEUSES_T_H

#include "firm_config.h"

#include "beuses.h"

/**
 * An association between a node and a point in time.
 */
struct _loc_t {
  ir_node *irn;  /**< A node. */
  unsigned time;       /**< A time. */
};

#define LOC_IS_INFINITE(loc) USES_IS_INIFINITE((loc)->time)

/**
 * Comparison function for locations.
 * @param a The first location.
 * @param b The second one.
 * @return see qsort(3) for semantic of the compare functions.
 */
int loc_compare(const void *loc1, const void *loc2);


static INLINE loc_t *
be_get_next_use_loc(
    be_uses_t *uses,
    const loc_t *from,
    loc_t *loc)
{
  loc->time = be_get_next_use(uses, from->irn, from->time, loc->irn);
  return loc;
}



#endif /* _BEUSES_T_H */
