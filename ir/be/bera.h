/**
 * Register allocation functions.
 * @author Sebastian Hack
 * @date 13.1.2005
 */

#ifndef _BERA_H
#define _BERA_H

#include "irnode.h"

/**
 * Check, if two values interfere.
 * @param a The first value.
 * @param b The second value.
 * @return 1, if @p a and @p b interfere, 0 if not.
 */
int values_interfere(const ir_node *a, const ir_node *b);

#endif /* _BERA_H */
