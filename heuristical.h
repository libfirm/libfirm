#ifndef KAPS_HEURISTICAL_H
#define KAPS_HEURISTICAL_H

#include "pbqp_t.h"

/**
 * Create an empty PBQP instance with the given number of nodes.
 */
pbqp* alloc_pbqp(int number_nodes);

/**
 * Free the given PBQP.
 */
void free_pbqp(pbqp *pbqp);

#endif /* KAPS_HEURISTICAL_H */
