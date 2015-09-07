/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @date   26.01.2006
 * @author Sebastian Hack
 * @brief Implements bipartite matchings.
 */
#ifndef FIRM_ADT_BIPARTITE_H
#define FIRM_ADT_BIPARTITE_H

#include <stdio.h>

#include "../begin.h"

/**
 * @ingroup algorithms
 * @defgroup bipartite Bipartite Matching
 * Solved bipartite matching problem.
 * (Variant with only 0/1 costs)
 * @{
 */

/** internal representation of bipartite matching problem */
typedef struct bipartite_t bipartite_t;

/** Create new bipartite matching problem with @p n_left elements on left side
 * and @p n_right elements on right side */
FIRM_API bipartite_t *bipartite_new(unsigned n_left, unsigned n_right);
/** Free memory occupied by bipartite matching problem */
FIRM_API void bipartite_free(bipartite_t *gr);
/** Add edge from @p i (on the left side) to @p j (on the right side) */
FIRM_API void bipartite_add(bipartite_t *gr, unsigned i, unsigned j);
/** Remove edge from @p i (on the left side) to @p j (on the right side) */
FIRM_API void bipartite_remv(bipartite_t *gr, unsigned i, unsigned j);
/** Return 1 if edge from @p i (on the left side) to @p j (on the right side)
 * exists, 0 otherwise */
FIRM_API int bipartite_adj(bipartite_t const *gr, unsigned i, unsigned j);
/** Solve bipartite matching problem */
FIRM_API void bipartite_matching(bipartite_t const *gr, int *matching);

/**
 * Dumps a bipartite graph to a file stream.
 */
FIRM_API void bipartite_dump_f(FILE *f, bipartite_t const *gr);

/**
 * Dumps a bipartite graph to file name.
 */
FIRM_API void bipartite_dump(char const *name, bipartite_t const *gr);

/** @} */

#include "../end.h"

#endif
