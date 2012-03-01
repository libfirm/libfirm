/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @date   26.01.2006
 * @author Sebastian Hack
 * @brief Implements bipartite matchings.
 */
#ifndef FIRM_ADT_BIPARTITE_H
#define FIRM_ADT_BIPARTITE_H

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
FIRM_API bipartite_t *bipartite_new(int n_left, int n_right);
/** Free memory occupied by bipartite matching problem */
FIRM_API void bipartite_free(bipartite_t *gr);
/** Add edge from @p i (on the left side) to @p j (on the right side) */
FIRM_API void bipartite_add(bipartite_t *gr, int i, int j);
/** Remove edge from @p i (on the left side) to @p j (on the right side) */
FIRM_API void bipartite_remv(bipartite_t *gr, int i, int j);
/** Return 1 if edge from @p i (on the left side) to @p j (on the right side)
 * exists, 0 otherwise */
FIRM_API int bipartite_adj(const bipartite_t *gr, int i, int j);
/** Solve bipartite matching problem */
FIRM_API void bipartite_matching(const bipartite_t *gr, int *matching);

/**
 * Dumps a bipartite graph to a file stream.
 */
FIRM_API void bipartite_dump_f(FILE *f, const bipartite_t *gr);

/**
 * Dumps a bipartite graph to file name.
 */
FIRM_API void bipartite_dump(const char *name, const bipartite_t *gr);

/** @} */

#include "../end.h"

#endif /* _BIPARTITE_H */
