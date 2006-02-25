/**
 * @file   bipartite.h
 * @date   26.01.2006
 * @author Sebastian Hack
 *
 * Copyright (C) 2006 Universitaet Karlsruhe
 * Released under the GPL
 *
 * Implements bipartite matchings.
 *
 */

#ifndef _BIPARTITE_H
#define _BIPARTITE_H

typedef struct _bipartite_t bipartite_t;

bipartite_t *bipartite_new(int n_left, int n_right);
void bipartite_free(bipartite_t *gr);
void bipartite_add(bipartite_t *gr, int i, int j);
void bipartite_remv(bipartite_t *gr, int i, int j);
int bipartite_adj(const bipartite_t *gr, int i, int j);
void bipartite_matching(const bipartite_t *gr, int *matching);
void bipartite_dump(FILE *f, const bipartite_t *gr);

#endif /* _BIPARTITE_H */
