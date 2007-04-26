/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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

typedef struct _bipartite_t bipartite_t;

bipartite_t *bipartite_new(int n_left, int n_right);
void bipartite_free(bipartite_t *gr);
void bipartite_add(bipartite_t *gr, int i, int j);
void bipartite_remv(bipartite_t *gr, int i, int j);
int bipartite_adj(const bipartite_t *gr, int i, int j);
void bipartite_matching(const bipartite_t *gr, int *matching);

/**
 * Dumps a bipartite graph to a file stream.
 */
void bipartite_dump_f(FILE *f, const bipartite_t *gr);

/**
 * Dumps a bipartite graph to file name.
 */
void bipartite_dump(const char *name, const bipartite_t *gr);

#endif /* _BIPARTITE_H */
