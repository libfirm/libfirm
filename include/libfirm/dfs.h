/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @file    dfs.h
 * @author  Sebastian Hack
 * @date    20.04.2007
 * @version $Id$
 * @summary
 *
 * Primitive depth-first search on the CFG.
 */
#ifndef FIRM_ANA_DFS_H
#define FIRM_ANA_DFS_H

#include "absgraph.h"

typedef struct _dfs_t      dfs_t;
typedef struct _dfs_node_t dfs_node_t;
typedef struct _dfs_edge_t dfs_edge_t;

typedef enum {
	DFS_EDGE_ANC,
	DFS_EDGE_FWD,
	DFS_EDGE_CROSS,
	DFS_EDGE_BACK
} dfs_edge_kind_t;

extern dfs_edge_kind_t dfs_get_edge_kind(const dfs_t *dfs, const void *src, const void *tgt);

extern dfs_t *dfs_new(const absgraph_t *graph_impl, void *graph);
extern void dfs_free(dfs_t *dfs);
extern void dfs_dump(const dfs_t *dfs, FILE *file);

#endif /* FIRM_ANA_DFS_H */
