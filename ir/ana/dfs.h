/**
 * @file   dfs.h
 * @date   20.04.2007
 * @author Sebastian Hack
 *
 * Primitive depth-first search on the CFG.
 *
 * Copyright (C) 2007 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _DFS_H
#define _DFS_H

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

extern dfs_edge_kind_t dfs_get_edge_kind(const dfs_t *dfs, void *src, void *tgt);

extern dfs_t *dfs_new(const absgraph_t *graph_impl, void *graph);
extern void dfs_free(dfs_t *dfs);
extern void dfs_dump(const dfs_t *dfs, FILE *file);

#endif /* _DFS_H */
