/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author  Sebastian Hack
 * @date    20.04.2007
 * @brief
 *
 * Primitive depth-first search on the CFG.
 */
#ifndef FIRM_ANA_DFS_H
#define FIRM_ANA_DFS_H

#include "absgraph.h"

typedef struct dfs_t      dfs_t;
typedef struct dfs_node_t dfs_node_t;
typedef struct dfs_edge_t dfs_edge_t;

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

#endif
