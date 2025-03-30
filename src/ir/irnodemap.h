/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   A nodemap. This variant is a thin wrapper around an ARR_F which
 *          uses node-indices for access. It is preferable over ir_nodehashmap
 *          if the info is dense (i.e. something is mapped for most nodes in
 *          the graph)
 * @author  Matthias Braun
 */
#ifndef FIRM_IRNODEMAP_H
#define FIRM_IRNODEMAP_H

#include "irnodemap_t.h"
#include "firm_types.h"
#include "array.h"
#include "irnode_t.h"
#include "irgraph_t.h"

/**
 * Allocate and initialize a new nodemap object
 *
 * @param irg           The graph the nodemap will run on.
 * @return              A new nodemap object.
 */
static inline void ir_nodemap_init(ir_nodemap *nodemap, const ir_graph *irg)
{
	unsigned max_idx = get_irg_last_idx(irg) + 32;
	nodemap->data = NEW_ARR_FZ(void*, max_idx);
}

/**
 * frees all internal memory used by the nodemap but does not free the
 * nodemap struct itself.
 */
static inline void ir_nodemap_destroy(ir_nodemap *nodemap)
{
	DEL_ARR_F(nodemap->data);
	nodemap->data = NULL;
}

/**
 * Insert a mapping from @p node to @p data.
 */
static inline void ir_nodemap_insert(ir_nodemap *nodemap, const ir_node *node,
                                     void *data)
{
	unsigned idx = get_irn_idx(node);
	size_t   len = ARR_LEN(nodemap->data);
	if (idx >= len) {
		ARR_RESIZE(void*, nodemap->data, idx+1);
		memset(nodemap->data + len, 0, (idx-len) * sizeof(nodemap->data[0]));
	}
	nodemap->data[idx] = data;
}

/**
 * Insert a mapping from @p node to @p data (fast version).
 *
 * @attention You must only use this version if you can be sure that the nodemap
 * already has enough space to store the mapping. This is the case if @p node
 * already existed at nodemap_init() time or ir_nodemap_insert() has been used
 * for this node)
 */
static inline void ir_nodemap_insert_fast(ir_nodemap *nodemap,
                                          const ir_node *node, void *data)
{
	unsigned idx = get_irn_idx(node);
	assert(idx < ARR_LEN(nodemap->data));
	nodemap->data[idx] = data;
}

/**
 * Get mapping for @p node. Returns NULL if nothing is mapped.
 */
static inline void *ir_nodemap_get(const ir_nodemap *nodemap,
                                   const ir_node *node)
{
	unsigned idx = get_irn_idx(node);
	if (idx >= ARR_LEN(nodemap->data))
		return NULL;
	return nodemap->data[idx];
}

#define ir_nodemap_get(type, nodemap, node) ((type*)ir_nodemap_get(nodemap, node))

/**
 * Get mapping for @p node (fast version). Returns NULL if nothing is mapped.
 *
 * @attention You must only use this function if you can be sure that the
 * nodemap has enough space to potentially contain the mapping. This is the
 * case if @p node already existed at nodemap_init() time or ir_nodemap_insert()
 * has been used for this node)
 */
static inline void *ir_nodemap_get_fast(const ir_nodemap *nodemap,
                                        const ir_node *node)
{
	unsigned idx = get_irn_idx(node);
	assert(idx < ARR_LEN(nodemap->data));
	return nodemap->data[idx];
}

#endif
